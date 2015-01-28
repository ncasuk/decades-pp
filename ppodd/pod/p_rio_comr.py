from ppodd.core import *

import datetime


def create_plot(match, co_orig, co_interp, ds):
    """
    """
    import matplotlib.pyplot as plt
    from matplotlib.dates import date2num, DateFormatter, HourLocator

    ts=match/86400.+date2num(datetime.datetime.strptime('%i-%i-%i' % tuple(ds['DATE']), '%d-%m-%Y'))
    title= ds['FLIGHT'].data.upper() + ' %.2i-%.2i-%i' % (tuple(ds['DATE']))
    plt.clf()
    perc=np.percentile(co_orig, [5, 95])
    co_orig_clean=co_orig[:]
    co_orig_clean[(co_orig_clean < perc[0]) | (co_orig_clean > perc[1])]=np.nan
    plt.plot_date(ts, co_orig_clean, 'b-')
    yl=plt.gca().get_ylim()
    plt.plot_date(ts, co_orig, 'b-', label='CO raw')
    plt.plot_date(ts, co_interp, 'g-', label='CO interp')

    plt.gca().set_ylim(yl)
    plt.title(title)
    plt.xlabel('utc (-)')
    plt.ylabel('CO mixing ratio (ppbV)')
    plt.legend(loc=2)
    plt.gca().xaxis.set_major_locator(HourLocator())
    plt.gca().xaxis.set_major_formatter(DateFormatter('%H:%M'))

    plt.twinx()
    plt.plot_date(ts, co_orig-co_interp, '-', color='red', label='CO delta')
    plt.gca().set_ylim(-10, 10)
    plt.grid()

    plt.legend()

    wow_min=np.where(ds['WOW_IND'] == 0)[0].min()
    wow_max=np.where(ds['WOW_IND'] == 0)[0].max()

    wow_times=ds['WOW_IND'].data.times/86400.+date2num(datetime.datetime.strptime('%i-%i-%i' % tuple(ds['DATE']), '%d-%m-%Y'))
    for i in [wow_min, wow_max]:
        plt.axvline(wow_times[i], lw=4, color='0.7', alpha=0.7)


def interpolate_cal_coefficients(sens, zero, cal_status, utc_time):
    """The calibration coefficients for the AL5002 instrument drift
    linearly between calibrations. To take account of this new
    coefficients are calculated for each data point, which
    can be used to recalculate the CO concentrations.

    """
    n=sens.size
    sens_new, zero_new=np.zeros(n, dtype=np.float32), np.zeros(n, dtype=np.float32)

    #get calibration periods
    ix=np.where(cal_status[1:]-cal_status[:-1] == -1)[0]
    #ignore data at the beginning
    ix=ix[ix>100]
    # the +2 is a dodgy way to make sure that the values have changed.
    # Apparently the zero and sens parameters do not change at
    # exactly the same time in the data stream
    ix=[10]+list(ix+2)+[n-1]
    for i in range(len(ix)-1):
        ix1=ix[i]
        ix2=ix[i+1]
        sens_new[ix1:ix2]=np.interp(utc_time[ix1:ix2], np.float32([utc_time[ix1], utc_time[ix2]]), [sens[ix1], sens[ix2]])
        zero_new[ix1:ix2]=np.interp(utc_time[ix1:ix2], np.float32([utc_time[ix1], utc_time[ix2]]), [zero[ix1], zero[ix2]])
    return (sens_new, zero_new)


class rio_co_mixingratio(cal_base):
    """Routine to calculate the Carbon Monoxide concentration from the AL52002 Instrument.

    The routine works with the data from the TCP packages that are stored on fish.
    Flagging is done using the static pressure and the pressure measurement in the
    calibration chamber of the instrument.

    """

    def __init__(self,dataset):
        self.input_names=['AL52CO_conc', 'AL52CO_sens', 'AL52CO_zero', 'AL52CO_cellpress', 'AL52CO_calpress', 'AL52CO_cal_status', 'AL52CO_utc_time', 'WOW_IND']
        self.outputs=[parameter('CO_AERO',
                                units='ppb',
                                frequency=1,
                                long_name='Mole fraction of Carbon Monoxide in air from the AERO AL5002 instrument',
                                standard_name='mole_fraction_of_carbon_monoxide_in_air')]
        self.version=1.00
        cal_base.__init__(self,dataset)

    def process(self):
        match=self.dataset.matchtimes(self.input_names)
        co_mr=self.dataset['AL52CO_conc'].data.ismatch(match)
        calpress=self.dataset['AL52CO_calpress'].data.ismatch(match)
        cal_status=self.dataset['AL52CO_cal_status'].data.ismatch(match)
        # cal_status is a character and needs to be converted to integer to do anything useful with it
        cal_status=np.int8(cal_status)
        sens=self.dataset['AL52CO_sens'].data.ismatch(match)
        sens[sens == 0.0] = np.nan
        zero=self.dataset['AL52CO_zero'].data.ismatch(match)
        zero[zero == 0.0] = np.nan
        utc_time=self.dataset['AL52CO_utc_time'].data.ismatch(match)
        wow_ind=self.dataset['WOW_IND'].data.ismatch(match)

        #We calculate the raw counts from the CO concentration and the calibration coefficients.
        #The AL52CO_counts variable can not be used because it does not necessarily match the
        #concentration. The data stream that is sent by the labview script is sent several times
        #a second while only *one* value in the stream is updated at a time. For example see:
        #
        #$ cat AL52CO01_20140126_060103_B828.cs
        #...
        #$AL52CO01,66,1390716327,0,0,B828,77.721321,8821.000000, ...
        #$AL52CO01,66,1390716327,0,0,B828,77.721321,8475.000000, ...
        #$AL52CO01,66,1390716328,0,0,B828,77.721321,8475.000000, ...
        #$AL52CO01,66,1390716328,0,0,B828,81.419006,8475.000000, ...
        #$AL52CO01,66,1390716328,0,0,B828,81.419006,8660.000000, ...
        #$AL52CO01,66,1390716329,0,0,B828,81.419006,8660.000000, ...
        #...
        #
        #The concentration changes in the forth line to 81.42 but the counts value is only update
        #the subsequent line.
        counts=co_mr/(1.0/sens)+zero
        #calc new interpolated calibration coefficients
        sens_new, zero_new=interpolate_cal_coefficients(sens, zero, cal_status, utc_time)

        conc_new=(counts-zero_new)*1.0/sens_new

        flag=np.zeros(co_mr.size, dtype=np.int8)     # initialize empty flag array, with all flag values set to 0
        flag[co_mr<-10]=3                            # flag very negative co_mr as 3
        flag[cal_status==1]=3                        # flag data while calibration is running
        flag[calpress>3.2]=3                         # flag when calibration gas pressure is increased

        co_aero=flagged_data(conc_new, match, flag)

        create_plot(match, co_mr, co_aero, self.dataset)

        try:
            create_plot(match, co_mr, co_aero, self.dataset)
        except:
            pass
        self.outputs[0].data=co_aero
