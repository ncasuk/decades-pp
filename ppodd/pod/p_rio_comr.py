"""
Decades processing routine for processing the data stream from the AERO AL5002 instrument.

Variables available are:

 AL52CO_cal_status
 AL52CO_calpress
 AL52CO_cellpress
 AL52CO_conc
 AL52CO_counts
 AL52CO_err
 AL52CO_flight_num
 AL52CO_lampflow
 AL52CO_lamptemp
 AL52CO_monoflow
 AL52CO_monopress
 AL52CO_monotemp
 AL52CO_packet_length
 AL52CO_ptp_sync
 AL52CO_sens
 AL52CO_temppmt
 AL52CO_ue9LJ_temp
 AL52CO_utc_time
 AL52CO_zero


Carbon Monoxide concentrations are calculated by linearly interpolation of the zero and sens
values inbetween calibrations.

The concentration of the calibration gas might be revised after a campaign and therefore the
CO concentration needs to be scaled. This is taken care off by a scaling factor which is the
fourth number in the CALCOMX flight constant. If the scaling factor is not available it is
assumed to be 1.0.

Data are flagged as "3" if either the AL52CO_cal_status flag is "1" or if the pressure in the
calibration chamber (AL52CO_calpress) exceeds as defined threshold (3.4).

"""

from ppodd.core import *

import datetime
import sys


def create_plot(match, co_orig, co_interp, cal_status, ds):
    """Creates an overview plot, that shows the CO timeseries before and after
    the interpolation of the calibration coefficients.

    On the second y-axis the CO delta is plotted. Calibration periods are indicated
    by black dots.

    """
    import matplotlib.pyplot as plt
    from matplotlib.dates import date2num, num2date, DateFormatter, HourLocator

    dt=datetime.datetime.strptime('%i-%i-%i' % tuple(ds['DATE']), '%d-%m-%Y')

    ts=match/86400.+date2num(dt)
    dt=datetime.datetime.strptime('%0.2i-%0.2i-%0.4i' % tuple(ds['DATE']), '%d-%m-%Y')
    title='QA-CO Aerolaser\n'+'%s - %s' % (ds['FLIGHT'].data.lower(), dt.strftime('%d-%b-%Y'))

    fig=plt.figure()
    ax0=fig.add_subplot(111)
    perc=np.percentile(co_orig, [2, 98])
    co_orig_clean=co_orig[:]
    co_orig_clean[(co_orig_clean > perc[1]) | (co_orig_clean < 0) | (cal_status == 1)]=np.nan
    ax0.plot_date(ts, co_orig_clean, 'b-')
    yl=ax0.get_ylim()
    ax0.plot_date(ts, co_orig, 'b-', label='CO raw')
    co_interp[cal_status == 1]=np.nan
    ax0.plot_date(ts, co_interp, 'g-', label='CO interp')

    ax0.set_ylim(yl)
    ax0.set_title(title)
    ax0.set_xlabel('utc (-)')
    ax0.set_ylabel('CO mixing ratio (ppbV)')
    ax0.xaxis.set_major_locator(HourLocator())
    ax0.xaxis.set_major_formatter(DateFormatter('%H:%M'))
    ax0.xaxis.grid(True)

    ax1=ax0.twinx()
    ax1.plot_date(ts, co_orig-co_interp, '-', color='red', label='CO delta')
    ax1.set_ylim(-10, 10)
    #axplt.grid()

    # overplot time periods when instrument is calibrating
    cal_status_ix=np.where(cal_status == 1)[0]
    if len(cal_status_ix > 0):
        plt.plot_date(ts[cal_status_ix], ts[cal_status_ix]*0.0, 'o', markersize=5, color='black', label='Cal')

    # add padding (3 percent) to the left and right of the plot
    xlim_margin=(ax0.get_xlim()[1]-ax0.get_xlim()[0])*0.03
    ax0.set_xlim((ax0.get_xlim()[0]-xlim_margin, ax0.get_xlim()[1]+xlim_margin))

    ax0.legend(loc='upper left')
    ax1.legend(loc='upper right')
    # estimate T/O and Landing and plot two vertical lines
    wow_min, wow_max = 0, 0
    counter=np.arange(ds['WOW_IND'][:].size)
    wow_min=np.where((ds['WOW_IND'][:] == 0) & (ds['HGT_RADR'][:,0] > 100))[0]
    if wow_min.size:
        wow_min=wow_min[0]
    wow_max=np.where((ds['WOW_IND'][:] == 1) & (counter > wow_min))[0]
    if wow_max.size:
        wow_max=wow_max[0]


    # overplot T/O and Landing on the figure
    wow_times=ds['WOW_IND'].data.times/86400.+date2num(datetime.datetime.strptime('%i-%i-%i' % tuple(ds['DATE']), '%d-%m-%Y'))
    for i in [wow_min, wow_max]:
        if i:
            ax0.axvline(wow_times[i], lw=4, color='0.7', alpha=0.7)


def interpolate_cal_coefficients(utc_time, sens, zero):
    """The calibration coefficients for the AL5002 instrument drift
    inbetween calibrations. It is assumed that the drifting is linear
    and too take account of this new coefficients are calculated for
    each data point, which are then used to recalculate the CO concentrations.

    """
    # create copies of sens and zero calibration coefficients
    sens_new, zero_new = sens[:], zero[:]
    #fill nan values with previous non-nan value
    for i in range(1, len(sens_new)):
        if np.isnan(sens_new[i]):
            sens_new[i]=sens_new[i-1]
        if np.isnan(zero_new[i]):
            zero_new[i]=zero_new[i-1]
    # get calibration periods
    ix=np.where(sens[1:]-sens[:-1] != 0)[0]
    #ix=np.where(sens[1:] != sens[:-1])[0]
    # remove nan values
    ix=ix[~np.isnan((sens[1:]-sens[:-1])[ix])]
    # ignore the first 100 data points
    ix=ix[ix>100]
    # the +2 is a dodgy way to make sure that the values have changed.
    # Apparently the zero and sens parameters do not change at
    # exactly the same time in the data stream
    ix=[10]+list(ix+4)+[sens.size-4]
    # loop over all calibration periods
    for i in range(len(ix)-1):
        ix1=ix[i]
        ix2=ix[i+1]
        sens_new[ix1:ix2]=np.interp(utc_time[ix1:ix2], np.float32([utc_time[ix1], utc_time[ix2]]), [sens[ix1], sens[ix2]])
        zero_new[ix1:ix2]=np.interp(utc_time[ix1:ix2], np.float32([utc_time[ix1], utc_time[ix2]]), [zero[ix1], zero[ix2]])

        # write calibration information to stdout
        timestamp=datetime.datetime.utcfromtimestamp(utc_time[ix1]).strftime('%Y-%m-%d %H:%M:%S')
        if i == 0:
            sys.stdout.write('\n    CO AERO Calibrations\n')
            sys.stdout.write('    '+41*'-'+'\n')
            sys.stdout.write('    | time                |   sens |   zero |\n')
            sys.stdout.write('    |'+39*'-'+'|\n')
        if np.isnan(sens[ix1]):
            sens_string='   nan'
        else:
            sens_string='%6.2f' % (sens[ix1],)
        if np.isnan(zero[ix1]):
            zero_string='   nan'
        else:
	    zero_string='%6i' % (zero[ix1],)
        sys.stdout.write('    | %s | %s | %s |\n' % (timestamp, sens_string, zero_string))

    sys.stdout.write('    '+41*'-'+'\n')
    return (sens_new, zero_new)


class rio_co_mixingratio(cal_base):
    """Routine to calculate the Carbon Monoxide concentration from the AL52002 Instrument.

    The routine works with the data from the TCP packages that are stored on Fish and Septic.
    Flagging is done using the cal_status flag ( AL52CO_cal_status) and the pressure measurement
    in the calibration chamber of the instrument (AL52CO_calpress).

    """

    def __init__(self,dataset):
        self.input_names=['AL52CO_conc', 'AL52CO_sens', 'AL52CO_zero', 'AL52CO_cellpress', 'AL52CO_calpress', 'AL52CO_cal_status', 'AL52CO_utc_time', 'AL52CO_counts', 'WOW_IND', 'HGT_RADR', 'CALCOMX']
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
        counts=self.dataset['AL52CO_counts'].data.ismatch(match)
        co_mr[counts == 0]=np.nan
        calpress=self.dataset['AL52CO_calpress'].data.ismatch(match)
        cal_status=self.dataset['AL52CO_cal_status'].data.ismatch(match)
        # cal_status is a character and needs to be converted to integer to do anything useful with it
        cal_status=np.int8(cal_status)
        sens=self.dataset['AL52CO_sens'].data.ismatch(match)
        sens[sens == 0.0]=np.nan
        #remove outliers: threshold is 25% difference from the overall median
        sens[np.abs(sens-np.nanmedian(sens)) > 0.25*np.nanmedian(sens)]=np.nan
        zero=self.dataset['AL52CO_zero'].data.ismatch(match)
        zero[zero == 0.0]=np.nan
        #remove outliers: threshold is 25% difference from the overall median
        zero[np.abs(zero-np.nanmedian(zero)) > 0.25*np.nanmedian(zero)]=np.nan
        utc_time=self.dataset['AL52CO_utc_time'].data.ismatch(match)
        wow_ind=self.dataset['WOW_IND'].data.ismatch(match)
        #
        # We calculate the raw counts from the CO concentration and the calibration coefficients.
        # The *AL52CO_counts variable can not be used* because it does not necessarily match the
        # concentration. The data stream that is sent by the labview script is produced several times
        # a second while only *one* value in the stream is updated at a time. For example see:
        #
        # $ cat AL52CO01_20140126_060103_B828.csv
        # ...
        # $AL52CO01,66,1390716327,0,0,B828,77.721321,8821.000000, ...
        # $AL52CO01,66,1390716327,0,0,B828,77.721321,8475.000000, ...
        # $AL52CO01,66,1390716328,0,0,B828,77.721321,8475.000000, ...
        # $AL52CO01,66,1390716328,0,0,B828,81.419006,8475.000000, ...
        # $AL52CO01,66,1390716328,0,0,B828,81.419006,8660.000000, ...
        # $AL52CO01,66,1390716329,0,0,B828,81.419006,8660.000000, ...
        # ...
        #
        # The concentration changes in the fourth line to 81.42 but the counts value is only updated
        # in the subsequent line.
        #
        # The scaling factor is needed to take care of revised CO calibration gas concentrations, which
        # FAAM learns only about post flight/campaign, when the calibration gas is reanalysed by an authority.
        if len(self.dataset['CALCOMX'].data) <= 3:
            scaling_factor=1.0
            sys.stdout.write('    Scaling factor not defined in flight-cst file.\n')
        else:
            scaling_factor=self.dataset['CALCOMX'][3]
            sys.stdout.write('    Scaling factor defined in flight-cst file.\n')
        sys.stdout.write('    Scaling factor set to %f.\n' % (scaling_factor))
        co_mr*=scaling_factor

        counts=co_mr/(1.0/sens)+zero
        # calc new interpolated calibration coefficients
        sens_new, zero_new=interpolate_cal_coefficients(utc_time, sens, zero)

        # recalculate the CO concentration using the interpolated calibration coefficients
        # zero_new and sens_new
        conc_new=(counts-zero_new)/sens_new

        # use both cal_status flag and pressure in calibration chamber for indexing calibration time periods
        cal_status_ix=np.where((cal_status == 1) | (calpress > 1.7))[0]
        # add time buffer to cal_status
        cal_status_buffer=8
        for i in range(cal_status_buffer*-1, cal_status_buffer+1):
            cal_status_ix=list(set(list(np.concatenate((np.array(cal_status_ix), np.array(cal_status_ix)+i)))))
        cal_status_ix=np.array(cal_status_ix)
        cal_status_ix=cal_status_ix[cal_status_ix < len(cal_status)]
        cal_status_ix=list(cal_status_ix)
        cal_status[cal_status_ix]=1
        flag=np.zeros(co_mr.size, dtype=np.int8)     # initialize flag array, with all values set to 0
        flag[co_mr<-10]=3                            # flag very negative co_mr as 3
        flag[cal_status==1]=3                        # flag data while calibration is running
        flag[calpress>3.4]=3                         # flag when calibration gas pressure is increased
        flag[counts==0]=3
        co_aero=flagged_data(conc_new, match, flag)

        # creating a plot which shows the "raw" time series and the one that uses
        # interpolated calibration coefficients
        try:
            create_plot(match, co_mr, conc_new, cal_status, self.dataset)
        except:
            pass
        self.outputs[0].data=co_aero

