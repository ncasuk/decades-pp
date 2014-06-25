from ppodd.core import *


def interpolate_cal_coefficients(sens, zero, cal_status, utc_time):
    """The calibration coefficients for the AL5002
    instrument drift linearly between calibrations. To take account of
    this new coefficients are calculated for every data point, which
    can be used to recalculate the CO concentrations.
    """
    n=sens.size
    sens_new, zero_new=np.zeros(n), np.zeros(n)
    ix=np.where(cal_status[1:]-cal_status[:-1] == -1)[0]
    ix=ix[ix>100]
    # the +20 is a dodgy way to make sure that the values have changed.
    # Apparently the zero and sens parameters do not change at
    # exactly the same time in the data stream
    ix=[10]+list(ix+20)+[n-1]
    for i in range(len(ix)-1):
        ix1=ix[i]
        ix2=ix[i+1]
        sens_new[ix1:ix2]=np.interp(utc_time[ix1:ix2], np.float32([utc_time[ix1], utc_time[ix2]]), [sens[ix1], sens[ix2]])
        zero_new[ix1:ix2]=np.interp(utc_time[ix1:ix2], np.float32([utc_time[ix1], utc_time[ix2]]), [zero[ix1], zero[ix2]])
    return (sens_new, zero_new)


class rio_co_mixingratio(cal_base):
    """Routine to calculate the Carbon Monoxide concentration from the AL52002 Instrument.

    The routine works with the data from the TCP packages that are stored by fish.
    Flagging is done using the static pressure and the pressure measurement in the
    calibration chamber of the instrument.

    Is the static pressure from the RVSM system lower than 500mb the data are unreliable
    and flagged 2. Is the pressure inside the calibration chamber greater than 3.1 bar
    the instrument is performing a calibration and data points are flagged 3.

    """

    def __init__(self,dataset):
        self.input_names=['AL52CO_conc', 'AL52CO_sens', 'AL52CO_zero', 'AL52CO_counts', 'AL52CO_cellpress', 'AL52CO_calpress', 'AL52CO_cal_status', 'AL52CO_utc_time']
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
        cal_status=np.int8(cal_status) # cal_status is a character and needs to be converted to integer to do anything useful with it
        sens=self.dataset['AL52CO_sens'].data.ismatch(match)
        zero=self.dataset['AL52CO_zero'].data.ismatch(match)
        utc_time=self.dataset['AL52CO_utc_time'].data.ismatch(match)
        counts=self.dataset['AL52CO_counts'].data.ismatch(match)

        sens_new, zero_new=interpolate_cal_coefficients(sens, zero, cal_status, utc_time)
        conc_new=(counts-zero_new)*1.0/sens_new

        flag=np.array([0]*co_mr.size, dtype=np.int8) # initialize empty flag array, with all flags set to 0
        flag[co_mr<-10]=3         # flag very negative co_mr as 3
        flag[cal_status==1]=3     # flag calibration data points
        flag[calpress>3.1]=3      # flag when calibration gas pressure is increased

        co_aero=flagged_data(conc_new, match, flag)

        self.outputs[0].data=co_aero
