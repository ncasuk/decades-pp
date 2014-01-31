
from ppodd.core import *

class co_mixingratio(cal_base):
    """Routine to calculate the Carbon Monoxide concentration from the AL52002 Instrument.

    The routine works with the data from the TCP packages that are stored by fish.
    Flagging is done using the static pressure and the pressure measurement in the
    calibration chamber of the instrument.

    Is the static pressure from the RVSM system lower than 500mb the data are unreliable
    and flagged 2. Is the pressure inside the calibration chamber greater than 3.1 bar
    the instrument is performing a calibration and data are flagged 3.

    The routine needs an interpolation method to calculate the change of calibration coefficients
    inbetween calibrations. This will be written after the CAST campaign and applied to all future
    data.

    """
    def __init__(self,dataset):
        self.input_names=['AL52CO_conc', 'AL52CO_cellpress', 'AL52CO_calpress', 'PS_RVSM']
        self.outputs=[parameter('CO_AERO',
                                units='ppb',
                                frequency=1,
                                long_name='Mole fraction of Carbon Monoxide in air from the AERO AL5002 instrument')]
        self.version=1.00
        cal_base.__init__(self,dataset) 

    def process(self):
        d=self.dataset
        match=d.matchtimes(['PS_RVSM', 'AL52CO_conc', 'AL52CO_calpress'])
        co_conc=d['AL52CO_conc'].data.ismatch(match)
        calpress=d['AL52CO_calpress'].data.ismatch(match)
        sp=d['PS_RVSM'].data.ismatch(match)
        print(sp.shape)
        flag=co_conc*0
        co_mr=flagged_data(co_conc, co_conc.times, flag) 
        co_mr.flag[co_conc < -10] = 3
        co_mr.flag[sp[:,0] < 500] = 2
        co_mr.flag[calpress > 3.1] = 3
        self.outputs[0].data=co_mr
