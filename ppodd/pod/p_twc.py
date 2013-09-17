from ppodd.pod import *
from ppodd.core import *
class twc(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALTNOS', 'CALTSAM', 'CALTAMB', 'CALTSRC', 'CALHTR1', 'CALHTR2', 'CALISRC', 
                          'SECS','Horace_TWCD', 'Horace_TNOS', 'Horace_TSAM', 'Horace_TAMB', 
                          'Horace_TSRC', 'Horace_HTR1', 'Horace_HTR2', 'Horace_ISRC', 'Horace_STAT',
                          'Horace_EV1C','Horace_EV2C']
        self.outputs=[parameter('TWC_DET',units='bits',frequency=64,number=664,long_name='Raw data from the TWC probe Lyman alpha detector')
                     ,parameter('TNOS_CAL',units='DEG K',frequency=1,number=665,long_name='TWC NOSE TEMP')
                     ,parameter('TWC_TSAM',units='K',frequency=1,number=666,long_name='Sample temperature in Kelvin from the TWC evaporator probe')
                     ,parameter('TAMB_CAL',units='DEG K',frequency=1,number=667,long_name='TWC AMBIENT TEMP')
                     ,parameter('TSRC_CAL',units='DEG K',frequency=1,number=668,long_name='TWC SOURCE TEMP')
                     ,parameter('HTR1_CAL',units='AMPS',frequency=1,number=669,long_name='TWC EVAP1 CURRENT')
                     ,parameter('HTR2_CAL',units='AMPS',frequency=1,number=670,long_name='TWC EVAP2 CURRENT')
                     ,parameter('ISRC_CAL',units='AMPS',frequency=1,number=671,long_name='TWC SOURCE CURRENT')
                     ,parameter('STAT_CAL',units='RBITS',frequency=1,number=672,long_name='TWC STATUS WORD')]
        #self.name='TWC'
        self.version=1.00
        fort_cal.__init__(self,dataset)
