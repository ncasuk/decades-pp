from cal_base import *
class c_twc(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALTNOS', 'CALTSAM', 'CALTAMB', 'CALTSRC', 'CALHTR1', 'CALHTR2', 'CALISRC', 'TWCD', 'TNOS', 'TSAM', 'TAMB', 'TSRC', 'HTR1', 'HTR2', 'ISRC', 'STAT']
        self.outputs=[parameter('TWC_DET',units='bits',frequency=64,number=664,description='Raw data from the TWC probe Lyman alpha detector')
                     ,parameter('TNOS_CAL',units='DEG K',frequency=1,number=665,description='TWC NOSE TEMP')
                     ,parameter('TWC_TSAM',units='K',frequency=1,number=666,description='Sample temperature in Kelvin from the TWC evaporator probe')
                     ,parameter('TAMB_CAL',units='DEG K',frequency=1,number=667,description='TWC AMBIENT TEMP')
                     ,parameter('TSRC_CAL',units='DEG K',frequency=1,number=668,description='TWC SOURCE TEMP')
                     ,parameter('HTR1_CAL',units='AMPS',frequency=1,number=669,description='TWC EVAP1 CURRENT')
                     ,parameter('HTR2_CAL',units='AMPS',frequency=1,number=670,description='TWC EVAP2 CURRENT')
                     ,parameter('ISRC_CAL',units='AMPS',frequency=1,number=671,description='TWC SOURCE CURRENT')
                     ,parameter('STAT_CAL',units='RBITS',frequency=1,number=672,description='TWC STATUS WORD')]
        self.name='TWC'
        self.version=1.00
        fort_cal.__init__(self,dataset)
