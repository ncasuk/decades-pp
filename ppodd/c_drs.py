from cal_base import *
class c_drs(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALCABT', 'GMTH', 'GMTM', 'EVM', 'SREG', 'CABT']
        self.outputs=[parameter('SECS',units='SECS',frequency=1,number=515,description='SECS FROM MIDNIGHT')
                     ,parameter('SREG_CAL',units='IBITS',frequency=2,number=641,description='SIGNAL REGISTER')
                     ,parameter('EVM_CAL',units='RNUM',frequency=2,number=644,description='EVENT MARK')
                     ,parameter('CAB_TEMP',units='degC',frequency=1,number=660,description='Cabin temperature at the core consoles')]
        self.name='DRS'
        self.version=1.00
        fort_cal.__init__(self,dataset)
