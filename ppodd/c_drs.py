from ppodd.cal_base import *
class c_drs(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALCABT', 'Horace_GMTH', 'Horace_GMTM', 'Horace_EVM', 'Horace_SREG', 'Horace_CABT']
        self.outputs=[parameter('SECS',units='SECS',frequency=1,number=515,long_name='SECS FROM MIDNIGHT')
                     ,parameter('SREG_CAL',units='IBITS',frequency=2,number=641,long_name='SIGNAL REGISTER')
                     ,parameter('EVM_CAL',units='RNUM',frequency=2,number=644,long_name='EVENT MARK')
                     ,parameter('CAB_TEMP',units='degC',frequency=1,number=660,long_name='Cabin temperature at the core consoles')]
        self.name='DRS'
        self.version=1.00
        fort_cal.__init__(self,dataset)
