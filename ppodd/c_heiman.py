from ppodd.cal_base import *
class c_heiman(fort_cal):
    def __init__(self,dataset):
        self.input_names=['PRTCCAL', 'HEIMCAL', 'Horace_SREG', 'Horace_HEIM', 'Horace_HEIC']
        self.outputs=[parameter('BTHEIM_U',units='degK',frequency=4,number=537,long_name='Uncorrected brightness temperature from the Heimann radiometer')]
        self.name='HEIMAN'
        self.version=1.00
        fort_cal.__init__(self,dataset)
