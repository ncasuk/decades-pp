from ppodd.pod import *
from ppodd.core import *
class rio_heiman(fort_cal):
    def __init__(self,dataset):
        self.input_names=['PRTCCAL', 'HEIMCAL', 'SREG', 'CORCON_heim_t', 'CORCON_heim_c']
        self.outputs=[parameter('BTHEIM_U-rio',units='degK',frequency=4,number=537,long_name='Uncorrected brightness temperature from the Heimann radiometer')]
        self.fortname='HEIMAN'
        #self.name='RIO_HEIMAN'
        self.version=1.00
        fort_cal.__init__(self,dataset)
        
        
    def process(self):
        rawnumbers=[27,141,142]
        for i,n in enumerate(self.input_names[2:]):
            self.dataset[n].number=rawnumbers[i]
        fort_cal.process(self)
