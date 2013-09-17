from ppodd.pod import *
from ppodd.core import *
class rio_lwc(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALLWC', 'CORCON_jw_lwc', 'TAS_RVSM', 'TAT_DI_R', 'PS_RVSM']
        self.outputs=[parameter('LWC_JW_U',units='gram kg-1',frequency=4,number=535,long_name='Uncorrected liquid water content from the Johnson Williams instrument.')]
        #self.name='RIO_LWC'
        self.fortname='LWC'
        self.version=1.00
        fort_cal.__init__(self,dataset)
        
    def process(self): 
        self.dataset[self.input_names[1]].number=42
        fort_cal.process(self)
