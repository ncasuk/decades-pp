from cal_base import *
class c_lwc(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALLWC', 'J/W', 'TAS_RVSM', 'TAT_DI_R', 'PS_RVSM']
        self.outputs=[parameter('LWC_JW_U',units='gram kg-1',frequency=4,number=535,description='Uncorrected liquid water content from the Johnson Williams instrument.')]
        self.name='LWC'
        self.version=1.00
        fort_cal.__init__(self,dataset)
