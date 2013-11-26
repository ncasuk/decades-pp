#from ppodd.pod import *
import ppodd
from ppodd.core import *
from ppodd.humidity_formulae import *
class calc_svp(cal_base):
    """
    
    
    """
    def __init__(self,dataset):
        self.input_names=['TAT_DI_R']
        self.outputs=[parameter('SAT_VP_W',units='hPa',frequency=32,long_name='Saturated Vapour Pressure wrt water')
                     ,parameter('SAT_VP_I',units='hPa',frequency=32,long_name='Saturated Vapour Pressure wrt ice')
                     ]
        self.version=1.00
        cal_base.__init__(self,dataset)
                
    def process(self):
        self.outputs[0].data=dp2vp(self.dataset['TAT_DI_R'].data)
        self.outputs[1].data=fp2vp(self.dataset['TAT_DI_R'].data)
