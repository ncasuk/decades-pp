#from ppodd.pod import *
import ppodd
from ppodd.core import *
from ppodd.humidity_formulae import *
class calc_vp(cal_base):
    """
    
    
    """
    def __init__(self,dataset):
        self.input_names=['TDEW_GE']
        self.outputs=[parameter('GE_VP_W',units='hPa',frequency=4,long_name='General Eastern Vapour Pressure wrt water')
                     ,parameter('GE_VP_I',units='hPa',frequency=4,long_name='General Eastern Vapour Pressure wrt ice')
                     ,parameter('GE_VP',units='hPa',frequency=4,long_name='General Eastern Vapour Pressure')
                     ]
        self.version=1.00
        cal_base.__init__(self,dataset)
                
    def process(self):
        tdew=self.dataset['TDEW_GE'].data
        vpw=dp2vp(tdew)
        vpi=fp2vp(tdew).raw_data
        vp=vpw[:]
        vp[np.where(tdew<273)]=vpi[np.where(tdew<273)]
        vp.flag[(tdew<273) & (tdew>258) & (vp.flag==0)]=1
        self.outputs[0].data=vpw
        self.outputs[1].data=vpi
        self.outputs[2].data=vp

