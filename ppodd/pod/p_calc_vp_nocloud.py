#from ppodd.pod import *
import ppodd
from ppodd.core import *
from ppodd.humidity_formulae import *


class calc_vp_nocloud(cal_base):
    """
    
    
    """
    def __init__(self,dataset):
        self.input_names=['WVSS2F_VMR','PS_RVSM','NV_TWC_C','WOW_IND']
        self.outputs=[parameter('VP_NO_CLOUD',
                                  units='hPa',
                                  frequency=1,
                                  long_name='Vapour pressure from WVSS with cloud flag derived from Nevzorov')
                     ]
        self.version=1.00
        cal_base.__init__(self,dataset)
                
    def process(self):
        d=self.dataset
        match=d.matchtimes(self.input_names)
        p1=d['PS_RVSM'].data.ismatch(match).get1Hz()
        p1f=p1.flag
        wv=d['WVSS2F_VMR'].data.ismatch(match).get1Hz()/1e6
        wvf=(wv.flag>1).astype('u1')

        wow = d['WOW_IND'].data.ismatch(match)
        wow=(wow != 0).astype('u1')
        
        nevw = d['NV_TWC_C'].ismatch(match)
        nevf=nevw.flag
        vp1=vmr2vp(wv,p1)
        s=nevw.raw_data.max(axis=1)-nevw.raw_data.min(axis=1)
        cloud=(s>0.0077).astype('u8') # 0.007694213489802859 would be equivalent to p_rio_nevzorov.get_no_cloud_mask 0.1/(100*0.502e-4*2589) for 100m/s
    
        _buffer=3
        #cloud=np.max(np.vstack([np.roll(cloud,i) for i in range(-_buffer,_buffer+1)]),axis=0)
           
        ix = np.where(cloud)[0]
        cloud[np.clip(np.concatenate([ix+i for i in range(-_buffer,_buffer+1)]), 0, len(cloud)-1)]=1

        cloud_flag=np.maximum.reduce([cloud,p1f,wvf,wow])
        self.outputs[0].data=ppodd.core.flagged_data(vp1,cloud_flag)

