#from ppodd.pod import *
import ppodd
from ppodd.core import *
from ppodd.humidity_formulae import *
class twc_calc(cal_base):
    """
    
    Use the fit to create a Mass Mixing ratio for the TWC and then a dewpoint.
    
    """
    def __init__(self,dataset):
        self.fit='TWC_FIT_GE'
        self.input_names=[self.fit,'PS_RVSM','TWC_DET','TWC_TSAM']
        self.outputs=[parameter('TWC_TDEW',units='K',frequency=64,number=725,long_name='Dew-point derived from TWC probe specific humidity (valid in cloud-free air)')
                     ,parameter('TWC_EVAP',units='gram kg-1',frequency=64,number=572,long_name='Total water specific humidity from the TWC avaporator instrument')]
        self.version=1.00
        cal_base.__init__(self,dataset)
        
    def __repr__(self):
        return 'Apply TWC fit '+self.fit
        
    def process(self):
        d=self.dataset
        match=d.matchtimes(self.input_names[1:])
        t64=d['TWC_DET'].data.copy()
        t64x=t64.times2d.ravel()
        sh=t64.shape
        vf=t64.flag
        F=0.93       
        Kv=427.0
        p0=1013.2
        t0=273.15
        uO=0.2095
        if(len(d[self.fit].data)==2):
            fit=np.array(d[self.fit].data)
            print 'Applying FIT=',fit
            ans=np.polyval(fit,t64)
            px=d['PS_RVSM'].data.ravel()
            px.interp1d()
            p1=px.interpolated(t64x).reshape(sh)
            tx=d['TWC_TSAM'].data
            tx.interp1d()
            t2=tx.interpolated(t64x).reshape(sh)
            KO=0.304+0.351*p1*F/p0
            vpo=(ans-(KO*uO*p1/(Kv*t2)))*t2
            vmro=vp2vmr(vpo,p1)
            mmr=vmr_mmr(vmro)
            dp=vp2dp(vpo.ravel()).reshape(sh)
        else:
            dp=np.zeros(sh)
            mmr=t64
            vf[:]=3
        self.outputs[0].data=flagged_data(dp,t64.times,flags=vf)
        self.outputs[1].data=flagged_data(mmr,t64.times,flags=vf)
