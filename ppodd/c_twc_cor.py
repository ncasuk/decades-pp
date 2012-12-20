from cal_base import *
from humidity_formulae import *
class c_twc_cor(cal_base):
    def __init__(self,dataset):
        self.input_names=['PS_RVSM','TAT_DI_R','TDEW_GE','TWC_DET','TWC_TSAM']
        self.outputs=[parameter('TWC_TDEW',units='K',frequency=64,number=725,long_name='Dew-point derived from TWC probe specific humidity (valid in cloud-free air)')
                     ,parameter('TWC_EVAP',units='gram kg-1',frequency=64,number=572,long_name='Total water specific humidity from the TWC avaporator instrument')]
        self.name='TWC_COR'
        self.version=1.00
        cal_base.__init__(self,dataset)
        
    def process(self):
        dx=self.dataset
        p1=dx['PS_RVSM'][:,0]
        p1f=dx['PS_RVSM'].flag[:,0]==0
        t1=dx['TAT_DI_R'][:,0]
        t1f=dx['TAT_DI_R'].flag[:,0]==0
        F=0.93
        t2=dx['TWC_TSAM'][:]
        t2f=dx['TWC_TSAM'].flag[:]==0
        v=dx['TWC_DET'][:,0]
        vf=dx['TWC_DET'].flag[:,0]==0
        ge=dx['TDEW_GE'][:,0]
        gef=dx['TDEW_GE'].flag[:,0]==0
        over=ge>278.0
        under=ge<=273-15.0
        iunder=np.where(under)
        vp1=np.array(dp2vp(ge),dtype='f8')
        svp=dp2vp(t1,p1)
        iuse=np.where(((vp1/svp)<0.7) & p1f & t1f & t2f & vf & gef & ((over) | (under)))[0]
        vp1[iunder]=fp2vp(ge[iunder])
        Kv=427.0
        p0=1013.2
        t0=273.15
        uO=0.2095
        KO=0.304+0.351*p1*F/p0
        t64=dx['TWC_DET'].data.copy()
        t64x=t64.times2d.ravel()
        sh=t64.shape
        vf=t64.flag
        print 'TWC_COR ',len(iuse)
        if(len(iuse)>10):
            print iuse.shape
            fit=np.polyfit(v[iuse],(vp1[iuse]/t2[iuse])+(KO[iuse]*uO*p1[iuse]/(Kv*t2[iuse])),1)
            import matplotlib.pyplot as plt
            plt.plot(v,(vp1/t2)+(KO*uO*p1/(Kv*t2)),'x')
            ans=np.polyval(fit,t64)
            plt.plot(t64.ravel(),ans.ravel())
            plt.xlim(np.min(v[iuse]),np.max(v[iuse]))
            print plt.xlim()
            plt.ylim(np.polyval(fit,plt.xlim()))
            print plt.ylim()
            plt.show()
            px=dx['PS_RVSM'].data.ravel()
            px.interp1d()
            p1=px.interpolated(t64x).reshape(sh)
            tx=dx['TWC_TSAM'].data
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
