from ppodd.pod import *
from ppodd.core import *
from ppodd.humidity_formulae import *
class twc_cor(cal_base):
    """
    Fit TWC sensor to GE chilled mirror
    
    Calculates Vapour pressures from GE where less than a threshold ( to screen out liquid and ice )
    Calculate a theoretical Oxygen absorption from pressure
    Fit the TWC detector less the oxygen correction against the GE vapour pressure
    
    Use the fit to create a Mass Mixing ratio for the TWC and then a dewpoint.
    
    """
    def __init__(self,dataset):
        self.input_names=['PS_RVSM','TAT_DI_R','TDEW_GE','TWC_DET','TWC_TSAM']
        self.outputs=[parameter('TWC_TDEWx',units='K',frequency=64,number=725,long_name='Dew-point derived from TWC probe specific humidity (valid in cloud-free air)')
                     ,parameter('TWC_EVAPx',units='gram kg-1',frequency=64,number=572,long_name='Total water specific humidity from the TWC avaporator instrument')]
        #self.name='TWC_COR'
        self.version=1.00
        cal_base.__init__(self,dataset)
        
    def process(self):
        dx=self.dataset
        match=dx.matchtimes(self.input_names)
        p1=dx['PS_RVSM'].data.ismatch(match)[:,0]
        p1f=p1.flag==0
        t1=dx['TAT_DI_R'].data.ismatch(match)[:,0]
        t1f=t1.flag==0
        F=0.93
        t2=dx['TWC_TSAM'].data.ismatch(match)[:]
        t2f=t2.flag==0
        v=dx['TWC_DET'].data.ismatch(match)[:,0]
        vf=v.flag==0
        ge=dx['TDEW_GE'].data.ismatch(match)[:,0]
        gef=ge.flag==0
        over=ge>278.0
        under=ge<=273-15.0
        iunder=np.where(under)
        vp1=np.array(dp2vp(ge),dtype='f8')
        svp=dp2vp(t1,p1)
        iuse=np.where(((vp1/svp)<0.7) & p1f & t1f & t2f & vf & gef)[0] #& ((over) | (under)))[0]
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
        #plt.plot(v,(vp1/t2)+(KO*uO*p1/(Kv*t2)),'x')
        #plt.plot(v[iuse],(vp1/t2)[iuse]+(KO*uO*p1/(Kv*t2))[iuse],'x')
        print 'TWC_COR ',len(iuse)
        if(len(iuse)>10):
            print iuse.shape
            fit=np.polyfit(v[iuse],(vp1[iuse]/t2[iuse])+(KO[iuse]*uO*p1[iuse]/(Kv*t2[iuse])),1)
            import matplotlib.pyplot as plt
            plt.plot(v,(vp1/t2)+(KO*uO*p1/(Kv*t2)),'x')
            plt.plot(v[iuse],(vp1/t2)[iuse]+(KO*uO*p1/(Kv*t2))[iuse],'x')
            ans=np.polyval(fit,t64)
            #plt.plot(t64.ravel(),ans.ravel())
            plt.plot(t64.ravel(),ans.ravel())
            plt.xlim(np.min(v[iuse]),np.max(v[iuse]))
            print plt.xlim()
            plt.ylim(np.polyval(fit,plt.xlim()))
            print plt.ylim()
            plt.ion()
            plt.title('TWC vs GE')
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
