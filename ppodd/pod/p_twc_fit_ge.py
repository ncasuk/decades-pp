#from ppodd.pod import *
import ppodd
from ppodd.core import *
from ppodd.humidity_formulae import *
class twc_fit_ge(cal_base):
    """
    Fit TWC sensor to GE chilled mirror
    
    Calculates Vapour pressures from GE where less than a threshold ( to screen out liquid and ice )
    Calculate a theoretical Oxygen absorption from pressure
    Fit the TWC detector less the oxygen correction against the GE vapour pressure
    
    """
    def __init__(self,dataset):
        self.input_names=['PS_RVSM','TAT_DI_R','TDEW_GE','TWC_DET','TWC_TSAM']
        #self.outputs=[parameter('TWC_TDEWx',units='K',frequency=64,number=725,long_name='Dew-point derived from TWC probe specific humidity (valid in cloud-free air)')
        #             ,parameter('TWC_EVAPx',units='gram kg-1',frequency=64,number=572,long_name='Total water specific humidity from the TWC avaporator instrument')]
        self.outputs=[constants_parameter('TWC_FIT',[])]
        #self.name='TWC_COR'
        self.version=1.00
        cal_base.__init__(self,dataset)
        
    def __repr__(self):
        return 'TWC fit to GE'
        
    def process(self):
        dx=self.dataset
        match=dx.matchtimes(self.input_names)
        p1=dx['PS_RVSM'].data.ismatch(match).get1Hz()
        p1f=p1.flag==0
        t1=dx['TAT_DI_R'].data.ismatch(match).get1Hz()
        t1f=t1.flag==0
        F=0.93
        t2=dx['TWC_TSAM'].data.ismatch(match)[:]
        t2f=t2.flag==0
        v=dx['TWC_DET'].data.ismatch(match).get1Hz()
        vf=v.flag==0
        ge=dx['TDEW_GE'].data.ismatch(match).get1Hz()
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
        if(len(iuse)>10):
            fit=np.polyfit(v[iuse],(vp1[iuse]/t2[iuse])+(KO[iuse]*uO*p1[iuse]/(Kv*t2[iuse])),1)
            print fit
            self.outputs[0].data=list(fit)
            import matplotlib.pyplot as plt
            plt.plot(v,(vp1/t2)+(KO*uO*p1/(Kv*t2)),'x')
            plt.plot(v[iuse],(vp1/t2)[iuse]+(KO*uO*p1/(Kv*t2))[iuse],'x')
            ans=np.polyval(fit,v)
            plt.plot(v,ans)
            plt.xlim(np.min(v[iuse]),np.max(v[iuse]))
            plt.ylim(np.polyval(fit,plt.xlim()))
            plt.ion()
            plt.title('TWC vs GE fit=%f + %f*x' % (fit[1],fit[0]))

