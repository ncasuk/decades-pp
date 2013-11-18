#from ppodd.pod import *
from ppodd.core import *
from ppodd.humidity_formulae import *
class twc_wvsscor(cal_base):
    """ Fit TWC sensor to WVSSII
    
    Calculates Vapour pressures from WVSSII where less than a threshold ( to screen out liquid and ice )
    Calculate a theoretical Oxygen absorption from pressure
    Fit the TWC detector less the oxygen correction against the WVSSII vapour pressure


"""
    def __init__(self,dataset):
        self.wvss='WVSS2B_VMR'
        self.input_names=['PS_RVSM',self.wvss,'TWC_DET','TWC_TSAM']
        self.outputs=[constants_parameter('TWC_FIT',[])]
        self.version=1.00
        cal_base.__init__(self,dataset)
        
    def process(self):
        dx=self.dataset
        match=dx.matchtimes(self.input_names)
        p1=dx['PS_RVSM'].data.ismatch(match).get1Hz()
        p1f=p1.flag==0
        F=0.93
        t2=dx['TWC_TSAM'].data.ismatch(match)
        t2f=t2.flag==0
        v=dx['TWC_DET'].data.ismatch(match).get1Hz()
        vf=v.flag==0
        vmr=dx[self.wvss].data/1e6
        vmf=np.isfinite(vmr)
        vp1=np.array(vmr2vp(vmr,p1),dtype='f8')
        iuse=np.where(((vp1/svp)<0.7) & p1f & t2f & vf & vmf)[0] #& ((over) | (under)))[0]
        Kv=427.0
        p0=1013.2
        t0=273.15
        uO=0.2095
        KO=0.304+0.351*p1*F/p0
        if(len(iuse)>10):
            fit=np.polyfit(v[iuse],(vp1[iuse]/t2[iuse])+(KO[iuse]*uO*p1[iuse]/(Kv*t2[iuse])),1)
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

