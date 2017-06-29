#from ppodd.pod import *
import ppodd
from ppodd.core import *
from ppodd.humidity_formulae import *

class twc_fit_wvss(cal_base):
    """
    Fit TWC sensor to WVSS
    
    Calculates Vapour pressures from GE where less than a threshold ( to screen out liquid and ice )
    Calculate a theoretical Oxygen absorption from pressure
    Fit the TWC detector less the oxygen correction against the GE vapour pressure
    
    """
    def __init__(self,dataset):
        self.input_names=['PS_RVSM','TAT_DI_R','TWC_DET','TWC_TSAM','WVSS2F_VMR','SAT_VP_W']
        self.outputs=[constants_parameter('TWC_FIT_WVSSF',[])]
        self.version=1.00
        cal_base.__init__(self,dataset)
        
    def __repr__(self):
        return 'TWC fit to WVSS'
        
    def process(self):
        print("******************************TEST WVSS2 TWC FIT*************************")
        d=self.dataset
        match=d.matchtimes(self.input_names)
        p1=d['PS_RVSM'].data.ismatch(match).get1Hz()
        p1f=p1.flag==0
        F=0.93
        t2=d['TWC_TSAM'].data.ismatch(match)
        t2f=t2.flag==0
        v=d['TWC_DET'].data.ismatch(match).get1Hz()
        vf=v.flag==0
        wv=d['WVSS2F_VMR'].data.ismatch(match).get1Hz()/1e6
        svp=d['SAT_VP_W'].data.ismatch(match).get1Hz()
        svpf=svp.flag==0
        vp1=vmr2vp(wv,p1)
        iuse=np.where(((vp1/svp)<0.7) & p1f & svpf & t2f & vf)[0]
        Kv=427.0
        p0=1013.2
        t0=273.15
        uO=0.2095
        KO=0.304+0.351*p1*F/p0
        if(len(iuse)>10):
            fit=np.polyfit(v[iuse],(vp1[iuse]/t2[iuse])+(KO[iuse]*uO*p1[iuse]/(Kv*t2[iuse])),1)
            print('WVSS FIT={}'.format(fit))
            self.outputs[0].data=list(fit)
            import matplotlib.pyplot as plt
            plt.ion()
            plt.figure()
            plt.plot(v,(vp1/t2)+(KO*uO*p1/(Kv*t2)),'bx', label='all data')
            plt.plot(v[iuse],(vp1/t2)[iuse]+(KO*uO*p1/(Kv*t2))[iuse],'g+', label='filtered (n=%i)' % (iuse.size,))
            ans=np.polyval(fit,v)
            plt.plot(v,ans,'r')
            plt.grid()
            plt.legend(loc='upper left')
            plt.xlabel('TWC_DET (raw counts)')
            plt.ylabel('VP-WVSS2F (mb)')  
            plt.xlim(np.min(v[iuse]),np.max(v[iuse]))
            plt.ylim(np.polyval(fit,plt.xlim()))
            plt.title('TWC vs WVSSF fit=%f + %f*x' % (fit[1],fit[0]))

