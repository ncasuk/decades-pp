#from ppodd.pod import *
import ppodd
from ppodd.core import *
from ppodd.humidity_formulae import *
from ppodd.pod.p_rio_nevzorov import get_no_cloud_mask


class twc_fit_ge(cal_base):
    """
    Fit TWC sensor to GE chilled mirror
    
    Calculates Vapour pressures from GE where less than a threshold ( to screen out liquid and ice )
    Calculate a theoretical Oxygen absorption from pressure
    Fit the TWC detector less the oxygen correction against the GE vapour pressure
    
    """
    def __init__(self,dataset):
        self.input_names=['PS_RVSM','TAT_DI_R','TWC_DET','TWC_TSAM','GE_VP','SAT_VP_W','WOW_IND','NV_TWC_P']
        self.outputs=[constants_parameter('TWC_FIT_GE',[])]
        self.version=1.00
        cal_base.__init__(self,dataset)
        
    def __repr__(self):
        return 'TWC fit to GE'
        
    def process(self):
        print("******************************TEST TWC FIT*************************")
        d=self.dataset
        match=d.matchtimes(self.input_names)
        p1=d['PS_RVSM'].data.ismatch(match).get1Hz()
        p1f=p1.flag==0
        F=0.93
        t2=d['TWC_TSAM'].data.ismatch(match)
        t2f=t2.flag==0
        v=d['TWC_DET'].data.ismatch(match).get1Hz()
        vf=v.flag==0
        vp1=d['GE_VP'].data.ismatch(match).get1Hz()
        vp1f=vp1.flag==0
        svp=d['SAT_VP_W'].data.ismatch(match).get1Hz()
        svpf=svp.flag==0
        nvtimes=d['CORCON_nv_twc_vcol'].ismatch(match).times2d
        nvshape=nvtimes.shape
        wow = d['WOW_IND'].data.ismatch(match)
        wow_int=wow.interp(times=nvtimes).reshape(nvshape)
        nevp = d['NV_TWC_P'].ismatch(match)
        no_cloud_mask=get_no_cloud_mask(nevp,wow_int)
        #iuse=np.where(((vp1/svp)<0.7) & p1f & svpf & t2f & vf & vp1f & (wow==0))[0]
        iuse=np.where(no_cloud_mask & p1f & t2f & vf & (wow==0))[0]
        Kv=427.0
        p0=1013.2
        t0=273.15
        uO=0.2095
        KO=0.304+0.351*p1*F/p0
        if(len(iuse)>10):
            fit=np.polyfit(v[iuse],(vp1[iuse]/t2[iuse])+(KO[iuse]*uO*p1[iuse]/(Kv*t2[iuse])),1)
            print('FIT={}'.format(fit))
            self.outputs[0].data=list(fit)
            import matplotlib.pyplot as plt
            #plt.ion()
            plt.figure()
            plt.plot(v,(vp1/t2)+(KO*uO*p1/(Kv*t2)),'bx', label='all data')
            plt.plot(v[iuse],(vp1/t2)[iuse]+(KO*uO*p1/(Kv*t2))[iuse],'g+', label='filtered (n=%i)' % (iuse.size,))
            ans=np.polyval(fit,v)
            plt.plot(v,ans,'r')
            plt.grid()
            plt.legend(loc='upper left')
            plt.xlabel('TWC_DET (raw counts)')
            plt.ylabel('VP-GE (mb)')            
            plt.xlim(np.min(v[iuse]),np.max(v[iuse]))
            plt.ylim(np.polyval(fit,plt.xlim()))
            plt.title('TWC vs GE (no cloud) fit=%f + %f*x' % (fit[1],fit[0]))
            

