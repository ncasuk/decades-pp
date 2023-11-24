#from ppodd.pod import *
import ppodd
from ppodd.core import *
from ppodd.humidity_formulae import *
from ppodd.pod.p_rio_nevzorov import get_no_cloud_mask


class twc_fit_wvss(cal_base):
    """
    Fit TWC sensor to WVSS
    
    Calculates Vapour pressures from GE where less than a threshold ( to screen out liquid and ice )
    Calculate a theoretical Oxygen absorption from pressure
    Fit the TWC detector less the oxygen correction against the GE vapour pressure
    
    """
    def __init__(self,dataset):
        #self.input_names=['PS_RVSM','TAT_DI_R','TWC_DET','TWC_TSAM',
        #                  'WVSS2F_VMR','CORCON_nv_twc_vcol','CORCON_nv_twc_icol','WOW_IND',
        #                  'NV_TWC_P','NV_TWC_C']
        self.input_names=['PS_RVSM','TWC_DET','TWC_TSAM','VP_NO_CLOUD'] #,'TAT_DI_R']
        self.outputs=[constants_parameter('TWC_FIT_WVSSF',[])]
        
        self.version=1.00
        cal_base.__init__(self,dataset)
        
    def __repr__(self):
        return 'TWC fit to WVSS'
        
    def process(self):
        print("******************************TEST CLOUD FLAGGING TWC FIT*************************")
        d=self.dataset
        match=d.matchtimes(self.input_names)
        p1=d['PS_RVSM'].data.ismatch(match).get1Hz()
        p1f=p1.flag
        F=0.93
        
        #t1=d['TAT_DI_R'].data.ismatch(match).get1Hz()  #TEST SA version
        #t1f=t1.flag
        
        t2=d['TWC_TSAM'].data.ismatch(match)
        t2f=t2.flag
        v=d['TWC_DET'].data.ismatch(match).get1Hz()
        vf=v.flag
        vp1=d['VP_NO_CLOUD'].data.ismatch(match)
        cloud=vp1.flag

        cloud_flag=np.maximum.reduce([cloud,p1f,t2f,vf])
        iuse=cloud_flag == 0
        print("cloud_flag ",iuse.shape,cloud_flag)
        Kv=427.0
        p0=1013.2
        t0=273.15
        uO=0.2095
        KO=0.304+0.351*p1*F/p0
        R=287.05
        if(len(iuse)>10):
        
            fit=np.polyfit(v[iuse],(vp1[iuse]/t2[iuse])+(KO[iuse]*uO*p1[iuse]/(Kv*t2[iuse])),1)
            #fit=np.polyfit(v[iuse],(62200*vp1[iuse]/(R*t1[iuse]))+(62200*KO[iuse]*uO*p1[iuse]/(R*Kv*t1[iuse])),1) #TEST SA version
            
            print('WVSS FIT={}'.format(fit))
            self.outputs[0].data=list(fit)
            import matplotlib.pyplot as plt
            #plt.ion()
            plt.figure()
            plt.plot(v.times,v)
            plt.plot(v[iuse].times,v[iuse],'g')
            plt.figure()
            
            plt.plot(v,(vp1/t2)+(KO*uO*p1/(Kv*t2)),'bx', label='all data')
            plt.plot(v[iuse],(vp1/t2)[iuse]+(KO*uO*p1/(Kv*t2))[iuse],'g+', label='filtered (n=%i)' % (iuse.sum(),))
            #plt.plot(v,(62200*vp1/(R*t1))+(62200*KO*uO*p1/(R*Kv*t1)),'bx', label='all data')                                          #TEST SA version
            #plt.plot(v[iuse],(62200*vp1/(R*t1))[iuse]+(62200*KO*uO*p1/(R*Kv*t2))[iuse],'g+', label='filtered (n=%i)' % (iuse.sum(),))
            
            ans=np.polyval(fit,v)
            plt.plot(v,ans,'r')
            plt.grid()
            plt.legend(loc='upper left')
            plt.xlabel('TWC_DET (raw counts)')
            plt.ylabel('VP-WVSS2F (mb)')  
            plt.xlim(np.min(v[iuse]),np.max(v[iuse]))
            plt.ylim(np.polyval(fit,plt.xlim()))
            plt.title('TWC vs WVSSF (no cloud) fit=%f + %f*x' % (fit[1],fit[0]))

