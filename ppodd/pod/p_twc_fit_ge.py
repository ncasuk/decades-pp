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
        self.input_names=['PS_RVSM','TAT_DI_R','TWC_DET','TWC_TSAM','GE_VP','WOW_IND','NV_TWC_C']
        self.outputs=[constants_parameter('TWC_FIT_GE',[])]
        self.version=1.00
        self.save_figs=''
        cal_base.__init__(self,dataset)
        
    def __repr__(self):
        return 'TWC fit to GE'
        
    def process(self):
        print("******************************TEST TWC FIT*************************")
        d=self.dataset
        match=d.matchtimes(self.input_names)
        p1=d['PS_RVSM'].data.ismatch(match).get1Hz()
        p1f=p1.flag
        vp1=d['GE_VP'].data.ismatch(match).get1Hz()
        vp1f=vp1.flag

        wow = d['WOW_IND'].data.ismatch(match)
        wow=(wow != 0).astype('u1')
        
        nevw = d['NV_TWC_C'].ismatch(match)
        nevf=nevw.flag

        s=nevw.raw_data.max(axis=1)-nevw.raw_data.min(axis=1)
        cloud=(s>0.0077).astype('u8') # 0.007694213489802859 would be equivalent to p_rio_nevzorov.get_no_cloud_mask 0.1/(100*0.502e-4*2589) for 100m/s
    
        _buffer=3
        #cloud=np.max(np.vstack([np.roll(cloud,i) for i in range(-_buffer,_buffer+1)]),axis=0)
           
        ix = np.where(cloud)[0]
        cloud[np.clip(np.concatenate([ix+i for i in range(-_buffer,_buffer+1)]), 0, len(cloud)-1)]=1

        F=0.93
        t2=d['TWC_TSAM'].data.ismatch(match)
        t2f=t2.flag
        v=d['TWC_DET'].data.ismatch(match).get1Hz()
        vf=v.flag
        #iuse=np.where(((vp1/svp)<0.7) & p1f & svpf & t2f & vf & vp1f & (wow==0))[0]
        cloud_flag=np.maximum.reduce([cloud,p1f,vp1f,wow,t2f,vf])
        iuse= cloud_flag==0
        Kv=427.0
        p0=1013.2
        t0=273.15
        uO=0.2095
        KO=0.304+0.351*p1*F/p0
        if(len(np.where(iuse)[0])>10):
            fit=np.polyfit(v[iuse],(vp1[iuse]/t2[iuse])+(KO[iuse]*uO*p1[iuse]/(Kv*t2[iuse])),1)
            print('GE FIT={}'.format(fit))
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
            if(self.save_figs):
                plt.savefig(os.path.join(self.save_figs,"TWC_GE_{}_Fit.png".format(d['FLIGHT'].data)))
                plt.close()
            plt.figure()
            vmro = (ans*t2/p1 - KO*uO/Kv)*1e6
            wv=vp2vmr(vp1,p1)*1e6
            plt.plot(v.times,wv,label='GE VMR')
            wv[~iuse]=np.nan
            plt.plot(v.times,wv,label='GE VMR NO CLOUD')
            plt.plot(v.times,vmro,label='TWC VMR')            
            
            plt.ylabel('VMR ppm')
            plt.legend()
            if(self.save_figs):
                plt.savefig(os.path.join(self.save_figs,"TWC_GE_{}_Compare.png".format(d['FLIGHT'].data)))
                plt.close()
            

