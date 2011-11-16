from cal_base import *
from humidity_formulae import *
class c_twc_cor(cal_base):
    def __init__(self,dataset):
        self.input_names=['PS_RVSM','TAT_DI_R','TDEW_GE','TWC_DET','TWC_TSAM']
        self.outputs=[parameter('TWC_TDEW',units='K',frequency=64,number=725,description='Dew-point derived from TWC probe specific humidity (valid in cloud-free air)')
                     ,parameter('TWC_EVAP',units='gram kg-1',frequency=64,number=572,description='Total water specific humidity from the TWC avaporator instrument')]
        self.name='TWC_COR'
        self.version=1.00
        cal_base.__init__(self,dataset)
        
    def process(self):
        dx=self.dataset
        p1=dx.get_para('PS_RVSM')[::1]
        p1f=dx.get_para('PS_RVSM').data.flags[::1]==0
        t1=dx.get_para('TAT_DI_R')[::1]
        t1f=dx.get_para('TAT_DI_R').data.flags[::1]==0
        F=0.93
        p2=p1*F
        t2=dx.get_para('TWC_TSAM')[::1]
        t2f=dx.get_para('TWC_TSAM').data.flags[::1]==0
        v=4095.0-dx.get_para('TWC_DET')[::1]
        vf=dx.get_para('TWC_DET').data.flags[::1]==0
        ge=dx.get_para('TDEW_GE')[::1]
        gef=dx.get_para('TDEW_GE').data.flags[::1]==0
        over=ge>278.0
        under=ge<=273-15.0
        iunder=np.where(under)
        vp1=np.array(dp2vp(ge),dtype='f8')
        svp=dp2vp(t1,p1)
        iuse=np.where((vp1/svp<0.7) & p1f & t1f & t2f & vf & gef & ((over) | (under)))
        vp1[iunder]=fp2vp(ge[iunder])
        Kv=427.0
        p0=1013.2
        t0=273.15
        uO=0.2095
        KO=0.304+0.351*p2/p0
        fit=np.polyfit((F*t0)*(vp1[iuse]*Kv+KO[iuse]*uO*p1[iuse])/(p0*Kv*t2[iuse]),v[iuse],1)
        vt=fit[1]
        gx=fit[0]
        t64=dx.get_para('TWC_DET').data
        t64x=timestamp(t64.timestamp.basetime,times=t64.timestamp.times,frequency=64)
        sh=(len(t64.timestamp.basetime),64)
        tx=dx.get_para('TWC_TSAM').data
        tx.interp1d()
        t2=tx.interpolated(t64x)
        v=4095.0-dx.get_para('TWC_DET')[:]
        vf=dx.get_para('TWC_DET').data.flags[:]
        px=dx.get_para('PS_RVSM').data
        px.interp1d()
        p1=px.interpolated(t64x)
        p2=p1*F
        KO=0.304+0.351*p2/p0
        vpo=(t2*Kv*p0*(v-vt)-gx*p1*F*KO*t0*uO)/(gx*F*Kv*t0)
        vmro=vp2vmr(vpo,p1)
        mmr=vmr_mmr(vmro)
        dp=vp2dp(vpo)
        self.outputs[0].data=flagged_data(dp.reshape(sh),t64x,flags=vf.reshape(sh))
        self.outputs[1].data=flagged_data(mmr.reshape(sh),t64x,flags=vf.reshape(sh))
