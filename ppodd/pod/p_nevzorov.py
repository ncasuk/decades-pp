from ppodd.core import *
class nevzorov(cal_base):
    """ 
"""
    def __init__(self,dataset):
        self.input_names=[  'CORCON_nv_lwc_vcol',
                            'CORCON_nv_lwc_icol',
                            'CORCON_nv_lwc_vref',
                            'CORCON_nv_lwc_iref',
                            'CORCON_nv_twc_vcol',
                            'CORCON_nv_twc_icol',
                            'CORCON_nv_twc_vref',
                            'CORCON_nv_twc_iref',
                            'TAS']
        self.outputs=[parameter('NV_LWC_U',units='gram m-3',frequency=64,number=602,long_name='Uncorrected liquid water content from the Nevzorov probe')
                     ,parameter('NV_TWC_U',units='gram m-3',frequency=64,number=605,long_name='Uncorrected total condensed water content from the Nevzorov probe.')]

        self.version=1.00
        cal_base.__init__(self,dataset)
        
    def process(self):
        CALNVLWC=[0.7,0.3e-4,3.29E-4,-6.82E-2,3.29E-4,-6.82E-2,3.29E-4,-6.82E-2,3.29E-4,-6.82E-2]
        CALNVTWC=[1.65,0.5e-4,3.29E-4,-6.82E-2,3.29E-4,-6.82E-2,3.29E-4,-6.82E-2,3.29E-4,-6.82E-2]
        CALNVL=[2589.0]
        t=self.dataset.matchtimes(self.input_names)
        licol=self.dataset['CORCON_nv_lwc_icol'].ismatch(t)   
        lvcol=self.dataset['CORCON_nv_lwc_vcol'].ismatch(t)     
        liref=self.dataset['CORCON_nv_lwc_iref'].ismatch(t)     
        lvref=self.dataset['CORCON_nv_lwc_vref'].ismatch(t) 
        ticol=self.dataset['CORCON_nv_twc_icol'].ismatch(t)   
        tvcol=self.dataset['CORCON_nv_twc_vcol'].ismatch(t)     
        tiref=self.dataset['CORCON_nv_twc_iref'].ismatch(t)     
        tvref=self.dataset['CORCON_nv_twc_vref'].ismatch(t) 
        AREA_TWC=CALNVTWC[0]
        AREA_LWC=CALNVLWC[0]
        K_TWC=CALNVTWC[1]
        K_LWC=CALNVLWC[1]
        L_WC=CALNVL[0]
        #calibrate raw data
        lwc_col_i = CALNVLWC[2]*licol + CALNVLWC[3]
        lwc_col_v = CALNVLWC[4]*lvcol + CALNVLWC[5]
        lwc_ref_i = CALNVLWC[6]*liref + CALNVLWC[7]
        lwc_ref_v = CALNVLWC[8]*lvref + CALNVLWC[9]
        #Calculate derived parameters.
        #
        #Sensor power (J/s).
        lwc_col_p=lwc_col_i*lwc_col_v
        lwc_ref_p=lwc_ref_i*lwc_ref_v
        times=licol.times2d.ravel()
        sh=licol.shape
 
        tas=self.dataset['TAS'].ismatch(t).ravel()
        tas.interp1d()
        tas=tas.interpolated(times).reshape(sh)
        lwc_p=lwc_col_p-K_LWC*lwc_ref_p  
        lwc_q=lwc_p/(tas*AREA_LWC*L_WC)
        
        twc_col_i = CALNVTWC[2]*ticol + CALNVTWC[3]
        twc_col_v = CALNVTWC[4]*tvcol + CALNVTWC[5]
        twc_ref_i = CALNVTWC[6]*tiref + CALNVTWC[7]
        twc_ref_v = CALNVTWC[8]*tvref + CALNVTWC[9]
        #Calculate derived parameters.
        #
        #Sensor power (J/s).
        twc_col_p=twc_col_i*twc_col_v
        twc_ref_p=twc_ref_i*twc_ref_v
 
        twc_p=twc_col_p-K_LWC*twc_ref_p  
        twc_q=twc_p/(tas*AREA_TWC*L_WC)
        self.outputs[0].data=lwc_q
        self.outputs[1].data=twc_q
        


