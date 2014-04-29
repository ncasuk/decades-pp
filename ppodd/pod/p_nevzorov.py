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
                            'TAS',
                            'CLWCIREF','CLWCVREF','CLWCICOL','CLWCVCOL',
                            'CTWCIREF','CTWCVREF','CTWCICOL','CTWCVCOL',
                            'CALNVLWC',
                            'CALNVTWC',
                            'CALNVL']
        self.outputs=[parameter('NV_LWC_U',units='gram m-3',frequency=64,number=602,long_name='Uncorrected liquid water content from the Nevzorov probe')
                     ,parameter('NV_TWC_U',units='gram m-3',frequency=64,number=605,long_name='Uncorrected total condensed water content from the Nevzorov probe.')]

        self.version=1.00
        cal_base.__init__(self,dataset)
        
    def process(self):
        #CALNVLWC=[0.7,0.3e-4,3.29E-4,-6.82E-2,3.29E-4,-6.82E-2,3.29E-4,-6.82E-2,3.29E-4,-6.82E-2]
        #CALNVTWC=[1.65,0.5e-4,3.29E-4,-6.82E-2,3.29E-4,-6.82E-2,3.29E-4,-6.82E-2,3.29E-4,-6.82E-2]
        #CALNVL=[2589.0]
        t=self.dataset.matchtimes(self.input_names)
        insts=['lwc','twc']
        measurements=['icol','vcol','iref','vref']
        cal={}
        times=self.dataset['CORCON_nv_lwc_vcol'].ismatch(t).times2d
        sh=times.shape
        nvl=self.dataset['CALNVL'][0]
        tas=self.dataset['TAS'].ismatch(t).ravel()
        tas.interp1d()
        tas=tas.interpolated(times).reshape(sh)
        for n,i in enumerate(insts):
            #For each instrument (i)
            area=self.dataset[('calnv%s' % i).upper()][0]
            K=self.dataset[('calnv%s' % i).upper()][1]
            for m in measurements:
                raw=self.dataset['CORCON_nv_%s_%s' % (i,m)].ismatch(t)
                cons=self.dataset[('c%s%s' % (i,m)).upper()]
                #Calibrate to volts or current
                cal['%s%s' % (i,m)]=(cons[0]+cons[1]*raw)*cons[2]
            #Sensor power (J/s).
            col_p=cal['%sicol' % i]*cal['%svcol' % i]  # V*I
            ref_p=cal['%siref' % i]*cal['%svref' % i]
            p=col_p-K*ref_p
            self.outputs[n].data=p/(tas*area*nvl)

