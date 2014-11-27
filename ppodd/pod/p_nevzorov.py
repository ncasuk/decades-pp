import sys
from ppodd.core import *

import numpy as np

def get_no_cloud_mask(twc_col_p, wow):
    """Create a mask where times in cloud are indicated by zero
    and time outside of cloud (*no* total water) are indicated by a
    one.
    The way we determine whether cloud or not is present is looking at the
    range (max-min) of the power reading of the total water collector. The
    variance in a cloud should be much higher than outside.

    """
    rng_limits=(0.01, 0.1)
    mask=np.zeros(twc_col_p.shape[0], dtype=np.int8)

    rng=np.max(twc_col_p, axis=1)-np.min(twc_col_p, axis=1)
    mask[(rng > rng_limits[0]) & (rng < rng_limits[1]) & (np.max(wow, axis=1) == 0)]=1
    #add two seconds buffer, so that all data two seconds before and after
    #are flagged as in clouds
    mask=np.min(np.vstack([mask,
			   np.roll(mask, -2),
			   np.roll(mask, -1),
			   np.roll(mask, 1),
			   np.roll(mask, 2)]), axis=0)
    return mask



def get_fitted_k(col_p, ref_p, ias, ps, no_cloud_mask, k):
    """The Nevzorov baseline is not constant, but varies as a function of
    indicated air speed (IAS_RVSM) and static air pressure (PS_RVSM).
    Abel et al. (2014) provide a fitting formula in the Appendix A to correct
    the K value (ratio between collector and reference power, when outside of
    clouds) to remove the zero offset of the liquid and total water measurements.

    Reference:
      S J Abel, R J Cotton, P A Barrett and A K Vance. A comparison of ice water
      content measurement techniques on the FAAM BAe-146 aircraft.
      Atmospheric Measurement Techniques 7(5):4815-4857, 2014.

    """
    from scipy.optimize import curve_fit

    def func(x, a, b):
        return x[0,:]/x[1,:]-k-(a*(1.0/x[2,:])+b*np.log10(x[3,:]))
    ix=np.where(no_cloud_mask == 1)[0]
    xdata=np.vstack([col_p[ix,:].ravel(),
		     ref_p[ix,:].ravel(),
		     ias[ix,:].ravel(),
		     ps[ix,:].ravel()])
    popt, pcov=curve_fit(func, xdata, xdata[0,:]*0.0)
    return (k+(popt[0]*(1.0/ias)+popt[1]*np.log10(ps)), popt)


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
                            'IAS_RVSM',
                            'PS_RVSM',
                            'WOW_FLAG',
                            'CLWCIREF','CLWCVREF','CLWCICOL','CLWCVCOL',
                            'CTWCIREF','CTWCVREF','CTWCICOL','CTWCVCOL',
                            'CALNVLWC',
                            'CALNVTWC',
                            'CALNVL']
        self.outputs=[parameter('NV_TWC_U',units='gram m-3',frequency=64,number=605,long_name='Uncorrected total condensed water content from the Nevzorov probe.'),
		      parameter('NV_LWC_U',units='gram m-3',frequency=64,number=602,long_name='Uncorrected liquid water content from the Nevzorov probe')]

        self.version=1.00
        cal_base.__init__(self,dataset)

    def process(self):
        #suppress divide by zero messages
        np.seterr(divide='ignore')
        t=self.dataset.matchtimes(self.input_names)
        insts=['twc','lwc']
        measurements=['icol','vcol','iref','vref']
        cal={}
        times=self.dataset['CORCON_nv_lwc_vcol'].ismatch(t).times2d
        sh=times.shape
        nvl=self.dataset['CALNVL'][0]

        tas=self.dataset['TAS'].ismatch(t).ravel()
        tas.interp1d()
        tas=tas.interpolated(times).reshape(sh)

        ias=self.dataset['IAS_RVSM'].ismatch(t).ravel()
        ias.interp1d()
        ias=ias.interpolated(times).reshape(sh)

        ps=self.dataset['PS_RVSM'].ismatch(t).ravel()
        ps.interp1d()
        ps=ps.interpolated(times).reshape(sh)

        wow_flag=self.dataset['WOW_FLAG'].ismatch(t).ravel()
        wow_flag.interp1d()
        wow_flag=wow_flag.interpolated(times).reshape(sh)

        for n,i in enumerate(insts):
            #For each instrument (i)
            area=self.dataset[('calnv%s' % i).upper()][1]
            K=self.dataset[('calnv%s' % i).upper()][0]
            for m in measurements:
                raw=self.dataset['CORCON_nv_%s_%s' % (i,m)].ismatch(t)
                cons=self.dataset[('c%s%s' % (i,m)).upper()]
                #Calibrate to volts or current
                cal['%s%s' % (i,m)]=(cons[0]+cons[1]*raw)*cons[2]
            #Sensor power (J/s).
            col_p=cal['%sicol' % i]*cal['%svcol' % i]  # V*I
            ref_p=cal['%siref' % i]*cal['%svref' % i]
            if i.lower() == 'twc':
                no_cloud_mask=get_no_cloud_mask(col_p, wow_flag)
            try:
                K, params=get_fitted_k(col_p, ref_p, ias, ps, no_cloud_mask, K)
                sys.stdout.write('Nevzorov %s baseline fitted ...\n   a_ias: %.2f\n   a_p: %.2f\n' % (i.upper(),params[0], params[1]))
            except:
                pass
            p=col_p-K*ref_p
            flag=np.zeros(sh, dtype=np.int8)
            flag[wow_flag != 0]=3
            self.outputs[n].data=flagged_data(p/(tas*area*nvl), times[:,0], flag)
