from ppodd.core import *

from ppodd.pod.p_nevzorov import *

input_names=['CORCON_nv_lwc_vcol',
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

 
  
t=d.matchtimes(input_names)
insts=['twc','lwc']
measurements=['icol','vcol','iref','vref']
cal={}
times=d['CORCON_nv_lwc_vcol'].ismatch(t).times2d
sh=times.shape
nvl=d['CALNVL'][0]

tas=d['TAS'].ismatch(t).ravel()
tas.interp1d()
tas=tas.interpolated(times).reshape(sh)

ias=d['IAS_RVSM'].ismatch(t).ravel()
ias.interp1d()
ias=ias.interpolated(times).reshape(sh)

ps=d['PS_RVSM'].ismatch(t).ravel()
ps.interp1d()
ps=ps.interpolated(times).reshape(sh)

wow_flag=d['WOW_FLAG'].ismatch(t).ravel()
wow_flag.interp1d()
wow_flag=wow_flag.interpolated(times).reshape(sh)


for n,i in enumerate(insts):
    #For each instrument (i)
    area=d[('calnv%s' % i).upper()][1]
    K=d[('calnv%s' % i).upper()][0]
    for m in measurements:
        raw=d['CORCON_nv_%s_%s' % (i,m)].ismatch(t)
        cons=d[('c%s%s' % (i,m)).upper()]
        #Calibrate to volts or current
        cal['%s%s' % (i,m)]=(cons[0]+cons[1]*raw)*cons[2]
    #Sensor power (J/s).
    col_p=cal['%sicol' % i]*cal['%svcol' % i]  # V*I
    ref_p=cal['%siref' % i]*cal['%svref' % i]
    if i.lower() == 'twc':
        no_cloud_mask=get_no_cloud_mask(col_p, ias)
    K=get_fitted_k(col_p, ref_p, ias, ps, no_cloud_mask, K)
    p=col_p-K*ref_p
    if i == 'twc':
        nv_twc_u=p/(tas*area*nvl)
    elif i == 'lwc':
        nv_lwc_u=p/(tas*area*nvl)
    else:
        pass
      

