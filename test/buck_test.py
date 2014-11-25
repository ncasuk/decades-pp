from ppodd.pod.p_rio_buck import *


input_names=['AERACK_buck_ppm',
             'AERACK_buck_mirr_temp',
             'AERACK_buck_pressure',
             'AERACK_buck_coldfinger_temp',
             'AERACK_buck_board_temp',
             'AERACK_buck_mirr_cln_flag',
             'AERACK_buck_balance',
             'AERACK_buck_dewpoint_flag',
             'AERACK_buck_pwm']

match=d.matchtimes(input_names)

buck_mirr_temp=d['AERACK_buck_mirr_temp'].ismatch(match)
buck_mirr_temp+=273.15
buck_pressure=d['AERACK_buck_pressure'].ismatch(match)
buck_mirr_ctrl_flag=d['AERACK_buck_mirr_cln_flag'].ismatch(match)
buck_mirr_flag=d['AERACK_buck_dewpoint_flag'].ismatch(match)
buck_status=d['AERACK_buck_pwm'].ismatch(match)

buck_mirror_control=get_buck_mirror_ctl(buck_mirr_temp)
vp_buck=calc_vp(buck_mirr_temp, buck_mirr_ctrl_flag)
buck_unc_k=calc_uncertainty(buck_mirr_temp, buck_pressure, buck_mirr_ctrl_flag)
vp_max=calc_vp(buck_mirr_temp, buck_mirr_ctrl_flag, buck_unc_k=buck_unc_k)
enhance=calc_enhance_factor(vp_buck, buck_mirr_temp, buck_pressure, buck_mirr_ctrl_flag)
vmr_buck=calc_vmr(vp_buck, enhance, buck_pressure)
vmr_max=calc_vmr(vp_max, enhance, buck_pressure)
vmr_unc=vmr_max-vmr_buck

result=c[:,0]/(buck_mirr_temp+buck_unc_k)**2+\
                  c[:,1]/(buck_mirr_temp+buck_unc_k)+\
                  c[:,2]+\
                  c[:,3]*(buck_mirr_temp+buck_unc_k)+\
                  c[:,4]*(buck_mirr_temp+buck_unc_k)**2+\
                  c[:,5]*(buck_mirr_temp+buck_unc_k)**3+\
                  c[:,6]*(buck_mirr_temp+buck_unc_k)**4+\
                  c[:,7]*(np.log(buck_mirr_temp+buck_unc_k))
