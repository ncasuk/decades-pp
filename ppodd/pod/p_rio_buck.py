from ppodd.core import *

import numpy as np
from scipy.optimize import fsolve

def calc_uncertainty(buck_mirr_temp, buck_pressure, buck_mirr_control):

    n=buck_mirr_temp.size
    buck_unc_c=np.zeros(n)
    buck_unc_r=np.zeros(n)
    buck_unc_t=np.zeros(n)
    buck_unc_i=np.zeros(n)
    buck_unc_b=np.zeros(n)
    buck_unc_k=np.zeros(n)


    buck_unc_temp=np.zeros(n)*np.nan

    #buck_mirr_t = data.mirr_t
    #buck_pressure = data.pressure
    #buck_mirror_control = data.mirr_ctl

    for i in range(0, n):
        # Calibration Uncertainty
        Uc=0.02+5E+27*buck_mirr_temp[i]**(-12.5)
        buck_unc_c[i]=Uc

        # Repeatability
        Ur=0.01+4E+19*buck_mirr_temp[i]**(-9.0)
        buck_unc_r[i]=Ur

        if buck_mirr_temp[i] > 248.0:
            lag=8            
        else:                # lag = ceil(((2 * (-0.36296)) * Buck_Mirr_T[i]) + 105.21)
            lag=np.ceil(2e+29*buck_mirr_temp[i]**(-11.902))
        
        if not np.isfinite(lag):
            lag=8
        #Bug 2 found
        #if ((i > lag) & (i < (n-lag))):
        #    fwdUt=np.std(buck_mirr_temp[i:i+lag])
        #    backUt=np.std(buck_mirr_temp[i-lag:i])
        #else:
        #    fwdUt=0
        #    backUt=0

        if lag != np.nan:
            #Change to cope with nans            
            fwdUt=np.nanstd(buck_mirr_temp[i:i+lag])
            backUt=np.nanstd(buck_mirr_temp[i-lag:i])
        else:
            fwdUt, backUt = 0,0

        if (buck_pressure[i] > 0.0):
            Ut=np.max([fwdUt, backUt])
            #3 Bug found
            buck_unc_t[i]=Ut
            buck_unc_temp[i]=Ut
        else:
            Ut = 0.0
        """do you need to change this to buck_mirr_t? """
        # Interpolation uncertainty from NPL cal points to applied distribution
        if(buck_mirr_temp[i] > 233.15):
            Ui=0.025
        else:
            Ui=-0.0044*buck_mirr_temp[i]+1.051

        buck_unc_i[i]=Ui

        # Bias uncertainty depending on knowledge of mirror state
        if (buck_mirr_control[i] < 2):
            Ub=0
            buck_unc_b[i]=0

        if (buck_mirr_control[i] == 2):
            # dew_frost_diff(c,Buck_mirror_control,Buck_Mirr_T,Buck_Pressure,Ub)
            lnesw=np.log(611.2)+(17.62*(buck_mirr_temp[i]-273.15))/(243.12+buck_mirr_temp[i]-273.15)
            dpi=273.15+(272.0*(lnesw-np.log(611.2))/(22.46-(lnesw-np.log(611.2))))
            buck_unc_b[i]=dpi-buck_mirr_temp[i]
            Ub=dpi-buck_mirr_temp[i]

        Uc=buck_unc_c[i]
        Ur=buck_unc_r[i]
        Ut=buck_unc_t[i]
        Ui=buck_unc_i[i]
        Ub=buck_unc_b[i]
        Uk=buck_unc_k[i]
        
        buck_unc_k[i]=2.0*np.sqrt(Uc**2+Ur**2+Ut**2+Ui**2+Ub**2)

    ix=np.where(buck_mirr_control[i] == 3)[0]
    buck_unc_k[ix]=np.nan

    #Buck_Unc_K[c]=2*sqrt(Buck_Unc_c[c]^2+Buck_Unc_r[c]^2+Buck_Unc_t[c]^2+Buck_Unc_i[c]^2+Buck_Unc_b[c]^2)+10*Buck_Mirror_Control[c] //root sum square. Coverage factor of 2
    return buck_unc_k


def get_buck_mirror_ctl(buck_mirr_temp):
    """Routine that uses temperature change (DT) to devise flagging
for Mirror Temperature measurement based on whether
in control, ice or water layer (or unknown) on the mirror.

    buck_mirr_temp: Temperature of the BUCK mirror in Kelvin
    """

    c=0
    d=buck_mirr_temp.size
    interval=30
    culm1=0
    #culm2=0
    e=0
    recovery=0
    mirrorstate=0
    mirrormin=0
    mirrormax=0
    DTmax=0
    DTmin=0
    DT=0
    timing=0

    buck_mirror_control=np.zeros(buck_mirr_temp.size, dtype=np.int)-9999

    #for i=0L, n_elements(data)-1 do begin
    #for i=long(interval)+1, n_elements(data)-interval-1 do begin
    for i in range(interval+1, buck_mirr_temp.size-interval-1):
        #culm1=0
        #culm2=0
        #e=0
        #do a moving average of [interval] data to determine the rate of change of Mirror Temp
        #while e < interval:
        #    culm1=culm1+(buck_mirr_temp[i+e])-(buck_mirr_temp[i+e-1])
            #culm2=culm2+(buck_mirr_temp[i+e-interval] )-(buck_mirr_temp[i+e-1-interval])
        #    e+=1
        #culm1=np.sum(buck_mirr_temp[i:i+interval]-buck_mirr_temp[i-1:i+interval-1])
        #DT=float(culm1)/float(interval)
        DT=np.mean(buck_mirr_temp[i:i+interval]-buck_mirr_temp[i-1:i+interval-1])
        #Calculate acceptable range of Delta Mirror T based on Mirror Temp
        if (buck_mirr_temp[i] < 220.0):
            DTmax=1.0/(220.0*0.0172438-3.6602)*0.5
        else:
            DTmax=1.0/(buck_mirr_temp[i]*0.0172438-3.6602)*0.5

        if (buck_mirr_temp[i] > 290.0):
            DTmin=1.0/(290.0*0.041044-12.232)*0.5
        else:
            DTmin=1.0/(buck_mirr_temp[i]*0.041044-12.232)*0.5

        buck_mirror_control[i]=2

        #Make a first cut at guessing the mirror state -
        #   0=water (above 273K),
        #   1=ice (when the mirror has been cold and then not above zero)
        #   2=not known.
        #these will be used to calculate the uncertainty owing to mirror state.

        if buck_mirr_temp[i] > 273.15:
            buck_mirror_control[i]=0

        if buck_mirr_temp[i] < 243.15:
            mirrormin=1
            mirrormax=1

        if buck_mirr_temp[i] > 273.15:
            timing=0
        else:
            timing+=1

        if buck_mirr_temp[i] > 243.15:
            if mirrormin > 0:
                if buck_mirr_temp[i] < 273.15:
                    mirrormax=1
                else:
                    mirrormin=0
                    mirrormax=0

        if mirrormin > 0:
            if mirrormax > 0:
                buck_mirror_control[i]=1
            else:
                buck_mirror_control[i]=2

        if timing > 600:
            buck_mirror_control[i]=1

        #If Mirror Delta T outside acceptable range then flag as 3.
        #Start an 80s counter (320 4hz cycles)(recovery) following flag,
        #and only unflag when this has expired

        if DT > DTmax:
            buck_mirror_control[i]=3
            recovery=80
        else:
            if recovery > 0:
                buck_mirror_control[i]=3
                recovery-=1

        if DT < DTmin:
            buck_mirror_control[i]=3
            recovery=80
        else:
            if recovery > 0:
                buck_mirror_control[i]=3
                recovery-=1

    return buck_mirror_control

#TODO: The wat_min50to0_coeff is never used; is this correct?
def get_enhance_coeff(buck_mirror_ctl):
    """
    """
    result=np.zeros((buck_mirror_ctl.size, 8), dtype=np.float32)

    # ice coefficients
    ice_coeff=[-6.0190570E-2, 7.3984060E-4, -3.0897838E-6, 4.3669918E-9, -9.4868712E+1, 7.2392075E-1, -2.1963437E-3, 2.4668279E-6]
    # water coefficients 0 to 100 Celsius
    wat_0to100_coeff=[-1.6302041E-1, 1.8071570E-3, -6.7703064E-6, 8.5813609E-9, -5.9890467E+1, 3.4378043E-1, -7.7326396E-4, 6.3405286E-7]
    # water coefficients -50 to 0 Celsius
    wat_min50to0_coeff=[-5.5898100E-2, 6.7140389E-4, -2.7492721E-6, 3.8268958E-9, -8.1985393E+1, 5.8230823E-1, -1.6340527E-3, 1.6725084E-6]

    result[buck_mirror_ctl < 1,:]=ice_coeff
    # !!! First bug found
    #result[(buck_mirror_ctl > 1) & (buck_mirror_ctl != 3),:]=wat_0to100_coeff
    result[(buck_mirror_ctl >= 1) & (buck_mirror_ctl != 3),:]=wat_0to100_coeff
    result[buck_mirror_ctl == 3,:]=np.nan
    return result


def get_vp_coeff(buck_mirror_ctl):
    """Result array is filled up with coefficient that apply either
    to liquid (water) or solid (ice) conditions.

    """
    rows=buck_mirror_ctl.size
    result=np.zeros((rows, 11), dtype=np.float32)
    # ice coefficients - from Murphy and Koop, valid below 273K
    #ice_coeff=[ 0.000000000, -5.8666426E3, 2.232870244E1, 1.39387003E-2, -3.4262402E-5, 2.7040955E-8, 0.0000000000, 6.7063522E-1]
    ice_coeff=[9.550426,-5723.265,3.53068,-0.00728332,-9999.9,-9999.9,-9999.9,-9999.9,-9999.9,-9999.9,0.0]
    # water coefficients - these are from Murphy and Koop, valid for liquid water from 123< T <332 K
    #wat_coeff=[ -2.836574400E3, -6.028076559E3, 1.954263612E1, -2.737830188E-2, 1.6261698E-5, 7.0229056E-10, -1.8680009E-13, 2.7150305]
    wat_coeff=[54.842763,-6763.22,-4.210,0.000367,0.0415,-218.8,53.878,-1331.22,-9.44523,0.014025,1.0]
    result[buck_mirror_ctl < 1,:]=wat_coeff
    #Bug 4 found    
    #result[buck_mirror_ctl > 1,:]=ice_coeff
    result[buck_mirror_ctl >= 1,:]=ice_coeff
    result[buck_mirror_ctl > 2,:]=np.nan
    return result


def calc_vp(buck_mirr_temp, buck_mirror_ctl, buck_unc_k=None):
    """Calculate vapour pressure depending on
    likely mirror state (ie water above 273.15K, Ice below that)

    """
    # check if buck_unc_k is an array or not
    if not hasattr(buck_unc_k, 'size'):
        n=buck_mirr_temp.size
        buck_unc_k=np.zeros(n, dtype=np.float32)

    c=get_vp_coeff(buck_mirror_ctl)
    result=np.exp(c[:,0]+c[:,1]/(buck_mirr_temp+buck_unc_k)+c[:,2]*np.log(buck_mirr_temp+buck_unc_k)+c[:,3]*(buck_mirr_temp+buck_unc_k)+\
           c[:,10]*(np.tanh(c[:,4]*((buck_mirr_temp+buck_unc_k)+c[:,5])))*(c[:,6]+c[:,7]/(buck_mirr_temp+buck_unc_k)+c[:,8]*np.log(buck_mirr_temp+buck_unc_k)+c[:,9]*(buck_mirr_temp+buck_unc_k))) 
    return result


def calc_vmr(vp, enhance, buck_pressure):
    """Calculate volume mixing ratio

    """
    vmr=vp/(buck_pressure*100.0-vp*enhance)*enhance*10.0E5
    vmr[vmr < 0]=np.nan
    return vmr


def calc_enhance_factor(vp_buck, buck_mirror_t, buck_pressure, buck_mirror_ctl):
    """
    """
    c=get_enhance_coeff(buck_mirror_ctl)

    # Calculate the enhancement factors and calculate the mixing ratio as ppmV
    result=np.exp((1.0-vp_buck/(buck_pressure*100.0))*\
                  c[:,0]+\
                  c[:,1]*buck_mirror_t+\
                  c[:,2]*buck_mirror_t**2+\
                  c[:,3]*buck_mirror_t**3)+\
                  ((buck_pressure*100.0)/vp_buck-1.0)*\
                  np.exp(c[:,4]+c[:,5]*buck_mirror_t+c[:,6]*buck_mirror_t**2+c[:,7]*buck_mirror_t**3)
    return result


def get_flag(buck_mirr_flag, buck_dewpoint_flag):
    flag=np.zeros(buck_mirr_flag.size, dtype=np.int)
    flag[buck_mirr_flag == 1]=2
    flag[buck_dewpoint_flag == 2]=3
    return flag
    
def calc_tdew_corrected(buck_mirr_control,vmr_buck,ps_rvsm,enhance):
    #this calculates the dewpoint corrected for the difference in pressure between outside air and inside the instrument
    #haven't recalculated the error, this is unlikely to change much so probably isn't worth doing?
    n=vmr_buck.size
    tfrost_corrected=np.zeros(n)
   # rows1=buck_mirr_control.size
    result=np.zeros(n)
    vp_corrected=np.zeros(n)
    vp_corrected=100*ps_rvsm*vmr_buck/(enhance*10e5+enhance*vmr_buck)
   
   #using the function given in Murphy and Koop 2005
    tdew_function = lambda tdew : (54.842763- 6763.22/tdew - 4.210*np.log(tdew ) + 0.000367*tdew+ np.tanh(0.0415*(tdew - 218.8))*(53.878- 1331.22/tdew- 9.44523*np.log(tdew) + 0.014025*tdew))-np.log(vp_here)
    tdew_corrected=np.zeros(n)
    vp_here=np.zeros(n)
    # Use the numerical solver to find the roots
    tdew_initial_guess = 300.0

    for i in range(0,n):
        vp_here=vp_corrected[i]
        tdew_corrected[i] = fsolve(tdew_function, tdew_initial_guess)

    #using the function given in Murphy and Koop 2005
    tfrost_corrected=(1.814625*np.log(vp_corrected)+6190.134)/(29.120-np.log(vp_corrected))

    #TODO: "THINK THIS COULD BE BETTER?"
    for j in range(0,n):
        if buck_mirr_control[j] < 1:
            result[j]=tdew_corrected[j]
        if buck_mirr_control[j] >= 1:
            result[j]=tfrost_corrected[j]
        if buck_mirr_control[j] > 2:
            result[j]=np.nan
    
    return result







class rio_buck_cr2(cal_base):
#class rio_buck_cr2(object):
    """Routine to process data from the BUCK CR2 Hygrometer.

    """

    def __init__(self,dataset):
        """AERACK_buck_ppm: raw ppm reading for the buck hygrometer
           AERACK_buck_mirr_temp: the reading to represent the buck mirror temperature
           AERACK_buck_dewpoint_flag: the reading to represent the buck mirror flag (0=clean mirror, 1=mirror contaminated, sshould be cleaned soon)
           AERACK_buck_pressure: the reading to represent the buck pressure transducer
"""


        self.input_names=['BUCK',
                          'AERACK_buck_ppm',
                          'AERACK_buck_mirr_temp',
                          'AERACK_buck_pressure',
                          'AERACK_buck_dewpoint_flag',
                          'AERACK_buck_mirr_cln_flag',
                          'PS_RVSM']

        self.outputs=[parameter('VMR_CR2', units='ppmv', frequency=1, number=783, long_name='Water vapour volume mixing ratio measured by the Buck CR2', standard_name='volume_mixing_ratio_of_water_in_air'),
                      parameter('VMR_C_U', units='ppmv', frequency=1, number=784, long_name='Uncertainty estimate for water vapour volume mixing ratio measured by the Buck CR2'),
                      parameter('TDEW_CR2', units='degK', frequency=1, number=785, long_name='Mirror Temperature measured by the Buck CR2 Hygrometer', standard_name='dew_point_temperature'),
                      parameter('TDEW_C_U', units='degK', frequency=1, number=786, long_name='Uncertainty estimate for Buck CR2 Mirror Temperature'),
                      parameter('TDEW_CR2_C', units='degK', frequency=1, long_name='Corrected dew point temperature measured by the Buck CR2 Hygrometer', standard_name='dew_point_temperature')]
        
        self.version=1.00
        cal_base.__init__(self,dataset)

    def process(self):
        match=self.dataset.matchtimes(self.input_names[1:])

        ps_rvsm=self.dataset['PS_RVSM'].ismatch(match).get1Hz()
        
        buck_mirr_temp=self.dataset['AERACK_buck_mirr_temp'].ismatch(match)               
        buck_mirr_temp+=273.15 #convert to Kelvin
        buck_mirr_temp[buck_mirr_temp == 273.15]=np.nan
        buck_mirr_temp[buck_mirr_temp < 0]=np.nan
        #apply calibration using coefficients from the flight constants file
        p=np.poly1d(self.dataset['BUCK'][::-1])
        buck_mirr_temp=p(buck_mirr_temp)
        buck_pressure=self.dataset['AERACK_buck_pressure'].ismatch(match)
        buck_dewpoint_flag=self.dataset['AERACK_buck_dewpoint_flag'].ismatch(match)        
        buck_mirr_cln_flag=self.dataset['AERACK_buck_mirr_cln_flag'].ismatch(match)      
        """!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!THIS IS TEMPORARY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"""
        n=buck_mirr_temp.size
        ps_rvsm=np.zeros(n)
        ps_rvsm=buck_pressure+100.0
        """!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"""

        buck_mirr_control=get_buck_mirror_ctl(buck_mirr_temp)
        vp_buck=calc_vp(buck_mirr_temp, buck_mirr_control)
        buck_unc_k=calc_uncertainty(buck_mirr_temp, buck_pressure, buck_mirr_control)
        vp_max=calc_vp(buck_mirr_temp, buck_mirr_control, buck_unc_k=buck_unc_k)
        enhance=calc_enhance_factor(vp_buck, buck_mirr_temp, buck_pressure, buck_mirr_control)
        vmr_buck=calc_vmr(vp_buck, enhance, buck_pressure)
        vmr_max=calc_vmr(vp_max, enhance, buck_pressure)
        vmr_unc=vmr_max-vmr_buck
        tdew_corrected=calc_tdew_corrected(buck_mirr_control,vmr_buck,ps_rvsm,enhance)   

        flag=get_flag(buck_mirr_cln_flag, buck_dewpoint_flag)
        flag[~np.isfinite(buck_mirr_temp)]=3
        
        vmr_buck=flagged_data(vmr_buck, match, flag)
        vmr_unc=flagged_data(vmr_unc, match, flag)
        tdew_cr2=flagged_data(buck_mirr_temp, match, flag)
        tdew_c_u=flagged_data(buck_unc_k, match, flag)
                
     
        

        for var in [vmr_buck, vmr_unc, tdew_cr2, tdew_c_u]:
            var[~np.isfinite(var)]=-9999.0

        """!!!!!!!!!!!NEED TO ADD IN TDEW_CORRECTED AS A NEW PARAMETER"""
        self.outputs[0].data=vmr_buck
        self.outputs[1].data=vmr_unc
        self.outputs[2].data=tdew_cr2
        self.outputs[3].data=tdew_c_u
        self.outputs[4].data=tdew_corrected
