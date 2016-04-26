import ppodd
from ppodd.core import *

class twc_rio(cal_base):
    """ 
    Flag and calibrate raw TWC data
    """
    def __init__(self,dataset):
        self.input_names=['CALTNOS', 'CALTSAM', 'CALTAMB', 'CALTSRC', 'CALHTR1', 'CALHTR2', 'CALISRC',
                          'TWCDAT_twc_detector', 'TWCDAT_twc_nose_temp', 'TWCDAT_twc_samp_temp', 'TWCDAT_twc_amb_temp', 
                          'TWCDAT_twc_srce_temp', 'TWCDAT_twc_evap1','TWCDAT_twc_evap2',                                                  
                          'TWCDAT_twc_srce_i','TWCDAT_twc_evap2_on','TWCDAT_status']
        
        self.outputs=[parameter('TWC_DET',units='bits',frequency=64,number=664,long_name='Raw data from the TWC probe Lyman alpha detector'),
                      parameter('TNOS_CAL',units='DEG K',frequency=1,number=665,long_name='TWC NOSE TEMP'),
                      parameter('TWC_TSAM',units='K',frequency=1,number=666,long_name='Sample temperature in Kelvin from the TWC evaporator probe'),
                      parameter('TAMB_CAL',units='DEG K',frequency=1,number=667,long_name='TWC AMBIENT TEMP'),
                      parameter('TSRC_CAL',units='DEG K',frequency=1,number=668,long_name='TWC SOURCE TEMP'),
                      parameter('HTR1_CAL',units='AMPS',frequency=1,number=669,long_name='TWC EVAP1 CURRENT'),
                      parameter('HTR2_CAL',units='AMPS',frequency=1,number=670,long_name='TWC EVAP2 CURRENT'),
                      parameter('ISRC_CAL',units='AMPS',frequency=1,number=671,long_name='TWC SOURCE CURRENT'),
                      parameter('STAT_CAL',units='RBITS',frequency=1,number=672,long_name='TWC STATUS WORD')]
        #self.name='TWC'
        self.version=1.00
        cal_base.__init__(self,dataset)
        
    def thermistor(self,data,A,B,C,D):
        # A combination of a simplified thermistor equation R=Nexp(B/T)
        # and the parallel resistance Rout=R*Rp/(R+Rp)
        # and linear voltage from Rout,  Decades=m*Rout+c
        # gives something like T=B/(ln(R)-ln(N)) , where  R=Rp*(Decades-c)/(c+m*Rp-Decades)
        # Constants
        # A is a combination of N, m and Rp..
        # B is the Beta value of the Thermistor
        # C is related to the decades value displayed for a short circuit ( but not quite... ) 
        # D is minus the decades value for an open circuit
        # Calculated using scipy.optimize.curve_fit(thermistor,decades,temp,p0=[11,4000,2000,8000]) 
        # p0 has to be a reasonable first guess - particularly D must be greater than the max negative decades data value.    
        #
        return B/(np.log(C-data)-np.log(data+D)+A)
        
    def fit_thermistor(self,data,temps):
        import scipy.optimize
        return scipy.optimize.curve_fit(self.thermistor,data,temps,p0=[11,4000,2000,8000])
                    
    def process(self):
        """
        detector between max and min ( -9,+9 )
        caltnos - nose_temp, use status word to flag ?  or > 383K  10K/s
        caltsam - samp_temp  "  323K < T < 388K                    3K/s
        caltamb - amb_temp  " 289K < T < 343K                      2K/s
        caltsrc - srce_temp " 378K < T < 393K                      5K/s
        calhtr1 - 0.3A < I < 6.6A                                 0.5A/s
        calhtr2 - 0.3A < I < 6.6A                                 0.5A/s
        calisrc - -0.4mA < I < -1.1mA                             0.05mA/s
        !c_twc     Lyman alpha total water - temps in K, currents in A, TWC decades refit 22/10/2013
        CALTNOS   1.2614E-16 -1.8668E-12  1.2704E-08  9.4262E-03  3.1654E+02
        CALTSAM   11.02757718 3956.07906064 1932.77875494 7162.35330738
        !CALTSAM   2.5406E-17  3.5253E-13  2.1300E-09  7.1707E-06  2.7927E-02  4.0343E+02
        CALTAMB   2.9951E-13  9.3288E-10 -4.0779E-06  1.6016E-02  2.7390E+02
        CALTSRC   2.8700E-18 -1.2794E-14  2.8480E-11  2.2585E-10  9.5178E-03  3.7298E+02
        CALHTR1   9.7752E-04  0.0000E+00
        CALHTR2   9.7752E-04  0.0000E+00
        CALISRC   9.7636E-08 -2.5957E-06
        """
        caltnos=self.dataset['CALTNOS'].data
        caltsam=self.dataset['CALTSAM'].data
        caltamb=self.dataset['CALTAMB'].data
        caltsrc=self.dataset['CALTSRC'].data
        calhtr1=self.dataset['CALHTR1'].data
        calhtr2=self.dataset['CALHTR2'].data
        calisrc=self.dataset['CALISRC'].data       
        # ranges ( min, max, max rate of change )
        # These should really be in the constants 
        ranges={'TWC_DET':(-9*1024,9*1024,20*1024),
                'TNOS_CAL':(314,383,10),
                'TWC_TSAM':(300,388,30),
                'TAMB_CAL':(289,343,2),
                'TSRC_CAL':(378,393,5),
                'HTR1_CAL':(0.3,6.6,0.5),
                'HTR2_CAL':(0.3,6.6,0.5),
                'ISRC_CAL':(-1.1e-3,-0.4e-3,0.05e-3),'STAT_CAL':(0,255,255)}
        times=self.inputs['TWCDAT_twc_detector'].times
        flags=np.zeros(times.shape,dtype='u1')
        flags64=np.zeros(times.at_frequency(64).shape,dtype='u1')
        self.outputs[0].data=flagged_data(self.inputs['TWCDAT_twc_detector'].data,times,flags64)
        self.outputs[1].data=flagged_data(np.polyval(caltnos,self.inputs['TWCDAT_twc_nose_temp'].data),times,flags.copy())
        if len(caltsam)==4:
            # Fit to sample temp using thermistor equation - should be 4 constants
            self.outputs[2].data=flagged_data(self.thermistor(self.inputs['TWCDAT_twc_samp_temp'].data,*caltsam),times,flags.copy())            
        else:
            # Old polynomial fit for sample temp
            self.outputs[2].data=flagged_data(np.polyval(caltsam,self.inputs['TWCDAT_twc_samp_temp'].data),times,flags.copy())
        self.outputs[3].data=flagged_data(np.polyval(caltamb,self.inputs['TWCDAT_twc_amb_temp'].data),times,flags.copy())
        self.outputs[4].data=flagged_data(np.polyval(caltsrc,self.inputs['TWCDAT_twc_srce_temp'].data),times,flags.copy())
        self.outputs[5].data=flagged_data(np.polyval(calhtr1,self.inputs['TWCDAT_twc_evap1'].data),times,flags.copy())
        self.outputs[6].data=flagged_data(np.polyval(calhtr2,self.inputs['TWCDAT_twc_evap2'].data),times,flags.copy())
        self.outputs[7].data=flagged_data(np.polyval(calisrc,self.inputs['TWCDAT_twc_srce_i'].data),times,flags.copy())
        self.outputs[8].data=self.inputs['TWCDAT_status'].data
        for o in self.outputs[:-1]:
            mn,mx,rate=ranges[o.name]
            o.data.flag[o.data<mn]=1
            o.data.flag[o.data>mx]=1
            #rchange=np.append([0],np.abs(np.diff(o.data.ravel())/np.diff(o.data.ravel().times)))
            rchange=np.append([0],np.abs(np.diff(np.array(o.data.ravel()))/(1./o.frequency)))
            rchange.shape=o.data.shape
            o.data.flag[rchange>rate]=1
        
        
         
         
