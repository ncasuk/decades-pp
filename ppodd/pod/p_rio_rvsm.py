#from ppodd.pod import *
from ppodd.core import *
from scipy.interpolate import interp1d

class rio_rvsm(cal_base):
    """
:ROUTINE:
  C_RVSM SUBROUTINE FORTVAX
    
:PURPOSE:
  Computes static pressure, pitot-static pressure, and pressure
  height from the 146 RVSM altitude and airspeed data.

:DESCRIPTION:
  RVSM altitude is available in ARINC-429 message 203 and is
  recorded as by the DRS as a 16 bit signed word, with the 
  LSB representing 4 feet.

  RVSM computed airspeed is available in ARINC-429 message
  206 and is recorded by the DRS as a 16 bit signed word, with
  the LSB representing 1/32 kt, but always zero.

  These values should be within the system accuracy
  specification and do not require calibration.

  Note that altitude is updated by the RVSM at about 20 Hz
  and airspeed is updated at about 10 Hz.  Both signals are
  sampled by the DRS at 32 Hz so there will be multiple
  values and aliasing effects.

:METHOD:
  For each DRS parameter to be calibrated:
  1. If data is FFFF or FFFE or out of range then flag 3
  2. Decode the altitude and use the tables in NASA TN D-822
     to back compute the static pressure.
  3. Decode the airspeed and use fundamental equations to 
     compute pitot-static pressure.
  4. Check the results for being within acceptable values.
  5. Set data flag bits (16+17)
    | 0: Good data
    | 1: Data of lower quality
    | 2: Probably faulty, exceed lims
    | 3: Data absent or invalid.

  Flagging - If a value can't be computed, due to missing data
  missing constants, divide be zeroes, etc, a value of 0 is
  used, flagged with a three.  If a value is outside its                                                                                                          
  limits for range, it is flagged with a two.                                                                                                                     
  If there are no problems with the data it is flagged with 0.                                                                                                    
  Any flags on input data are propagated through subsequent                                                                                                       
  calculations.                                                                                                                                                   
                                                                                                                                                                  
  Note that routine does not currently apply position error                                                                                                       
  corrections, nor interpolate missing data.                                                                                                                      
                                                                                                                                                                                 
:VERSION:
  1.00  23/07/03  W.D.N.JACKSON                                                                                                                                   
                                                                                                                                                                                 
:ARGUMENTS:
  :Inputs:                                                                                                                                                         
  | DRS para 222 RVAL 32 Hz RVSM altitude                                                                                                                         
  |     para 223 RVAS 32 Hz RVSM computed airspeed                                                                                                                
                                                                                                                                                                                 
  :Outputs:                                                                                                                                                        
  | Derived para 576 SPR  mb 32 Hz Static pressure                                                                                                                
  |         para 577 PSP  mb 32 Hz Pitot-static pressure                                                                                                          
  |         para 578 PHGT m  32 Hz Pressure height                                                                                                                
                                                                                                                                                                               
  :Flags:                                                                                                                                                          
  | Missing/corrupt data output as 0 flagged 3.                                                                                                                   
  | Out of range derived data flagged 2.                                                                                                                          
                                                                                                                                                                                 
:SUBPROGRAMS:
  S_PSP, ALT2PRESS, ISETFLG                                                                                                                                       
                                                                                                                                                                                 
:REFERENCES:
  NASA Technical Note D-822, Aug 1961, Tables of airspeed, altitude, and mach number.                                                                                                                                      
                                                                                                                                                                                 
  Interface Control Document, Air Data Display Unit, ISS 1G-80130-22.                                                                                                                                                    
                                                                                                                                                                                 
:CHANGES:
  V1.00 23/07/03  WDNJ
    Original version                                                                                                                           
  V1.01 23/10/03  WDNJ
    Now replicates data when missing                                                                                                           
  V1.02 11/12/03  WDNJ
    Fixes bug if initial data missing                                                                                                          
  V1.03 11/03/04  DAT
    Flags data outside altitude range 3                                                                                                         
  V1.04 17/03/04  WDNJ
    Now handles negative heights correctly and uses more accurate flagging criteria

"""
    def __init__(self,dataset):
        self.input_names=['PRTAFT_pressure_alt', 'PRTAFT_ind_air_speed']
        self.outputs=[parameter('PS_RVSM',units='hPa',frequency=32,number=576,long_name='Static pressure from the aircraft RVSM (air data) system')
                     ,parameter('Q_RVSM',units='hPa',frequency=32,number=577,long_name='Pitot static pressure inverted from RVSM (air data) system indicated airspeed')
                     ,parameter('PALT_RVS',units='m',frequency=32,number=578,long_name='Pressure altitude from the aircraft RVSM (air data) system')]
        self.version=1.00
        cal_base.__init__(self,dataset)

    def resample(self,P,flag,freq=32):
        Pflat=P.ravel()
        Pflat.interp1d()
        tnew=P.times.at_frequency(freq)
        ifl=interp1d(Pflat.times,flag.ravel(),bounds_error=False)
        flagnew=(ifl(tnew.ravel()).reshape(tnew.shape)!=0)*3
        return flagged_data(Pflat.interpolated(tnew.ravel()).reshape(tnew.shape),P.times,flagnew)
        
    def process(self):
        if(self.dataset[self.input_names[0]].data is not None):
            palt_feet=self.dataset[self.input_names[0]]*4
            flag_alt=(palt_feet<-2000) | (palt_feet>50000)
            palt_meters=palt_feet/3.28084
            high=palt_meters>11000
            low=palt_meters<=11000
            """ Compute Pressure from altitude and barometric formula """
            T0=288.15
            L0=-0.0065
            h0=0.0
            go=9.80665
            M=0.0289644
            R=8.31432
            P0=1013.2500
            P=P0*(T0/(T0+L0*(palt_meters-h0)))**(go*M/(R*L0))
            if(len(np.where(high)[0]) > 0):
                """ Lapse rate is 0.0 from 11000 to 20000 meters - maybe we never get this high.."""
                T1=216.65
                P1=226.3210
                h1=11000.0
                P.raw_data[high]=P1*np.exp((-go*M*(palt_meters.raw_data[high]-h1))/(R*T1))
            """ Resample at 32 Hz """
            self.outputs[0].data=self.resample(P,flag_alt)
            self.outputs[2].data=self.resample(palt_meters,flag_alt)
            if(self.dataset[self.input_names[1]].data  is not None):
                ias=self.dataset[self.input_names[1]]*0.514444/32.0  # m/s
                flag_ias=(ias<-50) | (ias>500) | flag_alt
                mach=ias/(340.294*np.sqrt(P/1013.25))
                pitot=P*((((mach**2.)/5.+1.)**3.5)-1.)
                self.outputs[1].data=self.resample(pitot,flag_ias)

