#from ppodd.pod import *
from ppodd.core import *
from scipy.interpolate import interp1d

class rio_rvsm(cal_base):
    """
FORTRAN routine C_TPRESS

 ROUTINE          C_TPRESS SUBROUTINE FORTVAX
     
 PURPOSE          Calibrates the five turbulence probe pressure transducers
                  into mb.

 DESCRIPTION      Apply calibration the combined transducer and DRS 
                  coefficients to DRS parameters 215 to 219 to obtain derived
                  parameters 773 to 777.  Invalid data is flagged with 3, data
                  outside limits is flagged with 2.

 METHOD           For each DRS parameter to be calibrated:
                  1. If data is FFFF or FFFE then flag 3
                  2. Apply the calibration constants
                  3. Check the results for being within acceptable values.
                  4. Set data flag bits (16+17) 0: Good data
                                                1: Data of lower quality
                                                2: Probably faulty, exceed lims
                                                3: Data absent or invalid.

                  Flagging - If a value can't be computed, due to missing data
                  missing constants, divide be zeroes, etc, a value of 0 is
                  used, flagged with a three.  If a value is outside its 
                  limits for range or rate of change, it is flagged with a two.
                  If there are no problems with the data it is flagged with 0.

 VERSION          1.00  23/07/03  W.D.N.JACKSON

 ARGUMENTS        Inputs:
                    DRS para 215 TBP1 32 Hz Turbulence probe centre port
                        para 216 TBP2 32 Hz Turbulence probe attack ports
                        para 217 TBP3 32 Hz Turbulence probe sideslip ports
                        para 218 TBP4 32 Hz Turbulence probe attack check
                        para 219 TBP5 32 Hz Turbulence probe sideslip check

                  Constants:
                        RCONST(1 to 4) Para 215 cal constants X0 to X3
                        RCONST(5 to 8) Para 216 cal constants X0 to X3
                        RCONST(9 to 12) Para 217 cal constants X0 to X3
                        RCONST(13 to 14) Para 218 cal constants X0 to X1
                        RCONST(15 to 16) Para 219 cal constants X0 to X1

                  Outputs:
                    Derived para 773 TBP0 mb 32 Hz Centre pressure
                            para 774 TBPA mb 32 Hz Attack pressure
                            para 775 TBPB mb 32 Hz Sideslip pressure
                            para 776 TBPC mb 32 Hz Attack check pressure
                            para 777 TBPD mb 32 Hz Sideslip check pressure

                  Flags:
                    Missing/corrupt data output as 0 flagged 3.
                    Out of range data flagged 2.

 SUBPROGRAMS      ISETFLG 

 REFERENCES       

 CHANGES          V1.00 23/07/03  WDNJ Original version
                  Note that V1.00 has no limit checking and no use is made of
                  the check pressures.
                  V1.01 25/03/04  WDNJ
                  Now takes third order calibration constants for the main
                  transducers, and first order for the check transducers.
                  V1.02 26/01/06 Phil Brown
                  Realistic min/max values provided for centre-port, Pa, Pb
                  for flagging purposes. Values alsoe provided for check
                  pressures Ca, Cb based on current (and probably wrong)
                  calibration coefficients.
                  V1.03 09/02/11 Axel Wellpott
                  From an email from Phil Brown: "The P0-S10 differential pressure
                  (para 773) is flagged 2 if it exceeds 130.0 hPa. This is easily 
                  exceeded when we do acceleration to max speed (min Angle of Attack)
                  so all the subsequent parameters calculated n C_TURB.for end up with a
                  flag-3 saetting. I reckon a better value would be 180.0 hPa."

*******************************************************************************

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

