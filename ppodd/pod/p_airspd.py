#from ppodd.pod import *
from ppodd.core import *
class airspd(fort_cal):
    """
FORTRAN routine C_AIRSPD

 ROUTINE          C_AIRSPD SUBROUTINE FORTVAX

 PURPOSE          Derives Indicated and True Airspedd (paras 516 & 517)

 DESCRIPTION      True Air Speed is the component of air flow parallel to
                  the Aircraft's longitudinal axis.

                  IAS =  340.294
                         * Mach no.
                         * SQRT(Static Pressure[mb]/ 1013.25)      in ms-1

                  TAS =  A/S correction factor
                         * 340.294
                         * Mach no.
                         * SQRT(De-iced True Air Temp[K] / 288.15) in ms-1

                  where:

                    288.15   is ICAO Standard temperature [K] at zero altitude.
                    340.294  is speed of sound [ms-1]         at zero altitude.
                    Mach no. is computed by subroutine S_MACH.

                  Flagging - If a value can't be computed, due to missing data
                  missing constants, divide be zeroes, etc, a value of 0 is
                  used, flagged with a three.  If a value is outside its
                  limits for range or rate of change, it is flagged with a two.
                  If there are no problems with the data it is flagged with 0.
                  Any flags on input data are propagated through subsequent
                  calculations.

 VERSION          1.00  020190   A.D.HENNINGS

 ARGUMENTS        For Indicated Airspeed:
                    RDER(IN,576) REAL*4 IN  Derived static pressure in mb
                    RDER(IN,577) REAL*4 IN  Derived Pitot static pressure in mb
                                            (samples IN = 1,32 )
                    RDER(OP,516) REAL*4 OUT Derived Indicated Airspeed  ms-1
                                            (samples OP = 1,32 )
                  For True Airspeed:
                    RCONST(1)    REAL*4 IN  True Airspeed correction factor
                    RDER(IN,576) REAL*4 IN  Derived static pressure in mb
                    RDER(IN,577) REAL*4 IN  Derived Pitot static pressure in mb
                    RDER(IN,520) REAL*4 IN  Derived De-iced True air temp deg K
                                            (samples IN = 1,32 )
                    RDER(IN,525) REAL*4 IN  Derived Non-Deiced True air temp deg K
                                            (samples IN = 1,32 )
                    RDER(OP,517) REAL*4 OUT Derived True Airspeed  ms-1
                                            (samples OP = 1,32 )

 SUBPROGRAMS      S_MACH, S_QCPT, ITSTFLG, ISETFLG

 REFERENCES       Code adapted from MRF1/HORACE
                  n.b. RCONST(1) (Air speed correction factor 'K' should be
                       determined by 'K & Gamma' aircraft runs. The value
                       in RCONST(1) is unity. Experimental values have been
                       found between 0.98 and 1.02; (HORACE used 1.002 from
                       June 1988 - Jan 1990., value suggested by S.Nicholls
                       after JASIN experiment).

 CHANGES          V1.01  02/06/93  Limit on max rate of change between
                  adjacent samples has been increased to 3.3 m/s.  This is
                  based on analysis of the high turbulence A257 flight, where
                  the histogram of rates of change showed meaningful changes
                  of up to 3.0 m/s between adjacent samples. (WDNJ)
                  Also changed so that data with flags of 2 are processed
                  rather than rejected and flags are stripped from data before
                  processing. (WDNJ)

                  V1.02 20/06/06 If TAT_DI flag is 2 or more, then takes
                  temperature input from TAT_NDI (Phil Brown)

*******************************************************************************

"""
    def __init__(self,dataset):
        self.input_names = ['TASCORR', 'SECS','PS_RVSM', 'Q_RVSM', 'TAT_DI_R']
        self.outputs = [parameter('IAS_RVSM',
                                  units='m s-1',
                                  frequency=32,
                                  number=516,
                                  long_name='Indicated air speed from the aircraft RVSM (air data) system'),
                        parameter('TAS_RVSM',
                                  units='m s-1',
                                  frequency=32,
                                  number=517,
                                  long_name='True air speed from the aircraft RVSM (air data) system and deiced temperature',
                                  standard_name='platform_speed_wrt_air')]
        #self.name='AIRSPD'
        self.version = 1.00
        fort_cal.__init__(self,dataset)
