#from ppodd.pod import *
from ppodd.core import *
class rvsm(fort_cal):
    """
FORTRAN routine C_RVSM

 ROUTINE          C_RVSM SUBROUTINE FORTVAX

 PURPOSE          Computes static pressure, pitot-static pressure, and pressure
                  height from the 146 RVSM altitude and airspeed data.

 DESCRIPTION      RVSM altitude is available in ARINC-429 message 203 and is
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

 METHOD           For each DRS parameter to be calibrated:
                  1. If data is FFFF or FFFE or out of range then flag 3
                  2. Decode the altitude and use the tables in NASA TN D-822
                     to back compute the static pressure.
                  3. Decode the airspeed and use fundamental equations to
                     compute pitot-static pressure.
                  4. Check the results for being within acceptable values.
                  5. Set data flag bits (16+17) 0: Good data
                                                1: Data of lower quality
                                                2: Probably faulty, exceed lims
                                                3: Data absent or invalid.

                  Flagging - If a value can't be computed, due to missing data
                  missing constants, divide be zeroes, etc, a value of 0 is
                  used, flagged with a three.  If a value is outside its
                  limits for range, it is flagged with a two.
                  If there are no problems with the data it is flagged with 0.
                  Any flags on input data are propagated through subsequent
                  calculations.

                  Note that routine does not currently apply position error
                  corrections, nor interpolate missing data.

 VERSION          1.00  23/07/03  W.D.N.JACKSON

 ARGUMENTS        Inputs:
                    DRS para 222 RVAL 32 Hz RVSM altitude
                        para 223 RVAS 32 Hz RVSM computed airspeed

                  Outputs:
                    Derived para 576 SPR  mb 32 Hz Static pressure
                            para 577 PSP  mb 32 Hz Pitot-static pressure
                            para 578 PHGT m  32 Hz Pressure height

                  Flags:
                    Missing/corrupt data output as 0 flagged 3.
                    Out of range derived data flagged 2.

 SUBPROGRAMS      S_PSP, ALT2PRESS, ISETFLG

 REFERENCES       NASA Technical Note D-822, Aug 1961, Tables of airspeed,
                  altitude, and mach number.

                  Interface Control Document, Air Data Display Unit, ISS
                  1G-80130-22.

 CHANGES          V1.00 23/07/03  WDNJ Original version
                  V1.01 23/10/03  WDNJ Now replicates data when missing
                  V1.02 11/12/03  WDNJ Fixes bug if initial data missing
                  V1.03 11/03/04  DAT Flags data outside altitude range 3
                  V1.04 17/03/04  WDNJ Now handles negative heights correctly
                                       and uses more accurate flagging criteria

*******************************************************************************

"""
    def __init__(self,dataset):
        self.input_names=['Horace_RVAL', 'Horace_RVAS']
        self.outputs = [parameter('PS_RVSM',units='hPa',frequency=32,number=576,long_name='Static pressure from the aircraft RVSM (air data) system',standard_name='air_pressure'),
                        parameter('Q_RVSM',units='hPa',frequency=32,number=577,long_name='Pitot static pressure inverted from RVSM (air data) system indicated airspeed'),
                        parameter('PALT_RVS',units='m',frequency=32,number=578,long_name='Pressure altitude from the aircraft RVSM (air data) system',standard_name='barometric_altitude')]
        #self.name='RVSM'
        self.version=1.00
        fort_cal.__init__(self,dataset)
