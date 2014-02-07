#from ppodd.pod import *
from ppodd.core import *
class rio_press(fort_cal):
    """
FORTRAN routine C_PRESS1

 ROUTINE          C_PRESS1 SUBROUTINE FORTVAX

 PURPOSE          Calibrates the cabin pressure sensor and the S9 static port.

 DESCRIPTION      Apply calibration the combined transducer and DRS
                  coefficients to DRS parameters 14 and 221 to obtain derived
                  parameters 579 and 778.  Invalid data is flagged with 3, data
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
                    DRS para  14 CABP  1 Hz Cabin pressure
                        para 221 S9SP 32 Hz S9 static pressure

                  Constants:
                        RCONST(1 to 3) Para 14 cal constants X0 to X2
                        RCONST(4 to 6) Para 221 cal constants X0 to X2

                  Outputs:
                    Derived para 579 CABP mb  1 Hz Cabin pressure
                            para 778 S9SP mb 32 Hz S9 static pressure

                  Flags:
                    Missing/corrupt data output as 0 flagged 3.
                    Out of range data flagged 2.

 SUBPROGRAMS      ISETFLG

 REFERENCES

 CHANGES          V1.00 23/07/03  WDNJ Original version
                  Note that V1.00 has no application of S9 position errors.

*******************************************************************************

"""
    def __init__(self,dataset):
        self.input_names=['CALCABP', 'CALS9SP', 'CORCON_cabin_p', 'CORCON_s9_press']
        self.outputs=[parameter('CAB_PRES',units='hPa',frequency=1,number=579,long_name='Cabin pressure'),
                      parameter('P9_STAT',units='hPa',frequency=32,number=778,long_name='Static pressure from S9 fuselage ports',standard_name='air_pressure')]
        #self.name='RIO_PRESS'
        self.fortname='PRESS1'
        self.version=1.00
        fort_cal.__init__(self,dataset)

    def process(self):
        self.dataset[self.input_names[2]].number=14
        self.dataset[self.input_names[3]].number=221
        fort_cal.process(self)
