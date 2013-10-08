from ppodd.core import *
class cnc(fort_cal):
    """
FORTRAN routine C_CNC

 ROUTINE          C_CNC SUBROUTINE FORTVAX

 PURPOSE          Produces (CNCC) CNC Concentration (568) as a real number

 DESCRIPTION      Takes 16 bit word from the DRS interface which contains
                  four decimal digits as BCD (leftmost digit being the highest
                  nibble). This word is recorded as DRS parameter 50 (CNC).
                  The four decimal digits represent a real number of the form:

                                    ab.cEd
                  
                  which is calculated thus:

                            (a + b/10 + y/100) * 10**d
                  
                  Checks are made that no hexadecimal figures are included
                  in the initial nibbles. Any data including these returns
                  a value of 0.0 and is flagged 3. (If the 4 BCD digits
                  return AAAA it means there was no response from the CNC
                  whilst EEEE means the CNC responded with ERROR.)

 VERSION          1.00 21/03/97 D H ANDERSON 

 ARGUMENTS        IRAW(64,512) I*4 IN  Up to 64 samples for up to 512 DRS pars
                  IFRQ(512)    I*4 IN  Sample rate of each DRS par (0-64)
                  RCONST(64)   R*4 IN  Inputs constants (none used by C_CNC)
                  RDER(64,1024)R*4 OUT Output array of up to 64 samples for
                                       each of 1024 parameters

 CHANGES          


    """
    def __init__(self,dataset):
        self.input_names=['Horace_CNC']
        self.outputs=[parameter('CPC_CONC',units='cm-3',frequency=1,number=568,long_name='Total condensation particle concentration from TSI 3025A')]
        #self.name='CNC'
        self.version=1.00
        fort_cal.__init__(self,dataset)
