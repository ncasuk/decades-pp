#from ppodd.pod import *
from ppodd.core import *
class psap(fort_cal):
    """
FORTRAN routine C_PSAP
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         ROUTINE      C_PSAP

         PURPOSE      Calibrates the parameters from the Particle Soot
                      Absorbtion Photometer (PSAP).

         DESCRIPTION  All raw parameters are digital conversions of the input
                      voltage. The digital values are converted using a
                      linear fit then the instrument cals are
                      applied to obtain the output derived values.
                      Parameter 175 is linear thus

                           output = v * 0.5E-5

                      v the derived voltage and vfs the full scale voltage.
                      Parameter 177 is logrithmic so

                          ouput = 10**((v/2.0) - 7.0)


         VERSION      1.00  D.P.Briggs

         SUBROUTINES  ISETFLG

         FILES        NONE

         PARAMETERS   RAW      DERIVED    FREQ   RANGE   UNITS
  PSAP LIN ABS COEFF  185        648      1Hz    0-10V    /m
  PSAP LOG ABS COEFF  186        649      1Hz    0-10V    /m
  PSAP TRANSMITTANCE  187                 1Hz    0-10V
  PSAP FLOW RATE      188                 1Hz    0-10V

         PSAP CONSTANT KEYWORDS
  CALPLIN   i j    NOTE : i & j are multiplexer calibrations.
  CALPLOG   i j
 
         CHANGES 

         V1.01  01/10/02  W.D.N.JACKSON
         Adjusted for 16 bit data from the new DRS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

"""
    def __init__(self,dataset):
        self.input_names=['CALPLIN', 'CALPLOG', 'Horace_PLIN', 'Horace_PLOG', 'Horace_PTRN', 'Horace_PFLO']
        self.outputs=[parameter('PSAP_LIN',units='m-1',frequency=1,number=648,long_name='Uncorrected absorption coefficient at 565nm, linear, from PSAP.')
                     ,parameter('PSAP_LOG',units='*',frequency=1,number=649,long_name='Uncorrected absorption coefficient at 565nm, log, from PSAP.')]
        #self.name='PSAP'
        self.version=1.00
        fort_cal.__init__(self,dataset)
