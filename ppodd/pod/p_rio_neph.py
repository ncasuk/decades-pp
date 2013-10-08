from ppodd.core import *
class rio_neph(fort_cal):
    """
FORTRAN routine C_NEPHL1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         ROUTINE      C_NEPHL1

         PURPOSE      Calibrates the parameters from the TSI 3 wavelength
                      Nephelometer.

         DESCRIPTION  The nephelometer output are subject to calibrations
                      internal to the instrument plus the normal MILLIE/DRS
                      calibrations.

                      All raw parameters are digital conversions of the input
                      voltage. The digital values are converted using a
                      linear fit then the nephelometer internal cals are
                      applied to obtain the output derived values.
                      Parameters 175, 176 & 183 are linear thus

                           output = a + b * v /vfs

                      where a and b are nephelometer internal constants,
                      v the derived voltage and vfs the full scale voltage.
                      Scattering parameters 177 through 182 are logarithmic
                      so

                          output = 10**((v/b) - a) - c

                      a, b and c are nephelometer internal constants, v
                      the derived voltage.

                      Parameter 184, nephelometer status, is the analog output
                      of a 4 bit D to A converter. The LSB corresponds to 
                      0.625V.

         VERSION      1.00  D.P.Briggs

         SUBROUTINES  ISETFLG

         FILES        NONE

         REFERENCES   Nephelometer Instruction Manual
                      Nephelometer internal technical note
  
         PARAMETERS   RAW      DERIVED    FREQ   RANGE   UNITS
  NEPH PRESSURE       175        760      1Hz    0-10V    mb
  NEPH TEMPERTURE     176        761      1Hz    0-10V    K
  NEPH BLUE SP        177        762      1Hz    0-10V    /m
  NEPH GREEN SP       178        763      1HZ    0-10V    /m
  NEPH RED SP         179        764      1Hz    0-10V    /m
  NEPH BLUE BSP       180        765      1Hz    0-10V    /m
  NEPH GREEN BSP      182        766      1Hz    0-10V    /m
  NEPH RED BSP        181        767      1Hz    0-10V    /m
  NEPH HUMIDITY       183        768      1Hz    0-5V     %
  NEPH STATUS         184        769      1Hz    0-10V    bits
 
         NEPHELOMETER CONSTANT KEYWORDS
   CALNPRS  I J A B      
   CALNTMP  I J A B
   CALNBTS  I J A B C    NOTE : I & J are multiplexer calibrations.
   CALNGTS  I J A B C           A, B & C are instrument internal cals.
   CALNRTS  I J A B C
   CALNBBS  I J A B C
   CALNGBS  I J A B C
   CALNRBS  I J A B C
   CALNHUM  I J A B
   CALNSTS  I J

         CHANGES 
         V1.01  12/05/97  W.D.N.JACKSON
                Miscellaneous bug fixes, the most serious being the incorrect
                calculation of the red backscattering coefficient, and a
                tendency to crash with floating overflows.
         V1.02  29/05/97  W.D.N.JACKSON
                Changed to reflect fact that Red BS comes in on 181 and green
                on para 182.
         V1.03  19/06/98  W.D.N.JACKSON
                Changed to reflect fact that bit 3 of the status word is 0
                (not 1) when on calibrate.
         V1.04  01/09/02  W.D.N.JACKSON
                Small changes to handle new 16 bit DRS.  Also status parameter
                is now calibrated.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

"""
    def __init__(self,dataset):
        self.input_names=['CALNPRS', 'CALNTMP', 'CALNBTS', 'CALNGTS', 'CALNRTS', 'CALNBBS', 'CALNGBS', 'CALNRBS', 'CALNHUM', 'CALNSTS', 
                          'AERACK_neph_pressure', 'AERACK_neph_temp', 
                          'AERACK_neph_total_blue', 'AERACK_neph_total_green', 'AERACK_neph_total_red', 
                          'AERACK_neph_backscatter_blue', 'AERACK_neph_backscatter_red', 'AERACK_neph_backscatter_green', 
                          'AERACK_neph_humidity', 'AERACK_neph_status'] # Why are the backscatter ones in a funny order ?
        self.outputs=[parameter('NEPH_PR',units='hPa',frequency=1,number=760,long_name='Internal sample pressure of the Nephelometer')
                     ,parameter('NEPH_T',units='K',frequency=1,number=761,long_name='Internal sample temperature of the Nephelometer')
                     ,parameter('TSC_BLUU',units='m-1',frequency=1,number=762,long_name='Uncorrected blue total scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('TSC_GRNU',units='m-1',frequency=1,number=763,long_name='Uncorrected green total scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('TSC_REDU',units='m-1',frequency=1,number=764,long_name='Uncorrected red total scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('BSC_BLUU',units='m-1',frequency=1,number=765,long_name='Uncorrected blue back scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('BSC_GRNU',units='m-1',frequency=1,number=766,long_name='Uncorrected green back scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('BSC_REDU',units='m-1',frequency=1,number=767,long_name='Uncorrected red back scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('NHUM_CAL',units='%',frequency=1,number=768,long_name='NEPH HUMIDITY')
                     ,parameter('NSTS_CAL',units='RBITS',frequency=1,number=769,long_name='NEPH STATUS')]
        #self.name='RIO_NEPH'
        self.fortname='NEPHL1'
        self.version=1.00
        fort_cal.__init__(self,dataset)
        
    def process(self): 
        for i in range(10):
            self.dataset[self.input_names[i+10]].number=i+175
        fort_cal.process(self)
