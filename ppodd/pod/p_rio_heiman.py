from ppodd.core import *

class rio_heiman(fort_cal):
    """
FORTRAN routine C_HEIMAN
 
:ROUTINE:          C_HEIMAN   SUBROUTINE FORTVAX

:PURPOSE:          To derive uncorrected Heimann temperatures

:DESCRIPTION:
  Converts rawdata input from the Heimann radiometer and
  black body source into uncorrected surface tempratures,
  parameter 537.
                  
  The Heimmann is recorded by para 141, 
  the blackbody reference temperature by para 142,
  and bit 0 of the signal register (para 27) indicates whether
  the Heimann is set to calibrate.  

:ARGUMENTS:
  | IRAW   input raw data
  | IFRQ   raw data frequencies
  | RCONST flight constants corresponding to PRTCCAL and HEIMCAL
  | RDER   output data

:SUBPROGRAMS:      

:REFERENCES:

:VERSION:
  1.00 D.R.Lauchlan

:CHANGES:          

:DESCRIPTION:
  Converts the two input parameters 141 (raw Heimann) and
  142 (black body reference temperature) into one, the
  uncorrected HEIMAN temp (para 537).

  The Heimann Radiometer data is converted using a quadratic
  fit::
  Surface temp =  RCONST(4) + RCONST(5)*x + RCONST(6)*x**2
  RCONST(4 to 6) correspond to the constants with the keyword
  HEIMCAL in the flight constants file.

  The black body signal (para 142) is converted using
  a quadratic fit::

   BB = a + b*x + c*x**2

  where constants a, b and c correspond to RCONST(1 to 3)
  from the keyword PRTCCAL in the flight constant file.

  Signal Register (para 27) bit 0 indicates the position of 
  the black body;  0 = b/b out of FoV, 1 = b/b in FoV. 

  If signal register bit 0 is set to 1 and black body
  reference temprature has been steady for the previous
  3 seconds (mean of each second differs by no more than
  0.1K), the b/b reference temperature is output.
  Otherwise the HEIMAN temprature is output. An offset
  is assigned accordingly:
                                para 27 
                                 bit 0
   233.26 to  313.06   Heimann-    0    (o/s = 0)
  1233.26 to 1313.06   Ref/BB -    0    (o/s = 1000)
  2233.26 to 2313.06   Heimann-    1    (o/s = 2000)
  3233.26 to 3313.06   Ref/BB -    1    (o/s = 2000 + 1000)

  (NOTE: an offset of 1000 is never assigned under 
         this scheme)

  Heimann data is output for the time that the
  reference temperature is output. This is done in
  4 second bursts imediately after the reference 
  sequence and overwrites the ramping sections.
  Non reference or dwelling calibration temeratures
  are flagged as 2.
  Dwell Heimann data is output for the corresponding
  calibration reference period after the instrument has 
  switched back to measure, ie para 27 bit 0 becomes 0. 

  Bits 16 and 17 of the output temperature RDER(x,537) are
  used to flag certain data conditions as follows :
    Bits 16 and 17 = 00  - Good data, MEASURE/HEIMANN
                   = 01  - Good data, but Heimann on CALIBRATE
                           and outputing DWELL temp
                           or looking at the Black Body temps
                           or BB moved out of field of view
                           and data are last Heimann dwell
                           data.
                   = 02  - Suspect or absent signal register 
                           data, non-reference calibration
                           temperatures and non-dwelling
                           calibration temperatures.
                   = 03  - Absent data, passed through from
                           IRAW(x,141)

:ARGUMENTS:
  | IRAW(f,141)   Raw Heimann data
  | IRAW(f,27)    Raw signal register data 
  | IRAW(f,142)   Raw black body data
  | IFRQ(141)     Recorded frequency of Heimann Radiometer    
  | IFRQ(27)      Recorded frequency of signal register  
  | IFRQ(142)     Recorded frequency of black body
  | RCONST(1-6)   Constants for quadratic fit
  | RDER(f,537)   Uncorrected Heimann temps (deg K) 

:SUBPROGRAMS:
  | ITSTFLG  -  Returns the value of bits 16 & 17    - SCILIB
  | ISETFLG  -  Sets the value of bits 16 & 17       - SCILIB
  | IBITS    -  Extracts selected bits from input    - FORTRAN
  | BTEST    -  Tests value of selected single bit   - FORTRAN
  | C_HEIMAN_LTST_BB  -  Checks array elemenets are within +/- 86                                - LOCAL

:REFERENCES:       

:VERSION:          1.00   09-11-94 D.R.Lauchlan
                  Based on C_BARNES V2.00 by D.P. Briggs

:CHANGES:
  V1.01  10/02/99  W.D.N.JACKSON
    Bug in flag checking of raw data fixed.
  V1.02  27/09/02  W.D.N.JACKSON
    Changed to include handling of 16 bit data from the new 
    DRS.  Also now expects calibrator temp cal to be in deg C.
  V1.03 11/11/04 J.P. TAYLOR
    Changed to account for 16bit representation of DRS
    parameters.  Allowable range of BB ref means changed from
    +/- 6 to +/- 86 this is equivalent to 0.1K with the new
    DRS 16bit data stream.


"""
    def __init__(self,dataset):
        self.input_names=['PRTCCAL', 'HEIMCAL', 'SREG', 'CORCON_heim_t', 'CORCON_heim_c']
        self.outputs=[parameter('BTHEIM_U',units='degK',frequency=4,number=537,long_name='Uncorrected brightness temperature from the Heimann radiometer')]
        self.fortname='HEIMAN'
        #self.name='RIO_HEIMAN'
        self.version=1.00
        fort_cal.__init__(self,dataset)
        
        
    def process(self):
        rawnumbers=[27,141,142]
        for i,n in enumerate(self.input_names[2:]):
            self.dataset[n].number=rawnumbers[i]
        fort_cal.process(self)
