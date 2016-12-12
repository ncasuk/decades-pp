from ppodd.core import *
class rio_sols(fort_cal):
    """
:ROUTINE:
  C_SOLS    SUBROUTINE  FORTVAX

:PURPOSE:
  CALIBRATE  PYRANOMETER & PYRGEOMETER RAW SIGNALS AND THERMISTORS.

:DESCRIPTION:
  Apply calibration coefficients to RAW parameters 81-89 and 91-99
  to obtain uncorrected values of signal flux, zero offset signal
  (W/m-2) and thermistor output (deg K)  for  each  of  the 
  upward-facing and downward-facing sets of: clear dome & red dome
  pyranometers and pyrgeometer.

  .. note         The actual configuration is specified by the array
                  ICONF, which has six elements whose meaning interpreted as:
                    1,4 : Clear dome pyranometer  (upper/lower)
                    2,5 : red    "       "           "     "
                    3,6 : Silver "   pyrgeometer     "     "
                  (normally: ICONF(1-3) Upper instruments.
                             ICONF(4-6) Lower instruments.)
                  
                  This value assists the processing of each instrument by 
                  selecting the correct range checking values to use.
                  Should the configuration aboard the aircraft be changed
                  the array ICONF should be adjusted accordingly by adding
                  offsets to the second calibration constant in the constants
                  file. 
                  e.g. If  the second constant for, say, the second instrument
                  was changed from 1.23456E-1 to 21.23456E-1, the offset would
                  decode to "2" after decoding.
                  This is assigned to ICONF(2) and would imply that the 
                  'channel' contained raw flux, zero-offset and thermistor 
                  values for a red dome - rather than clear dome - pyranometer.
                  and should be range-checked for that type of output only.

:METHOD:
  For each RAW parameter to be calibrated, for the six instruments:
  
    1. Check all its required constants are present (Flag <3)
       (if not, the calibration of that parameter will not proceed)
       [Also check that the normal configuration of instruments is to
       be used. Any changes are indicated by the presence of a large
       offset to the second calibration constant for any instrument.
       If this is present the offset is decoded to generate a revised
       ICONF indicator for that instrument. See note below.]
    2. Obtain the raw signal/zero value and float the result.
    3. Calibrate by applying the appropriate instrument cal in RCALB
       (which is loaded from the RCONST arguments) to both raw 
       signal flux and zero offset, which  use the same coefficients
       The gains are in W/m-2 /DRS count. DRS counts are related
       to radiometer output Voltage.
       Note that the output Voltage from the instrument  is the
       value after being amplified by the head amplifier.
    4. Range check and Rate-of-change check: (S/R QCPT)
        
         | the calibrated signal (Wm-2)
         | Zero offset           (DRS units)
         | temperature           (deg K)  
   
    5. Calibrate the thermistor input using two RCALB coefficients.
       Add 273.15 deg to thermistor results to express the 
       instrument thermopile temperature in degrees Kelvin.
    6. Check the result is within pre-defined limits

:FLAGGING:
    Set the calibrated values flag bits (16+17) as follows
      
      | 0: Good data  
      | 1: Data of lower quality
      | 2: Data probably faulty, exceeding limits
      | 3: Data absent or known to be invalid.

:VERSION:
  1.04 250692   A D HENNINGS

:ARGUMENTS:
  |     RCONST(1)  - REAL*4 IN  Upper Clear dome Signal & Zero const.
  | \*  RCONST(2)  - REAL*4 IN  Upper Clear dome Signal & Zero gain.
  |     RCONST(3)  - REAL*4 IN  Upper Clear dome Thermistor: constant
  |     RCONST(4)  - REAL*4 IN  Upper Clear dome Thermistor: coeff x.
  |     RCONST(5)  - REAL*4 IN  Upper Red   dome Signal & Zero const.
  | \*  RCONST(6)  - REAL*4 IN  Upper Red   dome Signal & Zero gain.
  |     RCONST(7)  - REAL*4 IN  Upper Red   dome Thermistor: constant
  |     RCONST(8)  - REAL*4 IN  Upper Red   dome Thermistor: coeff x.
  |     RCONST(9)  - REAL*4 IN  Upper I/R   dome Signal & Zero const.
  | \*  RCONST(10) - REAL*4 IN  Upper I/R   dome Signal & Zero gain.
  |     RCONST(11) - REAL*4 IN  Upper I/R   dome Thermistor: constant
  |     RCONST(12) - REAL*4 IN  Upper I/R   dome Thermistor: coeff x.
  |     RCONST(13) - REAL*4 IN  Lower Clear dome Signal & Zero const.
  | \*  RCONST(14) - REAL*4 IN  Lower Clear dome Signal & Zero gain.
  |     RCONST(15) - REAL*4 IN  Lower Clear dome Thermistor: constant
  |     RCONST(16) - REAL*4 IN  Lower Clear dome Thermistor: coeff x.
  |     RCONST(17) - REAL*4 IN  Lower Red   dome Signal & Zero const.
  | \*  RCONST(18) - REAL*4 IN  Lower Red   dome Signal & Zero gain.
  |     RCONST(19) - REAL*4 IN  Lower Red   dome Thermistor: constant
  |     RCONST(20) - REAL*4 IN  Lower Red   dome Thermistor: coeff x.
  |     RCONST(21) - REAL*4 IN  Lower I/R   dome Signal & Zero const.
  | \*  RCONST(22) - REAL*4 IN  Lower I/R   dome Signal & Zero gain.
  |     RCONST(23) - REAL*4 IN  Lower I/R   dome Thermistor: constant
  |     RCONST(24) - REAL*4 IN  Lower I/R   dome Thermistor: coeff x.
  |     (*  also contains an offset evaluated to ICONF() ).

  IFRQ(par)  - INT*4  IN  Input frequency of each sample. 
  IRAW(n,par)- INT*4  IN  Raw instrument voltage conversion. (samples n=1; par=81-89, 91-99)
  RDER(op,opar)REAL*4 OUT Raw flux signal, zero-offset signal and instrument temperature. (samples op=1; opar=673-690)


:SUBPROGRAMS:
  ITSTFLG, ISETFLG
 

:REFERENCES:
  Equations from MRF Instrument section.

:CHANGES:
  020490 Revised  range limits introduced.                 ADH

  100191                                                   ADH
  a) Range limits revised to allow for Pyranometer changes
  b) New arrays to hold raw input, constants etc for more straightforward indexing.
  c) Include ICONF to aid reconfiguring instrument types.

  010891 Range limits for ZERO now in terms of DRS units, revised limits in Wm-2 for signal.

  030292 Rates of change checks instituted on all BBR inputs.  ADH

  120698 Bug fixed in quality control processing when using non-
  standard configurations. MDG/WDNJ

  270600 I/R signal maximum increased to stop flagging good data
  value arbitary, as no explanation of numbers found. 1050. > 1500. DAT

  V1.06  02/10/02  Changed to use 16 bit DRS data.

  V1.07  27/11/02  Now takes X0 sensitivity constant as well as X1

  V1.08  22/07/04  Bug so doesn't crash if first data flagged 3

  V1.09  13/08/04  Quality Control zero limits increased for 16 bit data

"""
    def __init__(self,dataset):
        self.input_names=['CALUP1S', 'CALUP2S', 'CALUIRS', 'CALLP1S', 'CALLP2S', 'CALLIRS', 
                          'SECS', 
                          'UPPBBR_radiometer_1_sig', 'UPPBBR_radiometer_2_sig', 'UPPBBR_radiometer_3_sig', 
                          'UPPBBR_radiometer_1_zero', 'UPPBBR_radiometer_2_zero', 'UPPBBR_radiometer_3_zero', 
                          'UPPBBR_radiometer_1_temp', 'UPPBBR_radiometer_2_temp', 'UPPBBR_radiometer_3_temp',
                          'LOWBBR_radiometer_1_sig', 'LOWBBR_radiometer_2_sig', 'LOWBBR_radiometer_3_sig', 
                          'LOWBBR_radiometer_1_zero', 'LOWBBR_radiometer_2_zero', 'LOWBBR_radiometer_3_zero', 
                          'LOWBBR_radiometer_1_temp', 'LOWBBR_radiometer_2_temp', 'LOWBBR_radiometer_3_temp']
        self.outputs=[parameter('UP1S',units='W M-2',frequency=1,number=673,long_name='UPP VIS CLR SIG')
                     ,parameter('UP2S',units='W M-2',frequency=1,number=674,long_name='UPP VIS RED SIG')
                     ,parameter('UIRS',units='W M-2',frequency=1,number=675,long_name='UPP I/R SIGNAL')
                     ,parameter('UP1Z',units='W M-2',frequency=1,number=676,long_name='UPP VIS CLR ZERO')
                     ,parameter('UP2Z',units='W M-2',frequency=1,number=677,long_name='UPP VIS RED ZERO')
                     ,parameter('UIRZ',units='W M-2',frequency=1,number=678,long_name='UPP I/R ZERO')
                     ,parameter('UP1T',units='DEG C',frequency=1,number=679,long_name='UPP VIS CLR TEMP')
                     ,parameter('UP2T',units='DEG C',frequency=1,number=680,long_name='UPP VIS RED TEMP')
                     ,parameter('UIRT',units='DEG C',frequency=1,number=681,long_name='UPP I/R TEMP')
                     ,parameter('LP1S',units='WM-2',frequency=1,number=682,long_name='LWR VIS CLR SIG')
                     ,parameter('LP2S',units='WM-2',frequency=1,number=683,long_name='LWR VIS RED SIG')
                     ,parameter('LIRS',units='WM-2',frequency=1,number=684,long_name='LWR I/R SIGNAL')
                     ,parameter('LP1Z',units='WM-2',frequency=1,number=685,long_name='LWR VIS CLR ZERO')
                     ,parameter('LP2Z',units='WM-2',frequency=1,number=686,long_name='LWR VIS RED ZERO')
                     ,parameter('LIRZ',units='WM-2',frequency=1,number=687,long_name='LWR I/R ZERO')
                     ,parameter('LP1T',units='DEG C',frequency=1,number=688,long_name='LWR VIS CLR TEMP')
                     ,parameter('LP2T',units='DEG C',frequency=1,number=689,long_name='LWR VIS RED TEMP')
                     ,parameter('LIRT',units='DEG C',frequency=1,number=690,long_name='LWR I/R TEMP')]
        #self.name='C_RIO_SOLS'
        self.fortname='SOLS'
        self.version=1.00
        fort_cal.__init__(self,dataset)
        
    def process(self):
        rawnumbers=[ 81, 82, 83, 84, 85, 86, 87, 88, 89, 91, 92, 93, 94, 95, 96, 97, 98, 99]
        for i,n in enumerate(self.input_names[7:]):
            self.dataset[n].number=rawnumbers[i]
        fort_cal.process(self)
