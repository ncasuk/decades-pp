#from ppodd.pod import *
from ppodd.core import *
class grflux(fort_cal):
    """
FORTRAN routine C_GRFLUX
------------------------------------------------------------------------------
 ROUTINE  	   C_RFLUX        SUBROUTINE FORTVAX       [C_RFLUX.FOR]
     
 PURPOSE 	   CORRECT RAW FLUXES FOR PYRANOMETERS AND PYRGEOMETERS
                  
 DESCRIPTION      Flux corrections are performed for the six instruments
                  which are normally configured:
                  Upward-facing :-  Clear dome and Red dome pyranometers.
                                    Silver dome pyrgeometer.
                  Downward-facing:- Clear dome and Red dome pyranometers.
                                    Silver dome pyrgeometer.
                  
                  The actual configuration is specified by the preset array
                  ICONF, which has six elements whose meaning interpreted as:
                    1,4 : Clear dome pyranometer  (upper/lower)
                    2,5 : red    "       "           "     "
                    3,6 : Silver "   pyrgeometer     "     "
                  (normally: ICONF(1-3) Upper instruments.
                             ICONF(4-6) Lower instruments.)
                  
                 Check that the normal configuration of instruments is to
                 be used. Any changes are indicated by the presence of a large
                 offset to the last calibration constant for any instrument
                 (i.e. the obscurer indicator constant).
                 If this is present the offset is interpreted as a revised
                 ICONF indicator for that instrument. See note below.]

                  n.b. Lower instruments were fitted w.e.f. Flight H797
                       Upper instruments were fitted w.e.f. Flight H842

                  This value solely determines the control path through the
                  routine for the processing of each instruments inputs.
                  Should the configuration aboard the aircraft be changed
                  the array ICONF should be adjusted accordingly.
                  e.g. If ICONF(1) was modified := 2; it would imply that the
                  'channel' contained raw flux, zero-offset and thermistor
                  values for a red dome - rather than clear - pyranometer.
                  The value of ICONF(1) i.e. 2 would determine the processing
                  path, the selection of the appropriate set of constants
                  to apply for correction and the range checking.

 NOTE             CHANGES FROM STANDARD CONFIGURATION.                    
                  Should the configuration of BBR instruments aboard the 
                  aircraft be changed e.g. swapping a red dome for clear dome,
                  the array ICONF is  adjusted accordingly. The mechanism used
                  is to add an offset to the sixth constant in the calibration 
                  constants file (i.e. the obscurer) for that instrument.
                  Example: If the second 'channel' (inputs 674,677,680) which
                  in the standard configuration is a red dome pyranometer,
                  was replaced with a second clear dome instrument, the sixth
                  constant for the second line of the constants for C_RFLUX 
                  would be changed from 1.0000E+0 to 21.0000E+0, the offset 
                  decodes to "2" when detected by this program. 
                  This is assigned to ICONF(2) and would imply that the 
                  'channel' inputs contain raw flux, zero-offset and thermistor 
                  values for a red dome - rather than clear dome - pyranometer,
                  and should be range-checked for that type of output only.
                 
                  Corrections applied:
                  --------------------
                  Pyranometers (Clear and Red dome) are corrected for:
                  - Subtraction of a zero offset (mean over past 10 seconds)
                  - Attitude (pitch and roll) -Upper instruments only.
                    test if flux is above a critical limit indicating a direct
                    solar beam component.
                      If not direct, assume diffuse and apply no attitude corr.
                      If DIRECT, a geometric correction is used to "level"
                      the instrument to produce the equivalent hemispheric
                      downward flux through a horizontal surface (without
                      inclusion of diffuse component).
                      The ratio of the Direct:Direct+Diffuse components is
                      assumed to be 0.95 at present. This value could be
                      optimised for a particular atmosphere depending on the 
                      turbidity as a function of height.
                      
                      Correct for COSINE effect. (MRF Technical note No.7).
                      [Pitch and roll offsets of the instrument thermopiles
                      relative to the aircraft INS platform are derived in
                      flight by flying a box pattern in cloud-free skies -
                      These offsets are then used in addition to the INS pitch
                      and roll (meaned over two seconds). (See MRF Technical
                      note No 4.) and these values are supplied as arguments 
                      four and five in each set of CONSTANTS below.
                  - Time constant of thermopile relative to INS. The mean of
                    last two seconds of INS pitch/roll angles are used in the
                    attitude correction, giving an effective difference of
                    0.5 seconds.
                  - Correct flux output for proportion of hemispheric dome 
                    obscured by indicated obscurer pillar. (Rawlins 1986).

                  Pyrgeometers (IR) are corrected for:
                  - Zero offset (mean over past 10 seconds)
                  - Temperature sensitivity  (Coefficients in CONSTANTS below)
                  - Linear dependence 0.2% per degree with sensitivity defined 
                    as unity at zero C. applied to signal. (MRF Int note No 50)
                  - Calculation of flux (sigma T^4 correction)
                    Flux = signal +(sigma* Tsink^4) 
                    where sigma = Stefan-Boltzmann constant.
                  _ Upper instrument is corrected for dome transmission
                    effects (MRF Tech note 3)

 VERSION	   1.17 05-09-07   D Tiddeman  

 METHOD           1. First time routine is called, assign constants to named
                     program variables/arrays.
                     Decide on basis of input constants whether upper instr.
                     data is available to be processed. 
                  2. Derive/convert any intermediate results used multiply
                     within several code sections following.
                  3. Derive running mean zero-offsets over the past 10 seconds
                     for each instrument 
                     
                  4. Calculate mean pitch and roll values for the current 
                     second and use them to derive running means for the past 
                     two seconds.
                  5. Correct thermistor temperatures for non-linearity.
                  6. Cycle through each of six instrument input channels.
                     Use the control variable in ICONF() to select execution
                     of appropriate code sections.
                     In all cases; derive a signal zero-offset and reduce the
                     signal flux by this amount.
                     Apply temperature-dependance corrections to pyranometers.
                     For upward-facing pyranometers the 'critical' value to
                     discriminate between diffuse and direct-sun conditions is
                           FCRIT  = 920.*(COS(ZENRAD))**1.28 
                     where ZENRAD : solar zenith angle (in radians)
                     [N.B. This approximates to the 'German' equation but is
                      simpler, and does not produce negative values at low
                      Sun elevations]. 
                     Correct flux output for proportion of hemispheric dome 
                     obscured by indicated obscurer pillar. (Rawlins 1986).
                                                          
                  7. Range check flux output and set a flag accordingly.
                     Apply flag values to resulting flux output dependent on
                     relevant flag settings.
                  
 ARGUMENTS        RCONST(1),( 7)..(31)  - REAL*4 IN Temperature Sens. coeff a
                  RCONST(2),( 8)..(32)  - REAL*4 IN Temperature Sens. coeff b
                  RCONST(3),( 9)..(33)  - REAL*4 IN Temperature Sens. coeff c
                  RCONST(4),(10)..(34)  - REAL*4 IN Pitch offset of Instrument
                  RCONST(5),(11)..(35)  - REAL*4 IN Roll  offset of Instrument
                  RCONST(6),(12)..(36)  - REAL*4 IN Obscurer pillar type.
                  
                  RDER(1,par)             REAL*4  IN Six raw flux signals W/M-2
                             (par=673-675,682-684)
                  RDER(1,par)             REAL*4  IN six zero-offsets   (W/M-2)
                             (par=676-678,685-687)
                  RDER(1,par)             REAL*4  IN six instr. temperatures K
                             (par=679-681,688-690)
                  RDER(32,560)            REAL*4  IN INS Roll      (degrees)
                  RDER(32,561)            REAL*4  IN INS Pitch     (degrees)
                  RDER(32,562)            REAL*4  IN INS heading   (degrees)
                  RDER(1,642)             REAL*4  IN Solar azimuth (degrees)
                  RDER(1,643)             REAL*4  IN Solar zenith  (degrees)

                                                              Pos. Dome  Units
                  RDER(1,1019)            REAL*4 OUT Corrected Upp Clear W/m-2 
                  RDER(1,1020)            REAL*4 OUT flux.      "  Red dome " 
                  RDER(1,1021)            REAL*4 OUT            "  I/R      "
                  RDER(1,1022)            REAL*4 OUT           Low Clear    "
                  RDER(1,1023)            REAL*4 OUT            "  Red dome " 
                  RDER(1,1024)            REAL*4 OUT            "  I/R      " 

 SUBPROGRAMS  	   ITSTFLG, ISETFLG, S_RUNM, CORR_THM, RMEANOF, CIRC_AVRG

 REFERENCES 	   MRF Internal note  4.
                   "     "      "   12.
                   "     "      "   31.
                   "     "      "   50.
                   "     "      "   56.
                  MRF Technical note 3. Pyrgeometer Corrections due to Dome
                                        Transmission.  February 1991 Kilsby
                  MRF Technical note 7. Report of Broad-band radiative fluxes
                                        working group. 17/12/91      Saunders
                  MRF Technical note 8. Pyramometer calibrationsin Ascension
                                        of Feb.1992.   4/6/92        Seymour
                  RAWLINS  R        D/Met.O.(MRF)/13/1      1986.
                  SAUNDERS R         "        "   "         21/3/90
                  SAUNDERS R        M/MRF/13/5              22/7/92
                  
 CHANGES          10/01/91 A.D.Hennings.
                    Ability to change ICONF to when reconfiguring instrument
                    fit on A/C using the constants file.
                  10/01/91 Pitch & Roll averaging changed from 3 to 2 seconds.
                  25/01/91 Flags assessment changed; use of new flag IFLAG_SUN 
                  29/01/91 Roll limit checking:replace ROLBAR with ABS(ROLBAR).
                           Flags assessment changed; IFLAG_OUTPUT being max of
                           (signal,Pitch,Roll,Zenith) flags.               
                  30/07/91 FCRIT for Red dome now only used if no clear dome 
                  16/10/91 Corrected pyrgeometer temp sensitivity correction
                  20/01/92 Use INS heading instead of obsolete Omega heading.
                  03/02/92 New subroutine CIRC_AVRG to calc INS mean heading
                  21/07/92 Levelling of upper pyranometers changed to use 
                           direct beam component, and cosine effect included.
                           Recommendations of MRF Tech note 7.   (V1.13)
                           references to Tech note 8. and M/MRF/13/5
                  24/07/92 Pyrgeometer corrections for Dome transmission. 
                           (Downwelling) MRF Tech note 3.             
                  17/01/96 D Lauchlan
                           Unused variables removed
                  22/12/97 W D N Jackson, Flags cleared from all data before
                           use.
                  11/08/98 W D N Jackson, Upper pyranometer obscurer
                           corrections changed to correct values.  The
                           values have been incorrect in all previous versions
                           of C_RFLUX. The error is only small. (Source 
                           P Hignett)
                  05/09/07 D TIDDEMAN Will use GIN attitude if available rather 
                           then INU

------------------------------------------------------------------------------
"""
    def __init__(self,dataset):
        self.input_names=['CALCUCF', 'CALCURF', 'CALCUIF', 'CALCLCF', 'CALCLRF', 'CALCLIF', 'UP1S', 'UP2S', 'UIRS', 'UP1Z', 'UP2Z', 'UIRZ', 'UP1T', 'UP2T', 'UIRT', 'LP1S', 'LP2S', 'LIRS', 'LP1Z', 'LP2Z', 'LIRZ', 'LP1T', 'LP2T', 'LIRT', 'SOL_AZIM', 'SOL_ZEN', 'ROLL_GIN', 'PTCH_GIN', 'HDG_GIN']
        self.outputs = [parameter('SW_DN_C',units='W m-2',frequency=1,number=1019,long_name='Corrected downward short wave irradiance, clear dome', standard_name='downwelling_shortwave_flux_in_air'),
                        parameter('RED_DN_C',units='W m-2',frequency=1,number=1020,long_name='Corrected downward short wave irradiance, red dome'),
                        parameter('IR_DN_C',units='W m-2',frequency=1,number=1021,long_name='Corrected downward long wave irradiance.'),
                        parameter('SW_UP_C',units='W m-2',frequency=1,number=1022,long_name='Corrected upward short wave irradiance, clear dome', standard_name='upwelling_shortwave_flux_in_air'),
                        parameter('RED_UP_C',units='W m-2',frequency=1,number=1023,long_name='Corrected upward short wave irradiance, red dome'),
                        parameter('IR_UP_C',units='W m-2',frequency=1,number=1024,long_name='Corrected upward long wave irradiance')]
        #self.name='GRFLUX'
        self.version=1.00
        fort_cal.__init__(self,dataset)
