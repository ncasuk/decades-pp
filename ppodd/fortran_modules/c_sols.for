C-----------------------------------------------------------------------------
C ROUTINE      C_SOLS    SUBROUTINE  FORTVAX
C
C PURPOSE     CALIBRATE  PYRANOMETER & PYRGEOMETER RAW SIGNALS AND THERMISTORS.
C
C DESCRIPTION  Apply calibration coefficients to RAW parameters 81-89 and 91-99
C	       to obtain uncorrected values of signal flux, zero offset signal
C              (W/m-2) and thermistor output (deg K)  for  each  of  the 
C              upward-facing and downward-facing sets of: clear dome & red dome
C              pyranometers and pyrgeometer.
C
C NOTE             The actual configuration is specified by the array
C                  ICONF, which has six elements whose meaning interpreted as:
C                    1,4 : Clear dome pyranometer  (upper/lower)
C                    2,5 : red    "       "           "     "
C                    3,6 : Silver "   pyrgeometer     "     "
C                  (normally: ICONF(1-3) Upper instruments.
C                             ICONF(4-6) Lower instruments.)
C                  
C                  This value assists the processing of each instrument by 
C                  selecting the correct range checking values to use.
C                  Should the configuration aboard the aircraft be changed
C                  the array ICONF should be adjusted accordingly by adding
C                  offsets to the second calibration constant in the constants
C                  file. 
C                  e.g. If  the second constant for, say, the second instrument
C                  was changed from 1.23456E-1 to 21.23456E-1, the offset would
C                  decode to "2" after decoding.
C                  This is assigned to ICONF(2) and would imply that the 
C                  'channel' contained raw flux, zero-offset and thermistor 
C                  values for a red dome - rather than clear dome - pyranometer.
C                  and should be range-checked for that type of output only.
C
C METHOD       For each RAW parameter to be calibrated, for the six instruments:
C
C              1. Check all its required constants are present (Flag <3)
C                 (if not, the calibration of that parameter will not proceed)
C                 [Also check that the normal configuration of instruments is to
C                 be used. Any changes are indicated by the presence of a large
C                 offset to the second calibration constant for any instrument.
C                 If this is present the offset is decoded to generate a revised
C                 ICONF indicator for that instrument. See note below.]
C              2. Obtain the raw signal/zero value and float the result.
C              3. Calibrate by applying the appropriate instrument cal in RCALB
C                 (which is loaded from the RCONST arguments) to both raw 
C                 signal flux and zero offset, which  use the same coefficients
C                 The gains are in W/m-2 /DRS count. DRS counts are related
C                 to radiometer output Voltage.
C                 Note that the output Voltage from the instrument  is the
C                 value after being amplified by the head amplifier.
C              4. Range check and Rate-of-change check: (S/R QCPT)
C                 - the calibrated signal (Wm-2)
C                 - Zero offset           (DRS units)
C                 - temperature           (deg K)  
C
C              5. Calibrate the thermistor input using two RCALB coefficients.
C                 Add 273.15 deg to thermistor results to express the 
C                 instrument thermopile temperature in degrees Kelvin.
C              6. Check the result is within pre-defined limits
C              7. Set the calibrated values flag bits (16+17) as follows:
C                         0: Good data  
C                         1: Data of lower quality
C                         2: Data probably faulty, exceeding limits
C                         3: Data absent or known to be invalid.
C
C VERSION      1.04 250692   A D HENNINGS
C
C ARGUMENTS       RCONST(1)  - REAL*4 IN  Upper Clear dome Signal & Zero const.
C              *  RCONST(2)  - REAL*4 IN  Upper Clear dome Signal & Zero gain.
C                 RCONST(3)  - REAL*4 IN  Upper Clear dome Thermistor: constant
C                 RCONST(4)  - REAL*4 IN  Upper Clear dome Thermistor: coeff x.
C                 RCONST(5)  - REAL*4 IN  Upper Red   dome Signal & Zero const.
C              *  RCONST(6)  - REAL*4 IN  Upper Red   dome Signal & Zero gain.
C                 RCONST(7)  - REAL*4 IN  Upper Red   dome Thermistor: constant
C                 RCONST(8)  - REAL*4 IN  Upper Red   dome Thermistor: coeff x.
C                 RCONST(9)  - REAL*4 IN  Upper I/R   dome Signal & Zero const.
C              *  RCONST(10) - REAL*4 IN  Upper I/R   dome Signal & Zero gain.
C                 RCONST(11) - REAL*4 IN  Upper I/R   dome Thermistor: constant
C                 RCONST(12) - REAL*4 IN  Upper I/R   dome Thermistor: coeff x.
C                 RCONST(13) - REAL*4 IN  Lower Clear dome Signal & Zero const.
C              *  RCONST(14) - REAL*4 IN  Lower Clear dome Signal & Zero gain.
C                 RCONST(15) - REAL*4 IN  Lower Clear dome Thermistor: constant
C                 RCONST(16) - REAL*4 IN  Lower Clear dome Thermistor: coeff x.
C                 RCONST(17) - REAL*4 IN  Lower Red   dome Signal & Zero const.
C              *  RCONST(18) - REAL*4 IN  Lower Red   dome Signal & Zero gain.
C                 RCONST(19) - REAL*4 IN  Lower Red   dome Thermistor: constant
C                 RCONST(20) - REAL*4 IN  Lower Red   dome Thermistor: coeff x.
C                 RCONST(21) - REAL*4 IN  Lower I/R   dome Signal & Zero const.
C              *  RCONST(22) - REAL*4 IN  Lower I/R   dome Signal & Zero gain.
C                 RCONST(23) - REAL*4 IN  Lower I/R   dome Thermistor: constant
C                 RCONST(24) - REAL*4 IN  Lower I/R   dome Thermistor: coeff x.
C             (*  also contains an offset evaluated to ICONF() ).
C
C                 IFRQ(par)  _ INT*4  IN  Input frequency of each sample. 
C                 IRAW(n,par)- INT*4  IN  Raw instrument voltage conversion.
C                                         (samples n=1; par=81-89, 91-99)
C                 RDER(op,opar)REAL*4 OUT Raw flux signal, zero-offset signal
C                                         and instrument temperature.
C                                         (samples op=1; opar=673-690)
C
C
C SUBPROGRAMS  ITSTFLG, ISETFLG
C
C FILES        none
C
C REFERENCES   Equations from MRF Instrument section.
C
C CHANGES      020490 Revised  range limits introduced.                 ADH
C              100191                                                   ADH
C                     a) Range limits revised to allow for Pyranometer changes
C               "     b) New arrays to hold raw input, constants etc for
C                        more straightforward indexing.
C               "     c) Include ICONF to aid reconfiguring instrument types.
C              010891 Range limits for ZERO now in terms of DRS units, revised
C                     limits in Wm-2 for signal.
C              030292 Rates of change checks instituted on all BBR inputs.  ADH
C              120698 Bug fixed in quality control processing when using non-
C                     standard configurations. MDG/WDNJ
C              270600 I/R signal maximum increased to stop flagging good data
C                     value arbitary, as no explanation of numbers found.
C                     1050. > 1500. DAT
C              V1.06  02/10/02  Changed to use 16 bit DRS data.
C              V1.07  27/11/02  Now takes X0 sensitivity constant as well as X1
C              V1.08  22/07/04  Bug so doesn't crash if first data flagged 3
C              V1.09  13/08/04  Quality Control zero limits increased for 
C                               16 bit data
C------------------------------------------------------------------------------
      SUBROUTINE C_SOLS  (IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.09'

      IMPLICIT NONE
                                                !Argument arrays holding
      INTEGER*4 IRAW(64,512),IFRQ(512)          !raw samples, frequencies
      REAL*4  RCONST(64),RDER(64,1024)          !constants and derived output

!     Local pointers,indices

      LOGICAL   OFIRST/.TRUE./                  !First pass through routine?
      LOGICAL   VALCON                          !Validity of constants.      

      INTEGER*4 INST,                           !Instrument 'channel' (1-6)
     -          I,                              !Index for iteration
     -          IQ,                             !Index for QC use.
     -          IX,                             !Index (used for output par no)
     -          IPAR,                           !A RAW (input) parameter number
     -          IROW,                           !Row index (generally used)
     -          ICOL,                           !Column "     "        "
     -          ITSTFLG,                        !Integer function to test flag
     -          ISAMP,                          !Sample index;
     -          IFLAG(6,4),                     !Flag: validity of each constant
     -          IFLAGV,                         !Flag: value of a raw data samp
     -          IFLAGQ,	                        !Flag: value after Q/C   check
     -          INLIST(6,3),                    !Input RAW param numbers.
     -          IOUTLIST(6,3),                  !Output par no.(uncorr. calib)
     -          ICONF(6)                        !Instrument configuration indic
      REAL*4    RVAL,                           !A calibrated value.
     -          RTEMP,                          !temporary value for constant  
     -          RSEC,                           !Q/C : Current time in seconds 
     -          FLTM(6,3),                      !Q/C : second of prev good value
     -          FLVL(6,3),                      !Q/C : previous good value.
     -          FMAX(6,3),                      !Q/C : range:Maxima
     -          FMIN(6,3),                      !Q/C : range:Minima
     -          FROC(6,3),                      !Q/C : Rate of change limit
     -          FERRMX,                         !Q/C : Limit of QCPT errors.
     -          FERR(6,3),                      !Q/C : Count of prev err. values
     -          RCALB(6,4)                      !Calibration constants=RCONST()

      DATA ((INLIST(IROW,ICOL),ICOL=1,3),IROW=1,6)  !Raw parameter numbers
     -           /81,84,87,
     -            82,85,88,
     -            83,86,89,
     -            91,94,97,  
     -            92,95,98,
     -            93,96,99/

      DATA ((IOUTLIST(IROW,ICOL),ICOL=1,3),IROW=1,6)!Derived parameter numbers
     -           /673,676,679,    !Upper signals       (Clear,Red,I/R)
     -            674,677,680,    !      zeroes          "     "    "
     -            675,678,681,    !      thermistors     "     "    "
     -            682,685,688,    !Lower signals         "     "    "
     -            683,686,689,    !      zeroes          "     "    "
     -            684,687,690/    !      thermistors     "     "    "
                                                                    
      DATA ICONF   /1,2,3,4,5,6/  !Instrument configuration indicators
                                  !1,2,3:- Upper:Clear, Red pyran. Silver Pyrg
                                  !4,5,6:- Lower:  "     "    "      "     "

!     Q/C Initialisation: Time (seconds from midnight) of last good data point.

      DATA FLTM /18*0./

!     Q/C Initialisation: Value of last good data point.

      DATA FLVL /18*0./

!     Q/C Range limits (Max) for pyranometers (Clear and Red dome) and
!         Pyrgeometers in each location on aircraft.

      DATA ((FMAX(IROW,ICOL),ICOL=1,3),IROW=1,6) 
     -         /2000.0, 8500.0,  315.,  !Upper:  Clear: signal, zero, thermistor
     -          1000.0, 8800.0,  315.,  !        Red  :   "     "        "
     -          1500.0,33800.0,  315.,  !        I/R  :   "     "        "
     -          2000.0, 8500.0,  315.,  !Lower:  Clear:   "     "        "
     -          1050.0, 8800.0,  315.,  !        Red  :   "     "        "
     -          1050.0,33800.0,  315./  !        I/R  :   "     "        "

!       Units :  Wm-2  DRS bits. Deg K

!     Q/C Range limits (Min) for pyranometers (Clear and Red dome) and
!         Pyrgeometers in each location on aircraft.

      DATA ((FMIN(IROW,ICOL),ICOL=1,3),IROW=1,6) 
     -         /  40.0 , 7700.0, 215.,  !Upper: (Clear) signal, zero, thermistor
     -            20.0 , 7400.0, 215.,  !       (Red)
     -            10.0 ,31100.0, 215.,  !       (I/R)
     -            40.0 , 7700.0, 215.,  !Lower: (Clear) signal, zero, thermistor
     -            20.0 , 7400.0, 215.,  !       (Red)
     -            10.0 ,31100.0, 215./  !       (I/R)
!       Units :  Wm-2  DRS bits. Deg K
     
!     Q/C Initialisation: Maximum rate of change allowed between points.

      DATA ((FROC(IROW,ICOL),ICOL=1,3),IROW=1,6) 
     -         /  50.0 ,  160.0,  1.0,  !Upper: (Clear) signal, zero, thermistor
     -            50.0 ,  160.0,  1.0,  !       (Red)
     -            50.0 ,  160.0,  1.0,  !       (I/R)
     -            50.0 ,  160.0,  1.0,  !Lower: (Clear) signal, zero, thermistor
     -            50.0 ,  160.0,  1.0,  !       (Red)
     -            50.0 ,  160.0,  1.0/  !       (I/R)
!       Units :  Wm-2    DRS bits Deg K
     


!     Q/C Initialisation: Value of last good data point.

      DATA FERR /18*0./   !Init counts of current S_QCPT errors.
      
!     Q/C Initialisation: !Limit count of S_QCPT errors before resetting

      DATA FERRMX/3.0/  
*------------------------------------------------------------------------------
!+    Load constants and their flags into RCALB and strip out any configuration
!     change indicators.
!     (Transfer RCONST input into 2-dim array RCALB
!     Look at Measure/Zero cal constant;
!     If |constant| > 1.0, an offset has been added to constant - which has
!     to be interpreted as a replacement ICONF indicator for the instrument.
!     The constant is then restored to its 'proper' value.)
!
      SAVE
      IF (OFIRST) THEN
        OFIRST =.FALSE.                               !Only need to load once

        DO IROW =0,5
          DO ICOL =1,4
              RTEMP = RCONST (IROW*4+ICOL)            !The constant.
              IFLAG(IROW+1,ICOL) = ITSTFLG(RTEMP)     !Get its flag.

              IF ( ICOL.EQ.2) THEN                    !Configuration change?
                                                      !An offset would have been
                 IF (ABS(RTEMP) .GE. 1.0) THEN        !added to FIRST constant.
                   ICONF(IROW+1)=RTEMP                !Indicates a different
                   RTEMP= RTEMP-ICONF(IROW+1)         !instrument has been 
                   ICONF(IROW+1)= IABS(ICONF(IROW+1)) !installed this flight
                 ENDIF
              ENDIF      

              RCALB(IROW+1,ICOL) = RTEMP              !Retain the constant
          END DO
        END DO
      ENDIF
!-

!+    Process instruments: Execute loop once for each instrument, calculating 
!     its signal/zero fluxes and temperature.

      RSEC = RDER(1,515)                             !Current time, seconds.

      DO INST=1,6                                    !Six radiometers, in turn
        IF (IFRQ( INLIST(INST,1)) .GT. 0) THEN       !Frequency set? ... else
                                                     !skip instrument altogether

        DO I = 1,3                          !Process: Signal(1),Zero(2),Therm(3)
          IPAR = INLIST(INST,I)                      !Raw parameter no. selected
          IQ   = ICONF (INST)                        !Index to item in QC arrays

!         .. Check the required constants are valid for selected channel
  
          VALCON = .FALSE.
          IF ( I .EQ. 1 .OR. I .EQ.2) THEN           !Sig, Zero need two consts
            IF (IFLAG(INST,1) .LT. 3
     -          .AND.IFLAG(INST,2) .LT. 3 ) VALCON= .TRUE. 
          ELSEIF (I .EQ. 3)  THEN                    !Thermistor takes two
            IF (IFLAG(INST,3) .LT. 3            
     -          .AND.IFLAG(INST,4) .LT. 3 ) VALCON= .TRUE.
          ENDIF


          IF (VALCON) THEN                           !proceed if constants valid
            DO ISAMP=1,IFRQ( IPAR)                   !For each sample(nom. 1hz)
              IFLAGV=0
              IF(IRAW(ISAMP,IPAR).EQ.0) IFLAGV=3     !Get its validity flag
              IF(IRAW(ISAMP,IPAR).EQ.'FFFF'X) IFLAGV=3 !Get its validity flag

              IX = IOUTLIST(INST,I)                    !Output parameter no.
              IF(IFLAGV.LT.3)THEN                      !If valid, calibrate it:
                RVAL =FLOAT(IRAW(ISAMP,IPAR))          !extract uncalib value

                IF (I .EQ. 1 .OR. I .EQ.2) THEN        !is it a Signal or Zero?
                  RDER(ISAMP,IX )
     -              =RVAL*RCALB(INST,2)+RCALB(INST,1)  !yes, units Wm-2

                ELSEIF (I .EQ. 3)  THEN                !... or a Thermistor ?
                  RDER(ISAMP,IX )                      !     units deg K
     -              =RVAL*RCALB(INST,4)+RCALB(INST,3) +273.15 
                ENDIF                                    

!               Q/C the value:  calibrated Signal (as Wm-2),  Zero (as raw DRS)
!               or thermistor(deg K), for range limits and rate of change.
 
                                             !Put value into placeholder RTEMP
                IF (I .EQ. 1 .OR. I .EQ. 3 ) THEN        !Signal & thermistor.
                   RTEMP = RDER (ISAMP,IX)               !calibrated value -> QC

                ELSEIF (I.EQ.2) THEN                     !Zero.   
                   RTEMP = RVAL                          !raw value -> QC
                ENDIF                                    

                CALL S_QCPT( RSEC,                !Current second from midnight
     -                       FLTM(INST,I),        !Previous 'good' second
     -                       RTEMP,               !Current data point 
     -                       FLVL(INST,I),        !Previous 'good' data point
     -                       FMAX(IQ,I),          !Max range check value
     -                       FMIN(IQ,I),          !Min range check value
     -                       FROC(IQ,I),          !Rate of change value
     -                       FERRMX    ,          !Error count reset limit
     -                       FERR(INST,I),        !count of current errors
     -                       IFLAGQ)              !QC flag. 0:Good  2: failed.

                IFLAGQ = MAX (IFLAGQ,IFLAGV)      !Retain 'worst' flag.

              ENDIF                               !End section (if flag < 3)
                                                 
	      CALL ISETFLG( RDER(ISAMP,IX),IFLAGQ)!Flag derived value
            END DO                                !fetch next (up to IFRQ)sample
          END IF                                !End section. (constants valid)

        END DO            	              !Signal; Zero; Therm. processed.
        ENDIF                                 !Correct frequency was set.

      END DO                               !Next channel no. (instrument). 

      RETURN
      END		
