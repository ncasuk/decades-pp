C------------------------------------------------------------------------------
C ROUTINE  	   C_RFLUX        SUBROUTINE FORTVAX       [C_RFLUX.FOR]
C     
C PURPOSE 	   CORRECT RAW FLUXES FOR PYRANOMETERS AND PYRGEOMETERS
C                  
C DESCRIPTION      Flux corrections are performed for the six instruments
C                  which are normally configured:
C                  Upward-facing :-  Clear dome and Red dome pyranometers.
C                                    Silver dome pyrgeometer.
C                  Downward-facing:- Clear dome and Red dome pyranometers.
C                                    Silver dome pyrgeometer.
C                  
C                  The actual configuration is specified by the preset array
C                  ICONF, which has six elements whose meaning interpreted as:
C                    1,4 : Clear dome pyranometer  (upper/lower)
C                    2,5 : red    "       "           "     "
C                    3,6 : Silver "   pyrgeometer     "     "
C                  (normally: ICONF(1-3) Upper instruments.
C                             ICONF(4-6) Lower instruments.)
C                  
C                 Check that the normal configuration of instruments is to
C                 be used. Any changes are indicated by the presence of a large
C                 offset to the last calibration constant for any instrument
C                 (i.e. the obscurer indicator constant).
C                 If this is present the offset is interpreted as a revised
C                 ICONF indicator for that instrument. See note below.]
C
C                  n.b. Lower instruments were fitted w.e.f. Flight H797
C                       Upper instruments were fitted w.e.f. Flight H842
C
C                  This value solely determines the control path through the
C                  routine for the processing of each instruments inputs.
C                  Should the configuration aboard the aircraft be changed
C                  the array ICONF should be adjusted accordingly.
C                  e.g. If ICONF(1) was modified := 2; it would imply that the
C                  'channel' contained raw flux, zero-offset and thermistor
C                  values for a red dome - rather than clear - pyranometer.
C                  The value of ICONF(1) i.e. 2 would determine the processing
C                  path, the selection of the appropriate set of constants
C                  to apply for correction and the range checking.
C
C NOTE             CHANGES FROM STANDARD CONFIGURATION.                    
C                  Should the configuration of BBR instruments aboard the 
C                  aircraft be changed e.g. swapping a red dome for clear dome,
C                  the array ICONF is  adjusted accordingly. The mechanism used
C                  is to add an offset to the sixth constant in the calibration 
C                  constants file (i.e. the obscurer) for that instrument.
C                  Example: If the second 'channel' (inputs 674,677,680) which
C                  in the standard configuration is a red dome pyranometer,
C                  was replaced with a second clear dome instrument, the sixth
C                  constant for the second line of the constants for C_RFLUX 
C                  would be changed from 1.0000E+0 to 21.0000E+0, the offset 
C                  decodes to "2" when detected by this program. 
C                  This is assigned to ICONF(2) and would imply that the 
C                  'channel' inputs contain raw flux, zero-offset and thermistor 
C                  values for a red dome - rather than clear dome - pyranometer,
C                  and should be range-checked for that type of output only.
C                 
C                  Corrections applied:
C                  --------------------
C                  Pyranometers (Clear and Red dome) are corrected for:
C                  - Subtraction of a zero offset (mean over past 10 seconds)
C                  - Attitude (pitch and roll) -Upper instruments only.
C                    test if flux is above a critical limit indicating a direct
C                    solar beam component.
C                      If not direct, assume diffuse and apply no attitude corr.
C                      If DIRECT, a geometric correction is used to "level"
C                      the instrument to produce the equivalent hemispheric
C                      downward flux through a horizontal surface (without
C                      inclusion of diffuse component).
C                      The ratio of the Direct:Direct+Diffuse components is
C                      assumed to be 0.95 at present. This value could be
C                      optimised for a particular atmosphere depending on the 
C                      turbidity as a function of height.
C                      
C                      Correct for COSINE effect. (MRF Technical note No.7).
C                      [Pitch and roll offsets of the instrument thermopiles
C                      relative to the aircraft INS platform are derived in
C                      flight by flying a box pattern in cloud-free skies -
C                      These offsets are then used in addition to the INS pitch
C                      and roll (meaned over two seconds). (See MRF Technical
C                      note No 4.) and these values are supplied as arguments 
C                      four and five in each set of CONSTANTS below.
C                  - Time constant of thermopile relative to INS. The mean of
C                    last two seconds of INS pitch/roll angles are used in the
C                    attitude correction, giving an effective difference of
C                    0.5 seconds.
C                  - Correct flux output for proportion of hemispheric dome 
C                    obscured by indicated obscurer pillar. (Rawlins 1986).
C
C                  Pyrgeometers (IR) are corrected for:
C                  - Zero offset (mean over past 10 seconds)
C                  - Temperature sensitivity  (Coefficients in CONSTANTS below)
C                  - Linear dependence 0.2% per degree with sensitivity defined 
C                    as unity at zero C. applied to signal. (MRF Int note No 50)
C                  - Calculation of flux (sigma T^4 correction)
C                    Flux = signal +(sigma* Tsink^4) 
C                    where sigma = Stefan-Boltzmann constant.
C                  _ Upper instrument is corrected for dome transmission
C                    effects (MRF Tech note 3)
C
C VERSION	   1.17 05-09-07   D Tiddeman  
C
C METHOD           1. First time routine is called, assign constants to named
C                     program variables/arrays.
C                     Decide on basis of input constants whether upper instr.
C                     data is available to be processed. 
C                  2. Derive/convert any intermediate results used multiply
C                     within several code sections following.
C                  3. Derive running mean zero-offsets over the past 10 seconds
C                     for each instrument 
C                     
C                  4. Calculate mean pitch and roll values for the current 
C                     second and use them to derive running means for the past 
C                     two seconds.
C                  5. Correct thermistor temperatures for non-linearity.
C                  6. Cycle through each of six instrument input channels.
C                     Use the control variable in ICONF() to select execution
C                     of appropriate code sections.
C                     In all cases; derive a signal zero-offset and reduce the
C                     signal flux by this amount.
C                     Apply temperature-dependance corrections to pyranometers.
C                     For upward-facing pyranometers the 'critical' value to
C                     discriminate between diffuse and direct-sun conditions is
C                           FCRIT  = 920.*(COS(ZENRAD))**1.28 
C                     where ZENRAD : solar zenith angle (in radians)
C                     [N.B. This approximates to the 'German' equation but is
C                      simpler, and does not produce negative values at low
C                      Sun elevations]. 
C                     Correct flux output for proportion of hemispheric dome 
C                     obscured by indicated obscurer pillar. (Rawlins 1986).
C                                                          
C                  7. Range check flux output and set a flag accordingly.
C                     Apply flag values to resulting flux output dependent on
C                     relevant flag settings.
C                  
C ARGUMENTS        RCONST(1),( 7)..(31)  - REAL*4 IN Temperature Sens. coeff a
C                  RCONST(2),( 8)..(32)  - REAL*4 IN Temperature Sens. coeff b
C                  RCONST(3),( 9)..(33)  - REAL*4 IN Temperature Sens. coeff c
C                  RCONST(4),(10)..(34)  - REAL*4 IN Pitch offset of Instrument
C                  RCONST(5),(11)..(35)  - REAL*4 IN Roll  offset of Instrument
C                  RCONST(6),(12)..(36)  - REAL*4 IN Obscurer pillar type.
C                  
C                  RDER(1,par)             REAL*4  IN Six raw flux signals W/M-2
C                             (par=673-675,682-684)
C                  RDER(1,par)             REAL*4  IN six zero-offsets   (W/M-2)
C                             (par=676-678,685-687)
C                  RDER(1,par)             REAL*4  IN six instr. temperatures K
C                             (par=679-681,688-690)
C                  RDER(32,560)            REAL*4  IN INS Roll      (degrees)
C                  RDER(32,561)            REAL*4  IN INS Pitch     (degrees)
C                  RDER(32,562)            REAL*4  IN INS heading   (degrees)
C                  RDER(1,642)             REAL*4  IN Solar azimuth (degrees)
C                  RDER(1,643)             REAL*4  IN Solar zenith  (degrees)
C
C                                                              Pos. Dome  Units
C                  RDER(1,1019)            REAL*4 OUT Corrected Upp Clear W/m-2 
C                  RDER(1,1020)            REAL*4 OUT flux.      "  Red dome " 
C                  RDER(1,1021)            REAL*4 OUT            "  I/R      "
C                  RDER(1,1022)            REAL*4 OUT           Low Clear    "
C                  RDER(1,1023)            REAL*4 OUT            "  Red dome " 
C                  RDER(1,1024)            REAL*4 OUT            "  I/R      " 
C
C SUBPROGRAMS  	   ITSTFLG, ISETFLG, S_RUNM, CORR_THM, RMEANOF, CIRC_AVRG
C
C REFERENCES 	   MRF Internal note  4.
C                   "     "      "   12.
C                   "     "      "   31.
C                   "     "      "   50.
C                   "     "      "   56.
C                  MRF Technical note 3. Pyrgeometer Corrections due to Dome
C                                        Transmission.  February 1991 Kilsby
C                  MRF Technical note 7. Report of Broad-band radiative fluxes
C                                        working group. 17/12/91      Saunders
C                  MRF Technical note 8. Pyramometer calibrationsin Ascension
C                                        of Feb.1992.   4/6/92        Seymour
C                  RAWLINS  R        D/Met.O.(MRF)/13/1      1986.
C                  SAUNDERS R         "        "   "         21/3/90
C                  SAUNDERS R        M/MRF/13/5              22/7/92
C                  
C CHANGES          10/01/91 A.D.Hennings.
C                    Ability to change ICONF to when reconfiguring instrument
C                    fit on A/C using the constants file.
C                  10/01/91 Pitch & Roll averaging changed from 3 to 2 seconds.
C                  25/01/91 Flags assessment changed; use of new flag IFLAG_SUN 
C                  29/01/91 Roll limit checking:replace ROLBAR with ABS(ROLBAR).
C                           Flags assessment changed; IFLAG_OUTPUT being max of
C                           (signal,Pitch,Roll,Zenith) flags.               
C                  30/07/91 FCRIT for Red dome now only used if no clear dome 
C                  16/10/91 Corrected pyrgeometer temp sensitivity correction
C                  20/01/92 Use INS heading instead of obsolete Omega heading.
C                  03/02/92 New subroutine CIRC_AVRG to calc INS mean heading
C                  21/07/92 Levelling of upper pyranometers changed to use 
C                           direct beam component, and cosine effect included.
C                           Recommendations of MRF Tech note 7.   (V1.13)
C                           references to Tech note 8. and M/MRF/13/5
C                  24/07/92 Pyrgeometer corrections for Dome transmission. 
C                           (Downwelling) MRF Tech note 3.             
C                  17/01/96 D Lauchlan
C                           Unused variables removed
C                  22/12/97 W D N Jackson, Flags cleared from all data before
C                           use.
C                  11/08/98 W D N Jackson, Upper pyranometer obscurer
C                           corrections changed to correct values.  The
C                           values have been incorrect in all previous versions
C                           of C_RFLUX. The error is only small. (Source 
C                           P Hignett)
C                  05/09/07 D TIDDEMAN Will use GIN attitude if available rather 
C                           then INU
C
C------------------------------------------------------------------------------
      SUBROUTINE C_GRFLUX     (IRAW,IFRQ,RCONST,RDER)          
CDEC$ IDENT 'V1.17'
C
      IMPLICIT NONE
      INTEGER*4 IRAW(64,512), IFRQ(512)
      REAL*4    RCONST(64), RDER(64,1024)

      INTEGER   ITSTFLG 
      REAL      CIRC_AVRG              !Function returning average of angles
C
C working input data and processed output arrays
C
      REAL*4 ZIN(6),                   !Zero offset samples
     &       RTHM(6),                  !Uncorrected thermistor samples
     &       RFLX(6),                  !Uncorrected flux samples
     &       THM(6),                   !Corrected thermistor samples
     &       FLX(6),                   !Corrected flux samples
     &       PITINS,ROLINS,            !Input pitch & roll (mean of 32hz) degs
     &       PITCH ,ROLL,              !Corrected pitch and roll (Rads)
     &       HDGINS,                   !Input INS heading (degrees)
     &       SOLAZM,SOLZEN,            !Input Solar Azimuth & zenith angle. Rads
     &       HDGRAD,                   !Convert Omega heading to radians
     &       ZENRAD,                   !Convert Solar Zenith ang to radians
     &       AZMRAD,                   !Convert Solar Zenith ang to radians
     &       SUNHDG                    !Sun Heading (Sol Azm-A/c Omega hdg.)Rads
C
C C0NSTANT information
C
      REAL*8    TSA(6)                       !Temperature senstvty alph,beta gm
     -         ,TSB(6)                       !
     -         ,TSG(6)                       !
     -         ,PITOFF(6)                    !Angular offset   " Pitch.
     -         ,ROLOFF(6)                    !Angular offset   " Roll.
      INTEGER*4 IOBTYP(6)                    !Obscurer type (0: none 1:short
                                             !               2: tall)
C
C flags signifying validity of input arguments and derived values.
C
      INTEGER*4 IFLAG_ANG              !Test of sun angle too low
     -         ,IFLAG_ROLL             !INS Roll
     -         ,IFLAG_PIT              !INS Pitch
     -         ,IFLAG_AZM              !Solar azimuth angle
     -         ,IFLAG_ZEN              ! "    zenith   "
     -         ,IFLAG_INHDG            !INS Heading
     -         ,IFLAG_SHDG             !Sun hdg.  Max(IFLAG_AZM and IFLAG_INHDG)
     -         ,IFLAG_SUN              !Sun attitude Max(Pitch/Roll/Zen/Ang)
     -         ,IFLAG_FLX              !Raw flux input
     -         ,IFLAG_THM              !Corrected thermistor 
     -         ,IFLAG_ZER              !Meaned zero-offset
     -         ,IFLAG_SIGNAL           !Max of (IFLAG_FLX and IFLAG_ZER)
     -         ,IFLAG_CORRN            !Max of (all correction flags relevant)
     -         ,IFLAG_OUTPUT           !Max of (IFLAG_SIGNAL and IFLAG_CORRN)
                                       ! and result of range tests on output.
     -         ,IDUM                   !Argument, return value of no interest.
     &         ,IHDG,IPIT,IROL

C  arrays , counters and pointer arguments for Zero-offset mean derivation

      REAL*4    ZBAR(6)                  !Output means over past 10 seconds
      REAL*4    ZBUF(10,6), ZSUM(6)      !Buffer and total holder
      INTEGER*4 IZP(6),     IZCNT(6)     !Buffer pointer and counter of samples.
      DATA      IZP/6*1/,   IZCNT/6*0/   !Initialise ptrs, count of good samples
C
C  arrays , counters and pointer arguments for Pitch and Roll mean derivation
C
      REAL*4    PITBAR,ROLBAR            !Output means over past 2 seconds. degs
      REAL*4    PBUF(3),RBUF(3),PSUM,RSUM!Buffers and total holders
      INTEGER*4 IPPT,IRPT,IPCNT,IRCNT    !Buffer pointer and counter of samples.
      DATA      IPPT,IRPT/1,1/           !Initialise buffer pointers
      DATA      IPCNT,IRCNT/2*0/         !Initialise count of good samples


      LOGICAL   OFIRST/.TRUE./           !Indicator as to first time through rtn

      INTEGER*4 ICONF(6)                !6 input channels (instruments). 
      DATA      ICONF/                  !Control variables- Currently set as:
     -                 1,               !Upper clear dome pyranometer in chan 1
     -                 2,               !      red   dome pyranometer in chan 2
     -                 3,               !      silverdome pyrgeometer in chan 3
     -                 4,               !Lower clear dome pyranometer in chan 4
     -                 5,               !      red   dome pyranometer in chan 5
     -                 6/               !      silverdome pyrgeometer in chan 6

      REAL*4   RMAXFLX(6),RMINFLX(6)    !Range limits on corrected flux.
      DATA     RMAXFLX/                 !Max. admissible corrected flux output
     -             1380.,               !Upward-facing  clear  dome pyranometer
     -              700.,               !               red    dome pyranometer
     -              550.,               !               silver dome pyrgeometer
     -             1380.,               !Downward-facing clear dome pyranometer
     -              700.,               !                red   dome pyranometer
     -              750 /               !                silverdome pyrgeometer
      DATA     RMINFLX/                 !Min. admissible corrected flux output
     -              -20.,               !Upward-facing  clear  dome pyranometer
     -              -20.,               !               red    dome pyranometer
     -              -20.,               !               silver dome pyrgeometer
     -              -20.,               !Downward-facing clear dome pyranometer
     -              -20.,               !                red   dome pyranometer
     -               50./               !                silverdome pyrgeometer

      REAL*4    THETA,RCOSTH              !Angle between Sun and Normal to Instr
      REAL*4    ROLLIM,THTMAX             !Roll max limit: Sun-angle max limit
      PARAMETER (ROLLIM=7.0, THTMAX=80.0) !in degrees.
C
C local variables.
C
      LOGICAL   UPPERS                   !Upper instruments fitted?
      INTEGER*4 IS,IE                    !First and last instrument 'channel'
C      SAVE IS,IE
      INTEGER*4 IN,I                     !Instrument (channel); loop index
      REAL*4    FCRIT,FCRITVAL           !Critical flux value (direct/diffuse)
      REAL*8    SIGMA,                   !Stefan-Boltzmann constant.        
     -          FOBSC,                   !Obscurer value for any instrument
     -          TH,                      !Place holder for corrected thermistor
     -          FL                       !Place holder for corrected flux
      REAL*4    DEG2RD                   !Degrees to radians conversion factor
      REAL*4    RTEMP,                   !Temp vrb: used with ICONF changes.
     -          ROBTYP                   ! "    " : specify Obscurer type used.
      INTEGER*4 ITYPE,ISIG,ICOR          !Indices to data tables
C
C  levelling corrections
C                                       !Select INDX of solar zenith angle
      INTEGER*4 INDX                    !where  INDX = NINT(SOLZEN/10) + 1
                                        !INDX
                                        !1-3: (0 -29.9 deg)
                                        !4-6: (30-59.9 deg)
                                        !7-9: (60-89.9 deg)
                                        !10:  (  >89.9 deg)

      REAL*4    CEFF(10)/1.010, 1.005, 1.005, !Correction to pyranometers for  
     &                   1.005, 1.000, 0.995, !COSINE effect dependant on solar
     &                   0.985, 0.970, 0.930, !zenith angle. Determined by expt
     &                   0.930/               !Ref: Tech note 8. Table 4       
                                               
      REAL*4    FDIR(10)/.95,.95,.95,   !(Proportion of flux from direct source
     &                   .95,.95,.95,   !for varying solar zenith angles.)     
     &                   .95,.95,.95,   !Addressed by INDX as above.            
     &                   .95/           !Ref: M/MRF/13/5                       
                                                                               
                                        
C     table of proportion of hemispheric dome obscured by each pillar-type

      REAL*4    ROBSC(3,6)               !Obscurer corrections (Type,Up|Loc)
      DATA ((ROBSC(ITYPE,IN),IN=1,6),ITYPE=1,3)/    !Ref:RAWLINS 1986       
      !    Upper Instruments  |   Lower instruments      
      !Port    Starbd  Centre  Port    Starbd  Centre
     & 00.000, 00.000, 00.000, 00.000, 00.000, 00.000,  ! No pillar (Ind=1)
     & 00.010, 00.010, 00.000, 00.000, 00.000, 00.000,  ! Short "   ( "  2)
     & 00.040, 00.040, 00.000, 00.000, 00.000, 00.000/  ! Tall  "   ( "  3)
! The following lines contain the incorrect upper pyranometer corrections which
! have been used in all previous versions of C_RFLUX (WDNJ 11/8/98).
!     & 00.016, 00.016, 00.000, 00.000, 00.000, 00.000,  ! Short "   ( "  2)
!     & 00.046, 00.046, 00.000, 00.000, 00.000, 00.000/  ! Tall  "   ( "  3)

C     logic table combining two group input flag conditions resulting in an 
C     output flag.

      INTEGER*4 IFLAG_TABLE(0:3,0:3)
      DATA ((IFLAG_TABLE(ISIG,ICOR),ICOR=0,3),ISIG=0,3)/
      !        CORRECTION
      !     0   1   2   3 
      !   -------------------                 See Saunders LM 1990 for
     -      0,  1,  3,  3,  ! 0               details of this table.
     -      1,  2,  3,  3,  ! 1  SIGNAL
     -      2,  2,  3,  3,  ! 2
     -      3,  3,  3,  3  /! 3
      
      PARAMETER (SIGMA  = 5.669E-08)      
      PARAMETER (DEG2RD = 57.295776)     
      SAVE
!-----------------------------------------------------------------------------
!+
!  1. First time routine is called, assign constants to named
!     program variables/arrays.


      IF (OFIRST) THEN   
        OFIRST= .FALSE.
!
!       Prior to Flight H842 no upper radiometers were recorded in this form;
!       hence no data constants are passed to this routine. Check for condition.
!
        UPPERS = .FALSE.
        DO IN = 1 ,18                             !Any non-zero value indicates
        IF (RCONST(IN) .NE. 0.)  UPPERS = .TRUE.  !constants are being passed 
        END DO                                    !for upper instruments too.
!
!       Set 'channel' limits accordingly.
!
        IF (UPPERS) THEN           
          IS = 1                     !all six instrument present
          IE = 6
        ELSE
          IS = 4                     !only lower instruments fitted
          IE = 6
        ENDIF

!                       Put RCONST values into program variables.)
        DO IN = IS,IE
          TSA(IN)    = RCONST((IN-1)*6 +1) !Temperature sensitivity coefficents
          TSB(IN)    = RCONST((IN-1)*6 +2) ! Alpha, Beta, Gamma                
          TSG(IN)    = RCONST((IN-1)*6 +3) !
          PITOFF(IN) = RCONST((IN-1)*6 +4) !Pitch offset of instrument
          ROLOFF(IN) = RCONST((IN-1)*6 +5) !Pitch offset of instrument

!         Check whether the configuration has been modified by examining the 
!         last constant for each instrument (=IOBTYP). If it is >10 an offset
!         has been added to it; identify this and restore correct constant.
!
          RTEMP = RCONST((IN-1)*6 +6)            !Get obscurer value (+offset?)
          IF (ABS(RTEMP) .GE. 10.0) THEN         !An offset has been added.
              RTEMP      = RTEMP/10.             !Bring the offset into the
              ICONF(IN)  = INT(RTEMP)            !truncate range |1 - 6|>ICONF()
              ROBTYP     = (RTEMP-ICONF(IN))*10. !Restore the Obscurer const.
              ICONF(IN)  = IABS(ICONF(IN))       !Config indicator must be +ve.
              IOBTYP(IN) = NINT(ROBTYP)          !assign Obscurer type in use
                                                 !(1: none, 2: short, 3: tall)

          ELSE                                   !use default ICONF values
              IOBTYP(IN) = NINT(RTEMP)           !Obscurer type in use
          ENDIF

        END DO                           !next instrument.
      ENDIF                              !of First-time-through actions.
!-

!+
!  2. Derive/convert any intermediate results used several times
!     within code sections following.
!
!     Put input data into arrays. 

      IF (UPPERS) THEN
        DO IN = 1,3                              !Upper instruments
          RFLX (IN)  = RDER(1,673+IN -1)         !  Signal       w/m-2
          ZIN  (IN)  = RDER(1,676+IN -1)         !  zero         w/m-2
          RTHM (IN)  = RDER(1,679+IN -1)         !  thermistor   deg K
        END DO
      ENDIF

      DO IN = 1,3                                !Lower instruments
        RFLX (IN+3) = RDER(1,682+IN -1)          !  Signal       w/m-2
        ZIN  (IN+3) = RDER(1,685+IN -1)          !  zero         w/m-2
        RTHM (IN+3) = RDER(1,688+IN -1)          !  thermistor   deg K
      END DO

      IROL=560
      IPIT=561
      IHDG=562
      if(ITSTFLG(RDER(1,616)).EQ.0)IROL=616
      if(ITSTFLG(RDER(1,617)).EQ.0)IPIT=617
      if(ITSTFLG(RDER(1,618)).EQ.0)IHDG=618

      HDGINS = CIRC_AVRG( RDER(1,IHDG), 32)       !Mean of INS Heading samples
                                                 !(special for circular values)
      SOLAZM        = RDER(1,642)                !Solar azimuth angle
      SOLZEN        = RDER(1,643)                !Solar zenith   "
!-

!+    set flags for corrections

      IFLAG_INHDG = ITSTFLG (HDGINS)             !Flag of INS   heading
      CALL ISETFLG(HDGINS,0)                     !Strip flag
      IFLAG_ZEN   = ITSTFLG (SOLZEN)             !Flag of solar zenith angle
      CALL ISETFLG(SOLZEN,0)                     !Strip flag
      IFLAG_AZM   = ITSTFLG (SOLAZM)             !Flag of solar azimuth angle
      CALL ISETFLG(SOLAZM,0)                     !Strip flag
      IFLAG_SHDG  = MAX(IFLAG_INHDG,IFLAG_AZM)   !Choose higher heading flag
!-

!+    Convert samples to radians measure.

      HDGRAD = HDGINS /DEG2RD            !Convert INS   heading to radians   
      ZENRAD = SOLZEN/DEG2RD             !Convert Solar Zenith ang to radians
      AZMRAD = SOLAZM/DEG2RD             !Convert Solar Zenith ang to radians
      SUNHDG = AZMRAD - HDGRAD           !Sun Heading (Solar Az-A/C hdg (INS))
!-

      IF (SOLZEN .GT. 0. .AND. SOLZEN .LT.90.)THEN !Prevent exponentiation error
      FCRIT  = 920.*(COS(ZENRAD))**1.28  !Critical flux value (direct/diffuse)
      ENDIF

!+  3. Derive running mean of zero offsets for each instrument over ten seconds
       

      DO I=IS,IE
      CALL S_RUNM(ZBUF(1,I),IZP(I),IZCNT(I),10,ZIN(I),ZSUM(I),ZBAR(I))
      END DO

!-

!+  4. means of 32hz INS PITCH & ROLL arguments for one second.

   
      CALL RMEANOF(32 ,RDER(1,IROL), ROLINS, IDUM) !Mean of INS Roll samples.
      CALL RMEANOF(32 ,RDER(1,IPIT), PITINS, IDUM) !Mean of  "  Pitch  "    .

!    then derive running mean of pitch and roll values. (meaned over two secs)

      CALL S_RUNM(RBUF,IRPT,IRCNT,2,ROLINS,RSUM,ROLBAR)         !Roll
      CALL S_RUNM(PBUF,IPPT,IPCNT,2,PITINS,PSUM,PITBAR)         !Pitch

!     Set Pitch flag, no acceptability test currently used.

      IFLAG_PIT = ITSTFLG (PITBAR)
      CALL ISETFLG(PITBAR,0)                   !Strip flag

!     Roll limit acceptable?

      IFLAG_ROLL= ITSTFLG (ROLBAR)             !Flag of meaned Roll.
      CALL ISETFLG(ROLBAR,0)                   !Strip flag
      IF ( ABS(ROLBAR) .GT. ROLLIM)            !Comparison in degrees
     -    IFLAG_ROLL= MAX(IFLAG_ROLL,1)        !Flag if Roll too great

!  5.  Correct thermistor values for linearity
      
      CALL CORR_THM (RTHM,THM)                 !Input temps deg K, output deg C

!-----------------------------------------------------------------------------

      DO IN = IS,IE                        !Cycle through available instruments

      FOBSC     = ROBSC(IOBTYP(IN),IN)     !select correction for obscurer
      IFLAG_CORRN  = 0                     !Set corrections flag to valid
      IFLAG_FLX = ITSTFLG (RFLX(IN))       !Flag of raw flux input
      CALL ISETFLG(RFLX(IN),0)             !Strip flag
      IFLAG_ZER = ITSTFLG (ZBAR(IN))       !Flag of meaned zero-offset
      CALL ISETFLG(ZBAR(IN),0)             !Strip flag
      IFLAG_THM = ITSTFLG (THM (IN))       !Flag of corrected thermistor.
      CALL ISETFLG(THM (IN),0)             !Strip flag
                                           
      IFLAG_SIGNAL= MAX(IFLAG_FLX,IFLAG_ZER) !Obtain worst of (flx,zero) flag.

      IF (IFLAG_SIGNAL .EQ. 3) THEN         !**** Check Flux validity
         FLX(IN) =  -99.                    !Set output to 'failed' value.
         IFLAG_OUTPUT=  3                   !'Failed' flag.

      !------------------------------------------------------------------
      ELSE                                  ! OK to begin correcting flux.

        FLX(IN) = RFLX(IN) - ZBAR(IN)       !Subtract meaned zero-offset.

!       Perform temperature sensitivity correction. 

          IF (IFLAG_THM .LT. 2) THEN               !Thermistor temperatures
            FL      = FLX(IN)                      !have been corrected and 
            TH      = THM(IN)                      !converted  to C by CORR_THM.
            FLX(IN) = FL /
     -               (1.+ TH*(TSA(IN)         
     -                  + TH*(TSB(IN) 
     -                  + TH* TSG(IN) )))
          ENDIF

        !----------------------------------------------------------------------
        IF (ICONF(IN) .EQ. 3 .OR. ICONF(IN) .EQ. 6) THEN  !*** Pyrgeometers only
        !----------------------------------------------------------------------

!         Perform 'sigma* Tsink^4' correction

          IF (IFLAG_THM .LT. 2) THEN               
            FL = FLX(IN)                           
            FLX(IN) =FL * (1.0/(1.0-FOBSC))+SIGMA*(TH+273.16)**4
          ENDIF 
                                         !Correction to upper Pyrgeometer for   
                                         !dome transmission of downwelling I/R. 
          IF (ICONF(IN) .EQ. 3 )THEN      
            FLX(IN) = FLX(IN) + (-6.0 + 0.0175* FLX(IN))!see Tech note 3. page 2
          ENDIF                                    

          IFLAG_CORRN = IFLAG_THM                       !Relevant corrections
          IFLAG_OUTPUT = IFLAG_TABLE(IFLAG_SIGNAL,IFLAG_CORRN)

        !----------------------------------------------------------------------
        ELSE                      !Upper and Lower Pyranometer corrections 
        !----------------------------------------------------------------------

          IF (ICONF(IN) .EQ. 4 .OR. ICONF(IN) .EQ. 5) THEN  !Lower pyranometers

             FLX(IN)= FLX(IN)*(1.0/(1.0- FOBSC))            !Obscurer corr'n. 
                                                            !All corr'n complete
             IFLAG_CORRN = 0                                !no relevant corrs
             IFLAG_OUTPUT = IFLAG_TABLE(IFLAG_SIGNAL,IFLAG_CORRN)

          ELSE                                              !Upper Pyranometers

!+          Compare incoming flux with Fcrit (Critical value) of expected flux.
!           IF Flux > Fcrit; treat irradiation as being DIRECT.      
!           ELSE             assume it is DIFFUSE irradiation.        
!             (n.b. for RED dome, Fcrit value used is 1/2 normal Fcrit.) 

            FCRITVAL = FCRIT
            IF( ICONF(1)  .NE.  1) FCRITVAL = FCRIT * .5    !1/2 For RED dome.

            IF (FLX(1)  .GT. FCRITVAL) THEN                 !*Direct or Diffuse?
!-
                                                                     
!+            DIRECT is appropriate; check angle between Sun & normal-to-
!             instrument is not > 80 deg, before correction for platform level.

              PITCH=PITBAR + PITOFF(IN) !Combine  A/C mean and Inst offset Pitch
              PITCH=PITCH/DEG2RD        !.. and convert to radians
              ROLL =ROLBAR + ROLOFF(IN) !Combine A/C  mean and Inst offset Roll 
              ROLL = ROLL/DEG2RD        !.. and convert to radians

!             Find angle between Solar zenith and normal-to-Instrument.
                                                        !Ref:Tech note 7 Page 10
                                                        !Derive cosine of angle.
              RCOSTH = SIN(ROLL)*SIN(ZENRAD)*SIN(SUNHDG)
     &              +  COS(ROLL)*COS(PITCH) *COS(ZENRAD)
     &              -  COS(ROLL)*SIN(PITCH) *SIN(ZENRAD)*COS(SUNHDG)
              THETA = ACOS(RCOSTH)            !Express angle in radians

!             Compare with maximum allowable angle. ( must be < 80 Deg)

              IF (THETA .GT. THTMAX/DEG2RD) THEN 
                IFLAG_ANG =  2                !Failed Low sun test; Flag value  
              ELSE
                IFLAG_ANG =  0                !Angle Sun/Instr acceptable.
              ENDIF                                                         

!             Apply levelling correction using combined pitch and roll, if
!             necessary conditions are met:-

              IFLAG_CORRN = MAX (IFLAG_PIT,  IFLAG_ROLL)  !A/c  Attitude flags.
              IFLAG_CORRN = MAX (IFLAG_CORRN,IFLAG_ANG)                       
              IFLAG_SUN   = MAX (IFLAG_SHDG ,IFLAG_ZEN)                       
              IFLAG_CORRN = MAX (IFLAG_CORRN,IFLAG_SUN)                       

              IFLAG_OUTPUT = IFLAG_TABLE(IFLAG_SIGNAL,IFLAG_CORRN)

              IF ( IFLAG_CORRN .LT. 2 .AND. RCOSTH .NE.0.) THEN 

! *OLD VERSION* FLX(IN) = FLX(IN) * (COS(ZENRAD)/RCOSTH)   !levelling correction

!            Correct the flux for attitude of aircraft for direct component of 
!            beam. Also include COSINE effect correction.    (Ref: M/MRF/13/5)

                INDX = NINT(SOLZEN/10) + 1
                INDX = MIN (INDX,10)
                                                              
                FLX (IN) =              FLX(IN)/      
!                         --------------------------------------------
     &            (1.- FDIR(INDX)*(1.- CEFF(INDX)*(RCOSTH/COS(ZENRAD))))
              ENDIF

            ELSE                            !* Critical value, (flux less than.)
                                            !  Diffuse case;   make Obscurer
                                            !  correction if signal is valid.

              IFLAG_CORRN  = MAX(IFLAG_PIT,  IFLAG_ROLL)  
              IFLAG_CORRN  = MAX(IFLAG_CORRN,IFLAG_ZEN)  
              IFLAG_OUTPUT = IFLAG_TABLE (IFLAG_SIGNAL,IFLAG_CORRN)  
              FLX(IN) = FLX(IN)*(1.0/(1.0- FOBSC))

            ENDIF                           !* Critical value for direct?

            IF ( IFLAG_SIGNAL .EQ. 3) THEN 
              FLX(IN) = -99.                     !set  invalid flux to obvious
            ENDIF                                !known value.

          ENDIF                             !**  Upper or Lower pyranometers?
        ENDIF                               !*** pyranometer or pyrgeometer?

!       Perform range checks on valid output fluxes.

        IF (IFLAG_OUTPUT .LT. 3 ) THEN
            IF (FLX(IN) .GT. RMAXFLX(ICONF(IN)) .OR.
     -          FLX(IN) .LT. RMINFLX(ICONF(IN))     ) THEN
               IFLAG_OUTPUT = 2             !Failed, flag result as 'suspect'
            ENDIF
        ENDIF
      ENDIF                                 !**** Flux signal validity?   

!     Assign processed flux to output parameter

      RDER(1,1018 + IN) = FLX(IN)                      !Fill output argument
      CALL ISETFLG (RDER(1,1018 + IN), IFLAG_OUTPUT)   !Set output flag

      IFLAG_CORRN       = 0
      IFLAG_SIGNAL      = 0
      IFLAG_OUTPUT      = 0
      END DO                                           !(..Control value IN)

      RETURN
      END


C-----------------------------------------------------------------------------
C ROUTINE          CORR_THM     SUBROUTINE     FORTVAX            [C_RFLUX.FOR]
C
C PURPOSE          Correct thermistors for non-linearity using a quintic eqn.
C
C DESCRIPTION      The thermistors used in the pyrgeometer/pyranometers all
C                  have characteristic non-linear temperature dependence
C                  due to the manufacturing process. If not corrected for,
C                  this can lead to errors in temperature of up to 1 deg C.
C                  The thermistor manufacturers provide a curve of the the
C                  correction needed to be applied for a range of
C                  temperatures.  A quintic equation has been fitted to this
C                  curve to give the best fit coefficients used by this routine.
C
C METHOD           The routine takes an array of six thermistor values in deg K.
C                  In turn; notes each ones flag then clears the flag.      
C                  Fits -50 deg C to +40 deg C to within +/- .07 deg C.
C                  Eqn: RT + (RCON +V.RT +W.RT^2 +X.RT^3 +Y.RT^4 +Z.RT^5)
C                  where RT  : Raw thermistor value  (converted to Celsius)
C                        RCON: A constant
C                   V,W,X,Y,Z: Coefficients of quintic equation correcting temp.
C
C                  Loop through six thermistor values:
C                  a)   note each one's flag
C                  b)   if flag indicates input is valid (flag <3)
C                       - clear the flag bits from the raw thermistor value
C                       - assign the value (converted to deg C.) to a working
C                         variable,  which becomes the input variable to a the
C                         quintic equation above.
C                       - derive the corrected output using that equation.
C                       - set input flag value in output thermistor temperature.
C                       else; for an 'invalid' flag
C                       - set the output thermistor value to zero C
C                       - set its output's flag to 3 (= invalid)
C                  next loop.
C
C                  n.b. The corrected thermistor values are not saved at the
C                       end of calibration and are only calculated for local
C                       use in deriving corrected solar fluxes.
C
C VERSION          1.02  30-07-91  A.D HENNINGS
C
C REFERENCES       Best-fit coefficients and constants taken from fitting to
C                  manufacturers calibration data sheet.
C
C ARGUMENTS        REAL*4 RTHM(6)  IN   Six uncorrected thermistor values. deg K
C                  REAL*4 THM (6) OUT   Six  corrected thermistor values.  deg C
C
C SUBPROGRAM       ITSTFLG ISETFLG
C
C CHANGES          1.01 201190 Documentation.
C                  1.02 300791 Documentation.
C                  1.03 17-01-96 D Lauchlan
C                  Unused variables removed
C                  1.04 22-03-04 D Tiddeman flag stripping before calculation 
C                  changed to prevent crashes.
C------------------------------------------------------------------------------
      SUBROUTINE CORR_THM (RTHM,THM)
CDEC$ IDENT 'V1.04'
C
      IMPLICIT NONE
      REAL*8 V,W,X,Y,Z,            !Coefficients of powers 1, 2, 3, 4 & 5
     -       RT,RCON               !placeholder  for thermistor for calc.       
      REAL*4 RTHM(6),THM(6)        !Raw Thermistor, corrected thermistor.       
      INTEGER*4 I,IFLAG ,ITSTFLG
c      LOGICAL OFIRST_TIME/.TRUE./  !   "      "
      PARAMETER (RCON = -0.774,
     -              V =  6.08E-02,
     -              W =  2.47E-03,
     -              X = -6.29E-05,
     -              Y = -8.78E-07,
     -              Z =  1.37E-08)
!

      DO  I=1,6 
      IFLAG = ITSTFLG(RTHM(I))
      CALL ISETFLG(RTHM(I),0)                      !Clear flag before calc.
      IF (IFLAG .LT. 3) THEN
        RT     = RTHM(I) - 273.16                  !convert to Celsius
        THM(I) = RT + (RCON + RT*(V+ RT*(W+RT*(X+RT*(Y+RT*Z)))))                
        CALL ISETFLG(THM(I),IFLAG)                 !Replace original flag.
      ELSE
        THM(I) = 0.0                               !Set thermistors to failed.
        CALL ISETFLG(THM(I),3)                     !and flag as such
      ENDIF                                                                     
      END DO

      RETURN
      END                                                                       

        
C-------------------------------------------------------------------------------
C ROUTINE          RMEANOF        SUBROUTINE     FORTVAX         [C_RFLUX.FOR] 
C
C PURPOSE          Calculate the mean of an array of real values.
C
C DESCRIPTION      An array containing NOELS  real elements is received.
C                  Each element is checked and, if it has a Flag value
C                  (bits 16+17) of zero, is accumulated to a total, and
C                  the  count of good elements incremented.
C                  When all elements have been checked, the mean is derived
C                  such that:
C                  If no good elements were found, the mean is zero, flagged 3.
C                  Otherwise, the mean is the total/count, flagged 0.
C
C ARGUMENTS        INTEGER*4  NOELS IN   Number of elements in array passed
C                  REAL*4     RARR  IN   Array of reals - dimensioned to NOELS
C                  REAL*4     RMEAN OUT  Arithmetic mean of good samples, or 0.
C                  INTEGER*4  IFLAG OUT  Flag value of mean, 0:good 3:invalid.
C                    
C VERSION          1.00   19-03-90  A.D.HENNINGS
C
C SUBPROGRAMS      ITSTFLG  ISETFLG
C
C REFERENCES       None
C
C-----------------------------------------------------------------------------

      SUBROUTINE RMEANOF(NOELS,RARR,RMEAN,IFLAG)
CDEC$ IDENT 'V1.00'
C
      IMPLICIT NONE
      INTEGER*4 NOELS,IX,ITSTFLG,ICOUNT,IFLAG
      REAL*4    RARR(NOELS),RMEAN,SUMM

      SUMM = 0.
      ICOUNT   = 0
      DO IX= 1,NOELS
      IF (ITSTFLG(RARR(IX)) .EQ. 0) THEN
        SUMM = SUMM + RARR(IX)
        ICOUNT = ICOUNT+1
      ENDIF 
      END DO

      IF (ICOUNT .GT. 0 )THEN
        RMEAN = SUMM/FLOAT(ICOUNT)
        IFLAG = 0
      ELSE
        RMEAN  = 0.
        IFLAG  = 3
      ENDIF
      CALL ISETFLG(RMEAN,IFLAG)

      RETURN
      END
*--------------------------------------------------------------
C ROUTINE          CIRC_AVRG  FUNCTION   FORTVAX  
C
C PURPOSE          CALCULATE MEAN OF A SET (>2 <1000)  OF ANGLES, IN DEG.
C
C ARGUMENTS        REAL*4    ARR  IN         Array of Angles (in  Degrees)
C                  INTEGER*4 NUM  IN         Number of angle in array ARR.
C                  REAL*4    CIRC_AVANG OUT  Average angle of set (0-360 deg)
C
C DESCRIPTION      Given a set of angles (0-360 Deg) calculates their mean. 
C                  Handles values spanning 0 or 180.
C                  Returns mean    Flagged 0: If >2 and <= 1/2 of inputs valid
C                                          1: If < 1/2 of inputs valid.
C                                          3: If no valid inputs.
C                  N.B  ASSUMES ALL INPUT ANGLES ARE BETWEEN 0 & 360 DEG.
C
C VERSION          1.0   JAN 1992  A D HENNINGS
C                        MODIFIED FROM  "AVANG" V3.0 SEP 1984  D OFFILER
C                  1.01  DEC 1997  W D N JACKSON
C                        Stips flags before using data
C-------------------------------------------------------------------------------
      REAL  FUNCTION CIRC_AVRG( ARR , NUM)
CDEC$ IDENT 'V1.00'

      IMPLICIT NONE
      INTEGER NUM,NM1,I,ITSTFLG,ICOUNT,IFLAG
      REAL ARR(NUM)
      REAL TARR(1000),DIF
 

      DO I=1,NUM
      TARR (I) = ARR(I)                         !Move values to temporary array
      CALL ISETFLG(TARR(I),0)                   !Strip flag
      END DO                                    !as they may be altered later.

C Alter angles to same sign .

      IF ( NUM .GT. 2 ) THEN
         NM1 = NUM - 1
         DO I = 1 , NM1
            DIF = TARR(I) - TARR(I+1)
            IF ( ABS ( DIF ) .GT. 180.0 ) THEN
                     TARR(I+1) = TARR(I+1) + SIGN (360.0 , DIF )
            ENDIF
         ENDDO
      ENDIF

C  Sum the good points.

      CIRC_AVRG= 0.0
      ICOUNT= 0

      DO I = 1 , NUM
         IF (ITSTFLG (ARR(I)) .LE. 1) THEN         !Do check on original array
            CIRC_AVRG = CIRC_AVRG + TARR(I)        !..but use changed data
            ICOUNT =ICOUNT+1
         ENDIF
      ENDDO

C Calculate average.

      IF (ICOUNT .GT. 0 )THEN
          CIRC_AVRG = CIRC_AVRG / FLOAT (ICOUNT )
          IF (ICOUNT .GT. NUM/2 ) THEN         !More than half rejected, then
              IFLAG = 0                        !flag as reduced quality data.
          ELSE
              IFLAG = 1
          ENDIF
      ELSE
          CIRC_AVRG  = 0.
          IFLAG  = 3
      ENDIF

      IF ( CIRC_AVRG .LT.   0.0 ) CIRC_AVRG = CIRC_AVRG + 360.0
      IF ( CIRC_AVRG .GE. 360.0 ) CIRC_AVRG = CIRC_AVRG - 360.0

C Set the flag in the returned value

      CALL ISETFLG(CIRC_AVRG,IFLAG)

      END
