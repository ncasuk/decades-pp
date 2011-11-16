!
! ROUTINE          C_TPRESS SUBROUTINE FORTVAX
!     
! PURPOSE          Calibrates the five turbulence probe pressure transducers
!                  into mb.
!
! DESCRIPTION      Apply calibration the combined transducer and DRS 
!                  coefficients to DRS parameters 215 to 219 to obtain derived
!                  parameters 773 to 777.  Invalid data is flagged with 3, data
!                  outside limits is flagged with 2.
!
! METHOD           For each DRS parameter to be calibrated:
!                  1. If data is FFFF or FFFE then flag 3
!                  2. Apply the calibration constants
!                  3. Check the results for being within acceptable values.
!                  4. Set data flag bits (16+17) 0: Good data
!                                                1: Data of lower quality
!                                                2: Probably faulty, exceed lims
!                                                3: Data absent or invalid.
!
!                  Flagging - If a value can't be computed, due to missing data
!                  missing constants, divide be zeroes, etc, a value of 0 is
!                  used, flagged with a three.  If a value is outside its 
!                  limits for range or rate of change, it is flagged with a two.
!                  If there are no problems with the data it is flagged with 0.
!
! VERSION          1.00  23/07/03  W.D.N.JACKSON
!
! ARGUMENTS        Inputs:
!                    DRS para 215 TBP1 32 Hz Turbulence probe centre port
!                        para 216 TBP2 32 Hz Turbulence probe attack ports
!                        para 217 TBP3 32 Hz Turbulence probe sideslip ports
!                        para 218 TBP4 32 Hz Turbulence probe attack check
!                        para 219 TBP5 32 Hz Turbulence probe sideslip check
!
!                  Constants:
!                        RCONST(1 to 4) Para 215 cal constants X0 to X3
!                        RCONST(5 to 8) Para 216 cal constants X0 to X3
!                        RCONST(9 to 12) Para 217 cal constants X0 to X3
!                        RCONST(13 to 14) Para 218 cal constants X0 to X1
!                        RCONST(15 to 16) Para 219 cal constants X0 to X1
!
!                  Outputs:
!                    Derived para 773 TBP0 mb 32 Hz Centre pressure
!                            para 774 TBPA mb 32 Hz Attack pressure
!                            para 775 TBPB mb 32 Hz Sideslip pressure
!                            para 776 TBPC mb 32 Hz Attack check pressure
!                            para 777 TBPD mb 32 Hz Sideslip check pressure
!
!                  Flags:
!                    Missing/corrupt data output as 0 flagged 3.
!                    Out of range data flagged 2.
!
! SUBPROGRAMS      ISETFLG 
!
! REFERENCES       
!
! CHANGES          V1.00 23/07/03  WDNJ Original version
!                  Note that V1.00 has no limit checking and no use is made of
!                  the check pressures.
!                  V1.01 25/03/04  WDNJ
!                  Now takes third order calibration constants for the main
!                  transducers, and first order for the check transducers.
!                  V1.02 26/01/06 Phil Brown
!                  Realistic min/max values provided for centre-port, Pa, Pb
!                  for flagging purposes. Values alsoe provided for check
!                  pressures Ca, Cb based on current (and probably wrong)
!                  calibration coefficients.
!
!*******************************************************************************
      SUBROUTINE C_TPRESS(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.02'
!
      INTEGER*4 IRAW(64,512),IFRQ(512),IS,IP,IFLG,IVAL
      REAL*4    RCONST(64),RDER(64,1024),RVAL,RMAX(5),RMIN(5)
      DATA RMIN /30.,-30.,-20.,50.,50./
      DATA RMAX /130.,30.,20.,200.,200./
!
! Derive port pressures.  Note that by default derived parameters will have a 
! value of 0 flagged with a 3.
!
      DO IP=1,5                        !For each parameter
        DO IS=1,IFRQ(215)              !For each sample
          IFLG=0
          IVAL=IRAW(IS,214+IP)
          IF(IVAL.EQ.'FFFF'X.OR.IVAL.EQ.'FFFE'X) IFLG=3
          IF(IFLG.NE.3) THEN
            RVAL=FLOAT(IVAL)
            IF(IP.EQ.1) THEN
              RVAL=RCONST(1)+RVAL*RCONST(2)+RVAL*RVAL*RCONST(3)
     &             +RVAL*RVAL*RVAL*RCONST(4)
            ELSE IF(IP.EQ.2) THEN
              RVAL=RCONST(5)+RVAL*RCONST(6)+RVAL*RVAL*RCONST(7)
     &             +RVAL*RVAL*RVAL*RCONST(8)
            ELSE IF(IP.EQ.3) THEN
              RVAL=RCONST(9)+RVAL*RCONST(10)+RVAL*RVAL*RCONST(11)
     &             +RVAL*RVAL*RVAL*RCONST(12)
            ELSE IF(IP.EQ.4) THEN
              RVAL=RCONST(13)+RVAL*RCONST(14)
            ELSE IF(IP.EQ.5) THEN
              RVAL=RCONST(15)+RVAL*RCONST(16)
            END IF
            IF(RVAL.LT.RMIN(IP).OR.RVAL.GT.RMAX(IP)) IFLG=2
            RDER(IS,772+IP)=RVAL
            CALL ISETFLG(RDER(IS,772+IP),IFLG)
          END IF
        END DO
      END DO
! 
      RETURN
      END
