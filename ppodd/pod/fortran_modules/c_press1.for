!
! ROUTINE          C_PRESS1 SUBROUTINE FORTVAX
!     
! PURPOSE          Calibrates the cabin pressure sensor and the S9 static port.
!
! DESCRIPTION      Apply calibration the combined transducer and DRS 
!                  coefficients to DRS parameters 14 and 221 to obtain derived
!                  parameters 579 and 778.  Invalid data is flagged with 3, data
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
!                    DRS para  14 CABP  1 Hz Cabin pressure              
!                        para 221 S9SP 32 Hz S9 static pressure
!
!                  Constants:
!                        RCONST(1 to 3) Para 14 cal constants X0 to X2
!                        RCONST(4 to 6) Para 221 cal constants X0 to X2
!
!                  Outputs:
!                    Derived para 579 CABP mb  1 Hz Cabin pressure 
!                            para 778 S9SP mb 32 Hz S9 static pressure
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
!                  Note that V1.00 has no application of S9 position errors.
!
!*******************************************************************************
      SUBROUTINE C_PRESS1(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.00'
!
      INTEGER*4 IRAW(64,512),IFRQ(512),IS,IFLG,IVAL
      REAL*4    RCONST(64),RDER(64,1024),RVAL,RMAX(2),RMIN(2)
      DATA RMIN /650.,100./
      DATA RMAX /1050.,1050./
!
! Derive cabin pressure
!
        IFLG=0
        IVAL=IRAW(1,14)
        IF(IVAL.EQ.'FFFF'X.OR.IVAL.EQ.'FFFE'X) IFLG=3
        IF(IFLG.NE.3) THEN
          RVAL=FLOAT(IVAL)
          RVAL=RCONST(1)+RVAL*RCONST(2)+RVAL*RVAL*RCONST(3)
          IF(RVAL.LT.RMIN(1).OR.RVAL.GT.RMAX(1)) IFLG=2
          RDER(1,579)=RVAL
          CALL ISETFLG(RDER(1,579),IFLG)
        END IF
!
! Derive S9 port pressures.  Note that by default derived parameters will have a 
! value of 0 flagged with a 3.
!
      DO IS=1,IFRQ(221)              !For each sample
        IFLG=0
        IVAL=IRAW(IS,221)
        IF(IVAL.EQ.'FFFF'X.OR.IVAL.EQ.'FFFE'X) IFLG=3
        IF(IFLG.NE.3) THEN
          RVAL=FLOAT(IVAL)
          RVAL=RCONST(4)+RVAL*RCONST(5)+RVAL*RVAL*RCONST(6)
          IF(RVAL.LT.RMIN(2).OR.RVAL.GT.RMAX(2)) IFLG=2
          RDER(IS,778)=RVAL
          CALL ISETFLG(RDER(IS,778),IFLG)
        END IF
      END DO
! 
      RETURN
      END
