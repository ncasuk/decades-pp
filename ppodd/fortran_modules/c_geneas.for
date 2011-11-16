      SUBROUTINE C_GENEAS   (IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.02'
C
C ROUTINE  	   C_GENEAS     SUBROUTINE FORTVAX
C     
C PURPOSE 	   Derivation of Dew point   
C
C DESCRIPTION      Calculation of Dew Point in K from General Eastern Hygrometer
C                  
C                         529- Dew Point                             [K]
C
C                  The General Eastern Hygrometer (Parameter 58) is recorded 
C                  in binary with a range of 0 to 4095 DRS bits.
C                  A control signal (Parameter 59) is also recorded which 
C                  gives an indication of the amount of heating or cooling
C                  of the mirror.
C                  The instrument should be in control if the signal is between
C                  certain limits. 
C                  Outside these limits it still produces a dew point 
C                  reading,though of doubtful accuracy, and derived data 
C                  is flagged - FLAG = 2.
C                  
C VERSION	   1.00  240190   J HARMER
C                  1.01  17-01-96 D Lauchlan
C ARGUMENTS 
C                  Constants:
C                  GEMAX      Maximum control condition signal limit RCONST(1)
C                  GEMIN      Minimum control condition signal limit RCONST(2)
C                  CALGE(1)   GE Dew point calib. constant x0        RCONST(3)
C                  CALGE(2)   GE Dew point calib. constant x1        RCONST(4)
C                  CALGE(3)   GE Dew point calib. constant x2        RCONST(5)
C
C
C                  Inputs   : 
C                  GENERAL EASTERN 1011 DEW POINT    [drs units ] Para 58
C                  GENERAL EASTERN CONTROL SIGNAL    [drs units ] Para 59
C
C                  Outputs  : 
C                  DEW POINT                            [K]      Para 529
C           
C SUBPROGRAMS
C                  ITSTFLG          Examines bits 16,17 for flags
C                  ISETFLG          Sets flag bits 16,17 = 0 --> 3
C                  S_QCPT           Performs range and rate of change check
C
C REFERENCES 	   Code adapted from MRF1/MRF2
C
C CHANGES          v1.01 17-01-96 D Lauchlan
C                  Unused variables removed
C
C                  V1.02  27/09/02  W.D.N.JACKSON
C                  Changed to include handling of 16 bit data from the new 
C                  DRS.
C------------------------------------------------------------------------------
      IMPLICIT    NONE
      INTEGER*4   IRAW(64,512), IFRQ(512)
      REAL*4      RCONST(64), RDER(64,1024)

      INTEGER   ITSTFLG,IQFLAG,IFLAG3
      INTEGER   IFLAG, IFLAG1, IFLAG2, IS, IQ, IT
      REAL*4    GEMAX,GEMIN,CALGE(3)
      REAL*4    R529MX, R529MN, R529RG,RSEC,RVAL,R
      REAL*4    RLV529, RLT529 ,R529ERCNT               !Previous: Values/Time
      DATA      RLV529, RLT529 /2*0./                   !Init first time through
      DATA      R529ERCNT /1*1.0/                       !Init first time through
      
      PARAMETER (R529MX=324. , R529MN=195. , R529RG=1.)!Limits checks DEWPT [K]
C------------------------------------------------------------------------------
C Check constants and set up arrays, clear flags
      SAVE
      IFLAG=0
      IFLAG1=0
      IFLAG2=0
      IFLAG3=0
      IQFLAG=0
      DO IT=1,5
        IF (ITSTFLG(RCONST(IT)).EQ.3)IFLAG=3              !Check constant flags 
      END DO
C
      GEMAX=RCONST(1)
      GEMIN=RCONST(2)
      DO IQ=3,5
          CALGE(IQ-2)=RCONST(IQ)
      END DO
C           
      RSEC = RDER(1,515)                          !Time: seconds from midnight
C
C Calc dew point temperature from General Eastern 1011 Hygrometer
C      
      IF (IFRQ(58).GT.0.AND.IFRQ(59).GT.0) THEN
        DO IS = 1,IFRQ(58)                                !Loop thro samples
          IFLAG1 = 0                                      !Check quality flag
          IF(IRAW(IS,58).EQ.0.OR.IRAW(IS,58).EQ.'FFFF'X) IFLAG1=3
          IFLAG  = IFLAG1
          IF (IFLAG .LT. 3 ) THEN                         !If there is some data
            R=FLOAT(IRAW(IS,58))                          !Get para 58 raw data
            RDER(IS,529)=CALGE(3)*R**2 + CALGE(2)*R       !Apply calib constants
     -                          + CALGE(1)                ! [deg C]
            RDER(IS,529)=RDER(IS,529)+273.16              !Dew point [K]
            CALL S_QCPT (RSEC,RLT529,RDER(IS,529),RLV529, !Quality control point
     -                 R529MX,R529MN,R529RG,3.,R529ERCNT,IQFLAG)
            IF (IQFLAG .GT. IFLAG) IFLAG = IQFLAG         !Set worst flag
            IQ=((IS * IFRQ(59) - 1) / IFRQ(58)) + 1       !Find control signal 
            IFLAG2 = 0                                    !Check quality flag
            IF(IRAW(IS,59).EQ.0.OR.IRAW(IS,59).EQ.'FFFF'X) IFLAG2=3
            IF (IFLAG2 .GT. IFLAG) IFLAG = IFLAG2         !Set worst flag
            RVAL=FLOAT(IRAW(IS,59))                       !Get para 59 raw data
            IF (RVAL.LT.GEMIN .OR. RVAL.GT.GEMAX) THEN    !Control cond. on
                IFLAG3=2
            ELSE 
                IFLAG3=0
            ENDIF
            IF (IFLAG3.GT.IFLAG) IFLAG = IFLAG3           !Set worst flag
            CALL ISETFLG(RDER(IS,529),IFLAG)              !Put back worst flag
          ENDIF
        END DO
      ENDIF
C     RLV529= RDER(IFRQ(58),529)                          !Preserve last value
C                                                         !Done within S_QCPT
C                                                          !without the flag
C      SAVE RLT529,RLV529                                   !ANSI Fortran

      RETURN

      END
