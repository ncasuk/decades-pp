C
C ROUTINE	C_OZONE1 SUBROUTINE FORTVAX
C
C PURPOSE	A subroutine to calculate the ozone mixing ratio.
C
C DESCRIPTION   Calibration routine for TECO OZONE
C               Fourth order fit for temperature transducer
C               Linear fit for pressure transducer
C               Signal 10V =1000 ppb ozone corrected by pressure and temperature                              
C
C TO COMPILE	$FORT C_OZONE1
C
C VERSION	    1.00  30th Aug 1996  D.Tiddeman   Module Written
C               1.01  12th Sep 1999  I. Hawke     O3 Smoothing Algorithm
C               1.02  2 Oct 2002     N. Jackson   Convert to work on new DRS
C               1.03  11 Feb 2005   D.Tiddeman   No longer does P+T correction
C		1.04  Unknown       D.Tiddeman   RVSM, min and max flags addeds
C               1.05  31 Jan 2007   R Purvis     Rewritten to take out P, T
C                                                corrections
C					         Flow commented out
C                                                                             
C ARGUMENTS	IRAW(1,100) - on entry contains the raw ozone signal
C		IRAW(1,114) - on entry contains the raw ozone flow
C		IRAW(1,223) - on entry contains the raw rvsm airspeed
C		RCONST(X)  
C		RDER(1,574) - on exit contains the derived ozone mixing ratio
C
C
C*******************************************************************************
      SUBROUTINE C_OZONE1(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.05'
      IMPLICIT NONE
      INTEGER*4 IRAW(64,512),IFRQ(512)   
      INTEGER   IFLG,ITSTFLG,POINT,IS
      REAL*4 OZSIG
      REAL*4 RCONST(64),RDER(64,1024),FLOW
C
C	INITIALISE OZONE CHANNEL TO ZERO
C
      RDER(1,574)=0.0
C
C     SET UNUSED CHANNELS TO -9999 AND FLAG AS 3
C
C	RDER(1,691) = -9999
C      CALL ISETFLG(RDER(1,691),3)
C	RDER(1,692) = -9999
C      CALL ISETFLG(RDER(1,692),3)
C      RDER(1,693) = -9999
C      CALL ISETFLG(RDER(1,693),3)

C     SKIP PROCESSING IF ANY RAW DATA INVALID
C
      IFLG=0
      IF((IRAW(1,100).EQ.0).OR.(IRAW(1,100).EQ.'FFFF'X)) IFLG=3
C     IF(IRAW(1,114).EQ.0) IFLG=3
C     IF(IRAW(1,114).EQ.'FFFF'X) IFLG=3
C     FLOW=RCONST(16)+IRAW(1,114)*RCONST(17) !Flow in volts
C     IF(FLOW.LE.0.) IFLG=3
      IF(IFLG.EQ.3) GOTO 100 
C
C     CALCULATE OZONE MIXING RATIO
C
      ozsig=FLOAT(IRAW(1,100))	    ! ozsig becomes ozone signal
      ozsig=rconst(1)+ozsig*rconst(2)   ! convert drs bits to volts
      OZSIG=OZSIG*RCONST(4)+RCONST(3)   ! IN PPB
      RDER(1,574) = OZSIG
C
C 	INSERT FLAGS FOR ON GROUND USING RVSM(223)VALUE AND MIN AIR SPEED(21)
c 	MIN VALUE (18), OUTSIDE CALIBRATION RANGE(19, 300PPBv) AND MAX VALUE (20, 500PPBv)
C   
      IF(IFLG.EQ.0)THEN
      IF(ITSTFLG(RCONST(21)).EQ.0)THEN
        DO IS=1,32
          IF(IRAW(IS,223).LT.62*RCONST(21)) IFLG=1
        ENDDO
      ENDIF
      IF((ITSTFLG(RCONST(18)).EQ.0.AND.ITSTFLG(RCONST(19))).EQ.0
     &   .AND.ITSTFLG(RCONST(20)).EQ.0)THEN
        IF(OZSIG.GT.RCONST(19))IFLG=2
        IF(OZSIG.LT.RCONST(18).OR.OZSIG.GT.RCONST(20))IFLG=3
      ENDIF
      ENDIF
C
C     FLAG OZONE SIGNAL
C
  100 CALL ISETFLG(RDER(1,574),IFLG) 
      RETURN
      END
