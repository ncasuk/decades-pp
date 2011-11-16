C
C ROUTINE	C_COMR SUBROUTINE FORTVAX
C
C PURPOSE	A subroutine to calculate Carbon monoxide.
C
C DESCRIPTION	The CO analyser outputs one measurement.
C		This is input to the program as DRS bits, and converted
C		into PPB by multiplying the DRS bits by a calibration factor.
C
C
C TO COMPILE	$FORT C_COMR
C
C VERSION	1.00  8-Jul-2004  	D.Tiddeman
C               1.01  27-OCT-2005
C               1.02
C		    1.03 31-JAN-2007 R Purvis Changed timedelay after cal to 20 s
C		    1.04 18-SEP-2007 R Purvis	RCONST(5) added for correction factor
C                                                                
C ARGUMENTS	IRAW(1,154) - on entry contains the raw CO signal
C               IRAW(1,223) - on entry contains raw RVSM airspeed
C               IRAW(1,113) - cal info ?
C               RCONST(1,2,3,4) XO and X1 voltage cal for CO, v to ppb, ppb offs
C		RDER(1,782) - on exit contains the derived CO signal
C
C*******************************************************************************
      SUBROUTINE C_COMR(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.02'
      IMPLICIT NONE
      INTEGER*4 IRAW(64,1024),IFRQ(512)                 
      INTEGER*4   IFLG,IS,ITSTFLG,ISHIFT
      REAL*4 COMR,RERR,RFUDGE
      REAL*4 RCONST(64),RDER(64,1024)
      INTEGER*4 IWASCAL

      SAVE IWASCAL
C
C Set default values
C
      RERR=0.
      CALL ISETFLG(RERR,3)
      RDER(1,782)=RERR

C     Copy across raw signals
C
      COMR=FLOAT(IRAW(1,154))
C
C     Convert CO DRS signals first to voltage, then apply voltage to 
C     ppb conversion, then add instrument offset.
C
      RFUDGE=1.00
      ISHIFT=0
      IF(ITSTFLG(RCONST(8)).EQ.0)THEN
        IF(ITSTFLG(RCONST(5)).EQ.0)RFUDGE=RCONST(5)
      ENDIF

      COMR=((RCONST(1)+COMR*RCONST(2))*RCONST(3)+RCONST(4))
C
      IF(ITSTFLG(RCONST(5)).EQ.0)COMR=COMR*RCONST(5)

      IFLG=0
      IF(ITSTFLG(RCONST(8)).EQ.0) THEN
        DO IS=1,32
          IF(IRAW(IS,223).LT.RCONST(8)*62) IFLG=1
        ENDDO
      ENDIF
      IF(COMR.LT.0.) IFLG=2
      IF(IRAW(1,154).EQ.0) IFLG=3
      IF(IRAW(1,154).EQ.'FFFF'X) IFLG=3
      IF(ITSTFLG(RCONST(6)).EQ.0.AND.ITSTFLG(RCONST(7)).EQ.0)THEN
        IF(COMR.LT.RCONST(6).OR.COMR.GT.RCONST(7))IFLG=3
      ENDIF
      IF(IRAW(1,113).GT.8000) IWASCAL=20
      IF(IWASCAL.GT.0)THEN
        IFLG=MAX(IFLG,2)
        IWASCAL=IWASCAL-1
      ENDIF
      CALL ISETFLG(COMR,IFLG)
      RDER(1,782)=COMR
C
      RETURN 
      END
