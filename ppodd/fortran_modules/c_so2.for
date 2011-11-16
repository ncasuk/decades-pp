C
C ROUTINE	C_SO2 SUBROUTINE FORTVAX
C
C PURPOSE	A subroutine to calculate Carbon monoxide.
C
C DESCRIPTION	The SO2 analyser outputs one measurement.
C		This is input to the program as DRS bits, and converted
C		into PPB by multiplying the DRS bits by a calibration factor.
C
C
C TO COMPILE	$FORT C_SO2
C
C VERSION	1.00  8-Jul-2004  	D.Tiddeman
C                                                                               
C ARGUMENTS	IRAW(1,214) - on entry contains the raw SO2 signal
C               RCONST(1,2,3,4) XO and X1 voltage cal for SO2, v to ppb, ppb offs
C		RDER(1,740) - on exit contains the derived SO2 signal
C
C*******************************************************************************
      SUBROUTINE C_SO2(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.00'
      IMPLICIT NONE
      INTEGER*4 IRAW(64,1024),IFRQ(512)                 
      INTEGER   IFLG
      REAL*4 SO2,RERR
      REAL*4 RCONST(64),RDER(64,1024)
C
C Set default values
C
      RERR=0.
      CALL ISETFLG(RERR,3)
      RDER(1,740)=RERR

C     Copy across raw signals
C
      SO2=FLOAT(IRAW(1,214))
C
C     Convert CO DRS signals first to voltage, then apply voltage to 
C     ppb conversion, then add instrument offset.
C
      SO2=(RCONST(1)+SO2*RCONST(2))*RCONST(3)+RCONST(4)
C
      IFLG=0
      IF(IRAW(1,214).EQ.0) IFLG=3
      IF(IRAW(1,214).EQ.'FFFF'X) IFLG=3
      IF(SO2.LT.0.) IFLG=MAX(2,IFLG)
      CALL ISETFLG(SO2,IFLG)
      RDER(1,740)=SO2
C
      RETURN 
      END
