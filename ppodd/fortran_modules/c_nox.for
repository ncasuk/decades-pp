C
C ROUTINE	C_NOX SUBROUTINE FORTVAX
C
C PURPOSE	A subroutine to calculate Nitrogen monoxide, Nitrogen dioxide
C		and NOx measured by the TECO 42 NOx analyser.
C
C DESCRIPTION	The NOx analyser outputs three measurements, NO, NO2 and NOx.
C		These are input to the program as DRS bits, and converted
C		into PPB by multiplying the DRS bits by a calibration factor.
C
C
C TO COMPILE	$FORT C_NOX
C
C VERSION	1.00  28 Sept. 1998  	I. Hawke
C               1.01  23 June. 1999     I. Hawke     5ppb Offset included
C               1.02  07 Mar   2000     I. Hawke     Offset removed
C               1.03  07 Mar   2000     I. Hawke     5ppb offset included
C               1.04  29 Mar   2000     I. Hawke     New Conversion Algorithm
C               1.05  30 Mar   2000     I. Hawke     Flow Testing Added
C               1.06  02 Oct   2002     N. Jackson   Modified for 16 bit DRS
C               1.07  27 Oct   2005     D.Tiddeman   New constants for flagging
C                                                    low airspeeds or out of
C                                                    range values.
C               1.08  05 Dec  2005      D.Tiddeman   No flag 2 for negative
C                                                                               
C ARGUMENTS	IRAW(1,203) - on entry contains the raw NO signal
C		IRAW(1,204) - on entry contains the raw NO2 signal
C		IRAW(1,205) - on entry contains the raw NOx signal
C               IRAW(1,114) - on entry contains ozone flow signal
C               RCONST(1,2,3,4) XO and X1 voltage cal for NO, v to ppb, ppb offs
C               RCONST(5,6,7,8) same for NO2
C               RCONST(9,10,11,12) same for NOx
C               RCONST(13,14) X0 and X1 voltage cal for Ozone flow
C		RDER(1,770) - on exit contains the derived NO signal
C		RDER(1,771) - on exit contains the derived NO2 signal
C		RDER(1,772) - on exit contains the derived NOx signal
C
C*******************************************************************************
      SUBROUTINE C_NOX(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.08'
      IMPLICIT NONE
      INTEGER*4 IRAW(64,1024),IFRQ(512)                 
      INTEGER   IFLG,IFLG1,ITSTFLG,IS
      REAL*4 NO,NO2,NOX,OZF,RERR
      REAL*4 RCONST(64),RDER(64,1024)
C
C
C Set default values for output
C
      RERR=0.
      CALL ISETFLG(RERR,3)
      RDER(1,770)=RERR
      RDER(1,771)=RERR
      RDER(1,772)=RERR

C     Copy across raw signals
C
      NO=FLOAT(IRAW(1,203))
      NO2=FLOAT(IRAW(1,204))
      NOX=FLOAT(IRAW(1,205))
      OZF=FLOAT(IRAW(1,114))
C
C     Convert TECO NOX DRS signals first to voltage, than apply voltage to 
C     ppb conversion, then subtract instrument offset which ensures signal
C     voltage doesn't go negative.
C
      NO=(RCONST(1)+NO*RCONST(2))*RCONST(3)-RCONST(4)
      NO2=(RCONST(5)+NO2*RCONST(6))*RCONST(7)-RCONST(8)
      NOX=(RCONST(9)+NOX*RCONST(10))*RCONST(11)-RCONST(12)
C
C Convert ozone flow to voltage
C
      OZF=RCONST(13)+OZF*RCONST(14)
C
C Do flagging
C
      IF(IRAW(1,114).EQ.0) RETURN
      IF(IRAW(1,114).EQ.'FFFF'X) RETURN
      IF(OZF.LE.0.) RETURN             !Reject data if pump off or not recorded
C
      IFLG1=0
      IF(ITSTFLG(RCONST(17)).EQ.0)THEN
        DO IS=1,32
          IF(IRAW(IS,223).LT.62*RCONST(17)) IFLG1=1
        ENDDO
      ENDIF
      IFLG=IFLG1
      IF(IRAW(1,203).EQ.0) IFLG=3
      IF(IRAW(1,203).EQ.'FFFF'X) IFLG=3
      IF(ITSTFLG(RCONST(15)).EQ.0.AND.ITSTFLG(RCONST(16)).EQ.0)THEN
        IF(NO.LT.RCONST(15).OR.NO.GT.RCONST(16))IFLG=3
      ENDIF
      CALL ISETFLG(NO,IFLG)
      RDER(1,770)=NO
C
      IFLG=IFLG1
      IF(IRAW(1,204).EQ.0) IFLG=3
      IF(IRAW(1,204).EQ.'FFFF'X) IFLG=3
      IF(ITSTFLG(RCONST(15)).EQ.0.AND.ITSTFLG(RCONST(16)).EQ.0)THEN
        IF(NO2.LT.RCONST(15).OR.NO2.GT.RCONST(16))IFLG=3
      ENDIF
      CALL ISETFLG(NO2,IFLG)
      RDER(1,771)=NO2
C
      IFLG=IFLG1
      IF(IRAW(1,205).EQ.0) IFLG=3
      IF(IRAW(1,205).EQ.'FFFF'X) IFLG=3
      IF(ITSTFLG(RCONST(15)).EQ.0.AND.ITSTFLG(RCONST(16)).EQ.0)THEN
        IF(NOX.LT.RCONST(15).OR.NOX.GT.RCONST(16))IFLG=3
      ENDIF
      CALL ISETFLG(NOX,IFLG)
      RDER(1,772)=NOX
C

      RETURN 
      END
