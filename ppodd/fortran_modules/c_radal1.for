C
C ROUTINE	C_RADAL1 SUBROUTINE FORTVAX
C
C PURPOSE	To subroutine to calculate the aircraft altitude from the radar
C		altimeter.
C
C DESCRIPTION	The raw radar altimeter data is provided as a 16 bit signed
C               number from the ARINC 429 data bus, with a least bit resolution
C               of 0.25 ft.
C
C		The derived data is quality controlled to ensure that:
C		(a) data outside the range 0 to 8191.75 ft are flagged 3
C		(b) more than two values the same are flagged 3
C		(c) more than 1000' change between values is flagged 3
C
C TO COMPILE	$FORT C_RADAL1
C
C VERSION	1.00  02/10/02  W.D.N.JACKSON
C
C ARGUMENTS	IRAW(X,37)  - where x=1 or 2, on entry this contains the raw 
C			      radar height. 
C		IFRQ(37)    - on entry contains 2, the frequency of the raw 
C			      radar height.
C		RDER(X,575) - where x= 1 or 2, on exit contains the derived 
C			      radar height in meters.
C
C CHANGES	V1.01  WDNJ  05/11/04
C		Flagging criteria improved
C
!*******************************************************************************
      SUBROUTINE C_RADAL1(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.01'
      IMPLICIT  NONE
      INTEGER*4 IRAW(64,512),IFRQ(512),IFLG,IS,IV,ILSTVAL,IMATCH,
     &          ILSTFLG,IMATCHVAL
      REAL*4    RCONST(64),RDER(64,1024)
      DATA      ILSTVAL /-1/, IMATCH /0/, ILSTFLG /0/
      SAVE ILSTVAL,IMATCH,ILSTFLG
!
! Convert raw data
! to metres and store in appropriate element of array RDER.
! Repeat this for all samples passed to the routine. Note that IRAW contains
! the 16 bit number zero extended.
!
      
      DO IS=1,IFRQ(37)
        RDER(IS,575)=0.0
        IFLG=0
        IV=IRAW(IS,37)
        IF(IV.EQ.'FFFF'X) IFLG=3       !No DLU
        IF(IV.EQ.'FFFE'X) IFLG=3       !No ARINC data
        IF(IV.LT.0.OR.IV.GE.'7FFF'X) IFLG=3 !Glitch or maxed out
        IF(IV.EQ.ILSTVAL) IMATCH=IMATCH+1 !Count data replications
        IF(IV.NE.ILSTVAL) IMATCH=0
        IMATCHVAL=1                    !More than two the same is prob error
        IF(ILSTFLG.NE.0) IMATCHVAL=0          
        IF(IV.EQ.ILSTVAL.AND.IV.NE.0.AND.IMATCH.GT.IMATCHVAL) 
     &    IFLG=-1                      !Keep data value rather than set to 0
        IF(ABS(IV-ILSTVAL).GT.4000) IFLG=3 !More than 1000ft in 0.5s is error
        ILSTVAL=IV
        IF(IFLG.LT.3) RDER(IS,575)=IV*0.25*0.3048
        IF(IFLG.EQ.-1) IFLG=3
        ILSTFLG=IFLG
        CALL ISETFLG(RDER(IS,575),IFLG)
      END DO
      RETURN
      END
