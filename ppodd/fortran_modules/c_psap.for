!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         ROUTINE      C_PSAP
!
!         PURPOSE      Calibrates the parameters from the Particle Soot
!                      Absorbtion Photometer (PSAP).
!
!         DESCRIPTION  All raw parameters are digital conversions of the input
!                      voltage. The digital values are converted using a
!                      linear fit then the instrument cals are
!                      applied to obtain the output derived values.
!                      Parameter 175 is linear thus
!
!                           output = v * 0.5E-5
!
!                      v the derived voltage and vfs the full scale voltage.
!                      Parameter 177 is logrithmic so
!
!                          ouput = 10**((v/2.0) - 7.0)
!
!
!         VERSION      1.00  D.P.Briggs
!
!         SUBROUTINES  ISETFLG
!
!         FILES        NONE
!
!         PARAMETERS   RAW      DERIVED    FREQ   RANGE   UNITS
!  PSAP LIN ABS COEFF  185        648      1Hz    0-10V    /m
!  PSAP LOG ABS COEFF  186        649      1Hz    0-10V    /m
!  PSAP TRANSMITTANCE  187                 1Hz    0-10V
!  PSAP FLOW RATE      188                 1Hz    0-10V
!
!         PSAP CONSTANT KEYWORDS
!  CALPLIN   i j    NOTE : i & j are multiplexer calibrations.
!  CALPLOG   i j
! 
!         CHANGES 
!
!         V1.01  01/10/02  W.D.N.JACKSON
!         Adjusted for 16 bit data from the new DRS
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE C_PSAP(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.01'
      IMPLICIT NONE
      INTEGER*4 IRAW(64,512),IFRQ(512)
      REAL*4    RCONST(64),RDER(64,1024)
      REAL      RVOLT
      INTEGER*4 IFLAG,IPARM,IFREQ,IT,IF
!
! initialise output values to 0.0 flagged 3
      IFLAG = 3
      DO IPARM = 648,649
        DO IFREQ = 1,IFRQ(IPARM-463)
          RDER(IFREQ,IPARM)=0.0
          CALL ISETFLG(RDER(IFREQ,IPARM),IFLAG)
        ENDDO
      ENDDO  
!
! Check for possible error in DRS data by checking for all 0s or 1s.
! Do not process futher if any parameter is found faulty.
!
      IFLAG = .FALSE.
      DO IPARM = 185,188
        DO IFREQ = 1,IFRQ(IPARM)
          IF(IRAW(IFREQ,IPARM).EQ.0) IFLAG=.TRUE.
          IF(IRAW(IFREQ,IPARM).EQ.'FFFF'X) IFLAG=.TRUE.
        ENDDO
      ENDDO
      IF (IFLAG) RETURN
!
! FLAGGING SCHEME
!   filter transmittance < 0.5 flag 1
!   filter transmittance > 1.0 flag 3
!   flow rate < 1.0 lpm flag 1
!   flow rate = 0.0 lpm flag 3
!  Bit values are taken from old DRS version and scaled between .1 and .9 *2**16
!
      IT=IRAW(1,187)
      IF=IRAW(1,188)
      IF (IT .LT. 27546) THEN
        IFLAG = 1
      ELSE IF (IT .GT. 48538) THEN
        IFLAG = 3
      ELSE IF (IF .LT. 17050 .AND. IF .GE. 7194) THEN
        IFLAG = 1
      ELSE IF (IF .LT. 7194 ) THEN
        IFLAG = 3
      ELSE
        IFLAG = 0
      ENDIF 
!
      IF (IFLAG .LT. 3) THEN
!   185/648 1Hz   PSAP LIN ABS COEFF
        RVOLT = RCONST(1) + RCONST(2) * FLOAT(IRAW(1,185))
        RDER(1,648) = RVOLT * 0.5E-5
!      
!   186/649 1Hz   PSAP LOG ABS COEFF
        RVOLT = RCONST(3) + RCONST(4) * FLOAT(IRAW(1,186))
        RDER(1,649) = 10**((RVOLT/2.0) - 7.0)
      ENDIF
      CALL ISETFLG(RDER(1,648),IFLAG)
      CALL ISETFLG(RDER(1,649),IFLAG)
!
      RETURN
      END
