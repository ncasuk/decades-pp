!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         ROUTINE      C_NEPHL1
!
!         PURPOSE      Calibrates the parameters from the TSI 3 wavelength
!                      Nephelometer.
!
!         DESCRIPTION  The nephelometer output are subject to calibrations
!                      internal to the instrument plus the normal MILLIE/DRS
!                      calibrations.
!
!                      All raw parameters are digital conversions of the input
!                      voltage. The digital values are converted using a
!                      linear fit then the nephelometer internal cals are
!                      applied to obtain the output derived values.
!                      Parameters 175, 176 & 183 are linear thus
!
!                           output = a + b * v /vfs
!
!                      where a and b are nephelometer internal constants,
!                      v the derived voltage and vfs the full scale voltage.
!                      Scattering parameters 177 through 182 are logarithmic
!                      so
!
!                          output = 10**((v/b) - a) - c
!
!                      a, b and c are nephelometer internal constants, v
!                      the derived voltage.
!
!                      Parameter 184, nephelometer status, is the analog output
!                      of a 4 bit D to A converter. The LSB corresponds to 
!                      0.625V.
!
!         VERSION      1.00  D.P.Briggs
!
!         SUBROUTINES  ISETFLG
!
!         FILES        NONE
!
!         REFERENCES   Nephelometer Instruction Manual
!                      Nephelometer internal technical note
!  
!         PARAMETERS   RAW      DERIVED    FREQ   RANGE   UNITS
!  NEPH PRESSURE       175        760      1Hz    0-10V    mb
!  NEPH TEMPERTURE     176        761      1Hz    0-10V    K
!  NEPH BLUE SP        177        762      1Hz    0-10V    /m
!  NEPH GREEN SP       178        763      1HZ    0-10V    /m
!  NEPH RED SP         179        764      1Hz    0-10V    /m
!  NEPH BLUE BSP       180        765      1Hz    0-10V    /m
!  NEPH GREEN BSP      182        766      1Hz    0-10V    /m
!  NEPH RED BSP        181        767      1Hz    0-10V    /m
!  NEPH HUMIDITY       183        768      1Hz    0-5V     %
!  NEPH STATUS         184        769      1Hz    0-10V    bits
! 
!         NEPHELOMETER CONSTANT KEYWORDS
!   CALNPRS  I J A B      
!   CALNTMP  I J A B
!   CALNBTS  I J A B C    NOTE : I & J are multiplexer calibrations.
!   CALNGTS  I J A B C           A, B & C are instrument internal cals.
!   CALNRTS  I J A B C
!   CALNBBS  I J A B C
!   CALNGBS  I J A B C
!   CALNRBS  I J A B C
!   CALNHUM  I J A B
!   CALNSTS  I J
!
!         CHANGES 
!         V1.01  12/05/97  W.D.N.JACKSON
!                Miscellaneous bug fixes, the most serious being the incorrect
!                calculation of the red backscattering coefficient, and a
!                tendency to crash with floating overflows.
!         V1.02  29/05/97  W.D.N.JACKSON
!                Changed to reflect fact that Red BS comes in on 181 and green
!                on para 182.
!         V1.03  19/06/98  W.D.N.JACKSON
!                Changed to reflect fact that bit 3 of the status word is 0
!                (not 1) when on calibrate.
!         V1.04  01/09/02  W.D.N.JACKSON
!                Small changes to handle new 16 bit DRS.  Also status parameter
!                is now calibrated.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE C_NEPHL1(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.04'
      IMPLICIT NONE
      INTEGER   IFLAG,IPARM,ISTAT
      REAL      RVOLT
      INTEGER*4 IRAW(64,512),IFRQ(512)
      REAL*4    RCONST(64),RDER(64,1024)
!
! initialise output values to 0.0 flagged 3
!
      IFLAG = 3
      DO IPARM = 760,769
        RDER(1,IPARM)=0.0
        CALL ISETFLG(RDER(1,IPARM),IFLAG)
      ENDDO  
!
! Check all data valid.  Return if not.
!
      DO IPARM = 175,184
        IF(IRAW(1,IPARM).EQ.0) RETURN
        IF(IRAW(1,IPARM).EQ.'FFFF'X) RETURN
      ENDDO
!
!   184/769 1Hz   NEPH STATUS
! input is voltage proportional to a 4 bit counter at 0.625V (256 bits) per unit
! Status word bits have the following meanings (it is not clear if the status
! word is valid when the lamp is not working):
!   Bit 0 set - Backscatter on
!   Bit 1 set - Chopper on
!   Bit 2 set - Lamp off
!   Bit 3 set - Not on calibrate
!
      IFLAG=0
      RVOLT=FLOAT(IRAW(1,184))*RCONST(44)+RCONST(43)
      ISTAT=NINT(RVOLT/0.625)
      IF(ISTAT.LT.0.OR.ISTAT.GT.15) RETURN
      RDER(1,769) = FLOAT(ISTAT)
      CALL ISETFLG(RDER(1,769),IFLAG)
!
!   175/760 1Hz   NEPH PRESSURE
      RVOLT = RCONST(1) + RCONST(2) * FLOAT(IRAW(1,175))
      RDER(1,760) = RCONST(4) * RVOLT/10.0 + RCONST(3)
      CALL ISETFLG(RDER(1,760),IFLAG)
!      
!   176/761 1Hz   NEPH TEMPERATURE
      RVOLT = RCONST(5) + RCONST(6) * FLOAT(IRAW(1,176))
      RDER(1,761) = RCONST(8) * RVOLT/10.0 + RCONST(7)
      CALL ISETFLG(RDER(1,761),IFLAG)
!
!   183/768 1Hz   NEPH HUMIDITY 
      RVOLT = RCONST(39) + RCONST(40) * FLOAT(IRAW(1,183))
      RDER(1,768) = RCONST(42) * RVOLT/10.0 + RCONST(41)
      CALL ISETFLG(RDER(1,768),IFLAG)
!
! set flags for scattering coeffs:
!  lamp or reference chopper off, flag 3.
!  valve position in calibrate mode flag 2.
!  if not backscatter shutter on flag backscatter coeffs with 3
      IF (BTEST(ISTAT,1) .AND. .NOT.BTEST(ISTAT,2)) THEN
        IF (.NOT.BTEST(ISTAT,3)) IFLAG = 2
!
!   177/762 1Hz   NEPH BLUE SP 
        RVOLT = RCONST(9) + RCONST(10) * FLOAT(IRAW(1,177))
        RDER(1,762) = 10 ** ((RVOLT/RCONST(12)) - RCONST(11)) -
     &                RCONST(13)
        CALL ISETFLG(RDER(1,762),IFLAG)
!
!   178/763 1HZ   NEPH GREEN SP
        RVOLT = RCONST(14) + RCONST(15) * FLOAT(IRAW(1,178))
        RDER(1,763) = 10 ** ((RVOLT/RCONST(17)) - RCONST(16)) -
     &                RCONST(18)
        CALL ISETFLG(RDER(1,763),IFLAG)
!
!   179/764 1Hz   NEPH RED SP
        RVOLT = RCONST(19) + RCONST(20) * FLOAT(IRAW(1,179))
        RDER(1,764) = 10 ** ((RVOLT/RCONST(22)) - RCONST(21)) -
     &                RCONST(23)
        CALL ISETFLG(RDER(1,764),IFLAG)
!
        IF (BTEST(ISTAT,0)) THEN  !backscatter shutter on
!   180/765 1Hz   NEPH BLUE BSP  
          RVOLT = RCONST(24) + RCONST(25) * FLOAT(IRAW(1,180)) 
          RDER(1,765) = 10 ** ((RVOLT/RCONST(27)) - RCONST(26)) -
     &                  RCONST(28)
          CALL ISETFLG(RDER(1,765),IFLAG)
!
!   182/766 1Hz   NEPH GREEN BSP
          RVOLT = RCONST(29) + RCONST(30) * FLOAT(IRAW(1,182))
          RDER(1,766) = 10 ** ((RVOLT/RCONST(32)) - RCONST(31)) -
     &                  RCONST(33)
          CALL ISETFLG(RDER(1,766),IFLAG)
!
!   181/767 1Hz   NEPH RED BSP  
          RVOLT = RCONST(34) + RCONST(35) * FLOAT(IRAW(1,181))
          RDER(1,767) = 10 ** ((RVOLT/RCONST(37)) - RCONST(36)) -
     &                  RCONST(38)
          CALL ISETFLG(RDER(1,767),IFLAG)
        ENDIF
      ENDIF
!
      RETURN
      END
