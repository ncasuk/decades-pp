C
C ROUTINE          C_CNC SUBROUTINE FORTVAX
C
C PURPOSE          Produces (CNCC) CNC Concentration (568) as a real number
C
C DESCRIPTION      Takes 16 bit word from the DRS interface which contains
C                  four decimal digits as BCD (leftmost digit being the highest
C                  nibble). This word is recorded as DRS parameter 50 (CNC).
C                  The four decimal digits represent a real number of the form:
C
C                                    ab.cEd
C                  
C                  which is calculated thus:
C
C                            (a + b/10 + y/100) * 10**d
C                  
C                  Checks are made that no hexadecimal figures are included
C                  in the initial nibbles. Any data including these returns
C                  a value of 0.0 and is flagged 3. (If the 4 BCD digits
C                  return AAAA it means there was no response from the CNC
C                  whilst EEEE means the CNC responded with ERROR.)
C
C VERSION          1.00 21/03/97 D H ANDERSON 
C
C ARGUMENTS        IRAW(64,512) I*4 IN  Up to 64 samples for up to 512 DRS pars
C                  IFRQ(512)    I*4 IN  Sample rate of each DRS par (0-64)
C                  RCONST(64)   R*4 IN  Inputs constants (none used by C_CNC)
C                  RDER(64,1024)R*4 OUT Output array of up to 64 samples for
C                                       each of 1024 parameters
C
C CHANGES          
C
********************************************************************************
      SUBROUTINE C_CNC(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.00'
      IMPLICIT NONE
      INTEGER*4 IRAW(64,512)   !Raw data array (64 - need only be 1 here)
      INTEGER*4 IFRQ(512)      !Raw data frequency (always 1 in this case)
      REAL*4    RCONST(64)     !Constants array (64 - need only be 1 here)
      REAL*4    RDER(64,1024)  !Derived data array, treated as integer
      INTEGER*4 IBITS          !Bit (nibble) extraction from CNC 
      INTEGER*4 IUNI           !Unit value converted from hexadecimal nibble
      INTEGER*4 ITEN           !Tenths value converted from hexadecimal nibble 
      INTEGER*4 IHUN           !Hundredths value converted from hex nibble
      INTEGER*4 IEXP           !Exponential value converted from hex nibble 
      INTEGER*4 CNC,CNCC

C This routine uses parameter       50
C This routine produces parameter  568
C At sample rates of (hz)            1
C This routine uses no constants
C
      PARAMETER (CNC=50,CNCC=568)

C
C CNC - The contents of parameter 50 are used to calculate parameter 568.
C This data will comprise the 16 DRS bits as four BCD digits (but NO flag bits
C input). 
C Input/output sample rate is 1 Hz.
C
C read the 4 decimal digits (nibbles) from the single 16 bit word
C
      IUNI = IBITS(IRAW(1,CNC),12,4) !Units value for calc.
      ITEN = IBITS(IRAW(1,CNC),8,4)  !Tenths value for calc.
      IHUN = IBITS(IRAW(1,CNC),4,4)  !Hundedths value for calc.
      IEXP = IBITS(IRAW(1,CNC),0,4)  !Exponential value for calc.
C
C Check for error data:- any hexadecimals (ie. AAAA or EEEE) 
C and flag as 3 with value 0.0 then return for next seconds data
C
      IF (IUNI.GT.9 .OR. ITEN.GT.9 .OR. IHUN.GT.9 .OR. IEXP.GT.9) THEN
        RDER(1,CNCC) = 0.0
        CALL ISETFLG(RDER(1,CNCC),3)
      ELSE
C
C Calculate value for CNCC output
C
        RDER(1,CNCC) = (IUNI + (ITEN/10.) + (IHUN/100.)) * (10**IEXP)
        IF(RDER(1,CNCC).EQ.0.) THEN
          CALL ISETFLG(RDER(1,CNCC),3) !Warming up
        ELSE
          CALL ISETFLG(RDER(1,CNCC),0)
        ENDIF
      ENDIF

      RETURN
      END
