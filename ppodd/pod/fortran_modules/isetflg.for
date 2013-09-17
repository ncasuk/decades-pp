C
C ROUTINE          ISETFLG SUBROUTINE FORTVAX
C
C PURPOSE          Sets the flag bits in a variable
C
C DESCRIPTION      Flagged values are stored in bits 0 and 1 of a 32 bit
C                  word.  This routine simply sets these bits.  It will work
C                  with either REAL*4 or INTEGER*4 values.
C
C VERSION          1.00  21-12-89  N.JACKSON
C
C ARGUMENTS        IVALUE    R*4 or I*4  IN/OUT   Variable with flag bits 
C                  IFLAG     I*4         IN       Flag value (0-3)
C
C CHANGES          1.01  16-06-05  N.JACKSON
C                  Now sets uses bits 0 and 1 instead of 16 and 17 to match
C                  IEEE S_Floating mantissa.
C
      SUBROUTINE ISETFLG(IVALUE,IFLAG)
CDEC$ IDENT 'V1.01'
C
C The routine masks off bits 0 and 1 (the lowest 2 bits of IVALUE(1)) and
C then sets them with the lowest 2 bits in IFLAG.  IFLAG is masked to 2 bits
C in case an invalid number is sent.
C
      INTEGER*2 IVALUE(2),IFLAG(2)
      IVALUE(1)=(IVALUE(1).AND.'FFFC'X).OR.(IFLAG(1).AND.'0003'X)
      RETURN
      END
