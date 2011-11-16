C
C ROUTINE          ITSTFLG FUNCTION FORTVAX
C
C PURPOSE          Returns the flag value of a variable
C
C DESCRIPTION      Flagged values are stored in bits 0 and 1 of a 32 bit
C                  word.  This routine simply extracts these bits and returns
C                  their value.  It will work with either REAL*4 or INTEGER*4
C                  values.
C
C VERSION          1.00  21-12-89  N.JACKSON
C
C ARGUMENTS        IVALUE    R*4 or I*4  IN   Variable with flag bits 
C                  ITSTFLG   I*4         OUT  Flag value (0-3)
C
C CHANGES          1.01  16-06-05  N.JACKSON
C                  Now uses bits 0 and 1 rather than 16 and 17 to match the
C                  IEEE S_Floating mantissa.
C
      INTEGER FUNCTION ITSTFLG(IVALUE)
CDEC$ IDENT 'V1.01'
C
      INTEGER*2 IVALUE(2)
      ITSTFLG=IVALUE(1).AND.'0003'X
      RETURN
      END
