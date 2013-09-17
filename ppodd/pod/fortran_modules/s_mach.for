C---------------------------------------------------------------------------
C
C ROUTINE  	   S_MACH      SUBROUTINE FORTVAX
C     
C PURPOSE 	   COMPUTE MACH  NO. FROM STATIC PRESSURE AND PITOT STATIC.
C
C DESCRIPTION      The two input arguments Static and Pitot static pressure
C                  (normally taken from samples of parameters 576 and 577)
C                  have their flag values noted. The input arguments are
C                  ranged-checked, with an out-of-range condition giving
C                  a flag value of two. The input flag values, together
C                  with results of range-checking give a 'worst' flag value.
C                  If the worst value is <2 computation of Mach number proceeds.
C                  Otherwise the value of Mach no will be set to zero later.
C                  The value of Static pressure is also checked that it is 
C                  not zero, to avoid a 'divide-by-zero' error.
C                  Computation of the Mach no. 'RMACH' proceeds using the 
C                  formula below.
C                  The return value of Mach no. has its flag area set to the
C                  'worst' flag value found.
C
C                  RMACH = SQRT( 5.* ((1.+ PITOT/RSTAT)**(2./7.) - 1.)) 
C
C VERSION	   1.00    070290   A.D.HENNINGS
C                                                                  
C ARGUMENTS        RSTAT - R*4 IN    Static pressure       (100 - 1050 mb.)
C                  PITOT - R*4 IN    Pitot static pressure (0   - 125  mb.)
C                  RMACH - R*4 OUT   MACH NO.                 [none ]
C
C SUBPROGRAMS  	   ISETFLG, ITSTFLG
C
C REFERENCES 	   Code adapted from MRF1/HORACE
C
C CHANGES          V1.01  020490  Include check for divide-by-zero error
C                                Return 0.0 flagged '3' if Static pressure
C                                input is zero.
C------------------------------------------------------------------------------
 	SUBROUTINE S_MACH (RSTAT,PITOT,RMACH)
CDEC$ IDENT 'V1.01'
C
      IMPLICIT NONE
      INTEGER*4  ITSTFLG, IFLAG, IFLAG2
      REAL*4     RSTAT,PITOT,RMACH ,SRSTAT,SPITOT
      REAL*4     STMAX,STMIN,PIMAX,PIMIN
      PARAMETER (STMAX=1050. ,STMIN= 100. ,PIMAX=125. ,PIMIN=0.) !Static,Pitot
                                                                 !range limits.
C------------------------------------------------------------------------------

      SPITOT = PITOT                                        !Put input arguments
      SRSTAT = RSTAT                                        !into placeholders.

      IFLAG  =ITSTFLG (SRSTAT)                              !Test validity of
      IF (SRSTAT .GT. STMAX .OR. SRSTAT .LT.STMIN) IFLAG=2
      IFLAG2 =ITSTFLG (SPITOT)                              !input arguments
      IF (SPITOT .GT. PIMAX .OR. SPITOT .LT.PIMIN) IFLAG2=2

      IF (IFLAG.LT.IFLAG2 ) IFLAG=IFLAG2                   !Choose worst flag.

      IF (SRSTAT .EQ. 0   ) THEN                           !Divide-by-zero err?
        RMACH  = 0.                                        !Zero return value
        IFLAG  = 3                                         !Set flag
        CALL ISETFLG (RMACH,IFLAG)                         !Flag result invalid
        RETURN                                             !.. cannot proceed.
      ENDIF

                                                           !Exponentiation err?
      IF (SPITOT/SRSTAT .LT.0)IFLAG=3                      !Must be +ve or eqn
                                                           !below will fail.
                                                         
      CALL ISETFLG(SPITOT,0)                               !Clear flag bits
      CALL ISETFLG(SRSTAT,0)                               !before computation

C  If flag not fatal, compute Mach no.

      IF (IFLAG.LT.3 ) THEN                                
        RMACH = SQRT( 5.* ((1.+ SPITOT/SRSTAT)**(2./7.) - 1.)) 
      ELSE
        RMACH = 0.0                                        !Return flagged zero
      ENDIF                                                !if input is invalid

      CALL ISETFLG (RMACH,IFLAG)                           !Re-flag result
      RETURN

      END

