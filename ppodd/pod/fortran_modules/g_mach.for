C-------------------------------------------------------------------------------
C
C ROUTINE  	   G_MACH      SUBROUTINE FORTVAX                   [G_MACH.FOR]
C     
C PURPOSE 	   COMPUTE MACH  NO. FROM STATIC PRESSURE AND PITOT STATIC.
C
C DESCRIPTION      The two input arguments are Static and Pitot static pressure.
C                  The value of Static pressure is checked to make sure it is 
C                  not zero, to avoid a 'divide-by-zero' error. The division of
C		   Pitot staic pressure by Static pressure is check to make
C		   sure that it is not negative, is so the Mach number is set 
C		   to 0.0.
C                  Computation of the Mach no. 'RMACH' proceeds using the 
C                  formula below.
C                  
C                  RMACH = SQRT( 5.* ((1.+ PITOT/RSTAT)**(2./7.) - 1.)) 
C
C VERSION	   1.00    26-02-92	M.J.Glover
C                                                                  
C ARGUMENTS        RSTAT - R*4 IN    Static pressure       (100 - 1050 mb.)
C                  PITOT - R*4 IN    Pitot static pressure (0   - 125  mb.)
C                  RMACH - R*4 OUT   MACH NO.                 [none ]
C
C SUBPROGRAMS  	   None.
C
C REFERENCES 	   Code adapted from SCILIB:S_MACH
C
C CHANGES          None.
C                  
C------------------------------------------------------------------------------

	SUBROUTINE G_MACH (RSTAT,PITOT,RMACH)
CDEC$ IDENT 'V1.00'
C
	IMPLICIT NONE

	INTEGER*4  IFLAG

	REAL*4     RSTAT,PITOT,RMACH ,SRSTAT,SPITOT

C------------------------------------------------------------------------------

      	SPITOT = PITOT                                     !Put input arguments
      	SRSTAT = RSTAT                                     !into placeholders.
	IFLAG = 0

	IF (SRSTAT .EQ. 0 ) THEN                           !Divide-by-zero err?
        	RMACH  = 0.                                !Zero return value
	        RETURN                                     !.. cannot proceed.
      	ENDIF

	IF (SPITOT/SRSTAT .LT.0)IFLAG=3                    !Must be +ve or eqn
                                                           !below will fail.
                                                         
C If flag not fatal, compute Mach no.

  	IF (IFLAG.NE.3 ) THEN                                
        	RMACH = SQRT( 5.* ((1.+ SPITOT/SRSTAT)**(2./7.) - 1.)) 
	ELSE
		RMACH = 0.0                                !Return flagged zero
	ENDIF                                              !if input is invalid

	RETURN

	END
