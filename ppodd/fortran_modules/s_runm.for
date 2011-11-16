
C---------------------------------------------------------------------------
C
C ROUTINE  	   S_RUNM      SUBROUTINE FORTVAX
C     
C PURPOSE 	   COMPUTE RUNNING MEAN OF SAMPLES.
C
C DESCRIPTION      
C                  This routine computes the 'running' mean of a number of
C                  preceding samples. The first time it is called it will
C                  return only the value of the input sample. The second
C                  time it will return the mean of the first and second inputs,
C                  and so on, up to the number supplied by user argument.
C                  Invalid input samples are handled as follows:
C                   No previous good samples available: return input flagged "2"
C                      (if input received with flag 3, that flag overrides)
C                   One previous good sample: return that flagged "1"
C                   Two - (IREQNUM-1)"  " " : return mean of those flagged "0"
C
C USAGE            Initialising:
C                  Before calling this routine for the first time; users MUST 
C                  set up the corresponding symbols in the calling program:
C                  RBUFF (n) declared to size n = IREQNUM as below.
C                  IPTR   = 1
C                  IBCNT  = 0
C                  IREQNUM= However many samples user wants to take running mean
C
C                  If the routine is required to take means of more than one
C                  set of data alternately, then the set of arguments MUST be
C                  unique for each data set.
C
C METHOD                 
C                     The routine maintains a buffer which can accept up to
C                     600 samples i.e ten minutes worth for a 1Hz input.
C                     This buffer is treated as a circular queue. i.e. the
C                     pointer is advanced up to a defined limit (n),above which
C                     it is reset  to its lowest limit (1).
C                  1. Check validity of input arguments; return input flagged
C                     "3" if any argument out of range.
C                  2. If the buffer count is zero, initialise the required
C                     locations of the buffer to 'invalid-flagged' values
C                     Also initialise buffer pointer to start,and SUMM to 0.
C                  3. Each input sample has its validity flag noted 
C                  4. If the content of buffer location (IPTR) is valid:
C                       decrease the sum by the content of the buffer loc.
C                       (and) If the input sample is also valid:
C                               increase the sum by value of the input
C                       (else)If the input sample was found invalid:
C                               decrement count of valid buffer locations by 1
C                     (else) If the content of buffer location (IPTR)is invalid:
C                       (and) If the input sample is valid:
C                               increment count of valid buffer locations by 1
C                               decrease the sum by the value of input sample.
C                     (in all cases: update buffer location with current sample)
C                  5. Derive a mean for return.
C                     If no good samples are present in buffer (and therefore
C                        accumulated in SUMM) return the input - flagged as it
C                        was received.
C                     If this was the first good sample; return it flagged as
C                        "1" to indicate that a true mean is not yet available.
C                     If between two and the maximum required values fill the 
C                        buffer; return the mean= SUMM /count, flagged "0". 
C                  6. Advance the pointer along the buffer; when it advances 
C                     beyond the maximum length (IREQNUM) of number sample to
C                     be averaged over, reset it to start of buffer position.
C                   
C                  
C VERSION	   1.00  120290   A.D.HENNINGS
C                                                                  
C ARGUMENTS:       REAL*4 IN/OUT  RBUFF(600)!Buffer of valid samples (max=600)
C                  INT*4  IN/OUT  IPTR      !Pointer to current Buffer location 
C                  INT*4  IN/OUT  IBCNT     !Count of good samples in buffer.
C                  INT*4  IN      IREQNUM   !Samples required for 'full' buffer.
C                  REAL*4 IN      RIN       !Current input sample
C                  REAL*4 IN/OUT  SUMM      !Running total of 'good' samples. 
C                  REAL*4    OUT  AVRG      !Mean of last good samples.returned
C
C
C SUBPROGRAMS  	   ISETFLG, ITSTFLG
C
C REFERENCES 	   
C
C CHANGES          None
C------------------------------------------------------------------------------
 	SUBROUTINE S_RUNM ( RBUFF, IPTR,  IBCNT,
     -                       IREQNUM, RIN,   SUMM,  AVRG)
CDEC$ IDENT 'V1.00'
C
      IMPLICIT NONE
C declare arguments.

      INTEGER*4 IPTR, IBCNT, IREQNUM
      REAL*4    RIN,  SUMM,  AVRG,  RBUFF(600)

C internal variables.
      INTEGER*4  ITSTFLG, ISFLAG, IRFLAG, IAFLAG, IR
      LOGICAL*4 OBUFF, OINP                  !Validity of Buffer/Input contents


C check input arguments valid
      
      IF (     IREQNUM.LT.1 .OR. IREQNUM.GT. 600
     -    .OR. IPTR   .LT.1 .OR. IPTR   .GT. IREQNUM
     -    .OR. IBCNT  .LT.0 .OR. IBCNT  .GT. IREQNUM) THEN 

         AVRG = RIN                          !Return input flagged totally
         CALL ISETFLG(AVRG,3)                !invalid.
         RETURN
      ENDIF

C initialise RBUFF to invalid flags
      
      IF (IBCNT .EQ.0) THEN
          DO IR =1,IREQNUM
          CALL ISETFLG (RBUFF(IR),2)         !set flag bits of all required 
          END DO                             !buffer area.
          SUMM  = 0.                         !Re-initialise accumulated samples
          IPTR  = 1                          !Reset ptrs to initial position.
      ENDIF

C test input sample's flag

      ISFLAG = ITSTFLG(RIN)                  !Validity of input data good=0,1
      IF (ISFLAG .LT. 2) THEN 
         OINP = .TRUE.                       !Valid input data
      ELSE
         OINP = .FALSE.
      ENDIF

C test current RBUFF content's flag

      IRFLAG = ITSTFLG(RBUFF(IPTR))          !Validity of buffer data good:0,1
      IF (IRFLAG .LT. 2) THEN 
         OBUFF= .TRUE.                       !Valid data in buffer slot pointed
      ELSE                                   !to by IPTR.
         OBUFF= .FALSE.
      ENDIF

C update buffer, counts and totals according to validity of input and buffer.

      IF (OBUFF ) THEN
        SUMM = SUMM- RBUFF(IPTR)
        IF (OINP) THEN
           SUMM = SUMM + RIN
        ELSE                         ! -- else invalid input sample
           IBCNT = IBCNT - 1
        ENDIF
      ELSE                           ! -- invalid buffer content
        IF (OINP) THEN               ! and  valid input sample.   
          IBCNT = IBCNT + 1
          SUMM = SUMM + RIN
        ENDIF
      ENDIF       

      RBUFF(IPTR) = RIN                      !Always retain input, valid or not.

C and derive a return (mean) value.

      IF (IBCNT .EQ. 0) THEN                 !No valid samples available
          AVRG  = RIN                        !Return invalid input
          IAFLAG= 2                          !.. flagged as such.
          IF (ISFLAG .EQ. 3)IAFLAG=ISFLAG    !Input could have had flag='3'

      ELSE IF (IBCNT .EQ. 1) THEN            !Return the input as 'mean'
          AVRG =  RIN
          IAFLAG = 1                         !Suspect
          IF (ISFLAG .GT. 1)IAFLAG=ISFLAG    !Input could have been invalid

      ELSE IF (IBCNT .GT. 1) THEN
          AVRG    = SUMM/FLOAT(IBCNT)        !Take new mean of at least 2 good.
          IAFLAG = 0                         !good 
      ENDIF

      CALL ISETFLG(AVRG,IAFLAG)              !Return flagged average
      IPTR = IPTR + 1                        !..and advance  pointer
      IF (IPTR .GT. IREQNUM) IPTR = 1        !  but reset if upper limit reached

      RETURN
      END

