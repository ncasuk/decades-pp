C----------------------------------------------------------------------------
C ROUTINE  	   S_QCPT      SUBROUTINE FORTVAX
C     
C PURPOSE 	   PERFORM RANGE CHECK AND RATE OF CHANGE CHECK ON DATA POINT.
C
C DESCRIPTION      This routine is only valid for data which varies in a linear
C                  manner. It will not Q/C data which changes in a cyclic manner
C                  such as direction/angles going through 0/360.
C                  It quality-controls a data point with respect to range limits
C                  and check rate-of-change between it, and previous/good points
C                  Elementary checks performed to discrimimate between isolated
C                  'spikes' and a new trend departing from the previous data
C                  values.
C                  Data passing checks causes flag value: 0 to be returned
C                  Data failing checks causes flag value: 2 to be returned
C
C METHOD           1. Check for a break of time between samples (whole seconds 
C                     only).
C                     If true, initialise 'last time through' variables to
C                     current values of data and time.
C                  2. Attempt to eliminate spikes; after the error count RERCNT
C                     rises above RERRMX bad points, accept next point that is 
C                     within valid range.
C                     n.b  You must initialise RERRMX > 0, suggested value: 3
C                  3. Check rate-of-change and range limits:
C                     If either fails: set the return flag to 'failed' value: 2
C                                      increment the error counter.
C                     If both valid: Accept point, set flag to 'good'  value: 0 
C                                    reset error count
C                                    retain this value as 'last good point'
C                  4. Retain last-seconds time, for comparison next time through
C
C VERSION	   1.00  290190   A.D.HENNINGS
C                  1.01  17-01-96 D Lauchlan
C
C ARGUMENTS        RSEC   REAL*4 IN     Current time; seconds from midnight
C                  RLASTM REAL*4 IN/OUT Previous time; seconds from midnight
C                  RVAL   REAL*4 IN     Data value; current sample
C                  RLASTV REAL*4 IN/OUT Previous 'good' data value
C                  RMAX   REAL*4 IN     Q/C Max limit before rejecting
C                  RMIN   REAL*4 IN      "  Min   "     "       "
C                  RCHG   REAL*4 IN      "  Rate-of-change between succ samples.
C                  RERRMX REAL*4 IN      "  No.of succ bad pts before reset.    
C                  RERCNT REAL*4 IN/OUT  "  No.of succ bad pts found so far.    
C                  IFLAG  INT*4 OUT     Return value. 0: Good  2: Failed.
C                
C CHANGES          1.01 Unused variables removed.
C                  
C------------------------------------------------------------------------------
 	SUBROUTINE S_QCPT (rsec, rlastm, rval,rlastv,
     +                     rmax, rmin, rchg, rerrmx,rercnt,iflag)
CDEC$ IDENT 'V1.01'
C
        real*4  rsec                                   !Current second-from-mid.
        real*4  rlastm                                 !Last time (secs) checked
        real*4  rval                                   !current value
        real*4  rlastv                                 !Last acceptable value
        real*4  rmax                                   !Max acceptable value 
        real*4  rmin                                   !Min acceptable value 
        real*4  rchg                                   !acctble diff bet. samps 
        real*4  rerrmx                                 !no.bad pts before reset
        real*4  rercnt                                 !No. succ. bad pts found.
        integer*4 iflag                                !Value of flag on return
C-------------------------------------------------------------------------------



C       AFTER A BREAK, OR FIRST TIME THROUGH; INITIALISE 'PREVIOUS/LAST' VALUES

        if (rsec-rlastm .gt. 1. )then
           rlastv= rval                                 !Last 'good' value; this
           rlastm= rsec-1.                              !Last second processed.
        endif

C       DISCRIMINATE BETWEEN 'SPIKES' AND A NEW TREND.

        if (rercnt .gt.rerrmx)then                  !max error pts, accept next 
          if (rval .le. rmax .and. rval .ge. rmin) then  !within range limits.
            rlastv= rval                                 !Last good val is this
            rercnt  = 1.                                 !reset error counter
          endif                                   
        endif

C       CHECK NEW VALUE IS VALID FOR RATE-OF-CHANGE, AND RANGE LIMITS.

        if ( abs(rlastv-rval) .gt. rercnt*rchg .or.        !UNACCEPTABLE
     _       rval .gt. rmax .or. rval .lt. rmin) then    !------------
             rercnt = rercnt + 1.                        !Inc count of bad pts
             iflag = 2                                   !Set ret. flag invalid
        else                                           !ACCEPTABLE
                                                       !------------
             iflag = 0                                 !Set ret. flag valid
             rlastv = rval                             !only save if its good
             rercnt = 1.0                              !reset acc err marg count
        endif

C        PRESERVE VALUES FOR NEXT  TIME THROUGH.

        rlastm = rsec                                    !Preserve last second
        return
	end
