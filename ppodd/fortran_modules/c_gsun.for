C
C ROUTINE          C_SUN         SUBROUTINE FORTVAX  C_SUN.FOR
C
C PURPOSE          PUT SOLAR ZENITH AND AZIMUTH ANGLES IN MFD
C
C DESCRIPTION      Given date, time and location on the earth's
C                  surface this routine puts a solar zenith and
C                  azimuth angle in the array of derived parameters.
C                  It computes a value once every second. The 
C                  angles are only obtained if all the flags are
C                  set to less than 3 and the date, time and location
C                  are all within sensible limits. Any flags set on input
C                  are also set in the solar angles derived. If
C                  the input is in error or the flags are set to 3
C                  a value of -99. is returned for ZEN and AZIM.
C                  To test the routine:
C                  $ FOR C_SUN
C                  $ FOR TEST_C_SUN
C                  $ LINK TEST_C_SUN,C_SUN
C                  Ensure contents of files RCONST.DAT and TEST_C_SUN.DAT
C                  contain simulated data you require to test the routine
C                  with.
C
C VERSION          1.02  1st May 1992   J.A.Smith
C
C ARGUMENTS        RDER(1,515)  R*4 IN Time GMT (seconds from midnight)
C                  RDER(1,550)  R*4 IN Omega latitude degrees (north +ve)
C                  RDER(1,551)  R*4 IN Omega longitude degrees (east +ve)
C               or RDER(1,541)  R*4 IN INU latitude degrees (north +ve)
C               or RDER(1,542)  R*4 IN INU longitude degrees (east +ve)
C                  RCONST(1)    R*4 IN Day in month (1-31)
C                  RCONST(2)    R*4 IN Month in year (1-12)
C                  RCONST(3)    R*4 IN Year (eg 1984)
C                  RDER(1,642)  R*4 OUT Solar azimuth in degrees
C                  RDER(1,643)  R*4 OUT Solar zenith in degrees
C
C SUBPROGRAMS      S_SUN , ITSTFLG, ISETFLG
C
C CHANGES          01 Range checks for input data now done in S_SUN 
C                     RWS 30/10/90
C                1.02 Check added if time RSECS has reached midnight and
C                     if so to reduce RSECS to less than 86400 s and increase
C                     the date.  JAS 1/5/92
C                1.03 Following the demise of the Omega, now uses INU position
C                     for flights after 30/09/97.  Note that this routine is
C                     now always called by CALIBRATE, even if neither Omega or
C                     INU were available.  WDNJ 20/10/97
C                1.04 Now strips flags from data before use.  WDNJ 22/12/97
C                1.05 Can take GIN input 05/09/07
C#########################################################################
      SUBROUTINE C_GSUN ( IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.05'
C
      INTEGER*4 IRAW(64,512), IFRQ(512), IFLAG(6)
      INTEGER*4 DAYM(12)/31,29,31,30,31,30,31,31,30,31,30,31/
      INTEGER*4 IMIDNIGHTS           ! added for v1.02
      REAL*4    RCONST(64), RDER(64,1024)
      LOGICAL   BAD_INPUT
C
      RSECS = RDER(1,515)            ! Seconds elapsed since midnight GMT
      IDAY = INT(RCONST(1))          ! Date in month
      IMON = INT(RCONST(2))          ! Month in Year
      IYR = INT(RCONST(3))           ! Year
      IF((IYR.EQ.1997.AND.IMON.GE.10).OR.IYR.GT.1997) THEN
        RLAT = RDER(1,541)           ! INU latitude
        RLON = RDER(1,542)           ! INU longitude
	  IF(ITSTFLG(RDER(1,610)).LT.3)RLAT=RDER(1,610) !GIN latitude
	  IF(ITSTFLG(RDER(1,611)).LT.3)RLON=RDER(1,611) !GIN longitude
      ELSE    
        RLAT = RDER(1,550)           ! Omega latitude
        RLON = RDER(1,551)           ! Omega longitude
      END IF
C
      BAD_INPUT = .FALSE.
C
C   Check flags and only proceed if all less than 3
C
      DO I = 1 , 3
      IFLAG(I) = ITSTFLG(RCONST(I))
      ENDDO            
      IFLAG(4) = ITSTFLG(RSECS)
      IFLAG(5) = ITSTFLG(RLAT)
      IFLAG(6) = ITSTFLG(RLON)
      CALL ISETFLG(RSECS,0)
      CALL ISETFLG(RLAT,0)
      CALL ISETFLG(RLON,0)
C
      IMAXFL = 0
      DO I = 1 , 6
      IMAXFL = MAX ( IMAXFL , IFLAG(I) )       ! Get highest flag value
      IF (IFLAG(I) .GE. 3)THEN
        BAD_INPUT = .TRUE.
        CALL ISETFLG ( AZIM , 3 )             ! Set invalid data flags
        CALL ISETFLG ( ZEN  , 3 )
      ENDIF
      ENDDO
C
C If input parameters OK proceed
C
      IF ( .NOT. BAD_INPUT )THEN
C.........................................................................
C v1.02 If time has run over midnight reduce RSECS to less than 24 hours of
C	seconds, ( 86400 ). The day of the month IDAY is then increased by 
C	the number of midnights passed over.  
C	If this gives too many days for the month then IDAY is set to the 
C	first day and IMON to the next month. 
C	If the data has crossed into a New Year then IMON is set to January
C	and the year is incremented. 
C
      IF (RSECS.GE.86400.) THEN
          IMIDNIGHTS = (NINT(RSECS))/86400
          RSECS = RSECS - REAL(IMIDNIGHTS*86400)
          IDAY = IDAY + IMIDNIGHTS
          IF (MOD(IYR,4).NE.0) DAYM(2)=28  !reduce February if not a leap year
          IF (IDAY.GT.DAYM(IMON)) THEN
              IDAY = IDAY - DAYM(IMON)
              IMON = IMON + 1
              IF (IMON.EQ.13) THEN
                  IMON = 1
                  IYR = IYR + 1
              ENDIF
          ENDIF
      ENDIF
C.........................................................................
C
C  Now compute solar zenith and azimuth angle
C
      CALL S_SUN(IDAY,IMON,IYR,RSECS,RLAT,RLON,AZIM,ZEN) 

C
C  Flag values with highest input flag value
C
C  If azimuth or zenith angle not computed in S_SUN set flags to 3
C
      IF (AZIM.EQ.-99) THEN 
        CALL ISETFLG(AZIM,3) 
      ELSE
        CALL ISETFLG(AZIM,IMAXFL)
      ENDIF

      IF (ZEN.EQ.-99) THEN 
        CALL ISETFLG(ZEN,3) 
      ELSE
        CALL ISETFLG(ZEN,IMAXFL)
      ENDIF
C                                                        
      ELSE
      BAD_INPUT = .TRUE.
      AZIM = -99.0
      ZEN = -99.0
      CALL ISETFLG ( AZIM , 3 )             ! Set invalid data flags
      CALL ISETFLG ( ZEN  , 3 )
C
      ENDIF
C
C  Transfer to output array
C
      RDER(1,642) = AZIM
      RDER(1,643) = ZEN
C         
      RETURN
      END
