C
C ROUTINE          S_SUN         SUBROUTINE FORTVAX  S_SUN.FOR
C
C PURPOSE          COMPUTE SOLAR ZENITH AND AZIMUTH ANGLES 
C
C DESCRIPTION      Given date, time and location on the earth's
C                  surface this routine computes a solar zenith
C                  and azimuth angle. This routine was adapted from
C                  the one used in MRF1. 
C
C VERSION          1.01   301090 R.W. SAUNDERS
C
C ARGUMENTS        IDAY  I*4 IN Day in month (1-31)
C                  IMON  I*4 IN Month in year (1-12)
C                  IYR   I*4 IN Year (eg 1984)
C                  RSECS R*4 IN Time GMT (seconds from midnight)
C                  RLAT  R*4 IN Latitude degrees (north +ve)
C                  RLON  R*4 IN Longitude degrees (east +ve)
C                  AZIM  R*4 OUT Solar azimuth in degrees
C                  ZEN   R*4 OUT Solar zenith in degrees
C
C SUBPROGRAMS      DAT_CONV
C
C REFERENCES       Air Almanac useful for checking GHA and DECL
C                  Norton's Star Atlas for equation of time
C                  Robinson N. Solar Radiation Ch 2 for useful 
C                  introduction to theory/terminology
C
C CHANGES          01 Documentation improved, range checks now done on
C                     inputs RWS 30/10/90
C
C#########################################################################
      SUBROUTINE S_SUN(IDAY,IMON,IYR,RSECS,RLAT,RLON,AZIM,ZEN) 
CDEC$ IDENT 'V1.01'
C
!      IMPLICIT NONE
      INTEGER*4 LASTDAY/0/
      INTEGER*4 DAYM(12)/31,29,31,30,31,30,31,31,30,31,30,31/
!      REAL*8 RSECS,RLAT,RLON,TWOPI,HALFPI,D2R,R2D,RCD,RCD2
!      REAL*8 Y,Y2,EQNT,SINALP,TANEQN,DECL,RSINDC,RCOSDC,RSINLT,RCOSLT
!      REAL*8 RGMT,TIME,HRA,RSINEV,RCOSEV,ZEN,COSAZ,AZIM
!      INTEGER*4 ICD,IDAY,IMON,IYR
      DATA TWOPI/6.283185/,HALFPI/1.570796/,D2R/0.017453/
      DATA R2D/57.29578/

      SAVE
C
      AZIM = -99.                    ! Initialise azimuth
      ZEN = -99.                     ! Initialise zenith
C
C   Perform range checking for inputs
C
      IF (IMON .GT. 12)THEN
        RETURN
      ENDIF
      IF (( RSECS .GE. 0. AND. RSECS .LE. 86400.) .AND.
     1   ( IYR .GT. 1950 .AND. IYR .LT. 2200 ) .AND.
     2   ( IMON .GE.1 .AND. IMON .LE. 12) .AND.
     3   ( IDAY .GE. 1 .AND. IDAY .LE. DAYM(IMON)) .AND.
     4   ( RLAT .GE. -90. .AND. RLAT .LE. 90. ) .AND.
     5   ( RLON .GE. -180. .AND. RLON .LE. 180. ) )THEN

C
C   Only call this section once per day
C________________________________________________________________________
C
      IF ( LASTDAY .NE. IDAY)THEN
C
C   First get century day (ie no. of days since 0-JAN-1900)
C
      CALL DAT_CONV(IDAY,IMON,IYR,ICD)
!      print *,IYR,IMON,IDAY,RLAT,RLON,ICD
C
      print *,sizeof(ICD)
      RCD = FLOAT(ICD) / 36525.0            ! Fraction of days elapsed this
      RCD2 = RCD*RCD                        ! century
      Y = (RCD * 36000.769 + 279.697) / 360.0 

      print *,ICD
      print *,sizeof(ICD)
      print 10,FLOAT(ICD)
      print 10,RCD
      print 10,RCD2
      print 10,Y
10    format(f17.10)
      print *,sizeof(Y)
C      Y = AMOD( Y , 1.0 ) * 360.0
      Y = AMOD( Y, 1.E0) * 360.E0
      Y2 = Y * D2R 
      print *,RCD,RCD2,Y,Y2
      print 20,Y
20    format(f17.10)
      print *,sizeof(RCD),sizeof(RCD2),sizeof(Y),sizeof(Y2)

C                       
C       Compute equation of time (in seconds) for this day 
C       (No reference for this but it gives the correct answers
C       when compared with table in Norton's star Atlas)
C
      EQNT=-((93.0+14.23*RCD-0.0144*RCD2)*SIN(Y2))-((432.5-3.71*RCD
     + -0.2063     
     + *RCD2)*COS(Y2))+((596.9-0.81*RCD-0.0096*RCD2)*SIN(2.0*Y2))-((1.4        
     + +0.28*RCD)*COS(2.0*Y2))+((3.8+0.6*RCD)*SIN(3.0*Y2))+((19.5-0.21        
     + *RCD-0.0103*RCD2)*COS(3.0*Y2))-((12.8-0.03*RCD)*SIN(4.0*Y2))            
C
C     Get solar declination for given day (radians)
C
      SINALP = SIN((Y-EQNT/240.0) * D2R)                                        
      TANEQN = 0.43382 - 0.00027*RCD
      DECL = ATAN(TANEQN*SINALP)                                              
      EQNT = EQNT / 3600.0                  ! Convert to hours
!      print *,SINALP,TANEQN,DECL,EQNT
C
C     Sine and cosine of declination                                
C
      RSINDC = SIN(DECL)                                                      
      RCOSDC = COS(DECL)
!      print *,RSINDC,RCOSDC
C
      ENDIF
C________________________________________________________________________
C
      LASTDAY = IDAY
C
      RSINLT = SIN(RLAT*D2R)                     ! Sine of lat
      RCOSLT = COS(RLAT*D2R)                     ! Cos of lat
      RGMT = RSECS / 3600.                       ! Convert secs elapsed to
C                                                ! gmt (eg 12:30 = 12.5)
C     Calculate solar zenith (degrees)                                    
C                                                                           
      TIME = ( RLON / 15.0 ) + EQNT + RGMT        ! Local solar time (hours)
      HRA = ( TIME*15.0 + 180.0 ) * D2R           ! Local hour angle (note
      RSINEV = RSINDC*RSINLT + RCOSDC*RCOSLT*COS(HRA) ! when longitude is zero
      RCOSEV = SQRT(1.0 - RSINEV*RSINEV)          ! this equals the GHA given 
      ZEN = (HALFPI - ASIN(RSINEV)) * R2D         ! in the Air Almanac)
C
C     Calculate solar azimuth (degrees)                                   
C
      COSAZ = ( RSINDC - RSINEV*RSINLT) / (RCOSLT*RCOSEV)                            
      IF (COSAZ .LT. -1.0) COSAZ = -1.0                                               
      IF (COSAZ .GT. 1.0) COSAZ = 1.0                                                 
      AZIM = ACOS(COSAZ)                                                       
      IF (AMOD(TIME+72.0,24.0) .GE. 12.0) AZIM = TWOPI-AZIM  
      AZIM = AZIM*R2D                     
C
      ENDIF
C

!      Following lines are for debugging purpose
!      =========================================
!      print *,IDAY,IMON,IYR,RSECS,RLAT,RLON,AZIM,ZEN
!      print *,RSECS,RLAT,RLON,TWOPI,HALFPI,D2R,R2D,RCD,RCD2
!      print *,Y,Y2,EQNT,SINALP,TANEQN,DECL,RSINDC,RCOSDC,RSINLT,RCOSLT
!      print *,RGMT,TIME,HRA,RSINEV,RCOSEV,ZEN,COSAZ,AZIM
!      print *,ICD,IDAY,IMON,IYR


      RETURN                                                                
      END                                                                   
C
C ROUTINE          DAT_CONV  SUBROUTINE FORTVAX
C
C PURPOSE          To convert day,mon,yr to days since 0/1/1900
C
C DESCRIPTION      Given the day, month and year this routine 
C                  computed the no. of days elpased since 
C                  Jan 0 1900, the so-called century day.
C
C VERSION          1.00   130290  R.W. SAUNDERS
C
C ARGUMENTS        IDAY  I*4 IN   Day in month (1-31)
C                  IMON  I*4 IN   Month in year (1-12)
C                  IYR   I*4 IN   Year (eg 1984)
C                  ICD   I*4 OUT  Century day 
C
C CHANGES          None
C
C#########################################################################
CDEC$ IDENT 'V1.00'
C
      SUBROUTINE DAT_CONV(IDAY,IMON,IYR,ICD)
C
      DIMENSION IMON2(12)
      DATA IMON2/0,31,59,90,120,151,181,212,243,273,304,334/
C
      IYD = IMON2(IMON) + IDAY !IYD is the number of days so far this year
      IF(MOD(IYR,4).EQ.0.AND.IMON.GT.2)IYD=IYD+1  !Leap year adjustment
      ILEAP = (IYR-1901) / 4 !Number of leap years since 1900 excluding this one
      ICD = (IYR-1900)*365 + ILEAP + IYD
C
      RETURN
      END
