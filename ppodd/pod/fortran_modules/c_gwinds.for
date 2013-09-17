C
C ROUTINE          C_GWINDS SUBROUTINE FORTVAX
C
C PURPOSE          Computes raw winds from TAS, vanes, and INS data
C
C DESCRIPTION      Computes values of the three wind components, using true
C                  airspeed, angle of attack and sideslip, and INS velocity,
C                  attitude, and attitude rate information. Note that at this
C                  stage the INS data have not been corrected for drift, so
C                  these are 'raw' winds, which will normally be corrected
C                  later as part of the interactive renavigation processing. 
C                  Once errors have been evaluated for the three INS velocity
C                  components, they can be applied directly to the three wind
C                  components; the wind components do not need to be recomputed 
C                  from scratch.  To show that the winds are 'raw' all values
C                  of U, V and W are increased by 1000 m/s by this routine.  
C                  This makes it easy to see that normal (flagged 0 or 1) data
C                  are 'raw', but it may not be enough to say unabiguously 
C                  whether data that are already bad (flagged 2 or 3) are 'raw'
C                  or 'corrected'.
C
C                  The processing will handle the case that the INS is mounted
C                  off the boom axis, provided its position is specified in
C                  the flight constants file, using the INSPOSN keyword.  If
C                  the INS position is not specified then it is assumed to be
C                  in the nose bay, 7.06m behind the vanes, but on the axis of
C                  the boom.  All data is assumed to be at 32 Hz.
C                  
C                  This routine will not be called if there is no True
C                  Airspeed, or no INS information (with the exception of roll
C                  rate).  If there is no information from the angle of attack
C                  and sideslip vanes, winds will be computed using values of
C                  zero for these angles flagged with
C                  1's.  If there is no roll rate available (this wasn't
C                  recorded for the Ferranti 1012 INS), a value of 0 is used. 
C                  This doesn't matter if the INS is located on the boom axis,
C                  since in this case roll rate has no effect on winds.
C                  
C                  The output vertical wind takes the worst flag present on the
C                  AOA, VZ, TAS and pitch data.  The output horizontal wind
C                  components take the worst flag present on the AOSS, VN, VE,
C                  TAS, and heading data.  This is suitable when the
C                  aircraft is not banking and reflects the fact that good
C                  horizontal winds can be found even when the vertical
C                  velocity is bad.  However this flagging scheme fails to 
C                  reflect coupling between the vertical and horizontal
C                  measurement when the aircraft is banking.
C                  In addition horizontal wind components greater
C                  than 100 m/s and vertical components greater than 25 m/s
C                  are flagged with 2's, and if the change between adjacent
C                  samples (at 32 Hz) is greater than 1 m/s a flag of 2 is
C                  also applied.
C
C                  Input parameters (all at 32 Hz except 515):
C
C                  Para 515   Time, secs
C                  Para 779   Turb.probe dry true airspeed, m s-1
C                  Para 548   Angle of attack, deg
C                  Para 549   Angle of side slip, deg
C                  Para 558   INS velocity north, m s-1
C                  Para 559   INS velocity east, m s-1
C                  Para 557   INS vertical velocity, m s-1
C                  Para 560   INS roll, deg
C                  Para 561   INS pitch, deg
C                  Para 562   INS heading, deg
C                  Para 567   INS roll rate, deg s-1 (optional)
C                  Para 565   INS pitch rate, deg s-1
C                  Para 566   INS yaw rate, deg s-1
C
C                  Constants:
C
C                  RCONST(1)  Distance of vanes ahead of INS, m (optional)
C                  RCONST(2)  Distance of vanes to port of INS, m (optional)
C                  RCONST(3)  Distance of vanes above INS, m (optional)
C
C                  Output parameters (all at 32 Hz):
C
C                  Para 714   Northward wind component + 1000, m s-1
C                  Para 715   Eastward wind component + 1000, m s-1
C                  Para 716   Vertical wind component + 1000, m s-1
C            
C VERSION          1.00  10-5-93  W.D.N.JACKSON
C
C ARGUMENTS        IRAW(64,512) I*4 IN  Up to 64 samples for up to 512 DRS pars
C                  IFRQ(512)    I*4 IN  Sample rate of each DRS par (0-64)
C                  RCONST(64)   R*4 IN  Inputs constants
C                  RDER(64,1024)R*4 OUT Output array of up to 64 samples for
C                                       each of 1024 parameters
C
C CHANGES          1.01  20-04-98 W.D.N.JACKSON
C                  Error in computation of airspeed corrected. 
C                  1.02  14-06-2004 Phil Brown
C                  AoA and AoSS now compulsory input parameters to ensure
C                  this routine gets called after C_TURB
C                  1.03  09/07/04 Phil Brown
C                  Input TAS parameter is now 779 (Turb.probe dry TAS)
C                  1.04  25/08/04 Phil Brown
C                  Temporary. Suspend rate-of-change checking on winds.
C                  1.05  29/11/04 Phil Brown
C                  Temporary. Check flagging of RU,RV,RW when returned to try
C                  to suppress FLTINV errors.
C                  1.06  05/09/07 Dave Tiddeman
C                  Will use GIN inputs if available rather than INU
C
********************************************************************************
      SUBROUTINE C_GWINDS(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.04'
      INTEGER*4 IRAW(64,512)      !Raw data array
      INTEGER*4 IFRQ(512)         !Raw data frequency
      REAL*4    RCONST(64)        !Constants array
      REAL*4    RDER(64,1024)     !Derived data array
      INTEGER*4 VN,VE,VZ,ROL,PIT,HDG,ROLR,PITR,YAWR
C
C This routine uses the following parameters (note that the absence of AOA,
C AOSS or roll rate will not stop C_WINDS from being called).  All parameters,
C except time, are at 32 Hz:
C
      PARAMETER GMT=515                !Time, secs
      PARAMETER TAS=779                !True airspeed, m s-1
      PARAMETER AOA=548                !Angle of attack, deg
      PARAMETER AOS=549                !Angle of side slip, deg
C
C This routine takes three constants from the RCONST array.  They are
C all optional and if not specified will be defaulted to the position of the
C H423 INU on the 146 Core Console (16.002,-0.8128,-0.4390 m).
C
      PARAMETER PL=1                   !Const dist of vanes ahead of INS
      PARAMETER PM=2                   !Const dist of vanes to port of INS
      PARAMETER PN=3                   !Const dist of vanes above INS
C
C This routine computes the following parameters, all at 32 Hz:
C Note that TARDIS conventially labels parameter 714, Northerly component, as V
C and parameter 715, Easterly component, as U.
C
      PARAMETER U=714                  !Northward wind component, m s-1
      PARAMETER V=715                  !Eastward wind component, m s-1
      PARAMETER W=716                  !Vertical wind component, m s-1
C
C Set LFLAG to false if you want to treat all data as unflagged.
C
      DATA LFLAG   /.TRUE./           !Set false if want to ignore flagging

      DATA RLSTSEC /-2.0/              !Initial dummy value for last sec processed
      
      DATA VN/558/
      DATA VE/559/
      DATA VZ/557/
      DATA ROL/560/
      DATA PIT/561/
      DATA HDG/562/
      DATA ROLR/567/
      DATA PITR/565/
      DATA YAWR/566/
      

      SAVE
      IF(ITSTFLG(RDER(1,613)).EQ.0)THEN
        VN=613                 !GIN velocity north, m s-1
        VE=614                 !GIN velocity east, m s-1
        VZ=615                 !GIN vertical velocity, m s-1
        ROL=616                !GIN roll, deg
        PIT=617                !GIN pitch, deg
        HDG=618                !GIN heading, deg
        ROLR=622               !GIN roll rate, deg s-1 (optional)
        PITR=623               !GIN pitch rate, deg s-1
        YAWR=624               !GIN yaw rate, deg s-1
      ENDIF
        
      RDEFAOA=0.0                      !If not specified AOA is 0.0 flagged 1
      CALL ISETFLG(RDEFAOA,1)
      RDEFAOS=RDEFAOA                  !If not specified AOSS is 0.0 flagged 1

      IF(.NOT.LFLAG) THEN              !Ignore flagging
        DO I=1,32                      !For each sample in second
          CALL C_WINDS_UVW(RDER(I,TAS),RDER(I,AOA),RDER(I,AOS),
     -        RDER(I,VN),RDER(I,VE),RDER(I,VZ),
     -        RDER(I,HDG),RDER(I,PIT),RDER(I,ROL),
     -        RCONST(PL),RCONST(PM),RCONST(PN),
     -        RDER(I,YAWR),RDER(I,PITR),RDER(I,ROLR),
     -        RDER(I,U),RDER(I,V),RDER(I,W))
        END DO
      ELSE                             !Apply flags
        RL=RCONST(PL)                  !Get the INS position offsets
        RM=RCONST(PM)
        RN=RCONST(PN)
        IF(ITSTFLG(RL).GE.2) RL=16.002 !Use default values if not available
        IF(ITSTFLG(RM).GE.2) RM=-0.8128
        IF(ITSTFLG(RN).GE.2) RN=-0.4390
        LCONSEQ=.FALSE.                !Will set true if this is next second
        IF(RDER(1,GMT).EQ.RLSTSEC+1.0) LCONSEQ=.TRUE.
        RLSTSEC=RDER(1,GMT)            !Save current time
	  
        DO I=1,32                      !For each sample in second
          RTAS=RDER(I,TAS)             !Get the input values
          RAOA=RDER(I,AOA)
          RAOS=RDER(I,AOS)
          RVN=RDER(I,VN)
          RVE=RDER(I,VE)
          RVZ=RDER(I,VZ)
          RHDG=RDER(I,HDG)
          RPIT=RDER(I,PIT)
          RROL=RDER(I,ROL)
          RYAWR=RDER(I,YAWR)
          RPITR=RDER(I,PITR)
          RROLR=RDER(I,ROLR)
          IF(ITSTFLG(RAOA).GE.2) RAOA=RDEFAOA !Set AOA to 0 if missing
          IF(ITSTFLG(RAOS).GE.2) RAOS=RDEFAOS !Set AOSS to 0 if missing
          IF(ITSTFLG(RROLR).GE.2) RROLR=0.0   !Set roll rate to 0 if missing
          IHFLAG=MAX(ITSTFLG(RTAS),ITSTFLG(RAOS), !Compute worst horiz flag
     -        ITSTFLG(RVN),ITSTFLG(RVE),ITSTFLG(RHDG))
          IWFLAG=MAX(ITSTFLG(RTAS),ITSTFLG(RAOA), !Compute worst vert flag
     -        ITSTFLG(RVZ),ITSTFLG(RPIT))
          CALL ISETFLG(RTAS,0)         !Clear any flags before computation
          CALL ISETFLG(RAOA,0)
          CALL ISETFLG(RAOS,0)
          CALL ISETFLG(RVN,0)
          CALL ISETFLG(RVE,0)
          CALL ISETFLG(RVZ,0)
	  IF(VN.NE.558)RVZ=-RVZ
          CALL ISETFLG(RHDG,0)
          CALL ISETFLG(RPIT,0)
          CALL ISETFLG(RROL,0)
          CALL ISETFLG(RYAWR,0)
          CALL ISETFLG(RPITR,0)
          CALL ISETFLG(RROLR,0)
          CALL C_WINDS_UVW(RTAS,RAOA,RAOS,RVN,RVE,RVZ,RHDG,RPIT,RROL,
     -        RL,RM,RN,RYAWR,RPITR,RROLR,RU,RV,RW) !Compute wind components
          IUFLAG=IHFLAG                !Propagate worst case flag for each comp
          IVFLAG=IHFLAG
          IF(ABS(RU).GT.100.0) IUFLAG=MAX(IUFLAG,2) !Flag if out of range
          IF(ABS(RV).GT.100.0) IVFLAG=MAX(IVFLAG,2)
          IF(ABS(RW).GT.25.0) IWFLAG=MAX(IWFLAG,2)
          CALL ISETFLG(RU, 0)          ! ensure winds have zero flag
          CALL ISETFLG(RV, 0)
          CALL ISETFLG(RW, 0)
	  IF(VN.EQ.558)THEN
           RU=RU+1000.                  !Add offset to show winds are 'raw'
           RV=RV+1000.
           RW=RW+1000.
	  ENDIF

C suspend rate-of-change checks.
C          IF(ITSTFLG(RLSTU).EQ.0.AND.LCONSEQ.AND.ABS(RLSTU-RU).GT.1.0)
C     -        IUFLAG=MAX(IUFLAG,2)     !Flag if rate of change too high
C          IF(ITSTFLG(RLSTV).EQ.0.AND.LCONSEQ.AND.ABS(RLSTV-RV).GT.1.0)
C     -        IVFLAG=MAX(IVFLAG,2)
C          IF(ITSTFLG(RLSTW).EQ.0.AND.LCONSEQ.AND.ABS(RLSTW-RW).GT.1.0)
C     -        IWFLAG=MAX(IWFLAG,2)

          CALL ISETFLG(RU,IUFLAG)      !Apply flags to result
          CALL ISETFLG(RV,IVFLAG)
          CALL ISETFLG(RW,IWFLAG)
          RDER(I,U)=RU                 !Transfer results to output array
          RDER(I,V)=RV
          RDER(I,W)=RW
          RLSTU=RU                     !Save latest values
          RLSTV=RV
          RLSTW=RW
          LCONSEQ=.TRUE.               !Further samples in second are consequetve
        END DO
      END IF
      RETURN
      END
