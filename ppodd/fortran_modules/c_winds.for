C
C ROUTINE          C_WINDS SUBROUTINE FORTVAX
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
C
********************************************************************************
      SUBROUTINE C_WINDS(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.04'
      INTEGER*4 IRAW(64,512)      !Raw data array
      INTEGER*4 IFRQ(512)         !Raw data frequency
      REAL*4    RCONST(64)        !Constants array
      REAL*4    RDER(64,1024)     !Derived data array
C
C This routine uses the following parameters (note that the absence of AOA,
C AOSS or roll rate will not stop C_WINDS from being called).  All parameters,
C except time, are at 32 Hz:
C
      PARAMETER GMT=515                !Time, secs
      PARAMETER TAS=779                !True airspeed, m s-1
      PARAMETER AOA=548                !Angle of attack, deg
      PARAMETER AOS=549                !Angle of side slip, deg
      PARAMETER VN=558                 !INS velocity north, m s-1
      PARAMETER VE=559                 !INS velocity east, m s-1
      PARAMETER VZ=557                 !INS vertical velocity, m s-1
      PARAMETER ROL=560                !INS roll, deg
      PARAMETER PIT=561                !INS pitch, deg
      PARAMETER HDG=562                !INS heading, deg
      PARAMETER ROLR=567               !INS roll rate, deg s-1 (optional)
      PARAMETER PITR=565               !INS pitch rate, deg s-1
      PARAMETER YAWR=566               !INS yaw rate, deg s-1
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
          RU=RU+1000.                  !Add offset to show winds are 'raw'
          RV=RV+1000.
          RW=RW+1000.

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
********************************************************************************
C
C SUBROUTINE C_WINDS_UVW
C
C Computes the three wind components, using INS velocities, attitudes,
C attitude rates, vanes location, true airspeed, and angles of attack and
C sideslip.  All data are treated as unflagged real by 4 numbers.
C
C Arguments (all R*4) - no input arguments are changed:
C
C  RTAS   In  True airspeed (m/s)
C  RAOA   In  Angle of attack (deg, +ve when vane points down)
C  RAOS   In  Angle of attack (deg, +ve when vane points to left)
C  RVN    In  INS aircraft velocity component northwards (m/s, +ve when to N)
C  RVE    In  INS aircraft velocity component eastwards (m/s, +ve when to E)
C  RVZ    In  INS aircraft verical velocity component (m/s, +ve when up)
C  RHDG   In  INS aircraft heading (deg, +ve when left wing forward)
C  RPIT   In  INS aircraft pitch (deg, +ve when nose is up)
C  RROL   In  INS aircraft roll (deg, +ve left wing up)
C  RL     In  Distance of vanes/nose from INS (m, +ve when nose ahead)
C  RM     In  Distance of the vanes/nose from the INS (m, +ve when nose to port)
C  RN     In  Distance of the vanes/nose from the INS (m, +ve when nose above)
C  RYAWR  In  Yaw rate (deg/s, +ve when left wind moving ahead)
C  RPITR  In  Pitch rate (deg/s, +ve when nose moving up)
C  RROLR  In  Roll rate (deg/s, +ve when left wing moving up)
C  RU     Out Wind component Northwards (m/s)
C  RV     Out Wind component Eastwards (m/s)
C  RW     Out Wind component Upwards (m/s)
C
C Derives winds using the following matrix formulation of the wind equations:
C
C (U)              (-U'      ) (VN) [( 0 )    (0  )         (r')]            (l)
C (V) = (A3.A2).A1.(-U'tan(b))+(VW)+[( 0 )+A3.(-t')+(A3.A2).(0 )]x(A3.A2).A1.(m)
C (W)              ( U'tan(a)) (VZ) [(-p')    (0  )         (0 )]            (n)
C
C where U' = TAS / (1+ tan(b)^2 + tan(a)^2)^(1/2), b is angle of sideslip, a is
C angle of attack, p' is heading rate, t' is pitch rate, r' is roll rate, l m 
C and n are the position of the vanes and nose with respect to the INS (l +ve 
C forwards, m +ve to port, n +ve up), U is wind component northwards, V is wind 
C component westwards, W is wind component upwards, VN is aircraft velocity 
C northwards, VW is aircraft velocity westwards, VZ is aircraft velocity 
Cupwards, and
C
C    (1  0       0    )     (cos(t) 0 -sin(t))     ( cos(p) sin(p) 0)
C A1=(0 cos(r) -sin(r))  A2=( 0     1   0    )  A3=(-sin(p) cos(p) 0)
C    (0 sin(r)  cos(r))     (sin(t) 0  cos(t))     (  0      0     1)
C
C where r is roll, t is pitch, and p is heading.
C
C This is simpler to use than explicit wind component equations when the INS
C is off the aircraft axis.  Comparisons of the wind components derived by this
C subroutine with those derived by the normal wind equations, as used in 
C C_INS_WINDS, show no differences greater than 2E-5 m/s.  However direct use
C of the wind equations is about 30% faster.
C
C Ref: MRF Internal Note No 8 - 'The measurement of flight level winds and 
C aircraft position by the MRF Hercules' by S. Nicholls, together with
C additional notes by W.D.N.Jackson, February 1993, which extend the
C analysis to cases where the INS is off axis and derives the matrix equation
C used here.
C
C V1.00  15/03/93  W.D.N.JACKSON
C V1.01  20/04/98  W.D.N.JACKSON
C        Error in formulation of Axford/Nicholls/Jackson equations when
C        computing airspeed corrected.  (See note by R Wood and G W Inverarity)
C
      SUBROUTINE C_WINDS_UVW(RTAS,RAOA,RAOS,RVN,RVE,RVZ,RHDG,RPIT,RROL,
     -    RL,RM,RN,RYAWR,RPITR,RROLR,RU,RV,RW)
CDEC$ IDENT 'V1.01'
      REAL*4 RP(3)                     !Vanes posn wrt to INS, fore, port & up
      REAL*4 RWIND(3)                  !The 3 computed wind comps Un, Vw and Wu
      REAL*4 RVG(3)                    !INS VN, VW and VZ
      REAL*4 RA1(3,3)                  !Transformation matrix about roll axis
      REAL*4 RA2(3,3)                  !Transformation matrix about pitch axis
      REAL*4 RA3(3,3)                  !Transformation matrix about heading axis
      REAL*4 RT(3,3)                   !Full transformation matrix
      REAL*4 RTMP(3,3)                 !Temporary matrix store
      REAL*4 RUA(3)                    !Airflow vector in a/c frame
      REAL*4 RYR(3)                    !Yaw rate vector
      REAL*4 RPR(3)                    !Pitch rate vector
      REAL*4 RRR(3)                    !Roll rate vector
      REAL*4 RTEMP(3)                  !Temporary vector store

      RA1(1,1)=1.                      !Define roll transformation matrix
      RA1(1,2)=0.
      RA1(1,3)=0.
      RA1(2,1)=0.
      RA1(2,2)=COSD(RROL)
      RA1(2,3)=-SIND(RROL)
      RA1(3,1)=0.
      RA1(3,2)=-RA1(2,3)
      RA1(3,3)=RA1(2,2)

      RA2(1,1)=COSD(RPIT)              !Define pitch transformation matrix
      RA2(1,2)=0.
      RA2(1,3)=-SIND(RPIT)
      RA2(2,1)=0.
      RA2(2,2)=1.
      RA2(2,3)=0.
      RA2(3,1)=-RA2(1,3)
      RA2(3,2)=0.
      RA2(3,3)=RA2(1,1)

      RA3(1,1)=COSD(RHDG)              !Define heading transformation matrix
      RA3(1,2)=SIND(RHDG)
      RA3(1,3)=0.
      RA3(2,1)=-RA3(1,2)
      RA3(2,2)=RA3(1,1)
      RA3(2,3)=0.
      RA3(3,1)=0.
      RA3(3,2)=0.
      RA3(3,3)=1.

!      PRINT *,'RTAS/AOA/AOSS =',RTAS,RAOA,RAOS

      TANAOS=TAND(RAOS)                !Define airspeed vector
      TANAOA=TAND(RAOA)
      D=SQRT(1.0+TANAOS*TANAOS+TANAOA*TANAOA)
      RUA(1)=-RTAS/D
      RUA(2)=-RTAS*TANAOS/D
      RUA(3)=RTAS*TANAOA/D

      RP(1)=RL                         !Define INS offset vector
      RP(2)=RM
      RP(3)=RN

      RVG(1)=RVN                       !Define INS velocity vector
      RVG(2)=-RVE                      !Matrix eqn requires VW
      RVG(3)=RVZ

      RYR(1)=0.                        !Define yaw rate vector
      RYR(2)=0.
      RYR(3)=-RYAWR*3.14159/180.       !Convert to rad/s

      RPR(1)=0.                        !Define pitch rate vector
      RPR(2)=-RPITR*3.14159/180.       !Convert to rad/s
      RPR(3)=0.

      RRR(1)=RROLR*3.14159/180.        !Define roll rate vector in rad/s
      RRR(2)=0.
      RRR(3)=0.

      RWIND(1)=0.                      !Clear wind vector
      RWIND(2)=0.
      RWIND(3)=0.

      CALL C_WINDS_MULM(RA3,RA2,RTMP)  !Compute full transformation vector
      CALL C_WINDS_MULM(RTMP,RA1,RT)      
      CALL C_WINDS_MATV(RT,RUA,RUA)    !Transform airspeed to ground frame
      CALL C_WINDS_VADD(RUA,RWIND,RWIND) !This is first wind component
      CALL C_WINDS_VADD(RVG,RWIND,RWIND) !Add ground speed component
      CALL C_WINDS_MATV(RT,RP,RP)      !Transform INS offset to ground frame
      CALL C_WINDS_MULM(RA3,RA2,RTMP)  !Transfm roll rate effects to ground fram
      CALL C_WINDS_MATV(RTMP,RRR,RRR)  !Compute roll rate effects
      CALL C_WINDS_MATV(RA3,RPR,RTEMP) !Transfm pitch rate effects to ground frm
      CALL C_WINDS_VADD(RRR,RTEMP,RTEMP) !Add pitch rate effects
      CALL C_WINDS_VADD(RYR,RTEMP,RTEMP) !Add yaw rate effects to get full effect
      CALL C_WINDS_VMUL(RTEMP,RP,RTEMP) !Apply rate effects to INS offset vector
      CALL C_WINDS_VADD(RTEMP,RWIND,RWIND) !This is the last wind component
 
      RU=RWIND(1)                      !Transfer result to output arguments
      RV=-RWIND(2)                     !Convert westwards to eastwards
      RW=RWIND(3)

      RETURN
      END
********************************************************************************
      SUBROUTINE C_WINDS_MULM(A,B,C)
C
C Applies the 3x3 matrix A to the 3x3 matrix B, and leaves the result in C
C which may be the same as A or B.
C
C V1.00  15/03/93  W.D.N.JACKSON
C
CDEC$ IDENT 'V1.00'
      REAL*4 A(3,3),B(3,3),C(3,3),T(3,3)
      T(1,1)=A(1,1)*B(1,1)+A(1,2)*B(2,1)+A(1,3)*B(3,1)
      T(1,2)=A(1,1)*B(1,2)+A(1,2)*B(2,2)+A(1,3)*B(3,2)
      T(1,3)=A(1,1)*B(1,3)+A(1,2)*B(2,3)+A(1,3)*B(3,3)
      T(2,1)=A(2,1)*B(1,1)+A(2,2)*B(2,1)+A(2,3)*B(3,1)
      T(2,2)=A(2,1)*B(1,2)+A(2,2)*B(2,2)+A(2,3)*B(3,2)
      T(2,3)=A(2,1)*B(1,3)+A(2,2)*B(2,3)+A(2,3)*B(3,3)
      T(3,1)=A(3,1)*B(1,1)+A(3,2)*B(2,1)+A(3,3)*B(3,1)
      T(3,2)=A(3,1)*B(1,2)+A(3,2)*B(2,2)+A(3,3)*B(3,2)
      T(3,3)=A(3,1)*B(1,3)+A(3,2)*B(2,3)+A(3,3)*B(3,3)
      DO I=1,3
        DO J=1,3
          C(I,J)=T(I,J)
        END DO
      END DO
      RETURN
      END
********************************************************************************
      SUBROUTINE C_WINDS_MATV(A,B,C)
C
C Applies the 3x3 matrix A to the column vector B, and returns with the result
C in C, which may be the same as B.
C
C V1.00  15/03/93  W.D.N.JACKSON
C
CDEC$ IDENT 'V1.00'
      REAL*4 A(3,3),B(3),C(3),T(3)
      T(1)=A(1,1)*B(1)+A(1,2)*B(2)+A(1,3)*B(3)
      T(2)=A(2,1)*B(1)+A(2,2)*B(2)+A(2,3)*B(3)
      T(3)=A(3,1)*B(1)+A(3,2)*B(2)+A(3,3)*B(3)
      C(1)=T(1)
      C(2)=T(2)
      C(3)=T(3)
      RETURN
      END
********************************************************************************
      SUBROUTINE C_WINDS_VADD(A,B,C)
C
C Adds the 3 element column vector A to column vector B and returns the result
C in C, which can be the same as A or B.
C
C V1.00  15/03/93  W.D.N.JACKSON
C
CDEC$ IDENT 'V1.00'
      REAL*4 A(3),B(3),C(3)
      C(1)=A(1)+B(1)
      C(2)=A(2)+B(2)
      C(3)=A(3)+B(3)
      RETURN
      END
********************************************************************************
      SUBROUTINE C_WINDS_VMUL(A,B,C)    
C
C Multiplies the 3 element column vector A with the column vector B and returns
C the result in C, which can be the same as A or B.
C
C V1.00  15/03/93  W.D.N.JACKSON
C
CDEC$ IDENT 'V1.00'
      REAL*4 A(3),B(3),C(3),T(3)
      T(1)=A(2)*B(3)-A(3)*B(2)
      T(2)=A(3)*B(1)-A(1)*B(3)
      T(3)=A(1)*B(2)-A(2)*B(1)
      C(1)=T(1)
      C(2)=T(2)
      C(3)=T(3)
      RETURN
      END
