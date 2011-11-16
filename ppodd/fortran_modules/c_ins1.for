C
C ROUTINE         C_INS1 SUBROUTINE FORTVAX
C
C PURPOSE         Calibrates H-423 velocities, attitudes, and attitude rates
C
C DESCRIPTION     Handles the demultiplexing, calibration, interpolation and
C                 quality control of data from the Honeywell H-423 SNU 84 
C                 Inertial Navigation Unit, to produce the three aircraft
C                 velocity components (VN, VE and VZ), the three aircraft
C                 attitudes (Roll, Pitch and True Heading), and the three
C                 aircraft attitude rates (Roll rate, Pitch rate, and Yaw
C                 rate).  All INU parameters are 32 Hz, but for ease of
C                 computation there is a 1/32 s lag in the data - ie the
C                 second sample in each second in fact describes the 
C                 aircraft state at the start of the second.
C
C                 The three aircraft accelerations, in the aircraft body frame,
C                 together with INU latitude, longitude and altitude are
C                 produced at 1 Hz.
C
C                 The INU interface sends 7 16-bit parameters at 32 Hz to the 
C                 DRS.  The INU interface unit requests the I01 message from the
C                 INU 32 times a second.  The whole of the first I01 message 
C                 received in a second is recorded in parameter 163.  The
C                 time tags from the 2nd to 32nd messages are recorded in
C                 parameter 164, samples 2 to 32.  The least 8 bits of the
C                 velocities, attitudes, and attitude rates are packed into
C                 the remaining DRS parameters.  Because the information that
C                 would go into the first sample of parameters 164 to 169 is
C                 already available in parameter 163, the first sample in the
C                 second of each of these parameters is used to record status
C                 information as follows:
C
C                 1st sample of para 164 - IIU status word (see below)
C                 1st sample of para 165 - INU message I14, word 01
C                 1st sample of para 166 - INU message I14, word 04
C                 1st sample of para 167 - All 0's
C                 1st sample of para 168 - All 1's
C                 1st sample of para 169 - Unused (all 0's)
C
C                 The information in the above 6 words is sampled at the
C                 beginning of each second; therefore if an error is indicated
C                 some of the data in the previous second may also be bad, and
C                 not necessarily all the data in the current second may be bad.
C
C                 IIU status word:
C
C                 Bit 15  1 if ASMA link broken, IIU or SIMON off, else 0
C                     14  1 if IIU can get no response from INU, else 0
C                     13  1 if IIU has no valid baro information, else 0
C                     12  1 if any bit set in the IIU 1553 chip sts word, else 0
C                     11-3 Unused, set to 0
C                     2-0 IIU software version
C
C                 Input parameters are:
C
C                 Para 163 I01   32 Hz  This contains the full 32 word I01 
C                                       message, sampled once a second
C                      164 TTAG  32 Hz  Time tags taken from I01 messages
C                      165 VXVY  32 Hz  Bits 14-21 of VX (0-7) and VY (8-15)
C                      166 VZTH  32 Hz  Bits 14-21 of VZ (0-7) and bits 0-7 of
C                                       THDG (8-15)
C                      167 RORR  32 Hz  Bits 0-7 of ROLL (0-7) and ROLR (8-15)
C                      168 PIPR  32 Hz  Bits 0-7 of PITC (0-7) and PITR (8-15)
C                      169 PAYR  32 Hz  Bits 0-7 of PAZI (0-7) and YAWR (8-15)
C
C                 The least significant bits of the information recorded by the
C                 DRS are (H-423 manual, Table 3-1A, p3-10 onwards):
C
C                   Time tags      2**6   micro-sec
C                   Altitude       2**2   foot
C                   CNEXZ          2**-30 dimensionless
C                   Longitude      2**-31 pirads (1 pirad = 180 deg)
C                   Velocities     2**-18 foot/sec
C                   Accelerations  2**-5  foot/sec/sec
C                   Attitudes      2**-15 pirads 
C                   Attitude rates 2**-13 pirads/sec
C
C                 The INU I01 message contains the following 32 16-bit words 
C                 (not all used by this module):
C
C                   01                    INU mode word
C                   02                    INU time tag
C                   03/04, 05/06, 07/08   VX, VY, VZ
C                   09, 10, 11            Platform Azimuth, Roll, Pitch
C                   12, 13                True Heading, Magnetic Heading
C                   14, 15, 16            X, Y, Z accelerations
C                   17/18, 19/20, 21/22   CNEXX, CNEXY, CNEXZ direction cosines
C                   23/24, 25             Longitude, Inertial Altitude
C                   26, 27, 28            GC steering err, X & Y residual tilts
C                   29                    INU mode word 2 - current mode
C                   30, 31, 32            Roll, Pitch and Yaw rates
C
C                 Constants:
C
C                 The following constants are used by the module to compensate
C                 for the INU not being accurately aligned with the aircraft
C                 axes, they are the values that need to be added to the INU
C                 attitudes to obtain the aircraft attitude:
C
C                   RCONST(1) Roll offset (deg)  +ve Aircraft right bank wrt INU
C                   RCONST(2) Pitch offset (deg) +ve Aircraft pitched up wrt INU
C                   RCONST(3) Yaw offset (deg)   +ve Aircraft yawed CW wrt INU
C
C                 These are defined in the flight constants file using the
C                 INSLEVL keyword.
C
C                 Output parameters are:
C
C                 Para 538 IACF   1 Hz  m/s/s +ve Forwards
C                      539 IACS   1 Hz  m/s/s +ve Starboard
C                      540 IACU   1 Hz  m/s/s +ve Upwards
C                      541 ILAT   1 Hz  deg   -90 to +90
C                      542 ILNG   1 Hz  deg   -180 to +180
C                      543 IALT   1 Hz  m     +ve Upwards
C                      558 VN    32 Hz  m/s   +ve Northwards
C                      559 VE    32 Hz  m/s   +ve Eastwards
C                      557 VZ    32 Hz  m/s   +ve Upwards
C                      560 ROLL  32 Hz  deg   +ve Right bank
C                      561 PITC  32 Hz  deg   +ve Nose up
C                      562 THDG  32 Hz  deg   +ve CW wrt True North (0-360 deg)
C                      567 ROLR  32 Hz  deg/s +ve Banking right
C                      565 PITR  32 Hz  deg/s +ve Pitching up
C                      566 YAWR  32 Hz  deg/s +ve Yawing CW wrt North
C                      563 IGS   32 Hz  m/s   +ve Always
C                      564 IDA   32 Hz  deg   +ve Track to right of heading
C
C                 Velocities are computed in the Earth-centred, Earth-fixed 
C                 frame and expressed in local geodetic coordinates.  
C                 Accelerations are computed in the aircraft frame.  Positions 
C                 are uncorrected and based on whatever initial positions were 
C                 loaded when the INU was aligned.
C
C                 The attitude angles are the Euler angles used to transform
C                 between local geodetic and aircraft body co-ordinates.
C                 The local geodetic axes are rotated in the counterclockwise
C                 direction about the downward axis by the true heading, with
C                 the yaw rate directed along this axis.  These new axes are
C                 then rotated counterclockwise about the rotated eastward 
C                 axis by the pitch angle, with the pitch rate directed along
C                 this intermediate axis.  Finally, these new axes are rotated 
C                 counterclockwise about the rotated northward axis (which 
C                 becomes the forward axis in the aircraft body frame) by the 
C                 roll angle, with the roll rate directed along this axis.
C
C                 Since only the least eight significant bits of velocity,
C                 attitude, and attitude rate are recorded at 32 Hz, the true
C                 values have to be reconstituted.  This is done by computing
C                 the expected value and assuming that the actual value will
C                 always be within +-127 bits of the expected value.  The first
C                 expected velocity in a second is computed using the 
C                 accelerations available in the I01 word.  Subsequent values
C                 are based on the changes between the previous samples.
C                 Expected attitudes are computed in the same way, using the
C                 attitude rates available in the I01 word.  Expected attitude
C                 rates are computed using the current attitude rates.
C
C                 Note that the attitude data is corrected for any INU
C                 misalignment with the aircraft, provided that the INSLEVL
C                 constants are entered in the Flight Constants file for the
C                 flight.  Because there is a variable delay between the INU
C                 sampling the aircraft attitude and velocity and having the
C                 result ready to be read by the DRS, together with a further
C                 variable delay between the data being ready and it actually
C                 being read by the DRS, this routine linearly interpolates the
C                 INU measurements, which are made available with their actual
C                 measurement times, onto the equispaced 32 Hz sampling 
C                 intervals of the DRS.
C                 
C                 Most of the computation is quite straight forward, but the
C                 VN and VE velocities have to be derived from the VX and VY 
C                 velocities since the INU does not maintain its 'platform'
C                 aligned with True North, but lets it wander at a normally 
C                 fairly slow rate.  The wander angle, a, is the difference 
C                 between the INU platform azimuth and True North.  VN and VE
C                 are then derived using:
C
C                   VN =   cos(a).VX - sin(a).VY
C                   VE = - sin(a).VX - cos(a).VY
C
C                 Accelerations are converted from platform to aircraft frame
C                 by applying a suitable transformation matrix.
C
C                 INU Groundspeed, IGS, and Drift Angle, IDA, are derived from
C                 VN, VE, and THDG using:
C
C                   IGS = sqrt(VE**2 + VN**2)
C                   IDA = artan(VE/VN) - THDG
C
C                 Flagging:
C
C                 Output data are flagged with 2 if they exceed the following
C                 max, min, rate of change limits:
C
C                   VN, VE      +-250  m/s,   +-20   m/s/s
C                   VZ          +-30.5 m/s,   +-20   m/s/s
C                   ROLL, PITC  +-60   deg,   +-20   deg/s
C                   THDG        0-360  deg,   +-15   deg/s
C                   ROLR, PITR  +-20   deg/s, +-20   deg/s/s
C                   YAWR        +-15   deg/s, +-20   deg/s/s
C                   IGS         0-250  m/s,   +-20   m/s/s
C                   IDA         +-45   deg,   +-10   deg/s
C                   IACF        +-5    m/s/s, +-4    m/s/s/s
C                   IACS        +-20   m/s/s, +-4    m/s/s/s
C                   IACU      2.5-18   m/s/s, +-7    m/s/s/s
C                   ILAT        +-90   deg,   +-.015 deg/s
C                   ILNG        +-180  deg,   +-.015 deg/s
C                   IALT    -200-12000 m,     +-30.5 m/s
C
C                 Data are also flagged under any of the following
C                 circumstances:
C
C                 IIU sts bit 15 set (no ASMA link) - All data in sec flagged 3
C                 IIU sts bit 14 set (no INU link)  - All data in sec flagged 3
C                 IIU sts bit 13 set (no baro info) - All vert in sec flagged 2
C                 IIU sts bit 12 set (1553 chip err)- All data in sec flagged 2
C                 I14/01 not zero                   - All data in sec flagged 2
C                 All zeros word not zero           - All data in sec flagged 2
C                 All ones word not FFFF            - All data in sec flagged 2
C                 I01/1 bit 1 set (Sensor fail)     - All data in sec flagged 2
C                 I01/1 bit 2 set (Nav data fail)   - All data in sec flagged 2
C                 I01/1 bit 3 set (Degraded nav)    - All data in sec flagged 2
C                 I01/1 bit 4 set (Nav data unav.)  - All data in sec flagged 2
C                 I01/1 bit 5 set (Att data fail)   - All data in sec flagged 2
C                 I01/1 bit 9 set (Baro invalid)    - All vert in sec flagged 2
C                 I01/1 bit 10 set (BIT)            - All data in sec flagged 2
C                 I01/29 any bits except 9 (NAV) set- All data in sec flagged 2
C                 I01/29 more than one bit set      - All data in sec flagged 2
C                 Time tag has a value of FFFE   - All data in sample flagged 3
C
C                 In all cases the data take the worst of all possible flags, 
C                 and if the flag is three the data are set to zero.
C 
C VERSION         1.00  10-01-94  W.D.N.JACKSON
C
C ARGUMENTS       IRAW(2,64,512) I*2  IN  Up to 64 samples for up to 512 DRS 
C                                         parameters
C                 IFRQ(512)      I*4  IN  Sample rate of each DRS par (0-64)
C                 RCONST(64)     R*4  IN  Inputs constants
C                 RDER(64,1024)  R*4  OUT Output array of up to 64 samples for
C                                         each of 1024 parameters
C
C REFERENCES      SNU 84-1 Rev D INU specification
C                 Honeywell H-423 system description
C                 MRF Technical Note 15
C
C CHANGES         V1.01  03-02-94  W.D.N.JACKSON
C                 No longer checks I14-04 when setting flags
C
C                 V1.02  11-05-94  W.D.N.JACKSON
C                 Now produces valid data, but without interpolation, if the
C                 IIU synching of the INU time tag clock has failed.
C
C                 V1.03  25-06-94  W.D.N.JACKSON
C                 Problems with retrieving platform azimuth and true heading
C                 when crossing +-180 degrees fixed.
C
C                 V1.04  24-07-95  W.D.N.JACKSON
C                 Now retrieves accelerations and positions at 1 Hz.
C
C                 V1.05  22-01-97  W.D.N.JACKSON
C                 Bug fixed which sometimes stopped interpolation.
C
C                 V1.06  09-07-98  W.D.N.JACKSON
C                 Bug fixed which caused incorrect attitude rate calculations.
C
C                 V1.07  06-08-98  G.W. Inverarity
C                 Convert feet to m assuming they're US standard feet (WGS-84).
C
C                 V1.08  13-12-02  G.W. Inverarity
C                 1. Convert feet to m as international feet (Honeywell).
C                 2. Added RPMIN array of minimum value limits and changed
C                    RPROC(15) to 0.015, consistent with the values under 
C                    "Flagging" above.
C                 3. Replaced 4. and RTTOMS*2*16 by RTMAX (= RTTOMS*2**16) 
C                    when computing time differences.
C                 4. Added extra time tag checks when deciding whether
C                    or not to interpolate 32 Hz data.
C                 5. Extrapolate 1 Hz positions by integrating the equations
C                    for the rates of change of latitude, longitude and 
C                    altitude using Euler's method, working in double 
C                    precision to minimise rounding error, which can be of
C                    the order of 1 metre.
C                 6. Drift angle error when northward velocity zero corrected.
C                 7. Simplified the true heading and drift angle calculations
C                    using the MOD function.
C                 8. C_INS1_TRANS_BRATE rewritten.
C*******************************************************************************
      SUBROUTINE C_INS1(IRAW,IFRQ,RCONST,RDER)

CDEC$ IDENT     'V1.08'

      INTEGER*2 IRAW(2,64,512)         !Raw data array
      INTEGER*4 IFRQ(512)              !Raw data frequency (not used)
      REAL*4    RCONST(64)             !Constants array
      REAL*4    RDER(64,1024)          !Derived data array

      INTEGER*2 IVX,IVY,IVZ,IPA,IRO,IPI,ITH,IRR,IPR,IYR
      INTEGER*2 IXVX,IXVY,IXVZ,IXPA,IXRO,IXPI,IXTH,IXRR,IXPR,IXYR
      INTEGER*2 ITEMP
      INTEGER*4 ITEMP4,ITEMP4B
      REAL*4    RVX(32),RVY(32),RPA(32),RTDIF(32)
      REAL*4    RTT(0:32),RVN(0:32),RVE(0:32),RVZ(0:32),RROLL(0:32),
     -          RPITC(0:32),RTHDG(0:32),RROLR(0:32),RPITR(0:32),
     -          RYAWR(0:32)
      INTEGER*4 IPARA(17),IFLG(17)
      REAL*4    RPMIN(17),RPMAX(17),RPROC(17),RLSTVAL(17)

      REAL*8    DT        ! Time interval by which to extrapolate 1 Hz 
                          ! positions. 
      REAL*8    DL        ! Rate of change of latitude with time (rad/s)
      REAL*8    DLAMBDA   ! Rate of change of longitude with time (rad/s)
      REAL*8    RL        ! Meridional radius of curvature (m)
      REAL*8    RLAMBDA   ! Azimuthal radius of curvature (m) / COS(LAT)

      PARAMETER I01=163,TTAG=164,VXVY=165,VZTH=166 !Raw parameters
      PARAMETER RORR=167,PIPR=168,PAYR=169
      PARAMETER GMT=515,VN=558,VE=559,VZ=557       !Derived parameters
      PARAMETER ROLL=560,PITC=561,THDG=562
      PARAMETER ROLR=567,PITR=565,YAWR=566
      PARAMETER IGS=563,IDA=564
      PARAMETER IACF=538,IACS=539,IACU=540,ILAT=541,ILNG=542,IALT=543

      REAL*4    RFT2MTR ! International foot to metre conversion factor
      PARAMETER(RFT2MTR=0.3048)
      REAL*8    PI      ! Pi
      PARAMETER(PI=3.1415926535897932D0)
      REAL*8    RAD2DEG ! Radians to degrees
      PARAMETER(RAD2DEG=180.0D0/PI)
      
      DATA LFLAG /.TRUE./              !Set to false if don't want data flagging
      DATA LINTER /.TRUE./             !Set to false if don't want interpolation
      DATA RLSTSEC /-2.0/              !Initial value for last sec processed
      DATA IPARA /VN,VE,VZ,ROLL,PITC,THDG,ROLR,PITR,YAWR,IGS,IDA,
     -    IACF,IACS,IACU,ILAT,ILNG,IALT/ !Derived paras
      DATA RPMIN /2*-250.,-30.5,2*-60.,0.,2*-20.,-15.,0.,-45.,
     -    -5.,-20.,2.5,-90.,-180.,-200./ ! Min values
      DATA RPMAX /2*250.,30.5,2*60.,360.,2*20.,15.,250.,45.,
     -    5.,20.,18.,90.,180.,12000./ !Max values
      DATA RPROC /3*20.,2*20.,15.,3*20.,20.,10.,
     -    4.,4.,7.,0.015,0.015,30.5/ !Max rates of change/s

      SAVE
C
      RTTOMS=2.**6/1.0E6               !Converts time tags to seconds
      RTMAX=RTTOMS*2.**16              !Maximum time tag
      RVTMPS=RFT2MTR/2.**4             !Converts velocities to m/s
      RRATOD=180./2.**13               !Converts attitude rates to deg/s
      RATTOD=RRATOD/4.0                !Converts attitudes to degrees
      RATMSS=RVTMPS/2.0                !Converts accelerations to m/s/s
      RTSHFT=1.0/32.0                  !Data time shift in secs  
      RSINT=RTSHFT                     !DRS data sample interval
C
C Set flag false if this second is not immediately after previous one.
C
      LNXTSEC=.TRUE.
      IF(RDER(1,GMT).NE.RLSTSEC+1.0) LNXTSEC=.FALSE.
C
C Retrieve time tags - if INU is getting synched by IIU then time tags will
C always be in range 0 to 1s.  If INU 1553 clock not being reset by IIU then
C time tags will be in range 0 to RTMAX = 4.194304 s (2**22 microsec).  Set 
C interpolation flag to false if the first tag of the second is not in the 
C known range, thus indicating that the IIU is not synching the INU.
C
      ITT=JZEXT(IRAW(1,2,I01))
      RTT(1)=FLOAT(ITT)*RTTOMS
      DO I=2,32
        ITT=JZEXT(IRAW(1,I,TTAG))
        RTT(I)=FLOAT(ITT)*RTTOMS
      END DO
      DO I=2,32
        RTDIF(I)=RTT(I)-RTT(I-1)
        IF(RTDIF(I).LE.-RTMAX) RTDIF(I)=RTDIF(I)+RTMAX
        IF(RTDIF(I).LE.0.) RTDIF(I)=RTDIF(I)+1.0
      END DO
      LINTERP=LINTER
      IF(RTT(1).LT.0.970.OR.RTT(1).GT.0.991) LINTERP=.FALSE.
C
C Compute platform accelerations for use in retrieving VX, VY and VZ.
C
      RAX=FLOAT(IRAW(1,14,I01))*RATMSS
      RAY=FLOAT(IRAW(1,15,I01))*RATMSS
      RAZ=FLOAT(IRAW(1,16,I01))*RATMSS-9.75 !Remove gravitational acceleration
C
C Retrieve VX
C
      CALL MVBITS(IRAW(1,3,I01),0,14,IVX,2)
      CALL MVBITS(IRAW(1,4,I01),14,2,IVX,0)
      RVX(1)=FLOAT(IVX)*RVTMPS
      DO I=2,32
        IF(I.EQ.2) RXVX=RVX(1)+RAX*RTDIF(2)
        IF(I.GT.2) RXVX=RVX(I-1)+(RVX(I-1)-RVX(I-2))/RTDIF(I-1)*RTDIF(I)
        RXVX=RXVX/RVTMPS
        RXVX=MIN(RXVX,32767.)
        RXVX=MAX(RXVX,-32768.)
        IXVX=NINT(RXVX)
        CALL C_INS1_MERGE(IXVX,IRAW(1,I,VXVY),0,IVX)
        RVX(I)=FLOAT(IVX)*RVTMPS
      END DO
C
C Retrieve VY
C
      CALL MVBITS(IRAW(1,5,I01),0,14,IVY,2)
      CALL MVBITS(IRAW(1,6,I01),14,2,IVY,0)
      RVY(1)=FLOAT(IVY)*RVTMPS
      DO I=2,32
        IF(I.EQ.2) RXVY=RVY(1)+RAY*RTDIF(2)
        IF(I.GT.2) RXVY=RVY(I-1)+(RVY(I-1)-RVY(I-2))/RTDIF(I-1)*RTDIF(I)
        RXVY=RXVY/RVTMPS
        RXVY=MIN(RXVY,32767.)
        RXVY=MAX(RXVY,-32768.)
        IXVY=NINT(RXVY)
        CALL C_INS1_MERGE(IXVY,IRAW(1,I,VXVY),8,IVY)
        RVY(I)=FLOAT(IVY)*RVTMPS
      END DO
C
C Retrieve VZ
C
      CALL MVBITS(IRAW(1,7,I01),0,14,IVZ,2)
      CALL MVBITS(IRAW(1,8,I01),14,2,IVZ,0)
      RVZ(1)=FLOAT(IVZ)*RVTMPS
      DO I=2,32
        IF(I.EQ.2) RXVZ=RVZ(1)+RAZ*RTDIF(2)
        IF(I.GT.2) RXVZ=RVZ(I-1)+(RVZ(I-1)-RVZ(I-2))/RTDIF(I-1)*RTDIF(I)
        RXVZ=RXVZ/RVTMPS
        RXVZ=MIN(RXVZ,32767.)
        RXVZ=MAX(RXVZ,-32768.)
        IXVZ=NINT(RXVZ)
        CALL C_INS1_MERGE(IXVZ,IRAW(1,I,VZTH),0,IVZ)
        RVZ(I)=FLOAT(IVZ)*RVTMPS
      END DO
C
C Retrieve lat, long and altitude
C
      ITEMP4=IRAW(1,21,i01)
      CALL MVBITS(ITEMP4,0,16,ITEMP4B,16) !Latitude
      ITEMP4=IRAW(1,22,I01)
      CALL MVBITS(ITEMP4,0,16,ITEMP4B,0)
      RCNEXZ=FLOAT(ITEMP4B)/2.**30
      IF(RCNEXZ.GE.-1.AND.RCNEXZ.LE.1) RLAT=ASIND(RCNEXZ)
      IF (RLAT.GT.89.9) 
     &  print *,'Latitude close to 90 could cause problems'
      IF (RLAT.LT.-89.9) 
     &  print *,'Latitude close to -90 could cause problems'
      ITEMP4=IRAW(1,23,I01)
      CALL MVBITS(ITEMP4,0,16,ITEMP4B,16) !Longitude
      ITEMP4=IRAW(1,24,I01)
      CALL MVBITS(ITEMP4,0,16,ITEMP4B,0)
      RLNG=FLOAT(ITEMP4B)*180./2.**31
      RALT=FLOAT(IRAW(1,25,I01))*4.*RFT2MTR !Height in metres
C
C Compute basic attitude and attitude rates at start of second, and transform
C attitude rates from aircraft body co-ordinates to rates of change of
C Euler angles.
C
      RPA(1)=FLOAT(IRAW(1,9,I01))*RATTOD
      RROLL(1)=FLOAT(IRAW(1,10,I01))*RATTOD
      RPITC(1)=FLOAT(IRAW(1,11,I01))*RATTOD
      RTHDG(1)=FLOAT(IRAW(1,12,I01))*RATTOD
      RROLR(1)=FLOAT(IRAW(1,30,I01))*RRATOD
      RPITR(1)=FLOAT(IRAW(1,31,I01))*RRATOD
      RYAWR(1)=FLOAT(IRAW(1,32,I01))*RRATOD
C Compute wander angle and transform VX and VY to VN and VE
      RWA=RPA(1)-RTHDG(1)
      RVN(1)=COSD(RWA)*RVX(1)-SIND(RWA)*RVY(1)
      RVE(1)=-SIND(RWA)*RVX(1)-COSD(RWA)*RVY(1)
      CALL C_INS1_TRANS_BRATE(RLAT,RALT,RVN(1),RVE(1),RROLR(1),RPITR(1),
     &     RYAWR(1),RROLL(1),RPITC(1),RTHDG(1),RRR1,RPR1,RYR1)
      RROLR(1)=RRR1
      RPITR(1)=RPR1
      RYAWR(1)=RYR1
C
C Retrieve Roll
C
      DO I=2,32
        IF(I.EQ.2) RXRO=RROLL(1)+RRR1*RTDIF(2)
        IF(I.GT.2) RXRO=RROLL(I-1)+(RROLL(I-1)-RROLL(I-2))/RTDIF(I-1)
     -      *RTDIF(I)
        RXRO=RXRO/RATTOD
        RXRO=MIN(RXRO,32767.)
        RXRO=MAX(RXRO,-32768.)
        IXRO=NINT(RXRO)
        CALL C_INS1_MERGE(IXRO,IRAW(1,I,RORR),0,IRO)
        RROLL(I)=FLOAT(IRO)*RATTOD
      END DO
C
C Retrieve Pitch
C
      DO I=2,32
        IF(I.EQ.2) RXPI=RPITC(1)+RPR1*RTDIF(2)
        IF(I.GT.2) RXPI=RPITC(I-1)+(RPITC(I-1)-RPITC(I-2))/RTDIF(I-1)
     -      *RTDIF(I)
        RXPI=RXPI/RATTOD
        RXPI=MIN(RXPI,32767.)
        RXPI=MAX(RXPI,-32768.)
        IXPI=NINT(RXPI)
        CALL C_INS1_MERGE(IXPI,IRAW(1,I,PIPR),0,IPI)
        RPITC(I)=FLOAT(IPI)*RATTOD
      END DO
C
C Retrieve Platform azimuth
C
      DO I=2,32
        IF(I.EQ.2) RXPA=RPA(1)+RYR1*RTDIF(2)
        IF(I.GT.2) THEN
          RDIFF=MOD(MOD(RPA(I-1)-RPA(I-2),360.)+360.,360.)
          IF(RDIFF.GT.180.) RDIFF=RDIFF-360.
          RXPA=RPA(I-1)+RDIFF/RTDIF(I-1)*RTDIF(I)
        END IF
        RXPA=RXPA/RATTOD
        IF(RXPA.GT.32767.) RXPA=RXPA-65536.
        IF(RXPA.LT.-32768.) RXPA=RXPA+65536.
        RXPA=MIN(RXPA,32767.)
        RXPA=MAX(RXPA,-32768.)
        IXPA=NINT(RXPA)
        CALL C_INS1_MERGE(IXPA,IRAW(1,I,PAYR),0,IPA)
        RPA(I)=FLOAT(IPA)*RATTOD
      END DO
C
C Retrieve Heading
C
      DO I=2,32
        IF(I.EQ.2) RXTH=RTHDG(1)+RYR1*RTDIF(2)
        IF(I.GT.2) THEN
          RDIFF=MOD(MOD(RTHDG(I-1)-RTHDG(I-2),360.)+360.,360.)
          IF(RDIFF.GT.180.) RDIFF=RDIFF-360.
          RXTH=RTHDG(I-1)+RDIFF/RTDIF(I-1)*RTDIF(I)
        END IF
        RXTH=RXTH/RATTOD
        IF(RXTH.GT.32767.) RXTH=RXTH-65536.
        IF(RXTH.LT.-32768.) RXTH=RXTH+65536.
        RXTH=MIN(RXTH,32767.)
        RXTH=MAX(RXTH,-32768.)
        IXTH=NINT(RXTH)
        CALL C_INS1_MERGE(IXTH,IRAW(1,I,VZTH),8,ITH)
        RTHDG(I)=FLOAT(ITH)*RATTOD
      END DO
C 
C Retrieve Roll rate
C
      IRR=IRAW(1,30,I01)
      DO I=2,32
        IXRR=IRR
        CALL C_INS1_MERGE(IXRR,IRAW(1,I,RORR),8,IRR)
        RROLR(I)=FLOAT(IRR)*RRATOD
      END DO
C
C Retrieve Pitch rate
C
      IPR=IRAW(1,31,I01)
      DO I=2,32
        IXPR=IPR
        CALL C_INS1_MERGE(IXPR,IRAW(1,I,PIPR),8,IPR)
        RPITR(I)=FLOAT(IPR)*RRATOD
      END DO
C
C Retrieve Yaw rate
C
      IYR=IRAW(1,32,I01)
      DO I=2,32
        IXYR=IYR
        CALL C_INS1_MERGE(IXYR,IRAW(1,I,PAYR),8,IYR)
        RYAWR(I)=FLOAT(IYR)*RRATOD
      END DO
C
C Compute wander angle and transform VX and VY to VN and VE
C
      DO I=2,32
        RWA=RPA(I)-RTHDG(I)
        RVN(I)=COSD(RWA)*RVX(I)-SIND(RWA)*RVY(I)
        RVE(I)=-SIND(RWA)*RVX(I)-COSD(RWA)*RVY(I)
      END DO
C
C Transform the roll, pitch and yaw rates just recovered from aircraft body
C co-ordinates to rates of change of Euler angles.
C
      DO I=2,32
        CALL C_INS1_TRANS_BRATE(RLAT,RALT,RVN(I),RVE(I),RROLR(I),
     &        RPITR(I),RYAWR(I),RROLL(I),RPITC(I),RTHDG(I),RRR1,
     &        RPR1,RYR1)
        RROLR(I)=RRR1
        RPITR(I)=RPR1
        RYAWR(I)=RYR1
      END DO
C
C Retrieve accelerations and transform from platform to aircraft co-ordinates.
C Units are m/s/s and the normal up value is about 9.85.
C
      RAX=FLOAT(IRAW(1,14,I01))*RATMSS
      RAY=FLOAT(IRAW(1,15,I01))*RATMSS
      RAZ=FLOAT(IRAW(1,16,I01))*RATMSS
      CALL C_INS1_TRANS_ACCL(RAX,RAY,RAZ,RROLL(1),RPITC(1),RPA(1),
     -    RACF,RACS,RACU)
C
C Interpolate data onto equispaced 32 Hz intervals, using data time tags
C Because (assuming) the first data sample of each DRS second contains data
C with a time tag that puts the data validity point near the end of the 
C previous second, the data is shifted by one sample interval so that it can
C be accurately interpolated.  As a result there is a 1/32 s shift on all INU
C parameters output by this subroutine and, for example, the 2nd sample in
C each output second has a validity time which is exactly the start of the 
C second, whereas the 1st sample should really be the 32 sample of the previous
C second's data.
C
C It is necessary to interpolate the data because of computation delays and
C differences between the DRS and the INU sampling frequencies.  However since
C all data have an accurate (to 64 us) validity time attached this (linear)
C interpolation is relatively easy to do.
C
      DO I=0,32                        !Time shift the time tags
        RTT(I)=RTT(I)+RTSHFT
      END DO
      DO I=31,0,-1                     !Make sure times increase
        IF(RTT(I).GT.RTT(I+1)) RTT(I)=RTT(I)-1.0
      END DO
C
C If haven't got the last sample from the previous second then extrapolate
C back from the first and second samples of the current second.
C
      IF(.NOT.LNXTSEC) THEN
        RTT(0)=2*RTT(1)-RTT(2)
        RVN(0)=2*RVN(1)-RVN(2)
        RVE(0)=2*RVE(1)-RVE(2)
        RVZ(0)=2*RVZ(1)-RVZ(2)
        RROLL(0)=2*RROLL(1)-RROLL(2)
        RPITC(0)=2*RPITC(1)-RPITC(2)
        RTHDG(0)=2*RTHDG(1)-RTHDG(2)
        RROLR(0)=2*RROLR(1)-RROLR(2)
        RPITR(0)=2*RPITR(1)-RPITR(2)
        RYAWR(0)=2*RYAWR(1)-RYAWR(2)
      END IF
C
C Interpolate the data
C
      DO I=1,32
        IF(.NOT.LINTERP.OR.RTT(I-1).GE.RTT(I).OR.RTT(I-1).LT.-RTSHFT
     &    .OR.RTT(I-1).GT.1.0.OR.RTT(I).LT.-RTSHFT.OR.RTT(I).GT.1.0) 
     &    THEN            !Can't interpolate
          RPROP=1.
        ELSE
          RPROP=(RSINT*(I-1)-RTT(I-1))/(RTT(I)-RTT(I-1)) !Interpolation proporti
        END IF
        RDER(I,VN)=RVN(I-1)+(RVN(I)-RVN(I-1))*RPROP
        RDER(I,VE)=RVE(I-1)+(RVE(I)-RVE(I-1))*RPROP
        RDER(I,VZ)=RVZ(I-1)+(RVZ(I)-RVZ(I-1))*RPROP
        RDER(I,ROLL)=RROLL(I-1)+(RROLL(I)-RROLL(I-1))*RPROP
        RDER(I,PITC)=RPITC(I-1)+(RPITC(I)-RPITC(I-1))*RPROP
        RDER(I,ROLR)=RROLR(I-1)+(RROLR(I)-RROLR(I-1))*RPROP
        RDER(I,PITR)=RPITR(I-1)+(RPITR(I)-RPITR(I-1))*RPROP
        RDER(I,YAWR)=RYAWR(I-1)+(RYAWR(I)-RYAWR(I-1))*RPROP
        RDIFF=MOD(MOD(RTHDG(I)-RTHDG(I-1),360.)+360.,360.) !Heading needs special treatment
        IF(RDIFF.GT.180.) RDIFF=RDIFF-360.
        RDER(I,THDG)=MOD(MOD(RTHDG(I-1)+RDIFF*RPROP,360.)+360.,360.)
      END DO
C
C Save last samples of second for use in next second.
C
      RTT(0)=RTT(32)-RTSHFT            !Restore original time tag
      RVN(0)=RVN(32)
      RVE(0)=RVE(32)
      RVZ(0)=RVZ(32)
      RROLL(0)=RROLL(32)
      RPITC(0)=RPITC(32)
      RTHDG(0)=RTHDG(32)
      RROLR(0)=RROLR(32)
      RPITR(0)=RPITR(32)
      RYAWR(0)=RYAWR(32)
      RLSTSEC=RDER(1,GMT)
C
C Correct attitudes for INU levelling errors.
C
      DO I=1,32
        RDER(I,ROLL)=RDER(I,ROLL)+RCONST(1)
        RDER(I,PITC)=RDER(I,PITC)+RCONST(2)
        RDER(I,THDG)=MOD(MOD(RDER(I,THDG)+RCONST(3),360.)+360.,360.)
      END DO
C
C Compute ground speed and drift angle
C
      DO I=1,32
        RDER(I,IGS)=SQRT(RVN(I)**2+RVE(I)**2)
        IF(RDER(I,VE).EQ.0..AND.RDER(I,VN).EQ.0.) THEN
          RTRK=0.
        ELSE
          RTRK=ATAN2D(RDER(I,VE),RDER(I,VN))
        END IF
        RIDA=MOD(MOD(RTRK-RDER(I,THDG),360.)+360.,360.)
        IF(RIDA.GT.180.) RIDA=RIDA-360.
        RDER(I,IDA)=RIDA
      END DO
C
C Transfer 1Hz data to output buffer.
C
      RDER(1,IACF)=RACF
      RDER(1,IACS)=RACS
      RDER(1,IACU)=RACU
C
C Extrapolate the positions forward to the true start of the second by
C using Euler's method to integrate the equations for the rates of
C change of latitude, longitude and altitude.  Use double precision to
C inhibit further amplification of rounding error, which corresponds to
C position errors on the order of a metre.
C
      IF(LINTERP) THEN
         CALL RLATLONG(DBLE(RLAT),RALT,RVN(1),RVE(1),RL,RLAMBDA,DL,
     &        DLAMBDA)
         DT=DBLE(RTSHFT)-DBLE(RTT(1))
         IF(DT.LT.0.0D0) DT=DT+1.0D0
         RDER(1,ILAT)=REAL(DBLE(RLAT)+DT*DL*RAD2DEG)
         RDER(1,ILNG)=REAL(DBLE(RLNG)+DT*DLAMBDA*RAD2DEG)
         RDER(1,IALT)=REAL(DBLE(RALT)+DT*DBLE(RVZ(1)))
      ELSE !Can't interpolate
         RDER(1,ILAT)=RLAT
         RDER(1,ILNG)=RLNG
         RDER(1,IALT)=RALT
      END IF
C
C Flag the data if required:
C
C   IIU sts bit 15 set (no ASMA link) - All data in sec flagged 3
C   IIU sts bit 14 set (no INU link)  - All data in sec flagged 3
C   IIU sts bit 13 set (no baro info) - All vert in sec flagged 2
C   IIU sts bit 12 set (1553 chip err)- All data in sec flagged 2
C   I14/01 not zero                   - All data in sec flagged 2
C   All zeros word not zero           - All data in sec flagged 2
C   All ones word not FFFF            - All data in sec flagged 2
C   I01/1 bit 1 set (Sensor fail)     - All data in sec flagged 2
C   I01/1 bit 2 set (Nav data fail)   - All data in sec flagged 2
C   I01/1 bit 3 set (Degraded nav)    - All data in sec flagged 2
C   I01/1 bit 4 set (Nav data unav.)  - All data in sec flagged 2
C   I01/1 bit 5 set (Att data fail)   - All data in sec flagged 2
C   I01/1 bit 9 set (Baro invalid)    - All vert in sec flagged 2
C   I01/1 bit 10 set (BIT)            - All data in sec flagged 2
C   I01/29 any bits except 9 (NAV) set- All data in sec flagged 2
C   I01/29 more than one bit set      - All data in sec flagged 2
C   Time tag has a value of FFFE      - All data in sample flagged 3
C
C   Apply max/min/rate of change checks to output parameters.
C
      IF(LFLAG) THEN
        IALLFLG=0
        IVFLG=0
        IF(BTEST(IRAW(1,1,TTAG),15)) IALLFLG=3         !No link to IIU
        IF(BTEST(IRAW(1,1,TTAG),14)) IALLFLG=3         !No link to INU
        IF(BTEST(IRAW(1,1,TTAG),13)) IVFLG=2           !IIU has no baro data
        IF(BTEST(IRAW(1,1,TTAG),12)) IALLFLG=MAX(IALLFLG,2) !IIU 1553 chip error
        IF(IRAW(1,1,VXVY).NE.0) IALLFLG=MAX(IALLFLG,2) !I14/01 has a bit set
        IF(IRAW(1,1,RORR).NE.0) IALLFLG=MAX(IALLFLG,2) !Some 0s missing
        IF(IRAW(1,1,PIPR).NE.'FFFF'X) IALLFLG=MAX(IALLFLG,2) !Some 1s missing
        IF(BTEST(IRAW(1,1,I01),16-1)) IALLFLG=MAX(IALLFLG,2) !Sensor fail
        IF(BTEST(IRAW(1,1,I01),16-2)) IALLFLG=MAX(IALLFLG,2) !Nav data fail
        IF(BTEST(IRAW(1,1,I01),16-3)) IALLFLG=MAX(IALLFLG,2) !Degraded nav
        IF(BTEST(IRAW(1,1,I01),16-4)) IALLFLG=MAX(IALLFLG,2) !Nav data unavail
        IF(BTEST(IRAW(1,1,I01),16-5)) IALLFLG=MAX(IALLFLG,2) !Attitude data fail
        IF(BTEST(IRAW(1,1,I01),16-9)) IVFLG=2                !Baro invalid
        IF(BTEST(IRAW(1,1,I01),16-10)) IALLFLG=MAX(IALLFLG,2) !Doing BIT
        IF(.NOT.BTEST(IRAW(1,29,I01),16-9)) IALLFLG=MAX(IALLFLG,2) !Not NAVIGATE
        ISET=0
        DO I=0,15
          IF(BTEST(IRAW(1,29,I01),I)) ISET=ISET+1
        END DO
        IF(ISET.GT.1) IALLFLG=MAX(IALLFLG,2)
        DO J=1,17
          IFLG(J)=IALLFLG
          IF(J.EQ.3.OR.J.EQ.14.OR.J.EQ.17)
     -        IFLG(J)=MAX(IALLFLG,IVFLG) !Flag vertical measurements separately
          IF(J.EQ.10) IFLG(J)=MAX(IALLFLG,IFLG(1),IFLG(2)) !G/S
          IF(J.EQ.11) IFLG(J)=MAX(IALLFLG,IFLG(1),IFLG(2)) !D/A
        END DO
        DO I=1,32                      !Flag 32Hz parameters
          IFL=0
          IF(IRAW(1,I,TTAG).EQ.'FFFE'X) IFL=3
          DO J=1,11
            IFLAG=MAX(IFL,IFLG(J))
            IP=IPARA(J)
            IF(RDER(I,IP).LT.RPMIN(J).OR.RDER(I,IP).GT.RPMAX(J))
     &           IFLAG=MAX(2,IFLAG) ! Max/min checks
            IF((LNXTSEC.OR.I.GT.1).AND.ITSTFLG(RLSTVAL(J)).LT.2) THEN !ROC chks
              RDIFF=RDER(I,IP)-RLSTVAL(J)
              IF (J.EQ.6) THEN
                 RDIFF=MOD(MOD(RDIFF,360.)+360.,360.)
                 IF(RDIFF.GT.180.) RDIFF=RDIFF-360.
              END IF
              IF(ABS(RDIFF)*32.GT.RPROC(J)) IFLAG=MAX(2,IFLAG)
            END IF
            IF(IFLAG.EQ.3) RDER(I,IP)=0.
            CALL ISETFLG(RDER(I,IP),IFLAG)
            RLSTVAL(J)=RDER(I,IP)      !Save last value
          END DO
        END DO
        DO J=12,17                     !Flag 1Hz parameters
          IFLAG=MAX(IFL,IFLG(J))
          IP=IPARA(J)
          IF(RDER(1,IP).LT.RPMIN(J).OR.RDER(1,IP).GT.RPMAX(J))
     &         IFLAG=MAX(2,IFLAG) ! Max/min checks
          IF(LNXTSEC.AND.ITSTFLG(RLSTVAL(J)).LT.2) THEN !ROC chks
            RDIFF=RDER(1,IP)-RLSTVAL(J)
            IF(ABS(RDIFF).GT.RPROC(J)) IFLAG=MAX(2,IFLAG)
          END IF
          IF(IFLAG.EQ.3) RDER(1,IP)=0.
          CALL ISETFLG(RDER(1,IP),IFLAG)
          RLSTVAL(J)=RDER(1,IP)        !Save last value of second
        END DO
      END IF
C
      RETURN
      END
C*******************************************************************************
      SUBROUTINE C_INS1_MERGE(IEXPVAL,IPART,ISTART,INEWVAL)
CDEC$ IDENT 'V1.00'
C
C Reconstitutes a full sixteen bit word, using the expected 16 bit value, and
C the lowest 8 bits of the new 16 bit value.  Can only work when the expected
C value is correct to within +- 127 bits.
C
C Arguments:  IEXPVAL I*2 In  Expected new value (-32768 to 32767)
C             IPART   I*2 In  Contains new lowest 8 bits, in top or bottom byte
C             ISTART  I*4 In  0 if 8 bits in low bytes, else 8 bits in top byte
C             INEWVAL I*2 Out New value (-32768 to 32767)
C
      INTEGER*2 IEXPVAL,IPART,INEWVAL,ITEMP1,ITEMP2
      INTEGER*4 ISTART
      BYTE      BTEMP1(2),BTEMP2(2)
      EQUIVALENCE (ITEMP1,BTEMP1),(ITEMP2,BTEMP2)

      ITEMP1=IEXPVAL
      ITEMP2=IPART
      IF(ISTART.EQ.0) BTEMP1(1)=BTEMP2(1)
      IF(ISTART.NE.0) BTEMP1(1)=BTEMP2(2)
      IF(ITEMP1-IEXPVAL.GE.128) THEN
        IF(ITEMP1.GE.-32512) THEN
          ITEMP1=ITEMP1-256
        ELSE
          BTEMP1(2)='7F'X              !Heading/azimuth -180 to +180 change
        END IF
      ELSE IF(ITEMP1-IEXPVAL.LE.-128) THEN
        IF(ITEMP1.LE.32511) THEN
          ITEMP1=ITEMP1+256
        ELSE
          BTEMP1(2)='80'X              !Heading/azimuth +180 to -180 change
        END IF
      END IF
      INEWVAL=ITEMP1
      RETURN
      END
C*******************************************************************************
!+    Convert the attitude rate vector into Euler angle rates of change.
!
!     *********************** COPYRIGHT ********************
!     Crown Copyright 2002, Met Office. All rights reserved.
!     *********************** COPYRIGHT ********************
!
!     Subroutine Interface:

      SUBROUTINE C_INS1_TRANS_BRATE(LAT,ALT,VN,VE,RP,RQ,RR,RROL,RPIT,
     &     RHDG,RRR,RPR,RYR)

CDEC$ IDENT 'V2.00'

      IMPLICIT NONE

!     Description: 
!     Transforms attitude rate vector (p,q,r) of the aircraft with respect
!     to the inertial frame expressed in aircraft body co-ordinates to
!     the true yaw, pitch and roll rates (psi', theta' and phi',
!     respectively) about the local geodetic frame's downward axis, an
!     intermediate rotated horizontal axis and the aircraft body frame's
!     forward axis, respectively.
!
!     Method: 
!     The angular frequency vector of the aircraft with respect to the
!     inertial frame has components p, q and r with respect to the
!     aircraft's forward, starboard and downward axes, respectively.  To
!     obtain the angular frequency of the aircraft with respect to the
!     local geodetic frame it is necessary to subtract the sum of the
!     angular frequency of the local geodetic frame with respect to the
!     Earth frame and of the Earth frame with respect to the inertial
!     frame: Omega + rho = (Omega + d lambda / dt) Xhat + d L / dt Yhat,
!     where Xhat and Yhat are, respectively, the unit vectors directed
!     from the Earth's centre towards the north pole and the point in
!     the equatorial plane at 90 degrees west.  The rates of change of
!     latitude L and longitude lambda with respect to time are given by
!     routine RLATLONG in this file.  This vector is now transformed
!     into aircraft body co-ordinates by first transforming it into
!     local geodetic co-ordinates by pre-multiplying by the
!     transformation matrix
!
!     ( cos(L)  0  -sin(L) )
!     (    0    1    0     )
!     ( sin(L)  0   cos(L) )
!
!     to obtain Omega + rho = (Omega + d lambda / d t) cos(L) nhat + d L
!     / d t what + (Omega + d lambda / d t) sin(L) zhat, where nhat,
!     what and zhat are the unit vectors in the northward, westward and
!     upward directions.  Next, this is transformed into aircraft body
!     co-ordinates by pre-multiplying by the direction cosine matrix
!     (DCM) of C_INS1_TRANS_ACCL using the true heading rather than the
!     platform azimuth.  This is itself obtained from a sequence of
!     three Euler angle rotations.  The DCM for a counterclockwise
!     rotation of the true heading psi about the downward geodetic axis
!     is
!
!                      ( cos(psi)  sin(psi)  0 )
!      A3(psi) =       (-sin(psi)  cos(psi)  0 ).
!                      (   0          0      1 )
!
!     Next, the angle about which the pitch angle is defined is obtained
!     by rotating the east axis in the local geodetic frame
!     counterclockwise about the downward geodetic axis by the heading
!     angle psi.  The DCM for a counterclockwise rotation of the pitch
!     angle theta about this intermediate horizontal axis is
!
!                      ( cos(theta)  0  -sin(theta) )
!      A2(theta) =     (    0        1      0       ).
!                      ( sin(theta)  0   cos(theta) )
!
!     The DCM for a counterclockwise rotation of the roll angle phi
!     about the aircraft forward axis is
!
!                      ( 1      0          0    )
!      A1(phi) =       ( 0   cos(phi)  sin(phi) ).
!                      ( 0  -sin(phi)  cos(phi) )
!
!     Finally, the transformation matrix required to flip a
!     north/west/up co-ordinate system into a north/east/down system is
!
!                      ( 1  0  0 )
!     Oflip =          ( 0 -1  0 ).
!                      ( 0  0 -1 )
!
!     Putting these together, the DCM for transforming from the 
!     local geodetic frame to the aircraft body frame is
!     CGB =  A1(phi) A2(theta) A3(psi) Oflip.
!
!     Having subtracted CGB (Omega + rho) from (p, q, r) to obtain the
!     attitude rate vector of the aircraft with respect to the local
!     geodetic frame in aircraft body co-ordinates (p', q', r'), this
!     can be written in terms of the roll rate phi', pitch rate theta'
!     and yaw rate psi' as
!
!     ( p' )   ( phi')           (   0   )                    (  0  )
!     ( q' ) = (  0  ) + A1(phi) ( theta') + A1(phi) A2(theta)(  0  ),
!     ( r' )   (  0  )           (   0   )                    ( psi')
!
!     giving
!
!     p' =  phi' - psi' sin(theta)
!     q' =  theta' cos(phi) + psi' cos(theta) sin(phi)
!     r' = -theta' sin(phi) + psi' cos(theta) cos(phi)
!
!     which inverts to give (when theta != +/- 90 degrees)
!
!     phi' = p' + (q' sin(phi) + r' cos(phi)) tan(theta)
!     theta' = q' cos(phi) - r' sin(phi)
!     psi' = (q' sin(phi) + r' cos(phi)) / cos(theta).
!
!     When theta = +/- 90 degrees, so that the aircraft is pointing
!     straight up or down, the heading and roll angles are no longer
!     unique so the transformation fails.  This condition is trapped to
!     prevent the code crashing in this unlikely eventuality, with roll
!     and yaw rates of 0.0 deg/s returned instead.  There is no need to
!     return a warning flag, however, as pitch angles this steep already
!     trip the max/min checks in the main body of code, leading to
!     computed values having flags of 2.
!
!     Current Code Owner: W.D.N. Jackson
!
!     History:
!     Version  Date      Comment
!
!       1.00   10/01/94  Original code. (W.D.N. Jackson)
!       1.01   09/07/98  Volatile statement added to fix bug which caused 
!                        wrong values to be returned when the input and output 
!                        arguments were the same (W.D.N. Jackson)
!       2.00   08/10/02  Transformations corrected to yield the true yaw, 
!                        pitch and roll rates of the aircraft with respect to 
!                        the local geodetic frame. (G.W. Inverarity)
!
!     Code Description:
!     FORTRAN 77 with extensions recommended in the Met Office F77
!     Standard.
!
!     Scalar arguments with INTENT(IN):
!
      REAL*4 LAT        ! Latitude (deg)
      REAL*4 ALT        ! Altitude (m)
      REAL*4 VN         ! Northward velocity component (m/s)
      REAL*4 VE         ! Eastward velocity component (m/s)
      REAL*4 RP         ! Attitude rate component forward axis (deg/s)
      REAL*4 RQ         ! Attitude rate component starboard axis (deg/s)
      REAL*4 RR         ! Attitude rate component downward axis (deg/s)
      REAL*4 RROL       ! Roll angle phi (deg)
      REAL*4 RPIT       ! Pitch angle theta (deg)
      REAL*4 RHDG       ! Heading angle psi (deg)

!     Scalar arguments with INTENT(OUT):

      REAL*4 RRR        ! Roll rate (deg/s)
      REAL*4 RPR        ! Pitch rate (deg/s)
      REAL*4 RYR        ! Yaw rate (deg/s)

!     Local Parameters:

      REAL*8    OMEGA       ! WGS-84 Earth angular frequency (rad/s)
      PARAMETER (OMEGA=7.292115D-5)

!     Local Scalars:

      REAL*4 CR         ! cos(phi)
      REAL*4 SR         ! sin(phi)
      REAL*4 OMEGAF     ! Angular frequency component forward axis
      REAL*4 OMEGAN     ! Angular frequency component northward axis
      REAL*4 OMEGAS     ! Angular frequency component starboard axis
      REAL*4 OMEGAW     ! Angular frequency component westward axis
      REAL*4 OMEGAU     ! Angular frequency component body upward axis
      REAL*4 OMEGAZ     ! Angular frequency component geodetic upward axis
      REAL*4 RP1        ! Corrected angular frequency component forward axis
      REAL*4 RQ1        ! Corrected angular frequency component starboard axis
      REAL*4 RR1        ! Corrected angular frequency component downward axis
      REAL*4 TERM       ! q' sin(phi) + r' cos(phi)
      REAL*8 DL         ! Rate of change of latitude (rad/s)
      REAL*8 DLAMBDA    ! Rate of change of longitude (rad/s)
      REAL*8 DTERM      ! Omega + d lambda / d t
      REAL*8 RL         ! Meridional radius of curvature (m)
      REAL*8 RLAMBDA    ! Azimuthal radius of curvature (m) / COS(LAT)

!     Functions and subroutines used:

      EXTERNAL C_INS1_TRANS_ACCL ! In this file
      EXTERNAL RLATLONG ! In this file

!-    End of header

! Sum of Earth rate and platform rate angular frequency vectors in
! local geodetic co-ordinates.

      CALL RLATLONG(DBLE(LAT), ALT, VN, VE, RL, RLAMBDA, DL, DLAMBDA)
      DTERM = OMEGA + DLAMBDA
      OMEGAN = REAL(DTERM * COSD(DBLE(LAT)))
      OMEGAW = REAL(DL)
      OMEGAZ = REAL(DTERM * SIND(DBLE(LAT)))

! Transform into forward/starboard/upward aircraft body co-ordinates.

      CALL C_INS1_TRANS_ACCL(OMEGAN, OMEGAW, OMEGAZ, RROL, RPIT, RHDG,
     &     OMEGAF, OMEGAS, OMEGAU)

! Subtract this angular frequency vector from that reported by the INU
! in forward/starboard/downward aircraft body co-ordinates.

      RP1 = RP - OMEGAF
      RQ1 = RQ - OMEGAS
      RR1 = RR + OMEGAU

! Error in the roll rate and yaw rate if the pitch angle is within 2^(-22) 
! of +/- 90 degrees.

      IF (ABS(ABS(RPIT) - 90.0) .LT. 2.3841858E-7) THEN
         RRR = 0.0
         RYR = 0.0
      ELSE
         CR = COSD(RROL)
         SR = SIND(RROL)
         TERM = RQ1 * SR + RR1 * CR

         RRR = RP1 + TERM * TAND(RPIT)
         RYR = TERM / COSD(RPIT)
      END IF
      RPR = RQ1 * CR - RR1 * SR
      RETURN
      END
C*******************************************************************************
      SUBROUTINE C_INS1_TRANS_ACCL(RAX,RAY,RAZ,RROL,RPIT,RHDG,RAF,RAS,
     -    RAU)
C
C Transforms accelerations from the platform (navigation) frame to the 
C aircraft frame.  Uses the transpose of the Direction Cosine Matrix defined
C in SNU 84-1 Rev D, section 6.5.2.
C
C CHANGES         V1.01  24-07-98  G.W. Inverarity
C                 Entries RT(2,1), RT(3,1) corrected.
C
CDEC$ IDENT 'V1.01'
      REAL*4 RT(3,3)                   !Full transformation matrix

      SA=SIND(RHDG)                    !Compute sines and cosines
      CA=COSD(RHDG)
      SP=SIND(RPIT)
      CP=COSD(RPIT)
      SR=SIND(RROL)
      CR=COSD(RROL)
      RT(1,1)=CA*CP                    !Load matrix
      RT(2,1)=CA*SP*SR-SA*CR
      RT(3,1)=CA*SP*CR+SA*SR
      RT(1,2)=-SA*CP
      RT(2,2)=-SA*SP*SR-CA*CR
      RT(3,2)=CA*SR-SA*SP*CR
      RT(1,3)=SP
      RT(2,3)=-CP*SR
      RT(3,3)=-CP*CR
      RAF=RT(1,1)*RAX+RT(1,2)*RAY+RT(1,3)*RAZ !Transform accelerations
      RAS=RT(2,1)*RAX+RT(2,2)*RAY+RT(2,3)*RAZ
      RAD=RT(3,1)*RAX+RT(3,2)*RAY+RT(3,3)*RAZ
      RAU=-RAD                         !Convert down to up
      RETURN
      END

!+    Rates of change of latitude and longitude with time.
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 2002, Met Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Subroutine interface:

      SUBROUTINE RLATLONG(LAT, ALT, VN, VE, RL, RLAMBDA, RLAT, RLONG)

CDEC$ IDENT 'V1.00'

      IMPLICIT NONE

!     Description: 
!     Computes the rates of change of latitude and longitude with time.
!
!     Method: 
!     The rate of change of latitude L and longitude lambda with time are, 
!     respectively,
!
!     d L / d t = vn / (rL + h), 
!     d lambda / d t = ve / (rlambda + h) / cos(L),
!
!     where
!
!     rL = R0 (1 - e^2) / (1 - e^2 sin(L)^2)^(3/2), 
!     rlambda = R0 / sqrt(1 - e^2 sin(L)^2), 
!
!     and vn and ve are the components of the aircraft's velocity in the
!     Earth frame in the northward and eastward directions,
!     respectively, h is the aircraft's altitude above the WGS-84
!     ellipsoid, whose radius of curvature and eccentricity are,
!     respectively, R0 and e.
!
!     Note that LAT is REAL*8 and the radii of curvature are returned
!     to allow this routine to be used by INTTEXT:KF_EXTR.FOR as well.
!
!     Current Code Owner: W.D.N. Jackson
!
!     History:
!     Version    Date     Comment
!   
!        1.00    23/09/02 Original code. (G.W. Inverarity)
!
!     Code Description:
!     FORTRAN 77 with extensions recommended in the Met Office F77
!     Standard.
!
!     Scalar arguments with INTENT(IN):

      REAL*8     LAT               ! Latitude (deg)
      REAL*4     ALT               ! Altitude (m)
      REAL*4     VN                ! Northward velocity component (m/s)
      REAL*4     VE                ! Eastward velocity component (m/s)

!     Scalar arguments with INTENT(OUT):

      REAL*8     RL                ! Meridional radius of curvature (m)
      REAL*8     RLAMBDA           ! Azimuthal radius of curvature (m)
                                   ! / COS(LAT)
      REAL*8     RLAT              ! Latitude rate of change (rad/s)
      REAL*8     RLONG             ! Longitude rate of change (rad/s)

!     Local Parameters:

      REAL*8    F                  ! WGS-84 flattening
      PARAMETER(F=1.0D0/298.257223563D0)
      REAL*8    EPSQ               ! WGS-84 eccentricity squared
      PARAMETER(EPSQ=F*(2.0D0-F))
      REAL*8    R0                 ! WGS-84 equatorial radius (m)
      PARAMETER(R0=6.378137D6)

!     Local Scalars:

      REAL*8     TERM              ! 1 - EPSQ SIN(LAT)^2.

!-    End of header

      TERM = 1.0D0 - EPSQ * SIND(LAT) ** 2
      RL = R0 * (1.0D0 - EPSQ) / TERM ** 1.5D0
      RLAMBDA = R0 / SQRT(TERM)
      RLAT = DBLE(VN) / (RL + DBLE(ALT))
      RLONG = DBLE(VE) / (RLAMBDA + DBLE(ALT)) / COSD(LAT)
      RETURN
      END
