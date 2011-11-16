C
C ROUTINE          C_PRESS SUBROUTINE FORTVAX
C     
C PURPOSE          Calibrates static, Pitot-static and cabin pressures.  Derives
C                  pressure height.
C
C DESCRIPTION      Apply calibration coefficients to DRS parameters 8, 9 and 14
C                  to obtain Static Pressure (Para 576), Pitot-static pressure
C                  (Para 577) and Cabin Pressure (Para 579) in mb; also derive
C                  Pressure height (Para 578) in metres.  Parameter 14 does not
C                  have to be present and if absent Para 579 is filled with 0's
C                  flagged with threes.
C
C METHOD           For each DRS parameter to be calibrated:
C                  1. Check all its required constants are present (FLAG <3)
C                  2. Mask the 12 data bits and float the result.
C                  3. Calibrate the value using the constants within a linear
C                     or quadratic equation.
C                  4. Check the result for being within acceptable values.
C                  5. Set data flag bits (16+17) 0: Good data
C                                                1: Data of lower quality
C                                                2: Probably faulty, exceed lims
C                                                3: Data absent or invalid.
C                  Note that parameter 14 (cabin pressure) is optional; this
C                  module will be called by CALIBRATE whether the DRS was
C                  recording cabin pressure or not.
C
C                  Flagging - If a value can't be computed, due to missing data
C                  missing constants, divide be zeroes, etc, a value of 0 is
C                  used, flagged with a three.  If a value is outside its 
C                  limits for range or rate of change, it is flagged with a two.
C                  If there are no problems with the data it is flagged with 0.
C                  Any flags on input data are propagated through subsequent 
C                  calculations.
C
C VERSION          1.01  280892   A.D.HENNINGS
C
C ARGUMENTS        For Static pressure:
C                    RCONST(1) - REAL*4 IN   Constant in quadratic calib eqn.
C                    RCONST(2) - REAL*4 IN   Coeff X  in quadratic calib eqn.
C                    RCONST(3) - REAL*4 IN   Coeff X2 in quadratic calib eqn.
C                    IFRQ(8)   - INT*4  IN   Input frequency of sampling
C                    IRAW(IN,8)- INT*4  IN   Raw DRS static pressure sensor o/p
C                                            (samples IF = 1,IFRQ(8))
C                    RDER(OP,576) REAL*4 OUT Derived static pressure in mb.
C                                            (samples OP = 1,32)
C                  For Pitot-static pressure:
C                    RCONST(4) -  REAL*4 IN  Constant in linear calib eqn.
C                    RCONST(5) -  REAL*4 IN  Coeff X  in linear calib eqn.
C                    IFRQ(9)   -  INT*4  IN  Input frequency of sampling
C                    IRAW(IN,9)-  INT*4  IN  Raw DRS Pitot-static press sens o/p
C                                            (samples IF = 1,IFRQ(9))
C                    RDER(OP,577) REAL*4 OUT Derived Pitot-static pressure in mb
C                                            (samples OP = 1,32)
C                  For Pressure height:      
C                    RDER(IN,576) REAL*4 IN  Derived static pressure in mb.
C                                            (samples IN = 1,32)
C                    RDER(OP,578) REAL*4 OUT Derived Pressure height in metres
C                                            (samples OP = 1,32)
C                    n.b. computed by S_PHGT, valid limits -206.0m to 11.0km
C
C                  For Cabin pressure:
C                    RCONST(6) -  REAL*4 IN  Constant in quadratic calib eqn.
C                    RCONST(7) -  REAL*4 IN  Coeff X  in quadratic calib eqn.
C                    RCONST(8) -  REAL*4 IN  Coeff X2 in quadratic calib eqn.
C                    IFRQ(14) -   INT*4  IN  Input frequency of sampling
C                    IRAW(IN,14)  INT*4  IN  Raw DRS Cabin pressure sensor o/p
C                                            (samples IF = 1,IFRQ(14)) 1HZ
C                    RDER(OP,579) REAL*4 OUT Derived Cabin pressure in mb
C                                            (samples OP = 1)
C
C SUBPROGRAMS      S_PHGT, S_QCPT, ITSTFLG, ISETFLG 
C
C REFERENCES        Range limits taken from MRF1/MRF2 and match the sensor range
C                  Rates of change estimated using a max rate of descent
C                  of approx 1000m /minute give Static pressure r.o.c. of 
C                  0.05 mb between 32Hz samples, which is less than the
C                  resolution of 0.25 mb per DRS unit. Therefore the r.o.c.
C                  limit for static pressure is set to 1mb between samples,
C                  as the typical noise at low level is less than 3 units
C                  (=0.75 mb).
C                   The Pitot-static system is much noisier at low level
C                  and samples may be up to 50 DRS units different. Therefore 
C                  the r.o.c. limit is set to 1.5 mb (37 DRSU) to capture 
C                  only the extreme noise and spikes.
C                   The Cabin pressure sensor is of the same type as is used
C                  in the Static pressure system. Limits on normal range are
C                  the same maximum pressure as Static, a minimum set of 650mb
C                  which is marginally lower than the altitude power cut-out
C                  switch's activating level at 10,000ft.  Rates of change can
C                  vary in excess of environmental rates due to manual control
C                  of cabin pressurisation.
C                  
C CHANGES          V1.00 23/01/90  ADH Original version
C                  V1.01 28/08/92  Includes calibration of CABIN pressure (ADH)
C                  V1.02 02/06/93  Pitot-static r.o.c. limit now set to 4.4mb 
C                  (was 1.5mb) between samples.  This is based on analysis of 
C                  the high turbulence A257 flight when there were meaningful 
C                  changes of up to 4.0 mb between samples.  The limit on rate 
C                  of change of cabin pressure has been adjusted to 2.5mb/s in
C                  the light of experience.  Data flagged 2 is now processed, 
C                  and data flags propagate consistently.  (WDNJ)
C
C*******************************************************************************
      SUBROUTINE C_PRESS(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.02'
C
      IMPLICIT  INTEGER*4 (I)
      IMPLICIT  REAL*4    (R)
      INTEGER*4 IRAW(64,512),IFRQ(512)
      REAL*4    RCONST(64),RDER(64,1024)
      INTEGER*4 ICFLAG(8)
      DATA      R576ERCNT, R577ERCNT,R577ERCNT /3*1.0/ !S_QCPT error counts
      DATA      RLV576,RLV577,RLV579 /3*0./            !S_QCPT last good values
      DATA      RLT576,RLT577,RLT579 /3*0./            !S_QCPT last times
C
      PARAMETER R576MX=1050.           !Max static pressure (mb)
      PARAMETER R576MN=100.            !Min static pressure (mb)
      PARAMETER R576RG=1.              !Max static pressure change (32mb/s)
      PARAMETER R577MX=125.            !Max Pitot-static pressure (mb)
      PARAMETER R577MN=0.              !Min Pitot-static pressure (mb)
      PARAMETER R577RG=4.4             !Max Pitot-static pressure chnge (32mb/s)
      PARAMETER R579MX=1050.           !Max cabin pressure (mb)
      PARAMETER R579MN=650.            !Min cabin pressure (mb)
      PARAMETER R579RG=2.5             !Max cabin pressure change mb/s
C
C Note that if this routine does not compute a value for any reason then
C CALIBRATE will automatically use values of zero flagged with threes.
C
      SAVE
C      SAVE RLV576,RLV577,RLV579
C      SAVE RLT576,RLT577,RLT579
C      SAVE R576ERCNT,R577ERCNT,R579ERCNT

      DO IT=1,8
        ICFLAG(IT)=ITSTFLG(RCONST(IT)) !Note Constants flags
      END DO
      RSEC=RDER(1,515)                 !Time - seconds past midnight
C
C Derive static pressure and pressure height
C
      ICONFLG=MAX(ICFLAG(1),ICFLAG(2),ICFLAG(3)) !Check constants flags
      IF(ICONFLG.LT.3.AND.IFRQ(8).GT.0) THEN
        DO IS=1,IFRQ(8)                !For each data sample
          RVAL=FLOAT(IRAW(IS,8).AND.'FFF'X) !Just keep 12 DRS bits
          ISTPFLG= 0 !ITSTFLG(IRAW(IS,8))  !Check for GOULD flags
          IF(ISTPFLG.LT.3) THEN 
            RDER(IS,576)=(RVAL*RCONST(3)+RCONST(2))*RVAL+RCONST(1) !Cal static
            CALL S_QCPT(RSEC,RLT576,RDER(IS,576),RLV576, !Quality control point
     -          R576MX,R576MN,R576RG,3.,R576ERCNT,IQFLAG)
            ISTPFLG=MAX(ISTPFLG,IQFLAG)
            CALL ISETFLG(RDER(IS,576),ISTPFLG) !Apply flag
            IF(ISTPFLG.LT.3) CALL S_PHGT(RDER(IS,576),RDER(IS,578)) !Press hght
          END IF
        END DO                         !Next sample
      END IF
C
C Derive Pitot-static pressure
C 
      ICONFLG=MAX(ICFLAG(4),ICFLAG(5)) !Check constants flags
      IF(ICONFLG.LT.3.AND.IFRQ(9).GT.0) THEN
        DO IS=1,IFRQ(9)                !For each data sample
          RVAL=FLOAT(IRAW(IS,9).AND.'FFF'X) !Just keep 12 DRS bits
          IPSPFLG=0 !ITSTFLG(IRAW(IS,9))  !Check for GOULD flags
          IF(IPSPFLG.LT.3) THEN
            RDER(IS,577)=RVAL*RCONST(5)+RCONST(4) !Calibrate Pitot-static
            CALL S_QCPT(RSEC,RLT577,RDER(IS,577),RLV577, !Quality control point
     -          R577MX,R577MN,R577RG,3.,R577ERCNT,IQFLAG)
            IPSPFLG=MAX(IPSPFLG,IQFLAG)
            CALL ISETFLG(RDER(IS,577),IPSPFLG) !Apply flag
          END IF
        END DO                         !Next sample
      ENDIF
C
C Derive Cabin Pressure - note that this parameter was never processed on the
C GOULD computer so there is no need to check the raw data for flags.
C 
      ICONFLG=MAX(ICFLAG(6),ICFLAG(7),ICFLAG(8)) !Check constants flags
      IF(ICONFLG.LT.3.AND.IFRQ(14).GT.0) THEN
        DO IS=1,IFRQ(14)               !For each data sample
          RVAL=FLOAT(IRAW(IS,14).AND.'FFF'X) !Just keep 12 DRS bits
          RDER(IS,579)=(RVAL*RCONST(8)+RCONST(7))*RVAL+RCONST(6) !Cal cabin pres
           CALL S_QCPT(RSEC,RLT579,RDER(IS,579),RLV579, !Quality control point
     -        R579MX,R579MN,R579RG,3.,R579ERCNT,IQFLAG)
         CALL ISETFLG(RDER(IS,579),IQFLAG) !Apply flag
        END DO                         !Next sample
      END IF
C 
      RETURN
      END
