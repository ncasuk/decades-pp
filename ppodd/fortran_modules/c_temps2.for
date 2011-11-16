C
C ROUTINE          C_TEMPS2 SUBROUTINE FORTVAX
C     
C PURPOSE          Produces calibrated deiced and non-deiced temperatures
C
C DESCRIPTION      Calculates indicated and true air temperatures in K for the
C                  Deiced and Non-Deiced temperature sensors as follows:
C
C                  519 - Indicated Air Temperature  from Deiced     [K] at 32Hz
C                  520 - True Air Temperature       from Deiced     [K] at 32Hz
C                  524 - Indicated Air Temperature  from Non-deiced [K] at 32Hz
C                  525 - True Air Temperature       from Non-deiced [K] at 32Hz
C
C                  Note that this module only processes data recorded on the
C                  146 which only uses one parameter per temperature.
C                
C                  The Deiced Temperature is recorded on the DRS at 32Hz as 
C                  parameter 10 and the Non-deiced Temperature is recorded on 
C                  the DRS as parameter 23.
C
C                  Indicated Air Temperature is derived by application of 
C                  the appropriate second order calibration coefficients to the
C                  raw data.
C
C                  A correction for heating from the deicing heater is made to
C                  the deiced indicated air temperature if the heater is 
C                  switched on, as indicated by bit 5 of the signal register
C                  (parameter 27) being clear.  This heating correction is
C                  obtained from graphs of Temperature vs Machno in Rosemount
C                  Technical Reports 7597 & 7637.  If Machno is less than
C                  0.1 the data is flagged 1, because the Rosemount graph is 
C                  invalid below 0.1, and if Machno below 0.05, a value of 0.05
C                  is use to ensure a valid logarithm.  The algorithm used for
C                  heating correction is:
C
C             (exp(exp(1.171+(log(Machno)+2.738)*(-0.000568*(s+q)-0.452))))*0.1 
C
C                  where: s=static pressure       [mbs]
C                         q=pitot static pressure [mbs]
C
C
C                  True Air Temperature is derived as:
C
C                  TAT[K] = (Indicated Air Temperature[K]) /
C                                (1.0 +(0.2 * MACHNO * MACHNO * TRECFTR))
C
C                  where: MACHNO  is computed by scientific subroutine S_MACH.
C                         TRECFTR is the Temperature recovery factor - used to
C                                 compensate for effects of kinetic heating.
C                                 This is supplied as a constant from the
C                                 flight constants file to this routine.
C
C                                 It can be calculated from flight results of
C                                 slow/fast runs as:
C
C                   (Tindfast-Tindslow)/(Ffast*Tindslow-Fslow*Tindfast)
C
C                                 where: Tind = indicated temperature [K]
C                                        F    = 0.2 * Machno * Machno
C
C                  Flagging:
C
C                  Both deiced and non-deiced temperature calculations follow
C                  a similar scheme for error flagging, with worst case flags
C                  being propagated through the calculations.  Sources of error
C                  flags are: 
C
C                      Absence of calibration constants    - flag 3
C                      Absence of recovery factor constant - flag 3
C                      Static pressure errors              - Parameter 576 flag
C                      Pitot pressure errors               - Parameter 577 flag
C                      Max/min/rate of change errors       - flag 2
C                      Mach No less than 0.1               - flag 1
C
C                  Not all the above errors need affect all measurements.  For
C                  instance pressure errors will not affect Indicated Air
C                  Temperatures, unless the deicing heater is on.  Note that
C                  this module cannot be called if any of the raw (not derived)
C                  parameters are missing.  Also note that no raw data on which
C                  this module can be used will be carrying flags (only raw
C                  data transcribed on the Gould computer can carry flags).  If
C                  any temperature has a flag of three, its value is set to
C                  0.0 K (and flagged with a three).
C
C VERSION          1.00  10/09/92  W.D.N.JACKSON
C
C ARGUMENTS     
C                  Constants:
C                  RCONST(1)   Recovery factor for Deiced sensor
C                  RCONST(2)   Recovery factor for Non-deiced sensor
C                  RCONST(3)   Deiced X0 calibration constant (deg C)
C                  RCONST(4)   Deiced X1 calibration constant (deg C)
C                  RCONST(5)   Deiced X2 calibration constant (deg C)
C                  RCONST(6)   Non-deiced X0 calibration constant (deg C)
C                  RCONST(7)   Non-deiced X1 calibration constant (deg C)
C                  RCONST(8)   Non-deiced X2 calibration constant (deg C)
C
C                  Inputs: 
C                  DEICED TEMPERATURE            [bits 0-15]     Para  10 32Hz
C                  NON DEICED TEMPERATURE        [bits 0-15]     Para  23 32Hz
C                  SIGNAL REGISTER               [drs units-bcd] Para  27  2Hz
C                  STATIC PRESSURE               [mbs]           Para 576 32Hz
C                  PITOT STATIC PRESSURE         [mbs]           Para 577 32Hz
C
C                  Outputs: 
C                  INDICATED AIR TEMPERATURE (Deiced)   [K]      Para 519 32Hz
C                  TRUE AIR TEMPERATURE      (Deiced)   [K]      Para 520 32Hz
C                  INDICATED AIR TEMPERATURE (NonDeiced)[K]      Para 524 32Hz
C                  TRUE AIR TEMPERATURE      (NonDeiced)[K]      Para 525 32Hz
C                
C SUBPROGRAMS      S_MACH           Calculates Mach no
C                  ITSTFLG          Examines bits 16,17 for flags
C                  ISETFLG          Sets flag bits 16,17 = 0 --> 3
C                  S_QCPT           Performs range and rate of change check
C
C REFERENCES       Code adapted from C_TEMPS module.  See MRF Internal Note 55 -
C                  'Temperature Measurement Working Group Report' for full
C                  details of C-130 temperature measurement.
C
C CHANGES          V1.01  27/09/02  W.D.N.JACKSON
C                  Changed to handle 16 bit temperature recording.
C                  V1.02  23/05/05  D.A.TIDDEMAN
C                  Temperature heater correction changed to opposite sense
C                  Now raw para 27 bit 5 on = heater on
C*******************************************************************************
      SUBROUTINE C_TEMPS2(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.01'
      INTEGER*4 IRAW(64,512),IFRQ(512)
      REAL*4    RCONST(64),RDER(64,1024),RMACH(32)

      DATA      RLV519,RLV520,RLT519,RLT520/4*0./    !Init first time through
      DATA      RLV524,RLV525,RLT524,RLT525/4*0./    !Init first time through
      DATA      R519ERCNT,R520ERCNT/2*1.0/           !Init first time through
      DATA      R524ERCNT,R525ERCNT/2*1.0/           !Init first time through
      
      PARAMETER (R519MX=320.,R519MN=203.,R519RG=1.)  !Limits checks TEMPS [K]
      PARAMETER (R520MX=320.,R520MN=203.,R520RG=1.)  !Limits checks TEMPS [K]
      PARAMETER (R524MX=320.,R524MN=203.,R524RG=1.)  !Limits checks TEMPS [K]
      PARAMETER (R525MX=320.,R525MN=203.,R525RG=1.)  !Limits checks TEMPS [K]
C
      SAVE
      RSEC=RDER(1,515)                 !Time in seconds past midnight
      DO IS=1,32                       !Compute mach no for each sample
        RMACH(IS)=0.
        CALL ISETFLG(RMACH(IS),3)
        IF(ITSTFLG(RDER(IS,576)).NE.3.AND.ITSTFLG(RDER(IS,577)).NE.3)
     &    CALL S_MACH(RDER(IS,576),RDER(IS,577),RMACH(IS)) !Compute mach no
        IFLAG=ITSTFLG(RMACH(IS))       !Save its flag
        CALL ISETFLG(RMACH(IS),0)      !Strip flag
        IF(RMACH(IS).LT.0.05) RMACH(IS)=0.05 !Must be small and +ve
        IF(RMACH(IS).LT.0.1) IFLAG=MAX(IFLAG,1) !If airspeed <.1 set flag to 1
        CALL ISETFLG(RMACH(IS),IFLAG)  !Reapply flag
      END DO
C
C Calculate indicated and true deiced temperatures.
C
      ICFLAG=ITSTFLG(RCONST(3))        !Find worst flag on cal constants
      ICFLAG=MAX(ICFLAG,ITSTFLG(RCONST(4)))
      ICFLAG=MAX(ICFLAG,ITSTFLG(RCONST(5)))
      DO IS=1,IFRQ(10)                 !For each sample of data
        RV=FLOAT(IRAW(IS,10))          !Convert to real
C Calibrate to get indicated temperature
        RDER(IS,519)=RCONST(3)+RCONST(4)*RV+RCONST(5)*RV**2+273.16 !Calibrate
        IFLAG=ICFLAG                   !Set flag if constants were invalid
        IP=((IS*IFRQ(27)-1)/IFRQ(10))+1 !Signal register sample (1 or 2)
C If deicing heater is on, correct for the heating effect
        IF(BTEST(IRAW(IP,27),5)) THEN !If heater was on - removed.NOT.23/05/05
          RM=RMACH(IS)                 !Get mach no
          RS=RDER(IS,576)              !Get static pressure
          RP=RDER(IS,577)              !Get pitot-static pressure
          CALL ISETFLG(RM,0)           !Clear any flag bits
          CALL ISETFLG(RS,0)           !Clear any flag bits
          CALL ISETFLG(RP,0)           !Clear any flag bits
          RHCORR=0.1*EXP(EXP(1.171+(ALOG(RM)+2.738)* !Compute heater correction
     -        (-0.000568*(RS+RP)-0.452)))
          RDER(IS,519)=RDER(IS,519)-RHCORR !Apply to indicated temperature
          IFLAG=MAX(IFLAG,ITSTFLG(RMACH(IS))) !Note errors on mach no
          IFLAG=MAX(IFLAG,ITSTFLG(RDER(IS,576))) !Note errors on static
          IFLAG=MAX(IFLAG,ITSTFLG(RDER(IS,577))) !Note errors on pitot
        END IF
C Apply any flags, do quality control, any reflag if necessary
        IF(IFLAG.EQ.3) RDER(IS,519)=0.0 !If completely invalid set to zero
        CALL S_QCPT(RSEC,RLT519,RDER(IS,519),RLV519,R519MX,R519MN,
     -      R519RG,64.0,R519ERCNT,IQFLAG) !Carry out quality control
        IFLAG=MAX(IFLAG,IQFLAG)        !Use QC flag if worse
C Now work out true temperature
        RM=RMACH(IS)                   !Get mach no
        CALL ISETFLG(RM,0)             !Clear any flag bits
        RDER(IS,520)=RDER(IS,519)/(1.0+(0.2*RM**2*RCONST(1))) !Convert to true
        CALL ISETFLG(RDER(IS,519),IFLAG) !Apply flag to indicated temperature
C Apply any flags, do quality control, and reflag if necessary
        IFLAG=MAX(IFLAG,ITSTFLG(RMACH(IS))) !Note any errors on mach no
        IFLAG=MAX(IFLAG,ITSTFLG(RCONST(1))) !Note any errors on recovery factor
        IF(IFLAG.EQ.3) RDER(IS,520)=0.0 !If completely invalid set to zero
        CALL S_QCPT(RSEC,RLT520,RDER(IS,520),RLV520,R520MX,R520MN,
     -      R520RG,64.0,R520ERCNT,IQFLAG) !Carry out quality control
        IFLAG=MAX(IFLAG,IQFLAG)        !Use QC value if worse
        CALL ISETFLG(RDER(IS,520),IFLAG) !Apply flag to true temperature
      END DO                           !Get next temperature sample
C
C Calculate indicated and true non-deiced temperatures.
C
      ICFLAG=ITSTFLG(RCONST(6))        !Find worst flag on cal constants
      ICFLAG=MAX(ICFLAG,ITSTFLG(RCONST(7)))
      ICFLAG=MAX(ICFLAG,ITSTFLG(RCONST(8)))
      DO IS=1,IFRQ(23)                 !For each sample of data
        RV=FLOAT(IRAW(IS,23))          !Convert to real
C Calibrate to get indicated temperature
        RDER(IS,524)=RCONST(6)+RCONST(7)*RV+RCONST(8)*RV**2+273.16 !Calibrate
C Apply any flags, do quality control, any reflag if necessary
        IFLAG=ICFLAG                   !Set flag if constants were invalid
        IF(IFLAG.EQ.3) RDER(IS,524)=0.0 !If completely invalid set to zero
        CALL S_QCPT(RSEC,RLT524,RDER(IS,524),RLV524,R524MX,R524MN,
     -      R524RG,64.0,R524ERCNT,IQFLAG) !Carry out quality control
        IFLAG=MAX(IFLAG,IQFLAG)        !Use QC flag if worse
C Now work out true temperature
        RM=RMACH(IS)                   !Get mach no
        CALL ISETFLG(RM,0)             !Clear any flag bits
        RDER(IS,525)=RDER(IS,524)/(1.0+(0.2*RM**2*RCONST(2))) !Convert to true
        CALL ISETFLG(RDER(IS,524),IFLAG) !Apply flag to indicated temperature
C Apply any flags, do quality control, and reflag if necessary
        IFLAG=MAX(IFLAG,ITSTFLG(RMACH(IS))) !Note any errors on mach no
        IFLAG=MAX(IFLAG,ITSTFLG(RCONST(2))) !Note any errors on recovery factor
        IF(IFLAG.EQ.3) RDER(IS,525)=0.0 !If completely invalid set to zero
        CALL S_QCPT(RSEC,RLT525,RDER(IS,525),RLV525,R525MX,R525MN,
     -      R525RG,64.0,R525ERCNT,IQFLAG) !Carry out quality control
        IFLAG=MAX(IFLAG,IQFLAG)        !Use QC value if worse
        CALL ISETFLG(RDER(IS,525),IFLAG) !Apply flag to true temperature
      END DO                           !Get next temperature sample
C
      RETURN
      END
