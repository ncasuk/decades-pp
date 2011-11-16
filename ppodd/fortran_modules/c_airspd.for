C
C ROUTINE          C_AIRSPD SUBROUTINE FORTVAX
C     
C PURPOSE          Derives Indicated and True Airspedd (paras 516 & 517)
C
C DESCRIPTION      True Air Speed is the component of air flow parallel to
C                  the Aircraft's longitudinal axis.
C
C                  IAS =  340.294 
C                         * Mach no. 
C                         * SQRT(Static Pressure[mb]/ 1013.25)      in ms-1
C
C                  TAS =  A/S correction factor 
C                         * 340.294 
C                         * Mach no. 
C                         * SQRT(De-iced True Air Temp[K] / 288.15) in ms-1
C                  
C                  where:
C
C                    288.15   is ICAO Standard temperature [K] at zero altitude.
C                    340.294  is speed of sound [ms-1]         at zero altitude.
C                    Mach no. is computed by subroutine S_MACH.
C
C                  Flagging - If a value can't be computed, due to missing data
C                  missing constants, divide be zeroes, etc, a value of 0 is
C                  used, flagged with a three.  If a value is outside its 
C                  limits for range or rate of change, it is flagged with a two.
C                  If there are no problems with the data it is flagged with 0.
C                  Any flags on input data are propagated through subsequent 
C                  calculations.
C
C VERSION          1.00  020190   A.D.HENNINGS
C
C ARGUMENTS        For Indicated Airspeed:
C                    RDER(IN,576) REAL*4 IN  Derived static pressure in mb
C                    RDER(IN,577) REAL*4 IN  Derived Pitot static pressure in mb
C                                            (samples IN = 1,32 )
C                    RDER(OP,516) REAL*4 OUT Derived Indicated Airspeed  ms-1
C                                            (samples OP = 1,32 )
C                  For True Airspeed:
C                    RCONST(1)    REAL*4 IN  True Airspeed correction factor  
C                    RDER(IN,576) REAL*4 IN  Derived static pressure in mb
C                    RDER(IN,577) REAL*4 IN  Derived Pitot static pressure in mb
C                    RDER(IN,520) REAL*4 IN  Derived De-iced True air temp deg K
C                                            (samples IN = 1,32 )
C                    RDER(IN,525) REAL*4 IN  Derived Non-Deiced True air temp deg K
C                                            (samples IN = 1,32 )
C                    RDER(OP,517) REAL*4 OUT Derived True Airspeed  ms-1
C                                            (samples OP = 1,32 )
C
C SUBPROGRAMS      S_MACH, S_QCPT, ITSTFLG, ISETFLG
C
C REFERENCES       Code adapted from MRF1/HORACE
C                  n.b. RCONST(1) (Air speed correction factor 'K' should be
C                       determined by 'K & Gamma' aircraft runs. The value
C                       in RCONST(1) is unity. Experimental values have been
C                       found between 0.98 and 1.02; (HORACE used 1.002 from 
C                       June 1988 - Jan 1990., value suggested by S.Nicholls
C                       after JASIN experiment).
C
C CHANGES          V1.01  02/06/93  Limit on max rate of change between
C                  adjacent samples has been increased to 3.3 m/s.  This is
C                  based on analysis of the high turbulence A257 flight, where
C                  the histogram of rates of change showed meaningful changes
C                  of up to 3.0 m/s between adjacent samples. (WDNJ)
C                  Also changed so that data with flags of 2 are processed
C                  rather than rejected and flags are stripped from data before
C                  processing. (WDNJ)
C
C                  V1.02 20/06/06 If TAT_DI flag is 2 or more, then takes
C                  temperature input from TAT_NDI (Phil Brown)
C
C*******************************************************************************
      SUBROUTINE C_AIRSPD(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.02'
C
      IMPLICIT  INTEGER*4 (I)
      IMPLICIT  REAL*4    (R)
      INTEGER*4 IRAW(64,512),IFRQ(512)
      REAL*4    RCONST(64),RDER(64,1024)

      DATA      R516ERCNT,R517ERCNT /2*1.0/       !Stores S_QCPT error counts
      DATA      RLV516,RLV517       /2*0.0/       !Stores latest values
      DATA      RLT516,RLT517       /2*0.0/       !Stores latest times
                                                      
      PARAMETER IDATFRQ=32             !Output frequency
      PARAMETER R516MX=140.            !Maximum value for IAS - m/s
      PARAMETER R516MN=0.              !Minimum value for IAS - m/s
      PARAMETER R516RG=3.3             !Max diff between IAS 32 Hz samples - m/s
      PARAMETER R517MX=215.            !Maximum value for TAS - m/s
      PARAMETER R517MN=0.              !Minimum value for TAS - m/s
      PARAMETER R517RG=3.3             !Max diff between TAS 32 Hz samples - m/s
C
C Note that if this routine does not compute the IAS or TAS for any reason then
C CALIBRATE will automatically use values of zero flagged with threes.
C
      SAVE
      RSEC=RDER(1,515)                            !Time, secs past midnight
      ICORFLG=ITSTFLG(RCONST(1))                  !Note correction factr flg
      DO IS=1,IDATFRQ                             !For each data sample
        ISTPFLG=ITSTFLG(RDER(IS,576))             !Note Static press flag
        IPSPFLG=ITSTFLG(RDER(IS,577))             !Note Pitot Static flag
        ITATFLG_DI=ITSTFLG(RDER(IS,520))          !Note DI True Air temp flag
        ITATFLG_NDI=ITSTFLG(RDER(IS,525))         !Note NDI True Air temp flag
        RSTP=RDER(IS,576)                         !Take latest STP
        RTAT_DI=RDER(IS,520)                         !Take latest DI TAT
        RTAT_NDI=RDER(IS,525)                         !Take latest NDI TAT
        CALL ISETFLG(RSTP,0)                      !Clear flag from data
        CALL ISETFLG(RTAT_DI,0)                      !Clear flag from data
        CALL ISETFLG(RTAT_NDI,0)                      !Clear flag from data
C
C Derive Mach Number
C
        IMACFLG=3                                 !Default flag on Mach no
        ITMPFLG=MAX(ISTPFLG,IPSPFLG)     
        IF(ITMPFLG.LT.3.AND.RSTP.GT.0) THEN       !If no flag 3 data
          CALL S_MACH(RDER(IS,576),RDER(IS,577),RMACH) !Compute Mach number
          IMACFLG=ITSTFLG(RMACH)                  !Note Mach no flag
          CALL ISETFLG(RMACH,0)                   !Clear flag from data
        END IF
C
C Derive Indicated Air Speed
C
        IIASFLG=MAX(IMACFLG,ISTPFLG)
        IF(IIASFLG.LT.3.AND.RSTP.GT.0) THEN       !If no flag 3 data
          RDER(IS,516)=340.294*RMACH*SQRT(RSTP/1013.25) !Derive IAS
          CALL S_QCPT(RSEC,RLT516,RDER(IS,516),RLV516, !Quality control point
     -        R516MX,R516MN,R516RG,3.,R516ERCNT,IQFLAG)
          IIASFLG=MAX(IIASFLG,IQFLAG)             !Check Q/C flag
          CALL ISETFLG(RDER(IS,516),IIASFLG)      !Apply flag
        END IF
C
C Derive True Air Speed
C
        RDER(IS,517)=0.0
        CALL ISETFLG(RDER(IS,517),3)              ! default zero-flag3
C
        IF(ITATFLG_DI.LE.1) THEN                  ! If DI TAT OK use for calcs
          ITASFLG=MAX(IMACFLG,ITATFLG_DI,ICORFLG)           
          IF(ITASFLG.LT.3.AND.RTAT_DI.GT.0) THEN       !If no flag 3 data
            RDER(IS,517)=RCONST(1)*340.294*RMACH*SQRT(RTAT_DI/288.15) !Derive TAS
            CALL S_QCPT(RSEC,RLT517,RDER(IS,517),RLV517, !Quality control point
     -          R517MX,R517MN,R517RG,3.,R517ERCNT,IQFLAG)
            ITASFLG=MAX(ITASFLG,IQFLAG)             !Check Q/C flag
            CALL ISETFLG(RDER(IS,517),ITASFLG)      !Apply flag
          ENDIF
        ELSE                                      ! otherwise use NDI TAT
          ITASFLG=MAX(IMACFLG,ITATFLG_NDI,ICORFLG)           
          IF(ITASFLG.LT.3.AND.RTAT_NDI.GT.0) THEN       !If no flag 3 data
            RDER(IS,517)=RCONST(1)*340.294*RMACH*SQRT(RTAT_NDI/288.15) !Derive TAS
            CALL S_QCPT(RSEC,RLT517,RDER(IS,517),RLV517, !Quality control point
     -          R517MX,R517MN,R517RG,3.,R517ERCNT,IQFLAG)
            ITASFLG=MAX(ITASFLG,IQFLAG)             !Check Q/C flag
            CALL ISETFLG(RDER(IS,517),ITASFLG)      !Apply flag
          ENDIF
        ENDIF

      END DO                                      !Next sample
      RETURN
      END
