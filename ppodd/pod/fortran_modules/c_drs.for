C
C ROUTINE          C_DRS SUBROUTINE FORTVAX
C
C PURPOSE          Calibrates the DRS time, event, signal reg, cab press & temp
C
C DESCRIPTION      Produces time            (515) as real      secs past mdnt
C                  Produces event mark      (644) as real      drs units
C                  Produces signal register (641) as integer   drs units
C                  Produces cabin temp      (660) as real      deg C
C
C VERSION          1.00  10-1-90  N.JACKSON
C
C ARGUMENTS        IRAW(64,512) I*4 IN  Up to 64 samples for up to 512 DRS pars
C                  IFRQ(512)    I*4 IN  Sample rate of each DRS par (0-64)
C                  RCONST(64)   R*4 IN  Inputs constants (none used by C_DRS)
C                  RDER(64,1024)R*4 OUT Output array of up to 64 samples for
C                                       each of 1024 parameters
C
C CHANGES          1.01  7-4-92  W.D.N.JACKSON
C                  Bug fixed which caused time to be 1 minute out after
C                  midnight crossovers.
C
C                  1.02  30-09-02  W.D.N.JACKSON
C                  Changed to handle new DRS on 146, and also cabin 
C                  temperature.  Signal register now returned as a real
C                  number.
C
C                  1.03  04-01-06  D.A.TIDDEMAN
C                  Checks flags on cabin pressire constants before 
C                  processing.
C
********************************************************************************
      SUBROUTINE C_DRS(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.03'
      INTEGER*4 IRAW(64,512)      !Raw data array
      INTEGER*4 IFRQ(512)         !Raw data frequency
      REAL*4    RCONST(64)        !Constants array
      REAL*4    RDER(64,1024)     !Derived data array

      REAL*4    RVAL
      SAVE
C
C This routine uses parameters     2,3,4,27,207
C This routine produces parameters 515,641,644,660
C At sample rates of                 1,  2,  2,  1
C This routine uses two constants (1,2) for cabin .temp
C
      DATA LFSTCALL /.TRUE./           !True if first call to routine
      DATA LPSTMDNT /.FALSE./          !True after midnight crossover
      PARAMETER GMTH=2,GMTM=3,EVM=4,SREG=27,CABT=207
      PARAMETER SECSD=515,SREGD=641,EVMD=644,CABTD=660
C
C Event mark - The low 12 bits of parameter 4 are treated as a three digit
C BCD number and converted to a real number and put in the output array as
C parameter 644.  Output sample rate is 2 Hz.
C
      DO IS=1,MIN(IFRQ(EVM),2)
        J=IRAW(IS,EVM)
        IH=IBITS(J,8,4)                !Hundreds
        IT=IBITS(J,4,4)                !Tens
        IU=IBITS(J,0,4)                !Units
        IEVM=IH*100+IT*10+IU
        RDER(IS,EVMD)=FLOAT(IEVM)
        CALL ISETFLG(RDER(IS,EVMD),0)  !Apply flag
      END DO
C
C Signal register - The contents of parameter 27 are transferred directly to
C the derived data array without interpretation, as parameter 641.  This data
C will comprise the 8 DRS event bits.  Output sample rate is 2 Hz.
C
      DO IS=1,MIN(IFRQ(SREG),2)
        RDER(IS,SREGD)=FLOAT(IRAW(IS,SREG))
        CALL ISETFLG(RDER(IS,SREGD),0) !Apply flag
      END DO
C
C Cabin temperature - convert using temperature sensor calibration
C
      IF((ITSTFLG(RCONST(1)).EQ.0).AND.
     &     (ITSTFLG(RCONST(2)).EQ.0)) THEN
        DO IS=1,IFRQ(CABT)               !For each data sample
          RVAL=FLOAT(IRAW(IS,CABT))
          RDER(IS,CABTD)=RCONST(2)*RVAL+RCONST(1) !Cal cabin temp
          CALL ISETFLG(RDER(IS,CABTD),0) !Apply flag
        END DO                           !Next sample
      ENDIF
C
C Time - The time as seconds past midnight is calculated from parameters
C 2 and 3.  The values when this routine was last called is stored, and any
C negative change in time is taken and noted as a midnight crossover. 
C Thereafter 24*3600 secs are added to all times calculated.  The calculated
C time is returned as a real number in parameter 515 with a sample rate of
C 1 Hz.  If there are any flags set on parameters 2 or 3 then the routine
C takes no action and returns without altering parameter 515, which will 
C therefore indicate an invalid value.
C
C The time data in parameter 2 and 3 is stored as BCD numbers with
C 10s of hours, 1s of hours and 10s of mins in parameter 2 and
C 1s of mins, 10s of secs and 1s of secs in parameter 3
C
      J1=JZEXT(IRAW(1,GMTH))           !Get hours and mins
      J2=JZEXT(IRAW(1,GMTM))           !Get mins and secs
      JHRS=IBITS(J1,8,4)*10+IBITS(J1,4,4) !Compute hours
      JMIN=IBITS(J1,0,4)*10+IBITS(J2,8,4) !Compute minute
      JSEC=IBITS(J2,4,4)*10+IBITS(J2,0,4) !Compute seconds
      ISEC=JHRS*3600+JMIN*60+JSEC      !Compute full time
      IF(LFSTCALL) THEN                !Cater for first call to routine
        ILSTTIM=ISEC-1
        LFSTCALL=.FALSE.
      END IF
      IF(ISEC.LT.ILSTTIM) LPSTMDNT=.TRUE. !Identify midnight crossover
      IF(LPSTMDNT) ISEC=ISEC+86400     !Add value if past midnight
      RDER(1,SECSD)=FLOAT(ISEC)        !Store value
      CALL ISETFLG(RDER(1,SECSD),0)    !Set flags to good
      ILSTTIM=ISEC
      RETURN
      END
