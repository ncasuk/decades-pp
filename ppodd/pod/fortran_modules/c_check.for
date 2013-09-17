C
C ROUTINE          C_CHECK SUBROUTINE FORTVAX
C
C PURPOSE          Lists the inputs and outputs to a module every call
C
C DESCRIPTION      Types out all the inputs to and all the outputs from
C                  a module in the CALIBRATION program that has been selected
C                  with the /CHECK command option.  This routine is executed
C                  once a second.
C
C VERSION          1.00  1-9-90  N.JACKSON
C
C ARGUMENTS        IM            I*4 IN The number of the module being checked
C                  IDRS(64,512)  I*4 IN The raw data array
C                  RDER(64,1024) I*4 IN The derived data array
C                  IMDINP(32,64) I*4 IN The input parameters(up to 32) for each modl
C                  IMDOUT(32,64) I*4 IN The output params (up to 32) for each module
C                  RCONST(64,64) R*4 IN The constants (up to 64) for each module
C                  INFREQ(512)   I*4 IN The frequency of each input parameter
C                  IOUTFRQ(1024) I*4 IN The frequency of each output parameter
C                  CMDNAME(64)   C*6 IN The name of each module
C
C CHANGES          1.01  13/05/93  W.D.N.JACKSON
C                  Now displays input data as 16 bit rather than 12.
C
********************************************************************************
      SUBROUTINE C_CHECK(IM,IDRS,RDER,IMDINP,IMDOUT,RCONST,INFREQ 
     &   ,IOUTFRQ,CMDNAME)
CDEC$ IDENT 'V1.01'
      INTEGER*4   IM              !The number of the module being checked
      INTEGER*4   IDRS(64,512)    !The raw data array
      REAL*4      RDER(64,1024)   !The derived data array
      INTEGER*4   IMDINP(32,64)   !The input parameters(up to 32) for each modl
      INTEGER*4   IMDOUT(32,64)   !The output params (up to 32) for each module
      REAL*4      RCONST(64,64)   !The constants (up to 64) for each module
      INTEGER*4   INFREQ(512)     !The frequency of each input parameter
      INTEGER*4   IOUTFRQ(1024)   !The frequency of each output parameter
      CHARACTER   CMDNAME(64)*6   !The name of each module
      INTEGER*4   ITSTFLG
      REAL*4      RQRQ

      CHARACTER   CTIM*8
      PARAMETER TT=6

      CALL C_SPMCTIM(NINT(RDER(1,515)),CTIM) !Get the time from para 515 as string
      WRITE(TT,*) CMDNAME(IM)//' for '//CTIM !Write module name and time
C
C Search backwards through constants for first valid one
C Message if none found
C Else write out each valid constant
C
      IC=64                            !Search backwards for constants
      DO WHILE(ITSTFLG(RCONST(IC,IM)).EQ.3.AND.IC.GE.1)
        IC=IC-1
      END DO
      IF(IC.EQ.0) THEN                 !Message if none found
        WRITE(TT,*) 'No constants'
      ELSE
        WRITE(TT,*) 'Constants:'       !Else print them
        DO I=1,IC
          WRITE(TT,10,IOSTAT=IOS)
     &    RCONST(I,IM),ITSTFLG(RCONST(I,IM))
        ENDDO
      END IF
C
C For each parameter in the input list for the module, list all values 
C together with the flag indicator value.
C
      II=1                             !Pointer in module list
      DO WHILE(IMDINP(II,IM).NE.0.AND.II.LE.32) !For each input param
        IP=IMDINP(II,IM)               !Parameter number
        WRITE(TT,*) 'Input parameter ',IP
        IF(IP.LE.512) THEN             !Write raw data as integers
           IF(INFREQ(IP).GT.0) THEN
            DO I=1,INFREQ(IP)
              WRITE(TT,11,IOSTAT=IOS)   
     &        IDRS(I,IP).AND.'FFFF'X 
     &        ,ITSTFLG(IDRS(I,IP))
            ENDDO
           END IF
        ELSE                           !Write derived data as real
          IF(IOUTFRQ(IP).GT.0) THEN
            DO I=1,IOUTFRQ(IP)
              RQRQ=RDER(I,IP)
              CALL ISETFLG(RQRQ,0)
              WRITE(TT,10,IOSTAT=IOS) 
     &        RQRQ,ITSTFLG(RDER(I,IP))
            END DO
          END IF
        END IF
        II=II+1
      END DO
C
C For each parameter in the output list for the module, list all values
C together with the flag indicator value.
C
      II=1                             !Pointer into module list
      DO WHILE(IMDOUT(II,IM).NE.0.AND.II.LE.32) !For each parameter
        IP=IMDOUT(II,IM)               !Parameter number
        WRITE(TT,*) 'Output parameter ',IP !Write out all values
        IF(IOUTFRQ(IP).GT.0) THEN
          DO I=1,IOUTFRQ(IP) 
              RQRQ=RDER(I,IP)
              CALL ISETFLG(RQRQ,0)
            WRITE(TT,10,IOSTAT=IOS)
     &      RQRQ,ITSTFLG(RDER(I,IP))
          ENDDO
        END IF 
        II=II+1
      END DO
      WRITE(TT,*) ' '                  !Blank line at end
      RETURN
10    FORMAT(5(1PE11.4E1,I2,2X))
11    FORMAT(8(I6,I2,2X))
      END
