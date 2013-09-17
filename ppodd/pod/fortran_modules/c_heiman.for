C-----------------------------------------------------------------------------
C 
C ROUTINE          C_HEIMAN   SUBROUTINE FORTVAX
C
C PURPOSE          To derive uncorrected Heimann temperatures
C
C DESCRIPTION      Converts rawdata input from the Heimann radiometer and
C                  black body source into uncorrected surface tempratures,
C                  parameter 537.
C                   
C                  The Heimmann is recorded by para 141, 
C                  the blackbody reference temperature by para 142,
C                  and bit 0 of the signal register (para 27) indicates whether
C                  the Heimann is set to calibrate.  
C
C ARGUMENTS        IRAW   input raw data
C                  IFRQ   raw data frequencies
C                  RCONST flight constants corresponding to PRTCCAL and HEIMCAL
C                  RDER   output data
C
C SUBPROGRAMS      
C
C REFERENCES
C
C VERSION          1.00 D.R.Lauchlan
C
C CHANGES          
C DESCRIPTION      Converts the two input parameters 141 (raw Heimann) and
C                  142 (black body reference temperature) into one, the
C                  uncorrected HEIMAN temp (para 537).
C
C                  The Heimann Radiometer data is converted using a quadratic
C                  fit :
C                  Surface temp =  RCONST(4) + RCONST(5)*x + RCONST(6)*x**2
C                  RCONST(4 to 6) correspond to the constants with the keyword
C                  HEIMCAL in the flight constants file.
C 
C                  The black body signal (para 142) is converted using
C                  a quadratic fit :
C              
C                   BB = a + b*x + c*x**2
C
C                  where constants a, b and c correspond to RCONST(1 to 3)
C                  from the keyword PRTCCAL in the flight constant file.
C
C                  Signal Register (para 27) bit 0 indicates the position of 
C                  the black body;  0 = b/b out of FoV, 1 = b/b in FoV. 
C
C                  If signal register bit 0 is set to 1 and black body
C                  reference temprature has been steady for the previous
C                  3 seconds (mean of each second differs by no more than
C                  0.1K), the b/b reference temperature is output.
C                  Otherwise the HEIMAN temprature is output. An offset
C                  is assigned accordingly:
C                                                para 27 
C                                                 bit 0
C                   233.26 to  313.06   Heimann-    0    (o/s = 0)
C                  1233.26 to 1313.06   Ref/BB -    0    (o/s = 1000)
C                  2233.26 to 2313.06   Heimann-    1    (o/s = 2000)
C                  3233.26 to 3313.06   Ref/BB -    1    (o/s = 2000 + 1000)
C
C                  (NOTE: an offset of 1000 is never assigned under 
C                         this scheme)
C
C
C                  Heimann data is output for the time that the
C                  reference temperature is output. This is done in
C                  4 second bursts imediately after the reference 
C                  sequence and overwrites the ramping sections.
C                  Non reference or dwelling calibration temeratures
C                  are flagged as 2.
C                  Dwell Heimann data is output for the corresponding
C                  calibration reference period after the instrument has 
C                  switched back to measure, ie para 27 bit 0 becomes 0. 
C
C                  Bits 16 and 17 of the output temperature RDER(x,537) are
C                  used to flag certain data conditions as follows :
C                    Bits 16 and 17 = 00  - Good data, MEASURE/HEIMANN
C                                   = 01  - Good data, but Heimann on CALIBRATE
C                                           and outputing DWELL temp
C                                           or looking at the Black Body temps
C                                           or BB moved out of field of view
C                                           and data are last Heimann dwell
C                                           data.
C                                   = 02  - Suspect or absent signal register 
C                                           data, non-reference calibration
C                                           temperatures and non-dwelling
C                                           calibration temperatures.
C                                   = 03  - Absent data, passed through from
C                                           IRAW(x,141)
C
C ARGUMENTS        IRAW(f,141)   Raw Heimann data
C                  IRAW(f,27)    Raw signal register data 
C                  IRAW(f,142)   Raw black body data
C                  IFRQ(141)     Recorded frequency of Heimann Radiometer    
C                  IFRQ(27)      Recorded frequency of signal register  
C                  IFRQ(142)     Recorded frequency of black body
C                  RCONST(1-6)   Constants for quadratic fit
C                  RDER(f,537)   Uncorrected Heimann temps (deg K) 
C
C SUBPROGRAMS      ITSTFLG  -  Returns the value of bits 16 & 17    - SCILIB
C                  ISETFLG  -  Sets the value of bits 16 & 17       - SCILIB
C                  IBITS    -  Extracts selected bits from input    - FORTRAN
C                  BTEST    -  Tests value of selected single bit   - FORTRAN
C         C_HEIMAN_LTST_BB  -  Checks array elemenets are within
C                              +/- 86                                - LOCAL
C
C REFERENCES       
C
C VERSION          1.00   09-11-94 D.R.Lauchlan
C                  Based on C_BARNES V2.00 by D.P. Briggs
C
C CHANGES          V1.01  10/02/99  W.D.N.JACKSON
C                  Bug in flag checking of raw data fixed.
C                  
C                  V1.02  27/09/02  W.D.N.JACKSON
C                  Changed to include handling of 16 bit data from the new 
C                  DRS.  Also now expects calibrator temp cal to be in deg C.
C
C                  V1.03 11/11/04 J.P. TAYLOR
C                  Changed to account for 16bit representation of DRS
C                  parameters.  Allowable range of BB ref means changed from
C                  +/- 6 to +/- 86 this is equivalent to 0.1K with the new
C                  DRS 16bit data stream.
C----------------------------------------------------------------------------
      SUBROUTINE C_HEIMAN(IRAW,IFRQ,RCONST,RDER)


CDEC$ IDENT 'V1.03'
C
      INTEGER*4 IRAW(64,512),IFRQ(512)
      REAL*4    RCONST(64),RDER(64,1024)
 
      REAL*4    R_DWELL(5,64)    !buffer for dwelling Heimann

      INTEGER   L_LASTREG,       !last signal register
     +          L_PRESREG,       !present signal register
     +          I_COUNT,         !loop counter
     +          I_COUNT_1,       !loop counter
     +          I_COUNT_2,       !loop counter
     +          I_COUNT_3,       !loop counter
     +          I_DWELL_COUNT    !Heimann dwell count
      INTEGER*4 I_BBREF_MEAN(3), !last three seconds of BB mean temps
     +          I_BB_F,          !black body frequency
     +          I_SIGREG_F,      !signal register frequency 
     +          IFLAG,           !output data flag
     +          ISIGFLAG         !signal register flag
C functions:
      INTEGER   C_HEIMAN_LTST_BB,ITSTFLG 
      DATA L_LASTREG/.FALSE./
      
      SAVE
c      REAL      C_HEIMAN_RBB_CONV
C
      I_BB_F      = IFRQ(142)
      I_SIGREG_F  = IFRQ(27)

      DO I_COUNT_1 = 1,I_BB_F
        IFLAG = 0
        I_COUNT_2 = (1 + I_COUNT_1) / I_SIGREG_F
C
C if signal register set
        L_PRESREG = BTEST(IRAW(I_COUNT_2,27),0)
        IF (L_PRESREG) THEN

C  if last signal register not set
          IF (.NOT.L_LASTREG)  THEN
C    init BB ref means
            DO I_COUNT = 1,3
              I_BBREF_MEAN(I_COUNT) = I_COUNT * 100
            ENDDO
C    init dwell counter & buffer
            DO I_DWELL_COUNT = 1,5
              DO I_COUNT = 1,I_BB_F
                 R_DWELL(I_DWELL_COUNT,I_COUNT) = 2000.0
              ENDDO    
            ENDDO    
            I_DWELL_COUNT = 0
          ENDIF

C  if last 3 BB ref means are within +/-86 (equivalent to approx 0.1K)
          IF (C_HEIMAN_LTST_BB(I_BBREF_MEAN)) THEN !output BB ref temp
            RDER(I_COUNT_1,537) = 3000 + 
     -           273.16 +
     -           RCONST(1) + 
     -           RCONST(2) * FLOAT(IRAW(I_COUNT_1,142)) + 
     -           RCONST(3) * FLOAT(IRAW(I_COUNT_1,142)) ** 2
C       set flag to 2 if data is out of expected range (-20 to +40°C)
            IF (RDER(I_COUNT_1,537) .GT. 3313.16 .OR.
     +          RDER(I_COUNT_1,537) .LT. 3253.16)  IFLAG = 2

C       store Heimann for corresponding seconds (max 5)
            IF (I_COUNT_1 .EQ. 1) THEN
              I_DWELL_COUNT = I_DWELL_COUNT + 1
              IF (I_DWELL_COUNT .GT. 5) THEN
                DO I_COUNT = 5,2,-1
                  DO I_COUNT_3 = 1,I_BB_F
                    R_DWELL(I_COUNT-1,I_COUNT_3) = 
     +                           R_DWELL(I_COUNT,I_COUNT_3)
                  ENDDO
                ENDDO
                I_DWELL_COUNT = 5
              ENDIF
            ENDIF
             
            R_DWELL(I_DWELL_COUNT,I_COUNT_1) = 2000 +
     -           273.16 +
     -           RCONST(4) + 
     -           RCONST(5) * FLOAT(IRAW(I_COUNT_1,141)) + 
     -           RCONST(6) * FLOAT(IRAW(I_COUNT_1,141)) ** 2
C          write(*,*)-999.999

          ELSE     !output HEIMANN temp
C        write dwell Heimann data if any
            IF (I_DWELL_COUNT.NE.0) THEN 
              IF (I_DWELL_COUNT.EQ.5) 
     +            I_DWELL_COUNT = 4
              RDER(I_COUNT_1,537) =  
     +           R_DWELL(I_DWELL_COUNT,I_COUNT_1)
              IF(I_COUNT_1 .EQ. I_BB_F)
     +           I_DWELL_COUNT = I_DWELL_COUNT - 1
            ELSE
              RDER(I_COUNT_1,537) =  2000 + 
     -           273.16 +
     -           RCONST(4) + 
     -           RCONST(5) * FLOAT(IRAW(I_COUNT_1,141)) + 
     -           RCONST(6) * FLOAT(IRAW(I_COUNT_1,141)) ** 2
              IF (IFLAG .LT. 2)
     +            IFLAG = 2
            ENDIF
          ENDIF

C    roll on BB means
          IF (I_COUNT_1 .EQ. 4) THEN
            DO I_COUNT = 2,1,-1
              I_BBREF_MEAN(I_COUNT+1) = I_BBREF_MEAN(I_COUNT)
            ENDDO

            I_BBREF_MEAN(1) = 0 
            DO I_COUNT = 1,IFRQ(142)
              I_BBREF_MEAN(1) = I_BBREF_MEAN(1) + 
     +            IRAW(I_COUNT,142)
            ENDDO
            I_BBREF_MEAN(1) = I_BBREF_MEAN(1) / I_BB_F
          ENDIF

        ELSE       !output Heimann temp
C        write dwell HEIMAN data if any
          IF (I_DWELL_COUNT.NE.0) THEN 
            IF (I_DWELL_COUNT.EQ.5) 
     +          I_DWELL_COUNT = 4
            RDER(I_COUNT_1,537) =  
     +         R_DWELL(I_DWELL_COUNT,I_COUNT_1)
            IF(I_COUNT_1 .EQ. I_BB_F)
     +         I_DWELL_COUNT = I_DWELL_COUNT - 1
          ELSE
            RDER(I_COUNT_1,537) = 
     -           273.16 +
     -           RCONST(4) + 
     -           RCONST(5) * FLOAT(IRAW(I_COUNT_1,141)) + 
     -           RCONST(6) * FLOAT(IRAW(I_COUNT_1,141)) ** 2

          ENDIF

        ENDIF

C     Get flag value
        ISIGFLAG =0
        IF(IRAW(I_COUNT_2,27).EQ.'FFFF'X) ISIGFLAG=3
        IF ((RDER(I_COUNT_1,537) .GE. 1000.0 .AND.
     +       IFLAG .EQ. 0) .OR.
     +       ISIGFLAG .EQ. 1) THEN
          IFLAG = 1 
        ELSE IF (ISIGFLAG .GT. IFLAG) THEN
          IFLAG = 2
        ENDIF
C     Set flag
        CALL ISETFLG(RDER(I_COUNT_1,537),IFLAG)
C
C    roll on sig reg
        L_LASTREG = L_PRESREG

      ENDDO
C
      RETURN
      END 
C-----------------------------------------------------------------------------
C
C ROUTINE          C_HEIMAN_LTST_BB    
C
C PURPOSE          Checks array elemenets are within +/- 86
C
C DESCRIPTION      Returns TRUE if the deviation from the mean value of the
C                  passed array is no greater than +/- 86 otherwise FALSE.
C
C VERSION          1.00   17-02-94  D.P.Briggs
C                  1.01   11-11-04  J.P.Taylor
C                  Array now allowed to be within +/- 86  (was +/- 6)
C                  Change due to new 16bit representation on  DRS
C                  86 is equivalent to 0.1K.
C                  
      INTEGER FUNCTION C_HEIMAN_LTST_BB(I_MEANS)  
CDEC$ IDENT 'V1.00'

      INTEGER    I_COUNT
      INTEGER*4  I_MEANS(3), I_MEAN
 
      C_HEIMAN_LTST_BB = .TRUE.

      I_MEAN = (I_MEANS(1) + I_MEANS(2) + I_MEANS(3)) / 3

      DO I_COUNT = 1 , 3 
        IF ( ABS(I_MEAN - I_MEANS(I_COUNT)) .GT. 86) 
     +     C_HEIMAN_LTST_BB = .FALSE.
      ENDDO

      RETURN
      END 
