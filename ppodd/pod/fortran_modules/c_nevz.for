C
C ROUTINE          C_NEVZ SUBROUTINE FORTVAX
C     
C PURPOSE          Produces calibrated Nevzorov parameters
C
C DESCRIPTION      Calculates liquid and total water values for the Nevzorov
C                  together with reference and collector voltages, using the
C                  equations supplied with the unit, namely:
C
C                  Water content = V**2/U/L/SR
C
C                  where V is the output voltage (V)
C                        U is the True air speed (m/s)
C                        L is the energy expended in heating and evaporating
C                             the water, for which a value of 2589 J/g is used
C                        SR is the product of the sensor area and the resistanc
C                           of the collector sensor at the chosen temperature.
C
C                  Flagging:
C
C                  If on Calibrate, as indicated by bit 1 or 2 in the signal
C                  register being set then the data is flagged with a 2.
C                  Otherwise the data carries the flag of the True Airspeed
C                  parameter.
C
C VERSION          1.00  18/01/99  W.D.N.JACKSON
C
C ARGUMENTS     
C                  Constants:
C                  RCONST(1)   CALNVLW X0
C                  RCONST(2)   CALNVLW X1
C                  RCONST(3)   CALNVLR X0
C                  RCONST(4)   CALNVLR X1
C                  RCONST(5)   CALNVLC X0
C                  RCONST(6)   CALNVLC X1
C                  RCONST(7)   CALNVTW X0
C                  RCONST(8)   CALNVTW X1
C                  RCONST(9)   CALNVTR X0
C                  RCONST(10)  CALNVTR X1
C                  RCONST(11)  CALNVTC X0
C                  RCONST(12)  CALNVTC X1
C                  RCONST(13)  CALRSL X0     RS Value at T0
C                  RCONST(14)  CALRSL T0
C                  RCONST(15)  CALRST X0     RS value at T0
C                  RCONST(16)  CALRST T0
C
C                  Inputs: 
C                  NVLW Nevzorov Liquid Water                   Para 208  8 Hz
C                  NVLR Nevzorov Liquid Reference               Para 209  8 Hz
C                  NVLC Nevzorov Liquid Collector               Para 210  8 Hz
C                  NVTW Nevzorov Total Water                    Para 211  8 Hz
C                  NVTR Nevzorov Total Reference                Para 212  8 Hz
C                  NVTC Nevzorov Total Collector                Para 213  8 Hz
C                  SREG Signal register [bits 1-2]              Para  27  2 Hz
C                  TAS  True airspeed                           Para 517 32 Hz
C
C                  Outputs: 
C                  NVLW Nevzorov Liquid Water     [gm-3]         Para 602 8 Hz
C                  NVLR Nevzorov Liquid Reference [V]            Para 603 8 Hz
C                  NVLC Nevzorov Liquid Collector [V]            Para 604 8 Hz
C                  NVTW Nevzorov Total Water      [gm-3]         Para 605 8 Hz
C                  NVTR Nevzorov Total Reference  [V]            Para 606 8 Hz
C                  NVTC Nevzorov Total Collector  [V]            Para 607 8 Hz
C                
C SUBPROGRAMS      ITSTFLG          Examines bits 16,17 for flags
C                  ISETFLG          Sets flag bits 16,17 = 0 --> 3
C
C REFERENCES       
C
C CHANGES           V1.01  27/09/02  W.D.N.JACKSON
C                   Changed to include handling of 16 bit data from the new 
C                   DRS.
C                   V1.02  13/11/06
C                   Signal register bits 1 and 2 swapped to be the correct way
C                   round 1=TW, 2=LW
C                   V1.03  12/12/06
C                   Voltage flags explicitly set to zero
C
C*******************************************************************************
      SUBROUTINE C_NEVZ(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.03'
      INTEGER*4 IRAW(64,512),IFRQ(512)
      REAL*4    RCONST(64),RDER(64,1024)

      RERR=0.                          !Default error return
      CALL ISETFLG(RERR,3)             ! is 0 flagged 3
      DO I=1,8                         !For each sample
        RTAS=RDER((I-1)*4+1,517)
        ITASFLG=ITSTFLG(RTAS)          !Note TAS flag
        CALL ISETFLG(RTAS,0)           !Clear TAS flag
        IF(RTAS.LE.0) ITASFLG=3        !Invalid TAS
        RDER(I,602)=RERR               !Default return
        RV=RCONST(1)+RCONST(2)*IRAW(I,208) !Compute voltage
        IF(ITASFLG.LT.3) THEN          !If TAS valid
          RDER(I,602)=RV*RV/RTAS/2589/RCONST(13) !Compute liquid water
          CALL ISETFLG(RDER(I,602),0)
          IF(.NOT.BTEST(IRAW(1,27),2)) CALL ISETFLG(RDER(I,602),2) !Flag 2 if on cal
        END IF
        RDER(I,603)=RCONST(3)+RCONST(4)*IRAW(I,209) !Calibrate voltages
        RDER(I,604)=RCONST(5)+RCONST(6)*IRAW(I,210)
	CALL ISETFLG(RDER(I,603),0)
	CALL ISETFLG(RDER(I,604),0)
        RDER(I,605)=RERR               !Repeat the processing for total water
        RV=RCONST(7)+RCONST(8)*IRAW(I,211)
        IF(ITASFLG.LT.3) THEN
          RDER(I,605)=RV*RV/RTAS/2589/RCONST(15)
          CALL ISETFLG(RDER(I,605),0)
          IF(.NOT.BTEST(IRAW(1,27),1)) CALL ISETFLG(RDER(I,605),2)
        END IF
        RDER(I,606)=RCONST(9)+RCONST(10)*IRAW(I,212)
        RDER(I,607)=RCONST(11)+RCONST(12)*IRAW(I,213)
	CALL ISETFLG(RDER(I,606),0)
	CALL ISETFLG(RDER(I,607),0)
      END DO
C
      RETURN
      END
