!
! ROUTINE          C_RVSM SUBROUTINE FORTVAX
!     
! PURPOSE          Computes static pressure, pitot-static pressure, and pressure
!                  height from the 146 RVSM altitude and airspeed data.
!
! DESCRIPTION      RVSM altitude is available in ARINC-429 message 203 and is
!                  recorded as by the DRS as a 16 bit signed word, with the 
!                  LSB representing 4 feet.
!
!                  RVSM computed airspeed is available in ARINC-429 message
!                  206 and is recorded by the DRS as a 16 bit signed word, with
!                  the LSB representing 1/32 kt, but always zero.
!
!                  These values should be within the system accuracy
!                  specification and do not require calibration.
!
!                  Note that altitude is updated by the RVSM at about 20 Hz
!                  and airspeed is updated at about 10 Hz.  Both signals are
!                  sampled by the DRS at 32 Hz so there will be multiple
!                  values and aliasing effects.
!
! METHOD           For each DRS parameter to be calibrated:
!                  1. If data is FFFF or FFFE or out of range then flag 3
!                  2. Decode the altitude and use the tables in NASA TN D-822
!                     to back compute the static pressure.
!                  3. Decode the airspeed and use fundamental equations to 
!                     compute pitot-static pressure.
!                  4. Check the results for being within acceptable values.
!                  5. Set data flag bits (16+17) 0: Good data
!                                                1: Data of lower quality
!                                                2: Probably faulty, exceed lims
!                                                3: Data absent or invalid.
!
!                  Flagging - If a value can't be computed, due to missing data
!                  missing constants, divide be zeroes, etc, a value of 0 is
!                  used, flagged with a three.  If a value is outside its 
!                  limits for range, it is flagged with a two.
!                  If there are no problems with the data it is flagged with 0.
!                  Any flags on input data are propagated through subsequent 
!                  calculations.
!
!                  Note that routine does not currently apply position error
!                  corrections, nor interpolate missing data.
!
! VERSION          1.00  23/07/03  W.D.N.JACKSON
!
! ARGUMENTS        Inputs:
!                    DRS para 222 RVAL 32 Hz RVSM altitude
!                        para 223 RVAS 32 Hz RVSM computed airspeed
!
!                  Outputs:
!                    Derived para 576 SPR  mb 32 Hz Static pressure
!                            para 577 PSP  mb 32 Hz Pitot-static pressure
!                            para 578 PHGT m  32 Hz Pressure height
!
!                  Flags:
!                    Missing/corrupt data output as 0 flagged 3.
!                    Out of range derived data flagged 2.
!
! SUBPROGRAMS      S_PSP, ALT2PRESS, ISETFLG 
!
! REFERENCES       NASA Technical Note D-822, Aug 1961, Tables of airspeed,
!                  altitude, and mach number.
!
!                  Interface Control Document, Air Data Display Unit, ISS
!                  1G-80130-22.
!
! CHANGES          V1.00 23/07/03  WDNJ Original version
!                  V1.01 23/10/03  WDNJ Now replicates data when missing
!                  V1.02 11/12/03  WDNJ Fixes bug if initial data missing
!                  V1.03 11/03/04  DAT Flags data outside altitude range 3
!                  V1.04 17/03/04  WDNJ Now handles negative heights correctly
!                                       and uses more accurate flagging criteria
!
!*******************************************************************************
      SUBROUTINE C_RVSM(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.04'
!
      INTEGER*4 IRAW(64,512),IFRQ(512),IS,IVAL,IFLG1,IFLG2,ILSTVAL1,
     &    ILSTVAL2
      REAL*4    RCONST(64),RDER(64,1024),RVAL,RALT,RCAS,RPSP,RSTP
      DATA      ILSTVAL1 /'FFFE'X/, ILSTVAL2 /'FFFE'X/
!
      PARAMETER RSTPMX=1050.           !Max static pressure (mb)
      PARAMETER RSTPMN=116.            !Min static pressure (mb)
      PARAMETER RPSPMX=159.            !Max Pitot-static press (mb) 305 kts at SL!
      PARAMETER RPSPMN=0.              !Min Pitot-static pressure (mb)
      SAVE ILSTVAL1,ILSTVAL2
!
! Derive static pressure, pressure height, and pitot-static.
!
      DO IS=1,32
        IFLG1=0
        RDER(IS,576)=0.
        RDER(IS,578)=0.
! Process height and pressure
        IVAL=IRAW(IS,222)
        IF((IVAL.AND.'FFFF'X).EQ.'FFFE'X) IVAL=ILSTVAL1
        ILSTVAL1=IVAL
        IF(((IVAL.AND.'FFFF'X).EQ.'FFFF'X.AND.IRAW(IS,223).EQ.'FFFF'X)
     &      .OR.(IVAL.AND.'FFFF'X).EQ.'FFFE'X) IFLG1=3
        IF(BTEST(IVAL,15)) IVAL=IVAL.OR.'FFFF0000'X !Extend sign
        IF(IVAL.LT.-250.OR.IVAL.GT.12500) IFLG1=3 !Outside table range        
        IF(IFLG1.NE.3) THEN
          RVAL=FLOAT(IVAL)*4.          !Altitude in feet
          RALT=RVAL*0.3048             !Altitude in m
          CALL ALT2PRESS(RVAL,RSTP)    !Compute static pressure in mb
          IF(RSTP.LT.RSTPMN.OR.RSTP.GT.RSTPMX) IFLG1=2        
          RDER(IS,576)=RSTP
          RDER(IS,578)=RALT
        END IF
        CALL ISETFLG(RDER(IS,576),IFLG1)
        CALL ISETFLG(RDER(IS,578),IFLG1)
! Process airspeed
        IFLG2=0
        RDER(IS,577)=0.
        IVAL=IRAW(IS,223)
        IF((IVAL.AND.'FFFF'X).EQ.'FFFE'X) IVAL=ILSTVAL2 !No Arinc 429 signal
        ILSTVAL2=IVAL
        IF((IVAL.AND.'FFFF'X).EQ.'FFFF'X.OR.
     &  (IVAL.AND.'FFFF'X).EQ.'FFFE'X) IFLG2=3
        IF(BTEST(IVAL,15)) IVAL=IVAL.OR.'FFFF0000'X !Extend sign
        IF(IVAL.LT.0) IFLG2=3          !Should always be +ve
        IF(IVAL/32.GT.350) IFLG2=3     !Gross error (max 146 IAS is 305 kts)
        IF(IFLG1.NE.3.AND.IFLG2.NE.3) THEN
          IVAL=IVAL.AND.'FFFFFFF7'X    !Clear padding in LSB
          RCAS=FLOAT(IVAL)/32.         !computed airspeed in kt
          RCAS=RCAS*0.514444           !computed airspeed in m/s
          CALL S_PSP(RCAS,RSTP,RPSP)   !Compute pitot-static pressure in mb
          IF(RPSP.LT.RPSPMN.OR.RPSP.GT.RPSPMX) IFLG2=2        
          RDER(IS,577)=RPSP
        END IF
        CALL ISETFLG(RDER(IS,577),MAX(IFLG1,IFLG2))
      END DO
! 
      RETURN
      END
!*******************************************************************************
      SUBROUTINE S_PSP(RCAS,RSTP,RPSP)
CDEC$ IDENT 'V1.00'
!
! Computes pitot-static pressure from indicated (computed) airspeed and static 
! pressure from the following equations (see S_MACH and C_AIRSPD modules):
!
! IAS = 340.294 * Mach * SQRT(Static/1013.25)
! Mach= SQRT(5*((1+Pitot/Static)**(2/7)-1))
!
! where 340.294 is the speed of sound in m/s.
!
! RCAS - Computed airspeed (m/s)
! RSTP - Static pressure (mb)
! RPSP - Pitot-static pressure (mb)
!
      REAL*4 RCAS,RSTP,RPSP,RMACH

      RMACH=RCAS/340.294/SQRT(RSTP/1013.25)
      RPSP=RSTP*((((RMACH**2.)/5.+1.)**3.5)-1.)
      RETURN
      END
!*******************************************************************************
      SUBROUTINE ALT2PRESS(RALT,RPRESS)
CDEC$ IDENT 'V1.00'
!
! Converts altitudes in feet to pressures in mb using the tables provided in
! NASA Technical Note D-822 (Tables of airspeed, altitude and mach number based 
! on latest international values for atmospheric properties and physical
! constants. Sadie P Livingston and William Gracey. August 1961).  If altitude
! is outside the range -1000 to 50000 ft then returns a pressure of 0 mb.
!
! This routine is provided to convert the altitudes provided by the 146 RVSM
! system (Innovative Solutions & Support Inc Air Data Display Unit) into 
! static pressures, using the same standard tables as are used by the RVSM 
! system to convert pressure into altitude.
!
! Pressure values in the NASA tables are given in lb/sq ft.  These have been
! converted to mb using 1 lb/sq in = 68.9476258 mb and 144 sq in = 1 sq ft.
!
! Only simple linear interpolation is used between the tabulated values. This
! will give maximum error of 0.005 mb which is well below the recorded
! resolution, let alone the system accuracy.
!
! V1.00  14/05/03  W.D.N.Jackson
!
      REAL*4 RALT,RPRESS,RTABLE(2,442)
      INTEGER*4 IL,IP,IH

      DATA RTABLE(1:2,1:88) / !Heights (ft) and pressures (mb)
     & -1000.0,1050.408,
     &  -900.0,1046.644,
     &  -800.0,1042.890,
     &  -700.0,1039.146,
     &  -600.0,1035.416,
     &  -500.0,1031.691,
     &  -400.0,1027.985,
     &  -300.0,1024.284,
     &  -200.0,1020.597,
     &  -100.0,1016.915,
     &     0.0,1013.252,
     &   100.0,1009.594,
     &   200.0,1005.951,
     &   300.0,1002.312,
     &   400.0,998.6872,
     &   500.0,995.0770,
     &   600.0,991.4716,
     &   700.0,987.8806,
     &   800.0,984.2991,
     &   900.0,980.7273,
     &  1000.0,977.1649,
     &  1100.0,973.6171,
     &  1200.0,970.0739,
     &  1300.0,966.5452,
     &  1400.0,963.0259,
     &  1500.0,959.5163,
     &  1600.0,956.0211,
     &  1700.0,952.5306,
     &  1800.0,949.0544,
     &  1900.0,945.5880,
     &  2000.0,942.1310,
     &  2100.0,938.6836,
     &  2200.0,935.2458,
     &  2300.0,931.8176,
     &  2400.0,928.4037,
     &  2500.0,924.9947,
     &  2600.0,921.5999,
     &  2700.0,918.2147,
     &  2800.0,914.8392,
     &  2900.0,911.4732,
     &  3000.0,908.1168,
     &  3100.0,904.7700,
     &  3200.0,901.4328,
     &  3300.0,898.1098,
     &  3400.0,894.7917,
     &  3500.0,891.4880,
     &  3600.0,888.1891,
     &  3700.0,884.9045,
     &  3800.0,881.6294,
     &  3900.0,878.3640,
     &  4000.0,875.1033,
     &  4100.0,871.8571,
     &  4200.0,868.6204,
     &  4300.0,865.3932,
     &  4400.0,862.1757,
     &  4500.0,858.9677,
     &  4600.0,855.7693,
     &  4700.0,852.5804,
     &  4800.0,849.4012,
     &  4900.0,846.2316,
     &  5000.0,843.0715,
     &  5100.0,839.9209,
     &  5200.0,836.7800,
     &  5300.0,833.6486,
     &  5400.0,830.5268,
     &  5500.0,827.4146,
     &  5600.0,824.3120,
     &  5700.0,821.2189,
     &  5800.0,818.1354,
     &  5900.0,815.0615,
     &  6000.0,811.9971,
     &  6100.0,808.9376,
     &  6200.0,805.8924,
     &  6300.0,802.8568,
     &  6400.0,799.8259,
     &  6500.0,796.8095,
     &  6600.0,793.7979,
     &  6700.0,790.8005,
     &  6800.0,787.8080,
     &  6900.0,784.8251,
     &  7000.0,781.8517,
     &  7100.0,778.8879,
     &  7200.0,775.9337,
     &  7300.0,772.9891,
     &  7400.0,770.0540,
     &  7500.0,767.1238,
     &  7600.0,764.2078,
     &  7700.0,761.2966/
      DATA RTABLE(1:2,89:176) / !Heights (ft) and pressures (mb)
     &  7800.0,758.3951,
     &  7900.0,755.5032,
     &  8000.0,752.6208,
     &  8100.0,749.7479,
     &  8200.0,746.8847,
     &  8300.0,744.0263,
     &  8400.0,741.1822,
     &  8500.0,738.3429,
     &  8600.0,735.5132,
     &  8700.0,732.6930,
     &  8800.0,729.8824,
     &  8900.0,727.0767,
     &  9000.0,724.2852,
     &  9100.0,721.4986,
     &  9200.0,718.7215,
     &  9300.0,715.9540,
     &  9400.0,713.1913,
     &  9500.0,710.4431,
     &  9600.0,707.6995,
     &  9700.0,704.9655,
     &  9800.0,702.2411,
     &  9900.0,699.5215,
     & 10000.0,696.8162,
     & 10100.0,694.1158,
     & 10200.0,691.4250,
     & 10300.0,688.7437,
     & 10400.0,686.0671,
     & 10500.0,683.4003,
     & 10600.0,680.7429,
     & 10700.0,678.0951,
     & 10800.0,675.4521,
     & 10900.0,672.8187,
     & 11000.0,670.1948,
     & 11100.0,667.5806,
     & 11200.0,664.9711,
     & 11300.0,662.3712,
     & 11400.0,659.7809,
     & 11500.0,657.2001,
     & 11600.0,654.6241,
     & 11700.0,652.0578,
     & 11800.0,649.5010,
     & 11900.0,646.9490,
     & 12000.0,644.4065,
     & 12100.0,641.8737,
     & 12200.0,639.3456,
     & 12300.0,636.8271,
     & 12400.0,634.3181,
     & 12500.0,631.8188,
     & 12600.0,629.3242,
     & 12700.0,626.8392,
     & 12800.0,624.3591,
     & 12900.0,621.8884,
     & 13000.0,619.4274,
     & 13100.0,616.9711,
     & 13200.0,614.5244,
     & 13300.0,612.0873,
     & 13400.0,609.6599,
     & 13500.0,607.2322,
     & 13600.0,604.8191,
     & 13700.0,602.4108,
     & 13800.0,600.0120,
     & 13900.0,597.6227,
     & 14000.0,595.2383,
     & 14100.0,592.8586,
     & 14200.0,590.4933,
     & 14300.0,588.1328,
     & 14400.0,585.7771,
     & 14500.0,583.4310,
     & 14600.0,581.0944,
     & 14700.0,578.7626,
     & 14800.0,576.4405,
     & 14900.0,574.1278,
     & 15000.0,571.8200,
     & 15100.0,569.5169,
     & 15200.0,567.2235,
     & 15300.0,564.9396,
     & 15400.0,562.6605,
     & 15500.0,560.3910,
     & 15600.0,558.1262,
     & 15700.0,555.8710,
     & 15800.0,553.6255,
     & 15900.0,551.3846,
     & 16000.0,549.1487,
     & 16100.0,546.9222,
     & 16200.0,544.7054,
     & 16300.0,542.4933,
     & 16400.0,540.2908,
     & 16500.0,538.0931/
      DATA RTABLE(1:2,177:275) / !Heights (ft) and pressures (mb)
     & 16600.0,535.9050,
     & 16700.0,533.7216,
     & 16800.0,531.5431,
     & 16900.0,529.3789,
     & 17000.0,527.2147,
     & 17100.0,525.0601,
     & 17200.0,522.9150,
     & 17300.0,520.7748,
     & 17400.0,518.6441,
     & 17500.0,516.5183,
     & 17600.0,514.4019,
     & 17700.0,512.2904,
     & 17800.0,510.1837,
     & 17900.0,508.0865,
     & 18000.0,505.9990,
     & 18100.0,503.9113,
     & 18200.0,501.8382,
     & 18300.0,499.7697,
     & 18400.0,497.7061,
     & 18500.0,495.6472,
     & 18600.0,493.6028,
     & 18700.0,491.5583,
     & 18800.0,489.5233,
     & 18900.0,487.4980,
     & 19000.0,485.4727,
     & 19100.0,483.4617,
     & 19200.0,481.4507,
     & 19300.0,479.4493,
     & 19400.0,477.4565,
     & 19500.0,475.4686,
     & 19600.0,473.4877,
     & 19700.0,471.5137,
     & 19800.0,469.5462,
     & 19900.0,467.5851,
     & 20000.0,465.6311,
     & 20100.0,463.6833,
     & 20200.0,461.7422,
     & 20300.0,459.8079,
     & 20400.0,457.8802,
     & 20500.0,455.9588,
     & 20600.0,454.0440,
     & 20700.0,452.1356,
     & 20800.0,450.2337,
     & 20900.0,448.3387,
     & 21000.0,446.4498,
     & 21100.0,444.5671,
     & 21200.0,442.6911,
     & 21300.0,440.8219,
     & 21400.0,438.9584,
     & 21500.0,437.1021,
     & 21600.0,435.2515,
     & 21700.0,433.4076,
     & 21800.0,431.5695,
     & 21900.0,429.7386,
     & 22000.0,427.9134,
     & 22100.0,426.0944,
     & 22200.0,424.2816,
     & 22300.0,422.4756,
     & 22400.0,420.6753,
     & 22500.0,418.8817,
     & 22600.0,417.0939,
     & 22700.0,415.3127,
     & 22800.0,413.5373,
     & 22900.0,411.7682,
     & 23000.0,410.0052,
     & 23100.0,408.2480,
     & 23200.0,406.4970,
     & 23300.0,404.7527,
     & 23400.0,403.0137,
     & 23500.0,401.2814,
     & 23600.0,399.5548,
     & 23700.0,397.8340,
     & 23800.0,396.1194,
     & 23900.0,394.4110,
     & 24000.0,392.7084,
     & 24100.0,391.0121,
     & 24200.0,389.3214,
     & 24300.0,387.6364,
     & 24400.0,385.9578,
     & 24500.0,384.2848,
     & 24600.0,382.6176,
     & 24700.0,380.9562,
     & 24800.0,379.3010,
     & 24900.0,377.6515,
     & 25000.0,376.0078,
     & 25100.0,374.3698,
     & 25200.0,372.7376,
     & 25300.0,371.1111,
     & 25400.0,369.4908,
     & 25500.0,367.8758,
     & 25600.0,366.2665,
     & 25700.0,364.6630,
     & 25800.0,363.0652,
     & 25900.0,361.4733,
     & 26000.0,359.8865,
     & 26100.0,358.3059,
     & 26200.0,356.7307,
     & 26300.0,355.1612,
     & 26400.0,353.5970/
      DATA RTABLE(1:2,276:363) / !Heights (ft) and pressures (mb)
     & 26500.0,352.0389,
     & 26600.0,350.4862,
     & 26700.0,348.9387,
     & 26800.0,347.3969,
     & 26900.0,345.8609,
     & 27000.0,344.3302,
     & 27100.0,342.8047,
     & 27200.0,341.2850,
     & 27300.0,339.7705,
     & 27400.0,338.2618,
     & 27500.0,336.7584,
     & 27600.0,335.2607,
     & 27700.0,333.7678,
     & 27800.0,332.2806,
     & 27900.0,330.7987,
     & 28000.0,329.3221,
     & 28100.0,327.8512,
     & 28200.0,326.3856,
     & 28300.0,324.9248,
     & 28400.0,323.4697,
     & 28500.0,322.0199,
     & 28600.0,320.5753,
     & 28700.0,319.1356,
     & 28800.0,317.7015,
     & 28900.0,316.2723,
     & 29000.0,314.8488,
     & 29100.0,313.4301,
     & 29200.0,312.0167,
     & 29300.0,310.6086,
     & 29400.0,309.2057,
     & 29500.0,307.8076,
     & 29600.0,306.4147,
     & 29700.0,305.0272,
     & 29800.0,303.6449,
     & 29900.0,302.2673,
     & 30000.0,300.8947,
     & 30100.0,299.5272,
     & 30200.0,298.1649,
     & 30300.0,296.8076,
     & 30400.0,295.4554,
     & 30500.0,294.1081,
     & 30600.0,292.7655,
     & 30700.0,291.4282,
     & 30800.0,290.0957,
     & 30900.0,288.7684,
     & 31000.0,287.4455,
     & 31100.0,286.1279,
     & 31200.0,284.8155,
     & 31300.0,283.5074,
     & 31400.0,282.2045,
     & 31500.0,280.9060,
     & 31600.0,279.6128,
     & 31700.0,278.3243,
     & 31800.0,277.0406,
     & 31900.0,275.7618,
     & 32000.0,274.4877,
     & 32100.0,273.2184,
     & 32200.0,271.9539,
     & 32300.0,270.6936,
     & 32400.0,269.4387,
     & 32500.0,268.1885,
     & 32600.0,266.9427,
     & 32700.0,265.7017,
     & 32800.0,264.4654,
     & 32900.0,263.2339,
     & 33000.0,262.0067,
     & 33100.0,260.7843,
     & 33200.0,259.5663,
     & 33300.0,258.3534,
     & 33400.0,257.1449,
     & 33500.0,255.9408,
     & 33600.0,254.7414,
     & 33700.0,253.5468,
     & 33800.0,252.3564,
     & 33900.0,251.1705,
     & 34000.0,249.9892,
     & 34100.0,248.8124,
     & 34200.0,247.6402,
     & 34300.0,246.4724,
     & 34400.0,245.3089,
     & 34500.0,244.1502,
     & 34600.0,242.9958,
     & 34700.0,241.8458,
     & 34800.0,240.7000,
     & 34900.0,239.5590,
     & 35000.0,238.4223,
     & 35100.0,237.2895,
     & 35200.0,236.1614/
      DATA RTABLE(1:2,364:442) / !Heights (ft) and pressures (mb)
     & 35300.0,235.0381,
     & 35400.0,233.9187,
     & 35500.0,232.8036,
     & 35600.0,231.6927,
     & 35700.0,230.5862,
     & 35800.0,229.4840,
     & 35900.0,228.3861,
     & 36000.0,227.2925,
     & 36100.0,226.2028,
     & 36200.0,225.1183,
     & 36400.0,222.9646,
     & 36600.0,220.8316,
     & 36800.0,218.7191,
     & 37000.0,216.6267,
     & 37200.0,214.5545,
     & 37400.0,212.5018,
     & 37600.0,210.4688,
     & 37800.0,208.4555,
     & 38000.0,206.4613,
     & 38200.0,204.4857,
     & 38400.0,202.5298,
     & 38600.0,200.5921,
     & 38800.0,198.6731,
     & 39000.0,196.7727,
     & 39200.0,194.8900,
     & 39400.0,193.0256,
     & 39600.0,191.1793,
     & 39800.0,189.3503,
     & 40000.0,187.5385,
     & 40200.0,185.7444,
     & 40400.0,183.9676,
     & 40600.0,182.2075,
     & 40800.0,180.4647,
     & 41000.0,178.7381,
     & 41200.0,177.0283,
     & 41400.0,175.3348,
     & 41600.0,173.6570,
     & 41800.0,171.9961,
     & 42000.0,170.3504,
     & 42200.0,168.7211,
     & 42400.0,167.1070,
     & 42600.0,165.5083,
     & 42800.0,163.9249,
     & 43000.0,162.3563,
     & 43200.0,160.8036,
     & 43400.0,159.2652,
     & 43600.0,157.7416,
     & 43800.0,156.2325,
     & 44000.0,154.7376,
     & 44200.0,153.2572,
     & 44400.0,151.7911,
     & 44600.0,150.3393,
     & 44800.0,148.9010,
     & 45000.0,147.4766,
     & 45200.0,146.0655,
     & 45400.0,144.6679,
     & 45600.0,143.2842,
     & 45800.0,141.9134,
     & 46000.0,140.5560,
     & 46200.0,139.2110,
     & 46400.0,137.8795,
     & 46600.0,136.5603,
     & 46800.0,135.2542,
     & 47000.0,133.9600,
     & 47200.0,132.6787,
     & 47400.0,131.4094,
     & 47600.0,130.1520,
     & 47800.0,128.9072,
     & 48000.0,127.6738,
     & 48200.0,126.4523,
     & 48400.0,125.2424,
     & 48600.0,124.0444,
     & 48800.0,122.8580,
     & 49000.0,121.6825,
     & 49200.0,120.5185,
     & 49400.0,119.3656,
     & 49600.0,118.2236,
     & 49800.0,117.0922,
     & 50000.0,115.9723/

      RPRESS=0.
      IF(RALT.LT.-1000..OR.RALT.GE.50000.) RETURN
      IL=1                             !Find nearest two altitudes
      IH=442
      DO WHILE(IL+1.NE.IH)
        IP=(IH+IL)/2
        IF(RALT.LT.RTABLE(1,IP)) IH=IP
        IF(RALT.GE.RTABLE(1,IP)) IL=IP
      END DO
      RPRESS=RTABLE(2,IL)+(RTABLE(2,IH)-RTABLE(2,IL)) !Linear interpolation
     &    *(RALT-RTABLE(1,IL))/(RTABLE(1,IH)-RTABLE(1,IL))
      RETURN
      END
