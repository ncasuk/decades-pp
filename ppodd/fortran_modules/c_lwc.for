C
C ROUTINE	    C_LWC   SUBROUTINE FORT VAX   [C_LWC.FOR]
C
C PURPOSE	    To calibrate DRS parm 42 to tardis parm 535 (LWC)
C	
C DESCRIPTION 	    The Liquid Water Content (LWC) is a four hertz 
C		    parameter. It requires the True Air Speed (Parm 517), 
C		    True De_iced Temperature (parm 520) and Static 
C		    Pressure (parm 576). All these derived parameters 
C		    (517, 520, 576) are at 32 Hertz. So for each quarter 
C		    point of the LWC requires a sample of eight of 
C		    the derived paramters to be averaged. This is done using
C		    only good data points. If there are not eight samples but
C		    more than one then the flag for the derived LWC is set to 1.
C		    If the frequency of the DRS parm (42) is not equal to 4
C		    then no values are calculated and all four points of the
C		    LWC are set to -9999.0, with a flag of 3. If a point cannot
C		    be calculated then the value of it is set to -9999.0 with
C		    a flag value of 3. If the instrument is saturated then the
C		    flag value is 1. If the derived value for the LWC falls out
C		    of the bounds of -10 to 10 then the flag is set to 2.
C
C VERSION 	    1.02 17-01-96 D Lauchlan
C
C ARGUMENTS         IRAW(64,512) I*4  IN   Raw data for the parameters
C		    IFRQ(512)    I*4  IN   Frequencies of the data
C		    RCONST(64)   R*4  IN   Constants required by routine,(1-28)
C		    RDER(64,1024)R*4  OUT  Tardis parameters
C	
C COMMON	    None.
C                 
C SUBPROGRAMS	    ISETFLG (linked automatically)
C
C FILES		    None.
C	
C REFERENCES	    MRF2 Specification for Total Water Hygrometer 4 Dec 1989
C		    Ouldridge Feb 1982
C                   Johnson 1979
C
C CHANGES           110190 Documentational changes only         M.Glover  
C                   v 1.02 17-01-96 D Lauchlan
C                   Unused parameters removed
C
C                   V1.03  27/09/02  W.D.N.JACKSON
C                   Changed to include handling of 16 bit data from the new 
C                   DRS.
C###############################################################################

        SUBROUTINE C_LWC(IRAW, IFRQ, RCONST, RDER)
CDEC$ IDENT 'V1.03'
        INTEGER*4 IRAW(64,512), IFRQ(512)

	REAL*4 RCONST(64), RDER(64, 1024)
	
C	The frequencies of the derived parameters passed into this module
C	may change. That is why IFRQ_*** has been set up. Here is a table of
C	what values of it corresponds to what frequency;
C
C			Frq	IFRQ_***
C			 4	   0
C			16         1
C			32         7
C			64        15
C
	DATA IFRQ_TAS/7/, IFRQ_TDT/7/, IFRQ_PRESSURE/7/

C	Calibrate the Johnson_Williams Liquid Water Content Probe - DRS 
C	parameter 42, sample rate 4 Hz. This is to be put into g kg-1.
C	This uses the elements of RCONST from 1 to 2.

	IF (IFRQ(42).EQ.4) THEN  ! check the frequency.

		N_TAS=1
		N_TDT=1
		N_PRE=1

		DO IS=1, IFRQ(42)  ! for each sample
                        IRAW_FLAG=0
                        IF(IRAW(IS,42).EQ.0.OR.IRAW(IS,42)
     &                       .EQ.'FFFF'X) IRAW_FLAG=3
		       	ICHECK=1
		       	ICHECK_2=1
			TAS=0.0
			P=0.0
			TDT=0.0

C			See if all the const are there,if not set the flag to 3
		       	DO I=1,2,1
				IF (ITSTFLG(RCONST(I)).GT.2) THEN
					ICHECK=ICHECK+1
				END IF
                        END DO

                        SUM=0  ! Reset the sum.
			ICOUNT=0

C			Find the average of the TAS.
 			DO INC=N_TAS, N_TAS+IFRQ_TAS 
				IFLAG=ITSTFLG(RDER(INC, 517))
				IF (IFLAG.LT.1) THEN
		   			SUM=SUM+RDER(INC, 517)
					ICOUNT=ICOUNT+1
				END IF
   	 		END DO
			
C			Reset the starter for the do loop to be the old start 
C			point plus the incremental for the TAS frequency plus 
C			one.
			N_TAS=N_TAS+IFRQ_TAS+1

			IF (ICOUNT.EQ.0) THEN
C				No good points.
				ICHECK=-9999
			ELSE
	                        IF (ICOUNT.NE.IFRQ_TAS+1) THEN
					ICHECK_2=99
				END IF
	       			TAS=SUM/FLOAT(ICOUNT)
			END IF
 
                        SUM=0.0  ! Reset the sum.
			ICOUNT=0
                        
C			Find the average of the true de_iced temp.
                        DO INC=N_TDT, N_TDT+IFRQ_TDT
				IFLAG=ITSTFLG(RDER(INC, 520))
				IF (IFLAG.LT.1) THEN
			       		SUM=SUM+RDER(INC, 520)
					ICOUNT=ICOUNT+1
				END IF
			END DO

			N_TDT=N_TDT+IFRQ_TDT+1
                        IF (ICOUNT.EQ.0) THEN
				ICHECK=-9999
			ELSE
	                        IF (ICOUNT.NE.IFRQ_TDT+1) THEN
					ICHECK_2=99
				END IF
	       			TDT=SUM/FLOAT(ICOUNT)
			END IF

                        SUM=0  ! Reset the sum.
			ICOUNT=0

C			Find the static pressure average.
			DO INC=N_PRE, N_PRE+IFRQ_PRESSURE
				IFLAG=ITSTFLG(RDER(INC, 576))

C				Only use good data, namley of flag zero.
				IF (IFLAG.LT.1) THEN       
					SUM=SUM+RDER(INC, 576)     
					ICOUNT=ICOUNT+1    
				END IF                     
			END DO
		

			N_PRE=N_PRE+IFRQ_PRESSURE+1

			IF (ICOUNT.EQ.0) THEN
				ICHECK=-9999
			ELSE
	                        IF (ICOUNT.NE.IFRQ_PRESSURE+1) THEN
					ICHECK_2=99
				END IF
				P=SUM/ICOUNT
			END IF
                        
			IF (TDT.LT.10.0) THEN
				ICHECK=-9999
			ELSE
				RHO=(0.3484*P)/TDT
			END IF
		
C			Make sure that division by one does not happen.	
			IF ((TAS.LT.1).OR.(RHO.LT.1E-08)) THEN
				ICHECK=-9999
			END IF
			
C			ICHECK will be more than one if any of the constants 
C			are missing, or the true air speed is zero, or rho
C			is zero. 
			IF (ICHECK.EQ.1.and.icheck_2.eq.1) THEN
				IF(IRAW(IS,42).EQ.0.OR.IRAW(IS,42)
     &                              .EQ.'FFFF'X) IFLAG=3
C			ICHECK_2 will be diffrent than 1 if there are not eight
C			samples for the true de-iced temp or pressure.
			ELSE IF (ICHECK_2.EQ.99.and.icheck.eq.1) THEN
				IFLAG=1
			ELSE
				IFLAG=3
                        END IF
     				
C			If the flag of the raw data is less than three, then 
C			convert the raw data into derived data. This is done 
C			using ;
C
C				LWC=  (A+Bx)*77.2
C				      ------------
C					 TAS*RHO
C
C
C				RHO=0.3484*STATIC_PRESSURE
C				    ----------------------
C					TRUE_DE_ICED_TEMP
C
C

	       	       	IF (IFLAG.LT.3) THEN
		                RAW=FLOAT(IRAW(IS,42))
				RDER(IS, 535)=((RCONST(1)+RCONST(2)*RAW)
     1					       *77.2)/(TAS*RHO)
	     		ELSE

C				If the flag is three or above, set the 
C				derived data to -9999.0.
				RDER(IS, 535)=-9999.0
    			END IF

C			If the derived data is outside the bounds but not 
C			-9999.0, then set the flag to two.
			IF (((RDER(IS, 535).LT.-8.0).OR.
     1					(RDER(IS, 535).GT.8.0)).AND.
     2			    		(RDER(IS, 535).GT.-9000.0)) THEN
				CALL ISETFLG(RDER(IS, 535), 2)

C			If J/W > 300/TAS  set J/W FLAG=1
C			300/tas = instrument saturation value.
C			Ref Ouldridge Feb 1982, Johnson 1979

			ELSE IF	((RDER(IS, 535).GE.-8.0).AND.
     1					(RDER(IS, 535).LE.8.0).AND.
     3			    	     	(RDER(IS, 535).GT.(300/TAS))) THEN
			    	CALL ISETFLG(RDER(IS, 535), 1)

			ELSE

C				The derived data is within the limits then 
C				set the flag to that of the raw data. If the 
C				data is -9999.0 the flag will be three.
			 	CALL ISETFLG(RDER(IS, 535), IFLAG) 
	    		ENDIF

			IF(IRAW_FLAG.GT.ITSTFLG(RDER(IS, 535))) THEN
				CALL ISETFLG(RDER(IS, 535), IRAW_FLAG)
			END IF 

 		END DO    ! For the frequency of the LWC.
        ELSE

C		The data has not got the right frequency.
		DO IS=1,4
			RDER(IS, 535)=-9999.0
			CALL ISETFLG(RDER(IS,535),3)
		END DO
	ENDIF


	RETURN

	END
