C
C ROUTINE	    C_TWC   subroutine fortvax/fort77
C
C PURPOSE	    To calibrate DRS pars. 70-78 into TARDIS parameters 664-672
C	
C DESCRIPTION       The same algorithm is used for all nine parameters. First
C		    check to see if the right frequency has been set. Find
C		    the flag of the raw data. Work out the derived parameter,
C		    #665-#671, values of RCONST are used in a polynomial
C		    fit. For #664(Detector) and #672(status word) the raw
C		    data is converted from an integer to a real. Then the 
C		    derived data is tested to see if it lies between the
C		    accepted envelope of values for that parameter. The flag
C		    is set to 2 if it lies outside the envelope. If any
C		    other tests are failed the derived parameter is set to
C		    -9999.0 with the flag at 3. At the end, with all the
C		    parameters calculated,  a rate of change check is made. 
C		    This looks at the values set in RATE_CHANGE.
C
C		    Derived data limits and rate of change limits;
C
C		    DRS  TARDIS   min    max   rate/change   units
C		    par   par
C		    70    664     0      4094       -         DRS
C                   71    665     314    383      10.0         K
C                   72    666     323    388       3.0         K
C                   73    667     289    343       2.0         K
C                   74    668     378    393       5.0         K
C                   75    669     0.3    6.6       0.5         A
C                   76    670     0.3    6.6       0.5         A
C                   77    671     0.4E-3 1.1E-3    0.05E-3     A
C                   78    672     0      4095       -         DRS
C
C VERSION 	    1.00 080190 M.J.GLOVER
C
C ARGUMENTS         IRAW(64,512) I*4  IN   Raw data for the parameters
C		    IFRQ(512)    I*4  IN   Frequencies of the data
C		    RCONST(64)   R*4  IN   Constants required by routine,(1-32)
C		    RDER(64,1024)R*4  OUT  Tardis parameters
C	
C COMMON	    None.
C                 
C SUBPROGRAMS	    ISETFLG (linked automatically)
C
C FILES		    None.
C	
C REFERENCES	    MRF2 Specification for Total Water Hygrometer 4 Dec 1989
C
C CHANGES           V1.01  10/06/94  W.D.N.JACKSON / S.J.MOSS
C                   Modified to correctly compute evaporator currents when the
C                   modified TWC instrument is flown (ie for A188 onwards).  In
C                   this case DRS parameters 173 and 174 are also used.  If
C                   the CALHTR1 and CALHTR2 keywords in the flight constants 
C                   file have four values then processing for the modified 
C                   probe is used; if they have two values then the old
C                   processing is used.  Note that parameters 173 and 174 are
C                   optional for this routine and CALIBRATE does not insist
C                   that they are present.  Also note that when this routine is
C                   used for flights before A188 CALIBRATE issues a warning
C                   that some of the constants are absent; this can be ignored.
C
C###############################################################################

	SUBROUTINE C_TWC(IRAW, IFRQ, RCONST, RDER)
CDEC$ IDENT 'V1.01'
	INTEGER*4 IRAW(64,512), IFRQ(512)
	REAL*4 RCONST(64), RDER(64, 1024)
        REAL OLD_PARAS(7), RATE_CHANGE(7)

	DATA RATE_CHANGE/10.0, 3.0, 2.0, 5.0, 0.5, 0.5, 0.05E-03/

C	Calibrate the hygrometer detector output - DRS parameter 70, sample
C	rate 64 Hz. To be left as bits.                 
	IF (IFRQ(70).EQ.64) THEN   ! See if the right frequency is there.
       		DO IS=1, IFRQ(70)  ! Do for each sample.

C			If the raw data is inside the bounds, process it.
			IF (((IRAW(IS, 70).AND.'FFF'X).GT.-1).AND.
     1					((IRAW(IS,70).AND.'FFF'X)
     2							.LT.4095)) THEN
C				IFLAG=ITSTFLG(IRAW(IS, 70))
				IFLAG=0
               
C     				If the flag of the raw data is less than three,
C      				then convert the raw data into derived data.
	 			IF (IFLAG.LT.3) THEN
					RDER(IS, 664)=FLOAT(IRAW(IS,70)
     1						      .AND.'FFF'X)

C				If the flag is three or above, set the 
C				derived data to -9999.0.
	     			ELSE
					RDER(IS, 664)=-9999.0
				END IF
			ELSE

C				If the raw data is outside the bounds, set it 
C				to -9999.0.
				RDER(IS, 664)=-9999.0
          		ENDIF
               

C			If the derived data is outside the bounds of 0 and 
C			4094, set the flag to three.
			IF ((RDER(IS, 664).LT.0.0).OR.
     1					(RDER(IS, 664).GT.4094)) THEN
	     			CALL ISETFLG(RDER(IS, 664), 3) 
      			ELSE

C				If the derived data is within the bounds of 0
C				and 4094, then set the flag to that of the raw
C				data's.
				CALL ISETFLG(RDER(IS, 664), IFLAG) 
	    		END IF
   		END DO
	ELSE 

C		If the wrong frequency is there for the detector, then set all
C		the samples for this second to -9999.0, with their flags set 
C		to 3.
		DO IS=1, 64
			RDER(IS, 664)=-9999.0
			CALL ISETFLG(RDER(IS,664),3)
		END DO
	ENDIF

C	Calibrate the nose temperature - DRS parameter 71, sample rate 1 Hz
C	This is to be put into Kelvin. A do loop is used, as the sample rate
C	may well change. This uses the elements of RCONST from 1 to 5.
	IF (IFRQ(71).EQ.1) THEN  ! check the frequency.
		DO IS=1, IFRQ(71)  ! for each sample

C			See if all the const are there,if not set the flag to 3
		       	ICHECK=1
			DO I=1,5
				IF (ITSTFLG(RCONST(I)).GT.2) THEN
					ICHECK=ICHECK+1
				END IF
                        END DO
			
C			ICHECK will be more than one if any of the constants 
C			are missing 
			IF (ICHECK.EQ.1) THEN
C				IFLAG=ITSTFLG(IRAW(IS, 71))
				IFLAG=0
			ELSE
				IFLAG=3
			END IF
     				
C			If the flag of the raw data is less than three, then 
C			convert the raw data into derived data. This is done 
C			using a polynomial fit.
	       	       	IF (IFLAG.LT.3) THEN
		                RAW=FLOAT(IRAW(IS,71).AND.'FFF'X)
				RDER(IS, 665)=RCONST(1)

				DO INC=2,5
					RDER(IS, 665)=RDER(IS, 665)+
     1					              RCONST(INC)*
     2						      (RAW**(INC-1))
      				END DO
	     		ELSE

C				If the flag is three or above, set the 
C				derived data to -9999.0.
				RDER(IS, 665)=-9999.0
    			END IF


C			If the derived data is outside the bounds but not 
C			-9999.0, then set the flag to two.
			IF (((RDER(IS, 665).LT.314.0).OR.
     1					(RDER(IS, 665).GT.383.0)).AND.
     2					(RDER(IS, 665).GT.-9000.0)) THEN
				CALL ISETFLG(RDER(IS, 665), 2)
			ELSE

C				The derived data is within the limits then 
C				set the flag to that of the raw data. If the 
C				data is -9999.0 the flag will be three.
			 	CALL ISETFLG(RDER(IS, 665), IFLAG) 
	    		ENDIF	

 		END DO
        ELSE

C		The data has not got the right frequency.
		RDER(1, 665)=-9999.0
		CALL ISETFLG(RDER(1,665),3)
	ENDIF

C	Calibrate the sample temp -DRS parameter 72, sample rate 1 HZ. This is 
C	to be turned into Kelvin. This uses the elements of RCONST from 6 to 11
	IF (IFRQ(72).EQ.1) THEN  ! check the frequency.
		DO IS=1, IFRQ(72)  ! for each sample

C			See if all the const are there,if not set the flag to 3
		       	ICHECK=1
			DO I=6,11
				IF (ITSTFLG(RCONST(I)).GT.2) THEN
					ICHECK=ICHECK+1
				END IF
                        END DO
			
			
C			ICHECK will be more than one if any of the constants 
C			are missing. 
			IF (ICHECK.EQ.1) THEN
C				IFLAG=ITSTFLG(IRAW(IS, 72))
				IFLAG=0
			ELSE
				IFLAG=3
			END IF
     				
C			If the flag of the raw data is less than three, then 
C      			convert the raw data into derived data. This is done 
C			using a polynomial fit.
	       	       	IF (IFLAG.LT.3) THEN
	           		RAW=FLOAT(IRAW(IS,72).AND.'FFF'X)
		       		RDER(IS, 666)=RCONST(6)
		
				DO INC=7,11
					RDER(IS, 666)=RDER(IS, 666)+
     1					     	      RCONST(INC)*
     2						      (RAW**(INC-6))
      	 			END DO
	     		ELSE

C				If the flag is three or above, set the
C				derived data to -9999.0. 
				RDER(IS, 666)=-9999.0
	   		END IF

C			If the derived data is outside the bounds but not 
C			-9999.0, then set the flag to two.
     			IF ( ( (RDER(IS, 666).LT.323.0).OR.
     1					(RDER(IS, 666).GT.388.0) ).AND.
     2					(RDER(IS, 666).GT.-9000.0)) THEN
 				CALL ISETFLG(RDER(IS, 666), 2)
			ELSE

C				The derived data is within the limits then 
C				set the flag to that of the raw data. If the 
C				data is -9999.0 the flag will be three.
			 	CALL ISETFLG(RDER(IS, 666), IFLAG) 
	   		ENDIF	

		END DO
        ELSE

C		The data has not got the right frequency.
		RDER(1, 666)=-9999.0
		CALL ISETFLG(RDER(1,666),3)
	ENDIF
	
c	Calibrate the ambient temp - DRS parameter 73, sample rate 1 Hz. This 
c	is to be turned into Kelvin. This uses the elements of RCONST from 12
C	to 16
	IF (IFRQ(73).EQ.1) THEN  ! check the frequency.
		DO IS=1, IFRQ(73)  ! Do for each sample.

C			See if all the const are there, if not set the flag to 3
		       	ICHECK=1
			DO I=12,16
				IF (ITSTFLG(RCONST(I)).GT.2) THEN
					ICHECK=ICHECK+1
				END IF
                        END DO
			
C			ICHECK will be more than one if any of the constants 
C			are missing. 
			IF (ICHECK.EQ.1) THEN
C				IFLAG=ITSTFLG(IRAW(IS, 73))
				IFLAG=0
			ELSE
				IFLAG=3
			END IF
     				
C			If the flag of the raw data is less than three, then 
C			convert the raw data into derived data. This is done 
C			using a polynomial fit.
	       	       	IF (IFLAG.LT.3) THEN
	        	        RAW=FLOAT(IRAW(IS,73).AND.'FFF'X)
		     		RDER(IS, 667)=RCONST(12)

				DO INC=13,16
					RDER(IS, 667)=RDER(IS, 667)+
     1						      RCONST(INC)*
     2						      (RAW**(INC-12))
      		   		END DO
	     		ELSE

C				If the flag is three or above, set the 
C				derived data to -9999.0.
				RDER(IS, 667)=-9999.0
	
			END IF

C			If the derived data is outside the bounds but not 
C			-9999.0, then set the flag to two.
	     		IF (((RDER(IS, 667).LT.289.0).OR.
     1					(RDER(IS, 667).GT.343.0)).AND.
     2					(RDER(IS, 667).GT.-9000.0)) THEN
	    			CALL ISETFLG(RDER(IS, 667), 2)
	      		ELSE

C				The derived data is within the limits then 
C				set the flag to that of the raw data. If the 
C				data is -9999.0 the flag will be three.
			 	CALL ISETFLG(RDER(IS, 667), IFLAG) 
	  		ENDIF	

		END DO
        ELSE

C		The data has not got the right frequency.
		RDER(1, 667)=-9999.0
		CALL ISETFLG(RDER(1,667),3)
	ENDIF

C	Calibrate the source temp - DRS parameter 74, sample rate 1 Hz. This 
C	will be in Kelvin.This uses the elements of RCONST from 17 to 22.
	IF (IFRQ(74).EQ.1) THEN  ! check the frequency.
		DO IS=1, IFRQ(74)  ! Do for each sample.

C			See if all the const are there, if not set the flag to 3
			ICHECK=1
			DO I=17,22
				IF (ITSTFLG(RCONST(I)).GT.2) THEN
					ICHECK=ICHECK+1
				END IF
                        END DO
			
C			ICHECK will be more than one if any of the constants 
C			are missing. 
			IF (ICHECK.EQ.1) THEN
C				IFLAG=ITSTFLG(IRAW(IS, 74))
				IFLAG=0
			ELSE
				IFLAG=3
			END IF

C			If the flag of the raw data is less than three, then 
C			convert the raw data into derived data. This is done 
C			using a polynomial fit.
       	      		IF (IFLAG.LT.3) THEN
	        	        RAW=FLOAT(IRAW(IS,74).AND.'FFF'X)
		  		RDER(IS, 668)=RCONST(17)
		
				DO INC=18,22
					RDER(IS, 668)=RDER(IS, 668)+
     1					     	      RCONST(INC)*		
     2						      (RAW**(INC-17))
      		   		END DO
	     		ELSE

C				If the flag is three or above, set the 
C				derived data to -9999.0.
				RDER(IS, 668)=-9999.0
	      		END IF

C			If the derived data is outside the bounds but not 
C			-9999.0, then set the flag to two.
	      		IF (((RDER(IS, 668).LT.378.0).OR.
     1					(RDER(IS, 668).GT.393.0)).AND.
     2					(RDER(IS, 668).GT.-9000.0)) THEN
	     			CALL ISETFLG(RDER(IS, 668), 2)
	       		ELSE

C				The derived data is within the limits then 
C				set the flag to that of the raw data. If the 
C				data is -9999.0 the flag will be three.
			 	CALL ISETFLG(RDER(IS, 668), IFLAG) 
	  		ENDIF	

 		END DO
        ELSE

C		The data has not got the right frequency.
		RDER(1, 668)=-9999.0
		CALL ISETFLG(RDER(1,668),3)
	ENDIF
	
C	Calibrate the evaporator current 1- DRS parameter 75, sample rate 1 Hz
C	If it is a modified probe, ie there are four constants in the flight
C	constants file for the CALHTR1 keyword, then parameter 173 is also used.
C	This will be in amps. This uses the elements of RCONST from 23 to 26.
	IF (IFRQ(75).EQ.1) THEN  ! check the frequency.
		DO IS=1, IFRQ(75)  ! Do for each sample.

C			See if all the const are there, if not set the flag to 3
   			ICHECK=1
			DO I=23,26
				IF (ITSTFLG(RCONST(I)).GT.2) THEN
					ICHECK=ICHECK+1
				END IF
                        END DO
			
C			ICHECK will be more than one if any of the constants 
C			are missing. 
			IF (ICHECK.EQ.1.OR.ICHECK.EQ.3) THEN
C				IFLAG=ITSTFLG(IRAW(IS, 75))
				IFLAG=0
			ELSE
				IFLAG=3
			END IF
     				
C			If the flag of the raw data is less than three, then 
C			convert the raw data into derived data. This is done 
C			using a polynomial fit.
	       	       	IF (IFLAG.LT.3) THEN
				RAW=FLOAT(IRAW(IS,75).AND.'FFF'X)
				IF(ICHECK.EQ.1) THEN !It is a modified probe
				  RAW2=FLOAT(IRAW(IS,173).AND.'FFF'X)
				  RDER(IS, 669)=RCONST(23)+(RCONST(24)*RAW2)
     1				      +RCONST(25)*(RAW+RCONST(26))
				ELSE                 !It is an unmodified probe
				  RDER(IS, 669)=RCONST(23)+RCONST(24)*RAW
				END IF
	     		ELSE

C				If the flag is three or above, set the 
C				derived data to -9999.0.
				RDER(IS, 669)=-9999.0
	 		END IF

C			If the derived data is outside the bounds but not 
C			-9999.0, then set the flag to two.
	       		IF (((RDER(IS, 669).LT.0.3).OR.
     1					(RDER(IS, 669).GT.6.6)).AND.
     2					(RDER(IS, 669).GT.-9000.0)) THEN
	  			CALL ISETFLG(RDER(IS, 669), 2)
	  		ELSE

C				The derived data is within the limits then 
C				set the flag to that of the raw data. If the 
C				data is -9999.0 the flag will be three.
			 	CALL ISETFLG(RDER(IS, 669), IFLAG) 
	 		ENDIF	

		END DO
        ELSE

C		The data has not got the right frequency.
		RDER(1, 669)=-9999.0
		CALL ISETFLG(RDER(1,669),3)
	ENDIF

C	Calibrate the evaporator current 2- DRS parameter 76, sample rate 1Hz.
C	If it is a modified probe, ie there are four constants in the flight
C	constants file for the CALHTR2 keyword, then parameter 174 is also used.
C	This will be in amps. This uses the elements of RCONST from 27 to 30.
	IF (IFRQ(76).EQ.1) THEN  ! check the frequency.
		DO IS=1, IFRQ(76)  ! Do for each sample.

C			See if all the const are there, if not set the flag to 3
		      	ICHECK=1
			DO I=27,30
				IF (ITSTFLG(RCONST(I)).GT.2) THEN
					ICHECK=ICHECK+1
				END IF
                        END DO
			
C			ICHECK will be more than one if any of the constants 
C			are missing. 
			IF (ICHECK.EQ.1.OR.ICHECK.EQ.3) THEN
C				IFLAG=ITSTFLG(IRAW(IS, 76))
				IFLAG=0
			ELSE
				IFLAG=3
			END IF
     				
C			If the flag of the raw data is less than three, then 
C			convert the raw data into derived data. This is done 
C			using a polynomial fit.
	       	       	IF (IFLAG.LT.3) THEN
				RAW=FLOAT(IRAW(IS,76).AND.'FFF'X)
				IF(ICHECK.EQ.1) THEN !It is a modified probe
				  RAW2=FLOAT(IRAW(IS,174).AND.'FFF'X)
				  RDER(IS, 670)=RCONST(27)+(RCONST(28)*RAW2)
     1				      +RCONST(29)*(RAW+RCONST(30))
				ELSE                 !It is an unmodified probe
				  RDER(IS, 670)=RCONST(27)+RCONST(28)*RAW
				END IF
	     		ELSE

C				If the flag is three or above, set the 
C				derived data to -9999.0.
				RDER(IS, 670)=-9999.0
       			END IF

C			If the derived data is outside the bounds but not 
C			-9999.0, then set the flag to two.
			IF (((RDER(IS, 670).LT.0.3).OR.
     1					(RDER(IS, 670).GT.6.6)).AND.
     2					(RDER(IS, 670).GT.-9000.0)) THEN
				CALL ISETFLG(RDER(IS, 670), 2)
	    		ELSE

C				The derived data is within the limits then 
C				set the flag to that of the raw data. If the 
C				data is -9999.0 the flag will be three.
			 	CALL ISETFLG(RDER(IS, 670), IFLAG) 
	   		ENDIF	

		END DO
        ELSE

C   		The data has not got the right frequency.
		RDER(1, 670)=-9999.0
		CALL ISETFLG(RDER(1,670),3)
	ENDIF

C	Calibrate the source current - DRS parameter 77, sample rate 1 Hz. 
C	This will be in amps. This uses the elements of RCONST from 31 to 32.
	IF (IFRQ(77).EQ.1) THEN  ! check the frequency.
		DO IS=1, IFRQ(77)  ! Do for each sample.

C			See if all the const are there, if not set the flag to 3
			ICHECK=1
			DO I=31,32
				IF (ITSTFLG(RCONST(I)).GT.2) THEN
					ICHECK=ICHECK+1
				END IF
                        END DO
			
C			ICHECK will be more than one if any of the constants 
C			are missing. 
			IF (ICHECK.EQ.1) THEN
C				IFLAG=ITSTFLG(IRAW(IS, 77))
				IFLAG=0
			ELSE
				IFLAG=3
			END IF
     				
C			If the flag of the raw data is less than three, then 
C			convert the raw data into derived data. This is done 
C			using a polynomial fit.
	       	        IF (IFLAG.LT.3) THEN
	        	        RAW=FLOAT(IRAW(IS,77).AND.'FFF'X)
		      		RDER(IS, 671)=(RCONST(31)+RCONST(32)*
     1					       RAW)
	     		ELSE

C			If the flag is three or above, set the derived data 
C			to -9999.0. 
				RDER(IS, 671)=-9999.0
	  		END IF

C			If the derived data is outside the bounds but not 
C			-9999.0, then set the flag to two.
	   		IF (((RDER(IS, 671).GT.-0.4E-03).OR.
     1 					(RDER(IS, 671).LT.-1.1E-03)).AND.
     2					(RDER(IS, 671).GT.-9000.0)) THEN
				CALL ISETFLG(RDER(IS, 671), 2)
	  		ELSE

C				The derived data is within the limits then 
C				set the flag to that of the raw data. If the 
C				data is -9999.0 the flag will be three.
			 	CALL ISETFLG(RDER(IS, 671), IFLAG) 
	       		ENDIF	

		END DO
        ELSE

C		The data has not got the right frequency.
		RDER(1, 671)=-9999.0
		CALL ISETFLG(RDER(1,671),3)
	ENDIF

C	Calibrate the status word - DRS parameter 78, sample rate 1 Hz. This 
C	will be in raw data.
	IF (IFRQ(78).EQ.1) THEN ! check the frequency.
		DO IS=1, IFRQ(78) ! Do for each sample.

C			If the raw data is inside the bounds, process it.
			IF (((IRAW(IS, 78).AND.'FFF'X).GT.0).OR.
     1					((IRAW(IS,78).AND.'FFF'X).LT.4094)) 
     2								THEN
C				IFLAG=ITSTFLG(IRAW(IS, 78))
				IFLAG=0
               
C				If the flag of the raw data is less than 
C				three, then convert the raw data into derived
C				data.
	 			IF (IFLAG.LT.3) THEN
					RDER(IS, 672)=FLOAT(IRAW(IS,78)
     1 						      .AND.'FFF'X)
	     			ELSE

C 					If the flag is three or above, set the
C					derived data to -9999.0.
					RDER(IS, 672)=-9999.0
	 	  		END IF
            		ENDIF
               
C			If the derived data is outside the bounds of 0 and 
C			4095, set the flag to two.
	       		IF (((RDER(IS, 672).LT.0.0).OR.
     1 					(RDER(IS, 672).GT.4095)).AND.
     2					(RDER(IS, 672).GT.-9000.0)) THEN
		  		CALL ISETFLG(RDER(IS, 672), 2) 
	    		ELSE

C				If the derived data is within the bounds of 0
C				and 4095, then set the flag to that of the raw
C				data's.
				CALL ISETFLG(RDER(IS, 672), IFLAG) 
	   		END IF
                                                 
     		END DO
        ELSE

C 		If the wrong frequency is there for the status, then set all
C		the samples for this second to -9999.0, with their flags set 
C		to 3.
		RDER(1, 672)=-9999.0
		CALL ISETFLG(RDER(1,672),3)
	ENDIF

C	Check the rate of change for parametrs 665 to 671
	TIME=ABS(RDER(1, 515)-OLD_TIME)

C	If the time has been incremented by more than one, store the
C	parameters, and return.
	IF ((TIME.gt.1.1).or.(ITSTFLG(RDER(1,515)).GT.2)) THEN
		DO INC=665, 671
			OLD_PARAS(INC-664)=RDER(1, INC)
		END DO
	
		OLD_TIME=RDER(1, 515)
		RETURN
	END IF
	
	DO INC=665, 671           
C		Only bother with a parameter that is inside its bounds.
		IF (ITSTFLG(RDER(1,INC)).LT.2) THEN
			CHANGE=OLD_PARAS(INC-664)-RDER(1, INC)

C			Check the differnce of the old and new value against
C			the stored value in the array RATE_CHANGE.
			IF (ABS(CHANGE).GT.RATE_CHANGE(INC-664)) THEN
				CALL ISETFLG(RDER(1, INC), 2)
	    		END IF
		END IF
	END DO 

C	Store away the parameters
	DO INC=665, 671
		OLD_PARAS(INC-664)=RDER(1, INC)
	END DO
	
C	Store away the time.
	OLD_TIME=RDER(1, 515)

	RETURN

	END
