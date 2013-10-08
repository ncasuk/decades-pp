#from ppodd.pod import *
from ppodd.core import *
class twc(fort_cal):
    """
FORTRAN routine C_TWC

 ROUTINE	    C_TWC   subroutine fortvax/fort77

 PURPOSE	    To calibrate DRS pars. 70-78 into TARDIS parameters 664-672
	
 DESCRIPTION       The same algorithm is used for all nine parameters. First
		    check to see if the right frequency has been set. Find
		    the flag of the raw data. Work out the derived parameter,
		    #665-#671, values of RCONST are used in a polynomial
		    fit. For #664(Detector) and #672(status word) the raw
		    data is converted from an integer to a real. Then the 
		    derived data is tested to see if it lies between the
		    accepted envelope of values for that parameter. The flag
		    is set to 2 if it lies outside the envelope. If any
		    other tests are failed the derived parameter is set to
		    -9999.0 with the flag at 3. At the end, with all the
		    parameters calculated,  a rate of change check is made. 
		    This looks at the values set in RATE_CHANGE.

		    Derived data limits and rate of change limits;

		    DRS  TARDIS   min    max   rate/change   units
		    par   par
		    70    664     0      4094       -         DRS
                   71    665     314    383      10.0         K
                   72    666     323    388       3.0         K
                   73    667     289    343       2.0         K
                   74    668     378    393       5.0         K
                   75    669     0.3    6.6       0.5         A
                   76    670     0.3    6.6       0.5         A
                   77    671     0.4E-3 1.1E-3    0.05E-3     A
                   78    672     0      4095       -         DRS

 VERSION 	    1.00 080190 M.J.GLOVER

 ARGUMENTS         IRAW(64,512) I*4  IN   Raw data for the parameters
		    IFRQ(512)    I*4  IN   Frequencies of the data
		    RCONST(64)   R*4  IN   Constants required by routine,(1-32)
		    RDER(64,1024)R*4  OUT  Tardis parameters
	
 COMMON	    None.
                 
 SUBPROGRAMS	    ISETFLG (linked automatically)

 FILES		    None.
	
 REFERENCES	    MRF2 Specification for Total Water Hygrometer 4 Dec 1989

 CHANGES           V1.01  10/06/94  W.D.N.JACKSON / S.J.MOSS
                   Modified to correctly compute evaporator currents when the
                   modified TWC instrument is flown (ie for A188 onwards).  In
                   this case DRS parameters 173 and 174 are also used.  If
                   the CALHTR1 and CALHTR2 keywords in the flight constants 
                   file have four values then processing for the modified 
                   probe is used; if they have two values then the old
                   processing is used.  Note that parameters 173 and 174 are
                   optional for this routine and CALIBRATE does not insist
                   that they are present.  Also note that when this routine is
                   used for flights before A188 CALIBRATE issues a warning
                   that some of the constants are absent; this can be ignored.

###############################################################################
DEC$ IDENT 'V1.01'
	Calibrate the hygrometer detector output - DRS parameter 70, sample
	rate 64 Hz. To be left as bits.                 
			If the raw data is inside the bounds, process it.
				IFLAG=ITSTFLG(IRAW(IS, 70))
     				If the flag of the raw data is less than three,
      				then convert the raw data into derived data.
				If the flag is three or above, set the 
				derived data to -9999.0.
				If the raw data is outside the bounds, set it 
				to -9999.0.
			If the derived data is outside the bounds of 0 and 
			4094, set the flag to three.
				If the derived data is within the bounds of 0
				and 4094, then set the flag to that of the raw
				data's.
		If the wrong frequency is there for the detector, then set all
		the samples for this second to -9999.0, with their flags set 
		to 3.
	Calibrate the nose temperature - DRS parameter 71, sample rate 1 Hz
	This is to be put into Kelvin. A do loop is used, as the sample rate
	may well change. This uses the elements of RCONST from 1 to 5.
			See if all the const are there,if not set the flag to 3
			ICHECK will be more than one if any of the constants 
			are missing 
				IFLAG=ITSTFLG(IRAW(IS, 71))
			If the flag of the raw data is less than three, then 
			convert the raw data into derived data. This is done 
			using a polynomial fit.
				If the flag is three or above, set the 
				derived data to -9999.0.
			If the derived data is outside the bounds but not 
			-9999.0, then set the flag to two.
				The derived data is within the limits then 
				set the flag to that of the raw data. If the 
				data is -9999.0 the flag will be three.
		The data has not got the right frequency.
	Calibrate the sample temp -DRS parameter 72, sample rate 1 HZ. This is 
	to be turned into Kelvin. This uses the elements of RCONST from 6 to 11
			See if all the const are there,if not set the flag to 3
			ICHECK will be more than one if any of the constants 
			are missing. 
				IFLAG=ITSTFLG(IRAW(IS, 72))
			If the flag of the raw data is less than three, then 
      			convert the raw data into derived data. This is done 
			using a polynomial fit.
				If the flag is three or above, set the
				derived data to -9999.0. 
			If the derived data is outside the bounds but not 
			-9999.0, then set the flag to two.
				The derived data is within the limits then 
				set the flag to that of the raw data. If the 
				data is -9999.0 the flag will be three.
		The data has not got the right frequency.
	to 16
			See if all the const are there, if not set the flag to 3
			ICHECK will be more than one if any of the constants 
			are missing. 
				IFLAG=ITSTFLG(IRAW(IS, 73))
			If the flag of the raw data is less than three, then 
			convert the raw data into derived data. This is done 
			using a polynomial fit.
				If the flag is three or above, set the 
				derived data to -9999.0.
			If the derived data is outside the bounds but not 
			-9999.0, then set the flag to two.
				The derived data is within the limits then 
				set the flag to that of the raw data. If the 
				data is -9999.0 the flag will be three.
		The data has not got the right frequency.
	Calibrate the source temp - DRS parameter 74, sample rate 1 Hz. This 
	will be in Kelvin.This uses the elements of RCONST from 17 to 22.
			See if all the const are there, if not set the flag to 3
			ICHECK will be more than one if any of the constants 
			are missing. 
				IFLAG=ITSTFLG(IRAW(IS, 74))
			If the flag of the raw data is less than three, then 
			convert the raw data into derived data. This is done 
			using a polynomial fit.
				If the flag is three or above, set the 
				derived data to -9999.0.
			If the derived data is outside the bounds but not 
			-9999.0, then set the flag to two.
				The derived data is within the limits then 
				set the flag to that of the raw data. If the 
				data is -9999.0 the flag will be three.
		The data has not got the right frequency.
	Calibrate the evaporator current 1- DRS parameter 75, sample rate 1 Hz
	If it is a modified probe, ie there are four constants in the flight
	constants file for the CALHTR1 keyword, then parameter 173 is also used.
	This will be in amps. This uses the elements of RCONST from 23 to 26.
			See if all the const are there, if not set the flag to 3
			ICHECK will be more than one if any of the constants 
			are missing. 
				IFLAG=ITSTFLG(IRAW(IS, 75))
			If the flag of the raw data is less than three, then 
			convert the raw data into derived data. This is done 
			using a polynomial fit.
				If the flag is three or above, set the 
				derived data to -9999.0.
			If the derived data is outside the bounds but not 
			-9999.0, then set the flag to two.
				The derived data is within the limits then 
				set the flag to that of the raw data. If the 
				data is -9999.0 the flag will be three.
		The data has not got the right frequency.
	Calibrate the evaporator current 2- DRS parameter 76, sample rate 1Hz.
	If it is a modified probe, ie there are four constants in the flight
	constants file for the CALHTR2 keyword, then parameter 174 is also used.
	This will be in amps. This uses the elements of RCONST from 27 to 30.
			See if all the const are there, if not set the flag to 3
			ICHECK will be more than one if any of the constants 
			are missing. 
				IFLAG=ITSTFLG(IRAW(IS, 76))
			If the flag of the raw data is less than three, then 
			convert the raw data into derived data. This is done 
			using a polynomial fit.
				If the flag is three or above, set the 
				derived data to -9999.0.
			If the derived data is outside the bounds but not 
			-9999.0, then set the flag to two.
				The derived data is within the limits then 
				set the flag to that of the raw data. If the 
				data is -9999.0 the flag will be three.
   		The data has not got the right frequency.
	Calibrate the source current - DRS parameter 77, sample rate 1 Hz. 
	This will be in amps. This uses the elements of RCONST from 31 to 32.
			See if all the const are there, if not set the flag to 3
			ICHECK will be more than one if any of the constants 
			are missing. 
				IFLAG=ITSTFLG(IRAW(IS, 77))
			If the flag of the raw data is less than three, then 
			convert the raw data into derived data. This is done 
			using a polynomial fit.
			If the flag is three or above, set the derived data 
			to -9999.0. 
			If the derived data is outside the bounds but not 
			-9999.0, then set the flag to two.
				The derived data is within the limits then 
				set the flag to that of the raw data. If the 
				data is -9999.0 the flag will be three.
		The data has not got the right frequency.
	Calibrate the status word - DRS parameter 78, sample rate 1 Hz. This 
	will be in raw data.
			If the raw data is inside the bounds, process it.
				IFLAG=ITSTFLG(IRAW(IS, 78))
				If the flag of the raw data is less than 
				three, then convert the raw data into derived
				data.
 					If the flag is three or above, set the
					derived data to -9999.0.
			If the derived data is outside the bounds of 0 and 
			4095, set the flag to two.
				If the derived data is within the bounds of 0
				and 4095, then set the flag to that of the raw
				data's.
 		If the wrong frequency is there for the status, then set all
		the samples for this second to -9999.0, with their flags set 
		to 3.
	Check the rate of change for parametrs 665 to 671
	If the time has been incremented by more than one, store the
	parameters, and return.
		Only bother with a parameter that is inside its bounds.
			Check the differnce of the old and new value against
			the stored value in the array RATE_CHANGE.
	Store away the parameters
	Store away the time.

"""
    def __init__(self,dataset):
        self.input_names=['CALTNOS', 'CALTSAM', 'CALTAMB', 'CALTSRC', 'CALHTR1', 'CALHTR2', 'CALISRC', 
                          'SECS','Horace_TWCD', 'Horace_TNOS', 'Horace_TSAM', 'Horace_TAMB', 
                          'Horace_TSRC', 'Horace_HTR1', 'Horace_HTR2', 'Horace_ISRC', 'Horace_STAT',
                          'Horace_EV1C','Horace_EV2C']
        self.outputs=[parameter('TWC_DET',units='bits',frequency=64,number=664,long_name='Raw data from the TWC probe Lyman alpha detector')
                     ,parameter('TNOS_CAL',units='DEG K',frequency=1,number=665,long_name='TWC NOSE TEMP')
                     ,parameter('TWC_TSAM',units='K',frequency=1,number=666,long_name='Sample temperature in Kelvin from the TWC evaporator probe')
                     ,parameter('TAMB_CAL',units='DEG K',frequency=1,number=667,long_name='TWC AMBIENT TEMP')
                     ,parameter('TSRC_CAL',units='DEG K',frequency=1,number=668,long_name='TWC SOURCE TEMP')
                     ,parameter('HTR1_CAL',units='AMPS',frequency=1,number=669,long_name='TWC EVAP1 CURRENT')
                     ,parameter('HTR2_CAL',units='AMPS',frequency=1,number=670,long_name='TWC EVAP2 CURRENT')
                     ,parameter('ISRC_CAL',units='AMPS',frequency=1,number=671,long_name='TWC SOURCE CURRENT')
                     ,parameter('STAT_CAL',units='RBITS',frequency=1,number=672,long_name='TWC STATUS WORD')]
        #self.name='TWC'
        self.version=1.00
        fort_cal.__init__(self,dataset)
