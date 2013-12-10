#from ppodd.pod import *
from ppodd.core import *
class twc_horace(fort_cal):
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
        self.fortname='TWC'
        self.version=1.00
        fort_cal.__init__(self,dataset)
