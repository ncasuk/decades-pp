#from ppodd.pod import *
from ppodd.core import *
class lwc(fort_cal):
    """
FORTRAN routine C_LWC

 ROUTINE	    C_LWC   SUBROUTINE FORT VAX   [C_LWC.FOR]

 PURPOSE	    To calibrate DRS parm 42 to tardis parm 535 (LWC)
	
 DESCRIPTION 	    The Liquid Water Content (LWC) is a four hertz 
		    parameter. It requires the True Air Speed (Parm 517), 
		    True De_iced Temperature (parm 520) and Static 
		    Pressure (parm 576). All these derived parameters 
		    (517, 520, 576) are at 32 Hertz. So for each quarter 
		    point of the LWC requires a sample of eight of 
		    the derived paramters to be averaged. This is done using
		    only good data points. If there are not eight samples but
		    more than one then the flag for the derived LWC is set to 1.
		    If the frequency of the DRS parm (42) is not equal to 4
		    then no values are calculated and all four points of the
		    LWC are set to -9999.0, with a flag of 3. If a point cannot
		    be calculated then the value of it is set to -9999.0 with
		    a flag value of 3. If the instrument is saturated then the
		    flag value is 1. If the derived value for the LWC falls out
		    of the bounds of -10 to 10 then the flag is set to 2.

 VERSION 	    1.02 17-01-96 D Lauchlan

 ARGUMENTS         IRAW(64,512) I*4  IN   Raw data for the parameters
		    IFRQ(512)    I*4  IN   Frequencies of the data
		    RCONST(64)   R*4  IN   Constants required by routine,(1-28)
		    RDER(64,1024)R*4  OUT  Tardis parameters
	
 COMMON	    None.
                 
 SUBPROGRAMS	    ISETFLG (linked automatically)

 FILES		    None.
	
 REFERENCES	    MRF2 Specification for Total Water Hygrometer 4 Dec 1989
		    Ouldridge Feb 1982
                   Johnson 1979

 CHANGES           110190 Documentational changes only         M.Glover  
                   v 1.02 17-01-96 D Lauchlan
                   Unused parameters removed

                   V1.03  27/09/02  W.D.N.JACKSON
                   Changed to include handling of 16 bit data from the new 
                   DRS.
###############################################################################
DEC$ IDENT 'V1.03'
	The frequencies of the derived parameters passed into this module
	may change. That is why IFRQ_*** has been set up. Here is a table of
	what values of it corresponds to what frequency;

			Frq	IFRQ_***
			 4	   0
			16         1
			32         7
			64        15

	Calibrate the Johnson_Williams Liquid Water Content Probe - DRS 
	parameter 42, sample rate 4 Hz. This is to be put into g kg-1.
	This uses the elements of RCONST from 1 to 2.
			See if all the const are there,if not set the flag to 3
			Find the average of the TAS.
			Reset the starter for the do loop to be the old start 
			point plus the incremental for the TAS frequency plus 
			one.
				No good points.
			Find the average of the true de_iced temp.
			Find the static pressure average.
				Only use good data, namley of flag zero.
			Make sure that division by one does not happen.	
			ICHECK will be more than one if any of the constants 
			are missing, or the true air speed is zero, or rho
			is zero. 
			ICHECK_2 will be diffrent than 1 if there are not eight
			samples for the true de-iced temp or pressure.
			If the flag of the raw data is less than three, then 
			convert the raw data into derived data. This is done 
			using ;

				LWC=  (A+Bx)*77.2
				      ------------
					 TAS*RHO


				RHO=0.3484*STATIC_PRESSURE
				    ----------------------
					TRUE_DE_ICED_TEMP


				If the flag is three or above, set the 
				derived data to -9999.0.
			If the derived data is outside the bounds but not 
			-9999.0, then set the flag to two.
			If J/W > 300/TAS  set J/W FLAG=1
			300/tas = instrument saturation value.
			Ref Ouldridge Feb 1982, Johnson 1979
				The derived data is within the limits then 
				set the flag to that of the raw data. If the 
				data is -9999.0 the flag will be three.
		The data has not got the right frequency.

"""
    def __init__(self,dataset):
        self.input_names=['CALLWC', 'Horace_J_W', 'TAS_RVSM', 'TAT_DI_R', 'PS_RVSM']
        self.outputs=[parameter('LWC_JW_U',units='gram kg-1',frequency=4,number=535,long_name='Uncorrected liquid water content from the Johnson Williams instrument.')]
        #self.name='LWC'
        self.version=1.00
        fort_cal.__init__(self,dataset)
