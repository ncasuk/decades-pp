from ppodd.core import *
class ozone1(fort_cal):
    """
FORTRAN routine C_OZONE1

 ROUTINE	C_OZONE1 SUBROUTINE FORTVAX

 PURPOSE	A subroutine to calculate the ozone mixing ratio.

 DESCRIPTION   Calibration routine for TECO OZONE
               Fourth order fit for temperature transducer
               Linear fit for pressure transducer
               Signal 10V =1000 ppb ozone corrected by pressure and temperature                              

 TO COMPILE	$FORT C_OZONE1

 VERSION	    1.00  30th Aug 1996  D.Tiddeman   Module Written
               1.01  12th Sep 1999  I. Hawke     O3 Smoothing Algorithm
               1.02  2 Oct 2002     N. Jackson   Convert to work on new DRS
               1.03  11 Feb 2005   D.Tiddeman   No longer does P+T correction
		1.04  Unknown       D.Tiddeman   RVSM, min and max flags addeds
               1.05  31 Jan 2007   R Purvis     Rewritten to take out P, T
                                                corrections
					         Flow commented out
                                                                             
 ARGUMENTS	IRAW(1,100) - on entry contains the raw ozone signal
		IRAW(1,114) - on entry contains the raw ozone flow
		IRAW(1,223) - on entry contains the raw rvsm airspeed
		RCONST(X)  
		RDER(1,574) - on exit contains the derived ozone mixing ratio


*******************************************************************************

"""
    def __init__(self,dataset):
        self.input_names=['CALO3', 'CALO3P', 'CALO3T', 'CALO3F', 'CALO3MX', 'Horace_O3', 'Horace_RVAS']
        self.outputs=[parameter('O3_TECO',units='ppb',frequency=1,number=574,long_name='Mole fraction of ozone in air from the TECO 49 instrument')]
        #self.name='OZONE1'
        self.version=1.00
        fort_cal.__init__(self,dataset)
