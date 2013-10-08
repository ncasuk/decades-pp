#from ppodd.pod import *
from ppodd.core import *
class nox(fort_cal):
    """
FORTRAN routine C_NOX

 ROUTINE	C_NOX SUBROUTINE FORTVAX

 PURPOSE	A subroutine to calculate Nitrogen monoxide, Nitrogen dioxide
		and NOx measured by the TECO 42 NOx analyser.

 DESCRIPTION	The NOx analyser outputs three measurements, NO, NO2 and NOx.
		These are input to the program as DRS bits, and converted
		into PPB by multiplying the DRS bits by a calibration factor.


 TO COMPILE	$FORT C_NOX

 VERSION	1.00  28 Sept. 1998  	I. Hawke
               1.01  23 June. 1999     I. Hawke     5ppb Offset included
               1.02  07 Mar   2000     I. Hawke     Offset removed
               1.03  07 Mar   2000     I. Hawke     5ppb offset included
               1.04  29 Mar   2000     I. Hawke     New Conversion Algorithm
               1.05  30 Mar   2000     I. Hawke     Flow Testing Added
               1.06  02 Oct   2002     N. Jackson   Modified for 16 bit DRS
               1.07  27 Oct   2005     D.Tiddeman   New constants for flagging
                                                    low airspeeds or out of
                                                    range values.
               1.08  05 Dec  2005      D.Tiddeman   No flag 2 for negative
                                                                               
 ARGUMENTS	IRAW(1,203) - on entry contains the raw NO signal
		IRAW(1,204) - on entry contains the raw NO2 signal
		IRAW(1,205) - on entry contains the raw NOx signal
               IRAW(1,114) - on entry contains ozone flow signal
               RCONST(1,2,3,4) XO and X1 voltage cal for NO, v to ppb, ppb offs
               RCONST(5,6,7,8) same for NO2
               RCONST(9,10,11,12) same for NOx
               RCONST(13,14) X0 and X1 voltage cal for Ozone flow
		RDER(1,770) - on exit contains the derived NO signal
		RDER(1,771) - on exit contains the derived NO2 signal
		RDER(1,772) - on exit contains the derived NOx signal

*******************************************************************************

"""
    def __init__(self,dataset):
        self.input_names=['CALNO', 'CALNO2', 'CALNOX', 'CALO3F', 'CALNOMX',  
                          'Horace_NOX1', 'Horace_NOX2', 'Horace_NOX3', 'Horace_O3F', 'Horace_RVAS']
        self.outputs=[parameter('NO_TECO',units='ppb',frequency=1,number=770,long_name='Mole fraction of Nitrogen Monoxide (NO) in air from the TECO 42 instrument')
                     ,parameter('NO2_TECO',units='ppb',frequency=1,number=771,long_name='Mole fraction of Nitrogen Dioxide (NO2) in air from the TECO 42 instrument')
                     ,parameter('NOX_TECO',units='ppb',frequency=1,number=772,long_name='Mole fraction of NOx in air from the TECO 42 instrument')]
        #self.name='NOX'
        self.version=1.00
        fort_cal.__init__(self,dataset)
