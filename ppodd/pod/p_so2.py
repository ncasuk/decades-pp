#from ppodd.pod import *
from ppodd.core import *
class so2(fort_cal):
    """
FORTRAN routine C_SO2

 ROUTINE	C_SO2 SUBROUTINE FORTVAX

 PURPOSE	A subroutine to calculate Carbon monoxide.

 DESCRIPTION	The SO2 analyser outputs one measurement.
		This is input to the program as DRS bits, and converted
		into PPB by multiplying the DRS bits by a calibration factor.


 TO COMPILE	$FORT C_SO2

 VERSION	1.00  8-Jul-2004  	D.Tiddeman
                                                                               
 ARGUMENTS	IRAW(1,214) - on entry contains the raw SO2 signal
               RCONST(1,2,3,4) XO and X1 voltage cal for SO2, v to ppb, ppb offs
		RDER(1,740) - on exit contains the derived SO2 signal

*******************************************************************************

"""
    def __init__(self,dataset):
        self.input_names=['CALSO2', 'Horace_SO2']
        self.outputs=[parameter('SO2_TECO',units='ppb',frequency=1,number=740,
                      long_name='Mole Fraction of Sulphur Dioxide in air from TECO 43 instrument')]
        #self.name='SO2'
        self.version=1.00
        fort_cal.__init__(self,dataset)
