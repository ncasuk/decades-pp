#from ppodd.pod import *
from ppodd.core import *
class rio_radal(fort_cal):
    """
FORTRAN routine C_RADAL1

 ROUTINE	C_RADAL1 SUBROUTINE FORTVAX

 PURPOSE	To subroutine to calculate the aircraft altitude from the radar
		altimeter.

 DESCRIPTION	The raw radar altimeter data is provided as a 16 bit signed
               number from the ARINC 429 data bus, with a least bit resolution
               of 0.25 ft.

		The derived data is quality controlled to ensure that:
		(a) data outside the range 0 to 8191.75 ft are flagged 3
		(b) more than two values the same are flagged 3
		(c) more than 1000' change between values is flagged 3

 TO COMPILE	$FORT C_RADAL1

 VERSION	1.00  02/10/02  W.D.N.JACKSON

 ARGUMENTS	IRAW(X,37)  - where x=1 or 2, on entry this contains the raw 
			      radar height. 
		IFRQ(37)    - on entry contains 2, the frequency of the raw 
			      radar height.
		RDER(X,575) - where x= 1 or 2, on exit contains the derived 
			      radar height in meters.

 CHANGES	V1.01  WDNJ  05/11/04
		Flagging criteria improved

*******************************************************************************

"""
    def __init__(self,dataset):
        self.input_names=['PRTAFT_rad_alt']
        self.outputs=[parameter('HGT_RADR',units='m',frequency=2,number=575,long_name='Radar height from the aircraft radar altimeter.')]
        #self.name='RIO_RADAL'
        self.fortname='RADAL1'
        self.version=1.00
        fort_cal.__init__(self,dataset)


    def process(self): 
        self.dataset[self.input_names[0]].number=37
        fort_cal.process(self)
