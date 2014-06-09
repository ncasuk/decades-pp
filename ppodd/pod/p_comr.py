from ppodd.core import *
class comr(fort_cal):
    """
FORTRAN routine C_COMR

 ROUTINE        C_COMR SUBROUTINE FORTVAX

 PURPOSE        A subroutine to calculate Carbon monoxide.

 DESCRIPTION    The CO analyser outputs one measurement.
                This is input to the program as DRS bits, and converted
                into PPB by multiplying the DRS bits by a calibration factor.


 TO COMPILE     $FORT C_COMR

 VERSION        1.00  8-Jul-2004        D.Tiddeman
               1.01  27-OCT-2005
               1.02
               1.03 31-JAN-2007 R Purvis Changed timedelay after cal to 20
               1.04 18-SEP-2007 R Purvis        RCONST(5) added for correction factor
               1.05 30-JUL-2010 S Bauguitte increased CO flag count threshold from 8000 to 10000
               1.06 15-OCT-2012 A Wellpott CO upper threshold flagging added. Now values above
                                4995 are flagged with 3

 ARGUMENTS      IRAW(1,154) - on entry contains the raw CO signal
                IRAW(1,223) - on entry contains raw RVSM airspeed
                IRAW(1,113) - cal info ?
                RCONST(1,2,3,4) XO and X1 voltage cal for CO, v to ppb, ppb offs
                RDER(1,782) - on exit contains the derived CO signal

*******************************************************************************

    """
    def __init__(self,dataset):
        self.input_names=['CALCOMR', 'CALCOMX', 'Horace_CO', 'Horace_RVAS']
        self.outputs=[parameter('CO_AERO',units='ppb',frequency=1,number=782,long_name='Mole fraction of Carbon Monoxide in air from the AERO AL5002 instrument')]
        #self.name='COMR'
        self.version=1.00
        fort_cal.__init__(self,dataset)

