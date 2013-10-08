#from ppodd.pod import *
from ppodd.core import *
class fwvs(fort_cal):
    """
FORTRAN routine C_FWVS

 ROUTINE          C_FWVS SUBROUTINE FORTVAX

 PURPOSE          Produces FWVS dewpoint 

 DESCRIPTION      

 VERSION          1.00 30/04/07 D TIDDEMAN 

 ARGUMENTS        IRAW(64,512) I*4 IN  Up to 64 samples for up to 512 DRS pars
                  IFRQ(512)    I*4 IN  Sample rate of each DRS par (0-64)
                  RCONST(64)   R*4 IN  Inputs constants (none used by C_CNC)
                  RDER(64,1024)R*4 OUT Output array of up to 64 samples for
                                       each of 1024 parameters

 CHANGES          


    """
    def __init__(self,dataset):
        self.input_names=['Horace_FWVS']
        self.outputs=[parameter('FLDP',units='DEG K',frequency=1,number=573,long_name='FWVS DEW POINT')]
        #self.name='FWVS'
        self.version=1.00
        fort_cal.__init__(self,dataset)
