from ppodd.core import *
class drs(fort_cal):
    """
FORTRAN routine C_DRS

 ROUTINE          C_DRS SUBROUTINE FORTVAX

 PURPOSE          Calibrates the DRS time, event, signal reg, cab press & temp

 DESCRIPTION      Produces time            (515) as real      secs past mdnt
                  Produces event mark      (644) as real      drs units
                  Produces signal register (641) as integer   drs units
                  Produces cabin temp      (660) as real      deg C

 VERSION          1.00  10-1-90  N.JACKSON

 ARGUMENTS        IRAW(64,512) I*4 IN  Up to 64 samples for up to 512 DRS pars
                  IFRQ(512)    I*4 IN  Sample rate of each DRS par (0-64)
                  RCONST(64)   R*4 IN  Inputs constants (none used by C_DRS)
                  RDER(64,1024)R*4 OUT Output array of up to 64 samples for
                                       each of 1024 parameters

 CHANGES          1.01  7-4-92  W.D.N.JACKSON
                  Bug fixed which caused time to be 1 minute out after
                  midnight crossovers.

                  1.02  30-09-02  W.D.N.JACKSON
                  Changed to handle new DRS on 146, and also cabin 
                  temperature.  Signal register now returned as a real
                  number.

                  1.03  04-01-06  D.A.TIDDEMAN
                  Checks flags on cabin pressire constants before 
                  processing.


    """
    def __init__(self,dataset):
        self.input_names=['CALCABT', 'Horace_GMTH', 'Horace_GMTM', 'Horace_EVM', 'Horace_SREG', 'Horace_CABT']
        self.outputs=[parameter('SECS',units='SECS',frequency=1,number=515,long_name='SECS FROM MIDNIGHT')
                     ,parameter('SREG_CAL',units='IBITS',frequency=2,number=641,long_name='SIGNAL REGISTER')
                     ,parameter('EVM_CAL',units='RNUM',frequency=2,number=644,long_name='EVENT MARK')
                     ,parameter('CAB_TEMP',units='degC',frequency=1,number=660,long_name='Cabin temperature at the core consoles')]
        self.version=1.00
        fort_cal.__init__(self,dataset)
