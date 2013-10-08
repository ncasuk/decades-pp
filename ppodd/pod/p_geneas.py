#from ppodd.pod import *
from ppodd.core import *
class geneas(fort_cal):
    """
FORTRAN routine C_GENEAS
DEC$ IDENT 'V1.02'

 ROUTINE  	   C_GENEAS     SUBROUTINE FORTVAX
     
 PURPOSE 	   Derivation of Dew point   

 DESCRIPTION      Calculation of Dew Point in K from General Eastern Hygrometer
                  
                         529- Dew Point                             [K]

                  The General Eastern Hygrometer (Parameter 58) is recorded 
                  in binary with a range of 0 to 4095 DRS bits.
                  A control signal (Parameter 59) is also recorded which 
                  gives an indication of the amount of heating or cooling
                  of the mirror.
                  The instrument should be in control if the signal is between
                  certain limits. 
                  Outside these limits it still produces a dew point 
                  reading,though of doubtful accuracy, and derived data 
                  is flagged - FLAG = 2.
                  
 VERSION	   1.00  240190   J HARMER
                  1.01  17-01-96 D Lauchlan
 ARGUMENTS 
                  Constants:
                  GEMAX      Maximum control condition signal limit RCONST(1)
                  GEMIN      Minimum control condition signal limit RCONST(2)
                  CALGE(1)   GE Dew point calib. constant x0        RCONST(3)
                  CALGE(2)   GE Dew point calib. constant x1        RCONST(4)
                  CALGE(3)   GE Dew point calib. constant x2        RCONST(5)


                  Inputs   : 
                  GENERAL EASTERN 1011 DEW POINT    [drs units ] Para 58
                  GENERAL EASTERN CONTROL SIGNAL    [drs units ] Para 59

                  Outputs  : 
                  DEW POINT                            [K]      Para 529
           
 SUBPROGRAMS
                  ITSTFLG          Examines bits 16,17 for flags
                  ISETFLG          Sets flag bits 16,17 = 0 --> 3
                  S_QCPT           Performs range and rate of change check

 REFERENCES 	   Code adapted from MRF1/MRF2

 CHANGES          v1.01 17-01-96 D Lauchlan
                  Unused variables removed

                  V1.02  27/09/02  W.D.N.JACKSON
                  Changed to include handling of 16 bit data from the new 
                  DRS.
------------------------------------------------------------------------------

    """
    def __init__(self,dataset):
        self.input_names=['GELIMS', 'CALGE', 'SECS','Horace_HYGR','Horace_HYCS']
        self.outputs=[parameter('TDEW_GE',units='degK',frequency=4,number=529,long_name='Dew point from the General Eastern instrument.')]
        #self.name='GENEAS'
        self.version=1.00
        fort_cal.__init__(self,dataset)
