from ppodd.core import *

class rio_lwc(fort_cal):
    """
FORTRAN routine C_LWC

:ROUTINE:
  C_LWC   SUBROUTINE FORT VAX   [C_LWC.FOR]

:PURPOSE:
  To calibrate DRS parm 42 to tardis parm 535 (LWC)

:DESCRIPTION:
  The Liquid Water Content (LWC) is a four hertz
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

:VERSION:
  1.02 17-01-96 D Lauchlan

:ARGUMENTS:
  | IRAW(64,512) I*4  IN   Raw data for the parameters
  | FRQ(512)    I*4  IN   Frequencies of the data
  | RCONST(64)   R*4  IN   Constants required by routine,(1-28)
  | RDER(64,1024)R*4  OUT  Tardis parameters

:SUBPROGRAMS:
  ISETFLG (linked automatically)

:REFERENCES:
  MRF2 Specification for Total Water Hygrometer 4 Dec 1989
    Ouldridge Feb 1982 Johnson 1979

:CHANGES:           
  110190 Documentational changes only         M.Glover
  v 1.02 17-01-96 D Lauchlan
    Unused parameters removed
  V1.03  27/09/02  W.D.N.JACKSON
    Changed to include handling of 16 bit data from the new
                   DRS.

"""

    def __init__(self,dataset):
        self.input_names=['CALLWC', 'CORCON_jw_lwc', 'TAS_RVSM', 'TAT_DI_R', 'PS_RVSM']
        self.outputs=[parameter('LWC_JW_U',units='gram kg-1',frequency=4,number=535,long_name='Uncorrected liquid water content from the Johnson Williams instrument')]
        #self.name='RIO_LWC'
        self.fortname='LWC'
        self.version=1.00
        fort_cal.__init__(self,dataset)

    def process(self):
        self.dataset[self.input_names[1]].number=42
        fort_cal.process(self)
