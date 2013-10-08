#from ppodd.pod import *
from ppodd.core import *
class rio_sun(fort_cal):
    """
FORTRAN routine C_GSUN

 ROUTINE          C_SUN         SUBROUTINE FORTVAX  C_SUN.FOR

 PURPOSE          PUT SOLAR ZENITH AND AZIMUTH ANGLES IN MFD

 DESCRIPTION      Given date, time and location on the earth's
                  surface this routine puts a solar zenith and
                  azimuth angle in the array of derived parameters.
                  It computes a value once every second. The 
                  angles are only obtained if all the flags are
                  set to less than 3 and the date, time and location
                  are all within sensible limits. Any flags set on input
                  are also set in the solar angles derived. If
                  the input is in error or the flags are set to 3
                  a value of -99. is returned for ZEN and AZIM.
                  To test the routine:
                  $ FOR C_SUN
                  $ FOR TEST_C_SUN
                  $ LINK TEST_C_SUN,C_SUN
                  Ensure contents of files RCONST.DAT and TEST_C_SUN.DAT
                  contain simulated data you require to test the routine
                  with.

 VERSION          1.02  1st May 1992   J.A.Smith

 ARGUMENTS        RDER(1,515)  R*4 IN Time GMT (seconds from midnight)
                  RDER(1,550)  R*4 IN Omega latitude degrees (north +ve)
                  RDER(1,551)  R*4 IN Omega longitude degrees (east +ve)
               or RDER(1,541)  R*4 IN INU latitude degrees (north +ve)
               or RDER(1,542)  R*4 IN INU longitude degrees (east +ve)
                  RCONST(1)    R*4 IN Day in month (1-31)
                  RCONST(2)    R*4 IN Month in year (1-12)
                  RCONST(3)    R*4 IN Year (eg 1984)
                  RDER(1,642)  R*4 OUT Solar azimuth in degrees
                  RDER(1,643)  R*4 OUT Solar zenith in degrees

 SUBPROGRAMS      S_SUN , ITSTFLG, ISETFLG

 CHANGES          01 Range checks for input data now done in S_SUN 
                     RWS 30/10/90
                1.02 Check added if time RSECS has reached midnight and
                     if so to reduce RSECS to less than 86400 s and increase
                     the date.  JAS 1/5/92
                1.03 Following the demise of the Omega, now uses INU position
                     for flights after 30/09/97.  Note that this routine is
                     now always called by CALIBRATE, even if neither Omega or
                     INU were available.  WDNJ 20/10/97
                1.04 Now strips flags from data before use.  WDNJ 22/12/97
                1.05 Can take GIN input 05/09/07
                1.06 Changes made how lon/lat input is derived AxW 29/03/10
#########################################################################

"""
    def __init__(self,dataset):
        self.input_names=['DATE', 'SECS_GIN', 'LAT_GIN', 'LON_GIN']
        self.outputs=[parameter('SOL_AZIM',units='degree',frequency=1,number=642,long_name='Solar azimuth derived from aircraft position and time.')
                     ,parameter('SOL_ZEN',units='degree',frequency=1,number=643,long_name='Solar zenith derived from aircraft position and time.')]
        #self.name='SUN'
        self.fortname='GSUN'
        self.version=1.00
        fort_cal.__init__(self,dataset)
