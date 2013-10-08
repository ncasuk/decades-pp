#from ppodd.pod import *
from ppodd.core import *
class gwinds(fort_cal):
    """
FORTRAN routine C_GWINDS

 ROUTINE          C_GWINDS SUBROUTINE FORTVAX

 PURPOSE          Computes raw winds from TAS, vanes, and INS data

 DESCRIPTION      Computes values of the three wind components, using true
                  airspeed, angle of attack and sideslip, and INS velocity,
                  attitude, and attitude rate information. Note that at this
                  stage the INS data have not been corrected for drift, so
                  these are 'raw' winds, which will normally be corrected
                  later as part of the interactive renavigation processing. 
                  Once errors have been evaluated for the three INS velocity
                  components, they can be applied directly to the three wind
                  components; the wind components do not need to be recomputed 
                  from scratch.  To show that the winds are 'raw' all values
                  of U, V and W are increased by 1000 m/s by this routine.  
                  This makes it easy to see that normal (flagged 0 or 1) data
                  are 'raw', but it may not be enough to say unabiguously 
                  whether data that are already bad (flagged 2 or 3) are 'raw'
                  or 'corrected'.

                  The processing will handle the case that the INS is mounted
                  off the boom axis, provided its position is specified in
                  the flight constants file, using the INSPOSN keyword.  If
                  the INS position is not specified then it is assumed to be
                  in the nose bay, 7.06m behind the vanes, but on the axis of
                  the boom.  All data is assumed to be at 32 Hz.
                  
                  This routine will not be called if there is no True
                  Airspeed, or no INS information (with the exception of roll
                  rate).  If there is no information from the angle of attack
                  and sideslip vanes, winds will be computed using values of
                  zero for these angles flagged with
                  1's.  If there is no roll rate available (this wasn't
                  recorded for the Ferranti 1012 INS), a value of 0 is used. 
                  This doesn't matter if the INS is located on the boom axis,
                  since in this case roll rate has no effect on winds.
                  
                  The output vertical wind takes the worst flag present on the
                  AOA, VZ, TAS and pitch data.  The output horizontal wind
                  components take the worst flag present on the AOSS, VN, VE,
                  TAS, and heading data.  This is suitable when the
                  aircraft is not banking and reflects the fact that good
                  horizontal winds can be found even when the vertical
                  velocity is bad.  However this flagging scheme fails to 
                  reflect coupling between the vertical and horizontal
                  measurement when the aircraft is banking.
                  In addition horizontal wind components greater
                  than 100 m/s and vertical components greater than 25 m/s
                  are flagged with 2's, and if the change between adjacent
                  samples (at 32 Hz) is greater than 1 m/s a flag of 2 is
                  also applied.

                  Input parameters (all at 32 Hz except 515):

                  Para 515   Time, secs
                  Para 779   Turb.probe dry true airspeed, m s-1
                  Para 548   Angle of attack, deg
                  Para 549   Angle of side slip, deg
                  Para 558   INS velocity north, m s-1
                  Para 559   INS velocity east, m s-1
                  Para 557   INS vertical velocity, m s-1
                  Para 560   INS roll, deg
                  Para 561   INS pitch, deg
                  Para 562   INS heading, deg
                  Para 567   INS roll rate, deg s-1 (optional)
                  Para 565   INS pitch rate, deg s-1
                  Para 566   INS yaw rate, deg s-1

                  Constants:

                  RCONST(1)  Distance of vanes ahead of INS, m (optional)
                  RCONST(2)  Distance of vanes to port of INS, m (optional)
                  RCONST(3)  Distance of vanes above INS, m (optional)

                  Output parameters (all at 32 Hz):

                  Para 714   Northward wind component + 1000, m s-1
                  Para 715   Eastward wind component + 1000, m s-1
                  Para 716   Vertical wind component + 1000, m s-1
            
 VERSION          1.00  10-5-93  W.D.N.JACKSON

 ARGUMENTS        IRAW(64,512) I*4 IN  Up to 64 samples for up to 512 DRS pars
                  IFRQ(512)    I*4 IN  Sample rate of each DRS par (0-64)
                  RCONST(64)   R*4 IN  Inputs constants
                  RDER(64,1024)R*4 OUT Output array of up to 64 samples for
                                       each of 1024 parameters

 CHANGES          1.01  20-04-98 W.D.N.JACKSON
                  Error in computation of airspeed corrected. 
                  1.02  14-06-2004 Phil Brown
                  AoA and AoSS now compulsory input parameters to ensure
                  this routine gets called after C_TURB
                  1.03  09/07/04 Phil Brown
                  Input TAS parameter is now 779 (Turb.probe dry TAS)
                  1.04  25/08/04 Phil Brown
                  Temporary. Suspend rate-of-change checking on winds.
                  1.05  29/11/04 Phil Brown
                  Temporary. Check flagging of RU,RV,RW when returned to try
                  to suppress FLTINV errors.
                  1.06  05/09/07 Dave Tiddeman
                  Will use GIN inputs if available rather than INU


"""
    def __init__(self,dataset):
        self.input_names=['INSPOSN', 'SECS_GIN', 
        'TAS', 'VELN_GIN', 'VELE_GIN', 'VELD_GIN', 'ROLL_GIN', 'PTCH_GIN', 'HDG_GIN', 'ROLR_GIN', 'PITR_GIN', 'HDGR_GIN', 
        'AOA', 'AOSS']
        self.outputs=[parameter('V_C',units='m s-1',frequency=32,number=714,long_name='Northward wind component from turbulence probe and GIN')
                     ,parameter('U_C',units='m s-1',frequency=32,number=715,long_name='Eastward wind component from turbulence probe and GIN')
                     ,parameter('W_C',units='m s-1',frequency=32,number=716,long_name='Vertical wind component from turbulence probe and GIN')]
        #self.name='GWINDS'
        self.version=1.00
        fort_cal.__init__(self,dataset)
