#from ppodd.pod import *
from ppodd.core import *
class turb(fort_cal):
    """
FORTRAN routine C_TURB
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         ROUTINE      C_TURB

         PURPOSE      To calibrate and apply designated correction factors to
                      angle of attack (AOA), angle of sideslip (AOSS) and the
                      centre-static differential pressure (to derive TAS)).

         DESCRIPTION  Calibration of AOA and AOSS is assumed to be of the form:
                      
                      PA/q = a0(M) + a1(M)*alpha
                      PB/q = b0(M) + b1(M)*beta
                      where q is the pitot(dynamic) pressure.
                      Calculations follow the scheme described in BAES doc
                      ADE-46S-R-463-34 1233 (Page 78 of 116).
                      Initial value of pitot pressure is taken from RVSM and
                      used to calculate first guess AOA and AOSS. These are
                      to derive corrections to the centre-port along with 
                      separate calculation of static position error in the
                      centre-port measurement. AOA and AOSS are recalculated
                      with iteration continuing until specified tolerance is
                      achieved or max.iteration count exceeded. Corrected
                      centre-port pressure is then used to calculate TAS
                      (currently only the dry value) using:

                      TAS = Corrtn.fac * 340.294*M*SQRT(T/288.15)

         VERSION      1.01   Phil Brown 24/5/2004

         CHANGES      1.02   Phil Brown 11/6/2004
                             Logic changed to reproduce PVWAVE test version
                             MRFB:[BROWN.PVWAVE]TURB.PRO at this date
                      1.03   Phil Brown 28/6/2004
                             Check flags and values following return of calls
                             to S_MACH. Unacceptable causes C_TURB to return
                             its default values of output parameters (flag 3)
                      1.04   Phil Brown 2/7/04
                             Uses G_MACH routine to calculate Mach no. and
                             avoid complications due to flagging.
                      1.05   Phil Brown 08/07/04
                             Uses simpler Mach-dependent PE.Corrtn derived
                             empirically from B001-012 s&l legs.
                      1.06   Phil Brown 09/07/04
                             No position error correction currently applied
                             to P0 differential pressure.
                      1.07   Phil Brown 26/08/04
                             Change sign of AOSS. Cals were done against INS
                             drift angle (-ve for +ve AOSS).
                      1.08   Phil Brown 27/8/04
                             AOSS calcs revert to original, but assumed to use
                             new fit coefficients for B0 and B1
                      1.09   26/01/06 Phil Brown
                             Min/max limits provided for AoA, AoSS and TAS for
                             flagging purposes.
                      1.10   20/06/06 Phil Brown
                             Takes additional input of non-deiced temp, used as
                             alternative when de-iced is flagged 2 or more.
                      1.11   24/10/06 Phil Brown
                             Fix bug setting flag on TTND to zero before use.
                             Define 4 additional flight constants to apply
                             fudge factors to the calculated AoA / AoSS
                             These have the form:
                             AoA_new = AoA * ALPH1 + ALPH0
                             AoSS_new= AoSS * BET1 + BET0
                      1.12   08/10/2010 Axel Wellpott
                             added line "DATA TAS/-9999./"
                             Missing TAS data values were set to -999.
                             and not to -9999. as specified in the netcdf
                             files.

         SUBROUTINES: S10_PECORR, ITSTFLG, ISETFLG, G_MACH

         FILES        

         REFERENCES 

         CONSTANTS 
           RCONST(1-3) Coefficients of 2nd order polynomial in Mach to 
                       calculate AOA offset, A0 
           RCONST(4-6) Coefficients of 2nd order polynomial in Mach to 
                       calculate AOA sensitivity, A1 
           RCONST(7-9) Coefficients of 2nd order polynomial in Mach to 
                       calculate AOSS offset, B0 
           RCONST(10-12) Coefficients of 2nd order polynomial in Mach to 
                       calculate AOSS sensitivity, B1
           RCONST(13)  Tolerance for AOA/AOSS iteration
           RCONST(14)  True Airspeed correction factor (fudge factor to 
                       remove residual along-heading wind errors).
           RCONST(15-16) Coefficients of linear correction to calculated AoA
           RCONST(17-18) Coefficients of linear correction to calculated AoSS
  
         INPUT PARAMETERS
           516  IAS   32Hz
           520  TTDI  32Hz
           525  TTND  32Hz
           576  SPR   32Hz
           577  PSP   32Hz
           578  PHGT  32Hz
           773  TBP0  32Hz
           774  TBPA  32Hz
           775  TBPB  32Hz
           776  TBPC  32Hz
           777  TBPD  32Hz

         OUTPUT PARAMETERS
           
           548  AOA   32Hz  deg
           549  AOSS  32Hz  deg
           779  TASD  32Hz  ms-1
           780  TASW  32Hz  ms-1
           781  TPSP  32Hz  mb
 
         TURBULENCE PROBE CONSTANT KEYWORDS


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

"""
    def __init__(self,dataset):
        self.input_names=['AOA_A0', 'AOA_A1', 'AOSS_B0', 'AOSS_B1', 'TOLER', 'TASCOR1', 'ALPH0', 'ALPH1', 'BET0', 'BET1', 'IAS_RVSM', 'TAT_DI_R', 'TAT_ND_R', 'PS_RVSM', 'Q_RVSM', 'PALT_RVS', 'P0_S10', 'PA_TURB', 'PB_TURB', 'TBPC', 'TBPD']
        self.outputs=[parameter('AOA',units='degree',frequency=32,number=548,long_name='Angle of attack from the turbulence probe (positive, flow upwards wrt a/c axes)')
                     ,parameter('AOSS',units='degree',frequency=32,number=549,long_name='Angle of sideslip from the turbulence probe (positive, flow from left)')
                     ,parameter('TAS',units='m s-1',frequency=32,number=779,long_name='True airspeed (dry-air) from turbulence probe')
                     ,parameter('TASW',units='MS-1',frequency=32,number=780,long_name='TURB PROBE WET TAS')
                     ,parameter('PSP_TURB',units='hPa',frequency=32,number=781,long_name='Pitot-static pressure from centre-port measurements corrrected for AoA and AoSS')]
        #self.name='TURB'
        self.version=1.00
        fort_cal.__init__(self,dataset)
