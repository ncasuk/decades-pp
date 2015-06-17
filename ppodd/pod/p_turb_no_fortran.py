
from ppodd.core import *
import numpy as np


def py__s10_pecorr(amach):
    a0,a1,a2 = -0.011482,-0.148295,0.407040
    dcp_s10 = a0+amach*(a1+amach*a2)
    return dcp_s10


def py__g_mach(rstat,pitot):
    rmach = np.sqrt( 5.* ((1.+ pitot/rstat)**(2./7.) - 1.))
    return rmach


def py__c_turb(ttdi,ttnd,spr,psp,tp0,tpa,tpb,rconst, itermax=None):
#def py__c_turb(ttdi,ttnd,spr,psp,tp0,tpa,tpb,rconst, itermax=None):
    """
    Input:
      ias:  Indicated air speed
      ttdi: deiced temperature
      ttnd: non-deiced temperature
    """

    #define default maximum iteration
    if not itermax:
        itermax=6

    #disabloe error message when dividing by zero
    np.seterr(divide='ignore', invalid='ignore')
    
    q=psp
    amach=py__g_mach(spr,q)
    #a0 = rconst[0]+amach*(rconst[1]+amach*rconst[2])
    #a1 = rconst[3]+amach*(rconst[4]+amach*rconst[5])
    #b0 = rconst[6]+amach*(rconst[7]+amach*rconst[8])
    #b1 = rconst[9]+amach*(rconst[10]+amach*rconst[11])

    #aoa  = (tpa/q - a0)/a1
    #aoss = (tpb/q - b0)/b1

    #dcp_s10 = py__s10_pecorr(dcp_s10,amach)
    #Apparently the following line is not needed double check with the FORTRAN code
    #the variables dcp_s10 is never used in
    #dcp_s10 = py__s10_pecorr(amach)

    #dcpa = 0.0273+ aoa*(-0.0141+ aoa*(0.00193- aoa*5.2E-5))
    #dcpb = 0.0   +aoss*(0.0    + aoss*7.6172E-4)

    #q = tp0+(dcpa+dcpb)*q
    #q = p0

    #amach = py__g_mach(spr,q)

    #daoa = ias*0.0 + 999.0
    #daoss = ias*0.0 + 999.0
    #tol  = rconst[12]

    iter = 0
    #while ((daoa > tol) or (daoss > tol)) and (iter < itermax):
    #while np.any(np.where(((daoa > tol) | (daoss > tol)) & (iter < itermax))):
    while iter < itermax:
        #ix = np.where(((daoa > tol) | (daoss > tol)))[0]
        #print(iter,  ix.size)
        iter += 1
        a0 = rconst[0]+amach*(rconst[1]+amach*rconst[2])
        a1 = rconst[3]+amach*(rconst[4]+amach*rconst[5])
        b0 = rconst[6]+amach*(rconst[7]+amach*rconst[8])
        b1 = rconst[9]+amach*(rconst[10]+amach*rconst[11])

        aoa  = (tpa/q - a0)/a1
        aoss = (tpb/q - b0)/b1

        #daoa[ix] = (np.abs(aoanew-aoa))[ix]
        #daoss[ix]= (np.abs(aossnew-aoss))[ix]

        #aoa[ix] = aoanew[ix]
        #aoss[ix] = aossnew[ix]

        #dcp_s10 = py__s10_pecor(dcp_s10,amach)
        #Apparently the following line is not needed double check with the FORTRAN code
        #the variables dcp_s10 is never used in
        #dcp_s10 = py__s10_pecorr(amach)
        dcpa = 0.0273+ aoa*(-0.0141+ aoa*(0.00193- aoa*5.2E-5))
        dcpb = 0.0   +aoss*(0.0    + aoss*7.6172E-4)

        q = tp0+(dcpa+dcpb)*q
        #q = p0
        amach = py__g_mach(spr,q)
        
    #TODO: Should use non deiced temperature when ttdi is flagged    
    tas = rconst[13] * 340.294 * amach * np.sqrt(ttdi/288.15)
    aoa = aoa*rconst[15] + rconst[14]
    aoss = aoss*rconst[17] + rconst[16]
    tpsp = q
    #548,549,779,781
    #print('iter='+str(iter))
    return(aoa,aoss,tas,tpsp)



class turb_no_fortran(cal_base):
    """
FORTRAN routine C_TURB

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

"""

    def __init__(self,dataset):
        self.input_names=['AOA_A0', 'AOA_A1', 'AOSS_B0', 'AOSS_B1', 'TOLER', 'TASCOR1', 'ALPH0', 'ALPH1', 'BET0', 'BET1', 'IAS_RVSM', 'TAT_DI_R', 'TAT_ND_R', 'PS_RVSM', 'Q_RVSM', 'PALT_RVS', 'P0_S10', 'PA_TURB', 'PB_TURB', 'TBPC', 'TBPD']
        self.outputs=[parameter('AOA_NO_FORTRAN',units='degree',frequency=32,number=5480,long_name='Angle of attack from the turbulence probe (positive, flow upwards wrt a/c axes)'),
                      parameter('AOSS_NO_FORTRAN',units='degree',frequency=32,number=5490,long_name='Angle of sideslip from the turbulence probe (positive, flow from left)'),
                      parameter('TAS_NO_FORTRAN',units='m s-1',frequency=32,number=7790,long_name='True airspeed (dry-air) from turbulence probe',standard_name='platform_speed_wrt_air'),
                      parameter('PSP_TURB_NO_FORTRAN',units='hPa',frequency=32,number=7810,long_name='Pitot-static pressure from centre-port measurements corrrected for AoA and AoSS')]
        #self.name='TURB'
        self.version=1.00
        cal_base.__init__(self,dataset)

    def process(self):
        d=self.dataset

        match=d.matchtimes(['TAT_DI_R', 'TAT_ND_R', 'PS_RVSM', 'Q_RVSM', 'P0_S10', 'PA_TURB', 'PB_TURB'])

        rconst=d['AOA_A0'].data + 
               d['AOA_A1'].data + 
               d['AOSS_B0'].data + 
               d['AOSS_B1'].data + 
               d['TOLER'].data + 
               d['TASCOR1'].data + 
               d['ALPH0'].data + 
               d['ALPH1'].data + 
               d['BET0'].data + 
               d['BET1'].data

        #ias_rvsm=d['IAS_RVSM'].data.ismatch(match)
        tat_di_r=d['TAT_DI_R'].data.ismatch(match)
        tat_nd_r=d['TAT_ND_R'].data.ismatch(match)
        ps_rvsm=d['PS_RVSM'].data.ismatch(match)
        q_rvsm=d['Q_RVSM'].data.ismatch(match)
        #palt_rvs=d['PALT_RVS'].data.ismatch(match)
        p0_s10=d['P0_S10'].data.ismatch(match)
        pa_turb=d['PA_TURB'].data.ismatch(match)
        pb_turb=d['PB_TURB'].data.ismatch(match)
        #tbpc=d['TBPC'].data.ismatch(match)
        #tbpd=d['TBPD'].data.ismatch(match)

        aoa,aoss,tas,tpsp = py__c_turb(np.array(tat_di_r),
                                       np.array(tat_nd_r),
                                       np.array(ps_rvsm),
                                       np.array(q_rvsm),
                                       np.array(p0_s10),
                                       np.array(pa_turb),
                                       np.array(pb_turb),
                                       rconst)
        
        #TODO: Flagging needs to be done
        flag=np.zeros(aoa.size, dtype=np.int8) 
        #times=ias_rvsm.times
        times=tat_di_r.times
        aoa=flagged_data(aoa, times, flag)
        aoss=flagged_data(aoss, times, flag)
        tas=flagged_data(tas, times, flag)
        tpsp=flagged_data(tpsp, times, flag)

        self.outputs[0].data=aoa
        self.outputs[1].data=aoss
        self.outputs[2].data=tas
        self.outputs[3].data=tpsp


