from ppodd.core import *

import numpy as np

def py__c_winds_uvw(rtas,raoa,raos,rvn,rve,rvz,rhdg,rpit,rrol,rl,rm,rn,ryawr,rpitr,rrolr,ru,rv,rw):

    rp=np.zeros((3),dtype=np.float32)
    rwind=np.zeros((3),dtype=np.float32)
    rvg=np.zeros((3),dtype=np.float32)
    ra1=np.zeros((3,3),dtype=np.float32)
    ra2=np.zeros((3,3),dtype=np.float32)
    ra3=np.zeros((3,3),dtype=np.float32)
    rt=np.zeros((3,3),dtype=np.float32)
    rtmp=np.zeros((3,3),dtype=np.float32)
    rua=np.zeros((3),dtype=np.float32)
    ryr=np.zeros((3),dtype=np.float32)
    rpr=np.zeros((3),dtype=np.float32)
    rrr=np.zeros((3),dtype=np.float32)
    rtemp=np.zeros((3),dtype=np.float32)

    ra1=np.array([[ 1.0,                       0.0,                       0.0],
                  [ 0.0,  np.cos(np.deg2rad(rrol)),  np.sin(np.deg2rad(rrol))],
                  [ 0.0, -np.sin(np.deg2rad(rrol)),  np.cos(np.deg2rad(rrol))]])

    ra2=np.array([[ np.cos(np.deg2rad(rpit)), 0.0,  np.sin(np.deg2rad(rpit))],
                  [                      0.0, 1.0,                       0.0],
                  [-np.sin(np.deg2rad(rpit)), 0.0,  np.cos(np.deg2rad(rpit))]])

    ra3=np.array([[np.cos(np.deg2rad(rhdg)), -np.sin(np.deg2rad(rhdg)), 0.0],
                  [np.sin(np.deg2rad(rhdg)),  np.cos(np.deg2rad(rhdg)), 0.0],
                  [                     0.0,                       0.0, 1.0]])

    tanaos=np.tan(np.deg2rad(raos))                #Define airspeed vector
    tanaoa=np.tan(np.deg2rad(raoa))
    d=np.sqrt(1.0+(tanaos*tanaos)+(tanaoa*tanaoa))
    rua[0]=-rtas/d
    rua[1]=-rtas*tanaos/d
    rua[2]=rtas*tanaoa/d

    #rp=np.array([rl, rm, rn])
    rp=np.array([16.002, -0.8128, -0.4390])
    rvg=np.array([rvn,-rve,rvz])
    ryr=np.array([0.0, 0.0, -np.deg2rad(ryawr)])
    #ryr=np.array([0.0000000E+00,  0.0000000E+00, -4.0540417E-05])
    rpr=np.array([0.0, -np.deg2rad(rpitr), 0.0])
    rrr=np.array([np.deg2rad(rrolr), 0.0, 0.0])
    rwind=np.array([0.0, 0.0, 0.0])

    rtmp=py__c_winds_mulm(ra3,ra2)       #Compute full transformation vector
    rt=py__c_winds_mulm(rtmp,ra1)
    rua=py__c_winds_matv(rt, rua)        #Transform airspeed to ground frame
    rwind=py__c_winds_vadd(rua, rwind)   #This is first wind component
    rwind=py__c_winds_vadd(rvg, rwind)   #Add ground speed component
    rp=py__c_winds_matv(rt, rp)          #Transform INS offset to ground frame
    rtmp=py__c_winds_mulm(ra3, ra2)      #Transfm roll rate effects to ground fram
    rrr=py__c_winds_matv(rtmp, rrr)      #Compute roll rate effects
    rtemp=py__c_winds_matv(ra3, rpr)     #Transfm pitch rate effects to ground frm
    rtemp=py__c_winds_vadd(rrr, rtemp)   #Add pitch rate effects
    rtemp=py__c_winds_vadd(ryr, rtemp)   #Add yaw rate effects to get full effect
    rtemp=py__c_winds_vmul(rtemp, rp)    #Apply rate effects to INS offset vector
    rwind=py__c_winds_vadd(rtemp, rwind) #This is the last wind component
    print(rtemp)
    ru=rwind[0]                          #Transfer result to output arguments
    rv=-rwind[1]                         #Convert westwards to eastwards
    rw=rwind[2]
    return (rv,ru,rw)


def py__c_winds_mulm(a,b):
    c=np.zeros((3,3),dtype=np.float32)
    c[0,0]=a[0,0]*b[0,0]+a[1,0]*b[0,1]+a[2,0]*b[0,2]
    c[1,0]=a[0,0]*b[1,0]+a[1,0]*b[1,1]+a[2,0]*b[1,2]
    c[2,0]=a[0,0]*b[2,0]+a[1,0]*b[2,1]+a[2,0]*b[2,2]
    c[0,1]=a[0,1]*b[0,0]+a[1,1]*b[0,1]+a[2,1]*b[0,2]
    c[1,1]=a[0,1]*b[1,0]+a[1,1]*b[1,1]+a[2,1]*b[1,2]
    c[2,1]=a[0,1]*b[2,0]+a[1,1]*b[2,1]+a[2,1]*b[2,2]
    c[0,2]=a[0,2]*b[0,0]+a[1,2]*b[0,1]+a[2,2]*b[0,2]
    c[1,2]=a[0,2]*b[1,0]+a[1,2]*b[1,1]+a[2,2]*b[1,2]
    c[2,2]=a[0,2]*b[2,0]+a[1,2]*b[2,1]+a[2,2]*b[2,2]
    
    #c=np.float32(np.dot(b,a))
    return c


def py__c_winds_matv(a,b):
    c=np.zeros((3),dtype=np.float32)
    c[0]=a[0,0]*b[0]+a[1,0]*b[1]+a[2,0]*b[2]
    c[1]=a[0,1]*b[0]+a[1,1]*b[1]+a[2,1]*b[2]
    c[2]=a[0,2]*b[0]+a[1,2]*b[1]+a[2,2]*b[2]
    
    #c=np.float32(np.dot(b, a))
    return c


def py__c_winds_vadd(a,b):
    #c=np.zeros((3),dtype=np.float32)
    c=a+b
    return c


def py__c_winds_vmul(a,b):
    c=np.zeros((3),dtype=np.float32)
    c[0]=(a[1]*b[2])-(a[2]*b[1])
    c[1]=(a[2]*b[0])-(a[0]*b[2])
    c[2]=(a[0]*b[1])-(a[1]*b[0])
    
    #c=np.float32((np.roll(a, -1)*np.roll(b, -2))-(np.roll(a, -2)*np.roll(b,-1)))
    return c










def wind_calculations(vn, ve, vz, rol, pit, hdg, rolr, pitr, hdgr, tas, aoa, aoss):
    n=vn.size
    a3=np.zeros((n,3,3), dtype=np.float32)
    a2=np.zeros((n,3,3), dtype=np.float32)
    a1=np.zeros((n,3,3), dtype=np.float32)

    U=np.zeros((n,1,3), dtype=np.float32)

    ryr=np.zeros((n, 1, 3), dtype=np.float32)
    rpr=np.zeros((n, 1, 3), dtype=np.float32)
    rrr=np.zeros((n, 1, 3), dtype=np.float32)


    tanaos=np.tan(np.deg2rad(aoss))                #Define airspeed vector
    tanaoa=np.tan(np.deg2rad(aoa))
    d=np.sqrt(1.0+(tanaos*tanaos)+(tanaoa*tanaoa))
    U[:,0,0]=-tas/d
    U[:,0,1]=-tas*np.tan(np.deg2rad(aoss))/d
    U[:,0,2]=tas*np.tan(np.deg2rad(aoa))/d

    V=np.zeros((n,1,3), dtype=np.float32)
    V[:,0,0]=vn
    V[:,0,1]=-ve
    V[:,0,2]=-vz

    a1[:,0,0]=1.0
    a1[:,1,1]=np.cos(np.deg2rad(rol))
    a1[:,1,2]=np.sin(np.deg2rad(rol))
    a1[:,2,1]=-np.sin(np.deg2rad(rol))
    a1[:,2,2]=np.cos(np.deg2rad(rol))

    a2[:,0,0]=np.cos(np.deg2rad(pit))
    a2[:,0,2]=np.sin(np.deg2rad(pit))
    a2[:,1,1]=1.0
    a2[:,2,0]=-np.sin(np.deg2rad(pit))
    a2[:,2,2]=np.cos(np.deg2rad(pit))

    a3[:,0,0]=np.cos(np.deg2rad(hdg))
    a3[:,0,1]=-np.sin(np.deg2rad(hdg))
    a3[:,1,0]=np.sin(np.deg2rad(hdg))
    a3[:,1,1]=np.cos(np.deg2rad(hdg))
    a3[:,2,2]=1.0

    ryr[:,0,2]=-np.deg2rad(hdgr)
    rpr[:,0,1]=-np.deg2rad(pitr)
    rrr[:,0,0]=np.deg2rad(rolr)

    rp=np.zeros((n,1,3), dtype=np.float32)
    rp[:,:,:]= np.array([16.002, -0.8128, -0.4390])

    #https://groups.google.com/forum/#!topic/numpy/Ta25NNEf3hE
    a3_dot_a2=np.einsum('...ij,...jk->...ik', a2, a3)
    a3_dot_a2_dot_a1=np.einsum('...ij,...jk->...ik', a1, a3_dot_a2)
    U_=np.einsum('...ij,...jk->...ik', U, a3_dot_a2_dot_a1)

    rp=np.einsum('...ij,...jk->...ik', rp, a3_dot_a2_dot_a1)

    rrr_=np.einsum('...ij,...jk->...ik', rrr, a3_dot_a2_dot_a1)
    rpr_=np.einsum('...ij,...jk->...ik', rpr, a3)

    c=rpr_+rrr_+ryr

    corrections=np.cross(c, rp)
    corrections=corrections[:,0,:]
    corrections=corrections[:, np.newaxis, :]

    wind_vectors=V+U_+corrections
    wind_vectors[:,:,1]*=-1.0

    return (a3_dot_a2_dot_a1, V, U_, wind_vectors, corrections)










class gwinds_no_fortran(cal_base):
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
        self.input_names=['INSPOSN', 'SECS_GIN', 'TAS_NO_FORTRAN', 'VELN_GIN', 'VELE_GIN', 'VELD_GIN', 'ROLL_GIN', 'PTCH_GIN', 'HDG_GIN', 'ROLR_GIN', 'PITR_GIN', 'HDGR_GIN', 'AOA_NO_FORTRAN', 'AOSS_NO_FORTRAN']
        self.outputs=[parameter('V_C_NO_FORTRAN',units='m s-1',frequency=32,number=7140,long_name='Northward wind component from turbulence probe and GIN',standard_name='northward_wind'),
                      parameter('U_C_NO_FORTRAN',units='m s-1',frequency=32,number=7150,long_name='Eastward wind component from turbulence probe and GIN',standard_name='eastward_wind'),
                      parameter('W_C_NO_FORTRAN',units='m s-1',frequency=32,number=7160,long_name='Vertical wind component from turbulence probe and GIN',standard_name='upward_air_velocity')]
        #self.name='GWINDS'
        self.version=1.00
        cal_base.__init__(self,dataset)



    def process(self):
        d=self.dataset
        match=d.matchtimes(self.input_names)
        
        tas=d['TAS_NO_FORTRAN'].data.ismatch(match)
        aoa=d['AOA_NO_FORTRAN'].data.ismatch(match)
        aoss=d['AOSS_NO_FORTRAN'].data.ismatch(match)
        vn=d['VELN_GIN'].data.ismatch(match)
        ve=d['VELE_GIN'].data.ismatch(match)
        vz=d['VELD_GIN'].data.ismatch(match)
        hdg=d['HDG_GIN'].data.ismatch(match)
        pit=d['PTCH_GIN'].data.ismatch(match)
        rol=d['ROLL_GIN'].data.ismatch(match)

        #yawr=d['HDGR_GIN'].data.ismatch(match)
        hdgr=d['HDGR_GIN'].data.ismatch(match)
        pitr=d['PITR_GIN'].data.ismatch(match)
        rolr=d['ROLR_GIN'].data.ismatch(match)

        shape=tas.shape
        #print(shape)
        #rl=np.zeros(shape, dtype=np.float32)
        #rm=np.zeros(shape, dtype=np.float32)
        #rn=np.zeros(shape, dtype=np.float32)


        #ru=np.zeros(tas.shape, dtype=np.float32)
        #rv=np.zeros(tas.shape, dtype=np.float32)
        #rw=np.zeros(tas.shape, dtype=np.float32)

        #u=np.zeros(tas.shape, dtype=np.float32)
        #v=np.zeros(tas.shape, dtype=np.float32)
        #w=np.zeros(tas.shape, dtype=np.float32)

        a3_dot_a2_dot_a1, V, U_, wind_vectors, corrections=wind_calculations(vn.ravel(), ve.ravel(), vz.ravel(), rol.ravel(), pit.ravel(), hdg.ravel(), rolr.ravel(), pitr.ravel(), hdgr.ravel(), tas.ravel(), aoa.ravel(), aoss.ravel())

        u=wind_vectors[:,:,1].ravel().reshape(shape)
        v=wind_vectors[:,:,0].ravel().reshape(shape)
        w=wind_vectors[:,:,2].ravel().reshape(shape)
        
        flag=np.zeros(u.shape)
        times=tas.times
        u=flagged_data(u, times, flag)
        v=flagged_data(v, times, flag)
        w=flagged_data(w, times, flag)
        self.outputs[0].data=v
        self.outputs[1].data=u
        self.outputs[2].data=w

