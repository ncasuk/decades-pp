import ppodd
from ppodd.core import *
from os.path import getsize
from ppodd.resample import createtimes

class gin(cal_base):
    """ GIN processing

 Purpose:  Resample GIN data to 32Hz so it matches the time frame of turbulence and temperature data

 Description:  Use numpy.interpolation to resample frequency from 50 to 32Hz
 Missing out times where the gap is greater than 0.5 sec ( see resample.createtimes )
              
@author: Dave Tiddeman
    """
    def __init__(self,dataset):
        self.input_names=['DATE','GINDAT_time1','GINDAT_lat','GINDAT_lon','GINDAT_alt',
                          'GINDAT_veln','GINDAT_vele','GINDAT_veld',
                          'GINDAT_roll','GINDAT_ptch','GINDAT_hdg',
                          'GINDAT_wand','GINDAT_trck','GINDAT_gspd',
                          'GINDAT_rolr','GINDAT_pitr','GINDAT_hdgr',
                          'GINDAT_aclf','GINDAT_acls','GINDAT_acld',
                          'GINDAT_status']
        self.outputs=[
parameter('LAT_GIN',units='degree_north',frequency=32,number=610,long_name='Latitude from POS AV 510 GPS-aided Inertial Navigation unit'),
parameter('LON_GIN',units='degree_east',frequency=32,number=611,long_name='Longitude from POS AV 510 GPS-aided Inertial Navigation unit'),
parameter('ALT_GIN',units='m',frequency=32,number=612,long_name='Altitude from POS AV 510 GPS-aided Inertial Navigation unit'),
parameter('VELN_GIN',units='m s-1',frequency=32,number=613,long_name='Aircraft velocity north from POS AV 510 GPS-aided Inertial Navigation unit'),
parameter('VELE_GIN',units='m s-1',frequency=32,number=614,long_name='Aircraft velocity east from POS AV 510 GPS-aided Inertial Navigation unit'),
parameter('VELD_GIN',units='m s-1',frequency=32,number=615,long_name='Aircraft velocity down from POS AV 510 GPS-aided Inertial Navigation unit'),
parameter('ROLL_GIN',units='degree',frequency=32,number=616,long_name='Roll angle from POSAV GPS-aided Inertial Nav. unit (positive for left wing up)'),
parameter('PTCH_GIN',units='degree',frequency=32,number=617,long_name='Pitch angle from POSAV GPS-aided Inertial Nav. unit (positive for nose up)'),
parameter('HDG_GIN',units='degree',frequency=32,number=618,long_name='Headingfrom POSAV GPS-aided Inertial Navigation unit'),
parameter('WAND_GIN',units='deg s-1',frequency=32,number=619,long_name='GIN wander angle'),
parameter('TRCK_GIN',units='degree',frequency=32,number=620,long_name='Aircraft track angle POSAV GPS-aided Inertial Navigation unit'),
parameter('GSPD_GIN',units='m s-1',frequency=32,number=621,long_name='Groundspeed from POS AV 510 GPS-aided Inertial Navigation unit'),
parameter('ROLR_GIN',units='degree s-1',frequency=32,number=622,long_name='rate-of-change of GIN roll angle'),
parameter('PITR_GIN',units='degree s-1',frequency=32,number=623,long_name='rate-of-change of GIN pitch angle'),
parameter('HDGR_GIN',units='degree s-1',frequency=32,number=624,long_name='rate-of-change of GIN heading'),
parameter('ACLF_GIN',units='m s-2',frequency=32,number=625,long_name='Acceleration along the aircraft longitudinal axis (GIN) (positive forward)'),
parameter('ACLS_GIN',units='m s-2',frequency=32,number=626,long_name='Acceleration along the aircraft transverse axis (GIN) (positive starboard)'),
parameter('ACLD_GIN',units='m s-2',frequency=32,number=627,long_name='Acceleration along the aircraft vertical axis (GIN) (positive down)'),
parameter('SECS_GIN',units='s',frequency=1,number=515,long_name='Gin time secs past midnight')]
        cal_base.__init__(self,dataset)
   
    def process(self):
        ginhdgoffset=0.0
        if('GINHDGOFFSET' in self.dataset):
            ginhdgoffset=self.dataset['GINHDGOFFSET'][0]                        
        if(self.dataset['GINDAT_time1'].data!=None):
            tgin=self.dataset['GINDAT_time1'].times
            #tstep=timestamp((np.round(tgin[0]),np.round(tgin[-1])))
            tstep=timestamp(createtimes(tgin))
            tout=tstep.at_frequency(32)
            sh=tout.shape
            flg=self.dataset['GINDAT_status'][:]/3
            zero=(self.dataset['GINDAT_lat']==0) & (self.dataset['GINDAT_lon']==0) & (flg<2)
            flg[zero]=2
            flagg=timed_data(flg,tgin)        
            flagg.interp1d()
            flags=np.int8(flagg.interpolated(tout.ravel()).reshape(sh))
            for o in self.outputs:
                ppodd.logger.info('Interpolating %s' % str(o))
                if(o.name=='SECS_GIN'):
                   o.data=timed_data(tstep,tstep)
                else:
                    name='GINDAT_'+(o.name[:-4].lower())
                    if(name=='GINDAT_hdg'):
                        d=(ginhdgoffset+self.dataset[name].data) % 360
                    else:
                        d=self.dataset[name].data
                    d.interp1d()
                    o.data=flagged_data(d.interpolated(tout.ravel()).reshape(sh),tstep,flags)
                    x=~np.isfinite(o.data)
                    o.data.flag[x]=3 
                    o.data[x]=0 
        


