from ppodd.cal_base import *
from os.path import getsize
class c_readgin(file_reader):
    def __init__(self,dataset):
        self.name='READGIN'
        self.input_names=['DATE']
        self.filetype='GIN'
        self.outputs=[]
        self.data=None
        file_reader.__init__(self,dataset)
   
    def readfile(self,filename):
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
        dtype=[('grpid','<u2'),('bytes','<u2'),('time1','<f8'),('time2','<f8'),
('dist','<f8'),('timetype','<u1'),('disttype','<u1'),('LAT_GIN','<f8'),('LON_GIN','<f8'),
('ALT_GIN','<f8'),('VELN_GIN','<f4'),('VELE_GIN','<f4'),('VELD_GIN','<f4'),('ROLL_GIN','<f8'),
('PTCH_GIN','<f8'),('HDG_GIN','<f8'),('WAND_GIN','<f8'),('TRCK_GIN','<f4'),
('GSPD_GIN','<f4'),('ROLR_GIN','<f4'),
('PITR_GIN','<f4'),('HDGR_GIN','<f4'),('ACLF_GIN','<f4'),
('ACLS_GIN','<f4'),('ACLD_GIN','<f4'),('status','<u2'),('htime','<i4')]
        l=getsize(filename)
        dlength=136
        if(l % dlength)==0:
            self.data=np.memmap(filename,dtype=dtype,mode='r')
        else:
            self.data=np.memmap(filename,dtype=dtype,shape=(l/dlength),mode='r')
        if(self.data!=None):
            ind=np.where(self.data['grpid']==1)[0]
            gsecs=86400.0*self.ginday(self.dataset['DATE'].data)
            sgin=self.data['time1'][ind[0]]-gsecs
            if(sgin<0):
                sgin+=(86400.0*7)  # if was started on the Sunday after the date ...
            tfrom=self.data['time2'][ind[0]] - sgin
            print 'Gin times ',sgin,tfrom
            #tfrom=86400*np.round((sgin[0]-tout[:][0])/86400)
            tgin=self.data['time2'][ind]-tfrom
            tstep=timestamp((np.round(tgin[0]),np.round(tgin[-1])))
            tout=tstep.at_frequency(32)
            sh=tout.shape
            print 'SHAPE ',sh,(np.round(min(tgin)),np.round(max(tgin)))
            flg=self.data['status'][ind]/3
            flagg=timed_data(flg,tgin)        
            flagg.interp1d()
            flags=np.int8(flagg.interpolated(tout.ravel()).reshape(sh))
            for o in self.outputs:
                print 'Interpolating ',o
                if(o.name=='SECS_GIN'):
                   o.data=timed_data(tstep,tstep)
                else:
                    d=timed_data(self.data[o.name][ind],tgin)
                    d.interp1d()
                    o.data=flagged_data(d.interpolated(tout.ravel()).reshape(sh),tstep,flags)  
        
    def ginday(self,fromdate):
        return (time.localtime(date2time(fromdate)).tm_wday+1) % 7  # day since saturday night


