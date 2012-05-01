from cal_base import *
import numpy as np
from os.path import getsize
class c_readgin(file_reader):
    def __init__(self,dataset):
        self.name='READGIN'
        self.input_names=['SECS']
        self.filetype='GIN'
        self.outputs=[]
        self.data=None
        file_reader.__init__(self,dataset)
   
    def openfile(self,filename):
        self.outputs=[
parameter('LAT_GIN',units='degree_north',frequency=32,number=610,description='Latitude from POS AV 510 GPS-aided Inertial Navigation unit'),
parameter('LON_GIN',units='degree_east',frequency=32,number=611,description='Longitude from POS AV 510 GPS-aided Inertial Navigation unit'),
parameter('ALT_GIN',units='m',frequency=32,number=612,description='Altitude from POS AV 510 GPS-aided Inertial Navigation unit'),
parameter('VELN_GIN',units='m s-1',frequency=32,number=613,description='Aircraft velocity north from POS AV 510 GPS-aided Inertial Navigation unit'),
parameter('VELE_GIN',units='m s-1',frequency=32,number=614,description='Aircraft velocity east from POS AV 510 GPS-aided Inertial Navigation unit'),
parameter('VELD_GIN',units='m s-1',frequency=32,number=615,description='Aircraft velocity down from POS AV 510 GPS-aided Inertial Navigation unit'),
parameter('ROLL_GIN',units='degree',frequency=32,number=616,description='Roll angle from POSAV GPS-aided Inertial Nav. unit (positive for left wing up)'),
parameter('PTCH_GIN',units='degree',frequency=32,number=617,description='Pitch angle from POSAV GPS-aided Inertial Nav. unit (positive for nose up)'),
parameter('HDG_GIN',units='degree',frequency=32,number=618,description='Headingfrom POSAV GPS-aided Inertial Navigation unit'),
parameter('WAND_GIN',units='deg s-1',frequency=32,number=619,description='GIN wander angle'),
parameter('TRCK_GIN',units='degree',frequency=32,number=620,description='Aircraft track angle POSAV GPS-aided Inertial Navigation unit'),
parameter('GSPD_GIN',units='m s-1',frequency=32,number=621,description='Groundspeed from POS AV 510 GPS-aided Inertial Navigation unit'),
parameter('ROLR_GIN',units='degree s-1',frequency=32,number=622,description='rate-of-change of GIN roll angle'),
parameter('PITR_GIN',units='degree s-1',frequency=32,number=623,description='rate-of-change of GIN pitch angle'),
parameter('HDGR_GIN',units='degree s-1',frequency=32,number=624,description='rate-of-change of GIN heading'),
parameter('ACLF_GIN',units='m s-2',frequency=32,number=625,description='Acceleration along the aircraft longitudinal axis (GIN) (positive forward)'),
parameter('ACLS_GIN',units='m s-2',frequency=32,number=626,description='Acceleration along the aircraft transverse axis (GIN) (positive starboard)'),
parameter('ACLD_GIN',units='m s-2',frequency=32,number=627,description='Acceleration along the aircraft vertical axis (GIN) (positive down)')]
        dtype=[('grpid','<u2'),('bytes','<u2'),('Time','<f8'),('time2','<f8'),
('dist','<f8'),('time_types','<u1'),('dist_type','<u1'),('LAT_GIN','<f8'),('LON_GIN','<f8'),
('ALT_GIN','<f8'),('VELN_GIN','<f4'),('VELE_GIN','<f4'),('VELD_GIN','<f4'),('ROLL_GIN','<f8'),
('PTCH_GIN','<f8'),('HDG_GIN','<f8'),('WAND_GIN','<f8'),('TRCK_GIN','<f4'),
('GSPD_GIN','<f4'),('ROLR_GIN','<f4'),
('PITR_GIN','<f4'),('HDGR_GIN','<f4'),('ACLF_GIN','<f4'),
('ACLS_GIN','<f4'),('ACLD_GIN','<f4'),('status','<u2'),('htime','<i4')]
        l=getsize(filename)
        dlength=136
        if(l % dlength)==0:
            self.data=np.memmap(filename,dtype=dtype)
        else:
            self.data=np.memmap(filename,dtype=dtype,shape=(l/dlength))
        
    def process(self):
        if(self.data!=None):
            tout=timestamp(self.dataset.get_base_time(),frequency=32)        
            ind=np.where(self.data['grpid']==1)
            sgin=self.data['Time'][ind]
            tfrom=86400*np.round((sgin[0]-tout[:][0])/86400)
            tgin=self.data['Time'][ind]-tfrom
            sh=tout.shape
            flg=self.data['status'][ind]/3
            flagg=timed_data(flg,tgin)        
            flagg.interp1d()
            flags=np.int8(flagg.interpolated(tout[:]).reshape(sh))
            for o in self.outputs:
                print 'Interpolating ',o
                d=timed_data(self.data[o.name][ind],tgin)
                d.interp1d()
                o.data=flagged_data(d.interpolated(tout[:]).reshape(sh),tout,flags)  

