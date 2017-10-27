from ppodd.core import *
import ppodd
from os.path import getsize
from p_read_gindat import gintime

class read_horace_gin(file_read):
    """
Routine for reading in HORACE_GIN data
"""
    def __init__(self,dataset):
        self.input_names=['HORACE_GIN','DATE']
        self.outputs=[parameter('GINDAT_time1',long_name='the valid time for this packet (GPS seconds since Saturday night)'),
                      parameter('GINDAT_time2',long_name='the valid time for this packet (GPS seconds since GIN start up)'),
                      parameter('GINDAT_lat',long_name='Latitude (deg) GIN'),
                      parameter('GINDAT_lon',long_name='Longitude (deg) GIN'),
                      parameter('GINDAT_alt',long_name='Altitude (m) GIN'),
                      parameter('GINDAT_veln',long_name='North velocity (m/s) GIN'),
                      parameter('GINDAT_vele',long_name='East velocity (m/s) GIN'),
                      parameter('GINDAT_veld',long_name='Down velocity (m/s) GIN'),
                      parameter('GINDAT_roll',long_name='Aircraft roll (deg) GIN'),
                      parameter('GINDAT_ptch',long_name='Aircraft pitch (deg) GIN'),
                      parameter('GINDAT_hdg',long_name='Aircraft heading (deg) GIN'),
                      parameter('GINDAT_wand',long_name='Aircraft wander angle (deg) GIN'),
                      parameter('GINDAT_trck',long_name='Aircraft track angle (deg) GIN'),
                      parameter('GINDAT_gspd',long_name='Aircraft speed (m/s) GIN'),
                      parameter('GINDAT_rolr',long_name='Aircraft angular rate about longitudinal axis (deg/s) GIN'),
                      parameter('GINDAT_pitr',long_name='Aircraft angular rate about transverse axis deg/s) GIN'),
                      parameter('GINDAT_hdgr',long_name='Aircraft angular rate about down axis (deg/s) GIN'),
                      parameter('GINDAT_aclf',long_name='Aircraft longitudinal acceleration (m/s^2) GIN'),
                      parameter('GINDAT_acls',long_name='Aircraft transverse acceleration (m/s^2) GIN'),
                      parameter('GINDAT_acld',long_name='Aircraft down acceleration (m/s^2) GIN'),
                      parameter('GINDAT_status',long_name='Solution status GIN')]      
        self.data=None
        self.patterns=('*gin*.dat','*gin*.dat;*')
        file_read.__init__(self,dataset)
        

    def fixfilename(self,filename):
        return filename[:filename.index('.dat')+4]

   
    def readfile(self,filename):
        dtype=[('grpid','<u2'),('bytes','<u2'),('time1','<f8'),('time2','<f8'),
               ('dist','<f8'),('timetype','<u1'),('disttype','<u1'),('lat','<f8'),
               ('lon','<f8'),('alt','<f8'),('veln','<f4'),('vele','<f4'),('veld','<f4'),
               ('roll','<f8'),('ptch','<f8'),('hdg','<f8'),('wand','<f8'),('trck','<f4'),
               ('gspd','<f4'),('rolr','<f4'),('pitr','<f4'),('hdgr','<f4'),('aclf','<f4'),
               ('acls','<f4'),('acld','<f4'),('status','<u2'),('htime','<i4')]

        l=getsize(filename)
        dlength=136
        if(l % dlength)==0:
            self.data=np.fromfile(filename, dtype=dtype)
        else:
            self.data=np.fromfile(filename, dtype=dtype, count=l/dlength)
        if(self.data!=None):
            self.data=self.data[self.data['grpid']==1]
            times=gintime(self.data['time1'],self.dataset['DATE'].data)
            for o in self.outputs:
                o.data=timed_data(self.data[o.name[7:]],times)
        



