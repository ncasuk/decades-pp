from ppodd.core import *
from p_read_crios import read_crio

class read_wvss(read_crio):
    """
Routine for reading in WVSS2A data
"""

    def __init__(self,dataset,wvss):
        self.wvss=wvss
        self.input_names=[self.wvss,'DATE']
        read_crio.__init__(self,dataset)
        self.outputs=[
        parameter(self.wvss+'_VMR',units='ppmv',frequency=1,long_name=self.wvss+' Water Vapor Measurement linearly interpolated to 1Hz'),
        parameter(self.wvss+'_PRESS',units='hPa',frequency=1,long_name=self.wvss+' Pressure inside sample cell linearly interpolated to 1Hz'),
        parameter(self.wvss+'_TEMP',units='C',frequency=1,long_name=self.wvss+' Temperature of sample cell linearly interpolated to 1Hz'),
        parameter(self.wvss+'_VMR_RAW',units='ppmv',frequency=1,long_name=self.wvss+' Water Vapor Measurement'),
        parameter(self.wvss+'_PRESS_RAW',units='hPa',frequency=1,long_name=self.wvss+' Pressure inside sample cell'),
        parameter(self.wvss+'_TEMP_RAW',units='C',frequency=1,long_name=self.wvss+' Temperature of sample cell'),
        parameter(self.wvss+'_PP2F',units='C',frequency=1,long_name=self.wvss+' 2F2F (PP2F in digital counts)'),
        parameter(self.wvss+'_LasSigPow',units='counts',frequency=1,long_name=self.wvss+' power of laser signal'),
        parameter(self.wvss+'_PeakIndex',units='counts',frequency=1,long_name=self.wvss+' index of peak'),
        parameter(self.wvss+'_NullValue',units='counts',frequency=1,long_name=self.wvss+' Null Value'),
        parameter(self.wvss+'_MidPt',units='mAmp',frequency=1,long_name=self.wvss+' midpoint in mAmp'),
        parameter(self.wvss+'_AdjMidPt',units='mAmp',frequency=1,long_name=self.wvss+' adjusted midpoint in MAmp, if peak tracking turned on'),
        parameter(self.wvss+'_PT',units='',frequency=1,long_name=self.wvss+' unitless Pressure/Temperature correction factor'),
        parameter(self.wvss+'_BLCF',units='',frequency=1,long_name=self.wvss+' unitless Beers Law correction factor for non-linearity')]
        
    def readfile(self,filename):
        read_crio.readfile(self,filename)
        for o in self.outputs:
            if o.name==self.wvss+'_utc_time_msec':
                msec_offset=o.data[:]/1000.0
                tx=np.unique(np.expand_dims(o.times.astype(np.int),1)+[-1,0,1]) # spread data times to interpolate to over every second
                tx.sort()
                tx=timestamp(tx)
                o.times+=msec_offset
                times=o.times
        for o in self.outputs:
            if o.name==self.wvss+'_serial_data':
                data=(np.array([np.array(y) for y in np.char.split(o.data)])).astype(np.float)
        for i,o in enumerate(self.outputs[:3]):
            d=timed_data(data[:,i],times)
            inter=d.interp1d()
            o.data=timed_data(d.interpolated(tx),tx)
        for i,o in enumerate(self.outputs[3:14]):
            o.data=timed_data(data[:,i],times)
        
        

class read_wvss2a(read_wvss):
    def __init__(self,dataset):
        read_wvss.__init__(self,dataset,'WVSS2A')   
        
class read_wvss2b(read_wvss):
    def __init__(self,dataset):
        read_wvss.__init__(self,dataset,'WVSS2B')        
     

