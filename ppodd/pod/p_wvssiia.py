from ppodd.core import *
from scipy.interpolate import interp1d

class wvssii(cal_base):
    """
WVSSII interoplation to 1Hz from it's approximate 0.4Hz, and decode from serial strings.

"""
    def __init__(self,dataset,wvss):
        self.wvss=wvss
        self.input_names=[self.wvss+'_utc_time_msec',self.wvss+'_serial_data']
        self.outputs=[
        parameter(self.wvss+'_VMR',units='ppmv',frequency=1,long_name=self.wvss+' Water Vapor Measurement linearly interpolated to 1Hz'),
        parameter(self.wvss+'_PRESS',units='hPa',frequency=1,long_name=self.wvss+' Pressure inside sample cell'),
        parameter(self.wvss+'_TEMP',units='C',frequency=1,long_name=self.wvss+' Temperature of sample cell'),
        parameter(self.wvss+'_PP2F',units='C',frequency=1,long_name=self.wvss+' 2F2F (PP2F in digital counts)'),
        parameter(self.wvss+'_LasSigPow',units='counts',frequency=1,long_name=self.wvss+' power of laser signal'),
        parameter(self.wvss+'_PeakIndex',units='counts',frequency=1,long_name=self.wvss+' index of peak'),
        parameter(self.wvss+'_NullValue',units='counts',frequency=1,long_name=self.wvss+' Null Value'),
        parameter(self.wvss+'_MidPt',units='mAmp',frequency=1,long_name=self.wvss+' midpoint in mAmp'),
        parameter(self.wvss+'_AdjMidPt',units='mAmp',frequency=1,long_name=self.wvss+' adjusted midpoint in MAmp, if peak tracking turned on'),
        parameter(self.wvss+'_PT',units='',frequency=1,long_name=self.wvss+' unitless Pressure/Temperature correction factor'),
        parameter(self.wvss+'_BLCF',units='',frequency=1,long_name=self.wvss+' unitless Beers Law correction factor for non-linearity')]
        self.version=1.00
        cal_base.__init__(self,dataset)

        
    def process(self):
        if(self.dataset[self.input_names[0]].data!=None):
            times=self.dataset[self.input_names[0]].times
            tx=np.unique(np.expand_dims(times.astype(np.int),1)+[-1,0,1]) # spread data times to interpolate to over every second
            tx.sort()
            tx=timestamp(tx)
            times+=self.dataset[self.input_names[0]]/1000.0  # Real time to nearest millisecond
            data=(np.array([np.array(y) for y in np.char.split(self.dataset[self.input_names[1]])])).astype(np.float)
            for i,o in enumerate(self.outputs):
                d=timed_data(data[:,i],times)
                inter=d.interp1d()
                o.data=timed_data(d.interpolated(tx),tx)
        

class wvssiia(wvssii):
    def __init__(self,dataset):
        wvssii.__init__(self,dataset,'WVSS2A')
