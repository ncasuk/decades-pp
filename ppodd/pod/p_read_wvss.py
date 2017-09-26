from ppodd.core import *
from p_read_crios import read_crio
import numpy as np

class read_wvss(read_crio):
    """
Routine for reading in WVSS2 data
"""

    def __init__(self, dataset,wvss):
        self.input_names = [wvss, 'DATE']
        self.wvss = wvss
        read_crio.__init__(self,dataset)
        if(wvss+'_ident' in dataset):
            self.ident = dataset[wvss+'_ident'].long_name
        else:
            # This is for flights from B810 - B850 where this information not in the description file
            if (wvss == "WVSS2A"):
                self.ident = "Flush inlet WVSSII serial no. 4229"
            else:
                self.ident = "Rosemount inlet WVSSII serial no. 0388"
        self.wvss="WVSS2"+self.ident[0] # R for Rosemount and F for Flush
        long_name="Water Vapour Measurement from "+self.ident+" linearly interpolated to 1Hz"
        self.outputs=[parameter(self.wvss+'_VMR', units='ppmv', frequency=1, long_name=long_name),
                      parameter(self.wvss+'_PRESS', units='hPa', frequency=1, long_name=self.wvss+' Pressure inside sample cell linearly interpolated to 1Hz'),
                      parameter(self.wvss+'_TEMP', units='C', frequency=1, long_name=self.wvss+' Temperature of sample cell linearly interpolated to 1Hz'),
                      parameter(wvss+'_VMR_RAW', units='ppmv', frequency=1, long_name=self.wvss+' Water Vapour Measurement'),
                      parameter(wvss+'_PRESS_RAW', units='hPa', frequency=1, long_name=self.wvss+' Pressure inside sample cell'),
                      parameter(wvss+'_TEMP_RAW', units='C', frequency=1, long_name=self.wvss+' Temperature of sample cell'),
                      parameter(wvss+'_PP2F', units='C', frequency=1, long_name=self.wvss+' 2F2F (PP2F in digital counts)'),
                      parameter(wvss+'_LasSigPow', units='counts', frequency=1, long_name=self.wvss+' power of laser signal'),
                      parameter(wvss+'_PeakIndex', units='counts', frequency=1, long_name=self.wvss+' index of peak'),
                      parameter(wvss+'_NullValue', units='counts', frequency=1, long_name=self.wvss+' Null Value'),
                      parameter(wvss+'_MidPt', units='mAmp', frequency=1,long_name=self.wvss+' midpoint in mAmp'),
                      parameter(wvss+'_AdjMidPt', units='mAmp', frequency=1,long_name=self.wvss+' adjusted midpoint in MAmp, if peak tracking turned on'),
                      parameter(wvss+'_PT', units='',frequency=1, long_name=self.wvss+' unitless Pressure/Temperature correction factor'),
                      parameter(wvss+'_BLCF', units='',frequency=1, long_name=self.wvss+' unitless Beers Law correction factor for non-linearity')]        

    def strconv(self, arr, length=11):
        """
        Converts a line of serial data to floats - if there is an error returns nans
        """        
        for line in arr:
            try:
                if(len(line)!=length):
                    raise(ValueError('Wrong number of values in %s' % ' '.join(line)))
                yield np.array(line).astype(np.float)
            except ValueError as ve:
                ppodd.logger.warning(ve)
                yield np.empty((length,))+np.nan

    def readfile(self, filename):
        read_crio.readfile(self, filename)
        for o in self.outputs:
            if o.name.endswith('_utc_time_msec'):
                msec_offset = o.data[:]/1000.0
                tx=np.unique(np.expand_dims(o.times.astype(np.int),1)+[-1,0,1]) # spread data times to interpolate to over every second
                tx.sort()
                tx = timestamp(tx)                
                otimes = o.times.astype(np.float32)  # data format needs to match msec_offset
                otimes += msec_offset
                times = otimes
            if o.name.endswith('_serial_data'):
                data = np.array([y for y in self.strconv(np.char.split(o.data))])
        for i,o in enumerate(self.outputs[:3]):
            d = timed_data(data[:,i],times)
            o.data = flagged_data(d.interp(tx),tx,np.ones(len(tx),dtype='i1'))
        for i,o in enumerate(self.outputs[3:14]):
            o.data = timed_data(data[:,i],times)
        
        

class read_wvss2a(read_wvss):
    def __init__(self,dataset):
        read_wvss.__init__(self, dataset, 'WVSS2A')   
        
class read_wvss2b(read_wvss):
    def __init__(self,dataset):
        read_wvss.__init__(self, dataset, 'WVSS2B')        
     

