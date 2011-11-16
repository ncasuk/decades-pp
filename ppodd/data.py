import numpy as np
import numpy.ma as ma
import netCDF4
import sys
from scipy.interpolate import interp1d
import struct

class constants_parameter:
    def __init__(self,name,values):
        self.name=name
        self.data=values
    def __getitem__(self,sl):
        return self.data[sl]
    def __repr__(self):
        return self.name+'(Constants)'

class parameter:
    def __init__(self,name,data=None,description='',
                  standard_name='',units='',
                  frequency=1,number=0):
        self.name=name
        self.units=units
        if(description):
            self.description=description
        else:
            self.description=name
        self.standard_name=standard_name
        self.frequency=frequency
        self.number=number
        self.data=data
    def __repr__(self):
        return self.description+'('+self.units+')'
    def __getitem__(self,sl):
        return self.data[sl]
    def times(self):
        return self.data.timestamp
    def add_condition(self,condition):
        self.data.conditions.append(getattr(self.data,condition))                               
    def del_condition(self,condition):
        self.data.conditions.remove(getattr(self.data,condition))

class decades_dataset(list):
    def set_times(self,start,stop):
        self.start=start
        self.stop=stop
        times=np.arange(start,stop+1,1)
        self.add_time(times,key='base')
    def para_names(self):
        return [i.name for i in self]
    def get_para(self,name):
        for i in self:
            if(i.name==name):
               return i
    def add_file(self,filename,filetype):
        if(filetype.startswith('OUTPUT')):
            outtype=filetype[7:]
            possible_types=['NETCDF4', 'NETCDF4_CLASSIC', 'NETCDF3_CLASSIC','NETCDF3_64BIT']
            if(outtype in possible_types):
                self.output_type=outtype
            else:
                self.output_type=possible_types[0]
            filetype='OUTPUT'
        try:
            self.files.update({filename:filetype})
        except:
            self.files={filename:filetype}
    def add_time(self,times,key=None):
        if(key==None):
            try:
                key=times.frequency
            except:
                pass
            if key==None:
                raise KeyError        
        try:
            if(key not in self.times):
                self.times.update({key:times})
        except:
            self.times={key:times}

    def get_base_time(self):
        try:
            if('base' in self.times):
                return self.times['base']
            elif(1 in self.times):
                return self.times[1]
            else:
                for t in self.times.values():
                    if(t.frequency>1):
                        return t.basetime
        except:
            return None
    
    def get_time(self,key):
        try:
            return self.times[key]
        except:
            return None

    def get_times(self):
        try:
            return self.times
        except:
            return {}

class timestamp(np.ndarray):
    """ 1 or 2d contiguous time array """
    def __new__(self,basetime=None,times=None,frequency=1,dtype='f4'):
        """Create new timestamp"""
        if(isinstance(basetime,tuple)):
            if(len(basetime)==2):
                basetime=np.arange(basetime[0],basetime[1]+1,1,dtype=dtype)
        if(not(isinstance(basetime,np.ndarray))):
            if(times!=None):
                basetime=np.arange(times[0],times[-1]+1,1,dtype=dtype) 
            else:
                raise Exception,'No start,end or basetime found.'
        if(isinstance(basetime,timestamp)):
            basetime=basetime.basetime
        tim=np.empty((len(basetime),frequency),dtype=dtype)
        dt=np.linspace(0,1,frequency+1)[0:frequency]
        if(frequency==1):
            tim=basetime
        else:                
            tim[:]=np.resize(dt,(len(basetime),frequency))
            tim+=np.reshape(basetime[:],(-1,1))
        shape=(len(basetime)*frequency)
        obj=np.ndarray.__new__(self,shape,buffer=tim[:],dtype=dtype)
        obj.basetime=basetime
        obj.frequency=frequency
        obj.times=None
        if(times!=None):
            obj.times=times
            i1=times-obj.basetime[0]
            obj.d_index=np.where((i1<len(basetime)) & (i1>=0))[0]
            obj.cont_index=np.array(i1[obj.d_index],dtype='u8')
        return obj
    def goodtimes(self,times=None):
        if(times!=None):
            i1=times-self.basetime[0]
            self.d_index=np.where((i1<len(self.basetime)) & (i1>=0))[0]
            self.cont_index=np.array(i1[self.d_index],dtype='u8')
        g=np.zeros(self.basetime.shape,dtype='bool')
        g[self.cont_index]=True
        return g
    def align_data(self,data,fill=np.nan):
        if(self.times!=None):
            if(self.frequency==1):
                dat=np.empty((len(self.basetime)),dtype=data.dtype)
            else:
                dat=np.empty((len(self.basetime),self.frequency),dtype=data.dtype)
            dat[:]=fill
            dat[self.cont_index]=data[self.d_index]
            return dat.ravel()
        else:
            return data
    def good_data(self,data,goodtimes=None):
        if(goodtimes==None):
            goodtimes=self.goodtimes()
        dat=data.reshape((len(self.basetime),self.frequency))[goodtimes]
        return dat
    def good_time(self,goodtimes=None):
        if(goodtimes==None):
            goodtimes=self.goodtimes()
        dat=self.reshape((len(self.basetime),self.frequency))[goodtimes]
        return dat
    def good_basetime(self,goodtimes=None):
        if(goodtimes==None):
            goodtimes=self.goodtimes()
        dat=self.basetime[goodtimes]
        return dat
    def raw_time(self,goodtimes=None):
        if(self.times==None):
            return self
        else:
            return self.times[self.d_index]
    def filtered_time(self,fill=np.nan):
        dat=np.empty((len(self.basetime),self.frequency),dtype=self.dtype)
        dat[:]=fill
        dat[self.cont_index]=np.array([self.times[self.d_index]]).transpose()
        return dat.ravel()



class timed_data(object):
    """ Data with associated timestamp """
    def __init__(self,data,times,fill=np.nan):
        self.data=data[:]
        self.timestamp=times
        if(isinstance(times,timestamp)):
            self.data=self.timestamp.align_data(data,fill=fill)
        else:
            self.data=data[:]
        self.interp=None
        self.conditions=[]    
        self.start=self.timestamp[0]
        self.stop=self.timestamp[-1]
        self._getend()
        self.times=self.time()
        self.times.__getitem__=self._gettime
    class time:
        pass
    def raw_data(self,goodtimes=None):
        if(isinstance(self.timestamp,timestamp)):
            return self.timestamp.good_data(self.data,goodtimes=goodtimes)
        else:
            if(goodtimes==None):
                return data
            else:
                return data[goodtimes]
    def raw_time(self,goodtimes=None):
        if(isinstance(self.timestamp,timestamp)):
            return self.timestamp.good_basetime(goodtimes)
        else:
            if(goodtimes==None):
                return self.timestamp
            else:
                return self.timestamp[goodtimes]
    def _gettime(self,sl):
        return self.timestamp[self.getgood(sl)]
    def __len__(self):
        return len(self.timestamp)
    def interp1d(self,kind='linear',fill_value=np.nan):
        self.interp=interp1d(self.timestamp[:],self[:],
                             bounds_error=False,kind=kind,fill_value=fill_value)
        return self.interp
    def interpolated(self,times):
        if self.interp:
           return self.interp(times)
    def nan_condition(self):
        """ Removes non finite data points """
        return np.isfinite(self.data[:])    
    def _getend(self):
        """Returns next whole integer time after stop time - substitute for length"""
        if(self.stop % 1.0 == 0):
           self._end=int(self.stop+1.0)
        else:
           self._end=int(np.ceil(self.stop))
    def getdatalength(self):
        return len(self.times)
    def getgood(self,sl):
        if isinstance(sl,slice):
            start=sl.start
            stop=sl.stop
            if not(stop):
               stop=sys.maxint # This is the number it seems to use when no stop given
            if(stop<0):
               stop=len(self)+stop
            step=sl.step
        else:
           if(sl<0):
               sl=len(self)+sl
           start=sl
           stop=sl+1
           step=None
        if (step):
            ans=((self.timestamp[:]>=start) & 
                         (self.timestamp[:]<stop) & 
                         (self.timestamp[:] % step == 0))           
        else:
            ans=((self.timestamp[:]>=start) & 
                         (self.timestamp[:]<stop))
        for cond in self.conditions:
            ans &= cond()
        return ans       
    def getindexes(self,sl):
        """ Returns indexes based on time slice"""
        ans=np.where(self.getgood(sl))
        return ans
    def __getitem__(self,*args,**kwargs):
        """ Returns time slice of times """
        return self.data[self.getindexes(*args,**kwargs)]
    def get_basetime(self):
        try:
            l=self.timestamp.basetime
        except:
            l=self.timestamp
        return l       
    def get_goodtime(self):
        try:
            l=self.timestamp.goodtimes()
        except:
            l=True
        return l     
    def match_times(self,ldata):
        l=self.get_basetime()        
        ok=True
        for d in ldata:
            c=d.get_basetime()
            if(l.data!=c.data):
                if(not(np.all(l==c))):
                    ok=False
            if(not(ok)):
                break
        if(ok):
            l=self.get_goodtime()
            for d in ldata:
                l=l & d.get_goodtime()
            return l
        else:
            raise Exception,'Cannot match data with different base times.'    


class flagged_data(timed_data):
    """ Timed data with associated flag information """
    def __init__(self,data,timestamp,flags,flag=0,fill=np.nan):
        self.flags=timed_data(flags,timestamp,fill=-1)
        self.flag_thresh=flag
        timed_data.__init__(self,data,timestamp,fill=fill)
    def flag_le(self):
        """ Removes anything flagged > maxflag """
        return self.flags.data[:]<=self.flag_thresh
    def flag_gt(self):
        """ Keeps anything flagged > maxflag """
        return self.flags.data[:]>self.flag_thresh

def get_nc_data(dataset,name,timestamp=None):
    fr=dataset.variables[name].frequency
    if not(timestamp):
        timestamp=timestamps2d(dataset.variables['Time'],frequency=fr)
    if (name+'_FLAG' in dataset.variables):
        return flagged_data(dataset.variables[name],timestamp,
                          dataset.variables[name+'_FLAG'])
    else:    
        return timed_data(dataset.variables[name],timestamp)


