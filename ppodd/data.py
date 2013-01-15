import time
import numpy as np
from scipy.interpolate import interp1d

class constants_parameter:
    """ The class for describing constants """
    def __init__(self,name,values):
        self.name=name
        self.data=values
    def __getitem__(self,sl):
        return self.data[sl]
    def __str__(self):
        return self.name+'(Constants)'
    def __repr__(self):
        return self.name

class parameter:
    """ The class for all parameters """
    def __init__(self,name,data=None,long_name='',
                  standard_name='',units='',
                  frequency=1,number=0):
        self.name=name
        self.data=data
        if(long_name):
            self.long_name=long_name
        else:
            self.long_name=name
        self.standard_name=standard_name
        self.units=units
        self.frequency=frequency
        self.number=number
    def __repr__(self):
        return self.name
    def __str__(self):
        return self.long_name+'('+self.units+')'
    def __getitem__(self,*args):
        return self.data.__getitem__(*args)
    def __getslice__(self,*args):
        return self.data.__getslice__(*args)
    def __getatts__(self):
        ans=dict(vars(self))
        try:
            del ans['name']
            del ans['data']
        except KeyError:
            pass
        return ans
    attributes=property(__getatts__) 
    def __getattr__(self,name):
        """ Gets attributes from the data if not in the parameter object """
        return getattr(self.data,name)
    
class decades_dataset(list):
    """ A dataset made up of a list of parameters, and information about files etc """
    def __init__(self,*args,**kwargs):
        self.files={}
        self.history=''
        self.conventions='CF-1.0'
        self.source='FAAM BAe-146 Aircraft Data'
        self.references='http://www.faam.ac.uk'
        self.institution='FAAM'
        self.format_version='1.0'
        self.revision=0
        list.__init__(self,*args,**kwargs)
    def para_names(self):
        return [i.name for i in self]
    def get_para(self,name):
        for i in self:
            if(i.name==name):
                return i
        raise AttributeError("%r object has no parameter %r" %
                         (type(self).__name__, name))
    def __getitem__(self,para):
        ans=self.get_para(para)
        if ans is None:
            try:
                return list.__getitem__(self,para)
            except TypeError:
                return None
        else:
            return ans

    def __getatts__(self):
        ans=dict(vars(self))
        try:
            ans['files']=''
            for f in self.files:
                ans['files']+=f+':'+self.files[f]+'\n'
        except KeyError:
            pass
        if 'Title' not in ans:
            dt=time.gmtime(date2time(self['DATE'][:]))
            ans['Title']='Data from %s on %s' % (self['FLIGHT'][:],time.strftime('%d-%b-%Y',dt))
        return ans

    attributes=property(__getatts__) 
             
    def add_file(self,filename,filetype):
        """ Add a file to the dataset - the filetype tells it how to read in but doesnt do the reading """
        if(filetype.startswith('OUTPUT')):
            outtype=filetype[7:]
            possible_types=['NETCDF3_CLASSIC','NETCDF4', 'NETCDF4_CLASSIC', 'NETCDF3_64BIT']
            if(outtype in possible_types):
                self.output_type=outtype
            else:
                self.output_type=possible_types[0]
            filetype='OUTPUT'
        self.files.update({filename:filetype})

    def matchtimes(self,input_names,paras=[],notparas=[]):
        """ Finding matching times for a list of inputs """    
        for i in input_names:
            p=self.get_para(i)
            try:
                frqin=p.frequency
                paras.append(p)
                if paras[-1].data is not None:
                    if(len(paras)==1):
                        match=paras[0].data.times
                    else:
                        match=paras[-1].data.matchtimes(match)
                else:
                    match=timestamp([])
            except AttributeError:
                # If there is no frequency add to notparas list (probably constant) 
                notparas.append(p)
        return match

def date2time(fromdate):
    l=len(fromdate)
    if(l==3):
        fm=[0]*6+fromdate
        fm.reverse()
        fm=time.mktime(fm)
    elif(l==9):
        fm=time.mktime(fromdate)
    else:                      
        raise TypeError,'Incompatible date for conversion'
    return fm

class timestamp(np.ndarray):
    def __new__(cls,times=None,fromdate=None,dtype='f8'):
        """Create new timestamp"""
        if(isinstance(times,tuple)):
            if(len(times)==2):
                times=np.arange(times[0],times[1]+1,1,dtype=dtype)
        tim=np.asarray(times,dtype=dtype)
        if fromdate is not None:
            try:
                tim=times-date2time(fromdate)
            except (TypeError):
                try:
                    tim=times-fromdate       
                except (TypeError,ValueError):
                    raise Exception,'Incompatible date for conversion'
        obj=tim.view(cls)
        return obj

        
    def at_frequency(self,frequency=None):
        if frequency is not None:
            dt=np.linspace(0,1,frequency+1)[0:frequency]
            tim=np.empty((len(self),frequency),self.dtype)
            tim[:]=np.resize(dt,(len(self),frequency))
            tim+=np.reshape(self[:],(-1,1))
            return tim.view(type(self))
        else:
            return self
    def __array_finalize__(self, obj):
        if obj is None: return
    def match(self,othertimes):
        return np.intersect1d(self,othertimes).view(type(self))
    def ismatch(self,othertimes):
        return np.in1d(self,othertimes)
    def asindexes(self,start=None):
        """Only for 1d 1Hz"""
        if(start==None):
            start=np.min(self)
        result=np.asarray(self[:]-start,dtype=int)
        return result
    def tosecs(self,fromdate=None,dateformat='%Y %m %d'):
        if(fromdate):
            try:
                ans=self[:]-fromdate
            except TypeError:
                try:
                    fromdate=time.mktime(fromdate)
                except TypeError:
                    try:
                        fromdate=time.mktime(time.strptime(fromdate,dateformat))
                    except (TypeError,ValueError):
                        raise Exception,'Incompatible date for conversion'
                ans=self[:]-fromdate
        else:
            ans=self[:]-86400.0*int(self[0]/86400)
        return ans        

        

class timed_data(np.ndarray):
    """
    A timed parameter must be able to...
    
    find matched times ( from two ( or more ) parameters )
    extract data from matched times 
    put data onto a contiguous time frame / masked arrays
    
    
    keep it simple...
    
    All times ( except GIN ) are whole seconds with multiple data points per second, same as output data, but output must be contiguous
    
    The difficulty is going from data which isn't available every second to contiguous data and or vice versa
    
    fortran routines won't like missing data
    
    
    """
    def __new__(cls,data,times):
        data = np.asarray(data)
        obj = data.view(cls)
        obj.frequency=None
        if(len(data.shape)==2):
            obj.frequency=data.shape[1]
        obj.times=None
        if(times!=None):
            if(len(times)==len(data)):
                obj.times=times               
        if(obj.times==None):
            raise Exception,'No times'
        else:                    
            return obj
    def __array_finalize__(self, obj):
        if obj is None: return
        self.times = getattr(obj, 'times', None)
        self.frequency = getattr(obj, 'frequency', None)
    def _getrawdata(self):
        return self.view(np.ndarray)
    raw_data=property(_getrawdata)
    def __getslice__(self,a,b):
        result=np.ndarray.__getslice__(self,a,b)
        if(type(result)==type(self)):
            try:
                result.times=self.times.__getslice__(a,b)
            except AttributeError:
                result.times=self.times
        return result
    def __getitem__(self,index):
        """ Return timestamped result
            1d ( whole second ) time 
        """
        result=np.ndarray.__getitem__(self,index)
        tindex=index
        if(hasattr(tindex,'count')):  # is it a tuple
            try:
                if(len(index)==2):
                    tindex=index[0]
            except:
                pass
        if(type(result)==type(self)):
            result.times=self.times[tindex]
        return result
    def gettimes2d(self):
        if(self.frequency==None):
            return self.times
        else:
            return self.times.at_frequency(self.frequency)
    times2d=property(gettimes2d)
    def ravel(self):
        result=np.ndarray.ravel(self)
        result.times=self.times2d.ravel()
        result.frequency=None
        return result
    def interp1d(self,kind='linear',fill_value=np.nan):
        self.interp=interp1d(self.times[:],self[:],
                             bounds_error=False,kind=kind,fill_value=fill_value)
        return self.interp
    def interpolated(self,times):
        if self.interp:
            return self.interp(times)
    def matchtimes(self,otherdata):
        try:
            return self.times.match(otherdata.times)
        except AttributeError:
            return self.times.match(otherdata)


    def ismatch(self,times):
        return self[self.times.ismatch(times)]

    def twod_array(self,arr=None,frequency=None,indexes1d=False):
        if arr is None:
            arr=self.times
        if frequency is None:
            frequency=self.frequency
        try:
            if(indexes1d):
                return np.squeeze(np.resize(arr,(frequency,len(arr))).T)*frequency+np.arange(frequency)
            else:
                return np.squeeze(np.resize(arr,(frequency,len(arr))).T)
        except:
            return arr
        
        
    def asmasked(self,start=None,end=None,mask=None,fill_value=None,data=None,returntimes=False):
        """Only for 1d 1Hz times"""
        if data is None:
            data=self.raw_data
        if start is None:
            start=np.min(self.times)
        if end is None:
            end=np.max(self.times)
        t1=timestamp((start,end))
        t=timestamp(self.twod_array(t1))
        msk=~t.ismatch(self.times)
        d=np.ma.empty(t.shape,dtype=data.dtype,fill_value=fill_value)
        d[:]=d.fill_value
        ind=self.times.asindexes(start=start)
        xind=(ind>=0) & (ind<len(d))
        d[ind[xind]]=data[xind]
        if mask is not None:
            ind=self.twod_array(ind,indexes1d=True)
            xind=self.twod_array(xind)
            msk[ind[xind]]|=mask[xind]
        d.mask=msk
        if(returntimes):
            times=t1.at_frequency(self.frequency)
            d=(d,times)
        return d
        

class flagged_data(timed_data):
    """ Timed data with associated flag information """
    def __new__(cls,data,timestamp,flags,maxflag=3):
        obj = timed_data.__new__(cls,data,timestamp)
        obj.flag=flags # timed_data(flags,timestamp)  
        obj.maxflag=maxflag
        return obj
    def __array_finalize__(self, obj):
        timed_data.__array_finalize__(self, obj)
        self.flag = getattr(obj, 'flag', None)
        self.maxflag = getattr(obj, 'maxflag', 3)
    def __getslice__(self,a,b):
        result=timed_data.__getslice__(self,a,b)
        if(type(result)==type(self)):
            try:
                result.flag=self.flag.__getslice__(a,b)
            except AttributeError:
                result.flag=self.flag
        return result
    def ravel(self):
        result=timed_data.ravel(self)
        result.flag=self.flag.ravel()
        return result
    def __getitem__(self,index):
        """ Return timestamped result
            1d ( whole second ) time 
        """
        result=timed_data.__getitem__(self,index)
        if(type(result)==type(self)):
            result.flag=self.flag[index]
        return result
    def asmasked(self,maxflag=None,start=None,end=None,fill_value=None,returntimes=False):
        if maxflag is None:
            maxflag=self.maxflag
        ans=timed_data.asmasked(self,start=start,end=end,mask=self.flag>maxflag,
                                fill_value=fill_value,returntimes=returntimes)
        return ans
    def flagmasked(self,maxflag=None,start=None,end=None,fill_value=-1,returntimes=False):
        if maxflag is None:
            maxflag=self.maxflag
        ans=timed_data.asmasked(self,start=start,end=end,data=self.flag,
                                fill_value=fill_value,returntimes=returntimes)
        return ans

#x=timed_data(np.arange(14).reshape((7,2)),timestamp((0,6)))
#y=timed_data(np.arange(32).reshape((8,4)),timestamp((3,10)))
#z=timed_data(np.arange(32),timestamp((0,31)))
#t=x.matchtimes(y)
#t=z.matchtimes(t)
#z=y.ismatch(t)*x.ismatch(t)*z.ismatch(t)

