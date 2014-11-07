'''
Created on 4 Sep 2013

ppodd.core includes all the base classes for dealing with timed aircraft data
        
    
    
            

@author: Dave Tiddeman
'''
import time
import numpy as np
from scipy.interpolate import interp1d
import fnmatch
import os
import zipfile
import ppodd



try:
    from collections import OrderedDict
except ImportError:
    from OrderedDictOld import OrderedDict

class constants_parameter:
    """ The class for describing constants """
    def __init__(self,name,values,paratype='Constants'):
        self.name=name
        self.data=values
        self.type=paratype
    def __getitem__(self,*args):
        return self.data.__getitem__(*args)
    def __getslice__(self,*args):
        return self.data.__getslice__(*args)
    #def __getitem__(self,sl):
    #    return self.data[sl]
    def __str__(self):
        return self.name+'('+self.type+')'
    def __repr__(self):
        return repr(self.data)


class file_parameter(constants_parameter):
    """ The class for describing files """
    def __init__(self,name,files):
        constants_parameter.__init__(self,name,set(files),'Files')

    def add_file(self,filename):
        self.data.update(filename)

    def del_file(self,filename):
        self.data.remove(filename)

    def __getslice__(self,*args):
        ppodd.logger.debug("Can't slice set of files")
        return self.data
        
    def __getitem__(self,*args):
        ppodd.logger.debug("Can't index set of files")
        return self.data

class parameter(constants_parameter):
    """ The class for all data holding parameters. Arrays of timed_data ( a data array linked to a timestamp object ) or flagged_data which also includes a flag, and meta-data"""
    def __init__(self,name,data=None,long_name='',
                  standard_name='',units='',
                  frequency=1,number=0):
        constants_parameter.__init__(self,name,data,'Data')
        if(long_name):
            self.long_name=long_name
        else:
            self.long_name=name
        self.standard_name=standard_name
        self.units=units
        self.frequency=frequency
        self.number=number
    def __str__(self):
        return self.long_name+'('+self.units+')'
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
    
class decades_dataset(OrderedDict):
    """An ordered dictionary of data, constants, attribute or file parameters
        and a dictionary of processing modules
    """
    def __init__(self,*args,**kwargs):
        self.history=''
        self.modules=OrderedDict()
        self.outparas=None
        self.filetypes={}
        """ Import all the calibration modules
        The names are listed in calnames
        """
        self.getmods()
        from ppodd.pod.write_nc import write_nc
        self.write_nc=write_nc(self)
        OrderedDict.__init__(self)
        self.add_para('Attribute','conventions','CF-1.0')
        self.add_para('Attribute','source','FAAM BAe-146 Aircraft Data')
        self.add_para('Attribute','references','http://www.faam.ac.uk')
        self.add_para('Attribute','institution','FAAM')
        self.add_para('Attribute','format_version','1.0')
        self.add_para('Attribute','revision',0)
        self.add_para('Data','SECS',long_name='Seconds past midnight',number=515,
                                              units='s') # This is a place holder for a seconds past midnight value
                                                         # which is actually the time of each timed parameter.
        for ar in args:
            self.add_file(ar)
        
    def __repr__(self):
        return 'Decades dataset '+repr(self.keys())

    def add_para(self,paratype,name,*args,**kwargs):
        if(paratype=='Data'):
            self[name]=parameter(name,*args,**kwargs)
        elif(paratype=='Files'):
            if(name in self):
                self[name].add_file(args)
            else:
                self[name]=file_parameter(name,args)
        else:
            self[name]=constants_parameter(name,*args,paratype=paratype)
        

    def getfiles(self):
        """Get all the specified files as a list of tuples"""
        files=[]
        for ft in self.filetypes:
            if(ft in self):
                for fn in self[ft].data:
                    files.append((fn,ft))
        return files

    def clearfiles(self):
        """Remove any input file specifications"""
        for f in self.filetypes:
            if(f in self):
                del self[f]

    def parse_filenames(self,files=None,**kwargs):
        """Parse filenames in list for flight number and date"""
        from ppodd.util import fltno_date
        if(files):
            fs=files
        else:
            fs=self.getfiles()
        files=[]
        for fi in fs:
            df=self.DecadesFile(*fi)
            files.append(df.filename)
            if(df.filetype=='ZIP'):
                if(os.path.isfile(df.filename)):
                    z=zipfile.ZipFile(df.filename)
                    files+=z.namelist()
                    z.close()
            elif(os.path.isdir(df.filename)):
                files+=os.listdir(df.filename)
            
        return fltno_date(files,**kwargs)

    def DecadesFile(self,*args,**kwargs):
        """A decades file object for setting file types etc"""
        return decades_dataset.decades_file(self,*args,**kwargs)
        

    class decades_file(object):
        """A decades file class for setting file types etc"""
        def __init__(self,dataset,filename,filetype=None):
            if(not filetype):
                if(':' in filename):
                    filename,filetype=filename.split(':')
                else:
                    filetype=dataset.guesstype(filename)
            self.filetypes=dataset.filetypes
            self.filetype=filetype
            self.filename=filename
            
        def __getfilename__(self):
            return self.filetypes[self.filetype].fixfilename(self.__file__)

        def __setfilename__(self,val):
            self.__file__=val
        
        filename=property(__getfilename__,__setfilename__)
        
        def astuple(self):
            return (self.filename,self.filetype)    

    def guesstype(self,filen):
        ans='FOLDER'
        anso=None
        for n,o in self.filetypes.items():
            if(o.filetest(filen)):
                #ans=n
                if(not(anso)):
                    ans=n
                    anso=o
                else:
                    change=False
                    for patt in o.patterns:
                        if(anso.filetest(patt)):
                            change=True
                    if(change):
                        ans=n
                        anso=o
        return ans


    def getmods(self,classes=None):
        if(not classes):
            import ppodd.pod
            classes=ppodd.pod.modules
        for m in classes:
            try:
                self.modules[m]=classes[m](self)
            except:
                ppodd.logger.info('%s probably a base class so cannot instatiate' % m)

    def clearmods(self):
        for m in self.modules.values():
            if(m.runstate!='ignore'):
                m.__init__(self)

    def clear(self,files=False):
        for p in self:
            if self[p].type !='Files' or files:
                del self[p]


    def __getatts__(self):
        """ Get the attributes as if a NetCDF """
        ans={}
        for n in self:
            if self[n].type=='Attribute' or self[n].type=='Constants':
                ans[n]=self[n].data
        ans['history']=self.history
        ans['processing_version']=ppodd.version
        ans['files']=''
        for f,t in self.getfiles():
            ans['files']+=os.path.basename(f)+':'+t+'\n'
        try:
            dt=time.gmtime(date2time(self['DATE'][:]))
            ans['Title']='Data from %s on %s' % (self['FLIGHT'][:],time.strftime('%d-%b-%Y',dt))
        except KeyError:
            pass
        return ans

    attributes=property(__getatts__) 
             

    def add_file(self,filename,filetype=None):
        """ Add a file to the dataset - the filetype tells it how to read in but doesnt do the reading """
        df=self.DecadesFile(filename,filetype)
        if(df.filetype in self.filetypes):
            self.add_para('Files',df.filetype,df.filename)
        else:
            raise ValueError('No known reader for filetype %s' % df.filetype)

    def matchtimes(self,input_names,paras=None,notparas=None):
        """ Finding matching times for a list of inputs """  
        if paras is None:
            paras=[]
        if notparas is None:
            notparas=[]
        for i in input_names:
            p=self[i]
            try:
                frqin=p.frequency
                paras.append(p)
                if paras[-1].data is not None:
                    try:
                        match=paras[-1].data.matchtimes(match)
                    except NameError:
                        match=paras[-1].data.times
                else:
                    if(p.name!='SECS'):
                        match=timestamp([])
            except AttributeError:
                # If there is no frequency add to notparas list (probably constant) 
                notparas.append(p)
        return match

    def __setnocals__(self,x):
        for m in self.modules:
            if(m in x):
                self.modules[m].runstate='ignore'
            else:
                if(self.modules[m].runstate=='ignore'):
                    self.modules[m].runstate='ready'
        for y in x:
            if y not in self.modules:
                ppodd.logger.warning('%s module not in dataset' % y)

        
    def __getnocals__(self):
        """Which calibration modules to avoid"""
        nocals=[]
        for m in self.modules:
            if self.modules[m].runstate=='ignore':
                nocals.append(m)
        return nocals
        
    nocals=property(__getnocals__,__setnocals__)

    def __setcals__(self,x):
        for m in self.modules:
            if(m in x):
                self.modules[m].runstate='ready'
            else:
                self.modules[m].runstate='ignore'
        for y in x:
            if y not in self.modules:
                ppodd.logger.warning('%s module not in dataset' % y)

        
    def __getcals__(self):
        """Which calibration modules to use"""
        cals=[]
        for m in self.modules:
            if self.modules[m].runstate!='ignore':
                cals.append(m)
        return cals
        
    cals=property(__getcals__,__setcals__)
    
    def getdata(self,*args):
        if(not args):
            self.process()
            return self
        d={}
        for p in args:
            if(p in self):
                d[p]=self[p]
            else:
                for m in self.modules:
                    if(p in self.modules[m].getoutputnames()):
                        self.modules[m].run()
            if(p in self):
                d[p]=self[p]
            else:
                runone=True
                while(runone):
                    runone=False
                    for m in self.modules:
                        if(len(self.modules[m].outputs)==0 and self.modules[m].runstate=='ready'):
                            self.modules[m].run()
                            if(self.modules[m].runstate=='success'):
                                runone=True
                        if(p in self):
                            d[p]=self[p]
                            runone=False
                            break
                if(p not in self):
                    pass
        return d


 
    def process(self):
        """ Sorts calibrate modules - so they are run in order of availability of their inputs 
        and do the processing """
        finished=False
        while(not finished):
            """ Keep going while more modules to run """
            finished=True
            for cal in self.modules.values():
                """ For each unrun module """
                if(cal.runstate=='ready'):
                    cal.run()
                    if cal.runstate=='success':
                        finished=False
                if (self.outparas and not finished):
                    """ If all output parameters are present then we have finished """
                    finished=True
                    for i in self.outparas:
                        if(i not in self):
                            finished=False
        ppodd.logger.info("*** Finished Processing ***")
        self.clearmods()



def date2time(fromdate):
    """ Convert a date to a time """
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
    """ A class for time stamping data     --
    
    Could at some time be replaced by something from pandas which handles timestamps
    """
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
        """ Resample at a different frequency """
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
        """ Match this timestamp to another time """
        return np.intersect1d(self,othertimes).view(type(self))
    def ismatch(self,othertimes):
        """ Find intersection of this and other time """
        return np.in1d(self,othertimes)
    def asindexes(self,start=None):
        """Only for 1d 1Hz"""
        if(start==None):
            start=np.min(self)
        result=np.asarray(self[:]-start,dtype=int)
        return result
    def tosecs(self,fromdate=None,dateformat='%Y %m %d'):
        """ Convert unix or other time to seconds past midnight """
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
    
    fortran routines don't like missing data
    
    --
    
    Could at some time be replaced by something from pandas which handles timestamps
    
    """
    def __new__(cls,data,times):
        data = np.asarray(data)
        obj = data.view(cls)
        obj.frequency=None
        if(len(data.shape)==2):
            obj.frequency=data.shape[1]
        obj.times=None

        #if (times!=None):
        if hasattr(times, '__iter__'):
            if(len(times)==len(data)):
                obj.times=times               
        #if(obj.times==None):
        if not hasattr(obj.times, '__iter__'):
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
    def __setitem__(self,index,value):
        """ 
        """
        np.ndarray.__setitem__(self,index,value)
        try:
            np.ndarray.__setitem__(self.times,index,value.times)
        except (ValueError,AttributeError):
            pass
            
    def timesort(self):
        i1=np.arange(self.shape[0])
        i2=np.argsort(self.times)
        self[i1]=self[i2]            
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
        """Only for 2d 1Hz times"""
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
        d[~np.isfinite(d)]=d.fill_value
        if mask is not None:
            ind=self.twod_array(ind,indexes1d=True)
            xind=self.twod_array(xind)
            msk[ind[xind]]|=mask[xind]
        d.mask=msk
        if(returntimes):
            times=t1.at_frequency(self.frequency)
            d=(d,times)
        return d
        
    def get1Hz(self,angle=False):
        if(self.frequency>1):
            times=self.times
            if(angle):
                x=np.sum(np.cos(np.radians(self)),axis=1)
                y=np.sum(np.sin(np.radians(self)),axis=1)
                data=np.degrees(np.arctan2(y,x)) % 360
            else:
                data=np.average(self,axis=1)
            return timed_data(data,times)
        else:
            return self
        

class flagged_data(timed_data):
    """ Timed data with associated flag information """
    def __new__(cls,data,timestamp,flags,maxflag=3):
        obj = timed_data.__new__(cls,data,timestamp)
        obj.flag=np.asarray(flags) # timed_data(flags,timestamp)  
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
    def __setitem__(self,index,value):
        """ 
        """
        timed_data.__setitem__(self,index,value)
        try:
            np.ndarray.__setitem__(self.flag,index,value.flag)
        except AttributeError:
            pass

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

    def get1Hz(self,angle=False):
        if(self.frequency>1):
            flags=np.amin(self.flag,axis=1)
            times=self.times
            dat=self.raw_data[:]
            dat[np.isnan(dat)]=-9999.0
            weight=np.atleast_2d(flags).transpose()==self.flag
            if(angle):
                x=np.sum(np.cos(np.radians(dat))*weight,axis=1)
                y=np.sum(np.sin(np.radians(dat))*weight,axis=1)
                data=np.degrees(np.arctan2(y,x)) % 360
            else:
                data=np.average(dat,axis=1,weights=weight)
            return flagged_data(data,times,flags)
        else:
            return self

class cal_base(object):
    """ Base for all calibration modules.  Must be subclassed to do any processing,
        either directly or via fort_cal or file_read.
"""
    def __init__(self,dataset):
        """ Sub class should initialise the version inputs outputs and name as a minimum """
        self.dataset=dataset
        self.version=1.0
        self.runstate='ready'
        self.history=''
        i=self.input_names
        o=self.outputs
        self.name=self.__class__.__name__.upper()

    def getinput(self):
        if(self.input_names):
            self.inputs=self.dataset.getdata(*self.input_names)
        else:
            self.inputs={}
        return (len(self.inputs)==len(self.input_names))

    def getoutputnames(self):
        return [o.name for o in self.outputs]      

    def run(self):
        if(self.runstate=='ready'):
            self.runstate='running'
            if(self.getinput()):
                self.runstate='success' # can be altered by process if problems..
                try:
                    ppodd.logger.debug('Running...%s' % self.name)
                    self.process()
                    for o in self.outputs:
                        self.dataset[o.name]=o
                    self.addhistory()
                except Exception as e:
                    ppodd.logger.warning('Exception in ... %s' % self.name)
                    ppodd.logger.warning(str(e))
                    self.runstate='fail'
            else:
                self.runstate='fail'
                
    def addhistory(self):
        if(len(self.outputs)>0):
            self.dataset.history+='\n%s\n  Inputs=%s ,\n  Outputs=%s \n\n' % (self.name,
                str(self.input_names).replace("'",""),str(self.getoutputnames()).replace("'",""))
            self.history+='INPUTS\n'
            for i in self.input_names:
                try:
                    f=self.dataset[i].frequency
                    self.history+='  Parameter %s\n' % i
                except:
                    self.history+='  Constants %s=' % i
                    for c in self.dataset[i].data:
                        try:
                            self.history+='%e,' % c
                        except TypeError:
                            self.history+='%s,' % str(c)
                    self.history+='\n'
            self.history+='\n\nOUTPUTS\n'
            for o in self.outputs:
                self.history+=repr(o)+','+str(o)+'\n'
                
                
    def __repr__(self):
        return self.name



class file_read(cal_base):
    """ Base class for file reading modules """
    def __init__(self,dataset):
        cal_base.__init__(self,dataset)
        self.patterns=getattr(self,'patterns',('.*','*'))
        self.dataset.filetypes[self.input_names[0]]=self
            
    def getinput(self):
        ans=cal_base.getinput(self)
        try:
            self.parse_filenames(self.inputs[self.input_names[0]].data)
            if(not ans):
                ans=cal_base.getinput(self)
        except (KeyError,IndexError):
            pass
        return ans

    def filetest(self,filen):
        ans=False
        for patt in self.patterns:
            if fnmatch.fnmatch(os.path.basename(filen),patt):
                ans=True
        return ans
        
    def parse_filenames(self,files):
        from ppodd.util import fltno_date
        import dateutil.parser
        fltno,date=fltno_date(files)
        if('FLIGHT' not in self.dataset and fltno!='????'):
            self.dataset['FLIGHT']=constants_parameter('FLIGHT',fltno)
        if('DATE' not in self.dataset and date!='????????'):
            dt=dateutil.parser.parse(date)
            self.dataset['DATE']=constants_parameter('DATE',[dt.day,dt.month,dt.year])
 
    
    def process(self):
        for filename in self.inputs[self.input_names[0]].data:
            try:
                self.readfile(filename)
            except IOError as IOE:
                ppodd.logger.warning(IOE)  

    def fixfilename(self,filename):
        return filename

    def __repr__(self):        
        return self.name+' reads '+self.input_names[0]+' files'
                
        
             
class fort_cal(cal_base):
    """ Base class for calibration modules that call legacy fortran """
    def __init__(self,dataset):
        import os.path
        cal_base.__init__(self,dataset)
        self.pout=np.empty(len(self.outputs),dtype=np.int32,order='F')
        self.frqout=np.empty(len(self.outputs),dtype=np.int32,order='F')
        for i,p in enumerate(self.outputs):
            try:
                self.frqout[i]=p.frequency
            except AttributeError:
                self.frqout[i]=1
            self.pout[i]=p.number
        self.noutall=np.sum(self.frqout)
        self.fortname=getattr(self,'fortname',self.name) # Use the name as fortran module name unless explicitly set
    
    
    def process(self):
        """ Get the input data into an array matching the times..
        All input parameters must have a frequency and number set or will not be accepted as inputs
        Run the fortran
        Extract ouput into timestamped parameters     """
        from pod.c_runmod import c_runmod as run_old_module
        frqin=[]
        pin=[]
        inputs=[]
        constants=[]
        const=[]
        match=self.dataset.matchtimes(self.input_names,paras=inputs,notparas=constants)
        for c in constants:
            const.extend(c[:])
        for p in inputs:
            frqin.append(p.frequency)
            pin.append(p.number)
        constants=np.array(const,dtype=np.float32,order='F')    # Constants array
        frqin=np.array(frqin,dtype=np.int32,order='F')           # Input frequencies
        pin=np.array(pin,dtype=np.int32,order='F')               # Input parameter numbers
        length=len(match)
        if(length>0):
            """If there are data with any matching times"""
            din=np.empty((length,np.sum(frqin)),dtype=np.float32,order='F') # Input data
            flagin=np.zeros((length,np.sum(frqin)),dtype=np.int8,order='F') # Input flags
            ofs=0
            # Arrange inputs
            for i,p in enumerate(inputs):
                if(frqin[i]==1):
                    s=ofs
                else:
                    s=slice(ofs,ofs+frqin[i])    
                try:
                    if(p.name=='SECS'):
                        din[:,s]=match
                    else:   
                        din[:,s]=p.data.ismatch(match).raw_data
                except ValueError:
                    ppodd.logger.error('Problem with input %s in %s' % (p.name,self.name))
                    ppodd.logger.debug('S=%s' % s)
                    ppodd.logger.debug('Data Shape %s' % str(match.shape))
                    ppodd.logger.debug('DIN Shape %s' % str(din[:,s].shape))
                    raise ValueError
                try:
                    flagin[:,s]=p.data.flag.ismatch(match).raw_data 
                except:
                    pass
                ofs+=frqin[i]
            ppodd.logger.info('Calling fortran %s' % self.fortname)
            dout,flagout=run_old_module(self.fortname,constants,
                                        pin,frqin,din,flagin,
                                        self.pout,self.frqout,self.noutall)

            ofs=0
            for i,p in enumerate(self.outputs):
                frq=self.frqout[i]
                if(frq==1):
                    s=ofs
                else:
                    s=slice(ofs,ofs+frq)
                p.data=flagged_data(dout[:,s],match,flagout[:,s])
                ofs+=frq  

    def __repr__(self):        
        return self.name+' runs fortran '+self.fortname

