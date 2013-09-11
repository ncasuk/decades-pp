'''
Created on 4 Sep 2013

All the base classes for dealing with timed aircraft data
    decades_dataset
    which is a dictionary of 
        parameters or constants_parameters
        
    The data for each parameter is timed_data ( a data array linked to a timestamp object )
    or flagged_data which also includes a flag
    
    cal_base which is the base class for all calibration modules, which has subclasses 
        file_reader - for reading file inputs, and
        fort_cal - for running legacy fortran code
        or it can be subclassed directly for processing in a purely pythonic way.
        
    

@author: Dave Tiddeman
'''
import time
import numpy as np
from scipy.interpolate import interp1d
try:
    from collections import OrderedDict
except ImportError:
    from OrderedDictOld import OrderedDict

version='v004'

class constants_parameter:
    """ The class for describing constants """
    def __init__(self,name,values):
        self.name=name
        self.data=values
    def __getitem__(self,sl):
        return self.data[sl]
    def __str__(self):
        return self.name+'(Constant)'
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
    
class decades_dataset(OrderedDict):
    """ A dataset made up of a dictionary of parameters, and information about files etc """
    def __init__(self,*args,**kwargs):
        self.files=[]
        self.atts={}
        self.history=''
        self.atts['conventions']='CF-1.0'
        self.atts['source']='FAAM BAe-146 Aircraft Data'
        self.atts['references']='http://www.faam.ac.uk'
        self.atts['institution']='FAAM'
        self.atts['format_version']='1.0'
        self.atts['revision']=0
        self.usedmods=[]
        self.modules=OrderedDict()
        self.notlist=[]
        self.starttime=None
        self.endtime=None
        self.filetypes={}
        self.nocals=set()
        """ Import all the calibration modules
        The names are listed in calnames
        """
        #self.calnames=[]
        for m in self.getmodules():
            exec 'from %s import %s' % (m,m)
            if self.is_calmodule(eval('%s' % m)):
                exec 'self.modules["%s"]=%s(self)' % (m[2:].upper(),m)
        for m in self.modules.values():
            try:
                self.filetypes[m.filetype]=m
            except AttributeError:
                pass
        OrderedDict.__init__(self,*args,**kwargs)

    def getmodules(self):
        """ Gets a list of all the c_*.py files in this folder """
        import os.path
        import fnmatch
        import sys
        ldir=sorted(os.listdir(os.path.dirname(__file__)))
        for fil in ldir:
            if fnmatch.fnmatch(fil,'c_*.py'):
                yield (os.path.basename(fil))[:-3]

    def is_calmodule(self,mod):
        """ Checks whether it is a calibration module, by making sure it has a 'process' member """
        import inspect
        ans=False
        if inspect.isclass(mod):
            if issubclass(mod,cal_base):
                for n,typ in inspect.getmembers(mod):
                    if n=='process':
                        ans=True
        return ans



    def __getatts__(self):
        ans=self.atts
        ans['history']=self.history
        try:
            ans['files']=''
            for f,t in self.files:
                #ans['files']+=f+':'+self.files[f]+'\n'
                ans['files']+=f+':'+t+'\n'
        except KeyError:
            pass
        if 'Title' not in ans:
            try:
                dt=time.gmtime(date2time(self['DATE'][:]))
                ans['Title']='Data from %s on %s' % (self['FLIGHT'][:],time.strftime('%d-%b-%Y',dt))
            except KeyError:
                pass
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
        self.files.append((filename,filetype))

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
                print p,len(paras),frqin
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
        
    def process(self,write=False,calmods=[],nocals=[],outparas=None,start=None,end=None):
        """ Sorts calibrate modules - so they are run in order of availability of their inputs 
        and do the processing """
        self.starttime=start
        self.endtime=end
        cals=[]
        if(not(calmods)):
            calmods=self.modules.keys()
        for c in calmods:
            if(c in self.modules.keys()):
                cals.append(self.modules[c])
            else:
                print 'Warning:Module C_%s not available' % c
        notadded=cals            
        self.usedmods=[]
        self.notlist=[]
        finished=False
        self.nocals=set(nocals)
        while(len(notadded)>0):
            """ Keep going while more modules to add """
            for cal in notadded:
                """ For each unrun module """
                #paras=self.para_names()
                paras=self.keys()
                if (outparas is not None):
                    """ If all output parameters are present then we have finished """
                    finished=True
                    for i in outparas:
                        if(i not in paras):
                            finished=False
                ok=False
                if(not(finished)):
                    """ Load the modules """
                    c=cal
                    if(c.name=='WRITE_NC'):
                        """ If it is the writing module sync the output parameters with it """
                        if (outparas):
                            c.input_names=['DATE','FLIGHT']+outparas
                        else:
                            if(write):
                                outparas=c.input_names
                                outparas.remove('DATE')
                                outparas.remove('FLIGHT')
                    inp=c.input_names
                    ok=True
                    """ Run the module if not listed in self.nocals and have all the inputs """
                    if c.name in self.nocals:
                        ok=False
                    for i in inp:
                        if(i not in paras):
                            ok=False                
                if(ok):
                    """ Run the module... Only run WRITE_NC if write is True """
                    print 'PROCESSING .. '+c.name
                    if ((c.name=='WRITE_NC') & write) or (c.name!='WRITE_NC'):                        
                        self.usedmods.append(c)
                        c.run()
                else:
                    """ Not running the module this time add to dataset.notlist """
                    self.notlist.append(cal)             
            if((sorted(self.notlist)==sorted(notadded)) or finished):    # probably need some sort of loop
                """ If we have failed to run anything in the last loop, or we know we have finished """
                for cal in self.notlist:
                    c=cal
                    if(c.name=='WRITE_NC' and write):
                        """ If we still haven't written anything despite the write flag then we should try even though we 
                        don't have all the parameters requested 
                        If all parameters requested output everything """
                        if (outparas is not None):
                            if(outparas==['all']):
                                c.input_names=paras
                            else:
                                c.input_names=['DATE','FLIGHT']+outparas
                        self.usedmods.append(c)
                        print 'PROCESSING '+c.name
                        c.run()
                        finished=True
                    else:
                        """ If we haven't run the module add it to the nocals list """
                        self.nocals.update([c.name])
                break # Break out of the loop as we have finished
            notadded=self.notlist
            self.notlist=[]
        print "*** Finished Processing ***"
        return self.usedmods



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
    """ A class for time stamping data """
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

    def get1Hz(self):
        if(self.frequency>1):
            flags=np.amin(self.flag,axis=1)
            times=self.times
            weight=np.atleast_2d(flags).transpose()==self.flag
            data=np.average(self,axis=1,weights=weight)
            return flagged_data(data,times,flags)
        else:
            return self

class cal_base(object):
    """ Base for all calibration modules """
    def __init__(self,dataset):
        """ Sub class should initialise the version inputs outputs and name as a minimum """
        self.dataset=dataset
        self.version=1.0
        self.history=''

    def get_inputs(self):
        return [i.get_para() for i in self.input_names]
        
    def run(self):
        self.process()
        for o in self.outputs:
            self.dataset.update({o.name:o})
        self.addhistory()

    def process(self):
        pass
    
    def addhistory(self):
        if(len(self.outputs)>0):
            self.dataset.history+='\n%s\n  Inputs=%s ,\n  Outputs=%s \n\n' % (self.name,str(self.input_names),str(self.outputs))
            self.history+='INPUTS\n'
            for i in self.input_names:
                try:
                    f=self.dataset[i].frequency
                    self.history+='  Parameter %s\n' % i
                except:
                    self.history+='  Constants %s=' % i
                    for c in self.dataset[i][:]:
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

class file_reader(cal_base):
    """ Base class for file reading modules """
    def __init__(self,dataset):
        cal_base.__init__(self,dataset)
        self.patterns=('.*','*')
        if(not(self.__doc__)):
            self.__doc__='Routine for reading in %s data' % self.filetype
    def process(self):
        self.files=[]
        for filename,filetype in self.dataset.files:
            if(filetype==self.filetype):
                self.files.append(filename)
        for filename in self.files:
            self.readfile(filename)  

    def fixfilename(self,filename):
        return filename

    
            
        
             
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
        try:
            fdir=os.path.join(os.path.dirname(__file__),'fortran_modules')
            with open(os.path.join(fdir,'c_'+self.fortname.lower()+'.for'),'r') as fmod:
                comments=''
                for fline in fmod:
                    if(fline.startswith('C') or fline.startswith('!')):
                        comments+=fline[1:]
                    elif(fline.startswith('      SUBROUTINE')):
                        break
                try:
                    self.__doc__+=comments
                except TypeError:
                    self.__doc__=comments
        except IOError:
            pass            
    
    
    def process(self):
        """ Get the input data into an array matching the times..
        All input parameters must have a frequency and number set or will not be accepted as inputs
        Run the fortran
        Extract ouput into timestamped parameters     """
        from ppodd.c_runmod import c_runmod as run_old_module
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
                    din[:,s]=p.data.ismatch(match).raw_data
                except ValueError:
                    print 'S=',s
                    print 'Data',p.data.shape
                    print 'Match',match.shape
                    print p.data.ismatch(match).raw_data.shape
                    print din[:,s].shape
                    raise ValueError
                try:
                    flagin[:,s]=p.data.flag.ismatch(match).raw_data 
                except:
                    pass
                ofs+=frqin[i]
            # Call FORTRAN
            print 'Calling fortran %s' % self.fortname
            dout,flagout=run_old_module(self.fortname,constants,
                                        pin,frqin,din,flagin,
                                        self.pout,self.frqout,self.noutall)

            # Arrange ouputs
            ofs=0
            for i,p in enumerate(self.outputs):
                frq=self.frqout[i]
                if(frq==1):
                    s=ofs
                else:
                    s=slice(ofs,ofs+frq)
                p.data=flagged_data(dout[:,s],match,flagout[:,s])
                ofs+=frq
        cal_base.process(self)    

