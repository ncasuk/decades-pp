import numpy as np
from c_runmod import c_runmod as run_old_module
from data import *
import types
class cal_base(object):
    """ Base for all calibration modules """
    def __init__(self,dataset):
        """ initialise the version inputs outputs and name as a minimum """
        self.dataset=dataset

    def get_inputs():
        return [i.get_para() for i in self.input_names]  
                        
    def __repr__(self):
        return self.name

class file_reader(cal_base):
    """ Base class for file reading modules """
    def __init__(self,dataset):
        print 'Init file_reader '
        cal_base.__init__(self,dataset)
        self.files=[]
        for filename,filetype in self.dataset.files.items():
            if(filetype==self.filetype):
                self.files.append(filename)
                self.openfile(filename)
    def process(self):
        pass 
            
        
             
class fort_cal(cal_base):
    """ Base class for calibration modules that call legacy fortran """
    def __init__(self,dataset):
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
        self.fortname=getattr(self,'fortname',self.name)
    
    
    def process(self):
        frqin=[]
        pin=[]
        inputs=[]
        constants=[]
        for i in self.input_names:
            p=self.dataset.get_para(i)
            try:
                frqin.append(p.frequency)
                pin.append(p.number)
                inputs.append(p)
                if inputs[-1].data is not None:
                    if(len(inputs)==1):
                        match=inputs[0].data.times
                    else:
                        match=inputs[-1].data.matchtimes(match)
                else:
                    match=timestamp([])
            except AttributeError:
                constants.extend(p[:])
        constants=np.array(constants,dtype=np.float32,order='F')
        frqin=np.array(frqin,dtype=np.int32,order='F')
        pin=np.array(pin,dtype=np.int32,order='F')
        length=len(match)
        if(length>0):
            din=np.empty((length,np.sum(frqin)),dtype=np.float32,order='F')
            flagin=np.zeros((length,np.sum(frqin)),dtype=np.int8,order='F')
            ofs=0
            for i,p in enumerate(inputs):
                if(frqin[i]==1):
                    s=ofs
                else:
                    s=slice(ofs,ofs+frqin[i])    
                try:    
                    din[:,s]=p.data.ismatch(match).raw_data  # (goodtimes=match)
                except ValueError:
                    print 'S=',s
                    print 'Data',p.data.shape
                    print 'Match',match.shape
                    print p.data.ismatch(match).raw_data.shape
                    print din[:,s].shape
                    raise ValueError
                try:
                    flagin[:,s]=p.data.flags.ismatch(match).raw_data  # (goodtimes=match)
                except:
                    pass
                #self.dataset.add_time(p.data.timestamp)
                ofs+=frqin[i]
            #notmiss=(din[:,0] != -9999) & (np.isfinite(din[:,0]))
            
            #for i in range(1,len(din[0,:]),1):
            #    notmiss&=(din[:,i] != -9999) & (np.isfinite(din[:,i]))
            #ind=np.where(notmiss)[0]
            print 'Calling fortran %s' % self.fortname
            dout,flagout=run_old_module(self.fortname,constants,
                                        pin,frqin,din,flagin,
                                        self.pout,self.frqout,self.noutall)
            ofs=0
            for i,p in enumerate(self.outputs):
                frq=self.frqout[i]
                #if ( frq not in self.dataset.get_times()):
                #    self.dataset.add_time(timestamp(self.dataset.get_base_time().basetime,times=self.dataset.get_base_time()[match],frequency=frq))
                if(frq==1):
                    s=ofs
                else:
                    s=slice(ofs,ofs+frq)
                if(p.name=='TWC_DET'):
                    print 'TWC_DET',s,flagout[5000,s]      
                p.data=flagged_data(dout[:,s],match,flagout[:,s])
                ofs+=frq
        return

