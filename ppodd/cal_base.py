import numpy as np
from c_runmod import c_runmod as run_old_module
from data import *
import types
class cal_base(object):
    def __init__(self,dataset):
        """ initialise the version inputs outputs and name as a minimum """
        self.dataset=dataset

    def get_inputs():
        return [i.get_para() for i in self.input_names]  
                        
    def __repr__(self):
        return self.name

class file_reader(cal_base):
    def __init__(self,dataset):
        cal_base.__init__(self,dataset)
        self.files=[]
        for filename,filetype in self.dataset.files.items():
            if(filetype==self.filetype):
                self.files.append(filename)
                self.openfile(filename)
    def process(self):
        pass 
            
        
             
class fort_cal(cal_base):
    
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
            except AttributeError:
                constants.extend(p[:])
        constants=np.array(constants,dtype=np.float32,order='F')
        frqin=np.array(frqin,dtype=np.int32,order='F')
        pin=np.array(pin,dtype=np.int32,order='F')
        match=inputs[0].data.match_times([i.data for i in inputs])
        length=len(inputs[0].data.raw_time(goodtimes=match))
        din=np.empty((length,np.sum(frqin)),dtype=np.float32,order='F')
        flagin=np.zeros((length,np.sum(frqin)),dtype=np.int8,order='F')
        ofs=0
        for i,p in enumerate(inputs):
            din[:,ofs:ofs+frqin[i]]=p.data.raw_data(goodtimes=match)
            try:
                flagin[:,ofs:ofs+frqin[i]]=p.data.flags.raw_data(goodtimes=match)
            except:
                pass
            self.dataset.add_time(p.data.timestamp)
            ofs+=frqin[i]
        notmiss=din[:,0] != -9999
        for i in range(1,len(din[0,:]),1):
            notmiss&=din[:,i] != -9999
        ind=np.where(notmiss)[0]
        dout,flagout=run_old_module(self.name,constants,
                                    pin,frqin,din[ind],flagin[ind],
                                    self.pout,self.frqout,self.noutall)
        ofs=0
        for i,p in enumerate(self.outputs):
            frq=self.frqout[i]
            if ( frq not in self.dataset.get_times()):
                self.dataset.add_time(timestamp(self.dataset.get_base_time().basetime,times=self.dataset.get_base_time()[match],frequency=frq))
            p.data=flagged_data(dout[:,ofs:ofs+frq],self.dataset.get_time(frq),flagout[:,ofs:ofs+frq])
            ofs+=frq
        return

