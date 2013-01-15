from ppodd.cal_base import *
import numpy as np
from netCDF4 import Dataset
from os.path import getsize
class c_readnc(file_reader):
    def __init__(self,dataset):
        self.name='READNC'
        self.input_names=[]
        self.filetype='NC'
        self.outputs=[]
        self.data=None
        file_reader.__init__(self,dataset)
   
    def readfile(self,filename):
        self.outputs=[]
        self.file=Dataset(filename)
        self.var=self.file.variables
        for v in self.var:
            newpar=True
            if v.endswith('_FLAG'):
                if(v[:-5] in self.var):
                    newpar=False
            elif v=='Time':
                self.time=timestamp(self.var[v][:])
                newpar=False
            if newpar:
                p=parameter(v)
                for n in self.var[v].ncattrs():
                    setattr(p,n,self.var[v].getncattr(n))
                self.outputs.append(p)
        for o in self.outputs:
            if o.name+'_FLAG' in self.var:
                """ there is a flag """
                o.data=flagged_data(np.squeeze(self.var[o.name][:]),self.time,np.squeeze(self.var[o.name+'_FLAG'][:]))
            else:
                o.data=timed_data(np.squeeze(self.var[o.name][:]),self.time)
    

        

