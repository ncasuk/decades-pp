#from ppodd.pod import *
from ppodd.core import *
import numpy as np
from netCDF4 import Dataset
from os.path import getsize
class readnc(file_read):
    """
Routine for reading in NETCDF data
"""
    def __init__(self,dataset):
        #self.name='READNC'
        self.input_names=['NETCDF']
        self.outputs=[]
        self.data=None
        self.patterns=('*.nc',)
        file_read.__init__(self,dataset)
   
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
                    if(n!='_FillValue'):
                        setattr(p,n,self.var[v].getncattr(n))
                self.outputs.append(p)
        for o in self.outputs:
            data=np.squeeze(self.var[o.name][:])
            try:
                m=data.mask
                data=data.data
            except AttributeError:
                pass
            if o.name+'_FLAG' in self.var:
                """ there is a flag """
                flag=np.squeeze(self.var[o.name+'_FLAG'][:])
                try:
                    m=flag.mask
                    flag=flag.data
                    flag[flag<0]=3  # Convert masked out missing data to flagged 3
                except AttributeError:
                    pass
                o.data=flagged_data(data,self.time,flag)
            else:
                o.data=timed_data(data,self.time)
    

        

