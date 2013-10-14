#from ppodd.pod import *
from ppodd.core import *
import ppodd
import csv
import os.path
from os import listdir
import struct
class readwvssii(file_read):
    """
Routine for reading in WVSSII data
"""

    def __init__(self,dataset):
        self.input_names=['WVSSII','DATE']
        self.outputs=[]
        self.patterns=('WVSS*.txt',)
        file_read.__init__(self,dataset)
        
    def fixfilename(self,filename):
        if os.path.isdir(filename):
            return filename
        else:
            return os.path.dirname(filename)

    def readfile(self,filename):
        dx=self.dataset['DATE']
        start=np.datetime64('%4.4d%2.2d%2.2d' % (dx[2],dx[1],dx[0]))
        converttime = lambda x: (np.float_(np.datetime64(x[:8]+'-'+x[8:10]+':'+x[10:12]+':'+x[12:])-start))/1e6
        dirname=os.path.dirname(filename)
        file_type=os.path.basename(filename)[0:7]
        dtype=[]
        outputs=[]
        for i in [('Time','s'),('VMR','ppm'),('Press','hPa'),('Temp','C'),('hkp1',),('hkp2',),('hkp3',),('hkp4',),('hkp5',),('hkp6',),('hkp7',),('hkp8',)]:
            try:
                name=i[0]
                units=i[1]
            except:
                name=i[0]
                units='raw'
            if(name=='Time'):
                dtype.append((name,'f8'))
            else:
                dtype.append((name,'f4'))
                outputs.append(parameter(file_type+name,long_name=name,units=units,frequency=0))


        raw=np.genfromtxt(filename,dtype=dtype,converters={0:converttime})
        time=timestamp(raw['Time'])
        for o in outputs: 
             o.data=timed_data(raw[o.long_name],time)
        self.outputs=getattr(self,'outputs',[])+outputs
               
