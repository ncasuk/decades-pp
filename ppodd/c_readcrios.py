from ppodd.core import *
from ppodd.c_read1crio import c_read1crio
import os.path
from os import listdir
class c_readcrios(c_read1crio):
    """
    Reads all cRIO data in one folder
    """
    def __init__(self,dataset):
    
        print '_init_READCRIOS'
        self.name='READCRIOS'
        self.input_names=['DATE']
        self.filetype='CRIOS'
        self.outputs=[]
        file_reader.__init__(self,dataset)
        self.patterns=('*.csv',)

    def fixfilename(self,filename):
        if os.path.isdir(filename):
            return filename
        else:
            return os.path.dirname(filename)
                

    def readfile(self,filename):
        """
        Looks for the .csv definition files then calls c_read1crio 
        to read in data from each type
        """
        already_done=[]
        for f in listdir(filename):
            if f.endswith('.csv'):
                ftype=f[:6]
                if(ftype not in already_done):
                    already_done.append(ftype)
                    c_read1crio.readfile(self,os.path.join(filename,ftype))


