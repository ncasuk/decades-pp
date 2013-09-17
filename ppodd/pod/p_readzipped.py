from ppodd.pod import *
from ppodd.core import *
from ppodd.pod.p_readcrios import readcrios
from ppodd.pod.p_readm3 import readm3
from ppodd.util import *
import os
import zipfile
import tempfile
import shutil

class readzipped(readcrios,readm3):
    """ Unzip and read in raw data files
              
cRIO data or old M3 raw data zipped in a file is unzipped to 
a temporary file and read in 

@author: Dave Tiddeman
"""
    def __init__(self,dataset):
        #self.name='READZIPPED'
        self.input_names=['FLIGHT','DATE']
        self.filetype='ZIP'
        self.outputs=[]
        self.data=None
        file_reader.__init__(self,dataset)
        self.patterns=('*.zip',)
   
    def fixfilename(self,filename):
        return filename
        
    def readfile(self,filename):
        rd=tempfile.mkdtemp() 
        z=zipfile.ZipFile(filename)
        print 'Unzip %s to %s' % (filename,rd)
        #self.dataset.add_file(rd,'CRIOS')
        hascsv=False
        oldraw=None
        for name in z.namelist():
            try:
                nameout=os.path.join(rd,os.path.basename(name))
                if(nameout.find(';')>-1):
                    nameout=nameout[:nameout.find(';')]
                fout = open(nameout, 'wb')
                fout.write(z.read(name)) 
                fout.close()
                if(nameout.endswith('.csv')):
                    hascsv=True
                if(nameout.endswith('raw_data.dat')):
                    oldraw=readm3.fixfilename(self,os.path.join(rd,nameout))
            except IOError as ioe:
                print ioe,
                print '"'+name+'"'   
        z.close()
        if(hascsv):
            readcrios.readfile(self,rd) 
        if(oldraw):
            readm3.readfile(self,oldraw)
        shutil.rmtree(rd)
