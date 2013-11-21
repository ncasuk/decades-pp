#from ppodd.pod import *
from ppodd.core import *
import ppodd
from ppodd.pod.p_readfolder import readfolder
import os
import zipfile
import tempfile
import shutil
import atexit

class readzipped(readfolder):
    """ Unzip and read in raw data files
              
cRIO data or old M3 raw data zipped in a file is unzipped to 
a temporary file and read in 

@author: Dave Tiddeman
"""
    def __init__(self,dataset):
        self.input_names=['ZIP']
        self.outputs=[]
        self.data=None
        self.patterns=('*.zip',)
        file_read.__init__(self,dataset)
   
    def fixfilename(self,filename):
        return filename

    def filetest(self,filen):
        return file_read.filetest(self,filen)
                
    def readfile(self,filename):
        try:
            tempfolder=self.dataset.tempfolder
        except AttributeError:
            tempfolder = tempfile.mkdtemp()
            atexit.register(shutil.rmtree,tempfolder)
        z=zipfile.ZipFile(filename)
        ppodd.logger.info('Unzip %s to %s' % (filename,tempfolder))
        for name in z.namelist():
            try:
                nameout=os.path.join(tempfolder,os.path.basename(name))
                if(nameout.find(';')>-1):
                    nameout=nameout[:nameout.find(';')]
                fout = open(nameout, 'wb')
                fout.write(z.read(name)) 
                fout.close()
            except IOError as ioe:
                ppodd.logger.warning(str(ioe)+' "'+name+'"')   
        z.close()
        self.dataset.tempfolder=tempfolder
        readfolder.readfile(self,tempfolder)

