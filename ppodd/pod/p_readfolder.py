#from ppodd.pod import *
from ppodd.core import *
import os.path
from os import listdir
class readfolder(file_read):
    """
    Reads all cRIO data in one folder
    """
    def __init__(self,dataset):
        self.input_names=['FOLDER']
        self.outputs=[]
        self.patterns=('*',)
        file_read.__init__(self,dataset)

    def fixfilename(self,filename):
        if os.path.isdir(filename):
            return filename
        else:
            return os.path.dirname(filename)
                
    def filetest(self,filen):
        return os.path.isdir(filen)

    def add_file(self,filename,filetype):
        try:
            i=self.getoutputnames().index(filetype)
        except ValueError:
            i=-1
            if(filetype in self.dataset):
                self.outputs.append(self.dataset[filetype])
            else:
                self.outputs.append(file_parameter(filetype,[]))                
        self.outputs[i].data.update([filename])

    def readfile(self,filename):
        self.parse_filenames(listdir(filename))       
        for f in listdir(filename):
            df=self.dataset.DecadesFile(os.path.join(filename,f))
            if(df.filetype!='FOLDER'):
                """ Don't recurse into folders """
                self.add_file(*df.astuple())


