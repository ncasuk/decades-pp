from ppodd.core import *
import ppodd
#from ppodd.pod import *
import time
import os.path
from netCDF4 import Dataset
import ppodd
import glob

class write_fltcons(cal_base):
    """ Write data out to a NetCDF
    """
    def __init__(self,dataset):
        self.outputs=[]
        self.input_names=['DATE']
        self.name='WRITE_FLTCONS'
        self.version=1.00
        cal_base.__init__(self,dataset)
    
    def process(self,output):
        self.filename=output
        if(os.path.isfile(self.filename)):
            logopener='Updating flight constants'
            opener='r+'
            ppodd.logger.info('{} NetCDF {}'.format(logopener,self.filename))
            self.coredata=Dataset(self.filename,opener)
            paras=self.dataset.keys()
            for p in paras:
                try:
                    if self.dataset[p].type=='Constants':
                        data=self.dataset[p].data
                        try:
                            data=np.array(data,'f8')
                        except Exception as e:
                            pass
                        try:
                            c=self.coredata.setncattr(p,data)
                        except Exception as e:
                            print("something up with {} - {}".format(p,e))
                except Exception as e:
                    print("Problem with {} - {}".format(p,e))
            self.coredata.close()     
