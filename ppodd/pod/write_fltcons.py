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
    
    def process(self,output=None,nctype='NETCDF4',paras=None,onehz=False,fill_value=-9999,dtyp='f4',flag_fill=-1):
        possible_types=['NETCDF3_CLASSIC','NETCDF4', 'NETCDF4_CLASSIC', 'NETCDF3_64BIT']
        if(nctype in possible_types):
            self.netcdf_type=nctype
        else:
            self.netcdf_type='NETCDF4'
        self.filename=output
        if(output and os.path.isdir(output)):
            folder=output
            self.filename=None
        else:
            folder=os.path.expandvars('$NCDATA')
        if(self.filename is None):
            try:
                self.filename=os.path.join(folder,'core_faam_%4.4i%2.2i%2.2i_' % tuple(self.dataset['DATE'][-1::-1]))
                self.filename+='%s_r%1.1i_%s' % (ppodd.version,self.dataset['revision'].data,self.dataset['FLIGHT'][:])
                if(onehz):
                    self.filename+='_1hz'
                self.filename+='.nc'
            except KeyError:
                ppodd.logger.warning("""Unknown DATE or FLIGHT to create filename
Saving as output.nc""")
                self.filename='output.nc'
        opener='w'
        logopener='Creating'
        if(os.path.isfile(self.filename)):
            logopener='Updating'
            opener='r+'
        ppodd.logger.info('{} NetCDF {}'.format(logopener,self.filename))
        self.coredata=Dataset(self.filename,opener,format=self.netcdf_type)
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
