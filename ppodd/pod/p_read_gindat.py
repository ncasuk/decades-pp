import struct
import numpy as np
import ppodd

from ppodd.core import timestamp
from p_read_crios import read_crio

class read_gindat(read_crio):
    """
Routine for reading in GIN data
"""
    input_names=['GINDAT','DATE']
    def __init__(self,dataset):
        #self.input_names=['GINDAT','DATE']
        read_crio.__init__(self,dataset)

    def read_tcp_defin(self,deffile):
        outputs=read_crio.read_tcp_defin(self,deffile)
        self.label='GINDAT'
        for o in outputs:
            if(o.name=='G_grpid'):
                outputs.remove(o)
        for o in outputs:
            o.name=o.name.replace('G_',self.label+'_')
        self.full_descriptor+=struct.pack('<H',1)
        self.dtype=[(d[0].replace('G_',self.label+'_'),d[1]) for d in self.dtype if d[0]!='G_grpid' ]
        self.dtype[0]=('label', 'S6')
        return outputs

    def read_slowly(self,n,filen):
        return read_crio.read_slowly(self,n,filen,debug_lengths=False)

    def check_times(self,data):
        ppodd.logger.debug('GINDAT check_times')
        dtype_names=[d[0] for d in self.dtype]
        if (self.label+'_time1' in dtype_names):
            ind=np.arange(len(data[self.label+'_time1']))
            times=gintime(data[self.label+'_time1'],self.dataset['DATE'].data)
            good=ind
        else:
            ppodd.logger.warning('No recognised timing')
            return
        if 'GIN_TIME_OFFSET' in self.dataset.keys():
            times+=np.timedelta64(int(self.dataset['GIN_TIME_OFFSET'][0]), 's')
        return ind[good],times         
            

def gintime(time1,date):
    day=date2time(date).astype('datetime64[D]')
    gstart=np.busday_offset(day, 0, roll='backward',weekmask='0000001')
    gtime=gstart+(time1*1e9).astype('timedelta64[ns]')
    back=gtime<gtime[0]
    gtime[back]+=np.timedelta64(7,'D')
    return timestamp(gtime,dtype='datetime64[ns]')
   