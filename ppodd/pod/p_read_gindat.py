from ppodd.core import *
from p_read_crios import read_crio
import struct

class read_gindat(read_crio):
    """
Routine for reading in AERACK data
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
        dtype_names=[d[0] for d in self.dtype]
        if (self.label+'_time1' in dtype_names):
            ind=np.arange(len(data[self.label+'_time1']))
            times=gintime(data[self.label+'_time1'],data[self.label+'_time2'],self.dataset['DATE'].data)
            good=ind
        else:
            ppodd.logger.warning('No recognised timing')
            return
        return ind[good],times         
            
def ginday(fromdate):
    return (time.gmtime(date2time(fromdate)).tm_wday+1) % 7  # day since saturday night
    
def gintime(time1,time2,date):
    day=86400.0
    week=day*7
    gsecs=day*ginday(date)
    sgin=time1[0]-gsecs
    ppodd.logger.debug('GSECS=%d' % gsecs)
    ppodd.logger.debug('TIME1[0]=%d' % time1[0])
    ppodd.logger.debug('SGIN=%d' % sgin)
    if(sgin<0):
        sgin+=week  # if was started on the Sunday after the date ...
    tfrom=time2[0] - sgin
    diff1=time1[1:]-time1[:-1]
    diff2=time2[1:]-time2[:-1]
    """ If GIN is restarted - time2 will probably decrease, and unless this happens over the transition from Sat to Sun
        time1 will increase - so correct time2 with time1
        If both decrease - an unfortunately turn of events
        Assume crossed over midnight Saturday and add a weeks worth of seconds
    """
    if np.any(diff2<0):
        timereverse=np.where(diff2<0)[0]
        for incident in timereverse[::-1]:
            if(diff1[incident]<(-6*day)):
                diff1[incident]+=week
            time2[incident+1:]+=diff1[incident]-diff2[incident]
    ppodd.logger.debug('TIME2[0]=%d' % time2[0])
    ppodd.logger.debug('TFROM=%d' % tfrom)
    return timestamp(time2-tfrom)
       
        
            

