#from ppodd.pod import *
from ppodd.core import *
import csv
import os.path
from os import listdir
import struct

class readcrio(file_read):
    """
Routine for reading in CRIO data
"""

    def __init__(self,dataset):
        self.input_names=['CRIO','DATE']
        self.outputs=[]
        self.patterns=('*.csv',)
        file_read.__init__(self,dataset)


    def fixfilename(self,filename):
        d=os.path.dirname(filename)
        b=os.path.basename(filename)
        return os.path.join(d,b[:6])

    def readfile(self,filename):
        """ require 2 parts the definition and the data
            may have several data files that need combining 
            Assume they are all in one folder.
            Assume the name given is just
            folder/type
            eg.
            decades_data/Bxxx/AERACK01
            so any thing in folder/*type* is data
            and             folder/type_TCP*.csv is definition
        """
        ppodd.logger.info('Open CRIO file %s' % filename)
        dirname=os.path.dirname(filename)
        file_type=os.path.basename(filename)[:6]
        ls=listdir(dirname)
        bins=[]
        deffiles=[]
        for f in ls:
            if file_type in f:
                if f.endswith('.csv'):
                    deffiles.append(f)
                else:
                    bins.append(f)
        
        ppodd.logger.info('There are %i definition files ' % len(deffiles))
        deffile=os.path.join(dirname,sorted(deffiles)[-1])
        ppodd.logger.info('Using definition file = %s' % deffile)
        defin=csv.reader(open(deffile,'rb'),delimiter=',')
        conv={'text':'S','unsigned_int':'>u','signed_int':'>i','double':'>f','single_float':'>f'
             ,'>unsigned_int':'>u','>signed_int':'>i','>double':'>f','>single_float':'>f'
             ,'<unsigned_int':'<u','<signed_int':'<i','<double':'<f','<single_float':'<f'}
        label=''
        outputs=[]
        dt=[]
        total=0
        try:
            for row in defin:
                if(row):
                    if row[0]!='field' :
                        total+=int(row[1])
                        if(label==''):
                            full_descriptor=row[0]
                            label=full_descriptor[1:-2]
                            dt.append(('label','S'+row[1]))
                        elif((row[0]=='grpid') & (label=='G') & (row[1]=='2') & (row[2]=='2')):
                            label='GINDAT'
                            full_descriptor+=struct.pack('<H',1)
                            dt[0]=('label','S6')
                        else:
                            f=int(row[1])/int(row[2])
                            para=label+'_'+row[0]
                            outputs.append(parameter(para,frequency=f,
                                                  long_name=row[4],
                                                  units='RAW'))
                            if(f>1):
                                dt.append((para,conv[row[3]]+row[2],(f,)))
                            else:
                                dt.append((para,conv[row[3]]+row[2]))
        except ValueError:
            ppodd.logger.warning('Invalid CRIO definition %s' % deffile)
            return
        data=np.zeros((0,),dtype=dt)
        for fil in sorted(bins):
            filen=os.path.join(dirname,fil)
            statinfo = os.stat(filen)
            size=statinfo.st_size
            if (size % total) != 0:
                ppodd.logger.warning('Data truncated %s' % filen)
            n=size/total
            offs=0 
            if(n>0):
                z=np.memmap(filen,dtype=dt,mode='r',shape=(n,)) # Try a simple read 
                self.z=z
                if(np.any(z['label']!=full_descriptor)):
                    nfulld=len(full_descriptor)  # If the simple read fails read in byte at a time
                    z=np.empty((n,),dtype=dt)
                    n=0
                    with open(filen, "rb") as f:
                        strdata=f.read()
                    nl=0
                    inds=[]
                    try:
                        while(True):
                            nl=strdata.index(full_descriptor,nl)
                            inds.append(nl)
                            if(nl+total<=len(strdata)):
                                z.data[offs:offs+total]=strdata[nl:nl+total]
                                offs+=total
                                n+=1
                            nl+=total
                    except ValueError:
                        """import matplotlib.pyplot as plt #Plots to diagnose weird message lengths
                        plt.figure()
                        plt.ion()
                        inds=np.array(inds)
                        plt.plot(inds[1:]-inds[:-1])
                        plt.title('Packet lengths for '+os.path.basename(filen))"""
                        ppodd.logger.warning("Weird message lengths %s" % filen)
            if(n>0):
                data=np.append(data,z[:n],axis=0)
        dtype_names=[]
        for d in dt:
            dtype_names.append(d[0])
        if len(data)>0:
            
            if(label+'_utc_time' in dtype_names):
                self.rawtimes=data[label+'_utc_time']
                times=timestamp(data[label+'_utc_time'],fromdate=self.dataset['DATE'].data)
                times,ind=np.unique(times,return_index=True)
                twodays=(times>=0) & (times<2*24*3600)
                if((~twodays).any()):
                    ppodd.logger.warning("Some %s times out of range - Ignoring" % label)
                if(label+'_ptp_sync' in dtype_names):
                    good=(data[ind][label+'_ptp_sync']=='1') & twodays
                    if(np.all(good==False)):
                        good=twodays
                else:
                    good=twodays
                times=times[good]
            elif (label+'_time1' in dtype_names):
                # GIN ?
                ind=np.arange(len(data[label+'_time1']))
                times=gintime(data[label+'_time1'],data[label+'_time2'],self.dataset['DATE'].data)
                good=ind
            else:
                ppodd.logger.warning('No recognised timing')
                return             
            if len(np.where(data['label'] != full_descriptor)[0])!=0:
                ppodd.logger.warning('Could be corrupted')
            """
            for o in outputs:
                if o.name==label+'_utc_time':
                    o.data=timed_data(times,times)
                    o.number=515
                else:
                    o.data=timed_data(data[o.name][ind[good]],times) """
            tout=None
            for o in outputs:
                o.data=timed_data(data[o.name][ind[good]],times)
                                            

        self.outputs=getattr(self,'outputs',[])+outputs


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

