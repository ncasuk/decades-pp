from ppodd.core import *
import csv
import os.path
from os import listdir
import struct
class c_read1crio(file_reader):

    def __init__(self,dataset):
        print '_init_READ1CRIO'
        self.name='READ1CRIO'
        self.input_names=['DATE']
        self.filetype='CRIO'
        self.outputs=[]
        file_reader.__init__(self,dataset)
        self.patterns=('*.csv',)


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
        print 'Open CRIO file '+filename
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
        
        print 'There are %i definition files ' % len(deffiles)
        deffile=os.path.join(dirname,sorted(deffiles)[-1])
        print 'Using definition file = '+deffile
        defin=csv.reader(open(deffile,'rb'),delimiter=',')
        conv={'text':'S','unsigned_int':'>u','signed_int':'>i','double':'>f','single_float':'>f'
             ,'>unsigned_int':'>u','>signed_int':'>i','>double':'>f','>single_float':'>f'
             ,'<unsigned_int':'<u','<signed_int':'<i','<double':'<f','<single_float':'<f'}
        label=''
        outputs=[]
        dt=[]
        total=0
        for row in defin:
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
        print 'CRIO dtype=',dt
        print full_descriptor,' total packet length ',total
        print '%i files' % len(bins)
        print sorted(bins)
        data=np.zeros((0,),dtype=dt)
        for fil in sorted(bins):
            filen=os.path.join(dirname,fil)
            statinfo = os.stat(filen)
            size=statinfo.st_size
            if (size % total) != 0:
                print 'Data truncated',
            n=size/total
            print filen,n,
            offs=0        
            if(n>0):
                z=np.memmap(filen,dtype=dt,mode='r',shape=(n,)) # Try a simple read 
                if(np.any(z['label']!=full_descriptor)):
                    print 'Reading bytewise\n'
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
                        import matplotlib.pyplot as plt #Plots to diagnose weird message lengths
                        plt.figure()
                        plt.ion()
                        inds=np.array(inds)
                        plt.plot(inds[1:]-inds[:-1])
                        plt.title('Packet lengths for '+os.path.basename(filen))
                        pass
                else:
                    print 'Read Easily\n'
            if(n>0):
                data=np.append(data,z[:n],axis=0)
        dtype_names=[]
        for d in dt:
            dtype_names.append(d[0])
        print data.shape   
        if len(data)>0:
            
            if(label+'_utc_time' in dtype_names):
                time=timestamp(data[label+'_utc_time'],fromdate=self.dataset['DATE'].data)
                time,ind=np.unique(time,return_index=True)
                if(label+'_ptp_sync' in dtype_names):
                    good=data[ind][label+'_ptp_sync']=='1'
                    time=time[good]
                else:
                    good=np.arange(len(time))
            elif (label+'_time1' in dtype_names):
                # GIN ?
                ind=np.arange(len(data[label+'_time1']))
                gsecs=86400.0*self.ginday(self.dataset['DATE'].data)
                sgin=data[label+'_time1'][ind[0]]-gsecs
                if(sgin<0):
                    sgin+=(86400.0*7)  # if was started on the Sunday after the date ...
                tfrom=data[label+'_time2'][ind[0]] - sgin
                print 'Gin times ',sgin,tfrom
                time=timestamp(data[label+'_time2'][ind]-tfrom)
                good=np.arange(len(ind))
            else:
                raise ValueError('No recognised timing')             
            if len(np.where(data['label'] != full_descriptor)[0])!=0:
                print 'Could be corrupted'
            for o in outputs:
                if o.name==label+'_utc_time':
                    o.data=timed_data(time,time)
                    o.number=515
                else:
                    o.data=timed_data(data[o.name][ind[good]],time)

        self.outputs=getattr(self,'outputs',[])+outputs

    def ginday(self,fromdate):
        return (time.localtime(date2time(fromdate)).tm_wday+1) % 7  # day since saturday night

