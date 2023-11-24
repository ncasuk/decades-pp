from ppodd.core import *
import os.path


class read_crio(file_read):
    """
Routine for reading in CRIO data
"""

    def __init__(self,dataset):
        self.outputs=[]
        self.patterns=(self.input_names[0]+'*.csv',)
        file_read.__init__(self,dataset)


    def fixfilename(self,filename):
        d=os.path.dirname(filename)
        b=os.path.basename(filename)
        return os.path.join(d,b[:6])
        
    def read_tcp_defin(self,deffile):
        import csv
        defin=csv.reader(open(deffile,'rb'),delimiter=',')
        conv={'text':'S'}
        for en in {'':'>','>':'>','<':'<'}.items():
            for ty in {'unsigned_int':'u','int':'i','signed_int':'i','double':'f','single_float':'f',
                       'float':'f','single':'f','double_float':'f','boolean':'u','f':'f','i':'i','u':'u'}.items():
                conv[en[0]+ty[0]]=en[1]+ty[1]
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
                            if(row[0]=='packet_length'):
                                self.pack_len=np.dtype(dt[-1][1])
        except Exception as e:
            ppodd.logger.warning('Invalid CRIO definition %s' % deffile)
            ppodd.logger.warning(str(e))
            return
        self.total=total
        self.dtype=dt
        self.label=label
        self.full_descriptor=full_descriptor
        return outputs    

    def read_slowly(self,n,filen,debug_lengths=True):
        ppodd.logger.warning("Weird message lengths %s" % filen)
        offs=0
        z=np.empty((n,),dtype=self.dtype)
        n=0
        with open(filen, "rb") as f:
            strdata=f.read()
        nl=0
        inds=[]
        lens=[]
        lfd=len(self.full_descriptor)
        lpl=self.pack_len.itemsize
        packlen=np.array([self.total-lpl-lfd],dtype=self.pack_len)
        try:
            strtstr=self.full_descriptor+packlen.data[:] # Use this to only search for correct length packets
            while(True):
                nl=strdata.index(strtstr,nl)
                inds.append(nl)
                if(nl+self.total<=len(strdata)):
                    z.data[offs:offs+self.total]=strdata[nl:nl+self.total]
                    n+=1
                offs+=self.total
                nl+=self.total
                
        except ValueError:
            if(debug_lengths):
                import matplotlib.pyplot as plt #Plots to diagnose weird message lengths
                plt.figure()
                #plt.ion()
                inds=np.array(inds)
                plt.plot(inds[1:]-inds[:-1],label='Packet separation')
                plt.plot([0,len(inds)],[self.total,self.total],label='Correct')
                plt.title('Packet lengths for '+os.path.basename(filen))
                plt.legend()
        return n,z[:n]

    def read_tcp_data(self,dirname,bins):
        data=np.zeros((0,),dtype=self.dtype)
        for fil in sorted(bins):
            ppodd.logger.info('Reading %s packet length %i' % (fil,self.total))
            filen=os.path.join(dirname,fil)
            statinfo = os.stat(filen)
            size=statinfo.st_size
            if (size % self.total) != 0:
                ppodd.logger.warning('Data truncated %s' % filen)
            n=size/self.total
            if(n>0):
                #z=np.memmap(filen,dtype=self.dtype,mode='r',shape=(n,)) # Try a simple read 
                z=np.fromfile(filen,dtype=self.dtype) # Try a simple read 
                if(np.any(z['label']!=self.full_descriptor)):
                    n,z=self.read_slowly(n,filen)
            if(n>0):
                data=np.append(data,z,axis=0)
        return data
        
    def check_times(self,data):
        dtype_names=[d[0] for d in self.dtype]
        if(self.label+'_utc_time' in dtype_names):
            self.rawtimes=data[self.label+'_utc_time']
            times=timestamp(data[self.label+'_utc_time'])
            times,ind=np.unique(times,return_index=True)
            starttime=date2time(self.dataset['DATE'].data)
            twodays=(times-starttime>=np.timedelta64(0,'s')) & (times-starttime<np.timedelta64(2*24*3600,'s'))
            if((~twodays).any()):
                ppodd.logger.warning("Some %s times out of range - Ignoring" % self.label)
            if(self.label+'_ptp_sync' in dtype_names):
                good=(data[ind][self.label+'_ptp_sync']=='1') & twodays
                if(np.all(good==False)):
                    good=twodays
            else:
                good=twodays
            times=times[good]
        else:
            ppodd.logger.warning('No recognised timing')
            return
        return ind[good],times         

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
        #dirname=filename
        #file_type=self.input_names[0] # 
        file_type=os.path.basename(filename)[:6]
        ls=os.listdir(dirname)
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
        outputs=self.read_tcp_defin(deffile)
        data=self.read_tcp_data(dirname,bins)
        if len(data)>0:
            try:
                good,times=self.check_times(data)
                for o in outputs:
                    o.data=timed_data(data[o.name][good],times)
                self.outputs=getattr(self,'outputs',[])+outputs
            except TypeError as TE:
                ppodd.logger.warning(TE)
                ppodd.logger.warning('No %s data' % file_type)

class read_criox(read_crio):
    """
Routine for reading in unspecified cRIO data
"""

    def __init__(self,dataset):
        self.input_names=['CRIO','DATE']
        read_crio.__init__(self,dataset)
        self.patterns=('*.csv',)

class read_fltsumm(file_read):
    def __init__(self,dataset):
        self.input_names=['FLTSUMM']
        self.patterns=('FLTSUMM*.csv',)
        self.outputs=[]
        file_read.__init__(self,dataset)
    
    def readfile(filename):
        ppodd.logger.warning('No FLTSUMM reading capability yet')

