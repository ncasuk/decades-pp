from ppodd.core import *
import struct
import os
class c_readm3(file_reader):
    def __init__(self,dataset):
        self.name='READM3'
        self.input_names=[]
        self.filetype='M3'
        self.outputs=[]
        file_reader.__init__(self,dataset)
        self.patterns=('*raw_data.dat','*raw_hddr.dat','*raw_data.dat;*','*raw_hddr.dat;*')
        
    def fixfilename(self,filename):
        return filename[:filename.find('.dat')-5]
        
    def readfile(self,filename):
        hddr='_hddr.dat'
        self.filename=filename
        try:
            hd=open(self.filename+hddr)
            self.dat=self.filename+'_data.dat'
        except:
            try:
                hddr='_HDDR.DAT'
                hd=open(self.filename+hddr)
                self.dat=self.filename+'_DATA.DAT'
            except:
                return 
        a=hd.read(512)
        im=struct.unpack('<128i',a)
        if(im[21]==2 or im[21]==3):
            self.type='M3'
            self.fltno=str(a[0:4])
            self.date=str(a[4:6])+'-'+str(a[6:9])+'-'+str(a[10:12]) 
            self.idprms=im[32]
            self.iqprms=im[33]
            self.iqsecs=im[34]
            a=hd.read(512)
            im=struct.unpack('<128i',a)
            self.isectn=im[0]
            self.issrtt=im[1:1+self.isectn]
            self.isendt=im[41:41+self.isectn]
            self.isrecd=im[81:81+self.isectn]
            vms=1
            self.iqrsiz=im[121]
            ia=struct.unpack('<256h',hd.read(512))
            self.npara=256
            if 0 in ia:
                self.npara=list(ia).index(0)
                self.paras=np.zeros((self.npara),dtype=[('Numb','<i4'),('Freq','<i4'),('Other','<i4'),
                                                ('Name','|S20'),('Units','|S8'),('Shortname','|S4')])
            self.paras['Numb']=ia[0:self.npara]
            ia=struct.unpack('<256h',hd.read(512))
            self.paras['Other']=(np.array(ia[0:self.npara])-1)*2
            ia=struct.unpack('<256h',hd.read(512))
            self.paras['Freq']=ia[0:self.npara]
            self.paradesc()
        hd.close()
        dtype=[('Horace_dontknow','|S%i' % self.paras[0]['Other'])]
        for n in range(self.npara):
            dtype.append(('Horace_'+str(self.paras[n]['Shortname']),'<u2',int(self.paras[n]['Freq'])))
        dsize=2*np.sum(self.paras['Freq'])
        spare=2*self.iqrsiz-dsize-self.paras[0]['Other']
        dtype.append(('Horace_Spare','|S%i' % spare))
        self.time=self.get_time()
        print dtype,self.dat,len(self.time)
        self.data=np.memmap(self.dat,dtype=dtype,mode='r',shape=len(self.time))
        for p in self.paras:
            n='Horace_'+p['Shortname']
            f=p['Freq']
            self.outputs.append(parameter(n,number=p['Numb'],frequency=f,
                                          long_name=p['Name'],data=timed_data(self.data[n],self.time)
                                          ,units=p['Units']))

    def get_time(self):
        T=[]
        for i in range(self.isectn):
            T+=range(self.issrtt[i],self.isendt[i]+1,1)
        return timestamp(T,dtype='f8')

    def get_BCDtime(self):
        s=10*((self.data['Horace_GMTM']%256)/16)+self.data['Horace_GMTM']%16
        m=10*(self.data['Horace_GMTH']%16)+self.data['Horace_GMTM']/256
        h=10*(self.data['Horace_GMTH']/256)+(self.data['Horace_GMTH']%256)/16
        return timestamp(h*3600+m*60+s,dtype='f8')

    def time_BCD(self):
        s0=self.time%10
        s1=(self.time.astype(int)/10)%6
        m0=(self.time.astype(int)/60)%10
        m1=(self.time.astype(int)/600)%6
        h0=(self.time.astype(int)/3600)%10
        h1=(self.time.astype(int)/36000)
        return (s0+s1*16+m0*256,m1+h0*16+h1*256)

    def get_timex(self):
        return np.arange(self.issrtt[0],self.isendt[self.isectn-1]+1,1)

    def paradesc(self,descfile=None):
        if(descfile==None):
            descfile=os.path.join(os.path.dirname(__file__),'MFDPARDESC.DAT')
        lines=[]
        try:
            desc=open(descfile)
            lines=desc.readlines()
            desc.close()
        except:
            pass
        for i in range(self.npara):
            self.paras[i]['Name']='PARAMETER %i' % self.paras[i]['Numb']
            self.paras[i]['Units']='BITS'
            self.paras[i]['Shortname']='P%i' % self.paras[i]['Numb']
            for line in lines[5:]:
                p=int(line[0:6])
                if p==self.paras[i]['Numb']:
                    self.paras[i]['Name']=line[29:].strip()
                    self.paras[i]['Units']=line[19:27].strip()
                    self.paras[i]['Shortname']=line[13:17].strip().replace('/','_')
        return

 
