from cal_base import *
import numpy as np
import struct
import os
class c_readm3(file_reader):
    def __init__(self,dataset):
        self.name='READM3'
        self.input_names=[]
        self.filetype='M3'
        self.outputs=[]
        file_reader.__init__(self,dataset)
        
    def openfile(self,filename):
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
        dtype=[('dontknow','|S%i' % self.paras[0]['Other'])]
        for n in range(self.npara):
            dtype.append((str(self.paras[n]['Shortname']),'<u2',int(self.paras[n]['Freq'])))
        dsize=2*np.sum(self.paras['Freq'])
        spare=2*self.iqrsiz-dsize-self.paras[0]['Other']
        dtype.append(('Spare','|S%i' % spare))
        time=self.get_time()
        self.time=timestamp(times=time)
        self.times={1:self.time}
        print dtype,self.dat,len(time)
        self.data=np.memmap(self.dat,dtype=dtype,mode='r',shape=len(time))
        for p in self.paras:
            n=p['Shortname']
            f=p['Freq']
            if(f in self.times):
                t=self.times[f]
            else:
                t=timestamp(self.time,times=time,frequency=f)                
                self.times.update({f:t})
            self.outputs.append(parameter(n,number=p['Numb'],frequency=f,
                                          description=p['Name'],data=timed_data(self.data[n],t,fill=65535)
                                          ,units=p['Units']))

    def get_time(self):
        T=[]
        for i in range(self.isectn):
            T+=range(self.issrtt[i],self.isendt[i]+1,1)
        return np.array(T)

    def get_timex(self):
        return np.arange(self.issrtt[0],self.isendt[self.isectn-1]+1,1)

    def paradesc(self,descfile=None):
        if(descfile==None):
            descfile=os.path.join(os.environ['CALTEXT'],'MFDPARDESC.DAT')
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
                    self.paras[i]['Shortname']=line[13:17].strip()
        return

 
