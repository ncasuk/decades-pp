from cal_base import *
import numpy as np
import struct
import csv
import os
class c_read1crio(file_reader):

    def __init__(self,dataset):
        print '_init_READ1CRIO'
        self.name='READ1CRIO'
        self.input_names=['DATE']
        self.filetype='CRIO'
        self.outputs=[]
        file_reader.__init__(self,dataset)

    def openfile(self,filename):
        """ require 2 parts the definition and the data
            may have several data files that need combining 
            Assume they are all in one folder.
            Assume the name given is just
            folder/type
            eg.
            decades_data/Bxxx/AERACK01
            so any thing in folder/*type*.bin is data
            and             folder/type_TCP*.csv is definition
        """
        print 'Open CRIO file '+filename
        dirname=os.path.dirname(filename)
        file_type=os.path.basename(filename)[0:8]
        ls=os.listdir(dirname)
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
        conv={'text':'S','unsigned_int':'>u','signed_int':'>i','double':'>f','single_float':'>f'}
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
        data=np.zeros((0,),dtype=dt)
        for fil in sorted(bins):
            filen=os.path.join(dirname,fil)
            statinfo = os.stat(filen)
            size=statinfo.st_size
            if (size % total) != 0:
               print 'Data truncated',
            n=size/total
            print filen,n,
            offset=0
            mind=-1
            allok=True
            notdone=True
            while(notdone & (n>0)):
                d=np.memmap(filen,dtype=dt,mode='r',shape=(n,),offset=offset)
                if(np.any(d['label']!=full_descriptor)):
                    """ Not matching labels.... """
                    allok=False
                    if(d[0]['label']==full_descriptor):
                        mind=np.where(d['label']!=full_descriptor)[0][0]
                        data=np.append(data,d[:mind],axis=0)
                        offset+=mind*total+1
                        #print 'Ind ',mind,offset
                    else:
                        offset+=1
                        #print 'Offset ',offset
                else:
                    notdone=False
                    data=np.append(data,d,axis=0)    
                n=(size-offset)/total
            if not allok:
                print 'Some mismatches',len(data)
            else:
                print 'OK',len(data)
            
        if len(data)>0:
            time=timestamp(data[label+'_utc_time'],fromdate=self.dataset['DATE'].data)
            time,ind=np.unique(time,return_index=True)
            if len(np.where(data['label'] != full_descriptor)[0])!=0:
               print 'Could be corrupted'
            good=data[ind][label+'_ptp_sync']=='1'
            time=time[good]
            for o in outputs:
                if o.name==label+'_utc_time':
                    o.data=timed_data(time,time)
                    o.number=515
                else:
                    o.data=timed_data(data[o.name][ind[good]],time)

        self.outputs=getattr(self,'outputs',[])+outputs

