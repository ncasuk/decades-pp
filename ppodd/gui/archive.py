#!/usr/bin/env python
import Tkinter as Tk
import sys
#from views import viewarchive
from ppodd.core import decades_dataset
from ppodd.util import DecadesFile,fltno_date,find_file
from util import *
import os
import zipfile
import shutil
from tkFileDialog import askopenfilename
import re
from ppodd.ftpdata import ftpjob

class valFltno(ValidEntry):

    def check_key(self,P):
        return re.search('\W',P)==None and len(P)<=4
        
    def check_good(self,P):
        return re.search('^[a-z]\d{3}$',P)!=None

class valDate(ValidEntry):

    def check_key(self,P):
        return re.search('^\d*$',P)!=None and len(P)<=8

    def check_good(self,P):
        return re.search('^\d{8}$',P)!=None

class valRevision(ValidEntry):

    def check_key(self,P):
        return re.search('^\d*$',P)!=None and len(P)<=1

    def check_good(self,P):
        return re.search('^\d$',P)!=None


class viewarchive(ScrollFrame):
    def __init__(self,parent,dataset,**kwargs):
        self.printer=sys.stdout
        if('printer' in kwargs):
            self.printer=kwargs.pop('printer')
        self.setupdisplay(parent,**kwargs)
        self.dataset=dataset
        self.rawcore=os.path.expandvars("$RAWCORE")
        self.rawdata=os.path.expandvars("$RAWDATA")
        self.fltcons=os.path.expandvars("$FLTCONS")
        self.ncdata=os.path.expandvars("$NCDATA")
        self.fromfile=''
        for f in dataset.files:
            if(f[1]=='CRIOS'):
                if(self.rawdata not in f[0]):
                    self.fromfile=f[0]
            elif(f[1]=='ZIP'):
                if(self.rawcore not in f[0]):
                    self.fromfile=f[0]
            elif(f[1]=='CONST'):
                self.constfile=f[0]
                self.const['text']=f[0]
        try:
            self.ncfile=dataset.modules['WRITE_NC'].filename
        except AttributeError:
            self.ncfile=''
        self.setfiles()

    def setfiles(self):
        bn=os.path.basename(self.fromfile)
        self.iszip=False
        print bn
        fltno='X000'
        date=time.strftime('%Y%m%d')
        if(bn.endswith('.zip')):
            self.iszip=True
            print "Copy ZIP file"
            if re.search('^core_faam_\d{8}_r\d_[a-z]\d{3}_rawdlu.zip',bn):
                fltno,date,rev=fltno_date([bn])
                out=os.path.join(self.rawcore,bn)
            else:
                z=zipfile.ZipFile(self.fromfile)
                fltno,date,rev=fltno_date(z.namelist())
                z.close()
        elif(os.path.isdir(self.fromfile)):
            fltno,date,rev=fltno_date(os.listdir(self.fromfile))
        else:
            fltno,date,rev=fltno_date([])
        if(date<'20000101'):
            date=time.strftime('%Y%m%d')
        self.const.config(text='flight-cst_faam_%s_r0_%s.txt' % (date,fltno))

        self.enflt.val=fltno
        self.endat.val=date
        self.enrev.val='0'
        
        
        self.fromlab.config(text=self.fromfile)
        if(self.fromfile):
            self.rawcorebut['state']=Tk.NORMAL
            self.rawdatabut['state']=Tk.NORMAL
        else:
            self.rawcorebut['state']=Tk.DISABLED
            self.rawdatabut['state']=Tk.DISABLED
        self.nclab.config(text=self.ncfile)        
        if(self.iszip):
            self.rawcorebut.config(text='Copy to')
            self.rawdatabut.config(text='Unzip to')
        else:
            self.rawcorebut.config(text='Zip to')
            self.rawdatabut.config(text='Copy to')
        if(self.ncfile):
            self.ncftp['state']=Tk.NORMAL
        else:
            self.ncftp['state']=Tk.DISABLED        
        self.out()

    def out(self):
        outzip='$RAWCORE/core_faam_%s_r%s_%s_rawdlu.zip' % (self.endat.val,self.enrev.val,self.enflt.val)
        outdata='$RAWDATA/core_faam_%s_r%s_%s_rawdlu/' % (self.endat.val,self.enrev.val,self.enflt.val)
        self.rawcorelab.config(text=outzip)
        self.rawdatalab.config(text=outdata)
        self.find_const()
        self.iszipthere(outzip)

    def iszipthere(self,outzip):
        if(os.path.isfile(os.path.expandvars(outzip))):
            self.rawcoreftp['state']=Tk.NORMAL
        else:
            self.rawcoreftp['state']=Tk.DISABLED
    
    def find_const(self):
        ans=find_file(self.enflt.val,self.endat.val)
        if(ans!=''):
            self.const.config(text=ans)
            self.constcreate.config(text='Edit')
            self.constftp['state']=Tk.NORMAL
            found=False
            for i,f in enumerate(self.dataset.files):
                if f[1]=='CONST':
                    self.dataset.files[i]=(os.path.join(self.fltcons,ans),'CONST')
                    found=True
            if not found:
                self.dataset.add_file(os.path.join(self.fltcons,ans),'CONST')
        else:
            self.const.config(text='flight-cst_faam_%s_r0_%s.txt'  % (self.endat.val,self.enflt.val))
            self.constcreate.config(text='Create')
            self.constftp['state']=Tk.DISABLED

     
    def setupdisplay(self,parent,**kwargs):
        ScrollFrame.__init__(self,parent,**kwargs)
        self.parent=parent
        self.browse=Tk.Button(self,text='Browse',command=self.browse,**kwargs)
        self.browse.grid(row=0,column=0,columnspan=6)
        self.const=Tk.Label(self,**kwargs)
        self.constcreate=Tk.Button(self,text='Create',command=self.do_const,**kwargs)
        self.constftp=Tk.Button(self,text='FTP',command=self.ftp_const,**kwargs)
        self.const.grid(column=1,row=2,columnspan=4)
        self.constcreate.grid(column=0,row=2)
        self.constftp.grid(column=5,row=2)

        self.enflt=valFltno(self,command=self.out,width=4,**kwargs)
        self.endat=valDate(self,command=self.out,width=8,**kwargs)
        self.enrev=valRevision(self,command=self.out,width=1,**kwargs)
        
        Tk.Label(self,text='Flight',**kwargs).grid(column=0,row=1,sticky=Tk.E)
        self.enflt.grid(column=1,row=1,sticky=Tk.W)
        Tk.Label(self,text='Date',**kwargs).grid(column=2,row=1,sticky=Tk.E)
        self.endat.grid(column=3,row=1,sticky=Tk.W)
        Tk.Label(self,text='Revision',**kwargs).grid(column=4,row=1,sticky=Tk.E)
        self.enrev.grid(column=5,row=1,sticky=Tk.W)
        
        self.fromlab=Tk.Label(self,**kwargs)
        self.fromlab.grid(column=0,row=3,columnspan=6)
        
        self.rawcorelab=Tk.Label(self,**kwargs)
        self.rawcorebut=Tk.Button(self,text='Copy',command=self.tocore,**kwargs)
        self.rawcoreftp=Tk.Button(self,text='FTP',command=self.ftp_core,**kwargs)

        self.rawdatalab=Tk.Label(self,**kwargs)
        self.rawdatabut=Tk.Button(self,text='Copy',command=self.todata,**kwargs)
        
        self.rawcorelab.grid(column=1,row=4,columnspan=4)
        self.rawcorebut.grid(column=0,row=4)
        self.rawcoreftp.grid(column=5,row=4)

        self.rawdatalab.grid(column=1,row=5,columnspan=5)
        self.rawdatabut.grid(column=0,row=5)
        
        self.nclab=Tk.Label(self,**kwargs)
        self.ncftp=Tk.Button(self,text='FTP',command=self.ftp_nc,**kwargs)

        self.nclab.grid(column=0,row=6,columnspan=5)
        self.ncftp.grid(column=5,row=6)
        Tk.Label(self,text='FTP Login:',**kwargs).grid(column=0,row=7)
        self.login=ValidEntry(self,**kwargs)
        self.login.grid(column=1,row=7)
        Tk.Label(self,text='Password:',**kwargs).grid(column=3,row=7)
        self.password=ValidEntry(self,show="*",**kwargs)
        self.password.grid(column=4,row=7)
        

        


    def browse(self):
        filen=askopenfilename(initialdir=os.path.expandvars('$RAWCORE'),filetypes=[('all files','.*'),
                                                                                   ('all files','*'),
                                                                                   ('zip','*.zip'),
                                                                                   ('const','flt-const*.txt'),
                                                                                   ('nc','*.nc')])
        if(filen):
            fold=os.path.dirname(filen)
            base=os.path.basename(filen)
            index=-1
            cindex=-1
            for i in range(len(self.dataset.files)):
                if(self.dataset.files[i][0]==self.fromfile):
                    index=i
                elif(self.dataset.files[i][1]=='CONST'):
                    cindex=i
            import filecmp
            print 'FOLDER='+fold
            print '$FLTCONS='+self.fltcons
            newfile=''
            if(base.endswith('.zip')):
                newfile=filen
                newtype='ZIP'
            elif(base.endswith('.nc')):
                self.ncfile=filen
            elif(base.endswith('.txt')):
                if(filecmp.cmp(os.path.join(self.fltcons,base),filen)):
                    self.const['text']=base
                    self.constfile=filen
                    if(cindex>-1):
                        self.dataset.files[cindex]=(filen,'CONST')
                    else:
                        self.dataset.add_file(filen,'CONST')                        
            else:
                dlufolder=False
                for l in os.listdir(fold):
                    if(l.endswith('.csv')):
                        dlufolder=True
                if(dlufolder):
                    newfile=fold
                    newtype='CRIOS'
            if(newfile):
                if(index>-1):
                    self.dataset.files[index]=(newfile,newtype)
                else:
                    self.dataset.add_file(newfile,newtype)
                self.fromfile=newfile
            self.setfiles()
        

    def setconst(self):
        pass

    def getconst(self):
        return os.path.join(self.fltcons,self.const.cget('text'))
        
    def ftp_const(self):
        print "ftp "+self.const.cget('text'),
        job=ftpjob(self.getconst(),login=self.login.val,
                              passwd=self.password.val,host='eld526',
                              to='incoming/faam/campaign-raw-core',
                              printer=self.printer)
        print " job starting..."
        job.start()

    def ftp_nc(self):
        print("FTP "+self.ncfile),
        job=ftpjob(self.ncfile,login=self.login.val,
                              passwd=self.password.val,host='eld526',
                              to='incoming/faam/campaign-processed-core',
                              printer=self.printer)
        print " job starting..."
        job.start()
    

    def ftp_core(self):
        print "ftp "+self.rawcorelab.cget('text'),
        rcore=os.path.expandvars(self.rawcorelab.cget('text'))
        job=ftpjob(rcore,login=self.login.val,
                              passwd=self.password.val,host='eld526',
                              to='incoming/faam/campaign-raw-core',
                              printer=self.printer)
        print " job starting..."
        job.start()
        
    def do_const(self):
        if 'Edit' in self.constcreate.config('text'):
            const=self.getconst()
            print 'Edit '+const
            import subprocess
            subprocess.call(['gedit',const]) 
        else:
            self.constcreate.config(text='Edit')
            const=create_new_fltcons(self.enflt.val,self.endat.val)
            


    def tocore(self):
        rc=os.path.join(self.rawcore,'core_faam_%s_r%s_%s_rawdlu.zip' % (self.endat.val,self.enrev.val,self.enflt.val))
        if(self.iszip):
            print 'Copy %s to %s' % (self.fromfile,rc)
            shutil.copy(self.fromfile,rc)
        else:
            print 'Zip %s/* to %s' % (self.fromfile,rc)
            z=zipfile.ZipFile(rc,'w')
            for f in os.listdir(self.fromfile):
                z.write(os.path.join(self.fromfile,f),f)
            z.close()
        for i,f in enumerate(self.dataset.files):
            if(f[0]==self.fromfile):
                self.dataset.files[i]=(rc,'ZIP')
        print self.dataset.files
        self.iszipthere(rc)
        
    def todata(self):
        rd=os.path.join(self.rawdata,'core_faam_%s_r%s_%s_rawdlu/' % (self.endat.val,self.enrev.val,self.enflt.val))
        if(self.iszip):
            print 'Unzip %s to %s' % (self.fromfile,rd)
            try:
                os.mkdir(rd)
            except OSError as ose:
                print ose
            z=zipfile.ZipFile(self.fromfile)
            for name in z.namelist():
                try:
                    nameout=os.path.join(rd,os.path.basename(name))
                    if(nameout.find(';')>-1):
                        nameout=nameout[:nameout.find(';')]
                    fout = open(nameout, 'wb')
                    fout.write(z.read(name)) 
                    fout.close()
                    if(nameout.endswith('.csv')):
                        hascsv=True
                except IOError as ioe:
                    print ioe,
                    print '"'+name+'"'   
            z.close()
            
        else:
            print 'Copy %s/* to %s' % (self.fromfile,rd)
            try:
                os.mkdir(rd)
            except OSError:
                pass
            for f in os.listdir(self.fromfile):
                print os.path.join(self.fromfile,f),os.path.join(rd,f)
                shutil.copy(os.path.join(self.fromfile,f),os.path.join(rd,f))
        for i,f in enumerate(self.dataset.files):
            if(f[0]==self.fromfile):
                self.dataset.files[i]=(rd,'CRIOS')
        print self.dataset.files


       
        
if __name__=="__main__":
    root=Tk.Tk()
    d=decades_dataset()
    try:
        df=DecadesFile(d,sys.argv[1])
        d.add_file(*df.astuple())
    except IndexError:
        pass
    va=viewarchive(root,d)
    va.pack()
    Tk.mainloop()
