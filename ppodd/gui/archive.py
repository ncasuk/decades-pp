#!/usr/bin/env python
"""Classes for handling the archiving of core data.

@author Dave Tiddeman
""" 
import Tkinter as Tk
import sys
#from views import viewarchive
from ppodd.core import decades_dataset,file_parameter
from ppodd.util import fltno_date,find_file,ftpjob
import ppodd
from util import *
import os
import zipfile
import shutil
from tkFileDialog import askopenfilename
import re
from files import onefile


class archfile(Tk.Frame,object):
    """GUI for single archivable file.

@author Dave Tiddeman
""" 

    def __init__(self,parent,**kwargs):
        self.parent=parent
        self.var=Tk.StringVar()
        filename=''
        filetype='CONST'
        if('filename' in kwargs):
            filename=kwargs.pop('filename')
        if('filetype' in kwargs):
            filetype=kwargs.pop('filetype')
        Tk.Frame.__init__(self,parent,**kwargs)
        self.ftp=Tk.Button(self,text='FTP',command=self.doftp,**kwargs)
        Tk.Label(self,textvariable=self.var,**kwargs).grid(column=0,row=0)
        self.ftp.grid(column=1,row=0)
        self.filename=filename
        self.ftploc={'CONST':'incoming/faam/campaign-raw-core',
                     'ZIP':'incoming/faam/campaign-raw-core',
                     'NC':'incoming/faam/campaign-processed-core'}[filetype]
        
    def there(self):
        return self.ftp['state']==Tk.NORMAL

    def __setfilename__(self,name):
        self.var.set(name)
        if(os.path.isfile(name)):
            self.ftp['state']=Tk.NORMAL
        else:
            self.ftp['state']=Tk.DISABLED
        
    def __getfilename__(self):
        return self.var.get()
        
    filename=property(__getfilename__,__setfilename__)
    
    def doftp(self):
        self.parent.doftp(self.filename,self.ftploc)


class viewarchive(ScrollFrame):
    """GUI for group of archivable files.

@author Dave Tiddeman
""" 
    def __init__(self,parent,dataset,**kwargs):
        ScrollFrame.__init__(self,parent,**kwargs)
        self.rawcore=os.path.expandvars("$RAWCORE")
        self.parent=parent
        self.dataset=dataset
        self.browse=Tk.Button(self,text='Browse',command=self.browse,**kwargs)
        self.browse.grid(row=0,column=0,columnspan=4)
        self.orig=Tk.StringVar()
        self.original=Tk.Label(self,textvariable=self.orig,**kwargs)
        self.original.grid(row=1,column=0,columnspan=4)
        self.copy=Tk.Button(self,text='Copy to',command=self.copy,**kwargs)
        self.copy.grid(column=0,row=2,columnspan=4)
        self.core=archfile(self,filetype='ZIP',**kwargs)
        self.core.grid(column=0,row=3,sticky=Tk.E,columnspan=4)
        self.const=archfile(self,filetype='CONST',**kwargs)
        self.const.grid(column=0,row=4,sticky=Tk.E,columnspan=4)
        self.nc=archfile(self,filetype='NC',**kwargs)
        self.nc.grid(column=0,row=5,sticky=Tk.E,columnspan=4)
        self.nc1hz=archfile(self,filetype='NC',**kwargs)
        self.nc1hz.grid(column=0,row=6,sticky=Tk.E,columnspan=4)
        self.files=[self.nc1hz,self.nc,self.const,self.core,self]
        Tk.Label(self,text='FTP Address:',**kwargs).grid(column=0,row=7,sticky=Tk.W)
        self.host=ValidEntry(self,val='ftp.badc.rl.ac.uk',**kwargs)
        self.host.grid(column=1,row=7)
        Tk.Label(self,text='FTP Login:',**kwargs).grid(column=0,row=8,sticky=Tk.W)
        self.login=ValidEntry(self,**kwargs)
        self.login.grid(column=1,row=8)
        Tk.Label(self,text='Password:',**kwargs).grid(column=2,row=8)
        self.password=ValidEntry(self,show="*",**kwargs)
        self.password.grid(column=3,row=8)
    

    def pack(self,**kwargs):
        self.reset_files()
        ScrollFrame.pack(self,**kwargs)
        

    def reset_files(self):
        """Set files back to the datasets values"""
        self.fromfile=('','')
        for f in self.dataset.getfiles():
            self.check_file(*f)
        try:
            self.set_nc(self.dataset.write_nc.filename)
        except AttributeError:
            pass
        fltno,date=self.fltno_date()
        self.filenames(fltno,date)
        
    
    def copy(self):
        """Copy or zip raw data to zipped file in $RAWCORE"""
        f,t=self.fromfile
        if f:
            if(t=='ZIP'):
                print 'Copy %s to %s' % (f,self.core.filename)
                shutil.copy(f,self.core.filename)
            elif(t=='FOLDER'):
                print 'ZIP %s to %s' % (f,self.core.filename)
                z=zipfile.ZipFile(self.core.filename,'w')
                for fx in os.listdir(f):
                    z.write(os.path.join(f,fx),fx)
                z.close()
        self.core.filename=self.core.filename
        self.dataset['ZIP']=file_parameter('ZIP',self.core.filename)
                       

    def check_file(self,filename,filetype):
        """Check which file type and set appropriate attribute"""
        if(filetype=='FOLDER'):
            self.fromfile=(filename,filetype)
        elif(filetype=='ZIP'):
            if(self.rawcore not in filename):
                self.fromfile=(filename,filetype)
            else:
                self.core.filename=filename
        elif(filetype=='CONST'):
            self.const.filename=filename
        elif(filetype=='NC'):
            self.set_nc(filename)

    def set_nc(self,filename):
        """Set netcdf filename"""
        if('1hz' in filename.lower()):
            self.nc1hz.filename=filename
        else:
            self.nc.filename=filename

    
    def fltno_date(self):
        """Parse file names for flight number and date"""
        files=[(self.nc1hz.filename,'NETCDF'),
              (self.nc.filename,'NETCDF'),
              (self.const.filename,'CONST'),
              (self.core.filename,'ZIP'),
              self.fromfile]
        files=[item for item in files if item[0]]
        return self.dataset.parse_filenames(files=files)
        

    def filenames(self,fltno,date):
        """Set file names to a particular flight number and date"""       
        self.core.filename=find_file(fltno,date,ftype='ZIP')
        self.const.filename=find_file(fltno,date,ftype='CONST')
        self.nc.filename=find_file(fltno,date,ftype='NETCDF')
        self.nc1hz.filename=find_file(fltno,date,ftype='NC1HZ')
        self.dataset.clearfiles()
        if(self.core.there()):
            self.dataset.add_file(self.core.filename,'ZIP')
        elif(self.fromfile[0]):
            self.dataset.add_file(*self.fromfile)
        if(self.const.there()):
            self.dataset.add_file(self.const.filename,'CONST')


    def doftp(self,filename,loc):
        """Start an FTP job"""
        job=ftpjob(filename,host=self.host.val,login=self.login.val,
                              passwd=self.password.val,
                              to=loc)
        job.start()
        

    def __setfromfile__(self,n):
        name=n[0]
        t=n[1]
        self.orig.set(name)
        if(t=='ZIP' and name):
            self.copy['text']='Copy to'
            self.copy['state']=Tk.NORMAL
        elif(t=='FOLDER' and name):
            self.copy['text']='Zip to'
            self.copy['state']=Tk.NORMAL
        else:
            self.copy['text']=''
            self.copy['state']=Tk.DISABLED
    
    def __getfromfile__(self):
        f=self.orig.get()
        if 'Zip' in self.copy['text']:
            return (f,'FOLDER')
        elif 'Copy' in self.copy['text']:
            return (f,'ZIP')
        else:
             return ('','')
    
    fromfile=property(__getfromfile__,__setfromfile__)
    
    def browse(self):
        """Choose a file"""
        filen=askopenfilename(initialdir=os.path.expandvars('$RAWCORE'),filetypes=[('all files','.*'),
                                                                                   ('all files','*'),
                                                                                   ('zip','*.zip'),
                                                                                   ('const','flt-const*.txt'),
                                                                                   ('nc','*.nc')])
        if(filen):
            df=self.dataset.DecadesFile(filen)
            self.fromfile=('','')
            self.filenames(*self.dataset.parse_filenames(files=[df.astuple()]))
            self.check_file(*df.astuple())
        
     
        
if __name__=="__main__":
    root=Tk.Tk()
    d=decades_dataset()
    try:
        df=d.DecadesFile(sys.argv[1])
        d.add_file(*df.astuple())
    except IndexError:
        pass
    va=viewarchive(root,d)
    va.pack()
    Tk.mainloop()
