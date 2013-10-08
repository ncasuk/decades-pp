#!/usr/bin/env python
"""Classes for handling the core data file GUI.

@author Dave Tiddeman
""" 
import Tkinter as Tk
from tkFileDialog import askopenfilename
import os
import fnmatch
from util import *
from ppodd.util import *
from optparse import OptionParser
import re
import time
from ppodd.core import decades_dataset
import tkMessageBox
import zipfile
import shutil



        
class onefile(Tk.Frame,decades_dataset.decades_file):
    """GUI for single file.

@author Dave Tiddeman
""" 
    def __init__(self, parent, filename,filetype=None,bg=None,removable=True):
        Tk.Frame.__init__(self,parent,bg=bg)
        self.typ=Tk.StringVar()
        self.label=Tk.StringVar()
        decades_dataset.decades_file.__init__(self,parent.dataset,filename,filetype)
        self.parent=parent
        if(self.filetype=='CONST'):
            self.constedit=Tk.Button(self,text='Edit',command=self.edit_const,bg=bg)
            self.constnew=Tk.Button(self,text='Revise',command=self.new_const,bg=bg)
            if os.path.isfile(self.filename):
                self.constedit.grid(column=2,row=0)
            else:
                self.constnew=Tk.Button(self,text='Create',command=self.new_const,bg=bg)
            self.constnew.grid(column=1,row=0)
        else:
            self.typ.trace("w", self.changed)
            self.opt = Tk.OptionMenu(self,self.typ,*self.filetypes.keys())
            self.opt.config(bg=bg)
            self.opt["menu"].config(bg=bg)
            self.opt.grid(row=0,column=1,columnspan=2)
        Tk.Label(self, textvariable=self.label,bg=bg).grid(row=0,column=0)
        if(removable):
            self.rem = Tk.Button(self,text='Remove',command=self.remove,bg=bg)
            self.rem.grid(row=0,column=3)
 

    def __setfilename__(self,val):
        decades_dataset.decades_file.__setfilename__(self,val)
        self.label.set(self.filename)
        try:
            if os.path.isfile(self.filename):
                self.constnew['text']='Revise'
                self.constedit.grid(column=2,row=0)
            else:
                self.constnew['text']='Create'
                self.constedit.grid_remove()
        except AttributeError:
            pass
            

    filename=property(decades_dataset.decades_file.__getfilename__,__setfilename__)

    def remove(self):
        self.parent.remove(self)
    
    def changed(self,*args):
        self.label.set(self.filename)

    def __settype__(self,val):
        self.typ.set(val)
    
    def __gettype__(self):
        return self.typ.get()

    def new_const(self):
        if(self.constnew['text']=='Create'):
            print 'new const'
            if(create_new_fltcons(self.filename)):
                self.constnew['text']='Revise'
                self.constedit.grid(column=2,row=0)
                self.edit_const()
        else:
            print 'revise const'
            f=revise_fltcons(self.filename)
            if(f):
                self.filename=f
                self.label.set(self.filename)
                self.edit_const()
    def edit_const(self):
        print 'edit const'
        import subprocess
        subprocess.call(['gedit',self.filename]) 
        
    filetype = property(__gettype__,__settype__)



class viewfiles(ScrollFrame):
    def __init__(self, parent, dataset, **kwargs):
        ScrollFrame.__init__(self,parent,**kwargs)
        self.filex=[]
        self.dataset=dataset
        self.extra=Tk.Frame(self,**kwargs)
        addButton=Tk.Button(self.extra,text='Add File',command=self.add,**kwargs)
        addButton.grid(column=0,row=0)
        self.findButton=Tk.Button(self.extra,text='Find Constants',command=self.find,**kwargs)
        self.findButton.grid(column=1,row=0)
        self.initialdir=None
        self.filetypes=[('all files','.*'),('all files','*')]
        for f in self.dataset.filetypes:
            for p in self.dataset.filetypes[f].patterns:
                self.filetypes.append((f,p))
            if(f in self.dataset):
                self.initialdir=os.path.dirname(list(self.dataset[f].data)[0])


    def pack(self,**kwargs):
        self.reset_files()
        ScrollFrame.pack(self,**kwargs)


    def add(self,filen=None):
        if not filen:
            filen=askopenfilename(initialdir=self.initialdir,filetypes=self.filetypes)
        if(filen):
            self.filex.append(onefile(self,filen,bg=self.cget('bg')))
            self.filex[-1].grid(column=0,row=len(self.filex)-1,sticky=Tk.E)  
            self.move_extra()
        self.sync_files()

        
    def remove(self,line):
        line.grid_forget()
        self.filex.remove(line)
        for i,f in enumerate(self.filex):
            f.grid_configure(column=0,row=i)
        self.sync_files()
        self.move_extra()
            
    def sync_files(self):
        self.dataset.clearfiles()
        for name,ftype in self.files:
            self.dataset.add_file(name,ftype)

    def find(self):
        fltno,date=self.dataset.parse_filenames()
        const=find_file(fltno,date)
        if(const):
            self.add(const)
    
            
    def reset_files(self):
        while self.filex:
            f=self.filex.pop()
            f.grid_forget()
        self.files=self.dataset.getfiles()

    def move_extra(self):
        self.extra.grid_configure(column=0,row=len(self.filex))
        hasconst=False  
        for f in self.filex:  
            if(f.filetype=='CONST'):
                hasconst=True
        if(hasconst):
            self.findButton.grid_remove()
        else:
            self.findButton.grid()
        
    def __setfiles__(self,files): 
        for i,f in enumerate(files):
            self.filex.append(onefile(self,*f,bg=self.cget('bg')))
            self.filex[i].grid(column=0,row=i,sticky=Tk.E)
        self.move_extra()
            
    def __getfiles__(self):
        ans=[]
        for f in self.filex:
            ans.append((f.filename,f.filetype))
        return ans    
    files = property(__getfiles__,__setfiles__)


        
if __name__=="__main__":
    root=Tk.Tk()
    d=decades_dataset()
    try:
        df=d.DecadesFile(sys.argv[1])
        d.add_file(*df.astuple())
    except IndexError:
        pass
    va=viewfiles(root,d)
    va.pack()
    Tk.mainloop()

