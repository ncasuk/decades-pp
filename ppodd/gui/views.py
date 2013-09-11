#!/usr/bin/env python
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
import matplotlib.pyplot as plt
import tkMessageBox
import zipfile
import shutil


class onepara(Tk.Frame):
    def __init__(self,parent,para,selectable=True,bg=None):
        Tk.Frame.__init__(self,parent,bg=bg)
        self.checked=Tk.BooleanVar()
        self.para=para
        if(selectable):
            self.label=Tk.Checkbutton(self,text=repr(para),var=self.checked,bg=bg)
        else:
            self.label=Tk.Label(self,text=repr(para),bg=bg)
        self.label.pack(side=Tk.LEFT,expand=1)
        self.showbutton=Tk.Button(self,text='Show',command=self.show,bg=bg)
        self.showbutton.pack(side=Tk.LEFT)
        self.tooltip=ToolTip(self.label,msg=str(para))
        self.stooltip=ToolTip(self.showbutton,msg=str(para))
        self.pack(fill=Tk.BOTH)

    def show(self):
        try:
            from flaggedplots import timeSeries
            ts=timeSeries(self,self.para)
        except (AttributeError,ValueError):
            try:
                tkMessageBox.showinfo(repr(self.para),str(self.para)+'\n\nData: '+str(self.para.data),master=self)
            except AttributeError:
                tkMessageBox.showinfo(repr(self.para),str(self.para)+'\n\nNo information',master=self)
   
class viewparas(ScrollFrame):
    def __init__(self,parent,paralist,**kwargs):
        self.selectable=False
        if('selectable' in kwargs):
            self.selectable=kwargs.pop('selectable')
        ScrollFrame.__init__(self,parent,**kwargs)
        self.paras=[]
        for p in paralist:
            self.paras.append(onepara(self,p,selectable=self.selectable,**kwargs))
    def getselected(self):
        ans=[]
        for p in self.paras:
            if(p.checked.get()):
                ans.append(p.para)
        return ans             
    def setselected(self,paras):
        for p in self.paras:
            for px in paras:
                if(repr(p.para)==repr(px) or p.para==px or repr(p.para)==px or p.para==repr(px)): # To catch cases when 
                                                    # of 2 parameters, 2 strings or a mixture
                    p.checked.set(True)
    def selectall(self):
        for p in self.paras:
            p.checked.set(True)
    def deselectall(self):
        for p in self.paras:
            p.checked.set(False)


class onemodule(Tk.Frame):
    def __init__(self,parent,module,selectable=True,bg=None):
        Tk.Frame.__init__(self,parent,bg=bg)
        self.parent=parent
        self.checked=Tk.BooleanVar()
        self.module=module
        if(selectable):
            self.label=Tk.Checkbutton(self,text=repr(module),var=self.checked,justify=Tk.LEFT,bg=bg)
        else:
            self.label=Tk.Label(self,text=repr(module),justify=Tk.LEFT,bg=bg)
        self.inputs=Tk.Button(self,text='In',command=self.inview,bg=bg)
        self.intooltip=ToolTip(self.inputs,msg=str(module.input_names))
        self.outputs=Tk.Button(self,text='Out',command=self.outview,bg=bg)
        self.inputs.pack(side=Tk.LEFT)
        self.label.pack(side=Tk.LEFT,expand=1)
        self.outputs.pack(side=Tk.LEFT)
        self.outtooltip=ToolTip(self.outputs,msg=str(module.outputs))
        self.tooltip=ToolTip(self.label,msg=module.__doc__)
        self.pack(fill=Tk.X,expand=1)

    def inview(self):
        paras=[]
        for i in self.module.input_names:
            try:
                paras.append(self.module.dataset[i])
            except KeyError:
                paras.append(i)
        self.parent.settitle(repr(self.module)+' Inputs')
        self.parent.paraview('paras',paras=paras)
        
    def outview(self):
        self.parent.settitle(repr(self.module)+' Outputs')
        self.parent.paraview('paras',paras=self.module.outputs)

class viewmodules(ScrollFrame):
    def __init__(self,parent,modules,**kwargs):
        self.paraview=None
        self.selectable=False
        self.parent=parent
        if('paraview' in kwargs):
           self.paraview=kwargs.pop('paraview')
        if('selectable' in kwargs):
           self.selectable=kwargs.pop('selectable')
        ScrollFrame.__init__(self,parent,**kwargs)
        self.mlist=[]
        for m in modules:
            self.mlist.append(onemodule(self,m,self.selectable,**kwargs))
    def settitle(self,title):
        self.parent.settitle(title)
    def getselected(self):
        ans=[]
        for m in self.mlist:
            if(m.checked.get()):
                ans.append(m.module)
        return ans             
    def setselected(self,mods):
        for m in self.mlist:
            for mx in mods:
                if(repr(m.mod)==repr(mx) or m.module==mx or repr(m.module)==mx or m.module==repr(mx)): # To catch cases  
                                                    # of 2 modules, 2 strings or a mixture
                    m.checked.set(True)
    def selectall(self):
        for m in self.mlist:
            m.checked.set(True)
    def deselectall(self):
        for m in self.mlist:
            m.checked.set(False)



class DFile(DecadesFile):
    def __settype__(self,val):
        try:
            self.typ.set(val)
        except AttributeError:
            self.typ=Tk.StringVar()
            self.typ.set(val)
    
    def __gettype__(self):
        return self.typ.get()
        
    filetype = property(__gettype__,__settype__)


class onefile(Tk.Frame,DFile):

    def __init__(self, parent, filename,filetype=None,bg=None):
        Tk.Frame.__init__(self,parent,bg=bg)
        DFile.__init__(self,parent.dataset,filename,filetype)
        #self.file=DFile(filename,filetype)
        self.parent=parent
        self.typ.trace("w", self.changed)
        self.label=Tk.StringVar()
        self.label.set(self.filename)
        Tk.Label(self, textvariable=self.label,bg=bg).grid(row=0,column=0)
        self.opt = Tk.OptionMenu(self,self.typ,*self.filetypes.keys())
        self.opt.config(bg=bg)
        self.opt["menu"].config(bg=bg)
        self.opt.grid(row=0,column=1)
        self.rem = Tk.Button(self,text='Remove',command=self.remove,bg=bg)
        self.rem.grid(row=0,column=2)
 
    def remove(self):
        self.parent.remove(self)
    
    def changed(self,*args):
        self.label.set(self.filename)

        
        

class viewfiles(ScrollFrame):
    def __init__(self, parent, dataset, **kwargs):
        ScrollFrame.__init__(self,parent,**kwargs)
        self.filex=[]
        self.dataset=dataset
        self.extra=Tk.Frame(self,**kwargs)
        addButton=Tk.Button(self.extra,text='Add File',command=self.add,**kwargs)
        addButton.grid(column=0,row=0)
        editButton=Tk.Button(self.extra,text='Edit Constants',command=self.edit,**kwargs)
        editButton.grid(column=1,row=0)
        self.files=self.dataset.files
        self.initialdir=None
        if(self.dataset.files):
            self.initialdir=os.path.dirname(self.dataset.files[0][0])
        self.filetypes=[('all files','.*'),('all files','*')]
        x=DecadesFile(self.dataset,'')
        for f in x.filetypes:
            for p in x.filetypes[f].patterns:
                self.filetypes.append((f,p))

    def add(self,filen=None):
        if not filen:
            filen=askopenfilename(initialdir=self.initialdir,filetypes=self.filetypes)
        if(filen):
            self.filex.append(onefile(self,filen,bg=self.cget('bg')))
            self.filex[-1].grid(column=0,row=len(self.filex)-1,sticky=Tk.E)  
            self.move_extra()
    def edit(self):
        const=''
        files=[]
        for f in self.filex:
            files.append(os.path.basename(f.__file__))
            if(f.filetype=='CONST'):
                const=f.filename
        fltno,date,rev=fltno_date(files)
        if(const==''):
            const=find_file(fltno,date)
            if(const):
                const=os.path.join(os.path.expandvars('$FLTCONS'),const)
                self.add(filen=const)
            else:
                import tkMessageBox
                if tkMessageBox.askyesno("New constants", "Create new constants flight-cst_faam_%s_r0_%s.txt ?" % (date,fltno)):
                    const=create_new_fltcons(fltno,date)
                    self.add(filen=const)
        if(const):
            import subprocess
            subprocess.call(['gedit',const]) 

        
    def remove(self,line):
        line.grid_forget()
        self.filex.remove(line)
        for i,f in enumerate(self.filex):
            f.grid_configure(column=0,row=i)
            
    def reset_files(self):
        while self.filex:
            f=self.filex.pop()
            f.grid_forget()
        self.files=self.dataset.files

    def move_extra(self):
        self.extra.grid_configure(column=0,row=len(self.filex))
        
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



