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
    def __init__(self,parent,para,selectable=False,bg=None):
        Tk.Frame.__init__(self,parent,bg=bg)
        self.checked=Tk.BooleanVar()
        self.para=para
        try:
            name=para.name
        except AttributeError:
            name=str(para)
        if(selectable):
            self.label=Tk.Checkbutton(self,text=name,var=self.checked,bg=bg)
        else:
            self.label=Tk.Label(self,text=name,bg=bg)
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
                tkMessageBox.showinfo(repr(self.para),str(self.para)+'\n\n'+str(self.para.data),master=self)
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
                if repr(p.para).strip("'")==repr(px).strip("'"): # To catch cases when 
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
        self.outtooltip=ToolTip(self.outputs,msg=str(module.getoutputnames()))
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
                ans.append(repr(m.module))
        return ans             
    def setselected(self,mods):
        for m in self.mlist:
            for mx in mods:
                if repr(m.module).strip("'")==repr(mx).strip("'"): # To catch cases  
                                                    # of 2 modules, 2 strings or a mixture
                    m.checked.set(True)
    def selectall(self):
        for m in self.mlist:
            m.checked.set(True)
    def deselectall(self):
        for m in self.mlist:
            m.checked.set(False)

        


