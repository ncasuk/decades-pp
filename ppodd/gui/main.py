#!/usr/bin/env python
import Tkinter as Tk
from util import *
from ppodd.core import decades_dataset
from views import viewparas,viewmodules
from archive import viewarchive
from files import viewfiles


class main(Tk.Tk):
    
    def __init__(self,data=None,bg=None):
        Tk.Tk.__init__(self)
        self.config(bg=bg)
        self.viewstate='none'
        if(data==None):
            self.data=decades_dataset()
        else:
            self.data=data
        filemenu=Tk.Menu(self,tearoff=0,bg=self.cget('bg'))
        filemenu.add_command(label="Archive",command=self.archive)
        filemenu.add_command(label="Files",command=self.files)
        filemenu.add_command(label="Write",command=self.write)
        filemenu.add_separator()
        filemenu.add_command(label="Exit",command=self.quit)
        menubar=Tk.Menu(self,bg=self.cget('bg'))
        menubar.add_cascade(label="File",menu=filemenu)
        menubar.add_command(label="Modules",command=self.modules)
        menubar.add_command(label="Parameters",command=self.paras)
        menubar.add_command(label="Process",command=self.process)
        menubar.add_command(label="Quality Check",command=self.quality)
        menubar.add_command(label="Help",command=self.help)
        self.config(menu=menubar)
        self.title('PPODD')
        self.viewpane=Tk.Frame(self,bg=self.cget('bg'))
        self.viewpane.settitle=self.setviewtitle
        self.viewtitle=Tk.Label(self,text='',bg=self.cget('bg'))
        self.logpane=Tk.Frame(self,bg=self.cget('bg'))
        self.log=PrintLog(self.logpane,bg=self.cget('bg'))
        self.log.pack()
        self.viewtitle.pack()
        self.viewpane.pack()
        self.logpane.pack()
        self.mods=None
        self.paras=self.data.write_nc.input_names
        self.fileview=viewfiles(self.viewpane,self.data,bg=self.cget('bg'))
        self.setview('files')

        

    def setviewtitle(self,title):
        self.viewtitle['text']=title

    def quit(self):
        print "QUIT"
        Tk.Tk.quit(self)
        self.destroy()

    def help(self):
        helpwdgt=Tk.Toplevel( self, bg='black', padx=1, pady=1 )   
        helpwdgt.title("PPODD Help")  
        #helpwdgt.withdraw()                                                        
        #helpwdgt.overrideredirect( True )   
        helptext="""
Processing Decades Data
=======================

Screen layout:

----------------------------------------
(Menus) File: Modules: Parameters: Process: Help
----------------------------------------

   (View of Files/Modules/Parameters)
  

----------------------------------------


               ( Log )


----------------------------------------


Menus:-



File:

     Archive:
             Options for backing up raw data from aircraft, and archiving at BADC
     Files:
             The input files the dataset will use
     Write:
             Write data out as NetCDF
    
Modules:
     
     View available processing modules
     
Parameters:

     View parameters read in or created by processing
     
Process:

     Process data 


Help:

     Display this help text






"""                                             
        ScrollMessage( helpwdgt, text=helptext, bg='#FFFFDD').pack()
                                                         
    def setview(self,viewstate='none',paras=None):
        if(viewstate==self.viewstate):
            return
        if(self.viewstate.find('paras') > -1):
            if(self.paraview):
                self.paraview.forget()
                self.paras=self.paraview.getselected()
                del self.paraview
        elif(self.viewstate=='files'):
            self.fileview.forget()            
            self.data.files=self.fileview.files  
        elif(self.viewstate=='modules'):
            self.modview.forget()
            self.mods=self.modview.getselected()
            del self.modview            
        elif(self.viewstate=='archive'):
            self.archiveview.forget()  
        if(viewstate=='paras'):
            selectable=False
            if(not paras):
                paras=[self.data[p] for p in self.data]
                selectable=True
            else:
                viewstate='specialparas'
            self.paraview=viewparas(self.viewpane,paras,selectable=selectable,bg=self.cget('bg'))
            if(self.paras!=None):
                self.paraview.setselected(self.paras)
            self.paraview.pack()
        elif(viewstate=='files'):
            self.fileview.pack()
        elif(viewstate=='modules'):
            mods=self.data.modules
            self.modview=viewmodules(self.viewpane,mods.values(),selectable=True,paraview=self.setview,bg=self.cget('bg'))
            if(self.mods!=None):
                self.modview.setselected(self.mods)
            else:
                self.modview.selectall()
            self.modview.pack()
        elif(viewstate=='archive'):
            try:
                self.archiveview.pack()
            except AttributeError:
                self.archiveview=viewarchive(self.viewpane,self.data,bg=self.cget('bg'))
                self.archiveview.pack()
        self.viewstate=viewstate
    

    def archive(self):
        self.setviewtitle('Archive')
        self.setview('archive')
    def files(self):
        self.setviewtitle('Files')
        self.setview('files')
    def write(self):
        print "Write\n"
        self.data.write_nc.process(paras=self.paras)
    def writeonehz(self):
        print "Write\n"
        self.data.write_nc.process(onehz=True,paras=self.paras)
    def modules(self):
        self.setviewtitle('Modules')
        self.setview('modules')
    def paras(self):
        self.setviewtitle('All Parameters')
        self.setview('paras')
    def process(self):
        print "Process\n"
        try:
            self.mods=self.modview.getselected()
        except AttributeError:
            pass
        try:
            self.paras=self.paraview.getselected()
        except AttributeError:
            pass
        print 'Paras=',self.paras
        print 'Modules=',self.mods
        self.data.mods=self.mods
        if(self.viewstate=='files'):
            self.data.files=self.fileview.files
        self.data.process()
        if(self.viewstate=='paras'):
           self.viewstate='oldparas'
           self.setview('paras')
    def quality(self):
        print "Quality\nNot implemented fully\nIDL version\n"
        try:
            idlcomm="idl -quiet -e \"!path=!path+':$MRF_IDL' & checkf,'"+self.data.write_nc.filename+"'\""
            import subprocess
            subp = subprocess.Popen(idlcomm, shell = True)
        except AttributeError:
            print "Data hasn't been written can't run IDL quality checking"


           

if __name__=="__main__":
    colour='honeydew'
    m=main(bg=colour)
    Tk.mainloop()
