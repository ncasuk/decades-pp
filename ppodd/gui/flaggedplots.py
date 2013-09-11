import matplotlib.pyplot as plt
from matplotlib.widgets import Button
import matplotlib.image as mpimg
from PIL import Image
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2TkAgg
import Tkinter as Tk
from matplotlib.figure import Figure

class timeSeries(Tk.Toplevel):
    def __init__(self,parent,para):
        Tk.Toplevel.__init__(self,parent)
        self.parent = parent
        self.para=para
        self.gotflags=True        
        self.fig = Figure(figsize=(5,4), dpi=100)
        self.sax=self.fig.add_subplot(111)
        self.flags=['0','1','2','3']
        self.flag=Tk.StringVar()
        self.flag.set('0')
        try:
            self.doplot()
            self.canvas = FigureCanvasTkAgg(self.fig, master=self)
            self.canvas.show()
            self.canvas.get_tk_widget().pack(side=Tk.TOP, fill=Tk.BOTH, expand=1)
            toolbar = mytoolbar(self)
            toolbar.update()
            self.canvas._tkcanvas.pack(side=Tk.TOP, fill=Tk.BOTH, expand=1)
            self.title(self.para.name)
            if(self.gotflags):
                self.flag.trace("w",self.redraw)
        except Exception as e:
            self.destroy()
            raise e
            
    def getdata(self):
        p=self.para.ravel()
        if(self.gotflags):
            try:
                p=p[p.flag<=int(self.flag.get())]
            except AttributeError:
                self.gotflags=False
        return p
            
    def redoplot(self):
        p=self.getdata()
        self.plot.set_data(p.times,p)          

    def doplot(self,*args):
        p=self.getdata()
        self.sax.cla()
        self.plot,=self.sax.plot(p.times,p)
        self.sax.set_ylabel(self.para.name)
        self.sax.set_title(self.para.long_name)
        self.sax.set_xlabel('Seconds past midnight')
    
    def redraw(self,*args):
        self.redoplot()
        self.canvas.draw()
    

class mytoolbar(NavigationToolbar2TkAgg):
    """ Customized Matplotlib toolbar with extra buttons """
    def __init__(self,plotter):
        NavigationToolbar2TkAgg.__init__(self,plotter.canvas,plotter)
        #Choice of flag
        if(plotter.gotflags):
            Tk.Label(self,text='Flag').pack(side=Tk.LEFT)
            fchoice = Tk.OptionMenu(self, plotter.flag,*plotter.flags)
            fchoice.pack(side=Tk.LEFT)
        

