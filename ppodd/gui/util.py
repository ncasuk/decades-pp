import Tkinter as Tk
import time
import sys
import Queue
import os

class ToolTip( Tk.Toplevel ):
    """
    Provides a ToolTip widget for Tkinter.
    To apply a ToolTip to any Tkinter widget, simply pass the widget to the
    ToolTip constructor
    """ 
    def __init__( self, wdgt, msg=None, msgFunc=None, delay=1, follow=True ):
        """
        Initialize the ToolTip
        
        Arguments:
          wdgt: The widget this ToolTip is assigned to
          msg:  A static string message assigned to the ToolTip
          msgFunc: A function that retrieves a string to use as the ToolTip text
          delay:   The delay in seconds before the ToolTip appears(may be float)
          follow:  If True, the ToolTip follows motion, otherwise hides
        """
        self.wdgt = wdgt
        self.parent = self.wdgt.master                                          # The parent of the ToolTip is the parent of the ToolTips widget
        Tk.Toplevel.__init__( self, self.parent, bg='black', padx=1, pady=1 )      # Initalise the Toplevel
        self.withdraw()                                                         # Hide initially
        self.overrideredirect( True )                                           # The ToolTip Toplevel should have no frame or title bar
        
        self.msgVar = Tk.StringVar()                                               # The msgVar will contain the text displayed by the ToolTip        
        if msg == None:                                                         
            self.msgVar.set( 'No message provided' )
        else:
            self.msgVar.set( msg )
        self.msgFunc = msgFunc
        self.delay = delay
        self.follow = follow
        self.visible = 0
        self.lastMotion = 0
        Tk.Message( self, textvariable=self.msgVar, bg='#FFFFDD',
                 aspect=1000 ).grid()                                           # The test of the ToolTip is displayed in a Message widget
        self.wdgt.bind( '<Enter>', self.spawn, '+' )                            # Add bindings to the widget.  This will NOT override bindings that the widget already has
        self.wdgt.bind( '<Leave>', self.hide, '+' )
        self.wdgt.bind( '<Motion>', self.move, '+' )
        
    def spawn( self, event=None ):
        """
        Spawn the ToolTip.  This simply makes the ToolTip eligible for display.
        Usually this is caused by entering the widget
        
        Arguments:
          event: The event that called this funciton
        """
        self.visible = 1
        self.after( int( self.delay * 1000 ), self.show )                       # The after function takes a time argument in miliseconds
        
    def show( self ):
        """
        Displays the ToolTip if the time delay has been long enough
        """
        if self.visible == 1 and time.time() - self.lastMotion > self.delay:
            self.visible = 2
        if self.visible == 2:
            self.deiconify()
            
    def move( self, event ):
        """
        Processes motion within the widget.
        
        Arguments:
          event: The event that called this function
        """
        self.lastMotion = time.time()
        if self.follow == False:                                                # If the follow flag is not set, motion within the widget will make the ToolTip dissapear
            self.withdraw()
            self.visible = 1
        self.geometry( '+%i+%i' % ( event.x_root+10, event.y_root+10 ) )        # Offset the ToolTip 10x10 pixes southwest of the pointer
        try:
            self.msgVar.set( self.msgFunc() )                                   # Try to call the message function.  Will not change the message if the message function is None or the message function fails
        except:
            pass
        self.after( int( self.delay * 1000 ), self.show )
            
    def hide( self, event=None ):
        """
        Hides the ToolTip.  Usually this is caused by leaving the widget
        
        Arguments:
          event: The event that called this function
        """
        self.visible = 0
        self.withdraw()




class ScrollFrame(Tk.Frame,object):
    """ A scrollable Tk Frame which can then be filled with other widgets """
    def __init__(self,parent,**kwargs):
        self.outer=Tk.Frame(parent,bd=2, relief=Tk.SUNKEN,**kwargs)
        yscrollbar = Tk.Scrollbar(self.outer,**kwargs)
        yscrollbar.pack(fill=Tk.Y, side=Tk.RIGHT, expand=Tk.FALSE)
        canvas=Tk.Canvas(self.outer, bd=0, yscrollcommand=yscrollbar.set,**kwargs)
        canvas.pack(side=Tk.LEFT, fill=Tk.BOTH, expand=Tk.TRUE)
        self.outer.rowconfigure(0,weight=1)
        self.outer.columnconfigure(0,weight=1)
        yscrollbar.config(command=canvas.yview)
        canvas.yview_moveto(0)
        Tk.Frame.__init__(self,canvas,**kwargs)
        self_id = canvas.create_window(0, 0, window=self,
                                           anchor=Tk.NW)

       
        def _configure(event):
            # update the scrollbars to match the size of the inner frame
            size = (self.winfo_reqwidth(), self.winfo_reqheight())
            canvas.config(scrollregion="0 0 %s %s" % size)
            if self.winfo_reqwidth() != canvas.winfo_width():
                # update the canvas's width to fit the inner frame
                canvas.config(width=self.winfo_reqwidth())
        self.bind('<Configure>', _configure)

        
        def _configure_canvas(event):
            if self.winfo_reqwidth() != canvas.winfo_width():
                # update the inner frame's width to fill the canvas
                canvas.itemconfigure(self_id, width=canvas.winfo_width())
        canvas.bind('<Configure>', _configure_canvas)

        #self.outer.pack(fill=Tk.BOTH, expand=Tk.TRUE)
    
    def forget(self):
        self.outer.pack_forget()

    def pack(self):
        self.outer.pack(fill=Tk.BOTH, expand=Tk.TRUE)

                
class ScrollText(Tk.Text):
    """ A scrollable text widget """
    def  __init__(self,parent,*args,**kwargs):
        self.scrollbar = Tk.Scrollbar(parent,**kwargs)
        self.scrollbar.pack(side=Tk.RIGHT, fill=Tk.Y)
        Tk.Text.__init__(self,parent,*args, yscrollcommand=self.scrollbar.set, **kwargs)
        self.scrollbar.config(command=self.yview)
    def write(self,string):
        """ 
        Then tries to write to the widget"""
        self.insert(Tk.END,string)
        try:
            self.yview_moveto(1.0) # Try to move the view to the last thing written
        except:
            pass
        self.update_idletasks()

class ScrollMessage(ScrollFrame):
    """ A scrollable message widget """
    def __init__(self,parent,text,**kwargs):
        ScrollFrame.__init__(self,parent,**kwargs)
        mess=Tk.Message(self,text=text,**kwargs)
        mess.pack(fill=Tk.BOTH, expand=Tk.TRUE)

class PrintLog(Queue.Queue,ScrollText):
    """ A scrollable text widget which writes out redirected error statements
        To make it cope  with Threads in Tk it will put text in a Queue if it failes to write to the widget
        and periodically write queued text to the widget.
    """
    def __init__(self,*args,**kwargs):
        Queue.Queue.__init__(self)
        ScrollText.__init__(self,*args,**kwargs)
        self.stderr=sys.stderr
        sys.stderr=self
        self.after(2000, self.check_queue)
            
    def write(self,string):
        """ Writes first to the logfile,
        Then tries to write to the widget, if that fails puts the text in its Queue
        if that fails writes to the saved stdout"""
        try:
            ScrollText.write(self,string)
        except:
            try:
                self.put(string)
            except:
                print(string)     

    def check_queue(self):
        """ Checks its own Queue every 200ms and writes any text to the text widget """
        changed=0
        try:
            while(changed<10):
                string=self.get(False)
                self.insert(Tk.END,string)
                changed+=1
        except Queue.Empty:
            pass
        if(changed):
            try:
                self.yview_moveto(1.0) # Try to move the view to the last thing written
            except:
                pass
        self.update_idletasks()
        self.after(200, self.check_queue)

    def revert(self):
        """ Sets the outputs back how they were before it was called"""
        sys.stderr=self.stderr




class ValidEntry(Tk.Entry,object):
    """ An Entry widget with built in validation """
    def __init__(self,parent,*args,**kwargs):
        self.data=Tk.StringVar()
        self.lastgood=''
        self.command=None
        if('val' in kwargs):
            self.lastgood=kwargs.pop('val')
            self.data.set(self.lastgood) 
        if('command' in kwargs):
            self.command=kwargs.pop('command')  
        valcmd=(parent.register(self.validate),'%V','%P')
        Tk.Entry.__init__(self,parent,*args,textvariable=self.data,validate="all",validatecommand=valcmd,**kwargs)
        
    def validate(self,V,P):
        """ Validate enty if not forced"""
        val=True
        if(V!='forced'):
            if(V=='key'):
                val=self.check_key(P)
            if(self.check_bad(P) and V=='focusout'):
                val=False
                self.configure(validate='none')
                self.data.set(self.lastgood)
                self.configure(validate='all')        
        return val

    def check_key(self,P):
        """ Validation - should be overidden """
        return True
        
    def check_good(self,P):
        """ Validation - should be overidden """
        return True
                
    def check_bad(self,P):
        if(self.check_good(P)):
            self.lastgood=P
            if(self.command):
                self.command()
            return False
        else:
            return True
        
    def setdata(self,s):
        self.lastgood=s
        self.data.set(s)
        self.configure(validate='all')
        
    def getdata(self):
        return self.lastgood

    val=property(getdata,setdata)
        


