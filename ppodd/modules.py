class cal_base(object):
    """ Base for all calibration modules """
    def __init__(self,dataset):
        """ Sub class should initialise the version inputs outputs and name as a minimum """
        self.dataset=dataset
        self.version=1.0
        self.name=self.__class__.__name__.upper()[2:]
        self.history=''
        

    def get_inputs(self):
        return [i.get_para() for i in self.input_names]
        
    def run(self):
        self.process()
        for o in self.outputs:
            self.dataset.update({o.name:o})
        self.addhistory()

    def process(self):
        pass
    
    def addhistory(self):
        if(len(self.outputs)>0):
            self.dataset.history+='\n%s\n  Inputs=%s ,\n  Outputs=%s \n\n' % (self.name,str(self.input_names),str(self.outputs))
            self.history+='INPUTS\n'
            for i in self.input_names:
                try:
                    f=self.dataset[i].frequency
                    self.history+='  Parameter %s\n' % i
                except:
                    self.history+='  Constants %s=' % i
                    for c in self.dataset[i][:]:
                        try:
                            self.history+='%e,' % c
                        except TypeError:
                            self.history+='%s,' % str(c)
                    self.history+='\n'
            self.history+='\n\nOUTPUTS\n'
            for o in self.outputs:
                print o
                self.history+=repr(o)+','+str(o)+'\n'
                
                
    def __repr__(self):
        return self.name

class file_reader(cal_base):
    """ Base class for file reading modules """
    def __init__(self,dataset):
        cal_base.__init__(self,dataset)
        self.patterns=('.*','*')
        if(not(self.__doc__)):
            self.__doc__='Routine for reading in %s data' % self.filetype
    def process(self):
        self.files=[]
        for filename,filetype in self.dataset.files:
            if(filetype==self.filetype):
                self.files.append(filename)
        for filename in self.files:
            self.readfile(filename)  

    def fixfilename(self,filename):
        return filename

    
            
        
             
class fort_cal(cal_base):
    """ Base class for calibration modules that call legacy fortran """
    def __init__(self,dataset):
        import os.path
        cal_base.__init__(self,dataset)
        self.pout=np.empty(len(self.outputs),dtype=np.int32,order='F')
        self.frqout=np.empty(len(self.outputs),dtype=np.int32,order='F')
        for i,p in enumerate(self.outputs):
            try:
                self.frqout[i]=p.frequency
            except AttributeError:
                self.frqout[i]=1
            self.pout[i]=p.number
        self.noutall=np.sum(self.frqout)
        self.fortname=getattr(self,'fortname',self.name) # Use the name as fortran module name unless explicitly set
        try:
            fdir=os.path.join(os.path.dirname(__file__),'fortran_modules')
            with open(os.path.join(fdir,'c_'+self.fortname.lower()+'.for'),'r') as fmod:
                comments=''
                for fline in fmod:
                    if(fline.startswith('C') or fline.startswith('!')):
                        comments+=fline[1:]
                    elif(fline.startswith('      SUBROUTINE')):
                        break
                try:
                    self.__doc__+=comments
                except TypeError:
                    self.__doc__=comments
        except IOError:
            pass            
    
    
    def process(self):
        """ Get the input data into an array matching the times..
        All input parameters must have a frequency and number set or will not be accepted as inputs
        Run the fortran
        Extract ouput into timestamped parameters     """
        from ppodd.c_runmod import c_runmod as run_old_module
        frqin=[]
        pin=[]
        inputs=[]
        constants=[]
        const=[]
        match=self.dataset.matchtimes(self.input_names,paras=inputs,notparas=constants)
        for c in constants:
            const.extend(c[:])
        for p in inputs:
            frqin.append(p.frequency)
            pin.append(p.number)
        constants=np.array(const,dtype=np.float32,order='F')    # Constants array
        frqin=np.array(frqin,dtype=np.int32,order='F')           # Input frequencies
        pin=np.array(pin,dtype=np.int32,order='F')               # Input parameter numbers
        length=len(match)
        if(length>0):
            """If there are data with any matching times"""
            din=np.empty((length,np.sum(frqin)),dtype=np.float32,order='F') # Input data
            flagin=np.zeros((length,np.sum(frqin)),dtype=np.int8,order='F') # Input flags
            ofs=0
            # Arrange inputs
            for i,p in enumerate(inputs):
                if(frqin[i]==1):
                    s=ofs
                else:
                    s=slice(ofs,ofs+frqin[i])    
                try:    
                    din[:,s]=p.data.ismatch(match).raw_data
                except ValueError:
                    print 'S=',s
                    print 'Data',p.data.shape
                    print 'Match',match.shape
                    print p.data.ismatch(match).raw_data.shape
                    print din[:,s].shape
                    raise ValueError
                try:
                    flagin[:,s]=p.data.flag.ismatch(match).raw_data 
                except:
                    pass
                ofs+=frqin[i]
            # Call FORTRAN
            print 'Calling fortran %s' % self.fortname
            dout,flagout=run_old_module(self.fortname,constants,
                                        pin,frqin,din,flagin,
                                        self.pout,self.frqout,self.noutall)

            # Arrange ouputs
            ofs=0
            for i,p in enumerate(self.outputs):
                frq=self.frqout[i]
                if(frq==1):
                    s=ofs
                else:
                    s=slice(ofs,ofs+frq)
                p.data=flagged_data(dout[:,s],match,flagout[:,s])
                ofs+=frq
        cal_base.process(self)    

