
==================
Processing Modules
==================

Like previous versions the processing is divided up in to a collection of modules. Each one will most likely deal with one instrument, or small group thereof. The modules can also perform more basic functions ( eg. Unzipping files ), what they have in common is identified inputs and outputs, and a run method to go from one to the other.

The standard modules are all stored in the package ppodd.pod. For the most common uses, these need not be changed, but there will be new instruments, changes to existing, and even bugs found in the current.  These are the basis of any processing, but there is facility to amend these or add in modules from elsewhere. Within ppodd.pod each module with a name with p\_\* is assumed to contain a class. These are placed in the dictionary ppodd.pod.modules which the decades_dataset will load into it's own modules attribute.

There are three basic types of module.
  * cal_base – this is the basis of all modules, defines a method for checking inputs, and running processes.
  * file_read – a file reading class, will try and read in a list of files by calling its readfile method.
  * fort_cal – for running the legacy FORTRAN routines.  

These would then be sub-classed by something which actually does the work.  

cal_base
========

cal_base is the root of all calibration routines, and stores information on the current running state, the dataset it is being applied to a version number and running history.  It would need to be subclassed to do any calculations, either directly or via fort_cal or file_read.

The running state (.runstate) will start off as ready moving to running, and end as either success or fail depending on whether the inputs could be found.  

If a new module needs to be tested it can be added to the modules that the processing uses, without being placed in the standard module folder.


An example of a test module sub-classing from cal_base::

    from ppodd.core import *
     
    class potential_temp(cal_base):
        “”” Test module for calculating potential temperature “””
        def __init__(self,dataset):
            self.input_names=['PS_RVSM','TAT_DI_R']
            self.outputs=[parameter('POT_TEMP',units='K',
              frequency=32,long_name='Potential Temperature')]
            self.version=1.00
            cal_base.__init__(self,dataset) 
    
        def process(self):
            d=self.dataset
            match=d.matchtimes(['PS_RVSM','TAT_DI_R'])
            p1=d['PS_RVSM'].data.ismatch(match)
            t1=d['TAT_DI_R'].data.ismatch(match)
            pote=flagged_data(t1*(1000.0/p1)**(2.0/7.0),p1.times,p1.flag) #!Potential temp (K)
            pote.flag[t1.flag>p1.flag]=t1.flag[t1.flag>p1.flag]
            self.outputs[0].data=pote
    
    """
    An example of how to add a module for testing before being
    added to the main processing suite in ppodd.pod 
    """
 
    import ppodd.pod
    
    ppodd.pod.addmodule(potential_temp)
    d=decades_dataset()
    d.add_file('decades_data/b111.zip')
    d.add_file('decades_data/flight-cst_faam_20131001_r0_b111.txt')
    d.process()
    

We define a module a sub-class of cal_base which calculates potential temperature from pressure (PS_RVSM) and temperature (TAT_DI_R).  The input names are defined, and the output parameter defined.  In the process method the times of the 2 inputs are matched, the calculation performed and the flags set.

This new module can then be added by the line::

    ppodd.pod.addmodule(potential_temp)

So that when a decades_dataset is created the new module is available, and will be calculated if the inputs are there.


fort_cal
========

A sub-classed fort_cal would require little more than a definition of input, and outputs, and the name of the FORTRAN routine (assumed to be the same as the class name). It should then put the data into the correct size and shape arrays, and via f2py and ppodd.pod.c_runmod will run the FORTRAN. Once the calculations have completed the data will be extracted from the array and put into the output parameters. Only the __init__ method need be overridden.

Fortran calling example::

   from ppodd.core import *
    
   class ozone1(fort_cal):
   
       def __init__(self,dataset):
           self.input_names=['CALO3', 'CALO3P', 'CALO3T', 'CALO3F', \
             'CALO3MX', 'Horace_O3', 'Horace_RVAS']
           self.outputs=[parameter('O3_TECO',units='ppb',frequency=1,number=574, \
             long_name='Mole fraction of ozone in air from the TECO 49 instrument')]
           self.version=1.00
           fort_cal.__init__(self,dataset)
   
All the matching of timestamps etc should be handled by the base class, this only works for FORTRAN previously compiled and linked to the c_runmod.for using f2py. If files in the pod/fortran_modules folder are modified, then c_runmod.so will need to be remade (there is a makefile), however it is not anticipated that changes should be made to the FORTRAN, but to rather to either the python wrapper, or rewritten using python.


file_read
=========

A sub-classed file_read needs to override the __init__, and implement the readfile method.  readfile should take a file name as input, and may be called a number of times with different files. It should also be noted that the first input name should name the file type that this reads in, and this will be the name of the parameter which lists these files. A combination of the patterns tuple, and filetest method will be used to guess the file type when not specified, and fixfilename, will alter a full file path to something the readfile method understands.  file_read will try to parse any file names for flight number and date, if they are not already in the data.

File reading example::

   from ppodd.core import *
   import numpy as np
   import ppodd
   
   class readincloud(file_read):
       """
       Routine for reading in some imaginary instrument data
       """
       def __init__(self,dataset):
           self.input_names = ['INCLOUD']
           self.patterns = ('incloud*.txt',)
           self.outputs  =[parameter('INCL_TEMP',units='K',frequency=1, \
             long_name='In cloud temperature from imaginary instrument')]
           self.data = None
           file_read.__init__(self,dataset)
   
   
       def readfile(self,filename):
           x = np.genfromtxt(filename,delimiter=',',names=['Time','temp','volts'],skip_header=1)
           data = timed_data(x['temp'],x['Time'])
           if(self.outputs[0].data):
               self.outputs[0].data=np.append(self.outputs[0].data,data)
           else:
               self.outputs[0].data=data
           self.outputs[0].data.timesort()


The readfile method may be called more than once if there is a list of input files, and should deal with this appropriately – likely adding new data, and sorting if necessary. It defines patterns, which is a tuple of file search strings, to help other processes guess file types when not specified.
