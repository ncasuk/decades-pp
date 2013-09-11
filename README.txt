decades post processing libraries.

ppodd ( Post Processing of Decades Data - ppodd ) is the library of python modules for processing aircraft data.

This includes c_runmod.so which in turn calls legacy fortran code in ppodd/fortran_modules.

Typical usage - 

git clone git@77.68.61.13:decades-pp         # get the repository



The fortran_modules can be built like this..

cd decades_pp/ppodd/fortran_modules          #
make                                         # build fortran libraries



Running from command line
-------------------------

processdd data/flight-cst_faam_r0_xxxxxxxx_bxxx.txt data/bxxx_raw:M3 -o data/core_faam_xxxxxxxx_v001_r0_bxxx.nc  # calibrate data




Writing modules for Decades Post Processing
-------------------------------------------

All processing modules should be subclasses of ppodd.core.cal_base.

They should include a process method which does the main work of the class, and an __init__ method for setting up inputs and outputs.

inputs should be defined as a list of parameter names.

outputs should be a list of parameters ( of type parameter defined in ppodd/core.py )

There are two defined subclasses already which can be subclassed further.

fort_cal 
Calls fortran calibration routines.  These would generally have been written for the previous processing scheme 'Tardis'.  With these only the __init__ method need be defined to define inputs, outputs and the name ( which will be used to access the fortran routine ).

file_reader 
Reads files, but must be subclassed to specialist file readers for different input types.  In these a readfile method should be created to do the file reading.  The inputs should be specified in __init__, but often the outputs won't be defined until readfile is called.


Example
-------

Here is a simple example to create a new parameter with calculated potential temperature.  Should be stored in a file with the same name as the class in folder ppodd so that it gets imported automatically.

from ppodd.core import cal_base,parameter,flagged_data   # Import the base class, parameter and data types.

class c_potential_temp(cal_base):
    """ Calculate Potential Temperature """
    
    def __init__(self,dataset):
        self.input_names=['PS_RVSM','TAT_DI_R']             # Define inputs ( pressure and temperature )

        self.outputs=[parameter('POT_TEMP',units='K',frequency=32,long_name='Potential Temperature')] # Define outputs

        self.name='potential_temp'                          # Name 

        self.version=1.00                                   # Version

        cal_base.__init__(self,dataset)                     # Call parent's initialisation
        
    def process(self):
        d=self.dataset                              # The dataset with all the input data

        match=d.matchtimes(['PS_RVSM','TAT_DI_R'])  # Work out where times match ( doesn't check frequency ) 

        p1=d['PS_RVSM'].data.ismatch(match)         # Get pressure values

        t1=d['TAT_DI_R'].data.ismatch(match)        # Get temperature values

        # Calculate potential temperature as timestamped and flagged data
        # Set the timestamps and flags to be the same as the pressure
        pote=flagged_data(t1*(1000.0/p1)**(2.0/7.0),p1.times,p1.flag) # Potential temp (K)

        # Set the flags to be the worse of the two inputs
        pote.flag[t1.flag>p1.flag]=t1.flag[t1.flag>p1.flag]

        self.outputs[0].data=pote                   # Set output data



 
