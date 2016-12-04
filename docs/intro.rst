PPODD: Post Processing Of Decades Data

==========
Intro
==========

The core data from the FAAM BAe146, and previously the C-130 have always been calibrated post flight with the TARDIS (**T**ranscription of **A**ircraft **R**aw **D**ata **I**nto **S**cientific units) routines. These are a suite of FORTRAN routines written in a modular way so as each instrument or small group of similar instruments are calibrated with a particular module. Raw data were all recorded in one standard raw data file, and the data were processed one second at a time, and written to an output file of an in house binary format (MRF2 or MRF5). The modules had to be run in a particular order so within the main CALIBRATE.FOR routine an array was set up defining inputs and outputs of each module from whence an order could be calculated.

For FAAM it was decided to output in the more standard NetCDF so routines were written to convert the format, but leaving the basic output as MRF5. There has also been a need to include data not recorded by the standard DRS (data recording system), and hence in a different raw format. PSAP photometer, Buck hygrometer and notably the GIN (combined GPS and inertial navigation system) had to be squeezed in after the main processing. The GIN in particular had to be merged into the output dataset, and then parts of the processing re-run to calculate winds.

The interactive side of the processing was abandoned with the move to processing on Linux rather than the original VMS. Some data still couldn't be calculated one second at a time, notably the Total Water Content, which needs to be fitted against another hygrometer for all or part of the flight. These extra routines were written in PVwave/IDL and run after the rest of the code. 

Motive
======

The main reason for changing the code is the arrival of a new data recording system, DECADES so there will be no data recorded in the original raw data format. There are other problems alluded to already that could be addressed in the upgrade:

* The use of IDL, which often presents licensing issues, and is being moved away from by the Met Office in favour of Python.
* Old FORTRAN code, not well understood by the FAAM team so difficult to maintain adequate level of support.
* Reliance on old in house data formats, which were designed for a different operating system.
* There is a mix of input data formats, so having the main processing relying on just one format means all the other data have to be treated as bolt ons. 

Initial Plan
============

The new code will be chiefly Python/numpy. This is becoming the standard scientific analysis tool in the Met Office and is widely used in academia. It also has the advantage that we can still use original FORTRAN modules via f2py so that the transition can be more gradual. Modules can be rewritten as a background task or as and when they are needed.

The modular approach used previously will be kept, but with a more flexible internal representation of the data, and a wider variety of routines for reading data in. The modules will hopefully be more self describing, so that they can define their own inputs and outputs etc.

Internal data will be in numpy arrays, with associated timestamps. Matching these timestamps and extracting the relevant data to run through the various modules will be the basis of the new suite. 

Modules will be written to read in both new, and old format data for comparison purposes, and a limited level of backwards compatibility. However not every older data format has been ported, but should the need arise new modules can be added.


Implementation
==============

Now that the processing is in a usable state it is worth explaining the basic structure.

The basic unit is the ppodd.core.decades_dataset, which is a collection of parameters and a dictionary of processing modules. The processing modules are initialized from python files in ppodd.pod, which holds a dictionary of classes (ppodd.pod.modules) which are only instantiated when a dataset is created.

There are several types of parameters. The most important are the data parameters which are time stamped arrays with meta data. There are also constant parameters for storing flight calibration data, file parameters for information on input files and attribute parameters for global attribute data.

NB.There was some effort put in to creating a class of timed_data arrays. It is possible that this will be replaced in the future with a variation of pandas TimeSeries or DataFrame objects, but there is no pressing need for this now.

Every module will define a list of input_names. If all the input_names are in the data-set the module can be run. The run method extracts all the named inputs from the data-set, and then runs the process method of the module. There are two alternative ways of running the processing:

* Process all modules that it is able to with available inputs.
* Run the minimum modules needed to produce a particular set of output data parameters.

Most modules will define what outputs they produce at the initialization stage, but some notably those that read in data from files may not know what outputs they have until they are processed. Equally some modules may be able to run with smaller subset of inputs than is ideal. This is a slightly trickier problem, as we would not want the module to run until the best set of inputs are available if they are going to be, but we would still want it to run if the minimum are there. For this case it is usually best to only define the minimum inputs, and in some cases a second stage of processing would be written for when the extra inputs are available.
