
=====
Usage
=====

The routines can be run a number of different ways depending on need. Simple python scripts can run the processing if access to data within a program is needed. A command line, can run the processing and produce an output, or a gui can be run which enables a certain amount of interactive exploration of the data.  

A simple programming example::

    import ppodd.core

    d=ppodd.core.decades_dataset()
    d.add_file('decades_data/b111.zip')
    d.add_file('decades_data/flight-cst_faam_20131001_r0_b111.txt')
    d.process()
    d.write_nc()

d would then hold all the processed data. It could be plotted using matplotlib or calculations performed. Each parameter accessed d['parameter name'] there being various attributes, .data actually holding the numbers and can be viewed several ways.::

    d['JW_LWC_U'][:] is equivalent to d['JW_LWC_U'].data

    d['JW_LWC_U'].times is equivalent to d['JW_LWC_U'].data.times

See the explanation of the timed_data array, and parameters.

The same could be run from a command line::

    PPODD decades_data/flight-cst_faam_20131001_r0_b111.txt decades_data/b111.zip -w full

To wrap around this structure there is also a graphical user interface.  This should make the process of calibrating and checking core data relatively painless. 

To achieve the same result::

    PPODD (to open the GUI – or use the desktop shortcut)

From menu Files, Files then Add File (to add the relevant file ). Click Process, followed by File, Write_nc.  
