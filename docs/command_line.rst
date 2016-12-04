
=============
PPODD Command
=============

The command PPODD will start processing data, the arguments are a list of input files.  The types of these files can be set explicitly with a colon after the file name, or they will be assumed by the program, based one file suffix etc. (see file_read.patterns and file_read.filetest).  

There are various options to select which modules to use, which parameters to calculate, and the output file.  If no output options are selected or if no input files the GUI will open.::

    Usage: PPODD [options] [input_file1[:type1] input_file2[:type2] .. input_filen[:typen]
    
    
    Options:
    
      -h, --help            show this help message and exit
    
      -w full, and or 1hz, --write=full, and or 1hz
    
                            Output file types - if not writing open GUI
    
      -o output, --output=output
    
                            Output file or folder - if not writing open GUI
    
      -p p1,p2...pn or all, --parameters=p1,p2...pn or all
    
                            Parameters to output
    
      -m mod1,mod2..modn, --modules=mod1,mod2..modn
    
                            Only run these modules
    
      -n mod1,mod2..modn, --not_modules=mod1,mod2..modn
    
                            Don't run these modules
    
      -l filename, --logfile=filename
    
                            Log file
    
      --nctype=NCTYPE       NetCDF type of output

      
The output can be at 1Hz or full frequency, or both with the -w,--write option. The -o, --output option chooses the folder or the full file name of the output (if not chosen, the default folder is $NCDATA, and the default file name created from the flight number and date as prescribed by BADC).

PPODD GUI
=========

The graphical interface for PPODD can be run via the command line ( if no output options are selected ) or from a Desktop shortcut. If the Desktop shortcut is used it is possible to drag an input file onto the shortcut to open the program with that input.

All the standard functions are available through the GUI, but it is a little less flexible in terms of output options. It will only try and write to $NCDATA with the normal BADC naming convention. You do however have the ability to change input files, archive data, and have a quick look / quality check the data.
