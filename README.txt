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

or

PPODD ( for the GUI )


