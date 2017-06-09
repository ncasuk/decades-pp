===============================
DECADES post processing package
===============================

ppodd (Post Processing of Decades Data - ppodd) is the library of python modules for processing aircraft data.

This includes c_runmod.so which in turn calls legacy fortran code in ppodd/fortran_modules.


Installation
============

Typical usage::

    git clone https://github.com/ncasuk/decades-pp.git
    cd decades-pp
    python setup build
    sudo python setup install


The fortran_modules can be built like this::

    cd decades_pp/ppodd/fortran_modules          #
    make                                         # build fortran libraries


Running from command line
=========================

Example for processing a flight::

    PPODD -o data/core_faam_YYYYMMDD_v001_r0_bNNN.nc data/flight-cst_faam_r0_YYYYMMDD_bNNN.txt data/bNNN_raw:M3 -o data/core_faam_YYYYMMDD_v001_r0_bNNN  # calibrate data

or::

    PPODD

for the GUI.
