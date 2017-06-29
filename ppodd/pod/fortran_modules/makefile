#
# Makefile for legacy fortran modules to be included in new processing
#
#
#
# The compiler
FC = ifort
CALLIB = callib.a
AC = ar rc
OUT = ../.
# flags for debugging or for maximum performance, comment as necessary
FCFLAGS = -extend_source -fPIC
F2PYFLAGS = -lifcore --f77exec=$(FC) --f90exec=$(FC)
F2PYARGS = -c -m c_runmod c_runmod.for $(CALLIB)
# List of modules to be built within the package
MODULES = c_airspd
MODULES+= c_cnc
MODULES+= c_drs
#MODULES+= c_fwvs
MODULES+= c_geneas
MODULES+= c_heiman
#MODULES+= c_ins1
MODULES+= c_lwc
MODULES+= c_nephl1
MODULES+= c_nevz
MODULES+= c_nox
MODULES+= c_so2
MODULES+= c_comr
MODULES+= c_ozone1
MODULES+= c_press1
MODULES+= c_psap
MODULES+= c_radal1
MODULES+= c_rvsm
MODULES+= c_sols
MODULES+= c_temps2
MODULES+= c_tpress
MODULES+= c_turb
MODULES+= c_twc
MODULES+= c_winds
MODULES+= c_gwinds
MODULES+= c_gsun
MODULES += c_grflux
MODULES += itstflg
MODULES += isetflg
MODULES += s_mach
MODULES += g_mach
MODULES += s_runm
MODULES += s_qcpt
MODULES += s_sun

all: c_runmod.so
ifneq ($(OUT),.)
	mv c_runmod.so $(OUT)
endif
# "make" builds runmod.so
c_runmod.so: $(CALLIB)
	f2py $(F2PYFLAGS) $(F2PYARGS)

# Removes object files

$(CALLIB): $(MODULES)
	rm -f *.o

# General rule for building library from object (*.o) files
%: %.o
	$(AC) $(CALLIB) $<

# General rules for building prog.o from prog.for
%.o: %.for
	$(FC) $(FCFLAGS) -c $<
