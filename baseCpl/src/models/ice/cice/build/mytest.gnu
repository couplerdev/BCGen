#----------------------------------------------------------------------
#
#  File:  mac_ppc_absoft.gnu
#
#  Contains compiler and loader options for a Power PC Mac OS using 
#  the Absoft compiler and specifies the serial directory for 
#  communications modules.
#
#----------------------------------------------------------------------

#----------------------------------------------------------------------
#
#  Basic commands - use F77 for fixed form F90 for free form
#
#----------------------------------------------------------------------




INCLDIR = /home/hq/share/BCGen_case/BCGen_inst/include

F77 = mpif90
F90 = mpif90
LD = mpif90
AR = ar crv
CC = mpicc
Cp = /usr/bin/cp
Cpp = /usr/bin/gcc -P -E
AWK = /usr/bin/awk
ABI = 
COMMDIR = serial
 
#----------------------------------------------------------------------
#
# Set up necessary library and include paths.
#
#----------------------------------------------------------------------

#  netcdf paths

NETCDFINC = -I/home/hq/include
NETCDFLIB = -L/home/hq/lib

#  Enable MPI library for parallel code, yes/no.

MPI = yes

#  Enable trapping and traceback of floating point exceptions, yes/no.

TRAP_FPE = no

OPTIMIZE = no

#------------------------------------------------------------------
#  Set any precompiler options
#------------------------------------------------------------------

#DCOUPL              = -Dcoupled

Cpp_opts =   \
      $(DCOUPL)

Cpp_opts := $(Cpp_opts) -DPOSIX $(NETCDFINC) -DCCSMCOUPLED -I$(INCLDIR) -Dcoupled -Dncdf -DNXGLOB=100 -DNYGLOB=116 -DNTR_AERO=3 -DNCAT=1 -DBLCKX=50 -DBLCKY=58 -DMXBLCKS=4

#----------------------------------------------------------------------------
#
#                           C Flags
#
#----------------------------------------------------------------------------
 
CFLAGS = 

ifeq ($(OPTIMIZE),yes)
  CFLAGS := $(CFLAGS) 
else
  CFLAGS := $(CFLAGS) -g
endif
 
#----------------------------------------------------------------------------
#
#                           FORTRAN Flags
#
#----------------------------------------------------------------------------
 
FBASE = -v -I/usr/local/include -I/home/hq/include -I$(DepDir) -I$(INCLDIR) -I/home/hq/share/BCGen_case/BCGen_inst/include -ffree-line-length-none -I/usr/local/esmf/mod/modO/Linux.gfortran.64.mpiuni.default/ 
MODSUF = mod

ifeq ($(TRAP_FPE),yes)
  FBASE := $(FBASE) 
endif

ifeq ($(OPTIMIZE),yes)
  FFLAGS := $(FBASE) -O2 -cpu:g4 -N11 -round=NEAREST -altiVec
else
  FFLAGS := $(FBASE) -g
endif
 
#----------------------------------------------------------------------------
#
#                           Loader Flags and Libraries
#
#----------------------------------------------------------------------------
 
LDFLAGS = $(FFLAGS)
 
LIBS = -L/usr/local/absoft/lib -lnetcdf
 
ifeq ($(MPI),yes)
  LIBS := $(LIBS) -lmpi
endif

ifeq ($(TRAP_FPE),yes)
  LIBS := $(LIBS) -lfpe
endif
 
#LDLIBS = $(TARGETLIB) $(LIBRARIES) $(LIBS)
LDLIBS = $(LIBS)
 
#----------------------------------------------------------------------------
