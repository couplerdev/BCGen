#Make macros for CAM.

UNAMES        :=  LINUX
ROOTDIR       :=  /share/BCGen/develDir/BCGenV1.0/BCGen/baseCpl/src
INC_NETCDF    :=  
MOD_NETCDF    :=  
INC_PNETCDF   :=  
INC_MPI       :=  


include ../../Makefile.conf

INCLFLAG :=

null  :=
space := $(null) $(null)
comma := $(null),$(null)


SRC := $(shell cat Srcfiles)

dirs := . $(shell cat Filepath)

CURDIR := $(shell pwd)

VPATH := $(foreach dir,$(dirs),$(wildcard $(dir)))

VPATH := $(subst $(space),:,$(VPATH))

LIB_COMP := libatm.a

OBJS := $(addsuffix .o, $(basename $(SRC)))


OBJS: $(SRC)
	$(FC) -c $<

$(CURDIR)/Depends: $(CURDIR)/Srcfiles  $(CURDIR)/Filepath
	$(ROOTDIR)/models/cam/bld/mkDepends Filepath Srcfiles > $@

$(CURDIR)/Srcfiles: $(CURDIR)/Filepath
	$(ROOTDIR)/models/cam/bld/mkSrcfiles > $@



# compilation rules:
.SUFFIXES:
.SUFFIXES: .F .F90 .f90 .c .s .o

.F.o:
	$(FC) -c $(INCLFLAG) $<

.F90.o:
	$(FC) -c $(INCLFLAG) $<

.f90.o: 
	$(FC) -c $(INCLFLAG) $<

.c.o:
	$(FC) -c $(INCLFLAG) $<

.s.o:
	$(AS) -m $<

include $(CURDIR)/Depends


