#Make macros for CAM.

UNAMES        :=  LINUX
ROOTDIR       :=  /data2/workspace/BCGen/baseCpl/src
INC_NETCDF    :=  
MOD_NETCDF    :=  
USER_CFLAGS   :=  
USER_FFLAGS   :=  
INC_PNETCDF   :=  
INC_MPI       :=  
SPMD          :=  TRUE
CCSM_CFLAGS   :=     -DCO2A -DMAXPATCH_PFT=numpft+1 -DLSMLAT=1 -DLSMLON=1 -DPLON=72 -DPLAT=46 -DNUM_COMP_INST_ATM=1 -DNUM_COMP_INST_LND=1 -DNUM_COMP_INST_OCN=1 -DNUM_COMP_INST_ICE=1 -DNUM_COMP_INST_GLC=1 -DNUM_COMP_INST_ROF=1 -DNUM_COMP_INST_WAV=1 -DPLEV=30 -DPCNST=25 -DPCOLS=16 -DPSUBCOLS=1 -DPTRM=1 -DPTRN=1 -DPTRK=1 -DSTAGGERED  -DMODAL_AERO -DMODAL_AERO_3MODE 
CPPDEFS       := -DSPMD -fcray-pointer -fno-var-tracking

IGNORE_LINE_TRUNCAT := -ffree-line-length-none

include ../../Makefile.conf

INCLFLAG := -I${INCLDIR} -I${ESMF_MOD} -I${PIO_INCL} -I${NETCDFINCL}

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


LIB_COMP: $(OBJS)
	$(AR) csr $(LIB_COMP) $(OBJS)
	cp -f $(LIB_COMP) ../../../lib
	cp -f comp_atm.mod ../../../include

$(CURDIR)/Depends: $(CURDIR)/Srcfiles  $(CURDIR)/Filepath
	$(ROOTDIR)/models/cam/bld/mkDepends Filepath Srcfiles > $@

$(CURDIR)/Srcfiles: $(CURDIR)/Filepath
	$(ROOTDIR)/models/cam/bld/mkSrcfiles > $@



# compilation rules:
.SUFFIXES:
.SUFFIXES: .F .F90 .f90 .c .s .o

.F.o:
	$(FC) -c $(INCLFLAG) $(CCSM_CFLAGS) $(CPPDEFS) $(IGNORE_LINE_TRUNCAT) $<

.F90.o:
	$(FC) -c $(INCLFLAG) $(CCSM_CFLAGS) $(CPPDEFS) $(IGNORE_LINE_TRUNCAT) $<

.f90.o: 
	$(FC) -c $(INCLFLAG) $(CCSM_CFLAGS) $(CPPDEFS) $(IGNORE_LINE_TRUNCAT) $<

.c.o:
	$(FC) -c $(INCLFLAG) $(CCSM_CFLAGS) $(CPPDEFS) $(IGNORE_LINE_TRUNCAT) $<

.s.o:
	$(AS) -m $<

include $(CURDIR)/Depends


