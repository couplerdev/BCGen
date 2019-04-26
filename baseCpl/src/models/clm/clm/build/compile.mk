#
# File:  compile.mk
#
#----------------------------------------------------------------------------
#
#  This makefile finally performs the compile of the CLM code.  It is
#  called from a driver makefile that has already called makefiles for
#  preprocessing and dependency generation.
#
#----------------------------------------------------------------------------

SHELL = /bin/sh

#----------------------------------------------------------------------------
#
#  Define a symbol (TARGETX) for the executable name (clm)
#
#----------------------------------------------------------------------------

TARGETX = liblnd.a

#----------------------------------------------------------------------------
#
#  Define the dependency and include directories.
#
#----------------------------------------------------------------------------

DepDir = $(CLMEXEDIR)/compile/Depends

#----------------------------------------------------------------------------
#
#  Set valid suffixes.
#
#----------------------------------------------------------------------------

#  First clean out current list of suffixes, then define them
.SUFFIXES: 
.SUFFIXES: .o .c .f .f90 .d .do

ifeq ($(OPTIMIZE),yes)
  DEPSUF = .do
else
  DEPSUF = .d
endif

#----------------------------------------------------------------------------
#
#  Include architecture-specific flags and options. 
#
#----------------------------------------------------------------------------

ifneq (,$(CLMARCH))
  include $(CLMDIR)/build/$(CLMARCH).gnu
  export CLMARCH
else
  bogus:
	@echo "  Please setenv CLMARCH"
endif

#----------------------------------------------------------------------------
#
#  At this stage in the compile process, everything should be in the
#  compile and depend directories.
#
#----------------------------------------------------------------------------

SRCDIRS = $(CLMEXEDIR)/compile/ $(DepDir)/
VPATH = $(SRCDIRS)

#----------------------------------------------------------------------------
#
# Define source, object and dependency files.
#
#----------------------------------------------------------------------------

OBJS = 
DEPENDS = 

FSRCS   = $(strip $(foreach dir,$(SRCDIRS),$(wildcard $(dir)*.f)))
ifneq (,$(FSRCS))
  OBJS    := $(addprefix $(CLMEXEDIR)/compile/, $(notdir $(FSRCS:.f=.o))) $(OBJS)
  DEPENDS := $(addprefix $(DepDir)/, $(notdir $(FSRCS:.f=$(DEPSUF)))) $(DEPENDS)
endif

F90SRCS   = $(strip $(foreach dir,$(SRCDIRS),$(wildcard $(dir)*.f90)))
ifneq (,$(F90SRCS))
  OBJS    := $(addprefix $(CLMEXEDIR)/compile/, $(notdir $(F90SRCS:.f90=.o))) $(OBJS)
  DEPENDS := $(addprefix $(DepDir)/, $(notdir $(F90SRCS:.f90=$(DEPSUF)))) $(DEPENDS)
endif

CSRCS   = $(strip $(foreach dir,$(SRCDIRS),$(wildcard $(dir)*.c)))
ifneq (,$(CSRCS))
  OBJS    := $(addprefix $(CLMEXEDIR)/compile/, $(notdir $(CSRCS:.c=.o))) $(OBJS)
  DEPENDS := $(addprefix $(DepDir)/, $(notdir $(CSRCS:.c=$(DEPSUF)))) $(DEPENDS)
endif

#----------------------------------------------------------------------------
#
#  Make the executable.
#
#----------------------------------------------------------------------------

$(CLMEXEDIR)/$(TARGETX): $(OBJS)
	@echo "  GNUmakefile is making target '$(TARGETX)'"
	@$(AR) $(TARGETX) $(OBJS) 

#----------------------------------------------------------------------------
#
# Include all the dependency files
#
#----------------------------------------------------------------------------

# Sort to remove duplicates
DEPENDS := $(sort $(DEPENDS))

include $(DEPENDS)

#----------------------------------------------------------------------------
#
# Implicit rules for compilation
#
#----------------------------------------------------------------------------
 
# Cancel the implicit gmake rules for compiling
%.o : %.f
%.o : %.f90
%.o : %.c

%.o: %.f
	@echo $(CLMARCH) Compiling with implicit rule $<
	@cd $(CLMEXEDIR)/compile && $(F77) $(FFLAGS) -c $(notdir $<)
 
%.o: %.f90
	@echo $(CLMARCH) Compiling with implicit rule $<
	@cd $(CLMEXEDIR)/compile && $(F90) $(FFLAGS) -c $(notdir $<)
 
%.o: %.c
	@echo $(CLMARCH) Compiling with implicit rule $<
	@cd $(CLMEXEDIR)/compile && $(CC) $(Cpp_opts) $(CFLAGS) -c $(notdir $<)

