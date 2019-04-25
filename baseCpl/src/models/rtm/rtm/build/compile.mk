#
# File:  compile.mk
#
#----------------------------------------------------------------------------
#
#  This makefile finally performs the compile of the RTM code.  It is
#  called from a driver makefile that has already called makefiles for
#  preprocessing and dependency generation.
#
#----------------------------------------------------------------------------

SHELL = /bin/sh

#----------------------------------------------------------------------------
#
#  Define a symbol (TARGETX) for the executable name (rtm)
#
#----------------------------------------------------------------------------

TARGETX = librtm.a

#----------------------------------------------------------------------------
#
#  Define the dependency and include directories.
#
#----------------------------------------------------------------------------

DepDir = $(RTMEXEDIR)/compile/Depends

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

ifneq (,$(RTMARCH))
  include $(RTMDIR)/build/$(RTMARCH).gnu
  export RTMARCH
else
  bogus:
	@echo "  Please setenv RTMARCH"
endif

#----------------------------------------------------------------------------
#
#  At this stage in the compile process, everything should be in the
#  compile and depend directories.
#
#----------------------------------------------------------------------------

SRCDIRS = $(RTMEXEDIR)/compile/ $(DepDir)/
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
  OBJS    := $(addprefix $(RTMEXEDIR)/compile/, $(notdir $(FSRCS:.f=.o))) $(OBJS)
  DEPENDS := $(addprefix $(DepDir)/, $(notdir $(FSRCS:.f=$(DEPSUF)))) $(DEPENDS)
endif

F90SRCS   = $(strip $(foreach dir,$(SRCDIRS),$(wildcard $(dir)*.f90)))
ifneq (,$(F90SRCS))
  OBJS    := $(addprefix $(RTMEXEDIR)/compile/, $(notdir $(F90SRCS:.f90=.o))) $(OBJS)
  DEPENDS := $(addprefix $(DepDir)/, $(notdir $(F90SRCS:.f90=$(DEPSUF)))) $(DEPENDS)
endif

CSRCS   = $(strip $(foreach dir,$(SRCDIRS),$(wildcard $(dir)*.c)))
ifneq (,$(CSRCS))
  OBJS    := $(addprefix $(RTMEXEDIR)/compile/, $(notdir $(CSRCS:.c=.o))) $(OBJS)
  DEPENDS := $(addprefix $(DepDir)/, $(notdir $(CSRCS:.c=$(DEPSUF)))) $(DEPENDS)
endif

#----------------------------------------------------------------------------
#
#  Make the executable.
#
#----------------------------------------------------------------------------

$(RTMEXEDIR)/$(TARGETX): $(OBJS)
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
	@echo $(RTMARCH) Compiling with implicit rule $<
	@cd $(RTMEXEDIR)/compile && $(F77) $(FFLAGS) -c $(notdir $<)
 
%.o: %.f90
	@echo $(RTMARCH) Compiling with implicit rule $<
	@cd $(RTMEXEDIR)/compile && $(F90) $(FFLAGS) -c $(notdir $<)
 
%.o: %.c
	@echo $(RTMARCH) Compiling with implicit rule $<
	@cd $(RTMEXEDIR)/compile && $(CC) $(Cpp_opts) $(CFLAGS) -c $(notdir $<)

