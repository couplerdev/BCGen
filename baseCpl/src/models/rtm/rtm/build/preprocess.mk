#
# File:  preprocess.mk
#
#----------------------------------------------------------------------------
#
#  This makefile is called from the RTM driver makefile and performs
#  only the preprocessing step.
#
#----------------------------------------------------------------------------

SHELL    = /bin/sh

#----------------------------------------------------------------------------
#
#  Set valid suffixes.
#
#----------------------------------------------------------------------------

#  First clean out current list of suffixes, then define them
.SUFFIXES: 
.SUFFIXES: .c .f .f90 .F .F90 .C

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
	@echo "  Please set RTMARCH environment variable"
endif

#----------------------------------------------------------------------------
#
#  Define paths to sources in variable SRCDIRS.
#
#----------------------------------------------------------------------------

SRCDIRS = $(RTMEXEDIR)/
SRCDIRS := $(SRCDIRS) $(RTMDIR)/src/riverroute/
SRCDIRS := $(SRCDIRS) $(RTMDIR)/src/cpl_share/
SRCDIRS := $(SRCDIRS) $(RTMDIR)/src/cpl_mct/

#----------------------------------------------------------------------------
#
#  VPATH is the built-in symbol whose value is the path that gmake will 
#  search for dependencies.
#
#----------------------------------------------------------------------------

VPATH = $(SRCDIRS)

#----------------------------------------------------------------------------
#
# Define .F sources that must be preprocessed into the build directory as .f
# and add .f version to list of target source files.
#
#----------------------------------------------------------------------------

SOURCES = 
FSRCS   = $(strip $(foreach dir,$(SRCDIRS),$(wildcard $(dir)*.F)))
ifneq (,$(FSRCS))
  SOURCES := $(addprefix $(RTMEXEDIR)/compile/, $(notdir $(FSRCS:.F=.f))) \
             $(SOURCES)
endif

#----------------------------------------------------------------------------
#
# Define .F90 sources to be preprocessed into the build directory as .f90
#
#----------------------------------------------------------------------------

F90SRCS   = $(strip $(foreach dir,$(SRCDIRS),$(wildcard $(dir)*.F90)))
ifneq (,$(F90SRCS))
  SOURCES := $(addprefix $(RTMEXEDIR)/compile/, $(notdir $(F90SRCS:.F90=.f90))) \
             $(SOURCES)
endif

#----------------------------------------------------------------------------
#
# Define .C sources that must be preprocessed into the build directory as .c
#
#----------------------------------------------------------------------------

CSRCS   = $(strip $(foreach dir,$(SRCDIRS),$(wildcard $(dir)*.C)))
ifneq (,$(CSRCS))
  SOURCES := $(addprefix $(RTMEXEDIR)/compile/, $(notdir $(CSRCS:.C=.c))) \
             $(SOURCES)
endif

#----------------------------------------------------------------------------
#
# Define any .f sources that need to be copied into the build directory
#
#----------------------------------------------------------------------------

LFSRCS  = $(strip $(foreach dir,$(SRCDIRS),$(wildcard $(dir)*.f)))
ifneq (,$(LFSRCS))
  ifneq (,$(FSRCS))
    LFSRCS    := $(filter-out $(FSRCS:.F=.f),$(LFSRCS))
  endif
  ifneq (,$(LFSRCS))
    SOURCES := $(addprefix $(RTMEXEDIR)/compile/, $(notdir $(LFSRCS))) \
               $(SOURCES)
  endif
endif

#----------------------------------------------------------------------------
#
# Define .f90 sources that need to be copied into the build directory
#
#----------------------------------------------------------------------------

LF90SRCS  = $(strip $(foreach dir,$(SRCDIRS),$(wildcard $(dir)*.f90)))
ifneq (,$(LF90SRCS))
  ifneq (,$(F90SRCS))
    LF90SRCS    := $(filter-out $(F90SRCS:.F90=.f90),$(LF90SRCS))
  endif
  ifneq (,$(LF90SRCS))
    SOURCES := $(addprefix $(RTMEXEDIR)/compile/, $(notdir $(LF90SRCS))) \
               $(SOURCES)
  endif
endif

#----------------------------------------------------------------------------
#
# Define .c sources that need to be copied into the build directory
#
#----------------------------------------------------------------------------

LCSRCS   = $(strip $(foreach dir,$(SRCDIRS),$(wildcard $(dir)*.c)))
ifneq (,$(LCSRCS))
  ifneq (,$(CSRCS))
    LCSRCS    := $(filter-out $(CSRCS:.C=.c),$(LCSRCS))
  endif
  ifneq (,$(LCSRCS))
    SOURCES := $(addprefix $(RTMEXEDIR)/compile/, $(notdir $(LCSRCS))) \
               $(SOURCES)
  endif
endif

#----------------------------------------------------------------------------
#
# Preprocess all source files.  Implicit rules should take care of all cases.
#
#----------------------------------------------------------------------------

.PHONY: preprocess

preprocess: $(SOURCES)

#----------------------------------------------------------------------------
#
# Implicit rules for preprocessing.
#
#----------------------------------------------------------------------------
 
# Cancel the implicit gmake rules for preprocessing

%.c : %.C
%.f90 : %.F90
%.f : %.F

# Preprocessing rules for Fortran (.F, F90) and C files

$(RTMEXEDIR)/compile/%.f: %.F
	@echo '$(RTMARCH) preprocessing ' $<
	@$(Cpp) $(Cpp_opts) $< > $(RTMEXEDIR)/compile/$*.f

$(RTMEXEDIR)/compile/%.f90: %.F90
	@echo '$(RTMARCH) preprocessing ' $<
	@$(Cpp) $(Cpp_opts) $< > $(RTMEXEDIR)/compile/$*.f90

#  For some reason, our current Cpp options are incorrect for C files
#  so let the C compiler take care of ifdefs and just copy.

$(RTMEXEDIR)/compile/%.c: %.C
	@echo '$(RTMARCH) preprocessing ' $<
	@$(Cp) $< $(RTMEXEDIR)/compile/$*.c

# Preprocessing rules for Fortran f, f90 and c files
# Should only copy these files into the compile directory.

$(RTMEXEDIR)/compile/%.f: %.f
	@echo '$(RTMARCH) preprocessing ' $<
	@$(Cp) $< $(RTMEXEDIR)/compile/$*.f

$(RTMEXEDIR)/compile/%.f90: %.f90
	@echo '$(RTMARCH) preprocessing ' $<
	@$(Cp) $< $(RTMEXEDIR)/compile/$*.f90

$(RTMEXEDIR)/compile/%.c: %.c
	@echo '$(RTMARCH) preprocessing ' $<
	@$(Cp) $< $(RTMEXEDIR)/compile/$*.c

#----------------------------------------------------------------------------
