#
# File:  GNUmakefile
#
#----------------------------------------------------------------------------
#
#  This is the main driver makefile for compiling RTM.  It relies on
#  sub-makefiles to perform preprocessing, dependency analysis and compiling.
#
#  Several variables must be set either as environment variables or on the 
#  command line (e.g. gmake RTMDIR=/your/path).  These variables are:
#
#  RTMEXEDIR = the directory where you want the executable to end up.  A
#              common value might be . (the current working directory)
#  RTMDIR    = the path to the RTM distribution from which you want
#              to grab source files, etc.  This should be the path to the 
#              top level rtm directory, not the individual source,mpi 
#              directories.
#  RTMARCH   = the base name of a file in the build directory where 
#              architecture and location-specific information is defined
#
#  The optimize variable has a default value of yes and can not be set
#  as an environment variable.  It can only be changed to no via the
#  command line.
#  OPTIMIZE  = yes/no  whether you want full optimization or no optimization
#
#----------------------------------------------------------------------------
#
#  The bourne shell is safest for gmake
SHELL = /bin/sh

RTMDIR=/home/hq/share/BCGen_case/BCGen_inst/models/rof/rtm
RTMEXEDIR=/home/hq/share/BCGen_case/BCGen_inst/models/rof/rtm/build
RTMARCH=mytest
#
#
#  By default, you build optimized code.  To override, add "OPTIMIZE=no" 
#  to the gmake command line.
OPTIMIZE   = yes
#
#----------------------------------------------------------------------------
#
#  Check that path variables RTMEXEDIR, RTMDIR have been set.
#
#----------------------------------------------------------------------------

VARSDEFINED =

ifneq (,$(RTMEXEDIR))
  VARSDEFINED := $(VARSDEFINED)RTMEXEDIR
  export RTMEXEDIR
else
  bogusexedir:
	@echo "  Please setenv RTMEXEDIR"
endif

ifneq (,$(RTMDIR))
  VARSDEFINED := $(VARSDEFINED)RTMDIR
  export RTMDIR
else
  bogusrtmdir:
	@echo "  Please setenv RTMDIR"
endif

#  Set the directory where dependency info will reside.
DepDir = $(RTMEXEDIR)/compile/Depends

#----------------------------------------------------------------------------
#
#  Include architecture-specific flags and options. 
#
#----------------------------------------------------------------------------

ifneq (,$(RTMARCH))
  export RTMARCH
  VARSDEFINED := $(VARSDEFINED)RTMARCH
  ifneq (,$(RTMDIR))
     include $(RTMDIR)/build/$(RTMARCH).gnu
  endif
else
  bogusrtmarch:
	@echo "  Please setenv RTMARCH"
endif

#----------------------------------------------------------------------------
#
#  Define a symbol (TARGETX) for the executable name (rtm)
#
#----------------------------------------------------------------------------

TARGETX = rtm
ifeq ($(OPTIMIZE),no)
  #  If building a debug version, append "_db" to the executable name
  TARGETX := $(TARGETX)_db
endif

#----------------------------------------------------------------------------
#
#  Make the rtm executable
#
#----------------------------------------------------------------------------

.PHONY: depends preprocess compile dirs

$(TARGETX): depends
	@echo "  GNUmakefile is making target '$(TARGETX)'"
	@gmake -f $(RTMDIR)/build/compile.mk RTMEXEDIR=$(RTMEXEDIR) RTMDIR=$(RTMDIR) OPTIMIZE=$(OPTIMIZE) 

#----------------------------------------------------------------------------
#
#  Create the file dependencies for compiling.
#
#----------------------------------------------------------------------------

depends: preprocess
	@echo "$(RTMEXEDIR) $(RTMDIR)"
	@echo "$(RTMARCH) automatically generating dependencies for compiling"
	@gmake -f $(RTMDIR)/build/depends.mk RTMEXEDIR=$(RTMEXEDIR) OPTIMIZE=$(OPTIMIZE)

#----------------------------------------------------------------------------
#
#  Perform the preprocessing
#
#----------------------------------------------------------------------------

preprocess: dirs
	@echo "$(RTMARCH) preprocess RTM source"
	@gmake -f $(RTMDIR)/build/preprocess.mk

#----------------------------------------------------------------------------
#
#  Create the directory structure if it does not exist.
#
#----------------------------------------------------------------------------

dirs:
	@echo "$(RTMARCH) is creating the required directory structure"
	@cd $(RTMEXEDIR) &&  if [ ! -d compile ]; then mkdir compile; fi
	@cd $(RTMEXEDIR)/compile &&  \
            if [ ! -d Depends ]; then mkdir Depends; fi 
	@cd $(RTMEXEDIR)

#----------------------------------------------------------------------------
#
#  Include utilities makefile with rules for clean, clobber, etc.
#
#----------------------------------------------------------------------------

clean:
ifeq ($(VARSDEFINED),RTMEXEDIRRTMDIRRTMARCH)
	@cd $(RTMEXEDIR) && $(RM) core rtm rtm_db 
	@cd $(RTMEXEDIR) && $(RM) compile/Depends/*.* 
	@cd $(RTMEXEDIR) && $(RM) compile/*.f compile/*.f90 compile/*.c
	@cd $(RTMEXEDIR) && $(RM) compile/*.o compile/*.$(MODSUF) 
else
	@echo "  Please setenv RTMEXEDIR RTMDIR and RTMARCH"
	@echo "  Only $(VARSDEFINED) have been defined."
endif

#----------------------------------------------------------------------------
