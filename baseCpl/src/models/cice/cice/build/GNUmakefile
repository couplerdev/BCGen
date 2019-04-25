#
# File:  GNUmakefile
#
#----------------------------------------------------------------------------
#
#  This is the main driver makefile for compiling CICE.  It relies on
#  sub-makefiles to perform preprocessing, dependency analysis and compiling.
#
#  Several variables must be set either as environment variables or on the 
#  command line (e.g. gmake CICEDIR=/your/path).  These variables are:
#
#  CICEEXEDIR = the directory where you want the executable to end up.  A
#              common value might be . (the current working directory)
#  CICEDIR    = the path to the CICE distribution from which you want
#              to grab source files, etc.  This should be the path to the 
#              top level cice directory, not the individual source,mpi 
#              directories.
#  CICEARCH   = the base name of a file in the build directory where 
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

CICEDIR=/home/hq/share/BCGen_case/BCGen_inst/models/ice/cice
CICEEXEDIR=/home/hq/share/BCGen_case/BCGen_inst/models/ice/cice/build
CICEARCH=mytest
#
#
#  By default, you build optimized code.  To override, add "OPTIMIZE=no" 
#  to the gmake command line.
OPTIMIZE   = yes
#
#----------------------------------------------------------------------------
#
#  Check that path variables CICEEXEDIR, CICEDIR have been set.
#
#----------------------------------------------------------------------------

VARSDEFINED =

ifneq (,$(CICEEXEDIR))
  VARSDEFINED := $(VARSDEFINED)CICEEXEDIR
  export CICEEXEDIR
else
  bogusexedir:
	@echo "  Please setenv CICEEXEDIR"
endif

ifneq (,$(CICEDIR))
  VARSDEFINED := $(VARSDEFINED)CICEDIR
  export CICEDIR
else
  boguscicedir:
	@echo "  Please setenv CICEDIR"
endif

#  Set the directory where dependency info will reside.
DepDir = $(CICEEXEDIR)/compile/Depends

#----------------------------------------------------------------------------
#
#  Include architecture-specific flags and options. 
#
#----------------------------------------------------------------------------

ifneq (,$(CICEARCH))
  export CICEARCH
  VARSDEFINED := $(VARSDEFINED)CICEARCH
  ifneq (,$(CICEDIR))
     include $(CICEDIR)/build/$(CICEARCH).gnu
  endif
else
  boguscicearch:
	@echo "  Please setenv CICEARCH"
endif

#----------------------------------------------------------------------------
#
#  Define a symbol (TARGETX) for the executable name (cice)
#
#----------------------------------------------------------------------------

TARGETX = cice
ifeq ($(OPTIMIZE),no)
  #  If building a debug version, append "_db" to the executable name
  TARGETX := $(TARGETX)_db
endif

#----------------------------------------------------------------------------
#
#  Make the cice executable
#
#----------------------------------------------------------------------------

.PHONY: depends preprocess compile dirs

$(TARGETX): depends
	@echo "  GNUmakefile is making target '$(TARGETX)'"
	@gmake -f $(CICEDIR)/build/compile.mk CICEEXEDIR=$(CICEEXEDIR) CICEDIR=$(CICEDIR) OPTIMIZE=$(OPTIMIZE) 

#----------------------------------------------------------------------------
#
#  Create the file dependencies for compiling.
#
#----------------------------------------------------------------------------

depends: preprocess
	@echo "$(CICEEXEDIR) $(CICEDIR)"
	@echo "$(CICEARCH) automatically generating dependencies for compiling"
	@gmake -f $(CICEDIR)/build/depends.mk CICEEXEDIR=$(CICEEXEDIR) OPTIMIZE=$(OPTIMIZE)

#----------------------------------------------------------------------------
#
#  Perform the preprocessing
#
#----------------------------------------------------------------------------

preprocess: dirs
	@echo "$(CICEARCH) preprocess CICE source"
	@gmake -f $(CICEDIR)/build/preprocess.mk

#----------------------------------------------------------------------------
#
#  Create the directory structure if it does not exist.
#
#----------------------------------------------------------------------------

dirs:
	@echo "$(CICEARCH) is creating the required directory structure"
	@cd $(CICEEXEDIR) &&  if [ ! -d compile ]; then mkdir compile; fi
	@cd $(CICEEXEDIR)/compile &&  \
            if [ ! -d Depends ]; then mkdir Depends; fi 
	@cd $(CICEEXEDIR)

#----------------------------------------------------------------------------
#
#  Include utilities makefile with rules for clean, clobber, etc.
#
#----------------------------------------------------------------------------

clean:
ifeq ($(VARSDEFINED),CICEEXEDIRCICEDIRCICEARCH)
	@cd $(CICEEXEDIR) && $(RM) core cice cice_db 
	@cd $(CICEEXEDIR) && $(RM) compile/Depends/*.* 
	@cd $(CICEEXEDIR) && $(RM) compile/*.f compile/*.f90 compile/*.c
	@cd $(CICEEXEDIR) && $(RM) compile/*.o compile/*.$(MODSUF) 
else
	@echo "  Please setenv CICEEXEDIR CICEDIR and CICEARCH"
	@echo "  Only $(VARSDEFINED) have been defined."
endif

#----------------------------------------------------------------------------
