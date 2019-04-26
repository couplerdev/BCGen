#
# File:  GNUmakefile
#
#----------------------------------------------------------------------------
#
#  This is the main driver makefile for compiling CLM.  It relies on
#  sub-makefiles to perform preprocessing, dependency analysis and compiling.
#
#  Several variables must be set either as environment variables or on the 
#  command line (e.g. gmake CLMDIR=/your/path).  These variables are:
#
#  CLMEXEDIR = the directory where you want the executable to end up.  A
#              common value might be . (the current working directory)
#  CLMDIR    = the path to the CLM distribution from which you want
#              to grab source files, etc.  This should be the path to the 
#              top level clm directory, not the individual source,mpi 
#              directories.
#  CLMARCH   = the base name of a file in the build directory where 
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

CLMDIR=/home/hq/share/BCGen_case/BCGen_inst/models/lnd/clm
CLMEXEDIR=/home/hq/share/BCGen_case/BCGen_inst/models/lnd/clm/build
CLMARCH=mytest
#
#
#  By default, you build optimized code.  To override, add "OPTIMIZE=no" 
#  to the gmake command line.
OPTIMIZE   = yes
#
#----------------------------------------------------------------------------
#
#  Check that path variables CLMEXEDIR, CLMDIR have been set.
#
#----------------------------------------------------------------------------

VARSDEFINED =

ifneq (,$(CLMEXEDIR))
  VARSDEFINED := $(VARSDEFINED)CLMEXEDIR
  export CLMEXEDIR
else
  bogusexedir:
	@echo "  Please setenv CLMEXEDIR"
endif

ifneq (,$(CLMDIR))
  VARSDEFINED := $(VARSDEFINED)CLMDIR
  export CLMDIR
else
  bogusclmdir:
	@echo "  Please setenv CLMDIR"
endif

#  Set the directory where dependency info will reside.
DepDir = $(CLMEXEDIR)/compile/Depends

#----------------------------------------------------------------------------
#
#  Include architecture-specific flags and options. 
#
#----------------------------------------------------------------------------

ifneq (,$(CLMARCH))
  export CLMARCH
  VARSDEFINED := $(VARSDEFINED)CLMARCH
  ifneq (,$(CLMDIR))
     include $(CLMDIR)/build/$(CLMARCH).gnu
  endif
else
  bogusclmarch:
	@echo "  Please setenv CLMARCH"
endif

#----------------------------------------------------------------------------
#
#  Define a symbol (TARGETX) for the executable name (clm)
#
#----------------------------------------------------------------------------

TARGETX = clm
ifeq ($(OPTIMIZE),no)
  #  If building a debug version, append "_db" to the executable name
  TARGETX := $(TARGETX)_db
endif

#----------------------------------------------------------------------------
#
#  Make the clm executable
#
#----------------------------------------------------------------------------

.PHONY: depends preprocess compile dirs

$(TARGETX): depends
	@echo "  GNUmakefile is making target '$(TARGETX)'"
	@gmake -f $(CLMDIR)/build/compile.mk CLMEXEDIR=$(CLMEXEDIR) CLMDIR=$(CLMDIR) OPTIMIZE=$(OPTIMIZE) 

#----------------------------------------------------------------------------
#
#  Create the file dependencies for compiling.
#
#----------------------------------------------------------------------------

depends: preprocess
	@echo "$(CLMEXEDIR) $(CLMDIR)"
	@echo "$(CLMARCH) automatically generating dependencies for compiling"
	@gmake -f $(CLMDIR)/build/depends.mk CLMEXEDIR=$(CLMEXEDIR) OPTIMIZE=$(OPTIMIZE)

#----------------------------------------------------------------------------
#
#  Perform the preprocessing
#
#----------------------------------------------------------------------------

preprocess: dirs
	@echo "$(CLMARCH) preprocess CLM source"
	@gmake -f $(CLMDIR)/build/preprocess.mk

#----------------------------------------------------------------------------
#
#  Create the directory structure if it does not exist.
#
#----------------------------------------------------------------------------

dirs:
	@echo "$(CLMARCH) is creating the required directory structure"
	@cd $(CLMEXEDIR) &&  if [ ! -d compile ]; then mkdir compile; fi
	@cd $(CLMEXEDIR)/compile &&  \
            if [ ! -d Depends ]; then mkdir Depends; fi 
	@cd $(CLMEXEDIR)

#----------------------------------------------------------------------------
#
#  Include utilities makefile with rules for clean, clobber, etc.
#
#----------------------------------------------------------------------------

clean:
ifeq ($(VARSDEFINED),CLMEXEDIRCLMDIRCLMARCH)
	@cd $(CLMEXEDIR) && $(RM) core clm clm_db 
	@cd $(CLMEXEDIR) && $(RM) compile/Depends/*.* 
	@cd $(CLMEXEDIR) && $(RM) compile/*.f compile/*.f90 compile/*.c
	@cd $(CLMEXEDIR) && $(RM) compile/*.o compile/*.$(MODSUF) 
else
	@echo "  Please setenv CLMEXEDIR CLMDIR and CLMARCH"
	@echo "  Only $(VARSDEFINED) have been defined."
endif

#----------------------------------------------------------------------------
