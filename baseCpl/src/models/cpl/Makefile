SRC=baseCpl.F90
include ../Makefile.conf
OBJS_ALL = ./*.o
BASE_DEPS = $(LIBDIR)/*.o
ESMF_DEPS=$(ESMF_LIB)/libesmf.a
MCTDEPS=-L/usr/local/mct/lib -lmct -lmpeu
DEPS=-lmct -lnetcdf -lnetcdff -lmpeu -lpio -lesmf -lpio -lgptl -lpnetcdf
ESMF_DEPS = -lesmf -lrt -lstdc++ -ldl
FLAGS=-pthread
CPPDEFS = -ffree-line-length-none
EXTERNAL_LDFLAGS = -L/share/packages/lapack-3.7.1 -llapack -lblas
EXE= main
MAIN=main.F90

EXE:
	$(FC) -c -I$(INCLDIR) -I$(ESMF_MOD) -L$(LIBDIR) $(CPPDEFS) baseCpl.F90
	echo 'finish mod'
	$(FC) -o $(EXE) $(MAIN) $(OBJS_ALL) $(BASE_DEPS) $(FLAGS) $(CPPDEFS) -I$(INCLDIR) -L$(LIBDIR) \
	-L$(LIBDIR) -latm -lclm $(EXTERNAL_LDFLAGS) $(DEPS) $(MCTDEPS) $(ESMF_DEPS)
	#mv $(EXE) $(BINDIR)
