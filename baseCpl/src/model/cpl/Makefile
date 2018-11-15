SRC = baseCpl.F90 
include ../Makefile.conf
OBJS_ALL = ./*.o
COMP_DEPS = $(LIBDIR)/libatm.a $(LIBDIR)/libocn.a 
BASE_DEPS =  $(LIBDIR)/*.o
DEPS= -lmct -lnetcdf -lnetcdff -lmpeu  -lesmf -lpiof
FLAGS= -pthread -lrt
EXE = main
MAIN = main.F90
EXE:
	$(FC) -c -I$(INCLDIR) -I$(ESMF_MOD) -L$(LIBDIR) baseCpl.F90	
	echo 'finish mod'
	$(FC) -o $(EXE) $(MAIN) $(OBJS_ALL) $(BASE_DEPS) $(FLAGS) -I$(INCLDIR)  -L$(LIBDIR) -L$(COMP_DEPS) $(DEPS)
	mv $(EXE) $(BINDIR)

OBJ_ALL:
	$(FC) -c -I$(INCLDIR) -L$(LIBDIR) $(SRC)
