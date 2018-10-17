SRC = baseCpl.F90 main.F90
include ../Makefile.conf
OBJ_ALL = $(SRC: .F90=.o)
COMP_DEPS = $(LIBDIR)/libatm.a $(LIBDIR)/libglc.a $(LIBDIR)/libice.a $(LIBDIR)/liblnd.a $(LIBDIR)/libocn.a $(LIBDIR)/librof.a $(LIBDIR)/libwav.a 
BASE_DEPS = $(LIBDIR)/libmct.a $(LIBDIR)/libbcpl.a
EXE = main

EXE:
	$(FC) -c -I$(INCLDIR) -L$(LIBDIR) baseCpl.F90
	$(FC) -o $(EXE) $(OBJ_ALL) -I$(INCLDIR) -L$(LIBDIR) -L$(COMP_DEPS) -L$(BASE_DEPS)
	mv $(EXE) $(BINDIR)

OBJ_ALL:
	$(FC) -c -I$(INCLDIR) -L$(LIBDIR) $(SRC)
