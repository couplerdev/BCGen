SRC = mrg_mod.F90  fraction_mod.F90
include ../Makefile.conf
OBJ_ALL = $(SRC: .F90=.o)


OBJ_ALL:
	$(FC) -c -L$(LIBDIR) -L$(MCTLIB) -I$(MCTINCL) -I$(INCLDIR) $(SRC)
	mv *.mod $(INCLDIR)
	mv *.o $(LIBDIR)
