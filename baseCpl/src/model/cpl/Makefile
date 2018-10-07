SRC = baseCpl.F90 main.F90
include ../Makefile.conf
OBJ_ALL = $(SRC: .F90=.o)
EXE = main

EXE:
	$(FC) -o $(EXE) $(OBJ_ALL) -I$(INCLDIR) -L$(LIBDIR) 
	mv $(EXE) $(BINDIR)
	rm *.o
	rm *.mod

OBJ_ALL:
	$(FC) -c -L$(INCLDIR) -I$(LIBDIR) $(SRC)
