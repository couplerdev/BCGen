SRC = test.F90
include ../Makefile.conf
OBJ_ALL = $(SRC: .F90=.o)
EXE = test

EXE:
	$(FC) -o $(EXE) $(SRC) -I$(INCLDIR) -L$(LIBDIR)/libbcpl.a 

OBJ_ALL:
	$(FC) -c $(SRC) -I$(INCLDIR) -L$(LIBDIR)

