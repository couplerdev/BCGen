
SRC = timeM.F90
include ../Makefile.conf
OBJ_ALL = $(SRC: .F90=.o)

OBJ_ALL:
	$(FC) -c -L$(LIBDIR) -I$(INCLDIR) -I$(ESMF_MOD) $(SRC)
	mv *.o $(LIBDIR)
	mv *.mod $(INCLDIR)
