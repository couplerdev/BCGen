SRC=base_field.F90
include ../Makefile.conf
OBJ_ALL=$(SRC:.F90=.o)
FFLAGS = -ffree-line-length-none

OBJ_ALL:
	$(FC) $(FFLAGS) -c -I$(INCLDIR) -L$(LIBDIR) $(SRC)
	mv *.mod $(INCLDIR)
	mv *.o $(LIBDIR)
