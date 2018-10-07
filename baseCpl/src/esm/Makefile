SRC=base_fields.F90
include ../Makefile.conf
OBJ_ALL=$(SRC:.F90=.o)

OBJ_ALL:
	$(FC) -c -I$(INCLDIR) -L$(LIBDIR) $(SRC)
	mv *.mod $(INCLDIR)
	mv *.o $(LIBDIR)
