SRC=base_field.F90
include ../Makefile.conf
OBJ_ALL=$(SRC:.F90=.o)
FLAGS = -ffree-line-length-none


OBJ_ALL:
	$(FC) -c -I$(INCLDIR) -L$(LIBDIR) $(FLAGS) $(SRC)
	mv *.mod $(INCLDIR)
	mv *.o $(LIBDIR)
