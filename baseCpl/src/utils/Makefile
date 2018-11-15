SRC=base_mpi.F90 base_io.F90
include ../Makefile.conf
OBJ_ALL=$(SRC: .F90=.o)

OBJ_ALL:
	$(FC) -c -I$(INCLDIR) -I$(ESMF_MOD) -I$(PIO_INCL) $(SRC)
	mv *.o $(LIBDIR)
	mv *.mod $(INCLDIR)
