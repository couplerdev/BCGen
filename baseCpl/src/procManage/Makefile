
SRC = mpi_comm.F90 deploy_mod.F90 manage.F90 
include ../Makefile.conf
OBJ_ALL = $(SRC: .F90=.o)

OBJ_ALL:
	$(FC) -c -I$(INCLDIR) -L$(LIBDIR) $(SRC)
	cp -f *.o $(LIBDIR)
	cp -f *.mod $(INCLDIR)

