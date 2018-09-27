SRC = extend.F90 comms.F90 
include ../Makefile.conf
OBJ_ALL = $(SRC: .F90=.o)

OBJ_ALL:
	$(FC) -c  -I$(INCLDIR) -I$(NETCDFINCL) -L$(LIBDIR) -L$(NETCDFLIB) $(SRC)
	mv *.o $(LIBDIR)
	mv *.mod $(INCLDIR)

