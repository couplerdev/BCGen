SRC = base_sys.F90
include ../Makefile.conf
OBJ_ALL =  $(SRC: .F90=.o)
LIB_SYS = libsys.a

OBJ_ALL:
	$(FC) -c -I$(INCLDIR) -L$(LIBDIR) $(SRC)
	mv *.o $(LIBDIR)
	mv *.mod $(INCLDIR)

