SRC = logUtil.F90 
include ../Makefile.conf
OBJ_ALL = $(SRC: .F90=.o)

OBJ_ALL:
	$(FC) -c -I$(INCLDIR) -L$(LIBDIR) $(SRC)
	cp -f *.o $(LIBDIR)
	cp -f *.mod $(INCLDIR)
