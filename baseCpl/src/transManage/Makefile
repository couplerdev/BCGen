SRC = extend.F90  comms_nc.F90 comms.F90
include ../Makefile.conf
OBJ_ALL = $(SRC: .F90=.o)
SRC_test = testComms.F90
test = testComms

OBJ_ALL:
	$(FC) -c  -I$(INCLDIR) -I$(NETCDFINCL) -L$(LIBDIR) -L$(NETCDFLIB) -I$(PIO_INCL) -L$(PIO_LIB) $(SRC)
	cp -f *.o $(LIBDIR)
	cp -f *.mod $(INCLDIR)

test:
	$(FC) -o $(test) -I$(INCLDIR) -I$(NETCDFINCL) -L$(NETCDFLIB) -I$(PIO_INCL) -L$(PIO_LIB) \
	 $(SRC_test) $(OBJ_ALL) $(LIBDIR)/libbcpl.a $(LIBDIR)/libmct.a\
	 $(LIBDIR)/libmpeu.a -lnetcdff

