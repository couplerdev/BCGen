SRC = extend.F90  comms_nc.F90 comms.F90
include ../Makefile.conf
OBJ_ALL = $(SRC: .F90=.o)
SRC_test = testComms.F90
test = testComms

OBJ_ALL:
	$(FC) -c  -I$(INCLDIR) -I$(NETCDFINCL) -L$(LIBDIR) -L$(NETCDFLIB) $(SRC)
	mv *.o $(LIBDIR)
	mv *.mod $(INCLDIR)

test:
	$(FC) -o $(test) -I$(INCLDIR) -I$(NETCDFINCL)  -L$(NETCDFLIB) \
	 $(SRC_test) $(OBJ_ALL) $(LIBDIR)/libbcpl.a $(LIBDIR)/libmct.a\
	 $(LIBDIR)/libmpeu.a -lnetcdff

