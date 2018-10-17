AR = ar
ARFLAG = rcs
SRC = comp_ice.F90
LIB_COMP = libice.a
include ../Makefile.conf
OBJ_ALL = $(SRC: .F90=.o)

OBJ_ALL : 
	$(FC) -c  -I$(INCLDIR) -L$(LIBDIR)/libbcpl.a $(SRC) 
	$(AR) $(ARFLAG) $(LIB_COMP) ./*.o
	mv $(LIB_COMP) $(LIBDIR)
	mv *.o $(LIBDIR)
	mv *.mod $(INCLDIR)

.PHONY : clean
clean:
	rm *.mod *.o 

