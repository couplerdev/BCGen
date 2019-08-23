AR = ar
ARFLAG = rcs
SRC = comp_glc.F90
LIB_COMP = libglc.a
include ../Makefile.conf
OBJ_ALL = $(SRC: .F90=.o)

OBJ_ALL : 
	$(FC) -c  -I$(INCLDIR) -I$(ESMF_MOD) -L$(LIBDIR) $(SRC) 
	$(AR) $(ARFLAG) $(LIB_COMP) ./*.o
	cp -f $(LIB_COMP) $(LIBDIR)
	cp -f *.o $(LIBDIR)
	cp -f *.mod $(INCLDIR)

.PHONY : clean
clean:
	rm *.mod *.o 

