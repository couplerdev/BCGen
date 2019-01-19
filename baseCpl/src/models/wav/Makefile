AR = ar
ARFLAG = rcs
SRC = comp_wav.F90
LIB_COMP = libwav.a
include ../Makefile.conf
OBJ_ALL = $(SRC: .F90=.o)

OBJ_ALL : 
	$(FC) -c  -I$(INCLDIR) -L$(LIBDIR) $(SRC) 
	$(AR) $(ARFLAG) $(LIB_COMP) ./*.o
	mv $(LIB_COMP) $(LIBDIR)
	mv *.o $(LIBDIR)
	mv *.mod $(INCLDIR)

.PHONY : clean
clean:
	rm *.mod *.o 

