
SRC = comp_lnd.F90
include ../Makefile.conf
OBJ_ALL = $(SRC: .F90=.o)

OBJ_ALL : 
	$(FC) -c  -I$(INCLDIR) -L$(LIBDIR) $(SRC) 
	mv *.o $(LIBDIR)
	mv *.mod $(INCLDIR)

.PHONY : clean
clean:
	rm *.mod *.o 

