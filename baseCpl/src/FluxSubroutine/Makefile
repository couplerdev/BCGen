SRC =  flux_mod.F90

include ../Makefile.conf
OBJS_ALL = $(SRC: %.F90=%.o)

OBJS_ALL:
	$(FC) -c -L$(LIBDIR) -I$(INCLDIR) -I$(ESMF_INCL) -I$(ESMF_MOD) $(ESMF_LINKOPT32) $(SRC)
	cp -f *.o $(LIBDIR)
	cp -f *.mod $(INCLDIR)
