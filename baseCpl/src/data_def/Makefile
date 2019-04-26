SRC =  type_def.F90 comms_def.F90 time_def.F90 proc_def.F90 field_def.F90 global_var.F90

include ../Makefile.conf
OBJS_ALL = $(SRC: %.F90=%.o)

OBJS_ALL:
	$(FC) -c -L$(LIBDIR) -I$(INCLDIR) -I$(ESMF_INCL) -I$(ESMF_MOD) $(ESMF_LINKOPT32) $(SRC)
	mv *.o $(LIBDIR)
	mv *.mod $(INCLDIR)
