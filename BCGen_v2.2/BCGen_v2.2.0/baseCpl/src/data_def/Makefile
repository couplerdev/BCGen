SRC = comms_def.F90 proc_def.F90
include ../Makefile.conf
OBJS_ALL = $(SRC: .F90=.o)


OBJS_ALL:
	$(FC) -c -L$(LIBDIR) -I$(INCLDIR) $(SRC)
