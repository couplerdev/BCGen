SRC=examplePio.f90
include ../../Makefile.conf
EXE = main

EXE:
	$(FC) -c $(SRC) -I$(PIO_INCL) -I/usr/local/netcdf/include -I/usr/local/pnetcdf/include \
	-I/usr/local/netcdf-fortran/include
	$(FC) -o $(EXE) *.o $(PIO_FLAG)  $(PIO_DEPS) 

