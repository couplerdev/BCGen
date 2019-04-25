ifneq ($(SPMD), TRUE)
cpp_dirs += ../../MCT/mpi-serial
endif
cpp_dirs += ../../depUtils/ParallelIO

include Makefile.conf

ifneq ($(ESMF_BLD),$(null))
cpp_dirs += $(ESMF_LIB)
endif

.PHONY: debug clean all install

SRCS_F90 = shr/shr_kind_mod.F90 \
	   shr/shr_log_mod.F90 \
	   shr/shr_mpi_mod.F90 \
	   shr/shr_sys_mod.F90 \
  	   shr/shr_const_mod.F90 \
	   shr/shr_timer_mod.F90 \
   	   shr/mct_mod.F90 \
	   shr/shr_string_mod.F90 \
	   shr/shr_cal_mod.F90 \
 	   shr/shr_map_mod.F90 \
	   shr/shr_file_mod.F90 \
	   shr/shr_mem_mod.F90 \
           shr/shr_msg_mod.F90 \
	   shr/shr_spfn_mod.F90 \
           shr/shr_orb_mod.F90 \
           shr/shr_nl_mod.F90 \
    	   shr/shr_flux_mod.F90	\
	   shr/shr_pio_mod.F90 \
	   shr/shr_reprosum_mod.F90 \
           shr/shr_tInterp_mod.F90 \
 	   shr/shr_stream_mod.F90 \
           shr/shr_pcdf_mod.F90 \
	   shr/shr_mct_mod.F90 \
  	   shr/shr_nl_mod.F90 \
	   shr/shr_ncread_mod.F90 \
  	   shr/shr_stream_mod.F90 \
	   shr/shr_dmodel_mod.F90 \
	   shr/shr_strdata_mod.F90 \
	   shr/shr_flux_mod.F90 \
 	   shr/shr_vmath_mod.F90 \
	   shr/shr_isnan.h \
	   shr/shr_test_infnan_mod.F90 \
	   shr/shr_infnan_mod.F90 \
           shr/shr_scam_mod.F90 \
           shr/shr_assert_mod.F90 

OBJS_ALL = $(SRCS_F90:.F90=.o)


OBJS_ALL: $(SRCS_F90)
	$(CC) -c shr/shr_isnan.c -I./shr -D FORTRANUNDERSCORE
	$(CC) -c shr/shr_reprosumx86.c -D x86 -D FORTRANUNDERSCORE
	$(FC) -c $(FLAGS) $(CCFLAGS) $^
	make install

install:
	$(MV) *.mod $(INCLDIR)
	$(CP) ./shr/dtypes.h $(INCLDIR)
	$(MV) *.o $(LIBDIR)

	
clean:
	$(RM) *.o *.mod

mct_mod .o:  shr_kind_mod.o shr_sys_mod.o shr_mpi_mod.o shr_const_mod.o shr_log_mod.o
shr_kind_mod.o:
shr_log_mod.o:
shr_mpi_mod.o: shr_kind_mod.o shr_log_mod.o
shr_sys_mod.o: shr_kind_mod.o shr_mpi_mod.o shr_log_mod.o
shr_const_mod.o: shr_kind_mod.o
shr_file_mod.o: shr_kind_mod.o shr_sys_mod.o shr_log_mod.o
shr_timer_mod.o: shr_kind_mod.o shr_log_mod.o
shr_string_mod.o: shr_kind_mod.o shr_sys_mod.o shr_timer_mod.o shr_log_mod.o
shr_cal_mod.o: shr_kind_mod.o shr_const_mod.o shr_sys_mod.o shr_string_mod.o shr_log_mod.o
shr_map_mod.o: shr_const_mod.o shr_kind_mod.o shr_sys_mod.o shr_timer_mod.o shr_log_mod.o
shr_mct_mod.o: shr_kind_mod.o shr_sys_mod.o shr_mpi_mod.o shr_const_mod.o mct_mod.o \
               shr_log_mod.o use shr_pcdf_mod
shr_mem_mod.o: shr_kind_mod.o shr_log_mod.o
shr_msg_mod.o: shr_file_mod.o shr_log_mod.o shr_sys_mod
shr_ncread_mod.o: shr_string_mod.o shr_kind_mod.o shr_sys_mod.o shr_file_mod.o shr_log_mod.o 
shr_nl_mod.o: shr_string_mod.o
shr_orb_mod.o: shr_kind_mod.o shr_sys_mod.o shr_const_mod.o shr_log_mod.o
shr_pcdf_mod.o: shr_kind_mod.o shr_sys_mod.o shr_const_mod.o shr_log_mod.o mct_mod.o
shr_pio_mod.o: shr_kind_mod.o shr_file_mod.o shr_log_mod.o shr_mpi_mod.o shr_sys_mod.o \
             shr_string_mod.o 
shr_reprosum_mod.o: shr_kind_mod.o shr_log_mod.o shr_sys_mod.o shr_reprosumx86.o 
shr_spfn_mod.o: shr_kind_mod.o shr_const_mod.o
shr_scam_mod.o: shr_kind_mod.o shr_sys_mod.o shr_file_mod.o shr_log_mod.o shr_ncread_mod.o \
                shr_dmodel_mod.o mct_mod.o shr_strdata_mod.o 
shr_stream_mod.o: shr_kind_mod.o shr_sys_mod.o shr_const_mod.o shr_string_mod.o shr_mpi_mod.o \
                  shr_file_mod.o shr_cal_mod.o shr_log_mod.o
shr_infnan_mod.o: shr_test_infnan_mod.o
shr_test_infnan_mod.o:
shr_assert_mod.o: shr_infnan_mod.o shr_kind_mod.o shr_sys_mod.o shr_log_mod.o
shr_tInterp_mod.o: shr_sys_mod.o shr_cal_mod.o shr_kind_mod.o shr_const_mod.o shr_log_mod.o \
                   shr_orb_mod.o
shr_flux_mod.o: shr_kind_mod.o shr_const_mod.o shr_sys_mod.o shr_log_mod.o

shr_dmodel_mod.o: shr_sys_mod.o shr_log_mod.o shr_kind_mod.o shr_mpi_mod.o shr_file_mod.o \
        	  shr_string_mod.o shr_ncread_mod.o shr_const_mod.o shr_stream_mod.o      \
                  shr_map_mod.o
shr_strdata_mod.o: shr_const_mod.o shr_kind_mod.o shr_sys_mod.o shr_mpi_mod.o shr_file_mod.o \
                  shr_log_mod.o shr_stream_mod.o shr_string_mod.o shr_map_mod.o shr_cal_mod.o \
                  shr_orb_mod.o shr_tinterp_mod.o shr_dmodel_mod.o shr_mct_mod.o mct_mod.o
shr_vmath_mod.o: shr_kind_mod.o shr_log_mod.o 
