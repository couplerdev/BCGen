# CMake generated Testfile for 
# Source directory: /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit
# Build directory: /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(test_async_mpi "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_async_mpi")
set_tests_properties(test_async_mpi PROPERTIES  TIMEOUT "240")
add_test(test_spmd "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_spmd")
set_tests_properties(test_spmd PROPERTIES  TIMEOUT "240")
add_test(test_rearr "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_rearr")
set_tests_properties(test_rearr PROPERTIES  TIMEOUT "240")
add_test(test_intercomm2 "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_intercomm2")
set_tests_properties(test_intercomm2 PROPERTIES  TIMEOUT "240")
add_test(test_async_simple "/usr/local/bin/mpiexec" "-n" "3" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_async_simple")
set_tests_properties(test_async_simple PROPERTIES  TIMEOUT "240")
add_test(test_async_3proc "/usr/local/bin/mpiexec" "-n" "4" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_async_3proc")
set_tests_properties(test_async_3proc PROPERTIES  TIMEOUT "240")
add_test(test_async_4proc "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_async_4proc")
set_tests_properties(test_async_4proc PROPERTIES  TIMEOUT "240")
add_test(test_iosystem2_simple "/usr/local/bin/mpiexec" "-n" "3" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_iosystem2_simple")
set_tests_properties(test_iosystem2_simple PROPERTIES  TIMEOUT "240")
add_test(test_iosystem2_simple2 "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_iosystem2_simple2")
set_tests_properties(test_iosystem2_simple2 PROPERTIES  TIMEOUT "240")
add_test(test_iosystem2 "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_iosystem2")
set_tests_properties(test_iosystem2 PROPERTIES  TIMEOUT "240")
add_test(test_iosystem3_simple "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_iosystem3_simple")
set_tests_properties(test_iosystem3_simple PROPERTIES  TIMEOUT "240")
add_test(test_iosystem3_simple2 "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_iosystem3_simple2")
set_tests_properties(test_iosystem3_simple2 PROPERTIES  TIMEOUT "240")
add_test(test_iosystem3 "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_iosystem3")
set_tests_properties(test_iosystem3 PROPERTIES  TIMEOUT "240")
add_test(test_pioc "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_pioc")
set_tests_properties(test_pioc PROPERTIES  TIMEOUT "240")
add_test(test_pioc_unlim "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_pioc_unlim")
set_tests_properties(test_pioc_unlim PROPERTIES  TIMEOUT "240")
add_test(test_pioc_putget "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_pioc_putget")
set_tests_properties(test_pioc_putget PROPERTIES  TIMEOUT "240")
add_test(test_pioc_fill "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_pioc_fill")
set_tests_properties(test_pioc_fill PROPERTIES  TIMEOUT "240")
add_test(test_darray "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_darray")
set_tests_properties(test_darray PROPERTIES  TIMEOUT "240")
add_test(test_darray_frame "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_darray_frame")
set_tests_properties(test_darray_frame PROPERTIES  TIMEOUT "240")
add_test(test_darray_multi "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_darray_multi")
set_tests_properties(test_darray_multi PROPERTIES  TIMEOUT "240")
add_test(test_darray_multivar "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_darray_multivar")
set_tests_properties(test_darray_multivar PROPERTIES  TIMEOUT "240")
add_test(test_darray_multivar2 "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_darray_multivar2")
set_tests_properties(test_darray_multivar2 PROPERTIES  TIMEOUT "240")
add_test(test_darray_multivar3 "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_darray_multivar3")
set_tests_properties(test_darray_multivar3 PROPERTIES  TIMEOUT "240")
add_test(test_darray_1d "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_darray_1d")
set_tests_properties(test_darray_1d PROPERTIES  TIMEOUT "240")
add_test(test_darray_3d "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_darray_3d")
set_tests_properties(test_darray_3d PROPERTIES  TIMEOUT "240")
add_test(test_darray_fill "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_darray_fill")
set_tests_properties(test_darray_fill PROPERTIES  TIMEOUT "240")
add_test(test_decomp_uneven "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_decomp_uneven")
set_tests_properties(test_decomp_uneven PROPERTIES  TIMEOUT "240")
add_test(test_decomps "/usr/local/bin/mpiexec" "-n" "5" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/cunit/test_decomps")
set_tests_properties(test_decomps PROPERTIES  TIMEOUT "240")
