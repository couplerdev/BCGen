# CMake generated Testfile for 
# Source directory: /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/examples/c
# Build directory: /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/examples/c
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(examplePio "/usr/local/bin/mpiexec" "-n" "4" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/examples/c/examplePio")
set_tests_properties(examplePio PROPERTIES  TIMEOUT "60")
add_test(example1 "/usr/local/bin/mpiexec" "-n" "4" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/examples/c/example1")
set_tests_properties(example1 PROPERTIES  TIMEOUT "60")
add_test(darray_no_async "/usr/local/bin/mpiexec" "-n" "4" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/examples/c/darray_no_async")
set_tests_properties(darray_no_async PROPERTIES  TIMEOUT "60")
