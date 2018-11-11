# CMake generated Testfile for 
# Source directory: /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/unit
# Build directory: /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/unit
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(pio_unit_test "/usr/local/bin/mpiexec" "-n" "4" "/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/unit/pio_unit_test")
set_tests_properties(pio_unit_test PROPERTIES  TIMEOUT "60")
