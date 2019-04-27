# CMake generated Testfile for 
# Source directory: /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/tests/unit
# Build directory: /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/tests/unit
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(pio_unit_test "/home/hq/bin/mpiexec" "-n" "4" "/home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/tests/unit/pio_unit_test")
set_tests_properties(pio_unit_test PROPERTIES  TIMEOUT "60")
