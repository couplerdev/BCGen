# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.9


tests/general/CMakeFiles/ncdf_fail.dir/ncdf_fail.F90.o.requires: tests/general/CMakeFiles/ncdf_fail.dir/pio_tutil.mod.proxy
tests/general/CMakeFiles/ncdf_fail.dir/ncdf_fail.F90.o: tests/general/CMakeFiles/ncdf_fail.dir/pio_tutil.mod.stamp
tests/general/CMakeFiles/ncdf_fail.dir/ncdf_fail_tgv.mod.proxy: tests/general/CMakeFiles/ncdf_fail.dir/ncdf_fail.F90.o.provides
tests/general/CMakeFiles/ncdf_fail.dir/ncdf_fail.F90.o.provides.build:
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod tests/general/ncdf_fail_tgv tests/general/CMakeFiles/ncdf_fail.dir/ncdf_fail_tgv.mod.stamp GNU
	$(CMAKE_COMMAND) -E touch tests/general/CMakeFiles/ncdf_fail.dir/ncdf_fail.F90.o.provides.build
tests/general/CMakeFiles/ncdf_fail.dir/build: tests/general/CMakeFiles/ncdf_fail.dir/ncdf_fail.F90.o.provides.build

tests/general/CMakeFiles/ncdf_fail.dir/util/pio_tutil.F90.o: /usr/local/include/mpi.mod
tests/general/CMakeFiles/ncdf_fail.dir/util/pio_tutil.F90.o: src/gptl/CMakeFiles/gptl.dir/perf_mod.mod.stamp
tests/general/CMakeFiles/ncdf_fail.dir/util/pio_tutil.F90.o: src/flib/CMakeFiles/piof.dir/pio.mod.stamp
tests/general/CMakeFiles/ncdf_fail.dir/pio_tutil.mod.proxy: tests/general/CMakeFiles/ncdf_fail.dir/util/pio_tutil.F90.o.provides
tests/general/CMakeFiles/ncdf_fail.dir/util/pio_tutil.F90.o.provides.build:
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod tests/general/pio_tutil tests/general/CMakeFiles/ncdf_fail.dir/pio_tutil.mod.stamp GNU
	$(CMAKE_COMMAND) -E touch tests/general/CMakeFiles/ncdf_fail.dir/util/pio_tutil.F90.o.provides.build
tests/general/CMakeFiles/ncdf_fail.dir/build: tests/general/CMakeFiles/ncdf_fail.dir/util/pio_tutil.F90.o.provides.build
