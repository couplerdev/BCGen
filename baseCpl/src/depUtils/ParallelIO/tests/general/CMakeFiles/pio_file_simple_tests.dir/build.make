# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.9

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/local/bin/cmake

# The command to remove a file.
RM = /usr/local/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO

# Include any dependencies generated for this target.
include tests/general/CMakeFiles/pio_file_simple_tests.dir/depend.make

# Include the progress variables for this target.
include tests/general/CMakeFiles/pio_file_simple_tests.dir/progress.make

# Include the compile flags for this target's objects.
include tests/general/CMakeFiles/pio_file_simple_tests.dir/flags.make

tests/general/pio_file_simple_tests.F90: tests/general/pio_file_simple_tests.F90.in
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating pio_file_simple_tests.F90"
	cd /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general && util/pio_tf_f90gen.pl --annotate-source --out=/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general/pio_file_simple_tests.F90 /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general/pio_file_simple_tests.F90.in

tests/general/CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.o: tests/general/CMakeFiles/pio_file_simple_tests.dir/flags.make
tests/general/CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.o: tests/general/pio_file_simple_tests.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building Fortran object tests/general/CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.o"
	cd /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general && /usr/local/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general/pio_file_simple_tests.F90 -o CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.o

tests/general/CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.i"
	cd /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general && /usr/local/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general/pio_file_simple_tests.F90 > CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.i

tests/general/CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.s"
	cd /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general && /usr/local/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general/pio_file_simple_tests.F90 -o CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.s

tests/general/CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.o.requires:

.PHONY : tests/general/CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.o.requires

tests/general/CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.o.provides: tests/general/CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.o.requires
	$(MAKE) -f tests/general/CMakeFiles/pio_file_simple_tests.dir/build.make tests/general/CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.o.provides.build
.PHONY : tests/general/CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.o.provides

tests/general/CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.o.provides.build: tests/general/CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.o


tests/general/CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.o: tests/general/CMakeFiles/pio_file_simple_tests.dir/flags.make
tests/general/CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.o: tests/general/util/pio_tutil.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building Fortran object tests/general/CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.o"
	cd /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general && /usr/local/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general/util/pio_tutil.F90 -o CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.o

tests/general/CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.i"
	cd /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general && /usr/local/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general/util/pio_tutil.F90 > CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.i

tests/general/CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.s"
	cd /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general && /usr/local/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general/util/pio_tutil.F90 -o CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.s

tests/general/CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.o.requires:

.PHONY : tests/general/CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.o.requires

tests/general/CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.o.provides: tests/general/CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.o.requires
	$(MAKE) -f tests/general/CMakeFiles/pio_file_simple_tests.dir/build.make tests/general/CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.o.provides.build
.PHONY : tests/general/CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.o.provides

tests/general/CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.o.provides.build: tests/general/CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.o


# Object files for target pio_file_simple_tests
pio_file_simple_tests_OBJECTS = \
"CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.o" \
"CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.o"

# External object files for target pio_file_simple_tests
pio_file_simple_tests_EXTERNAL_OBJECTS =

tests/general/pio_file_simple_tests: tests/general/CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.o
tests/general/pio_file_simple_tests: tests/general/CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.o
tests/general/pio_file_simple_tests: tests/general/CMakeFiles/pio_file_simple_tests.dir/build.make
tests/general/pio_file_simple_tests: src/flib/libpiof.a
tests/general/pio_file_simple_tests: src/clib/libpioc.a
tests/general/pio_file_simple_tests: /usr/local/lib/libnetcdf.so
tests/general/pio_file_simple_tests: src/gptl/libgptl.a
tests/general/pio_file_simple_tests: /usr/local/lib/libnetcdff.so
tests/general/pio_file_simple_tests: /usr/local/Pnetcdf/lib/libpnetcdf.a
tests/general/pio_file_simple_tests: tests/general/CMakeFiles/pio_file_simple_tests.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Linking Fortran executable pio_file_simple_tests"
	cd /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/pio_file_simple_tests.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
tests/general/CMakeFiles/pio_file_simple_tests.dir/build: tests/general/pio_file_simple_tests

.PHONY : tests/general/CMakeFiles/pio_file_simple_tests.dir/build

tests/general/CMakeFiles/pio_file_simple_tests.dir/requires: tests/general/CMakeFiles/pio_file_simple_tests.dir/pio_file_simple_tests.F90.o.requires
tests/general/CMakeFiles/pio_file_simple_tests.dir/requires: tests/general/CMakeFiles/pio_file_simple_tests.dir/util/pio_tutil.F90.o.requires

.PHONY : tests/general/CMakeFiles/pio_file_simple_tests.dir/requires

tests/general/CMakeFiles/pio_file_simple_tests.dir/clean:
	cd /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general && $(CMAKE_COMMAND) -P CMakeFiles/pio_file_simple_tests.dir/cmake_clean.cmake
.PHONY : tests/general/CMakeFiles/pio_file_simple_tests.dir/clean

tests/general/CMakeFiles/pio_file_simple_tests.dir/depend: tests/general/pio_file_simple_tests.F90
	cd /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/general/CMakeFiles/pio_file_simple_tests.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : tests/general/CMakeFiles/pio_file_simple_tests.dir/depend

