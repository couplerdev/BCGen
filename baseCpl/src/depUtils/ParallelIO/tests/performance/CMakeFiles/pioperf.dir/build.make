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
include tests/performance/CMakeFiles/pioperf.dir/depend.make

# Include the progress variables for this target.
include tests/performance/CMakeFiles/pioperf.dir/progress.make

# Include the compile flags for this target's objects.
include tests/performance/CMakeFiles/pioperf.dir/flags.make

tests/performance/CMakeFiles/pioperf.dir/pioperformance.F90.o: tests/performance/CMakeFiles/pioperf.dir/flags.make
tests/performance/CMakeFiles/pioperf.dir/pioperformance.F90.o: tests/performance/pioperformance.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object tests/performance/CMakeFiles/pioperf.dir/pioperformance.F90.o"
	cd /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/performance && /usr/local/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/performance/pioperformance.F90 -o CMakeFiles/pioperf.dir/pioperformance.F90.o

tests/performance/CMakeFiles/pioperf.dir/pioperformance.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/pioperf.dir/pioperformance.F90.i"
	cd /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/performance && /usr/local/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/performance/pioperformance.F90 > CMakeFiles/pioperf.dir/pioperformance.F90.i

tests/performance/CMakeFiles/pioperf.dir/pioperformance.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/pioperf.dir/pioperformance.F90.s"
	cd /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/performance && /usr/local/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/performance/pioperformance.F90 -o CMakeFiles/pioperf.dir/pioperformance.F90.s

tests/performance/CMakeFiles/pioperf.dir/pioperformance.F90.o.requires:

.PHONY : tests/performance/CMakeFiles/pioperf.dir/pioperformance.F90.o.requires

tests/performance/CMakeFiles/pioperf.dir/pioperformance.F90.o.provides: tests/performance/CMakeFiles/pioperf.dir/pioperformance.F90.o.requires
	$(MAKE) -f tests/performance/CMakeFiles/pioperf.dir/build.make tests/performance/CMakeFiles/pioperf.dir/pioperformance.F90.o.provides.build
.PHONY : tests/performance/CMakeFiles/pioperf.dir/pioperformance.F90.o.provides

tests/performance/CMakeFiles/pioperf.dir/pioperformance.F90.o.provides.build: tests/performance/CMakeFiles/pioperf.dir/pioperformance.F90.o


# Object files for target pioperf
pioperf_OBJECTS = \
"CMakeFiles/pioperf.dir/pioperformance.F90.o"

# External object files for target pioperf
pioperf_EXTERNAL_OBJECTS =

tests/performance/pioperf: tests/performance/CMakeFiles/pioperf.dir/pioperformance.F90.o
tests/performance/pioperf: tests/performance/CMakeFiles/pioperf.dir/build.make
tests/performance/pioperf: src/flib/libpiof.a
tests/performance/pioperf: src/clib/libpioc.a
tests/performance/pioperf: /usr/local/lib/libnetcdf.so
tests/performance/pioperf: src/gptl/libgptl.a
tests/performance/pioperf: /usr/local/lib/libnetcdff.so
tests/performance/pioperf: /usr/local/Pnetcdf/lib/libpnetcdf.a
tests/performance/pioperf: tests/performance/CMakeFiles/pioperf.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking Fortran executable pioperf"
	cd /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/performance && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/pioperf.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
tests/performance/CMakeFiles/pioperf.dir/build: tests/performance/pioperf

.PHONY : tests/performance/CMakeFiles/pioperf.dir/build

tests/performance/CMakeFiles/pioperf.dir/requires: tests/performance/CMakeFiles/pioperf.dir/pioperformance.F90.o.requires

.PHONY : tests/performance/CMakeFiles/pioperf.dir/requires

tests/performance/CMakeFiles/pioperf.dir/clean:
	cd /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/performance && $(CMAKE_COMMAND) -P CMakeFiles/pioperf.dir/cmake_clean.cmake
.PHONY : tests/performance/CMakeFiles/pioperf.dir/clean

tests/performance/CMakeFiles/pioperf.dir/depend:
	cd /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/performance /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/performance /share/BCGen/develDir/BCGenV2.1/BCGen/baseCpl/src/depUtils/ParallelIO/tests/performance/CMakeFiles/pioperf.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : tests/performance/CMakeFiles/pioperf.dir/depend

