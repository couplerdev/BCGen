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
CMAKE_SOURCE_DIR = /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO

# Include any dependencies generated for this target.
include src/flib/CMakeFiles/piof.dir/depend.make

# Include the progress variables for this target.
include src/flib/CMakeFiles/piof.dir/progress.make

# Include the compile flags for this target's objects.
include src/flib/CMakeFiles/piof.dir/flags.make

src/flib/pionfatt_mod.F90: src/flib/pionfatt_mod.F90.in
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating pionfatt_mod.F90"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && genf90/src/genf90/genf90.pl /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pionfatt_mod.F90.in > pionfatt_mod.F90

src/flib/pionfput_mod.F90: src/flib/pionfput_mod.F90.in
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Generating pionfput_mod.F90"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && genf90/src/genf90/genf90.pl /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pionfput_mod.F90.in > pionfput_mod.F90

src/flib/pionfget_mod.F90: src/flib/pionfget_mod.F90.in
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Generating pionfget_mod.F90"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && genf90/src/genf90/genf90.pl /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pionfget_mod.F90.in > pionfget_mod.F90

src/flib/piodarray.F90: src/flib/piodarray.F90.in
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Generating piodarray.F90"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && genf90/src/genf90/genf90.pl /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/piodarray.F90.in > piodarray.F90

src/flib/CMakeFiles/piof.dir/pio_nf.F90.o: src/flib/CMakeFiles/piof.dir/flags.make
src/flib/CMakeFiles/piof.dir/pio_nf.F90.o: src/flib/pio_nf.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building Fortran object src/flib/CMakeFiles/piof.dir/pio_nf.F90.o"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pio_nf.F90 -o CMakeFiles/piof.dir/pio_nf.F90.o

src/flib/CMakeFiles/piof.dir/pio_nf.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/piof.dir/pio_nf.F90.i"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pio_nf.F90 > CMakeFiles/piof.dir/pio_nf.F90.i

src/flib/CMakeFiles/piof.dir/pio_nf.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/piof.dir/pio_nf.F90.s"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pio_nf.F90 -o CMakeFiles/piof.dir/pio_nf.F90.s

src/flib/CMakeFiles/piof.dir/pio_nf.F90.o.requires:

.PHONY : src/flib/CMakeFiles/piof.dir/pio_nf.F90.o.requires

src/flib/CMakeFiles/piof.dir/pio_nf.F90.o.provides: src/flib/CMakeFiles/piof.dir/pio_nf.F90.o.requires
	$(MAKE) -f src/flib/CMakeFiles/piof.dir/build.make src/flib/CMakeFiles/piof.dir/pio_nf.F90.o.provides.build
.PHONY : src/flib/CMakeFiles/piof.dir/pio_nf.F90.o.provides

src/flib/CMakeFiles/piof.dir/pio_nf.F90.o.provides.build: src/flib/CMakeFiles/piof.dir/pio_nf.F90.o


src/flib/CMakeFiles/piof.dir/pio.F90.o: src/flib/CMakeFiles/piof.dir/flags.make
src/flib/CMakeFiles/piof.dir/pio.F90.o: src/flib/pio.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Building Fortran object src/flib/CMakeFiles/piof.dir/pio.F90.o"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pio.F90 -o CMakeFiles/piof.dir/pio.F90.o

src/flib/CMakeFiles/piof.dir/pio.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/piof.dir/pio.F90.i"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pio.F90 > CMakeFiles/piof.dir/pio.F90.i

src/flib/CMakeFiles/piof.dir/pio.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/piof.dir/pio.F90.s"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pio.F90 -o CMakeFiles/piof.dir/pio.F90.s

src/flib/CMakeFiles/piof.dir/pio.F90.o.requires:

.PHONY : src/flib/CMakeFiles/piof.dir/pio.F90.o.requires

src/flib/CMakeFiles/piof.dir/pio.F90.o.provides: src/flib/CMakeFiles/piof.dir/pio.F90.o.requires
	$(MAKE) -f src/flib/CMakeFiles/piof.dir/build.make src/flib/CMakeFiles/piof.dir/pio.F90.o.provides.build
.PHONY : src/flib/CMakeFiles/piof.dir/pio.F90.o.provides

src/flib/CMakeFiles/piof.dir/pio.F90.o.provides.build: src/flib/CMakeFiles/piof.dir/pio.F90.o


src/flib/CMakeFiles/piof.dir/pio_kinds.F90.o: src/flib/CMakeFiles/piof.dir/flags.make
src/flib/CMakeFiles/piof.dir/pio_kinds.F90.o: src/flib/pio_kinds.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_7) "Building Fortran object src/flib/CMakeFiles/piof.dir/pio_kinds.F90.o"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pio_kinds.F90 -o CMakeFiles/piof.dir/pio_kinds.F90.o

src/flib/CMakeFiles/piof.dir/pio_kinds.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/piof.dir/pio_kinds.F90.i"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pio_kinds.F90 > CMakeFiles/piof.dir/pio_kinds.F90.i

src/flib/CMakeFiles/piof.dir/pio_kinds.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/piof.dir/pio_kinds.F90.s"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pio_kinds.F90 -o CMakeFiles/piof.dir/pio_kinds.F90.s

src/flib/CMakeFiles/piof.dir/pio_kinds.F90.o.requires:

.PHONY : src/flib/CMakeFiles/piof.dir/pio_kinds.F90.o.requires

src/flib/CMakeFiles/piof.dir/pio_kinds.F90.o.provides: src/flib/CMakeFiles/piof.dir/pio_kinds.F90.o.requires
	$(MAKE) -f src/flib/CMakeFiles/piof.dir/build.make src/flib/CMakeFiles/piof.dir/pio_kinds.F90.o.provides.build
.PHONY : src/flib/CMakeFiles/piof.dir/pio_kinds.F90.o.provides

src/flib/CMakeFiles/piof.dir/pio_kinds.F90.o.provides.build: src/flib/CMakeFiles/piof.dir/pio_kinds.F90.o


src/flib/CMakeFiles/piof.dir/pio_types.F90.o: src/flib/CMakeFiles/piof.dir/flags.make
src/flib/CMakeFiles/piof.dir/pio_types.F90.o: src/flib/pio_types.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_8) "Building Fortran object src/flib/CMakeFiles/piof.dir/pio_types.F90.o"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pio_types.F90 -o CMakeFiles/piof.dir/pio_types.F90.o

src/flib/CMakeFiles/piof.dir/pio_types.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/piof.dir/pio_types.F90.i"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pio_types.F90 > CMakeFiles/piof.dir/pio_types.F90.i

src/flib/CMakeFiles/piof.dir/pio_types.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/piof.dir/pio_types.F90.s"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pio_types.F90 -o CMakeFiles/piof.dir/pio_types.F90.s

src/flib/CMakeFiles/piof.dir/pio_types.F90.o.requires:

.PHONY : src/flib/CMakeFiles/piof.dir/pio_types.F90.o.requires

src/flib/CMakeFiles/piof.dir/pio_types.F90.o.provides: src/flib/CMakeFiles/piof.dir/pio_types.F90.o.requires
	$(MAKE) -f src/flib/CMakeFiles/piof.dir/build.make src/flib/CMakeFiles/piof.dir/pio_types.F90.o.provides.build
.PHONY : src/flib/CMakeFiles/piof.dir/pio_types.F90.o.provides

src/flib/CMakeFiles/piof.dir/pio_types.F90.o.provides.build: src/flib/CMakeFiles/piof.dir/pio_types.F90.o


src/flib/CMakeFiles/piof.dir/piolib_mod.F90.o: src/flib/CMakeFiles/piof.dir/flags.make
src/flib/CMakeFiles/piof.dir/piolib_mod.F90.o: src/flib/piolib_mod.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_9) "Building Fortran object src/flib/CMakeFiles/piof.dir/piolib_mod.F90.o"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/piolib_mod.F90 -o CMakeFiles/piof.dir/piolib_mod.F90.o

src/flib/CMakeFiles/piof.dir/piolib_mod.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/piof.dir/piolib_mod.F90.i"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/piolib_mod.F90 > CMakeFiles/piof.dir/piolib_mod.F90.i

src/flib/CMakeFiles/piof.dir/piolib_mod.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/piof.dir/piolib_mod.F90.s"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/piolib_mod.F90 -o CMakeFiles/piof.dir/piolib_mod.F90.s

src/flib/CMakeFiles/piof.dir/piolib_mod.F90.o.requires:

.PHONY : src/flib/CMakeFiles/piof.dir/piolib_mod.F90.o.requires

src/flib/CMakeFiles/piof.dir/piolib_mod.F90.o.provides: src/flib/CMakeFiles/piof.dir/piolib_mod.F90.o.requires
	$(MAKE) -f src/flib/CMakeFiles/piof.dir/build.make src/flib/CMakeFiles/piof.dir/piolib_mod.F90.o.provides.build
.PHONY : src/flib/CMakeFiles/piof.dir/piolib_mod.F90.o.provides

src/flib/CMakeFiles/piof.dir/piolib_mod.F90.o.provides.build: src/flib/CMakeFiles/piof.dir/piolib_mod.F90.o


src/flib/CMakeFiles/piof.dir/pio_support.F90.o: src/flib/CMakeFiles/piof.dir/flags.make
src/flib/CMakeFiles/piof.dir/pio_support.F90.o: src/flib/pio_support.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_10) "Building Fortran object src/flib/CMakeFiles/piof.dir/pio_support.F90.o"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pio_support.F90 -o CMakeFiles/piof.dir/pio_support.F90.o

src/flib/CMakeFiles/piof.dir/pio_support.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/piof.dir/pio_support.F90.i"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pio_support.F90 > CMakeFiles/piof.dir/pio_support.F90.i

src/flib/CMakeFiles/piof.dir/pio_support.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/piof.dir/pio_support.F90.s"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pio_support.F90 -o CMakeFiles/piof.dir/pio_support.F90.s

src/flib/CMakeFiles/piof.dir/pio_support.F90.o.requires:

.PHONY : src/flib/CMakeFiles/piof.dir/pio_support.F90.o.requires

src/flib/CMakeFiles/piof.dir/pio_support.F90.o.provides: src/flib/CMakeFiles/piof.dir/pio_support.F90.o.requires
	$(MAKE) -f src/flib/CMakeFiles/piof.dir/build.make src/flib/CMakeFiles/piof.dir/pio_support.F90.o.provides.build
.PHONY : src/flib/CMakeFiles/piof.dir/pio_support.F90.o.provides

src/flib/CMakeFiles/piof.dir/pio_support.F90.o.provides.build: src/flib/CMakeFiles/piof.dir/pio_support.F90.o


src/flib/CMakeFiles/piof.dir/pionfatt_mod.F90.o: src/flib/CMakeFiles/piof.dir/flags.make
src/flib/CMakeFiles/piof.dir/pionfatt_mod.F90.o: src/flib/pionfatt_mod.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_11) "Building Fortran object src/flib/CMakeFiles/piof.dir/pionfatt_mod.F90.o"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pionfatt_mod.F90 -o CMakeFiles/piof.dir/pionfatt_mod.F90.o

src/flib/CMakeFiles/piof.dir/pionfatt_mod.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/piof.dir/pionfatt_mod.F90.i"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pionfatt_mod.F90 > CMakeFiles/piof.dir/pionfatt_mod.F90.i

src/flib/CMakeFiles/piof.dir/pionfatt_mod.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/piof.dir/pionfatt_mod.F90.s"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pionfatt_mod.F90 -o CMakeFiles/piof.dir/pionfatt_mod.F90.s

src/flib/CMakeFiles/piof.dir/pionfatt_mod.F90.o.requires:

.PHONY : src/flib/CMakeFiles/piof.dir/pionfatt_mod.F90.o.requires

src/flib/CMakeFiles/piof.dir/pionfatt_mod.F90.o.provides: src/flib/CMakeFiles/piof.dir/pionfatt_mod.F90.o.requires
	$(MAKE) -f src/flib/CMakeFiles/piof.dir/build.make src/flib/CMakeFiles/piof.dir/pionfatt_mod.F90.o.provides.build
.PHONY : src/flib/CMakeFiles/piof.dir/pionfatt_mod.F90.o.provides

src/flib/CMakeFiles/piof.dir/pionfatt_mod.F90.o.provides.build: src/flib/CMakeFiles/piof.dir/pionfatt_mod.F90.o


src/flib/CMakeFiles/piof.dir/pionfput_mod.F90.o: src/flib/CMakeFiles/piof.dir/flags.make
src/flib/CMakeFiles/piof.dir/pionfput_mod.F90.o: src/flib/pionfput_mod.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_12) "Building Fortran object src/flib/CMakeFiles/piof.dir/pionfput_mod.F90.o"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pionfput_mod.F90 -o CMakeFiles/piof.dir/pionfput_mod.F90.o

src/flib/CMakeFiles/piof.dir/pionfput_mod.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/piof.dir/pionfput_mod.F90.i"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pionfput_mod.F90 > CMakeFiles/piof.dir/pionfput_mod.F90.i

src/flib/CMakeFiles/piof.dir/pionfput_mod.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/piof.dir/pionfput_mod.F90.s"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pionfput_mod.F90 -o CMakeFiles/piof.dir/pionfput_mod.F90.s

src/flib/CMakeFiles/piof.dir/pionfput_mod.F90.o.requires:

.PHONY : src/flib/CMakeFiles/piof.dir/pionfput_mod.F90.o.requires

src/flib/CMakeFiles/piof.dir/pionfput_mod.F90.o.provides: src/flib/CMakeFiles/piof.dir/pionfput_mod.F90.o.requires
	$(MAKE) -f src/flib/CMakeFiles/piof.dir/build.make src/flib/CMakeFiles/piof.dir/pionfput_mod.F90.o.provides.build
.PHONY : src/flib/CMakeFiles/piof.dir/pionfput_mod.F90.o.provides

src/flib/CMakeFiles/piof.dir/pionfput_mod.F90.o.provides.build: src/flib/CMakeFiles/piof.dir/pionfput_mod.F90.o


src/flib/CMakeFiles/piof.dir/pionfget_mod.F90.o: src/flib/CMakeFiles/piof.dir/flags.make
src/flib/CMakeFiles/piof.dir/pionfget_mod.F90.o: src/flib/pionfget_mod.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_13) "Building Fortran object src/flib/CMakeFiles/piof.dir/pionfget_mod.F90.o"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pionfget_mod.F90 -o CMakeFiles/piof.dir/pionfget_mod.F90.o

src/flib/CMakeFiles/piof.dir/pionfget_mod.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/piof.dir/pionfget_mod.F90.i"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pionfget_mod.F90 > CMakeFiles/piof.dir/pionfget_mod.F90.i

src/flib/CMakeFiles/piof.dir/pionfget_mod.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/piof.dir/pionfget_mod.F90.s"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/pionfget_mod.F90 -o CMakeFiles/piof.dir/pionfget_mod.F90.s

src/flib/CMakeFiles/piof.dir/pionfget_mod.F90.o.requires:

.PHONY : src/flib/CMakeFiles/piof.dir/pionfget_mod.F90.o.requires

src/flib/CMakeFiles/piof.dir/pionfget_mod.F90.o.provides: src/flib/CMakeFiles/piof.dir/pionfget_mod.F90.o.requires
	$(MAKE) -f src/flib/CMakeFiles/piof.dir/build.make src/flib/CMakeFiles/piof.dir/pionfget_mod.F90.o.provides.build
.PHONY : src/flib/CMakeFiles/piof.dir/pionfget_mod.F90.o.provides

src/flib/CMakeFiles/piof.dir/pionfget_mod.F90.o.provides.build: src/flib/CMakeFiles/piof.dir/pionfget_mod.F90.o


src/flib/CMakeFiles/piof.dir/piodarray.F90.o: src/flib/CMakeFiles/piof.dir/flags.make
src/flib/CMakeFiles/piof.dir/piodarray.F90.o: src/flib/piodarray.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_14) "Building Fortran object src/flib/CMakeFiles/piof.dir/piodarray.F90.o"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/piodarray.F90 -o CMakeFiles/piof.dir/piodarray.F90.o

src/flib/CMakeFiles/piof.dir/piodarray.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/piof.dir/piodarray.F90.i"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/piodarray.F90 > CMakeFiles/piof.dir/piodarray.F90.i

src/flib/CMakeFiles/piof.dir/piodarray.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/piof.dir/piodarray.F90.s"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && /home/hq/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/piodarray.F90 -o CMakeFiles/piof.dir/piodarray.F90.s

src/flib/CMakeFiles/piof.dir/piodarray.F90.o.requires:

.PHONY : src/flib/CMakeFiles/piof.dir/piodarray.F90.o.requires

src/flib/CMakeFiles/piof.dir/piodarray.F90.o.provides: src/flib/CMakeFiles/piof.dir/piodarray.F90.o.requires
	$(MAKE) -f src/flib/CMakeFiles/piof.dir/build.make src/flib/CMakeFiles/piof.dir/piodarray.F90.o.provides.build
.PHONY : src/flib/CMakeFiles/piof.dir/piodarray.F90.o.provides

src/flib/CMakeFiles/piof.dir/piodarray.F90.o.provides.build: src/flib/CMakeFiles/piof.dir/piodarray.F90.o


# Object files for target piof
piof_OBJECTS = \
"CMakeFiles/piof.dir/pio_nf.F90.o" \
"CMakeFiles/piof.dir/pio.F90.o" \
"CMakeFiles/piof.dir/pio_kinds.F90.o" \
"CMakeFiles/piof.dir/pio_types.F90.o" \
"CMakeFiles/piof.dir/piolib_mod.F90.o" \
"CMakeFiles/piof.dir/pio_support.F90.o" \
"CMakeFiles/piof.dir/pionfatt_mod.F90.o" \
"CMakeFiles/piof.dir/pionfput_mod.F90.o" \
"CMakeFiles/piof.dir/pionfget_mod.F90.o" \
"CMakeFiles/piof.dir/piodarray.F90.o"

# External object files for target piof
piof_EXTERNAL_OBJECTS =

src/flib/libpiof.a: src/flib/CMakeFiles/piof.dir/pio_nf.F90.o
src/flib/libpiof.a: src/flib/CMakeFiles/piof.dir/pio.F90.o
src/flib/libpiof.a: src/flib/CMakeFiles/piof.dir/pio_kinds.F90.o
src/flib/libpiof.a: src/flib/CMakeFiles/piof.dir/pio_types.F90.o
src/flib/libpiof.a: src/flib/CMakeFiles/piof.dir/piolib_mod.F90.o
src/flib/libpiof.a: src/flib/CMakeFiles/piof.dir/pio_support.F90.o
src/flib/libpiof.a: src/flib/CMakeFiles/piof.dir/pionfatt_mod.F90.o
src/flib/libpiof.a: src/flib/CMakeFiles/piof.dir/pionfput_mod.F90.o
src/flib/libpiof.a: src/flib/CMakeFiles/piof.dir/pionfget_mod.F90.o
src/flib/libpiof.a: src/flib/CMakeFiles/piof.dir/piodarray.F90.o
src/flib/libpiof.a: src/flib/CMakeFiles/piof.dir/build.make
src/flib/libpiof.a: src/flib/CMakeFiles/piof.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/CMakeFiles --progress-num=$(CMAKE_PROGRESS_15) "Linking Fortran static library libpiof.a"
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && $(CMAKE_COMMAND) -P CMakeFiles/piof.dir/cmake_clean_target.cmake
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/piof.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
src/flib/CMakeFiles/piof.dir/build: src/flib/libpiof.a

.PHONY : src/flib/CMakeFiles/piof.dir/build

src/flib/CMakeFiles/piof.dir/requires: src/flib/CMakeFiles/piof.dir/pio_nf.F90.o.requires
src/flib/CMakeFiles/piof.dir/requires: src/flib/CMakeFiles/piof.dir/pio.F90.o.requires
src/flib/CMakeFiles/piof.dir/requires: src/flib/CMakeFiles/piof.dir/pio_kinds.F90.o.requires
src/flib/CMakeFiles/piof.dir/requires: src/flib/CMakeFiles/piof.dir/pio_types.F90.o.requires
src/flib/CMakeFiles/piof.dir/requires: src/flib/CMakeFiles/piof.dir/piolib_mod.F90.o.requires
src/flib/CMakeFiles/piof.dir/requires: src/flib/CMakeFiles/piof.dir/pio_support.F90.o.requires
src/flib/CMakeFiles/piof.dir/requires: src/flib/CMakeFiles/piof.dir/pionfatt_mod.F90.o.requires
src/flib/CMakeFiles/piof.dir/requires: src/flib/CMakeFiles/piof.dir/pionfput_mod.F90.o.requires
src/flib/CMakeFiles/piof.dir/requires: src/flib/CMakeFiles/piof.dir/pionfget_mod.F90.o.requires
src/flib/CMakeFiles/piof.dir/requires: src/flib/CMakeFiles/piof.dir/piodarray.F90.o.requires

.PHONY : src/flib/CMakeFiles/piof.dir/requires

src/flib/CMakeFiles/piof.dir/clean:
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib && $(CMAKE_COMMAND) -P CMakeFiles/piof.dir/cmake_clean.cmake
.PHONY : src/flib/CMakeFiles/piof.dir/clean

src/flib/CMakeFiles/piof.dir/depend: src/flib/pionfatt_mod.F90
src/flib/CMakeFiles/piof.dir/depend: src/flib/pionfput_mod.F90
src/flib/CMakeFiles/piof.dir/depend: src/flib/pionfget_mod.F90
src/flib/CMakeFiles/piof.dir/depend: src/flib/piodarray.F90
	cd /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib /home/hq/git/BCGen/baseCpl/src/depUtils/ParallelIO/src/flib/CMakeFiles/piof.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : src/flib/CMakeFiles/piof.dir/depend

