#
# File fdepends.awk
#
# NOTE - this script been stripped down to take care of 
#            F90 modules only
#
# This script does makefiles for individual object targets.  However, 
# it only includes dependencies for F90 modules
#
# It initializes itself by defining the target half of each 
# dependency line, then printing a special first dependency 
# line containing target's object file, subroutine name, makefile 
# name, a colon, and the source file on which the others are 
# dependent.
#
# Example of first dependency line: 
# CICE.o ObjDepends/CICE.do: CICE.f
#
# Example of other dependency line: 
# CICE.o: io.o
#
# Predefined variables     Typical values
#     NAME                    CICE
#     SUF                      .f
#     COMPDIR                  ./compile
#
BEGIN { PRLINE = NAME".o: "
        print NAME".o: " NAME SUF }

#
# awk reads each line of the filename argument $2 until it finds 
# the pattern "use".
# 

/use/ { # if "use" is not the first token on the line, skip it
        if ( $1 != "use" ) next

        # Otherwise, assume the second field is the F90 module name,
        # remove any comma at the end of the second field (due to 
        # ONLY or rename), and print it in a dependency line.
        # exclude system-installed modules (eg netcdf) from dependencies
        sub(/,/,"",$2)
        lwr = tolower($2)
        if ( lwr == "netcdf") next
        if ( lwr == "mpi") next
        if ( lwr == "pio") next
        if ( lwr == "mct_mod") next
        if ( lwr == "time_mod") next
        if ( lwr == "time_type") next
        if ( lwr == "perf_mod") next
        if ( lwr == "proc_def") next
        if ( lwr == "global_var") next
        if ( lwr == "base_field") next
        if ( lwr == "esmf") next
        if ( substr(lwr,1,3) == "shr" && lwr != "shr_megan_mod") next
        print PRLINE COMPDIR "/" $2".o"
       }
