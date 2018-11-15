program testNc

    use comms_nc, only : sMatReadnc
    use mct_mod
    use comms_def

implicit none
character(len=20)    :: filename = "testFile.nc"
type(map_mod)        :: mapper

mapper%map_type = "smat"
 
call sMatReadnc(mapper, filename) 


end program
