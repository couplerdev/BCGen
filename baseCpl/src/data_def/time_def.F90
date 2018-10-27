module time_type
    !------------------------------------
    ! time relative macros and types
    !------------------------------------
    use ESMF
    implicit none
    integer,   parameter :: max_clocks = 
    integer,   parameter :: max_alarms =
    integer,   parameter :: clock_drv = 
    integer,   parameter :: clock_atm = 
    integer,   parameter :: clock_ocn = 
    integer,   parameter :: alarm_restart = 
    integer,   parameter :: alarm_run = 
    integer,   parameter :: alarm_stop = 
    integer,   parameter :: alarm_datestop = 
    integer,   parameter :: alarm_history  =
    integer,   parameter :: alarm_atmrun =
    integer,   parameter :: alarm_ocnrun = 
    integer,   parameter :: alarm_histavg =  
    type EClockPointer
        type(ESMF_Clock), pointer :: EClock=> null()
    end type EClockPointer
    type timeManager
        private 
        type(EClockPointer) :: ECP(max_clocks)
        type(ESMF_Alarm) :: EAlarm(max_clocks, max_alarms)
    end type timeManager


end module time_type
