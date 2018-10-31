module time_type
    !------------------------------------
    ! time relative macros and types
    !------------------------------------
    use ESMF
    implicit none
    integer,   parameter :: max_clocks = 2+1
    integer,   parameter :: max_alarms = 2+7
    integer,   parameter :: clock_drv = 1
    integer,   parameter :: clock_atm = 2
    integer,   parameter :: clock_ocn = 3
    integer,   parameter :: alarm_restart = 1
    integer,   parameter :: alarm_run = 2
    integer,   parameter :: alarm_stop = 3
    integer,   parameter :: alarm_datestop = 4
    integer,   parameter :: alarm_history  = 5
    integer,   parameter :: alarm_histavg = 6
    integer,   parameter :: alarm_atmrun = 7
    integer,   parameter :: alarm_ocnrun = 8
    character(len=32), parameter :: clock_drv_name = "clock_drv"
    character(len=32), parameter :: clock_atm_name = "clock_atm"
    character(len=32), parameter :: clock_ocn_name = "clock_ocn"
    character(len=32), parameter :: alarm_restart_name = "alarm_restart_name"
    character(len=32), parameter :: alarm_run_name = "alarm_run_name"
    character(len=32), parameter :: alarm_stop_name = "alarm_stop_name"
    character(len=32), parameter :: alarm_datestop_name = "alarm_datestop_name"
    character(len=32), parameter :: alarm_history_name = "alarm_history_name"
    character(len=32), parameter :: alarm_histavg_name = "alarm_histavg_name"
    character(len=32), parameter :: alarm_atmrun_name = "alarm_atmrun_name"
    character(len=32), parameter :: alarm_ocnrun_name = "alarm_ocnrun_name"
    character(len=64), parameter :: time_cal_noleap = "NO_LEAP"
    character(len=64), parameter :: time_cal_gregorian = "GREGORIAN" 
    type EClockPointer
        type(ESMF_Clock), pointer :: EClock=> null()
    end type EClockPointer
    type timeManager
        private 
        type(EClockPointer) :: ECP(max_clocks)
        type(ESMF_Alarm) :: EAlarm(max_clocks, max_alarms)
    end type timeManager


end module time_type
