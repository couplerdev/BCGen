module time_type
    !------------------------------------
    ! time relative macros and types
    !------------------------------------
    use shr_kind_mod
    use ESMF
    implicit none
    integer,   parameter :: max_clocks = 3+1
    integer,   parameter :: max_alarms = 3+6
    integer,   parameter :: clock_drv = 1
    integer,   parameter :: clock_lnd = 2
    integer,   parameter :: clock_atm = 3
    integer,   parameter :: clock_ocn = 4
    integer,   parameter :: alarm_restart = 1
    integer,   parameter :: alarm_run = 2
    integer,   parameter :: alarm_stop = 3
    integer,   parameter :: alarm_datestop = 4
    integer,   parameter :: alarm_history  = 5
    integer,   parameter :: alarm_histavg = 6
    integer,   parameter :: alarm_lndrun = 7
    integer,   parameter :: alarm_atmrun = 8
    integer,   parameter :: alarm_ocnrun = 9
    character(SHR_KIND_CS), parameter :: clock_drv_name = "clock_drv"
    character(SHR_KIND_CS), parameter :: clock_lnd_name = "clock_lnd"
    character(SHR_KIND_CS), parameter :: clock_atm_name = "clock_atm"
    character(SHR_KIND_CS), parameter :: clock_ocn_name = "clock_ocn"
    character(SHR_KIND_CS), parameter :: alarm_restart_name = "alarm_restart_name"
    character(SHR_KIND_CS), parameter :: alarm_run_name = "alarm_run_name"
    character(SHR_KIND_CS), parameter :: alarm_stop_name = "alarm_stop_name"
    character(SHR_KIND_CS), parameter :: alarm_datestop_name = "alarm_datestop_name"
    character(SHR_KIND_CS), parameter :: alarm_history_name = "alarm_history_name"
    character(SHR_KIND_CS), parameter :: alarm_histavg_name = "alarm_histavg_name"
    character(SHR_KIND_CS), parameter :: alarm_lndrun_name = "alarm_lndrun_name"
    character(SHR_KIND_CS), parameter :: alarm_atmrun_name = "alarm_atmrun_name"
    character(SHR_KIND_CS), parameter :: alarm_ocnrun_name = "alarm_ocnrun_name"
    character(SHR_KIND_CL), parameter :: time_cal_noleap = "NO_LEAP"
    character(SHR_KIND_CL), parameter :: time_cal_gregorian = "GREGORIAN" 
    type(ESMF_CalKind_Flag), parameter :: time_cal_default = ESMF_CALKIND_NOLEAP
    type EClockPointer
        type(ESMF_Clock), pointer :: EClock=> null()
    end type EClockPointer
    type timeManager
        type(EClockPointer) :: ECP(max_clocks)
        type(ESMF_Alarm) :: EAlarm(max_clocks, max_alarms)
    end type timeManager


end module time_type
