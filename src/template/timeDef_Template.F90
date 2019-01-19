module time_type
    !------------------------------------
    ! time relative macros and types
    !------------------------------------
    use shr_kind_mod
    use ESMF
    implicit none
    #set $clockCnt = len($proc_cfgs)
    integer,   parameter :: max_clocks = $clockCnt+1
    integer,   parameter :: max_alarms = $clockCnt+6
    integer,   parameter :: clock_drv = 1
    #for $index, $model in enumerate($proc_cfgs)
         #set $name = $model.name
         #set $id = $index+2
    integer,   parameter :: clock_${name} = $id
    #end for
    integer,   parameter :: alarm_restart = 1
    integer,   parameter :: alarm_run = 2
    integer,   parameter :: alarm_stop = 3
    integer,   parameter :: alarm_datestop = 4
    integer,   parameter :: alarm_history  = 5
    integer,   parameter :: alarm_histavg = 6
    #for $index, $model in  enumerate($proc_cfgs)
         #set $name = $model.name
         #set $id = $index+7
    integer,   parameter :: alarm_${name}run = $id
    #end for
    character(SHR_KIND_CS), parameter :: clock_drv_name = "clock_drv"
    #for $model in $proc_cfgs
         #set $name = $model.name
    character(SHR_KIND_CS), parameter :: clock_${name}_name = "clock_${name}"
    #end for
    character(SHR_KIND_CS), parameter :: alarm_restart_name = "alarm_restart_name"
    character(SHR_KIND_CS), parameter :: alarm_run_name = "alarm_run_name"
    character(SHR_KIND_CS), parameter :: alarm_stop_name = "alarm_stop_name"
    character(SHR_KIND_CS), parameter :: alarm_datestop_name = "alarm_datestop_name"
    character(SHR_KIND_CS), parameter :: alarm_history_name = "alarm_history_name"
    character(SHR_KIND_CS), parameter :: alarm_histavg_name = "alarm_histavg_name"
    #for $model in $proc_cfgs
         #set $name = $model.name
    character(SHR_KIND_CS), parameter :: alarm_${name}run_name = "alarm_${name}run_name"
    #end for
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
