module time_mod
    use ESMF
    use sys
    implicit none
    integer, parameter :: NUMALARMS = 4
    integer, parameter :: NUMCOMPS = 2
    type(ESMF_Alarm) :: alarm(NUMALARMS)
    integer :: dtime(NUMCOMPS)
    public :: time_clockInit
    public :: time_clockAdvance
    public :: time_clockPrint

    public :: time_EclockGetData
    public :: time_alarmInit
    public :: time_alarmSetOn
    public :: time_alarmSetOff
    public :: time_alarmIsOn
    integer :: get_interval    
    logical :: is_restart

    character(len=*), private, parameter :: &
        time_optNONE = "none", &
        time_optNever = "never", &
        time_optNsteps = "nsteps", &
        time_optEnd = "end"

    type(ESMF_Calendar), private, save :: time_cal

contains


subroutine time_clockInit(EClock_drv, EClock_atm, cal)

    implicit none
    type(ESMF_Clock), intent(inout) :: EClock_drv
    type(ESMF_Clock), intent(inout) :: EClock_atm
    type(ESMF_CalKind_Flag), intent(inout), optional  :: cal
    type(ESMF_VM) :: vm
    type(ESMF_Time) :: currTime
    type(ESMF_Time) :: stopTime
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: histTime
    type(ESMF_Time) :: refTime
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_CalKind_Flag) :: esmf_caltype
    type(ESMF_Calender) :: calendar
    type(ESMF_Time) :: atm_offset    ! offset is to define the diff between 
                                     ! drv and comp
    type(ESMF_TimeInterval) :: atm_hist
    type(ESMF_TimeInterval) :: atm_interval
    type(ESMF_TimeInterval) :: intervals(NUMCOMPS)
    type(ESMF_Time) :: times(NUMCOMPS)   
    type(ESMF_Time) :: atm_time
    type(ESMF_Time) :: drv_time
    type(ESMF_TimeINterval) :: tmp_interval

    integer :: stop_yy = 1
    integer :: stop_mm = 0
    integer :: stop_d = 0 
    integer :: stop_m = 0
    integer :: stop_s = 0

    integer :: atm_yy = 0
    integer :: atm_mm = 0 
    integer :: atm_d = 0
    integer :: atm_m = 0 
    integer :: atm_s = 0 

    integer :: atm_dt_d = 0   ! months are not allowed 
    integer :: atm_dt_m = 0
    integer :: atm_dt_s = 3

    integer :: atm_hist_d = 0
    integer :: atm_hist_m = 0 
    integer :: atm_hist_s = 9 
    ! local variables
    integer :: i    
    integer :: rc
    integer :: sec
    integer :: step

    if(present(cal))then
        esmf_caltype = cal
    else
        esmf_caltype = ESMF_CALKIND_360DAY
    end if

    call ESMF_Initialize(vm=vm, defaultCalkind=esmf_caltype, defaultlogfilename=trim(logs), &
                         logkindflag=ESMF_LOGKIND_MULTI, rc=rc)

    calendar = ESMF_CalendarCreate(esmf_caltype, name="calendar", rc=rc)
    time_cal = calendar

    if(.not. isrestart)then
        i=1
        call ESMF_TimeSet(atm_time, yy=atm_yy, mm=atm_mm, dd=atm_d, m=atm_m,&
                              s=atm_s,calendar=calendat, rc=rc)
        times(i) = atm_time
 
        drv_time = minval(atm_time)
        i = 1
        call ESMF_TimeIntervalSet(atm_interval, dd=atm_dt_d, m=atm_dt_m, s=atm_dt_s,&
                               rc=rc)
        intervals(i) = atm_interval
        i+=1
        do i = 1, NUMCOMPS
            call ESMF_TimeIntervalGet(intervals(i), s=sec, rc=rc)
            dtime(i) = sec
        end do
        step = minval(dtime)
        do i = 1, NUMCOMPS
            if( mod(dtime(i), step)/=0 )then
                call sys_abort("invalidate step set")
            end if
        end do 
        call ESMF_TimeIntervalSet(timeStep, s=step, rc=rc)      

        call ESMF_TimeIntervalSet(atm_hist, dd=atm_hist_d, m=atm_hist_m, s=atm_hist_s,rc=rc)
        

        ! init stop times
        call ESMF_TimeSet(stopTime, yy=stop_yy,mm=stop_mm, dd=stop_d, m=stop_m, s=stop_s, rc=rc) 

        ! validate times
        currTime = drv_time
        start_time = drv_time
        call ESMF_ClockCreate(timeStep, EClock_drv, stopTime=stopTime, name='clock drv', rc=rc)
        
        call ESMF_ClockCreate(atm_step, EClock_atm, stopTime=stopTime, name='clock atm', rc=rc)

        !--------------------------------------
        ! regist alarm
        !-------------------------------------- 
        


    else


    end if


end subroutine time_clockInit


subroutine time_alarmInit(EClock, alarm, opt, optN, refTime, alarmName)

    implicit none
    type(ESMF_Clock),    intent(inout)  :: EClock
    type(ESMF_Alarm),    intent(inout)  :: alarm
    character(len=*),    intent(in),    optional :: opt
    integer,             intent(in),    optional :: optN
    type(ESMF_Time),     intent(in),    optional :: refTime

    ! local data
    type(ESMF_TimeInterval) :: alarmInterval
    type(ESMF_Time)  :: alarmTime
    type(ESMF_Time)  :: currTime
    logical :: update_nextAlarm = .true.

    call ESMF_ClockGet(EClock, CurrTime=currTime, rc=rc)

    if(present(refTime))then
        alarmTime = refTime
    else
        alarmTime = currTime
    end if

    selectcase(trim(opt))
    
    case (time_optNONE)
        call ESMF_TimeIntervalSet(alarmInterval, yy=9999, rc=rc)
        call ESMF_TimeSet(alarmTime, yy=9999, mm=12, dd=1, s=0, calendar=time_cal, rc=rc)
    case (time_optNever)
        call ESMF_TimeIntervalSet(alarmInterval, yy=9999, rc=rc)
        call ESMF_TimeSet(alarmTime, yy=9999, mm=12, dd=1, s=0, calendar=time_cal,rc=rc)
    case (time_optNsteps)
        call ESMF_ClockGet(EClock, TimeStep=alarmInterval, rc=rc)
        if (.not. present(opt_n)) call base_sys_abort(trim(alarmName)//"invalid option:"//time_optNsteps)
        if (opt_n <=0) call base_sys_abort(trim(alarmName)//" not valid opt_n")
        alarmInterval = alarmInterval*opt_n
    case default
        call base_sys_abort(trim(alarmName)//" unkown opt")
    end select

    if(update_nextAlarm)then
        alarmTime = alarmTime - alarmInterval
        do while(alarmTime<=currTime)
            alarmTime= alarmTime + alarmInterval
        end do
    end if

    alarm = ESMF_AlarmCreate(name=alarmname, clock=EClock, ringTime=alarmTime, &
      ringInterval=alarmInterval, rc=rc)

end subroutine time_alarmInit

subroutine time_alarmSetOn(EClock, alarmname)
  
    implicit none
    type(ESMF_Clock), intent(inout) :: EClock
    character(len=*), intent(in), optional :: alarmname

    type(ESMF_Alarm), pointer :: EAlarm
    type(ESMF_Alarm), pointer :: EAlarm_list(:)

    
    

end subroutine time_alarmSetOn

logical function time_alarmIsOn(EClock, alarmname)

    implicit none
    
    type(ESMF_Clock),   intent(in) :: EClock
    character(len=*),   intent(in) :: alarmname

    !-------local------
    integer :: n
    integer :: rc
    logical :: found 
    logical :: set
    type(ESMF_Time)  :: ETime1, ETime2
    type(ESMF_Alarm), pointer :: EAlarm
    type(ESMF_Alarm), pointer :: EAlarm_list(:)
    integer   :: AlarmCount 

    AlarmCount = 2
    allocate(EAlarm_list(AlarmCount))
    call ESMF_ClockGetAlarmList(EClock, alarmLisFlag=ESMF_ALARMLIST_ALL, &
           alarmList=EAlarm_list, alarmCount=AlarmCount, rc=rc)
    do n = 1, AlarmCount
         name = trim('unset')
         call ESMF_AlarmGet(EAlarm_list(n), name=name)
         if (trim(name)==trim(alarmname))then
             found = .true.
             time_alarmIsOn = ESMF_AlarmIsRinging(alarm=EAlarm_list(n), rc=rc)
             if (trim(alarmname==trim()))
             end if
          end if
     end do

     if(.not. found) then
         write(logunit, *) "ERROR alarm not valid"
         call base_sys_abort("ERROR alarm not valid")
     end if
     deallocate(EAlarm_list)

end function time_alarmIsOn


integer function get_interval(time_interval)
    ! no only support gregory calender
    implicit none
    type(ESMF_TimeInterval), intent(in) :: time_interval
    integer :: sec
    integer :: rc     

    call ESMF_TimeIntervalSet(time_interval, s=sec, rc=rc)    
    get_interval = sec

end function

end module time_mod 
