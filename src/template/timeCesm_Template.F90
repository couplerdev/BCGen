module time_mod
    use shr_kind_mod
    use ESMF
    use mpi
    use shr_sys_mod
    !use base_sys
    use global_var,  only: metaData
    use time_type
    !use base_mpi
    use shr_mpi_mod
    use base_io
    !use base_file
    use shr_file_mod
    implicit none
    #set $ncomps = len($proc_cfgs)
    integer, parameter :: NUMALARMS = $ncomps+6
    integer, parameter :: NUMCOMPS = $ncomps
    type(ESMF_Alarm) :: alarm(NUMALARMS)
    integer :: dtime(NUMCOMPS)
    public :: time_clockRegist
    public :: time_clockInit
    public :: time_clockAdvance
    public :: time_clockGetInfo
    public :: time_clockDateInSync
    !public :: time_clockPrint

    !public :: time_EclockGetData
    public :: time_EclockInit
    public :: time_alarmInit
    public :: time_alarmSetOn
    public :: time_alarmSetOff
    public :: time_timeYmdInit
    public :: time_alarmIsOn
    public :: get_interval    
    !public :: is_restart

    character(len=*), public, parameter :: &
        time_optNONE     = "none", &
        time_optNever    = "never", &
        time_optNsteps   = "nsteps", &
        time_optEnd      = "end", &
        time_optNSeconds = "nseconds", &
        time_optNMinutes = "nminutes", &
        time_optNHours   = "nhours", &
        time_optNDays    = "ndays", &
        time_optNMonths  = "nmonths", &
        time_optNYears    = "nyears", &
        time_optYearly   = "yearly", &
        time_optDate     = "Date", &
        time_optMonthly  = "monthly"

    type(ESMF_Calendar), private, save :: time_cal
    logical :: end_restart
    integer, parameter   :: SecPerDay = 86400

contains

subroutine time_clockRegist(SyncClock, eclock, id)

    implicit none
    type(timeManager),   intent(inout)  :: SyncClock
    type(ESMF_Clock),    intent(in)     :: eclock
    integer,             intent(in)     :: id
 
    SyncClock%ECP(id)%eclock = eclock

end subroutine time_clockRegist

subroutine time_clockInit(SyncClock, nmlfile, mpicom, EClock_drv, &
                          #for $model in $proc_cfgs
                               #set $name = $model.name
                          EClock_${name}, &
                          #end for
                          restart, restart_file, cal)

    implicit none
    type(timeManager), intent(inout)         :: SyncClock
    character(len=*),  intent(in)            :: nmlfile
    integer,           intent(in)            :: mpicom
    logical,           intent(in)            :: restart
    character(len=*),  intent(in)            :: restart_file
    type(ESMF_Clock), intent(inout), pointer :: EClock_drv
    #for $model in $proc_cfgs
         #set $name = $model.name
    type(ESMF_Clock), intent(inout), pointer :: EClock_${name}
    #end for
    type(ESMF_CalKind_Flag), intent(in), optional  :: cal
    type(ESMF_VM) :: vm
    type(ESMF_Time) :: currTime
    type(ESMF_Time) :: stopTime
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: histTime
    type(ESMF_Time) :: refTime
    type(ESMF_Time) :: OffsetTime
    type(ESMF_Time) :: StopTime1
    type(ESMF_Time) :: StopTime2
    type(ESMF_TimeInterval) :: TimeStep
    type(ESMF_CalKind_Flag) :: esmf_caltype
                                     ! drv and comp
    type(ESMF_TimeInterval) :: intervals(NUMCOMPS)
    type(ESMF_Time) :: times(NUMCOMPS)   
    type(ESMF_Time) :: drv_time
    type(ESMF_TimeInterval) :: tmp_interval

    character(SHR_KIND_CL)  :: calendar
    character(SHR_KIND_CL)  :: stop_option
    integer                 :: stop_n
    integer                 :: stop_ymd
    integer                 :: stop_tod
    character(SHR_KIND_CL)  :: restart_option
    integer                 :: restart_n
    integer                 :: restart_ymd
    character(SHR_KIND_cL)  :: history_option
    integer                 :: history_n
    integer                 :: history_ymd
    character(SHR_KIND_CL)  :: histavg_option
    integer                 :: histavg_n
    integer                 :: histavg_ymd
    integer                 :: start_ymd
    integer                 :: start_tod
    integer                 :: curr_ymd
    integer                 :: curr_tod
    integer                 :: ref_ymd
    integer                 :: ref_tod
    #for $model in $proc_cfgs
         #set $name = $model.name
    integer            :: ${name}_cpl_dt
    integer            :: ${name}_cpl_offset
    #end for
    !logical            :: end_restart

    ! local variables
    integer :: i, iam, ierr   
    integer :: rc, n
    integer :: unitn
    integer :: sec
    integer :: step
    logical :: flag= .false.
    logical :: tempTrue = .true.
    integer :: dtime(max_clocks)
    integer :: offset(max_clocks)
    character(*), parameter :: logs="time.log"
    character(*), parameter :: subname = "(time_clockInit)"

    namelist /time_args/ calendar, curr_ymd, curr_tod, &
          stop_option, stop_n, stop_ymd, stop_tod,     &
          restart_option, restart_n, restart_ymd,      &
          history_option, history_n, history_ymd,      &
          histavg_option, histavg_n, histavg_ymd,      &
          start_ymd, start_tod, ref_ymd, ref_tod,      &
          #for $model in $proc_cfgs
               #set $name = $model.name
          ${name}_cpl_dt, ${name}_cpl_offset,      &
          #end for
          end_restart

    allocate(SyncClock%ECP(clock_drv)%EClock)
    #for $model in $proc_cfgs
         #set $name = $model.name
    allocate(SyncClock%ECP(clock_${name})%EClock)
    #end for

    EClock_drv => SyncClock%ECP(clock_drv)%EClock
    #for $model in $proc_cfgs
         #set $name = $model.name
    EClock_${name} => SyncClock%ECP(clock_${name})%EClock 
    #end for

    !-------------------------------------------------------
    !   init option and conf data from nml file on root
    !   bcast to other pes
    !-------------------------------------------------------

    call mpi_comm_rank(mpicom, iam, ierr)

    if(iam==0)then
       calendar       = time_cal_noleap 
       stop_option    = ''
       stop_n         = -1
       stop_ymd       = -1
       stop_tod       = 0
       restart_option = time_optYearly
       restart_n      = -1
       restart_ymd    = -1
       history_option = time_optNever
       history_n      = -1
       history_ymd    = -1
       histavg_option = time_optNever
       histavg_n      = -1
       histavg_ymd    = -1
       start_ymd      = 0
       start_tod      = 0
       ref_ymd        = 0
       ref_tod        = 0 
       curr_ymd       = 0
       curr_tod       = 0
    
       #for $model in $proc_cfgs
            #set $name = $model.name
       ${name}_cpl_dt     = 0
       ${name}_cpl_offset = 0
       #end for
       end_restart    = .true.

       unitn = shr_file_getUnit()
       write(logUnit, *)trim(subname), 'read time_args from', trim(nmlfile)
       open(unitn, file=trim(nmlfile), status='old')
       ierr  = 1
       do while(ierr/=0)
           read(unitn,nml=time_args, iostat=ierr)
           if(ierr < 0)then
               call shr_sys_abort(subname//':: namelist return error')
           end if
       end do
    end if
    print *,'nmlfile read curr_time:',curr_ymd
    call MPI_Barrier(mpicom, ierr)
    if(restart)then
        if(metaData%iamin_cpl)then
            call base_io_read(restart_file, start_ymd, 'time start_ymd')
            call base_io_read(restart_file, start_tod, 'time start_tod')
            call base_io_read(restart_file, ref_ymd, 'time ref_ymd')
            call base_io_read(restart_file, ref_tod, 'time ref_ymd')
            call base_io_read(restart_file, curr_ymd, 'time curr_ymd')
            call base_io_read(restart_file, curr_tod, 'time curr_tod')
        end if
        
    end if
    !-----------------------------------------------------------------
    ! Broadcast namelist data
    !----------------------------------------------------------------- 
    call shr_mpi_bcast(calendar,       mpicom)
    call shr_mpi_bcast(stop_n,         mpicom)
    call shr_mpi_bcast(stop_option,    mpicom)
    call shr_mpi_bcast(stop_ymd,       mpicom)
    call shr_mpi_bcast(stop_tod,       mpicom)
    call shr_mpi_bcast(restart_n,      mpicom)
    call shr_mpi_bcast(restart_ymd,    mpicom)
    call shr_mpi_bcast(restart_option, mpicom)
    call shr_mpi_bcast(history_n,      mpicom)
    call shr_mpi_bcast(history_option, mpicom)
    call shr_mpi_bcast(history_ymd,    mpicom)
    call shr_mpi_bcast(histavg_n,      mpicom)
    call shr_mpi_bcast(histavg_option, mpicom)
    call shr_mpi_bcast(histavg_ymd,    mpicom)
    call shr_mpi_bcast(start_ymd,      mpicom)
    call shr_mpi_bcast(start_tod,      mpicom)
    call shr_mpi_bcast(ref_ymd,        mpicom)
    call shr_mpi_bcast(ref_tod,        mpicom)
    call shr_mpi_bcast(curr_ymd,       mpicom)
    call shr_mpi_bcast(curr_tod,       mpicom)
    #for $model in $proc_cfgs
         #set $name = $model.name
    call shr_mpi_bcast(${name}_cpl_dt,     mpicom)
    call shr_mpi_bcast(${name}_cpl_offset,     mpicom)
    #end for
    call shr_mpi_bcast(end_restart,    mpicom)
    if(iam==0)then
        write(*,*)subname//' bcast end'
    end if
     
    if(ref_ymd == 0)then
        ref_ymd = start_ymd
        ref_tod = start_tod
    endif
    print *, 'check your curr_time',curr_ymd
    if(curr_ymd == 0)then
        curr_ymd = start_ymd
        curr_tod = start_tod
    endif

    if(stop_ymd <0)then
        stop_ymd = 99990101
        stop_tod = 0
    endif 

    flag =  &
    #for $model in $proc_cfgs
         #set $name = $model.name
         abs(${name}_cpl_offset)>${name}_cpl_dt .or. &
    #end for
         (.false.)
    if(flag)then
        write(logUnit, *)trim(subname), ' ERROR: invalid offset'
        call shr_sys_abort('ERROR: invalid offset')
    end if

    if((start_ymd<101) .or. (start_ymd > 99991231))then
        write(logUnit, *)trim(subname), ' ERROR: illegal start_ymd', start_ymd
        call shr_sys_abort('ERROR: illegal start_ymd')
    end if


    if(present(cal))then
        esmf_caltype = cal
    else
        esmf_caltype = ESMF_CALKIND_360DAY
    end if
   
      
    !-------------------------------------------------------------
    !  time init and clock init
    !-------------------------------------------------------------

    
    call ESMF_Initialize(vm=vm, defaultCalkind=esmf_caltype, defaultlogfilename=trim(logs), &
                         logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    !call ESMF_CalendarPrint(esmf_caltype, rc=rc)
    time_cal = ESMF_CalendarCreate(esmf_caltype, rc=rc)
    dtime = 0 
    #for $model in $proc_cfgs
         #set $name = $model.name
    dtime(clock_${name}) = ${name}_cpl_dt
    #end for 
    dtime(clock_drv) = maxval(dtime)
    dtime(clock_drv) = minval(dtime)

    call time_TimeYmdInit(startTime, time_cal, start_ymd, start_tod, rc, "Start date")
    call time_TimeYmdInit(refTime, time_cal, ref_ymd, ref_tod, rc, "Reference date")
    call time_TimeYmdInit(currTime, time_cal, curr_ymd, curr_tod, rc, "Current date")

    do n = 1, max_clocks
        if(mod(dtime(n), dtime(clock_drv))/= 0)then
            write(logUnit, *)trim(subname), ' ERROR: dtime inconsistent'
            call shr_sys_abort('ERROR: dtime inconsistent')
        end if
    enddo
    !--------------------------------------------------------------
    ! regist alarm: first init comp clock and relative alarms
    ! then init drv alarms
    !--------------------------------------------------------------
    do n = 1, max_clocks
        call ESMF_TimeIntervalSet(TimeStep, s=dtime(n), rc=rc)
        call time_EClockInit(TimeStep, startTime, refTime, currTime, SyncClock%ECP(n)%EClock)

        call time_AlarmInit(SyncClock%ECP(n)%EClock, EAlarm=SyncClock%EAlarm(n, alarm_run), &
                            opt=time_optNSeconds , opt_n = dtime(n), RefTime=currTime, &
                            alarmname=trim(alarm_run_name))
        ! stop 
        call time_AlarmInit(SyncClock%ECP(n)%EClock, EAlarm=SyncClock%EAlarm(n,alarm_stop), &
                            opt=stop_option , opt_n=stop_n, opt_ymd=stop_ymd, &
                            opt_tod = stop_tod, RefTime=currTime, alarmname=trim(alarm_stop_name))
        ! date stop
        call time_AlarmInit(SyncClock%ECP(n)%EClock, EAlarm =SyncClock%EAlarm(n, alarm_datestop), &
                            opt=time_optDate,   opt_ymd = stop_ymd, opt_tod = stop_tod, &
                            RefTime=startTime, alarmname=trim(alarm_datestop_name)) 
       ! restart 
        call time_AlarmInit(SyncClock%ECP(n)%EClock, EAlarm=SyncClock%EAlarm(n, alarm_restart), &
                            opt=restart_option, opt_n=restart_n, opt_ymd=restart_ymd, &
                            RefTime=currTime, alarmname=trim(alarm_restart_name))

        ! history
        call time_AlarmInit(SyncClock%ECP(n)%EClock, EAlarm=SyncClock%EAlarm(n, alarm_history), &
                            opt=history_option, opt_n=history_n, opt_ymd=history_ymd, &
                            RefTime=startTime, alarmname=trim(alarm_history_name))
        ! histavg
        call time_AlarmInit(SyncClock%ECP(n)%EClock, EAlarm=SyncClock%EAlarm(n, alarm_histavg), &
                            opt=histavg_option, opt_n=histavg_n, opt_ymd=histavg_ymd, &
                            RefTime=startTime, alarmname=trim(alarm_histavg_name))

        call ESMF_AlarmGet(SyncClock%EAlarm(n, alarm_stop), RingTime=StopTime1, rc=rc)
        call ESMF_AlarmGet(SyncClock%EAlarm(n, alarm_datestop), RingTime=StopTime2, rc=rc)
            
        if(StopTime2 < StopTime1)then
            call ESMF_ClockSet(SyncClock%ECP(n)%EClock, StopTime=StopTime2)
        else
            call ESMF_ClockSet(SyncClock%ECP(n)%EClock, StopTime=StopTime1)
        end if
    end do

    !call base_sys_abort('end clock init')
    offset(clock_drv) = 0
    #for $model in $proc_cfgs
         #set $name = $model.name
    offset(clock_$name) = $(name)_cpl_offset
    #end for 

    do n = 1, max_clocks
        if(abs(offset(n))> dtime(n))then
            write(logUnit, *)subname, 'ERROR: offset too large', n, dtime(n), offset(n)
            call shr_sys_abort('ERROR: offset too large')
        end if

        offset(n) = offset(n) + dtime(n)

        if(mod(offset(n), dtime(clock_drv))/=0)then
            write(logUnit, *)subname, "ERROR: offset not multiple", n, dtime(clock_drv), offset(n)
            call shr_sys_abort("ERROR:offset not multiple")
        end if
    end do
    
    #for $model in $proc_cfgs
         #set $name = $model.name
    call ESMF_TimeIntervalSet(TimeStep, s=offset(clock_${name}), rc=rc)
    OffsetTime=  currTime + TimeStep
    call time_alarmInit(SyncClock%ECP(clock_drv)%EClock, EAlarm=SyncClock%EAlarm(clock_drv, alarm_${name}run),&
                  opt=time_optNSeconds, opt_n=dtime(clock_${name}), RefTime=OffsetTime,&
                  alarmname=trim(alarm_${name}run_name))
    #end for
    print *,'time init end', start_ymd, stop_ymd
    
end subroutine time_clockInit

subroutine time_clockGetInfo(EClock, curr_yr, curr_mon, curr_day, &
                          curr_ymd, curr_tod, prev_ymd, prev_tod, start_ymd, &
                          start_tod, StepNo, ref_ymd, ref_tod, &
                          stop_ymd, stop_tod, dtime, ECurrTime, alarmcount, &
                          curr_cday, next_cday, curr_time, prev_time, calendar)

    implicit none
    type(ESMF_Clock),    intent(in)      :: EClock
    integer,  optional,  intent(inout)   :: curr_yr
    integer,  optional,  intent(inout)   :: curr_mon
    integer,  optional,  intent(inout)   :: curr_day
    integer,  optional,  intent(inout)   :: curr_ymd
    integer,  optional,  intent(inout)   :: curr_tod
    integer,  optional,  intent(inout)   :: prev_ymd
    integer,  optional,  intent(inout)   :: prev_tod
    integer,  optional,  intent(inout)   :: start_ymd
    integer,  optional,  intent(inout)   :: start_tod
    integer,  optional,  intent(inout)   :: StepNo
    integer,  optional,  intent(inout)   :: ref_ymd
    integer,  optional,  intent(inout)   :: ref_tod
    integer,  optional,  intent(inout)   :: stop_ymd
    integer,  optional,  intent(inout)   :: stop_tod
    integer,  optional,  intent(inout)   :: dtime
    integer,  optional,  intent(inout)   :: alarmcount
    type(ESMF_Time), optional, intent(inout) :: ECurrTime
    real(R8),     optional,  intent(inout)   :: curr_cday
    real(R8),     optional,  intent(inout)   :: next_cday
    real(R8),     optional,  intent(inout)   :: curr_time
    real(R8),     optional,  intent(inout)   :: prev_time
    character(len=*), optional, intent(inout)  :: calendar

    !---local
    character(len=*), parameter ::subname='(time_EClockClockGetInfo)'
    type(ESMF_Time) :: currTime
    type(ESMF_Time) :: prevTime
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime
    type(ESMF_Time) :: refTime
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_TimeInterval) :: timediff
    integer :: rc
    integer(8) ::advSteps
    integer :: yy, mm, dd, sec
    integer :: ymd
    integer :: tod
    integer :: ldtime
    integer :: intyrs
    integer :: intmon
    integer :: intsec
    integer :: days
    integer :: seconds
    integer :: acount
    real(8)    :: doy, tmpdoy
    type(ESMF_Calendar) :: tmpCal
    real, parameter :: cl=1.0
    type(ESMF_Time) :: tmpTime
    type(ESMF_TimeInterval) :: tmpDTime

    if(present(calendar) .and. (.not. present(next_cday))) calendar = trim(time_cal_noleap) 
     
    call  ESMF_ClockGet(EClock, currTime=currTime, &
           advanceCount=advSteps, prevTime=prevTime, TimeStep=timeStep, &
           startTime=startTime, stopTime=stopTime, refTime=refTime, &
           AlarmCount=acount, rc=rc)
     
    call  ESMF_ClockGet(EClock, currTime=currTime, &
           advanceCount=advSteps, prevTime=prevTime, TimeStep=timeStep, &
           startTime=startTime, stopTime=stopTime, refTime=refTime, &
           AlarmCount=acount, rc=rc)
     
    call ESMF_TimeGet(currTime, yy=yy, mm=mm, dd=dd, s=sec, dayofyear_r8=doy, rc=rc)
    call time_TimeYmdGet(currTime, ymd=ymd, tod=tod)
    call ESMF_TimeIntervalGet(timeStep, s=ldtime, rc=rc) 
    
    if(present(curr_yr)) curr_yr = yy
    if(present(curr_mon)) curr_mon = mm
    if(present(curr_day)) curr_day = dd
    if(present(curr_tod)) curr_tod = tod
    if(present(curr_ymd)) curr_ymd =  ymd
    if(present(ECurrTime)) ECurrTime = currTime
    if(present(StepNo)) StepNo = advSteps
    if(present(dtime)) dtime = ldtime
    if(present(curr_cday)) curr_cday = doy
    if(present(alarmcount)) alarmcount = acount
    if(present(next_cday))then
        tmpCal =  ESMF_CalendarCreate(time_cal_default, name='Noleap', rc=rc)
        call ESMF_TimeSet(tmpTime, yy=yy, mm=mm, dd=dd, s=tod, calendar=tmpCal)
        call ESMF_TimeIntervalSet(tmpDtime, d=1, rc=rc)
        tmpTime = tmpTime + tmpDTime
        call ESMF_TimeGet(tmpTime, dayOfYear_r8=tmpdoy, rc=rc)
        next_cday = tmpdoy

    end if

    if(present(curr_time))then
        timediff = currTime - refTime
        call ESMF_TimeIntervalGet(timediff, d=days, s=seconds, rc=rc)
        curr_time = days+seconds/real(SecPerDay)
    end if

    if(present(prev_time)) then
        timediff = prevTime - RefTime
        call ESMF_TimeIntervalGet(timediff, d=days, s=seconds, rc=rc)
        prev_time = days + seconds/real(SecPerDay)
    end if

    if(present(prev_ymd) .or. present(prev_tod))then
        call time_TimeYmdGet(prevTime, ymd=ymd, tod=tod)
        if(present(prev_ymd)) prev_ymd = ymd
        if(present(prev_tod)) prev_tod = tod
    end if
      
    if(present(start_ymd) .or. present(start_tod))then
        call time_TimeYmdGet(startTime, ymd=ymd, tod=tod)
        if(present(start_ymd)) start_ymd = ymd
        if(present(start_tod)) start_tod = tod
    end if

    if(present(stop_ymd) .or. present(stop_tod))then
        call time_TimeYmdGet(stopTime, ymd=ymd, tod=tod)
        if(present(stop_ymd)) stop_ymd = ymd
        if(present(stop_tod)) stop_tod = tod
    end if
  
    if(present(ref_ymd) .or. present(ref_tod))then
        call time_TimeYmdGet(refTime, ymd=ymd, tod=tod)
        if(present(ref_ymd)) ref_ymd = ymd
        if(present(ref_tod)) ref_tod = tod
    end if

end subroutine time_clockGetInfo


subroutine time_clockAdvance(SyncClock)

    implicit none
    type(timeManager), intent(inout) ::SyncClock

    character(len=*), parameter :: subname = '(time_clockAdvance)'
    integer :: n
    type(ESMF_Time) :: drvCT, clkCT
    integer :: rc

    do n = 1, max_clocks
        print *,'clock_:',n
        call time_alarmSetOff(SyncClock%ECP(n)%EClock)
    end do

    call ESMF_ClockAdvance(SyncClock%ECP(clock_drv)%EClock, rc=rc)
    #for $model in $proc_cfgs
         #set $name = $model.name
    if(ESMF_AlarmIsRinging(SyncClock%EAlarm(clock_drv,alarm_${name}run)))then
        call ESMF_ClockAdvance(SyncClock%ECP(clock_${name})%EClock, rc=rc)
    end if
    #end for
    if(end_restart)then
        do n = 1, max_clocks
            if(time_alarmIsOn(SyncClock%ECP(n)%EClock, alarm_stop_name) .or. &
               time_alarmIsOn(SyncClock%ECP(n)%EClock, alarm_datestop_name))then
                call time_alarmSetOn(SyncClock%ECP(n)%EClock, alarm_restart_name)
            end if
        end do
    end if

end subroutine time_clockAdvance

logical function time_clockDateInSync(EClock, ymd, tod, prev)

    type(ESMF_Clock),      intent(in)  :: EClock
    integer,               intent(in)  :: ymd
    integer,               intent(in)  :: tod
    logical,  optional,    intent(in)  :: prev

    character(len=*), parameter :: subname = "(time_clockDateInSync)"
    type(ESMF_Time) :: ETime
    integer         :: ymd1
    integer         :: tod1
    logical         :: previous
    integer         :: rc

    previous = .false.
    if(present(prev))then
        previous = prev
    end if

    if(previous)then
        call ESMF_ClockGet(EClock, prevTime=ETime, rc=rc)
    else 
        call ESMF_ClockGet(EClock, currTime=ETime, rc=rc)
    end if
    call time_TimeYmdGet(ETime, ymd=ymd1, tod=tod1)
    print *, 'DateInSync: curr_time:', ymd1, tod1, " in comp:", ymd, tod
    if((ymd == ymd1) .and. (tod == tod1))then
        time_clockDateInSync = .true.
    else 
        time_clockDateInSync = .false.
    end if

end function time_clockDateInSync

subroutine time_EClockInit(TimeStep, startTime, refTime, currTime, EClock)

    implicit none
    type(ESMF_TimeInterval),  intent(in)    :: TimeStep
    type(ESMF_Time),          intent(in)    :: startTime
    type(ESMF_Time),          intent(in)    :: refTime
    type(ESMF_Time),          intent(in)    :: currTime
    type(ESMF_Clock),         intent(inout) :: EClock

    ! local
    character(*),  parameter :: subname = '(time_EClockInit)'
    integer :: rc
    type(ESMF_Time) :: clocktime
    type(ESMF_Calendar) :: tmpCal
    character(SHR_KIND_CL) :: desc

    desc =  'shared time_manager clock'
    tmpCal = ESMF_CalendarCreate(time_cal_default, name='NOLEAP', rc=rc)
    call time_timeYmdInit(clocktime, tmpCal, 99990101, 0, rc, "default stop date")
  
     
    EClock = ESMF_ClockCreate(name=trim(desc), TimeStep=TimeStep, startTime=startTime, &
                              refTime=refTime, stopTime=clocktime, rc=rc)
    !call ESMF_TimeIntervalPrint(TimeStep, rc=rc)
    !---------advance clock to current time-------
    call ESMF_ClockGet(EClock, currTime=clocktime, rc=rc)
    do while(clockTime < currTime)
        call ESMF_ClockAdvance(EClock, rc=rc)
        call ESMF_ClockGet(EClock, currTime=clocktime)
    end do

    if(clocktime /= currTime)then
        write(logUnit, *)trim(subname), ": WARING clocktime and currtime inconsistent"
    end if

end subroutine time_EClockInit

logical function time_EClockDateInSync(EClock, ymd, tod, prev)

    implicit none
    type(ESMF_Clock),    intent(in)   :: EClock
    integer,             intent(in)   :: ymd
    integer,             intent(in)   :: tod
    logical, optional,   intent(in)   :: prev

    character(len=*),  parameter   :: subname = '(time_EClockDateInSync)'
    type(ESMF_Time)  :: ETime
    integer   :: ymd1
    integer   :: tod1
    logical   :: previous
    integer   :: rc
  
    previous = .false.
    if(present(prev))then
        previous = prev
    end if

    if(previous)then
        call ESMF_ClockGet(EClock, prevTime=ETime, rc=rc)
    else
        call ESMF_ClockGet(EClock, currTime=ETime, rc=rc)
    end if
    call time_TimeYmdGet(ETime, ymd=ymd1, tod=tod1)

    if((ymd == ymd1) .and. (tod == tod1))then
        time_EClockDateInSync = .true.
    else
        time_EClockDateInSync = .false.
    end if

end function time_EClockDateInSync

subroutine time_alarmInit(EClock, EAlarm, opt, opt_n, opt_ymd, opt_tod, refTime, alarmName)

    implicit none
    type(ESMF_Clock),    intent(inout)  :: EClock
    type(ESMF_Alarm),    intent(inout)  :: EAlarm
    character(len=*),    intent(in),    optional :: opt
    integer,             intent(in),    optional :: opt_n
    integer,             intent(in),    optional :: opt_ymd
    integer,             intent(in),    optional :: opt_tod
    type(ESMF_Time),     intent(in),    optional :: refTime
    character(*),        intent(in),    optional :: alarmName

    ! local data
    character(len=*), parameter :: subname = "time_alarmInit"
    integer  :: lymd
    integer  :: ltod
    integer  :: cyy, cmm, cdd, csec
    integer  :: nyy, nmm, ndd, nsec 
    type(ESMF_Calendar)  :: tmpCal
    type(ESMF_TimeInterval) :: alarmInterval
    type(ESMF_Time)  :: alarmTime
    type(ESMF_Time)  :: currTime
    type(ESMF_Time)  :: nextAlarm
    logical :: update_nextAlarm = .true.
    integer :: rc

    call ESMF_ClockGet(EClock, CurrTime=currTime, rc=rc)
    call ESMF_TimeGet(currTime, yy=cyy, mm=cmm, dd=cdd, s=csec, rc=rc)

    if(present(refTime))then
        alarmTime = refTime
    else
        alarmTime = currTime
    end if

    ltod = 0
    if(present(opt_tod))then
        ltod = opt_tod
    end if

    lymd = - 1
    if(present(opt_ymd))then
        lymd = opt_ymd
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
        if (.not. present(opt_n)) call shr_sys_abort(trim(alarmName)//"invalid option:"//time_optNsteps)
        if (opt_n <=0) call shr_sys_abort(trim(alarmName)//" not valid opt_n")
        alarmInterval = alarmInterval*opt_n
    case (time_optEnd)
        call shr_sys_abort(subname//':end option'//trim(opt))
    case (time_optNSeconds)
        call ESMF_TimeIntervalSet(AlarmInterval, s=1, rc=rc)
        if(.not. present(opt_n))call shr_sys_abort(subname//":"//trim(opt)//' requires opt_n')
        if(opt_n <= 0)call shr_sys_abort(subname//":"//trim(opt)//' invalid opt_n')
        alarmInterval = alarmInterval*opt_n
    case (time_optNMinutes)
        call ESMF_TimeIntervalSet(alarmInterval, s=60, rc=rc)
        if(.not. present(opt_n))call shr_sys_abort(subname//':'//trim(opt)//'requires opt_n')
        if(opt_n <=0)call shr_sys_abort(subname//':'//trim(opt)//' invalid opt_n')
        alarmInterval = alarmInterval*opt_n 
    case (time_optNHours)
        call ESMF_TimeIntervalSet(alarmInterval, s=3600, rc=rc)
        if(.not. present(opt_n))call shr_sys_abort(subname//':'//trim(opt)//'requires opt_n')
        if(opt_n<=0)call shr_sys_abort(subname//':'//trim(opt)//' invalid opt_n')
        alarmInterval = alarmInterval*opt_n
    case (time_optNDays)
        call ESMF_TimeIntervalSet(alarmInterval, d=1, rc=rc)
        if(.not. present(opt_n))call shr_sys_abort(subname//':'//trim(opt)//'requires opt_n')
        if(opt_n<=0)call shr_sys_abort(subname//':'//trim(opt)//' invalid opt_n')
        alarmInterval = alarmInterval*opt_n
    case (time_optNMonths)
        call ESMF_TimeIntervalSet(alarmInterval, mm=1, rc=rc)
        if(.not. present(opt_n))call shr_sys_abort(subname//':'//trim(opt)//'requires opt_n')
        if(opt_n <= 0)call shr_sys_abort(subname//':'//trim(opt)//' invalid opt_n')
        alarmInterval = alarmInterval*opt_n
    case (time_optNYears)
        call ESMF_TimeIntervalSet(alarmInterval, yy=1, rc=rc)
        if(.not. present(opt_n))call shr_sys_abort(subname//':'//trim(opt)//'requires opt_n')
        if(opt_n <=0) call shr_sys_abort(subname//':'//trim(opt)//' invalid opt_n')
        alarmInterval=alarmInterval*opt_n
    case (time_optYearly)
        call ESMF_TimeIntervalSet(alarmInterval, yy=1, rc=rc)
        call ESMF_TimeSet(nextAlarm, yy=cyy, mm=1, dd=1, s=0, calendar=time_cal, rc=rc)
    case (time_optMonthly)
        call ESMF_TimeIntervalSet(alarmInterval, mm=1, rc=rc)
        call ESMF_TimeSet(nextAlarm, yy=cyy, mm=cmm, dd=1, s=0, calendar=time_cal, rc=rc)
    case (time_optDate)
        call ESMF_TimeIntervalSet(alarmInterval, yy=9999, rc=rc)
        if(.not. present(opt_ymd))call shr_sys_abort(subname//':'//trim(opt)//' requires opt_ymd')
        if(lymd <0 .or. ltod<0)then
            call shr_sys_abort(subname//':'//trim(opt)//'opt_ymd, opt_tod invalid')
        end if
        tmpCal = ESMF_CalendarCreate(time_cal_default, name='NOLEAP', rc=rc)
        call time_TimeYmdInit(nextAlarm, tmpCal,lymd, ltod,rc=rc, info="optDate")
        update_nextalarm = .false. 
    case default
        call shr_sys_abort(trim(alarmName)//" unkown opt")
    end select

    if(update_nextAlarm)then
        alarmTime = alarmTime - alarmInterval
        do while(alarmTime<=currTime)
            alarmTime= alarmTime + alarmInterval
        end do
    end if
    EAlarm = ESMF_AlarmCreate(name=alarmname, clock=EClock, ringTime=alarmTime, &
      ringInterval=alarmInterval, rc=rc)
    !if(rc/=0)then
    !    print *, 'in alarm Create:',alarmname, rc
    !end if
end subroutine time_alarmInit

subroutine time_alarmSetOn(EClock, alarmname)
  
    implicit none
    type(ESMF_Clock), intent(inout) :: EClock
    character(len=*), intent(in), optional :: alarmname

    type(ESMF_Alarm), pointer :: EAlarm
    type(ESMF_Alarm), pointer :: EAlarm_list(:)
    character(len=*), parameter :: subname = '(time_alarmSetOn)'
    character(len=*), parameter :: xalarm = 'unset'
    integer    :: AlarmCount
    character(SHR_KIND_CS) :: tempName
    logical    :: found
    logical    :: set
    integer    :: n
    integer    :: rc

    set = .false.
    call time_ClockGetInfo(EClock, AlarmCount=AlarmCount)
    allocate(EAlarm_list(AlarmCount))
    call ESMF_ClockGetAlarmList(EClock, alarmListFlag=ESMF_ALARMLIST_ALL,&
                   alarmList=EAlarm_list, alarmCount=AlarmCount, rc=rc)

    do n = 1, AlarmCount
        found = .false.
        if(present(alarmname))then
            call ESMF_AlarmGet(EAlarm_list(n), name=tempName)
            if(trim(tempName)==trim(alarmname)) found= .true.
        else
            found = .true.
        end if
        if(found)then
            set = .true.
            call ESMF_AlarmRingerOn(EAlarm_list(n), rc=rc)
        end if
    end do
    
    if(present(alarmname) .and. .not. set)then
        write(logUnit, *)subname, ' ERROR in alarmname ', trim(alarmname)
        call shr_sys_abort('ERROR: in alarmname')
    end if
    deallocate(EAlarm_list)

end subroutine time_alarmSetOn

subroutine time_alarmSetOff(EClock, alarmname)

    implicit none
    type(ESMF_Clock),           intent(inout)  :: EClock
    character(len=*), optional, intent(inout)  :: alarmname

    character(len=*), parameter :: subname = '(time_alarmSetOff)'
    character(len=*), parameter :: xalarm = 'unset'
    integer :: n
    integer :: rc
    logical :: found
    logical :: set
    character(SHR_KIND_CS) :: tempName
    type(ESMF_Alarm), pointer :: EAlarm
    type(ESMF_Alarm), pointer :: EAlarm_list(:)
    integer :: AlarmCount

    set = .false.
    call time_clockGetInfo(EClock, AlarmCount=alarmCount)
    
    allocate(EAlarm_list(alarmCount))  
    call ESMF_ClockGetAlarmList(EClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
           alarmList=EAlarm_list, alarmCount=AlarmCount, rc=rc)
    do n = 1, alarmCount
        found = .false.
        if(present(alarmname))then
            call ESMF_AlarmGet(EAlarm_list(n), name=tempName)
            if(trim(tempName)==trim(alarmname)) found = .true.
        else
            found = .true.
        end if
        if(found)then
            set = .true.
            call ESMF_AlarmRingerOff(EAlarm_list(n), rc=rc)
        end if
    end do
    if(present(alarmname) .and. .not. set)then
        write(logUnit, *) subname, ' ERROR in alarmname ', trim(alarmname)
        call shr_sys_abort('ERROR: in  alarmname')
    end if
    deallocate(EAlarm_list)
end subroutine time_alarmSetOff

logical function time_alarmIsOn(EClock, alarmname)

    implicit none
    
    type(ESMF_Clock),   intent(in) :: EClock
    character(len=*),   intent(in) :: alarmname

    !-------local------
    integer :: n
    integer :: rc
    logical :: found 
    logical :: set
    character(SHR_KIND_CS) :: name
    type(ESMF_Time)  :: ETime1, ETime2
    type(ESMF_Alarm), pointer :: EAlarm
    type(ESMF_Alarm), pointer :: EAlarm_list(:)
    integer   :: AlarmCount 

    time_alarmIsOn = .false.
    call time_ClockGetInfo(EClock, AlarmCount=AlarmCount)
    allocate(EAlarm_list(AlarmCount))
    call ESMF_ClockGetAlarmList(EClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
           alarmList=EAlarm_list, alarmCount=AlarmCount, rc=rc)
    do n = 1, AlarmCount
         name = trim('unset')
         call ESMF_AlarmGet(EAlarm_list(n), name=name) 
         if (trim(name)==trim(alarmname))then
             found = .true.
             time_alarmIsOn = ESMF_AlarmIsRinging(alarm=EAlarm_list(n), rc=rc)
             if (trim(alarmname)==trim(alarm_datestop_name))then
                 call ESMF_ClockGet(EClock, CurrTime=ETime1, rc=rc)
                 call ESMF_AlarmGet(EAlarm_list(n), RingTime=ETime2, rc=rc)
                 if(ETime1>=ETime2) time_alarmIsOn = .true.
             end if
          end if
     end do

     if(.not. found) then
         write(logunit, *) "ERROR alarm not valid"
         call shr_sys_abort("ERROR alarm not valid")
     end if
     deallocate(EAlarm_list)

end function time_alarmIsOn

subroutine time_TimeYmdInit(time, cal, ymd, tod, rc, info)

    implicit none
    type(ESMF_Time),          intent(inout)  :: time
    type(ESMF_Calendar),      intent(in)     :: cal
    integer,                  intent(in)     :: ymd
    integer,      optional,   intent(in)     :: tod
    integer,      optional,   intent(inout)  :: rc
    character(*), optional,   intent(in)     :: info

    integer :: y, m, d, sec
    integer :: lymd  
    type(ESMF_Time) :: ltime 
 
    lymd = ymd
    y = lymd/10000
    lymd = mod(lymd, 10000)
    m = lymd/100
    lymd = mod(lymd, 100)
    d = lymd
    sec = 0
    if(present(tod))then
        sec = tod
    end if
    if((sec<0) .or. (lymd<0)) then
        write(logUnit, *)  ":ERROR ymd or tod" 
        call shr_sys_abort("ERROR ymd or tod")
    end if
  
   
    call ESMF_TimeSet(time, yy=y, mm=m, dd=d, s=sec, calendar=cal, rc=rc)

end subroutine time_TimeYmdInit


subroutine time_TimeYmdGet(ETime, offset, ymd, tod)

    implicit none
    
    type(ESMF_Time),      intent(in)     :: ETime
    integer,  optional,   intent(in)     :: offset
    integer,  optional,   intent(inout)  :: ymd
    integer,  optional,   intent(inout)  :: tod

    character(len=*),  parameter   :: subname = "(time_TimeYmdGet)"
    type(ESMF_Time)          :: ETimeAdd
    type(ESMF_TimeInterval)  :: ETimeOff
    integer                  :: year
    integer                  :: month
    integer                  :: day
    integer                  :: sec
    integer                  :: rc


    ETimeAdd = ETime
    if(present(offset))then
        if(offset > 0)then
            call ESMF_TimeIntervalSet(ETimeOff, s=offset, rc=rc)
            ETimeAdd = ETime + ETimeOff
        else if(offset < 0)then
            call ESMF_TimeIntervalSet(ETimeOff, s=-offset, rc=rc)
            ETimeAdd = ETime - ETimeOff
        end if
    end if
    call ESMF_TimeGet(ETimeAdd, yy=year, mm=month, dd=day, s=sec, rc=rc)

    if(present(ymd))then
        ymd = year*10000+month*100+day
    end if

    if(present(tod))then
        tod = sec
    end if

end subroutine time_TimeYmdGet


integer function get_interval(time_interval)
    ! no only support gregory calender
    implicit none
    type(ESMF_TimeInterval), intent(in) :: time_interval
    integer :: sec
    integer :: rc     

    call ESMF_TimeIntervalGet(time_interval, s=sec, rc=rc)    
    get_interval = sec

end function

end module time_mod 
