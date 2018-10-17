module timeM
    
    implicit none
    type Clock
        integer :: seconds
        integer :: minutes
        integer :: hours
        integer :: days
        !integer :: months
        !integer :: years
        integer :: interval = 1
    end type Clock
    integer :: total_days =  1 

    #for $model in $proc_cfgs
    #set $model_name = $model.name
    #set $model_time = $model.Time
    #set $model_seconds = $model.Time["sec"]
    #set $model_minutes = $model.Time["minute"]
    #set $model_hours = $model.Time["h"]
    #set $model_days = $model.Time["d"]
    
    integer :: ${model_name}_seconds = ${model_seconds}
    integer :: ${model_name}_minutes = ${model_minutes}
    integer :: ${model_name}_hours = ${model_hours}
    integer :: ${model_name}_days = ${model_days}
    #end for


    public :: clock_init
    public :: clock_advance
    public :: triger
contains 

subroutine clock_init(EClock, &
                     #for $model in $proc_cfgs 
                     #set $model_name = $model.name 
                     EClock_${model_name},& 
                     #end for
                     interval)
 
    implicit none
    type(Clock), intent(inout)    :: EClock
    #for $model in $proc_cfgs
    #set $model_name = $model.name
    type(Clock), intent(inout)    :: EClock_${model_name}
    #end for
    integer, optional, intent(in) :: interval
 
    EClock%seconds = 0 
    EClock%minutes = 0
    EClock%hours   = 0
    EClock%days    = 0
    #for $model in $proc_cfgs
    #set $model_name = $model.name
    EClock_${model_name}%seconds = ${model_name}_seconds
    EClock_${model_name}%minutes = ${model_name}_minutes
    EClock_${model_name}%hours = ${model_name}_hours
    EClock_${model_name}%days = ${model_name}_days
    EClock_${model_name}%interval = EClock_${model_name}%seconds+ (EClock_${model_name}%minutes +\
                   (EClock_${model_name}%hours+EClock_${model_name}%days*24)*60)*60
    #end for
    !EClock%months  = 0 
    !EClock%years   = 0 ! from base so far
    EClock%interval = interval

end subroutine

subroutine clock_advance(EClock)

   implicit none
   type(Clock), intent(inout) :: EClock
   integer  :: plus
   
   EClock%seconds = EClock%seconds + EClock%interval
   if(EClock%seconds < 60)return 
   
   plus = EClock%seconds/60
   EClock%seconds = mod(EClock%seconds, 60) 
   EClock%minutes = EClock%minutes + plus
   if(EClock%minutes < 60)return 
    
   plus = EClock%minutes/60
   EClock%minutes = mod(EClock%minutes, 60)
   EClock%hours   = EClock%hours + plus
   if(EClock%hours < 24)return 

   plus = EClock%hours/24
   EClock%hours = mod(EClock%hours, 24)
   EClock%days  = EClock%days + plus

end subroutine clock_advance

subroutine triger(EClock,EClock_s ,run, flag_name)

    implicit none
    type(Clock), intent(in)       :: EClock
    type(Clock), intent(in)       :: EClock_s
    logical, intent(inout)        :: run
    character(*),  intent(in)     :: flag_name
    integer   :: time_run
    integer   :: tmp_m
    integer   :: tmp_h
    integer   :: tmp_d
    integer   :: tmp_mod

    run = .false.
    if(flag_name=='stop_clock')then
        if(EClock%days >= total_days)then
            run = .false.
        end if
    end if
    time_run = EClock_s%interval
    tmp_m = mod(60, time_run)
    tmp_h = mod(60*60, time_run)
    tmp_d = mod(tmp_h*24, time_run)
    tmp_mod = mod(EClock%seconds, time_run) +&
              mod(EClock%minutes*tmp_m, time_run) + &
              mod(EClock%hours*tmp_h, time_run) + &
              mod(EClock%days*tmp_d, time_run)
    tmp_mod = mod(tmp_mod, time_run)
    if(tmp_mod == 0)then
        run = .true.
    end if

end subroutine triger

subroutine clock_print(EClock)

    implicit none
    type(Clock), intent(in) :: EClock

    write(*,*) EClock%days, "days:", EClock%hours, "hours:", EClock%minutes, "minites:",&
               EClock%seconds, "seconds"

end subroutine clock_print

subroutine clock_info(EClock, info)

    implicit none
    type(Clock),  intent(inout)      :: EClock
    character(len=*), intent(inout)  :: info
    character(len=20)                :: tmpCh  
   
    write(tmpCh,*)EClock%days
    info = info//tmpCh//"days:  "
    
    tmpCh = ""
    write(tmpCh,*)EClock%hours
    info = info//tmpCh//"hours:  "
 
    tmpCh = ""
    write(tmpCh,*)EClock%minutes
    info = info//tmpCh//"minutes:  "
    
    tmpCh = ""
    write(tmpCh,*)EClock%seconds
    info = info//tmpCh//"seconds:  "

end subroutine clock_info

end module timeM
