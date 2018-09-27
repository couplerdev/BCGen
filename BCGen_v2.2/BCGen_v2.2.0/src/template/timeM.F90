module timeM
    
    implicit none
    type Clock
        integer :: seconds
        integer :: minites
        integer :: hours
        integer :: days
        !integer :: months
        !integer :: years
        integer :: interval = 1
    end type Clock
    integer :: total_days =  1 

    
    integer :: a_seconds = 0
    integer :: a_minutes = 0
    integer :: a_hours = 0
    integer :: a_days = 0
    
    integer :: b_seconds = 1
    integer :: b_minutes = 0
    integer :: b_hours = 0
    integer :: b_days = 0


    public :: clock_init
    public :: clock_advance
    public :: triger
contains 

subroutine clock_init(EClock,
                     a, 
                     b, 
                     interval)
 
    implicit none
    type(Clock), intent(inout)    :: EClock
    type(Clock), intent(inout)    :: EClock_a
    type(Clock), intent(inout)    :: EClock_b
    integer, optional, intent(in) :: interval
 
    EClock%seconds = 0 
    EClock%minites = 0
    EClock%hours   = 0
    EClock%days    = 0
    EClock_a%seconds = a_seconds
    EClock_a%minutes = a_minutes
    EClock_a%hours = a_hours
    EClock_a%days = a_days
    EClock_a%interval = EClock_a%seconds+ (EClock_a%minutes +\
                   (EClock_a%hours+EClock_a%days*24)*60)*60
    EClock_b%seconds = b_seconds
    EClock_b%minutes = b_minutes
    EClock_b%hours = b_hours
    EClock_b%days = b_days
    EClock_b%interval = EClock_b%seconds+ (EClock_b%minutes +\
                   (EClock_b%hours+EClock_b%days*24)*60)*60
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
   EClock%minites = EClock%minites + plus
   if(EClock%minites < 60)return 
    
   plus = EClock%minites/60
   EClock%minites = mod(EClock%minites, 60)
   EClock%hours   = EClock%hours + plus
   if(EClock%hours < 24)return 

   plus = EClock%hours/24
   EClock%hours = mod(EClock%hours, 24)
   EClock%days  = EClock%days + plus

end subroutine clock_advance

subroutine triger(EClock,EClock_s ,run)

    implicit none
    type(Clock), intent(in)       :: EClock
    type(Clock), intent(in)       :: EClock_s
    logical, intent(inout)        :: run
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
              mod(EClock%minites*tmp_m, time_run) + &
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

    write(*,*) EClock%days, "days:", EClock%hours, "hours:", EClock%minites, "minites:",&
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
    write(tmpCh,*)EClock%minites
    info = info//tmpCh//"minites:  "
    
    tmpCh = ""
    write(tmpCh,*)EClock%seconds
    info = info//tmpCh//"seconds:  "

end subroutine clock_info

end module timeM
