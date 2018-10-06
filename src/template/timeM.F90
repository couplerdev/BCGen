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

    
    integer :: glc_seconds = 
    integer :: glc_minutes = 0
    integer :: glc_hours = 
    integer :: glc_days = 
    
    integer :: ocn_seconds = 
    integer :: ocn_minutes = 0
    integer :: ocn_hours = 
    integer :: ocn_days = 
    
    integer :: atm_seconds = 
    integer :: atm_minutes = 0
    integer :: atm_hours = 
    integer :: atm_days = 
    
    integer :: ice_seconds = 
    integer :: ice_minutes = 0
    integer :: ice_hours = 
    integer :: ice_days = 
    
    integer :: rof_seconds = 
    integer :: rof_minutes = 0
    integer :: rof_hours = 
    integer :: rof_days = 
    
    integer :: wav_seconds = 
    integer :: wav_minutes = 0
    integer :: wav_hours = 
    integer :: wav_days = 
    
    integer :: lnd_seconds = 
    integer :: lnd_minutes = 0
    integer :: lnd_hours = 
    integer :: lnd_days = 


    public :: clock_init
    public :: clock_advance
    public :: triger
contains 

subroutine clock_init(EClock,
                     glc, 
                     ocn, 
                     atm, 
                     ice, 
                     rof, 
                     wav, 
                     lnd, 
                     interval)
 
    implicit none
    type(Clock), intent(inout)    :: EClock
    type(Clock), intent(inout)    :: EClock_glc
    type(Clock), intent(inout)    :: EClock_ocn
    type(Clock), intent(inout)    :: EClock_atm
    type(Clock), intent(inout)    :: EClock_ice
    type(Clock), intent(inout)    :: EClock_rof
    type(Clock), intent(inout)    :: EClock_wav
    type(Clock), intent(inout)    :: EClock_lnd
    integer, optional, intent(in) :: interval
 
    EClock%seconds = 0 
    EClock%minites = 0
    EClock%hours   = 0
    EClock%days    = 0
    EClock_glc%seconds = glc_seconds
    EClock_glc%minutes = glc_minutes
    EClock_glc%hours = glc_hours
    EClock_glc%days = glc_days
    EClock_glc%interval = EClock_glc%seconds+ (EClock_glc%minutes +\
                   (EClock_glc%hours+EClock_glc%days*24)*60)*60
    EClock_ocn%seconds = ocn_seconds
    EClock_ocn%minutes = ocn_minutes
    EClock_ocn%hours = ocn_hours
    EClock_ocn%days = ocn_days
    EClock_ocn%interval = EClock_ocn%seconds+ (EClock_ocn%minutes +\
                   (EClock_ocn%hours+EClock_ocn%days*24)*60)*60
    EClock_atm%seconds = atm_seconds
    EClock_atm%minutes = atm_minutes
    EClock_atm%hours = atm_hours
    EClock_atm%days = atm_days
    EClock_atm%interval = EClock_atm%seconds+ (EClock_atm%minutes +\
                   (EClock_atm%hours+EClock_atm%days*24)*60)*60
    EClock_ice%seconds = ice_seconds
    EClock_ice%minutes = ice_minutes
    EClock_ice%hours = ice_hours
    EClock_ice%days = ice_days
    EClock_ice%interval = EClock_ice%seconds+ (EClock_ice%minutes +\
                   (EClock_ice%hours+EClock_ice%days*24)*60)*60
    EClock_rof%seconds = rof_seconds
    EClock_rof%minutes = rof_minutes
    EClock_rof%hours = rof_hours
    EClock_rof%days = rof_days
    EClock_rof%interval = EClock_rof%seconds+ (EClock_rof%minutes +\
                   (EClock_rof%hours+EClock_rof%days*24)*60)*60
    EClock_wav%seconds = wav_seconds
    EClock_wav%minutes = wav_minutes
    EClock_wav%hours = wav_hours
    EClock_wav%days = wav_days
    EClock_wav%interval = EClock_wav%seconds+ (EClock_wav%minutes +\
                   (EClock_wav%hours+EClock_wav%days*24)*60)*60
    EClock_lnd%seconds = lnd_seconds
    EClock_lnd%minutes = lnd_minutes
    EClock_lnd%hours = lnd_hours
    EClock_lnd%days = lnd_days
    EClock_lnd%interval = EClock_lnd%seconds+ (EClock_lnd%minutes +\
                   (EClock_lnd%hours+EClock_lnd%days*24)*60)*60
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
