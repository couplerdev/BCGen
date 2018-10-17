module timeM
use ESMF

    implicit none
    type Clock
        type(ESMF_Time) :: time
        type(ESMF_TimeInterval) :: interval
    end type Clock

    !------------------------------------------------
    ! initial Clock structure by ESMF Time class
    ! and ESMF interval class
    !------------------------------------------------
    type(ESMF_Time) :: ESMFClock_atm
    type(ESMF_Time) :: ESMFClock_ocn
    type(ESMF_Time) :: ESMFClock_drv
    
    type(ESMF_TimeInterval) :: atm_interval
    type(ESMF_TimeInterval) :: ocn_interval
   
    integer :: base_atm_yy
    integer :: base_atm_mm
    integer :: base_atm_dd
    integer :: base_atm_h
    integer :: base_atm_m
    integer :: base_atm_s

    integer :: base_ocn_yy
    integer :: base_ocn_mm
    integer :: base_ocn_dd
    integer :: base_ocn_h
    integer :: base_ocn_m
    integer :: base_ocn_s

    integer :: interval_atm_d
    integer :: interval_atm_h
    integer :: interval_atm_m
    integer :: interval_atm_s

    integer :: interval_ocn_d
    integer :: interval_ocn_h
    integer :: interval_ocn_m
    integer :: interval_ocn_s

    integer :: interval_drv_d
    integer :: interval_drv_h
    integer :: interval_drv_m
    integer :: interval_drv_s

    public :: clock_init
    public :: clock_advance
    public :: triger
    private :: intervalCheck

contains

subroutine clock_init(EClock_drv, &
                      EClock_atm, &
                      EClock_ocn)
    implicit none
    type(Clock), intent(inout)  :: EClock_drv
    type(Clock), intent(inout)  :: EClock_atm
    type(Clock), intent(inout)  :: EClock_ocn
    integer :: clockCounts = 2
    type(ESMF_Time), allocatable :: timers(:)
    integer :: rc
    integer :: iter = 1, it
    type(ESMF_Time) :: minTimer

    allocate(timers(clockCounts))

    call ESMF_TimeSet(EClock_atm%time, yy=base_atm_yy, mm=base_atm_mm, dd=base_atm_dd, &
                      h=base_atm_h, m=base_atm_m, s=base_atm_s, rc=rc)
    timers(iter) = EClock_atm%time
    iter +=1

    call ESMF_TimeSet(EClock_ocn%time, yy=base_ocn_yy, mm=base_ocn_yy, dd=base_ocn_dd, &
                      h=base_ocn_h, m=base_ocn_m, s=base_ocn_s, rc=rc)
    timers(iter) = EClock_ocn%time
    minTimer = timers(iter)
    do it = 1, iter
        if (minTimer>timers(it))then
            minTimer = timers(it)
        end if
    end do
    EClock_drv%time = minTimer
    
    call ESMF_TimeIntervalSet(EClock_atm%interval, d=interval_atm_d, h=interval_atm_h, &
                            m=interval_atm_m, s=interval_atm_s, rc=rc)
 
    call ESMF_TimeIntervalSet(EClock_ocn%interval, d=interval_ocn_d, h=interval_ocn_h, &
                            m=interval_ocn_m, s=interval_ocn_s, rc=rc)

    call intervalCheck(EClock_atm, EClock_ocn)

    call ESMF_TimeIntervalSet(EClock_drv%interval, d=interval_drv_d, h=interval_drv_h, &
                            m=interval_ocn_m, s=interval_ocn_s, rc=rc)
end subroutine clock_init

subroutine clock_advance(EClock_drv, EClock_atm, EClock_ocn)
    implicit none
    type(Clock),  intent(inout) :: EClock_drv
    type(Clock),  intent(inout) :: EClock_atm
    type(Clock),  intent(inout) :: EClock_ocn 

    integer :: step =1 
    do it = 1,  

end subroutine clock_advance

subroutine 

subroutine intervalCheck(EClock_atm, EClock_ocn)
    implicit none
    type(Clock), intent(inout) :: EClock_atm
    type(Clock), intent(inout) :: EClock_ocn
    
    integer :: interval_atm_sec
    integer :: interval_ocn_sec
    integer :: interval_drv_sec

    integer :: intervalCounts=2
    integer, allocatable :: intervals(:)
    integer :: iter = 1
    integer :: it
    integer :: last

    interval_atm_sec = ((interval_atm_d*24+interval_atm_h)*60+interval_atm_m)*60+interval_atm_s
    interval_ocn_sec = ((interval_ocn_d*24+interval_ocn_h)*60+interval_ocn_m)*60+interval_atm_s
    intervals(iter) = interval_atm_sec
    intervals(iter) = interval_ocn_sec
    last = intervals(iter)
    do it=1, iter
        last = gcd(intervals(it), last)
    end do
    interval_drv_s = mod(last, 60)
    last /= 60
    interval_drv_m = mod(last, 60)
    last /= 60
    interval_drv_h = mod(last, 24)
    last /= 24
    interval_drv_d = last

end subroutine intervalCheck


end module timeM
