module base_file
use logUtil
use base_sys
    implicit none

    character(len=20) :: logFile = "basecplLog"
    integer, parameter :: base_file_maxUnit = 99
    integer, parameter :: base_file_minUnit = 10
    logical, save ::UnitStatus(0:base_file_maxUnit) = .false.
    public :: log_init
    public :: base_file_getUnit    

contains


subroutine log_init(logFile, logUnit)

    implicit none
    character(len=*),    intent(in) :: logFile
    integer,             intent(in) :: logUnit
    logical                         :: stats
    character(*), parameter   ::subname = 'log_init'

    inquire(file=logFile, exist=stats)
    if (.not. stats) then
        call base_sys_abort(subname//":File not exsits")
    end if
    open(unit=logUnit, file=logFile)
    
end subroutine log_init

subroutine log_final(logFile, logUnit)

    implicit none
    character(len=*),   intent(in)  :: logFile
    integer,            intent(in)  :: logUnit
    close(unit=logUnit)

end subroutine log_final

integer function base_file_getUnit(unitn)

    implicit none
    integer,  intent(in), optional ::unitn

    integer :: n
    logical :: is_open
    character(*),  parameter ::subname = '(base_file_getUnit)'  

     if(present(unitn))then
         inquire(unitn, opened=is_open)
         if(unitn < 0 .or. unitn > base_file_maxUnit)then
             write(logUnit, *)'invalid unit number req:', unitn
             call base_sys_abort('ERROR: bad unit number')
         else if (is_open .or. UnitStatus(unitn) .or. unitn==0 .or. unitn ==5 .or. unitn==6)then
             write(logUnit, *)'unit number', unitn,'already in use'
             call base_sys_abort('ERROR: unit number in use')
         else
             base_file_getUnit = unitn
             UnitStatus(unitn)    = .true.
             return
         end if
     else
         do n=base_file_maxUnit, base_file_minUnit, -1
             inquire(n, opened=is_open)
             if(n==5 .or. n==6 .or. is_open)then
                 cycle
             end if
             if(.not. UnitStatus(n))then
                 base_file_getUnit = n
                 UnitStatus(n)     = .true.
                 return
             end if
         end do
     end if

     call base_sys_abort(subname//': Error: no available unit')

end function base_file_getUnit

end module base_file
