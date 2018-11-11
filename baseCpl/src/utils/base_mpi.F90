module base_mpi
use mpi
use logUtil
!----------------------------------------------------------
!     This moudle is used for wrapped mpi API, making it is
!     easy to  use. 
!----------------------------------------------------------

implicit none
    public :: base_mpi_bcast
    public :: base_mpi_abort
    public :: base_mpi_chkerr

    interface base_mpi_bcast ;module procedure &
       base_mpi_bcastInt, &
       base_mpi_bcastLogical, &
       base_mpi_bcastChar, &
       base_mpi_bcastR8
    end interface

contains

subroutine base_mpi_chkerr(rcode, string)

    implicit none
    integer,       intent(in) :: rcode
    character(*),  intent(in) :: string
    !--local----
    character(*), parameter    :: subName=  '(base_mpi_chkerr)'
    character(MPI_MAX_ERROR_STRING)  :: lString
    integer :: length
    integer :: ierr

    if(rcode/=MPI_SUCCESS)then
        call MPI_ERROR_STRING(rcode,lString, length, ierr)
        write(logUnit, *) trim(subName),"", lString(1:length)
        call base_mpi_abort(string,rcode)
    end if
end subroutine base_mpi_chkerr

subroutine base_mpi_abort(string, rcode)
    implicit none

    character(*), optional, intent(in) :: string
    integer, optional, intent(in)      :: rcode
    
    character(*), parameter :: subName = '(base_mpi_abort)'
    integer :: ierr
    integer :: rc

    if(present(string) .and. present(rcode))then
        write(logUnit,*) trim(subName), ":", trim(string), rcode
    end if

    if(present(rcode))then
        rc = rcode
    else
        rc = 1001
    end if
    call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)

end subroutine base_mpi_abort

subroutine base_mpi_barrier(comm, string)

    implicit none
    integer,   intent(in) :: comm
    character(*), optional, intent(in) :: string
    !---local---
    character(*), parameter    :: subName = '(base_mpi_barrier)'
    integer :: ierr     

    call MPI_BARRIER(comm, ierr)
    if(present(string))then
        call base_mpi_chkerr(ierr, subName//trim(string))
    else
        call base_mpi_chkerr(ierr, subName)
    end if

end subroutine base_mpi_barrier

subroutine base_mpi_bcastInt(msg, comm, string, pebcast, root)
    implicit none
    integer, intent(inout) :: msg
    integer, intent(in)    :: comm
    character(*), optional, intent(in) :: string
    integer,      optional, intent(in) :: pebcast
    integer,      optional, intent(in) :: root

    character(*), parameter   :: subName = '(shr_mpi_bcastInt)'
    integer :: ierr
    integer :: lsize
    integer :: local_root

    lsize = 1
    local_root = 0
    if(present(root))local_root = root

    call MPI_BCAST(msg, lsize, MPI_INTEGER, local_root, comm, ierr)
    if(present(string))then
        call base_mpi_chkerr(ierr, subName//trim(string))
    else
        call base_mpi_chkerr(ierr, subName)
    end if

end subroutine base_mpi_bcastInt


subroutine base_mpi_bcastLogical(msg, comm, string, root)

    implicit none
    logical,  intent(inout) :: msg
    integer,  intent(in)    :: comm
    character(*), optional, intent(in) :: string
    integer,      optional, intent(in) :: root

    character(*), parameter   :: subName = '(base_mpi_bcastLogical)'
    integer    :: ierr
    integer    :: lsize
    integer    :: local_root 
   
    lsize = 1
    local_root = 0
    if(present(root)) local_root = root
    call MPI_BCAST(msg, lsize, MPI_LOGICAL, local_root, comm, ierr)

    if(present(string))then
        call base_mpi_chkerr(ierr, subName//trim(string))
    else
        call base_mpi_chkerr(ierr, subName)
    end if


end subroutine base_mpi_bcastLogical

subroutine base_mpi_bcastChar(msg, comm, string, root)
    
    implicit none
    character(len=*), intent(inout)  :: msg
    integer,          intent(in)     :: comm
    character(*), optional, intent(in)  :: string
    integer,      optional, intent(in)  :: root

    character(len=*), parameter   :: subName = '(shr_mpi_bcastChar)'
    integer :: ierr
    integer :: lsize
    integer :: local_root
 
    lsize = len(msg)
    local_root = 0
    if(present(root)) local_root = root

    call MPI_BCAST(msg, lsize, MPI_CHARACTER, local_root, comm, ierr)
    if(present(string))then
        call base_mpi_chkerr(ierr, subName//trim(string))
    else 
        call base_mpi_chkerr(ierr, subName)
    end if

end subroutine base_mpi_bcastChar

subroutine base_mpi_bcastR8(msg, comm, string, root)

    implicit none
    real(8),    intent(inout)  :: msg
    integer, intent(in)     :: comm
    character(len=*), optional, intent(in)  :: string
    integer,          optional, intent(in)  :: root

    character(len=*), parameter  :: subName = '(base_mpi_bcastR8)'
    integer :: ierr
    integer :: lsize
    integer :: local_root
    
    lsize = 1
    local_root = 0
    if(present(root))local_root = root

    call MPI_BCAST(msg, lsize, MPI_REAL8, local_root, comm, ierr)
    if(present(string))then
        call base_mpi_chkerr(ierr, subName//trim(string))
    else
        call base_mpi_chkerr(ierr, subName)
    end if

end subroutine base_mpi_bcastR8


end module base_mpi
