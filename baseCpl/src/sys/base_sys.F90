module base_sys
    implicit none
include "mpif.h"    
    private 
    
    public :: base_sys_abort

contains 

subroutine base_sys_abort()
    implicit none
    integer :: rc
    integer :: ierr

    call MPI_Abort(MPI_COMM_WORLD, rc, ierr)


end subroutine base_sys_abort




end module base_sys
