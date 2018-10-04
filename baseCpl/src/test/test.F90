program test
use base_sys, only : base_sys_abort
#include<mpif.h>

integer :: ierr
call MPI_Init(ierr)


call base_sys_abort()


end program test
