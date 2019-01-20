program main

use comp_domain
use mct_mod
use mpi
implicit none
    integer :: ierr
    type(gsMap) :: my_gsmap
    type(gGrid) :: domain
    call MPI_Init(ierr)
    call atm_domain_init(MPI_COMM_WORLD,my_gsmap, domain )
    write(*,*)'domain initiated'
    call MPI_finalize(ierr)

    

end program main
