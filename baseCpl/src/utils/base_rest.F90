module base_rest_mod

   use base_sys_mod,    only : base_sys_abort, base_sys_flush
   use base_mpi_mod,    only : base_mpi_bcast
   use base_file_mod,   only : base_file_getUnit, base_file_freeUnit
   use mct_mod
   use ESMF

   use global_var
   use proc_def
   use timeCesm
   use base_io_mod

   implicit none
   
   private

   public :: base_rest_read
   public :: base_ rest_write

   ! local
   logical :: iamin_CPLID
   integer :: mpicom_GLOID
   integer :: mpicom_CPLID
   integer, parameter :: r8 = 8
   integer, parameter :: CS = 100
   ! prognostic
   ! logical :: atm_prognostic
   ! logical :: ocn_prognostic

   
contains

subroutine base_rest_read(metaData, rest_file)

    implicit none
    type(Meta),    intent(in) :: metaData
    character(*),  intent(in) :: rest_file
    integer :: n, n1, n2, n3
    real(r8), allocatable :: ds(:)
    real(r8), allocatable :: ns(:)
    character(CS)         :: string
    integer ::  ierr
    type(gsMap),  pointer :: gsmap
    character(len=*), parameter :: subname = "(base_rest_read)"

    imain_CPLID = metaData%iamin_CPLID
    mpicom_GLOID = metaData%mpicom_gloid
    mpicom_CPLID = metaData%mpicom_cplid
    ! prognostic
    ! 
    !

    if(iamin_CPLID)then
        call base_io_read(rest_file, metaData%atm%comp_gsmap, atm2x_atmx, 'atm2x_atmx' )
        call base_io_read(rest_file, metaData%ocn%comp_gsmap, ocn2x_ocnx, 'ocn2x_ocnx')
    end if

    n = size(budg_dataG)  ! ?
    allocate(ds(n), ns(n))
    call base_io_read(rest_file, ds, 'budg_dataG')
    call base_io_read(rest_file, ns, 'budg_ns')

    n = 0
    do n1 = 1, size(budg_dataG, dim=1)
    do n2 = 1, size(budg_dataG, dim=2)
    do n3 = 1, size(budg_dataG, dim=3)
        n = n + 1
        budg_dataG(n1, n2, n3) = ds(n)
        budg_ns(n1, n2, n3) = ns(n)
    end do
    end do
    end do

    deallocate(ds, ns)
    
 
end subroutine base_rest_read

subroutine base_rest_write(metaData, EClock_d, SyncClock)



end subroutine base_rest_write







end module base_rest_mod
