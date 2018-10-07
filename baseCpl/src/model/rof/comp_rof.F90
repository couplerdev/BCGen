module comp_rof
use mct_mod
use timeM
use proc_def
use logUtil, only: logUnit
use base_fields, only: flds_c2x => flds_rof2x_fields, &
                       flds_x2c => flds_x2rof_fields
    implicit none
     integer :: comp_id

     integer, parameter :: CXX = 4096
     character(CXX) :: fld_ar = ""
     character(CXX) :: fld_ai = ""
     character(*), parameter :: model_name = "rof"

     public :: rof_init_mct
     public :: rof_run_mct
     public :: rof_final_mct

contains

subroutine rof_init_mct(my_proc, ID, EClock, gsMap_rofrof,&
        rof2x_rofrof, x2rof_rofrof, domain, ierr)
     implicit none
     type(proc), intent(inout)      :: my_proc
     integer, intent(in)            :: ID
     type(Clock), intent(in)        :: EClock
     type(gsMap), intent(inout)     :: gsMap_rofrof
     type(AttrVect), intent(inout)  :: rof2x_rofrof
     type(AttrVect), intent(inout)  :: x2rof_rofrof
     type(gGrid),    intent(inout)  :: domain
     integer, intent(inout) :: ierr
     integer :: local_comm, i

     logical :: first_time = .true.
     
     integer :: ngy, ngx
     integer :: nlseg
     integer :: llseg
     integer :: nproc

     integer, allocatable :: start(:)
     integer, allocatable :: length(:)
     integer              :: root = 0
     integer              :: comm_rank
     integer              :: comm_size
     integer              :: lsize, gsize

     character(*), parameter :: F00 ="('(rof_init_mct) ',8a)"
     character(*), parameter :: F01 ="('(rof_init_mct) ',a,4i8)"
     character(*), parameter :: F02 ="('(rof_init_mct) ',a,4es13.6)"
     character(*), parameter :: F03 ="('(rof_init_mct) ',a,i8,a)"
     character(*), parameter :: F90 ="('(rof_init_mct) ',73('='))"
     character(*), parameter :: F91 ="('(rof_init_mct) ',73('-'))"
     character(*), parameter :: subName = "(rof_init_mct)"

     local_comm = my_proc%comp_comm(ID)
     call mpi_comm_rank(local_comm, comm_rank, ierr)
     call mpi_comm_size(local_comm, comm_size, ierr)
 
     ngx = 10
     ngy = 10
     nlseg = 1
     nproc = comm_size

     if(comm_rank == root) then
         write(logUnit, *) 'read in xrof input from file = xrof_in'
         write(logUnit, F00)
         write(logUnit, F00) '          Model :  ', trim(model_name)
         write(logUnit, F01) '            NGX :  ', ngx
         write(logUnit, F01) '            NGY :  ', ngy
         write(logUnit, F03) '  Decomposition :  ', 0
         write(logUnit, F03) '  Num pes in X  :  ', nproc
         write(logUnit, F03) '  Num of local Segment :  ', nlseg
         write(logUnit, F00)
     end if

     !------------------------------------
     ! init time manager 
     !------------------------------------


     !----------------------------------
     !  init mct grid 
     !----------------------------------
     allocate(start(nlseg))
     allocate(length(nlseg))
  
     gsize = my_proc%rof_gsize
     lsize = gsize / nproc
     llseg = lsize / nlseg

     do i = 1, nlseg
         start(i) = (comm_rank*lsize) + (i-1)*llseg
         length(i) = llseg
     end do

     !--------------------------------------
     !  init MCT global seg map
     !--------------------------------------
     
     call gsMap_init(gsMap_rofrof, start, length, root, local_comm, ID)
     
     !--------------------------------------
     ! init MCT Domain
     !--------------------------------------
     
     call rof_domain_init(local_comm, gsMap_rofrof, domain) 
 
     !---------------------------------------
     ! init AttrVect
     !--------------------------------------
     call avect_init(rof2x_rofrof, rList=flds_c2x, lsize=lsize)
     call avect_init(x2rof_rofrof, rList=flds_x2c, lsize=lsize)

     call avect_zero(rof2x_rofrof) 
     call avect_zero(x2rof_rofrof)


end subroutine rof_init_mct


subroutine rof_run_mct(my_proc, ID, EClock, rof2x_rofrof, x2rof_rofrof, ierr)
    
    implicit none
    type(proc), intent(inout)      :: my_proc
    integer,    intent(in)         :: ID
    type(Clock), intent(in)        :: EClock
    type(AttrVect), intent(inout)  :: rof2x_rofrof
    type(AttrVect), intent(inout)  :: x2rof_rofrof
    integer,        intent(inout)  :: ierr

    integer :: comm_rank, i
    integer :: av_lsize, n_rflds, n_iflds
    integer :: n, nf

    call mpi_comm_rank(my_proc%comp_comm(ID), comm_rank, ierr)
     
    av_lsize = avect_lsize(rof2x_rofrof)
    n_rflds = avect_nRattr(x2rof_rofrof)
    n_iflds = avect_nRattr(x2rof_rofrof)
   
    do nf=1, n_rflds
        do n = 1, av_lsize
            rof2x_rofrof%rAttr(nf, n) = (nf*100)
        end do
    end do

end subroutine rof_run_mct


subroutine rof_final_mct()

     write(logUnit, *)"rof final"
       
end subroutine rof_final_mct

subroutine rof_domain_init(mpicomm, gsMap_rofrof, domain)

    implicit none
    integer,       intent(in)    :: mpicomm
    type(gsMap),   intent(in)    :: gsMap_rofrof
    type(gGrid),   intent(inout) :: domain

    real(8), pointer :: data(:)
    integer, pointer :: idata(:)
    integer :: ierr, lsize

    call gGrid_init(GGrid=domain, &
         CoordChars=trim('x:y:z'), &
         otherchars=trim('lat:lon:area:frac:mask:aream'), &
         lsize=gsMap_lsize(gsMap_rofrof, mpicomm))
    call avect_zero(domain%data)
 
    lsize = gsMap_lsize(gsMap_rofrof, mpicomm)

    allocate(data(lsize))
    allocate(idata(lsize))
    data(:) = -9999.0
    call gGrid_importRAttr(domain, "lat", data, lsize)
    call gGrid_importRAttr(domain, "lon", data, lsize)
    call gGrid_importRAttr(domain, "area", data, lsize)
    call gGrid_importRAttr(domain, "frac", data, lsize)

    data(:) = 0.0
    call gGrid_importRAttr(domain, "mask", data, lsize)
    call gGrid_importRAttr(domain, "aream", data, lsize)
    deallocate(data)
    deallocate(idata)


end subroutine rof_domain_init

end module comp_rof
