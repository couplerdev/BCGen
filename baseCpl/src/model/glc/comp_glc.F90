module comp_glc
use mct_mod
use timeM
use proc_def
use global_var
use logUtil, only: logUnit
use base_fields, only: flds_c2x => flds_glc2x_fields, &
                       flds_x2c => flds_x2glc_fields
    implicit none
     integer :: comp_id

     integer, parameter :: CXX = 4096
     character(CXX) :: fld_ar = ""
     character(CXX) :: fld_ai = ""
     character(*), parameter :: model_name = "glc"

     public :: glc_init_mct
     public :: glc_run_mct
     public :: glc_final_mct

contains

subroutine glc_init_mct(compInfo, EClock, glc2x_glcglc, x2glc_glcglc, ierr)
     implicit none
     type(compMeta), target, intent(inout)      :: compInfo
     type(Clock), intent(in)        :: EClock
     type(AttrVect), intent(inout)  :: glc2x_glcglc
     type(AttrVect), intent(inout)  :: x2glc_glcglc
     integer, intent(inout) :: ierr
     type(gsMap), pointer    :: gsMap_glcglc
     type(gGrid), pointer    :: domain
     integer :: ID
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

     character(*), parameter :: F00 ="('(glc_init_mct) ',8a)"
     character(*), parameter :: F01 ="('(glc_init_mct) ',a,4i8)"
     character(*), parameter :: F02 ="('(glc_init_mct) ',a,4es13.6)"
     character(*), parameter :: F03 ="('(glc_init_mct) ',a,i8,a)"
     character(*), parameter :: F90 ="('(glc_init_mct) ',73('='))"
     character(*), parameter :: F91 ="('(glc_init_mct) ',73('-'))"
     character(*), parameter :: subName = "(glc_init_mct)"

     call compMeta_getInfo(compInfo, ID=ID, gsmap=gsmap_glcglc, domain=domain, &
                          comm=local_comm, gsize=gsize)


     call mpi_comm_rank(local_comm, comm_rank, ierr)
     call mpi_comm_size(local_comm, comm_size, ierr)
 
     ngx = 10
     ngy = 10
     nlseg = 1
     nproc = comm_size

     if(comm_rank == root) then
         write(logUnit, *) 'read in xglc input from file = xglc_in'
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
  
     lsize = gsize / nproc
     llseg = lsize / nlseg

     do i = 1, nlseg
         start(i) = (comm_rank*lsize) + (i-1)*llseg
         length(i) = llseg
     end do

     !--------------------------------------
     !  init MCT global seg map
     !--------------------------------------
     
     call gsMap_init(gsMap_glcglc, start, length, root, local_comm, ID)
     
     !--------------------------------------
     ! init MCT Domain
     !--------------------------------------
     
     call glc_domain_init(local_comm, gsMap_glcglc, domain) 
 
     !---------------------------------------
     ! init AttrVect
     !--------------------------------------
     call avect_init(glc2x_glcglc, rList=flds_c2x, lsize=lsize)
     call avect_init(x2glc_glcglc, rList=flds_x2c, lsize=lsize)

     call avect_zero(glc2x_glcglc) 
     call avect_zero(x2glc_glcglc)


end subroutine glc_init_mct


subroutine glc_run_mct(compInfo, EClock, glc2x_glcglc, x2glc_glcglc, ierr)
    
    implicit none
    type(compMeta), target, intent(inout)      :: compInfo
    type(Clock), intent(in)        :: EClock
    type(AttrVect), intent(inout)  :: glc2x_glcglc
    type(AttrVect), intent(inout)  :: x2glc_glcglc
    integer,        intent(inout)  :: ierr
    integer               :: ID
    type(gGrid), pointer  :: domain
    integer               :: local_comm
    integer :: comm_rank, i
    integer :: av_lsize, n_rflds, n_iflds
    integer :: n, nf

    call compMeta_getInfo(compInfo, comm=local_comm, domain=domain, ID=ID)
    
    call mpi_comm_rank(local_comm, comm_rank, ierr)
     
    av_lsize = avect_lsize(glc2x_glcglc)
    n_rflds = avect_nRattr(x2glc_glcglc)
    n_iflds = avect_nRattr(x2glc_glcglc)
   
    do nf=1, n_rflds
        do n = 1, av_lsize
            glc2x_glcglc%rAttr(nf, n) = (nf*100)
        end do
    end do

end subroutine glc_run_mct


subroutine glc_final_mct()

     write(logUnit, *)"glc final"
       
end subroutine glc_final_mct

subroutine glc_domain_init(mpicomm, gsMap_glcglc, domain)

    integer       ,intent(in)    :: mpicomm
    type(gsMap)   ,intent(in)    :: gsMap_glcglc
    type(gGrid)   ,intent(inout) :: domain

    real(8)    ,pointer :: data(:)
    integer    ,pointer :: idata(:)
    integer :: ierr, lsize

    call gGrid_init(GGrid=domain, &
        CoordChars=trim('x:y:z'), &
        otherchars=trim('lat:lon:area:frac:mask:aream'), &
        lsize=gsMap_lsize(gsMap_glcglc, mpicomm))
    call avect_zero(domain%data)

    lsize = gsMap_lsize(gsMap_glcglc, mpicomm)

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

end subroutine glc_domain_init


end module comp_glc
