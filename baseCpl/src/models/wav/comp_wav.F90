module comp_wav
use mct_mod
use timeM
use proc_def
use logUtil, only: logUnit
use base_fields, only: flds_c2x => flds_wav2x_fields, &
                       flds_x2c => flds_x2wav_fields
    implicit none
     integer :: comp_id

     integer, parameter :: CXX = 4096
     character(CXX) :: fld_ar = ""
     character(CXX) :: fld_ai = ""
     character(*), parameter :: model_name = "wav"

     public :: wav_init_mct
     public :: wav_run_mct
     public :: wav_final_mct

contains

subroutine wav_init_mct(modelInfo, EClock, wav2x_wavwav, x2wav_wavwav, ierr)
     implicit none
     type(model_info), target, intent(inout)      :: modelInfo
     type(Clock), intent(in)              :: EClock
     type(AttrVect), intent(inout)        :: wav2x_wavwav
     type(AttrVect), intent(inout)        :: x2wav_wavwav
     integer, intent(inout) :: ierr
     integer              :: local_comm, i
     integer              :: ID
     type(gsMap), pointer :: gsMap_wavwav
     type(gGrid), pointer :: domain

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

     character(*), parameter :: F00 ="('(wav_init_mct) ',8a)"
     character(*), parameter :: F01 ="('(wav_init_mct) ',a,4i8)"
     character(*), parameter :: F02 ="('(wav_init_mct) ',a,4es13.6)"
     character(*), parameter :: F03 ="('(wav_init_mct) ',a,i8,a)"
     character(*), parameter :: F90 ="('(wav_init_mct) ',73('='))"
     character(*), parameter :: F91 ="('(wav_init_mct) ',73('-'))"
     character(*), parameter :: subName = "(wav_init_mct)"

     local_comm = modelInfo%comm
     ID = modelInfo%ID 
     domain => modelInfo%domain
     gsMap_wavwav => modelInfo%gsmap
     gsize = modelInfo%gsize
     call mpi_comm_rank(local_comm, comm_rank, ierr)
     call mpi_comm_size(local_comm, comm_size, ierr)
 
     ngx = 10
     ngy = 10
     nlseg = 1
     nproc = comm_size

     if(comm_rank == root) then
         write(logUnit, *) 'read in xwav input from file = xwav_in'
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
     
     call gsMap_init(gsMap_wavwav, start, length, root, local_comm, ID)
     
     !--------------------------------------
     ! init MCT Domain
     !--------------------------------------
     
     call wav_domain_init(local_comm, gsMap_wavwav, domain) 
 
     !---------------------------------------
     ! init AttrVect
     !--------------------------------------
     call avect_init(wav2x_wavwav, rList=flds_c2x, lsize=lsize)
     call avect_init(x2wav_wavwav, rList=flds_x2c, lsize=lsize)

     call avect_zero(wav2x_wavwav) 
     call avect_zero(x2wav_wavwav)


end subroutine wav_init_mct


subroutine wav_run_mct(modelInfo, EClock, wav2x_wavwav, x2wav_wavwav, ierr)
    
    implicit none
    type(model_info), target, intent(inout)      :: modelInfo
    type(Clock), intent(in)        :: EClock
    type(AttrVect), intent(inout)  :: wav2x_wavwav
    type(AttrVect), intent(inout)  :: x2wav_wavwav
    integer,        intent(inout)  :: ierr

    integer     :: ID
    integer     :: local_comm
    type(gGrid), pointer :: domain

    integer :: comm_rank, i
    integer :: av_lsize, n_rflds, n_iflds
    integer :: n, nf

    local_comm = modelInfo%comm
    ID = modelInfo%ID
    domain => modelInfo%domain
    call mpi_comm_rank(local_comm, comm_rank, ierr)
     
    av_lsize = avect_lsize(wav2x_wavwav)
    n_rflds = avect_nRattr(x2wav_wavwav)
    n_iflds = avect_nRattr(x2wav_wavwav)
   
    do nf=1, n_rflds
        do n = 1, av_lsize
            wav2x_wavwav%rAttr(nf, n) = (nf*100)
        end do
    end do

end subroutine wav_run_mct


subroutine wav_final_mct()

     write(logUnit, *)"wav final"
       
end subroutine wav_final_mct

subroutine wav_domain_init(mpicomm, gsMap_wavwav, domain)

    implicit none
    integer,       intent(in)    :: mpicomm
    type(gsMap),   intent(in)    :: gsMap_wavwav
    type(gGrid),   intent(inout) :: domain

    real(8), pointer :: data(:)
    integer, pointer :: idata(:)
    integer :: ierr, lsize

    call gGrid_init(GGrid=domain, &
         CoordChars=trim('x:y:z'), &
         otherchars=trim('lat:lon:area:frac:mask:aream'), &
         lsize=gsMap_lsize(gsMap_wavwav, mpicomm))
    call avect_zero(domain%data)
 
    lsize = gsMap_lsize(gsMap_wavwav, mpicomm)

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


end subroutine wav_domain_init

end module comp_wav
