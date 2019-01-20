module proc_def
!use base_sys
use shr_sys_mod
use mct_mod
use type_def
use shr_kind_mod
use comms_def
    implicit none
    type procMeta
        integer, allocatable :: IDs(:)
        integer, allocatable :: comms(:)
        integer, allocatable :: ranks(:)
        integer :: groupLen
        integer :: predefSize
        character(SHR_KIND_CS), allocatable ::  models(:)
    end type procMeta 

    type compMeta
        integer :: ID
        type(mct_gsMap) :: comp_gsmap
        type(mct_gGrid) :: domain
        character(SHR_KIND_CL) :: srcFile
        integer :: comm
        integer :: gsize
        integer :: nx
        integer :: ny
        logical :: prognostic
        character(SHR_KIND_CL) :: case_name
    end type compMeta 

    type confMeta
        character(len=64)  :: nmlfile
        character(len=64)  :: restart_file
        character(len=64)  :: pio_nmlfile
        logical :: restart
    end type confMeta

    public :: procMeta_init
    public :: procMeta_final
    public :: procMeta_print
    public :: procMeta_addToModel
    public :: procMeta_getInfo
    public :: compMeta_getInfo
    public :: compMeta_final
    public :: confMeta_init
    public :: confMeta_getInfo

interface confMeta_init
    module procedure confMeta_initFile
    module procedure confMeta_initDefault
end interface confMeta_init


contains

subroutine procMeta_init(proc, predefSize, ierr)

    implicit none
    type(procMeta),    intent(inout)   :: proc
    integer, optional, intent(in)      :: predefSize
    integer, optional, intent(inout)   :: ierr

    proc%predefSize = 10 
    proc%groupLen = 0
    if(present(predefSize))then
        proc%predefSize = predefSize
    end if

    allocate(proc%IDs(proc%predefSize))
    allocate(proc%comms(proc%predefSize))
    allocate(proc%models(proc%predefSize))
    allocate(proc%ranks(proc%predefSize))


end subroutine procMeta_init 

subroutine procMeta_final(proc, ierr)

    implicit none
    type(procMeta),      intent(inout)  :: proc
    integer,  optional,  intent(in)     :: ierr
 
    deallocate(proc%IDs)
    deallocate(proc%comms)
    deallocate(proc%models)
    deallocate(proc%ranks)

end subroutine procMeta_final

subroutine procMeta_print(proc, wunit, ierr)

    implicit none
    type(procMeta),     intent(in)     :: proc
    integer, optional,  intent(in)     :: wunit
    integer, optional,  intent(inout)  :: ierr

    if(present(wunit))then
        write(wunit, *)'procMeta:', proc%IDs
        write(wunit, *)'procMeta:', proc%comms
        write(wunit, *)'procMeta:', proc%models
    else
        write(*,*)'procMeta:', proc%IDs
        write(*,*)'procMeta:', proc%comms
        write(*,*)'procMeta:', proc%models
    end if

end subroutine procMeta_print

subroutine procMeta_addToModel(proc, ID, comm, modelName, ierr)

    implicit none
    type(procMeta),     intent(inout)   :: proc
    integer,            intent(in)      :: ID
    integer,            intent(in)      :: comm
    character(SHR_KIND_CS), intent(in)  :: modelName
    integer, optional,  intent(in)      :: ierr

    !local
    integer  :: half, n
    integer, allocatable :: IDs(:)
    integer, allocatable :: comms(:)
    integer :: rank
    character(SHR_KIND_CS), allocatable :: models(:)

    if(proc%groupLen==proc%predefSize)then
        call shr_sys_abort('predef size not enough')
    end if
    n = proc%groupLen+1
    proc%IDs(n) = ID
    proc%comms(n) = comm
    proc%models = modelName
    call MPI_COMM_RANK(comm, rank, ierr)
    proc%ranks(n)  = rank
    proc%groupLen = n

end subroutine procMeta_addToModel

subroutine procMeta_getInfo(proc, ID, comm, rank, iamroot, modelName, ierr)

    implicit none
    type(procMeta),         intent(in)    :: proc
    integer,                intent(in)    :: ID
    integer,   optional,    intent(inout) :: comm
    integer,   optional,    intent(inout) :: rank
    logical,   optional,    intent(inout) :: iamroot
    character(*), optional, intent(inout) :: modelName
    integer,   optional,    intent(inout) :: ierr

    integer  :: idx
    integer  :: n

    do n = 1, proc%groupLen
        if(proc%IDs(n)==ID)then
            idx = ID
            exit
        end if
    end do

    if(present(comm))then
        comm = proc%comms(idx)
    end if
    if(present(rank))then
        rank = proc%ranks(idx)
    end if
    if(present(iamroot))then
        iamroot = .false.
        if(proc%ranks(idx)==0)then
           iamroot = .true.
        end if
    end if
    if(present(modelName))then
        modelName = proc%models(idx)
    end if

end subroutine procMeta_getInfo

subroutine compMeta_getInfo(comp, ID, comp_gsmap, domain,case_name, comm, gsize, nx, ny, srcFile, prognostic,ierr)

    implicit none
    type(compMeta),         intent(inout)   :: comp
    integer,     optional,  intent(inout)   :: ID
    type(mct_gsMap), optional,  intent(inout)   :: comp_gsmap
    type(mct_gGrid), optional,  intent(inout)   :: domain
    logical,     optional,  intent(inout)   :: prognostic
    character(*), optional, intent(inout)   :: case_name
    integer,     optional,  intent(inout)   :: comm
    integer,     optional,  intent(inout)   :: gsize
    integer,     optional,  intent(inout)   :: nx
    integer,     optional,  intent(inout)   :: ny
    character(*), optional, intent(inout)   :: srcFile
    integer,     optional,  intent(inout)   :: ierr

    if(present(ID)) ID = comp%ID
    if(present(comp_gsmap)) comp_gsmap = comp%comp_gsmap
    if(present(domain)) domain =  comp%domain
    if(present(comm)) comm = comp%comm
    if(present(gsize)) gsize = comp%gsize
    if(present(srcFile)) srcFile = comp%srcFile
    if(present(prognostic)) prognostic = comp%prognostic
    if(present(case_name)) case_name = comp%case_name
    if(present(nx)) nx = comp%nx
    if(present(ny)) ny = comp%ny
    if(present(ierr)) ierr=0


end subroutine compMeta_getInfo

subroutine compMeta_final(comp, ierr)

    implicit none
    type(compMeta),      intent(inout) :: comp
    integer, optional,   intent(inout) :: ierr

end subroutine compMeta_final

subroutine confMeta_initFile(conf, conf_file, pio_nmlfile,  ierr)

    implicit none
    type(confMeta),         intent(inout)    :: conf
    character(*),           intent(in)       :: conf_file
    character(*),           intent(in)       :: pio_nmlfile
    integer,      optional, intent(inout)    :: ierr 

    conf%nmlfile = conf_file
    conf%pio_nmlfile = pio_nmlfile

end subroutine confMeta_initFile

subroutine confMeta_initDefault(conf, ierr)

    implicit none
    type(confMeta),          intent(inout)  :: conf
    integer,      optional,  intent(inout)  :: ierr

end subroutine confMeta_initDefault 

subroutine confMeta_getInfo(conf, nmlfile, restart, restart_file, pio_nmlfile)

    implicit none
    type(confMeta),             intent(in)    :: conf
    character(len=*), optional, intent(inout) :: nmlfile
    character(len=*), optional, intent(inout) :: pio_nmlfile
    character(len=*), optional, intent(inout) :: restart_file
    logical,          optional, intent(inout) :: restart

    if(present(nmlfile))then
        nmlfile = conf%nmlfile
    end if

    if(present(pio_nmlfile))then
        pio_nmlfile = conf%pio_nmlfile
    end if

    if(present(restart_file))then
        restart_file = conf%restart_file
    end if

    if(present(restart))then
        restart = conf%restart
    end if

end subroutine confMeta_getInfo

end module proc_def

