module proc_def
use mct_mod
use type_def
use comms_def
   implicit none
   type procMeta
       integer, allocatable :: IDs(:)    ! comp ID
       integer, allocatable :: comms(:)
       integer, allocatable :: ranks(:)
       integer :: groupLen
       integer :: predefSize 
       character(len=MODELNAMELEN), allocatable :: models(:)
   end type procMeta

   type compMeta
       integer ::ID
       type(gsMap) :: comp_gsmap
       type(gGrid) :: domain
       integer :: comm
       integer :: gsize
   end type compMeta

   type confMeta
       character(len=64) :: nmlfile
       character(len=64) :: restart_file
       logical :: restart
   end type confMeta


   public :: procMeta_init
   public :: procMeta_final
   public :: procMeta_print
   public :: procMeta_addToModel
   public :: procMeta_getInfo
   public :: compMeta_getInfo
   public :: confMeta_init
   public :: confMeta_getInfo


interface confMeta_init
    module procedure confMeta_initFile
    module procedure confMeta_initDefault
end interface

contains

subroutine procMeta_init(proc, predefSize, ierr)

    implicit none
    type(procMeta),    intent(inout) :: proc   
    integer, optional, intent(in)    :: predefSize
    integer, optional, intent(inout) :: ierr

    proc%predefSize = 10
    proc%groupLen = 0
    if(present(predefSize))then
        proc%predefSize = predefSize
    end if
    
    allocate(proc%IDs(proc%predefSize))
    allocate(proc%comms(proc%predefSize))
    allocate(proc%models(proc%predefSize))

end subroutine procMeta_init

subroutine procMeta_final(proc, ierr)

    implicit none
    type(procMeta),    intent(inout) :: proc
    integer, optional, intent(inout) :: ierr

    deallocate(proc%IDs)
    deallocate(proc%comms)
    deallocate(proc%models)

end subroutine procMeta_final

subroutine procMeta_print(proc, wunit, ierr)

    implicit none
    type(procMeta),    intent(in)    :: proc 
    integer, optional, intent(in)    :: wunit
    integer, optional, intent(inout) :: ierr
    
    if(present(wunit))then
        write(wunit,*)'procMeta:',proc%IDs
        write(wunit,*)'procMeta:',proc%comms
        write(wunit,*)'procMeta:',proc%models
    else
        write(*,*) 'procMeta', proc%IDs
        write(*,*) 'procMeta', proc%comms
        write(*,*) 'procMeta', proc%models
    end if

end subroutine procMeta_print

subroutine procMeta_addToModel(proc, ID, comm, modelName, ierr)

    implicit none
    type(procMeta),  intent(inout) :: proc
    integer,         intent(in)    :: ID
    integer,         intent(in)    :: comm
    character(len=MODELNAMELEN), intent(in) :: modelName
    integer,  optional,  intent(in) :: ierr
    
    ! local
    integer :: half, n
    integer, allocatable :: IDs(:)
    integer, allocatable :: comms(:)
    integer :: rank
    character(len=MODELNAMELEN), allocatable :: models(:)

    if(proc%groupLen==proc%predefSize)then
        call base_sys_abort("predef size not enough")
    end if

    n = proc%groupLen+1
    proc%IDs(n) = ID
    proc%comms(n) = comm
    proc%models = modelName
    call MPI_COMM_RANK(comm, rank, ierr)
    proc%ranks(n) = rank
    proc%groupLen = n

end subroutine procMeta_addToModel

subroutine procMeta_getInfo(proc, ID, comm, rank, modelName, ierr)
    
    implicit none
    type(procMeta),    intent(in) :: proc
    integer,           intent(in) :: ID
    integer, optional, intent(inout) :: comm
    integer, optional, intent(inout) :: rank
    character(*), optional, intent(inout) :: modelName
    integer, optional, intent(inout) :: ierr

    integer :: idx
    integer :: n

    do n = 1, proc%groupLen
        if(proc%IDs(n)==ID)then
            idx = ID
            exit
        end if
    end do

    if(present(comm))then
        comm = proc%comms(idx)
    end if
    if(present(rank)) then
        rank= proc%ranks(idx)
    end if
    if(present(modelName))then
        modelName = proc%models(idx)
    end if

end subroutine procMeta_getInfo


subroutine compMeta_getInfo(comp, ID, comp_gsmap, domain, comm, gsize, ierr)

    implicit none
    type(compMeta),        intent(inout)  :: comp
    integer,     optional, intent(inout)  :: ID
    type(gsMap), optional, intent(inout)  :: comp_gsmap
    type(gGrid), optional, intent(inout)  :: domain
    integer,     optional, intent(inout)  :: comm
    integer,     optional, intent(inout)  :: gsize
    integer,     optional, intent(inout)  :: ierr

    if(present(ID)) ID =comp%ID
    if(present(gsmap)) comp_gsmap = comp%comp_gsmap
    if(present(domain)) domain = comp%domain
    if(present(comm)) comm= comp%comm
    if(present(gsize)) gsize = comp%comm

end subroutine compMeta_getInfo

subroutine confMeta_initFile(conf, conf_file, ierr)

    implicit none
    type(confMeta),    intent(inout)  :: conf
    character(*),      intent(inout)  :: conf_file
    integer, optional, intent(inout)  :: ierr

end subroutine confMeta_initFile


subroutine confMeta_initDefault(conf, ierr)
     
    implicit none
    type(confMeta),      intent(inout)     :: conf
    integer,   optional, intent(inout)  :: ierr

end subroutine confMeta_initDefault

subroutine confMeta_getInfo(conf, nmlfile, restart, restart_file)

    implicit none
    type(confMeta),                intent(in)    :: conf
    character(len=*),   optional,  intent(inout) :: nmlfile
    character(len=*),   optional,  intent(inout) :: restart_file
    logical,            optional,  intent(inout) :: restart

    if(present(nmlfile))then
        nmlfile = conf%nmlfile
    end if
    
    if(present(restart_file))then
        restart_file =  conf%restart_file
    end if

    if(present(restart))then
        restart = conf%restart
    end if
end subroutine confMeta_getInfo
end module proc_def
