module field_def
!--------------------------------------------------------
!
!    This is field manage mod
!
!--------------------------------------------------------
use shr_sys_mod
use logUtil
!use type_def, only : FIELDSLEN
use shr_kind_mod
use proc_def
    implicit none
    type fieldDesc
        character(SHR_KIND_CL) :: shortname
        character(SHR_KIND_CL) :: longname
        character(SHR_KIND_CL) :: stdname
        character(SHR_KIND_CL) :: units
    end type fieldDesc
    type fldsMeta
        character(SHR_KIND_CL) :: domain_fld
        integer   :: capacity
        integer   :: items
        type(fieldDesc), allocatable :: lookup(:)
    end type fldsMeta

    public :: fldsMeta_init
    public :: fldsMeta_add
    public :: fldsMeta_lookup
    public :: fldsMeta_final

interface fldsMeta_add
    module procedure fldsMeta_add_desc
    module procedure fldsMeta_add_name
end interface

contains 

subroutine fldsMeta_init(fldsMetaData, cap)

    implicit none
    type(fldsMeta),    intent(inout)  :: fldsMetaData
    integer,           intent(in)     :: cap

    fldsMetaData%capacity = cap
    allocate(fldsMetaData%lookup(cap))
    fldsMetaData%items = 0

end subroutine fldsMeta_init

subroutine fldsMeta_add_desc(fldsMetaData, fldDesc, ierr)

    implicit none
    type(fldsMeta),    intent(inout) :: fldsMetaData
    type(fieldDesc),   intent(in)    :: fldDesc
    integer, optional, intent(inout) :: ierr
    
    if(present(ierr))ierr=0
    if(fldsMetaData%capacity==fldsMetaData%items)then
        if(present(ierr))then
            ierr = 1
        end if
        write(logUnit, *)'fldsMeta out of capacity'
        return 
    end if
    fldsMetaData%lookup(fldsMetaData%items+1) = fldDesc
    fldsMetaData%items = fldsMetaData%items + 1

end subroutine fldsMeta_add_desc

subroutine fldsMeta_add_name(fldsMetaData, shortname, longname, stdname, units, ierr)

    implicit none
    type(fldsMeta),         intent(inout)  :: fldsMetaData
    character(*),           intent(in)     :: shortname
    character(*), optional, intent(in)     :: longname
    character(*), optional, intent(in)     :: stdname
    character(*), optional, intent(in)     :: units
    integer,      optional, intent(inout)  :: ierr

    ! local
    character(SHR_KIND_CL) :: unkown = "unkown"
    character(SHR_KIND_CL) :: llongname = ''
    character(SHR_KIND_CL) :: lstdname = ''
    character(SHR_KIND_CL) :: lunits = ''
    type(fieldDesc)     :: fldDesc

    llongname = unkown
    lstdname = unkown
    lunits = unkown

    if(present(longname))llongname = longname
    if(present(stdname)) lstdname = stdname
    if(present(units)) lunits = units
    if(present(ierr))  ierr = 0
    
    if(fldsMetaData%capacity==fldsMetaData%items)then
        if(present(ierr))ierr=1
        write(logUnit, *)'fldsMeta out of capacity'
        return 
    end if

    fldDesc%shortname = shortname
    fldDesc%longname = llongname
    fldDesc%stdname = lstdname
    fldDesc%units = lunits

    fldsMetaData%lookup(fldsMetaData%items) = fldDesc

    fldsMetaData%items = fldsMetaData%items + 1

end subroutine fldsMeta_add_name


subroutine fldsMeta_lookup(fldsMetaData, shortname, longname, stdname, units)

    implicit none
    type(fldsMeta),         intent(in)     :: fldsMetaData
    character(*),           intent(in)     :: shortname
    character(*), optional, intent(inout)  :: longname
    character(*), optional, intent(inout)  :: stdname
    character(*), optional, intent(inout)  :: units

    ! local 
    integer                 :: i
    character(SHR_KIND_CL) :: unkown = "unkown"
    character(SHR_KIND_CL) :: llongname = ''
    character(SHR_KIND_CL) :: lstdname = ''
    character(SHR_KIND_CL) :: lunits = ''

    llongname = unkown
    lstdname  = unkown
    lunits = unkown
   
    do i = 1, fldsMetaData%items
        if(fldsMetaData%lookup(i)%shortname==shortname)then
            llongname = fldsMetaData%lookup(i)%longname
            lstdname = fldsMetaData%lookup(i)%stdname
            lunits = fldsMetaData%lookup(i)%units
        end if
    end do

    if(present(longname))longname = llongname
    if(present(stdname)) stdname = lstdname
    if(present(units))units = lunits
end subroutine fldsMeta_lookup

subroutine fldsMeta_final(fldsMetaData)

    implicit none
    type(fldsMeta),     intent(inout)  :: fldsMetaData

    deallocate(fldsMetaData%lookup)
    fldsMetaData%capacity =  0
    fldsMetaData%items = 0 
   
end subroutine fldsMeta_final

end module field_def
