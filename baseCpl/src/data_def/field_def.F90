module field
use base_sys
use type_def
    type fieldMeta
        character(FIELDSLEN) :: domain_fld
        character(FIELDSLEN) :: atm_State
        character(FIELDSLEN) :: atm_Flux
        integer   :: fldCounts
        integer   :: items
        character(FIELDSLEN), dimension(:,:) :: lookup
    end type fieldMeta

    public :: fieldMeta_init
    public :: field_set
    public :: field_add
    public :: field_lookup
    public :: fieldMeta_final
    public :: set_lookup

interface fieldMeta_init
    module procedure fieldMeta_initHardCoded
    module procedure fieldMeta_initConf
end interface

contains

subroutine fieldMeta_initHardCoded(fields, items, ierr)

    implicit none
    type(fieldMeta),   intent(inout) :: fields
    integer,           intent(in)    :: items
    integer, optional, intent(inout) :: ierr

    fields%fldCounts = 100
    allocate(fields%lookup(fields%fldCounts,items))

    call field_set(fields%atm_State,"")
    call field_set(fields%atm_Flux, "")

end subroutine fieldMeta_initHardCoded

subroutine fieldMeta_initConf(fields, items, conf_file, ierr)

    implicit none
    type(fieldMeta),   intent(inout)  :: fields
    integer，          intent(in)     :: items
    character(len=100), intent(in)    :: conf_file
    integer, optional, intent(inout)  :: ierr   
   
end subroutine fieldMeta_initConf

subroutine 

subroutine fieldMeta_final(fields, ierr)

    implicit none
    type(fieldMeta), intent(inout)   :: fields

end subroutine fieldMeta_final

subroutine fieldMeta_print(fields, ierr)

    implicit none
    type(fields),      intent(inout)  ::fields
    integer, optional, intent(inout)  :: ierr

    ! generate code

end subroutine fieldMeta_print

subroutine field_set(fields, flds, ierr)

    implicit none
    character(FIELDSLEN),  intent(inout) :: fields
    character(FIELDSLEN),  intent(in)    :: flds
    integer,   optional,   intent(inout) :: ierr

    fields = flds

end subroutine field_set

subroutine field_add(fields, fld, ierr)

    implicit none
    character(FIELDSLEN), intent(inout) :: fields
    character(FLDLEN),    intent(in)    :: fld
    integer, optional,    intent(inout) :: ierr
  
    fields = fields//trim(fld)

subroutine field_check()


end subroutine field_check

end module field
