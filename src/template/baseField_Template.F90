module base_fields

    use global_var
    implicit none
    public :: flds_init

contains

subroutine flds_init(metaData, ierr)

    implicit none
    type(Meta),         intent(inout) :: metaData
    integer,  optional, intent(inout) :: ierr
    
    ! local data
    type(fieldDesc)  :: fldDesc
    character(FIELDSLEN)   :: shortname
    character(FIELDSLEN)   :: longname
    character(FIELDSLEN)   :: stdname
    character(FIELDSLEN)   :: units
    integer :: idx, totalFld

    #set totalFld = len($field_cfgs)
    totalFld = $(totalFld)
    call fieldMeta_init(metaData%fieldMeta, totalFld)
    
    #for fld in $fieldVar_cfgs
         #set val = $fieldVar_cfgs[$fld]
    metaData%$fld = "$val"
    #end for

    #for fldMeta in $field_cfgs
         #set $sname = fldMeta.shortname
         #set $lname = fldMeta.longname
         #set $stdname = fldMeta.stdname
         #set $units = fldMeta.units
    fldDesc%shortname = "$sname"
    fldDesc%longname = "$lname"
    fldDesc%stdname = "$stdname"
    fldDesc%units = "$units"
    call fieldMeta_add(metaData%fieldMeta, fldDesc)
    #end for

end subroutine flds_init


end module base_fields
