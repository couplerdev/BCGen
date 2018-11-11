module base_field

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

    totalFld = 6
    call fldsMeta_init(metaData%fldsMetaData, totalFld)
    
    metaData%flds_dom = "lat:lon:area:aream:mask:frac"
    metaData%flds_x2ocn_fluxes = "Faxarain:Faxasnow:Faxaprec:Faxalwdn:Foxxswnet:Fax&
abcphidry:Faxabcphodry:Faxabcphiwet:Faxaocphidry&
:Faxaocphodry:Faxaocphiwet:Faxadstwet1:Faxadstwe&
t2:Faxadstwet3:Faxadstwet4:Faxadstdry1:Faxadstdr&
y2:Faxadstdry3:Faxadstdry4:Foxxtaux:Foxxtauy:Fox&
xlat:Foxxsen:Foxxlwup:Foxxevap:Fioimelth:Fioimel&
tw:Fioisalt:Forrroff:Forrioff"
    metaData%flds_ocn2x_states = "Sot:Sos:Sou:Sov:Sodhdx:Sodhdy:Sobldepth"
    metaData%flds_atm2x_fluxes = "Faxarainc:Faxarainl:Faxasnowc:Faxasnowl:Faxalwdn:&
Faxaswndr:Faxaswvdr:Faxaswndf:Faxaswvdf:Faxaswne&
t:Faxabcphidry:Faxabcphodry:Faxabcphiwet:Faxaocp&
hidry:Faxaocphodry:Faxaocphiwet:Faxadstwet1:Faxa&
dstwet2:Faxadstwet3:Faxadstwet4:Faxadstdry1:Faxa&
dstdry2:Faxadstdry3:Faxadstdry4"
    metaData%flds_x2ocn = "Faxarain:Faxasnow:Faxaprec:Faxalwdn:Foxxswnet:Fax&
abcphidry:Faxabcphodry:Faxabcphiwet:Faxaocphidry&
:Faxaocphodry:Faxaocphiwet:Faxadstwet1:Faxadstwe&
t2:Faxadstwet3:Faxadstwet4:Faxadstdry1:Faxadstdr&
y2:Faxadstdry3:Faxadstdry4:Foxxtaux:Foxxtauy:Fox&
xlat:Foxxsen:Foxxlwup:Foxxevap:Fioimelth:Fioimel&
tw:Fioisalt:Forrroff:Forrioff:Sotref:Soqref:Sossq:Sore:Sou10:Soduu10n:Soustar"
    metaData%flds_x2atm = "Sflfrac:Sfifrac:Sfofrac:Sxavsdr:Sxanidr:Sxavsdf:S&
xanidf:Sxtref:Sxqref:Sot:Sxt:Slfv:Slram1:Slsnowh&
:Sisnowh:Sossq:Sore:Sxu10:Soustar:Faxxtaux:Faxxtauy:Faxxlat:Faxxsen:Faxxlwup:Faxxev&
ap:Fallflxdst1:Fallflxdst2:Fallflxdst3:Fallflxds&
t4"
    metaData%flds_dom_coord = "lat:lon:area:aream:mask:frac"
    metaData%flds_x2atm_states = "Sflfrac:Sfifrac:Sfofrac:Sxavsdr:Sxanidr:Sxavsdf:S&
xanidf:Sxtref:Sxqref:Sot:Sxt:Slfv:Slram1:Slsnowh&
:Sisnowh:Sossq:Sore:Sxu10:Soustar"
    metaData%flds_x2ocn_states = "Sotref:Soqref:Sossq:Sore:Sou10:Soduu10n:Soustar"
    metaData%flds_x2atm_fluxes = "Faxxtaux:Faxxtauy:Faxxlat:Faxxsen:Faxxlwup:Faxxev&
ap:Fallflxdst1:Fallflxdst2:Fallflxdst3:Fallflxds&
t4"
    metaData%flds_atm2x = "Faxarainc:Faxarainl:Faxasnowc:Faxasnowl:Faxalwdn:&
Faxaswndr:Faxaswvdr:Faxaswndf:Faxaswvdf:Faxaswne&
t:Faxabcphidry:Faxabcphodry:Faxabcphiwet:Faxaocp&
hidry:Faxaocphodry:Faxaocphiwet:Faxadstwet1:Faxa&
dstwet2:Faxadstwet3:Faxadstwet4:Faxadstdry1:Faxa&
dstdry2:Faxadstdry3:Faxadstdry4:Saz:Sau:Sav:Satbot:Saptem:Sashum:Sapbot:Sadens:Sa&
pslv"
    metaData%flds_ocn2x = "Sot:Sos:Sou:Sov:Sodhdx:Sodhdy:Sobldepth:Fiooq"
    metaData%flds_atm2x_states = "Saz:Sau:Sav:Satbot:Saptem:Sashum:Sapbot:Sadens:Sa&
pslv"
    metaData%flds_ocn2x_fluxes = "Fiooq"

    fldDesc%shortname = "aream"
    fldDesc%longname = "unknow"
    fldDesc%stdname = " cell area from mapping file"
    fldDesc%units = " m^2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "frac"
    fldDesc%longname = " area_fraction"
    fldDesc%stdname = " area fraction"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "area"
    fldDesc%longname = "unknow"
    fldDesc%stdname = " cell area"
    fldDesc%units = " m^2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "mask"
    fldDesc%longname = "unknow"
    fldDesc%stdname = " mask"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "lon"
    fldDesc%longname = "unknow"
    fldDesc%stdname = " longitude"
    fldDesc%units = " degrees east"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "lat"
    fldDesc%longname = "unknow"
    fldDesc%stdname = " latitude"
    fldDesc%units = " degrees north"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)

end subroutine flds_init


end module base_field
