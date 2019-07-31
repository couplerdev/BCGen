module base_field
    use shr_kind_mod
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
    character(SHR_KIND_CL)   :: shortname
    character(SHR_KIND_CL)   :: longname
    character(SHR_KIND_CL)   :: stdname
    character(SHR_KIND_CL)   :: units
    integer :: idx, totalFld

    totalFld = 124
    call fldsMeta_init(metaData%fldsMetaData, totalFld)
    
    metaData%flds_x2ocn = "Faxa_rain:Faxa_snow:Faxa_prec:Faxa_lwdn:Foxx_swnet:Faxa_bcphidry:Faxa_bcphodry:Faxa_bcphiwet:Faxa_ocphidry:Faxa_ocphodry:Faxa_ocphiwet:Faxa_dstwet1:Faxa_dstwet2:Faxa_dstwet3:Faxa_dstwet4:Faxa_dstdry1:Faxa_dstdry2:Faxa_dstdry3:Faxa_dstdry4:Foxx_taux:Foxx_tauy:Foxx_lat:Foxx_sen:Foxx_lwup:Foxx_evap:Fioi_melth:Fioi_meltw:Fioi_salt:Forr_roff:Forr_ioff:Sa_pslv:So_duu10n:Si_ifrac:Sw_lamult:Sw_ustokes:Sw_vstokes:Sw_hstokes"
    metaData%flds_x2rof_fluxes = "Flrl_rofliq:Flrl_rofice"
    metaData%flds_rof2x_states = "Slrr_volr"
    metaData%flds_xao_fluxes = "Faox_taux:Faox_tauy:Faox_lat:Faox_sen:Faox_lwup:Faox_evap"
    metaData%flds_x2ice_fluxes = "Faxa_rain:Faxa_snow:Faxa_lwdn:Faxa_swndr:Faxa_swvdr:Faxa_swndf:Faxa_swvdf:Faxa_bcphidry:Faxa_bcphodry:Faxa_bcphiwet:Faxa_ocphidry:Faxa_ocphodry:Faxa_ocphiwet:Faxa_dstwet1:Faxa_dstwet2:Faxa_dstwet3:Faxa_dstwet4:Faxa_dstdry1:Faxa_dstdry2:Faxa_dstdry3:Faxa_dstdry4:Fioo_q"
    metaData%flds_lnd2x = "Sl_avsdr:Sl_anidr:Sl_avsdf:Sl_anidf:Sl_tref:Sl_qref:Sl_t:Sl_fv:Sl_ram1:Sl_snowh:Sl_u10:Fall_swnet:Fall_taux:Fall_tauy:Fall_lat:Fall_sen:Fall_lwup:Fall_evap:Fall_flxdst1:Fall_flxdst2:Fall_flxdst3:Fall_flxdst4:Flrl_rofliq:Flrl_rofice"
    metaData%flds_rof2x = "Forr_roff:Forr_ioff:Flrr_flood:Slrr_volr"
    metaData%flds_dom = "lat:lon:area:aream:mask:frac:lat:lon:area:aream:mask:frac"
    metaData%flds_x2ocn_fluxes = "Faxa_rain:Faxa_snow:Faxa_prec:Faxa_lwdn:Foxx_swnet:Faxa_bcphidry:Faxa_bcphodry:Faxa_bcphiwet:Faxa_ocphidry:Faxa_ocphodry:Faxa_ocphiwet:Faxa_dstwet1:Faxa_dstwet2:Faxa_dstwet3:Faxa_dstwet4:Faxa_dstdry1:Faxa_dstdry2:Faxa_dstdry3:Faxa_dstdry4:Foxx_taux:Foxx_tauy:Foxx_lat:Foxx_sen:Foxx_lwup:Foxx_evap:Fioi_melth:Fioi_meltw:Fioi_salt:Forr_roff:Forr_ioff"
    metaData%flds_x2rof_states = ""
    metaData%flds_atm2x_fluxes = "Faxa_rainc:Faxa_rainl:Faxa_snowc:Faxa_snowl:Faxa_lwdn:Faxa_swndr:Faxa_swvdr:Faxa_swndf:Faxa_swvdf:Faxa_swnet:Faxa_bcphidry:Faxa_bcphodry:Faxa_bcphiwet:Faxa_ocphidry:Faxa_ocphodry:Faxa_ocphiwet:Faxa_dstwet1:Faxa_dstwet2:Faxa_dstwet3:Faxa_dstwet4:Faxa_dstdry1:Faxa_dstdry2:Faxa_dstdry3:Faxa_dstdry4"
    metaData%flds_x2ice = "Faxa_rain:Faxa_snow:Faxa_lwdn:Faxa_swndr:Faxa_swvdr:Faxa_swndf:Faxa_swvdf:Faxa_bcphidry:Faxa_bcphodry:Faxa_bcphiwet:Faxa_ocphidry:Faxa_ocphodry:Faxa_ocphiwet:Faxa_dstwet1:Faxa_dstwet2:Faxa_dstwet3:Faxa_dstwet4:Faxa_dstdry1:Faxa_dstdry2:Faxa_dstdry3:Faxa_dstdry4:Fioo_q:Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem:Sa_shum:Sa_pbot:Sa_dens:So_t:So_s:So_u:So_v:So_dhdx:So_dhdy"
    metaData%flds_dom_coord = "lat:lon:area:aream:mask:frac"
    metaData%flds_x2atm_states = "Sf_lfrac:Sf_ifrac:Sf_ofrac:Sx_avsdr:Sx_anidr:Sx_avsdf:Sx_anidf:Sx_tref:Sx_qref:So_t:Sx_t:Sl_fv:Sl_ram1:Sl_snowh:Si_snowh:So_ssq:So_re:Sx_u10:So_ustar"
    metaData%flds_x2ocn_states = "Sa_pslv:So_duu10n:Si_ifrac:Sw_lamult:Sw_ustokes:Sw_vstokes:Sw_hstokes"
    metaData%flds_x2atm = "Faxx_taux:Faxx_tauy:Faxx_lat:Faxx_sen:Faxx_lwup:Faxx_evap:Fall_flxdst1:Fall_flxdst2:Fall_flxdst3:Fall_flxdst4:Sf_lfrac:Sf_ifrac:Sf_ofrac:Sx_avsdr:Sx_anidr:Sx_avsdf:Sx_anidf:Sx_tref:Sx_qref:So_t:Sx_t:Sl_fv:Sl_ram1:Sl_snowh:Si_snowh:So_ssq:So_re:Sx_u10:So_ustar"
    metaData%flds_ice2x_fluxes = "Faii_swnet:Fioi_swpen:Faii_taux:Fioi_taux:Faii_tauy:Fioi_tauy:Faii_lat:Faii_sen:Faii_lwup:Faii_evap:Fioi_melth:Fioi_meltw:Fioi_salt"
    metaData%flds_xao_fields = "So_avsdr:So_anidr:So_avsdf:So_anidf:So_tref:So_qref:So_ssq:So_re:So_u10:So_duu10n:So_ustar:Faox_taux:Faox_tauy:Faox_lat:Faox_sen:Faox_lwup:Faox_evap"
    metaData%flds_ocn2x_states = "So_t:So_s:So_u:So_v:So_dhdx:So_dhdy:So_bldepth"
    metaData%flds_ice2x = "Faii_swnet:Fioi_swpen:Faii_taux:Fioi_taux:Faii_tauy:Fioi_tauy:Faii_lat:Faii_sen:Faii_lwup:Faii_evap:Fioi_melth:Fioi_meltw:Fioi_salt:Si_avsdr:Si_anidr:Si_avsdf:Si_anidf:Si_tref:Si_qref:Si_t:Si_snowh:Si_u10:Si_ifrac"
    metaData%flds_x2lnd_states = "Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem:Sa_shum:Sa_pbot:Slrr_volr"
    metaData%flds_x2lnd = "Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem:Sa_shum:Sa_pbot:Slrr_volr:Faxa_rainc:Faxa_rainl:Faxa_snowc:Faxa_snowl:Faxa_lwdn:Faxa_swndr:Faxa_swvdr:Faxa_swndf:Faxa_swvdf:Faxa_bcphidry:Faxa_bcphodry:Faxa_bcphiwet:Faxa_ocphidry:Faxa_ocphodry:Faxa_ocphiwet:Faxa_dstwet1:Faxa_dstwet2:Faxa_dstwet3:Faxa_dstwet4:Faxa_dstdry1:Faxa_dstdry2:Faxa_dstdry3:Faxa_dstdry4:Flrr_flood"
    metaData%flds_lnd2x_states = "Sl_avsdr:Sl_anidr:Sl_avsdf:Sl_anidf:Sl_tref:Sl_qref:Sl_t:Sl_fv:Sl_ram1:Sl_snowh:Sl_u10"
    metaData%flds_x2ice_states = "Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem:Sa_shum:Sa_pbot:Sa_dens:So_t:So_s:So_u:So_v:So_dhdx:So_dhdy"
    metaData%flds_x2lnd_fluxes = "Faxa_rainc:Faxa_rainl:Faxa_snowc:Faxa_snowl:Faxa_lwdn:Faxa_swndr:Faxa_swvdr:Faxa_swndf:Faxa_swvdf:Faxa_bcphidry:Faxa_bcphodry:Faxa_bcphiwet:Faxa_ocphidry:Faxa_ocphodry:Faxa_ocphiwet:Faxa_dstwet1:Faxa_dstwet2:Faxa_dstwet3:Faxa_dstwet4:Faxa_dstdry1:Faxa_dstdry2:Faxa_dstdry3:Faxa_dstdry4:Flrr_flood"
    metaData%flds_atm2x = "Faxa_rainc:Faxa_rainl:Faxa_snowc:Faxa_snowl:Faxa_lwdn:Faxa_swndr:Faxa_swvdr:Faxa_swndf:Faxa_swvdf:Faxa_swnet:Faxa_bcphidry:Faxa_bcphodry:Faxa_bcphiwet:Faxa_ocphidry:Faxa_ocphodry:Faxa_ocphiwet:Faxa_dstwet1:Faxa_dstwet2:Faxa_dstwet3:Faxa_dstwet4:Faxa_dstdry1:Faxa_dstdry2:Faxa_dstdry3:Faxa_dstdry4:Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem:Sa_shum:Sa_pbot:Sa_dens:Sa_pslv"
    metaData%flds_ocn2x = "So_t:So_s:So_u:So_v:So_dhdx:So_dhdy:So_bldepth:Fioo_q"
    metaData%flds_dom_other = "lat:lon:area:aream:mask:frac"
    metaData%flds_lnd2x_fluxes = "Fall_swnet:Fall_taux:Fall_tauy:Fall_lat:Fall_sen:Fall_lwup:Fall_evap:Fall_flxdst1:Fall_flxdst2:Fall_flxdst3:Fall_flxdst4:Flrl_rofliq:Flrl_rofice"
    metaData%flds_ice2x_states = "Si_avsdr:Si_anidr:Si_avsdf:Si_anidf:Si_tref:Si_qref:Si_t:Si_snowh:Si_u10:Si_ifrac"
    metaData%flds_atm2x_states = "Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem:Sa_shum:Sa_pbot:Sa_dens:Sa_pslv"
    metaData%flds_rof2x_fluxes = "Forr_roff:Forr_ioff:Flrr_flood"
    metaData%flds_xao_states = "So_tref:So_qref:So_ssq:So_re:So_u10:So_duu10n:So_ustar"
    metaData%flds_ocn2x_fluxes = "Fioo_q"
    metaData%flds_x2atm_fluxes = "Faxx_taux:Faxx_tauy:Faxx_lat:Faxx_sen:Faxx_lwup:Faxx_evap:Fall_flxdst1:Fall_flxdst2:Fall_flxdst3:Fall_flxdst4"
    metaData%flds_x2rof = "Flrl_rofliq:Flrl_rofice:"

    fldDesc%shortname = "Faxa_rainl"
    fldDesc%longname = " Large-scale (stable) precipitation rate"
    fldDesc%stdname = " large_scale_precipitation_flux"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_rainc"
    fldDesc%longname = " Convective precipitation rate"
    fldDesc%stdname = " convective_precipitation_flux"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sf_ifrac"
    fldDesc%longname = " Surface ice fraction"
    fldDesc%stdname = " sea_ice_area_fraction"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Flrr_flood"
    fldDesc%longname = " Waterrflux due to flooding"
    fldDesc%stdname = " flooding_water_flux"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "So_dhdx"
    fldDesc%longname = " Zonal sea surface slope"
    fldDesc%stdname = " sea_surface_eastward_slope"
    fldDesc%units = " m m-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "So_dhdy"
    fldDesc%longname = " Meridional sea surface slope"
    fldDesc%stdname = " sea_surface_northward_slope"
    fldDesc%units = " m m-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sl_fv"
    fldDesc%longname = " Surface fraction velocity in land"
    fldDesc%stdname = " fraction_velocity"
    fldDesc%units = " m s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Si_tref"
    fldDesc%longname = " Reference temperature at 2 meters"
    fldDesc%stdname = " air_temperature"
    fldDesc%units = " K"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Flrl_rofice"
    fldDesc%longname = " Water flux from land (frozen)"
    fldDesc%stdname = " frozen_water_flux_into_runoff"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sx_qref"
    fldDesc%longname = " Reference specific humidity at 2 meters"
    fldDesc%stdname = " specific_humidity"
    fldDesc%units = " kg kg-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_bcphiwet"
    fldDesc%longname = " Hydrophylic black carbon wet deposition flux"
    fldDesc%stdname = " wet_deposition_flux_of_hydrophylic_black_carbon"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faii_tauy"
    fldDesc%longname = " Meridional surface stress"
    fldDesc%stdname = " surface_downward_northward_stress"
    fldDesc%units = " N m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "aream"
    fldDesc%longname = "unknow"
    fldDesc%stdname = " cell area from mapping file"
    fldDesc%units = " m^2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Foxx_swnet"
    fldDesc%longname = " Net shortwave radiation"
    fldDesc%stdname = " surface_net_shortwave_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_snow"
    fldDesc%longname = " Water flux due to snow"
    fldDesc%stdname = " surface_snow_melt_flux"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Fall_lwup"
    fldDesc%longname = " Surface upward longwave heat flux"
    fldDesc%stdname = " surface_net_upward_longwave_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sl_t"
    fldDesc%longname = " Surface temperature"
    fldDesc%stdname = " surface_temperature"
    fldDesc%units = " K"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sx_anidr"
    fldDesc%longname = " Direct albedo (near-infrared radiation)"
    fldDesc%stdname = " surface_direct_albedo_due_to_near_infrared_radiation"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sw_ustokes"
    fldDesc%longname = " Stokes drift u component"
    fldDesc%stdname = " wave_model_stokes_drift_eastward_velocity"
    fldDesc%units = " m/s"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sx_t"
    fldDesc%longname = " Surface temperature"
    fldDesc%stdname = " surface_temperature"
    fldDesc%units = " K"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Flrl_rofliq"
    fldDesc%longname = " Water flux from land (liquid)"
    fldDesc%stdname = " water_flux_into_runoff"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faii_lat"
    fldDesc%longname = " Surface latent heat flux"
    fldDesc%stdname = " surface_upward_latent_heat_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sx_anidf"
    fldDesc%longname = " Diffuse albedo (near-infrared radiation)"
    fldDesc%stdname = " surface_diffuse_albedo_due_to_near_infrared_radiation"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sl_snowh"
    fldDesc%longname = " Surface snow water equivalent"
    fldDesc%stdname = " surface_snow_water_equivalent"
    fldDesc%units = " m"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Fioo_q"
    fldDesc%longname = " Ocean freeze (q>0) or melt (q<0) potential"
    fldDesc%stdname = " surface_snow_and_ice_melt_heat_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_ocphodry"
    fldDesc%longname = " Hydrophobic organic carbon dry deposition flux"
    fldDesc%stdname = " dry_deposition_flux_of_hydrophobic_organic_carbon"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Slrr_volr"
    fldDesc%longname = " River channel water volume"
    fldDesc%stdname = " rtm_volr"
    fldDesc%units = " m3"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_rain"
    fldDesc%longname = " Water flux due to rain"
    fldDesc%stdname = " rainfall_flux"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sf_lfrac"
    fldDesc%longname = " Surface land fraction"
    fldDesc%stdname = " land_area_fraction"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sx_avsdr"
    fldDesc%longname = " Direct albedo (visible radiation)"
    fldDesc%stdname = " surface_direct_albedo_due_to_visible_radiation"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "So_bldepth"
    fldDesc%longname = " Ocean Boundary Layer Depth"
    fldDesc%stdname = " ocean_boundary_layer_depth"
    fldDesc%units = " m"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Fioi_tauy"
    fldDesc%longname = " Meridional surface stress"
    fldDesc%stdname = " surface_downward_northward_stress"
    fldDesc%units = " N m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Fioi_taux"
    fldDesc%longname = " Zonal surface stress"
    fldDesc%stdname = " surface_downward_eastward_stress"
    fldDesc%units = " N m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_dstwet2"
    fldDesc%longname = " Dust wet deposition flux (size 2)"
    fldDesc%stdname = " wet_deposition_flux_of_dust"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_dstwet3"
    fldDesc%longname = " Dust wet deposition flux (size 3)"
    fldDesc%stdname = " wet_deposition_flux_of_dust"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sx_avsdf"
    fldDesc%longname = " Diffuse albedo (visible radiation)"
    fldDesc%stdname = " surface_diffuse_albedo_due_to_visible_radiation"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_dstwet1"
    fldDesc%longname = " Dust wet deposition flux (size 1)"
    fldDesc%stdname = " wet_deposition_flux_of_dust"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_dstwet4"
    fldDesc%longname = " Dust wet deposition flux (size 4)"
    fldDesc%stdname = " wet_deposition_flux_of_dust"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "So_s"
    fldDesc%longname = " Sea surface salinity"
    fldDesc%stdname = " sea_surface_salinity"
    fldDesc%units = " g kg-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "So_u"
    fldDesc%longname = " Zonal sea water velocity"
    fldDesc%stdname = " eastward_sea_water_velocity"
    fldDesc%units = " m s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "So_t"
    fldDesc%longname = " Surface temperature"
    fldDesc%stdname = " surface_temperature"
    fldDesc%units = " K"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_snowc"
    fldDesc%longname = " Convective snow rate (water equivalent)"
    fldDesc%stdname = " convective_snowfall_flux"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_prec"
    fldDesc%longname = " Water flux (rain+snow)"
    fldDesc%stdname = " precipitation_flux"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Foxx_sen"
    fldDesc%longname = " Sensible heat flux"
    fldDesc%stdname = " surface_upward_sensible_heat_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sa_u"
    fldDesc%longname = " Zonal wind at the lowest model level"
    fldDesc%stdname = " eastward_wind"
    fldDesc%units = " m s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_ocphiwet"
    fldDesc%longname = " Hydrophylic organic carbon wet deposition flux"
    fldDesc%stdname = " wet_deposition_flux_of_hydrophylic_organic_carbon"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sa_v"
    fldDesc%longname = " Meridional wind at the lowest model level"
    fldDesc%stdname = " northward_wind"
    fldDesc%units = " m s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sw_hstokes"
    fldDesc%longname = " Stokes drift depth"
    fldDesc%stdname = " wave_model_stokes_drift_depth"
    fldDesc%units = " m"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sa_z"
    fldDesc%longname = " Height at the lowest model level"
    fldDesc%stdname = " height"
    fldDesc%units = " m"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Si_t"
    fldDesc%longname = " Surface temperature"
    fldDesc%stdname = " surface_temperature"
    fldDesc%units = " K"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sa_pslv"
    fldDesc%longname = " Sea level pressure"
    fldDesc%stdname = " air_pressure_at_sea_level"
    fldDesc%units = " Pa"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sl_tref"
    fldDesc%longname = " Reference temperature at 2 meters"
    fldDesc%stdname = " air_temperature"
    fldDesc%units = " K"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Si_anidf"
    fldDesc%longname = " Diffuse albedo (near-infrared radiation)"
    fldDesc%stdname = " surface_diffuse_albedo_due_to_near_infrared_radiation"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Si_avsdf"
    fldDesc%longname = " Diffuse albedo (visible radiation)"
    fldDesc%stdname = " surface_diffuse_albedo_due_to_visible_radiation"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sl_qref"
    fldDesc%longname = " Reference specific humidity at 2 meters"
    fldDesc%stdname = " specific_humidity"
    fldDesc%units = " kg kg-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faii_lwup"
    fldDesc%longname = " Surface upward longwave heat flux"
    fldDesc%stdname = " surface_net_upward_longwave_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Si_anidr"
    fldDesc%longname = " Direct albedo (near-infrared radiation)"
    fldDesc%stdname = " surface_direct_albedo_due_to_near_infrared_radiation"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Si_avsdr"
    fldDesc%longname = " Direct albedo (visible radiation)"
    fldDesc%stdname = " surface_direct_albedo_due_to_visible_radiation"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sw_lamult"
    fldDesc%longname = " Langmuir multiplier"
    fldDesc%stdname = " wave_model_langmuir_multiplier"
    fldDesc%units = "unknow"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "So_duu10n"
    fldDesc%longname = " Wind speed squared at 10 meters"
    fldDesc%stdname = " square_of_wind_speed"
    fldDesc%units = " m2 s-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_dstdry4"
    fldDesc%longname = " Dust dry deposition flux (size 4)"
    fldDesc%stdname = " dry_deposition_flux_of_dust"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_dstdry3"
    fldDesc%longname = " Dust dry deposition flux (size 3)"
    fldDesc%stdname = " dry_deposition_flux_of_dust"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_dstdry2"
    fldDesc%longname = " Dust dry deposition flux (size 2)"
    fldDesc%stdname = " dry_deposition_flux_of_dust"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_dstdry1"
    fldDesc%longname = " Dust dry deposition flux (size 1)"
    fldDesc%stdname = " dry_deposition_flux_of_dust"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Forr_roff"
    fldDesc%longname = " Water flux due to runoff (liquid)"
    fldDesc%stdname = " water_flux_into_sea_water"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Fall_evap"
    fldDesc%longname = " Evaporation water flux"
    fldDesc%stdname = " water_evaporation_flux"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Fall_sen"
    fldDesc%longname = " Sensible heat flux"
    fldDesc%stdname = " surface_upward_sensible_heat_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "So_ustar"
    fldDesc%longname = " Surface fraction velocity in ocean"
    fldDesc%stdname = " fraction_velocity"
    fldDesc%units = " m s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_swvdf"
    fldDesc%longname = " Diffuse visible incident solar radiation"
    fldDesc%stdname = " surface_downward_diffuse_shortwave_flux_due_to_visible_radiation"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Si_qref"
    fldDesc%longname = " Reference specific humidity at 2 meters"
    fldDesc%stdname = " specific_humidity"
    fldDesc%units = " kg kg-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sa_ptem"
    fldDesc%longname = " Potential temperature at the lowest model level"
    fldDesc%stdname = " air_potential_temperature"
    fldDesc%units = " K"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_swnet"
    fldDesc%longname = " Net shortwave radiation"
    fldDesc%stdname = " surface_net_shortwave_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faii_taux"
    fldDesc%longname = " Zonal surface stress"
    fldDesc%stdname = " surface_downward_eastward_stress"
    fldDesc%units = " N m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_swvdr"
    fldDesc%longname = " Direct visible incident solar radiation"
    fldDesc%stdname = " surface_downward_direct_shortwave_flux_due_to_visible_radiation"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_ocphidry"
    fldDesc%longname = " Hydrophylic organic carbon dry deposition flux"
    fldDesc%stdname = " dry_deposition_flux_of_hydrophylic_organic_carbon"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Foxx_lwup"
    fldDesc%longname = " Surface upward longwave heat flux"
    fldDesc%stdname = " surface_net_upward_longwave_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faii_sen"
    fldDesc%longname = " Sensible heat flux"
    fldDesc%stdname = " surface_upward_sensible_heat_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "area"
    fldDesc%longname = "unknow"
    fldDesc%stdname = " cell area"
    fldDesc%units = " m^2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sw_vstokes"
    fldDesc%longname = " Stokes drift v component"
    fldDesc%stdname = " wave_model_stokes_drift_northward_velocity"
    fldDesc%units = " m/s"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "lon"
    fldDesc%longname = "unknow"
    fldDesc%stdname = " longitude"
    fldDesc%units = " degrees east"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_bcphidry"
    fldDesc%longname = " Hydrophylic black carbon dry deposition flux"
    fldDesc%stdname = " dry_deposition_flux_of_hydrophylic_black_carbon"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sf_ofrac"
    fldDesc%longname = " Surface ocean fraction"
    fldDesc%stdname = " sea_area_fraction"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sa_tbot"
    fldDesc%longname = " Temperature at the lowest model level"
    fldDesc%stdname = " air_temperature"
    fldDesc%units = " K"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Si_snowh"
    fldDesc%longname = " Surface snow depth"
    fldDesc%stdname = " surface_snow_thickness"
    fldDesc%units = " m"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "So_re"
    fldDesc%longname = " Square of exch. coeff (tracers)"
    fldDesc%stdname = "unknow"
    fldDesc%units = "unknow"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faii_swnet"
    fldDesc%longname = " Net shortwave radiation"
    fldDesc%stdname = " surface_net_shortwave_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sl_anidf"
    fldDesc%longname = " Diffuse albedo (near-infrared radiation)"
    fldDesc%stdname = " surface_diffuse_albedo_due_to_near_infrared_radiation"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxx_evap"
    fldDesc%longname = " Evaporation water flux"
    fldDesc%stdname = " water_evaporation_flux"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Si_ifrac"
    fldDesc%longname = " Fractional ice coverage wrt ocean"
    fldDesc%stdname = " sea_ice_area_fraction"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sl_anidr"
    fldDesc%longname = " Direct albedo (near-infrared radiation)"
    fldDesc%stdname = " surface_direct_albedo_due_to_near_infrared_radiation"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Foxx_lat"
    fldDesc%longname = " Surface latent heat flux"
    fldDesc%stdname = " surface_upward_latent_heat_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Forr_ioff"
    fldDesc%longname = " Water flux due to runoff (frozen)"
    fldDesc%stdname = " frozen_water_flux_into_sea_water"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Fall_flxdst4"
    fldDesc%longname = " Dust flux (particle bin number 4)"
    fldDesc%stdname = " dust_flux"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Fall_flxdst3"
    fldDesc%longname = " Dust flux (particle bin number 3)"
    fldDesc%stdname = " dust_flux"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Fall_flxdst2"
    fldDesc%longname = " Dust flux (particle bin number 2)"
    fldDesc%stdname = " dust_flux"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Fall_flxdst1"
    fldDesc%longname = " Dust flux (particle bin number 1)"
    fldDesc%stdname = " dust_flux"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Fall_taux"
    fldDesc%longname = " Zonal surface stress"
    fldDesc%stdname = " surface_downward_eastward_stress"
    fldDesc%units = " N m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Fall_swnet"
    fldDesc%longname = " Net shortwave radiation"
    fldDesc%stdname = " surface_net_shortwave_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxx_sen"
    fldDesc%longname = " Sensible heat flux"
    fldDesc%stdname = " surface_upward_sensible_heat_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sl_avsdf"
    fldDesc%longname = " Diffuse albedo (visible radiation)"
    fldDesc%stdname = " surface_diffuse_albedo_due_to_visible_radiation"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Fall_lat"
    fldDesc%longname = " Surface latent heat flux"
    fldDesc%stdname = " surface_upward_latent_heat_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sx_tref"
    fldDesc%longname = " Reference temperature at 2 meters"
    fldDesc%stdname = " air_temperature"
    fldDesc%units = " K"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Foxx_tauy"
    fldDesc%longname = " Meridional surface stress"
    fldDesc%stdname = " surface_downward_northward_stress"
    fldDesc%units = " N m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Foxx_taux"
    fldDesc%longname = " Zonal surface stress"
    fldDesc%stdname = " surface_downward_eastward_stress"
    fldDesc%units = " N m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sl_avsdr"
    fldDesc%longname = " Direct albedo (visible radiation)"
    fldDesc%stdname = " surface_direct_albedo_due_to_visible_radiation"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faii_evap"
    fldDesc%longname = " Evaporation water flux"
    fldDesc%stdname = " water_evaporation_flux"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxx_lat"
    fldDesc%longname = " Surface latent heat flux"
    fldDesc%stdname = " surface_upward_latent_heat_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sa_shum"
    fldDesc%longname = " Specific humidity at the lowest model level"
    fldDesc%stdname = " specific_humidity"
    fldDesc%units = " kg kg-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sa_pbot"
    fldDesc%longname = " Pressure at the lowest model level"
    fldDesc%stdname = " air_pressure"
    fldDesc%units = " Pa"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxx_tauy"
    fldDesc%longname = " Meridional surface stress"
    fldDesc%stdname = " surface_downward_northward_stress"
    fldDesc%units = " N m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxx_taux"
    fldDesc%longname = " Zonal surface stress"
    fldDesc%stdname = " surface_downward_eastward_stress"
    fldDesc%units = " N m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Sa_dens"
    fldDesc%longname = " Density at the lowest model level"
    fldDesc%stdname = " air_density"
    fldDesc%units = " kg m-3"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_lwdn"
    fldDesc%longname = " Downward longwave heat flux"
    fldDesc%stdname = " downwelling_longwave_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "frac"
    fldDesc%longname = " area_fraction"
    fldDesc%stdname = " area fraction"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_swndr"
    fldDesc%longname = " Direct near-infrared incident solar radiation"
    fldDesc%stdname = " surface_downward_direct_shortwave_flux_due_to_near_infrared_radiation"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "lat"
    fldDesc%longname = "unknow"
    fldDesc%stdname = " latitude"
    fldDesc%units = " degrees north"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_swndf"
    fldDesc%longname = " Diffuse near-infrared incident solar radiation"
    fldDesc%stdname = " surface_downward_diffuse_shortwave_flux_due_to_near_infrared_radiation"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Fall_tauy"
    fldDesc%longname = " Meridional surface stress"
    fldDesc%stdname = " surface_downward_northward_stress"
    fldDesc%units = " N m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_snowl"
    fldDesc%longname = " Large-scale (stable) snow rate (water equivalent)"
    fldDesc%stdname = " large_scale_snowfall_flux"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "So_ssq"
    fldDesc%longname = " Surface saturation specific humidity in ocean"
    fldDesc%stdname = " specific_humidity_at_saturation"
    fldDesc%units = " kg kg-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Fioi_swpen"
    fldDesc%longname = " Net shortwave radiation penetrating into ice and ocean"
    fldDesc%stdname = " net_downward_shortwave_flux_in_sea_ice_due_to_penetration"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "mask"
    fldDesc%longname = "unknow"
    fldDesc%stdname = " mask"
    fldDesc%units = " unitless"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxa_bcphodry"
    fldDesc%longname = " Hydrophobic black carbon dry deposition flux"
    fldDesc%stdname = " dry_deposition_flux_of_hydrophobic_black_carbon"
    fldDesc%units = " kg m-2 s-1"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)
    fldDesc%shortname = "Faxx_lwup"
    fldDesc%longname = " Surface upward longwave heat flux"
    fldDesc%stdname = " surface_net_upward_longwave_flux"
    fldDesc%units = " W m-2"
    call fldsMeta_add(metaData%fldsMetaData, fldDesc)

end subroutine flds_init


end module base_field
