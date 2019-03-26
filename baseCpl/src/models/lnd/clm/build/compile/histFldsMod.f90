

module histFldsMod

!-----------------------------------------------------------------------
!BOP
!
! !MODULE: histFldsMod
!
! !DESCRIPTION:
! Module containing initialization of clm history fields and files
! This is the module that the user must modify in order to add new
! history fields or modify defaults associated with existing history
! fields.
!
! !USES:
  use shr_kind_mod, only: r8 => shr_kind_r8
  use clm_varcon, only: dzsoi_decomp
  use clm_varctl  , only : iulog
  implicit none
!
! !PUBLIC MEMBER FUNCTIONS:
  public hist_initFlds ! Build master field list of all possible history
                       ! file fields
!
! !REVISION HISTORY:
! Created by Mariana Vertenstein 03/2003
! heald (11/28/06)
! F. Li and S. Levis (11/06/12)
!EOP
!------------------------------------------------------------------------

contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: hist_initFlds
!
! !INTERFACE:
  subroutine hist_initFlds()
!
! !DESCRIPTION:
! Build master field list of all possible fields in a history file.
! Each field has associated with it a ``long\_name'' netcdf attribute that
! describes what the field is, and a ``units'' attribute. A subroutine is
! called to add each field to the masterlist.
!
! !USES:
    use clmtype
    use clm_varcon , only : spval
    use clm_varpar , only : maxpatch_glcmec
    use clm_atmlnd , only : clm_a2l
    use clm_varctl , only : create_glacier_mec_landunit, &
                            use_c13, use_c14





    use clm_glclnd , only : clm_s2x
    use histFileMod, only : hist_add_subscript, hist_addfld1d, hist_addfld2d, &
                            hist_printflds
    use surfrdMod  , only : crop_prog
    use shr_megan_mod  , only : shr_megan_linkedlist, shr_megan_megcomp_t, shr_megan_megcomps_n

    use clm_varpar , only :  ndecomp_cascade_transitions, ndecomp_pools, nlevdecomp, nlevdecomp_full
!
! !ARGUMENTS:
    implicit none

    type(shr_megan_megcomp_t), pointer :: meg_cmp
    integer :: imeg
!
! !REVISION HISTORY:
! Mariana Vertenstein: Created 03/2003
! Mariana Vertenstein: Updated interface to create history fields 10/2003
!
!EOP
!-----------------------------------------------------------------------
    real(r8), pointer :: data2dptr(:,:), data1dptr(:) ! temp. pointers for slicing larger arrays
    integer :: k,l, ii, jj
    character(24) :: fieldname
    character(100) :: longname
    character(8) :: vr_suffix
    character(10) :: active

    ! Determine what subscripts to add
    ! (uncomment the following call and modify it appropriately)

    ! call hist_add_subscript(subname='subscript_name', subdim=subscript_dim)

    ! NOTE: make a field not appear on the primary history tape by default -
    ! add the keyword to default='inactive' to the call to addfld_1d or addfld_2d

    ! add suffix if number of soil decomposition depths is greater than 1
    if (nlevdecomp .gt. 1) then
       vr_suffix = "_vr"
    else 
       vr_suffix = ""
    endif


    call hist_addfld1d (fname='H2OSFC',  units='mm',  &
         avgflag='A', long_name='surface water depth', &
         ptr_col=cws%h2osfc)

    call hist_addfld1d (fname='FH2OSFC',  units='unitless',  &
         avgflag='A', long_name='fraction of ground covered by surface water', &
         ptr_col=cps%frac_h2osfc)

    call hist_addfld1d (fname='QH2OSFC',  units='mm/s',  &
         avgflag='A', long_name='surface water runoff', &
         ptr_col=cwf%qflx_h2osfc_surf)

    call hist_addfld1d (fname='TH2OSFC',  units='K',  &
         avgflag='A', long_name='surface water temperature', &
         ptr_col=ces%t_h2osfc)

    call hist_addfld1d (fname='QFLOOD',  units='mm/s',  &
         avgflag='A', long_name='runoff from river flooding', &
         ptr_lnd=clm_a2l%forc_flood)

    call hist_addfld1d (fname='TWS',  units='mm',  &
         avgflag='A', long_name='total water storage', &
         ptr_lnd= grc%tws)

    call hist_addfld1d (fname='VOLR',  units='m3',  &
         avgflag='A', long_name='river channel water storage', &
         ptr_lnd=clm_a2l%volr)


    ! Snow properties
    ! These will be vertically averaged over the snow profile

    call hist_addfld1d (fname='SNOW_DEPTH',  units='m',  &
         avgflag='A', long_name='snow height of snow covered area', &
         ptr_col=cps%snow_depth, c2l_scale_type='urbanf')!, default='inactive')

    call hist_addfld1d (fname='SNOWDP',  units='m',  &
         avgflag='A', long_name='gridcell mean snow height', &
         ptr_col=cps%snowdp, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='FSNO',  units='unitless',  &
         avgflag='A', long_name='fraction of ground covered by snow', &
         ptr_col=cps%frac_sno, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='FSNO_EFF',  units='unitless',  &
         avgflag='A', long_name='effective fraction of ground covered by snow', &
         ptr_col=cps%frac_sno_eff, c2l_scale_type='urbanf')!, default='inactive')

    ! Temperatures

    call hist_addfld1d (fname='TSA', units='K',  &
         avgflag='A', long_name='2m air temperature', &
         ptr_pft=pes%t_ref2m)

    call hist_addfld1d (fname='TSA_U', units='K',  &
         avgflag='A', long_name='Urban 2m air temperature', &
         ptr_pft=pes%t_ref2m_u, set_nourb=spval)

    call hist_addfld1d (fname='TSA_R', units='K',  &
         avgflag='A', long_name='Rural 2m air temperature', &
         ptr_pft=pes%t_ref2m_r, set_spec=spval)

    call hist_addfld1d(fname='TBUILD', units='K',  &
         avgflag='A', long_name='internal urban building temperature', &
         ptr_lunit=lps%t_building, set_nourb=spval, l2g_scale_type='unity')

    call hist_addfld1d (fname='TREFMNAV', units='K',  &
         avgflag='A', long_name='daily minimum of average 2-m temperature', &
         ptr_pft=pes%t_ref2m_min)

    call hist_addfld1d (fname='TREFMXAV', units='K',  &
         avgflag='A', long_name='daily maximum of average 2-m temperature', &
         ptr_pft=pes%t_ref2m_max)

    call hist_addfld1d (fname='TREFMNAV_U', units='K',  &
         avgflag='A', long_name='Urban daily minimum of average 2-m temperature', &
         ptr_pft=pes%t_ref2m_min_u, set_nourb=spval)

    call hist_addfld1d (fname='TREFMXAV_U', units='K',  &
         avgflag='A', long_name='Urban daily maximum of average 2-m temperature', &
         ptr_pft=pes%t_ref2m_max_u, set_nourb=spval)

    call hist_addfld1d (fname='TREFMNAV_R', units='K',  &
         avgflag='A', long_name='Rural daily minimum of average 2-m temperature', &
         ptr_pft=pes%t_ref2m_min_r, set_spec=spval)

    call hist_addfld1d (fname='TREFMXAV_R', units='K',  &
         avgflag='A', long_name='Rural daily maximum of average 2-m temperature', &
         ptr_pft=pes%t_ref2m_max_r, set_spec=spval)

    call hist_addfld1d (fname='TV', units='K',  &
         avgflag='A', long_name='vegetation temperature', &
         ptr_pft=pes%t_veg)

    call hist_addfld1d (fname='TV24', units='K',  &
         avgflag='A', long_name='vegetation temperature (last 24hrs)', &
         ptr_pft=pvs%t_veg24, default='inactive')

    call hist_addfld1d (fname='TV240', units='K',  &
         avgflag='A', long_name='vegetation temperature (last 240hrs)', &
         ptr_pft=pvs%t_veg240, default='inactive')

    call hist_addfld1d (fname='TG',  units='K',  &
         avgflag='A', long_name='ground temperature', &
         ptr_col=ces%t_grnd, c2l_scale_type='urbans')

    call hist_addfld1d (fname='TG_U', units='K',  &
         avgflag='A', long_name='Urban ground temperature', &
         ptr_col=ces%t_grnd_u, set_nourb=spval, c2l_scale_type='urbans')

    call hist_addfld1d (fname='TG_R', units='K',  &
         avgflag='A', long_name='Rural ground temperature', &
         ptr_col=ces%t_grnd_r, set_spec=spval)

    call hist_addfld1d (fname='HCSOI',  units='MJ/m2',  &
         avgflag='A', long_name='soil heat content', &
         ptr_col=ces%hc_soi, set_lake=spval, set_urb=spval, l2g_scale_type='veg')

    call hist_addfld1d (fname='HC',  units='MJ/m2',  &
         avgflag='A', long_name='heat content of soil/snow/lake', &
         ptr_col=ces%hc_soisno, set_urb=spval)

    call hist_addfld2d (fname='TSOI',  units='K', type2d='levgrnd', &
         avgflag='A', long_name='soil temperature (vegetated landunits only)', &
         ptr_col=ces%t_soisno, l2g_scale_type='veg')

    call hist_addfld2d (fname='TSOI_ICE',  units='K', type2d='levgrnd', &
         avgflag='A', long_name='soil temperature (ice landunits only)', &
         ptr_col=ces%t_soisno, l2g_scale_type='ice')

    call hist_addfld1d (fname='TSOI_10CM',  units='K', &
         avgflag='A', long_name='soil temperature in top 10cm of soil', &
         ptr_col=ces%t_soi_10cm, set_urb=spval)

    call hist_addfld2d (fname='TLAKE',  units='K', type2d='levlak', &
         avgflag='A', long_name='lake temperature', &
         ptr_col=ces%t_lake)

    ! New lake fields
    call hist_addfld2d (fname='LAKEICEFRAC',  units='unitless', type2d='levlak', &
         avgflag='A', long_name='lake layer ice mass fraction', &
         ptr_col=cws%lake_icefrac)

         ! This will be more useful than LAKEICEFRAC for many users.
    call hist_addfld1d (fname='LAKEICETHICK', units='m', &
         avgflag='A', long_name='thickness of lake ice (including physical expansion on freezing)', &
         ptr_col=cws%lake_icethick, set_nolake=spval)

    call hist_addfld1d (fname='TKE1',  units='W/(mK)', &
         avgflag='A', long_name='top lake level eddy thermal conductivity', &
         ptr_col=cps%savedtke1)

    call hist_addfld1d (fname='EFLX_GRND_LAKE', units='W/m^2', &
         avgflag='A', long_name='net heat flux into lake/snow surface, excluding light transmission', &
         ptr_pft=pef%eflx_grnd_lake, set_nolake=spval)

    call hist_addfld1d (fname='RAM_LAKE', units='s/m', &
         avgflag='A', long_name='aerodynamic resistance for momentum (lakes only)', &
         ptr_pft=pps%ram1_lake, set_nolake=spval, default='inactive')

    call hist_addfld1d (fname='RH_LEAF', units='fraction', &
         avgflag='A', long_name='fractional humidity at leaf surface', &
         ptr_pft=pps%rh_leaf, set_spec=spval, default='inactive')

    call hist_addfld1d (fname='RHAF', units='fraction', &
         avgflag='A', long_name='fractional humidity of canopy air', &
         ptr_pft=pps%rhaf, set_spec=spval, default='inactive')

    call hist_addfld1d (fname='UST_LAKE', units='m/s', &
         avgflag='A', long_name='friction velocity (lakes only)', &
         ptr_col=cps%ust_lake, set_nolake=spval, default='inactive')

    call hist_addfld1d (fname='Z0MG', units='m', &
         avgflag='A', long_name='roughness length over ground, momentum', &
         ptr_col=cps%z0mg, default='inactive')

    call hist_addfld1d (fname='Z0HG', units='m', &
         avgflag='A', long_name='roughness length over ground, sensible heat', &
         ptr_col=cps%z0hg, default='inactive')

    call hist_addfld1d (fname='Z0QG', units='m', &
         avgflag='A', long_name='roughness length over ground, latent heat', &
         ptr_col=cps%z0qg, default='inactive')

    ! End new lake fields

    ! Allow active layer fields to be optionally output even if not running CN

    call hist_addfld1d (fname='ALT', units='m', &
         avgflag='A', long_name='current active layer thickness', &
         ptr_col=cps%alt, default='inactive')

    call hist_addfld1d (fname='ALTMAX', units='m', &
         avgflag='A', long_name='maximum annual active layer thickness', &
         ptr_col=cps%altmax, default='inactive')

    call hist_addfld1d (fname='ALTMAX_LASTYEAR', units='m', &
         avgflag='A', long_name='maximum prior year active layer thickness', &
         ptr_col=cps%altmax_lastyear, default='inactive')


    ! Specific humidity

    call hist_addfld1d (fname='Q2M', units='kg/kg',  &
         avgflag='A', long_name='2m specific humidity', &
         ptr_pft=pes%q_ref2m)

    ! Relative humidity

    call hist_addfld1d (fname='RH2M', units='%',  &
         avgflag='A', long_name='2m relative humidity', &
         ptr_pft=pes%rh_ref2m)

    call hist_addfld1d (fname='RH2M_U', units='%',  &
         avgflag='A', long_name='Urban 2m relative humidity', &
         ptr_pft=pes%rh_ref2m_u, set_nourb=spval)

    call hist_addfld1d (fname='RH2M_R', units='%',  &
         avgflag='A', long_name='Rural 2m specific humidity', &
         ptr_pft=pes%rh_ref2m_r, set_spec=spval)

    ! Wind

    call hist_addfld1d (fname='U10', units='m/s', &
         avgflag='A', long_name='10-m wind', &
         ptr_pft=pps%u10_clm)
    call hist_addfld1d (fname='VA', units='m/s', &
         avgflag='A', long_name='atmospheric wind speed plus convective velocity', &
         ptr_pft=pps%va, default='inactive')

    ! Surface radiation

    call hist_addfld1d (fname='SABV', units='W/m^2',  &
         avgflag='A', long_name='solar rad absorbed by veg', &
         ptr_pft=pef%sabv, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='SABG', units='W/m^2',  &
         avgflag='A', long_name='solar rad absorbed by ground', &
         ptr_pft=pef%sabg, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='SABG_PEN', units='watt/m^2',  &
         avgflag='A', long_name='Rural solar rad penetrating top soil or snow layer', &
         ptr_pft=pef%sabg_pen, set_spec=spval)

    call hist_addfld1d (fname='FSDSVD', units='W/m^2',  &
         avgflag='A', long_name='direct vis incident solar radiation', &
         ptr_pft=pef%fsds_vis_d)

    call hist_addfld1d (fname='FSDSND', units='W/m^2',  &
         avgflag='A', long_name='direct nir incident solar radiation', &
         ptr_pft=pef%fsds_nir_d)

    call hist_addfld1d (fname='FSDSVI', units='W/m^2',  &
         avgflag='A', long_name='diffuse vis incident solar radiation', &
         ptr_pft=pef%fsds_vis_i)

    call hist_addfld1d (fname='FSDSNI', units='W/m^2',  &
         avgflag='A', long_name='diffuse nir incident solar radiation', &
         ptr_pft=pef%fsds_nir_i)

    call hist_addfld1d (fname='FSRVD', units='W/m^2',  &
         avgflag='A', long_name='direct vis reflected solar radiation', &
         ptr_pft=pef%fsr_vis_d, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='FSRND', units='W/m^2',  &
         avgflag='A', long_name='direct nir reflected solar radiation', &
         ptr_pft=pef%fsr_nir_d, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='FSRVI', units='W/m^2',  &
         avgflag='A', long_name='diffuse vis reflected solar radiation', &
         ptr_pft=pef%fsr_vis_i, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='FSRNI', units='W/m^2',  &
         avgflag='A', long_name='diffuse nir reflected solar radiation', &
         ptr_pft=pef%fsr_nir_i, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='FSDSVDLN', units='W/m^2',  &
         avgflag='A', long_name='direct vis incident solar radiation at local noon', &
         ptr_pft=pef%fsds_vis_d_ln)

    call hist_addfld1d (fname='FSDSVILN', units='W/m^2',  &
         avgflag='A', long_name='diffuse vis incident solar radiation at local noon', &
         ptr_pft=pef%fsds_vis_i_ln)

    call hist_addfld1d (fname='PARVEGLN', units='W/m^2',  &
         avgflag='A', long_name='absorbed par by vegetation at local noon', &
         ptr_pft=pef%parveg_ln)

    call hist_addfld1d (fname='FSDSNDLN', units='W/m^2',  &
         avgflag='A', long_name='direct nir incident solar radiation at local noon', &
         ptr_pft=pef%fsds_nir_d_ln)

    call hist_addfld1d (fname='FSRVDLN', units='W/m^2',  &
         avgflag='A', long_name='direct vis reflected solar radiation at local noon', &
         ptr_pft=pef%fsr_vis_d_ln, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='FSRNDLN', units='W/m^2',  &
         avgflag='A', long_name='direct nir reflected solar radiation at local noon', &
         ptr_pft=pef%fsr_nir_d_ln, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='FSA', units='W/m^2',  &
         avgflag='A', long_name='absorbed solar radiation', &
         ptr_pft=pef%fsa, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='FSA_U', units='W/m^2',  &
         avgflag='A', long_name='Urban absorbed solar radiation', &
         ptr_pft=pef%fsa_u, c2l_scale_type='urbanf', set_nourb=spval)

    call hist_addfld1d (fname='FSA_R', units='W/m^2',  &
         avgflag='A', long_name='Rural absorbed solar radiation', &
         ptr_pft=pef%fsa_r, set_spec=spval)

    call hist_addfld1d (fname='FSR', units='W/m^2',  &
         avgflag='A', long_name='reflected solar radiation', &
         ptr_pft=pef%fsr, c2l_scale_type='urbanf')

    ! Rename of FSR for Urban intercomparision project
    call hist_addfld1d (fname='SWup', units='W/m^2',  &
         avgflag='A', long_name='upwelling shortwave radiation', &
         ptr_pft=pef%fsr, c2l_scale_type='urbanf', default='inactive')

    call hist_addfld1d (fname='FIRA', units='W/m^2',  &
         avgflag='A', long_name='net infrared (longwave) radiation', &
         ptr_pft=pef%eflx_lwrad_net, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='FIRA_U', units='W/m^2',  &
         avgflag='A', long_name='Urban net infrared (longwave) radiation', &
         ptr_pft=pef%eflx_lwrad_net_u, c2l_scale_type='urbanf', set_nourb=spval)

    call hist_addfld1d (fname='FIRA_R', units='W/m^2',  &
         avgflag='A', long_name='Rural net infrared (longwave) radiation', &
         ptr_pft=pef%eflx_lwrad_net_r, set_spec=spval)

    call hist_addfld1d (fname='FIRE', units='W/m^2',  &
         avgflag='A', long_name='emitted infrared (longwave) radiation', &
         ptr_pft=pef%eflx_lwrad_out, c2l_scale_type='urbanf')

    ! Rename of FIRE for Urban intercomparision project
    call hist_addfld1d (fname='LWup', units='W/m^2',  &
         avgflag='A', long_name='upwelling longwave radiation', &
         ptr_pft=pef%eflx_lwrad_out, c2l_scale_type='urbanf', default='inactive')

    call hist_addfld1d (fname='BUILDHEAT', units='W/m^2',  &
         avgflag='A', long_name='heat flux from urban building interior to walls and roof', &
         ptr_col=cef%eflx_building_heat, set_nourb=0._r8, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='URBAN_AC', units='W/m^2',  &
         avgflag='A', long_name='urban air conditioning flux', &
         ptr_col=cef%eflx_urban_ac, set_nourb=0._r8, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='URBAN_HEAT', units='W/m^2',  &
         avgflag='A', long_name='urban heating flux', &
         ptr_col=cef%eflx_urban_heat, set_nourb=0._r8, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='TRAFFICFLUX', units='W/m^2',  &
         avgflag='A', long_name='sensible heat flux from urban traffic', &
         ptr_pft=pef%eflx_traffic_pft, set_nourb=0._r8, c2l_scale_type='urbanf', &
         default='inactive')

    call hist_addfld1d (fname='WASTEHEAT', units='W/m^2',  &
         avgflag='A', long_name='sensible heat flux from heating/cooling sources of urban waste heat', &
         ptr_pft=pef%eflx_wasteheat_pft, set_nourb=0._r8, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='HEAT_FROM_AC', units='W/m^2',  &
         avgflag='A', long_name='sensible heat flux put into canyon due to heat removed from air conditioning', &
         ptr_pft=pef%eflx_heat_from_ac_pft, set_nourb=0._r8, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='Qanth', units='W/m^2',  &
         avgflag='A', long_name='anthropogenic heat flux', &
         ptr_pft=pef%eflx_anthro, set_nourb=0._r8, c2l_scale_type='urbanf', &
         default='inactive')

    call hist_addfld1d (fname='Rnet', units='W/m^2',  &
         avgflag='A', long_name='net radiation', &
         ptr_pft=pef%netrad, c2l_scale_type='urbanf', &
         default='inactive')

    ! Solar zenith angle and solar declination angle

    call hist_addfld1d (fname='COSZEN', units='none', &
         avgflag='A', long_name='cosine of solar zenith angle', &
         ptr_col=cps%coszen, default='inactive')

    call hist_addfld1d (fname='DECL', units='radians', &
         avgflag='A', long_name='solar declination angle', &
         ptr_col=cps%decl, default='inactive')

    ! Surface energy fluxes

    call hist_addfld1d (fname='FCTR', units='W/m^2',  &
         avgflag='A', long_name='canopy transpiration', &
         ptr_pft=pef%eflx_lh_vegt, set_lake=0._r8, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='FCEV', units='W/m^2',  &
         avgflag='A', long_name='canopy evaporation', &
         ptr_pft=pef%eflx_lh_vege, set_lake=0._r8, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='FGEV', units='W/m^2',  &
         avgflag='A', long_name='ground evaporation', &
         ptr_pft=pef%eflx_lh_grnd, c2l_scale_type='urbanf') 

    call hist_addfld1d (fname='FSH_NODYNLNDUSE', units='W/m^2',  &
         avgflag='A', long_name='sensible heat not including correction for land use change', &
         ptr_pft=pef%eflx_sh_tot, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='FSH', units='W/m^2',  &
         avgflag='A', long_name='sensible heat', &
         ptr_lnd=gef%eflx_sh_totg)

    call hist_addfld1d (fname='FSH_U', units='W/m^2',  &
         avgflag='A', long_name='Urban sensible heat', &
         ptr_pft=pef%eflx_sh_tot_u, c2l_scale_type='urbanf', set_nourb=spval)

    call hist_addfld1d (fname='FSH_R', units='W/m^2',  &
         avgflag='A', long_name='Rural sensible heat', &
         ptr_pft=pef%eflx_sh_tot_r, set_spec=spval)

    call hist_addfld1d (fname='GC_HEAT1',  units='J/m^2',  &
         avgflag='A', long_name='initial gridcell total heat content', &
         ptr_lnd=ges%gc_heat1)

    call hist_addfld1d (fname='GC_HEAT2',  units='J/m^2',  &
         avgflag='A', long_name='post land cover change total heat content', &
         ptr_lnd=ges%gc_heat2, default='inactive')

    call hist_addfld1d (fname='EFLX_DYNBAL',  units='W/m^2',  &
         avgflag='A', long_name='dynamic land cover change conversion energy flux', &
         ptr_lnd=gef%eflx_dynbal)

    call hist_addfld1d (fname='Qh', units='W/m^2',  &
         avgflag='A', long_name='sensible heat', &
         ptr_pft=pef%eflx_sh_tot, c2l_scale_type='urbanf', &
         default = 'inactive')

    call hist_addfld1d (fname='Qle', units='W/m^2',  &
         avgflag='A', long_name='total evaporation', &
         ptr_pft=pef%eflx_lh_tot, c2l_scale_type='urbanf', &
         default = 'inactive')

    call hist_addfld1d (fname='EFLX_LH_TOT', units='W/m^2', &
         avgflag='A', long_name='total latent heat flux [+ to atm]', &
         ptr_pft=pef%eflx_lh_tot, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='EFLX_LH_TOT_U', units='W/m^2',  &
         avgflag='A', long_name='Urban total evaporation', &
         ptr_pft=pef%eflx_lh_tot_u, c2l_scale_type='urbanf', set_nourb=spval)

    call hist_addfld1d (fname='EFLX_LH_TOT_R', units='W/m^2',  &
         avgflag='A', long_name='Rural total evaporation', &
         ptr_pft=pef%eflx_lh_tot_r, set_spec=spval)

    call hist_addfld1d (fname='Qstor', units='W/m^2',  &
         avgflag='A', long_name='storage heat flux (includes snowmelt)', &
         ptr_pft=pef%eflx_soil_grnd, c2l_scale_type='urbanf', &
         default = 'inactive')

    call hist_addfld1d (fname='FSH_V', units='W/m^2',  &
         avgflag='A', long_name='sensible heat from veg', &
         ptr_pft=pef%eflx_sh_veg, set_lake=0._r8, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='FSH_G', units='W/m^2',  &
         avgflag='A', long_name='sensible heat from ground', &
         ptr_pft=pef%eflx_sh_grnd, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='FGR', units='W/m^2',  &
!         avgflag='A', long_name='heat flux into soil/snow including snow melt', &
         avgflag='A', long_name='heat flux into soil/snow including snow melt and lake / snow light transmission', &
         ptr_pft=pef%eflx_soil_grnd, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='FGR_U', units='W/m^2',  &
         avgflag='A', long_name='Urban heat flux into soil/snow including snow melt', &
         ptr_pft=pef%eflx_soil_grnd_u, c2l_scale_type='urbanf', set_nourb=spval)

    call hist_addfld1d (fname='FGR_R', units='W/m^2',  &
!         avgflag='A', long_name='Rural heat flux into soil/snow including snow melt', &
         avgflag='A', long_name='Rural heat flux into soil/snow including snow melt and snow light transmission', &
         ptr_pft=pef%eflx_soil_grnd_r, set_spec=spval)

    call hist_addfld1d (fname='FSM',  units='W/m^2',  &
         avgflag='A', long_name='snow melt heat flux', &
         ptr_col=cef%eflx_snomelt, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='FSM_U',  units='W/m^2',  &
         avgflag='A', long_name='Urban snow melt heat flux', &
         ptr_col=cef%eflx_snomelt_u, c2l_scale_type='urbanf', set_nourb=spval)

    call hist_addfld1d (fname='FSM_R',  units='W/m^2',  &
         avgflag='A', long_name='Rural snow melt heat flux', &
         ptr_col=cef%eflx_snomelt_r, set_spec=spval)

    call hist_addfld1d (fname='FGR12',  units='W/m^2',  &
         avgflag='A', long_name='heat flux between soil layers 1 and 2', &
         ptr_col=cef%eflx_fgr12, set_lake=spval)

    call hist_addfld2d (fname='FGR_SOIL_R', units='watt/m^2', type2d='levgrnd', &
         avgflag='A', long_name='Rural downward heat flux at interface below each soil layer', &
         ptr_col=cef%eflx_fgr, set_spec=spval, default='inactive')

    call hist_addfld1d (fname='TAUX', units='kg/m/s^2',  &
         avgflag='A', long_name='zonal surface stress', &
         ptr_pft=pmf%taux)

    ! Rename of TAUX for Urban intercomparision project (when U=V)
    call hist_addfld1d (fname='Qtau', units='kg/m/s^2',  &
         avgflag='A', long_name='momentum flux', &
         ptr_pft=pmf%taux, default='inactive')

    call hist_addfld1d (fname='TAUY', units='kg/m/s^2',  &
         avgflag='A', long_name='meridional surface stress', &
         ptr_pft=pmf%tauy)

    ! Vegetation phenology

    call hist_addfld1d (fname='ELAI', units='m^2/m^2', &
          avgflag='A', long_name='exposed one-sided leaf area index', &
         ptr_pft=pps%elai)

    call hist_addfld1d (fname='ESAI', units='m^2/m^2', &
          avgflag='A', long_name='exposed one-sided stem area index', &
         ptr_pft=pps%esai)

    call hist_addfld1d (fname='LAISUN', units='none', &
         avgflag='A', long_name='sunlit projected leaf area index', &
         ptr_pft=pps%laisun, set_urb=0._r8)

    call hist_addfld1d (fname='LAISHA', units='none', &
         avgflag='A', long_name='shaded projected leaf area index', &
         ptr_pft=pps%laisha, set_urb=0._r8)

    call hist_addfld1d (fname='TLAI', units='none', &
         avgflag='A', long_name='total projected leaf area index', &
         ptr_pft=pps%tlai)

    call hist_addfld1d (fname='TSAI', units='none', &
         avgflag='A', long_name='total projected stem area index', &
         ptr_pft=pps%tsai)

    ! Canopy physiology

    call hist_addfld1d (fname='RSSUN', units='s/m',  &
         avgflag='M', long_name='sunlit leaf stomatal resistance', &
         ptr_pft=pps%rssun, set_lake=spval, set_urb=spval, &
         default='inactive')

    call hist_addfld1d (fname='RSSHA', units='s/m',  &
         avgflag='M', long_name='shaded leaf stomatal resistance', &
         ptr_pft=pps%rssha, set_lake=spval, set_urb=spval, &
         default='inactive')

    call hist_addfld1d (fname='BTRAN', units='unitless',  &
         avgflag='A', long_name='transpiration beta factor', &
         ptr_pft=pps%btran, set_lake=spval, set_urb=spval)

    call hist_addfld1d (fname='FPSN', units='umol/m2s',  &
         avgflag='A', long_name='photosynthesis', &
         ptr_pft=pcf%fpsn, set_lake=0._r8, set_urb=0._r8)

    call hist_addfld1d (fname='FPSN_WC', units='umol/m2s',  &
         avgflag='A', long_name='Rubisco-limited photosynthesis', &
         ptr_pft=pcf%fpsn_wc, set_lake=0._r8, set_urb=0._r8)

    call hist_addfld1d (fname='FPSN_WJ', units='umol/m2s',  &
         avgflag='A', long_name='RuBP-limited photosynthesis', &
         ptr_pft=pcf%fpsn_wj, set_lake=0._r8, set_urb=0._r8)

    call hist_addfld1d (fname='FPSN_WP', units='umol/m2s',  &
         avgflag='A', long_name='Product-limited photosynthesis', &
         ptr_pft=pcf%fpsn_wp, set_lake=0._r8, set_urb=0._r8)

    call hist_addfld1d (fname='DSTFLXT', units='kg/m2/s',  &
         avgflag='A', long_name='total surface dust emission', &
         ptr_pft=pdf%flx_mss_vrt_dst_tot, set_lake=0._r8, set_urb=0._r8)
    call hist_addfld1d (fname='DPVLTRB1', units='m/s',  &
         avgflag='A', long_name='turbulent deposition velocity 1', &
         ptr_pft=pdf%vlc_trb_1, default='inactive')
    call hist_addfld1d (fname='DPVLTRB2', units='m/s',  &
         avgflag='A', long_name='turbulent deposition velocity 2', &
         ptr_pft=pdf%vlc_trb_2, default='inactive')
    call hist_addfld1d (fname='DPVLTRB3', units='m/s',  &
         avgflag='A', long_name='turbulent deposition velocity 3', &
         ptr_pft=pdf%vlc_trb_3, default='inactive')
    call hist_addfld1d (fname='DPVLTRB4', units='m/s',  &
         avgflag='A', long_name='turbulent deposition velocity 4', &
         ptr_pft=pdf%vlc_trb_4, default='inactive')

    ! for MEGAN emissions diagnositics
    if (shr_megan_megcomps_n>0) then
       
       ! loop over megan compounds
       meg_cmp => shr_megan_linkedlist
       do while(associated(meg_cmp))
          imeg = meg_cmp%index

          call hist_addfld1d ( fname='MEG_'//trim(meg_cmp%name), units='kg/m2/sec',  &
               avgflag='A', long_name='MEGAN flux', &
               ptr_pft=pvf%meg(imeg)%flux_out, set_lake=0._r8, set_urb=0._r8 )

          meg_cmp => meg_cmp%next_megcomp
       enddo
       
       call hist_addfld1d (fname='VOCFLXT', units='moles/m2/sec',  &
            avgflag='A', long_name='total VOC flux into atmosphere', &
            ptr_pft=pvf%vocflx_tot, set_lake=0._r8, set_urb=0._r8)

       call hist_addfld1d (fname='GAMMA', units='non',  &
            avgflag='A', long_name='total gamma for VOC calc', &
            ptr_pft=pvf%gamma_out, set_lake=0._r8, default='inactive')

       call hist_addfld1d (fname='GAMMAL', units='non',  &
            avgflag='A', long_name='gamma L for VOC calc', &
            ptr_pft=pvf%gammaL_out, set_lake=0._r8, default='inactive')

       call hist_addfld1d (fname='GAMMAT', units='non',  &
            avgflag='A', long_name='gamma T for VOC calc', &
            ptr_pft=pvf%gammaT_out, set_lake=0._r8, default='inactive')

       call hist_addfld1d (fname='GAMMAP', units='non',  &
            avgflag='A', long_name='gamma P for VOC calc', &
            ptr_pft=pvf%gammaP_out, set_lake=0._r8, default='inactive')

       call hist_addfld1d (fname='GAMMAA', units='non',  &
            avgflag='A', long_name='gamma A for VOC calc', &
            ptr_pft=pvf%gammaA_out, set_lake=0._r8, default='inactive')

       call hist_addfld1d (fname='GAMMAS', units='non',  &
            avgflag='A', long_name='gamma S for VOC calc', &
            ptr_pft=pvf%gammaS_out, set_lake=0._r8, default='inactive')

       call hist_addfld1d (fname='GAMMAC', units='non',  &
            avgflag='A', long_name='gamma C for VOC calc', &
            ptr_pft=pvf%gammaC_out, set_lake=0._r8, default='inactive')

       call hist_addfld1d (fname='EOPT', units='non',  &
            avgflag='A', long_name='Eopt coefficient for VOC calc', &
            ptr_pft=pvf%Eopt_out, set_lake=0._r8, default='inactive')

       call hist_addfld1d (fname='TOPT', units='non',  &
            avgflag='A', long_name='topt coefficient for VOC calc', &
            ptr_pft=pvf%topt_out, set_lake=0._r8, default='inactive')

       call hist_addfld1d (fname='ALPHA', units='non',  &
            avgflag='A', long_name='alpha coefficient for VOC calc', &
            ptr_pft=pvf%alpha_out, set_lake=0._r8, default='inactive')

       call hist_addfld1d (fname='CP', units='non',  &
            avgflag='A', long_name='cp coefficient for VOC calc', &
            ptr_pft=pvf%cp_out, set_lake=0._r8, default='inactive')

       call hist_addfld1d (fname='PAR_sun', units='umol/m2/s', &
            avgflag='A', long_name='sunlit PAR', &
            ptr_pft=pvf%paru_out, set_lake=0._r8, default='inactive')

       call hist_addfld1d (fname='PAR24_sun', units='umol/m2/s', &
            avgflag='A', long_name='sunlit PAR (24 hrs)', &
            ptr_pft=pvf%par24u_out, set_lake=0._r8, default='inactive')

       call hist_addfld1d (fname='PAR240_sun', units='umol/m2/s', &
            avgflag='A', long_name='sunlit PAR (240 hrs)', &
            ptr_pft=pvf%par240u_out, set_lake=0._r8, default='inactive')

       call hist_addfld1d (fname='PAR_shade', units='umol/m2/s', &
            avgflag='A', long_name='shade PAR', &
            ptr_pft=pvf%para_out, set_lake=0._r8, default='inactive')

       call hist_addfld1d (fname='PAR24_shade', units='umol/m2/s', &
            avgflag='A', long_name='shade PAR (24 hrs)', &
            ptr_pft=pvf%par24a_out, set_lake=0._r8, default='inactive')

       call hist_addfld1d (fname='PAR240_shade', units='umol/m2/s', &
            avgflag='A', long_name='shade PAR (240 hrs)', &
            ptr_pft=pvf%par240a_out, set_lake=0._r8, default='inactive')

    endif

    call hist_addfld1d (fname='FSUN24', units='K',  &
         avgflag='A', long_name='fraction sunlit (last 24hrs)', &
         ptr_pft=pvs%fsun24, default='inactive')

    call hist_addfld1d (fname='FSUN240', units='K',  &
         avgflag='A', long_name='fraction sunlit (last 240hrs)', &
         ptr_pft=pvs%fsun240, default='inactive')

    call hist_addfld1d (fname='FSI24', units='K',  &
         avgflag='A', long_name='indirect radiation (last 24hrs)', &
         ptr_pft=pvs%fsi24, default='inactive')

    call hist_addfld1d (fname='FSI240', units='K',  &
         avgflag='A', long_name='indirect radiation (last 240hrs)', &
         ptr_pft=pvs%fsi240, default='inactive')

    call hist_addfld1d (fname='FSD24', units='K',  &
         avgflag='A', long_name='direct radiation (last 24hrs)', &
         ptr_pft=pvs%fsd24, default='inactive')

    call hist_addfld1d (fname='FSD240', units='K',  &
         avgflag='A', long_name='direct radiation (last 240hrs)', &
         ptr_pft=pvs%fsd240, default='inactive')

    ! Hydrology

    call hist_addfld1d (fname='SoilAlpha',  units='unitless',  &
         avgflag='A', long_name='factor limiting ground evap', &
         ptr_col=cws%soilalpha, set_urb=spval)

    call hist_addfld1d (fname='SoilAlpha_U',  units='unitless',  &
         avgflag='A', long_name='urban factor limiting ground evap', &
         ptr_col=cws%soilalpha_u, set_nourb=spval)

    call hist_addfld1d (fname='FCOV',  units='unitless',  &
         avgflag='A', long_name='fractional impermeable area', &
         ptr_col=cws%fcov, l2g_scale_type='veg')
    call hist_addfld1d (fname='FSAT',  units='unitless',  &
         avgflag='A', long_name='fractional area with water table at surface', &
         ptr_col=cws%fsat, l2g_scale_type='veg')
    call hist_addfld1d (fname='ZWT',  units='m',  &
         avgflag='A', long_name='water table depth (vegetated landunits only)', &
         ptr_col=cws%zwt, l2g_scale_type='veg')

    call hist_addfld1d (fname='INT_SNOW',  units='mm',  &
         avgflag='A', long_name='accumulated swe (vegetated landunits only)', &
         ptr_col=cws%int_snow, l2g_scale_type='veg')
    call hist_addfld1d (fname='FROST_TABLE',  units='m',  &
         avgflag='A', long_name='frost table depth (vegetated landunits only)', &
         ptr_col=cws%frost_table, l2g_scale_type='veg')
    call hist_addfld1d (fname='ZWT_PERCH',  units='m',  &
         avgflag='A', long_name='perched water table depth (vegetated landunits only)', &
         ptr_col=cws%zwt_perched, l2g_scale_type='veg')
    call hist_addfld1d (fname='QDRAI_PERCH',  units='mm/s',  &
         avgflag='A', long_name='perched wt drainage', &
         ptr_col=cwf%qflx_drain_perched, c2l_scale_type='urbanf')
    call hist_addfld1d (fname='QDRAI_XS',  units='mm/s',  &
         avgflag='A', long_name='saturation excess drainage', &
         ptr_col=cwf%qflx_rsub_sat, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='WA',  units='mm',  &
         avgflag='A', long_name='water in the unconfined aquifer (vegetated landunits only)', &
         ptr_col=cws%wa, l2g_scale_type='veg')

    call hist_addfld1d (fname='QCHARGE',  units='mm/s',  &
         avgflag='A', long_name='aquifer recharge rate (vegetated landunits only)', &
         ptr_col=cws%qcharge, l2g_scale_type='veg')








    active = "inactive"

    call hist_addfld2d (fname='SMP',  units='mm', type2d='levgrnd',  &
         avgflag='A', long_name='soil matric potential (vegetated landunits only)', &
         ptr_col=cws%smp_l, set_spec=spval, l2g_scale_type='veg', default=active)

    call hist_addfld2d (fname='HK',  units='mm/s', type2d='levgrnd',  &
         avgflag='A', long_name='hydraulic conductivity (vegetated landunits only)', &
         ptr_col=cws%hk_l, set_spec=spval, l2g_scale_type='veg', default='inactive')

    call hist_addfld1d (fname='H2OSNO',  units='mm',  &
         avgflag='A', long_name='snow depth (liquid water)', &
         ptr_col=cws%h2osno, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='ERRH2OSNO',  units='mm',  &
         avgflag='A', long_name='imbalance in snow depth (liquid water)', &
         ptr_col=cws%errh2osno, c2l_scale_type='urbanf')

    ! As defined here, snow_sources - snow_sinks will equal the change in h2osno at 
    ! any given time step but only if there is at least one snow layer (for all landunits 
    ! except lakes).  h2osno also includes snow that is part of the soil column (an 
    ! initial snow layer is only created if h2osno > 10mm). Also note that monthly average
    ! files of snow_sources and snow sinks must be weighted by number of days in the month to 
    ! diagnose, for example, an annual value of the change in h2osno. 

    call hist_addfld1d (fname='SNOW_SOURCES',  units='mm/s',  &
         avgflag='A', long_name='snow sources (liquid water)', &
         ptr_col=cws%snow_sources, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='SNOW_SINKS',  units='mm/s',  &
         avgflag='A', long_name='snow sinks (liquid water)', &
         ptr_col=cws%snow_sinks, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='H2OCAN', units='mm',  &
         avgflag='A', long_name='intercepted water', &
         ptr_pft=pws%h2ocan, set_lake=0._r8)

    call hist_addfld2d (fname='H2OSOI',  units='mm3/mm3', type2d='levgrnd', &
         avgflag='A', long_name='volumetric soil water (vegetated landunits only)', &
         ptr_col=cws%h2osoi_vol, l2g_scale_type='veg')

    call hist_addfld2d (fname='SOILLIQ',  units='kg/m2', type2d='levgrnd', &
         avgflag='A', long_name='soil liquid water (vegetated landunits only)', &
         ptr_col=cws%h2osoi_liq, l2g_scale_type='veg')

    call hist_addfld2d (fname='SOILICE',  units='kg/m2', type2d='levgrnd', &
         avgflag='A', long_name='soil ice (vegetated landunits only)', &
         ptr_col=cws%h2osoi_ice, l2g_scale_type='veg')

    call hist_addfld1d (fname='SOILWATER_10CM',  units='kg/m2', &
         avgflag='A', long_name='soil liquid water + ice in top 10cm of soil (veg landunits only)', &
         ptr_col=cws%h2osoi_liqice_10cm, set_urb=spval, l2g_scale_type='veg', &
         set_lake=spval)

    call hist_addfld1d (fname='SNOWLIQ',  units='kg/m2',  &
         avgflag='A', long_name='snow liquid water', &
         ptr_col=cws%snowliq)

    call hist_addfld1d (fname='SNOWICE',  units='kg/m2',  &
         avgflag='A', long_name='snow ice', &
         ptr_col=cws%snowice)

    call hist_addfld1d (fname='QTOPSOIL',  units='mm/s',  &
         avgflag='A', long_name='water input to surface', &
         ptr_col=cwf%qflx_top_soil, c2l_scale_type='urbanf', default='inactive')

    call hist_addfld1d (fname='QINFL',  units='mm/s',  &
         avgflag='A', long_name='infiltration', &
         ptr_col=cwf%qflx_infl, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='QOVER',  units='mm/s',  &
         avgflag='A', long_name='surface runoff', &
         ptr_col=cwf%qflx_surf, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='QRGWL',  units='mm/s',  &
         avgflag='A', long_name='surface runoff at glaciers (liquid only), wetlands, lakes', &
         ptr_col=cwf%qflx_qrgwl, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='QSNWCPLIQ', units='mm H2O/s', &
         avgflag='A', long_name='excess rainfall due to snow capping', &
         ptr_pft=pwf%qflx_snwcp_liq, c2l_scale_type='urbanf', default='inactive')

    call hist_addfld1d (fname='QSNWCPICE_NODYNLNDUSE', units='mm H2O/s', &
         avgflag='A', &
         long_name='excess snowfall due to snow capping not including correction for land use change', &
         ptr_pft=pwf%qflx_snwcp_ice, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='QSNWCPICE',  units='mm/s',  &
         avgflag='A', long_name='excess snowfall due to snow capping', &
         ptr_lnd=gwf%qflx_snwcp_iceg)

    call hist_addfld1d (fname='QDRAI',  units='mm/s',  &
         avgflag='A', long_name='sub-surface drainage', &
         ptr_col=cwf%qflx_drain, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='QRUNOFF_NODYNLNDUSE',  units='mm/s',  &
         avgflag='A', &
         long_name='total liquid runoff (does not include QSNWCPICE) not including correction for land use change', &
         ptr_col=cwf%qflx_runoff, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='QRUNOFF',  units='mm/s',  &
         avgflag='A', long_name='total liquid runoff (does not include QSNWCPICE)', &
         ptr_lnd=gwf%qflx_runoffg)

    call hist_addfld1d (fname='GC_LIQ1',  units='mm',  &
         avgflag='A', long_name='initial gridcell total liq content', &
         ptr_lnd=gws%gc_liq1)

    call hist_addfld1d (fname='GC_LIQ2',  units='mm',  &  
         avgflag='A', long_name='post landuse change gridcell total liq content', &              
         ptr_lnd=gws%gc_liq2, default='inactive')     

    call hist_addfld1d (fname='QFLX_LIQ_DYNBAL',  units='mm/s',  &  
         avgflag='A', long_name='liq dynamic land cover change conversion runoff flux', &              
         ptr_lnd=gwf%qflx_liq_dynbal)     

    call hist_addfld1d (fname='GC_ICE1',  units='mm',  &  
         avgflag='A', long_name='initial gridcell total ice content', &              
         ptr_lnd=gws%gc_ice1)     

    call hist_addfld1d (fname='GC_ICE2',  units='mm',  &  
         avgflag='A', long_name='post land cover change total ice content', &              
         ptr_lnd=gws%gc_ice2, default='inactive')

    call hist_addfld1d (fname='QFLX_ICE_DYNBAL',  units='mm/s',  &
         avgflag='A', long_name='ice dynamic land cover change conversion runoff flux', &                                   
         ptr_lnd=gwf%qflx_ice_dynbal)

    call hist_addfld1d (fname='QRUNOFF_U', units='mm/s',  &
         avgflag='A', long_name='Urban total runoff', &
         ptr_col=cwf%qflx_runoff_u, set_nourb=spval, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='QRUNOFF_R', units='mm/s',  &
         avgflag='A', long_name='Rural total runoff', &
         ptr_col=cwf%qflx_runoff_r, set_spec=spval)

    call hist_addfld1d (fname='QINTR', units='mm/s',  &
         avgflag='A', long_name='interception', &
         ptr_pft=pwf%qflx_prec_intr, set_lake=0._r8)

    call hist_addfld1d (fname='QDRIP', units='mm/s',  &
         avgflag='A', long_name='throughfall', &
         ptr_pft=pwf%qflx_prec_grnd, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='QSNOMELT',  units='mm/s',  &
         avgflag='A', long_name='snow melt', &
         ptr_col=cwf%qflx_snow_melt, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='QSNOFRZ', units='kg/m2/s', &
         avgflag='A', long_name='column-integrated snow freezing rate', &
         ptr_col=cwf%qflx_snofrz_col, default='inactive', &
         set_lake=spval, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='QSOIL', units='mm/s',  &
         avgflag='A', long_name= &
         'Ground evaporation (soil/snow evaporation + soil/snow sublimation - dew)', &
         ptr_pft=pwf%qflx_evap_soi, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='QVEGE', units='mm/s',  &
         avgflag='A', long_name='canopy evaporation', &
         ptr_pft=pwf%qflx_evap_can, set_lake=0._r8, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='QVEGT', units='mm/s',  &
         avgflag='A', long_name='canopy transpiration', &
         ptr_pft=pwf%qflx_tran_veg, set_lake=0._r8, c2l_scale_type='urbanf')

    call hist_addfld1d (fname='QIRRIG', units='mm/s', &
         avgflag='A', long_name='water added through irrigation', &
         ptr_col=cwf%qflx_irrig, set_lake=0._r8)

    if (create_glacier_mec_landunit) then

       call hist_addfld1d (fname='QICE',  units='mm/s',  &
            avgflag='A', long_name='ice growth/melt', &
            ptr_col=cwf%qflx_glcice, set_noglcmec=spval)

       call hist_addfld1d (fname='QICE_FRZ',  units='mm/s',  &
            avgflag='A', long_name='ice growth', &
            ptr_col=cwf%qflx_glcice_frz, set_noglcmec=spval)

       call hist_addfld1d (fname='QICE_MELT',  units='mm/s',  &
            avgflag='A', long_name='ice melt', &
            ptr_col=cwf%qflx_glcice_melt, set_noglcmec=spval)

       call hist_addfld1d (fname='gris_mask',  units='unitless',  &
            avgflag='A', long_name='Greenland mask', &
            ptr_gcell= grc%gris_mask)

       call hist_addfld1d (fname='gris_area',  units='km^2',  &
            avgflag='A', long_name='Greenland ice area', &
            ptr_gcell= grc%gris_area)

       call hist_addfld1d (fname='aais_mask',  units='unitless',  &
            avgflag='A', long_name='Antarctic mask', &
            ptr_gcell= grc%aais_mask)

       call hist_addfld1d (fname='aais_area',  units='km^2',  &
            avgflag='A', long_name='Antarctic ice area', &
            ptr_gcell= grc%aais_area)

   endif

    ! Water and energy balance checks

    call hist_addfld1d (fname='ERRSOI',  units='W/m^2',  &
         avgflag='A', long_name='soil/lake energy conservation error', &
         ptr_col=cebal%errsoi)

    call hist_addfld1d (fname='ERRSEB',  units='W/m^2',  &
         avgflag='A', long_name='surface energy conservation error', &
         ptr_pft=pebal%errseb)

    call hist_addfld1d (fname='ERRSOL',  units='W/m^2',  &
         avgflag='A', long_name='solar radiation conservation error', &
         ptr_pft=pebal%errsol, set_urb=spval)

    call hist_addfld1d (fname='ERRH2O', units='mm',  &
         avgflag='A', long_name='total water conservation error', &
         ptr_col=cwbal%errh2o)

    ! Atmospheric forcing

    call hist_addfld1d (fname='RAIN', units='mm/s',  &
         avgflag='A', long_name='atmospheric rain', &
         ptr_lnd=clm_a2l%forc_rain)

    call hist_addfld1d (fname='SNOW', units='mm/s',  &
         avgflag='A', long_name='atmospheric snow', &
         ptr_lnd=clm_a2l%forc_snow)

    call hist_addfld1d (fname='TBOT', units='K',  &
         avgflag='A', long_name='atmospheric air temperature', &
         ptr_lnd=clm_a2l%forc_t)

    call hist_addfld1d (fname='THBOT', units='K',  &
         avgflag='A', long_name='atmospheric air potential temperature', &
         ptr_lnd=clm_a2l%forc_th)

    call hist_addfld1d (fname='WIND', units='m/s',  &
         avgflag='A', long_name='atmospheric wind velocity magnitude', &
         ptr_lnd=clm_a2l%forc_wind)

    ! Rename of WIND for Urban intercomparision project
    call hist_addfld1d (fname='Wind', units='m/s',  &
         avgflag='A', long_name='atmospheric wind velocity magnitude', &
         ptr_gcell=clm_a2l%forc_wind, default = 'inactive')

    call hist_addfld1d (fname='Tair', units='K',  &
         avgflag='A', long_name='atmospheric air temperature', &
         ptr_gcell=clm_a2l%forc_t, default='inactive')

    call hist_addfld1d (fname='PSurf', units='Pa',  &
         avgflag='A', long_name='surface pressure', &
         ptr_gcell=clm_a2l%forc_pbot, default='inactive')

    call hist_addfld1d (fname='Rainf', units='mm/s',  &
         avgflag='A', long_name='atmospheric rain', &
         ptr_gcell=clm_a2l%forc_rain, default='inactive')

    call hist_addfld1d (fname='SWdown', units='W/m^2',  &
         avgflag='A', long_name='atmospheric incident solar radiation', &
         ptr_gcell=clm_a2l%forc_solar, default='inactive')

    call hist_addfld1d (fname='LWdown', units='W/m^2',  &
         avgflag='A', long_name='atmospheric longwave radiation', &
         ptr_gcell=clm_a2l%forc_lwrad, default='inactive')

    call hist_addfld1d (fname='RH', units='%',  &
         avgflag='A', long_name='atmospheric relative humidity', &
         ptr_gcell=clm_a2l%forc_rh, default='inactive')

    call hist_addfld1d (fname='QBOT', units='kg/kg',  &
         avgflag='A', long_name='atmospheric specific humidity', &
         ptr_lnd=clm_a2l%forc_q)

    ! Rename of QBOT for Urban intercomparision project
    call hist_addfld1d (fname='Qair', units='kg/kg',  &
         avgflag='A', long_name='atmospheric specific humidity', &
         ptr_lnd=clm_a2l%forc_q, default='inactive')

    call hist_addfld1d (fname='ZBOT', units='m',  &
         avgflag='A', long_name='atmospheric reference height', &
         ptr_lnd=clm_a2l%forc_hgt)

    call hist_addfld1d (fname='FLDS', units='W/m^2',  &
         avgflag='A', long_name='atmospheric longwave radiation', &
         ptr_lnd=clm_a2l%forc_lwrad)

    call hist_addfld1d (fname='FSDS', units='W/m^2',  &
         avgflag='A', long_name='atmospheric incident solar radiation', &
         ptr_lnd=clm_a2l%forc_solar)

    call hist_addfld1d (fname='PCO2', units='Pa',  &
         avgflag='A', long_name='atmospheric partial pressure of CO2', &
         ptr_lnd=clm_a2l%forc_pco2)

    call hist_addfld1d (fname='PBOT', units='Pa',  &
         avgflag='A', long_name='atmospheric pressure', &
         ptr_lnd=clm_a2l%forc_pbot)




    active = "inactive"

    call hist_addfld1d (fname='T10', units='K',  &
         avgflag='A', long_name='10-day running mean of 2-m temperature', &
         ptr_pft=pes%t10, default=active)



! CH4



    call hist_addfld1d (fname='SNORDSL', units='m^-6', &
         avgflag='A', long_name='top snow layer effective grain radius', &
         ptr_col=cps%snw_rds_top, set_urb=spval, &
         default='inactive')

    call hist_addfld1d (fname='SNOTTOPL', units='K/m', &
         avgflag='A', long_name='snow temperature (top layer)', &
         ptr_col=cps%snot_top, set_urb=spval, &
         default='inactive')

    call hist_addfld1d (fname='SNOdTdzL', units='K/m', &
         avgflag='A', long_name='top snow layer temperature gradient (land)', &
         ptr_col=cps%dTdz_top, set_urb=spval, &
         default='inactive')

    call hist_addfld1d (fname='SNOLIQFL', units='fraction', &
         avgflag='A', long_name='top snow layer liquid water fraction (land)', &
         ptr_col=cps%sno_liq_top, set_urb=spval, &
         default='inactive')

    call hist_addfld1d (fname='SNOFSRVD', units='W/m^2',  &
         avgflag='A', long_name='direct vis reflected solar radiation from snow', &
         ptr_pft=pef%fsr_sno_vd, &
         default='inactive')

    call hist_addfld1d (fname='SNOFSRND', units='W/m^2',  &
         avgflag='A', long_name='direct nir reflected solar radiation from snow', &
         ptr_pft=pef%fsr_sno_nd, &
         default='inactive')
   
    call hist_addfld1d (fname='SNOFSRVI', units='W/m^2',  &
         avgflag='A', long_name='diffuse vis reflected solar radiation from snow', &
         ptr_pft=pef%fsr_sno_vi, &
         default='inactive')

    call hist_addfld1d (fname='SNOFSRNI', units='W/m^2',  &
         avgflag='A', long_name='diffuse nir reflected solar radiation from snow', &
         ptr_pft=pef%fsr_sno_ni, &
         default='inactive')

    call hist_addfld1d (fname='SNOFSDSVD', units='W/m^2',  &
         avgflag='A', long_name='direct vis incident solar radiation on snow', &
         ptr_pft=pef%fsds_sno_vd, &
         default='inactive')
   
    call hist_addfld1d (fname='SNOFSDSND', units='W/m^2',  &
         avgflag='A', long_name='direct nir incident solar radiation on snow', &
         ptr_pft=pef%fsds_sno_nd, &
         default='inactive')
   
    call hist_addfld1d (fname='SNOFSDSVI', units='W/m^2',  &
         avgflag='A', long_name='diffuse vis incident solar radiation on snow', &
         ptr_pft=pef%fsds_sno_vi, &
         default='inactive')
   
    call hist_addfld1d (fname='SNOFSDSNI', units='W/m^2',  &
         avgflag='A', long_name='diffuse nir incident solar radiation on snow', &
         ptr_pft=pef%fsds_sno_ni, &
         default='inactive')

! New lake code
    call hist_addfld1d (fname='H2OSNO_TOP', units='kg/m2', &
         avgflag='A', long_name='mass of snow in top snow layer', &
         ptr_col=cps%h2osno_top, set_urb=spval)

    call hist_addfld1d (fname='SNOBCMCL', units='kg/m2', &
         avgflag='A', long_name='mass of BC in snow column', &
         ptr_col=cps%mss_bc_col, set_urb=spval)

    call hist_addfld1d (fname='SNOBCMSL', units='kg/m2', &
         avgflag='A', long_name='mass of BC in top snow layer', &
         ptr_col=cps%mss_bc_top, set_urb=spval)

    call hist_addfld1d (fname='SNOOCMCL', units='kg/m2', &
         avgflag='A', long_name='mass of OC in snow column', &
         ptr_col=cps%mss_oc_col, set_urb=spval)
   
    call hist_addfld1d (fname='SNOOCMSL', units='kg/m2', &
         avgflag='A', long_name='mass of OC in top snow layer', &
         ptr_col=cps%mss_oc_top, set_urb=spval)

    call hist_addfld1d (fname='SNODSTMCL', units='kg/m2', &
         avgflag='A', long_name='mass of dust in snow column', &
         ptr_col=cps%mss_dst_col, set_urb=spval)
    
    call hist_addfld1d (fname='SNODSTMSL', units='kg/m2', &
         avgflag='A', long_name='mass of dust in top snow layer', &
         ptr_col=cps%mss_dst_top, set_urb=spval)

    call hist_addfld1d (fname='DSTDEP', units='kg/m^2/s', &
         avgflag='A', long_name='total dust deposition (dry+wet) from atmosphere', &
         ptr_col=cwf%flx_dst_dep, set_urb=spval)

    call hist_addfld1d (fname='BCDEP', units='kg/m^2/s', &
         avgflag='A', long_name='total BC deposition (dry+wet) from atmosphere', &
         ptr_col=cwf%flx_bc_dep, set_urb=spval)
   
    call hist_addfld1d (fname='OCDEP', units='kg/m^2/s', &
         avgflag='A', long_name='total OC deposition (dry+wet) from atmosphere', &
         ptr_col=cwf%flx_oc_dep, set_urb=spval)


    !-------------------------------
    ! Forcings sent to GLC
    !-------------------------------

    if (maxpatch_glcmec > 0) then

       call hist_addfld2d (fname='QICE_FORC', units='mm/s', type2d='glc_nec', &
            avgflag='A', long_name='qice forcing sent to GLC', &
            ptr_lnd=clm_s2x%qice, default='inactive')

       call hist_addfld2d (fname='TSRF_FORC', units='K', type2d='glc_nec', &
            avgflag='A', long_name='surface temperature sent to GLC', &
            ptr_lnd=clm_s2x%tsrf, default='inactive')

       call hist_addfld2d (fname='TOPO_FORC', units='m', type2d='glc_nec', &
            avgflag='A', long_name='topographic height sent to GLC', &
            ptr_lnd=clm_s2x%topo, default='inactive')

    end if

    ! Print masterlist of history fields

    call hist_printflds()

  end subroutine hist_initFlds


  subroutine hist_addfld_decomp (fname, type2d, units, avgflag, long_name, ptr_col, ptr_pft, default)

! !USES:
    use clm_varpar, only : nlevdecomp_full

    use histFileMod, only : hist_addfld1d, hist_addfld2d
    use abortutils, only: endrun
    use clm_varctl, only : iulog
    use surfrdMod , only : crop_prog
!
! !ARGUMENTS:
    implicit none
    character(len=*), intent(in) :: fname                    ! field name
    character(len=*), intent(in) :: type2d                   ! 2d output type
    character(len=*), intent(in) :: units                    ! units of field
    character(len=1), intent(in) :: avgflag                  ! time averaging flag
    character(len=*), intent(in) :: long_name                ! long name of field
    real(r8)        , optional, pointer    :: ptr_col(:,:)   ! pointer to column array
    real(r8)        , optional, pointer    :: ptr_pft(:,:)   ! pointer to pft array
    character(len=*), optional, intent(in) :: default        ! if set to 'inactive, field will not appear on primary tape

    real(r8)        , pointer  :: ptr_1d(:)

    if (present(ptr_col)) then

       ! column-level data
       if (present(default)) then
          if ( nlevdecomp_full .gt. 1 ) then
             call hist_addfld2d (fname=trim(fname), units=units, type2d=type2d, &
                  avgflag=avgflag, long_name=long_name, &
                  ptr_col=ptr_col, default=default)
          else
             ptr_1d => ptr_col(:,1)
             call hist_addfld1d (fname=trim(fname), units=units, &
                  avgflag=avgflag, long_name=long_name, &
                  ptr_col=ptr_1d, default=default)
          endif
       else
          if ( nlevdecomp_full .gt. 1 ) then
             call hist_addfld2d (fname=trim(fname), units=units, type2d=type2d, &
                  avgflag=avgflag, long_name=long_name, &
                  ptr_col=ptr_col)
          else
             ptr_1d => ptr_col(:,1)
             call hist_addfld1d (fname=trim(fname), units=units, &
                  avgflag=avgflag, long_name=long_name, &
                  ptr_col=ptr_1d)
          endif
       endif
       
    else if (present(ptr_pft)) then
       
       ! pft-level data
       if (present(default)) then
          if ( nlevdecomp_full .gt. 1 ) then
             call hist_addfld2d (fname=trim(fname), units=units, type2d=type2d, &
                  avgflag=avgflag, long_name=long_name, &
                  ptr_pft=ptr_pft, default=default)
          else
             ptr_1d => ptr_pft(:,1)
             call hist_addfld1d (fname=trim(fname), units=units, &
                  avgflag=avgflag, long_name=long_name, &
                  ptr_pft=ptr_1d, default=default)
          endif
       else
          if ( nlevdecomp_full .gt. 1 ) then
             call hist_addfld2d (fname=trim(fname), units=units, type2d=type2d, &
                  avgflag=avgflag, long_name=long_name, &
                  ptr_pft=ptr_pft)
          else
             ptr_1d => ptr_pft(:,1)
             call hist_addfld1d (fname=trim(fname), units=units, &
                  avgflag=avgflag, long_name=long_name, &
                  ptr_pft=ptr_1d)
          endif
       endif
       
    else
       write(iulog, *) ' error: hist_addfld_decomp needs either pft or column level pointer'
       write(iulog, *) fname
       call endrun()
    endif
  end subroutine hist_addfld_decomp
  

end module histFldsMod
