module cam_cpl_indices
  
  use seq_flds_mod
  use mct_mod
  use seq_drydep_mod, only: drydep_fields_token, lnd_drydep
  use shr_megan_mod,  only: shr_megan_fields_token, shr_megan_mechcomps_n

  implicit none

  SAVE
  public                               ! By default make data private

  integer :: index_a2x_Sa_z            ! bottom atm level height
  integer :: index_a2x_Sa_u            ! bottom atm level zon wind
  integer :: index_a2x_Sa_v            ! bottom atm level mer wind
  integer :: index_a2x_Sa_tbot         ! bottom atm level temp
  integer :: index_a2x_Sa_ptem         ! bottom atm level pot temp
  integer :: index_a2x_Sa_shum         ! bottom atm level spec hum
  integer :: index_a2x_Sa_dens         ! bottom atm level air den
  integer :: index_a2x_Sa_pbot         ! bottom atm level pressure
  integer :: index_a2x_Sa_pslv         ! sea level atm pressure
  integer :: index_a2x_Faxa_lwdn       ! downward lw heat flux
  integer :: index_a2x_Faxa_rainc      ! prec: liquid "convective"
  integer :: index_a2x_Faxa_rainl      ! prec: liquid "large scale"
  integer :: index_a2x_Faxa_snowc      ! prec: frozen "convective"
  integer :: index_a2x_Faxa_snowl      ! prec: frozen "large scale"
  integer :: index_a2x_Faxa_swndr      ! sw: nir direct  downward
  integer :: index_a2x_Faxa_swvdr      ! sw: vis direct  downward
  integer :: index_a2x_Faxa_swndf      ! sw: nir diffuse downward
  integer :: index_a2x_Faxa_swvdf      ! sw: vis diffuse downward
  integer :: index_a2x_Faxa_swnet      ! sw: net
  integer :: index_a2x_Faxa_bcphidry   ! flux: Black Carbon hydrophilic dry deposition
  integer :: index_a2x_Faxa_bcphodry   ! flux: Black Carbon hydrophobic dry deposition
  integer :: index_a2x_Faxa_bcphiwet   ! flux: Black Carbon hydrophilic wet deposition
  integer :: index_a2x_Faxa_ocphidry   ! flux: Organic Carbon hydrophilic dry deposition
  integer :: index_a2x_Faxa_ocphodry   ! flux: Organic Carbon hydrophobic dry deposition
  integer :: index_a2x_Faxa_ocphiwet   ! flux: Organic Carbon hydrophilic dry deposition
  integer :: index_a2x_Faxa_dstwet1    ! flux: Size 1 dust -- wet deposition
  integer :: index_a2x_Faxa_dstwet2    ! flux: Size 2 dust -- wet deposition
  integer :: index_a2x_Faxa_dstwet3    ! flux: Size 3 dust -- wet deposition
  integer :: index_a2x_Faxa_dstwet4    ! flux: Size 4 dust -- wet deposition
  integer :: index_a2x_Faxa_dstdry1    ! flux: Size 1 dust -- dry deposition
  integer :: index_a2x_Faxa_dstdry2    ! flux: Size 2 dust -- dry deposition
  integer :: index_a2x_Faxa_dstdry3    ! flux: Size 3 dust -- dry deposition
  integer :: index_a2x_Faxa_dstdry4    ! flux: Size 4 dust -- dry deposition
  integer :: index_a2x_Sa_co2prog      ! bottom atm level prognostic co2
  integer :: index_a2x_Sa_co2diag      ! bottom atm level diagnostic co2

  integer :: index_x2a_Sx_t            ! surface temperature             
  integer :: index_x2a_So_t            ! sea surface temperature         
  integer :: index_x2a_Sf_lfrac        ! surface land fraction           
  integer :: index_x2a_Sf_ifrac        ! surface ice fraction            
  integer :: index_x2a_Sf_ofrac        ! surface ocn fraction            
  integer :: index_x2a_Sx_tref         ! 2m reference temperature        
  integer :: index_x2a_Sx_qref         ! 2m reference specific humidity  
  integer :: index_x2a_Sx_avsdr        ! albedo, visible, direct         
  integer :: index_x2a_Sx_anidr        ! albedo, near-ir, direct         
  integer :: index_x2a_Sx_avsdf        ! albedo, visible, diffuse        
  integer :: index_x2a_Sx_anidf        ! albedo, near-ir, diffuse        
  integer :: index_x2a_Sl_snowh        ! surface snow depth over land
  integer :: index_x2a_Si_snowh        ! surface snow depth over ice
  integer :: index_x2a_Sl_fv           ! friction velocity
  integer :: index_x2a_Sl_ram1         ! aerodynamical resistance
  integer :: index_x2a_Sl_soilw        ! volumetric soil water
  integer :: index_x2a_Faxx_taux       ! wind stress, zonal              
  integer :: index_x2a_Faxx_tauy       ! wind stress, meridional         
  integer :: index_x2a_Faxx_lat        ! latent          heat flux       
  integer :: index_x2a_Faxx_sen        ! sensible        heat flux       
  integer :: index_x2a_Faxx_lwup       ! upward longwave heat flux       
  integer :: index_x2a_Faxx_evap       ! evaporation    water flux       
  integer :: index_x2a_Fall_flxdst1    ! dust flux size bin 1    
  integer :: index_x2a_Fall_flxdst2    ! dust flux size bin 2    
  integer :: index_x2a_Fall_flxdst3    ! dust flux size bin 3    
  integer :: index_x2a_Fall_flxdst4    ! dust flux size bin 4
  integer :: index_x2a_Fall_flxvoc     ! MEGAN emissions fluxes   
  integer :: index_x2a_Fall_fco2_lnd   ! co2 flux from land   
  integer :: index_x2a_Faoo_fco2_ocn   ! co2 flux from ocean  
  integer :: index_x2a_Faoo_fdms_ocn   ! dms flux from ocean
  integer :: index_x2a_So_ustar	       ! surface friction velocity in ocean
  integer :: index_x2a_So_re           ! square of atm/ocn exch. coeff 
  integer :: index_x2a_So_ssq          ! surface saturation specific humidity in ocean 
  integer :: index_x2a_Sl_ddvel        ! dry deposition velocities from land
  integer :: index_x2a_Sx_u10          ! 10m wind

contains

  subroutine cam_cpl_indices_set( )

    type(attrVect) :: a2x      ! temporary
    type(attrVect) :: x2a      ! temporary

    ! Determine attribute vector indices

    ! create temporary attribute vectors
    call attrVect_init(x2a, rList=seq_flds_x2a_fields, lsize=1)
    call attrVect_init(a2x, rList=seq_flds_a2x_fields, lsize=1)

    ! Initialize av indices
    index_x2a_Sx_avsdr      = attrVect_indexra(x2a,'Sx_avsdr')
    index_x2a_Sx_anidr      = attrVect_indexra(x2a,'Sx_anidr')
    index_x2a_Sx_avsdf      = attrVect_indexra(x2a,'Sx_avsdf')
    index_x2a_Sx_anidf      = attrVect_indexra(x2a,'Sx_anidf')
    index_x2a_Sx_t          = attrVect_indexra(x2a,'Sx_t')
    index_x2a_So_t          = attrVect_indexra(x2a,'So_t')
    index_x2a_Sl_snowh      = attrVect_indexra(x2a,'Sl_snowh')
    index_x2a_Si_snowh      = attrVect_indexra(x2a,'Si_snowh')
    
    index_x2a_Sl_fv         = attrVect_indexra(x2a,'Sl_fv')
    index_x2a_Sl_ram1       = attrVect_indexra(x2a,'Sl_ram1')
    index_x2a_Sl_soilw      = attrVect_indexra(x2a,'Sl_soilw',perrWith='quiet')
    
    index_x2a_Sx_tref       = attrVect_indexra(x2a,'Sx_tref')
    index_x2a_Sx_qref       = attrVect_indexra(x2a,'Sx_qref')

    index_x2a_Sf_ifrac      = attrVect_indexra(x2a,'Sf_ifrac')
    index_x2a_Sf_ofrac      = attrVect_indexra(x2a,'Sf_ofrac')
    index_x2a_Sf_lfrac      = attrVect_indexra(x2a,'Sf_lfrac')

    index_x2a_Sx_u10        = attrVect_indexra(x2a,'Sx_u10')
    index_x2a_Faxx_taux     = attrVect_indexra(x2a,'Faxx_taux')
    index_x2a_Faxx_tauy     = attrVect_indexra(x2a,'Faxx_tauy')
    index_x2a_Faxx_lat      = attrVect_indexra(x2a,'Faxx_lat')
    index_x2a_Faxx_sen      = attrVect_indexra(x2a,'Faxx_sen')
    index_x2a_Faxx_lwup     = attrVect_indexra(x2a,'Faxx_lwup')
    index_x2a_Faxx_evap     = attrVect_indexra(x2a,'Faxx_evap')
    index_x2a_So_ustar      = attrVect_indexra(x2a,'So_ustar')
    index_x2a_So_re         = attrVect_indexra(x2a,'So_re')
    index_x2a_So_ssq        = attrVect_indexra(x2a,'So_ssq')
    index_x2a_Sl_fv         = attrVect_indexra(x2a,'Sl_fv')
    index_x2a_Sl_ram1       = attrVect_indexra(x2a,'Sl_ram1')
    index_x2a_Fall_flxdst1  = attrVect_indexra(x2a,'Fall_flxdst1')
    index_x2a_Fall_flxdst2  = attrVect_indexra(x2a,'Fall_flxdst2')
    index_x2a_Fall_flxdst3  = attrVect_indexra(x2a,'Fall_flxdst3')
    index_x2a_Fall_flxdst4  = attrVect_indexra(x2a,'Fall_flxdst4')
    index_x2a_Fall_fco2_lnd = attrVect_indexra(x2a,'Fall_fco2_lnd',perrWith='quiet')
    index_x2a_Faoo_fco2_ocn = attrVect_indexra(x2a,'Faoo_fco2_ocn',perrWith='quiet')
    index_x2a_Faoo_fdms_ocn = attrVect_indexra(x2a,'Faoo_fdms_ocn',perrWith='quiet')

    if (shr_megan_mechcomps_n>0) then
       index_x2a_Fall_flxvoc = attrVect_indexra(x2a,trim(shr_megan_fields_token))
    else
       index_x2a_Fall_flxvoc = 0
    endif

    if ( lnd_drydep )then
       index_x2a_Sl_ddvel   = attrVect_indexra(x2a, trim(drydep_fields_token))
    else
       index_x2a_Sl_ddvel   = 0
    end if

    index_a2x_Sa_z          = attrVect_indexra(a2x,'Sa_z')
    index_a2x_Sa_u          = attrVect_indexra(a2x,'Sa_u')
    index_a2x_Sa_v          = attrVect_indexra(a2x,'Sa_v')
    index_a2x_Sa_tbot       = attrVect_indexra(a2x,'Sa_tbot')
    index_a2x_Sa_ptem       = attrVect_indexra(a2x,'Sa_ptem')
    index_a2x_Sa_pbot       = attrVect_indexra(a2x,'Sa_pbot')
    index_a2x_Sa_pslv       = attrVect_indexra(a2x,'Sa_pslv')
    index_a2x_Sa_shum       = attrVect_indexra(a2x,'Sa_shum')
    index_a2x_Sa_dens       = attrVect_indexra(a2x,'Sa_dens')
    index_a2x_Faxa_swnet    = attrVect_indexra(a2x,'Faxa_swnet')
    index_a2x_Faxa_lwdn     = attrVect_indexra(a2x,'Faxa_lwdn')
    index_a2x_Faxa_rainc    = attrVect_indexra(a2x,'Faxa_rainc')
    index_a2x_Faxa_rainl    = attrVect_indexra(a2x,'Faxa_rainl')
    index_a2x_Faxa_snowc    = attrVect_indexra(a2x,'Faxa_snowc')
    index_a2x_Faxa_snowl    = attrVect_indexra(a2x,'Faxa_snowl')
    index_a2x_Faxa_swndr    = attrVect_indexra(a2x,'Faxa_swndr')
    index_a2x_Faxa_swvdr    = attrVect_indexra(a2x,'Faxa_swvdr')
    index_a2x_Faxa_swndf    = attrVect_indexra(a2x,'Faxa_swndf')
    index_a2x_Faxa_swvdf    = attrVect_indexra(a2x,'Faxa_swvdf')
    index_a2x_Faxa_bcphidry = attrVect_indexra(a2x,'Faxa_bcphidry')
    index_a2x_Faxa_bcphodry = attrVect_indexra(a2x,'Faxa_bcphodry')
    index_a2x_Faxa_bcphiwet = attrVect_indexra(a2x,'Faxa_bcphiwet')
    index_a2x_Faxa_ocphidry = attrVect_indexra(a2x,'Faxa_ocphidry')
    index_a2x_Faxa_ocphodry = attrVect_indexra(a2x,'Faxa_ocphodry')
    index_a2x_Faxa_ocphiwet = attrVect_indexra(a2x,'Faxa_ocphiwet')
    index_a2x_Faxa_dstdry1  = attrVect_indexra(a2x,'Faxa_dstdry1')
    index_a2x_Faxa_dstdry2  = attrVect_indexra(a2x,'Faxa_dstdry2')
    index_a2x_Faxa_dstdry3  = attrVect_indexra(a2x,'Faxa_dstdry3')
    index_a2x_Faxa_dstdry4  = attrVect_indexra(a2x,'Faxa_dstdry4')
    index_a2x_Faxa_dstwet1  = attrVect_indexra(a2x,'Faxa_dstwet1')
    index_a2x_Faxa_dstwet2  = attrVect_indexra(a2x,'Faxa_dstwet2')
    index_a2x_Faxa_dstwet3  = attrVect_indexra(a2x,'Faxa_dstwet3')
    index_a2x_Faxa_dstwet4  = attrVect_indexra(a2x,'Faxa_dstwet4')
    index_a2x_Sa_co2prog    = attrVect_indexra(a2x,'Sa_co2prog',perrWith='quiet')
    index_a2x_Sa_co2diag    = attrVect_indexra(a2x,'Sa_co2diag',perrWith='quiet')

    call attrVect_clean(x2a)
    call attrVect_clean(a2x)

  end subroutine cam_cpl_indices_set

end module cam_cpl_indices
