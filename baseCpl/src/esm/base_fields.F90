module base_fields


    implicit none
    integer, parameter :: CXX = 4096

    !-----------------------------------------------
    ! domain flds
    !-----------------------------------------------

    character(CXX)   :: flds_dom_coord
    character(CXX)   :: flds_dom_other

    !------------------------------------------------
    ! state + flux fields
    !------------------------------------------------
     
    character(CXX)   :: flds_atm2x_states
    character(CXX)   :: flds_atm2x_fluxes
    character(CXX)   :: flds_x2atm_states
    character(CXX)   :: flds_x2atm_states
    
    character(CXX)   :: flds_ice2x_states
    character(CXX)   :: flds_ice2x_fluxes
    character(CXX)   :: flds_x2ice_states
    character(CXX)   :: flds_x2ice_fluxes
    
    character(CXX)   :: flds_lnd2x_states
    character(CXX)   :: flds_lnd2x_fluxes
    character(CXX)   :: flds_x2lnd_states
    character(CXX)   :: flds_x2lnd_fluxes
    
    character(CXX)   :: flds_ocn2x_states
    character(CXX)   :: flds_ocn2x_fluxes
    character(CXX)   :: flds_x2ocn_states
    character(CXX)   :: flds_ocn2x_fluxes

    character(CXX)   :: flds_glc2x_states
    character(CXX)   :: flds_glc2x_fluxes
    character(CXX)   :: flds_x2glc_states
    character(CXX)   :: flds_x2glc_fluxes
    
    character(CXX)   :: flds_snow2x_states
    character(CXX)   :: flds_snow2x_fluxes
    character(CXX)   :: flds_x2snow_states
    character(CXX)   :: flds_x2snow_fluxes
   
    character(CXX)   :: flds_wav2x_states
    character(CXX)   :: flds_wav2x_fluxes
    character(CXX)   :: flds_x2wav_states
    character(CXX)   :: flds_x2wav_fluxes

    character(CXX)   :: flds_rof2x_states
    character(CXX)   :: flds_rof2x_fluxes
    character(CXX)   :: flds_x2rof_states
    character(CXX)   :: flds_x2rof_fluxes

    !----------------------------------------------
    ! combined state/flux fields
    !----------------------------------------------
    character(CXX)   :: flds_dom_fields
    character(CXX)   :: flds_atm2x_fields
    character(CXX)   :: flds_x2atm_fields
    character(CXX)   :: flds_ice2x_fields
    character(CXX)   :: flds_x2ice_fields
    character(CXX)   :: flds_lnd2x_fields
    character(CXX)   :: flds_x2lnd_fields
    character(CXX)   :: flds_ocn2x_fields
    character(CXX)   :: flds_x2ocn_fields
    character(CXX)   :: flds_rof2x_fields
    character(CXX)   :: flds_glc2x_fields
    character(CXX)   :: flds_x2glc_fields
    character(CXX)   :: flds_snow2x_fields
    character(CXX)   :: flds_x2snow_fields
    character(CXX)   :: flds_wav2x_fields
    character(CXX)   :: flds_x2wav_fields

    


end moudle base_fields
