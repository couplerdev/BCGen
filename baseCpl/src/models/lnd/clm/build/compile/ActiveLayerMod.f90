

module ActiveLayerMod
  
  
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: ActiveLayerMod
!
! !DESCRIPTION:
! Module holding routines for calculation of active layer dynamics
!
  ! !USES:
  use shr_kind_mod , only: r8 => shr_kind_r8
  use shr_const_mod, only: SHR_CONST_TKFRZ
  use clm_varctl  , only: iulog
  
  implicit none
  save
  private
  ! !PUBLIC MEMBER FUNCTIONS:
  public:: alt_calc
  
  !
  ! !REVISION HISTORY:
  ! 6/27/2011 Created by C. Koven
  !
  !EOP
  !-----------------------------------------------------------------------
  
contains


  subroutine alt_calc(lbc, ubc, num_soilc, filter_soilc)
!
! !DESCRIPTION:
!
!  define active layer thickness similarly to frost_table, except set as deepest thawed layer and define on nlevgrnd
!  also update annual maxima, and keep track of prior year for rooting memory
!
! !USES:
    use clmtype
    use clm_varpar      , only : nlevgrnd
    use clm_time_manager    , only : get_curr_date, get_step_size
    use shr_const_mod, only: SHR_CONST_TKFRZ
    use clm_varctl  , only: iulog
    use clm_varcon, only: zsoi
    
    
!
! !ARGUMENTS:
    implicit none
    integer, intent(in) :: lbc, ubc        ! column bounds
    integer, intent(in) :: num_soilc       ! number of soil columns in filter
    integer, intent(in) :: filter_soilc(:) ! filter for soil columns
!
! !CALLED FROM:
! 
!
! !REVISION HISTORY:
!
! 6/27/2011 Created by C. Koven
!
! !LOCAL VARIABLES:
! local pointers to implicit in scalars
!
! column level
    real(r8), pointer :: t_soisno(:,:)          ! soil temperature (Kelvin)  (-nlevsno+1:nlevgrnd)
    real(r8), pointer :: alt(:)                 ! current depth of thaw
    real(r8), pointer :: altmax(:)              ! maximum annual depth of thaw
    real(r8), pointer :: altmax_lastyear(:)     ! prior year maximum annual depth of thaw
    integer, pointer :: alt_indx(:)             ! current depth of thaw
    integer, pointer :: altmax_indx(:)          ! maximum annual depth of thaw
    integer, pointer :: altmax_lastyear_indx(:) ! prior year maximum annual depth of thaw
    real(r8), pointer :: lat(:)                 ! gridcell latitude (radians)
    integer , pointer :: cgridcell(:)           ! gridcell index of column
    
    ! local variables
    integer  :: c, j, fc, g                     ! counters
    integer  :: alt_ind                         ! index of base of activel layer
    integer  :: year                            ! year (0, ...) for nstep+1
    integer  :: mon                             ! month (1, ..., 12) for nstep+1
    integer  :: day                             ! day of month (1, ..., 31) for nstep+1
    integer  :: sec                             ! seconds into current date for nstep+1
    integer  :: dtime                           ! time step length in seconds
    integer  :: k_frz                           ! index of first nonfrozen soil layer
    logical  :: found_thawlayer                 ! used to break loop when first unfrozen layer reached
    real(r8) :: t1, t2, z1, z2                  ! temporary variables
    
    ! Assign local pointers to derived type arrays
    t_soisno                               => ces%t_soisno
    alt                                    => cps%alt
    altmax                                 => cps%altmax
    altmax_lastyear                        => cps%altmax_lastyear
    alt_indx                               => cps%alt_indx
    altmax_indx                            => cps%altmax_indx
    altmax_lastyear_indx                   => cps%altmax_lastyear_indx
    
    ! Assign local pointers to derived subtypes components (gridcell-level and mapping)
    lat             =>  grc%lat
    cgridcell       =>col%gridcell
    
    
    ! on a set annual timestep, update annual maxima
    ! make this 1 January for NH columns, 1 July for SH columns
    call get_curr_date(year, mon, day, sec)
    dtime =  get_step_size()
    if ( (mon .eq. 1) .and. (day .eq. 1) .and. ( sec / dtime .eq. 1) ) then
       do fc = 1,num_soilc
          c = filter_soilc(fc)
          g = cgridcell(c)
          if ( lat(g) .gt. 0. ) then 
             altmax_lastyear(c) = altmax(c)
             altmax_lastyear_indx(c) = altmax_indx(c)
             altmax(c) = 0.
             altmax_indx(c) = 0
          endif
       end do
    endif
    if ( (mon .eq. 7) .and. (day .eq. 1) .and. ( sec / dtime .eq. 1) ) then
       do fc = 1,num_soilc
          c = filter_soilc(fc)
          g = cgridcell(c)
          if ( lat(g) .le. 0. ) then 
             altmax_lastyear(c) = altmax(c)
             altmax_lastyear_indx(c) = altmax_indx(c)
             altmax(c) = 0.
             altmax_indx(c) = 0
          endif
       end do
    endif
    
    do fc = 1,num_soilc
       c = filter_soilc(fc)
       
       ! calculate alt for a given timestep
       ! start from base of soil and search upwards for first thawed layer.
       ! note that this will put talik in with active layer
       ! a different way of doing this could be to keep track of how long a given layer has ben frozen for, and define ALT as the first layer that has been frozen for less than 2 years.
       if (t_soisno(c,nlevgrnd) > SHR_CONST_TKFRZ ) then
          alt(c) = zsoi(nlevgrnd)
          alt_indx(c) = nlevgrnd
       else
          k_frz=0
          found_thawlayer = .false.
          do j=nlevgrnd-1,1,-1
             if ( ( t_soisno(c,j) > SHR_CONST_TKFRZ ) .and. .not. found_thawlayer ) then
                k_frz=j
                found_thawlayer = .true.
             endif
          end do
          
          if ( k_frz .gt. 0 ) then
             ! define active layer as the depth at which the linearly interpolated temperature line intersects with zero
             z1 = zsoi(k_frz)
             z2 = zsoi(k_frz+1)
             t1 = t_soisno(c,k_frz)
             t2 = t_soisno(c,k_frz+1)
             alt(c) = z1 + (t1-SHR_CONST_TKFRZ)*(z2-z1)/(t1-t2)
             alt_indx(c) = k_frz
          else
             alt(c)=0._r8
             alt_indx(c) = 0
          endif
       endif
       
       
       ! if appropriate, update maximum annual active layer thickness
       if (alt(c) .gt. altmax(c)) then
          altmax(c) = alt(c)
          altmax_indx(c) = alt_indx(c)
       endif
       
    end do
    
  end subroutine alt_calc
  
  
end module ActiveLayerMod
