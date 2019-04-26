

module comp_rof
  
!========================================================================
! DESCRIPTION:
! Interface of the active runoff component of CESM 
! with the main CESM driver. This is a thin interface taking CESM driver information
! in MCT (Model Coupling Toolkit) format and converting it to use by RTM

  use shr_kind_mod     , only : r8 => shr_kind_r8
  use shr_file_mod     , only : shr_file_setLogUnit, shr_file_setLogLevel, &
                                shr_file_getLogUnit, shr_file_getLogLevel, &
                                shr_file_getUnit, shr_file_setIO
  use shr_const_mod    , only : SHR_CONST_REARTH
  use RunoffMod        , only : runoff
  use RtmVar           , only : rtmlon, rtmlat, ice_runoff, iulog, &
                                nsrStartup, nsrContinue, nsrBranch, & 
                                inst_index, inst_suffix, inst_name, RtmVarSet, &
                                rtm_active, flood_active
  use RtmSpmd          , only : masterproc, mpicom_rof, iam, RtmSpmdInit
  use RtmMod           , only : Rtmini, Rtmrun
  use RtmTimeManager   , only : timemgr_setup, get_curr_date, get_step_size, advance_timestep 
  use perf_mod         , only : t_startf, t_stopf, t_barrierf
  use rtm_cpl_indices  , only : rtm_cpl_indices_set, nt_rtm, rtm_tracers, &
                                index_x2r_Flrl_rofliq, index_x2r_Flrl_rofice, &
                                index_r2x_Forr_roff,   index_r2x_Forr_ioff, &
                                index_r2x_Flrr_flood, index_r2x_Slrr_volr
  use mct_mod
  use ESMF
use mct_mod
use time_mod, only: time_ClockGetInfo, time_alarmIsOn, time_clockDateInSync
use time_type
use proc_def
use global_var
!
! PUBLIC MEMBER FUNCTIONS:
  implicit none
  SAVE
  private                              ! By default make data private
!
! PUBLIC MEMBER FUNCTIONS:
  public :: rof_init_mct               ! rof initialization
  public :: rof_run_mct                ! rof run phase
  public :: rof_final_mct              ! rof finalization/cleanup
!
! PUBLIC DATA MEMBERS:
! None
!
! PRIVATE MEMBER FUNCTIONS:
  private :: rof_SetgsMap_mct         ! Set the river runoff model MCT GS map
  private :: rof_domain_mct           ! Set the river runoff model domain information
  private :: rof_export_mct           ! Export the river runoff model data to the CESM coupler
!
! PRIVATE DATA MEMBERS:
  real(r8), pointer :: totrunin(:,:)   ! cell tracer lnd forcing on rtm grid (mm/s)

! REVISION HISTORY:
! Author: Mariana Vertenstein
!===============================================================
contains
!===============================================================

!========================================================================

  subroutine rof_init_mct( compInfo, EClock, x2rof, rof2x, ierr )

    !---------------------------------------------------------------------------
    ! DESCRIPTION:
    ! Initialize runoff model and obtain relevant atmospheric model arrays
    ! back from (i.e. albedos, surface temperature and snow cover over land).
    !
    ! !ARGUMENTS:
    type(compMeta), target, intent(inout) :: compInfo
    type(ESMF_Clock),           intent(in)    :: EClock     ! Input synchronization clock
    type(mct_aVect) ,           intent(inout) :: x2rof      ! River import state
    type(mct_aVect),            intent(inout) :: rof2x      ! River export state
    integer                  , intent(inout) :: ierr
    !
    ! !LOCAL VARIABLES:
    integer :: ROFID	                             ! rof identifyer
    integer :: mpicom_rof                            ! mpi communicator
    type(mct_gsMap),         pointer :: gsMap_rof    ! runoff model MCT GS map
    type(mct_gGrid),         pointer :: dom_r        ! runoff model domain
    integer :: lsize                                 ! size of attribute vector
    integer :: g,i,j,n                               ! indices
    logical :: exists                                ! true if file exists
    integer :: nsrest                                ! restart type
    integer :: ref_ymd                               ! reference date (YYYYMMDD)
    integer :: ref_tod                               ! reference time of day (sec)
    integer :: start_ymd                             ! start date (YYYYMMDD)
    integer :: start_tod                             ! start time of day (sec)
    integer :: stop_ymd                              ! stop date (YYYYMMDD)
    integer :: stop_tod                              ! stop time of day (sec)
    logical :: brnch_retain_casename                 ! flag if should retain the case name on a branch start type
    integer :: lbnum                                 ! input to memory diagnostic
    integer :: shrlogunit,shrloglev                  ! old values for log unit and log level
    integer :: begr, endr
    character(len=SHR_KIND_CL) :: caseid             ! case identifier name
    character(len=SHR_KIND_CL) :: ctitle             ! case description title
    character(len=SHR_KIND_CL) :: starttype          ! start-type (startup, continue, branch, hybrid)
    character(len=SHR_KIND_CL) :: calendar           ! calendar type name
    character(len=SHR_KIND_CL) :: hostname           ! hostname of machine running on
    character(len=SHR_KIND_CL) :: version            ! Model version
    character(len=SHR_KIND_CL) :: username           ! user running the model
    character(len=32), parameter :: sub = 'rof_init_mct'
    character(len=*),  parameter :: format = "('("//trim(sub)//") :',A)"
    !---------------------------------------------------------------------------

    ! Obtain cdata_r (initalized in ccsm_comp_mod.F90 in the call to 
    ! seq_cdata_init for cdata_rr)
    ROFID = compInfo%ID
    dom_r => compInfo%domain
    mpicom_rof = compInfo%comm
    gsmap_rof => compInfo%comp_gsmap

    ! Determine attriute vector indices
    call rtm_cpl_indices_set()

    ! Initialize rtm MPI communicator 
    call RtmSpmdInit(mpicom_rof)








    ! Initialize io log unit
    inst_name   = 'ROF'
    inst_index  = 5
    inst_suffix = '_HQ_5'

    call shr_file_getLogUnit (shrlogunit)
    if (masterproc) then
       inquire(file='rof_modelio.nml'//trim(inst_suffix),exist=exists)
       if (exists) then
          iulog = shr_file_getUnit()
          call shr_file_setIO('rof_modelio.nml'//trim(inst_suffix),iulog)
       end if
       write(iulog,format) "RTM land model initialization"
    else
       iulog = shrlogunit
    end if
    
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogUnit (iulog)
    
    ! Initialize rtm
    call time_clockGetInfo(EClock,                               &
                                   start_ymd=start_ymd,                  &
                                   start_tod=start_tod, ref_ymd=ref_ymd, &
                                   ref_tod=ref_tod, stop_ymd=stop_ymd,   &
                                   stop_tod=stop_tod,                    &
                                   calendar=calendar )

    call timemgr_setup(calendar_in=calendar,                           &
                       start_ymd_in=start_ymd, start_tod_in=start_tod, &
                       ref_ymd_in=ref_ymd, ref_tod_in=ref_tod,         &
                       stop_ymd_in=stop_ymd, stop_tod_in=stop_tod)  

    nsrest = nsrStartup

    call RtmVarSet(caseid_in=caseid, ctitle_in=ctitle,             &
                   brnch_retain_casename_in=brnch_retain_casename, &
                   nsrest_in=nsrest, version_in=version,           &
                   hostname_in=hostname, username_in=username)

    ! Read namelist, grid and surface data
    call Rtmini()

    if (rtm_active) then
       ! Initialize memory for input state
       begr = runoff%begr
       endr = runoff%endr
       allocate (totrunin(begr:endr,nt_rtm))
       
       ! Initialize rof gsMap for ocean rof and land rof
       call rof_SetgsMap_mct( mpicom_rof, ROFID, gsMap_rof)
       
       ! Initialize rof domain
       lsize = mct_gsMap_lsize(gsMap_rof, mpicom_rof)
       call rof_domain_mct( lsize, gsMap_rof, dom_r )
       
       ! Initialize lnd -> rtm attribute vector		
       call mct_aVect_init(x2rof, rList=trim(metaData%flds_x2rof), lsize=lsize)
       call mct_aVect_zero(x2rof)
       
       ! Initialize rtm -> ocn attribute vector		
       call mct_aVect_init(rof2x, rList=trim(metaData%flds_rof2x), lsize=lsize)
       call mct_aVect_zero(rof2x) 
       
       ! Create mct river runoff export state
       call rof_export_mct( rof2x )
    end if

    ! Fill in infodata

    ! Reset shr logging to original values
    call shr_file_setLogUnit (shrlogunit)
    call shr_file_setLogLevel(shrloglev)

    if(masterproc) then
       print *, 'DEBUG_HQ_ROF_INIT_FIN0'
       write(iulog,*) TRIM(Sub) // ':end::'
       print *, 'DEBUG_HQ_ROF_INIT_FIN1'
    endif

  end subroutine rof_init_mct

!---------------------------------------------------------------------------

  subroutine rof_run_mct(compInfo, EClock, x2rof, rof2x, ierr)

    !-------------------------------------------------------
    ! DESCRIPTION:
    ! Run runoff model

    ! ARGUMENTS:
    implicit none
    type(compMeta), target, intent(inout) :: compInfo
    type(ESMF_Clock) , intent(in)    :: EClock    ! Input synchronization clock from driver
    type(mct_aVect)  , intent(inout) :: x2rof     ! Import state from runoff model
    type(mct_aVect)  , intent(inout) :: rof2x     ! Export state from runoff model
    integer, intent(inout) :: ierr

    ! LOCAL VARIABLES:
    integer :: ymd_sync, ymd              ! current date (YYYYMMDD)
    integer :: yr_sync, yr                ! current year
    integer :: mon_sync, mon              ! current month
    integer :: day_sync, day              ! current day
    integer :: tod_sync, tod              ! current time of day (sec)
    logical :: rstwr                      ! .true. ==> write restart file before returning
    logical :: nlend                      ! .true. ==> signaling last time-step
    integer :: shrlogunit,shrloglev       ! old values for share log unit and log level
    integer :: lsize                      ! local size
    integer :: lbnum                      ! input to memory diagnostic
    integer :: g,i                        ! indices
    type(mct_gGrid),        pointer :: dom_r    ! runoff model domain
    real(r8),               pointer :: data(:)  ! temporary
    character(len=32)               :: rdate    ! date char string for restart file names
    character(len=32), parameter    :: sub = "rof_run_mct"
    !-------------------------------------------------------

    if(masterproc) then
       print *, 'DEBUG_HQ_ROF_RUN_START0'
    endif







    if (.not.rtm_active) return

    ! Reset shr logging to my log file
    call shr_file_getLogUnit (shrlogunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogUnit (iulog)

    ! Determine time of next atmospheric shortwave calculation
    call time_clockGetInfo(EClock, &
         curr_ymd=ymd, curr_tod=tod_sync,  &
         curr_yr=yr_sync, curr_mon=mon_sync, curr_day=day_sync)

    ! Map MCT to land data type (output is totrunin - module variable) 
    call t_startf ('lc_rof_import')
    call rof_import_mct( x2rof, totrunin=totrunin )
    call t_stopf ('lc_rof_import')

    ! Run rtm (input is totrunin, output is runoff%runoff)
    ! First advance rtm time step
    write(rdate,'(i4.4,"-",i2.2,"-",i2.2,"-",i5.5)') yr_sync,mon_sync,day_sync,tod_sync
    nlend = time_alarmIsOn( EClock , trim(alarm_stop_name))
    rstwr = time_alarmIsOn( EClock , trim(alarm_restart_name))
    call advance_timestep()
    call Rtmrun(totrunin, rstwr, nlend, rdate)

    ! Map roff data to MCT datatype (input is runoff%runoff, output is r2x_r)
    call t_startf ('lc_rof_export')
    call rof_export_mct( rof2x )
    call t_stopf ('lc_rof_export')

    ! Check that internal clock is in sync with master clock
    call get_curr_date( yr, mon, day, tod )
    ymd = yr*10000 + mon*100 + day
    tod = tod
    if ( .not. time_clockDateInSync( EClock, ymd, tod ) )then
       call time_ClockGetInfo( EClock, curr_ymd=ymd_sync, curr_tod=tod_sync )
       write(iulog,*)' rtm ymd=',ymd     ,'  rtm tod= ',tod
       write(iulog,*)'sync ymd=',ymd_sync,' sync tod= ',tod_sync
       call shr_sys_abort( sub//":: RTM clock is not in sync with Master Sync clock" )
    end if
    
    ! Reset shr logging to my original values
    call shr_file_setLogUnit (shrlogunit)
    call shr_file_setLogLevel(shrloglev)
  








  end subroutine rof_run_mct

!===============================================================================

  subroutine rof_final_mct

    !-----------------------------------------------------
    ! DESCRIPTION:
    ! Finalize rof surface model
    !
    ! ARGUMENTS:
    implicit none
    !-----------------------------------------------------

   ! fill this in
  end subroutine rof_final_mct

!===============================================================================

  subroutine rof_SetgsMap_mct( mpicom_r, ROFID, gsMap_rof)

    !-----------------------------------------------------
    ! DESCRIPTION:
    ! Set the MCT GS map for the runoff model
    !
    ! ARGUMENTS:
    implicit none
    integer        , intent(in)    :: mpicom_r      ! MPI communicator for rof model
    integer        , intent(in)    :: ROFID         ! Land model identifier
    type(mct_gsMap), intent(inout) :: gsMap_rof     ! MCT gsmap for runoff -> land data
    !
    ! LOCAL VARIABLES
    integer,allocatable :: gindex(:)         ! indexing for runoff grid cells
    integer :: n, ni                         ! indices
    integer :: lsize,gsize                   ! size of runoff data and number of grid cells
    integer :: begr, endr                    ! beg, end runoff indices
    integer :: ier                           ! error code
    character(len=32), parameter :: sub = 'rof_SetgsMap_mct'
    !-----------------------------------------------------

    begr  = runoff%begr
    endr  = runoff%endr
    lsize = runoff%lnumr
    gsize = rtmlon*rtmlat

    ! Check 
    ni = 0
    do n = begr,endr
       ni = ni + 1
       if (ni > lsize) then
          write(iulog,*) sub, ' : ERROR runoff count',n,ni,runoff%lnumr
          call shr_sys_abort( sub//' ERROR: runoff > expected' )
       endif
    end do
    if (ni /= lsize) then
       write(iulog,*) sub, ' : ERROR runoff total count',ni,runoff%lnumr
       call shr_sys_abort( sub//' ERROR: runoff not equal to expected' )
    endif

    ! Determine gsmap_rof
    allocate(gindex(lsize),stat=ier)
    ni = 0
    do n = begr,endr
       ni = ni + 1
       gindex(ni) = runoff%gindex(n)
    end do
    call mct_gsMap_init( gsMap_rof, gindex, mpicom_r, ROFID, lsize, gsize )
    deallocate(gindex)

  end subroutine rof_SetgsMap_mct

!===============================================================================

  subroutine rof_domain_mct( lsize, gsMap_r, dom_r )

    !-----------------------------------------------------
    !
    ! !DESCRIPTION:
    ! Send the runoff model domain information to the coupler
    !
    ! !ARGUMENTS:
    implicit none
    integer        , intent(in)    :: lsize       ! Size of runoff domain information
    type(mct_gsMap), intent(inout) :: gsMap_r     ! Output MCT GS map for runoff model
    type(mct_ggrid), intent(out)   :: dom_r       ! Domain information from the runoff model
    !
    ! LOCAL VARIABLES
    integer :: n, ni              ! index
    integer , pointer :: idata(:) ! temporary
    real(r8), pointer :: data(:)  ! temporary
    real(r8) :: re = SHR_CONST_REARTH*0.001_r8 ! radius of earth (km)
    character(len=32), parameter :: sub = 'rof_domain_mct'
    !-----------------------------------------------------

    ! lat/lon in degrees,  area in radians^2, mask is 1 (land), 0 (non-land)
    ! Note that in addition land carries around landfrac for the purposes of domain checking
    call mct_gGrid_init( GGrid=dom_r, CoordChars=trim('x:y:z'), &
      OtherChars=trim('lat:lon:area:frac:mask:aream'), lsize=lsize )

    ! Allocate memory
    allocate(data(lsize))

    ! Determine global gridpoint number attribute, GlobGridNum, which is set automatically by MCT
    call mct_gsMap_orderedPoints(gsMap_r, iam, idata)
    call mct_gGrid_importIAttr(dom_r,'GlobGridNum',idata,lsize)

    ! Determine domain (numbering scheme is: West to East and South to North to South pole)
    ! Initialize attribute vector with special value
    data(:) = -9999.0_R8 
    call mct_gGrid_importRAttr(dom_r,"lat"  ,data,lsize) 
    call mct_gGrid_importRAttr(dom_r,"lon"  ,data,lsize) 
    call mct_gGrid_importRAttr(dom_r,"area" ,data,lsize) 
    call mct_gGrid_importRAttr(dom_r,"aream",data,lsize) 
    data(:) = 0.0_R8     
    call mct_gGrid_importRAttr(dom_r,"mask" ,data,lsize) 

    ! Determine bounds numbering consistency
    ni = 0
    do n = runoff%begr,runoff%endr
       ni = ni + 1
       if (ni > runoff%lnumr) then
          write(iulog,*) sub, ' : ERROR runoff count',n,ni,runoff%lnumr
          call shr_sys_abort( sub//' ERROR: runoff > expected' )
       end if
    end do
    if (ni /= runoff%lnumr) then
       write(iulog,*) sub, ' : ERROR runoff total count',ni,runoff%lnumr
       call shr_sys_abort( sub//' ERROR: runoff not equal to expected' )
    endif

    ! Fill in correct values for domain components
    ni = 0
    do n = runoff%begr,runoff%endr
       ni = ni + 1
       data(ni) = runoff%lonc(n)
    end do
    call mct_gGrid_importRattr(dom_r,"lon",data,lsize) 

    ni = 0
    do n = runoff%begr,runoff%endr
       ni = ni + 1
       data(ni) = runoff%latc(n)
    end do
    call mct_gGrid_importRattr(dom_r,"lat",data,lsize) 

    ni = 0
    do n = runoff%begr,runoff%endr
       ni = ni + 1
       data(ni) = runoff%area(n)*1.0e-6_r8/(re*re)
    end do
    call mct_gGrid_importRattr(dom_r,"area",data,lsize) 

    ni = 0
    do n = runoff%begr,runoff%endr
       ni = ni + 1
       data(ni) = 1.0_r8
    end do
    call mct_gGrid_importRattr(dom_r,"mask",data,lsize) 
    call mct_gGrid_importRattr(dom_r,"frac",data,lsize) 

    deallocate(data)
    deallocate(idata)

  end subroutine rof_domain_mct

!====================================================================================
 
  subroutine rof_import_mct( x2r_r, totrunin)

    !---------------------------------------------------------------------------
    ! DESCRIPTION:
    ! Obtain the runoff input from the coupler
    !
    ! ARGUMENTS:
    implicit none
    type(mct_aVect), intent(inout) :: x2r_r         
    real(r8)       , pointer       :: totrunin(:,:) 
    !
    ! LOCAL VARIABLES
    integer :: n2, n, nt, begr, endr, nliq, nfrz
    character(len=32), parameter :: sub = 'rof_import_mct'
    !---------------------------------------------------------------------------
    
    ! Note that totrunin is a flux

    nliq = 0
    nfrz = 0
    do nt = 1,nt_rtm
       if (trim(rtm_tracers(nt)) == 'LIQ') then
          nliq = nt
       endif
       if (trim(rtm_tracers(nt)) == 'ICE') then
          nfrz = nt
       endif
    enddo
    if (nliq == 0 .or. nfrz == 0) then
       write(iulog,*) trim(sub),': ERROR in rtm_tracers LIQ ICE ',nliq,nfrz,rtm_tracers
       call shr_sys_abort()
    endif

    begr = runoff%begr
    endr = runoff%endr
    do n = begr,endr
       n2 = n - begr + 1
       totrunin(n,nliq) = x2r_r%rAttr(index_x2r_Flrl_rofliq,n2)
       totrunin(n,nfrz) = x2r_r%rAttr(index_x2r_Flrl_rofice,n2)
    enddo

  end subroutine rof_import_mct

!====================================================================================

  subroutine rof_export_mct( r2x_r )

    !---------------------------------------------------------------------------
    ! DESCRIPTION:
    ! Send the runoff model export state to the coupler
    !
    ! ARGUMENTS:
    implicit none
    type(mct_aVect), intent(inout) :: r2x_r  ! Runoff to coupler export state
    !
    ! LOCAL VARIABLES
    integer :: ni, n, nt, nliq, nfrz
    logical :: first_time = .true.
    character(len=32), parameter :: sub = 'rof_export_mct'
    !---------------------------------------------------------------------------
    
    nliq = 0
    nfrz = 0
    do nt = 1,nt_rtm
       if (trim(rtm_tracers(nt)) == 'LIQ') then
          nliq = nt
       endif
       if (trim(rtm_tracers(nt)) == 'ICE') then
          nfrz = nt
       endif
    enddo
    if (nliq == 0 .or. nfrz == 0) then
       write(iulog,*) trim(sub),': ERROR in rtm_tracers LIQ ICE ',nliq,nfrz,rtm_tracers
       call shr_sys_abort()
    endif

    r2x_r%rattr(:,:) = 0._r8

    if (first_time) then
       if (masterproc) then
       if ( ice_runoff )then
          write(iulog,*)'Snow capping will flow out in frozen river runoff'
       else
          write(iulog,*)'Snow capping will flow out in liquid river runoff'
       endif
       endif
       first_time = .false.
    end if

    ni = 0
    if ( ice_runoff )then
       do n = runoff%begr,runoff%endr
          ni = ni + 1
          if (runoff%mask(n) == 2) then
             ! liquid and ice runoff are treated separately - this is what goes to the ocean
             r2x_r%rAttr(index_r2x_Forr_roff,ni) = &
                  runoff%runoff(n,nliq)/(runoff%area(n)*1.0e-6_r8*1000._r8)
             r2x_r%rAttr(index_r2x_Forr_ioff,ni) = &
                  runoff%runoff(n,nfrz)/(runoff%area(n)*1.0e-6_r8*1000._r8)
             if (ni > runoff%lnumr) then
                write(iulog,*) sub, ' : ERROR runoff count',n,ni
                call shr_sys_abort( sub//' : ERROR runoff > expected' )
             endif
          endif
       end do
    else
       do n = runoff%begr,runoff%endr
          ni = ni + 1
          if (runoff%mask(n) == 2) then
             ! liquid and ice runoff are bundled together to liquid runoff, and then ice runoff set to zero
             r2x_r%rAttr(index_r2x_Forr_roff,ni) =   &
                  (runoff%runoff(n,nfrz)+runoff%runoff(n,nliq))/(runoff%area(n)*1.0e-6_r8*1000._r8)
             r2x_r%rAttr(index_r2x_Forr_ioff,ni) = 0._r8
             if (ni > runoff%lnumr) then
                write(iulog,*) sub, ' : ERROR runoff count',n,ni
                call shr_sys_abort( sub//' : ERROR runoff > expected' )
             endif
          endif
       end do
    end if

    ! Flooding back to land, sign convention is positive in land->rof direction
    ! so if water is sent from rof to land, the flux must be negative.
    ni = 0
    do n = runoff%begr, runoff%endr
       ni = ni + 1
       r2x_r%rattr(index_r2x_Flrr_flood,ni) = -runoff%flood(n)
    end do

    ! Want volr on land side to do a correct water balance
    ni = 0
    do n = runoff%begr, runoff%endr
      ni = ni + 1
         r2x_r%rattr(index_r2x_Slrr_volr,ni) = runoff%volr_nt1(n) &
                                             / (runoff%area(n))
    end do

  end subroutine rof_export_mct

end module comp_rof
