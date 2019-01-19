module atm_comp_mct

    use pio       , only : file_desc_t, io_desc_t, var_desc_t, pio_double,  pio_def_dim,&
                           pio_put_att, pio_enddef, pio_initdecomp, pio_read_darray, pio_freedecomp, &
                           pio_closefile, pio_write, pio_def_var, pio_inq_varid, &
                           pio_noerr, pio_bcast_error, pio_internal_error, pio_seterrorhandling
    use mct_mod
    use esmf
    use mpi
    use timeCesm
    use global_var
    use base_sys
    use base_fields
    ! real phycs and dynamics modules     
    use camsrfexch   , only : cam_out_t, cam_in_t
    use phys_grid    , only: get_ncols_p, get_gcol_all_p, &
                             ngcols, get_gcol_p, get_rlat_all_p, &
                             get_rlon_all_p, get_area_all_p
    use cam_cpl_indices
    use co2_cycle    , only: c_i, co2_readFlux_ocn, co2_readFlux_fuel, co2_transport, & 
                             co2_time_interp_ocn, co2_time_interp_fuel, data_flux_ocn, data_flux_fuel
    use physconst    , only: mwco2

    use radiation    , only: radiation_get, radiation_do, radiation_nextsw_cday

    use time_manager , only: get_curr_calday, advance_timestep, get_curr_date, get_nstep, &
                             is_first_step, get_step_size, timemgr_init, timemgr_check_restart

    implicit none
    public :: atm_init_mct
    public :: atm_run_mct
    public :: atm_final_mct

    private :: atm_SetgsMap_mct
    private :: atm_import_mct
    private :: atm_export_mct
    private :: atm_domain_mct
    private :: atm_read_srfrest_mct
    private :: atm_write_srfrest_mct
    
    ! private data
    type(cam_in_t),  pointer :: cam_in(:)
    type(cam_out_t), pointer :: cam_out(:)

    type(attrVect) :: a2x_a_SNAP
    type(attrVect) :: a2x_a_SUM

    integer, parameter :: nlen = 256
    character(len=nlen) :: fname_srf_cam
    character(len=nlen) :: pname_srf_cam
    character(len=nlen) :: rsfilename_spec_cam


contains
    

subroutine atm_init_mct(compInfo, EClock, x2a_a, a2x_a)

    type(compMeta),      intent(in)    :: compInfo
    type(ESMF_Clock),    intent(in)    :: EClock
    type(attrVect),      intent(inout) :: x2a_a
    type(attrVect),      intent(inout) :: a2x_a
    
    ! local
    type(gsMap),   pointer :: gsmap_atm
    type(gGrid),   pointer :: domain_atm
    integer  :: ATMID
    integer  :: mpicom_atm
    integer  :: lsize
    integer  :: iradsw
    logical  :: exists          ! ture if file exists
    real(r8) :: nextsw_cday     ! calendar of next atm shortwave
    integer  :: stepno          ! time step
    integer  :: dtime_sync      ! integer timestep size
    integer  :: currentymd      ! current year-month-day
    integer  :: dtime           ! time step increment  (sec)
    integer  :: atm_cpl_dt      ! driver atm coupling time step
    integer  :: nstep           ! CAM nstep
    real(r8) :: caldayp1        ! CAM calendar day for next cam time step
    integer  :: dtime_cam       ! Time-step increment (sec)
    integer  :: ymd             ! CAM current date (YYYYMMDD)
    integer  :: yr              ! CAM current year
    integer  :: mon             ! CAM current month
    integer  :: day             ! CAM current day
    integer  :: tod             ! CAM current time of day (sec)
    integer  :: start_ymd       ! Start date (YYYYMMDD)
    integer  :: start_tod       ! Start time of day (sec)
    integer  :: ref_ymd         ! Reference date (YYYYMMDD)
    integer  :: ref_tod         ! Reference time of day (sec)
    integer  :: stop_ymd        ! Stop date (YYYYMMDD)
    integer  :: stop_tod        ! Stop time of day (sec)
    logical  :: perpetual_run   ! If in perpetual mode or not
    integer  :: perpetual_ymd   ! Perpetual date (YYYYMMDD)
    integer  :: logunit, loglev  
    character(len=100) :: calendar
    character(len=100) :: starttype
    integer :: lbnum
    integer :: hdim1_d, hdim2_D   ! dimensions of rectangular horizontal grid
                                  ! data structure, IF 1D data structure, then
                                  ! hdim2_d == 1
    character(len=64) :: filein   ! Input namelist filename
    logical  :: first_time = .true.
   

    call compMeta_getInfo(compInfo, ID=ATMID, comp_gsmap=gsmap_atm, &
                    domain=domain_atm, comm=mpicomm_atm)
    
    if(first_time)then
        call cam_instance_init(ATMID)

        ! set filename specifier for restart surface file 
        ! (%c=caseid, $y=year, $m=month, $d=day, $s=seconds in  day)
        rsfilename_spec_cam = '%c.cam'//trim(inst_suffix)//'.rs.%y-%m-%d-%s.nc'

        ! determine attrvect indices
        call cam_cpl_indices_set()
       
        ! Redirect share output to cam log
        call spmdinit(mpicom_atm)

        if(masterproc)then
            inquire(file='atm_modelio.nml'//trim(inst_suffix), exist=exists)
            if(exists)then
                iulog = base_file_getUnit()   ! ??? not correct
                call base_file_setIO('atm_modelio.nml'//timr(inst_suffix), iulog)
            end if
            write(iulog, *)"CAM atmosphere model initialization"
        end if

        ! get log

        ! consistency check
        if(co2_readFlux_ocn .and. index_x2a_Faoo_fco2_ocn/=0)then
            write(iulog, *)'error co2_readFlux_ocn and index_x2a_Faoo_fco2_ocn cannot both be active'
            call base_sys_abort('')
        end if

        ! get data from infodata obj....


        !
        ! get nsrest from startup type methods
        !
        if(trim(starttype) == trim(seq_infodata_start_type_start))then
            nsrest = 0
        else if(trim(starttype)==trim(seq_infodata_start_type_cont))then
            nsrest = 1
        else if(trim(starttype)==trim(seq_infodata_start_type_brnch))then
            nsrest = 3
        else
            write(iulog, *)'atm_comp_mct: ERROR: unknown starttype'
            call base_sys_abort('')
        end if

        ! init time manager
        call time_clockGetInfo(EClock, start_ymd=start_ymd, &
                    start_tod=start_tod, ref_ymd=ref_ymd, ref_tod=ref_tod, &
                    stop_ymd=stop_ymd, stop_tod=stop_tod, calendar=calendar)

        ! read namelist
        filein = "atm_in"//trim(inst_suffix)
        call read_namelist(single_column_in=single_column, scmlat_in=scmlat, &
                       scmlon_in=scmlon, nlfilename_in=filein)

        ! Initialize cam time manager
        if(nsrest == 0)then
            call timemgr_init(calendar_in=calendar, start_ymd=start_ymd, &
                           start_tod=start_tod, ref_ymd=ref_ymd, &
                           ref_tod=ref_tod,  stop_ymd=stop_ymd, &
                           stop_tod=stop_tod, perpetual_run=perpetual_run,&
                           perpetual_ymd=perpetual_ymd)
        end if
        ! first phase of cam initialization
        ! Initialize mpicom_atm, allocate cam_in and cam_out and determine
        ! atm decomposition (needed to initialize gsmap)
        ! for an initial run, cam_in and cam_out are allocated in cam_initial
        ! for a restart/branch run, cam_in and cam_out are allocated in restart
        ! set defaults then override with user-specified input and initialize
        ! time manager Note that the following arguments are needed to cam_init
        ! for timemgr_restart only

        call cam_init(cam_out, cam_in, mpicom_atm, start_ymd, start_tod, ref_ymd, &
                      ref_tod, stop_ymd, stop_tod, perpetual_run, perpetual_ymd, calendar)

        ! check consistency of restart time information with input clock
        if(nsrest /= 0)then
            dtime_cam = get_step_size()
            call timemgr_clock_restart(calendar, start_ymd, start_tod, ref_ymd, &
                                       ref_tod, dtime_cam, perpetual_run, perpetual_ymd)
        end if

        ! init MCT gsMap, domain and attrVect
        call atm_SetgsMap_mct(mpicom_atm, ATMID, gsMap_atm)
        lsize = gsMap_lsize(gsMap_atm, mpicom_atm)

        ! init MCT domain
        call atm_domain_mct(lsize, gsMap_atm, domain_atm)
       
        ! init mct attrVector
        call attrVect_init(a2x_a, rList=flds_a2x_fields, lsize=lsize)
        call attrVect_zero(a2x_a)
 
        call attrVect_init(x2a_a, rList=flds_x2a_fields, lsize=lsize)
        call attrVect_zero(x2a_a)

        call attrVect_init(a2x_a_SNAP, rList=a2x_avg_flds, lsize=lsize)
        call attrVect_zero(a2x_a_SNAP)

        call attrVect_init(a2x_a_SUM, rList=a2x_avg_flds, lsize=lsize)
        call attrVect_zero(a2x_a_SUM)

        ! init averaging counter
        avg_count = 0
       
        ! create initial atm export state
        call atm_export_mct(cam_out, a2x_a)

        ! set flag to specify that an extra albedo calculation is to be done
        ! (i.e. specify active)
        !subroutine get atm spec
        call get_horiz_grid_dim_d(hdim1_d, hdim2_d)
        ! back to spec

        ! set flag to indicate that CAM will provide carbon and dust deposition
        ! fluxes. This is now hardcoded to .true. since the ability of CICE to
        ! read these fluxes from a file has been removed.
          ! info put data emmm

        ! set time step of radiation computation as the current calday
        ! This will only be used on the first timestep of an initial run

        if(nsrest == 0)then
            nextsw_cday = get_curr_calday()
            ! seq info putdata emmm
        end if

        ! End redirection of share output to cam log
        
        ! call file setLogUnit
        ! call file setLogLevel

        first_time = .false. 
    else 
        ! for initial run, run cam radiation/clouds and return
        ! for restart run, read restart x2a_a
        ! Note -a2x_a is computed upon the completion of the previous run -
        ! cam_run1 is called only for the purposes  of finishing the flux
        ! averaged calculation to compute a2x_a 
        ! Note - cam_run1 is called on restart only to have cam internal state
        ! consistent with the a2x_a state sent to the coupler

        ! Redirect share output to cam log
        
        ! log set
        ! log level set
        ! 
 
        call time_clockGetInfo(EClock, curr_ymd=CurrentYMD, StepNo=StepNo, dtime=DTime_Sync)
        if(StepNo==0)then
            call atm_import_mct(x2a_a, cam_in)
            call cam_run1(cam_in, cam_out)
            call atm_export_mct(cam_out, a2x_a)
        else
            call atm_read_srfrest_mct(EClock, compInfo, x2a_a, a2x_a)
            call atm_import_mct(x2a_a, cam_in)
            call cam_run1(cam_in, cam_out)
        end if 

        ! compute time of next radiation computation, like in run method for
        ! exact restart

        call time_clockGetInfo(EClock, dtime=atm_cpl_dt)
        dtime = get_step_size()
        nstep = get_nstep()
        if(nstep < 1 .or. dtime < atm_cpl_dt)then
            nextsw_cday = radiation_nextsw_cday()
        else if(dtime == atm_cpl_dt)then
            caldayp1 = get_curr_calday(offset=int(dtime))
            nextsw_cday = radiation_nextsw_cday()
            if(caldayp1 /= nextsw_cday) nextsw_cday = -1._r8
        else 
            call base_sys_abort('dtime must be less than or equal to atm_cpl_dt')
        end if

        ! put data in infodata

        ! end redirection of share output to cam log
        ! file setlog
        ! file setlogLevel

    end if

end subroutine atm_init_mct


subroutine atm_run_mct(compInfo, EClock, x2a_a, a2x_a)

    use timeCesm
    use pmgrid,         only: plev, plevp
    use constituents,   only: pcnst
    use base_sys_mod
    use chemistry,      only: chem_reset_fluxes
    use offline_driver, only: offline_driver_dorun, offline_driver_end_of_data

    type(Meta),           intent(in)    :: metaData
    type(ESMF_Clock),     intent(in)    :: EClock
    type(attrVect),       intent(inout) :: x2a_a
    type(attrVect),       intent(inout) :: a2x_a

    ! local variables
    integer :: lsize              ! local size of attrVect
    integer :: StepNo             ! time step
    integer :: DTime_Sync         ! integer timestep size
    integer :: CurrentYMD         ! current  year-month-day
    integer :: iradsw             ! shortwave radiation frequency (time steps)
    logical :: dosend             ! send data back to driver if true
    integer :: dtime              ! time step increment (sec)
    integer :: atm_cpl_dt         ! driver atm coupling time step
    integer :: ymd_sync           ! Sync date (YYYYMMDD)
    integer :: yr_sync            ! Sync current year
    integer :: mon_sync           ! Sync current month
    integer :: day_sync           ! Sync current day
    integer :: tod_sync           ! Sync current time of day (sec)
    integer :: ymd                ! CAM current date
    integer :: yr                 ! CAM current year
    integer :: mon                ! CAM current month
    integer :: day                ! CAM current day
    integer :: tod                ! CAM current time of day (sec)
    integer :: nstep              ! CAM nstep
    integer :: logunit, loglev    !
    real(r8) :: caldayp1          ! CAM calendar day for next cam time step
    real(r8) :: nextsw_cday       ! calendar of next atm shortwave
    logical :: rstwr              ! write restart file before returning if true
    logical :: nlend              ! Flag signaling last time-step
    logical :: rstwr_sync         ! write restart file before returning if ture
    logical :: nlend_sync         ! Flag signaling last time-step
    logical :: first_time = .true.
    character(len=*), parameter :: subname = "atm_run_mct" 
    integer :: lbnum

    ! logfile init and set 
     

    ! Note that sync clock time should match cam time at end of time step/loop
    ! not beginning
    call time_clockGetInfo(EClock, curr_ymd=ymd_sync, curr_tod, tod_sync, &
          curr_yr=yr_sync, curr_mon=mon_sync, curr_day=day_sync)

    ! load orbital parameters not support presently

    nlend_sync =  time_alarmIsOn(EClock, alarm_stop_name) ! stop name not defined
    rstwr_sync =  time_alarmIsOn(EClock, alarm_restart_name) ! restart_name not defined

    ! Map input from mct to cam data structure

    call atm_import_mct(x2a_a, cam_in)

    ! Cycle over all time steps in the atm coupling interval
  
    dosend = .true.
    do while(.not. dosend)
        ! (re)set surface fluxes of chem tracers here to MEGAN fluxes (from CLM)
        ! or to zero so that fluxes read from file can be added to MEGAN
        call chem_reset_fluxes(x2a_a%rAttr, cam_in)

        ! determint if dosend
        ! when time is not updated at the beginning of the loop - then return
        ! only if are in sync with clock before time is updated

        call get_curr_date(yr, mon, day, tod)  ! not implemented in timeCesm
        ymd = yr*10000+mon*100+day
        tod = tod   ! emmm
        if(offline_driver_dorun)then
            dosend = offline_driver_end_of_data()
        else 
            dosend = time_ClockDateInSync(EClock, ymd, tod) ! not implemented in timeCesm
        end if

        ! determine if time to write cam restart and stop
    
        rstwr =  .false.
        if(rstwr_sync .and. dosend)rstwr = .true.
        nlend = .false.
        if(nlend_sync .and. dosend)nlend = .true.

        ! single column specific input maybe not implemented
        if(single_column)then
            call scam_use_iop_srf(cam_in)
        end if

        ! RUN CAM (run2, run3, run4)

        call cam_run2(cam_out, cam_in)
        
        call cam_run3(cam_out)

        call cam_run4(cam_out, cam_in, rstwr, nlend, &
             yr_spec=ys_sync, mon_spec=mon_sync, day_spec=day_sync, sec_spec=tod_sync)

        ! Advance cam time step
        if(.not.offline_driver_dorun )then

            call advance_timestep() ! ???
        end if

        ! run cam radiation/clouds (run1)
        call cam_run1(cam_in, cam_out)

        ! Map output form cam to mct data structures

        call atm_export_mct(cam_out, a2x_a)

        ! compute snapshot attribute vector for accumulation

        ! don't accumulate on first coupling freq ts1 and ts2
        ! for consistency with ccsm3 when flxave is off
        nstep  = get_nstep()
        if (nstep <= 2)then
            call attrVect_copy(a2x_a, a2x_a_SUM)
            avg_count = 1
        else
            call attrVect_copy(a2x_a, a2x_s_SNAP)
            call attrVect_accum(aVin=a2x_a_SNAP, aVout=a2x_a_SUM)
            avg_count = avg_count + 1
        end if

        ! finish accumulation of attribute vector and average and copy
        ! accumulation field into output attribute vector
        call attrVect_avg(a2x_a_SUM, avg_count)
        call attrVect_copy(a2x_a_SUM, a2x_a)
        call attrVect_zero(a2x_a_SUM)
        avg_count = 0
        
        ! Get time of next radiation calculation - albedos will need to be 
        ! calculated by each surface model at this time
        call time_clockGetInfo(EClock, dtime=atm_cpl_dt)
        dtime = get_step_size()
        if(dtime < atm_cpl_dt)then
            nextsw_cday = radiation_nextsw_cday()
        else if(dtime == atm_cpl_dt)then
            caldayp1 = get_curr_calday(offset=int(dtime))
            nextsw_cday = radiation_nextsw_cday()
            if(caldayp1 /= nextsw_cday)nextsw_cday = -1._r8
        else 
            call base_sys_abort('dtime must be less than or equal to atm_cpl_dt')
        end if
        ! add nextsw_cday to infodata ????
        
        ! write merged surface data restart file if appropriate
        if(rstwr_sync)then
            call atm_write_srfrest_mct()   ! not implement yet
        end if
    
        ! check for consistency of internal cam clock with master sync clock
         
        dtime = get_step_size()
        call get_curr_date(yr, mon, day, tod, offset=-dtime)
        ymd = yr*10000+ mon*100+day
        !tod = tod
        if((.not. time_clockDateInSync(EClock, ymd, tod)) .and. (.not. offline_driver_dorun))then
            call time_clockGetInfo(EClock, curr_ymd=ymd_sync, curr_tod=tod_sync)
            write(logUnit, *), 'cam ymd=', ymd, 'cam tod=', tod
            write(logUnit, *), 'sync ymd=', ymd_sync, 'sync tod=', tod_sync
            call base_sys_abort(subname//': CAM clock is not in sync with master Sync Clock')
        end if

        ! end redirection of share output to cam log

    end do


end subroutine atm_run_mct


subroutine atm_final_mct(metaData, EClock, x2a_a, a2x_a)

    implicit none
    type(Meta),         intent(in)    :: metaData
    type(ESMF_Clock),   intent(in)    :: EClock
    type(attrVect),     intent(inout) :: x2a_a
    type(attrVect),     intent(inout) :: a2x_a

    call cam_final(cam_out, cam_in)


end subroutine atm_final_mct


subroutine atm_SetgsMap_mct(mpicom_atm, ATMID， gsmap_atm)
    use phys_grid, only : get_nlcols_p

    integer,         intent(in)    :: mpicom_atm
    integer,         intent(in)    :: ATMID
    type(gsMap),     intent(inout) :: gsmap_atm
    
    integer, allocatable :: gindex(:)
    integer :: i, n, c, ncols, sizebuf, nlcols
    integer :: ier

    sizebuf = 0
    do c= begchunk, endchunk
        ncols = get_ncols_p(c) 
        do i = 1, ncols
            sizebuf = sizebuf + 1  !!???
        end do
    end do

    allocate(gindex(sizebuf))

    n = 0
    do c = begchunk, endchunk 
        ncols = get_ncols_p(c)
        do i = 1, ncols
            n = n+1
            gindex(n) = get_gcol_p(c, i)
        end do
    end do

    nlcols = get_nlcols_p()
    call gsMap_init(gsmap_atm, gindex, mpicom_atm, ATMID, nlcols, ngcols)
    deallocate(gindex)

end subroutine atm_SetgsMap_mct

subroutine atm_import_mct(x2a_a, cam_in)

    use dust_intr,   only : dust_idx1
    use shr_const_mod, only : get_spc_ndx
    use seq_drydep_mod, only : n_drydep

    type(attrVect),     intent(inout) :: x2a_a
    type(cam_in_t),     intent(inout) :: cam_in(begchunk:endchunk)

    ! local variables
    integer  :: i, lat, n, c, ig
    integer  :: ncols
    integer  :: dust_ndx
    logical, save :: first_time = .true.

    ig = 1
    do c = begchunk, endchunk
        ncols = get_ncols_p(c)
        cam_in(c)%cflx(:,:) = 0._r8
        do i = 1, ncols
            cam_in(c)%wsx(i) = -x2a_a%rAttr(index_x2a_Faxx_taux, ig)
            cam_in(c)%wsy(i) = -x2a_a%rAttr(index_x2a_Faxx_tauy, ig)
            cam_in(c)%lhf(i) = -x2a_a%rAttr(index_x2a_Faxx_lat, ig)
            cam_in(c)%shf(i) = -x2a_a%rAttr(index_x2a_Faxx_sen, ig)
            cam_in(c)%lwup(i) = -x2a_a%rAttr(index_x2a_Faxx_lwup, ig)
            cam_in(c)%cflx(i, 1) = -x2a_a%rAttr(index_x2a_Faxx_evap, ig)
            cam_in(c)%asdir(i) = x2a_a%rAttr(index_x2a_Sx_avsdr, ig)
            cam_in(c)%aldir(i) = x2a_a%rAttr(index_x2a_Sx_anidr, ig)
            cam_in(c)%asdif(i) = x2a_a%rAttr(index_x2a_Sx_avsdf, ig)
            cam_in(c)%aldif(i) = x2a_a%rAttr(index_x2a_Sx_anidf, ig)
            cam_in(c)%ts(i) = x2a_a%rAttr(index_x2a_Sx_t, ig)
            cam_in(c)%sst(i) = x2a_a%rAttr(index_x2a_So_t, ig)
            cam_in(c)%snowhland(i) = x2a_a%rAttr(index_x2a_Sl_snowh, ig)
            cam_in(c)%snowhice(i) = x2a_a%rAttr(index_x2a_Si_snowh, ig)
            cam_in(c)%tref(i) = x2a_a%rAttr(index_x2a_Sx_tref, ig)
            cam_in(c)%qref(i) = x2a_a%rAttr(index_x2a_Sx_qref, ig)
            cam_in(c)%u10(i) = x2a_a%rAttr(index_x2a_Sx_u10, ig)
            cam_in(c)%icefrac(i) = x2a_a%rAttr(index_x2a_Sf_ifrac, ig)
            cam_in(c)%ocnfrac(i) = x2a_a%rAttr(index_x2a_Sf_ofrac, ig)
            cam_in(c)%landfrac(i) = x2a_a%rAttr(index_x2a_Sf_lfrac, ig)
            if(associated(cam_in(c)%ram1))cam_in(c)%ram1(i) = x2a_a%rAttr(index_x2a_Sl_ram1, ig)
            if(associated(cam_in(c)%fv))cam_in(c)%fv(i) = x2a_a%rAttr(index_x2a_Sl_fv, ig)
            if(associated(cam_in(c)%soilw))cam_in(c)%soilw(i) = x2a_a%rAttr(index_x2a_Sl_soilw, ig)
            if (dust_ndx>0)then

            end if

            ! dry dep velocities
            if(index_x2a_Sl_ddvel/=0 .and. n_drydep>0)then
                cam_in(c)%depvel(i,:n_drydep) = &
                       x2a_a%rAttr(index_x2a_Sl_ddvel:index_x2a_Sl_ddvel+n_drydep-1, ig)
            end if

            !  fields needed to calculate water isotopes to ocean evaporation
            !  processs
            cam_in(c)%ustar(i) = x2a_a%rAttr(index_x2a_So_ustar, ig)
            cam_in(c)%re(i)    = x2a_a%rAttr(index_x2a_So_re, ig)
            cam_in(c)%ssq(i)   = x2a_a%rAttr(index_x2a_Sp_ssq, ig)
 
            ! bgc scenarios
            if(index_x2a_Fall_fco2_lnd/=0)then
                cam_in(c)%fco2_lnd(i) = -x2a_a%rAttr(index_x2a_Fall_fco2_lnd, ig)
            end if
            if(index_x2a_Faoo_fco2_ocn/=0)then
                cam_in(c)%fco2_lnd(i) = -x2a_a%rAttr(index_x2a_Faoo_fco2_ocn, ig)
            end if
            if(index_x2a_Faoo_fdms_ocn/=0)then
                cam_in(c)%fdms(i) = -x2a_a%rAttr(index_x2a_Faoo_fdms_ocn, ig)
            end if
            ig = ig + 1
        end do  
    end do
     
    ! get total co2 flux from components,
    ! note - co2_transport determines if cam_in(c)%cflx(i, c_i(1:4)) is
    ! allocated
    if (co2_transport())then
        if(co2_readFlux_ocn)then
            call co2_time_interp_ocn()   !???
        end if
        if(co2_readFlux_fuel)then
            call co2_time_interp_fuel()
        end if

        ! from ocn  : data read in or from coupler or zero
        ! from fuel : data read in or zero
        ! from lnd  : through coupler or zero
        do c = begchunk, endchunk
            ncols = get_ncols_p(c)
            do i = 1, ncols
                ! all co2 fluxes in unit kgCO2/m2/s ! co2 flux from ocn
                if(index_x2a_Faoo_fco2_ocn/=0)then
                    cam_in(c)%cflx(i, c_i(1)) = cam_in(c)%fco2_ocn(i)
                else if(co2_readFlux_ocn)then
                    cam_in(c)%cflx(i, c_i(1)) = &
                       -data_flux_ocn%co2flx(i,c)*(1._r8-cam_in(c)%landfrac(i))&
                       * mwco2*1.0e-3_r8
                else 
                    cam_in(c)%cflx(i,c_i(1)) = 0._r8
                end if
                 
                ! co2 flux from fossil fuel
                if(co2_readFlux_fuel) then
                    cam_in(c)%clfx(i, c_i(2))  = data_flux_fuel%co2flx(i,c)
                else
                    cam_in(c)%clfx(i, c_i(2))  = 0._r8
                end if
 
                ! co2 flux from land (cpl already multiplies flux by land
                ! fraction)
                if(index_x2a_Fall_fco2_lnd/=0)then
                    cam_in(c)%cflx(i, c_i(3)) = cam_in(c)%fco2_lnd(i)
                else
                    cam_in(c)%clfx(i, c_i(3)) = 0._r8
                end if
              
                ! merged co2 flux
                cam_in(c)%cflx(i, c_i(4)) = cam_in(c)%cflx(i, c_i(1))+ &
                                            cam_in(c)%cflx(i, c_i(2))+ &
                                            cam_in(c)%cflx(i, c_i(3))
            end do        
        end do
    end if
    
    ! if first step , determine longwave up flux from the surface temperature
    if(first_time)then
        if(is_first_step())then
            do c = begchunk, endchunk
                ncols = get_ncols_p(c)
                do i = 1, ncols
                    cam_in(c)%lwup(i) = shr_const_stepol*(cam_in(c)%ts(i)**4)
                end do
            end do
        first_time = .false.
        end if
    end if


end subroutine atm_import_mct

subroutine atm_export_mct(cam_out, a2x_a)

    implicit none
    type(cam_out_t),    intent(in)    :: cam_out(begchunk:endchunk)
    type(attrVect),     intent(inout) :: a2x_a

    ! local variables
    integer :: avsize, avnat
    integer :: i, m, c, n, ig
    integer :: ncols

    ! copy from component arrays into chunk  array data structure
    ! rearrange data from chunk structure into lat-lon buffer and subsequently
    ! create attrVect
    ig = 1
    do c = begchunk, endchunk
        ncols = get_ncols_p(c)
        do i = 1, ncols
            a2x_a%rAttr(index_a2x_Sa_pslv, ig) = cam_out(c)%psl(i)
            a2x_a%rAttr(index_a2x_Sa_z,    ig) = cam_out(c)%zbot(i)
            a2x_a%rAttr(index_a2x_Sa_u,    ig) = cam_out(c)%ubot(i)
            a2x_a%rAttr(index_a2x_Sa_v,    ig) = cam_out(c)%vbot(i)
            a2x_a%rAttr(index_a2x_Sa_tbot, ig) = cam_out(c)%tbot(i)
            a2x_a%rAttr(index_a2x_Sa_ptem, ig) = cam_out(c)%thbot(i)
            a2x_a%rAttr(index_a2x_Sa_pbot, ig) = cam_out(c)%pbot(i)
            a2x_a%rAttr(index_a2x_Sa_shum, ig) = cam_out(c)%qbot(i, 1)
            a2x_a%rAttr(index_a2x_Sa_dens, ig) = cam_out(c)%rho(i)
            a2x_a%rAttr(index_a2x_Faxa_swnet, ig) = cam_out(c)%netsw(i)
            a2x_a%rAttr(index_a2x_Faxa_lwdn,  ig) = cam_out(c)%flwds(i)
            a2x_a%rAttr(index_a2x_Faxa_rainc, ig) = (cam_out(c)%precc(i) - cam_out(c)%precsc(i))*1000._r8
            a2x_a%rAttr(index_a2x_Faxa_rainl, ig) = (cam_out(c)%percl(i) - cam_out(c)%precsl(i))*1000._r8
            a2x_a%rAttr(index_a2x_Faxa_snowc, ig) = cam_out(c)%precsc(i)*1000._r8
            a2x_a%rAttr(index_a2x_Faxa_snowl, ig) = cam_out(c)%precsl(i)*1000._r8
            a2x_a%rAttr(index_a2x_Faxa_swndr, ig) = cam_out(c)%soll(i)
            a2x_a%rAttr(index_a2x_Faxa_swvdr, ig) = cam_out(c)%sols(i)
            a2x_a%rAttr(index_a2x_Faxa_swndf, ig) = cam_out(c)%solld(i)
            a2x_a%rAttr(index_a2x_Faxa_swvdf, ig) = cam_out(c)%solsd(i)

            ! aerosol deposition fluxes
            a2x_a%rAttr(index_a2x_Faxa_bcphidry, ig) = cam_out(c)%bcphidry(i)
            a2x_a%rAttr(index_a2x_Faxa_bcphodry, ig) = cam_out(c)%bcphodry(i)
            a2x_a%rAttr(index_a2x_Faxa_bcphiwet, ig) = cam_out(c)%bcphiwet(i)
            a2x_a%rAttr(index_a2x_Faxa_ocphidry, ig) = cam_out(c)%ocphidry(i)
            a2x_a%rAttr(index_a2x_Faxa_ocphodry, ig) = cam_out(c)%ocphodry(i)
            a2x_a%rAttr(index_a2x_Faxa_ocphidry, ig) = cam_out(c)%ocphidry(i)
            a2x_a%rAttr(index_a2x_Faxa_dstwet1,  ig) = cam_out(c)%dstwet1(i) 
            a2x_a%rAttr(index_a2x_Faxa_dstdry1,  ig) = cam_out(c)%dstdry1(i)
            a2x_a%rAttr(index_a2x_Faxa_dstwet2,  ig) = cam_out(c)%dstwet2(i)
            a2x_a%rAttr(index_a2x_Faxa_dstdry2,  ig) = cam_out(c)%dstdry2(i)
            a2x_a%rAttr(index_a2x_Faxa_dstwet3,  ig) = cam_out(c)%dstwet3(i)
            a2x_a%rAttr(index_a2x_Faxa_dstdry3,  ig) = cam_out(c)%dstdry3(i)
            a2x_a%rAttr(index_a2x_Faxa_dstwet4,  ig) = cam_out(c)%dstwet4(i)
            a2x_a%rAttr(index_a2x_Faxa_dstdry4,  ig) = cam_out(c)%dstdry4(i)
 
            if(index_a2x_Sa_co2prog/=0)then
                a2x_a%rAttr(index_a2x_Sa_co2prog, ig) = cam_out(c)%co2prog(i) ! atm prognostic co2
            end if 

            if(index_a2x_Sa_co2diag/=0)then 
                a2x_a%rAttr(index_a2x_Sa_co2diag, ig) = cam_out(c)%co2diag(i) ! atm diagnostic co2
            end if

            ig = ig + 1
        end do
    end do

end subroutine atm_export_mct


subroutine atm_domain_mct(lsize, gsmap_atm, domain_atm)

    implicit none
    integer,       intent(in)    :: lsize
    type(gsMap),   intent(in)    :: gsmap_atm
    type(gGrid),   intent(inout) :: domain_atm
    ! local variables
    integer, parameter :: r8 = 8
    integer, parameter :: CONST_PI = 3.14159
    integer :: n, i, c, ncols
    real(r8) :: lats(pcols)
    real(r8) :: lons(pcols)
    real(r8) :: area(pcols)
    real(r8), pointer :: data(:)
    integer,  pointer :: idata(:)
    real(r8), pointer :: radtodeg = 180.0/CONST_PI

    call gGrid_init(GGrid=domain_atm, CoordChars=trim(), OtherChars=trim(), lsize=lsize)

    ! allocate memory
    allocate(data(lsize))
    ! init attrvect with special value
    call gsMap_orderedPoints(gsmap_atm, iam, idata)  ! iam ???
    call gGrid_importIAttr(domain_atm, 'GlobalGridNum', idata, lsize)

    ! determine domain
    data(:) = -9999.0_r8  ! r8 not debug yet
    call gGrid_importRAttr(domain_atm, "lat", data, lsize)
    call gGrid_importRAttr(domain_atm, "lon", data, lsize) 
    call gGrid_importRAttr(domain_atm, "area", data, lsize)
    call gGrid_importRAttr(domain_atm, "aream", data, lsize)
    data(:) = 0.0_r8
    call gGrid_importRAttr(domain_atm, "mask", data, lsize)
    data(:) = 1.0_r8
    call gGrid_importRAttr(domain_atm, "frac", data, lsize)

    ! fill in correct values for domain components
    n = 0
    do c = begchunk, endchunk
        ncols = get_ncols_p(c)
        call get_rlat_all_p(c, ncols, lats)
        do i =1, ncols
            n = n + 1
            data(n) = lats(i)*radtodeg
        end do
    end do
    call gGrid_importRAttr(domain_atm, "lat", data, lsize)

    n = 0
    do c = begchunk, endchunk
        ncols = get_ncols_p(c)
        call get_rlon_all_p(c, ncols, lons)
        do i = 1, ncols
            n = n + 1
            data(n) = lons(i)*radtodeg
        end do
    end do
    call gGrid_importRAttr(domain_atm, "lon", data, lsize)

    n = 0
    do c = begchunk, endchunk
        ncols = get_ncols_p(c)
        call get_real_all_p(c, cols, area)
        do i=1, ncols
            n = n + 1
            data(n) = area(i)
        end do
    end do
    call gGrid_importRAttr(domain_atm, "area", data, lsize)

    n = 0 
    do c = begchunk, endchunk
        bcols = get_ncols_p(c)
        do i = 1, ncols
            n = n + 1
            data(n) = 1._r8
        end do
    end do
    call gGrid_importRAttr(domain_atm, "mask", data, lsize)
    deallocate(data)

end subroutine atm_domain_mct

subroutine atm_read_srfrest_mct(EClock, compInfo, x2a_a, a2x_a)

    implicit none
    type(ESMF_Clock),   intent(in)      :: EClock
    type(CompMeta),     intent(inout)   :: compInfo
    type(attrVect),     intent(inout)   :: x2a_a
    type(attrVect),     intent(inout)   :: a2x_a

    ! local
    integer        :: npts     ! array size
    integer        :: rcode    ! return error code
    type(attrVect) :: gData    ! global/gathered bundle data
    integer        :: yr_sepc  ! current year
    integer        :: mon_spec ! current month
    integer        :: day_spec ! current day
    integer        :: sec_spec ! current time of day (sec)

    ! Determine and open surface restart dataset
    integer, pointer :: dof(:)
    integer :: lnx, nf_x2a, nf_a2x, k
    real(r8), allocatable :: tmp(:)
    type(file_desc_t) :: file
    type(io_desc_t)   :: iodesc
    type(var_desc_t)  :: varid
    character(len=100) :: itemc
    type(string)      :: mstring ! mct char type
    
    call time_clockGetInfo(EClock, curr_yr=yr_spec, curr_mon=mon_spec, &
                    curr_day=day_spec, curr_tod=sec_spec)
    fname_srf_cam = interpret_filename_spec(rsfilename_spec_cam, case=get_restcase(), &
             yr_spec=yr_spec, mon_spec=mon_spec, day_spec=day_spec, sec_spec=sec_spec)
    pname_srf_cam = trim(get_restartdir())//fname_srf_cam
    call getfil(pname_srf_cam, fname_srf_cam)

    call cam_pio_openfile(File, fname_srf_name,0)
    call gsMap_OrderedPoints(compInfo%gsMap, iam, Dof)
    lnx = gsMap_gsize(compInfo%gsMap)
    call pio_initdecomp(pio_subsystem, pio_double, (/lnx/), dof, iodesc)
    allocate(tmp(size(dof)))
    deallocate(dof)

    nf_x2a = attrVect_nRattr(x2a_a)

    do k=1, nf_x2a
        call attrVect_getRList(mstring, k, x2a_a)
        itemc = string_toChar(mstring)
        call string_clean(mstring)

        call pio_seterrorhandling(File, pio_bcast_error)
        rcode = pio_inq_varid(File, 'x2a_'//trim(itemc), varid)
        if(rcode==pio_noerr)then
            call pio_read_darray(File, varid, iodesc, tmp, rcode)
            x2a_a%rAttr(k, :) = tmp(:)
        else
            if(masterproc)then
                write(iulog, *)'srfrest warning: field', trim(itemc), 'is not on restart file'
                write(iulog, *)'for backwards compatibility will set it to 0'
            end if
            x2a_a%rAttr(k, :) = 0._r8
        end if
        call pio_seterrorhandling(File, pio_internal_error)
    end do

    nf_a2x = attrVect_nRattr(a2x_a)

    do k = 1, nf_a2x
        call attrVect_getRList(mstring, k, a2x_a)
        itemc = string_toChar(mstring)
        call string_clean(mstring)

        rcode = pio_inq_varid(File, 'a2x_'//trim(itemc), varid)
        call pio_read_darray(File, varid, iodesc, tmp, rcode)
        a2x_rAttr(k, :) = tmp(:)
    end do

    call pio_freedecomp(File, iodesc)
    call pio_closefile(File)
    deallocate(tmp)

end subroutine atm_read_srfrest_mct


subroutine atm_write_srfrest_mct(compInfo, x2a_a, a2x_a, yr_spec, &
                 mon_spec, day_spec, sec_spec)

    use cam_pio_utils

    type(compMeta),      intent(in) :: compInfo
    type(attrVect),      intent(in) :: x2a_a
    type(attrVect),      intent(in) :: a2x_a
    integer,             intent(in) :: yr_spec      ! simulation year
    integer,             intent(in) :: mon_spec     ! simulation month
    integer,             intent(in) :: day_spec     ! simulation day
    integer,             intent(in) :: sec_spec     ! seconds into current simulation day

    ! local variables
    integer         :: rcode   ! return error code
    type(attrVect)  :: gData   ! global/gathered bundle data
    
    ! determine and open surface restart dataset
    integer, pointer  :: dof(:)
    integer :: nf_x2a, nf_a2x, lnx, dimid(2), k
    type(file_desc_t) :: file
    type(var_desc_t), pointer :: varid_x2a(:), varid_a2x(:)
    type(io_desc_t)    :: iodesc
    character(len=100) :: itemc     ! string converted to char
    type(string)       :: mstring   ! mct char type


    fname_srf_name =  interpret_filenmae_spec(rsfilename_spec_cam, yr_spec=yr_spec, mon_spec=mon_spec,&
                               day_spec=day_spec, sec_spec=sec_spec)
    call cam_pio_createfile(File, fname_srf_cam, 0)

    call gsMap_OrderedPoints(compInfo%gsMap, Dof)
    lnx =  gsMap_gsize(compInfo%gsMap)
    call pio_initdecomp(pio_subsystem, pio_double, (/lnx/), dof, iodesc)

    deallocate(dof)

    nf_x2a = attrVect_nRattr(x2a_a)
    allocate(varid_x2a(nf_x2a))
    
    rcode = pio_def_dim(File, 'x2a_nx', lnx, dimid(1))
    do k = 1, nf_x2a
        call attrVect_getRList(mstring, k, x2a_a)
        itemc = string_toChar(mstring)
        call string_clean(mstring)
        rcode = pio_def_var(File, 'x2a_'//trim(itemc), PIO_DOUBLE, dimid, varid_x2a(k))
        rcode = pio_put_att(File, varid_x2a(k), '_fillvalue', fillvalue)
    end do
    
    nf_a2x = attrVect_nRattr(a2x_a)
    allocate(varid_a2x(nf_a2x))

    rcode = pio_def_dim(File, 'a2x_nx', lnx, dimid(1))
    do k = 1, nf_a2x
        call attrVect_getRList(mstring, k, a2x_a)
        itemc = string_toChar(mstring)
        call string_clean(mstring)
        rcode = PIO_def_var(File, 'a2x_'//trim(itemc), PIO_DOUBLE,dimid, varid_a2x(k))
        rcode = PIO_put_att(File, varid_a2x(k), "_fillvalue", fillvalue)
    end do

    rcode = pio_enddef(File)

    do k = 1, nf_x2a
         call pio_write_darray(File, varid_x2a(k), iodesc, x2a_a%rattr(k,:), rcode)
    end do

    do k = 1, nf_a2x
         call pio_write_darray(File, varid_a2x(k), iodesc, a2x_a%rattr(k,:), rcode)
    end do

    deallocate(varid_x2a, varid_a2x)

    call pio_freedecomp(File, iodesc)
    call pio_closefile(File)

end module atm_comp_mct
