module baseCpl
use mpi
use base_sys
use logUtil
use base_mpi
use proc_def
use comms_def
use global_var
use procm, only: pm_init => init, clean
use comms
use time_mod
use ESMF
use mct_mod
use mrg_mod
use comp_atm
use comp_ocn

    implicit none
    !type(Meta),  pointer :: metaData

    !declare gsMap for each Model
    type(gsMap), pointer ::gsMap_atmatm
    type(gsMap), pointer ::gsMap_atmx
    type(gsMap), pointer ::gsMap_ocnocn
    type(gsMap), pointer ::gsMap_ocnx

    ! declare AttrVect of each Model(c2x_cx, c2x_cc, x2c_cx, x2c_cc)
    type(AttrVect), pointer :: atm2x_atmatm
    type(AttrVect), pointer :: atm2x_atmx
    type(AttrVect), pointer :: x2atm_atmatm
    type(AttrVect), pointer :: x2atm_atmx
    type(AttrVect), pointer :: ocn2x_ocnocn
    type(AttrVect), pointer :: ocn2x_ocnx
    type(AttrVect), pointer :: x2ocn_ocnocn
    type(AttrVect), pointer :: x2ocn_ocnx

    ! declare Temp Merge AttrVect of each Model(m2x_nx) 
    type(AttrVect) ::atm2x_ocnx
    type(AttrVect) ::ocn2x_atmx

    type(gGrid),  pointer :: domain_atm
    type(gGrid),  pointer :: domain_ocn

    logical ::atm_run
    logical ::ocn_run

    type(ESMF_Clock), pointer   :: EClock_atm
    type(ESMF_Clock), pointer   :: EClock_ocn
    type(ESMF_Clock), pointer   :: EClock_drv

    type(timeManager), target :: SyncClock
    logical :: restart_alarm
    logical :: history_alarm
    logical :: histavg_alarm
    logical :: stop_alarm
    logical :: atmrun_alarm
    logical :: ocnrun_alarm
    public  :: cpl_init  
    public  :: cpl_run
    public  :: cpl_final

contains 

subroutine cpl_init()
    implicit none
    integer :: ierr
    integer :: comm_rank
    logical :: restart
    character(len=64)  :: nmlfile
    character(len=64)  :: restart_file
    logical :: iamroot
    integer :: rc
    
    call pm_init(metaData)
    call confMeta_getInfo(metaData%conf, nmlfile=nmlfile, restart=restart, &
         restart_file=restart_file)
    call time_ClockInit(SyncClock, nmlfile,MPI_COMM_WORLD, EClock_drv, &
         EClock_atm, &
         EClock_ocn, &
         restart, restart_file, time_cal_default)
    call procMeta_getInfo(metaData%my_proc,ID=GLOID, iamroot=iamroot)

    !-------------------------------------------------------------
    !   Define Model_AV_MM
    !-------------------------------------------------------------

    atm2x_atmatm => metaData%atm2x_atmatm
    atm2x_atmx => metaData%atm2x_atmx
    x2atm_atmatm => metaData%x2atm_atmatm
    x2atm_atmx => metaData%x2atm_atmx
    ocn2x_ocnocn => metaData%ocn2x_ocnocn
    ocn2x_ocnx => metaData%ocn2x_ocnx
    x2ocn_ocnocn => metaData%x2ocn_ocnocn
    x2ocn_ocnx => metaData%x2ocn_ocnx

    domain_atm =>   metaData%atm%domain
    gsMap_atmatm => metaData%atm%comp_gsMap
    domain_ocn =>   metaData%ocn%domain
    gsMap_ocnocn => metaData%ocn%comp_gsMap
     
    !-------------------------------------------------------
    ! Model init
    !-------------------------------------------------------
    if(metaData%iamin_modelatm)then
         call atm_init_mct(metaData%atm, EClock_atm, x2atm_atmatm, atm2x_atmatm, ierr=ierr)
    end if
    if(metaData%iamin_modelocn)then
         call ocn_init_mct(metaData%ocn, EClock_ocn, x2ocn_ocnocn, ocn2x_ocnocn, ierr=ierr)
    end if

    call MPI_Barrier(MPI_COMM_WORLD, ierr)
        if(iamroot)then
            write(logUnit, *) '-------------All Model init rank', comm_rank
        end if
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    !--------------------------------------------------------
    !  Model_x gsmap_ext av_ext
    !--------------------------------------------------------
    if(metaData%iamin_modelatm2cpl)then
        call gsmap_init_ext(metaData, gsMap_atmatm, metaData%modelatm_id, &
                        gsMap_atmx, metaData%cplid, metaData%modelatm2cpl_id)

        call avect_init_ext(metaData, atm2x_atmatm, metaData%modelatm_id, &
                        atm2x_atmx, metaData%cplid, gsMap_atmx, & 
                        metaData%modelatm2cpl_id)
        
        call avect_init_ext(metaData, x2atm_atmatm, metaData%modelatm_id, &
                        x2atm_atmx, metaData%cplid, gsMap_atmx, &
                        metaData%modelatm2cpl_id)  

        call mapper_rearrsplit_init(metaData%mapper_Catm2x, metaData, gsMap_atmatm,&
                         metaData%modelatm_id, gsMap_atmx, metaData%cplid, &
                         metaData%modelatm2cpl_id, ierr) 
        call mapper_rearrsplit_init(metaData%mapper_Cx2atm, metaData, gsMap_atmx,&
                         metaData%cplid, gsMap_atmatm, metaData%modelatm_id, &
                         metaData%modelatm2cpl_id, ierr)
        call MPI_Barrier(metaData%mpi_modelatm2cpl, ierr)
        call mapper_comp_map(metaData%mapper_Catm2x, atm2x_atmatm, atm2x_atmx, 100+10+1, ierr)
    end if
    
    if(metaData%iamroot_modelatm)then
        write(logUnit, *)'---------------atm initiated-------------'
    end if
    if(metaData%iamin_modelocn2cpl)then
        call gsmap_init_ext(metaData, gsMap_ocnocn, metaData%modelocn_id, &
                        gsMap_ocnx, metaData%cplid, metaData%modelocn2cpl_id)

        call avect_init_ext(metaData, ocn2x_ocnocn, metaData%modelocn_id, &
                        ocn2x_ocnx, metaData%cplid, gsMap_ocnx, & 
                        metaData%modelocn2cpl_id)
        
        call avect_init_ext(metaData, x2ocn_ocnocn, metaData%modelocn_id, &
                        x2ocn_ocnx, metaData%cplid, gsMap_ocnx, &
                        metaData%modelocn2cpl_id)  

        call mapper_rearrsplit_init(metaData%mapper_Cocn2x, metaData, gsMap_ocnocn,&
                         metaData%modelocn_id, gsMap_ocnx, metaData%cplid, &
                         metaData%modelocn2cpl_id, ierr) 
        call mapper_rearrsplit_init(metaData%mapper_Cx2ocn, metaData, gsMap_ocnx,&
                         metaData%cplid, gsMap_ocnocn, metaData%modelocn_id, &
                         metaData%modelocn2cpl_id, ierr)
        call MPI_Barrier(metaData%mpi_modelocn2cpl, ierr)
        call mapper_comp_map(metaData%mapper_Cocn2x, ocn2x_ocnocn, ocn2x_ocnx, 100+10+1, ierr)
    end if
    
    if(metaData%iamroot_modelocn)then
        write(logUnit, *)'---------------ocn initiated-------------'
    end if

    if(metaData%iamin_cpl)then
        call avect_init_ext(metaData, atm2x_atmx, metaData%cplid, &
                           atm2x_ocnx, metaData%cplid, gsMap_ocnx, &
                           metaData%modelocn2cpl_id)
        call mapper_spmat_init(metaData%mapper_Smatatm2ocn, gsMap_ocnx, gsMap_ocnx, metaData%mpi_cpl, &
                           "map_fv4x5_to_gx3v7_bilin_da_091218.nc", 'X')   
        call avect_init_ext(metaData, ocn2x_ocnx, metaData%cplid, &
                           ocn2x_atmx, metaData%cplid, gsMap_atmx, &
                           metaData%modelatm2cpl_id)
        call mapper_spmat_init(metaData%mapper_Smatocn2atm, gsMap_atmx, gsMap_atmx, metaData%mpi_cpl, &
                           "map_fv4x5_to_gx3v7_aave_da_091218.nc", 'X')   
    end if

    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    write(logUnit, *)'---------------Init End------------'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

end subroutine cpl_init

subroutine cpl_run()

    implicit none
    integer  :: ierr, s, i, comm_rank
    character(*), parameter :: subname = "(cpl_run)"
    logical :: stop_alarm = .false.
    logical :: history_alarm = .false.
    logical :: histavg_alarm = .false.
    logical :: restart_alarm = .false.
    logical :: atmrun_alarm
    logical :: ocnrun_alarm

    call mpi_comm_rank(MPI_COMM_WORLD, comm_rank, ierr)
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    write(*,*)'----------rank:', comm_rank, ' begin run------------'
    do while(.not. stop_alarm)
        call time_clockAdvance(SyncClock)
        stop_alarm = time_alarmIsOn(SyncClock%ECP(clock_drv)%EClock, alarm_stop_name)
        history_alarm = time_alarmIsOn(SyncClock%ECP(clock_drv)%EClock, alarm_history_name)
        histavg_alarm = time_alarmIsOn(SyncClock%ECP(clock_drv)%EClock, alarm_histavg_name)
        restart_alarm = time_alarmIsOn(SyncClock%ECP(clock_drv)%EClock, alarm_restart_name)
        atmrun_alarm = time_alarmIsOn(SyncClock%ECP(clock_drv)%EClock, alarm_atmrun_name)
        ocnrun_alarm = time_alarmIsOn(SyncClock%ECP(clock_drv)%EClock, alarm_ocnrun_name)
        if(time_alarmIsOn(EClock_drv, alarm_datestop_name))then
            if(metaData%iamroot_cpl)then
                write(logUnit,*)' '
                write(logUnit,*)subname,'Note: Stopping from alarm date stop'
                write(logUnit,*)' '
            end if
            stop_alarm = .true.
        end if
            if(atm_run)then
        if(metaData%iamin_modelatm2cpl)then
            call mapper_comp_map(metaData%Mapper_Catm2x, x2atm_atmx, x2atm_atmatm, msgtag=103, ierr=ierr)
        end if
    end if


            if(ocn_run)then
        if(metaData%iamin_modelocn2cpl)then
            call mapper_comp_map(metaData%Mapper_Cocn2x, x2ocn_ocnx, x2ocn_ocnocn, msgtag=103, ierr=ierr)
        end if
    end if


            if(atm_run)then
        if(metaData%iamin_modelatm)then
            call atm_run_mct(metaData%atm, EClock_atm, x2atm_atmatm, atm2x_atmatm, ierr=ierr)
        end if
    end if


            if(ocn_run)then
        if(metaData%iamin_modelocn)then
            call ocn_run_mct(metaData%ocn, EClock_ocn, x2ocn_ocnocn, ocn2x_ocnocn, ierr=ierr)
        end if
    end if


            if(atm_run)then
        if(metaData%iamin_modelatm2cpl)then
            call mapper_comp_map(metaData%Mapper_Catm2x, x2atm_atmx, x2atm_atmatm, msgtag=103, ierr=ierr)
        end if
    end if


            if(ocn_run)then
        if(metaData%iamin_modelocn2cpl)then
            call mapper_comp_map(metaData%Mapper_Cocn2x, x2ocn_ocnx, x2ocn_ocnocn, msgtag=103, ierr=ierr)
        end if
    end if


            if(atm_run)then
        if(metaData%iamin_modelatm2cpl)then
            call mapper_comp_map(metaData%mapper_Smatocn2atm, ocn2x_ocnx, ocn2x_atmx, msgtag=103, field="So_t:&
                 So_s:So_u", ierr=ierr)
        end if
    end if


            if(ocn_run)then
        if(metaData%iamin_modelocn2cpl)then
            call mapper_comp_map(metaData%mapper_Smatatm2ocn, atm2x_atmx, atm2x_ocnx, msgtag=103, field="Sa_z:&
                 Sa_u:Sa_v:Sa_tbot:Sa_ptem", ierr=ierr)
        end if
    end if


            if(ocn_run)then
        if(metaData%iamin_modelocn2cpl)then
            call mrg_x2ocn(metaData, atm2x_ocnx, ocn2x_ocnx, x2ocn_ocnx)
        end if
    end if


            if(atm_run)then
        if(metaData%iamin_modelatm2cpl)then
            call mrg_x2atm(metaData, ocn2x_atmx, atm2x_atmx, x2atm_atmx)
        end if
    end if


    end do
    

end subroutine cpl_run

subroutine cpl_final()

    implicit none

    !----------------------------------------
    !     end component
    !----------------------------------------
    if(metaData%iamin_modelatm)then
          call atm_final_mct()
    end if

    if(metaData%iamin_modelocn)then
          call ocn_final_mct()
    end if


end subroutine cpl_final

end module baseCpl
