module baseCpl
use proc_def
use comms_def
use procm, only: pm_init => init, clean
use comms
use timeM
use mct_mod
use comp_atm
use comp_ocn

    implicit none
    type(Meta),  pointer :: metaData

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

    type(EClock)   :: EClock_atm
    type(EClock)   :: EClock_ocn
    type(EClock)   :: EClock_drv

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
    
    call pm_init(metaData)
    call confMeta_getInfo(metaData%conf, nmlfile=nmlfile, restart=restart,
restart_file=restart_file)
    call time_ClockInit(SyncClock, nmlfile, restart, restart_file,
time_cal_noleap)
    call procMeta_getInfo(metaData%my_proc,ID=GLOID, iamroot=iamroot)

    !-------------------------------------------------------------
    !   Define Model_AV_MM
    !-------------------------------------------------------------

    atm2x_atmatm => metaData%name
    atm2x_atmx => metaData%name
    x2atm_atmatm => metaData%name
    x2atm_atmx => metaData%name
    ocn2x_ocnocn => metaData%name
    ocn2x_ocnx => metaData%name
    x2ocn_ocnocn => metaData%name
    x2ocn_ocnx => metaData%name

    domain_atm =>   metaData%model_atm%domain
    gsMap_atmatm => metaData%model_atm%gsMap
    domain_ocn =>   metaData%model_ocn%domain
    gsMap_ocnocn => metaData%model_ocn%gsMap
     
    !-------------------------------------------------------
    ! Model init
    !-------------------------------------------------------
    if(metaData%iamin_model{name})then
         call atm_init_mct(my_proc%model_atm, EClock, x2atm_atmatm, atm2x_atmatm, ierr=ierr)
    end if
    if(metaData%iamin_model{name})then
         call ocn_init_mct(my_proc%model_ocn, EClock, x2ocn_ocnocn, ocn2x_ocnocn, ierr=ierr)
    end if

    call MPI_Barrier(MPI_COMM_WORLD, ierr)
        if(iamroot)then
            write(logUnit, *) '-------------All Model init rank', comm_rank
        end if
    call MPI_Barrier(MPI_COMM_WORLD)

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
                         metaData%cpl_id, gsMap_atmatm, metaData%modelatm_id, &
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
                         metaData%cpl_id, gsMap_ocnocn, metaData%modelocn_id, &
                         metaData%modelocn2cpl_id, ierr)
        call MPI_Barrier(metaData%mpi_modelocn2cpl, ierr)
        call mapper_comp_map(metaData%mapper_Cocn2x, ocn2x_ocnocn, ocn2x_ocnx, 100+10+1, ierr)
    end if
    
    if(metaData%iamroot_modelocn)then
        write(logUnit, *)'---------------ocn initiated-------------'
    end if

    if(metaData%iamin_cpl)then
        call avect_init_ext(metaData, atm2x_atmx, metaData%cplid, &
                           atm2x_ocnx, metaData%cpplid, gsMap_ocnx, &
                           metaData%modelocn2cpl_id)
        call mapper_spmat_init(metaData%mapper_Smatatm2ocn, gsMap_ocnx, gsMap_ocnx, mpicom, &
                           "map_fv4x5_to_gx3v7_bilin_da_091218.nc", 'X')   
        call avect_init_ext(metaData, ocn2x_ocnx, metaData%cplid, &
                           ocn2x_atmx, metaData%cpplid, gsMap_atmx, &
                           metaData%modelatm2cpl_id)
        call mapper_spmat_init(metaData%mapper_Smatocn2atm, gsMap_atmx, gsMap_atmx, mpicom, &
                           "map_fv4x5_to_gx3v7_aave_da_091218.nc", 'X')   
    end if

    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    write(logUnit, *)'---------------Init End------------'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

end subroutine cpl_init

subroutine cpl_run()

    implicit none
    integer  :: ierr, s, i, comm_rank

    call mpi_comm_rank(MPI_COMM_WORLD, comm_rank)
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    write(*,*)'----------rank:', comm_rank, ' begin run------------'
    do while(.not. stop_clock)
        call time_clockAdvance(SyncClock)
        stop_alarm = time_alarmIsOn(clock_drv, alarm_stop)
        history_alarm = time_alarmIsOn(clock_drv, alarm_history)
        histavg_alarm = time_alarmIsOn(clock_drv, alarm_histavg)
        restart_alarm = time_alarmIsOn(clock_drv, alarm_restart)
        atmrun_alarm = time_alarmIsOn(clock_drv, alarm_atmrun)
        atmrun_alarm = time_alarmIsOn(clock_drv, alarm_atmrun)
        ocnrun_alarm = time_alarmIsOn(clock_drv, alarm_ocnrun)
        ocnrun_alarm = time_alarmIsOn(clock_drv, alarm_ocnrun)
        if(time_alarmIsOn(EClock_drv, alarm_datestop))
            if(metaData%iamroot_cpl)then
                write(logUnit,*)' '
                write(logUnit,*)subname,'Note: Stopping from alarm date stop'
                write(logUnit,*)' '
            end if
            stop_alarm = .true.
        end if
            if(atm_run)then
        if(my_proc%iamin_modelatm2cpl)then
            call mapper_comp_name(my_proc%Mapper_Catm2x, x2atm_atmx, x2atm_atmatm, msg_tag, ierr=ierr)
        end if
    end if


            if(ocn_run)then
        if(my_proc%iamin_modelocn2cpl)then
            call mapper_comp_name(my_proc%Mapper_Cocn2x, x2ocn_ocnx, x2ocn_ocnocn, msg_tag, ierr=ierr)
        end if
    end if


            if(atm_run)then
        if(my_proc%iamin_modelatm)then
            call atm_run_mct(my_proc%model_atm, EClock, x2atm_atmatm, atm2x_atmatm, ierr=ierr)
        end if
    end if


            if(ocn_run)then
        if(my_proc%iamin_modelocn)then
            call ocn_run_mct(my_proc%model_ocn, EClock, x2ocn_ocnocn, ocn2x_ocnocn, ierr=ierr)
        end if
    end if


            if(atm_run)then
        if(my_proc%iamin_modelatm2cpl)then
            call mapper_comp_name(my_proc%Mapper_Catm2x, x2atm_atmx, x2atm_atmatm, msg_tag, ierr=ierr)
        end if
    end if


            if(ocn_run)then
        if(my_proc%iamin_modelocn2cpl)then
            call mapper_comp_name(my_proc%Mapper_Cocn2x, x2ocn_ocnx, x2ocn_ocnocn, msg_tag, ierr=ierr)
        end if
    end if


            if(atm_run)then
        if(my_proc%iamin_modelatm2cpl)then
            call mapper_comp_comm(my_proc%mapper_Smatocn2atm, ocn2x_ocnx, ocn2x_atmx, 103, field="So_t:So_s:So_u", ierr=ierr)
        end if
    end if


            if(ocn_run)then
        if(my_proc%iamin_modelocn2cpl)then
            call mapper_comp_comm(my_proc%mapper_Smatatm2ocn, atm2x_atmx, atm2x_ocnx, 103, field="Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem", ierr=ierr)
        end if
    end if


            if(ocn_run)then
        if(my_proc%iamin_modelocn2cpl)then
            call mrg_x2ocn(my_proc, atm2x_ocnx, ocn2x_ocnx, x2ocn_ocnx)
        end if
    end if


            if(atm_run)then
        if(my_proc%iamin_modelatm2cpl)then
            call mrg_x2atm(my_proc, ocn2x_atmx, atm2x_atmx, x2atm_atmx)
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
