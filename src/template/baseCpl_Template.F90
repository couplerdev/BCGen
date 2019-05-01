module baseCpl
use mpi
use shr_kind_mod
use shr_sys_mod
!use base_sys
use logUtil
use shr_mpi_mod
!use base_mpi
use proc_def
use comms_def
use global_var
use procm, only: pm_init => init, clean
use comms
use time_mod
use ESMF
use flux_mod
use mct_mod
use mrg_mod
#for $model in $proc_cfgs
     #set $name = $model.name
use comp_${name}
#end for
#if $conf_cfgs['rest']
use base_rest_mod
#end if
#if $conf_cfgs['hist']
use base_hist_mod
#end if
use base_domain_mod
use base_io
use fraction_mod
    implicit none
    !type(Meta),  pointer :: metaData

    !declare gsMap for each Model
    #for $model in $proc_cfgs
         #set $gms = $model.gsMaps
         #set $cpl_name = $gms['cpl'].name
         #set $comp_name = $gms['comp'].name
    type(mct_gsMap), pointer ::$comp_name
    type(mct_gsMap)          :: $cpl_name
    #end for    

    ! declare AttrVect of each Model(c2x_cx, c2x_cc, x2c_cx, x2c_cc)
    #for $model in $proc_cfgs
         #set $avs = $model.attrVects
         #for $av in $avs
              #set $name = $avs[$av].name
    type(mct_aVect), pointer :: $name
         #end for
    #end for
   
    ! declare xao attrVect if xao is defined
    #for $fake in $fake_cfgs
        #set $fakeModel = $fake_cfgs[$fake]
        #for $var in $fakeModel.variables
    type(mct_aVect) :: $var
        #end for
    #end for
    
   ! declare Temp Merge AttrVect of each Model(m2x_nx) 
    #for $cfg in $merge_cfgs
    type(mct_aVect) ::$cfg
    #end for
    #for $model in $proc_cfgs
         #set $domainm = $model.domain['m']
         #set $domainx = $model.domain['x']
    type(mct_gGrid),  pointer :: $domainm
    type(mct_gGrid),  pointer :: $domainx
    #end for

    type AreaCorrectFactor
        real(r8), pointer :: drv2mdl(:), mdl2drv(:)
    end type AreaCorrectFactor

    #for $frac in $fraction_cfgs
    type(mct_aVect)  :: $frac
    #end for

    #for $model in $proc_cfgs
        #set $grid = $model.name
    type(AreaCorrectFactor) :: areacor_${grid}
    #end for     

    #for $model in $proc_cfgs
         #set $name = $model.name
    logical ::${name}_run
    #end for

    #for $model in $proc_cfgs
         #set $name = $model.name
    type(ESMF_Clock), pointer   :: EClock_${name}
    #end for
    type(ESMF_Clock), pointer   :: EClock_drv

    type(timeManager), target :: SyncClock
    logical :: restart_alarm
    logical :: history_alarm
    logical :: histavg_alarm
    logical :: stop_alarm
    #for $model in $proc_cfgs
         #set $name = $model.name
    logical :: ${name}run_alarm
    #end for
    public  :: cpl_init  
    public  :: cpl_run
    public  :: cpl_final

    #if $conf_cfgs['rest']==True
    character(SHR_KIND_CL) :: rest_file = $conf_cfgs['rest_file']     
    #end if
    #if $conf_cfgs['hist']
    character(SHR_KIND_CL) :: hist_file = $conf_cfgs['hist_file']
    #end if


contains 

subroutine cpl_init()
    implicit none
    integer :: ierr
    integer :: comm_rank
    logical :: restart
    character(SHR_KIND_CL)  :: nmlfile
    character(SHR_KIND_CL)  :: restart_file
    logical :: iamroot
    integer :: lsize
    integer :: rc
    
    call pm_init(metaData)
    call confMeta_getInfo(metaData%conf, nmlfile=nmlfile, restart=restart, &
         restart_file=restart_file)
    call time_ClockInit(SyncClock, nmlfile,MPI_COMM_WORLD, EClock_drv, &
    #for $model in $proc_cfgs
         #set $name = $model.name
         EClock_${name}, &
    #end for
         restart, restart_file, time_cal_default)
    call procMeta_getInfo(metaData%my_proc,ID=GLOID, iamroot=iamroot)

    !-------------------------------------------------------------
    !   Define Model_AV_MM
    !-------------------------------------------------------------

    #for $model in $proc_cfgs
         #set $avs = $model.attrVects
         #for $av in $avs
              #set $name = $avs[$av].name
    $name => metaData%$name
         #end for
    #end for

    #for $model in $proc_cfgs
         #set $gm = $model.gsMaps["comp"].name
         #set $domainm = $model.domain['m']
         #set $domainx = $model.domain['x']
         #set $model_name = $model.name
    ${domainm} =>   metaData%${model_name}%domain
    ${domainx} =>   metaData%domain_${model_name}x
    $gm => metaData%${model_name}%comp_gsMap
    #end for
     
    !-------------------------------------------------------
    ! Model init
    !-------------------------------------------------------
    #for $model_name in $model_cfgs
         #set $cfg = $model_cfgs[$model_name]
         #set $name = $cfg['model_unique_name']
         #set $subroutine  = $cfg['subroutine']
         #set $init_method = $subroutine['init_method']
    if(metaData%iamin_model${name})then
         $init_method.getStrFormat()
    end if
    #end for

    call MPI_Barrier(MPI_COMM_WORLD, ierr)
        if(iamroot)then
            write(logUnit, *) '-------------All Model init rank', comm_rank
        end if
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    !--------------------------------------------------------
    !  Model_x gsmap_ext av_ext
    !--------------------------------------------------------
    #for $model_name in $model_cfgs
        #set $cfg = $model_cfgs[$model_name]
        #set $av_mx = $cfg['mx_av_set']
        #set $gm = $cfg['mx_gsmap_set']
        #set $dom = $cfg['domain']
        #set $name = $cfg['model_unique_name']
        #set $av_mx_mm = $av_mx['mx_mm']['name']
        #set $av_xm_mm = $av_mx['xm_mm']['name']
        #set $av_mx_mx = $av_mx['mx_mx']['name']
        #set $av_xm_mx = $av_mx['xm_mx']['name']
        #set $gm_mx = $gm['mx']['name']
        #set $gm_mm = $gm['mm']['name']
        #set $dom_mx= $dom['mx']
        #set $dom_mm= $dom['mm'] 
  
    if(metaData%iamin_model${name}2cpl)then
        call gsmap_init_ext(metaData, $gm_mm, metaData%model${name}_id, &
                        $gm_mx, metaData%cplid, metaData%model${name}2cpl_id)

        call avect_init_ext(metaData, $av_mx_mm, metaData%model${name}_id, &
                        $av_mx_mx, metaData%cplid, $gm_mx, & 
                        metaData%model${name}2cpl_id)
        
        call avect_init_ext(metaData, $av_xm_mm, metaData%model${name}_id, &
                        $av_xm_mx, metaData%cplid, $gm_mx, &
                        metaData%model${name}2cpl_id)  

        call mapper_rearrsplit_init(metaData%mapper_C${name}2x, metaData, $gm_mm,&
                         metaData%model${name}_id, $gm_mx, metaData%cplid, &
                         metaData%model${name}2cpl_id, ierr) 
        call mapper_rearrsplit_init(metaData%mapper_Cx2${name}, metaData, $gm_mx,&
                         metaData%cplid, $gm_mm, metaData%model${name}_id, &
                         metaData%model${name}2cpl_id, ierr)
        call MPI_Barrier(metaData%mpi_model${name}2cpl, ierr)
        call mapper_comp_map(metaData%mapper_C${name}2x, $av_mx_mm, $av_mx_mx, msgtag=100+10+1, ierr=ierr)
        call mapper_comp_map(metaData%mapper_C${name}2x, $av_xm_mm, $av_xm_mx, msgtag=100+10+1, ierr=ierr)
        call gGrid_init_ext(metaData, $dom_mm, metaData%model${name}_id, $dom_mx,&
                   metaData%cplid, $gm_mx, metaData%model${name}2cpl_id)
        call mapper_comp_map(metaData%mapper_C${name}2x, $dom_mm%data, $dom_mx%data, msgtag=100+10+1,ierr=ierr)
    end if
    
    if(metaData%iamroot_model${name})then
        write(logUnit, *)'---------------${name} initiated-------------'
    end if
#end for

    if(metaData%iamin_cpl)then
        #for $cfg in $merge_cfgs
            #set $av = $merge_cfgs[$cfg]
            #set $gm = $av['gm']
            #set $gn = $av['gn']
            #set $dst = $av['dst']
            #set $src = $av['src']
        lsize = mct_gsmap_lsize($gn, metaData%mpi_model${dst}2cpl) 
        call mct_aVect_init($cfg, rList=metaData%flds_${src}2x_states, lsize=lsize)
        call mct_aVect_zero($cfg)
        #end for
    end if

    if(metaData%iamin_cpl)then
        #for $cfg in $fake_cfgs
            #set $fake_model = $fake_cfgs[$cfg]
            #set $fld = $fake_model.flds[2]
            #for $var in $fake_model.variables
                #set $fake_av = $fake_model.variables[$var]
                #set $grid = $fake_av.grid
                #set $model = $model_cfgs[$grid]
                #set $gm = $model['mx_gsmap_set']
                #set $gx = $gm['mx']['name']
        lsize = mct_gsmap_lsize($gx, metaData%mpi_model${$grid}2cpl) 
        call mct_aVect_init($var, rList=metaData%$(fld), lsize=lsize) 
        call mct_aVect_zero($var)  
            #end for
        #end for
    end if
    
    if(metaData%iamin_cpl)then
        #for $cfg in $merge_cfgs
            #set $av = $merge_cfgs[$cfg]
            #set $mapper_name = $av['mapperName']
            #set $gm_mx = $av['gm']
            #set $gm_nx = $av['gn']
            #set $same_grid = $av['samegrid']
        call mapper_spmat_init_rc(metaData%${mapper_name}, $gm_mx, $gm_nx, metaData%mpi_cpl,metaData%datarc, &
                           '${mapper_name}', 'X', samegrid=$same_grid)   
        #end for
    end if
    
    #for $fracName in $fraction_cfgs 
         #set $frac = $fraction_cfgs[$fracName]
         #set $namearg = $frac.init.name
         #set $arglist = $frac.init.argList
    call $frac.init.toString($namearg, $arglist)
    #end for   

    
    #for $fracName in $fraction_cfgs
        #set $frac = $fraction_cfgs[$fracName]
        #for $mapper in $frac.mappers
        #set $subrt = $mapper.mapSubroutine
    call $subrt.toString($subrt.name, $subrt.argList)
        #end for
    #end for

    #for $fake in $fake_cfgs:
        #set $fakeModel = $fake_cfgs[$fake]
        #set $vars = $fakeModel.variables
        #for $varName in  $vars
            #set $var = $vars[$varName]
            #set $init = $var.method['init'][0]
    call $init.toString($init.name, $init.argList) 
        #end for
    #end for


    ! areacorr init
    #for $model_name in $model_cfgs
        #set $model = $model_cfgs[$model_name]
        #set $grid = $model['model_unique_name']
        #set $av_mx = $model['mx_av_set']
        #set $gm = $model['mx_gsmap_set']['mm']
        #set $av_mx_mm = $av_mx['mx_mm']['name']
        #set $av_mx_mx = $av_mx['mx_mx']['name']
     
        if(metaData%iamin_model${grid})then
        call mapper_comp_map(metaData%mapper_Cx2${grid},domain_${grid}x%data, &
                             domain_${grid}${grid}%data, msgtag=100+10+1 )
        call domain_areafactinit(metaData%${grid}, areacor_$grid%mdl2drv, areacor_$grid%drv2mdl)
        call mct_avect_vecmult($av_mx_mm, areacor_$grid%mdl2drv, metaData%flds_${grid}2x_fluxes)
        end if
        call mapper_comp_map(metaData%mapper_C${grid}2x,$av_mx_mm, $av_mx_mx, msgtag=100+10+1)
        call MPI_Barrier(MPI_COMM_WORLD, ierr)        

    #end for

    ! read driver restart file
    #if $conf_cfgs['rest']
    if(restart)call base_rest_read(metaData, rest_file)
    #end if

    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    write(logUnit, *)'---------------Init End------------'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

end subroutine cpl_init

subroutine cpl_run()

    implicit none
    integer  :: ierr, s, i, comm_rank
    character(*), parameter :: subname = "(cpl_run)"
    logical :: stop_alarm = .false.
    logical :: hist_run = .false.
    logical :: histavg_run = .false.
    logical :: restart_run = .false.
    #for $model in $proc_cfgs
         #set $name = $model.name
    logical :: ${name}_run
    #end for


    call base_io_init()
    call mpi_comm_rank(MPI_COMM_WORLD, comm_rank, ierr)
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    write(*,*)'----------rank:', comm_rank, ' begin run------------'
    do while(.not. stop_alarm)
        call time_clockAdvance(SyncClock)
        stop_alarm = time_alarmIsOn(SyncClock%ECP(clock_drv)%EClock, alarm_stop_name)
        hist_run = time_alarmIsOn(SyncClock%ECP(clock_drv)%EClock, alarm_history_name)
        histavg_run = time_alarmIsOn(SyncClock%ECP(clock_drv)%EClock, alarm_histavg_name)
        restart_run = time_alarmIsOn(SyncClock%ECP(clock_drv)%EClock, alarm_restart_name)
        #for $model in $proc_cfgs
             #set $name = $model.name
        ${name}_run = time_alarmIsOn(SyncClock%ECP(clock_drv)%EClock, alarm_${name}run_name)
        #end for
        if(time_alarmIsOn(EClock_drv, alarm_datestop_name))then
            if(metaData%iamroot_cpl)then
                write(logUnit,*)' '
                write(logUnit,*)subname,'Note: Stopping from alarm date stop'
                write(logUnit,*)' '
            end if
            stop_alarm = .true.
        end if
        #for subrt in $subrt_cfgs
        $subrt
        #end for
    end do
    

end subroutine cpl_run

subroutine cpl_final()

    implicit none
    integer :: ierr
    !----------------------------------------
    !     end component
    !----------------------------------------
    #for $model_name in $model_cfgs
         #set $cfg = $model_cfgs[$model_name]
         #set $name = $cfg['model_unique_name']
         #set $subroutine = $cfg['subroutine']
         #set $final_method = $subroutine['final_method']
    if(metaData%iamin_model${name})then
         #for method in $final_method
              #set $final_method_name = $method['method_name']
              #set $params = $method['params']
              #set $args  = []
              #for $key in $params
                   #set item = $key + '=' + $params[$key]
                   $args.append(str(item))
              #end for
              #set $args = ",".join(args)
          call ${final_method_name}($args)
          #end for
    end if

    #end for

    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    print *,'========== end of coupled climate models =========='
    call MPI_Finalize(ierr)

end subroutine cpl_final

end module baseCpl
