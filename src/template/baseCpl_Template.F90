module baseCpl
use proc_def 
use comms_def
use procm, only: pm_init => init, clean
use comms
use timeM
use mct_mod
use mrg_mod
use fraction_mod
#for $model in $proc_cfgs
     #set $name = $model.name
use comp_${name}
#end for

     implicit none
     type(proc), target :: my_proc

    ! Declare gsMap of each Model
    #for $model in $proc_cfgs
                #set $gms = $model.gsMaps
                #for $gm in $gms
                        #set $name = $gms[$gm].name
        type(gsMap) :: $name
                #end for
    #end for

    ! Declare AttrVect of each Model(c2x_cx,c2x_cc,x2c_cx,x2c_cc)
    #for $model in $proc_cfgs
                #set $avs = $model.attrVects
                #for $av in $avs
                        #set $name = $avs[$av].name
        type(AttrVect),pointer ::$name
                #end for
    #end for


    ! Declare Temp Merge AttrVect of each Model(m2x_nx)
    #for $cfg in $merge_cfgs
                #set $cfg = $merge_cfgs[$cfg]
                #for $mn_av in $cfg['dst']
                        #set $av_mx_nx = $mn_av['dst_av'].name
         type(AttrVect):: $av_mx_nx
                #end for
    #end for

    ! Declare Control Var
    #for $model in $proc_cfgs
         #set $name = $model.name
	 logical :: ${name}_run
    #end for

    #for $frac in $fraction_cfgs
    type(AttrVect) :: $frac
    #end for
    
     logical :: stop_clock
     type(clock) :: EClock
 
     public :: cpl_init
     public :: cpl_run
     public :: cpl_final

contains

subroutine cpl_init()
    implicit none
    integer :: ierr
    integer :: comm_rank
    call pm_init(my_proc)
    call clock_init(EClock)
    
    !-------------------------------------------------------------------
    ! !A in 0,1,gsize=8   B in 2,3,gsize=12   C in 2,3,gsize=16
    ! !Cpl in 0,1,2,3
    !-------------------------------------------------------------------

    !-------------------------------------------------------------------
    !  !Define Model_AV_MM 
    !-------------------------------------------------------------------
    
    #for $model in $proc_cfgs
                #set $avs = $model.attrVects
                #for $av in $avs
                        #set $name = $avs[$av].name
                $name=> my_proc%$name
                #end for
    #end for

    #for $frac in $fraction_cfgs
         #set $init = $fraction_cfgs[$frac].init
         #set $init_str = $init.toString($init.name, $init.argList)
         call $init_str
    #end for

    call MPI_Comm_rank(MPI_COMM_WORLD, comm_rank, ierr)


    !-------------------------------------------------------------------
    ! !Model Init
    !-------------------------------------------------------------------
       #for $cfg in $model_cfgs
                #set $name = $cfg['model_unique_name']
                #set $subroutine = $cfg['subroutine']
                #set $init_method = $subroutine['init_method']
                if(my_proc%iamin_model${name})then
                    $init_method.getFuncFormat()
                end if
       #end for

    
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
        write(*,*) '<<==== All Model Init Rank:', comm_rank, &
        " Over ====>>"
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
        write(*,*) ' '
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    

    !-------------------------------------------------------------------
    ! !Model_X gsmap_ext av_ext
    !-------------------------------------------------------------------
        #for $cfg in $model_cfgs
                #set $av_mx = $cfg['mx_av_set']
                #set $gm = $cfg['mx_gsmap_set']
                #set $name = $cfg['model_unique_name']
                #set $av_mx_mm = $av_mx['mx_mm']['name']
                #set $av_xm_mm = $av_mx['xm_mm']['name']
                #set $av_mx_mx = $av_mx['mx_mx']['name']
                #set $av_xm_mx = $av_mx['xm_mx']['name']
                #set $gm_mx = $gm['mx']['name']  
                #set $gm_mm = $gm['mm']['name']  
                if(my_proc%iamin_model${name}2cpl)then
                    call gsmap_init_ext(my_proc, $gm_mm, &
                                        my_proc%model${name}_id, &
                                        $gm_mx, my_proc%cplid, &
                                        my_proc%model${name}2cpl_id )

                    call avect_init_ext(my_proc, $av_mx_mm,&
                                        my_proc%model${name}_id, $av_mx_mx, &
                                        my_proc%cplid, $gm_mx, &
                                        my_proc%model${name}2cpl_id)

                    call avect_init_ext(my_proc, $av_xm_mm,&
                                        my_proc%model${name}_id, $av_xm_mx, &
                                        my_proc%cplid, $gm_mx, &
                                        my_proc%model${name}2cpl_id)
                    call mapper_rearrsplit_init(my_proc%mapper_C${name}2x, &   
                                                my_proc, $gm_mm, my_proc%model${name}_id, &
                                                $gm_mx, my_proc%cplid, &
                                                my_proc%model${name}2cpl_id, ierr)

                    call mapper_rearrsplit_init(my_proc%mapper_Cx2${name}, &
                                                my_proc, $gm_mx, my_proc%cplid, &
                                                $gm_mm, my_proc%model${name}_id, &
                                                my_proc%model${name}2cpl_id, ierr)

                    call MPI_Barrier(my_proc%mpi_model${name}2cpl, ierr)
                    call mapper_comp_map(my_proc%mapper_C${name}2x, &
                                         $av_mx_mm, $av_mx_mx, 100+10+1, ierr)
                end if
        #end for


    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    write(6,*) '<<========= Rank:',comm_rank,' Model-XInit End====>>'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
            write(*,*) ' '
    call MPI_Barrier(MPI_COMM_WORLD, ierr)


    if(my_proc%iamin_cpl) then

        #for $cfg in $merge_cfgs
                #set $name = $cfg
        #set $cfg = $merge_cfgs[$cfg]
                #set $av_mx_mx = $cfg['src']
                #set $gm_mx = $cfg['gm']
                #set $dst_info = $cfg['dst']
                #for $mn_av in $dst_info
                        #set $d_av = $mn_av['dst_av']
                        #set $av_mx_nx = $d_av.name
                        #set $gm_nx = $mn_av['dst_gm']
                        #set $dst_model_name = $mn_av['dst_model_name']
                        #set $mapper_name = $mn_av['dst_mapper']
                        #set $smat_size = $mn_av['smat_size']
        call avect_init_ext(my_proc, $av_mx_mx,&
                            my_proc%cplid, $av_mx_nx,&
                            my_proc%cplid, $gm_nx, &
                            my_proc%model${dst_model_name}2cpl_id)

        call mapper_spmat_init(my_proc,&
                               my_proc%${mapper_name}, &
                               my_proc%cplid, &
                               my_proc%${dst_model_name}_gsize, my_proc%${name}_gsize, &
                               $smat_size,&
                               $gm_mx, $gm_nx)

                #end for
        #end for
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        write(*,*) "<<=== Rank:" , comm_rank, &
            " lb2x_ax:", avect_lsize(b2x_ax),&
            " lc2x_ax:", avect_lsize(c2x_ax),&
            " la2x_bx:", avect_lsize(a2x_bx),&
            " lc2x_bx:", avect_lsize(c2x_bx),&
            " la2x_cx:", avect_lsize(a2x_cx),&
            " lb2x_cx:", avect_lsize(b2x_cx),&
            "===>>"
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        write(*,*) " "
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
    end if
    write(*,*)'<========= Init End  ===========>'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

end subroutine cpl_init

subroutine cpl_run()

    implicit none
    integer :: ierr,s,i,comm_rank
    
    call mpi_comm_rank(my_proc%comp_comm(my_proc%gloid), comm_rank, ierr)
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    write(*,*) '<<============== Rank:',comm_rank,' Begin Run==================>>'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
            write(*,*) ' '
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    s = 0

    call triger(EClock, stop_clock, "stop_clock")
    do while(.not. stop_clock)

        call clock_advance(EClock)
        #for $cfg in $model_cfgs
               #set $name = $cfg['model_unique_name']
        call triger(EClock, ${name}_run, "${name}_run")
        #end for
        call triger(EClock, stop_clock, "stop_clock")
        s = s+1
        if(s==10) stop_clock = .true.



        !------------------------------------------------------------
        !  Run phase 1 X2M_MX --> X2M_MM
        !  (M is Model, X is CPL)
        !------------------------------------------------------------


        #for $cfg in $model_cfgs
                #set $name = $cfg['model_unique_name']
                #set $run_method = $cfg['subroutine']['run_method']
                #set $run_phase1_method = $run_method['run_phase1_method']
        if(${name}_run)then
            if(my_proc%iamin_model${name}2cpl)then
                if(s == 3 .and. my_proc%iamin_modela2cpl) then
                    do i=1,avect_lsize(x2a_ax)
                        x2a_ax%rAttr(1,i) = x2a_ax%rAttr(1,i) + (comm_rank+1)*10+i
                    enddo
                endif
                if(s == 7 .and. my_proc%iamin_modelb2cpl) then
                    do i=1,avect_lsize(x2b_bx)
                        x2b_bx%rAttr(1,i) = x2b_bx%rAttr(1,i) + (comm_rank+1)*10+i
                    enddo
                endif
                
                $run_phase1_method.getFuncFormat()

                if(s == 3 .and. my_proc%iamin_modela2cpl) then
                    call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                    write(*,*) '<<===X2A_AA_VALUE Rank:',comm_rank, x2a_aa%rAttr(1,:)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                end if
            end if
        end if
        #end for

        call MPI_Barrier(MPI_COMM_WORLD, ierr)


        !------------------------------------------------------------
        !  Run phase 2, Model Run,  X2M_MM --> M2X_MM
        !  (M is Model, X is CPL)
        !------------------------------------------------------------

        #for $cfg in $model_cfgs
                #set $name = $cfg['model_unique_name']
                #set $run_method = $cfg['subroutine']['run_method']
                #set $run_phase2_method = $run_method['run_phase2_method']
        if(${name}_run)then
            if(my_proc%iamin_model${name})then
                $run_phase2_method.getFuncFormat()
            end if
        end if
        #end for

        call MPI_Barrier(MPI_COMM_WORLD, ierr)
                    write(*,*)
        call MPI_Barrier(MPI_COMM_WORLD, ierr)

        !------------------------------------------------------------
        !  Run phase 3
        !  For each Model:
        !  Step1: Rearrange, M2X_MM --> M2X_MX
        !  Step2: SparseMul With Other Model, M2X_MX --> M2X_BX
        !  (M is Model, X is CPL, B is Another Model)
        !------------------------------------------------------------
        !  For example:
        ! rearrage(a2x_aa,b2x_bb,c2x_cc) => (a2x_ax,b2x_bx,c2x_cx)
        ! sparse(a2x_ax b2x_bx c2x_cx) =>
        ! (a2x_bx,a2x_cx) (b2x_cx,b2x_a2) (c2x_ax,c2x_bx)
        !
   
       #for $cfg in $model_cfgs
                #set $name = $cfg['model_unique_name']
                #set $run_method = $cfg['subroutine']['run_method']
                #set $run_phase3_method = $run_method['run_phase3_method']
        if(${name}_run)then
            if(my_proc%iamin_model${name}2cpl)then
                $run_phase3_method.getFuncFormat()
            end if
        end if
        #end for

        !------------------------------------------------------------
        !  Run phase 4
        !  Merge (A2X_MX, B2X_MX, C2X_MX, M2X_MX)--> X2M_MX
        !  (M is Model, X is CPL, A,B,C is Another Model)
        !------------------------------------------------------------
        ! For Example:
        ! (c2x_ax,b2x_ax,a2x_ax) => (x2a_ax)
        ! (c2x_bx,b2x_bx,a2x_bx) => (x2b_bx)
        ! (c2x_cx,b2x_cx,a2x_cx) => (x2c_cx)
    if(my_proc%iamin_cpl) then
        if(s==10) then
            ! merge *2x_ax --> x2a_ax in rfield "x", cal the mean of all
            !call mapper_comp_avMerge(a2x_ax, b2x_ax, c2x_ax, x2a_ax, "x")
            call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                    write(*,*) '<<===X2A_AX_Merge_VALUE Rank:',comm_rank, x2a_ax%rAttr(1,:)
            #for $mgr_routine in $merge_subroutines
                #set func_str = $mgr_routine.toString($mgr_routine.name, $mgr_routine.argList)
                call ${mgr_routine.toString($mgr_routine.name,$mgr_routine.argList)}
            #end for
            call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
        endif
    endif

    end do

end subroutine cpl_run

subroutine cpl_final()

    implicit none

    !----------------------------------------------------------------------
    !     end component
    !----------------------------------------------------------------------
       #for $cfg in $model_cfgs
                #set $name = $cfg['model_unique_name']
                #set $subroutine = $cfg['subroutine']
                #set $final_method = $subroutine['final_method']
    if(my_proc%iamin_model${name})then
                #for $method in $final_method
                        #set $final_method_name = $method['method_name']
                        #set $params = $method['params']
                        #set $args = []
                        #for $key in $params
                                #set item = $key + '=' + $params[$key]
                                $args.append(str(item)) 
                        #end for
                        #set $args = ",".join(args)
        call ${final_method_name}(${args})
                #end for
    end if
       #end for
    call clean(my_proc)

end subroutine cpl_final

end module baseCpl
