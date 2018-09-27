module baseCpl
use proc_def 
use comms_def
use procm, only: pm_init => init, clean
use comms
use timeM
use mct_mod
!use mrg_mod
!use fraction_mod
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
                if(iamroot_${name})then
                    write(*,*)'-------------${name} initiated-----------'
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
        call triger(EClock_${name}, ${name}_run)
        #end for
        call triger(EClock, stop_clock, "stop_clock")

        #for $subrt in $subrt_cfgs
$subrt
        #end for



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
