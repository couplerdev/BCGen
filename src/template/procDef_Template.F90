module proc_def
use mct_mod
use comms_def
    implicit none
!include "mpif.h"

    type proc
        !-----------------------------------------------
        ! Meta desc of proc
        !-----------------------------------------------
        integer :: num_comms
        integer :: num_flags
        integer :: num_models
        integer :: my_rank
        integer :: my_size
        #set $ncomps=len($proc_cfgs) * 2 + 2 
        integer :: ncomps = $ncomps
        !-----------------------------------------------
        ! define flags
        !-----------------------------------------------
        logical :: nothing

        !-----------------------------------------------
        ! define model variables
        !-----------------------------------------------
    #for $model in $proc_cfgs
        #set $name = $model.name
        #set $avs = $model.attrVects
        #set $mappers = $model.mappers
        character(len=20) :: model${name}
        integer :: ${name}_size
        integer :: ${name}_gsize
        #for $av in $avs 
        type(AttrVect) :: $avs[$av].name
        #end for
        #for $mapper in $mappers
        type(map_mod)  :: $mappers[$mapper].name
        #end for
    #end for

        character(len=20) :: iList = "fieldi"
        character(len=20) :: rList = "fieldr"

        
    #for $model in $proc_cfgs
        #set $name = $model.name
        type(map_mod)  :: $name
    #end for
        !sparse mat   emmmm 
    #for $model in $merge_cfgs
        #set $dst_info = $merge_cfgs[$model]['dst']
        #for $dst in $dst_info
        type(map_mod)   :: $dst['dst_mapper']
        #end for
    #end for




        !------------------------------------------------------
        ! define relative comm variables
        !------------------------------------------------------
        integer :: mpi_glocomm
        integer :: mpi_cpl
    #for $model in $proc_cfgs
        #set $name = $model.name
        integer :: mpi_model${name}
        integer :: mpi_model${name}2cpl
    #end for

        !-------------------------------------------------------
        ! To support the ncomps used in mct_world_init
        ! add array to store mpi_comm user get it from 
        ! ID
        !-------------------------------------------------------
        integer :: gloid         = 1
        integer :: cplid         = 2
    #for $index,$model in enumerate($proc_cfgs)
        #set $name = $model.name
        #set $m_id = 3+$index
        #set $minx_id = 3+$index+len($proc_cfgs)
        integer :: model${name}_id = $m_id
        integer :: model${name}2cpl_id = $minx_id
    #end for

        integer, dimension(:), pointer :: comp_comm
        integer, dimension(:), pointer :: comp_id
        ! judge if in model_a/b/c
        logical, dimension(:), pointer :: iamin_model

        !-------------------------------------------------------
        ! define comm control variables and run control 
        !-------------------------------------------------------

        logical :: iam_root
        logical :: iamin_cpl
        logical :: iamroot_cpl

    #for $model in $proc_cfgs
        #set $name = $model.name
        logical :: iamin_model${name}
        logical :: iamin_model${name}2cpl
        logical :: iamroot_model${name}
        logical :: iamroot_model${name}2cpl
        logical :: ${name}_run
    #end for


    end type proc




end module proc_def
