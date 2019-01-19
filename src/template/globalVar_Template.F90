module global_var
use shr_kind_mod
use mct_mod
use type_def
use proc_def
use field_def
   implicit none
   type Meta
       type(fldsMeta)    :: fldsMetaData
       type(procMeta)    :: my_proc
       type(confMeta)    :: conf
       #for $model in $proc_cfgs
           #set $name = $model.name
       type(compMeta)    :: $name
       #end for    
       character(SHR_KIND_CL) :: datanml
       character(SHR_KIND_CL) :: datarc
       character(SHR_KIND_CL) :: case_name
       !-----------------------------------------
       ! meta desc of proc and comps
       !-----------------------------------------
       integer  :: num_comms
       integer  :: num_models
       integer  :: my_rank
       #set $ncomps = len($proc_cfgs)*2+2
       integer  :: ncomps = $ncomps

       !-----------------------------------------
       !  comp id
       !-----------------------------------------
       integer    ::  gloid = 1
       integer    ::  cplid = 2
       #for $index, $model in enumerate($proc_cfgs)
            #set $name = $model.name
            #set $m_id = 3+$index
            #set $minx_id = 3 + $index+len($proc_cfgs)
       integer   :: model${name}_id = $m_id
       integer   :: model${name}2cpl_id = $minx_id
       #end for

       !-------------------------------------------
       ! used for mct_init
       !-------------------------------------------
       integer, allocatable  :: comp_comm(:)
       integer, allocatable  :: comp_id(:)
       logical, pointer      :: iamin_model(:)
       character(SHR_KIND_CL), allocatable :: comp_name(:)
       integer, allocatable  :: comp_comm_iam(:)

       !------------------------------------------
       !   intermediate vars
       !------------------------------------------
       #for $model in $proc_cfgs
           #set $name = $model.name
           #set $avs = $model.attrVects
           #set $mappers = $model.mappers
           #set $grid = $model.domain
           #for $av in $avs
       type(mct_aVect)   :: $avs[$av].name
           #end for
       type(mct_gGrid)      :: $model.domain
           #for $mapper in $mappers
       type(map_mod)    :: $mappers[$mapper].name
           #end for
       #end for

       !---------------------------------------------
       !  define relative comm
       !---------------------------------------------
       integer    :: mpi_glocomm
       integer    :: mpi_cpl
       #for $model in $proc_cfgs
            #set $name = $model.name
       integer    :: mpi_model${name}
       integer    :: mpi_model${name}2cpl
       #end for
       
       logical    :: iam_root
       logical    :: iamin_cpl
       logical    :: iamroot_cpl

       #for $model in $proc_cfgs
            #set $name =  $model.name
       logical    :: iamin_model${name}
       logical    :: iamin_model${name}2cpl
       logical    :: iamroot_model${name}
       logical    :: iamroot_model${name}2cpl
       logical    :: ${name}_run
       #end for

       
       #for $model in $merge_cfgs
            #set $dst_info = $merge_cfgs[$model]['dst']
            #for $dst in $dst_info
       type(map_mod)   :: $dst['dst_mapper']
            #end for
       #end for

       #for $fld in $fieldVar_cfgs
            #set $val = $fieldVar_cfgs[$fld]
       character(SHR_KIND_CL) :: $fld 
       #end for

   end type Meta
    
   type(Meta), target  :: metaData
   integer, parameter  :: gloid = 1
   integer, parameter  :: cplid = 2
   #for $index, $model in enumerate($proc_cfgs)
        #set $name = $model.name
        #set $my_id = 2+2*$index
   integer, parameter  :: ${name}id = $my_id
   integer, parameter  :: ${name}2xid = $my_id+1
   #end for

end module global_var
