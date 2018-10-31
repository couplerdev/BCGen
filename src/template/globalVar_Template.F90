module global_var

   implicit none
   type Meta
       type(field)       :: Fields
       type(procMeta)    :: my_proc
       type(confMeta)    :: conf
       #for $model in $proc_cfgs
           #set $name = $model.name
       type(compMeta)    :: $name
       #end for    
       
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
       integer, dimension(:)  :: comp_comm
       integer, dimension(:)  :: comp_id
       integer, dimension(:)  :: imain_model

       !------------------------------------------
       !   intermediate vars
       !------------------------------------------
       #for $model in $proc_cfgs
           #set $name = $model.name
           #set $avs = $model.attrVects
           #set $mappers = $model.mappers
           #set $grid = $model.domain
           #for $av in $avs
       type(AttrVect)   :: $avs[$av].name
           #end for
       type(gGrid)      :: $model.domain
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

       #for $model in $proc_cfgs
            #set $name = $model.name
       type(map_mod)   ::$name
       #end for
       
       #for $model in $merge_cfgs
            #set $dst_info = $merge_cfgs[$model]['dst']
            #for $dst in $dst_info
       type(map_mod)   :: $dst['dst_mapper']
            #end for
       #end for

   end type Meta
    
   type(Meta)          :: metaData
   integer, parameter  :: gloid
   integer, parameter  :: cplid
   #for $model in $proc_cfgs
        #set $name = $model.name
   integer, parameter  :: ${name}id
   integer, parameter  :: ${name}2xid
   #end for

end module global_var
