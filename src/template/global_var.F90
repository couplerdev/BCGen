module global_var
use proc_def
use field
   implicit none
   type Meta
       type(fldsMeta)    :: fldsMetaData
       type(procMeta)    :: my_proc
       type(confMeta)    :: conf
       type(compMeta)    :: atm
       type(compMeta)    :: ocn
       
       !-----------------------------------------
       ! meta desc of proc and comps
       !-----------------------------------------
       integer  :: num_comms
       integer  :: num_models
       integer  :: my_rank
       integer  :: ncomps = 6

       !-----------------------------------------
       !  comp id
       !-----------------------------------------
       integer    ::  gloid = 1
       integer    ::  cplid = 2
       integer   :: modelatm_id = 3
       integer   :: modelatm2cpl_id = 5
       integer   :: modelocn_id = 4
       integer   :: modelocn2cpl_id = 6

       !-------------------------------------------
       ! used for mct_init
       !-------------------------------------------
       integer, allocatable  :: comp_comm(:)
       integer, allocatable  :: comp_id(:)
       integer, allocatable  :: imain_model(:)

       !------------------------------------------
       !   intermediate vars
       !------------------------------------------
       type(AttrVect)   :: atm2x_atmatm
       type(AttrVect)   :: atm2x_atmx
       type(AttrVect)   :: x2atm_atmatm
       type(AttrVect)   :: x2atm_atmx
       type(gGrid)      :: domain_atm
       type(map_mod)    :: Mapper_Catm2x
       type(map_mod)    :: Mapper_Cx2atm
       type(AttrVect)   :: ocn2x_ocnocn
       type(AttrVect)   :: ocn2x_ocnx
       type(AttrVect)   :: x2ocn_ocnocn
       type(AttrVect)   :: x2ocn_ocnx
       type(gGrid)      :: domain_ocn
       type(map_mod)    :: Mapper_Cocn2x
       type(map_mod)    :: Mapper_Cx2ocn

       !---------------------------------------------
       !  define relative comm
       !---------------------------------------------
       integer    :: mpi_glocomm
       integer    :: mpi_cpl
       integer    :: mpi_modelatm
       integer    :: mpi_modelocn
       
       logical    :: iam_root
       logical    :: iamin_cpl
       logical    :: iamroot_cpl

       logical    :: iamin_modelatm
       logical    :: iamin_modelatm2cpl
       logical    :: iamroot_modelatm
       logical    :: iamroot_modelatm2cpl
       logical    :: atm_run
       logical    :: iamin_modelocn
       logical    :: iamin_modelocn2cpl
       logical    :: iamroot_modelocn
       logical    :: iamroot_modelocn2cpl
       logical    :: ocn_run

       
       type(map_mod)   :: mapper_Smatatm2ocn
       type(map_mod)   :: mapper_Smatocn2atm

       character(FIELDSLEN) :: flds_dom 
       character(FIELDSLEN) :: flds_x2ocn_fluxes 
       character(FIELDSLEN) :: flds_ocn2x_states 
       character(FIELDSLEN) :: flds_atm2x_fluxes 
       character(FIELDSLEN) :: flds_x2ocn 
       character(FIELDSLEN) :: flds_x2atm 
       character(FIELDSLEN) :: flds_dom_coord 
       character(FIELDSLEN) :: flds_x2atm_states 
       character(FIELDSLEN) :: flds_x2ocn_states 
       character(FIELDSLEN) :: flds_x2atm_fluxes 
       character(FIELDSLEN) :: flds_atm2x 
       character(FIELDSLEN) :: flds_ocn2x 
       character(FIELDSLEN) :: flds_atm2x_states 
       character(FIELDSLEN) :: flds_ocn2x_fluxes 

   end type Meta
    
   type(Meta)          :: metaData
   integer, parameter  :: gloid = 1
   integer, parameter  :: cplid = 2
   integer, parameter  :: atmid = 2
   integer, parameter  :: atm2xid = 2+1
   integer, parameter  :: ocnid = 4
   integer, parameter  :: ocn2xid = 4+1

end module global_var
