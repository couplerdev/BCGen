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
       type(compMeta)    :: lnd
       type(compMeta)    :: atm
       type(compMeta)    :: ocn
       character(SHR_KIND_CL) :: datanml
       character(SHR_KIND_CL) :: datarc
       character(SHR_KIND_CL) :: case_name
       !-----------------------------------------
       ! meta desc of proc and comps
       !-----------------------------------------
       integer  :: num_comms
       integer  :: num_models
       integer  :: my_rank
       integer  :: ncomps = 8

       !-----------------------------------------
       !  comp id
       !-----------------------------------------
       integer    ::  gloid = 1
       integer    ::  cplid = 2
       integer   :: modellnd_id = 3
       integer   :: modellnd2cpl_id = 6
       integer   :: modelatm_id = 4
       integer   :: modelatm2cpl_id = 7
       integer   :: modelocn_id = 5
       integer   :: modelocn2cpl_id = 8

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
       type(mct_aVect)   :: lnd2x_lndlnd
       type(mct_aVect)   :: lnd2x_lndx
       type(mct_aVect)   :: x2lnd_lndlnd
       type(mct_aVect)   :: x2lnd_lndx
       type(mct_gGrid)      :: domain_lnd
       type(map_mod)    :: Mapper_Clnd2x
       type(map_mod)    :: Mapper_Cx2lnd
       type(mct_aVect)   :: atm2x_atmatm
       type(mct_aVect)   :: atm2x_atmx
       type(mct_aVect)   :: x2atm_atmatm
       type(mct_aVect)   :: x2atm_atmx
       type(mct_gGrid)      :: domain_atm
       type(map_mod)    :: Mapper_Catm2x
       type(map_mod)    :: Mapper_Cx2atm
       type(mct_aVect)   :: ocn2x_ocnocn
       type(mct_aVect)   :: ocn2x_ocnx
       type(mct_aVect)   :: x2ocn_ocnocn
       type(mct_aVect)   :: x2ocn_ocnx
       type(mct_gGrid)      :: domain_ocn
       type(map_mod)    :: Mapper_Cocn2x
       type(map_mod)    :: Mapper_Cx2ocn

       !---------------------------------------------
       !  define relative comm
       !---------------------------------------------
       integer    :: mpi_glocomm
       integer    :: mpi_cpl
       integer    :: mpi_modellnd
       integer    :: mpi_modellnd2cpl
       integer    :: mpi_modelatm
       integer    :: mpi_modelatm2cpl
       integer    :: mpi_modelocn
       integer    :: mpi_modelocn2cpl
       
       logical    :: iam_root
       logical    :: iamin_cpl
       logical    :: iamroot_cpl

       logical    :: iamin_modellnd
       logical    :: iamin_modellnd2cpl
       logical    :: iamroot_modellnd
       logical    :: iamroot_modellnd2cpl
       logical    :: lnd_run
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

       character(SHR_KIND_CXX) :: flds_dom 
       character(SHR_KIND_CXX) :: flds_x2ocn_fluxes 
       character(SHR_KIND_CXX) :: flds_ocn2x_states 
       character(SHR_KIND_CXX) :: flds_x2lnd_states 
       character(SHR_KIND_CXX) :: flds_lnd2x_states 
       character(SHR_KIND_CXX) :: flds_x2ocn 
       character(SHR_KIND_CXX) :: flds_lnd2x_fluxes 
       character(SHR_KIND_CXX) :: flds_ocn2x 
       character(SHR_KIND_CXX) :: flds_x2atm 
       character(SHR_KIND_CXX) :: flds_dom_coord 
       character(SHR_KIND_CXX) :: flds_atm2x_states 
       character(SHR_KIND_CXX) :: flds_x2ocn_states 
       character(SHR_KIND_CXX) :: flds_atm2x_fluxes 
       character(SHR_KIND_CXX) :: flds_atm2x 
       character(SHR_KIND_CXX) :: flds_x2atm_fluxes 
       character(SHR_KIND_CXX) :: flds_x2lnd 
       character(SHR_KIND_CXX) :: flds_x2lnd_fluxes 
       character(SHR_KIND_CXX) :: flds_lnd2x 
       character(SHR_KIND_CXX) :: flds_x2atm_states 
       character(SHR_KIND_CXX) :: flds_ocn2x_fluxes 

   end type Meta
    
   type(Meta), target  :: metaData
   integer, parameter  :: gloid = 1
   integer, parameter  :: cplid = 2
   integer, parameter  :: lndid = 2
   integer, parameter  :: lnd2xid = 2+1
   integer, parameter  :: atmid = 4
   integer, parameter  :: atm2xid = 4+1
   integer, parameter  :: ocnid = 6
   integer, parameter  :: ocn2xid = 6+1

end module global_var
