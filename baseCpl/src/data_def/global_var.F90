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
       type(compMeta)    :: ocn
       type(mct_gsMap) :: comp_gsmap_ocnx
       type(compMeta)    :: atm
       type(mct_gsMap) :: comp_gsmap_atmx
       type(compMeta)    :: atm
       type(mct_gsMap) :: comp_gsmap_atmx
       type(compMeta)    :: ice
       type(mct_gsMap) :: comp_gsmap_icex
       type(compMeta)    :: rof
       type(mct_gsMap) :: comp_gsmap_rofx
       type(compMeta)    :: lnd
       type(mct_gsMap) :: comp_gsmap_lndx
       type(compMeta)    :: lnd
       type(mct_gsMap) :: comp_gsmap_lndx
       character(SHR_KIND_CL) :: datanml
       character(SHR_KIND_CL) :: datarc
       character(SHR_KIND_CL) :: case_name
       !-----------------------------------------
       ! meta desc of proc and comps
       !-----------------------------------------
       integer  :: num_comms
       integer  :: num_models
       integer  :: my_rank
       integer  :: ncomps = 16

       !-----------------------------------------
       !  comp id
       !-----------------------------------------
       integer    ::  gloid = 1
       integer    ::  cplid = 2
       integer   :: modelocn_id = 3
       integer   :: modelocn2cpl_id = 10
       integer   :: modelatm_id = 4
       integer   :: modelatm2cpl_id = 11
       integer   :: modelatm_id = 5
       integer   :: modelatm2cpl_id = 12
       integer   :: modelice_id = 6
       integer   :: modelice2cpl_id = 13
       integer   :: modelrof_id = 7
       integer   :: modelrof2cpl_id = 14
       integer   :: modellnd_id = 8
       integer   :: modellnd2cpl_id = 15
       integer   :: modellnd_id = 9
       integer   :: modellnd2cpl_id = 16

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
       type(mct_aVect)   :: ocn2x_ocnocn
       type(mct_aVect)   :: ocn2x_ocnx
       type(mct_aVect)   :: x2ocn_ocnocn
       type(mct_aVect)   :: x2ocn_ocnx
       type(mct_gGrid)      :: domain_ocnx
       type(mct_gGrid)      :: domain_ocnocn
       type(map_mod)    :: Mapper_Cocn2x
       type(map_mod)    :: Mapper_Cx2ocn
       type(mct_aVect)   :: atm02x_atm0atm0
       type(mct_aVect)   :: atm02x_atm0x
       type(mct_aVect)   :: x2atm0_atm0atm0
       type(mct_aVect)   :: x2atm0_atm0x
       type(mct_gGrid)      :: domain_atm0x
       type(mct_gGrid)      :: domain_atm0atm0
       type(map_mod)    :: Mapper_Catm02x
       type(map_mod)    :: Mapper_Cx2atm0
       type(mct_aVect)   :: atm02x_atm0atm0
       type(mct_aVect)   :: atm02x_atm0x
       type(mct_aVect)   :: x2atm0_atm0atm0
       type(mct_aVect)   :: x2atm0_atm0x
       type(mct_gGrid)      :: domain_atm0x
       type(mct_gGrid)      :: domain_atm0atm0
       type(map_mod)    :: Mapper_Catm02x
       type(map_mod)    :: Mapper_Cx2atm0
       type(mct_aVect)   :: ice2x_iceice
       type(mct_aVect)   :: ice2x_icex
       type(mct_aVect)   :: x2ice_iceice
       type(mct_aVect)   :: x2ice_icex
       type(mct_gGrid)      :: domain_icex
       type(mct_gGrid)      :: domain_iceice
       type(map_mod)    :: Mapper_Cice2x
       type(map_mod)    :: Mapper_Cx2ice
       type(mct_aVect)   :: rof2x_rofrof
       type(mct_aVect)   :: rof2x_rofx
       type(mct_aVect)   :: x2rof_rofrof
       type(mct_aVect)   :: x2rof_rofx
       type(mct_gGrid)      :: domain_rofx
       type(mct_gGrid)      :: domain_rofrof
       type(map_mod)    :: Mapper_Crof2x
       type(map_mod)    :: Mapper_Cx2rof
       type(mct_aVect)   :: lnd02x_lnd0lnd0
       type(mct_aVect)   :: lnd02x_lnd0x
       type(mct_aVect)   :: x2lnd0_lnd0lnd0
       type(mct_aVect)   :: x2lnd0_lnd0x
       type(mct_gGrid)      :: domain_lnd0x
       type(mct_gGrid)      :: domain_lnd0lnd0
       type(map_mod)    :: Mapper_Clnd02x
       type(map_mod)    :: Mapper_Cx2lnd0
       type(mct_aVect)   :: lnd02x_lnd0lnd0
       type(mct_aVect)   :: lnd02x_lnd0x
       type(mct_aVect)   :: x2lnd0_lnd0lnd0
       type(mct_aVect)   :: x2lnd0_lnd0x
       type(mct_gGrid)      :: domain_lnd0x
       type(mct_gGrid)      :: domain_lnd0lnd0
       type(map_mod)    :: Mapper_Clnd02x
       type(map_mod)    :: Mapper_Cx2lnd0

       !---------------------------------------------
       !  define relative comm
       !---------------------------------------------
       integer    :: mpi_glocomm
       integer    :: mpi_cpl
       integer    :: mpi_modelocn
       integer    :: mpi_modelocn2cpl
       integer    :: mpi_modelatm
       integer    :: mpi_modelatm2cpl
       integer    :: mpi_modelatm
       integer    :: mpi_modelatm2cpl
       integer    :: mpi_modelice
       integer    :: mpi_modelice2cpl
       integer    :: mpi_modelrof
       integer    :: mpi_modelrof2cpl
       integer    :: mpi_modellnd
       integer    :: mpi_modellnd2cpl
       integer    :: mpi_modellnd
       integer    :: mpi_modellnd2cpl
       
       logical    :: iam_root
       logical    :: iamin_cpl
       logical    :: iamroot_cpl

       logical    :: iamin_modelocn
       logical    :: iamin_modelocn2cpl
       logical    :: iamroot_modelocn
       logical    :: iamroot_modelocn2cpl
       logical    :: ocn_run
       logical    :: iamin_modelatm
       logical    :: iamin_modelatm2cpl
       logical    :: iamroot_modelatm
       logical    :: iamroot_modelatm2cpl
       logical    :: atm_run
       logical    :: iamin_modelatm
       logical    :: iamin_modelatm2cpl
       logical    :: iamroot_modelatm
       logical    :: iamroot_modelatm2cpl
       logical    :: atm_run
       logical    :: iamin_modelice
       logical    :: iamin_modelice2cpl
       logical    :: iamroot_modelice
       logical    :: iamroot_modelice2cpl
       logical    :: ice_run
       logical    :: iamin_modelrof
       logical    :: iamin_modelrof2cpl
       logical    :: iamroot_modelrof
       logical    :: iamroot_modelrof2cpl
       logical    :: rof_run
       logical    :: iamin_modellnd
       logical    :: iamin_modellnd2cpl
       logical    :: iamroot_modellnd
       logical    :: iamroot_modellnd2cpl
       logical    :: lnd_run
       logical    :: iamin_modellnd
       logical    :: iamin_modellnd2cpl
       logical    :: iamroot_modellnd
       logical    :: iamroot_modellnd2cpl
       logical    :: lnd_run

       
       type(map_mod)   :: mapper_Smatice2atm0
       type(map_mod)   :: mapper_Smatocn2atm0
       type(map_mod)   :: mapper_Smatatm02ice
       type(map_mod)   :: mapper_Smatatm02ocn
       type(map_mod)   :: mapper_Smatrof2lnd0
       type(map_mod)   :: mapper_Smatlnd02atm0
       type(map_mod)   :: mapper_Smatlnd02rof
       type(map_mod)   :: mapper_Smatatm02lnd0

       character(SHR_KIND_CXX) :: flds_x2ocn 
       character(SHR_KIND_CXX) :: flds_x2rof_fluxes 
       character(SHR_KIND_CXX) :: flds_rof2x_states 
       character(SHR_KIND_CXX) :: flds_xao_fluxes 
       character(SHR_KIND_CXX) :: flds_x2ice_fluxes 
       character(SHR_KIND_CXX) :: flds_lnd2x 
       character(SHR_KIND_CXX) :: flds_rof2x 
       character(SHR_KIND_CXX) :: flds_dom 
       character(SHR_KIND_CXX) :: flds_x2ocn_fluxes 
       character(SHR_KIND_CXX) :: flds_x2rof_states 
       character(SHR_KIND_CXX) :: flds_atm2x_fluxes 
       character(SHR_KIND_CXX) :: flds_x2ice 
       character(SHR_KIND_CXX) :: flds_dom_coord 
       character(SHR_KIND_CXX) :: flds_x2atm_states 
       character(SHR_KIND_CXX) :: flds_x2ocn_states 
       character(SHR_KIND_CXX) :: flds_x2atm 
       character(SHR_KIND_CXX) :: flds_ice2x_fluxes 
       character(SHR_KIND_CXX) :: flds_xao_fields 
       character(SHR_KIND_CXX) :: flds_ocn2x_states 
       character(SHR_KIND_CXX) :: flds_ice2x 
       character(SHR_KIND_CXX) :: flds_x2lnd_states 
       character(SHR_KIND_CXX) :: flds_x2lnd 
       character(SHR_KIND_CXX) :: flds_lnd2x_states 
       character(SHR_KIND_CXX) :: flds_x2ice_states 
       character(SHR_KIND_CXX) :: flds_x2lnd_fluxes 
       character(SHR_KIND_CXX) :: flds_atm2x 
       character(SHR_KIND_CXX) :: flds_ocn2x 
       character(SHR_KIND_CXX) :: flds_dom_other 
       character(SHR_KIND_CXX) :: flds_lnd2x_fluxes 
       character(SHR_KIND_CXX) :: flds_ice2x_states 
       character(SHR_KIND_CXX) :: flds_atm2x_states 
       character(SHR_KIND_CXX) :: flds_rof2x_fluxes 
       character(SHR_KIND_CXX) :: flds_xao_states 
       character(SHR_KIND_CXX) :: flds_ocn2x_fluxes 
       character(SHR_KIND_CXX) :: flds_x2atm_fluxes 
       character(SHR_KIND_CXX) :: flds_x2rof 

   end type Meta
    
   type(Meta), target  :: metaData
   integer, parameter  :: gloid = 1
   integer, parameter  :: cplid = 2
   integer, parameter  :: ocnid = 2
   integer, parameter  :: ocn2xid = 2+1
   integer, parameter  :: atmid = 4
   integer, parameter  :: atm2xid = 4+1
   integer, parameter  :: atmid = 6
   integer, parameter  :: atm2xid = 6+1
   integer, parameter  :: iceid = 8
   integer, parameter  :: ice2xid = 8+1
   integer, parameter  :: rofid = 10
   integer, parameter  :: rof2xid = 10+1
   integer, parameter  :: lndid = 12
   integer, parameter  :: lnd2xid = 12+1
   integer, parameter  :: lndid = 14
   integer, parameter  :: lnd2xid = 14+1

end module global_var
