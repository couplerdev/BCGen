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
        integer :: ncomps = 16
        !-----------------------------------------------
        ! define flags
        !-----------------------------------------------
        logical :: nothing

        !-----------------------------------------------
        ! define model variables
        !-----------------------------------------------
        character(len=20) :: modelglc
        integer :: glc_size
        integer :: glc_gsize
        type(AttrVect) :: glc2x_glcglc
        type(AttrVect) :: glc2x_glcx
        type(AttrVect) :: x2glc_glcglc
        type(AttrVect) :: x2glc_glcx
        type(gGrid) :: domain_glc
        type(map_mod)  :: Mapper_Cglc2x
        type(map_mod)  :: Mapper_Cx2glc
        character(len=20) :: modelocn
        integer :: ocn_size
        integer :: ocn_gsize
        type(AttrVect) :: ocn2x_ocnocn
        type(AttrVect) :: ocn2x_ocnx
        type(AttrVect) :: x2ocn_ocnocn
        type(AttrVect) :: x2ocn_ocnx
        type(gGrid) :: domain_ocn
        type(map_mod)  :: Mapper_Cocn2x
        type(map_mod)  :: Mapper_Cx2ocn
        character(len=20) :: modelatm
        integer :: atm_size
        integer :: atm_gsize
        type(AttrVect) :: atm2x_atmatm
        type(AttrVect) :: atm2x_atmx
        type(AttrVect) :: x2atm_atmatm
        type(AttrVect) :: x2atm_atmx
        type(gGrid) :: domain_atm
        type(map_mod)  :: Mapper_Catm2x
        type(map_mod)  :: Mapper_Cx2atm
        character(len=20) :: modelice
        integer :: ice_size
        integer :: ice_gsize
        type(AttrVect) :: ice2x_iceice
        type(AttrVect) :: ice2x_icex
        type(AttrVect) :: x2ice_iceice
        type(AttrVect) :: x2ice_icex
        type(gGrid) :: domain_ice
        type(map_mod)  :: Mapper_Cice2x
        type(map_mod)  :: Mapper_Cx2ice
        character(len=20) :: modelrof
        integer :: rof_size
        integer :: rof_gsize
        type(AttrVect) :: rof2x_rofrof
        type(AttrVect) :: rof2x_rofx
        type(AttrVect) :: x2rof_rofrof
        type(AttrVect) :: x2rof_rofx
        type(gGrid) :: domain_rof
        type(map_mod)  :: Mapper_Crof2x
        type(map_mod)  :: Mapper_Cx2rof
        character(len=20) :: modelwav
        integer :: wav_size
        integer :: wav_gsize
        type(AttrVect) :: wav2x_wavwav
        type(AttrVect) :: wav2x_wavx
        type(AttrVect) :: x2wav_wavwav
        type(AttrVect) :: x2wav_wavx
        type(gGrid) :: domain_wav
        type(map_mod)  :: Mapper_Cwav2x
        type(map_mod)  :: Mapper_Cx2wav
        character(len=20) :: modellnd
        integer :: lnd_size
        integer :: lnd_gsize
        type(AttrVect) :: lnd2x_lndlnd
        type(AttrVect) :: lnd2x_lndx
        type(AttrVect) :: x2lnd_lndlnd
        type(AttrVect) :: x2lnd_lndx
        type(gGrid) :: domain_lnd
        type(map_mod)  :: Mapper_Clnd2x
        type(map_mod)  :: Mapper_Cx2lnd

        character(len=20) :: iList = "fieldi"
        character(len=20) :: rList = "fieldr"

        
        type(map_mod)  :: glc
        type(map_mod)  :: ocn
        type(map_mod)  :: atm
        type(map_mod)  :: ice
        type(map_mod)  :: rof
        type(map_mod)  :: wav
        type(map_mod)  :: lnd
        !sparse mat   emmmm 
        type(map_mod)   :: mapper_Smatocn2wav
        type(map_mod)   :: mapper_Smatocn2atm
        type(map_mod)   :: mapper_Smatatm2lnd
        type(map_mod)   :: mapper_Smatatm2ocn
        type(map_mod)   :: mapper_Smatatm2ice
        type(map_mod)   :: mapper_Smatatm2wav
        type(map_mod)   :: mapper_Smatrof2lnd
        type(map_mod)   :: mapper_Smatice2wav
        type(map_mod)   :: mapper_Smatice2atm
        type(map_mod)   :: mapper_Smatwav2ocn
        type(map_mod)   :: mapper_Smatlnd2atm




        !------------------------------------------------------
        ! define relative comm variables
        !------------------------------------------------------
        integer :: mpi_glocomm
        integer :: mpi_cpl
        integer :: mpi_modelglc
        integer :: mpi_modelglc2cpl
        integer :: mpi_modelocn
        integer :: mpi_modelocn2cpl
        integer :: mpi_modelatm
        integer :: mpi_modelatm2cpl
        integer :: mpi_modelice
        integer :: mpi_modelice2cpl
        integer :: mpi_modelrof
        integer :: mpi_modelrof2cpl
        integer :: mpi_modelwav
        integer :: mpi_modelwav2cpl
        integer :: mpi_modellnd
        integer :: mpi_modellnd2cpl

        !-------------------------------------------------------
        ! To support the ncomps used in mct_world_init
        ! add array to store mpi_comm user get it from 
        ! ID
        !-------------------------------------------------------
        integer :: gloid         = 1
        integer :: cplid         = 2
        integer :: modelglc_id = 3
        integer :: modelglc2cpl_id = 10
        integer :: modelocn_id = 4
        integer :: modelocn2cpl_id = 11
        integer :: modelatm_id = 5
        integer :: modelatm2cpl_id = 12
        integer :: modelice_id = 6
        integer :: modelice2cpl_id = 13
        integer :: modelrof_id = 7
        integer :: modelrof2cpl_id = 14
        integer :: modelwav_id = 8
        integer :: modelwav2cpl_id = 15
        integer :: modellnd_id = 9
        integer :: modellnd2cpl_id = 16

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

        logical :: iamin_modelglc
        logical :: iamin_modelglc2cpl
        logical :: iamroot_modelglc
        logical :: iamroot_modelglc2cpl
        logical :: glc_run
        logical :: iamin_modelocn
        logical :: iamin_modelocn2cpl
        logical :: iamroot_modelocn
        logical :: iamroot_modelocn2cpl
        logical :: ocn_run
        logical :: iamin_modelatm
        logical :: iamin_modelatm2cpl
        logical :: iamroot_modelatm
        logical :: iamroot_modelatm2cpl
        logical :: atm_run
        logical :: iamin_modelice
        logical :: iamin_modelice2cpl
        logical :: iamroot_modelice
        logical :: iamroot_modelice2cpl
        logical :: ice_run
        logical :: iamin_modelrof
        logical :: iamin_modelrof2cpl
        logical :: iamroot_modelrof
        logical :: iamroot_modelrof2cpl
        logical :: rof_run
        logical :: iamin_modelwav
        logical :: iamin_modelwav2cpl
        logical :: iamroot_modelwav
        logical :: iamroot_modelwav2cpl
        logical :: wav_run
        logical :: iamin_modellnd
        logical :: iamin_modellnd2cpl
        logical :: iamroot_modellnd
        logical :: iamroot_modellnd2cpl
        logical :: lnd_run


    end type proc




end module proc_def
