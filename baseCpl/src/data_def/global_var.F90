module global_var
!----------------------------------------------
!This module regist all the global var
!and to some gabage collection work
!
!----------------------------------------------
implicit
type Meta

    type(field)    :: Fields
    type(procMeta) :: my_proc
    type(compMeta) :: atm
    type(compMeta) :: glc
    !type(compMeta) :: drv

    !--------------------------------------------
    !  meta desc of proc and comps
    !--------------------------------------------
    integer :: num_comms
    integer :: num_models
    integer :: my_rank
    integer :: ncomps = 

    !---------------------------------------------
    !   comp id
    !---------------------------------------------    
    integer :: gloid            = 1
    integer :: cplid            = 2
    integer :: modelatm_id      = 3  
    integer :: modelatm2cpl_id  = 4
    integer :: modelocn_id      = 5
    integer :: modelocn2cpl_id  = 6


    !---------------------------------------------
    ! used for mct_init
    !---------------------------------------------
    integer, dimension(:) :: comp_comm
    integer, dimension(:) :: comp_id
    integer, dimension(:) :: iamin_model

    !-------------------------
    ! intermediate vars
    !-------------------------
    type(AttrVect) :: atm2x_atmatm
    type(AttrVect) :: atm2x_atmx
    type(AttrVect) :: x2atm_atmatm
    type(AttrVect) :: x2atm_atmx
    type(gGrid)    :: domain_atm
    type(map_mod)  :: Mapper_Catm2x
    type(map_mod)  :: Mapper_Cx2atm
    
    !--------------------------
    !  define relative comm
    !--------------------------
    integer :: mpi_glocomm
    integer :: mpi_cpl
    integer :: mpi_modelatm
    integer :: mpi_modelocn


    logical :: iam_root
    logical :: iamin_cpl
    logical :: iamroot_cpl

    logical :: iamin_modelatm
    logical :: iamin_modelatm2cpl
    logical :: iamroot_modelatm
    logical :: iamroot_modelatm2cpl
    logical :: atm_run

    logical :: iamin_modelocn
    logical :: iamin_modelocn2cpl
    logical :: iamroot_modelocn
    logical :: iamroot_modelocn2cpl
    logical :: ocn_run

    type(map_mod) :: mapper_Smatocn2atm
    type(map_mod) :: mapper_Smatatm2ocn


end type Meta
!--------------------------------------------
!    global variables declare here
!--------------------------------------------
    type(Meta)  :: metaData
    integer, parameter :: GLOID
    integer, parameter :: CPLID
    integer, parameter :: ATMID
    integer, parameter :: ATM2XID
    integer, parameter :: OCNID
    integer, parameter :: OCN2XID




!-----------------------------------------
! relavent initialization method
!-----------------------------------------
    

end module global_var
