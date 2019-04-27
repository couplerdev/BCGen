module comms_def
use shr_kind_mod
use mct_mod
    implicit none
    type map_mod
        character(SHR_KIND_CS) :: map_type  ! copy::rearr::spmat::online
        character(SHR_KIND_CS) :: coord
        !character(len=64) :: mapper_name
        type(mct_rearr) :: rearr
        type(mct_sMat) :: sMat
        type(mct_sMatP) :: sMatPlus
        type(mct_sMat) :: sMatX2A
        type(mct_sMatP) :: sMatX2APlus
        type(mct_gGrid), pointer :: dom_s  ! using pointer to save mem
        type(mct_gGrid), pointer :: dom_d
        type(mct_gsMap), pointer :: gsmap_s
        type(mct_gsMap), pointer :: gsmap_d
    end type map_mod


end module comms_def
