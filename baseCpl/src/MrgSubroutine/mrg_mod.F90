module mrg_mod
use mpi
use mct_mod
use proc_def
use global_var
    implicit none
!include "mpif.h"
    public :: mrg_x2atm
!    public :: mrg_x2lnd
    public :: mrg_x2ocn
!    public :: mrg_x2rof
!    public :: mrg_x2ice
!    public :: mrg_x2wav

contains

subroutine mrg_x2atm(metaData, ocn2x_atmx,  atm2x_atmx,&
x2atm_atmx)
    implicit none
    type(Meta),       intent(in)    :: metaData
!    type(attrVect),   intent(in)    :: lnd2x_atmx
    type(attrVect),   intent(in)    :: ocn2x_atmx
!    type(attrVect),   intent(in)    :: ice2x_atmx
    type(attrVect),   intent(in)    :: atm2x_atmx
    type(attrVect),   intent(inout) :: x2atm_atmx

    write(*,*)'atm mrged'    

end subroutine mrg_x2atm

subroutine mrg_x2ocn(metaData, atm2x_ocnx,  ocn2x_ocnx,&
x2ocn_ocnx)

    implicit none
    type(Meta),       intent(in)    :: metaData
    type(attrVect),   intent(in)    :: atm2x_ocnx
!    type(attrVect),   intent(in)    :: wav2x_ocnx
    type(attrVect),   intent(in)    :: ocn2x_ocnx
    type(attrVect),   intent(inout) :: x2ocn_ocnx

    write(*,*)'ocn mrged'

end subroutine mrg_x2ocn


end module mrg_mod
