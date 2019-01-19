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
    type(Meta),        intent(in)    :: metaData
!    type(attrVect),   intent(in)    :: lnd2x_atmx
    type(mct_aVect),   intent(in)    :: ocn2x_atmx
!    type(attrVect),   intent(in)    :: ice2x_atmx
    type(mct_aVect),   intent(in)    :: atm2x_atmx
    type(mct_aVect),   intent(inout) :: x2atm_atmx

    integer :: av_size
    integer :: nf
    integer :: i,j

    av_size = mct_avect_lsize(atm2x_atmx)
    nf = mct_avect_nRattr(atm2x_atmx)

    !call mct_avect_copy(atm2x_atmx, x2atm_atmx)
    do i = 0, av_size
    do j = 0, nf
        x2atm_atmx%rAttr(i,j) = atm2x_atmx%rAttr(i,j)
    end do
    end do   

    write(*,*)'========== atm mrged =========='    

end subroutine mrg_x2atm

subroutine mrg_x2ocn(metaData, atm2x_ocnx,  ocn2x_ocnx,&
x2ocn_ocnx)

    implicit none
    type(Meta),        intent(in)    :: metaData
    type(mct_aVect),   intent(in)    :: atm2x_ocnx
!    type(attrVect),   intent(in)    :: wav2x_ocnx
    type(mct_aVect),   intent(in)    :: ocn2x_ocnx
    type(mct_aVect),   intent(inout) :: x2ocn_ocnx

    write(*,*)'========== ocn mrged =========='

end subroutine mrg_x2ocn


end module mrg_mod
