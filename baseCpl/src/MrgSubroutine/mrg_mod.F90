module mrg_mod

use mct_mod
use proc_def
    implicit none
include "mpif.h"
    public :: mrg_x2atm
    public :: mrg_x2lnd
    public :: mrg_x2ocn
    public :: mrg_x2rof
    public :: mrg_x2ice
    public :: mrg_x2wav

contains

subroutine mrg_x2atm(my_proc, lnd2x_atmx, ocn2x_atmx, ice2x_atmx, atm2x_atmx,&
x2atm_atmx)
    implicit none
    type(proc),       intent(in)    :: my_proc
    type(attrVect),   intent(in)    :: lnd2x_atmx
    type(attrVect),   intent(in)    :: ocn2x_atmx
    type(attrVect),   intent(in)    :: ice2x_atmx
    type(attrVect),   intent(in)    :: atm2x_atmx
    type(attrVect),   intent(inout) :: x2atm_atmx

    write(*,*)'atm mrged'    

end subroutine mrg_x2atm

subroutine mrg_x2ice(my_proc, atm2x_icex, ice2x_icex, x2ice_icex)

    implicit none
    type(proc),       intent(in)    :: my_proc
    type(attrVect),   intent(in)    :: atm2x_icex
    type(attrVect),   intent(in)    :: ice2x_icex
    type(attrVect),   intent(inout) :: x2ice_icex

    write(*,*)'ice mrged'

end subroutine mrg_x2ice

subroutine mrg_x2rof(my_proc, lnd2x_rofx, rof2x_rofx, x2rof_rofx)
  
    implicit none
    type(proc),       intent(in)    :: my_proc
    type(attrVect),   intent(in)    :: lnd2x_rofx
    type(attrVect),   intent(in)    :: rof2x_rofx
    type(attrVect),   intent(inout) :: x2rof_rofx

    write(*,*) 'rof mrged'

end subroutine mrg_x2rof

subroutine mrg_x2lnd(my_proc, atm2x_lndx, rof2x_lndx, lnd2x_lndx, x2lnd_lndx)

    implicit none
    type(proc),       intent(in)    :: my_proc
    type(attrVect),   intent(in)    :: atm2x_lndx
    type(attrVect),   intent(in)    :: rof2x_lndx
    type(attrVect),   intent(in)    :: lnd2x_lndx
    type(attrVect),   intent(inout) :: x2lnd_lndx


    write(*,*)'lnd mrged'

end subroutine mrg_x2lnd

subroutine mrg_x2ocn(my_proc, atm2x_ocnx, wav2x_ocnx, ocn2x_ocnx,&
x2ocn_ocnx)

    implicit none
    type(proc),       intent(in)    :: my_proc
    type(attrVect),   intent(in)    :: atm2x_ocnx
    type(attrVect),   intent(in)    :: wav2x_ocnx
    type(attrVect),   intent(in)    :: ocn2x_ocnx
    type(attrVect),   intent(inout) :: x2ocn_ocnx

    write(*,*)'ocn mrged'

end subroutine mrg_x2ocn

subroutine mrg_x2wav(my_proc, atm2x_wavx, ocn2x_wavx, ice2x_wavx, wav2x_wavx,&
x2wav_wavx)

    implicit none
    type(proc),       intent(in)    :: my_proc
    type(attrVect),   intent(in)    :: atm2x_wavx
    type(attrVect),   intent(in)    :: ocn2x_wavx
    type(attrVect),   intent(in)    :: ice2x_wavx
    type(attrVect),   intent(in)    :: wav2x_wavx
    type(attrVect),   intent(inout) :: x2wav_wavx

    write(*,*)'wav mrged'

end subroutine mrg_x2wav

end module mrg_mod
