module base_domain_mod

use shr_kind_mod,   only: r8=>shr_kind_r8, CL=>shr_kind_cl
use shr_sys_mod,    only: shr_sys_flush, shr_sys_abort
use shr_mpi_mod,    only: shr_mpi_min, shr_mpi_max

use mct_mod
use global_var
use type_def
use proc_def

    implicit none
    public :: domain_areafactinit

contains
subroutine domain_areafactinit(comp, mdl2drv, drv2mdl)
    implicit none
    type(compMeta), target, intent(in) :: comp
    real(r8), pointer                  :: mdl2drv(:)
    real(r8), pointer                  :: drv2mdl(:)

    type(mct_gGrid), pointer :: domain
    integer                  :: ID
    integer                  :: mpicom
    !logical                  :: iamroot
    integer                  :: j1, j2, m1, n, rcode
    integer                  :: gridsize, m2dsize, d2msize
    real(r8)                 :: rmin1, rmax1, rmin, rmax
    real(r8)                 :: rmask, rarea, raream
    character(len=*), parameter :: subName = "(domain_araefactinit)"

    ID = comp%ID
    domain => comp%domain
    mpicom = comp%comm
    !iamroot = comp%iamroot
    
    gridsize = mct_gGrid_lsize(domain)
    allocate(drv2mdl(gridsize), mdl2drv(gridsize), stat=rcode)
    if(rcode /= 0)call shr_sys_abort(subname//"allocate area correction factors")

    j1 = mct_gGrid_indexRA(domain, "area", dieWith=subName)
    j2 = mct_gGrid_indexRA(domain, "aream", dieWith=subName)
    m1 = mct_gGrid_indexRA(domain, "mask", dieWith=subName)
 
    mdl2drv(:) = 1.0_R8
    drv2mdl(:) = 1.0_R8

    do n=1, gridsize
        rmask = domain%data%rAttr(m1, n)
        rarea = domain%data%rAttr(j1, n)
        raream = domain%data%rAttr(j2, n)
        if(abs(rmask)>=1.0e-6)then
            if(rarea*raream /= 0.0_R8)then
                mdl2drv(n) = rarea/raream
                drv2mdl(n) = 1.0_R8/mdl2drv(n)
            else
                mdl2drv(n) = 1.0_R8
                drv2mdl(n) = 1.0_R8
                !write(logunit, *) trim(subname), 'ERROR area, aream= ', &
                !      rarea, raream, ' in ', n, gridsize
                !call shr_sys_flush(logunit)
                !call shr_sys_abort()
            end if
        end if
    end do

    rmin1 = minval(mdl2drv)
    rmax1 = maxval(mdl2drv)
    call shr_mpi_min(rmin1, rmin, mpicom)
    call shr_mpi_max(rmax1, rmax, mpicom)

    rmin1 = minval(drv2mdl)
    rmax1 = maxval(drv2mdl)
    call shr_mpi_min(rmin1, rmin, mpicom)
    call shr_mpi_max(rmax1, rmax, mpicom)


end subroutine domain_areafactinit




end module base_domain_mod
