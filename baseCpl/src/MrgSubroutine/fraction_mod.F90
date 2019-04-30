module fraction_mod
use shr_kind_mod, only: r8 => shr_kind_r8
use proc_def
use mct_mod
use comms
implicit none

    public   fraction_atm_init
    public   fraction_ocn_init
    public   fraction_ocn_initIce
    public   fraction_ice_init
    public   fraction_lnd_init
    public   fraction_glc_init
    public   fraction_rof_init
    public   fraction_ice_update
    public   fraction_ocn_update
! 注意到其实fraction也是可以满足生成代码形式的，只要构造fraction的方式是
! 利用稀疏矩阵乘法，这也就意味着这个实现还有改进的余地

contains

subroutine fraction_atm_init(domain, fraction_atm, fraclist)

    implicit none
    type(mct_gGrid), target, intent(inout)   :: domain
    type(mct_aVect),intent(inout)    :: fraction_atm
    character(*),  intent(in)       :: fraclist
    !type(map_mod)   :: mapper_ocn2atm
    !type(map_mod)   :: mapper_lnd2atm

    type(mct_gGrid), pointer :: dom_atm
    
    integer :: lsize
    integer :: ka, kl, ko

    dom_atm=>domain
    lsize  = mct_avect_lsize(dom_atm%data) !?
    call mct_avect_init(fraction_atm, rList=fraclist, lSize=lsize)
    call mct_avect_zero(fraction_atm)
    ka = mct_avect_indexRA(fraction_atm, "afrac")
    fraction_atm%rAttr(ka, :) = 1.0_R8

end subroutine fraction_atm_init


subroutine fraction_ocn_init(domain, fraction_ocn, fraclist)

    implicit none
    type(mct_gGrid),   target, intent(in)     :: domain
    type(mct_aVect),  intent(inout)  :: fraction_ocn
    character(*),     intent(in)     :: fraclist
  
    character(*), parameter :: subName = "fraction_ocn_init"
    type(mct_ggrid), pointer :: dom
    integer :: lsize
    integer :: ko, kf

    dom => domain
    lsize = mct_avect_lsize(dom%data)
    call mct_avect_init(fraction_ocn,rList=fraclist, lsize=lsize )
    call mct_avect_zero(fraction_ocn)
    ko = mct_avect_indexRa(fraction_ocn, "ofrac", perrWith=subName)    
    kf = mct_avect_indexRa(dom%data, "frac", perrWith=subName)
    fraction_ocn%rAttr(ko,:) = dom%data%rAttr(kf,:)

end subroutine fraction_ocn_init

subroutine fraction_ocn_initIce(domain, fraction_ocn, fraclist)

    implicit none
    type(mct_gGrid),   target, intent(in)    :: domain
    type(mct_aVect),  intent(inout)  :: fraction_ocn
    character(*),     intent(in)     :: fraclist
  
    character(*), parameter :: subName = "fraction_ocn_init"
    type(mct_ggrid), pointer :: dom
    integer :: lsize
    integer :: ko, kf

    dom => domain
    lsize = mct_avect_lsize(dom%data)
    call mct_avect_init(fraction_ocn,rList=fraclist, lsize=lsize )
    call mct_avect_zero(fraction_ocn)

end subroutine fraction_ocn_initIce

subroutine fraction_ice_init(domain, fraction_ice, fraclist)


    implicit none
    type(mct_gGrid),   target, intent(in)     :: domain
    type(mct_aVect),  intent(inout)  :: fraction_ice
    character(*),     intent(in)     :: fraclist
  
    character(*), parameter :: subName = "fraction_ice_init"
    type(mct_ggrid), pointer :: dom
    integer :: lsize
    integer :: ko, kf

    dom => domain
    lsize = mct_avect_lsize(dom%data)
    call mct_avect_init(fraction_ice,rList=fraclist, lsize=lsize )
    call mct_avect_zero(fraction_ice)
    !ko = mct_avect_indexRa(fraction_ice, "ofrac", perrWith=subName)    
    kf = mct_avect_indexRa(dom%data, "frac", perrWith=subName)
    !fraction_ice%rAttr(ko,:) = dom%data%rAttr(kf,:)

end subroutine fraction_ice_init

subroutine fraction_lnd_init(domain, fraction_lnd, fraclist)

    implicit none
    type(mct_gGrid),   target, intent(in)     :: domain
    type(mct_aVect),  intent(inout)  :: fraction_lnd
    character(*),     intent(in)     :: fraclist
  
    character(*), parameter :: subName = "fraction_lnd_init"
    type(mct_ggrid), pointer :: dom
    integer :: lsize
    integer :: kk, kf

    dom => domain
    lsize = mct_avect_lsize(dom%data)
    call mct_avect_init(fraction_lnd,rList=fraclist, lsize=lsize )
    call mct_avect_zero(fraction_lnd)
    kk = mct_avect_indexRa(fraction_lnd, "lfrin", perrWith=subName)    
    kf = mct_avect_indexRa(dom%data, "frac", perrWith=subName)
    fraction_lnd%rAttr(kk,:) = dom%data%rAttr(kf,:)

end subroutine fraction_lnd_init

subroutine fraction_rof_init(domain, fraction_rof, fraclist)

    implicit none
    type(mct_gGrid),   target, intent(in)     :: domain
    type(mct_aVect),  intent(inout)  :: fraction_rof
    character(*),     intent(in)     :: fraclist
  
    character(*), parameter :: subName = "fraction_rof_init"
    type(mct_ggrid), pointer :: dom
    integer :: lsize
    integer :: ko, kf

    dom => domain
    lsize = mct_avect_lsize(dom%data)
    call mct_avect_init(fraction_rof,rList=fraclist, lsize=lsize )
    call mct_avect_zero(fraction_rof)
    ko = mct_avect_indexRa(fraction_rof, "rfrac", perrWith=subName)    
    kf = mct_avect_indexRa(dom%data, "frac", perrWith=subName)

end subroutine fraction_rof_init

subroutine fraction_glc_init(domain, fraction_glc, fraclist)

    implicit none
    type(mct_gGrid),   target, intent(in)     :: domain
    type(mct_aVect),  intent(inout)  :: fraction_glc
    character(*),     intent(in)     :: fraclist
  
    character(*), parameter :: subName = "fraction_glc_init"
    type(mct_ggrid), pointer :: dom
    integer :: lsize
    integer :: ko, kf

    dom => domain
    lsize = mct_avect_lsize(dom%data)
    call mct_avect_init(fraction_glc,rList=fraclist, lsize=lsize )
    call mct_avect_zero(fraction_glc)
    fraction_glc%rAttr(:,:) = 1.0_r8

end subroutine fraction_glc_init


subroutine fraction_ocn_update(comp, fraction_ocn)

    implicit none
    type(compMeta),   target, intent(in)     :: comp   
    type(mct_aVect),  intent(inout)  :: fraction_ocn
    integer :: lsize
    integer :: ki, kl, ka

    ! do nothing only map process needed


end subroutine fraction_ocn_update

subroutine fraction_ice_update(domain, i2x_i, fraction_ice)
    
    implicit none
    type(mct_gGrid),  target, intent(in)     :: domain
    type(mct_aVect), intent(in)     :: i2x_i
    type(mct_aVect), intent(inout)  :: fraction_ice
    character(*), parameter :: subName = "fraction_ice_update"
    type(mct_ggrid), pointer :: dom
    integer :: lsize
    integer :: ki, ko, kf
    
    dom => domain 
    call mct_aVect_copy(i2x_i, fraction_ice, "Si_ifrac","ifrac")
    ki = mct_aVect_indexRA(fraction_ice, "ifrac")
    !ko = mct_aVect_indexRA(fraction_ice, "ofrac")
    kf = mct_aVect_indexRA(dom%data, "frac", perrwith=subName)
    fraction_ice%rAttr(ki, :) = fraction_ice%rAttr(ki,:)*dom%data%rAttr(kf,:)
    fraction_ice%rAttr(ko, :) = dom%data%rAttr(kf,:) - fraction_ice%rAttr(ki,:)

end subroutine fraction_ice_update

subroutine fraction_atm_update(domain, fraction_atm)

    implicit none
    type(mct_gGrid),  target, intent(in)    :: domain
    type(mct_aVect),  intent(inout) :: fraction_atm
    character(*),  parameter :: subName = "fraction_atm_update"
    type(mct_ggrid), pointer :: dom
    integer :: lsize
    integer :: ki, ko, kl, n
    real(r8), pointer :: fcorr(:)   

    ki = mct_aVect_indexRA(fraction_atm, "ifrac")
    ko = mct_aVect_indexRA(fraction_atm, "ofrac")
    kl = mct_aVect_indexRA(fraction_atm, "lfrac")
    lsize = mct_aVect_lsize(fraction_atm)
    allocate(fcorr(lsize))
    do n = 1, lsize
        if((fraction_atm%rAttr(ki, n)+fraction_atm%rAttr(ko,n))>0.0_r8)then
            fcorr(n) = ((1.0_r8-fraction_atm%rAttr(kl, n))/(fraction_atm%rAttr(ki,n)+fraction_atm%rAttr(ko,n)))
        else 
            fcorr(n) = 0.0_r8
        end if
    end do
    fraction_atm%rAttr(ki,:) = fraction_atm%rAttr(ki,:)*fcorr(:)
    fraction_atm%rAttr(ko,:) = fraction_atm%rAttr(ko,:)*fcorr(:)
    deallocate(fcorr)


end subroutine fraction_atm_update


end module fraction_mod
