module comp_domain
use mct_mod
    implicit none
    integer  :: comp_id
    !---------------------------------------------------------
    ! notice that the fields of a2x and x2a maybe different
    ! but here we just assume they are same
    !---------------------------------------------------------
    integer,parameter :: CXX=4096
    character(CXX) :: fld_ar=''
    character(CXX) :: fld_ai=''
    character(*),parameter :: model_name='atm'

    public :: atm_domain_init
contains


!--- todo

subroutine atm_domain_init(mpicomm, gsMap_atmatm, domain)

  integer    , intent(in)   :: mpicomm
  type(gsMap), intent(in)   :: gsMap_atmatm
  type(gGrid), intent(inout)             :: domain

  !---local variables---
  real(8)    , pointer  :: data(:)     ! temporary
  integer , pointer  :: idata(:)    ! temporary
  integer ::ierr,lsize

  call gGrid_init(GGrid=domain, &
      CoordChars=trim("lat:lon"), &
      otherchars=trim("area:frac:mask:aream"),&
      lsize=0)


end subroutine atm_domain_init

end module comp_domain
