

module CLMVICMapMod

!-----------------------------------------------------------------------
!BOP
!
! !MODULE: CLMVICMapMod
!
! !DESCRIPTION:
! Performs  the mapping from CLM layers to VIC layers
! Specifically, 10 (or 23 when more_vertlayers == .true.) 
! CLM hydrologically active soil layers are mapped to three VIC layers
! by assigning the first nlvic(1) layers to VIC layer 1
!              the next nlvic(2) layers  to VIC alyer 2
!              and the remaining to VIC layer 3
!
! !USES:
  use shr_kind_mod, only: r8 => shr_kind_r8
!
! !PUBLIC TYPES:
  implicit none
  save
!
! !PUBLIC MEMBER FUNCTIONS:
end module CLMVICMapMod
