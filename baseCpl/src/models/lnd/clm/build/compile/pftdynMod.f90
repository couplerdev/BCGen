

module pftdynMod

!---------------------------------------------------------------------------
!BOP
!
! !MODULE: pftdynMod
!
! !USES:
  use spmdMod
  use clmtype
  use decompMod   , only : get_proc_bounds
  use clm_varsur  , only : pctspec
  use clm_varpar  , only : max_pft_per_col
  use clm_varctl  , only : iulog, use_c13, use_c14
  use shr_sys_mod , only : shr_sys_flush
  use shr_kind_mod, only : r8 => shr_kind_r8
  use abortutils  , only : endrun
  use ncdio_pio
!
! !DESCRIPTION:
! Determine pft weights at current time using dynamic landuse datasets.
! ASSUMES that only have one dynamic landuse dataset.
!
! !PUBLIC TYPES:
  implicit none
  private
  save
  public :: pftdyn_init
  public :: pftdyn_interp
  public :: pftdyn_wbal_init
  public :: pftdyn_wbal
!
! !REVISION HISTORY:
! Created by Peter Thornton
! slevis modified to handle CNDV and crop model
! 19 May 2009: PET - modified to handle harvest fluxes
! F. Li and S. Levis (11/06/12)

!EOP
!
! ! PRIVATE TYPES
  integer , pointer   :: yearspft(:)
  real(r8), pointer   :: wtpft1(:,:)   
  real(r8), pointer   :: wtpft2(:,:)
  real(r8), pointer   :: harvest(:)   
  real(r8), pointer   :: wtcol_old(:)
  integer :: nt1
  integer :: nt2
  integer :: ntimes
  logical :: do_harvest
  type(file_desc_t)  :: ncid   ! netcdf id
  ! default multiplication factor for epsilon for error checks
  real(r8), private, parameter :: eps_fact = 2._r8
!---------------------------------------------------------------------------

contains
  
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: pftdyn_init
!
! !INTERFACE:
  subroutine pftdyn_init()
!
! !DESCRIPTION:
! Initialize dynamic landuse dataset (position it to the right time samples
! that bound the initial model date)
!
! !USES:
    use clm_time_manager, only : get_curr_date
    use clm_varctl  , only : fpftdyn
    use clm_varpar  , only : numpft, maxpatch_pft, numurbl
    use fileutils   , only : getfil
!
! !ARGUMENTS:
    implicit none
!
!
! !LOCAL VARIABLES:
!EOP
    integer  :: i,j,m,n,g,nl                    ! indices
    real(r8) :: sumpct                          ! sum for error check
    integer  :: varid                           ! netcdf ids
    integer  :: year                            ! year (0, ...) for nstep+1
    integer  :: mon                             ! month (1, ..., 12) for nstep+1
    integer  :: day                             ! day of month (1, ..., 31) for nstep+1
    integer  :: sec                             ! seconds into current date for nstep+1
    integer  :: ier, ret                        ! error status
    logical  :: found                           ! true => input dataset bounding dates found
    logical  :: readvar	                        ! true => variable is on input dataset
    integer  :: begg,endg                       ! beg/end indices for land gridcells
    integer  :: begl,endl                       ! beg/end indices for land landunits
    integer  :: begc,endc                       ! beg/end indices for land columns
    integer  :: begp,endp                       ! beg/end indices for land pfts
    real(r8), pointer :: pctgla(:)          ! percent of gcell is glacier
    real(r8), pointer :: pctlak(:)          ! percent of gcell is lake
    real(r8), pointer :: pctwet(:)          ! percent of gcell is wetland
    real(r8), pointer :: pcturb(:,:)        ! percent of gcell is urbanized
    real(r8), pointer :: pcturb_tot(:)      ! percent of grid cell is urban (sum over density classes)
    character(len=256) :: locfn                 ! local file name
    character(len= 32) :: subname='pftdyn_init' ! subroutine name
 !-----------------------------------------------------------------------
    call get_proc_bounds(begg,endg,begl,endl,begc,endc,begp,endp)

    ! Error check

    if ( maxpatch_pft /= numpft+1 )then
       call endrun( subname//' maxpatch_pft does NOT equal numpft+1 -- this is invalid for dynamic PFT case' )
    end if

    allocate(pctgla(begg:endg),pctlak(begg:endg))
    allocate(pctwet(begg:endg),pcturb(begg:endg,numurbl),pcturb_tot(begg:endg))

    ! Set pointers into derived type


    ! pctspec must be saved between time samples
    ! position to first time sample - assume that first time sample must match starting date
    ! check consistency -  special landunits, grid, frac and mask
    ! only do this once

    ! read data PCT_PFT corresponding to correct year

    allocate(wtpft1(begg:endg,0:numpft), wtpft2(begg:endg,0:numpft), stat=ier)
    if (ier /= 0) then
       call endrun( subname//' allocation error for wtpft1, wtpft2' )
    end if
    
    allocate(harvest(begg:endg),stat=ier)
    if (ier /= 0) then
       call endrun( subname//' allocation error for harvest')
    end if

    allocate(wtcol_old(begp:endp),stat=ier)
    if (ier /= 0) then
       call endrun( subname//' allocation error for wtcol_old' )
    end if

    if (masterproc) then
       write(iulog,*) 'Attempting to read pft dynamic landuse data .....'
    end if

    ! Obtain file
    call getfil (fpftdyn, locfn, 0)
    call ncd_pio_openfile (ncid, locfn, 0)

    ! Obtain pft years from dynamic landuse file
    
    call ncd_inqdid(ncid, 'time', varid)
    call ncd_inqdlen(ncid, varid, ntimes)

    ! Consistency check
    
    call check_dim(ncid, 'lsmpft', numpft+1)

    allocate (yearspft(ntimes), stat=ier)
    if (ier /= 0) then
       write(iulog,*) subname//' allocation error for yearspft'; call endrun()
    end if

    call ncd_io(ncid=ncid, varname='YEAR', flag='read', data=yearspft)

    call ncd_io(ncid=ncid, varname='PCT_WETLAND', flag='read', data=pctwet, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( trim(subname)//' ERROR: PCT_WETLAND NOT on pftdyn file' )

    call ncd_io(ncid=ncid, varname= 'PCT_LAKE', flag='read', data=pctlak, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( trim(subname)//' ERROR: PCT_LAKE NOT on pftdyn file' )

    call ncd_io(ncid=ncid, varname= 'PCT_GLACIER', flag='read', data=pctgla, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( trim(subname)//' ERROR: PCT_GLACIER NOT on pftdyn file' )

    call ncd_io(ncid=ncid, varname= 'PCT_URBAN'  , flag='read', data=pcturb, &
         dim1name=grlnd, readvar=readvar)
    if (.not. readvar) call endrun( trim(subname)//' ERROR: PCT_URBAN NOT on pftdyn file' )
    pcturb_tot(:) = 0._r8
    do n = 1, numurbl
       do nl = begg,endg
          pcturb_tot(nl) = pcturb_tot(nl) + pcturb(nl,n)
       enddo
    enddo

    ! Consistency check
    do g = begg,endg
    !   this was causing a fail, even though values are the same to within 1e-15
    !   if (pctlak(g)+pctwet(g)+pcturb(g)+pctgla(g) /= pctspec(g)) then 
       if (abs((pctlak(g)+pctwet(g)+pcturb_tot(g)+pctgla(g))-pctspec(g)) > 1e-13_r8) then 
          write(iulog,*) subname//'mismatch between input pctspec = ',&
                     pctlak(g)+pctwet(g)+pcturb_tot(g)+pctgla(g),&
                    ' and that obtained from surface dataset ', pctspec(g),' at g= ',g
           call endrun()
       end if
    end do

    ! Determine if current date spans the years
    ! If current year is less than first dynamic PFT timeseries year,
    ! then use the first year from dynamic pft file for both nt1 and nt2,
    ! forcing constant weights until the model year enters the dynamic
    ! pft dataset timeseries range.
    ! If current year is equal to or greater than the last dynamic pft
    ! timeseries year, then use the last year for both nt1 and nt2, 
    ! forcing constant weights for the remainder of the simulation.
    ! This mechanism permits the introduction of a dynamic pft period in the middle
    ! of a simulation, with constant weights before and after the dynamic period.
    ! PET: harvest - since harvest is specified as a rate for each year, this
    ! approach will not work. Instead, need to seta flag that indicates harvest is
    ! zero for the period before the beginning and after the end of the dynpft timeseries.

    call get_curr_date(year, mon, day, sec)

    if (year < yearspft(1)) then
       nt1 = 1
       nt2 = 1
       do_harvest = .false.
    else if (year >= yearspft(ntimes)) then
       nt1 = ntimes
       nt2 = ntimes
       do_harvest = .false.
    else
       found = .false.
       do n = 1,ntimes-1 
          if (year == yearspft(n)) then
             nt1 = n
             nt2 = nt1 + 1
             found = .true.
             do_harvest = .true.
          end if   
       end do
       if (.not. found) then
          write(iulog,*) subname//' error: model year not found in pftdyn timeseries'
          write(iulog,*)'model year = ',year
          call endrun()
       end if
    end if

    ! Get pctpft time samples bracketing the current time

    call pftdyn_getdata(nt1, wtpft1, begg,endg,0,numpft)
    call pftdyn_getdata(nt2, wtpft2, begg,endg,0,numpft)
    

    ! convert weights from percent to proportion
    do m = 0,numpft
       do g = begg,endg
          wtpft1(g,m) = wtpft1(g,m)/100._r8
          wtpft2(g,m) = wtpft2(g,m)/100._r8
       end do
    end do
       
    deallocate(pctgla,pctlak,pctwet,pcturb,pcturb_tot)

  end subroutine pftdyn_init

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: pftdyn_interp
!
! !INTERFACE:
  subroutine pftdyn_interp()
!
! !DESCRIPTION:
! Time interpolate dynamic landuse data to get pft weights for model time
! Note that harvest data are stored as rates (not weights) and so time interpolation is 
! not necessary - the harvest rate is held constant through the year.  This is consistent with
! the treatment of changing PFT weights, where interpolation of the annual endpoint weights leads to 
! a constant rate of change in PFT weight through the year, with abrupt changes in the rate at
! annual boundaries. This routine is still used to get the next harvest time slice, when needed.
! This routine is also used to turn off the harvest switch when the model year runs past the end of
! the dynpft time series.
!
! !USES:
    use clm_time_manager, only : get_curr_date, get_curr_calday, &
                                 get_days_per_year
    use clm_varcon      , only : istsoil
    use clm_varcon      , only : istcrop
    use clm_varpar      , only : numpft
    implicit none
!
!
! !LOCAL VARIABLES:
!EOP
!
! !ARGUMENTS:
    integer  :: begg,endg        ! beg/end indices for land gridcells
    integer  :: begl,endl        ! beg/end indices for land landunits
    integer  :: begc,endc        ! beg/end indices for land columns
    integer  :: begp,endp        ! beg/end indices for land pfts
    integer  :: i,j,m,p,l,g,c    ! indices
    integer  :: year             ! year (0, ...) for nstep+1
    integer  :: mon              ! month (1, ..., 12) for nstep+1
    integer  :: day              ! day of month (1, ..., 31) for nstep+1
    integer  :: sec              ! seconds into current date for nstep+1
    real(r8) :: cday             ! current calendar day (1.0 = 0Z on Jan 1)
    real(r8) :: days_per_year    ! days per year
    integer  :: ier              ! error status
    integer  :: lbc,ubc
    real(r8) :: wt1              ! time interpolation weights
    real(r8), pointer :: wtpfttot1(:)            ! summation of pft weights for renormalization
    real(r8), pointer :: wtpfttot2(:)            ! summation of pft weights for renormalization
    real(r8), parameter :: wtpfttol = 1.e-10     ! tolerance for pft weight renormalization
    character(len=32) :: subname='pftdyn_interp' ! subroutine name
    logical  :: readvar    ! F. Li and S. Levis
!-----------------------------------------------------------------------

    call get_proc_bounds(begg,endg,begl,endl,begc,endc,begp,endp)

    ! Set pointers into derived type

    allocate(wtpfttot1(begc:endc),wtpfttot2(begc:endc))
    wtpfttot1(:) = 0._r8
    wtpfttot2(:) = 0._r8

    ! Interpolat pctpft to current time step - output in pctpft_intp
    ! Map interpolated pctpft to subgrid weights
    ! assumes that maxpatch_pft = numpft + 1, that each landunit has only 1 column, 
    ! SCAM and CNDV have not been defined, and create_croplandunit = .false.

    ! If necessary, obtain new time sample

    ! Get current date

    call get_curr_date(year, mon, day, sec)

    ! Obtain new time sample if necessary.
    ! The first condition is the regular crossing of a year boundary
    ! when within the dynpft timeseries range. The second condition is
    ! the case of the first entry into the dynpft timeseries range from
    ! an earlier period of constant weights.

    if (year > yearspft(nt1) .or. (nt1 == 1 .and. nt2 == 1 .and. year == yearspft(1))) then

       if (year >= yearspft(ntimes)) then
          nt1 = ntimes
          nt2 = ntimes
       else
          nt1        = nt2
          nt2        = nt2 + 1
          do_harvest = .true.
       end if
       
       if (year > yearspft(ntimes)) then
          do_harvest = .false.
       endif
       
       if (nt2 > ntimes .and. masterproc) then
          write(iulog,*)subname,' error - current year is past input data boundary'
       end if
       
       do m = 0,numpft
          do g = begg,endg
             wtpft1(g,m) = wtpft2(g,m)
          end do
       end do

       call pftdyn_getdata(nt2, wtpft2, begg,endg,0,numpft)


       do m = 0,numpft
          do g = begg,endg
             wtpft2(g,m) = wtpft2(g,m)/100._r8
          end do
       end do
    
    end if  ! end of need new data if-block 

    ! Interpolate pft weight to current time

    cday          = get_curr_calday() 
    days_per_year = get_days_per_year()

    wt1 = ((days_per_year + 1._r8) - cday)/days_per_year

    do p = begp,endp
       c = pft%column(p)
       g = pft%gridcell(p)
       l = pft%landunit(p)
       if (lun%itype(l) == istsoil .or. lun%itype(l) == istcrop) then
          m = pft%itype(p)
          wtcol_old(p)      = pft%wtcol(p)
!         --- recoded for roundoff performance, tcraig 3/07 from k.lindsay
!         pft%wtgcell(p)   = wtpft1(g,m)*wt1 + wtpft2(g,m)*wt2
          wtpfttot1(c) = wtpfttot1(c)+pft%wtgcell(p)    
          pft%wtgcell(p)   = wtpft2(g,m) + wt1*(wtpft1(g,m)-wtpft2(g,m))
          pft%wtlunit(p)   = pft%wtgcell(p) / lun%wtgcell(l)
          pft%wtcol(p)     = pft%wtgcell(p) / lun%wtgcell(l)
          wtpfttot2(c) = wtpfttot2(c)+pft%wtgcell(p)
       end if

    end do

!   Renormalize pft weights so that sum of pft weights relative to grid cell 
!   remain constant even as land cover changes.  Doing this eliminates 
!   soil balance error warnings.  (DML, 4/8/2009)
    do p = begp,endp
       c = pft%column(p)
       g = pft%gridcell(p)
       l = pft%landunit(p)
       if (lun%itype(l) == istsoil .or. lun%itype(l) == istcrop) then
          if (wtpfttot2(c) /= 0 .and. &
              abs(wtpfttot1(c)-wtpfttot2(c)) > wtpfttol) then
             pft%wtgcell(p)   = (wtpfttot1(c)/wtpfttot2(c))*pft%wtgcell(p)
             pft%wtlunit(p)   = pft%wtgcell(p) / lun%wtgcell(l)
             pft%wtcol(p)     = pft%wtgcell(p) / lun%wtgcell(l)
          end if
       end if

    end do
   
    deallocate(wtpfttot1,wtpfttot2) 
    
  end subroutine pftdyn_interp

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: pftdyn_getdata
!
! !INTERFACE:
  subroutine pftdyn_getdata(ntime, pctpft, begg, endg, pft0, maxpft)
!
! !DESCRIPTION:
! Obtain dynamic landuse data (pctpft) and make sure that
! percentage of PFTs sum to 100% cover for vegetated landunit
!
! !USES:
    use clm_varpar  , only : numpft
!
! !ARGUMENTS:
    implicit none
    integer , intent(in)  :: ntime
    integer , intent(in)  :: begg,endg,pft0,maxpft
    real(r8), intent(out) :: pctpft(begg:endg,pft0:maxpft)
!
!
! !LOCAL VARIABLES:
!EOP
    integer  :: i,j,m,n
    integer  :: err, ierr, ret
    real(r8) :: sumpct,sumerr                     ! temporary
    real(r8), pointer :: arrayl(:,:)              ! temporary array
    logical  :: readvar
   
    character(len=32) :: subname='pftdyn_getdata' ! subroutine name
!-----------------------------------------------------------------------
    
    allocate(arrayl(begg:endg,pft0:maxpft))	
    call ncd_io(ncid=ncid, varname= 'PCT_PFT', flag='read', data=arrayl, &
         dim1name=grlnd, nt=ntime, readvar=readvar)
    pctpft(begg:endg,pft0:maxpft) = arrayl(begg:endg,pft0:maxpft)
    deallocate(arrayl)		
    if (.not. readvar) call endrun( trim(subname)//' ERROR: PCT_PFT NOT on pftdyn file' )

    err = 0
    do n = begg,endg
       ! THESE CHECKS NEEDS TO BE THE SAME AS IN surfrdMod.F90!
       if (pctspec(n) < 100._r8 * (1._r8 - eps_fact*epsilon(1._r8))) then  ! pctspec not within eps_fact*epsilon of 100
          sumpct = 0._r8
          do m = 0, numpft
             sumpct = sumpct + pctpft(n,m) * 100._r8/(100._r8-pctspec(n))
          end do
          if (abs(sumpct - 100._r8) > 0.1e-4_r8) then
             err = 1; ierr = n; sumerr = sumpct
          end if
          if (sumpct < -0.000001_r8) then
             err = 2; ierr = n; sumerr = sumpct
          end if
       end if
    end do
    if (err == 1) then
       write(iulog,*) subname,' error: sum(pct) over numpft+1 is not = 100.',sumerr,ierr,pctspec(ierr),pctpft(ierr,:)
       call endrun()
    else if (err == 2) then
       write(iulog,*)subname,' error: sum(pct) over numpft+1 is < 0.',sumerr,ierr,pctspec(ierr),pctpft(ierr,:)
       call endrun()
    end if
    
  end subroutine pftdyn_getdata


!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: pftdyn_wbal_init
!
! !INTERFACE:
  subroutine pftdyn_wbal_init( begc, endc )
!
! !DESCRIPTION:
! initialize the column-level mass-balance correction term.
! Called in every timestep.
!
! !USES:
!
! !ARGUMENTS:
    implicit none
    integer, intent(IN)  :: begc, endc    ! proc beginning and ending column indices
!
!
! !LOCAL VARIABLES:
!EOP
    integer  :: c             ! indices
!-----------------------------------------------------------------------

    ! Set pointers into derived type


    ! set column-level canopy water mass balance correction flux
    ! term to 0 at the beginning of every timestep
    
    do c = begc,endc
       cwf%h2ocan_loss(c) = 0._r8
    end do
    
  end subroutine pftdyn_wbal_init

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: pftdyn_wbal
!
! !INTERFACE:
  subroutine pftdyn_wbal( begg, endg, begc, endc, begp, endp )
!
! !DESCRIPTION:
! modify pft-level state and flux variables to maintain water balance with
! dynamic pft-weights.
! Canopy water balance does not need to consider harvest fluxes, since pft weights are
! not affected by harvest.
!
! !USES:
    use clm_varcon  , only : istsoil
    use clm_varcon  , only : istcrop
    use clm_time_manager, only : get_step_size
!
! !ARGUMENTS:
    implicit none
    integer, intent(IN)  :: begg     ! beg indices for land gridcells
    integer, intent(IN)  :: endg     ! end indices for land gridcells
    integer, intent(IN)  :: begc     ! beg indices for land columns
    integer, intent(IN)  :: endc     ! end indices for land columns
    integer, intent(IN)  :: begp     ! beg indices for land plant function types
    integer, intent(IN)  :: endp     ! end indices for land plant function types
!
!
! !LOCAL VARIABLES:
!EOP
    integer  :: pi,p,c,l,g    ! indices
    integer  :: ier           ! error code
    real(r8) :: dtime         ! land model time step (sec)
    real(r8) :: dwt           ! change in pft weight (relative to column)
    real(r8) :: init_h2ocan   ! initial canopy water mass
    real(r8) :: new_h2ocan    ! canopy water mass after weight shift
    real(r8), allocatable :: loss_h2ocan(:) ! canopy water mass loss due to weight shift
    character(len=32) :: subname='pftdyn_wbal' ! subroutine name
!-----------------------------------------------------------------------

    ! Set pointers into derived type


    ! Allocate loss_h2ocan
    allocate(loss_h2ocan(begp:endp), stat=ier)
    if (ier /= 0) then
          write(iulog,*)subname,' allocation error for loss_h2ocan'; call endrun()
    end if

    ! Get time step

    dtime = get_step_size()

    ! set column-level canopy water mass balance correction flux
    ! term to 0 at the beginning of every weight-shifting timestep

    do c = begc,endc
       cwf%h2ocan_loss(c) = 0._r8 ! is this OR pftdyn_wbal_init redundant?
    end do

    do p = begp,endp
       l = pft%landunit(p)
       loss_h2ocan(p) = 0._r8

       if (lun%itype(l) == istsoil .or. lun%itype(l) == istcrop) then

          ! calculate the change in weight for the timestep
          dwt = pft%wtcol(p)-wtcol_old(p)
  
          if (dwt > 0._r8) then
          
             ! if the pft gained weight, then the 
             ! initial canopy water state is redistributed over the
             ! new (larger) area, conserving mass.

             pws%h2ocan(p) = pws%h2ocan(p) * (wtcol_old(p)/pft%wtcol(p))
          
          else if (dwt < 0._r8) then
          
             ! if the pft lost weight on the timestep, then the canopy water
             ! mass associated with the lost weight is directed to a 
             ! column-level flux term that gets added to the precip flux
             ! for every pft calculation in Hydrology1()
             
             init_h2ocan = pws%h2ocan(p) * wtcol_old(p)
             loss_h2ocan(p) = pws%h2ocan(p) * (-dwt)
             new_h2ocan = init_h2ocan - loss_h2ocan(p)
             if (abs(new_h2ocan) < 1e-8_r8) then
                new_h2ocan = 0._r8
                loss_h2ocan(p) = init_h2ocan
             end if
             if (pft%wtcol(p) /= 0._r8) then  
                pws%h2ocan(p) = new_h2ocan/pft%wtcol(p)
             else
                pws%h2ocan(p) = 0._r8
                loss_h2ocan(p) = init_h2ocan
             end if 
       

          end if

       end if
    end do

    do pi = 1,max_pft_per_col
       do c = begc,endc
          if (pi <= col%npfts(c)) then
             p = col%pfti(c) + pi - 1
             cwf%h2ocan_loss(c) = cwf%h2ocan_loss(c) + loss_h2ocan(p)/dtime
          end if
       end do
    end do

    ! Deallocate loss_h2ocan
    deallocate(loss_h2ocan)
    
  end subroutine pftdyn_wbal
  



end module pftdynMod
