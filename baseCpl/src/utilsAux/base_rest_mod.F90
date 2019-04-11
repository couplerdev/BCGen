module base_rest_mod

! !USES:

   use shr_sys_mod
   use shr_mpi_mod
   use shr_file_mod
   use shr_cal_mod,  only: shr_cal_date2ymd
   use mct_mod
   use ESMF

   !use seq_avdata_mod    ! drv aVects & associated domain, fract, cdata (public read/write data)
   !use seq_diag_mct      ! diagnostic routines                          (public read/write data)
   !use seq_comm_mct      ! Sets mpi communicators, logunit and loglevel
   !use seq_cdata_mod     ! "cdata" type & methods (domain + decomp + infodata in one datatype)
   !use seq_infodata_mod  ! "infodata" gathers various control flags into one datatype
   use time_mod          ! clock & alarm routines
   use base_io           ! lower level io routines
   use global_var
   use proc_def


   implicit none

   private

! !PUBLIC TYPES:

   ! no public types

! !PUBLIC MEMBER FUNCTIONS

   public :: base_rest_read   ! read   restart data
   public :: base_rest_write  ! write  restart data

! !PUBLIC DATA MEMBERS:

   ! no public data

!EOP

   !----------------------------------------------------------------------------
   ! local data
   !----------------------------------------------------------------------------

   logical     :: iamin_CPLID            ! pe associated with CPLID
   integer(IN) :: mpicom_GLOID           ! MPI global communicator
   integer(IN) :: mpicom_CPLID           ! MPI cpl communicator

   logical     :: atm_prognostic       
   logical     :: ice_prognostic       
   logical     :: ocn_prognostic       

   integer(IN) :: info_debug = 0         ! local info_debug level

!===============================================================================
contains
!===============================================================================

subroutine base_rest_read(metaData, rest_file)

   implicit none

   type(Meta), target, intent(inout) :: metaData
   character(*),       intent(in) :: rest_file  ! restart file path/name

   integer(IN)          :: n,n1,n2,n3
   real(r8),allocatable :: ds(:)         ! for reshaping diag data for restart file
   real(r8),allocatable :: ns(:)         ! for reshaping diag data for restart file
   character(CS)        :: string
   integer(IN)          :: ierr          ! MPI error return
   type(mct_gsMap),pointer :: gsmap
   character(len=*),parameter :: subname = "(seq_rest_read) "
   type(mct_aVect),  pointer  :: atm2x_atmx
   type(mct_aVect)   :: fractions_atmx
   type(mct_aVect),  pointer  :: ice2x_icex
   type(mct_aVect)   :: fractions_icex
   type(mct_aVect),  pointer  :: ocn2x_ocnx
   type(mct_aVect)   :: fractions_ocnx

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   !----------------------------------------------------------------------------
   ! get required infodata
   !----------------------------------------------------------------------------
   iamin_CPLID  = metaData%iamin_cpl
   mpicom_gloid = metaData%mpi_glocomm
   mpicom_cplid = metaData%mpi_cpl
  
   atm2x_atmx => metaData%atm2x_atmx
   ice2x_icex => metaData%ice2x_icex
   ocn2x_ocnx => metaData%ocn2x_ocnx

   call compMeta_getInfo(metaData%atm, prognostic=atm_prognostic)
   call compMeta_getInfo(metaData%ice, prognostic=ice_prognostic)
   call compMeta_getInfo(metaData%ocn, prognostic=ocn_prognostic)
 
   if (iamin_CPLID) then
      !if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
      !if(ocn_present)then
         call compMeta_getInfo(metaData%atm, comp_gsmap=gsmap)
         call base_io_read(rest_file,gsmap,fractions_atmx,'fractions_atmx')
         call base_io_read(rest_file,gsmap,atm2x_atmx,'atm2x_atmx')
      !end if
      !if(atm_present)then
         call compMeta_getInfo(metaData%ice, comp_gsmap=gsmap)
         call base_io_read(rest_file,gsmap,fractions_icex,'fractions_icex')
         call base_io_read(rest_file,gsmap,ice2x_icex,'ice2x_icex')
      !end if
      !if(ice_present)then
         call compMeta_getInfo(metaData%ocn, comp_gsmap=gsmap)
         call base_io_read(rest_file,gsmap,fractions_ocnx,'fractions_ocnx')
         call base_io_read(rest_file,gsmap,ocn2x_ocnx,'ocn2x_ocnx')
      !end if
      
      !n = size(budg_dataG)
      !allocate(ds(n),ns(n))
      !call base_io_read(rest_file,ds,'budg_dataG')
      !call base_io_read(rest_file,ns,'budg_ns')

      !n = 0
      !do n1 = 1,size(budg_dataG,dim=1)
      !do n2 = 1,size(budg_dataG,dim=2)
      !do n3 = 1,size(budg_dataG,dim=3)
      !   n = n + 1
      !   budg_dataG(n1,n2,n3) = ds(n)
      !   budg_ns   (n1,n2,n3) = ns(n)
      !enddo
      !enddo
      !enddo
!     call shr_mpi_bcast(budg_dataG,cpl_io_root) ! not necessary, io lib does bcast

      !deallocate(ds,ns)

      !if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)

   endif

end subroutine base_rest_read

!===============================================================================

subroutine base_rest_write(metaData, EClock_d)

   implicit none

   type(Meta),  target,   intent(inout)    :: metaData
   type(ESMF_Clock)      ,intent(in)    :: EClock_d      ! driver clock
   !type(seq_timemgr_type),intent(inout) :: seq_SyncClock ! contains ptr to driver clock

   integer(IN)   :: n,n1,n2,n3,fk
   integer(IN)   :: curr_ymd         ! Current date YYYYMMDD
   integer(IN)   :: curr_tod         ! Current time-of-day (s)
   integer(IN)   :: yy,mm,dd         ! year, month, day
   character(CL) :: case_name        ! case name
   character(CL) :: cvar             ! char variable
   integer(IN)   :: ivar             ! integer variable
   real(r8)      :: rvar             ! real variable
   logical       :: whead,wdata      ! flags header/data writing
   logical       :: cdf64            ! true => create netCDF with 64 bit addressing
   logical       :: cplroot          ! root pe on cpl id
   integer(IN)   :: iun              ! unit number
   character(CL) :: rest_file        ! Local path to restart filename
   integer(IN)   :: ierr             ! MPI error return
   type(mct_gsMap),pointer :: gsmap
   type(procMeta), pointer :: my_proc

   real(r8),allocatable :: ds(:)     ! for reshaping diag data for restart file
   real(r8),allocatable :: ns(:)     ! for reshaping diag data for restart file
   character(len=*),parameter :: subname = "(base_rest_write) "
   type(mct_aVect)           :: atm2x_atmx
   type(mct_aVect)           :: fractions_atmx
   type(mct_aVect)           :: ice2x_icex
   type(mct_aVect)           :: fractions_icex
   type(mct_aVect)           :: ocn2x_ocnx
   type(mct_aVect)           :: fractions_ocnx
   character(len=*), parameter :: prefix="./archive/"


!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   !----------------------------------------------------------------------------
   ! get required infodata
   !----------------------------------------------------------------------------
   iamin_CPLID  = metaData%iamin_cpl
   mpicom_gloid = metaData%mpi_glocomm
   mpicom_cplid = metaData%mpi_cpl
   cplroot = metaData%iamroot_cpl
   my_proc => metaData%my_proc

   !atm2x_atmx => metaData%atm2x_atmx
   !ice2x_icex => metaData%ice2x_icex
   !ocn2x_ocnx => metaData%ocn2x_ocnx

   call compMeta_getInfo(metaData%atm, prognostic=atm_prognostic)
   call compMeta_getInfo(metaData%ice, prognostic=ice_prognostic)
   call compMeta_getInfo(metaData%ocn, prognostic=ocn_prognostic)
   ! Write out infodata and time manager data to restart file

   case_name = metaData%case_name
   call time_clockGetInfo( EClock_d, curr_ymd=curr_ymd, curr_tod=curr_tod)
   call shr_cal_date2ymd(curr_ymd,yy,mm,dd)
   write(rest_file,"(3a,i4.4,a,i2.2,a,i2.2,a,i5.5,a)") &
      trim(prefix),trim(case_name), '.cpl.r.', yy,'-',mm,'-',dd,'-',curr_tod,'.nc'

   ! Write driver data to restart file

   if (iamin_CPLID) then

      !if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)

      ! copy budg_dataG into 1d array
      !n = size(budg_dataG)
      !allocate(ds(n),ns(n))
      !call shr_mpi_bcast(budg_dataG,mpicom_CPLID) ! pio requires data on all pe's?

      !n = 0
      !do n1 = 1,size(budg_dataG,dim=1)
      !do n2 = 1,size(budg_dataG,dim=2)
      !do n3 = 1,size(budg_dataG,dim=3)
      !   n = n + 1
      !   ds(n) = budg_dataG(n1,n2,n3)
      !   ns(n) = budg_ns(n1,n2,n3)
      !enddo
      !enddo
      !enddo

      if (cplroot) then
         iun = shr_file_getUnit()
         cvar = metaData%case_name
         !call confMeta_getInfo(metaData%conf, restart_file=cvar)
         !call seq_infodata_GetData(infodata,restart_pfile=cvar)
         !if (loglevel > 0) write(logunit,"(3A)") subname," write rpointer file ", &
         !   trim(cvar)
         open(iun, file=cvar, form='FORMATTED')
         write(iun,'(a)') rest_file
         close(iun)
         call shr_file_freeUnit( iun )
      endif

      call shr_mpi_bcast(rest_file,mpicom_CPLID)
      call base_io_wopen(my_proc, rest_file,clobber=.true.,cdf64=cdf64)

      ! loop twice (for perf), first time write header, second time write data
      do fk = 1,2
         if (fk == 1) then
            whead = .true.
            wdata = .false.
            !call base_io_redef(rest_file)
         elseif (fk == 2) then
            call mpi_barrier(metaData%mpi_cpl, ierr)
            whead = .false.
            wdata = .true.
            call base_io_enddef(rest_file)
         else
            call shr_sys_abort('driver_write_rstart fk illegal')
         end if
         !call seq_infodata_GetData(infodata,nextsw_cday=rvar)
         call base_io_write(rest_file,rvar,'nextsw_cday',whead=whead,wdata=wdata)
         !call seq_infodata_GetData(infodata,precip_fact=rvar)
         call base_io_write(rest_file,rvar,'precip_fact',whead=whead,wdata=wdata)
         !call seq_infodata_GetData(infodata,case_name=cvar)
         call base_io_write(rest_file,trim(cvar),'case_name',whead=whead,wdata=wdata)

         call time_clockGetInfo( EClock_d, start_ymd=ivar)
         call base_io_write(rest_file,ivar,'timemgr_start_ymd',whead=whead,wdata=wdata)
         call time_clockGetInfo( EClock_d, start_tod=ivar)
         call base_io_write(rest_file,ivar,'timemgr_start_tod',whead=whead,wdata=wdata)
         call time_clockGetInfo( EClock_d, ref_ymd=ivar)
         call base_io_write(rest_file,ivar,'timemgr_ref_ymd'  ,whead=whead,wdata=wdata)
         call time_clockGetInfo( EClock_d, ref_tod=ivar)
         call base_io_write(rest_file,ivar,'timemgr_ref_tod'  ,whead=whead,wdata=wdata)
         call time_clockGetInfo( EClock_d, curr_ymd=ivar)
         call base_io_write(rest_file,ivar,'timemgr_curr_ymd' ,whead=whead,wdata=wdata)
         call time_clockGetInfo( EClock_d, curr_tod=ivar)
         call base_io_write(rest_file,ivar,'timemgr_curr_tod' ,whead=whead,wdata=wdata)

         !call base_io_write(rest_file,ds,'budg_dataG',whead=whead,wdata=wdata)
         !call base_io_write(rest_file,ns,'budg_ns',whead=whead,wdata=wdata)

         
         call compMeta_getInfo(metaData%atm, comp_gsmap=gsmap)
         !call base_io_write(rest_file,gsmap,fractions_atmx,'fractions_atmx',whead=whead,wdata=wdata)
         call base_io_write(rest_file,gsmap,metaData%atm2x_atmx,'atm2x_atmx',whead=whead,wdata=wdata)
         
         call compMeta_getInfo(metaData%ice, comp_gsmap=gsmap)
         !call base_io_write(rest_file,gsmap,fractions_icex,'fractions_icex',whead=whead,wdata=wdata)
         call base_io_write(rest_file,gsmap,metaData%ice2x_icex,'ice2x_icex',whead=whead,wdata=wdata)
         
         call compMeta_getInfo(metaData%ocn, comp_gsmap=gsmap)
         !call base_io_write(rest_file,gsmap,fractions_ocnx,'fractions_ocnx',whead=whead,wdata=wdata)
         call base_io_write(rest_file,gsmap,metaData%ocn2x_ocnx,'ocn2x_ocnx',whead=whead,wdata=wdata)
      enddo

      call base_io_close(rest_file)
      !deallocate(ds,ns)

      !if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif
   write(*,*)'========== restart file has been written =========='

end subroutine base_rest_write

!===============================================================================

end module base_rest_mod
