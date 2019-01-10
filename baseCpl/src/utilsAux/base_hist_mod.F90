
module base_hist_mod

! !USES:

   use shr_kind_mod,      only: R8 => SHR_KIND_R8, IN => SHR_KIND_IN
   use shr_kind_mod,      only: CL => SHR_KIND_CL, CS => SHR_KIND_CS
   use shr_sys_mod,       only: shr_sys_abort, shr_sys_flush
   use shr_cal_mod,       only: shr_cal_date2ymd
   use mct_mod           ! adds mct_ prefix to mct lib
   use ESMF

   use proc_def
   use base_io
   use time_mod

  
   implicit none

   private

! !PUBLIC TYPES:
  
   ! no public types

! !PUBLIC MEMBER FUNCTIONS

   public :: base_hist_write     ! write instantaneous hist file
   public :: base_hist_writeavg  ! write time-avg      hist file
   !public :: base_hist_writeaux  ! write auxiliary     hist files
   public :: base_hist_spewav    ! write avs to history file for debugging

! !PUBLIC DATA MEMBERS:

   ! no public data

!EOP

   !----------------------------------------------------------------------------
   ! local/module data
   !----------------------------------------------------------------------------

   logical     :: iamin_CPLID            ! pe associated with CPLID
   integer(IN) :: mpicom_gloid           ! MPI global communicator
   integer(IN) :: mpicom_cplid           ! MPI cpl communicator

   !integer(IN) :: nthreads_GLOID         ! OMP global number of threads
   !integer(IN) :: nthreads_CPLID         ! OMP cpl number of threads
   !logical     :: drv_threading          ! driver threading control

   logical     :: atm_prognostic         ! .true.  => atm comp expects input
   logical     :: ocn_prognostic         ! .true.  => ocn comp expects input

   logical     :: cdf64                  ! true => use 64 bit addressing in netCDF files

   !--- domain equivalent 2d grid size ---
   integer(IN) :: atm_nx, atm_ny         ! nx,ny of 2d grid, if known
   integer, parameter :: num_inst_atm = 1
   integer(IN) :: ocn_nx, ocn_ny         ! nx,ny of 2d grid, if known
   integer, parameter :: num_inst_ocn = 1
  
   character(*), parameter :: time_histavg_type = "never"

   integer(IN) :: info_debug = 0         ! local info_debug level

!===============================================================================
contains
!===============================================================================

subroutine base_hist_write(metaData, EClock_d)

   implicit none

   type(Meta), target,  intent(inout) :: metaData
   type (ESMF_Clock),   intent(in) :: EClock_d   ! driver clock

   integer(IN)   :: curr_ymd     ! Current date YYYYMMDD
   integer(IN)   :: curr_tod     ! Current time-of-day (s)
   integer(IN)   :: start_ymd    ! Starting date YYYYMMDD
   integer(IN)   :: start_tod    ! Starting time-of-day (s)
   real(R8)      :: curr_time    ! Time interval since reference time
   integer(IN)   :: yy,mm,dd     ! year, month, day
   integer(IN)   :: fk           ! index
   character(CL) :: time_units   ! units of time variable
   character(CL) :: calendar     ! calendar type
   character(CL) :: case_name    ! case name
   character(CL) :: hist_file    ! Local path to history filename
   integer(IN)   :: lsize        ! local size of an aVect
   real(R8)      :: tbnds(2)     ! CF1.0 time bounds
   logical       :: whead,wdata  ! for writing restart/history cdf files
   type(mct_gsMap),pointer :: gsmap
   type(mct_aVect), pointer :: atm2x_atmx
   type(mct_aVect), pointer :: x2atm_atmx
   type(mct_aVect)          :: fractions_atmx   ! need to support latter
   type(mct_gGrid)          :: dom_atmx
   type(mct_aVect), pointer :: ocn2x_ocnx
   type(mct_aVect), pointer :: x2ocn_ocnx
   type(mct_aVect)          :: fractions_ocnx   ! need to support latter
   type(mct_gGrid)          :: dom_ocnx
   type(procMeta), pointer :: my_proc
 
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   !----------------------------------------------------------------------------
   ! get required metaData desc
   !----------------------------------------------------------------------------
   
   iamin_CPLID  = metaData%iamin_cpl
   mpicom_gloid = metaData%mpi_glocomm
   mpicom_cplid = metaData%mpi_cpl

   atm2x_atmx => metaData%atm2x_atmx
   x2atm_atmx => metaData%x2atm_atmx
   ocn2x_ocnx => metaData%ocn2x_ocnx
   x2ocn_ocnx => metaData%x2ocn_ocnx
   my_proc => metaData%my_proc

   call compMeta_getInfo(metaData%atm, prognostic=atm_prognostic, case_name = case_name, domain=dom_atmx)
   call compMeta_getInfo(metaData%ocn, prognostic=ocn_prognostic, case_name = case_name, domain=dom_ocnx)


   !--- Get current date from clock needed to label the history pointer file ---

   call time_clockGetInfo( EClock_d, curr_ymd=curr_ymd, curr_tod=curr_tod, &
        start_ymd=start_ymd, start_tod=start_tod, curr_time=curr_time, &
        calendar=calendar)
   call shr_cal_date2ymd(curr_ymd,yy,mm,dd)
   write(hist_file,"(2a,i4.4,a,i2.2,a,i2.2,a,i5.5,a)") &
      trim(case_name), '.cpl.hi.', yy,'-',mm,'-',dd,'-',curr_tod,'.nc'

   time_units = 'days since ' &
        // base_io_date2yyyymmdd(start_ymd) // ' ' // base_io_sec2hms(start_tod)

   if (iamin_CPLID) then

      !if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
      call base_io_wopen(my_proc, hist_file,clobber=.true.,cdf64=cdf64)


      ! loop twice, first time write header, second time write data for perf

      do fk = 1,2
         if (fk == 1) then
            whead = .true.
            wdata = .false.
         elseif (fk == 2) then
            whead = .false.
            wdata = .true.
            call base_io_enddef(hist_file)
         else
            call shr_sys_abort('seq_hist_write fk illegal')
         end if

         tbnds = curr_time
!------- tcx nov 2011 tbnds of same values causes problems in ferret
         if (tbnds(1) >= tbnds(2)) then
            call base_io_write(hist_file,&
                              time_units=time_units,time_cal=calendar,time_val=curr_time,&
                              whead=whead,wdata=wdata)
         else
            call base_io_write(hist_file,&
                              time_units=time_units,time_cal=calendar,time_val=curr_time,&
                              whead=whead,wdata=wdata,tbnds=tbnds)
         endif

         !if (atm_present) then
            call compMeta_GetInfo(metaData%atm, comp_gsmap=gsmap)
            call base_io_write(hist_file,gsmap,dom_atmx%data,'dom_atmx', &
                              nx=atm_nx,ny=atm_ny,nt=1,whead=whead,wdata=wdata,pre='domatm')
            call base_io_write(hist_file,gsmap,fractions_atmx,'fractions_atmx', &
                              nx=atm_nx,ny=atm_ny,nt=1,whead=whead,wdata=wdata,pre='fracatm')
            call base_io_write(hist_file,gsmap,x2atm_atmx,'x2atm_ax', &
                              nx=atm_nx,ny=atm_ny,nt=1,whead=whead,wdata=wdata,pre='x2atm')
            call base_io_write(hist_file,gsmap,atm2x_atmx,'atm2x_atmx', &
                              nx=atm_nx,ny=atm_ny,nt=1,whead=whead,wdata=wdata,pre='atm2x')
         !endif
         !if (ocn_present) then
            call compMeta_GetInfo(metaData%ocn, comp_gsmap=gsmap)
            call base_io_write(hist_file,gsmap,dom_ocnx%data,'dom_ocnx', &
                              nx=ocn_nx,ny=ocn_ny,nt=1,whead=whead,wdata=wdata,pre='domocn')
            call base_io_write(hist_file,gsmap,fractions_ocnx,'fractions_ocnx', &
                              nx=ocn_nx,ny=ocn_ny,nt=1,whead=whead,wdata=wdata,pre='fracocn')
            call base_io_write(hist_file,gsmap,x2ocn_ocnx,'x2ocn_ax', &
                              nx=ocn_nx,ny=ocn_ny,nt=1,whead=whead,wdata=wdata,pre='x2ocn')
            call base_io_write(hist_file,gsmap,ocn2x_ocnx,'ocn2x_ocnx', &
                              nx=ocn_nx,ny=ocn_ny,nt=1,whead=whead,wdata=wdata,pre='ocn2x')
         !endif
      end do
      call base_io_close(hist_file)
      
   endif

end subroutine base_hist_write

!===============================================================================

subroutine base_hist_writeavg(metaData, EClock_d,write_now)

   implicit none

   type(Meta), target,  intent(inout) :: metaData
   type (ESMF_Clock),   intent(in) :: EClock_d   ! driver clock
   logical          ,   intent(in) :: write_now  ! write or accumulate

   integer(IN)        :: curr_ymd     ! Current date YYYYMMDD
   integer(IN)        :: curr_tod     ! Current time-of-day (s)
   integer(IN)        :: prev_ymd     ! Previous date YYYYMMDD
   integer(IN)        :: prev_tod     ! Previous time-of-day (s)
   integer(IN)        :: start_ymd    ! Starting date YYYYMMDD
   integer(IN)        :: start_tod    ! Starting time-of-day (s)
   real(R8)           :: curr_time    ! Time interval since reference time
   real(R8)           :: prev_time    ! Time interval since reference time
   real(R8)           :: avg_time     ! Average time of tavg
   integer(IN)        :: yy,mm,dd     ! year, month, day
   integer(IN)        :: fk           ! index
   character(CL)      :: time_units   ! units of time variable
   character(CL)      :: calendar     ! calendar type
   integer(IN)        :: lsize        ! local size of an aVect
   character(CL)      :: case_name    ! case name
   character(CL)      :: hist_file    ! Local path to history filename
   logical                 :: whead,wdata  ! flags write header vs. data
   integer(IN)             :: iidx ! component instance counter
   type(mct_gsMap),pointer :: gsmap

   type(mct_aVect),save  :: atm2x_atmx_avg(num_inst_atm)   ! tavg aVect/bundle
   type(mct_aVect),save  :: x2atm_atmx_avg(num_inst_atm)
   type(mct_aVect),save  :: ocn2x_ocnx_avg(num_inst_ocn)   ! tavg aVect/bundle
   type(mct_aVect),save  :: x2ocn_ocnx_avg(num_inst_ocn)
   
   type(mct_aVect), pointer :: atm2x_atmx
   type(mct_aVect), pointer :: x2atm_atmx
   type(mct_gGrid), pointer :: dom_atmx
   type(mct_aVect), pointer :: ocn2x_ocnx
   type(mct_aVect), pointer :: x2ocn_ocnx
   type(mct_gGrid), pointer :: dom_ocnx
   type(procMeta),  pointer :: my_proc

   integer(IN)        ,save  :: cnt                 ! counts samples in tavg
   real(R8)           ,save  :: tbnds(2)            ! CF1.0 time bounds

   logical            ,save  :: first_call = .true. ! flags 1st call of this routine

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
   x2atm_atmx => metaData%x2atm_atmx
   ocn2x_ocnx => metaData%ocn2x_ocnx
   x2ocn_ocnx => metaData%x2ocn_ocnx
   
   my_proc => metaData%my_proc
   call compMeta_getInfo(metaData%atm, prognostic=atm_prognostic,nx=atm_nx,&
                      ny=atm_ny, domain=dom_atmx)
   call compMeta_getInfo(metaData%ocn, prognostic=ocn_prognostic,nx=ocn_nx,&
                      ny=ocn_ny, domain=dom_ocnx)
   !call seq_infodata_getData(infodata, cpl_cdf64=cdf64 )

   ! Get current date from clock needed to label the histavg pointer file

   call time_clockGetInfo( EClock_d, curr_ymd=curr_ymd, curr_tod=curr_tod, &
        start_ymd=start_ymd, start_tod=start_tod, curr_time=curr_time, prev_time=prev_time, &
        calendar=calendar)

   if (first_call) then
      !if (atm_present) then
         do iidx = 1, num_inst_atm
            lsize = mct_aVect_lsize(atm2x_atmx)
            call mct_aVect_init(atm2x_atmx_avg(iidx),atm2x_atmx,lsize)
            call mct_aVect_zero(atm2x_atmx_avg(iidx))
            lsize = mct_aVect_lsize(x2atm_atmx)
            call mct_aVect_init(x2atm_atmx_avg(iidx),x2atm_atmx,lsize)
            call mct_aVect_zero(x2atm_atmx_avg(iidx))
         enddo
      !endif
      !if (ocn_present) then
         do iidx = 1, num_inst_ocn
            lsize = mct_aVect_lsize(ocn2x_ocnx)
            call mct_aVect_init(ocn2x_ocnx_avg(iidx),ocn2x_ocnx,lsize)
            call mct_aVect_zero(ocn2x_ocnx_avg(iidx))
            lsize = mct_aVect_lsize(x2ocn_ocnx)
            call mct_aVect_init(x2ocn_ocnx_avg(iidx),x2ocn_ocnx,lsize)
            call mct_aVect_zero(x2ocn_ocnx_avg(iidx))
         enddo
      !endif
      cnt = 0
      tbnds(1) = prev_time
      first_call = .false.
   endif

   if (.not.write_now) then
      cnt = cnt + 1
      !if (atm_present) then
         do iidx = 1, num_inst_atm
            atm2x_atmx_avg(iidx)%rAttr = atm2x_atmx_avg(iidx)%rAttr + &
                                                       atm2x_atmx%rAttr
            x2atm_atmx_avg(iidx)%rAttr = x2atm_atmx_avg(iidx)%rAttr + &
                                                       x2atm_atmx%rAttr
         enddo
      !endif
      !if (ocn_present) then
         do iidx = 1, num_inst_atm
            ocn2x_ocnx_avg(iidx)%rAttr = ocn2x_ocnx_avg(iidx)%rAttr + &
                                                       ocn2x_ocnx%rAttr
            x2ocn_ocnx_avg(iidx)%rAttr = x2ocn_ocnx_avg(iidx)%rAttr + &
                                                       x2ocn_ocnx%rAttr
         enddo
      !endif

   else
      cnt = cnt + 1
      tbnds(2) = curr_time
      !if (atm_present) then
         do iidx = 1, num_inst_atm
            atm2x_atmx_avg(iidx)%rAttr = (atm2x_atmx_avg(iidx)%rAttr + &
                                                       atm2x_atmx%rAttr) / (cnt * 1.0_r8)
            x2atm_atmx_avg(iidx)%rAttr = (x2atm_atmx_avg(iidx)%rAttr + &
                                                       x2atm_atmx%rAttr) / (cnt * 1.0_r8)
         enddo
      !endif
      !if (ocn_present) then
         do iidx = 1, num_inst_atm
            ocn2x_ocnx_avg(iidx)%rAttr = (ocn2x_ocnx_avg(iidx)%rAttr + &
                                                       ocn2x_ocnx%rAttr) / (cnt * 1.0_r8)
            x2ocn_ocnx_avg(iidx)%rAttr = (x2ocn_ocnx_avg(iidx)%rAttr + &
                                                       x2ocn_ocnx%rAttr) / (cnt * 1.0_r8)
         enddo
      !endif
      case_name="no case" ! temp usage
      !call comp_getInfo(metaData, case_name=case_name)
      call time_ClockGetInfo( EClock_d, prev_ymd=prev_ymd, prev_tod=prev_tod)

      ! some problem here
      if (time_histavg_type == time_optNYears) then
         call shr_cal_date2ymd(prev_ymd,yy,mm,dd)
         write(hist_file,"(2a,i4.4,a)") &
            trim(case_name), '.cpl.ha.', yy,'.nc'
      elseif (time_histavg_type == time_optNMonths) then
         call shr_cal_date2ymd(prev_ymd,yy,mm,dd)
         write(hist_file,"(2a,i4.4,a,i2.2,a)") &
            trim(case_name), '.cpl.ha.', yy,'-',mm,'.nc'
      elseif (time_histavg_type == time_optNdays) then
         call shr_cal_date2ymd(prev_ymd,yy,mm,dd)
         write(hist_file,"(2a,i4.4,a,i2.2,a,i2.2,a)") &
            trim(case_name), '.cpl.ha.', yy,'-',mm,'-',dd,'.nc'
      else
         call shr_cal_date2ymd(curr_ymd,yy,mm,dd)
         write(hist_file,"(2a,i4.4,a,i2.2,a,i2.2,a,i5.5,a)") &
            trim(case_name), '.cpl.ha.', yy,'-',mm,'-',dd,'-',curr_tod,'.nc'
      endif

      time_units = 'days since ' &
           // base_io_date2yyyymmdd(start_ymd) // ' ' // base_io_sec2hms(start_tod)

      if (iamin_CPLID) then

         !if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
         call base_io_wopen(my_proc, hist_file,clobber=.true.,cdf64=cdf64)

         ! loop twice, first time write header, second time write data for perf

         do fk = 1,2
            if (fk == 1) then
               whead = .true.
               wdata = .false.
            elseif (fk == 2) then
               whead = .false.
               wdata = .true.
               call base_io_enddef(hist_file)
            else
               call shr_sys_abort('seq_hist_writeavg fk illegal')
            end if

            avg_time = 0.5_r8 * (tbnds(1) + tbnds(2))
!---------- tcx nov 2011 tbnds of same values causes problems in ferret
            if (tbnds(1) >= tbnds(2)) then
               call base_io_write(hist_file,&
                              time_units=time_units,time_cal=calendar,time_val=avg_time,&
                              whead=whead,wdata=wdata)
            else
               call base_io_write(hist_file,&
                              time_units=time_units,time_cal=calendar,time_val=avg_time,&
                              whead=whead,wdata=wdata,tbnds=tbnds)
            endif
            call compMeta_getInfo(metaData%atm, comp_gsmap=gsmap)
            call base_io_write(hist_file,gsmap,dom_atmx%data,'dom_atmx', &
                               nx=atm_nx,ny=atm_ny,nt=1,whead=whead,wdata=wdata,pre='domatm')
            call base_io_write(hist_file,gsmap,x2atm_atmx_avg,'x2atm_atmx', &
                               nx=atm_nx,ny=atm_ny,nt=1,whead=whead,wdata=wdata, &
                               pre='x2atmavg',tavg=.true.)
            call base_io_write(hist_file,gsmap,atm2x_atmx_avg,'atm2x_atmx', &
                               nx=atm_nx,ny=atm_ny,nt=1,whead=whead,wdata=wdata, &
                               pre='atm2xavg',tavg=.true.)
            call compMeta_getInfo(metaData%ocn, comp_gsmap=gsmap)
            call base_io_write(hist_file,gsmap,dom_ocnx%data,'dom_ocnx', &
                               nx=ocn_nx,ny=ocn_ny,nt=1,whead=whead,wdata=wdata,pre='domocn')
            call base_io_write(hist_file,gsmap,x2ocn_ocnx_avg,'x2ocn_ocnx', &
                               nx=ocn_nx,ny=ocn_ny,nt=1,whead=whead,wdata=wdata, &
                               pre='x2ocnavg',tavg=.true.)
            call base_io_write(hist_file,gsmap,ocn2x_ocnx_avg,'ocn2x_ocnx', &
                               nx=ocn_nx,ny=ocn_ny,nt=1,whead=whead,wdata=wdata, &
                               pre='ocn2xavg',tavg=.true.)
         enddo

         call base_io_close(hist_file)
         !if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)

         !if (atm_present) then
            do iidx = 1, num_inst_atm
               call mct_aVect_zero(atm2x_atmx_avg(iidx))
               call mct_aVect_zero(x2atm_atmx_avg(iidx))
            enddo
         !endif
         !if (atm_present) then
            do iidx = 1, num_inst_atm
               call mct_aVect_zero(ocn2x_ocnx_avg(iidx))
               call mct_aVect_zero(x2ocn_ocnx_avg(iidx))
            enddo
         !endif
         cnt = 0
         tbnds(1) = curr_time

      endif
   endif

end subroutine base_hist_writeavg

!===============================================================================

subroutine base_hist_spewav(metaData, aname,gsmap,av,nx,ny,nt,write_now,flds)

   implicit none

   type(Meta),  target,   intent(inout) :: metaData
   character(*),          intent(in) :: aname      ! avect name for hist file
   type(mct_gsmap),       intent(in) :: gsmap      ! gsmap
   type(mct_aVect),       intent(in) :: av         ! avect
   integer(IN),           intent(in) :: nx         ! 2d global size nx
   integer(IN),           intent(in) :: ny         ! 2d global size ny
   integer(IN),           intent(in) :: nt         ! number of time samples per file
   logical,optional,      intent(in) :: write_now  ! write a sample now, if not used, write every call
   character(*),intent(in),optional :: flds   ! list of fields to write

   !--- local ---
   character(CL)           :: case_name         ! case name
   integer(IN)             :: n,fk,fk1          ! index
   integer(IN)             :: samples_per_file
   integer(IN)             :: lsize             ! local size of an aVect
   logical                 :: first_call
   integer(IN)             :: found = -10
   logical                 :: useavg
   logical                 :: lwrite_now     
   logical                 :: whead,wdata  ! for writing restart/history cdf files
   real(r8)                :: tbnds(2)

   integer(IN),parameter   :: maxout = 20
   integer(IN)       ,save :: ntout = 0
   character(CS)     ,save :: tname(maxout) = 'x1y2z3'
   integer(IN)       ,save :: ncnt(maxout)  = -10
   integer(IN)       ,save :: nfiles(maxout) = 0
   character(CL)     ,save :: hist_file(maxout)       ! local path to history filename
   type(mct_aVect)   ,save :: avavg(maxout)           ! av accumulator if needed
   integer(IN)       ,save :: avcnt(maxout) = 0       ! accumulator counter
   logical           ,save :: fwrite(maxout) = .true. ! first write

   type(mct_aVect)         :: avflds                  ! non-avg av for a subset of fields
   type(procMeta),  pointer :: my_proc


   real(R8),parameter :: c0 = 0.0_r8 ! zero

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   !----------------------------------------------------------------------------
   ! get required infodata
   !----------------------------------------------------------------------------
   iamin_CPLID  = metaData%iamin_cpl
   mpicom_GLOID = metaData%mpi_glocomm
   mpicom_CPLID = metaData%mpi_cpl
   
   my_proc => metaData%my_proc
   !call seq_infodata_getData(infodata, cpl_cdf64=cdf64 )


   lwrite_now = .true.
   useavg = .false.
   if (present(write_now)) then
      useavg = .true.
      lwrite_now = write_now
   endif
 
   first_call = .true.
   do n = 1,ntout
      if (trim(tname(n)) == trim(aname)) then
         first_call = .false.
         found = n
      endif
   enddo

   if (first_call) then
      ntout = ntout + 1
      if (ntout > maxout) then
         write(logunit,*) 'write_history_spewAV maxout exceeded',ntout,maxout
         call shr_sys_abort()
      endif
      tname(ntout) = trim(aname)
      ncnt(ntout) = -10
      nfiles(ntout) = 0
      if (iamin_CPLID .and. useavg) then
         lsize = mct_aVect_lsize(av)
         call mct_aVect_init(avavg(ntout),av,lsize)
         call mct_aVect_zero(avavg(ntout))
         avcnt(ntout) = 0
      endif
      found = ntout
   endif

!  if (.not. iamin_CPLID) return
   if (iamin_CPLID) then !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

   samples_per_file = nt      

   if (useavg) then
      if (lwrite_now) then
         avcnt(found) = avcnt(found) + 1
         avavg(found)%rAttr = (avavg(found)%rAttr + av%rAttr) / (avcnt(found) * 1.0_r8)
      else
         avcnt(found) = avcnt(found) + 1
         avavg(found)%rAttr = avavg(found)%rAttr + av%rAttr
      endif
   endif

   if (lwrite_now) then

      ncnt(found) = ncnt(found) + 1
      if (ncnt(found) < 1 .or. ncnt(found) > samples_per_file) then
         ncnt(found) = 1
         nfiles(found) = nfiles(found) + 1
      endif

      if (ncnt(found) == 1) then
         fk1 = 1
         case_name = metaData%case_name
         write(hist_file(found),"(a,i4.4,a)") &
            trim(case_name)//'.cpl.h'//trim(aname)//'.',nfiles(found),'.nc'
      else
         fk1 = 2
      endif

      if (fk1 == 1) then
         call base_io_wopen(my_proc, hist_file(found),clobber=.true.,cdf64=cdf64)
      else
         call base_io_wopen(my_proc, hist_file(found),clobber=.false.,cdf64=cdf64)
      endif

      ! loop twice, first time write header, second time write data for perf

      do fk = fk1,2
         if (fk == 1) then
            whead = .true.
            wdata = .false.
         elseif (fk == 2) then
            whead = .false.
            wdata = .true.
         else
            call shr_sys_abort('seq_hist_spewav fk illegal')
         end if

         if (present(flds)) then
            if (fk == fk1) then
               lsize = mct_aVect_lsize(av)
               call mct_aVect_init(avflds, rList=flds, lsize=lsize)
               call mct_aVect_zero(avflds)
            end if
         end if

         tbnds = real(ncnt(found),r8)
!------- tcx nov 2011 tbnds of same values causes problems in ferret
         if (tbnds(1) >= tbnds(2)) then
           call base_io_write(hist_file(found),&
                             time_units='nstep',time_cal='nstep',time_val=real(ncnt(found),r8),&
                             nt=ncnt(found),whead=whead,wdata=wdata)
         else
           call base_io_write(hist_file(found),&
                             time_units='nstep',time_cal='nstep',time_val=real(ncnt(found),r8),&
                             nt=ncnt(found),whead=whead,wdata=wdata,tbnds=tbnds)
         endif

         if (useavg) then
            if (present(flds)) then
               call mct_aVect_copy(aVin=avavg(found), aVout=avflds)
               call base_io_write(hist_file(found), gsmap, avflds, trim(aname), &
                                 nx=nx, ny=ny, nt=ncnt(found), whead=whead, wdata=wdata, &
                                 pre=trim(aname),tavg=.true.,use_float=.true.)
            else
               call base_io_write(hist_file(found), gsmap, avavg(found), trim(aname), &
                                 nx=nx, ny=ny, nt=ncnt(found), whead=whead, wdata=wdata, &
                                 pre=trim(aname),tavg=.true., use_float=.true.)
            end if
         else if (present(flds)) then
            call mct_aVect_copy(aVin=av, aVout=avflds)
            call base_io_write(hist_file(found), gsmap, avflds, trim(aname), &
                              nx=nx,ny=ny,nt=ncnt(found),whead=whead,wdata=wdata,pre=trim(aname),&
                              use_float=.true.)
         else
            call base_io_write(hist_file(found), gsmap, av, trim(aname), &
                              nx=nx,ny=ny,nt=ncnt(found),whead=whead,wdata=wdata,pre=trim(aname),&
                              use_float=.true.)
         endif
   
         if (present(flds)) then
            if (fk == 2) then
               call mct_aVect_clean(avflds)
            end if
         end if

         if (fk == 1) call base_io_enddef(hist_file(found))
         if (fk == 2) then
            fwrite(found) = .false.
            if (useavg) then
               call mct_aVect_zero(avavg(found))
               avcnt(found) = 0
            endif
         endif
      enddo

      call base_io_close(hist_file(found))

   endif   ! lwrite_now

   endif   ! iamin_CPLID <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

end subroutine base_hist_spewav

!===============================================================================

end module base_hist_mod
