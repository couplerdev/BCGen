
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

   #for $model in $proc_cfgs
        #set $model_name = $model.name
   logical     :: ${model_name}_prognostic         ! .true.  => ${model_name} comp expects input
   #end for

   logical     :: cdf64                  ! true => use 64 bit addressing in netCDF files

   !--- domain equivalent 2d grid size ---
   #for $model in $proc_cfgs
        #set $model_name = $model.name
   integer(IN) :: ${model_name}_nx, ${model_name}_ny         ! nx,ny of 2d grid, if known
   integer, parameter :: num_inst_$(model_name) = 1
   #end for
  
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
   type(mct_gsMap), pointer :: gsmap
   #for $model in $proc_cfgs
       #set $_name = $model.name
   type(mct_aVect), pointer :: $(_name)2x_$(_name)x
   type(mct_aVect), pointer :: x2$(_name)_$(_name)x
   type(mct_aVect)          :: fractions_$(_name)x   ! need to support latter
   type(mct_gGrid)          :: dom_$(_name)x
   #end for
   type(procMeta), pointer :: my_proc
   character(*), parameter :: prefix = "./archive/"

 
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   !----------------------------------------------------------------------------
   ! get required metaData desc
   !----------------------------------------------------------------------------
   
   iamin_CPLID  = metaData%iamin_cpl
   mpicom_gloid = metaData%mpi_glocomm
   mpicom_cplid = metaData%mpi_cpl

   #for $model in $proc_cfgs
       #set $_name = $model.name
   $(_name)2x_$(_name)x => metaData%$(_name)2x_$(_name)x
   x2$(_name)_$(_name)x => metaData%x2$(_name)_$(_name)x
   #end for
   my_proc => metaData%my_proc

   #for $model in $proc_cfgs
        #set $name = $model.name
   call compMeta_getInfo(metaData%${name}, prognostic=${name}_prognostic, case_name = case_name, domain=dom_${name}x)
   #end for


   !--- Get current date from clock needed to label the history pointer file ---

   call time_clockGetInfo( EClock_d, curr_ymd=curr_ymd, curr_tod=curr_tod, &
        start_ymd=start_ymd, start_tod=start_tod, curr_time=curr_time, &
        calendar=calendar)
   call shr_cal_date2ymd(curr_ymd,yy,mm,dd)

   case_name = metaData%case_name
   write(hist_file,"(3a,i4.4,a,i2.2,a,i2.2,a,i5.5,a)") &
      trim(prefix), trim(case_name), '.cpl.hi.', yy,'-',mm,'-',dd,'-',curr_tod,'.nc'

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
            call shr_sys_abort('base_hist_write fk illegal')
         end if

         !print *, '---------curr_time---------', fk
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

         !print *,'call :-----------------'
         #for $model in $proc_cfgs
             #set $model_name = $model.name
         !if (${model_name}_present) then
            !call compMeta_GetInfo(metaData%${model_name}, comp_gsmap=gsmap)
                gsmap => metaData%$(model_name)%comp_gsmap
            !print *, 'base io'
            call base_io_write(hist_file,gsmap,dom_${model_name}x%data,'dom_${model_name}x', &
                              nx=${model_name}_nx,ny=${model_name}_ny,nt=1,whead=whead,wdata=wdata,pre='dom${model_name}')
                gsmap => metaData%comp_gsmap_${model_name}x
            !print *, 'data write'
            !call base_io_write(hist_file,gsmap,fractions_${model_name}x,'fractions_${model_name}x', &
            !                  nx=${model_name}_nx,ny=${model_name}_ny,nt=1,whead=whead,wdata=wdata,pre='frac${model_name}')
            call base_io_write(hist_file,gsmap,x2${model_name}_${model_name}x,'x2${model_name}_${model_name}x', &
                              nx=${model_name}_nx,ny=${model_name}_ny,nt=1,whead=whead,wdata=wdata,pre='x2${model_name}')
            call base_io_write(hist_file,gsmap,${model_name}2x_${model_name}x,'${model_name}2x_${model_name}x', &
                              nx=${model_name}_nx,ny=${model_name}_ny,nt=1,whead=whead,wdata=wdata,pre='${model_name}2x')
         !endif
         #end for
         !print *,'***********'
      end do
      call base_io_close(hist_file)
      
   endif

   write(*,*)'========== history file has been written =========='

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

   #for $model in $proc_cfgs
        #set $model_name = $model.name
   type(mct_aVect),save  :: ${model_name}2x_${model_name}x_avg(num_inst_${model_name})   ! tavg aVect/bundle
   type(mct_aVect),save  :: x2${model_name}_${model_name}x_avg(num_inst_${model_name})
   #end for
   
   #for $model in $proc_cfgs
       #set $_name = $model.name
   type(mct_aVect), pointer :: $(_name)2x_$(_name)x
   type(mct_aVect), pointer :: x2$(_name)_$(_name)x
   type(mct_gGrid), pointer :: dom_$(_name)x
   #end for
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

   #for $model in $proc_cfgs
       #set $_name = $model.name
   $(_name)2x_$(_name)x => metaData%$(_name)2x_$(_name)x
   x2$(_name)_$(_name)x => metaData%x2$(_name)_$(_name)x
   #end for
   
   my_proc => metaData%my_proc
   #for $model in $proc_cfgs
        #set $_name = $model.name
   call compMeta_getInfo(metaData%${_name}, prognostic=${_name}_prognostic,nx=${_name}_nx,&
                      ny=${_name}_ny, domain=dom_$(_name)x)
   #end for 
   !call seq_infodata_getData(infodata, cpl_cdf64=cdf64 )

   ! Get current date from clock needed to label the histavg pointer file

   call time_clockGetInfo( EClock_d, curr_ymd=curr_ymd, curr_tod=curr_tod, &
        start_ymd=start_ymd, start_tod=start_tod, curr_time=curr_time, prev_time=prev_time, &
        calendar=calendar)

   if (first_call) then
   #for $model in $proc_cfgs
        #set $model_name =  $model.name
      !if (${model_name}_present) then
         do iidx = 1, num_inst_${model_name}
            lsize = mct_aVect_lsize(${model_name}2x_${model_name}x)
            call mct_aVect_init(${model_name}2x_${model_name}x_avg(iidx),${model_name}2x_${model_name}x,lsize)
            call mct_aVect_zero(${model_name}2x_${model_name}x_avg(iidx))
            lsize = mct_aVect_lsize(x2${model_name}_${model_name}x)
            call mct_aVect_init(x2${model_name}_${model_name}x_avg(iidx),x2${model_name}_${model_name}x,lsize)
            call mct_aVect_zero(x2${model_name}_${model_name}x_avg(iidx))
         enddo
      !endif
   #end for
      cnt = 0
      tbnds(1) = prev_time
      first_call = .false.
   endif

   if (.not.write_now) then
      cnt = cnt + 1
      #for $model in $proc_cfgs
          #set $model_name = $model.name
      !if (${model_name}_present) then
         do iidx = 1, num_inst_atm
            ${model_name}2x_${model_name}x_avg(iidx)%rAttr = ${model_name}2x_${model_name}x_avg(iidx)%rAttr + &
                                                       ${model_name}2x_${model_name}x%rAttr
            x2${model_name}_${model_name}x_avg(iidx)%rAttr = x2${model_name}_${model_name}x_avg(iidx)%rAttr + &
                                                       x2${model_name}_${model_name}x%rAttr
         enddo
      !endif
      #end for

   else
      cnt = cnt + 1
      tbnds(2) = curr_time
      #for $model in $proc_cfgs
           #set $model_name = $model.name
      !if (${model_name}_present) then
         do iidx = 1, num_inst_atm
            ${model_name}2x_${model_name}x_avg(iidx)%rAttr = (${model_name}2x_${model_name}x_avg(iidx)%rAttr + &
                                                       ${model_name}2x_${model_name}x%rAttr) / (cnt * 1.0_r8)
            x2${model_name}_${model_name}x_avg(iidx)%rAttr = (x2${model_name}_${model_name}x_avg(iidx)%rAttr + &
                                                       x2${model_name}_${model_name}x%rAttr) / (cnt * 1.0_r8)
         enddo
      !endif
      #end for
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
            #for $model in $proc_cfgs
                 #set $model_name = $model.name
            call compMeta_getInfo(metaData%${model_name}, comp_gsmap=gsmap)
            call base_io_write(hist_file,gsmap,dom_${model_name}x%data,'dom_${model_name}x', &
                               nx=${model_name}_nx,ny=${model_name}_ny,nt=1,whead=whead,wdata=wdata,pre='dom${model_name}')
            call base_io_write(hist_file,gsmap,x2${model_name}_${model_name}x_avg,'x2${model_name}_${model_name}x', &
                               nx=${model_name}_nx,ny=${model_name}_ny,nt=1,whead=whead,wdata=wdata, &
                               pre='x2${model_name}avg',tavg=.true.)
            call base_io_write(hist_file,gsmap,${model_name}2x_${model_name}x_avg,'${model_name}2x_${model_name}x', &
                               nx=${model_name}_nx,ny=${model_name}_ny,nt=1,whead=whead,wdata=wdata, &
                               pre='${model_name}2xavg',tavg=.true.)
            #end for
         enddo

         call base_io_close(hist_file)
         !if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)

         #for $model in $proc_cfgs
              #set $model_name = $model.name
         !if (atm_present) then
            do iidx = 1, num_inst_atm
               call mct_aVect_zero(${model_name}2x_${model_name}x_avg(iidx))
               call mct_aVect_zero(x2${model_name}_${model_name}x_avg(iidx))
            enddo
         !endif
         #end for
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
