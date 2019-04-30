module comms
!use base_file
use shr_kind_mod, only: r8 => shr_kind_r8, IN => shr_kind_in
use shr_file_mod
use mct_mod
use comms_def
use extend
use proc_def
use comms_nc, only : sMatPInitnc_mapfile
    implicit none
    
    public :: mapper_init
    public :: mapper_rearrsplit_init
    public :: mapper_spmat_init
    public :: mapper_spmat_init_rc
    !public :: mapper_spmat_init_nil
    public :: mapper_spmat_init_nml
    public :: mapper_comp_map
    public :: mapper_comp_interpolation  
    public :: mapper_comp_avMerge
    public :: mapper_readdata
    private :: gsmap_check    

interface mapper_init ; module procedure &
    mapper_init_nil, &
    mapper_init_func
end interface mapper_init

interface mapper_comp_avNorm ; module procedure &
    mapper_comp_avNormArr, &
    mapper_comp_avNormAvF
end interface mapper_comp_avNorm

!interface mapper_spmat_init ; module procedure &
!    mapper_spmat_init_file, &
!    mapper_spmat_init_nil, &
!    mapper_spmat_init_nml
!end interface mapper_spmat_init

contains

subroutine mapper_init_nil(mapper, ierr)

    implicit none
    type(map_mod), intent(inout)   :: mapper
    integer, optional, intent(in)  :: ierr
   
    mapper%map_type = "nil"

end subroutine mapper_init_nil

subroutine mapper_init_func()

end subroutine mapper_init_func

subroutine mapper_rearrsplit_init(mapper, metaData, gsmap_s, ID_s, gsmap_d, ID_d, ID_join, ierr)

    implicit none
    type(map_mod), intent(inout)   :: mapper
    type(Meta),    intent(in)      :: metaData
    type(mct_gsMap),   intent(in)      :: gsmap_s
    integer,       intent(in)      :: ID_s
    type(mct_gsMap),   intent(in)      :: gsmap_d
    integer,       intent(in)      :: ID_d
    integer,       intent(in)      :: ID_join
    integer,  optional,  intent(in):: ierr

    integer    :: mpicom_s, mpicom_d, mpicom_join
    type(mct_gsMap) :: gsmap_s_join
    type(mct_gsMap) :: gsmap_d_join

    mpicom_s = metaData%comp_comm(ID_s)
    mpicom_d = metaData%comp_comm(ID_d)
    mpicom_join = metaData%comp_comm(ID_join)

    !--if(gsmap_Identical(gsmap_s, gsmap_d))then
    if(1 == 0)then
        mapper%map_type = "copy"
    else if(1 == 1) then 
        mapper%map_type = "rearr"
        call gsmap_extend(gsmap_s, gsmap_s_join, mpicom_s, mpicom_join, ID_join)
        call gsmap_extend(gsmap_d, gsmap_d_join, mpicom_d, mpicom_join, ID_join)

        call gsmap_check(gsmap_s_join, gsmap_d_join)
        call mct_rearr_init(gsmap_s_join, gsmap_d_join, mpicom_join, mapper%rearr)

        call mct_gsMap_clean(gsmap_s_join)
        call mct_gsMap_clean(gsmap_d_join)
   else 
       mapper%map_type = "spmat"
   end if

end subroutine mapper_rearrsplit_init


! Build sMat, gsMap_s, gsMap_d must In comp_comm(ID_s)
! 
!subroutine mapper_spmat_init_nil(metaData, mapper,&
!                ID_s, &
!                nRows, nCols, nElements,&
!                gsMap_s, gsMap_d)
!    type(Meta), intent(inout) :: metaData
!    type(map_mod), intent(inout) :: mapper
!    integer, intent(in) :: ID_s
!    integer, intent(in) :: nRows ! gsMap_d%gSize
!    integer, intent(in) :: nCols ! gsMap_s%gSize
!    integer, intent(in) :: nElements ! nums of elem which is not zero
!    type(mct_gsMap), intent(inout) :: gsMap_s, gsMap_d
    

!    integer comm_rank,ierr,n
!    integer, dimension(:), pointer :: rows, cols
!    real, dimension(:), pointer :: weights
    

!    call mpi_comm_rank(metaData%comp_comm(ID_s), comm_rank, ierr)

!    if (metaData%iamin_model(ID_s) .and. comm_rank == 0 ) then
!        allocate(rows(nElements), cols(nElements), &
!                weights(nElements), stat=ierr)
!        do n=1, nElements
!            rows(n) = n-1
!            cols(n) = n-1
!            weights(n) = n
!        end do
!        call mct_sMat_Init(mapper%sMat,nRows,nCols,nElements)

!        call mct_sMat_importGRowI(mapper%sMat, rows, size(rows))
!        call mct_sMat_importGColI(mapper%sMat, cols, size(cols))
!        call mct_sMat_importMatrix(mapper%sMat, weights, size(weights))
!        deallocate(rows, cols, weights, stat=ierr)
!    endif
!    mapper%map_type = "spmat"

!    call MPI_Barrier(metaData%comp_comm(ID_s), ierr)
!    call mct_sMatP_Init(mapper%sMatPlus, &
!           mapper%sMat, gsMap_s, gsMap_d, &
!           mct_sMatP_Xonly, 0, metaData%comp_comm(ID_s), ID_s)
!    call MPI_Barrier(metaData%comp_comm(ID_s), ierr)

!end subroutine mapper_spmat_init_nil

subroutine mapper_spmat_init_rc(mapper, gsmap_src, gsmap_dst, mpicom, &
                               rcfile, mapname, maprctype, samegrid, string)

    implicit none
    type(map_mod),       intent(inout)  :: mapper
    type(mct_gsmap),     intent(in)     :: gsmap_src
    type(mct_gsmap),     intent(in)     :: gsmap_dst
    integer,             intent(in)     :: mpicom
    character(*),        intent(in)     :: rcfile
    character(*),        intent(in)     :: mapname
    character(*),        intent(in)     :: maprctype
    logical,             intent(in), optional :: samegrid
    character(*),        intent(in), optional :: string

    character(*), parameter :: subname = "mapper_spmat_init_rc"
    integer :: ssize, dsize
    character(SHR_KIND_CL) :: mapfilePath
    !character(len=64) :: maptype
    integer :: ierr

    if(present(samegrid) .and. samegrid)then
        mapper%map_type = "rearr"
        call mct_rearr_init(gsmap_src, gsmap_dst, mpicom, mapper%rearr)
    else
        mapper%map_type="spmat"
        call I90_LoadF(rcfile, ierr)
        print *, mapname
        if(ierr/=0)then
            call shr_sys_abort(subname//':abort not find rcfile:'//trim(rcfile))
        end if
        print *,'end rcfile', mapname
        call I90_Label(trim(mapname), ierr)
        if(ierr/=0)then
            call shr_sys_abort(subname//':abort not find label'//trim(mapname))
        end if
        call I90_gtoken(mapfilePath, ierr)

        if(ierr/=0)then
            call shr_sys_abort(subname//': abort not get mapfilePath')
        end if
    !call I90_Label(trim(maprctype), ierr)
    !if(ierr/=0)then
    !    call shr_sys_abort(subname//': abort not find label'//trim(maprctype))
    !end if
    !call I90_gtoken(maptype, ierr)
        call sMatPinitnc_mapfile(mapper%sMatPlus, gsmap_src, gsmap_dst, &
                            trim(mapfilePath), trim(maprctype), mpicom)
         call I90_Release(ierr)
    end if

end subroutine mapper_spmat_init_rc

!----------------------------------------
!  read weight from nfl weight to init spmat
!----------------------------------------
subroutine mapper_spmat_init_nml(mapper, gsmap_src, gsmap_dst, mpicom, &
                                nmlfile, mapname, maprctype, samegrid, string)
    implicit none
    type(map_mod),   intent(inout) :: mapper
    type(mct_gsmap),     intent(in)    :: gsmap_src
    type(mct_gsmap),     intent(in)    :: gsmap_dst
    integer,         intent(in)    :: mpicom
    character(*),    intent(in)    :: nmlfile
    character(*),    intent(in)    :: mapname
    character(*),    intent(in)    :: maprctype
    logical,         intent(in), optional    :: samegrid
    character(*),    intent(in), optional    :: string

    character(*), parameter :: subname = "mapper_spmat_init_nml"
    integer   :: ssize, dsize
    character(SHR_KIND_CL) :: maplname
    character(SHR_KIND_CL) :: maprcfile
    character(SHR_KIND_CL) :: mapsname
    integer   :: unitn
    integer   :: ierr
 
    namelist /mapperFile/ mapsname, maplname

    unitn = shr_file_getUnit()
    write(*,*)'your nmlfile:',trim(nmlfile)
    open(unitn, file=trim(nmlfile), status='old')
    do while(ierr/=0)
        read(unitn, nml=mapperFile, iostat=ierr)
        if(ierr<0)then
           call shr_sys_abort(subname//'" namelist nmlfile error"')
        end if
        if(mapsname==mapname)then
            maprcfile = maplname
            cycle
        end if
    end do
    close(unitn)
    call sMatPinitnc_mapfile(mapper%sMatPlus, gsmap_src, gsmap_dst, &
                        trim(maprcfile), trim(maprctype), mpicom)

end subroutine mapper_spmat_init_nml


subroutine mapper_spmat_init(mapper, gsmap_src, gsmap_dst, mpicom,&
                       maprcfile, maprctype, samegrid, string)
    implicit none

    type(map_mod),         intent(inout)  :: mapper
    type(mct_gsmap),       intent(in)     :: gsmap_src
    type(mct_gsmap),       intent(in)     :: gsmap_dst
    integer,               intent(in)     :: mpicom
    character(len=*),      intent(in)     :: maprcfile
    character(len=*),      intent(in)     :: maprctype
    logical,               intent(in), optional :: samegrid
    character(len=*),      intent(in), optional :: string

    integer   :: ssize, dsize
   
    call sMatPInitnc_mapfile(mapper%sMatPlus, gsmap_src, gsmap_dst, &
                   trim(maprcfile),  trim(maprctype), mpicom)

    


end subroutine mapper_spmat_init

!subroutine mapper_spmat_init(my_proc, mapper,&
!                          ID_s, gsmap_src, gsmap_dst,&
!                          grid_src, grid_dst, map_type)
!    implicit none
!    type(proc),        intent(inout)   :: my_proc
!    type(map_mod),     intent(inout)   :: mapper
!    integer,           intent(inout)   :: ID_s
!    type(gsmap),       intent(in)      :: gsmap_src, gsmap_dst
!    type(gGrid),       intent(in)      :: grid_src, grid_dst
!    character(len=20), intent(in)      :: map_type

!    if(map_type=="spmat")then
!        exit()
!    endif
!    mapper%dom_s=>grid_src
!    mapper%dom_d=>grid_dst
!    mapper%gsmap_s = gsmap_src
!    mapper%gsmap_d = gsmap_dst
!    mapper%map_type = map_type


!end subroutine mapper_spmat_init

subroutine mapper_comp_map(mapper, src, dst, field,norm, avwts, avwtsfld, msgtag, ierr)
    
    implicit none
    type(map_mod),   intent(inout)             :: mapper
    type(mct_aVect), intent(inout)             :: src
    type(mct_aVect), intent(inout)             :: dst
    integer,         optional,   intent(in)    :: msgtag
    integer,         optional,   intent(inout) :: ierr
    logical,         optional,   intent(in)    :: norm
    character(len=*),optional,   intent(in)    :: field
    type(mct_aVect), optional,   intent(in)    :: avwts 
    character(len=*),optional,   intent(in)    :: avwtsfld

    logical :: lnorm

    lnorm  = .true.
    if(present(norm))then
        lnorm = norm
    end if
    if(mapper%map_type=="copy")then
        call mct_avect_copy(src, dst)
    else if(mapper%map_type=="rearr")then
        if(present(field))then
            call mct_rearr_rearrange_fldlist(src, dst, mapper%rearr, tag=msgtag, fldlist=field)
        else
            call mct_rearr_rearrange(src, dst, mapper%rearr, msgtag)
        end if
    else if(mapper%map_type=="spmat")then
        if(present(avwts))then
            if(present(field))then
                call mapper_comp_avNorm(mapper, src, dst, avwts, trim(avwtsfld),rList=field,&
                           norm=lnorm)
            else
                call mapper_comp_avNorm(mapper, src, dst, avwts, trim(avwtsfld), norm=lnorm)
            end if
        else
            if(present(field))then
                call mapper_comp_avNorm(mapper, src, dst, rList=field, norm=lnorm)
            else
                call mapper_comp_avNorm(mapper, src, dst, norm=lnorm)
            end if
        end if
    end if

end subroutine mapper_comp_map

!-------------------------------------------------
! check two gsmap whether they have the same gsize
!-------------------------------------------------
subroutine gsmap_check(gsmap1, gsmap2)

    implicit none
    type(mct_gsMap), intent(in)   :: gsmap1
    type(mct_gsMap), intent(in)   :: gsmap2

    integer :: gsize1, gsize2
     
    gsize1 = gsmap1%gsize
    gsize2 = gsmap2%gsize

    if(gsize1 /= gsize2)then
        !--------------------------------------
        ! need modify when sys implemented
        !--------------------------------------
        write(*,*)'gsize not same'
    end if

end subroutine gsmap_check

!------------------------------------------------------
!   interpolation only based sparse matrix muliplation
!------------------------------------------------------
subroutine mapper_comp_interpolation(mapper,&
                AV_s, AV_d)
    type(map_mod),   intent(inout) :: mapper
    type(mct_aVect), intent(inout) :: AV_s, AV_d
    integer comm_rank, ierr
    !call mpi_comm_rank(my_proc%comp_comm(ID_s), comm_rank, ierr)
    call mct_sMat_avMult(AV_s, mapper%sMatPlus, AV_d)
    !call MPI_Barrier(my_proc%comp_comm(ID_s), ierr)

end subroutine mapper_comp_interpolation


!subroutine mapper_comp_avNorm(mapper,&
!                AV_s, AV_d, rList)
!    type(map_mod),   intent(inout) :: mapper
!    type(mct_aVect), intent(inout) :: AV_s, AV_d
!    character(len=*), optional, intent(in)    :: rList 
!
!    integer comm_rank, ierr
!    if (present(rList)) then 
!        call mct_sMat_avMult(AV_s, mapper%sMatPlus, AV_d, rList=rList)
!    else
!        call mct_sMat_avMult(AV_s, mapper%sMatPlus, AV_d)
!    endif

!end subroutine mapper_comp_avNorm


subroutine mapper_comp_avNormAvF(mapper, av_i, av_o, avf_i, avfifld, rList, norm)

    implicit none
    type(map_mod),       intent(in)    :: mapper
    type(mct_aVect),     intent(in)    :: av_i
    type(mct_aVect),     intent(inout) :: av_o
    type(mct_aVect),     intent(in)    :: avf_i
    character(len=*),    intent(in)    :: avfifld
    character(len=*),    intent(in), optional :: rList
    logical,             intent(in), optional :: norm

    integer :: lsize_i, lsize_f, lsize_o, kf, j
    real(r8), allocatable :: frac_i(:), frac_o(:)
    logical :: lnorm
    character(*), parameter :: subName = "map_avNormAvF"

    lnorm = .true.
    if (present(norm))then
        lnorm  = norm
    end if
    
    lsize_i = mct_aVect_lsize(av_i)
    lsize_f = mct_aVect_lsize(avf_i)
 
    if(lsize_i /= lsize_f)then
        write(*,*)subname, 'ERROR: lsize_i ne lsize_f', lsize_i, lsize_f
        call shr_sys_abort(subname//'ERROR size_i ne lsize_f')
    end if

    allocate(frac_i(lsize_i))
    do j=1, lsize_i
       kf =  mct_aVect_indexRA(avf_i, trim(avfifld))
       frac_i(j) = avf_i%rAttr(kf, j)
    enddo

    if(present(rList))then
        call mapper_comp_avNormArr(mapper, av_i, av_o, frac_i, rList=rList, norm=lnorm)
    else
        call mapper_comp_avNormArr(mapper, av_i, av_o, frac_i, norm=lnorm)
    end if

    deallocate(frac_i)

end subroutine mapper_comp_avNormAvF

subroutine mapper_comp_avNormArr(mapper, av_i, av_o, norm_i, rList, norm)

    implicit none
    type(map_mod),     intent(in)     :: mapper
    type(mct_aVect),   intent(in)     :: av_i
    type(mct_aVect),   intent(inout)  :: av_o
    real(r8),          intent(in), optional :: norm_i(:)
    character(len=*),  intent(in), optional :: rList
    logical,           intent(in), optional :: norm

    type(mct_sMatp)   :: sMatp
    type(mct_aVect)   :: avp_i, avp_o
    integer           :: i,j,ier, kf
    integer           :: lsize_i, lsize_o
    real(r8)          :: normval
    character(SHR_KIND_CX) :: lrList
    logical           :: lnorm
    character(*), parameter :: subName = "(mapper_comp_avNormArr)"
    character(*), parameter :: ffld =  'norm8wt'

    sMatp   = mapper%sMatPlus
    lsize_i = mct_aVect_lsize(av_i)
    lsize_o = mct_aVect_lsize(av_o)

    lnorm = .true.
    if(present(norm))then
        lnorm = norm
    endif

    if(present(norm_i) .and. .not. lnorm)then
        write(*,*) subname, 'ERROR: norm_i and norm = false'
        call shr_sys_abort(subname//'ERROR norm_i and norm = false')
    end if

    if(present(norm_i))then
        if(size(norm_i)/=lsize_i)then
            write(*, *)subname, 'ERROR: size(norm_i) ne lsize_i', size(norm_i), lsize_i
            call shr_sys_abort(subname//' ERROR size(norm_i) ne lsize_i')
        end if
    end if

    if(present(rList))then
        call mct_aVect_init(avp_i, rList=trim(rList)//':'//ffld, lsize=lsize_i)
        call mct_aVect_init(avp_o, rList=trim(rList)//':'//ffld, lsize=lsize_o)
    else
        lrList = trim(mct_aVect_exportRList2c(av_i))
        call mct_aVect_init(avp_i, rList=trim(lrList)//':'//ffld, lsize=lsize_i)
        lrList = trim(mct_aVect_exportRList2c(av_o))
        call mct_aVect_init(avp_o, rList=trim(lrList)//':'//ffld, lsize=lsize_o)
    end if

    call mct_aVect_copy(aVin=av_i, aVout=avp_i, VECTOR=mct_usevector)
    kf = mct_aVect_indexRA(avp_i, ffld)
    do j = 1, lsize_i
        avp_i%rAttr(kf, j) = 1.0_r8
    end do

    if(present(norm_i))then
        do j = 1, lsize_i
            avp_i%rAttr(:,j) = avp_i%rAttr(:,j)*norm_i(j)  
        end do
    end if

    call mct_sMat_avMult(avp_i, sMatp, avp_o, VECTOR=mct_usevector)

    if(lnorm)then
        do j = 1, lsize_o
            kf = mct_aVect_indexRA(avp_o, ffld)
            normval = avp_o%rAttr(kf, j)
            if(normval /= 0.0_r8)then
                normval = 1.0_r8/normval
            end if
            avp_o%rAttr(:,j) = avp_o%rAttr(:,j)*normval
        end do
    end if

    call mct_aVect_copy(aVin=avp_o, aVout=av_o, VECTOR=mct_usevector)

    call mct_aVect_clean(avp_i)
    call mct_aVect_clean(avp_o)
 
end subroutine mapper_comp_avNormArr


subroutine mapper_comp_avMerge( &
                s1, s2, s3, AV_d, field)
    type(mct_aVect), intent(in) :: s1,s2,s3
    type(mct_aVect), intent(inout) :: AV_d
    character(len=*), intent(in)    :: field
    integer ix1,ix2,ix3,ix_d, lsize,i
    ix1 = mct_avect_indexRA(s1, field)
    ix2 = mct_avect_indexRA(s2, field)
    ix3 = mct_avect_indexRA(s3, field)
    ix_d = mct_avect_indexRA(AV_d, field)
    lsize = mct_avect_lsize(AV_d) 
    do i=1,lsize
        AV_d%rAttr(ix_d,i) = &
           (s1%rAttr(ix1,i) + s2%rAttr(ix2,i) + s3%rAttr(ix3,i)) / 3.0
    enddo

end subroutine mapper_comp_avMerge

subroutine mapper_readdata(maprcfile, maprcname, mpicom, ID, &
         ni_s, nj_s, av_s, gsmap_s, avfld_s, filefld_s, &
         ni_d, nj_d, av_d, gsmap_d, avfld_d, filefld_d, string)

    !--- lifted from work by J Edwards, April 2011

    use shr_pio_mod, only : shr_pio_getiosys, shr_pio_getiotype
    use pio, only : pio_openfile, pio_closefile, pio_read_darray, pio_inq_dimid, &
       pio_inq_dimlen, pio_inq_varid, file_desc_t, io_desc_t, iosystem_desc_t, &
       var_desc_t, pio_int, pio_get_var, pio_double, pio_initdecomp, pio_freedecomp
    implicit none
    !-----------------------------------------------------
    ! 
    ! Arguments
    !
    character(len=*),intent(in)    :: maprcfile
    character(len=*),intent(in)    :: maprcname
    integer(IN)     ,intent(in)    :: mpicom
    integer(IN)     ,intent(in)    :: ID
    integer(IN)     ,intent(out)  ,optional :: ni_s
    integer(IN)     ,intent(out)  ,optional :: nj_s
    type(mct_avect) ,intent(inout),optional :: av_s
    type(mct_gsmap) ,intent(in)   ,optional :: gsmap_s
    character(len=*),intent(in)   ,optional :: avfld_s
    character(len=*),intent(in)   ,optional :: filefld_s
    integer(IN)     ,intent(out)  ,optional :: ni_d
    integer(IN)     ,intent(out)  ,optional :: nj_d
    type(mct_avect) ,intent(inout),optional :: av_d
    type(mct_gsmap) ,intent(in)   ,optional :: gsmap_d
    character(len=*),intent(in)   ,optional :: avfld_d
    character(len=*),intent(in)   ,optional :: filefld_d
    character(len=*),intent(in)   ,optional :: string
    !
    ! Local Variables
    !
    type(iosystem_desc_t), pointer :: pio_subsystem
    integer(IN)       :: pio_iotype
    type(file_desc_t) :: File    ! PIO file pointer
    type(io_desc_t)   :: iodesc  ! PIO parallel io descriptor
    integer(IN)       :: rcode   ! pio routine return code
    type(var_desc_t)  :: vid     ! pio variable  ID
    integer(IN)       :: did     ! pio dimension ID
    integer(IN)       :: na      ! size of source domain
    integer(IN)       :: nb      ! size of destination domain
    integer(IN)       :: i       ! index
    integer(IN)       :: mytask  ! my task
    integer(IN), pointer :: dof(:)    ! DOF pointers for parallel read
    character(len=256):: fileName
    character(len=64) :: lfld_s, lfld_d, lfile_s, lfile_d
    character(*),parameter :: areaAV_field = 'aream'
    character(*),parameter :: areafile_s   = 'area_a'
    character(*),parameter :: areafile_d   = 'area_b'
    character(len=*),parameter :: subname  = "(seq_map_readdata) "
    !-----------------------------------------------------

    !if (seq_comm_iamroot(CPLID) .and. present(string)) then
    !   write(logunit,'(A)') subname//' called for '//trim(string)
    !   call shr_sys_flush(logunit)
    !endif

    call MPI_COMM_RANK(mpicom,mytask,rcode)

    lfld_s = trim(areaAV_field)
    if (present(avfld_s)) then
       lfld_s = trim(avfld_s)
    endif

    lfld_d = trim(areaAV_field)
    if (present(avfld_d)) then
       lfld_s = trim(avfld_d)
    endif

    lfile_s = trim(areafile_s)
    if (present(filefld_s)) then
       lfile_s = trim(filefld_s)
    endif

    lfile_d = trim(areafile_d)
    if (present(filefld_d)) then
       lfile_d = trim(filefld_d)
    endif

    call I90_allLoadF(trim(maprcfile),0,mpicom,rcode)
    if(rcode /= 0) then
       write(logunit,*)"Cant find maprcfile file ",trim(maprcfile)
       call shr_sys_abort(trim(subname)//"i90_allLoadF File Not Found")
    endif

    call i90_label(trim(maprcname),rcode)
    if(rcode /= 0) then
       write(logunit,*)"Cant find label ",maprcname
       call shr_sys_abort(trim(subname)//"i90_label Not Found")
    endif

    call i90_gtoken(filename,rcode)
    if(rcode /= 0) then
       write(logunit,*)"Error reading token ",filename
       call shr_sys_abort(trim(subname)//"i90_gtoken Error on filename read")
    endif

    pio_subsystem => shr_pio_getiosys(ID)
    pio_iotype = shr_pio_getiotype(ID)

    rcode = pio_openfile(pio_subsystem, File, pio_iotype, filename)

    if (present(ni_s)) then 
       rcode = pio_inq_dimid (File, 'ni_a', did)  ! number of lons in input grid
       rcode = pio_inq_dimlen(File, did  , ni_s)
    end if
    if(present(nj_s)) then
       rcode = pio_inq_dimid (File, 'nj_a', did)  ! number of lats in input grid
       rcode = pio_inq_dimlen(File, did  , nj_s)
    end if
    if(present(ni_d)) then
       rcode = pio_inq_dimid (File, 'ni_b', did)  ! number of lons in output grid
       rcode = pio_inq_dimlen(File, did  , ni_d)
    end if
    if(present(nj_d)) then
       rcode = pio_inq_dimid (File, 'nj_b', did)  ! number of lats in output grid
       rcode = pio_inq_dimlen(File, did  , nj_d)
    endif

    !--- read and load area_a ---
    if (present(av_s)) then
       if (.not.present(gsmap_s)) then
          call shr_sys_abort(trim(subname)//' ERROR av_s must have gsmap_s')
       endif
       rcode = pio_inq_dimid (File, 'n_a', did)  ! size of  input vector
       rcode = pio_inq_dimlen(File, did  , na)
       i = mct_avect_indexra(av_s, trim(lfld_s))
       call mct_gsmap_OrderedPoints(gsMap_s, mytask, dof)
       call pio_initdecomp(pio_subsystem, pio_double, (/na/), dof, iodesc)
       deallocate(dof)
       rcode = pio_inq_varid(File,trim(lfile_s),vid)
       call pio_read_darray(File, vid, iodesc, av_s%rattr(i,:), rcode)
       call pio_freedecomp(File,iodesc)
    end if

    !--- read and load area_b ---
    if (present(av_d)) then
       if (.not.present(gsmap_d)) then
          call shr_sys_abort(trim(subname)//' ERROR av_d must have gsmap_d')
       endif
       rcode = pio_inq_dimid (File, 'n_b', did)  ! size of output vector
       rcode = pio_inq_dimlen(File, did  , nb)
       i = mct_avect_indexra(av_d, trim(lfld_d))
       call mct_gsmap_OrderedPoints(gsMap_d, mytask, dof)
       call pio_initdecomp(pio_subsystem, pio_double, (/nb/), dof, iodesc)
       deallocate(dof)
       rcode = pio_inq_varid(File,trim(lfile_d),vid)
       call pio_read_darray(File, vid, iodesc, av_d%rattr(i,:), rcode)
       call pio_freedecomp(File,iodesc)
    endif


    call pio_closefile(File)

  end subroutine mapper_readdata


end module comms
