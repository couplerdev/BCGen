module comms
!use base_file
use shr_kind_mod
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
    private :: gsmap_check    

interface mapper_init ; module procedure &
    mapper_init_nil, &
    mapper_init_func
end interface mapper_init

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

    mapper%map_type="spmat"
    call I90_LoadF(rcfile, ierr)
    if(ierr/=0)then
        call shr_sys_abort(subname//':abort not find rcfile')
    end if
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

subroutine mapper_comp_map(mapper, src, dst, msgtag,  ierr, field)
    
    implicit none
    type(map_mod),   intent(inout)              :: mapper
    type(mct_aVect), intent(inout)           :: src
    type(mct_aVect), intent(inout)           :: dst
    integer,         optional,   intent(in)  :: msgtag
    integer,         optional,   intent(inout) :: ierr
    character(len=*),optional,   intent(in)  :: field

    if(mapper%map_type=="copy")then
        call mct_avect_copy(src, dst)
    else if(mapper%map_type=="rearr")then
        call mct_rearr_rearrange(src, dst, mapper%rearr, msgtag)
    else if(mapper%map_type=="spmat")then
        call mapper_comp_avNorm(mapper, src, dst,  field)
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


subroutine mapper_comp_avNorm(mapper,&
                AV_s, AV_d, rList)
    type(map_mod),   intent(inout) :: mapper
    type(mct_aVect), intent(inout) :: AV_s, AV_d
    character(len=*),optional, intent(in)    :: rList 
    integer comm_rank, ierr
    if (present(rList)) then 
        call mct_sMat_avMult(AV_s, mapper%sMatPlus, AV_d, rList=rList)
    else
        call mct_sMat_avMult(AV_s, mapper%sMatPlus, AV_d)
    endif

end subroutine mapper_comp_avNorm


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

end module comms
