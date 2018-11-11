module base_io
use global_var
use ESMF
use pio
use time_type

    implicit none
    public :: base_io_wopen
    public :: base_io_close
    public :: base_io_read
    public :: base_io_write

    interface base_io_read
        module procedure base_io_read_av
        !module procedure base_io_read_avs
        module procedure base_io_read_int
        module procedure base_io_read_r8
    end interface

    interface base_io_write
        module procedure base_io_write_av
        module procedure base_io_write_avs
        module procedure base_io_write_int
        module procedure base_io_write_r8
    end interface

    ! local data
    character(*),  parameter       :: prefix = "base_io_"
    character(*),  parameter       :: wfilename = ''
    type(file_desc_t), save        :: cpl_io_file
    integer                        :: cpl_pio_iotype
    type(iosystem_desc_t), pointer :: cpl_io_subsystem
    real,          parameter       :: fillvalue = 0.0  ! SHR_CONST_SPVAL
    character(*),  parameter       :: version = 'cpl7v10' 
    character(*),  parameter       :: version0 = 'cpl7v00'
    
    character(len=64)  :: charvar
    integer            :: io_comm

contains

subroutine base_io_wopen(my_proc, filename, clobber, cdf64)

    implicit none
    type(procMeta), pointer, intent(in) :: my_proc
    character(len=*),    intent(in) :: filename
    logical, optional,   intent(in) :: clobber
    logical, optional,   intent(in) :: cdf64

    logical :: exists
    logical :: lclobber
    logical :: lcdf64
    integer :: iam, mpicom
    integer :: rcode
    integer :: nmode
    integer :: ierr
    character(len=64) :: lversion
    character(len=*), parameter :: subName = '(base_io_wopen)'

    lversion = trim(version0)
    call procMeta_getInfo(my_proc,ID=CPLID, comm=mpicom, rank=iam)
    call MPI_COMM_RANK(mpicom, iam, ierr)
    lclobber = .false.
    if(present(clobber))lclobber = clobber
    
    lcdf64 = .false.
    if(present(cdf64)) lcdf64 = cdf64

    if (.not. pio_file_is_open(cpl_io_file))then
        if(iam==0)inquire(file=trim(filename), exist=exists)
        call base_mpi_bcast(exists, mpicom, 'base_io_wopen exists')
        if(exists)then
            if(lclobber)then
                nmode = pio_clobber
                if(lcdf64) nmode = ior(nmode, PIO_64BIT_OFFSET)
                rcode = pio_createfile(cpl_io_subsystem, cpl_io_file, cpl_pio_iotype, trim(filename), nmode)
                if(iam==0)write(logUnit, *) subName,'create file', trim(filename)
                rcode = pio_put_att(cpl_io_file, pio_global, 'file_version',version)
            else
                rcode = pio_openfile(cpl_io_subsystem, cpl_io_file,cpl_pio_iotype, trim(filename), pio_write)
                if(iam==0)write(logUnit, *)subname, 'open file', trim(filename)
                call pio_seterrorhandling(cpl_io_file, PIO_BCAST_ERROR)
                rcode = pio_get_att(cpl_io_file, pio_global, "file_version", lversion)
                call pio_seterrorhandling(cpl_io_file, PIO_INTERNAL_ERROR)
                if(trim(lversion)/=trim(version))then
                    rcode = pio_redef(cpl_io_file)
                    rcode = pio_put_att(cpl_io_file, pio_global, "file_version",version)
                    rcode = pio_enddef(cpl_io_file)
                end if
            end if
        else
            nmode = pio_noclobber
            if(lcdf64) nmode = ior(nmode, PIO_64BIT_OFFSET) 
            rcode = pio_createfile(cpl_io_subsystem, cpl_io_file, cpl_pio_iotype, trim(filename), nmode)
            if(iam==0)write(logUnit, *)subname, ' create file', trim(filename)
            rcode = pio_put_att(cpl_io_file, pio_global, "file_version", version)
        end if
    else if(trim(wfilename) /= trim(filename))then
        if(iam==0)write(logUnit, *)subname, 'different file currently open', trim(filename)
        call base_sys_abort("different file currently open")
    else

    end if

end subroutine base_io_wopen

subroutine base_io_close(filename)

    use pio, only : pio_closefile

    implicit none
    character(*),    intent(in)   :: filename

    integer  :: iam
    integer  :: rcode
    character(*), parameter  :: subName = '(base_io_close)'

    call procMeta_getInfo(metaData%my_proc, ID=CPLID, rank=iam)
   
    if(.not. pio_file_is_open(cpl_io_file))then

    else if(trim(wfilename) /= trim(filename))then
        call pio_closefile(cpl_io_file)
    else
        if(iam==0)write(logUnit, *)subName, 'different file currently open', trim(filename)
        call base_sys_abort("different file currently open")
    end if

    wfilename = ''

end subroutine base_io_close

subroutine base_io_redef(filename)

    implicit none
    character(len=*), intent(in)  :: filename
    integer :: rcode
 
    rcode = pio_redef(cpl_io_file)

end subroutine base_io_redef

subroutine base_io_enddef(filename)

    implicit none
    character(len=*), intent(in)  :: filename
    integer :: rcode

    rcode  = pio_enddef(cpl_io_file)

end subroutine base_io_enddef

subroutine base_io_read_av(filename, comp_gsmap, AV, dname, pre)

    implicit none
    character(len=*),  intent(in)    :: filename
    type(gsmap),       intent(in)    :: comp_gsmap
    type(attrVect),    intent(inout) :: AV
    character(len=*),  intent(in)    :: dname
    character(len=*),  intent(in), optional :: pre

    integer :: rcode
    integer :: iam, mpicom
    integer :: nf, ns, ng
    integer :: i, j, k, n, ndims
    type(file_desc_t) :: pioid
    integer :: dimid(2)
    type(var_desc_t) :: varid
    integer :: lnx, lny
    type(string) :: mstring
    character(len=64) ::itemc
    logical :: exists
    type(io_desc_t) :: iodesc
    integer, pointer :: dof(:)
    character(len=64) :: lversion
    character(len=64) :: name1
    character(len=64) :: lpre
    character(*), parameter :: subName = '(base_io_read_av)'

    lversion = trim(version0) 
    
    lpre = trim(dname)
    if(present(pre)) then
        lpre = trim(pre) 
    end if

    call procMeta_getInfo(metaData%my_proc, ID=CPLID, rank=iam, comm=mpicom)

    call gsmap_OrderedPoints(comp_gsmap, iam, Dof)
    
    ns = avect_lsize(AV)
    nf = avect_nRattr(AV)
   
    if(iam==0)inquire(file=trim(filename), exist=exists)
    call base_mpi_bcast(exists, mpicom, 'base_io_read_av exists')
    if(exists)then
        rcode= pio_openfile(cpl_io_subsystem, pioid,cpl_pio_iotype, trim(filename), pio_nowrite)
        if(iam==0) write(logUnit, *) subname, 'open file', trim(filename)
        call pio_seterrorhandling(pioid, PID_BCAST_ERROR)
    else
        if(iam==0) write(logUnit, *) subname, 'ERROR: file invalid',trim(filename),'', trim(dname)
        call base_sys_abort("ERROR: file invalid")
    end if

    do k = 1, nf
        call attrVect_getRList(mstring, k, AV)
        itemc = string_toChar(mstring)
        call string_clean(mstring)
        if(trim(lversion)==trim(version))then
            name1 = trim(lpre)//'_'//trim(itemc)
        else
            name1 = trim(prefix)//trim(dname)//'_'//trim(itemc)
        end if
        call pio_seterrorhandling(pioid, PIO_BCAST_ERROR)
        rcode = pio_inq_varid(pioid, trim(name1), varid)
        if(rcode == pio_noerr)then
            if(k==1)then
                rcode = pio_inq_varndims(pioid, varid, ndims)
                rcode = pio_inq_vardimid(pioid, varid, dimid(1:ndims))
                rcode = pio_inq_dimlen(pioid, dimid(1), lnx)
                if(ndims>=2)then
                    rcode = pio_inq_dimlen(pioid, dimid(2), lny)
                else
                    lny = 1 
                end if
                ng = lnx*lny
                if(ng/=gsmap_gsize(comp_gsmap))then
                    if(iam==0)write(logUnit, *)subname,'ERROR: dimension notmatch', &
                                lnx, lny, gsmap_gsize(comp_gsmap)
                    call base_sys_abort('ERROR: dimension  notmatch')
                end if
                call pio_initdecomp(cpl_io_subsystem, pio_double, (/lnx, lny/),dof, iodesc) 
                deallocate(dof)
            end if
            call pio_read_darray(pioid, varid, iodesc, av%rattr(k,:), rcode)
        else
            write(logUnit, *)'base_io_readav warning: field', trim(itemc), 'is not on restart file'
            write(logUnit, *)'for backwards compatibility will set it to 0'
            av%rattr(k,:) = 0.0
        end if
        call pio_seterrorhandling(pioid, PIO_INTERNAL_ERROR)
    end do
    
    do n = 1, ns
    do k = 1, nf
        if(AV%rAttr(k, n)==fillvalue)then
            AV%rAttr(k, n) = 0.0
        end if
    end do
    end do

    call pio_freedecomp(pioid, iodesc)
    call pio_closefile(pioid)

end subroutine base_io_read_av

subroutine base_io_read_int(filename, idata, dname)

    implicit none
    character(len=*),   intent(in)    :: filename
    integer,            intent(inout) :: idata
    character(len=*),   intent(in)    :: dname
 
    integer :: i1d(1)
    character(*), parameter :: subName = '(base_io_read_int)'
    
    call base_io_read_int1d(filename, i1d, dname)
    idata = i1d(1)

end subroutine base_io_read_int

subroutine base_io_read_int1d(filename, idata, dname)

    implicit none
    character(len=*),   intent(in)    :: filename
    integer,            intent(inout) :: idata(:)
    character(len=*),   intent(in)    :: dname

    integer :: rcode
    integer :: iam, mpicom
    type(file_desc_t) :: pioid
    type(var_desc_t) :: varid
    logical :: exists
    character(len=64) ::lversion
    character(len=64) :: name1
    character(*), parameter :: subName = '(base_io_read_int1d)'
 
    call procMeta_getInfo(metaData%my_proc, ID=CPLID, rank=iam, comm=mpicom)
    lversion = trim(version0)
    
    if(iam==0)inquire(file=trim(filename), exist=exists)
    call base_mpi_bcast(exists, mpicom, 'base_io_read_int1d exists')
    if (exists)then
        rcode = pio_openfile(cpl_io_subsystem, pioid, cpl_pio_iotype, trim(filename), pio_nowrite)
        call pio_seterrorhandling(pioid, PIO_BCAST_ERROR)
        rcode = pio_get_att(pioid, pio_global, "file_version", lversion)
        call pio_seterrorhandling(pioid, PIO_INTERNAL_ERROR)
    else
        if(iam==0)write(logUnit, *)subname, 'ERROR: file invalid', trim(filename), ' ', trim(dname)
        call base_sys_abort("ERROR: file invalid")
    end if

    if(trim(lversion)==trim(version))then
        name1 = trim(dname)
    else
        name1 = trim(prefix)//trim(dname)
    end if
    rcode = pio_inq_varid(pioid, trim(name1), varid)
    rcode = pio_get_var(pioid, varid, idata)

    call pio_closefile(pioid)

end subroutine base_io_read_int1d

subroutine base_io_read_r8(filename, rdata, dname)

    implicit none
    character(len=*),   intent(in)    :: filename
    real(kind=8),       intent(inout) :: rdata
    character(len=*),   intent(in)    :: dname

    real(kind=8) :: r1d(1)
    character(*), parameter :: subName = '(base_io_read_r8)'

    call base_io_read_r81d(filename, r1d, dname)
    rdata = r1d(1)

end subroutine base_io_read_r8

subroutine base_io_read_r81d(filename, rdata, dname)

    implicit none
    character(len=*),    intent(in)    :: filename
    real(kind=8),        intent(inout) :: rdata(:)
    character(len=*),    intent(in)    :: dname

    integer :: rcode
    integer :: iam, mpicom
    type(file_desc_T) :: pioid
    type(var_desc_T) :: varid
    logical :: exists
    character(len=64) :: lversion
    character(len=64) :: name1
    character(*), parameter :: subName = '(base_io_read_r81d)'

   call procMeta_getInfo(metaData%my_proc, rank=iam, comm=mpicom)
    lversion = trim(version0)
   
    if(iam==0)inquire(file=trim(filename), exist=exists)
    call base_mpi_bcast(exists, mpicom, 'base_io_read_r81d exists')
    if(exists)then
        rcode = pio_openfile(cpl_io_subsystem, pioid, cpl_pio_iotype, trim(filename), pio_nowrite)
        call pio_seterrorhandling(pioid, PIO_BCAST_ERROR)
        rcode = pio_get_att(pioid, pio_global, "file_version", lversion)
        call pio_seterrorhandling(pioid, PIO_INTERNAL_ERROR)
    else
        if(iam==0)write(logUnit, *)subName, 'ERROR: file invalid',trim(filename), ' ', trim(dname)
        call base_sys_abort("ERROR: file invalid")
    end if

    if(trim(lversion)==trim(version))then
        name1 = trim(dname)
    else
        name1 = trim(prefix)//trim(dname)
    end if
    rcode = pio_inq_varid(pioid, trim(name1), varid)
    rcode = pio_get_var(pioid, varid, rdata)

    call pio_closefile(pioid)

end subroutine base_io_read_r81d

subroutine base_io_read_char(filename, rdata, dname)

    implicit none
    character(len=*),  intent(in)    :: filename
    character(len=*),  intent(inout) :: rdata
    character(len=*),  intent(in)    :: dname

    integer :: rcode
    integer :: iam, mpicom
    type(file_desc_T) :: pioid
    type(var_desc_t)  :: varid
    logical :: exists
    character(len=64) :: lversion
    character(len=64) :: name1
    character(*), parameter :: subName = '(base_io_read_char)'

    call procMeta_getInfo(metaData%my_proc, ID=CPLID, rank=iam, comm=mpicom)
    lversion = trim(version0)

    if(iam==0) inquire(file=trim(filename), exist=exists)
    call base_mpi_bcast(exists, mpicom, 'base_io_read_char exists')
    if(exists)then
        rcode = pio_openfile(cpl_io_subsystem, pioid, cpl_pio_iotype, trim(filename), pio_nowrite)
        call pio_seterrorhandling(pioid, PIO_BCAST_ERROR)
        rcode = pio_get_att(pioid, pio_global, "file_version", lversion)
        call pio_seterrorhandling(pioid, PIO_INTERNAL_ERROR)
    else
        if(iam==0)write(logUnit, *)subname, 'ERROR: file invalid',trim(filename), ' ', trim(dname)
        call base_sys_abort("ERROR: file invalid")
    end if

    if(trim(lversion)==trim(version))then
        name1 = trim(dname)
    else 
        name1 = trim(prefix)//trim(dname)
    end if
    rcode = pio_inq_varid(pioid, trim(name1), varid)
    rcode = pio_get_var(pioid, varid, charvar)
    rdata = trim(charvar)
   
    call pio_closefile(pioid)

end subroutine base_io_read_char



subroutine base_io_write_av(filename, comp_gsmap, AV, dname, whead, wdata, nx, ny, nt,&
                           fillval, pre, tavg, use_float)
    
    implicit none
    character(len=*),  intent(in) :: filename
    type(gsmap),       intent(in) :: comp_gsmap
    type(attrVect),    intent(in) :: AV
    character(len=*),  intent(in) :: dname
    logical, optional, intent(in) :: wdata
    logical, optional, intent(in) :: whead
    integer, optional, intent(in) :: nx
    integer, optional, intent(in) :: ny
    integer, optional, intent(in) :: nt
    real(kind=8), optional, intent(in) :: fillval
    character(len=*), optional, intent(in) :: pre
    logical, optional, intent(in) :: tavg
    logical, optional, intent(in) :: use_float

    integer :: rcode
    integer :: mpicom
    integer :: iam
    integer :: nf, ns, ng
    integer :: i,j,k,n
    integer, target ::dimid2(2)
    integer, target :: dimid3(3)
    integer, pointer :: dimid(:)
    type(var_desc_t) :: varid
    type(io_desc_t) :: iodesc
    integer(PIO_OFFSET) :: frame
    type(string) :: mstring  !!!!!
    character(len=64) :: itemc
    character(len=64) :: name1
    character(len=64) :: cunit
    character(len=64) :: lname
    character(len=64) :: sname
    character(len=64) :: lpre
    logical :: exists
    logical :: lwhead, lwdata
    integer :: lnx, lny
    real(kind=8) :: lfillvalue
    character(len=*), parameter :: subName='(base_io_write)'
    integer :: lbum
    integer, pointer :: Dof(:)
    logical :: luse_float

    lfillvalue = fillval
    if(present(fillval))then
        lfillvalue = fillval
    end if

    lpre = trim(dname)
    if(present(pre))then
        lpre = trim(pre) 
    end if

    lwhead = .true.
    lwdata = .true.
    if(present(whead)) lwhead = whead
    if(present(wdata)) lwdata = wdata

    if(.not. lwhead .and. .not. lwdata)then
        return
    end if
   
    luse_float = .false.
    if(present(use_float)) luse_float = use_float
    
    call procMeta_getInfo(metaData%my_proc, ID=CPLID, rank=iam)

    ng = gsmap_gsize(comp_gsmap)
    lnx = ng
    lny =  1

    nf = avect_nRattr(AV)
    if(nf < 1)then
        write(logUnit,*) subname, 'ERROR: nf = ', nf, trim(dname)
        call base_sys_abort("ERROR: nf")
    end if

    if(present(nx)) then
        if(nx/=0)lnx = nx
    end if
    if(present(ny)) then
        if(ny/=0) lny = ny
    end if
    if(lnx*lny /= ng) then
        if(iam==0)write(logUnit, *)subname,'ERROR: grid2d size not consistent', ng, lnx, lny, trim(dname)
        call base_sys_abort(subname//" ERROR: grid2d size not consitent")
    end if

    if(lwhead) then
        rcode = pio_def_dim(cpl_io_file, trim(lpre)//'_nx', lnx, dimid2(1)) 
        rcode = pio_def_dim(cpl_io_file, trim(lpre)//'_ny', lny, dimid2(2))

        if(present(nt)) then
            dimid3(1:2) = dimid2
            rcode =  pio_inq_dimid(cpl_io_file, 'time', dimid3(3))
            dimid => dimid3
        else
            dimid => dimid2
        end if

        frame = 1
        if(present(nt))then
            frame = nt
        end if
        do k = 1, nf
            call avect_getRList(mstring,k, AV)  ! check defined
            itemc = string_toChar(mstring)  ! not defined
            call string_clean(mstring)
            name1 = trim(lpre)//'_'//trim(itemc)
            rcode = pio_inq_varid(cpl_io_file, trim(name1), varid)
            call pio_setframe(varid, frame)
            call pio_write_adarray(cpl_io_file, varid, iodesc, av%rattr(k:), rcode, fillval=lfillvalue)
        end do

        call pio_freedecomp(cpl_io_file, iodesc)

    end if

end subroutine base_io_write_av

subroutine  base_io_write_int(filename, idata, dname, whead, wdata)
   
    implicit none
    character(len=*),  intent(in) :: filename
    integer,           intent(in) :: idata
    character(len=*),  intent(in) :: dname
    logical, optional, intent(in) :: whead
    logical, optional, intent(in) :: wdata

    integer :: rcode
    integer :: iam
    type(var_desc_t) :: varid
    character(len=64) :: cunit
    character(len=64) :: lname
    character(len=64) :: sname
    logical :: exists
    logical :: lwhead, lwdata
    character(len=*), parameter ::subName = '(base_io_write_int)'

    lwhead = .true.
    lwdata = .true.
    if(present(whead)) lwhead = whead
    if(present(wdata)) lwdata = wdata

    if(.not. lwhead .and. .not. lwdata)then
        return
    end if

    call procMeta_getInfo(metaData%my_proc, ID=CPLID, rank=iam)
    if(lwhead)then
        call fldsMeta_lookup(metaData%fldsMetaData, trim(dname), longname=lname, stdname=sname, units=cunit)
        rcode = pio_def_var(cpl_io_file, trim(dname), PIO_INT, varid)
        rcode = pio_put_att(cpl_io_file, varid, "units", trim(cunit))
        rcode = pio_put_att(cpl_io_file, varid, "long_name", trim(lname))
        rcode = pio_put_att(cpl_io_file, varid, "standard_name", trim(sname))
        if(lwdata) call base_io_enddef(filename)
    end if

    if(lwdata)then
        rcode = pio_inq_varid(cpl_io_file, trim(dname),varid)
        rcode = pio_put_var(cpl_io_file, varid,idata)
    end if

end subroutine base_io_write_int

subroutine base_io_write_r8(filename, rdata, dname, whead, wdata)

    implicit none
    character(len=*),  intent(in) :: filename
    real(kind=8),      intent(in) :: rdata
    character(len=*),  intent(in) :: dname
    logical, optional, intent(in) :: whead
    logical, optional, intent(in) :: wdata

    integer :: rcode
    integer :: iam 
    type(var_desc_t) :: varid
    character(len=64) :: cunit
    character(len=64) :: lname
    character(len=64) :: sname
    logical :: exists
    logical :: lwhead, lwdata
    character(len=*), parameter :: subName = '(seq_io_write_r8)'


    lwhead = .true.
    lwdata = .true.
    if(present(whead)) lwhead = whead
    if(present(wdata)) lwdata = wdata

    if(.not. lwhead .and. .not. lwdata)then
        return
    end if

    call procMeta_getInfo(metaData%my_proc, ID=CPLID, rank=iam)
    
    if(lwhead) then
        call fldsMeta_lookup(metaData%fldsMetaData,trim(dname), longname=lname, stdname=sname, units=cunit)
        rcode = pio_def_var(cpl_io_file, trim(dname), PIO_DOUBLE, varid)
        if(rcode == PIO_NOERR)then
            rcode = pio_put_att(cpl_io_file, varid, "units", trim(cunit))
            rcode = pio_put_att(cpl_io_file, varid, "long_name", trim(lname))
            rcode = pio_put_att(cpl_io_file, varid, "standard_name", trim(sname))
            if(lwdata) call base_io_enddef(filename)
        end if
     end if

     if(lwdata)then
         rcode = pio_inq_varid(cpl_io_file, trim(dname), varid)
         rcode = pio_put_var(cpl_io_file, varid, rdata)
     end if

end subroutine base_io_write_r8

subroutine base_io_write_char(filename, rdata, dname, whead, wdata)

    implicit none
    character(len=*),  intent(in) :: filename
    character(len=*),  intent(in) :: rdata
    character(len=*),  intent(in) :: dname
    logical, optional, intent(in) :: whead 
    logical, optional, intent(in) :: wdata

    integer :: rcode
    integer :: mpicom
    integer :: iam
    integer :: dimid(1) 
    type(var_desc_t) :: varid
    character(len=64) :: cunit
    character(len=64) :: lname
    character(len=64) :: sname
    integer :: lnx
    logical :: exists
    logical :: lwhead, lwdata
    character(*), parameter :: subName = '(base_io_write_char)'

    lwhead = .true.
    lwdata = .true.
    if(present(whead)) lwhead =whead
    if(present(wdata)) lwdata = wdata

    if(.not. lwhead .and. .not. lwdata)then
        return 
    end if

    call procMeta_getInfo(metaData%my_proc, ID=CPLID, rank=iam)
    if(lwhead)then
        call fldsMeta_lookup(metaData%fldsMetaData, trim(dname), longname=lname, stdname=sname, units=cunit)
        lnx = len(charvar)
        rcode = pio_def_dim(cpl_io_file, trim(dname)//'_len', lnx, dimid(1))
        rcode = pio_def_var(cpl_io_file, trim(dname), PIO_CHAR, dimid, varid)
        rcode = pio_put_att(cpl_io_file, varid, "units", trim(cunit))
        rcode = pio_put_att(cpl_io_file, varid, "long_name", trim(lname))
        rcode = pio_put_att(cpl_io_file, varid, "standard_name", trim(sname))
       if(lwdata) call base_io_enddef(filename)
    end if

    if(lwdata)then
        charvar = ''
        charvar = trim(rdata)
        rcode = pio_inq_varid(cpl_io_file, trim(dname), varid)
        rcode = pio_put_var(cpl_io_file, varid, charvar)
    end if


end subroutine base_io_write_char

subroutine base_io_write_time(filename, time_units, time_cal, time_val, nt, whead, wdata, tbnds)

    implicit none
    character(len=*),   intent(in) :: filename
    character(len=*),   intent(in) :: time_units
    character(len=*),   intent(in) :: time_cal
    real(kind=8),       intent(in) :: time_val
    integer, optional,  intent(in) :: nt
    logical, optional,  intent(in) :: whead
    logical, optional,  intent(in) :: wdata
    real(kind=8), optional, intent(in) :: tbnds(2)

    integer :: rcode
    integer :: iam
    integer :: dimid(1)
    integer :: dimid2(2)
    type(var_desc_t) ::varid
    integer :: lnx
    logical :: exists
    logical :: lwhead, lwdata
    integer :: start(4), cnt(4)
    character(len=200) :: lcalendar
    real(kind=8) ::time_val_ld(1)
    character(*), parameter :: subName = '(base_io_write_time)'

    lwhead = .true.
    lwdata = .true.
    if(present(whead)) lwhead = whead
    if(present(wdata)) lwdata = wdata
    
    if(.not. lwhead .and. .not. lwdata)then
        return 
    end if

    call procMeta_getInfo(metaData%my_proc, ID=CPLID, rank=iam)
    
    if(lwhead) then
        rcode = pio_def_dim(cpl_io_file, 'time', PIO_UNLIMITED, dimid(1))
        rcode = pio_def_var(cpl_io_file, 'time', PIO_DOUBLE, dimid, varid)
        rcode = pio_put_att(cpl_io_file, varid, 'units', trim(time_units))
        lcalendar = time_cal_noleap
        if(trim(lcalendar)==trim('NO_LEAP'))then
            lcalendar = 'noleap'
        else if(trim(lcalendar)==trim('GREGORIAN'))then
            lcalendar = 'gregorian'
        end if
        rcode  = pio_put_att(cpl_io_file, varid, 'calendar', trim(lcalendar))
        if(present(tbnds))then
            rcode = pio_put_att(cpl_io_file, varid, 'bounds', 'time_bnds')
            dimid2(2) = dimid(1)
            rcode = pio_def_dim(cpl_io_file, 'ntb', 2, dimid2(1))
            rcode = pio_def_var(cpl_io_file, 'time_bnds', PIO_DOUBLE, dimid2, varid)
        end if
        if(lwdata) call base_io_enddef(filename)
    end if

    if(lwdata)then
        start = 1
        cnt = 1
        if(present(nt)) then
            start(1) = nt
        end if
        time_val_ld(1) = time_val 
        rcode = pio_inq_varid(cpl_io_file, 'time', varid)
        rcode = pio_put_var(cpl_io_file, varid, start, cnt, time_val_ld)
        if(present(tbnds))then
            rcode = pio_inq_varid(cpl_io_file, 'time_bnds', varid)
            start = 1
            cnt = 1
            if(present(nt))then
                start(2) = nt
            end if
            cnt(1) = 2
            rcode = pio_put_var(cpl_io_file, varid, start, cnt, tbnds)
        end if
    end if
     
end subroutine base_io_write_time


end module base_io
