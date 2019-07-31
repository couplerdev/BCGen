module procM

use shr_kind_mod
use shr_pio_mod
use shr_string_mod
use mct_mod
use comms_def, only: map_mod
use proc_def, only: procMeta, compMeta
use global_var
use field_def
use base_field
use comms, only: mapper_init
use deploy_mod, only: deploy, deploy_cpl
use mpi_comm, only:  iam_comm_root

!use deploy_mod
!use m_attrvect, only: AttrVect, mct_init => init, mct_clean => clean
    implicit none
include"mpif.h"

    public :: init
    public :: clean

contains

subroutine init(metaData)
    
    implicit none 
    type(Meta), intent(inout), target :: metaData
    integer :: ierr    
    integer :: num_rank
    integer :: num_size 
    integer :: iter
    integer, dimension(:), pointer :: mycomms
    integer, dimension(:), pointer :: myids
    character(*), parameter :: nmlfile = "/root/workspace/BCGen-test/conf/nmlfile"
    character(*), parameter :: datanml = "/root/workspace/BCGen-test/src/nmlfile_src"
    character(*), parameter :: datarc = "/root/workspace/BCGen-test/src/mapper.rc"
    character(*), parameter :: pionml = "/root/workspace/BCGen-test/conf/pioNml"
    integer :: testData ! if test
    integer :: local_rank ! if test

    ! 初始化comp数目，以及comms数
    metaData%num_models = 7
    metaData%num_comms =  2*7+2
    metaData%case_name = "my_case"
    
    
    
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, num_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, num_size, ierr)
    call confMeta_init(metaData%conf, nmlfile, pionml, ierr=ierr)
    !call mct_world_init(ncomps, MPI_COMM_WORLD, comms, comps) ! comms? comps?

    !call shr_pio_init1(metaData%ncomps, pionml, MPI_COMM_WORLD)
    !----------------------------------------------------------
    ! set up every comp's comm
    !----------------------------------------------------------
    metaData%mpi_glocomm = MPI_COMM_WORLD
    call shr_pio_init1(metaData%ncomps, pionml, metaData%mpi_glocomm)

    allocate(metaData%iamin_model(metaData%ncomps))
    allocate(metaData%comp_name(metaData%ncomps))
    allocate(metaData%comp_comm_iam(metaData%ncomps))

    metaData%comp_comm_iam = -1


    metaData%comp_name(metaData%gloid) = shr_string_toUpper(trim("global"))
    metaData%comp_name(metaData%cplid) = shr_string_toUpper(trim("coupler"))
    metaData%comp_name(metaData%modelocn_id) = shr_string_toUpper(trim("ocn"))
    metaData%comp_name(metaData%modelocn2cpl_id) = shr_string_toUpper(trim("ocn2cpl"))
    metaData%comp_name(metaData%modelatm_id) = shr_string_toUpper(trim("atm"))
    metaData%comp_name(metaData%modelatm2cpl_id) = shr_string_toUpper(trim("atm2cpl"))
    metaData%comp_name(metaData%modelatm_id) = shr_string_toUpper(trim("atm"))
    metaData%comp_name(metaData%modelatm2cpl_id) = shr_string_toUpper(trim("atm2cpl"))
    metaData%comp_name(metaData%modelice_id) = shr_string_toUpper(trim("ice"))
    metaData%comp_name(metaData%modelice2cpl_id) = shr_string_toUpper(trim("ice2cpl"))
    metaData%comp_name(metaData%modelrof_id) = shr_string_toUpper(trim("rof"))
    metaData%comp_name(metaData%modelrof2cpl_id) = shr_string_toUpper(trim("rof2cpl"))
    metaData%comp_name(metaData%modellnd_id) = shr_string_toUpper(trim("lnd"))
    metaData%comp_name(metaData%modellnd2cpl_id) = shr_string_toUpper(trim("lnd2cpl"))
    metaData%comp_name(metaData%modellnd_id) = shr_string_toUpper(trim("lnd"))
    metaData%comp_name(metaData%modellnd2cpl_id) = shr_string_toUpper(trim("lnd2cpl"))

    do iter = 1, metaData%ncomps
        metaData%iamin_model(iter) = .false.
    end do
    metaData%iamin_model(1) = .true.
    ! deploy_cpl
    call deploy_cpl(metaData%mpi_glocomm, metaData%mpi_cpl, &
                  metaData%cplid, metaData%iamin_model, 0, ierr)
    print *,'cpl deployed'
    if(metaData%iamin_model(metaData%cplid))then
        call MPI_Comm_rank(metaData%mpi_cpl, local_rank, ierr)
        if(local_rank==0)then
            testData = 100
        end if
        call MPI_Bcast(testData, 1, MPI_INTEGER, 0, metaData%mpi_cpl, ierr)
    end if
    print *, 'bcast end'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    call deploy(metaData%mpi_glocomm, metaData%mpi_modelocn,&
                metaData%mpi_modelocn2cpl, &
                metaData%modelocn_id, metaData%cplid, &
                metaData%modelocn2cpl_id, metaData%iamin_model, 0, ierr)
    call deploy(metaData%mpi_glocomm, metaData%mpi_modelatm,&
                metaData%mpi_modelatm2cpl, &
                metaData%modelatm_id, metaData%cplid, &
                metaData%modelatm2cpl_id, metaData%iamin_model, 0, ierr)
    call deploy(metaData%mpi_glocomm, metaData%mpi_modelatm,&
                metaData%mpi_modelatm2cpl, &
                metaData%modelatm_id, metaData%cplid, &
                metaData%modelatm2cpl_id, metaData%iamin_model, 0, ierr)
    call deploy(metaData%mpi_glocomm, metaData%mpi_modelice,&
                metaData%mpi_modelice2cpl, &
                metaData%modelice_id, metaData%cplid, &
                metaData%modelice2cpl_id, metaData%iamin_model, 0, ierr)
    call deploy(metaData%mpi_glocomm, metaData%mpi_modelrof,&
                metaData%mpi_modelrof2cpl, &
                metaData%modelrof_id, metaData%cplid, &
                metaData%modelrof2cpl_id, metaData%iamin_model, 0, ierr)
    call deploy(metaData%mpi_glocomm, metaData%mpi_modellnd,&
                metaData%mpi_modellnd2cpl, &
                metaData%modellnd_id, metaData%cplid, &
                metaData%modellnd2cpl_id, metaData%iamin_model, 0, ierr)
    call deploy(metaData%mpi_glocomm, metaData%mpi_modellnd,&
                metaData%mpi_modellnd2cpl, &
                metaData%modellnd_id, metaData%cplid, &
                metaData%modellnd2cpl_id, metaData%iamin_model, 0, ierr)
    print *, 'comp deployed'


    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !write(*,*), "my_world_rank:", num_rank, " my_in_model", metaData%iamin_model
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    if(metaData%iamin_model(metaData%modelocn_id))then
        call MPI_Comm_rank(metaData%mpi_modelocn, local_rank, ierr)
        print *,'Im ocn: in comp:',local_rank, ' in glo:',num_rank
    end if
    if(metaData%iamin_model(metaData%modelatm_id))then
        call MPI_Comm_rank(metaData%mpi_modelatm, local_rank, ierr)
        print *,'Im atm: in comp:',local_rank, ' in glo:',num_rank
    end if
    if(metaData%iamin_model(metaData%modelatm_id))then
        call MPI_Comm_rank(metaData%mpi_modelatm, local_rank, ierr)
        print *,'Im atm: in comp:',local_rank, ' in glo:',num_rank
    end if
    if(metaData%iamin_model(metaData%modelice_id))then
        call MPI_Comm_rank(metaData%mpi_modelice, local_rank, ierr)
        print *,'Im ice: in comp:',local_rank, ' in glo:',num_rank
    end if
    if(metaData%iamin_model(metaData%modelrof_id))then
        call MPI_Comm_rank(metaData%mpi_modelrof, local_rank, ierr)
        print *,'Im rof: in comp:',local_rank, ' in glo:',num_rank
    end if
    if(metaData%iamin_model(metaData%modellnd_id))then
        call MPI_Comm_rank(metaData%mpi_modellnd, local_rank, ierr)
        print *,'Im lnd: in comp:',local_rank, ' in glo:',num_rank
    end if
    if(metaData%iamin_model(metaData%modellnd_id))then
        call MPI_Comm_rank(metaData%mpi_modellnd, local_rank, ierr)
        print *,'Im lnd: in comp:',local_rank, ' in glo:',num_rank
    end if
    print *,'before comm init'
    !  初始化comp_comm
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !write(*,*)'comm initiated'
    allocate(metaData%comp_comm(metaData%ncomps))
    metaData%comp_comm(metaData%gloid)       = metaData%mpi_glocomm
    metaData%comp_comm(metaData%cplid)       = metaData%mpi_cpl

    metaData%comp_comm(metaData%modelocn_id)     = metaData%mpi_modelocn
    metaData%comp_comm(metaData%modelocn2cpl_id) =metaData%mpi_modelocn2cpl 
    metaData%comp_comm(metaData%modelatm_id)     = metaData%mpi_modelatm
    metaData%comp_comm(metaData%modelatm2cpl_id) =metaData%mpi_modelatm2cpl 
    metaData%comp_comm(metaData%modelatm_id)     = metaData%mpi_modelatm
    metaData%comp_comm(metaData%modelatm2cpl_id) =metaData%mpi_modelatm2cpl 
    metaData%comp_comm(metaData%modelice_id)     = metaData%mpi_modelice
    metaData%comp_comm(metaData%modelice2cpl_id) =metaData%mpi_modelice2cpl 
    metaData%comp_comm(metaData%modelrof_id)     = metaData%mpi_modelrof
    metaData%comp_comm(metaData%modelrof2cpl_id) =metaData%mpi_modelrof2cpl 
    metaData%comp_comm(metaData%modellnd_id)     = metaData%mpi_modellnd
    metaData%comp_comm(metaData%modellnd2cpl_id) =metaData%mpi_modellnd2cpl 
    metaData%comp_comm(metaData%modellnd_id)     = metaData%mpi_modellnd
    metaData%comp_comm(metaData%modellnd2cpl_id) =metaData%mpi_modellnd2cpl 
    print *, 'comm set' 
    call procMeta_init(metaData%my_proc, metaData%ncomps)
    !allocate(metaData%my_proc%IDs(metaData%ncomps))
    !allocate(metaData%my_proc%IDs(metaData%ncomps))
    !allocate(metaData%my_proc%)
    call procMeta_addToModel(metaData%my_proc, metaData%gloid, metaData%mpi_glocomm, 'global', ierr)
    if(metaData%iamin_model(metaData%cplid))then
        call procMeta_addToModel(metaData%my_proc, metaData%cplid, metaData%mpi_cpl, 'coupler', ierr)
    end if
    if(metaData%iamin_model(metaData%modelocn_id))then
        call procMeta_addToModel(metaData%my_proc, metaData%modelocn_id, metaData%mpi_modelocn, 'ocn', ierr)
    end if
    if(metaData%iamin_model(metaData%modelocn2cpl_id))then
        call procMeta_addToModel(metaData%my_proc, metaData%modelocn2cpl_id, &
                             metaData%mpi_modelocn2cpl,'ocn2cpl', ierr)
    end if
    if(metaData%iamin_model(metaData%modelatm_id))then
        call procMeta_addToModel(metaData%my_proc, metaData%modelatm_id, metaData%mpi_modelatm, 'atm', ierr)
    end if
    if(metaData%iamin_model(metaData%modelatm2cpl_id))then
        call procMeta_addToModel(metaData%my_proc, metaData%modelatm2cpl_id, &
                             metaData%mpi_modelatm2cpl,'atm2cpl', ierr)
    end if
    if(metaData%iamin_model(metaData%modelatm_id))then
        call procMeta_addToModel(metaData%my_proc, metaData%modelatm_id, metaData%mpi_modelatm, 'atm', ierr)
    end if
    if(metaData%iamin_model(metaData%modelatm2cpl_id))then
        call procMeta_addToModel(metaData%my_proc, metaData%modelatm2cpl_id, &
                             metaData%mpi_modelatm2cpl,'atm2cpl', ierr)
    end if
    if(metaData%iamin_model(metaData%modelice_id))then
        call procMeta_addToModel(metaData%my_proc, metaData%modelice_id, metaData%mpi_modelice, 'ice', ierr)
    end if
    if(metaData%iamin_model(metaData%modelice2cpl_id))then
        call procMeta_addToModel(metaData%my_proc, metaData%modelice2cpl_id, &
                             metaData%mpi_modelice2cpl,'ice2cpl', ierr)
    end if
    if(metaData%iamin_model(metaData%modelrof_id))then
        call procMeta_addToModel(metaData%my_proc, metaData%modelrof_id, metaData%mpi_modelrof, 'rof', ierr)
    end if
    if(metaData%iamin_model(metaData%modelrof2cpl_id))then
        call procMeta_addToModel(metaData%my_proc, metaData%modelrof2cpl_id, &
                             metaData%mpi_modelrof2cpl,'rof2cpl', ierr)
    end if
    if(metaData%iamin_model(metaData%modellnd_id))then
        call procMeta_addToModel(metaData%my_proc, metaData%modellnd_id, metaData%mpi_modellnd, 'lnd', ierr)
    end if
    if(metaData%iamin_model(metaData%modellnd2cpl_id))then
        call procMeta_addToModel(metaData%my_proc, metaData%modellnd2cpl_id, &
                             metaData%mpi_modellnd2cpl,'lnd2cpl', ierr)
    end if
    if(metaData%iamin_model(metaData%modellnd_id))then
        call procMeta_addToModel(metaData%my_proc, metaData%modellnd_id, metaData%mpi_modellnd, 'lnd', ierr)
    end if
    if(metaData%iamin_model(metaData%modellnd2cpl_id))then
        call procMeta_addToModel(metaData%my_proc, metaData%modellnd2cpl_id, &
                             metaData%mpi_modellnd2cpl,'lnd2cpl', ierr)
    end if
    print *,'procMeta_add finished'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    allocate(metaData%comp_id(metaData%ncomps))
    do iter = 1, metaData%ncomps
        metaData%comp_id(iter) = iter
    end do


    !  there are bugs in mycomms=>metaData%comp_comm, need modify
    mycomms => metaData%comp_comm
    myids => metaData%comp_id

    do iter = 1, metaData%ncomps
        if(metaData%iamin_model(iter))then
            call MPI_COMM_RANK(metaData%comp_comm(iter), local_rank, ierr)
            metaData%comp_comm_iam(iter) = local_rank
        end if
    end do

    call mct_world_init(metaData%ncomps, MPI_COMM_WORLD, mycomms, myids)

    write(*,*)'mct_world_init initiated'
    if(num_rank==0) then
        metaData%iam_root = .true.
    else
        metaData%iam_root = .false.
    end if

    metaData%iamin_cpl = .false.
    if(metaData%iamin_model(metaData%cplid))then
        write(*,*)'Im cpl',num_rank
        call iam_comm_root(metaData%mpi_cpl, metaData%iamroot_cpl, ierr)
        metaData%iamin_cpl = .true.
    end if

    metaData%iamin_modelocn = .false. 
    
    if(metaData%iamin_model(metaData%modelocn_id))then
        call iam_comm_root(metaData%mpi_modelocn, metaData%iamroot_modelocn, ierr)
        metaData%iamin_modelocn = .true.
        call MPI_Comm_rank(metaData%mpi_modelocn,local_rank, ierr)
    end if

    metaData%iamin_modelocn2cpl = .false.
    if(metaData%iamin_model(metaData%modelocn2cpl_id))then
        call iam_comm_root(metaData%mpi_modelocn2cpl, &
            metaData%iamroot_modelocn2cpl, ierr)
        metaData%iamin_modelocn2cpl = .true.
    end if
    metaData%iamin_modelatm = .false. 
    
    if(metaData%iamin_model(metaData%modelatm_id))then
        call iam_comm_root(metaData%mpi_modelatm, metaData%iamroot_modelatm, ierr)
        metaData%iamin_modelatm = .true.
        call MPI_Comm_rank(metaData%mpi_modelatm,local_rank, ierr)
    end if

    metaData%iamin_modelatm2cpl = .false.
    if(metaData%iamin_model(metaData%modelatm2cpl_id))then
        call iam_comm_root(metaData%mpi_modelatm2cpl, &
            metaData%iamroot_modelatm2cpl, ierr)
        metaData%iamin_modelatm2cpl = .true.
    end if
    metaData%iamin_modelatm = .false. 
    
    if(metaData%iamin_model(metaData%modelatm_id))then
        call iam_comm_root(metaData%mpi_modelatm, metaData%iamroot_modelatm, ierr)
        metaData%iamin_modelatm = .true.
        call MPI_Comm_rank(metaData%mpi_modelatm,local_rank, ierr)
    end if

    metaData%iamin_modelatm2cpl = .false.
    if(metaData%iamin_model(metaData%modelatm2cpl_id))then
        call iam_comm_root(metaData%mpi_modelatm2cpl, &
            metaData%iamroot_modelatm2cpl, ierr)
        metaData%iamin_modelatm2cpl = .true.
    end if
    metaData%iamin_modelice = .false. 
    
    if(metaData%iamin_model(metaData%modelice_id))then
        call iam_comm_root(metaData%mpi_modelice, metaData%iamroot_modelice, ierr)
        metaData%iamin_modelice = .true.
        call MPI_Comm_rank(metaData%mpi_modelice,local_rank, ierr)
    end if

    metaData%iamin_modelice2cpl = .false.
    if(metaData%iamin_model(metaData%modelice2cpl_id))then
        call iam_comm_root(metaData%mpi_modelice2cpl, &
            metaData%iamroot_modelice2cpl, ierr)
        metaData%iamin_modelice2cpl = .true.
    end if
    metaData%iamin_modelrof = .false. 
    
    if(metaData%iamin_model(metaData%modelrof_id))then
        call iam_comm_root(metaData%mpi_modelrof, metaData%iamroot_modelrof, ierr)
        metaData%iamin_modelrof = .true.
        call MPI_Comm_rank(metaData%mpi_modelrof,local_rank, ierr)
    end if

    metaData%iamin_modelrof2cpl = .false.
    if(metaData%iamin_model(metaData%modelrof2cpl_id))then
        call iam_comm_root(metaData%mpi_modelrof2cpl, &
            metaData%iamroot_modelrof2cpl, ierr)
        metaData%iamin_modelrof2cpl = .true.
    end if
    metaData%iamin_modellnd = .false. 
    
    if(metaData%iamin_model(metaData%modellnd_id))then
        call iam_comm_root(metaData%mpi_modellnd, metaData%iamroot_modellnd, ierr)
        metaData%iamin_modellnd = .true.
        call MPI_Comm_rank(metaData%mpi_modellnd,local_rank, ierr)
    end if

    metaData%iamin_modellnd2cpl = .false.
    if(metaData%iamin_model(metaData%modellnd2cpl_id))then
        call iam_comm_root(metaData%mpi_modellnd2cpl, &
            metaData%iamroot_modellnd2cpl, ierr)
        metaData%iamin_modellnd2cpl = .true.
    end if
    metaData%iamin_modellnd = .false. 
    
    if(metaData%iamin_model(metaData%modellnd_id))then
        call iam_comm_root(metaData%mpi_modellnd, metaData%iamroot_modellnd, ierr)
        metaData%iamin_modellnd = .true.
        call MPI_Comm_rank(metaData%mpi_modellnd,local_rank, ierr)
    end if

    metaData%iamin_modellnd2cpl = .false.
    if(metaData%iamin_model(metaData%modellnd2cpl_id))then
        call iam_comm_root(metaData%mpi_modellnd2cpl, &
            metaData%iamroot_modellnd2cpl, ierr)
        metaData%iamin_modellnd2cpl = .true.
    end if
   
    metaData%iamin_modelocn = .false.
    if(metaData%iamin_model(metaData%modelocn_id))then
        call iam_comm_root(metaData%mpi_modelocn, metaData%iamroot_modelocn, ierr)
        metaData%iamin_modelocn = .true.
    end if


    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !write(*,*)'before mapper_init'

    call mapper_init(metaData%mapper_Cocn2x, ierr)
    call mapper_init(metaData%mapper_Cx2ocn, ierr)

    call mapper_init(metaData%mapper_Catm2x, ierr)
    call mapper_init(metaData%mapper_Cx2atm, ierr)

    call mapper_init(metaData%mapper_Catm2x, ierr)
    call mapper_init(metaData%mapper_Cx2atm, ierr)

    call mapper_init(metaData%mapper_Cice2x, ierr)
    call mapper_init(metaData%mapper_Cx2ice, ierr)

    call mapper_init(metaData%mapper_Crof2x, ierr)
    call mapper_init(metaData%mapper_Cx2rof, ierr)

    call mapper_init(metaData%mapper_Clnd2x, ierr)
    call mapper_init(metaData%mapper_Cx2lnd, ierr)

    call mapper_init(metaData%mapper_Clnd2x, ierr)
    call mapper_init(metaData%mapper_Cx2lnd, ierr)


    !-------------------------------------------
    !   init model desc info only used for MCT
    !-------------------------------------------
    metaData%ocn%ID = metaData%modelocn_id
    metaData%ocn%comm = metaData%comp_comm(metaData%modelocn_id)
    metaData%ocn%gsize = 11600
    metaData%atm%ID = metaData%modelatm_id
    metaData%atm%comm = metaData%comp_comm(metaData%modelatm_id)
    metaData%atm%gsize = 3312
    metaData%atm%ID = metaData%modelatm_id
    metaData%atm%comm = metaData%comp_comm(metaData%modelatm_id)
    metaData%atm%gsize = 3312
    metaData%ice%ID = metaData%modelice_id
    metaData%ice%comm = metaData%comp_comm(metaData%modelice_id)
    metaData%ice%gsize = 11600
    metaData%rof%ID = metaData%modelrof_id
    metaData%rof%comm = metaData%comp_comm(metaData%modelrof_id)
    metaData%rof%gsize = 259200
    metaData%lnd%ID = metaData%modellnd_id
    metaData%lnd%comm = metaData%comp_comm(metaData%modellnd_id)
    metaData%lnd%gsize = 3312
    metaData%lnd%ID = metaData%modellnd_id
    metaData%lnd%comm = metaData%comp_comm(metaData%modellnd_id)
    metaData%lnd%gsize = 3312

    !-------------------------------------------
    !   init field 
    !-------------------------------------------
    call flds_init(metaData, ierr)

    !-------------------------------------------
    !   init nmlfile
    !-------------------------------------------
    !call confMeta_init(metaData%conf, nmlfile, ierr=ierr)
    metaData%datanml = datanml
    metaData%datarc = datarc

    !-------------------------------------------
    !   init pio
    !-------------------------------------------
    call shr_pio_init2(metaData%comp_id, metaData%comp_name, metaData%iamin_model,&
          metaData%comp_comm, metaData%comp_comm_iam)



end subroutine init

subroutine clean(metaData)
    
    implicit none
    type(Meta), intent(inout) :: metaData
    integer :: ierr

    call procMeta_Final(metaData%my_proc, ierr)
    call compMeta_Final(metaData%ocn, ierr)
    call compMeta_Final(metaData%atm, ierr)
    call compMeta_Final(metaData%atm, ierr)
    call compMeta_Final(metaData%ice, ierr)
    call compMeta_Final(metaData%rof, ierr)
    call compMeta_Final(metaData%lnd, ierr)
    call compMeta_Final(metaData%lnd, ierr)

    call MPI_Finalize(ierr) 

end subroutine clean

end module procM

