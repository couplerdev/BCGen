module procM

use shr_kind_mod
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
    #set $nmlfile = $conf_cfgs['nmlfile']
    #set $datanml = $conf_cfgs['dataNml']
    #set $datarc = $conf_cfgs['datarc']
    character(*), parameter :: nmlfile = "$nmlfile"
    character(*), parameter :: datanml = "$datanml"
    character(*), parameter :: datarc = "$datarc"
    integer :: testData ! if test
    integer :: local_rank ! if test

    ! 初始化comp数目，以及comms数
    #set $ncomps = len($proc_cfgs)
    metaData%num_models = $ncomps
    metaData%num_comms =  2*$ncomps+2
    
    
    
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, num_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, num_size, ierr)
   
    !call mct_world_init(ncomps, MPI_COMM_WORLD, comms, comps) ! comms? comps?

    
    !----------------------------------------------------------
    ! set up every comp's comm
    !----------------------------------------------------------
    metaData%mpi_glocomm = MPI_COMM_WORLD

    allocate(metaData%iamin_model(metaData%ncomps))
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

    #for $model in $proc_cfgs
         #set $name = $model.name
    call deploy(metaData%mpi_glocomm, metaData%mpi_model${name},&
                metaData%mpi_model${name}2cpl, &
                metaData%model${name}_id, metaData%cplid, &
                metaData%model${name}2cpl_id, metaData%iamin_model, 0, ierr)
    #end for
    print *, 'comp deployed'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !write(*,*), "my_world_rank:", num_rank, " my_in_model", metaData%iamin_model
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    #for $model in $proc_cfgs
         #set $name = $model.name
    if(metaData%iamin_model(metaData%model${name}_id))then
        call MPI_Comm_rank(metaData%mpi_model${name}, local_rank, ierr)
        print *,'Im ${name}: in comp:',local_rank, ' in glo:',num_rank
    end if
    #end for
    !  初始化comp_comm
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !write(*,*)'comm initiated'
    allocate(metaData%comp_comm(metaData%ncomps))
    metaData%comp_comm(metaData%gloid)       = metaData%mpi_glocomm
    metaData%comp_comm(metaData%cplid)       = metaData%mpi_cpl

    #for $model in $proc_cfgs
         #set $name = $model.name
    metaData%comp_comm(metaData%model${name}_id)     = metaData%mpi_model${name}
    metaData%comp_comm(metaData%model${name}2cpl_id) =metaData%mpi_model${name}2cpl 
    #end for
    
    call procMeta_init(metaData%my_proc, metaData%ncomps)
    !allocate(metaData%my_proc%IDs(metaData%ncomps))
    !allocate(metaData%my_proc%IDs(metaData%ncomps))
    !allocate(metaData%my_proc%)
    call procMeta_addToModel(metaData%my_proc, metaData%gloid, metaData%mpi_glocomm, 'global', ierr)
    if(metaData%iamin_model(metaData%cplid))then
        call procMeta_addToModel(metaData%my_proc, metaData%cplid, metaData%mpi_cpl, 'coupler', ierr)
    end if
    #for $model in $proc_cfgs
         #set $name = $model.name
    if(metaData%iamin_model(metaData%model${name}_id))then
        call procMeta_addToModel(metaData%my_proc, metaData%model${name}_id, metaData%mpi_model${name}, '$name', ierr)
    end if
    if(metaData%iamin_model(metaData%model${name}2cpl_id))then
        call procMeta_addToModel(metaData%my_proc, metaData%model${name}2cpl_id, &
                             metaData%mpi_model${name}2cpl,'${name}2cpl', ierr)
    end if
    #end for
    print *,'procMeta_add finished'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    allocate(metaData%comp_id(metaData%ncomps))
    do iter = 1, metaData%ncomps
        metaData%comp_id(iter) = iter
    end do


    !  there are bugs in mycomms=>metaData%comp_comm, need modify
    mycomms => metaData%comp_comm
    myids => metaData%comp_id


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

    #for $model in $proc_cfgs
         #set $name = $model.name 
    metaData%iamin_model${name} = .false.
    if(metaData%iamin_model(metaData%model${name}_id))then
        call iam_comm_root(metaData%mpi_model${name}, metaData%iamroot_model${name}, ierr)
        metaData%iamin_model${name} = .true.
        call MPI_Comm_rank(metaData%mpi_model${name},local_rank, ierr)
    end if

    metaData%iamin_model${name}2cpl = .false.
    if(metaData%iamin_model(metaData%model${name}2cpl_id))then
        call iam_comm_root(metaData%mpi_model${name}2cpl, &
            metaData%iamroot_model${name}2cpl, ierr)
        metaData%iamin_model${name}2cpl = .true.
    end if
    #end for
   
    metaData%iamin_modelocn = .false.
    if(metaData%iamin_model(metaData%modelocn_id))then
        call iam_comm_root(metaData%mpi_modelocn, metaData%iamroot_modelocn, ierr)
        metaData%iamin_modelocn = .true.
    end if


    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !write(*,*)'before mapper_init'

    #for $model in $proc_cfgs
         #set $name = $model.name
    call mapper_init(metaData%mapper_C${name}2x, ierr)
    call mapper_init(metaData%mapper_Cx2${name}, ierr)

    #end for

    !-------------------------------------------
    !   init model desc info only used for MCT
    !-------------------------------------------
    #for $model in $proc_cfgs
         #set $name = $model.name
         #set $gsize = $model.gSize
    metaData%${name}%ID = metaData%model${name}_id
    metaData%${name}%comm = metaData%comp_comm(metaData%model${name}_id)
    metaData%${name}%gsize = $gsize
    #end for

    !-------------------------------------------
    !   init field 
    !-------------------------------------------
    call flds_init(metaData, ierr)

    !-------------------------------------------
    !   init nmlfile
    !-------------------------------------------
    call confMeta_init(metaData%conf, nmlfile, ierr=ierr)
    metaData%datanml = datanml
    metaData%datarc = datarc

end subroutine init

subroutine clean(metaData)
    
    implicit none
    type(Meta), intent(inout) :: metaData
    integer :: ierr

    call procMeta_Final(metaData%my_proc, ierr)
    #for $model in $proc_cfgs
         #set $name = $model.name
    call compMeta_Final(metaData%${name}, ierr)
    #end for

    call MPI_Finalize(ierr) 

end subroutine clean

end module procM

