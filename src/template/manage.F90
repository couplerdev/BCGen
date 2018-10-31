module procM

use mct_mod
use comms_def, only: mct_mod
use proc_def, only: procMeta, compMeta
use global_var
use field_def
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
    type(Meta),      intent(inout) :: metaData
    integer :: ierr    
    integer :: num_rank
    integer :: num_size 
    integer :: iter

    metaData%num_models = 2
    metaData%num_comms =  2*2+2
    
    

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

    call deploy(metaData%mpi_glocomm, metaData%mpi_modelatm,&
                metaData%mpi_modelatm2cpl, &
                metaData%modelatm_id, metaData%cplid, &
                metaData%modelatm2cpl_id, metaData%iamin_model, 0, ierr)
    call deploy(metaData%mpi_glocomm, metaData%mpi_modelocn,&
                metaData%mpi_modelocn2cpl, &
                metaData%modelocn_id, metaData%cplid, &
                metaData%modelocn2cpl_id, metaData%iamin_model, 0, ierr)

    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    write(*,*), "my_world_rank:", num_rank, " my_in_model", metaData%iamin_model
    call MPI_Barrier(MPI_COMM_WORLD, ierr)


    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !write(*,*)'comm initiated'
    allocate(metaData%comp_comm(metaData%ncomps))
    metaData%comp_comm(metaData%gloid)       = metaData%mpi_glocomm
    metaData%comp_comm(metaData%cplid)       = metaData%mpi_cpl

    metaData%comp_comm(metaData%modelatm_id)     = metaData%mpi_modelatm
    metaData%comp_comm(metaData%modelatm2cpl_id) =metaData%mpi_modelatm2cpl 
    metaData%comp_comm(metaData%modelocn_id)     = metaData%mpi_modelocn
    metaData%comp_comm(metaData%modelocn2cpl_id) =metaData%mpi_modelocn2cpl 

    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    write(*,*)'comp_comm initiated'
    allocate(metaData%comp_id(metaData%ncomps))
    do iter = 1, metaData%ncomps
        metaData%comp_id(iter) = iter
    end do

    call mct_world_init(metaData%ncomps, MPI_COMM_WORLD, metaData%comp_comm, metaData%comp_id)

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

    metaData%iamin_modelatm = .false.
    if(metaData%iamin_model(metaData%modelatm_id))then
        call iam_comm_root(metaData%mpi_modelatm, metaData%iamroot_modelatm, ierr)
        metaData%iamin_modelatm = .true.
    end if

    metaData%iamin_modelatm2cpl = .false.
    if(metaData%iamin_model(metaData%modelatm2cpl_id))then
        call iam_comm_root(metaData%mpi_modelatm2cpl, &
            metaData%iamroot_modelatm2cpl, ierr)
        metaData%iamin_modelatm2cpl = .true.
    end if
    metaData%iamin_modelocn = .false.
    if(metaData%iamin_model(metaData%modelocn_id))then
        call iam_comm_root(metaData%mpi_modelocn, metaData%iamroot_modelocn, ierr)
        metaData%iamin_modelocn = .true.
    end if

    metaData%iamin_modelocn2cpl = .false.
    if(metaData%iamin_model(metaData%modelocn2cpl_id))then
        call iam_comm_root(metaData%mpi_modelocn2cpl, &
            metaData%iamroot_modelocn2cpl, ierr)
        metaData%iamin_modelocn2cpl = .true.
    end if
   
    metaData%iamin_modelocn = .false.
    if(metaData%iamin_model(metaData%modelocn_id))then
        call iam_comm_root(metaData%mpi_modelocn, metaData%iamroot_modelocn, ierr)
        metaData%iamin_modelocn = .true.
    end if


    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !write(*,*)'before mapper_init'

    call mapper_init(metaData%mapper_Catm2x, ierr)
    call mapper_init(metaData%mapper_Cx2atm, ierr)

    call mapper_init(metaData%mapper_Cocn2x, ierr)
    call mapper_init(metaData%mapper_Cx2ocn, ierr)


    !-------------------------------------------
    !   init model desc info only used for MCT
    !-------------------------------------------
    metaData%atm%ID = ATMID
    metaData%atm%comm = metaData%comp_comm(atmid)
    metaData%ocn%ID = ATMID
    metaData%ocn%comm = metaData%comp_comm(ocnid)

end subroutine init

subroutine clean(metaData)
    
    implicit none
    type(Meta), intent(inout) :: metaData
    integer :: ierr

    call procMeta_Final(metaData%proc, ierr)
    call compMeta_Final(metaData%atm, ierr)
    call compMeta_Final(metaData%ocn, ierr)

    call MPI_Finalize(ierr) 

end subroutine clean

end module procM

