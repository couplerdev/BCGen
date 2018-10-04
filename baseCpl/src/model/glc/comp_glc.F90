module comp_glc
use mct_mod
use timeM
use proc_def
use logUnitl, only: logUnit
    implicit none
     integer :: comp_id

     integer, parameter :: CXX = 4096
     character(CXX) :: fld_ar = ""
     character(CXX) :: fld_ai = ""
     character(*), parameter :: model_name = "glc"

     public :: glc_init_mct
     public :: glc_run_mct
     public :: glc_final_mct

contains

subroutine glc_init_mct(my_proc, ID, EClock, gsMap_glcglc,&
        glc2x_glcglc, x2glc_glcglc, domain, ierr)
     implicit none
     type(proc), intent(inout)      :: my_proc
     integer, intent(in)            :: ID
     type(Clock), intent(in)        :: EClock
     type(gsMap), intent(inout)     :: gsMap_glcglc
     type(AttrVect), intent(inout)  :: glc2x_glcglc
     type(AttrVect), intent(inout)  :: x2glc_glcglc
     type(gGrid),    intent(inout)  :: domain
     integer. intent(inout) :: ierr
     integer :: local_comm, i

     logical :: first_time = .true.
     
     integer :: ngy, ngx
     integer :: nlseg
     integer :: llseg

     integer, allocatable :: start(:)
     integer, allocatable :: length(:)
     integer              :: root = 0
     integer              :: comm_rank
     integer              :: comm_size
     integer              :: lsize, gsize

     character(*), parameter :: F00 ="('(glc_init_mct) ',8a)"
     character(*), parameter :: F01 ="('(glc_init_mct) ',a,4i8)"
     character(*), parameter :: F02 ="('(glc_init_mct) ',a,4es13.6)"
     character(*), parameter :: F03 ="('(glc_init_mct) ',a,i8,a)"
     character(*), parameter :: F90 ="('(glc_init_mct) ',73('='))"
     character(*), parameter :: F91 ="('(glc_init_mct) ',73('-'))"
     character(*), parameter :: subName = "(glc_init_mct)"

     local_comm = my_proc%comp_comm(ID)
     call mpi_comm_rank(local_comm, comm_rank, ierr)
     call mpi_comm_size(local_comm, comm_size, ierr)
 
     ngx = 10
     ngy = 10
     nlseg = 1
     nproc = comm_size

     if(comm_rank == root) then
         write(logUnit, *) 'read in xglc input from file = xglc_in'
         write(logUnit, F00)
         write(logUnit, F00) '          Model :  ', trim(model_name)
         write(logUnit, F01) '            NGX :  ', ngx
         write(logUnit, F01) '            NGY :  ', ngy
         write(logUnit, F03) '  Decomposition :  ', 0
         write(logUnit, F03) '  Num pes in X  :  ', nproc
         write(logUnit, F03) '  Num of local Segment :  ', nlseg
         write(logUnit, F00)
     end if

     !------------------------------------
     ! init time manager 
     !------------------------------------


     !----------------------------------
     !  init mct grid 
     !--------------------------------

end subroutine glc_init_mct


subroutine glc_run_mct()



end subroutine glc_run_mct


subroutine glc_final_mct()


       
end subroutine glc_final_mct


end module comp_glc
