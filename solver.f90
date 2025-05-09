module solver
  use datatype, only: cg_set
  use mpi_utils, only: myrank, barrier_mpi, allreduce_mpi
  use omp_lib
  implicit none
  private

  integer, public, parameter :: miccg_solver = 1
  integer, public, parameter :: petsc_solver = 2

  public :: solve_sparse_system

  contains

  subroutine solve_sparse_system(cg, b, x, solver)
    use hormone_routines, only: miccg, get_preconditioner
    use petsc_routines, only: solve_system_petsc
    type(cg_set), intent(inout) :: cg
    real(8), allocatable, intent(in)    :: b(:)  ! Right-hand side vector
    real(8), allocatable, intent(inout) :: x(:)  ! Input vector (right-hand side or initial guess), contains the solution on output
    integer, intent(in) :: solver
    real(8) :: start_time, end_time, pc_time, ksp_time
    integer :: iterations

    select case (solver)
    case (miccg_solver)
      print*, "--> Solving using hormone MICCG..."
      start_time = omp_get_wtime()
      call get_preconditioner(cg)
      end_time = omp_get_wtime()
      pc_time = end_time - start_time
      start_time = omp_get_wtime()
      call miccg(cg, b, x)
      end_time = omp_get_wtime()
      ksp_time = end_time - start_time

    case (petsc_solver)
      if (myrank==0) print*, "--> Solving using PETSc..."
      call solve_system_petsc(cg, b, x, pc_time, ksp_time, iterations)
      call barrier_mpi()
      if (myrank==0) print*, "    Converged after ", iterations, " iterations"
      call allreduce_mpi('max', pc_time)
      call allreduce_mpi('max', ksp_time)
    case default
      stop "Error: Unknown solver"

    end select

    ! TODO: fix timings for MPI
    if (myrank==0) then
      print*, "    Time taken (PC) : ", pc_time
      print*, "    Time taken (KSP): ", ksp_time
      print*, "    Total time      : ", pc_time + ksp_time
    endif
  end subroutine solve_sparse_system

end module solver
