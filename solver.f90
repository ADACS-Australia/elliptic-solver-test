module solver
  use datatype, only: cg_set
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
      call cpu_time(start_time)
      call get_preconditioner(cg)
      call cpu_time(end_time)
      pc_time = end_time - start_time
      call cpu_time(start_time)
      call miccg(cg, b, x)
      call cpu_time(end_time)
      ksp_time = end_time - start_time

    case (petsc_solver)
      print*, "--> Solving using PETSc..."
      call solve_system_petsc(cg, b, x, pc_time, ksp_time, iterations)
      print*, "    Converged after ", iterations, " iterations"

    case default
      stop "Error: Unknown solver"

    end select

    print*, "    Time taken (PC) : ", pc_time
    print*, "    Time taken (KSP): ", ksp_time
    print*, "    Total time      : ", pc_time + ksp_time

  end subroutine solve_sparse_system

end module solver
