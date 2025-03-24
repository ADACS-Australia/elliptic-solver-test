module solver
  use datatype, only: cg_set
  implicit none
  private

  integer, public, parameter :: miccg_solver = 1
  integer, public, parameter :: petsc_solver = 2

  public :: solve_sparse_system

  contains

  ! Placeholder for the solver subroutine, solving Ax = b
  subroutine solve_sparse_system(cg, b, x, solver)
    use hormone_routines, only: miccg, get_preconditioner
    use petsc_solver, only: solve_system_petsc
    type(cg_set), intent(inout) :: cg
    real(8), allocatable, intent(in)    :: b(:)  ! Right-hand side vector
    real(8), allocatable, intent(inout) :: x(:)  ! Input vector (right-hand side or initial guess), contains the solution on output
    integer, intent(in) :: solver
    real(8) :: start_time, end_time

    select case (solver)
    case (miccg_solver)
      print*, "--> Solving using hormone MICCG..."
      call cpu_time(start_time)
      call get_preconditioner(cg)
      call miccg(cg, b, x)
      call cpu_time(end_time)

    case (petsc_solver)
      print*, "--> Solving using PETSc..."
      call solve_system_petsc(cg, b, x, start_time, end_time)

    case default
      stop "Error: Unknown solver"

    end select

    print*, "    Time taken: ", end_time - start_time

  end subroutine solve_sparse_system

end module solver
