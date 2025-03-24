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
    use miccg_hormone, only: miccg, get_preconditioner
    type(cg_set), intent(inout) :: cg
    real(8), allocatable, intent(in)    :: b(:)  ! Right-hand side vector
    real(8), allocatable, intent(inout) :: x(:)  ! Input vector (right-hand side or initial guess), contains the solution on output
    integer, intent(in) :: solver
    real(8) :: start_time, end_time

    call cpu_time(start_time)

    select case (solver)
    case (1)
      print*, "==> Using MICCG solver..."
      call get_preconditioner(cg)
      call miccg(cg, b, x)
    case (2)
      print*, "==> Using PETSc solver..."
      stop "PETSc solver not implemented yet"
    case default
      print*, "Error: Unknown solver"
      stop
    end select

    call cpu_time(end_time)

    print*, "Time taken: ", end_time - start_time

  end subroutine solve_sparse_system

end module solver
