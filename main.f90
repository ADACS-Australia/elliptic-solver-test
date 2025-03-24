program test_sparse_solver
  use datatype, only: cg_set
  use setup, only: setup_system
  use solver, only: solve_sparse_system
  use petsc_solver, only: sparse_solve

  implicit none
  type(cg_set) :: cg
  real(8), allocatable :: x(:) ! Input/output vector
  real(8), allocatable :: b(:) ! Right-hand side vector
  real(8), allocatable :: x_ref(:) ! Reference solution

  logical :: use_reference_matrix = .true.

  call setup_system(cg, x, b, x_ref, use_reference_matrix)

  !-- MICCG solver ---!
  ! Call the solver (placeholder implementation)
  call solve_sparse_system(cg, b, x)

  ! If x_ref is available, compare the solution
  if (use_reference_matrix) then
    print*, "Comparing the solution with the reference solution:"
    print*, "  Error in solution: ", sum(abs(x/x_ref - 1))/size(x)
  endif

  !-- PETSc solver ---!
  call sparse_solve(cg, b, x)

  ! If x_ref is available, compare the solution
  if (use_reference_matrix) then
    print*, "Comparing the solution with the reference solution:"
    print*, "  Error in solution: ", sum(abs(x/x_ref - 1))/size(x)
  endif

end program test_sparse_solver
