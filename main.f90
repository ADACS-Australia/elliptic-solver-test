program test_sparse_solver
  use datatype, only: cg_set
  use setup, only: setup_system
  use solver, only: solve_sparse_system, miccg_solver
  use petsc_solver, only: sparse_solve
  use tools, only: verify, compare

  implicit none
  type(cg_set) :: cg
  real(8), allocatable :: x(:) ! Input/output vector
  real(8), allocatable :: b(:) ! Right-hand side vector
  real(8), allocatable :: x_ref(:) ! Reference solution

  logical :: use_reference_matrix = .true.

  call setup_system(cg, x, b, x_ref, use_reference_matrix)

  ! Call the solver (placeholder implementation)
  call solve_sparse_system(cg, b, x, miccg_solver)
  call verify(cg, x, b)
  if (use_reference_matrix) call compare(x, x_ref)

  !-- PETSc solver ---!
  call sparse_solve(cg, b, x)
  call verify(cg, x, b)
  if (use_reference_matrix) call compare(x, x_ref)

end program test_sparse_solver
