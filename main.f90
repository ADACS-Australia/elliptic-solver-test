program test_sparse_solver
  use datatype, only: cg_set
  use setup, only: setup_system
  use solver, only: solve_sparse_system, miccg_solver, petsc_solver
  use tools, only: verify, compare
  use mpi_utils, only: init_mpi, finalize_mpi

  implicit none

  type(cg_set) :: cg
  real(8), allocatable :: x(:) ! Input/output vector
  real(8), allocatable :: b(:) ! Right-hand side vector
  real(8), allocatable :: x_ref(:) ! Reference solution
  logical, parameter :: use_reference_matrix = .true.

  call init_mpi()

  call setup_system(cg, x, b, x_ref, use_reference_matrix)

  print*, "------------------------------------------"

  ! Call the solver (placeholder implementation)
  call solve_sparse_system(cg, b, x, miccg_solver)
  call verify(cg, x, b)
  if (use_reference_matrix) call compare(x, x_ref)

  print*, "------------------------------------------"

  !-- PETSc solver ---!
  call solve_sparse_system(cg, b, x, petsc_solver)
  call verify(cg, x, b)
  if (use_reference_matrix) call compare(x, x_ref)

  call finalize_mpi()

end program test_sparse_solver
