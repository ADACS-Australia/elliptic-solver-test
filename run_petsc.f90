program test_sparse_solver
  use datatype, only: cg_set
  use setup, only: setup_system
  use solver, only: solve_sparse_system, miccg_solver, petsc_solver
  use tools, only: verify, compare
  use mpi_utils, only: init_mpi, finalize_mpi, myrank, barrier_mpi

  implicit none

  type(cg_set) :: cg
  real(8), allocatable :: x(:) ! Input/output vector
  real(8), allocatable :: b(:) ! Right-hand side vector
  real(8), allocatable :: x_ref(:) ! Reference solution
  logical, parameter :: use_reference_matrix = .true.

  call init_mpi()

  call setup_system(cg, x, b, x_ref, use_reference_matrix)

  !-- PETSc solver ---!
  call solve_sparse_system(cg, b, x, petsc_solver)
  call barrier_mpi()
  if (myrank==0) then
    call verify(cg, x, b)
    if (use_reference_matrix) call compare(x, x_ref)
  endif

  ! call finalize_mpi() ! no need to call, petsc finalizes MPI

end program test_sparse_solver
