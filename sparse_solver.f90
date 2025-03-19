module sparse_matrix_solver
  implicit none
  private
  public :: setup_matrix, solve_sparse_system

  contains

  ! Subroutine to set up the test matrix
  subroutine setup_matrix(cg, lmax)
    use datatype, only: cg_set
    type(cg_set), intent(out) :: cg
    integer, intent(in) :: lmax
    integer :: l
    integer, parameter :: Adiags = 2  ! Number of diagonals
    integer, parameter :: cdiags = 2  ! Number of diagonals

    ! Allocate memory for the matrix
    cg%lmax = lmax
    cg%Adiags = Adiags
    allocate(cg%A(Adiags, lmax))
    allocate(cg%ia(Adiags))

    ! Define the diagonal offsets
    cg%ia = [0, 1]  ! Example: main diagonal, first upper

    ! Initialize the matrix with some test values
    cg%A = 0.0d0
    do l = 1, cg%lmax
      ! Main diagonal
      cg%A(1, l) = 1.0d0 ! Example value

      ! First upper diagonal
      if (l < lmax) cg%A(2, l) = -2.0 ! Example value
    end do

    ! Allocate memory for the preconditioner
    cg%cdiags = cdiags
    allocate(cg%c(cdiags, lmax))
    allocate(cg%ic(cdiags))
    cg%ic = [0, 1]  ! Example: main diagonal, first upper

    cg%alpha = 0.99d0
  end subroutine setup_matrix

  ! Placeholder for the solver subroutine, solving Ax = b
  subroutine solve_sparse_system(cg, b, x)
    use datatype, only: cg_set
    use miccg_hormone, only: miccg, get_preconditioner
    type(cg_set), intent(inout) :: cg
    real(8), allocatable, intent(in)    :: b(:)  ! Right-hand side vector
    real(8), allocatable, intent(inout) :: x(:)  ! Input vector (right-hand side or initial guess), contains the solution on output
    integer :: l

    ! Print the matrix and result for debugging
    print *, "Matrix diagonals:"
    do l = 1, cg%Adiags
      print *, "Diagonal ", cg%ia(l), ": ", cg%A(l, :)
    end do
    print *, "Input vector x: ", x

    ! Call the MICCG solver
    print *, "Calling the MICCG solver..."
    call get_preconditioner(cg)

    call miccg(cg, b, x)
    print*, "MICCG solver finished"
    print *, "Solution vector x: ", x
  end subroutine solve_sparse_system

end module sparse_matrix_solver

program test_sparse_solver
  use datatype, only: cg_set
  use sparse_matrix_solver, only: setup_matrix, solve_sparse_system
  implicit none

  type(cg_set) :: cg
  integer, parameter :: lmax = 5  ! Size of the matrix
  real(8), allocatable :: x(:) ! Input/output vector
  real(8), allocatable :: b(:) ! Right-hand side vector

  ! Allocate memory for x and b
  allocate(x(lmax), b(lmax))

  ! Set up the test matrix
  call setup_matrix(cg, lmax)

  ! Set up a test right-hand side vector b
  b = [2.0d0, 3.0d0, 1.0d0, 5.0d0, 4.0d0]

  ! Set up a test input vector x
  x = [1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0]

  ! Call the solver (placeholder implementation)
  call solve_sparse_system(cg, b, x)

end program test_sparse_solver