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
    use miccg_hormone, only: miccg, get_preconditioner, Apk
    type(cg_set), intent(inout) :: cg
    real(8), allocatable, intent(in)    :: b(:)  ! Right-hand side vector
    real(8), allocatable, intent(inout) :: x(:)  ! Input vector (right-hand side or initial guess), contains the solution on output
    real(8), allocatable :: b_verify(:)
    real(8) :: error

    ! Call the MICCG solver
    print *, "Calling the MICCG solver..."
    call get_preconditioner(cg)

    call miccg(cg, b, x)
    print*, "MICCG solver finished"

    ! Verify solution
    print*, "Verifying solution by multiplying Ax"
    allocate(b_verify(size(x)))
    call Apk(cg,x,b_verify)
    ! Error in solution
    error = sum(abs(b/b_verify - 1))/size(b)
    print*, "  Error in b: ", error
    if (error > 1.0d-5) then
      print*, "  Error in b is too large"
    else
      print*, "  Solution is correct"
    end if

  end subroutine solve_sparse_system

end module sparse_matrix_solver

program test_sparse_solver
  use datatype, only: cg_set
  use sparse_matrix_solver, only: setup_matrix, solve_sparse_system
  implicit none

  type(cg_set) :: cg
  integer :: lmax  ! Size of the matrix
  real(8), allocatable :: x(:) ! Input/output vector
  real(8), allocatable :: b(:) ! Right-hand side vector

  real(8), allocatable :: x_ref(:) ! Reference solution

  logical :: use_reference_matrix = .true.

  ! ------------------------------------------------------------
  if (use_reference_matrix) then
    print*, "  Using reference matrix"

    ! Files dumped from radshock_x test, using:

    ! ! Export A and ia from cg into separate binary dump files
    ! open(unit=10, file="A_dump.bin", form="unformatted", status="replace")
    ! write(10) cg%A
    ! close(10)

    ! open(unit=11, file="ia_dump.bin", form="unformatted", status="replace")
    ! write(11) cg%ia
    ! close(11)

    ! open(unit=11, file="rsrc_dump.bin", form="unformatted", status="replace")
    ! write(11) rsrc
    ! close(11)

    ! open(unit=12, file="x_dump.bin", form="unformatted", status="replace")
    ! write(12) x
    ! close(12)

    ! ! Write the size information to a text file
    ! open(unit=13, file="array_size.txt", form="formatted", status="replace")
    ! write(13, '(I0)') cg%lmax
    ! close(13)

    ! call miccg(cg, rsrc, x) ! returns erad^{n+1}

    ! ! Write the updated x to a binary file
    ! open(unit=14, file="x_updated.bin", form="unformatted", status="replace")
    ! write(14) x
    ! close(14)

    ! stop 'Files dumped'


    ! Read array sizes from text file
    open(unit=13, file="reference_matrix/array_size.txt", status="old", &
         form="formatted", action="read")
    read(13, '(I4)') lmax
    close(13)

    print*, "Reference matrix lmax: ", lmax

    ! Allocate arrays
    allocate(x(lmax), b(lmax))
    allocate(x_ref(lmax))
    cg%lmax = lmax
    cg%Adiags = 2
    cg%cdiags = 2
    allocate(cg%A(cg%Adiags, cg%lmax))
    allocate(cg%c(cg%cdiags, cg%lmax))
    allocate(cg%ia(cg%Adiags))
    allocate(cg%ic(cg%cdiags))
    cg%ia = [0, 1]
    cg%ic = [0, 1]

    ! Read matrix data from binary dumps
    open(unit=10, file="reference_matrix/A_dump.bin", status="old", form="unformatted")
    read(10) cg%A
    close(10)

    open(unit=10, file="reference_matrix/ia_dump.bin", status="old", form="unformatted")
    read(10) cg%ia
    close(10)

    ! Read right-hand side vector
    open(unit=10, file="reference_matrix/rsrc_dump.bin", status="old", form="unformatted")
    read(10) b
    close(10)

    ! Read initial x vector
    open(unit=10, file="reference_matrix/x_dump.bin", status="old", form="unformatted")
    read(10) x
    close(10)

    ! Read reference solution
    open(unit=10, file="reference_matrix/x_updated.bin", status="old", form="unformatted")
    read(10) x_ref
    close(10)
  ! ------------------------------------------------------------
  else
    print*, "Generating test matrix"
    ! Generate test matrix
    lmax = 5

    ! Allocate memory for x and b
    allocate(x(lmax), b(lmax))

    ! Set up the test matrix
    call setup_matrix(cg, lmax)

    ! Set up a test right-hand side vector b
    b = [2.0d0, 3.0d0, 1.0d0, 5.0d0, 4.0d0]

    ! Set up a test input vector x
    x = [1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0]
  endif
  ! ------------------------------------------------------------

  ! Call the solver (placeholder implementation)
  call solve_sparse_system(cg, b, x)

  ! If x_ref is available, compare the solution
  if (use_reference_matrix) then
    print*, "Comparing the solution with the reference solution:"
    print*, "  Error in solution: ", sum(abs(x/x_ref - 1))/size(x)
  endif

end program test_sparse_solver