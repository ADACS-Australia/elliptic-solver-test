module setup
  use datatype, only: cg_set
  use mpi_utils, only: myrank
  implicit none
  private

  public :: setup_system

  contains

  ! Subroutine to set up the test matrix
  subroutine setup_simple(cg, x, b)
    type(cg_set), intent(out) :: cg
    real(8), allocatable, intent(inout) :: x(:), b(:)
    integer, parameter :: Adiags = 2  ! Number of diagonals
    integer, parameter :: cdiags = 2  ! Number of diagonals
    integer, parameter :: lmax = 5    ! Number of rows/columns
    integer :: l  ! Loop index

    ! Allocate memory for x and b
    allocate(x(lmax), b(lmax))

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

    ! Set up a test right-hand side vector b
    b = [2.0d0, 3.0d0, 1.0d0, 5.0d0, 4.0d0]

    ! Set up a test input vector x
    x = [1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0]

  end subroutine setup_simple

  subroutine setup_reference(cg, x, b, x_ref)
  type(cg_set), intent(out) :: cg
  real(8), allocatable, intent(inout) :: x(:), b(:), x_ref(:)
  integer :: lmax, Adiags, cdiags  ! Size of the matrix

  character(len=*), parameter :: grv_path = "reference_matrix/grv/"
  character(len=*), parameter :: rad_path = "reference_matrix/rad/"
  character(len=256) :: ref_path
  integer, parameter :: problem_type = 1  ! 1 for rad, 2 for grv

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

    ! Decide which reference to load based on the problem_type parameter
    select case (problem_type)
      case (1)
      ref_path = rad_path
      case (2)
      ref_path = grv_path
      case default
      stop "Invalid problem_type specified"
    end select

    ! Read array sizes from text file
    open(unit=13, file=trim(ref_path)//"array_size.txt", status="old", &
       form="formatted", action="read")
    read(13, '(I6)') lmax
    close(13)

    open(unit=13, file=trim(ref_path)//"Adiags.txt", status="old", &
       form="formatted", action="read")
    read(13, '(I6)') Adiags
    close(13)

    open(unit=13, file=trim(ref_path)//"cdiags.txt", status="old", &
       form="formatted", action="read")
    read(13, '(I6)') cdiags
    close(13)

    if (myrank==0) print*, "    lmax: ", lmax
    if (myrank==0) print*, "    Adiags: ", Adiags
    if (myrank==0) print*, "    cdiags: ", cdiags

    ! Allocate arrays
    allocate(x(lmax), b(lmax))
    allocate(x_ref(lmax))
    allocate(cg%A(Adiags, lmax))
    allocate(cg%c(cdiags, lmax))
    allocate(cg%ia(Adiags))
    allocate(cg%ic(cdiags))

    cg%lmax = lmax
    cg%Adiags = Adiags
    cg%cdiags = cdiags

    ! Read matrix data from binary dumps
    open(unit=10, file=trim(ref_path)//"A_dump.bin", status="old", form="unformatted")
    read(10) cg%A
    close(10)

    open(unit=10, file=trim(ref_path)//"ia_dump.bin", status="old", form="unformatted")
    read(10) cg%ia
    close(10)

    open(unit=10, file=trim(ref_path)//"ic_dump.bin", status="old", form="unformatted")
    read(10) cg%ic
    close(10)

    ! Read right-hand side vector
    open(unit=10, file=trim(ref_path)//"rsrc_dump.bin", status="old", form="unformatted")
    read(10) b
    close(10)

    ! Read initial x vector
    open(unit=10, file=trim(ref_path)//"x_dump.bin", status="old", form="unformatted")
    read(10) x
    close(10)

    ! Read reference solution
    open(unit=10, file=trim(ref_path)//"x_updated.bin", status="old", form="unformatted")
    read(10) x_ref
    close(10)

  end subroutine setup_reference

  subroutine setup_system(cg, x, b, x_ref, use_reference_matrix)
  type(cg_set), intent(inout) :: cg
  real(8), allocatable, intent(inout) :: x(:) ! Input/output vector
  real(8), allocatable, intent(inout) :: b(:) ! Right-hand side vector
  real(8), allocatable, intent(inout) :: x_ref(:) ! Reference solution
  logical, intent(in) :: use_reference_matrix

  if (use_reference_matrix) then
    if (myrank==0) print*, "--> Reading reference matrix"
    call setup_reference(cg, x, b, x_ref)
  else
    if (myrank==0) print*, "--> Generating test matrix"
    call setup_simple(cg, x, b)
  endif

  end subroutine setup_system

end module setup
