module tools
  use datatype, only: cg_set
  implicit none
  private
  public :: verify, compare
contains

! Verify solution by multiplying Ax
subroutine verify(cg, x, b)
  use miccg_hormone, only: Apk
  type(cg_set), intent(in) :: cg
  real(8), dimension(:), allocatable, intent(in) :: x
  real(8), dimension(:), allocatable, intent(in) :: b
  real(8), dimension(:), allocatable :: b_verify
  real(8), parameter :: tol = 1.0d-5
  real(8) :: error

  print*, "--> Verifying solution by multiplying Ax"
  allocate(b_verify(size(b)))

  call Apk(cg,x,b_verify)
  error = sum(abs(b/b_verify - 1))/size(b)
  print*, "  Error in b: ", error

  if (error > tol) then
    print*, "  Error in b is too large"
  else
    print*, "  Solution is correct"
  end if

end subroutine verify

subroutine compare(x, x_ref)
  real(8), allocatable, intent(in) :: x(:), x_ref(:)

  print*, "--> Comparing the solution with the reference solution:"
  print*, "  Error in solution: ", sum(abs(x/x_ref - 1))/size(x)

end subroutine compare

end module tools
