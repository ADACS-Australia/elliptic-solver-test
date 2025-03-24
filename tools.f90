module tools
  use datatype, only: cg_set
  implicit none
  private

  public :: verify, compare

contains

! Verify solution by multiplying Ax
subroutine verify(cg, x, b)
  use hormone_routines, only: Apk
  type(cg_set), intent(in) :: cg
  real(8), dimension(:), allocatable, intent(in) :: x
  real(8), dimension(:), allocatable, intent(in) :: b
  real(8), dimension(:), allocatable :: b_verify
  real(8) :: error

  print*, "--> Verifying solution by multiplying Ax"
  allocate(b_verify(size(b)))

  call Apk(cg,x,b_verify)
  error = sum(abs(b/b_verify - 1))/size(b)
  print*, "    Error in b: ", error

end subroutine verify

! Compare with a reference solution
subroutine compare(x, x_ref)
  real(8), allocatable, intent(in) :: x(:), x_ref(:)

  print*, "--> Comparing with reference x"
  print*, "    Error in x: ", sum(abs(x/x_ref - 1))/size(x)

end subroutine compare

end module tools
