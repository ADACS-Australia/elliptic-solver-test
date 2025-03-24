module solver
  implicit none
  private
  public :: solve_sparse_system

  contains

  ! Placeholder for the solver subroutine, solving Ax = b
  subroutine solve_sparse_system(cg, b, x)
    use datatype, only: cg_set
    use miccg_hormone, only: miccg, get_preconditioner, Apk
    type(cg_set), intent(inout) :: cg
    real(8), allocatable, intent(in)    :: b(:)  ! Right-hand side vector
    real(8), allocatable, intent(inout) :: x(:)  ! Input vector (right-hand side or initial guess), contains the solution on output
    real(8), allocatable :: b_verify(:)
    real(8) :: error
    real(8) :: start_time, end_time

    call cpu_time(start_time)
    ! Call the MICCG solver
    print *, "Calling the MICCG solver..."
    call get_preconditioner(cg)

    call miccg(cg, b, x)
    print*, "MICCG solver finished"
    call cpu_time(end_time)

    print*, "Time taken: ", end_time - start_time

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

end module solver
