module petsc_solver
#include <petsc/finclude/petsc.h>
  use petsc

  implicit none
  private
  public :: sparse_solve

  contains
  subroutine sparse_solve(cg,b,x)
    use datatype, only: cg_set
    type(cg_set), intent(inout) :: cg
    real(8), allocatable, intent(in)    :: b(:)  ! Right-hand side vector
    real(8), allocatable, intent(inout) :: x(:)  ! Input vector (right-hand side or initial guess), contains the solution on output
    integer :: row, col, d
    real(8) :: start_time, end_time
    PetscErrorCode :: ierr
    Mat            :: A_petsc
    Vec            :: x_petsc, b_petsc
    KSP            :: ksp
    PetscInt       :: n
    PetscScalar    :: val
    PetscScalar, pointer :: vec_ptr(:)

    ! Initialize PETSc
    call PetscInitialize(PETSC_NULL_CHARACTER, ierr)
    if (ierr /= 0) stop 'PETSc initialization failed'

    ! Set matrix and vector size
    n = size(b)

    ! Create matrix and vectors
    call MatCreate(PETSC_COMM_WORLD, A_petsc, ierr)
    call MatSetSizes(A_petsc, PETSC_DECIDE, PETSC_DECIDE, n, n, ierr)
    call MatSetFromOptions(A_petsc, ierr)
    call MatSetUp(A_petsc, ierr)

    call VecCreate(PETSC_COMM_WORLD, b_petsc, ierr)
    call VecSetSizes(b_petsc, PETSC_DECIDE, n, ierr)
    call VecSetFromOptions(b_petsc, ierr)

    call VecDuplicate(b_petsc, x_petsc, ierr)

    do row = 1, cg%lmax
      do d = 1, cg%Adiags
        col = row + cg%ia(d)  ! Compute column index
        if (col >= 1 .and. col <= cg%lmax) then
          val = cg%A(d, row)
          call MatSetValue(A_petsc, row-1, col-1, val, INSERT_VALUES, ierr)
          if (d /= 1) call MatSetValue(A_petsc, col-1, row-1, val, INSERT_VALUES, ierr)
        end if
      end do
    end do

    call MatAssemblyBegin(A_petsc, MAT_FINAL_ASSEMBLY, ierr)
    call MatAssemblyEnd(A_petsc, MAT_FINAL_ASSEMBLY, ierr)

    ! Set right-hand side vector b
    call VecGetArrayF90(b_petsc, vec_ptr, ierr)
    vec_ptr(:) = b(:)
    call VecRestoreArrayF90(b_petsc, vec_ptr, ierr)

    ! Create linear solver context
    call KSPCreate(PETSC_COMM_WORLD, ksp, ierr)
    call KSPSetOperators(ksp, A_petsc, A_petsc, ierr)
    call KSPSetFromOptions(ksp, ierr)

    ! Solve Ax = b
    call cpu_time(start_time)  ! Start timing
    call KSPSolve(ksp, b_petsc, x_petsc, ierr)
    call cpu_time(end_time)  ! End timing
    write(*,*) 'Time taken for KSPSolve: ', end_time-start_time, ' seconds'

    ! View the solution
    ! call VecView(x_petsc, PETSC_VIEWER_STDOUT_WORLD, ierr)

    call VecGetArrayF90(x_petsc, vec_ptr, ierr)
    x(:) = vec_ptr(:)
    call VecRestoreArrayF90(x_petsc, vec_ptr, ierr)

    ! view matrix
    ! call MatView(A_petsc, PETSC_VIEWER_STDOUT_WORLD, ierr)

    ! Clean up
    call KSPDestroy(ksp, ierr)
    call VecDestroy(b_petsc, ierr)
    call VecDestroy(x_petsc, ierr)
    call MatDestroy(A_petsc, ierr)

    ! Finalize PETSc
    call PetscFinalize(ierr)
  end subroutine sparse_solve

end module petsc_solver
