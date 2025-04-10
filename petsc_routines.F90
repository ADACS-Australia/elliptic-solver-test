module petsc_routines
  use datatype, only: cg_set
#include <petsc/finclude/petsc.h>
  use petsc
  use mpi_utils, only: myrank, stop_mpi
  use mpi, only: MPI_COMM_WORLD
  implicit none
  private

  public :: solve_system_petsc, init_petsc, finalise_petsc

  PetscErrorCode :: ierr
  Mat :: A_petsc
  Vec :: x_petsc, b_petsc
  KSP :: ksp
  PC :: pc

  contains

  subroutine solve_system_petsc(cg,b,x,pc_time,ksp_time,iterations)
    type(cg_set), intent(inout) :: cg
    real(8), allocatable, intent(in)    :: b(:)  ! Right-hand side vector
    real(8), allocatable, intent(inout) :: x(:)  ! Input vector (right-hand side or initial guess), contains the solution on output
    real(8), intent(out) :: pc_time, ksp_time
    integer, intent(out) :: iterations
    integer :: size

    ! Initialise PETSc and setup all the data structures
    print*,"calling init_petsc()"
    call init_petsc()

    print*, "MPI_COMM_WORLD", MPI_COMM_WORLD
    print*, "PETSC_COMM_WORLD", PETSC_COMM_WORLD
    call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)
    print*, myrank, "MPI_COMM_SIZE", size
    call MPI_Comm_size(PETSC_COMM_WORLD, size, ierr)
    print*, myrank, "PETSC_COMM_SIZE", size

    print*,"calling setup_petsc()"
    call setup_petsc(cg, b, pc_time)
    print*,"calling solve_petsc()"
    call solve_petsc(ksp_time)
    print*,"calling KSPGetiterationNumber()"
    call KSPGetIterationNumber(ksp, iterations, ierr)

    print*,"calling get_solution_f90()"
    ! Get the solution back into a fortran array
    call get_solution_f90(x)

    ! View the solution
    ! call VecView(x_petsc, PETSC_VIEWER_STDOUT_WORLD, ierr)
    ! View matrix
    ! call MatView(A_petsc, PETSC_VIEWER_STDOUT_WORLD, ierr)

    print*,"calling cleanup_petsc()"
    ! Clean up PETSc and free all the data structures
    call cleanup_petsc()
    print*,"calling finalise_petsc()"
    call finalise_petsc()

  end subroutine solve_system_petsc

  subroutine init_petsc
    call PetscInitialize(PETSC_NULL_CHARACTER, ierr)
    if (ierr /= 0) stop 'PETSc initialization failed'
  end subroutine init_petsc

  subroutine setup_petsc(cg,b,pc_time)
    type(cg_set), intent(inout) :: cg
    real(8), allocatable, intent(in) :: b(:)  ! Right-hand side vector
    real(8), intent(out) :: pc_time
    integer :: row, col, d, row_start, row_end, n_vec
    PetscInt    :: n
    PetscScalar :: val
    PetscScalar, pointer :: vec_ptr(:)
    real(8) :: start_time, end_time

    ! Set matrix and vector size
    n = size(b)

    ! Create matrix and vectors
    call MatCreate(PETSC_COMM_WORLD, A_petsc, ierr)
    ! call MatSetType(A_petsc, MATSBAIJ, ierr)
    call MatSetSizes(A_petsc, PETSC_DECIDE, PETSC_DECIDE, n, n, ierr)
    ! call MatSetOption(A_petsc,MAT_SPD,PETSC_TRUE, ierr)
    ! call MatSetOption(A_petsc,MAT_SPD_ETERNAL,PETSC_TRUE, ierr)
    call MatSetFromOptions(A_petsc, ierr)
    call MatSetUp(A_petsc, ierr)

    call VecCreate(PETSC_COMM_WORLD, b_petsc, ierr)
    call VecSetSizes(b_petsc, PETSC_DECIDE, n, ierr)
    call VecSetFromOptions(b_petsc, ierr)

    call VecDuplicate(b_petsc, x_petsc, ierr)

    call MatGetOwnershipRange(A_petsc, row_start, row_end, ierr)
    print*, myrank, "    PETSc matrix ownership range: ", row_start, row_end
    ! call stop_mpi(1)

    ! do row = 1, cg%lmax
    do row = row_start+1, row_end
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
    call VecGetLocalSize(b_petsc, n_vec, ierr)
    print*, myrank, "    PETSc vector size: ", n_vec
    print*, myrank, "    row_end-row_start: ", row_end-row_start
    vec_ptr(row_start+1:row_end) = b(row_start+1:row_end)
    call VecRestoreArrayF90(b_petsc, vec_ptr, ierr)

    ! Create linear solver context
    call KSPCreate(PETSC_COMM_WORLD, ksp, ierr)
    call KSPSetOperators(ksp, A_petsc, A_petsc, ierr)
    call KSPSetFromOptions(ksp, ierr)
    call KSPGetPC(ksp,pc,ierr)
    call cpu_time(start_time)
    call PCSetup(pc,ierr)
    call cpu_time(end_time)
    call KSPSetup(ksp, ierr)
    pc_time = end_time - start_time

  end subroutine setup_petsc

  subroutine solve_petsc(ksp_time)
    real(8), intent(out) :: ksp_time
    real(8) :: start_time, end_time
    call cpu_time(start_time)
    call KSPSolve(ksp, b_petsc, x_petsc, ierr)
    call cpu_time(end_time)
    ksp_time = end_time - start_time
  end subroutine solve_petsc

  subroutine get_solution_f90(x)
    real(8), allocatable, intent(inout) :: x(:)
    PetscScalar, pointer :: vec_ptr(:)
    integer :: n_vec

    print*,'size(x)',size(x)
    call VecGetArrayF90(x_petsc, vec_ptr, ierr)
    call VecGetLocalSize(x_petsc, n_vec, ierr)
    print*, myrank, "    PETSc vector size: ", n_vec
    x(:) = vec_ptr(:)
    ! x = 1.0
    call VecRestoreArrayF90(x_petsc, vec_ptr, ierr)

  end subroutine get_solution_f90

  subroutine cleanup_petsc
    call KSPDestroy(ksp, ierr)
    call VecDestroy(b_petsc, ierr)
    call VecDestroy(x_petsc, ierr)
    call MatDestroy(A_petsc, ierr)
  end subroutine cleanup_petsc

  subroutine finalise_petsc
    call PetscFinalize(ierr)
  end subroutine finalise_petsc

end module petsc_routines
