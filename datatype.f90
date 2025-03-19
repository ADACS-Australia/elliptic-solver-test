module datatype
  implicit none
  type cg_set
    integer :: lmax        ! Size of the matrix
    integer :: Adiags        ! Number of diagonals
    integer :: cdiags        ! Number of diagonals
    real(8), allocatable :: A(:, :)  ! Matrix diagonals (Adiags x lmax)
    real(8), allocatable :: c(:, :)  ! Preconditioner diagonals (cdiags x lmax)
    integer, allocatable :: ia(:)    ! Diagonal offsets
    integer, allocatable :: ic(:)    ! Diagonal offsets
    real(8):: alpha
  end type cg_set
end module datatype