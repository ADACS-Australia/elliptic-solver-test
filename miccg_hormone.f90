! HORMONE's implementation of the MICCG solver
module miccg_hormone
  implicit none
  private
  public :: miccg, get_preconditioner

  contains

!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!
!                            SUBROUTINE MICCG
!
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

! PURPOSE: To solve Ax=b using the MICCG method

subroutine miccg(cg,b,x)
  use datatype, only: cg_set
  type(cg_set),intent(in):: cg
  real(8),allocatable,intent(in):: b(:)
  real(8),allocatable,intent(inout):: x(:)
  real(8),allocatable:: r(:),q(:),p(:)
  real(8):: alpha,beta,pAp,rr_old,rr,error,normb
  integer:: n,l,lmax
  real(8),parameter:: cgerr = 1d-6

  !-----------------------------------------------------------------------------

  lmax = cg%lmax
  allocate(r(1:lmax),q(1:lmax),p(1:lmax))

  normb = 0d0
  !$omp parallel

  !$omp do private(l) reduction(+:normb)
  do l = 1, lmax
    normb = normb + b(l)**2
  end do
  !$omp end do

  call Apk(cg,x,p)

  !$omp do private(l)
  do l = 1, lmax
    r(l) = b(l) - p(l) ! r0 = b - Ax0
  end do
  !$omp end do

  !$omp single
  call cctr(cg,r,p) ! p0 = (CC^T)^{-1}r0

  rr_old = 0d0
  !$omp end single
  !$omp do private(l) reduction(+: rr_old)
  do l = 1, lmax
    rr_old = rr_old + r(l)*p(l) ! rr_old = (r0,p0)
  end do
  !$omp end do

  ! Start loop ===============================================================
  main_loop: do n = 1, lmax

    !!$!$omp master
    !!$  print*,n,norm2(r)/norm2(b)
    !!$!$omp end master
    call Apk(cg,p,q) ! q=Ap_k

    !$omp single
    pAp = 0d0
    !$omp end single
    !$omp do private(l) reduction(+:pAp)
    do l = 1, lmax
      pAp = pAp + p(l)*q(l)
    end do
    !$omp end do

    !$omp single
    alpha = rr_old/pAp
    error = 0d0
    !$omp end single
    !$omp do private(l) reduction(+:error)
    do l = 1, lmax
      x(l) = x(l) + alpha*p(l) ! x_k+1 = x_k + alpha*p_k
      r(l) = r(l) - alpha*q(l) ! r_k+1 = r_k - alpha*Ap_k
      error = error + r(l)**2
    end do
    !$omp end do

    !++++++++++ check convergence ++++++++++!
    if(error<cgerr**2*normb)exit main_loop
    !+++++++++++++++++++++++++++++++++++++++!

    !$omp single
    call cctr(cg,r,q) ! q = (CC^T)^{-1}r_{k+1}

    rr = 0d0
    !$omp end single
    !$omp do private(l) reduction(+:rr)
    do l = 1, lmax
      rr = rr + q(l)*r(l)
    end do
    !$omp end do

    !$omp single
    beta = rr/rr_old
    rr_old = rr
    !$omp end single

    !$omp do private(l)
    do l = 1, lmax
      p(l) = q(l) + beta*p(l)
    end do
    !$omp end do

  end do main_loop
  ! End loop =================================================================

  !$omp end parallel

  deallocate(r,p,q)

  return
end subroutine miccg

!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!
!                             SUBROUTINE APK
!
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

! PURPOSE: To calculate A*p_k

subroutine Apk(cg,p,q)
  use datatype, only: cg_set
  type(cg_set),intent(in):: cg
  real(8),allocatable,intent(in):: p(:)
  real(8),allocatable,intent(inout):: q(:)
  integer::l,ll,m

  !-----------------------------------------------------------------------------

  !$omp do private(l,ll,m)
  do l = 1, cg%lmax
    q(l) = cg%A(1,l)*p(l)
    do ll = 2, cg%Adiags
      m = cg%ia(ll)
      if(l>m         ) q(l) = q(l) + cg%A(ll,l-m)*p(l-m)
      if(l<=cg%lmax-m) q(l) = q(l) + cg%A(ll,l  )*p(l+m)
    end do
  end do
  !$omp end do

  return
end subroutine Apk

!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!
!                            SUBROUTINE CCTR
!
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

! PURPOSE: To compute Incomplete Cholesky decomposition
!          q = (CC^T)^{-1}r_k

subroutine cctr(cg,r,q)
  use datatype, only: cg_set
  type(cg_set),intent(in):: cg
  real(8),allocatable,intent(in):: r(:)
  real(8),allocatable,intent(inout):: q(:)
  real(8),allocatable:: y(:)
  integer::l,ll,m

  !-----------------------------------------------------------------------------

  allocate(y,mold=r)

  do l = 1, cg%lmax
    y(l) = r(l)
    do ll = 2, cg%cdiags
      m = cg%ic(ll)
      if(l>m)then
        y(l) = y(l) - cg%c(ll,l-m)/cg%c(1,l-m)*y(l-m)
      end if
    end do
  end do

  do l = cg%lmax, 1, -1
    q(l) = y(l)
    do ll = 2, cg%cdiags
      m = cg%ic(ll)
      if(l<=cg%lmax-m)then
        q(l) = q(l) - cg%c(ll,l)*q(l+m)
      end if
    end do
    q(l) = q(l)/cg%c(1,l)
  end do

  return
end subroutine cctr

!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!
!                      SUBROUTINE GET_PRECONDITIONER
!
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

! PURPOSE: To calculate preconditioner matrix elements

subroutine get_preconditioner(cg)
  use datatype, only: cg_set
  type(cg_set),intent(inout):: cg
  integer:: l,ll,ld,i,n,m
  logical:: found

 !-----------------------------------------------------------------------------

 ! Calculate pre-conditioner matrix elements

 !$omp parallel do private(l,ll,ld)
  do l = 1, cg%lmax
   do ll = 1, cg%cdiags
    cg%c(ll,l) = 0d0
    do ld = 1, cg%adiags
     if(cg%ia(ld)==cg%ic(ll))then
      cg%c(ll,l) = cg%A(ld,l)
     end if
    end do
   end do
  end do
 !$omp end parallel do

  do l = 1, cg%lmax

   do ll = 2, cg%cdiags
    m = cg%ic(ll)
    if(l>m)then
     do ld = ll, cg%cdiags
      n = cg%ic(ld)
      found = .false.
      find_eq_loop:do i = 1, cg%cdiags
       if(cg%ic(i)==n-m)then
        cg%c(i,l) = cg%c(i,l) - cg%c(ll,l-m)*cg%c(ld,l-m)/cg%c(1,l-m)
        found = .true.
        exit find_eq_loop
       end if
      end do find_eq_loop
      if(.not.found)then
       cg%c(1,l) = cg%c(1,l) &
                 - cg%alpha*cg%c(ll,l-m)*cg%c(ld,l-m)/cg%c(1,l-m)
       if(l>n)then
        cg%c(1,l) = cg%c(1,l) &
                  - cg%alpha*cg%c(ll,l-n)*cg%c(ld,l-n)/cg%c(1,l-n)
       end if
      end if
     end do
    end if

   end do

  end do


 return
 end subroutine get_preconditioner


end module miccg_hormone