module gr_fitting
  use iso_fortran_env

  implicit none

contains
  function poly_fit(x, y, order, weights, ycalc, status) result(coeff)
    integer, intent(in) :: order
    real(kind=real64), dimension(0:order) :: coeff
    real(kind=real64), intent(in), dimension(:) :: x, y
    real(kind=real64), intent(in), dimension(:), optional, target :: weights
    real(kind=real64), intent(out), dimension(:), optional :: ycalc
    logical, intent(out), optional :: status

    ! Polynomial fit to supplied data. (Derived from PDL)

    real(kind=real64), dimension(size(x), 0:order) :: xx
    real(kind=real64), dimension(size(x)) :: yy
    real(kind=real64) :: xbar, ybar
    real(kind=real64), dimension(:), pointer :: wt
    real(kind=real64), dimension(0:order,0:order) :: ccc, yyy, aaa, tmp
    integer :: npts, i
    logical :: wall

    npts = size(x)

    if (size(y) /= npts) then
       write(error_unit, "(a,i0,a,i0,a)") &
            & "gr_polyfit: Sizes of x & y must be equal, X(",&
            & npts,"), Y(",size(y),")"
       if (present(status)) status = .false.
       coeff = 0._real64
       return
    end if

    if (present(weights)) then
       if (size(weights) /= npts) then
          write(error_unit, "(a,i0,a,i0,a)") &
               & "gr_polyfit: Sizes of x, y & weights must be equal, X, Y(",&
               & npts,"), WEIGHTS(",size(weights),")"
          if (present(status)) status = .false.
          return
       end if
       wt => weights
       wall = .false.
    else
       allocate(wt(npts))
       wt(:) = 1._real64
       wall = .true.
    end if

    xbar = sum(abs(x))/real(npts, real64)
    if (xbar == 0.) xbar = 1._real64
    ybar = sum(abs(y))/real(npts, real64)
    if (ybar == 0.) ybar = 1._real64

    xx(:,1) = x / xbar
    yy = y /ybar

 
    do i = 0, order
       xx(:,i) = xx(:,1)**i
    end do

    ccc = matmul(transpose(xx), xx*spread(wt, 2, order+1))
    yyy = matmul(transpose(xx), spread(yy*wt, 2, order+1))

    tmp = matrix_invert(ccc)

    aaa = matmul(tmp, yyy)

    coeff = aaa(:,0)*ybar
    do i = 0, order
       coeff(i) = coeff(i)/xbar**i
    end do

    if (present(ycalc)) then
       ycalc = coeff(0)
       do i = 1, order
          ycalc = ycalc + x**i * coeff(i)
       end do
    end if

    if (present(status)) status = .true.
    if (wall) deallocate(wt)
  end function poly_fit

  function matrix_invert(x) result(xi)
    real(kind=real64), intent(in), dimension(:,:) :: x
    real(kind=real64), dimension(size(x,1),size(x,2)) :: xi

    ! Invert a matrix

    real(kind=real64), dimension(size(x,1),size(x,2)) :: a
    real(kind=real64), dimension(size(x,2)) :: e
    integer :: i, n
    integer, dimension(size(x,2)) :: p

    n = size(x,2)
    if (size(x,1) /= n) then
       write(error_unit, "(a)") "invert: Non square input"
       xi = 0.
       return
    end if

    a = x
    p = lu_fact(a)

    do i = 1, n
       e = 0._real64
       e(i) = 1.
       call lu_subst(a, p, e)
       xi(i,:) = e
    end do

  end function matrix_invert

  function lu_fact(x) result(p)
    real(kind=real64), dimension(:,:), intent(inout) :: x
    integer, dimension(size(x,2)) :: p

    ! Decompose a matrix

    real(kind=real64) :: tmp, tmp1
    real(kind=real64), dimension(size(x,2)) :: s
    integer :: i,j,k
    integer :: iswap
    integer :: n

    n = size(x,2)
    if (size(x,1) /= n) then
       write(error_unit, "(A,i0,'x',i0)") &
            & "lu_fact:: input matrix not square: ", shape(x)
       return
    end if

    p = [(i, i = 1, n)]
    s = maxval(abs(x), 1)

    do k = 1, n-1
       j = k-1
       do
          j = j+1
          tmp1 = abs(x(k,p(j))/s(p(j)))
          if (any(tmp1 >= abs(x(k,p(k:)))/s(p(k:)))) exit
       end do


       iswap = p(k)
       p(k) = p(j)
       p(j) = iswap
       tmp1 =1._real64 / x(k,p(k))
       do i = k+1, n
          tmp = x(k, p(i)) * tmp1
          x(k,p(i)) = tmp
          do j = k+1, n
             x(j,p(i)) = x(j,p(i)) - tmp*x(j,p(k))
          end do
       end do
    end do
  end function lu_fact

  subroutine lu_subst(a, p, b)
    real(kind=real64), dimension(:,:), intent(in) :: a
    integer, dimension(:), intent(in) :: p
    real(kind=real64), dimension(:), intent(inout) :: b

    ! LU substitution.

    real(kind=real64), dimension(size(a,2)) :: x
    integer :: i, j, k, n
    real(kind=real64) :: tot

    n = size(a,2)
    if (size(a,1) /= n .or. size(b) /= n .or. size(p) /= n) then
       write(error_unit, "(a)") "lu_subst: Sizes do not match"
       return
    end if

    do k = 1, n
       do i = k+1, n
          b(p(i)) = b(p(i)) - a(k, p(i)) * b(p(k))
       end do
    end do

    do i = n, 1, -1
       tot = b(p(i))
       do j = i+1, n
          tot = tot - a(j, p(i))*x(j)
       end do
       x(i) = tot/a(i,p(i))
    end do

    b(:) = x(:)

  end subroutine lu_subst
end module gr_fitting
