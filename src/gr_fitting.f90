! Copyright (C) 2013
! James Tappin

! This is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 3, or (at your option)
! any later version.

! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License along with
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.

module gr_fitting
  ! Fit functions to datasets.

  use iso_fortran_env

  use gtk, only: GTK_MESSAGE_ERROR
  use gr_msg

  implicit none

  private :: gser, gcf

  character(len=160), private :: error_str

contains
  function poly_fit(x, y, order, weights, ycalc, status, chi2) result(coeff)
    integer, intent(in) :: order
    real(kind=real64), dimension(0:order) :: coeff
    real(kind=real64), intent(in), dimension(:) :: x, y
    real(kind=real64), intent(in), dimension(:), optional, target :: weights
    real(kind=real64), intent(out), dimension(:), optional :: ycalc
    real(kind=real64), intent(out), optional :: chi2
    logical, intent(out), optional :: status

    ! Polynomial fit to supplied data. (Derived from PDL), Chi**2 isn't

    real(kind=real64), dimension(size(x), 0:order) :: xx
    real(kind=real64), dimension(size(x)) :: yy, yfit
    real(kind=real64) :: xbar, ybar
    real(kind=real64), dimension(:), pointer :: wt
    real(kind=real64), dimension(0:order,0:order) :: ccc, yyy, aaa, tmp
    integer :: npts, i
    logical :: wall

    npts = size(x)

    if (size(y) /= npts) then
       write(error_str, "(a,i0,a,i0,a)") &
            & "gr_polyfit: Sizes of x & y must be equal, X(",&
            & npts,"), Y(",size(y),")"
       call gr_message(error_str, type=GTK_MESSAGE_ERROR)
       if (present(status)) status = .false.
       coeff = 0._real64
       return
    end if

    if (present(weights)) then
       if (size(weights) /= npts) then
          write(error_str, "(a,i0,a,i0,a)") &
               & "gr_polyfit: Sizes of x, y & weights must be equal, X, Y(",&
               & npts,"), WEIGHTS(",size(weights),")"
          call gr_message(error_str, type=GTK_MESSAGE_ERROR)
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

    if (present(ycalc) .or. present(chi2)) then
       yfit = coeff(0)
       do i = 1, order
          yfit = yfit + x**i * coeff(i)
       end do
       if (present(ycalc)) ycalc = yfit
       if (present(chi2)) chi2 = sum((y-yfit)**2 * wt**2)
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
       call gr_message("invert: Non square input", type=GTK_MESSAGE_ERROR)
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
       write(error_str, "(A,i0,'x',i0)") &
            & "lu_fact:: input matrix not square: ", shape(x)
       call gr_message(error_str, type=GTK_MESSAGE_ERROR)
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
       call gr_message("lu_subst: Sizes do not match", type=GTK_MESSAGE_ERROR)
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

  function gammap(a, x, itmax, eps, gln)
    real(kind=real64) :: gammap
    real(kind=real64), intent(in) :: a, x
    integer, intent(in), optional :: itmax
    real(kind=real64), intent(in), optional :: eps
    real(kind=real64), intent(out), optional :: gln

    if (x < 0. .or. a <= 0.) then
       call gr_message("gammap: invalid input arguments", &
            & type=GTK_MESSAGE_ERROR)
       gammap = -huge(x)
    end if

    if (x < a+1.) then
       gammap = gser(a, x, itmax, eps, gln)
    else
       gammap = gcf(a, x, itmax, eps, gln)
    end if
  end function gammap

  function gser(a, x, itmax, eps, gln)
    real(kind=real64) :: gser
    real(kind=real64), intent(in) :: a, x
    integer, intent(in), optional :: itmax
    real(kind=real64), intent(in), optional :: eps
    real(kind=real64), intent(out), optional :: gln

    integer :: nimax, i
    real(kind=real64) :: tol, lng, ap, del, sum

    if (present(itmax)) then
       nimax = itmax
    else
       nimax = 100
    end if
    if (present(eps)) then
       tol = eps
    else
       tol = 3e-10
    end if

    lng = log_gamma(a)

    if (x < 0.) then
       call gr_message("gammap (gser): invalid input arguments", &
            & type=GTK_MESSAGE_ERROR)
       gser = -huge(x)
    else if (x == 0.) then
       gser = 0._real64
    else
       ap = a
       sum = 1./a
       del = sum
       do i = 1, nimax
          ap = ap+1.
          del = del*x/ap
          sum = sum+del
          if (abs(del) < abs(sum)*tol) exit
       end do
       if (i == nimax+1) &
            & call gr_message("gammap (gser): ITMAX too small")
       gser = sum + exp(-x + a*log(x)-lng)
    end if

    if (present(gln)) gln = lng
  end function gser

  function gcf(a, x, itmax, eps, gln)
    real(kind=real64) :: gcf
    real(kind=real64), intent(in) :: a, x
    integer, intent(in), optional :: itmax
    real(kind=real64), intent(in), optional :: eps
    real(kind=real64), intent(out), optional :: gln

    integer :: nimax, i
    real(kind=real64) :: tol, lng, a0, a1, gold, b0, b1, fac
    real(kind=real64) :: an, ana, anf, g

    if (present(itmax)) then
       nimax = itmax
    else
       nimax = 100
    end if
    if (present(eps)) then
       tol = eps
    else
       tol = 3e-10
    end if

    gold = 0.
    a0 = 1.
    a1 = x
    b0 = 0.
    b1 = 1.
    fac = 1.

    lng = log(gamma(a))

    do i = 1, nimax
       an = real(i, real64)
       ana = an - a
       a0 = (a1 + a0*ana)*fac
       b0 = (b1 + b0*ana)*fac
       anf = an * fac
       a1 = x*a0 + anf*a1
       b1 = x*b0 + anf*b1

       if (a1 == 0.) cycle

       fac = 1./a1
       g = b1*fac
       if (abs(g-gold)/g < tol) exit
       gold = g
    end do
    if (i == nimax+1) &
         & call gr_message("gammap (gcf): ITMAX too small")

    gcf = exp(-x + a*log(x)-lng)*g
    if (present(gln)) gln = lng
  end function gcf
end module gr_fitting
