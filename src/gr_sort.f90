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

module gr_sort
  ! General sorting routines (for axis sorting).
  use iso_fortran_env

  implicit none

  interface sort
     module procedure sort1
     module procedure sort1i
     module procedure sort2
     module procedure isort1
  end interface sort

  private :: sort1, sort1i, sort2, isort1

contains
  subroutine sort1(a, b)
    real(kind=real64), dimension(:), intent(in) :: a
    real(kind=real64), dimension(:), intent(out) :: b
    
    ! Heapsort, based loosely on the numerical recipes routine, 
    ! but returns to a new array. 

    integer :: l, ir, i, j, n
    real(kind=real64) :: rb
    logical :: is_idx

    n = size(a,1)
    if (size(b,1) /= n) then
       write(error_unit, *) "SORT:: arrays must be the same size"
       return
    end if

    l = n/2 + 1
    ir = n
    b=a
    if (n <= 1) return

    do
       if (l > 1) then
          l = l-1
          rb = b(l)
       else
          rb = b(ir)
          b(ir) = b(1)
          ir = ir-1
          if (ir == 1) then
             b(1) = rb
             return
          end if
       end if

       i=l
       j = l+l
       do
          if (j > ir)  exit

          if (j < ir) then
             if (b(j) < b(j+1)) j = j+1
          end if
          if (rb < b(j)) then
             b(i) = b(j)
             i=j
             j=j+j
          else
             exit
          end if
       end do
       b(i) = rb
    end do
  end subroutine sort1

  subroutine sort1i(a, b, idx)
    real(kind=real64), dimension(:), intent(in) :: a
    real(kind=real64), dimension(:), intent(out) :: b
    integer(kind=int32), dimension(:), intent(out) :: idx

    ! Heapsort, based loosely on the numerical recipes routine, 
    ! but returns to a new array. 

    integer :: l, ir, i, j, n, ib
    real(kind=real64) :: rb

    n = size(a,1)
    if (size(b,1) /= n) then
       write(error_unit, *) "SORT:: arrays must be the same size"
       return
    end if

    if (size(idx,1) /= n) then
       write(error_unit, *) "SORT:: if present the IDX array must be the same size as the input array."
       return
    end if
    do i = 1, n
       idx(i) = i
    end do

    l = n/2 + 1
    ir = n
    b=a
    if (n <= 1) return

    do
       if (l > 1) then
          l = l-1
          rb = b(l)
          ib = idx(l)
       else
          rb = b(ir)
          ib = idx(ir)
          b(ir) = b(1)
          idx(ir) = idx(1)
          ir = ir-1
          if (ir == 1) then
             b(1) = rb
             idx(1) = ib
             return
          end if
       end if

       i=l
       j = l+l
       do
          if (j > ir)  exit

          if (j < ir) then
             if (b(j) < b(j+1)) j = j+1
          end if
          if (rb < b(j)) then
             b(i) = b(j)
             idx(i) = idx(j)
             i=j
             j=j+j
          else
             exit
          end if
       end do
       b(i) = rb
       idx(i) = ib
    end do
  end subroutine sort1i


  subroutine sort2(a,b,index)
    real(kind=real64), dimension(:,:), intent(in) :: a
    real(kind=real64), dimension(:,:), intent(out) :: b
    integer, intent(in), optional :: index

    ! Heapsort, based loosely on the numerical recipes routine, 
    ! but returns to a new array. Sort a 2-D array on a column.

    integer :: idx
    integer :: l, ir, i, j, n
    real(kind=real64), dimension(size(a,1)) :: rb

    if (present(index)) then 
       idx = index
    else
       idx = 1
    end if
    n = size(a,2)
    if (size(b,2) /= n .or. size(a,1) /= size(b,1)) then
       write(error_unit, *) "SORT:: arrays must be the same size"
       return
    end if

    l = n/2 + 1
    ir = n
    b=a
    if (n <= 1) return

    do
       if (l > 1) then
          l = l-1
          rb = b(:,l)
       else
          rb = b(:,ir)
          b(:,ir) = b(:,1)
          ir = ir-1
          if (ir == 1) then
             b(:,1) = rb
             return
          end if
       end if

       i=l
       j = l+l
       do
          if (j > ir)  exit

          if (j < ir) then
             if (b(idx,j) < b(idx,j+1)) j = j+1
          end if
          if (rb(idx) < b(idx,j)) then
             b(:,i) = b(:,j)
             i=j
             j=j+j
          else
             exit
          end if
       end do
       b(:,i) = rb
    end do
  end subroutine sort2

  subroutine isort1(a,b)
    integer(kind=int32), dimension(:), intent(in) :: a
    integer(kind=int32), dimension(:), intent(out) :: b

    ! Heapsort, based loosely on the numerical recipes routine, 
    ! but returns to a new array. 

    integer :: l, ir, i, j, n
    integer(kind=int32) :: rb

    n = size(a,1)
    if (size(b,1) /= n) then
       write(error_unit, *) "SORT:: arrays must be the same size"
       return
    end if

    l = n/2 + 1
    ir = n
    b=a
    if (n <= 1) return

    do
       if (l > 1) then
          l = l-1
          rb = b(l)
       else
          rb = b(ir)
          b(ir) = b(1)
          ir = ir-1
          if (ir == 1) then
             b(1) = rb
             return
          end if
       end if

       i=l
       j = l+l
       do
          if (j > ir)  exit

          if (j < ir) then
             if (b(j) < b(j+1)) j = j+1
          end if
          if (rb < b(j)) then
             b(i) = b(j)
             i=j
             j=j+j
          else
             exit
          end if
       end do
       b(i) = rb
    end do
  end subroutine isort1

end module gr_sort
