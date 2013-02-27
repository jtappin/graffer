module gr_sort
  use iso_fortran_env

  implicit none

  interface sort
     module procedure sort1
     module procedure sort2
  end interface sort

contains
  subroutine sort1(a,b)
    real(kind=real64), dimension(:), intent(in) :: a
    real(kind=real64), dimension(:), intent(out) :: b

    ! Heapsort, based loosely on the numerical recipes routine, 
    ! but returns to a new array. 

    integer :: l, ir, i, j, n
    real(kind=8) :: rb

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
       if ( l > 1) then
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

  subroutine sort2(a,b,index)
    real(kind=real64), dimension(:,:), intent(in) :: a
    real(kind=real64), dimension(:,:), intent(out) :: b
    integer, intent(in), optional :: index

    ! Heapsort, based loosely on the numerical recipes routine, 
    ! but returns to a new array. Sort a 2-D array on a column.

    integer :: idx
    integer :: l, ir, i, j, n
    real(kind=8), dimension(size(a,1)) :: rb

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
       if ( l > 1) then
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
end module gr_sort
