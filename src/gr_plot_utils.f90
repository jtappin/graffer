module gr_plot_utils
  ! Utilities to compute plot parameters (no actual plotting)

  use iso_fortran_env

  use plplot, only: plflt

  use graff_globals
  use gr_utils

  implicit none

  integer, private, dimension(3) :: tick_index
  real, private, dimension(3) :: top_last

contains
  subroutine gr_axis_range(axis, a0, a1)
    integer, intent(in) :: axis
    real(kind=plflt), intent(out) :: a0, a1

    integer :: ilog
    real(kind=plflt) :: diff, scale, s0, s1

    a0 = pdefs%axrange(1, axis)
    a1 = pdefs%axrange(2, axis)
    diff = abs(a1-a0)

    if (.not. btest(pdefs%axsty(axis)%idl, exact_bit)) then
       if (pdefs%axtype(axis) == 1) then
          ilog = floor(log10(a0))
          a0 = 10._plflt ** ilog
          ilog = ceiling(log10(a1))
          a1 = 10._plflt ** ilog
          if (a0 == a1) a1 = a1*10._plflt
       else
          scale = 10._plflt ** floor(log10(diff/5._plflt))

          s0 = a0 - modulo(a0, scale)
          if (s0 + 4._plflt*scale <= a0) then
             s0 = s0 + 4._plflt*scale
          else if (s0 + scale <= a0) then
             s0 = s0 + scale
          end if

          s1 = a1 - modulo(a1, scale)
          if (s1 < a1) then
             if (s1 + scale >= a1) then
                s1 = s1 + scale
             else  if (s1 + 4._plflt*scale >= a1) then
                s1 = s1 + 4._plflt*scale
             else
                s1 = s1 + 9._plflt*scale
             end if
          end if
          a0 = s0
          a1 = s1
       end if
    end if

    if (btest(pdefs%axsty(axis)%idl, extend_bit)) then
       if (pdefs%axtype(axis) == 1) then
          a0 = 0.9*a0
          a1 = a0/0.9
       else
          a0 = a0 - 0.05_plflt * diff
          a1 = a1 + 0.05_plflt * diff
       end if
    end if

    if (pdefs%axtype(axis) == 1) then
       a0 = log10(a0)
       a1 = log10(a1)
    end if
  end subroutine gr_axis_range

  subroutine gr_axis_box(axis, options, spacing, nminor)
    integer, intent(in) :: axis
    character(len=*), intent(out) :: options
    real(kind=plflt), intent(out) :: spacing
    integer, intent(out) :: nminor

    type(graff_style), pointer :: axsty
    integer :: other_axis

    axsty => pdefs%axsty(axis)

    if (axis == 1) then
       other_axis = 2
    else
       other_axis = 1
    end if

    if (btest(axsty%idl, axis_bit)) then
       options = 't'         ! Needed as Origin Axis && Suppress edges is OK
    else if (axis == 3) then
       options = 'ct'
    else if (btest(axsty%idl, box_bit) .or. &
         & (pdefs%y_right .and. axis == 2)) then
       options = 'bt'
    else
       options = 'bct'
    end if

    if (btest(axsty%extra, origin_bit) .and. &
         & pdefs%axrange(1,other_axis)*pdefs%axrange(2,other_axis) < 0) &
         & options = trim(options)//'a'

    if (axsty%minor /= 0) then
       options = trim(options)//'s'
       if (axsty%minor > 1) then
          nminor = axsty%minor
       else
          nminor = 0
       end if
    else
       nminor = 0
    end if

    spacing = axsty%xmajor

    if (pdefs%axtype(axis) == 1) options = trim(options)//'l'

    if (.not. btest(axsty%extra, annot_bit)) then
       if (axis == 3) then
          options = trim(options)//'m'
       else
          options = trim(options)//'n'
       end if
       if (axis /= 1)  options = trim(options)//'v'
    end if

    if (axsty%format /= '' .or. btest(axsty%time, time_bit)) then
       options = trim(options)//'o'
       tick_index(axis) = 0
       top_last(axis) = 0.5
    end if

  end subroutine gr_axis_box

  subroutine gr_viewport(corners, aspect)
    real(kind=plflt), dimension(4), intent(out) :: corners
    real(kind=plflt), intent(out) :: aspect

    real(kind=plflt) :: dx, dy, xoff, yoff, roff

    if (pdefs%isotropic) then
       corners = 0._plflt
       dx = pdefs%transform%world(2,1)- pdefs%transform%world(1,1)
       dy = pdefs%transform%world(4,1)- pdefs%transform%world(3,1)
       aspect = dy/dx
    else if (pdefs%aspect(1) > 0.) then
       if (pdefs%aspect(2) > 0.) then
          corners = real([pdefs%aspect(2), 1.-pdefs%aspect(2), &
               & pdefs%aspect(2), 1.-pdefs%aspect(2)], plflt)
       else
          corners = 0._plflt
       end if
       aspect = real(pdefs%aspect(1), plflt)
    else if (pdefs%position(1) > 0.) then
       corners = real(pdefs%position([1,3,2,4]), plflt)
       aspect = 0._plflt
    else
       xoff = real(0.04 + 0.08*pdefs%charsize, plflt)
       yoff = real(0.025 + 0.075*pdefs%charsize, plflt)
       if (pdefs%y_right) then
          roff = xoff
       else
          roff = 0.025_plflt
       end if

       corners = [xoff, 1._plflt-roff, yoff, 1._plflt-yoff]
       aspect = 0._plflt
    end if
  end subroutine gr_viewport

  subroutine gr_format_labels(axis, value, label, length)
    integer :: axis, length
    real(kind=plflt) :: value
    character(len=length) :: label

    ! NOTE: No intent() settings as otherwise it won't match the
    ! interface required.

    integer :: iaxis, ios
    character(len=120) :: iom

    if (axis == 1) then
       iaxis = 1
    else if (pdefs%transform%world_selected == 1) then
       iaxis = 2
    else 
       iaxis = 3
    end if
    
    if (btest(pdefs%axsty(iaxis)%time, time_bit)) then
       call gr_format_time(iaxis, value, label)
    else
       write(label, pdefs%axsty(iaxis)%format, iostat=ios, iomsg=iom) value
       if (ios /= 0) then
          write(label, pdefs%axsty(iaxis)%format, iostat=ios, iomsg=iom) &
               & nint(value)
          if (ios /= 0) label = ''
       end if
    end if
  end subroutine gr_format_labels

  subroutine gr_format_time(axis, value, label)
    integer, intent(in) :: axis
    real(kind=plflt), intent(in) :: value
    character(len=*), intent(out) :: label

    integer(kind=int16) :: unit, munit
    type(graff_style), pointer :: axstyle
    real(kind=plflt), dimension(0:3), parameter :: &
         & ucf = [1./3600._plflt, 1./60._plflt, 1.0_plflt, 24._plflt]
    real(kind=plflt) :: cf, t, range
    integer :: zero
    integer :: d, h, m, s
    real(kind=plflt) :: hh, mm

    axstyle => pdefs%axsty(axis)

    unit = iand(ishft(axstyle%time,-1), 3_int16)
    munit = iand(ishft(axstyle%time,-3), 3_int16)

    cf = ucf(unit)/ucf(munit)
    t = value*cf
    range = abs(pdefs%axrange(2,axis)-pdefs%axrange(1,axis))
    zero = axstyle%tzero

    select case (munit)
    case(0)
       if (range*ucf(unit)*3600._plflt > 5.) then
          write(label, "(I0)") nint(t+zero)
       else
          write(label, "(F4.1)") t+zero
       end if

    case(1)
       m = floor(t)
       s = nint(60*(t-m))
       if (tick_index(axis) == 0 .or. m /= top_last(axis)) then
          write(label, "(I0.2,':',I2.2)") m+zero, s
       else
          write(label, "(I2.2)") s
       end if
       top_last(axis) = m

    case(2)
       h = floor(t)
       mm = 60.*(t-h)
       
       if (tick_index(axis) == 0 .or. h /= top_last(axis)) then
          if (range*ucf(unit) > 1.) then
             write(label, "(I0.2,':',I2.2)") h+zero, nint(mm)
          else
             s = nint(60*(mm-int(mm)))
             write(label, "(I0.2,':',I2.2,':',I2.2)") h+zero, int(mm), s
          end if
       else
          if (range*ucf(unit) > 1.) then
             write(label, "(I2.2)") nint(mm)
          else
             s = nint(60*(mm-int(mm)))
             write(label, "(I2.2,':',I2.2)") int(mm), s
          end if
       end if

       top_last = h

    case(3)
       d = floor(t)
       hh = 24.*(t-d)
       if (tick_index(axis) == 0 .or. d /= top_last(axis)) then
          if (range*ucf(unit)/24. > 1.) then
             write(label, "(I0'/',I2.2)") d+zero, nint(hh)
          else
             m = nint(60.*(h-int(h)))
             write(label, "(I0'/',I2.2,':',I2.2)") d+zero, int(hh), m
          end if
       else
          if (range*ucf(unit)/24. > 1.) then
             write(label, "(I2.2)")nint(hh)
          else
             m = nint(60.*(h-int(h)))
             write(label, "(I2.2,':',I2.2)")int(hh), m
          end if
       end if

       top_last = d

    end select

    tick_index(axis) = tick_index(axis)+1
  end subroutine gr_format_time
end module gr_plot_utils
