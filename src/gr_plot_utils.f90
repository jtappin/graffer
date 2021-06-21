! Copyright (C) 2013-2020
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

module gr_plot_utils
  ! Utilities to compute plot parameters (no actual plotting)

  use iso_fortran_env
  use iso_c_binding

  use plplot, only: plflt
  use gtk_hl
  use gtk_sup

  use graff_globals
  use gr_utils
  use gr_msg

  implicit none

  integer, private, dimension(3) :: tick_index
  real, private, dimension(3) :: top_last

  interface gr_plot_devices_type
     module procedure gr_plot_devices_type_d
     module procedure gr_plot_devices_type_l
  end interface gr_plot_devices_type

  ! Interfaces for the PLPLOT device list routines, which are not part
  ! of the normal Fortran API.

  ! void plgDevs( const char ***p_menustr, const char ***p_devname,
  !               int *p_ndev )
  interface
     subroutine plgdevs(p_menustr, p_devname, p_ndev) bind(c, name='plgDevs')
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr) :: p_menustr, p_devname
       integer(kind=c_int) :: p_ndev
     end subroutine plgdevs
  end interface
  ! void plgFileDevs( const char ***p_menustr, const char ***p_devname,
  !                   int *p_ndev )
  interface
     subroutine plgfiledevs(p_menustr, p_devname, p_ndev) &
          & bind(c, name='plgFileDevs')
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr) :: p_menustr, p_devname
       integer(kind=c_int) :: p_ndev
     end subroutine plgfiledevs
  end interface


contains
  subroutine gr_axis_range(axis, a0, a1, ok)
    integer, intent(in) :: axis
    real(kind=plflt), intent(out) :: a0, a1
    logical, intent(inout) :: ok

    ! Determine axis range given request and options.

    integer :: ilog
    real(kind=plflt) :: diff, scale, s0, s1
    character(len=200) :: err_buffer
    a0 = pdefs%axrange(1, axis)
    a1 = pdefs%axrange(2, axis)
    diff = abs(a1-a0)
    if (pdefs%axtype(axis) == 1 .and. min(a0,a1) <= 0.) then
       ok = .false.
       write(err_buffer, "(a,i0)") &
            & "gr_plot_draw: Zero or negative limit for log axis ",axis
       call hl_gtk_info_bar_message(gr_infobar, &
            & trim(err_buffer)//c_null_char)
       return
    end if

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

    ! Create the plbox axis codes based on the requested options.

    type(graff_style), pointer :: axsty
    integer :: other_axis, p10

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

    if (axsty%minor /= 1)  options = trim(options)//'s'
    nminor = axsty%minor

    axsty%is_big_log = .false.
    
    if (pdefs%axtype(axis) == 1) then
       p10=int(abs(log10(pdefs%axrange(2,axis)/pdefs%axrange(1,axis))))
       if (p10 > 6) then
          axsty%is_big_log = .true.
          options = trim(options)//'o'
          if (p10 <= 15) then
             spacing = 2._real64
             nminor = 2
          else if (p10 <= 30) then
             spacing = 5._real64
             nminor = 5
          else
             spacing = 0._real64
             nminor = 0
          end if
       else
          options = trim(options)//'l'
          spacing = 0._plflt
       end if
    else if (axsty%major /= 0) then
       spacing = abs(pdefs%axrange(2,axis)- pdefs%axrange(1,axis))/&
            & real(axsty%major)
       p10 = floor(log10(spacing))
       spacing = spacing/(10._real64**p10)
       if (spacing < 1.5) then
          spacing = 1.0_real64
       else if (spacing < 2.5) then
          spacing = 2.0_real64
       else if (spacing < 4.0) then
          spacing = 3.0_real64
       else if (spacing < 7.0) then
          spacing = 5.0_real64
       else
          spacing = 10.0_real64
       end if
       spacing = spacing * 10._real64 ** p10
    else
       spacing = 0._plflt
    end if
    

    if (.not. btest(axsty%extra, annot_bit)) then
       if (axis == 3) then
          options = trim(options)//'m'
       else
          options = trim(options)//'n'
       end if
       if (axis /= 1 .and. &
            & .not. btest(pdefs%axsty(axis)%extra, yrot_bit)) &
            & options = trim(options)//'v'
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

    ! Define the viewport.

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
    else if (pdefs%position(1) /= pdefs%position(3) .and. &
         &   pdefs%position(2) /= pdefs%position(4)) then
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

  subroutine gr_format_labels(axis, value, label)
    integer, intent(in) :: axis
    real(kind=plflt), intent(in) :: value
    character(len=*), intent(out) :: label

    ! Format specification for axis labels.

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

    if (pdefs%axsty(iaxis)%is_big_log) then
       write(label, "('10#u',i0,'#d')", iostat=ios, iomsg=iom) nint(value)
       if (ios /= 0) label = ''
    else if (btest(pdefs%axsty(iaxis)%time, time_bit)) then
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

    ! Define time formatting options

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

  subroutine gr_plot_devices(list, descr, fileonly)
    character(len=*), dimension(:), allocatable, intent(out) :: list
    character(len=*), dimension(:), allocatable, intent(out), optional :: descr
    logical, intent(in), optional :: fileonly

    type(c_ptr), target, dimension(40) :: menup, devp
    integer(kind=c_int), target :: ndev
    logical :: filed
    integer :: i

    if (present(fileonly)) then
       filed = fileonly
    else
       filed = .false.
    end if
    ndev = size(devp)

    if (filed) then
       call plgfiledevs(c_loc(menup), c_loc(devp), ndev)
    else
       call plgdevs(c_loc(menup), c_loc(devp), ndev)
    end if

    allocate(list(ndev))
    if (present(descr)) allocate(descr(ndev))
    do i = 1, ndev
       call c_f_string(devp(i), list(i))
       if (present(descr)) call c_f_string(menup(i), descr(i))
    end do
  end subroutine gr_plot_devices

  function gr_plot_has_device(device)
    logical :: gr_plot_has_device
    character(len=*), intent(in) :: device

    type(c_ptr), target, dimension(40) :: menup, devp
    integer(kind=c_int), target :: ndev
    integer ::i
    character(len=20) :: dev

    ndev = size(devp)

    call plgdevs(c_loc(menup), c_loc(devp), ndev)
    gr_plot_has_device = .false.

    do i = 1, ndev
       call c_f_string(devp(i), dev)
       if (lowcase(trim(dev)) == lowcase(trim(device))) then
          gr_plot_has_device = .true.
          exit
       end if
    end do
  end function gr_plot_has_device

  subroutine gr_plot_devices_type_d(type, list, descr)
    character(len=*), intent(in) :: type
    character(len=*), dimension(:), allocatable, intent(out) :: list
    character(len=*), dimension(:), allocatable, intent(out) :: descr

    character(len=len(list)), dimension(:), allocatable :: listf
    character(len=len(descr)), dimension(:), allocatable :: descf
    integer :: i, j, nm

    call gr_plot_devices(listf, descf)
    nm = count(index(listf, type) == 1)
    if (nm == 0) return

    allocate(list(nm), descr(nm))

    j = 1
    do i = 1, size(listf)
       if (index(listf(i), type) == 1) then
          list(j) = listf(i)
          descr(j) = descf(i)
          j = j+1
       end if
    end do

  end subroutine gr_plot_devices_type_d
  subroutine gr_plot_devices_type_l(type, list)
    character(len=*), intent(in) :: type
    character(len=*), dimension(:), allocatable, intent(out) :: list

    character(len=len(list)), dimension(:), allocatable :: listf
    integer :: i, j, nm

    call gr_plot_devices(listf)
    nm = count(index(listf, type) == 1)
    if (nm == 0) return

    allocate(list(nm))

    j = 1
    do i = 1, size(listf)
       if (index(listf(i), type) == 1) then
          list(j) = listf(i)
          j = j+1
       end if
    end do

  end subroutine gr_plot_devices_type_l

  subroutine gr_default_device(type, device)
    character(len=*), intent(in) :: type
    character(len=*), intent(out) :: device

    select case (lowcase(type))
    case ('ps')
       if (gr_plot_has_device('pscairo')) then
          device = 'pscairo'
       else if (gr_plot_has_device('psqt')) then
          device = 'psqt'
       else
          device = 'ps'
       end if
    case ('eps')
       if (gr_plot_has_device('epscairo')) then
          device = 'epscairo'
       else if (gr_plot_has_device('epsqt')) then
          device = 'epsqt'
       else
          device = 'ps'
       end if
    case ('pdf')
       if (gr_plot_has_device('pdfcairo')) then
          device = 'pdfcairo'
       else if (gr_plot_has_device('pdfqt')) then
          device = 'pdfqt'
       else
          call gr_message("No PDF device found", GTK_MESSAGE_ERROR)
       end if
    case ('svg')
       if (gr_plot_has_device('svgqt')) then
          device = 'svgqt'
       else if (gr_plot_has_device('svgcairo')) then
          device = 'svgcairo'
       else
          device = 'svg'
       end if
    case default
       call gr_message("Invalid output type "//trim(type), GTK_MESSAGE_WARNING)
    end select
  end subroutine gr_default_device
end module gr_plot_utils
