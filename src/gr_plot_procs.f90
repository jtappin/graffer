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

module gr_plot_procs
  ! General graphics routines: error bars, annotations, key, contouring,
  ! shading and time stamp.

  use iso_fortran_env
  use iso_c_binding

  use plplot
  use gtk_hl

  use graff_types
  use graff_globals

  use gr_plot_tools
  use gr_plot_utils

  use gr_colours
  
  use gr_text_utils
  use gr_interfaces

  use ieee_arithmetic, only: ieee_is_finite
  
  implicit none

contains

  ! ********************************************
  ! ERROR BARS 
  ! ********************************************

  subroutine gr_plot_xy_errors(index, x, y, xydata)
    integer, intent(in) :: index
    real(kind=plflt), intent(in), dimension(:) :: x, y
    real(kind=plflt), intent(in), dimension(:,:) :: xydata

    ! Error bars for regular XY plots

    real(kind=plflt), allocatable, dimension(:) :: elo, ehi
    type(graff_data), pointer :: data
    logical :: xlog, ylog

    data => pdefs%data(index)
    call gr_plot_linesty(0_int16)

    xlog = pdefs%axtype(1) == 1
    if (pdefs%y_right .and. data%y_axis == 1) then
       ylog = pdefs%axtype(3) == 1
    else
       ylog = pdefs%axtype(2) == 1
    end if

    allocate(elo(data%ndata), &
         & ehi(data%ndata))

    select case (data%type)
    case(1)
       elo = xydata(2,:)-xydata(3,:)
       ehi = xydata(2,:)+xydata(3,:)
       if (ylog) then
          elo = log10(elo)
          ehi = log10(ehi)
       end if
       call plerry (x, elo, ehi)

    case(2)
       elo = xydata(2,:)-xydata(3,:)
       ehi = xydata(2,:)+xydata(4,:)
       if (ylog) then
          elo = log10(elo)
          ehi = log10(ehi)
       end if
       call plerry (x, elo, ehi)

    case(3)
       elo = xydata(1,:)-xydata(3,:)
       ehi = xydata(1,:)+xydata(3,:)
       if (xlog) then
          elo = log10(elo)
          ehi = log10(ehi)
       end if
       call plerrx (elo, ehi, y)

    case(4)
       elo = xydata(1,:)-xydata(3,:)
       ehi = xydata(1,:)+xydata(4,:)
       if (xlog) then
          elo = log10(elo)
          ehi = log10(ehi)
       end if
       call plerrx (elo, ehi, y)

    case(5)
       elo = xydata(1,:)-xydata(3,:)
       ehi = xydata(1,:)+xydata(3,:)
       if (xlog) then
          elo = log10(elo)
          ehi = log10(ehi)
       end if
       call plerrx (elo, ehi, y)
       elo = xydata(2,:)-xydata(4,:)
       ehi = xydata(2,:)+xydata(4,:)
       if (ylog) then
          elo = log10(elo)
          ehi = log10(ehi)
       end if
       call plerry (x, elo, ehi)

    case(6)
       elo = xydata(1,:)-xydata(3,:)
       ehi = xydata(1,:)+xydata(3,:)
       if (xlog) then
          elo = log10(elo)
          ehi = log10(ehi)
       end if
       call plerrx (elo, ehi, y)
       elo = xydata(2,:)-xydata(4,:)
       ehi = xydata(2,:)+xydata(5,:)
       if (ylog) then
          elo = log10(elo)
          ehi = log10(ehi)
       end if
       call plerry (x, elo, ehi)

    case(7)
       elo = xydata(1,:)-xydata(3,:)
       ehi = xydata(1,:)+xydata(4,:)
       if (xlog) then
          elo = log10(elo)
          ehi = log10(ehi)
       end if
       call plerrx (elo, ehi, y)
       elo = xydata(2,:)-xydata(5,:)
       ehi = xydata(2,:)+xydata(5,:)
       if (ylog) then
          elo = log10(elo)
          ehi = log10(ehi)
       end if
       call plerry (x, elo, ehi)

    case(8)
       elo = xydata(1,:)-xydata(3,:)
       ehi = xydata(1,:)+xydata(4,:)
       if (xlog) then
          elo = log10(elo)
          ehi = log10(ehi)
       end if
       call plerrx (elo, ehi, y)
       elo = xydata(2,:)-xydata(5,:)
       ehi = xydata(2,:)+xydata(6,:)
       if (ylog) then
          elo = log10(elo)
          ehi = log10(ehi)
       end if
       call plerry (x, elo, ehi)

    end select

  end subroutine gr_plot_xy_errors

  subroutine gr_plot_rt_errors(index)
    integer, intent(in) :: index

    ! Error bars for polar plots.

    real(kind=plflt), pointer, dimension(:,:) :: xydata
    real(kind=plflt), pointer, dimension(:) :: r, th
    real(kind=plflt), allocatable, dimension(:) :: xhi, xlo, yhi, ylo
    real(kind=plflt), parameter :: dtor = pl_pi/180._plflt
    real(kind=plflt) :: scale
    type(graff_data), pointer :: data

    data => pdefs%data(index)
    call gr_plot_linesty(0_int16)

    if (data%mode == 1) then
       scale = 1._plflt
    else
       scale = dtor
    end if

    r => data%xydata(1,:)
    th => data%xydata(2,:)

    xydata => data%xydata
    allocate(xlo(data%ndata), xhi(data%ndata), &
         & ylo(data%ndata), yhi(data%ndata))

    select case (data%type)
    case(1)
       xlo = r * cos((th-xydata(3,:))*scale)
       xhi = r * cos((th+xydata(3,:))*scale)
       ylo = r * sin((th-xydata(3,:))*scale)
       yhi = r * sin((th+xydata(3,:))*scale)
       call gr_polar_errors(index, xlo, xhi, ylo, yhi)
    case(2)
       xlo = r * cos((th-xydata(3,:))*scale)
       xhi = r * cos((th+xydata(4,:))*scale)
       ylo = r * sin((th-xydata(3,:))*scale)
       yhi = r * sin((th+xydata(4,:))*scale)
       call gr_polar_errors(index, xlo, xhi, ylo, yhi)
    case(3)
       xlo = (r-xydata(3,:)) * cos(th*scale)
       xhi = (r+xydata(3,:)) * cos(th*scale)
       ylo = (r-xydata(3,:)) * sin(th*scale)
       yhi = (r+xydata(3,:)) * sin(th*scale)
       call gr_polar_errors(index, xlo, xhi, ylo, yhi)
    case(4)
       xlo = (r-xydata(3,:)) * cos(th*scale)
       xhi = (r+xydata(4,:)) * cos(th*scale)
       ylo = (r-xydata(3,:)) * sin(th*scale)
       yhi = (r+xydata(4,:)) * sin(th*scale)
       call gr_polar_errors(index, xlo, xhi, ylo, yhi)
    case(5)
       xlo = (r-xydata(3,:)) * cos(th*scale)
       xhi = (r+xydata(3,:)) * cos(th*scale)
       ylo = (r-xydata(3,:)) * sin(th*scale)
       yhi = (r+xydata(3,:)) * sin(th*scale)
       call gr_polar_errors(index, xlo, xhi, ylo, yhi)
       xlo = r * cos((th-xydata(4,:))*scale)
       xhi = r * cos((th+xydata(4,:))*scale)
       ylo = r * sin((th-xydata(4,:))*scale)
       yhi = r * sin((th+xydata(4,:))*scale)
       call gr_polar_errors(index, xlo, xhi, ylo, yhi)
    case(6)
       xlo = (r-xydata(3,:)) * cos(th*scale)
       xhi = (r+xydata(3,:)) * cos(th*scale)
       ylo = (r-xydata(3,:)) * sin(th*scale)
       yhi = (r+xydata(3,:)) * sin(th*scale)
       call gr_polar_errors(index, xlo, xhi, ylo, yhi)
       xlo = r * cos((th-xydata(4,:))*scale)
       xhi = r * cos((th+xydata(5,:))*scale)
       ylo = r * sin((th-xydata(4,:))*scale)
       yhi = r * sin((th+xydata(5,:))*scale)
       call gr_polar_errors(index, xlo, xhi, ylo, yhi)
    case(7)
       xlo = (r-xydata(3,:)) * cos(th*scale)
       xhi = (r+xydata(4,:)) * cos(th*scale)
       ylo = (r-xydata(3,:)) * sin(th*scale)
       yhi = (r+xydata(4,:)) * sin(th*scale)
       call gr_polar_errors(index, xlo, xhi, ylo, yhi)
       xlo = r * cos((th-xydata(5,:))*scale)
       xhi = r * cos((th+xydata(5,:))*scale)
       ylo = r * sin((th-xydata(5,:))*scale)
       yhi = r * sin((th+xydata(5,:))*scale)
       call gr_polar_errors(index, xlo, xhi, ylo, yhi)
    case(8)
       xlo = (r-xydata(3,:)) * cos(th*scale)
       xhi = (r+xydata(4,:)) * cos(th*scale)
       ylo = (r-xydata(3,:)) * sin(th*scale)
       yhi = (r+xydata(4,:)) * sin(th*scale)
       call gr_polar_errors(index, xlo, xhi, ylo, yhi)
       xlo = r * cos((th-xydata(5,:))*scale)
       xhi = r * cos((th+xydata(6,:))*scale)
       ylo = r * sin((th-xydata(5,:))*scale)
       yhi = r * sin((th+xydata(6,:))*scale)
       call gr_polar_errors(index, xlo, xhi, ylo, yhi)
    end select

  end subroutine gr_plot_rt_errors

  subroutine gr_polar_errors(index, xlo, xhi, ylo, yhi)
    integer, intent(in) :: index

    ! Draw a set of error bars in polar coordinates

    real(kind=plflt), dimension(:), intent(inout) :: xlo, xhi, ylo, yhi

    logical :: xlog, ylog
    integer :: i

    xlog = pdefs%axtype(1) == 1
    if (pdefs%y_right .and. pdefs%data(index)%y_axis == 1) then
       ylog = pdefs%axtype(3) == 1
    else
       ylog = pdefs%axtype(2) == 1
    end if

    if (xlog) then
       xlo = log10(xlo)
       xhi = log10(xhi)
    end if
    if (ylog) then
       ylo = log10(ylo)
       yhi = log10(yhi)
    end if

    do i = 1, size(xlo)
       call pljoin(xlo(i), ylo(i), xhi(i), yhi(i))
    end do
  end subroutine gr_polar_errors

  ! *********************************
  ! TEXT, KEY etc.
  ! *********************************

  subroutine gr_text_draw(index, anchor)
    integer, intent(in) :: index
    logical, intent(in) :: anchor

    ! Draw a text annotation on a plot

    type(graff_text), pointer :: text
    real(kind=plflt) :: x, y, dx, dy
    real(kind=plflt), dimension(3) :: xa, ya, xav, yav
    real(kind=plflt) :: xd, yd, dxw, dyw
    integer :: i
    character(len=120) :: err_buffer

    text => pdefs%text(index)

    if (text%colour == -1)  return
    if (text%colour == -2) then
       call gr_custom_line(text%c_vals)
    else
       call plcol0(int(text%colour))
    end if

    call plschr(0._plflt, real(pdefs%charsize*text%size, plflt))

    if (text%ffamily <= 0 .or. text%ffamily > size(font_list)) &
         & text%ffamily = 1_int16
    if (text%font <= 0 .or. text%font > size(font_shape)) text%font = 1_int16

    call plsfont(font_list(text%ffamily), font_shape(text%font), &
         & font_weight(text%font))

    select case (text%norm)
    case(0)                      ! World coordinates
       call gr_plot_transform(index=text%axis+1, full=.true._int8, &
            & noupdate=.true.)
       x = text%x
       if (pdefs%axtype(1) == 1) x = log10(x)
       y = text%y
       if (pdefs%axtype(text%axis+1) == 1) y = log10(y)
    case(1)                      ! Normalized coordinates
       call gr_plot_coords_n_w(text%x, text%y, x, y, nolog=.true.)
       call gr_plot_transform(full=.true._int8, noupdate=.true.)
    case(2)                      ! Frame (viewport) cooordinates
       call gr_plot_coords_v_w(text%x, text%y, x, y, nolog=.true.)
       call gr_plot_transform(full=.true._int8, noupdate=.true.)
    case default
       write(err_buffer, "(A,i0)") &
            & "gr_text_add: Invalid coordinate system: ", &
            & text%norm
       call hl_gtk_info_bar_message(gr_infobar, trim(err_buffer)//c_null_char)
       call gr_plot_transform(full=.false._int8)
       return
    end select

    call gr_plot_coords_w_d(x, y, xd, yd, nolog=.true.)
    dx = cos(text%orient*pl_pi/180._plflt)
    dy = -sin(text%orient*pl_pi/180._plflt)  ! Device coords are top-down
    call gr_plot_coords_d_w(xd+dx, yd+dy, dxw, dyw, nolog=.true.)
    dxw = dxw - x
    dyw = dyw - y

    call plptex(x, y, dxw, dyw, real(text%align, plflt), text%text)

    if (anchor) then
       call gr_plot_coords_w_v(x, y, xav(2), yav(2), nolog=.true.)
       xav(1) = xav(2) - 0.005
       xav(3) = xav(2) + 0.005
       yav(1) = yav(2) - 0.01
       yav(3) = yav(2) - 0.01

       do i = 1, 3
          call gr_plot_coords_v_w(xav(i), yav(i), xa(i), ya(i), nolog=.true.)
          call plcol0(1)
          call plwidth(1._plflt)
          call gr_plot_linesty(0_int16)
       end do
       call plline(xa, ya)
    end if
    call gr_plot_transform(full=.false._int8)
    call plsfont(font_list(pdefs%hardset%font_family), &
         & font_shape(pdefs%hardset%font_wg_sl), &
         & font_weight(pdefs%hardset%font_wg_sl))

  end subroutine gr_text_draw

  subroutine gr_key_draw

    ! Draw a key on a plot.

    real(kind=plflt) :: csize, lsp, tx, yoff
    real(kind=plflt), dimension(2) :: xn, yn, xw, yw
    real(kind=plflt), dimension(:), allocatable :: y, x0
    real(kind=plflt), dimension(:), allocatable :: x, ys, xx

    integer :: nkey, nrows, i, j, irow, icol
    character(len=120) :: descr

    if (.not. allocated(pdefs%key%list)) return

    nkey = count(pdefs%data(pdefs%key%list+1)%colour /= -1)
    if (nkey == 0) return
    nrows = ceiling(real(nkey)/pdefs%key%cols)

    if (pdefs%key%one_point) then
       allocate(x(3), ys(3))
    else
       allocate(x(2), ys(2), xx(4))
    end if

    call gr_plot_transform(full=.false._int8, noupdate=.true., index=1)

    if (pdefs%key%csize == 0.) then
       csize = pdefs%charsize
    else
       csize = pdefs%key%csize*pdefs%charsize
    end if

    select case (pdefs%key%norm)
    case(0)
       xw = pdefs%key%x
       if (pdefs%axtype(1) == 1) xw = log10(xw)
       yw = pdefs%key%y
       if (pdefs%axtype(2) == 1) yw = log10(yw)
       call gr_plot_coords_w_n(pdefs%key%x(1), pdefs%key%y(1), xn(1), yn(2))
       call gr_plot_coords_w_n(pdefs%key%x(2), pdefs%key%y(2), xn(1), yn(2))
    case(1)
       xn = pdefs%key%x
       yn = pdefs%key%y
       call gr_plot_coords_n_w(pdefs%key%x(1), pdefs%key%y(1), xw(1), yw(1), &
            & nolog=.true.)
       call gr_plot_coords_n_w(pdefs%key%x(2), pdefs%key%y(2), xw(2), yw(2), &
            & nolog=.true.)
    case(2)
       call gr_plot_coords_v_n(pdefs%key%x(1), pdefs%key%y(1), xn(1), yn(1))
       call gr_plot_coords_v_n(pdefs%key%x(2), pdefs%key%y(2), xn(2), yn(2))

       call gr_plot_coords_v_w(pdefs%key%x(1), pdefs%key%y(1), xw(1), yw(1), &
            & nolog=.true.)
       call gr_plot_coords_v_w(pdefs%key%x(2), pdefs%key%y(2), xw(2), yw(2), &
            & nolog=.true.)
    end select

    call gr_plot_transform(full=.true._int8, noupdate=.true., index=1)

    call gr_plot_linesty(0_int16)
    call plcol0(1)

    if (pdefs%key%frame) call plline(xw([1,2,2,1,1]), yw([1,1,2,2,1]))

    if (pdefs%key%title  /= '') then
       lsp = (yw(2)-yw(1))/(nrows+1.2_plflt)
       call plschr(0._plflt, 1.2_plflt*csize)
       call plptex(sum(xw)/2._plflt, yw(2)-lsp*0.6_plflt, 1._plflt, 0._plflt, &
            & 0.5_plflt, trim(pdefs%key%title))
    else
       lsp =  (yw(2)-yw(1))/real(nrows, plflt)
    end if

    allocate(y(nrows))
    y = [ ((i + .2_plflt)*lsp, i = 0, nrows-1) ] + yw(1)
    allocate(x0(pdefs%key%cols))
    x0 = xw(1) + (xw(2)-xw(1))* [ (real(i, plflt), i = 0, pdefs%key%cols-1) ]/&
         & real(pdefs%key%cols, plflt)

    if (pdefs%key%one_point) then
       x = [.05_plflt, .125_plflt, .2_plflt] * &
            & (xw(2)-xw(1))/real(pdefs%key%cols, plflt)
       tx = 0.3_plflt * (xw(2)-xw(1))/real(pdefs%key%cols, plflt)
       ys = 0.3_plflt * lsp
    else
       x = [.05_plflt, 0.3_plflt] * (xw(2)-xw(1))/real(pdefs%key%cols, plflt)
       tx = 0.4_plflt * (xw(2)-xw(1))/real(pdefs%key%cols, plflt)
       ys = [0._plflt, 0.5_plflt * lsp]
    end if

    yoff = 0.

    call plschr(0._plflt, csize)

    do j = 1, nkey
       i = pdefs%key%list(j)+1

       irow = nrows - mod((j-1), nrows)
       icol = (j-1) / nrows + 1

       call gr_plot_linesty(pdefs%data(i)%line, &
            & scale = ceiling(sqrt(pdefs%data(i)%thick)))
       call plcol0(int(pdefs%data(i)%colour))
       call plwidth(pdefs%data(i)%thick)

       if (pdefs%data(i)%pline == 2 .and. .not. pdefs%key%one_point) then
          xx = [x(1), sum(x)/2., sum(x)/2., x(2)]
          call plline(x0(icol)+xx, y(irow) + ys([1,1,2,2]))
       else if (pdefs%data(i)%pline /= 0) then
          call plline(x0(icol)+x, y(irow) + ys)
       end if

       if (pdefs%data(i)%psym /= 0) then
          if (pdefs%key%one_point) then
             call gr_plot_symbol([x0(icol)+x(2)], [y(irow)], &
                  & pdefs%data(i)%psym, pdefs%data(i)%symsize)
          else
             call gr_plot_symbol(x0(icol)+x, y(irow)+ys, &
                  & pdefs%data(i)%psym, pdefs%data(i)%symsize)
          end if
       end if

       descr = pdefs%data(i)%descript
       if (pdefs%y_right .and. pdefs%key%side) then
          if (pdefs%data(i)%y_axis == 0) then
             descr = trim(descr)//' (l)'
          else 
             descr = trim(descr)//' (r)'
          end if
       end if

       call plcol0(1)
       call plptex(x0(icol)+tx, y(irow)+sum(ys)/real(size(ys)), &
            & 1._plflt, 0._plflt, 0._plflt, descr)
    end do
  end subroutine gr_key_draw

  ! *********************************************************
  !    CONTOURING & SHADING

  subroutine gr_contour(index)
    integer, intent(in) :: index

    ! Contouring with different properties for different contours

    type(graff_data), pointer :: data
    real(kind=real64), dimension(:,:), pointer :: z
    real(kind=real64), dimension(:), pointer ::  clevels
    real(kind=real64), dimension(:,:), allocatable ::  x2, y2
    real(kind=real64), dimension(:), allocatable :: x1, y1
    logical :: c2d, clall
    integer :: i, icc, icidx
    integer(kind=int16), dimension(3) :: icrc
    
    real(kind=real64) :: zmin, zmax, xmin, xmax, ymin, ymax, ccol
    logical :: xlog, ylog

    data => pdefs%data(index)

    if (.not. allocated(data%zdata%z)) return
    call gr_plot_transform(dataset=index, full=data%noclip)

    xlog = pdefs%axtype(1) == 1
    if (pdefs%y_right .and. pdefs%data(index)%y_axis == 1) then
       ylog = pdefs%axtype(3) == 1
    else
       ylog = pdefs%axtype(2) == 1
    end if

    z => data%zdata%z
    if (data%zdata%x_is_2d .or. data%zdata%y_is_2d) then
       allocate(x2(data%ndata, data%ndata2), y2(data%ndata, data%ndata2))
       c2d = .true.
    else
       allocate(x1(data%ndata), y1(data%ndata2))
       c2d = .false.
    end if

    if (data%zdata%x_is_2d) then
       if (data%zdata%y_is_2d) then
          x2 = data%zdata%x
          y2 = data%zdata%y
       else
          x2 = data%zdata%x
          do i = 1, data%ndata
             y2(i,:) = data%zdata%y(1,:)
          end do
       end if
    else
       if (data%zdata%y_is_2d) then
          do i = 1, data%ndata2
             x2(:,i) = data%zdata%x(:,1)
          end do
          y2 = data%zdata%y
       else
          x1 = data%zdata%x(:,1)
          y1 = data%zdata%y(1,:)
       end if
    end if

    if (c2d) then
       if (xlog) x2 = log10(x2)
       if (ylog) y2 = log10(y2)
    else
       if (xlog) x1 = log10(x1)
       if (ylog) y1 = log10(y1)
    end if

    if (data%zdata%set_levels) then
       if (.not. allocated(data%zdata%levels)) return
       clevels => data%zdata%levels
       clall = .false.
    else
       allocate(clevels(data%zdata%n_levels))
       if (data%zdata%lmap == 1) then
          zmin = minval(z, ieee_is_finite(z) .and. z > 0.)
          zmax = maxval(z, ieee_is_finite(z) .and. z > 0.)
       else
          zmin = minval(z, ieee_is_finite(z))
          zmax = maxval(z, ieee_is_finite(z))
       end if
       call gr_make_levels(zmin, zmax, data%zdata%lmap, clevels)
       clall = .true.
    end if

    if (data%zdata%fill == 1_int8) then
       xmin = pdefs%axrange(1,1)
       xmax = pdefs%axrange(2,1)
       if (xlog) then
          xmin = log10(xmin)
          xmax = log10(xmax)
       end if
       if (pdefs%y_right .and. data%y_axis == 1) then
          ymin = pdefs%axrange(1,3)
          ymax = pdefs%axrange(2,3)
       else
          ymin = pdefs%axrange(1,2)
          ymax = pdefs%axrange(2,2)
       end if
       if (ylog) then
          ymin = log10(ymin)
          ymax = log10(ymax)
       end if
    end if

    call plcol0(1)
    call plwidth(1.0_plflt)
    call gr_plot_linesty(0_int16)

    if (data%zdata%fill == 2_int8) call hl_gtk_info_bar_message(gr_infobar, &
         & "gr_contour: Contour ticks not (yet) supported"//c_null_char)

   do i = 1, data%zdata%n_levels
       if (data%zdata%label /= 0 .and. &
            & mod(i, max(data%zdata%label,1)) == data%zdata%label_off) then
          call pl_setcontlabelparam(real(0.006*data%zdata%charsize, plflt), &
               & real(0.5*data%zdata%charsize, plflt), 0.5_plflt, 1)
       else
          call pl_setcontlabelparam(0.006_plflt, &
               & real(0.5*data%zdata%charsize, plflt), 0.25_plflt, 0)
       end if
       
       ! I'm not convinced that this will do what it is supposed to do!
       ! It might have made sense in 2013, but I don't get it now.
       ! SJT 2/4/20
       if (data%zdata%fill == 1_int8) then
          if (allocated(data%zdata%colours) .and. data%zdata%n_cols > 0) &
               & ccol = real(data%zdata%colours(mod(i-1, &
               & data%zdata%n_cols)+1), real64)
          if (c2d) then
             call gr_plshade(z, xmin, xmax, ymin, ymax, clevels(i), &
                  & ccol, x2, y2)
          else
             call gr_plshade(z, xmin, xmax, ymin, ymax, clevels(i), &
                  & ccol, x1, y1)
          end if
       else
          if (allocated(data%zdata%thick) .and. data%zdata%n_thick > 0) &
               & call plwidth(data%zdata%thick(mod(i-1, &
               & data%zdata%n_thick)+1))
          if (allocated(data%zdata%colours) .and. data%zdata%n_cols > 0) then
             icidx = mod(i-1, data%zdata%n_cols)+1
             icc = int(data%zdata%colours(icidx))
             if (icc == -1) cycle
             if (icc == -2) then
                icrc= data%zdata%raw_colours(:, icidx)
                call gr_custom_line(icrc)
             else
                call plcol0(icc)
             end if
          end if
          if (allocated(data%zdata%style) .and. data%zdata%n_sty > 0) then
             if (allocated(data%zdata%thick) .and. data%zdata%n_thick > 0) then
                call gr_plot_linesty(data%zdata%style(mod(i-1, &
                     & data%zdata%n_sty)+1), &
                     & scale=ceiling(sqrt(data%zdata%thick(mod(i-1, &
                     & data%zdata%n_thick)+1))))
             else
                call gr_plot_linesty(data%zdata%style(mod(i-1, &
                     & data%zdata%n_sty)+1))
             end if
          end if
          if (c2d) then
             call plcont(z, 1, data%ndata, 1, data%ndata2, [clevels(i)], x2, y2)
          else
             call plcont(z, 1, data%ndata, 1, data%ndata2, [clevels(i)], x1, y1)
          end if
       end if
    end do
    if (clall) deallocate(clevels)

  end subroutine gr_contour

  subroutine gr_shade(index)
    integer, intent(in) :: index

    ! Display data as a colour image.

    type(graff_data), pointer :: data
    real(kind=real64), dimension(:,:), allocatable :: z
    real(kind=real64), dimension(:,:), allocatable :: x2, y2
    real(kind=real64), dimension(:), allocatable :: x1, y1
    logical :: c2d, xlog, ylog
    integer :: i, nx, ny
    real(kind=real64) :: zmin, zmax, xmin, xmax, ymin, ymax

    data => pdefs%data(index)

    if (.not. allocated(data%zdata%z)) return
    call gr_plot_transform(dataset=index, full=data%noclip)

    xlog = pdefs%axtype(1) == 1
    if (pdefs%y_right .and. pdefs%data(index)%y_axis == 1) then
       ylog = pdefs%axtype(3) == 1
    else
       ylog = pdefs%axtype(2) == 1
    end if

    nx = data%ndata
    ny = data%ndata2

    allocate(z(nx, ny))
    z = data%zdata%z

    if (data%zdata%range(1) == data%zdata%range(2)) then
       zmin = minval(z)
       zmax = maxval(z)
    else
       zmin = data%zdata%range(1)
       zmax = data%zdata%range(2)
    end if
    if (data%zdata%ilog == 1_int16) then
       if (min(zmin, zmax) > 0.) then
          z = log10(z)
          zmin = log10(zmin)
          zmax = log10(zmax)
       else
          call hl_gtk_info_bar_message(gr_infobar, &
            & "Setting a zero or negative limit for a log mapping, "//&
            & "using linear"//c_null_char)
       end if
    else if (data%zdata%ilog == 2_int16) then
       where(z /= 0.) z = z / sqrt(abs(z))
       if (zmin /= 0.) zmin = zmin / sqrt(abs(zmin))
       if (zmax /= 0.) zmax = zmax / sqrt(abs(zmax))
    end if

    allocate(x1(nx+1), y1(ny+1))
    if (data%zdata%x_is_2d .or. data%zdata%y_is_2d) then
       allocate(x2(nx+1, ny+1), y2(nx+1, ny+1))
       if (data%zdata%x_is_2d) then
          x2(:,1) = [data%zdata%x(1,1), &
               & (data%zdata%x(1:nx-1,1)+data%zdata%x(2:,1))/2._real64, &
               & data%zdata%x(nx,1)]
          x2(:,ny+1) = [data%zdata%x(1,ny), &
               & (data%zdata%x(1:nx-1,ny)+data%zdata%x(2:,ny))/2._real64, &
               & data%zdata%x(nx,ny)]
          do i = 2, ny
             x2(:,i) = ([data%zdata%x(1,i-1), &
                  & (data%zdata%x(1:nx-1,i-1)+data%zdata%x(2:,i-1))/2._real64, &
                  & data%zdata%x(nx,i-1)] + &
                  & [data%zdata%x(1,i), &
                  & (data%zdata%x(1:nx-1,i)+data%zdata%x(2:,i))/2._real64, &
                  & data%zdata%x(nx,i)]) / 2._real64
          end do
       else
          x1 = [data%zdata%x(1,1), &
               & (data%zdata%x(1:nx-1,1)+data%zdata%x(2:,1))/2._real64, &
               & data%zdata%x(nx,1)]
          do i = 1, ny+1
             x2(:,i) = x1
          end do
       end if

       if (data%zdata%y_is_2d) then
          y2(1,:) = [data%zdata%y(1,1), &
               & (data%zdata%y(1,1:ny-1)+data%zdata%y(1,2:))/2._real64, &
               & data%zdata%y(1,ny)]
          y2(nx+1,:) = [data%zdata%y(nx,1), &
               & (data%zdata%y(nx,1:ny-1)+data%zdata%y(nx,2:))/2._real64, &
               & data%zdata%y(nx,ny)]
          do i = 2, nx
             y2(i,:) = ([data%zdata%y(i-1,1), &
                  & (data%zdata%y(i-1,1:ny-1)+data%zdata%y(i-1,2:))/2._real64, &
                  & data%zdata%y(i-1,ny)] + &
                  & [data%zdata%y(i,1), &
                  & (data%zdata%y(i,1:ny-1)+data%zdata%y(i,2:))/2._real64, &
                  & data%zdata%y(i,ny)]) / 2._real64
          end do
       else
          y1 = [data%zdata%y(1,1), &
               & (data%zdata%y(1,1:ny-1)+data%zdata%y(1,2:))/2._real64, &
               & data%zdata%y(1,ny)]
          do i = 1, nx+1
             y2(i,:) = y1
          end do
       end if
       c2d = .true.
    else
       x1 = [data%zdata%x(1,1), &
            & (data%zdata%x(1:nx-1,1)+data%zdata%x(2:,1))/2._real64, &
            & data%zdata%x(nx,1)]
       y1 = [data%zdata%y(1,1), &
            & (data%zdata%y(1,1:ny-1)+data%zdata%y(1,2:))/2._real64, &
            & data%zdata%y(1,ny)]
       c2d = .false.
    end if

    if (c2d) then
       if (xlog) x1 = log10(x1)
       if (ylog) y1 = log10(y1)
    else
       if (xlog) x1 = log10(x1)
       if (ylog) y1 = log10(y1)
    end if

    xmin = pdefs%axrange(1,1)
    xmax = pdefs%axrange(2,1)
    if (xlog) then
       xmin = log10(xmin)
       xmax = log10(xmax)
    end if

    if (pdefs%y_right .and. data%y_axis == 1) then
       ymin = pdefs%axrange(1,3)
       ymax = pdefs%axrange(2,3)
    else
       ymin = pdefs%axrange(1,2)
       ymax = pdefs%axrange(2,2)
    end if
    if (ylog) then
       ymin = log10(ymin)
       ymax = log10(ymax)
    end if

    call gr_ct_get(int(data%zdata%ctable), .true., invert=data%zdata%invert, &
         & gamma=data%zdata%gamma)

    if (c2d) then
       call plimagefr(z, xmin, xmax, ymin, ymax, minval(z), maxval(z), &
            & zmin, zmax, x2, y2)
    else
       call plimagefr(z, xmin, xmax, ymin, ymax, minval(z), maxval(z), &
            & zmin, zmax, x1, y1)
    end if


  end subroutine gr_shade

  subroutine gr_shade_smooth(index)
    integer, intent(in) :: index

    ! Display data as a colour image.
    ! using plshades (slower but better looking)

    type(graff_data), pointer :: data
    real(kind=real64), dimension(:,:), allocatable :: z
    real(kind=real64), dimension(:), allocatable ::  clevels
    real(kind=real64), dimension(:,:), allocatable ::  x2, y2
    real(kind=real64), dimension(:), allocatable :: x1, y1
    logical :: c2d
    integer :: i
    real(kind=real64) :: zmin, zmax, xmin, xmax, ymin, ymax, z0, z1
    logical :: xlog, ylog, zflag

    data => pdefs%data(index)
    if (.not. allocated(data%zdata%z)) return
    call gr_plot_transform(dataset=index, full=data%noclip)

    xlog = pdefs%axtype(1) == 1
    if (pdefs%y_right .and. pdefs%data(index)%y_axis == 1) then
       ylog = pdefs%axtype(3) == 1
    else
       ylog = pdefs%axtype(2) == 1
    end if

    allocate(z(data%ndata, data%ndata2))
    z = data%zdata%z

    z0 = minval(z)
    z1 = maxval(z)
    if (data%zdata%range(1) == data%zdata%range(2)) then
       zmin = z0
       zmax = z1
    else
       zmin = data%zdata%range(1)
       zmax = data%zdata%range(2)
    end if

    zflag = .false.
    if (data%zdata%ilog == 1_int16) then
       if (min(zmin, zmax) > 0.) then
          z = log10(z)
          zmin = log10(zmin)
          zmax = log10(zmax)
       else
          call hl_gtk_info_bar_message(gr_infobar, &
            & "Setting a zero or negative limit for a log mapping, "//&
            & "using linear"//c_null_char)
          zflag = .true.
       end if
    else if (data%zdata%ilog == 2_int16) then
       where(z /= 0.) z = z / sqrt(abs(z))
       if (zmin /= 0.) zmin = zmin / sqrt(abs(zmin))
       if (zmax /= 0.) zmax = zmax / sqrt(abs(zmax))
    end if

    where(.not. ieee_is_finite(z)) z = data%zdata%missing

    if (data%zdata%shade_levels == 0) data%zdata%shade_levels = 256
    allocate(clevels(data%zdata%shade_levels))

    do i = 1, size(clevels)
       clevels(i) = zmin + real(i-1)*(zmax - zmin)/real(size(clevels)-1)
    end do
!    if (.not. zflag .and. data%zdata%ilog) clevels = 10.**clevels
    clevels(1) = min(clevels(1), z0)
    clevels(size(clevels)) = max(clevels(size(clevels)), z1)

    if (data%zdata%x_is_2d .or. data%zdata%y_is_2d) then
       allocate(x2(data%ndata, data%ndata2), y2(data%ndata, data%ndata2))
       c2d = .true.
    else
       allocate(x1(data%ndata), y1(data%ndata2))
       c2d = .false.
    end if

    if (data%zdata%x_is_2d) then
       if (data%zdata%y_is_2d) then
          x2 = data%zdata%x
          y2 = data%zdata%y
       else
          x2 = data%zdata%x
          do i = 1, data%ndata
             y2(i,:) = data%zdata%y(1,:)
          end do
       end if
    else
       if (data%zdata%y_is_2d) then
          do i = 1, data%ndata2
             x2(:,i) = data%zdata%x(:,1)
          end do
          y2 = data%zdata%y
       else
          x1 = data%zdata%x(:,1)
          y1 = data%zdata%y(1,:)
       end if
    end if

    if (c2d) then
       if (xlog) x2 = log10(x2)
       if (ylog) y2 = log10(y2)
    else
       if (xlog) x1 = log10(x1)
       if (ylog) y1 = log10(y1)
    end if

    xmin = pdefs%axrange(1,1)
    xmax = pdefs%axrange(2,1)
    if (xlog) then
       xmin = log10(xmin)
       xmax = log10(xmax)
    end if

    if (pdefs%y_right .and. data%y_axis == 1) then
       ymin = pdefs%axrange(1,3)
       ymax = pdefs%axrange(2,3)
    else
       ymin = pdefs%axrange(1,2)
       ymax = pdefs%axrange(2,2)
    end if
    if (ylog) then
       ymin = log10(ymin)
       ymax = log10(ymax)
    end if

    call gr_ct_get(int(data%zdata%ctable), .true., invert=data%zdata%invert, &
         & gamma=data%zdata%gamma)

    if (c2d) then
       call gr_plshades(z, xmin, xmax, ymin, ymax, clevels, x2, y2)
    else
       call gr_plshades(z, xmin, xmax, ymin, ymax, clevels, x1, y1)
    end if
  end subroutine gr_shade_smooth

  subroutine gr_stamp(xn, yn, just)
    real(kind=plflt), intent(in), optional :: xn, yn, just

    ! Put a timestamp on a plot

    real(kind=plflt) :: xxn, yyn, align, xw, yw
    character(len=19) :: date

    if (present(xn)) then
       xxn = xn
    else
       xxn = 0.99_plflt
    end if
    if (present(yn)) then
       yyn = yn
    else
       yyn = 0.02_plflt
    end if
    if (present(just)) then
       align = just
    else
       align = 1.0_plflt
    end if

    call gr_date(date)

    call gr_plot_coords_n_w(xxn, yyn, xw, yw)
    call gr_plot_transform(full=.true._int8)
    call plsfont(font_list(1), font_shape(1), font_weight(1))
    call plschr(0._plflt, real(pdefs%charsize, plflt)*0.5_plflt)
    call plptex(xw, yw, 1._plflt, 0._plflt, align, date)
  end subroutine gr_stamp

  subroutine gr_make_levels(zmin, zmax, map, levels)
    real(kind=real64), intent(in) :: zmin, zmax
    integer(kind=int16), intent(in) :: map
    real(kind=real64), dimension(:), intent(out) :: levels

    real(kind=real64) :: rg, lmx, lmn
    integer :: i, nl
    real(kind=real64), dimension(:), allocatable :: slev
    
    nl = size(levels)

    select case (map)
    case(0)                    ! Linear scaling
       rg = zmax - zmin

       do i = 1, nl
          levels(i) = rg * (real(i, real64) - 0.5_real64) / &
               & real(nl, real64) + zmin
       end do
       
    case(1)                    ! Log scaling
       lmx = log10(zmax)
       lmn = log10(zmin)
       
       if (.not. ieee_is_finite(lmx) .or. .not. ieee_is_finite(lmn)) then
          levels(:) = 0._real64
       else
          rg = lmx-lmn
          do i = 1, nl
             levels(i) = 10._real64 ** (rg * (real(i, real64) - 0.5_real64)/ &
                  & real(nl,real64) + lmn)
          end do
       end if

    case (2)                 ! Square root scaling
       if (zmax == 0._real64) then
          lmx = 0._real64
       else
          lmx = zmax / sqrt(abs(zmax))
       end if
       if (zmin == 0._real64) then
          lmn = 0._real64
       else
          lmn = zmin / sqrt(abs(zmin))
       end if

       rg = lmx-lmn
       allocate(slev(nl))

       do i = 1, nl
          slev(i) = rg * (real(i, real64) - 0.5_real64) / real(nl, real64) &
               & + lmn
       end do
       levels = slev**2
       where(slev < 0._real64) levels = -levels
       
    end select
  end subroutine gr_make_levels
end module gr_plot_procs
