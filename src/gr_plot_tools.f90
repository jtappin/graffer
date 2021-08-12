! Copyright (C) 2013-2021
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

module gr_plot_tools
  ! Generic plot related stuff that makes plplot calls (e.g. coordinates
  ! and linestyles)

  use iso_fortran_env
  use iso_c_binding

  use plplot
  use gtk_draw_hl

  use graff_globals
  use gr_colours

  implicit none

contains

  ! ******************************************************************
  !   COORDINATE CONVERTERS

  ! Device to normalized
  subroutine gr_plot_coords_d_n(xin, yin, xout, yout)
    real(kind=plflt), intent(in) :: xin, yin
    real(kind=plflt), intent(out) :: xout, yout

    integer(kind=c_int) :: width, height

    call hl_gtk_drawing_area_get_size(gr_drawing_area, &
         & width=width, height=height)

    xout = xin/real(width, plflt)
    yout = 1._plflt - yin/real(height, plflt)

  end subroutine gr_plot_coords_d_n

  ! Device to viewport
  subroutine gr_plot_coords_d_v(xin, yin, xout, yout)
    real(kind=plflt), intent(in) :: xin, yin
    real(kind=plflt), intent(out) :: xout, yout

    real(kind=plflt) :: xtmp, ytmp

    call gr_plot_coords_d_n(xin, yin, xtmp, ytmp)
    call gr_plot_coords_n_v(xtmp, ytmp, xout, yout)

  end subroutine gr_plot_coords_d_v

  ! Device to world
  subroutine gr_plot_coords_d_w(xin, yin, xout, yout, nolog, y_axis)
    real(kind=plflt), intent(in) :: xin, yin
    real(kind=plflt), intent(out) :: xout, yout
    logical, intent(in), optional :: nolog
    integer, intent(in), optional :: y_axis

    real(kind=plflt) :: xtmp, ytmp

    call gr_plot_coords_d_n(xin, yin, xtmp, ytmp)
    call gr_plot_coords_n_w(xtmp, ytmp, xout, yout, &
         & nolog=nolog, y_axis=y_axis)

  end subroutine gr_plot_coords_d_w

  ! Normalized to device
  subroutine gr_plot_coords_n_d(xin, yin, xout, yout)
    real(kind=plflt), intent(in) :: xin, yin
    real(kind=plflt), intent(out) :: xout, yout

    integer(kind=c_int) :: width, height

    call hl_gtk_drawing_area_get_size(gr_drawing_area, &
         & width=width, height=height)

    xout = xin*real(width, plflt)
    yout = (1._plflt - yin)*real(height, plflt)

  end subroutine gr_plot_coords_n_d

  ! Normalized to viewport
  subroutine gr_plot_coords_n_v(xin, yin, xout, yout)
    real(kind=plflt), intent(in) :: xin, yin
    real(kind=plflt), intent(out) :: xout, yout

    real(kind=plflt) :: xvn0, xvn1, yvn0, yvn1

    call plgvpd(xvn0, xvn1, yvn0, yvn1)

    xout = (xin-xvn0)/(xvn1-xvn0)
    yout = (yin-yvn0)/(yvn1-yvn0)

  end subroutine gr_plot_coords_n_v

  ! Normalized to world
  subroutine gr_plot_coords_n_w(xin, yin, xout, yout, nolog, y_axis)
    real(kind=plflt), intent(in) :: xin, yin
    real(kind=plflt), intent(out) :: xout, yout
    logical, intent(in), optional :: nolog
    integer, intent(in), optional :: y_axis

    real(kind=plflt) :: xtmp, ytmp

    call gr_plot_coords_n_v(xin, yin, xtmp, ytmp)
    call gr_plot_coords_v_w(xtmp, ytmp, xout, yout, nolog=nolog, y_axis=y_axis)

  end subroutine gr_plot_coords_n_w

  ! Viewport to device
  subroutine gr_plot_coords_v_d(xin, yin, xout, yout)
    real(kind=plflt), intent(in) :: xin, yin
    real(kind=plflt), intent(out) :: xout, yout

    real(kind=plflt) :: xtmp, ytmp

    call gr_plot_coords_v_n(xin, yin, xtmp, ytmp)
    call gr_plot_coords_n_d(xtmp, ytmp, xout, yout)

  end subroutine gr_plot_coords_v_d

  ! Viewport to normalized
  subroutine gr_plot_coords_v_n(xin, yin, xout, yout)
    real(kind=plflt), intent(in) :: xin, yin
    real(kind=plflt), intent(out) :: xout, yout

    real(kind=plflt) :: xvn0, xvn1, yvn0, yvn1

    call plgvpd(xvn0, xvn1, yvn0, yvn1)

    xout = xin*(xvn1-xvn0)+xvn0
    yout = yin*(yvn1-yvn0)+yvn0

  end subroutine gr_plot_coords_v_n

  ! Viewport to world
  subroutine gr_plot_coords_v_w(xin, yin, xout, yout, nolog, y_axis)
    real(kind=plflt), intent(in) :: xin, yin
    real(kind=plflt), intent(out) :: xout, yout
    logical, intent(in), optional :: nolog
    integer, intent(in), optional :: y_axis

    real(kind=plflt) :: xvw0, xvw1, yvw0, yvw1
    logical :: do_log
    integer :: iaxis

    if (present(nolog)) then
       do_log = .not. nolog
    else
       do_log = .true.
    end if

    if (present(y_axis)) then
       call gr_plot_transform(index=y_axis, noupdate=.true.)
       iaxis = y_axis+1
    else
       iaxis = pdefs%transform%world_selected+1
    end if
    call plgvpw(xvw0, xvw1, yvw0, yvw1)

    xout = xin*(xvw1-xvw0)+xvw0
    yout = yin*(yvw1-yvw0)+yvw0

    if (do_log) then
       if (pdefs%axtype(1) == 1) xout = 10._plflt ** xout
       if (pdefs%axtype(iaxis) == 1) yout = 10._plflt ** yout
    end if

    if (present(y_axis)) call gr_plot_transform()

  end subroutine gr_plot_coords_v_w

  ! World to device
  subroutine gr_plot_coords_w_d(xin, yin, xout, yout, nolog, y_axis)
    real(kind=plflt), intent(in) :: xin, yin
    real(kind=plflt), intent(out) :: xout, yout
    logical, intent(in), optional :: nolog
    integer, intent(in), optional :: y_axis

    real(kind=plflt) :: xtmp, ytmp

    call gr_plot_coords_w_n(xin, yin, xtmp, ytmp, &
         & nolog=nolog, y_axis=y_axis)
    call gr_plot_coords_n_d(xtmp, ytmp, xout, yout)

  end subroutine gr_plot_coords_w_d

  ! World to normalized
  subroutine gr_plot_coords_w_n(xin, yin, xout, yout, nolog, y_axis)
    real(kind=plflt), intent(in) :: xin, yin
    real(kind=plflt), intent(out) :: xout, yout
    logical, intent(in), optional :: nolog
    integer, intent(in), optional :: y_axis

    real(kind=plflt) :: xtmp, ytmp

    call gr_plot_coords_w_v(xin, yin, xtmp, ytmp, &
         & nolog=nolog, y_axis=y_axis)
    call gr_plot_coords_v_n(xtmp, ytmp, xout, yout)

  end subroutine gr_plot_coords_w_n

  ! World to viewport
  subroutine gr_plot_coords_w_v(xin, yin, xout, yout, nolog, y_axis)
    real(kind=plflt), intent(in) :: xin, yin
    real(kind=plflt), intent(out) :: xout, yout
    logical, intent(in), optional :: nolog
    integer, intent(in), optional :: y_axis

    logical :: do_log
    real(kind=plflt) :: xvw0, xvw1, yvw0, yvw1
    real(kind=plflt) :: xtmp, ytmp
    integer :: iaxis

    if (present(nolog)) then
       do_log = .not. nolog
    else
       do_log = .true.
    end if

    xtmp = xin
    ytmp = yin

    if (present(y_axis)) then
       call gr_plot_transform(index=y_axis, noupdate=.true.)
       iaxis = y_axis+1
    else
       iaxis = pdefs%transform%world_selected+1
    end if

    call plgvpw(xvw0, xvw1, yvw0, yvw1)

    if (do_log) then
       if (pdefs%axtype(1) == 1) xtmp = log10(xtmp)
       if (pdefs%axtype(iaxis)  == 1) ytmp = log10(ytmp)
    end if

    xout = (xtmp-xvw0)/(xvw1-xvw0)
    yout = (ytmp-yvw0)/(yvw1-yvw0)

    if (present(y_axis)) call gr_plot_transform()

  end subroutine gr_plot_coords_w_v

  ! **************************************************************
  !    Transform settings

  subroutine gr_plot_transform(dataset, index, full, noupdate)
    integer, intent(in), optional :: dataset, index
    logical(kind=int8), intent(in), optional :: full
    logical, intent(in), optional :: noupdate

    ! Set/select plot transform

    integer :: ds_index, widx
    logical :: iupdate, all
    real(kind=plflt) :: x0, x1, y0, y1

    if (present(noupdate)) then
       iupdate = .not. noupdate
    else
       iupdate = .true.
    end if

    if (present(full)) then
       all = full
    else
       all = .not. pdefs%transform%viewport_enabled
    end if

    if (.not. pdefs%y_right) then
       widx = 1
    else if (present(dataset)) then
       if (dataset == 0) then
          ds_index = pdefs%cset
       else
          ds_index = dataset
       end if
       widx = pdefs%data(ds_index)%y_axis+1
    else if (present(index)) then
       widx = index
    else
       widx = pdefs%transform%world_selected
    end if

    if (.not. pdefs%transform%viewport_enabled) &
         & call gr_plot_viewport

    call plwind(pdefs%transform%world(1, widx), &
         & pdefs%transform%world(2, widx), &
         & pdefs%transform%world(3, widx), &
         & pdefs%transform%world(4, widx))

    if (iupdate) pdefs%transform%world_selected = widx

    if (all) then
       call gr_plot_coords_n_w(0._plflt, 0._plflt, x0, y0, nolog=.true.)
       call gr_plot_coords_n_w(1._plflt, 1._plflt, x1, y1, nolog=.true.)

       call plvpor(0._plflt, 1._plflt, 0._plflt, 1._plflt)
       call plwind(x0, x1, y0, y1)
       pdefs%transform%viewport_enabled = .false.
    else
       pdefs%transform%viewport_enabled = .true.
    end if
  end subroutine gr_plot_transform

  subroutine gr_plot_viewport

    ! Set up viewport.

    if (pdefs%transform%viewport(1) == pdefs%transform%viewport(2)) then
       call plvasp(pdefs%transform%vp_aspect)
       call plgvpd(pdefs%transform%viewport(1), &
            & pdefs%transform%viewport(2), &
            & pdefs%transform%viewport(3), &
            & pdefs%transform%viewport(4))
    else if (pdefs%transform%vp_aspect == 0.) then
       call plvpor(pdefs%transform%viewport(1), &
            & pdefs%transform%viewport(2), &
            & pdefs%transform%viewport(3), &
            & pdefs%transform%viewport(4))
    else
       call plvpas(pdefs%transform%viewport(1), &
            & pdefs%transform%viewport(2), &
            & pdefs%transform%viewport(3), &
            & pdefs%transform%viewport(4), &
            & pdefs%transform%vp_aspect)
    end if
  end subroutine gr_plot_viewport

  ! *************************************************************
  !     SYMBOLS & LINESTYLES


  function gr_vp_aspect()
    real(kind=plflt) :: gr_vp_aspect

    ! Return the physical aspect ratio of the viewport,
    ! for use to make sure symbols, error caps etc. aren't squashed.

    real(kind=plflt) :: p_xp, p_yp, p_xmin, p_xmax, p_ymin, p_ymax
    integer :: p_xleng, p_yleng, p_xoff, p_yoff
    
    call plgpage(p_xp, p_yp, p_xleng, p_yleng, p_xoff, p_yoff)
    call plgvpd(p_xmin, p_xmax, p_ymin, p_ymax)

    gr_vp_aspect = p_yleng*(p_ymax-p_ymin) / &
         & (p_xleng*(p_xmax-p_xmin))
  end function gr_vp_aspect
    
  subroutine gr_plot_symbol(x, y, index, symsize, use)
    real(kind=plflt), intent(in), dimension(:) :: x, y
    integer(kind=int16), intent(in) :: index
    real(kind=real64), intent(in) :: symsize
    logical, dimension(:), intent(in), optional, target :: use

    ! Plot symbols at data points

    real(kind=plflt) :: dx, dy, aspect
    real(kind=plflt) :: x0, x1, y0, y1
    real(kind=plflt), dimension(:), allocatable :: xs, ys
    logical :: filled
    integer :: npoints, i
    real(kind=plflt) :: th
    logical, dimension(:), pointer :: iuse

    if (present(use)) then
       iuse => use
    else
       allocate(iuse(size(x)))
       iuse(:) = .true.
    end if

    call plgvpw(x0,x1,y0,y1)
    aspect = gr_vp_aspect()
    
    dx = abs(x1-x0)/100._plflt
    dy = abs(y1-y0)/100._plflt
    if (aspect > 1._plflt) then
       dx = dx*aspect
    else if (aspect < 1._plflt) then
       dy = dy / aspect
    end if
    
    select case (index)
    case(1)     ! PLUS
       npoints = 5
       allocate(xs(npoints), ys(npoints))
       xs = [real(kind=plflt) :: 1., -1., 0., 0., 0.]*symsize
       ys = [real(kind=plflt) :: 0., 0., 0., 1., -1.]*symsize
       filled = .false.
    case(2)     ! Asterisk
       npoints = 11
       allocate(xs(npoints), ys(npoints))
       xs = [real(kind=plflt) :: 1., -1., 0., 0., 0., 0., 1., -1., 0., &
            & -1., 1.]*symsize
       ys = [real(kind=plflt) :: 0., 0., 0., 1., -1., 0., 1., -1., 0., &
            & 1., -1.]*symsize
       filled = .false.
    case(3)     ! Point
       npoints = 5
       allocate(xs(npoints), ys(npoints))
       xs = [real(kind=plflt) :: -.01, 0., .01, 0., -.01]
       ys = [real(kind=plflt) :: 0., .01, 0., -.01, 0.]
    case(4)     ! Diamond
       npoints = 5
       allocate(xs(npoints), ys(npoints))
       xs = [real(kind=plflt) :: 1., 0., -1., 0., 1.]*symsize
       ys = [real(kind=plflt) :: 0., 1., 0., -1., 0.]*symsize
       filled = .false.
    case(5)     ! Triangle
       npoints = 4
       allocate(xs(npoints), ys(npoints))
       xs = [real(kind=plflt) :: -1., 1., 0., -1]*symsize
       ys = [real(kind=plflt) :: -1., -1., 1., -1]*symsize
       filled = .false.
    case(6)     ! Square
       npoints = 5
       allocate(xs(npoints), ys(npoints))
       xs = [real(kind=plflt) :: 1., 1., -1., -1., 1.]*symsize
       ys = [real(kind=plflt) :: 1., -1., -1., 1., 1.]*symsize
       filled = .false.
    case(7)     ! Cross
       npoints = 5
       allocate(xs(npoints), ys(npoints))
       xs = [real(kind=plflt) :: 1., -1., 0., -1., 1.]*symsize
       ys = [real(kind=plflt) :: 1., -1., 0, 1., -1.]*symsize
       filled = .false.
    case(8)     ! Circle
       npoints = 31
       allocate(xs(npoints), ys(npoints))
       do i = 1, npoints
          th = real(i, plflt)*2._plflt*pl_pi/real(npoints-1, plflt)
          xs(i) = cos(th)*symsize
          ys(i) = sin(th)*symsize
       end do
       filled = .false.
    case(9)     ! Filled diamond
       npoints = 4
       allocate(xs(npoints), ys(npoints))
       xs = [real(kind=plflt) :: 1., 0., -1., 0.]*symsize
       ys = [real(kind=plflt) :: 0., 1., 0., -1.]*symsize
       filled = .true.
    case(10)    ! Filled triangle
       npoints = 3
       allocate(xs(npoints), ys(npoints))
       xs = [real(kind=plflt) :: -1., 1., 0.]*symsize
       ys = [real(kind=plflt) :: -1., -1., 1.]*symsize
       filled = .true.
    case(11)    ! Filled square
       npoints = 4
       allocate(xs(npoints), ys(npoints))
       xs = [real(kind=plflt) :: 1., 1., -1., -1.]*symsize
       ys = [real(kind=plflt) :: 1., -1., -1., 1.]*symsize
       filled = .true.
    case(12)    ! Filled circle
       npoints = 30
       allocate(xs(npoints), ys(npoints))
       do i = 1, npoints
          th = real(i, plflt)*2._plflt*pl_pi/real(npoints, plflt)
          xs(i) = cos(th)*symsize
          ys(i) = sin(th)*symsize
       end do
       filled = .true.
    case(13)    ! inverted triangle
       npoints = 4
       allocate(xs(npoints), ys(npoints))
       xs = [real(kind=plflt) :: -1., 1., 0., -1]*symsize
       ys = [real(kind=plflt) :: 1., 1., -1., 1]*symsize
       filled = .false.
    case(14)    ! filled inverted triangle
       npoints = 3
       allocate(xs(npoints), ys(npoints))
       xs = [real(kind=plflt) :: -1., 1., 0.]*symsize
       ys = [real(kind=plflt) :: 1., 1., -1]*symsize
       filled = .true.
    case(15)    ! Hexagon
       npoints = 7
       allocate(xs(npoints), ys(npoints))
       do i = 1, npoints
          th = real(i, plflt) * 2._plflt * pl_pi / real(npoints-1, plflt)
          xs(i) = cos(th)
          ys(i) = sin(th)
       end do
       filled = .false.
    case(16)    ! Filled Hexagon
       npoints = 6
       allocate(xs(npoints), ys(npoints))
       do i = 1, npoints
          th = real(i, plflt) * 2._plflt * pl_pi / real(npoints, plflt)
          xs(i) = cos(th)
          ys(i) = sin(th)
       end do
       filled = .true.
    case(17)    ! Horizontal bar
       npoints = 2
       allocate(xs(npoints), ys(npoints))
       xs = [real(kind=plflt) :: -1., 1.]*symsize
       ys = 0._plflt 
       filled = .false.
    case(18)    ! Vertical bar
       npoints = 2
       allocate(xs(npoints), ys(npoints))
       xs = 0._plflt 
       ys = [real(kind=plflt) :: -1., 1.]*symsize
       filled = .false.
    case default
       npoints = 7
       allocate(xs(npoints), ys(npoints))
       xs = [real(kind=plflt) :: 0., 0., .8, .8, .6, -.6, -.8] * symsize
       ys = [real(kind=plflt) :: -1., -.2, .2, .7, .9, .9, .7] * symsize
       filled = .false.

    end select

    call gr_plot_linesty(0_int16)

    do i = 1, size(x)
       if (.not. iuse(i)) cycle
       if (filled) then
          call plfill(x(i)+xs*dx, y(i)+ys*dy)
       else
          call plline(x(i)+xs*dx, y(i)+ys*dy)
       end if
    end do

    if (.not. present(use)) deallocate(iuse)

  end subroutine gr_plot_symbol

  subroutine gr_plot_linesty(index, scale)
    integer(kind=int16), intent(in) :: index
    integer, intent(in), optional :: scale

    ! Define the linestyle.

    integer, dimension(:), allocatable :: ld, lg
    integer :: nseg

    select case (index)
    case(0)
       call pllsty(1)
       return

    case(1)
       nseg = 1
       allocate(ld(1), lg(1))
       ld = 20
       lg = 1980

    case(2)
       nseg = 1
       allocate(ld(1), lg(1))
       ld = 1000
       lg = 2000

    case(3)
       nseg = 2
       allocate(ld(2), lg(2))
       ld = [1000, 20]
       lg = [2000, 2000]

    case(4)
       nseg = 4
       allocate(ld(4), lg(4))
       ld = [1000, 20, 20, 20]
       lg = [2000, 2000, 2000, 2000]

    case(5)
       nseg = 1
       allocate(ld(1), lg(1))
       ld = 2000
       lg = 3000
    end select

    ! Only scale dashes & gaps, not dots.
    if (present(scale)) then
       where(ld > 100) ld = ld*max(scale,1)
       where(lg > 100) lg = lg*max(scale,1)
    end if

    call plstyl(ld, lg)

  end subroutine gr_plot_linesty

end module gr_plot_tools
