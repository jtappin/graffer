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

module gr_axis_autoscale
  ! Autoscale an axis and update necessary flags.

  use iso_c_binding
  use iso_fortran_env

  use gtk, only: gtk_entry_set_text, gtk_widget_set_sensitive

  use graff_types
  use graff_globals
  use gr_eval
  use gr_cb_common

  use ieee_arithmetic, only: ieee_is_finite
  
  use plplot, only: pi=>pl_pi

  implicit none

  private :: gr_autoscale_x_rect, gr_autoscale_x_polar, &
       & gr_autoscale_y_rect, gr_autoscale_y_polar
  
contains
  subroutine gr_autoscale(axis, shrink, visible, functions)
    integer, intent(in) :: axis
    logical, intent(in), optional :: shrink, visible, functions

    ! Auto scale an Axis

    real(kind=real64) :: axmin, axmax
    type(graff_data), pointer :: data
    integer :: status
    logical :: ishrink, vis_only, xy_only
    character(len=32) :: text
    integer(kind=int16) :: i

    if (axis == 3 .and. .not. pdefs%y_right) return

    if (present(shrink)) then
       ishrink = shrink
    else
       ishrink = .false.
    end if

    if (present(visible)) then
       vis_only = visible
    else
       vis_only = .false.
    end if

    if (present(functions)) then
       xy_only = .not. functions
    else
       xy_only = .false.
    end if
    
    axmin = huge(1._real64)
    axmax = -huge(1._real64)

    do i = 1, pdefs%nsets
       data => pdefs%data(i)

       if (xy_only .and. data%type < 0) cycle
       
       select case (data%type)
       case(-4)
          if (vis_only .and. data%zdata%format == 2) cycle
          status = gr_evaluate(i)
          if (status /= 0) cycle
       case(-3:-1)
          if (vis_only .and. data%colour == -1) cycle
          status = gr_evaluate(i)
          if (status /= 0) cycle
       case(0:8)
          if (vis_only .and. data%colour == -1) cycle
          if (.not. allocated(data%xydata%x)) cycle
       case(9)
          if (vis_only .and. data%zdata%format == 2) cycle
          if (.not. (allocated(data%zdata%x) .and. &
               & allocated(data%zdata%y))) cycle
       end select

       if (axis == 1) then
          if (data%mode == 0) then
             call gr_autoscale_x_rect(data, axmin, axmax, vis_only)
          else
             call gr_autoscale_x_polar(data, axmin, axmax, vis_only)
          end if
       else if (data%y_axis == axis-2 .or. .not. pdefs%y_right) then
          if (data%mode == 0) then
             call gr_autoscale_y_rect(data, axmin, axmax, vis_only)
          else
             call gr_autoscale_y_polar(data, axmin, axmax, vis_only)
          end if
       end if
    end do

    if (axmin >= axmax) return

    if (ishrink .or. vis_only) then
       pdefs%axrange(:,axis) = [axmin, axmax]
    else
       pdefs%axrange(:,axis) = [min(axmin,pdefs%axrange(1,axis)), &
            & max(axmax,pdefs%axrange(2,axis))]
    end if

    do i = 1, 2
       write(text, "(1pg0.5)") pdefs%axrange(i,axis)
       call gtk_entry_set_text(rbox(i, axis), adjustl(trim(text))//c_null_char)
    end do

    do i = 1, pdefs%nsets
       select case (pdefs%data(i)%type)
       case(-1) 
          if (axis == 1 .and.&
               & pdefs%data(i)%funct%range(1,1) == &
               & pdefs%data(i)%funct%range(2,1)) &
               & pdefs%data(i)%funct%evaluated = .false.
       case(-2)
          if (((axis == 2 .and. pdefs%data(i)%y_axis == 0) .or.&
               & (axis == 2 .and. pdefs%data(i)%y_axis == 1) .and. &
               & pdefs%data(i)%funct%range(1,1) == &
               & pdefs%data(i)%funct%range(2,1))) &
               & pdefs%data(i)%funct%evaluated = .false.
       case(-4)
          if ((axis == 1 .and. &
               & pdefs%data(i)%funct%range(1,1) == &
               & pdefs%data(i)%funct%range(2,1)) .or.  &
               & (((axis == 2 .and. pdefs%data(i)%y_axis == 0) .or.&
               & (axis == 2 .and. pdefs%data(i)%y_axis == 1)) .and. &
               & pdefs%data(i)%funct%range(1,2) == &
               & pdefs%data(i)%funct%range(2,2))) &
               & pdefs%data(i)%funct%evaluated = .false.
       end select
    end do

!!$    call gtk_widget_set_sensitive(log_chb(axis), &
!!$         & f_c_logical(minval(pdefs%axrange(:,axis)) > 0.))

    call gr_plot_draw(.true.)
  end subroutine gr_autoscale

  subroutine gr_autoscale_x_rect(data, axmin, axmax, visible)
    type(graff_data), intent(in) :: data
    real(kind=real64), intent(inout) :: axmin, axmax
    logical, intent(in) :: visible
    
    logical, dimension(:), allocatable :: mask, maske
    real(kind=real64), pointer, dimension(:) :: par
    
    ! Auto scale the X axis for a rectangular coordinate DS

    if ((data%type >= 0 .and. data%type <= 8) .or. &
         & data%type == -2 .or. data%type == -3) then
       if (data%y_axis == 0) then
          par => pdefs%axrange(:,2)
       else
          par => pdefs%axrange(:,3)
       end if
       allocate(mask(data%ndata))
       if (visible) then
          mask = ieee_is_finite(data%xydata%x) .and. &
               & data%xydata%y <= maxval(par)  .and. &
               & data%xydata%y >= minval(par)
       else
          mask = ieee_is_finite(data%xydata%x)
       end if
       if (pdefs%axtype(1) == 1) &
            & mask = mask .and. data%xydata%x > 0._real64
       if (count(mask) == 0) return

       axmin = min(axmin, minval(data%xydata%x, mask))
       axmax = max(axmax, maxval(data%xydata%x, mask))

    end if

    select case (data%type)
    case(3,5,6)
       allocate(maske(data%ndata))
       maske = ieee_is_finite(data%xydata%x_err(1,:)) .and. mask
       if (count(maske) > 0) then
          axmin = min(axmin, minval(data%xydata%x- &
               & data%xydata%x_err(1,:), maske))
          axmax = max(axmax, maxval(data%xydata%x+ &
               & data%xydata%x_err(1,:), maske))
       end if
    case(4,7,8)
       allocate(maske(data%ndata))
       maske = ieee_is_finite(data%xydata%x_err(1,:)) .and. mask
       if (count(maske) > 0) &
            & axmin = min(axmin, minval(data%xydata%x- &
            & data%xydata%x_err(1,:), maske))
       maske = ieee_is_finite(data%xydata%x_err(2,:)) .and. mask
       if (count(maske) > 0) &
            & axmax = max(axmax, maxval(data%xydata%x + &
            & data%xydata%x_err(2,:), maske))
    case(9)
       axmin = min(axmin, minval(data%zdata%x))
       axmax = max(axmax, maxval(data%zdata%x))
    case(-1, -4)
       if (data%funct%range(1,1) /= data%funct%range(2,1)) then
          axmin = min(axmin, minval(data%funct%range(:,1)))
          axmax = max(axmax, maxval(data%funct%range(:,1)))
       end if
    case(-2, -3)
       axmin = min(axmin, minval(data%xydata%x, mask))
       axmax = max(axmax, maxval(data%xydata%x, mask))
    end select
  end subroutine gr_autoscale_x_rect

  subroutine gr_autoscale_y_rect(data, axmin, axmax, visible)
    type(graff_data), intent(in) :: data
    real(kind=real64), intent(inout) :: axmin, axmax
    logical, intent(in) :: visible
    
    logical, dimension(:), allocatable :: mask, maske
    real(kind=real64), pointer, dimension(:) :: par

    ! Auto scale the Y axis for a rectangular coordinate DS

    if ((data%type >= 0 .and. data%type <= 8) .or. &
         & data%type == -1 .or. data%type == -3) then
       par => pdefs%axrange(:,1)
       allocate(mask(data%ndata))
       if (visible) then
          mask = ieee_is_finite(data%xydata%y) .and. &
               & data%xydata%x <= maxval(par)  .and. &
               & data%xydata%x >= minval(par)
       else
          mask = ieee_is_finite(data%xydata%y)
       end if
       
       if (ieee_is_finite(data%min_val)) &
            & mask = mask .and. data%xydata%y >= data%min_val
       if (ieee_is_finite(data%max_val)) &
            & mask =  mask .and. data%xydata%y <= data%max_val
       if (pdefs%axtype(data%y_axis+2) == 1) &
            & mask = mask .and. data%xydata%y > 0._real64

       if (count(mask) == 0) return

       axmin = min(axmin, minval(data%xydata%y, mask))
       axmax = max(axmax, maxval(data%xydata%y, mask))
    end if
    
    select case (data%type)
    case(1,5,7)
       allocate(maske(data%ndata))
       maske = ieee_is_finite(data%xydata%y_err(1,:)) .and. mask
       if (count(maske) > 0) then 
          axmin = min(axmin, minval(data%xydata%y- &
               & data%xydata%y_err(1,:), maske))
          axmax = max(axmax, maxval(data%xydata%y+ &
               & data%xydata%y_err(1,:), maske))
       end if
    case(2,6,8)
       allocate(maske(data%ndata))
       maske = ieee_is_finite(data%xydata%y_err(1,:)) .and. mask
       if (count(maske) > 0) &
            & axmin = min(axmin, minval(data%xydata%y- &
            & data%xydata%y_err(1,:), maske))
       maske = ieee_is_finite(data%xydata%y_err(2,:)) .and. mask
       if (count(maske) > 0) &
            & axmax = max(axmax, maxval(data%xydata%y + &
            & data%xydata%y_err(2,:), maske))

    case(9)
       axmin = min(axmin, minval(data%zdata%y))
       axmax = max(axmax, maxval(data%zdata%y))
    case(-2)
       if (data%funct%range(1,1) /= data%funct%range(2,1)) then
          axmin = min(axmin, minval(data%funct%range(:,1)))
          axmax = max(axmax, maxval(data%funct%range(:,1)))
       end if
    case(-4)
       if (data%funct%range(1,2) /= data%funct%range(2,2)) then
          axmin = min(axmin, minval(data%funct%range(:,2)))
          axmax = max(axmax, maxval(data%funct%range(:,2)))
       end if
    case(-1, -3) 
       axmin = min(axmin, minval(data%xydata%y, mask))
       axmax = max(axmax, maxval(data%xydata%y, mask))
    end select

     
  end subroutine gr_autoscale_y_rect

  subroutine gr_autoscale_x_polar(data, axmin, axmax, visible)
    type(graff_data), intent(in), target :: data
    real(kind=real64), intent(inout) :: axmin, axmax
    logical, intent(in) :: visible

    ! Autoscale the X axis for a polar dataset.

    real(kind=real64) :: scale
    real(kind=real64), dimension(:), allocatable :: r, th, x, y
    type(graff_xydata), pointer :: xydata
    logical, dimension (:), allocatable :: mask
    real(kind=real64), pointer, dimension(:) :: par

    if (data%mode == 2) then
       scale = pi/180._real64
    else
       scale = 1._real64
    end if

    xydata => data%xydata
    if (data%y_axis == 0) then
       par => pdefs%axrange(:,2)
    else
       par => pdefs%axrange(:,3)
    end if

    allocate(r(data%ndata), th(data%ndata), x(data%ndata), &
         & mask(data%ndata))
    if (visible) allocate(y(data%ndata))
    select case (data%type)
    case(:0)     ! Question about -4
       r = xydata%x
       th = xydata%y*scale
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
    case(1)
       r = xydata%x
       th = (xydata%y-xydata%y_err(1,:))*scale
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       th = (xydata%y+xydata%y_err(1,:))*scale
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       
    case(2)
       r = xydata%x
       th = (xydata%y+xydata%y_err(1,:))*scale
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       th = (xydata%y+xydata%y_err(2,:))*scale
       x = r * cos(th)
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       
    case(3)
       r = xydata%x-xydata%x_err(1,:)
       th = xydata%y*scale
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       r = xydata%x+xydata%x_err(1,:)
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
    case(4)
       r = xydata%x-xydata%x_err(1,:)
       th = xydata%y*scale
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       r = xydata%x+xydata%x_err(1,:)
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       
    case(5)
       r = xydata%x-xydata%x_err(1,:)
       th = (xydata%y-xydata%y_err(1,:))*scale
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       r = xydata%x+xydata%x_err(1,:)
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       th = (xydata%y+xydata%y_err(1,:))*scale
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       r = xydata%x-xydata%x_err(1,:)
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       
    case(6)
       r = xydata%x-xydata%x_err(1,:)
       th = (xydata%y-xydata%y_err(1,:))*scale
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       r = xydata%x+xydata%x_err(1,:)
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       th = (xydata%y+xydata%y_err(2,:))*scale
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       r = xydata%x-xydata%x_err(1,:)
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       
    case(7)
       r = xydata%x-xydata%x_err(1,:)
       th = (xydata%y-xydata%y_err(1,:))*scale
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       r = xydata%x+xydata%x_err(2,:)
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       th = (xydata%y+xydata%y_err(1,:))*scale
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       r = xydata%x-xydata%x_err(1,:)
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       
    case(8)
       r = xydata%x-xydata%x_err(1,:)
       th = (xydata%y-xydata%y_err(1,:))*scale
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       r = xydata%x+xydata%x_err(2,:)
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       th = (xydata%y+xydata%y_err(2,:))*scale
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
       r = xydata%x-xydata%x_err(1,:)
       x = r * cos(th)
       if (visible) then
          y = r * sin(th)
          mask = ieee_is_finite(x) .and. &
               & y >= minval(par) .and. y <= maxval(par)
       else
          mask = ieee_is_finite(x)
       end if
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
    end select

  end subroutine gr_autoscale_x_polar

  subroutine gr_autoscale_y_polar(data, axmin, axmax, visible)
    type(graff_data), intent(in), target :: data
    real(kind=real64), intent(inout) :: axmin, axmax
    logical, intent(in) :: visible

    ! Autoscale the Y axis for a polar dataset.

    real(kind=real64) :: scale
    real(kind=real64), dimension(:), allocatable :: r, th, y, x
    type(graff_xydata), pointer :: xydata
    logical, dimension(:), allocatable :: mask
    real(kind=real64), pointer, dimension(:) :: par

    if (data%mode == 2) then
       scale = pi/180._real64
    else
       scale = 1._real64
    end if

    xydata => data%xydata
    par => pdefs%axrange(:,1)

    allocate(r(data%ndata), th(data%ndata), y(data%ndata), &
         & mask(data%ndata))

    if (visible) allocate(x(data%ndata))

    select case (data%type)
    case(:0)     ! Question about -4
       r = xydata%x
       th = xydata%y*scale
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       
    case(1)
       r = xydata%x
       th = (xydata%y-xydata%y_err(1,:))*scale
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       th = (xydata%y+xydata%y_err(1,:))*scale
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       
    case(2)
       r = xydata%x
       th = (xydata%y+xydata%y_err(1,:))*scale
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       th = (xydata%y+xydata%y_err(2,:))*scale
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       
    case(3)
       r = xydata%x-xydata%x_err(1,:)
       th = xydata%y*scale
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       r = xydata%x+xydata%x_err(1,:)
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       
    case(4)
       r = xydata%x-xydata%x_err(1,:)
       th = xydata%y*scale
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       r = xydata%x+xydata%x_err(2,:)
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       
    case(5)
       r = xydata%x-xydata%x_err(1,:)
       th = (xydata%y-xydata%y_err(1,:))*scale
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       r = xydata%x+xydata%x_err(1,:)
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       th = (xydata%y+xydata%y_err(1,:))*scale
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       r = xydata%x-xydata%x_err(1,:)
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       
    case(6)
       r = xydata%x-xydata%x_err(1,:)
       th = (xydata%y-xydata%y_err(1,:))*scale
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       r = xydata%x+xydata%x_err(1,:)
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       th = (xydata%y+xydata%y_err(2,:))*scale
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       r = xydata%x-xydata%x_err(1,:)
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       
    case(7)
       r = xydata%x-xydata%x_err(1,:)
       th = (xydata%y-xydata%y_err(1,:))*scale
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       r = xydata%x+xydata%x_err(2,:)
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       th = (xydata%y+xydata%y_err(1,:))*scale
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       r = xydata%x-xydata%x_err(1,:)
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       
    case(8)
       r = xydata%x-xydata%x_err(1,:)
       th = (xydata%y-xydata%y_err(1,:))*scale
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       r = xydata%x+xydata%x_err(2,:)
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       th = (xydata%y+xydata%y_err(2,:))*scale
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
       r = xydata%x-xydata%x_err(1,:)
       y = r * sin(th)
       if (visible) then
          x = r * cos(th)
          mask = ieee_is_finite(y) .and. &
               & x >= minval(par) .and. x <= maxval(par)
       else
          mask = ieee_is_finite(y)
       end if
       axmin = min(axmin, minval(y, mask))
       axmax = max(axmax, maxval(y, mask))
    end select

  end subroutine gr_autoscale_y_polar
end module gr_axis_autoscale
