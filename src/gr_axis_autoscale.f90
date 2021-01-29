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
          if (.not. allocated(data%xydata)) cycle
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
       else if (data%y_axis == axis-2) then
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
    
    logical, dimension(:), allocatable :: mask
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
          mask = ieee_is_finite(data%xydata(1,:)) .and. &
               & data%xydata(2,:) <= maxval(par)  .and. &
               & data%xydata(2,:) >= minval(par)
       else
          mask = ieee_is_finite(data%xydata(1,:))
       end if
       if (pdefs%axtype(1) == 1) &
            & mask = mask .and. data%xydata(1,:) > 0._real64
    end if
    
    select case (data%type)
    case(0:2)
       axmin = min(axmin, minval(data%xydata(1,:), mask))
       axmax = max(axmax, maxval(data%xydata(1,:), mask))
    case(3,5,6)
       axmin = min(axmin, minval(data%xydata(1,:)-data%xydata(3,:), mask))
       axmax = max(axmax, maxval(data%xydata(1,:)+data%xydata(3,:), mask))
    case(4,7,8)
       axmin = min(axmin, minval(data%xydata(1,:)-data%xydata(3,:), mask))
       axmax = max(axmax, maxval(data%xydata(1,:)+data%xydata(4,:), mask))
    case(9)
       axmin = min(axmin, minval(data%zdata%x))
       axmax = max(axmax, maxval(data%zdata%x))
    case(-1, -4)
       if (data%funct%range(1,1) /= data%funct%range(2,1)) then
          axmin = min(axmin, minval(data%funct%range(:,1)))
          axmax = max(axmax, maxval(data%funct%range(:,1)))
       end if
    case(-2, -3)
       axmin = min(axmin, minval(data%xydata(1,:), mask))
       axmax = max(axmax, maxval(data%xydata(1,:), mask))
    end select
  end subroutine gr_autoscale_x_rect

  subroutine gr_autoscale_y_rect(data, axmin, axmax, visible)
    type(graff_data), intent(in) :: data
    real(kind=real64), intent(inout) :: axmin, axmax
    logical, intent(in) :: visible
    
    logical, dimension(:), allocatable :: mask
    real(kind=real64), pointer, dimension(:) :: par

    ! Auto scale the Y axis for a rectangular coordinate DS

    if ((data%type >= 0 .and. data%type <= 8) .or. &
         & data%type == -1 .or. data%type == -3) then
       par => pdefs%axrange(:,1)
       allocate(mask(data%ndata))
       if (visible) then
          mask = ieee_is_finite(data%xydata(2,:)) .and. &
               & data%xydata(1,:) <= maxval(par)  .and. &
               & data%xydata(1,:) >= minval(par)
       else
          mask = ieee_is_finite(data%xydata(2,:))
       end if
       
       if (ieee_is_finite(data%min_val)) &
            & mask = mask .and. data%xydata(2,:) >= data%min_val
       if (ieee_is_finite(data%max_val)) &
            & mask =  mask .and. data%xydata(2,:) <= data%max_val
       if (pdefs%axtype(data%y_axis+2) == 1) &
            & mask = mask .and. data%xydata(2,:) > 0._real64
         
    end if
    
    select case (data%type)
    case(0,3,4)
       axmin = min(axmin, minval(data%xydata(2,:), mask))
       axmax = max(axmax, maxval(data%xydata(2,:), mask))
    case(1)
       axmin = min(axmin, minval(data%xydata(2,:)-data%xydata(3,:), mask))
       axmax = max(axmax, maxval(data%xydata(2,:)+data%xydata(3,:), mask))
    case(5)
       axmin = min(axmin, minval(data%xydata(2,:)-data%xydata(4,:), mask))
       axmax = max(axmax, maxval(data%xydata(2,:)+data%xydata(4,:), mask))
    case(7)
       axmin = min(axmin, minval(data%xydata(2,:)-data%xydata(5,:), mask))
       axmax = max(axmax, maxval(data%xydata(2,:)+data%xydata(5,:), mask))
    case(2)
       axmin = min(axmin, minval(data%xydata(2,:)-data%xydata(3,:), mask))
       axmax = max(axmax, maxval(data%xydata(2,:)+data%xydata(4,:), mask))
    case(6)
       axmin = min(axmin, minval(data%xydata(2,:)-data%xydata(4,:), mask))
       axmax = max(axmax, maxval(data%xydata(2,:)+data%xydata(5,:), mask))
    case(8)
       axmin = min(axmin, minval(data%xydata(2,:)-data%xydata(5,:), mask))
       axmax = max(axmax, maxval(data%xydata(2,:)+data%xydata(6,:), mask))
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
       axmin = min(axmin, minval(data%xydata(2,:), mask))
       axmax = max(axmax, maxval(data%xydata(2,:), mask))
    end select

     
  end subroutine gr_autoscale_y_rect

  subroutine gr_autoscale_x_polar(data, axmin, axmax, visible)
    type(graff_data), intent(in), target :: data
    real(kind=real64), intent(inout) :: axmin, axmax
    logical, intent(in) :: visible

    ! Autoscale the X axis for a polar dataset.

    real(kind=real64) :: scale
    real(kind=real64), dimension(:), allocatable :: r, th, x, y
    real(kind=real64), dimension(:,:), pointer :: xydata
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
    case(:0)
       r = xydata(1,:)
       th = xydata(2,:)*scale
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
       r = xydata(1,:)
       th = (xydata(2,:)-xydata(3,:))*scale
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
       th = (xydata(2,:)+xydata(3,:))*scale
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
       r = xydata(1,:)
       th = (xydata(2,:)+xydata(3,:))*scale
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
       th = (xydata(2,:)+xydata(4,:))*scale
       x = r * cos(th)
       axmin = min(axmin, minval(x, mask))
       axmax = max(axmax, maxval(x, mask))
    case(3)
       r = xydata(1,:)-xydata(3,:)
       th = xydata(2,:)*scale
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
       r = xydata(1,:)+xydata(3,:)
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
       r = xydata(1,:)-xydata(3,:)
       th = xydata(2,:)*scale
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
       r = xydata(1,:)+xydata(4,:)
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
       r = xydata(1,:)-xydata(3,:)
       th = (xydata(2,:)-xydata(4,:))*scale
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
       r = xydata(1,:)+xydata(3,:)
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
       th = (xydata(2,:)+xydata(4,:))*scale
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
       r = xydata(1,:)-xydata(3,:)
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
       r = xydata(1,:)-xydata(3,:)
       th = (xydata(2,:)-xydata(4,:))*scale
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
       r = xydata(1,:)+xydata(3,:)
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
       th = (xydata(2,:)+xydata(5,:))*scale
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
       r = xydata(1,:)-xydata(3,:)
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
       r = xydata(1,:)-xydata(3,:)
       th = (xydata(2,:)-xydata(5,:))*scale
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
       r = xydata(1,:)+xydata(4,:)
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
       th = (xydata(2,:)+xydata(5,:))*scale
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
       r = xydata(1,:)-xydata(3,:)
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
       r = xydata(1,:)-xydata(3,:)
       th = (xydata(2,:)-xydata(5,:))*scale
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
       r = xydata(1,:)+xydata(4,:)
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
       th = (xydata(2,:)+xydata(6,:))*scale
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
       r = xydata(1,:)-xydata(3,:)
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
    real(kind=real64), dimension(:,:), pointer :: xydata
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
    case(:0)
       r = xydata(1,:)
       th = xydata(2,:)*scale
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
       r = xydata(1,:)
       th = (xydata(2,:)-xydata(3,:))*scale
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
       th = (xydata(2,:)+xydata(3,:))*scale
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
       r = xydata(1,:)
       th = (xydata(2,:)+xydata(3,:))*scale
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
       th = (xydata(2,:)+xydata(4,:))*scale
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
       r = xydata(1,:)-xydata(3,:)
       th = xydata(2,:)*scale
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
       r = xydata(1,:)+xydata(3,:)
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
       r = xydata(1,:)-xydata(3,:)
       th = xydata(2,:)*scale
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
       r = xydata(1,:)+xydata(4,:)
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
       r = xydata(1,:)-xydata(3,:)
       th = (xydata(2,:)-xydata(4,:))*scale
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
       r = xydata(1,:)+xydata(3,:)
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
       th = (xydata(2,:)+xydata(4,:))*scale
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
       r = xydata(1,:)-xydata(3,:)
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
       r = xydata(1,:)-xydata(3,:)
       th = (xydata(2,:)-xydata(4,:))*scale
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
       r = xydata(1,:)+xydata(3,:)
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
       th = (xydata(2,:)+xydata(5,:))*scale
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
       r = xydata(1,:)-xydata(3,:)
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
       r = xydata(1,:)-xydata(3,:)
       th = (xydata(2,:)-xydata(5,:))*scale
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
       r = xydata(1,:)+xydata(4,:)
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
       th = (xydata(2,:)+xydata(5,:))*scale
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
       r = xydata(1,:)-xydata(3,:)
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
       r = xydata(1,:)-xydata(3,:)
       th = (xydata(2,:)-xydata(5,:))*scale
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
       r = xydata(1,:)+xydata(4,:)
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
       th = (xydata(2,:)+xydata(6,:))*scale
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
       r = xydata(1,:)-xydata(3,:)
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
