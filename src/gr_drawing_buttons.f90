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

module gr_drawing_buttons
  ! Mouse button event handlers.

  use iso_fortran_env
  use iso_c_binding

  use gdk_events

  use gtk, only: GDK_BUTTON_PRESS, GDK_BUTTON_RELEASE, GDK_SHIFT_MASK, &
       & GDK_CONTROL_MASK, GTK_RESPONSE_YES, GTK_MESSAGE_QUESTION, &
       & GTK_BUTTONS_YES_NO

  use graff_types
  use graff_globals
  use gr_plot_tools
  use gr_text_widgets
  use gr_text_pick_widgets

  use gr_cb_common

  implicit none

  real(kind=real64), private :: point_x, point_y
  integer, private :: point_after = -1

contains

  subroutine gr_drawing_plot(fevent)
    type(gdkeventbutton), pointer :: fevent

    ! Actions on button press in drawing mode

    type(graff_data), pointer :: data

    data => pdefs%data(pdefs%cset)

    ! No action for functions or if editing disabled.
    if (.not. data%medit .or. data%type < 0 .or. data%type == 9) return

    select case (fevent%button)
    case(1)
       call gr_drawing_left(fevent, data)
    case(2)
       call gr_drawing_centre(fevent, data)
    case(3)
       call gr_drawing_right(fevent, data)
    end select

  end subroutine gr_drawing_plot

  subroutine gr_drawing_left(fevent, data)
    type(gdkeventbutton), pointer :: fevent
    type(graff_data) :: data

    ! Actions on Left button press/release

    real(kind=real64) :: dr0, dr1
    integer :: closest

    if (fevent%type ==  GDK_BUTTON_PRESS) then
       call gr_ds_device
       if (data%ndata == 0) then
          point_after = 0
       else if (fevent%state == GDK_SHIFT_MASK) then
          dr0 = sqrt((fevent%x-transient%x_dev(1))**2 + &
               & (fevent%y-transient%y_dev(1))**2)
          dr1 = sqrt((fevent%x-transient%x_dev(data%ndata))**2 + &
               & (fevent%y-transient%y_dev(data%ndata))**2)
          if (dr0 <= dr1) then
             point_after = 0
          else
             point_after = data%ndata
          end if
       else if (fevent%state == GDK_CONTROL_MASK) then
          dr0 = gr_dist_seg(transient%x_dev, transient%y_dev, &
               & fevent%x, fevent%y, closest)
          if (dr0 > 5.) then
             point_after = -1
          else
             point_after = closest
          end if
       else if (fevent%state == 0) then
          point_after = data%ndata
       else
          point_after = -1    ! Invalid modifiers
       end if
    else if (fevent%type ==  GDK_BUTTON_RELEASE .and. point_after >= 0) then
       if (iand(fevent%state, &
            & ior(GDK_CONTROL_MASK, GDK_SHIFT_MASK)) == 0) then
          call gr_plot_coords_d_w(fevent%x, fevent%y, point_x, point_y)
          call gr_point_add(data)
          call gr_plot_draw(.true.)
       end if
       point_after = -1
    end if
  end subroutine gr_drawing_left

  function gr_dist_seg(x_dev, y_dev, x, y, imin) result(dr)
    real(kind=c_double) :: dr
    real(kind=c_double), intent(in), dimension(:) :: x_dev, y_dev
    real(kind=c_double), intent(in) :: x, y
    integer, intent(out) :: imin

    ! Distance in device coordinates between a point and the nearest
    ! line segment

    real(kind=c_double) :: xl, xu, yl, yu, tmp, grad, yint, xp, yp, dx, dy
    real(kind=c_double), dimension(:), allocatable :: off

    integer :: i
    integer :: npts

    npts = size(x_dev)
    allocate(off(npts-1))

    do i = 1, npts-1
       xl = min(x_dev(i), x_dev(i+1))
       xu = max(x_dev(i), x_dev(i+1))
       if (xu-xl < 10.0) then
          tmp = (xl+xu)/2.0
          xl = tmp - 5.0
          xu = tmp + 5.0
       end if
       yl = min(y_dev(i), y_dev(i+1))
       yu = max(y_dev(i), y_dev(i+1))
       if (yu - yl < 10.0) then
          tmp = (yl+yu)/2.0
          yl = tmp - 5.0
          yu = tmp + 5.0
       end if
       if (x >= xl .and. x <= xu .and. y >= yl .and. y <= yu) then
          if (y_dev(i) == y_dev(i+1)) then
             off(i) = abs(y_dev(i) - y)
          else if (x_dev(i) == x_dev(i+1)) then
             off(i) = abs(x_dev(i) - x)

          else
             grad = (y_dev(i+1)-y_dev(i))/(x_dev(i+1)-x_dev(i))
             yint = y_dev(i) - grad*x_dev(i)
             yp = x*grad + yint
             xp = (y-yint)/grad
             dy = yp - y
             dx = xp - x
             off(i) = abs(dx*dy)/sqrt(dx**2+dy**2)
          end if
       else
          off(i) = huge(1._c_double)
       end if
    end do

    imin = minloc(off,1)
    dr = minval(off)
  end function gr_dist_seg

  subroutine gr_drawing_centre(fevent, data)
    type(gdkeventbutton), pointer :: fevent
    type(graff_data) :: data

    ! Actions on middle button press/release

    real(kind=real64), dimension(:), allocatable :: dr
    real(kind=real64) :: xnew, ynew, scale

    if (fevent%type ==  GDK_BUTTON_PRESS) then
       call gr_ds_device
       allocate(dr(data%ndata))

       dr = sqrt((transient%x_dev-fevent%x)**2 + &
            & (transient%y_dev-fevent%y)**2)

       if (minval(dr) > 5.) then
          point_after = -1
       else
          point_after = minloc(dr, 1)
       end if
    else if (fevent%type ==  GDK_BUTTON_RELEASE .and. point_after > 0) then
       if (iand(fevent%state, &
            & ior(GDK_CONTROL_MASK, GDK_SHIFT_MASK)) == 0) then

          call gr_plot_coords_d_w(fevent%x, fevent%y, xnew, ynew)
          if (data%mode /= 0) then
             if (data%mode == 2) then
                scale = 180._real64/pl_pi
             else
                scale = 1.0_real64
             end if
             data%xydata%x(point_after) = sqrt(xnew**2 + ynew**2)
             data%xydata%y(point_after) = atan2(ynew, xnew)*scale
          else
             data%xydata%x(point_after) = xnew
             data%xydata%y(point_after) = ynew
          end if
          call gr_plot_draw(.true.)
       end if
    end if
  end subroutine gr_drawing_centre

  subroutine gr_drawing_right(fevent, data)
    type(gdkeventbutton), pointer :: fevent
    type(graff_data) :: data

    ! Actions on right button press/release

    real(kind=real64), dimension(:), allocatable :: dr
    integer(kind=c_int) :: iresp

    if (fevent%type ==  GDK_BUTTON_PRESS) then
       call gr_ds_device
       allocate(dr(data%ndata))

       dr = sqrt((transient%x_dev-fevent%x)**2 + &
            & (transient%y_dev-fevent%y)**2)

       if (minval(dr) > 5.) then
          point_after = -1
       else
          point_after = minloc(dr, 1)
       end if
    else if (fevent%type ==  GDK_BUTTON_RELEASE .and. point_after > 0) then
       if (iand(fevent%state, &
            & ior(GDK_CONTROL_MASK, GDK_SHIFT_MASK)) == 0) then
          iresp = hl_gtk_message_dialog_show( &
               & ["DELETE DATA POINT              ", &
               &  "This will delete the data point", &
               &  "Do you wish to proceed?        "], &
               & GTK_BUTTONS_YES_NO, type=GTK_MESSAGE_QUESTION, &
               & parent=gr_window)
          if (iresp == GTK_RESPONSE_YES) then
             call gr_point_delete(data)
             call gr_plot_draw(.true.)
          end if
       end if
       point_after = -1
    end if
  end subroutine gr_drawing_right

  subroutine gr_point_add(data)
    type(graff_data) :: data

    ! Add a point to a dataset

    real(kind=real64), dimension(:), allocatable :: xtmp, ytmp
    real(kind=real64), dimension(:,:), allocatable :: xetmp, yetmp
    integer :: nxe, nye, i, j
    real(kind=real64) :: scale

    if (point_after == -1) return

    nxe = nx_errors(data%type)
    nye = ny_errors(data%type)

    allocate(xtmp(data%ndata+1),ytmp(data%ndata+1))
    if (nxe > 0) allocate(xetmp(nxe,data%ndata+1))
    if (nye > 0) allocate(yetmp(nye,data%ndata+1))
    
    if (data%mode == 2) then
       scale = 180._real64/pl_pi
    else
       scale = 1.0_real64
    end if
    j = 1
    do i = 1, data%ndata+1
       if (i == point_after+1) then
          if (data%mode == 0) then
             xtmp(i) = point_x
             ytmp(i) = point_y
          else
             xtmp(i) = sqrt(point_x**2 + point_y**2)
             ytmp(i) = atan2(point_y, point_x)*scale
          end if
          if (nxe > 0) xetmp(:,i) = 0._real64
          if (nye > 0) yetmp(:,i) = 0._real64
       else
          xtmp(i) = data%xydata%x(j)
          ytmp(i) = data%xydata%y(j)
          if (nxe > 0) xetmp(:,i) = data%xydata%x_err(:,j)
          if (nye > 0) yetmp(:,i) = data%xydata%y_err(:,j)
          j = j+1
       end if
    end do

    if (allocated(data%xydata%x)) deallocate(data%xydata%x, data%xydata%y)
    if (allocated(data%xydata%x_err)) deallocate(data%xydata%x_err)
    if (allocated(data%xydata%y_err)) deallocate(data%xydata%y_err)
    
    call move_alloc(xtmp, data%xydata%x)
    call move_alloc(ytmp, data%xydata%y)
    if (nxe > 0) call move_alloc(xetmp, data%xydata%x_err)
    if (nxe > 0) call move_alloc(yetmp, data%xydata%y_err)

    data%ndata = data%ndata + 1
  end subroutine gr_point_add

  subroutine gr_point_delete(data)
    type(graff_data) :: data

    ! Delete a point from a dataset

    real(kind=real64), dimension(:), allocatable :: xtmp, ytmp
    real(kind=real64), dimension(:,:), allocatable :: xetmp, yetmp
    integer :: nxe, nye, i, j

    if (point_after <= 0) return
    if (data%ndata == 1) then
       if (allocated(data%xydata%x)) deallocate(data%xydata%x, &
            & data%xydata%y)
       if (allocated(data%xydata%x_err)) deallocate(data%xydata%x_err)
       if (allocated(data%xydata%y_err)) deallocate(data%xydata%y_err)
       data%ndata = 0
    else
       nxe = nx_errors(data%type)
       nye = ny_errors(data%type)

       allocate(xtmp(data%ndata+1),ytmp(data%ndata-1))
       if (nxe > 0) allocate(xetmp(nxe,data%ndata-1))
       if (nye > 0) allocate(yetmp(nye,data%ndata-1))

       j = 1
       do i = 1, data%ndata
          if (i == point_after) cycle
          xtmp(j) = data%xydata%x(i)
          ytmp(j) = data%xydata%y(i)
          if (nxe > 0) xetmp(:,j) = data%xydata%x_err(:,i)
          if (nye > 0) yetmp(:,j) = data%xydata%y_err(:,i)
          j = j+1
       end do

       if (allocated(data%xydata%x)) deallocate(data%xydata%x, data%xydata%y)
       if (allocated(data%xydata%x_err)) deallocate(data%xydata%x_err)
       if (allocated(data%xydata%y_err)) deallocate(data%xydata%y_err)

       call move_alloc(xtmp, data%xydata%x)
       call move_alloc(ytmp, data%xydata%y)
       if (nxe > 0) call move_alloc(xetmp, data%xydata%x_err)
       if (nxe > 0) call move_alloc(yetmp, data%xydata%y_err)

       data%ndata = data%ndata - 1
    end if
  end subroutine gr_point_delete

  subroutine gr_drawing_text(fevent)
    type(gdkeventbutton), pointer :: fevent

    ! Actions on a button event in text mode

    real(kind=real64) ::  dr, dr1, xd, yd
    integer :: i, iclose
    type(graff_text), pointer :: text
    integer(kind=c_int) :: iresp

    if (fevent%button == 1) then
       if (fevent%type ==  GDK_BUTTON_RELEASE) return
       call gr_text_menu(x=fevent%x, y=fevent%y)
    else
       if (pdefs%ntext == 0) return
       dr = huge(1._real64)
       do i = 1, pdefs%ntext
          text => pdefs%text(i)
          select case (text%norm)
          case(0)
             call gr_plot_coords_w_d(text%x, text%y, xd, yd, &
                  & y_axis=text%axis+1)
          case(1)
             call gr_plot_coords_n_d(text%x, text%y, xd, yd)
          case(2)
             call gr_plot_coords_v_d(text%x, text%y, xd, yd)
          end select

          dr1 = sqrt((xd-fevent%x)**2 + (yd-fevent%y)**2)
          if (dr1 < dr) then
             dr = dr1
             iclose = i
          end if
       end do

       if (fevent%type == GDK_BUTTON_PRESS) then
          select case  (fevent%button)
          case(2)
             if (dr <= 5.) then
                call gr_text_menu(index=iclose)
             else
                iclose = gr_text_pick()
                if (iclose >= 1) call gr_text_menu(index=iclose)
             end if
          case(3)
             if (dr <= 5.) then
                iresp = hl_gtk_message_dialog_show( &
                     & ["DELETE TEXT STRING         ", &
                     &  "This will delete the string", &
                     &  "Do you wish to proceed?    "], &
                     & GTK_BUTTONS_YES_NO, type=GTK_MESSAGE_QUESTION, &
                     & parent=gr_window)
                if (iresp == GTK_RESPONSE_YES) then
                   call gr_delete_text(iclose)
                   call gr_plot_draw(.true.)
                end if
             end if
          end select
       end if
    end if
  end subroutine gr_drawing_text
end module gr_drawing_buttons
