module gr_drawing_buttons
  use iso_fortran_env
  use iso_c_binding

  use gdk_events

  use gtk, only: GDK_BUTTON_PRESS, GDK_BUTTON_RELEASE, GTK_MESSAGE_QUESTION, &
       & GTK_BUTTONS_YES_NO, GTK_RESPONSE_YES, GDK_SHIFT_MASK, GDK_CONTROL_MASK


  use graff_types
  use graff_globals
  use gr_plot_tools
  use gr_text_widgets
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
    real(kind=real64), dimension(:), allocatable :: drc

    if (fevent%type ==  GDK_BUTTON_PRESS) then
       call gr_ds_device
       call gr_plot_coords_d_w(fevent%x, fevent%y, point_x, point_y)
       if (data%ndata == 0) then
          point_after = 0
       else if (fevent%state == GDK_SHIFT_MASK) then
          dr0 = sqrt((fevent%x-pdefs%transient%x_dev(1))**2 + &
               & (fevent%y-pdefs%transient%y_dev(1))**2)
          dr1 = sqrt((fevent%x-pdefs%transient%x_dev(data%ndata))**2 + &
               & (fevent%y-pdefs%transient%y_dev(data%ndata))**2)
          if (dr0 <= dr1) then
             point_after = 0
          else
             point_after = data%ndata
          end if
       else if (fevent%state == GDK_CONTROL_MASK) then
          dr0 = sqrt((fevent%x-pdefs%transient%x_dev(1))**2 + &
               & (fevent%y-pdefs%transient%y_dev(1))**2)
          dr1 = sqrt((fevent%x-pdefs%transient%x_dev(data%ndata))**2 + &
               & (fevent%y-pdefs%transient%y_dev(data%ndata))**2)
          if (dr0 <= dr1) then
             point_after = 0
          else
             point_after = data%ndata
          end if
          if (data%ndata >= 2) then
             allocate(drc(data%ndata-1))
             drc =  sqrt((fevent%x - &
                  & (pdefs%transient%x_dev(:data%ndata-1) + &
                  & pdefs%transient%x_dev(2:))/2._real64)**2 + &
                  & (fevent%y - &
                  & (pdefs%transient%y_dev(:data%ndata-1) + &
                  &  pdefs%transient%y_dev(2:))/2._real64)**2)
             if (minval(drc) < min(dr0, dr1)) point_after = minloc(drc,1)
          end if
       else if (fevent%state == 0) then
          point_after = data%ndata
       else
          point_after = -1    ! Invalid modifiers
       end if
    else if (fevent%type ==  GDK_BUTTON_RELEASE .and. point_after >= 0) then
       if (iand(fevent%state, &
            & ior(GDK_CONTROL_MASK, GDK_SHIFT_MASK)) == 0) then
          call gr_point_add(data)
          call gr_plot_draw(.true.)
       end if
       point_after = -1
    end if
  end subroutine gr_drawing_left

  subroutine gr_drawing_centre(fevent, data)
    type(gdkeventbutton), pointer :: fevent
    type(graff_data) :: data

    ! Actions on middle button press/release

    real(kind=real64), dimension(:), allocatable :: dr
    real(kind=real64) :: xnew, ynew, scale

    if (fevent%type ==  GDK_BUTTON_PRESS) then
       call gr_ds_device
       allocate(dr(data%ndata))

       dr = sqrt((pdefs%transient%x_dev-fevent%x)**2 + &
            & (pdefs%transient%y_dev-fevent%y)**2)

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
             data%xydata(1,point_after) = sqrt(xnew**2 + ynew**2)
             data%xydata(2,point_after) = atan2(ynew, xnew)*scale
          else
             data%xydata(1,point_after) = xnew
             data%xydata(2,point_after) = ynew
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

       dr = sqrt((pdefs%transient%x_dev-fevent%x)**2 + &
            & (pdefs%transient%y_dev-fevent%y)**2)

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

    real(kind=real64), dimension(:,:), allocatable :: xytmp
    integer :: n1, i, j
    real(kind=real64) :: scale

    if (point_after == -1) return

    select case (data%type)
    case(0)
       n1 = 2
    case(1,3)
       n1 = 3
    case(2,4,5)
       n1 = 4
    case(6,7)
       n1 = 5
    case(8)
       n1 = 6
    end select
    allocate(xytmp(n1, data%ndata+1))

    if (data%mode == 2) then
       scale = 180._real64/pl_pi
    else
       scale = 1.0_real64
    end if
    j = 1
    do i = 1, data%ndata+1
       if (i == point_after+1) then
          if (data%mode == 0) then
             xytmp(:2, i) = [point_x, point_y]
          else
             xytmp(:2, i) = [sqrt(point_x**2 + point_y**2), &
                  & atan2(point_y, point_x)*scale]
          end if
          if (n1 > 2) xytmp(3:,i) = 0._real64
       else
          xytmp(:,i) = data%xydata(:,j)
          j = j+1
       end if
    end do

    if (allocated(data%xydata)) deallocate(data%xydata)
    call move_alloc(xytmp, data%xydata)
    data%ndata = data%ndata + 1
  end subroutine gr_point_add

  subroutine gr_point_delete(data)
    type(graff_data) :: data

    ! Delete a point from a dataset

    real(kind=real64), dimension(:,:), allocatable :: xytmp
    integer :: n1, i, j

    if (point_after <= 0) return
    if (data%ndata == 1) then
       if (allocated(data%xydata)) deallocate(data%xydata)
       data%ndata = 0
    else
       select case (data%type)
       case(0)
          n1 = 2
       case(1,3)
          n1 = 3
       case(2,4,5)
          n1 = 4
       case(6,7)
          n1 = 5
       case(8)
          n1 = 6
       end select
       allocate(xytmp(n1, data%ndata-1))

       j = 1
       do i = 1, data%ndata
          if (i == point_after) cycle
          xytmp(:,j) = data%xydata(:,i)
          j = j+1
       end do
       if (allocated(data%xydata)) deallocate(data%xydata)
       call move_alloc(xytmp, data%xydata)
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

       if (fevent%type == GDK_BUTTON_PRESS .and. dr <= 5.) then
          select case  (fevent%button)
          case(2)
             call gr_text_menu(index=iclose)
          case(3)
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
          end select
       end if
    end if
  end subroutine gr_drawing_text
end module gr_drawing_buttons
