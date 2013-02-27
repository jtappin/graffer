module gr_axis_autoscale
  use iso_c_binding
  use iso_fortran_env

  use graff_types
  use graff_globals
  use gr_eval
  use gr_cb_common

  use plplot, only: pi=>pl_pi

  implicit none

contains
  subroutine gr_autoscale(axis, shrink)
    integer :: axis
    logical, optional :: shrink

    ! Auto scale an Axis

    real(kind=real64) :: axmin, axmax
    type(graff_data), pointer :: data
    integer :: i, status
    logical :: ishrink
    character(len=32) :: text

    if (axis == 3 .and. .not. pdefs%y_right) return

    if (present(shrink)) then
       ishrink = shrink
    else
       ishrink = .false.
    end if

    axmin = huge(1._real64)
    axmax = -huge(1._real64)

    if (axis == 1) then
       do i = 1, pdefs%nsets
          data => pdefs%data(i)
          if (data%type < 0) then
             status = gr_evaluate(dataset=data)
          else
             status = 0
          end if
          if (status /= 0 .or. .not. allocated(data%xydata)) cycle

          if (data%mode == 0) then
             call gr_autoscale_x_rect(data, axmin, axmax)
          else
             call gr_autoscale_x_polar(data, axmin, axmax)
          end if
       end do
    else
       do i = 1, pdefs%nsets
          data => pdefs%data(i)
          if (pdefs%y_right .and. data%y_axis /= axis-2) cycle
          if (data%type < 0) then
             status = gr_evaluate(dataset=data)
          else
             status = 0
          end if
          if (status /= 0 .or. .not. allocated(data%xydata)) cycle

          if (data%mode == 0) then
             call gr_autoscale_y_rect(data, axmin, axmax)
          else
             call gr_autoscale_y_polar(data, axmin, axmax)
          end if
       end do
    end if

    if (axmin >= axmax) return

    if (ishrink) then
       pdefs%axrange(:,axis) = [axmin, axmax]
    else
       pdefs%axrange(:,axis) = [min(axmin,pdefs%axrange(1,axis)), &
            & max(axmax,pdefs%axrange(2,axis))]
    end if

    do i = 1, 2
       write(text, "(g0.5)") pdefs%axrange(i,axis)
       call gtk_entry_set_text(rbox(i, axis), adjustl(trim(text))//c_null_char)
    end do

    call gtk_widget_set_sensitive(log_chb(axis), &
         & f_c_logical(minval(pdefs%axrange(:,axis)) > 0.))

    call gr_plot_draw(.true.)
  end subroutine gr_autoscale

  subroutine gr_autoscale_x_rect(data, axmin, axmax)
    type(graff_data), intent(in) :: data
    real(kind=real64), intent(inout) :: axmin, axmax

    ! Auto scale the X axis for a rectangular coordinate DS

    select case (data%type)
    case(0:2)
       axmin = min(axmin, minval(data%xydata(1,:)))
       axmax = max(axmax, maxval(data%xydata(1,:)))
    case(3,5,6)
       axmin = min(axmin, minval(data%xydata(1,:)-data%xydata(3,:)))
       axmax = max(axmax, maxval(data%xydata(1,:)+data%xydata(3,:)))
    case(4,7,8)
       axmin = min(axmin, minval(data%xydata(1,:)-data%xydata(3,:)))
       axmax = max(axmax, maxval(data%xydata(1,:)+data%xydata(4,:)))
    case(9)
       axmin = min(axmin, minval(data%zdata%x))
       axmax = max(axmax, maxval(data%zdata%x))
    case(-1, -4)
       if (data%funct%range(1,1) /= data%funct%range(2,1)) then
          axmin = min(axmin, minval(data%funct%range(:,1)))
          axmax = max(axmax, maxval(data%funct%range(:,1)))
       end if
    case(-2, -3)
       axmin = min(axmin, minval(data%xydata(1,:)))
       axmax = max(axmax, maxval(data%xydata(1,:)))
    end select
  end subroutine gr_autoscale_x_rect

  subroutine gr_autoscale_y_rect(data, axmin, axmax)
    type(graff_data), intent(in) :: data
    real(kind=real64), intent(inout) :: axmin, axmax

    ! Auto scale the Y axis for a rectangular coordinate DS

    select case (data%type)
    case(0,3,4)
       axmin = min(axmin, minval(data%xydata(2,:)))
       axmax = max(axmax, maxval(data%xydata(2,:)))
    case(1)
       axmin = min(axmin, minval(data%xydata(2,:)-data%xydata(3,:)))
       axmax = max(axmax, maxval(data%xydata(2,:)+data%xydata(3,:)))
    case(5)
       axmin = min(axmin, minval(data%xydata(2,:)-data%xydata(4,:)))
       axmax = max(axmax, maxval(data%xydata(2,:)+data%xydata(4,:)))
    case(7)
       axmin = min(axmin, minval(data%xydata(2,:)-data%xydata(5,:)))
       axmax = max(axmax, maxval(data%xydata(2,:)+data%xydata(5,:)))
    case(2)
       axmin = min(axmin, minval(data%xydata(2,:)-data%xydata(3,:)))
       axmax = max(axmax, maxval(data%xydata(2,:)+data%xydata(4,:)))
    case(6)
       axmin = min(axmin, minval(data%xydata(2,:)-data%xydata(4,:)))
       axmax = max(axmax, maxval(data%xydata(2,:)+data%xydata(5,:)))
    case(8)
       axmin = min(axmin, minval(data%xydata(2,:)-data%xydata(5,:)))
       axmax = max(axmax, maxval(data%xydata(2,:)+data%xydata(6,:)))
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
       axmin = min(axmin, minval(data%xydata(2,:)))
       axmax = max(axmax, maxval(data%xydata(2,:)))
    end select
  end subroutine gr_autoscale_y_rect

  subroutine gr_autoscale_x_polar(data, axmin, axmax)
    type(graff_data), intent(in), target :: data
    real(kind=real64), intent(inout) :: axmin, axmax

    ! Autoscale the X axis for a polar dataset.

    real(kind=real64) :: scale
    real(kind=real64), dimension(:), allocatable :: r, th, x
    real(kind=real64), dimension(:,:), pointer :: xydata

    if (data%mode == 2) then
       scale = pi/180._real64
    else
       scale = 1._real64
    end if

    xydata => data%xydata

    allocate(r(data%ndata), th(data%ndata), x(data%ndata))
    select case (data%type)
    case(:0)
       r = xydata(1,:)
       th = xydata(2,:)*scale
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
    case(1)
       r = xydata(1,:)
       th = (xydata(2,:)-xydata(3,:))*scale
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
       th = (xydata(2,:)+xydata(3,:))*scale
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
    case(2)
       r = xydata(1,:)
       th = (xydata(2,:)+xydata(3,:))*scale
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
       th = (xydata(2,:)+xydata(4,:))*scale
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
    case(3)
       r = xydata(1,:)-xydata(3,:)
       th = xydata(2,:)*scale
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
       r = xydata(1,:)+xydata(3,:)
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
    case(4)
       r = xydata(1,:)-xydata(3,:)
       th = xydata(2,:)*scale
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
       r = xydata(1,:)+xydata(4,:)
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
    case(5)
       r = xydata(1,:)-xydata(3,:)
       th = (xydata(2,:)-xydata(4,:))*scale
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
       r = xydata(1,:)+xydata(3,:)
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
       th = (xydata(2,:)+xydata(4,:))*scale
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
       r = xydata(1,:)-xydata(3,:)
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
    case(6)
       r = xydata(1,:)-xydata(3,:)
       th = (xydata(2,:)-xydata(4,:))*scale
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
       r = xydata(1,:)+xydata(3,:)
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
       th = (xydata(2,:)+xydata(5,:))*scale
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
       r = xydata(1,:)-xydata(3,:)
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
    case(7)
       r = xydata(1,:)-xydata(3,:)
       th = (xydata(2,:)-xydata(5,:))*scale
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
       r = xydata(1,:)+xydata(4,:)
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
       th = (xydata(2,:)+xydata(5,:))*scale
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
       r = xydata(1,:)-xydata(3,:)
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
    case(8)
       r = xydata(1,:)-xydata(3,:)
       th = (xydata(2,:)-xydata(5,:))*scale
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
       r = xydata(1,:)+xydata(4,:)
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
       th = (xydata(2,:)+xydata(6,:))*scale
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
       r = xydata(1,:)-xydata(3,:)
       x = r * cos(th)
       axmin = min(axmin, minval(x))
       axmax = max(axmax, maxval(x))
    end select

  end subroutine gr_autoscale_x_polar

  subroutine gr_autoscale_y_polar(data, axmin, axmax)
    type(graff_data), intent(in), target :: data
    real(kind=real64), intent(inout) :: axmin, axmax

    ! Autoscale the Y axis for a polar dataset.

    real(kind=real64) :: scale
    real(kind=real64), dimension(:), allocatable :: r, th, y
    real(kind=real64), dimension(:,:), pointer :: xydata

    if (data%mode == 2) then
       scale = pi/180._real64
    else
       scale = 1._real64
    end if

    xydata => data%xydata

    allocate(r(data%ndata), th(data%ndata), y(data%ndata))
    select case (data%type)
    case(:0)
       r = xydata(1,:)
       th = xydata(2,:)*scale
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
    case(1)
       r = xydata(1,:)
       th = (xydata(2,:)-xydata(3,:))*scale
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
       th = (xydata(2,:)+xydata(3,:))*scale
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
    case(2)
       r = xydata(1,:)
       th = (xydata(2,:)+xydata(3,:))*scale
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
       th = (xydata(2,:)+xydata(4,:))*scale
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
    case(3)
       r = xydata(1,:)-xydata(3,:)
       th = xydata(2,:)*scale
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
       r = xydata(1,:)+xydata(3,:)
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
    case(4)
       r = xydata(1,:)-xydata(3,:)
       th = xydata(2,:)*scale
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
       r = xydata(1,:)+xydata(4,:)
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
    case(5)
       r = xydata(1,:)-xydata(3,:)
       th = (xydata(2,:)-xydata(4,:))*scale
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
       r = xydata(1,:)+xydata(3,:)
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
       th = (xydata(2,:)+xydata(4,:))*scale
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
       r = xydata(1,:)-xydata(3,:)
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
    case(6)
       r = xydata(1,:)-xydata(3,:)
       th = (xydata(2,:)-xydata(4,:))*scale
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
       r = xydata(1,:)+xydata(3,:)
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
       th = (xydata(2,:)+xydata(5,:))*scale
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
       r = xydata(1,:)-xydata(3,:)
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
    case(7)
       r = xydata(1,:)-xydata(3,:)
       th = (xydata(2,:)-xydata(5,:))*scale
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
       r = xydata(1,:)+xydata(4,:)
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
       th = (xydata(2,:)+xydata(5,:))*scale
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
       r = xydata(1,:)-xydata(3,:)
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
    case(8)
       r = xydata(1,:)-xydata(3,:)
       th = (xydata(2,:)-xydata(5,:))*scale
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
       r = xydata(1,:)+xydata(4,:)
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
       th = (xydata(2,:)+xydata(6,:))*scale
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
       r = xydata(1,:)-xydata(3,:)
       y = r * sin(th)
       axmin = min(axmin, minval(y))
       axmax = max(axmax, maxval(y))
    end select

  end subroutine gr_autoscale_y_polar
end module gr_axis_autoscale
