module gr_msg
  use iso_c_binding
  use iso_fortran_env

  use gtk_hl
  use gtk_sup

  use gtk, only: GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, GTK_MESSAGE_ERROR, &
       & GTK_MESSAGE_INFO

  use graff_globals

  implicit none

  interface gr_message
     module procedure gr_message_1, gr_message_n
  end interface gr_message

  private :: is_terminal

contains
  subroutine gr_message_1(message, type)
    character(len=*), intent(in) :: message
    integer(kind=c_int), intent(in), optional :: type

    ! Error message (1 line)

    integer(kind=c_int) :: msgtype, iresp
    character(len=len(message)) :: hdr

    if (present(type)) then
       msgtype = type
    else
       msgtype = GTK_MESSAGE_WARNING
    end if

    if (c_associated(gr_infobar) .and. msgtype /= GTK_MESSAGE_INFO) then
       call hl_gtk_info_bar_message(gr_infobar, trim(message)//c_null_char, &
            & type=msgtype)
    else
       select case (msgtype)
       case(GTK_MESSAGE_ERROR)
          hdr = "GRAFFER"
          iresp = hl_gtk_message_dialog_show([hdr, message], &
               & GTK_BUTTONS_OK, type=msgtype)
       case(GTK_MESSAGE_WARNING)
          if (is_terminal()) then
             write(error_unit, "(A)") message
          else
             hdr = "GRAFFER"
             iresp = hl_gtk_message_dialog_show([hdr, message], &
                  & GTK_BUTTONS_OK, type=msgtype)
          end if
       case(GTK_MESSAGE_INFO)
          write(error_unit, "(A)") message
       end select
    end if
  end subroutine gr_message_1

  subroutine gr_message_n(message, type)
    character(len=*), dimension(:), intent(in) :: message
    integer(kind=c_int), intent(in), optional :: type

    ! Error message (>1 line)

    integer(kind=c_int) :: msgtype, iresp
    character(len=1), dimension(:), allocatable :: cmsg
    character(len=len(message)) :: hdr

    if (present(type)) then
       msgtype = type
    else
       msgtype = GTK_MESSAGE_WARNING
    end if

    if (c_associated(gr_infobar) .and. msgtype /= GTK_MESSAGE_INFO) then
       call f_c_string(message, cmsg)
       call hl_gtk_info_bar_message(gr_infobar, cmsg, &
            & type=msgtype)
    else
       select case (msgtype)
       case(GTK_MESSAGE_ERROR)
          hdr = "GRAFFER"
          iresp = hl_gtk_message_dialog_show([hdr, message], &
               & GTK_BUTTONS_OK, type=msgtype)
       case(GTK_MESSAGE_WARNING)
          if (is_terminal()) then
             write(error_unit, "(A)") message
          else
             hdr = "GRAFFER"
             iresp = hl_gtk_message_dialog_show([hdr, message], &
                  & GTK_BUTTONS_OK, type=msgtype)
          end if
       case(GTK_MESSAGE_INFO)
          write(error_unit, "(A)") message
       end select
    end if
  end subroutine gr_message_n

  function is_terminal()
    logical :: is_terminal

    ! Determine if the error unit is connected to a terminal
    ! This is a rough & ready heuristic that may or may not work on systems
    ! other than Linux

    character(len=120) :: name

    inquire(unit=error_unit, name=name)
    is_terminal = name /= 'stderr'

  end function is_terminal
end module gr_msg
