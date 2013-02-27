module gr_general_comment_widgets
  use iso_c_binding
  use iso_fortran_env

  use gtk_hl
  use gtk_sup

  use graff_types
  use graff_globals

  implicit none

  type(c_ptr), private :: comm_window, comm_view

contains
  subroutine gr_comment_menu

    ! Add comments to the file.

    type(c_ptr) :: base, junk, jb, sbox
    logical, dimension(2), target :: iapply = [.false., .true.]

    comm_window = hl_gtk_window_new("Remarks"//c_null_char, &
         & destroy=c_funloc(gr_comment_quit), data_destroy=c_loc(iapply(1)), &
         & parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(comm_window, base)

    comm_view = hl_gtk_text_view_new(sbox, ssize=[600_c_int, 300_c_int])
    if (allocated(pdefs%remarks)) call hl_gtk_text_view_insert(comm_view, &
         & pdefs%remarks, replace=TRUE)

    call hl_gtk_box_pack(base, sbox)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    junk = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_comment_quit), data=c_loc(iapply(2)))
    call hl_gtk_box_pack(jb, junk)

    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_comment_quit), data=c_loc(iapply(1)))
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(comm_window)

  end subroutine gr_comment_menu

  recursive subroutine gr_comment_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Quit adding comments.

    logical, pointer :: apply
    integer(kind=c_int) :: nch

    call c_f_pointer(data, apply)

    if (apply) then
       call hl_gtk_text_view_get_info(comm_view, nchars=nch)
       if (allocated(pdefs%remarks)) deallocate(pdefs%remarks)

       if (nch /= 0) call hl_gtk_text_view_get_text(comm_view, pdefs%remarks)
    end if

    call gtk_widget_destroy(comm_window)

    pdefs%chflag = .true.
    pdefs%transient%changes =  pdefs%transient%changes+1_int16

  end subroutine gr_comment_quit
end module gr_general_comment_widgets
