module gr_mode
  use iso_c_binding
  use iso_fortran_env

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_combo_box_get_active, gtk_label_new, &
       & gtk_toggle_button_get_active, TRUE, FALSE

  use graff_types
  use graff_globals
  use gr_cb_common

  implicit none

contains
  function gr_mode_new() result(table)
    type(c_ptr) :: table

    ! Mode setting widgets

    type(c_ptr) :: jb, junk

    table = hl_gtk_box_new()

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(table, jb)
    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(table, jb, expand=FALSE)

    junk = gtk_label_new("Draw/Text Mode:"//c_null_char)
    call hl_gtk_box_pack(jb, junk, expand=FALSE)

    junk = hl_gtk_combo_box_new(initial_choices=['Draw','Text'], &
         & changed=c_funloc(gr_text_mode), &
         & active=int(pdefs%transient%mode, c_int), &
         & tooltip="Select drawing or text mode"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    junk = hl_gtk_check_button_new("Cross Hairs"//c_null_char, &
         & toggled=c_funloc(gr_cross_hairs), &
         & initial_state=f_c_logical(pdefs%transient%hairs), &
         & tooltip="Toggle display of cross hairs at the cursor"//c_nulL_char)
    call hl_gtk_box_pack(jb, junk)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(table, jb)
    junk = gtk_label_new("X:"//c_null_char)
    call hl_gtk_box_pack(jb, junk, expand=FALSE)
    cursor_position(1) = hl_gtk_entry_new(editable=FALSE, size=75_c_int)
    call hl_gtk_box_pack(jb, cursor_position(1))

    junk = gtk_label_new("Y:"//c_null_char)
    call hl_gtk_box_pack(jb, junk, expand=FALSE)
    cursor_position(2) = hl_gtk_entry_new(editable=FALSE, size=75_c_int)
    call hl_gtk_box_pack(jb, cursor_position(2))

  end function gr_mode_new

  subroutine gr_text_mode(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Toggle text/drawing modes

    if (.not. gui_active) return

    pdefs%transient%mode = int(gtk_combo_box_get_active(widget), int16)
    call gr_draw_tips
    call gr_plot_draw(.false.)

  end subroutine gr_text_mode

  subroutine gr_cross_hairs(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Toggle cross hairs (not actually available, XOR mode doesn't
    ! work in Cairo drivers).

    if (.not. gui_active) return

    pdefs%transient%hairs = &
         & c_f_logical(gtk_toggle_button_get_active(widget))

  end subroutine gr_cross_hairs

end module gr_mode
