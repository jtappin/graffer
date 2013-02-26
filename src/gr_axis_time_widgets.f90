module gr_axis_time_widgets
  use iso_c_binding
  use iso_fortran_env

  use gtk_hl
  use gtk_sup

  use graff_types
  use graff_globals
  use gr_utils

  implicit none

  type(c_ptr), private :: time_window, time_internal_cbo, time_external_cbo, &
       & time_zero_sb
  integer, private :: iaxis
contains
  subroutine gr_time_menu(axis)
    integer, intent(in) :: axis

    type(graff_style), pointer :: axstyle
    integer(int16) :: unit, munit
    logical, target, dimension(2) :: iapply = [.false., .true.]
    type(c_ptr) :: base, jb, junk
    character(len=*), dimension(*), parameter :: &
         & ulist = ['Seconds', 'Minutes', 'Hours  ', 'Days   ']

    axstyle => pdefs%axsty(axis)
    iaxis = axis

    unit = iand(ishft(axstyle%time,-1), 3_int16)
    munit = iand(ishft(axstyle%time,-3), 3_int16)

    time_window = hl_gtk_window_new("Time axis settings"//c_null_char, &
         & destroy = c_funloc(gr_time_quit), data_destroy=c_loc(iapply(1)), &
         & parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(time_window, base)

    jb = hl_gtk_table_new()
    call hl_gtk_box_pack(base, jb)

    junk = gtk_label_new("Internal units:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 0_c_int)

    time_internal_cbo = hl_gtk_combo_box_new(initial_choices=ulist, &
         & active=int(unit, c_int), tooltip = &
         & "Select the unit in which the axis values are stored"//c_null_char)
    call hl_gtk_table_attach(jb, time_internal_cbo, 1_c_int, 0_c_int)

    junk = gtk_label_new("Display units:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int)

    time_external_cbo = hl_gtk_combo_box_new(initial_choices=ulist, &
         & active=int(munit, c_int), tooltip = &
         & "Select the unit in which the axis values are to be displayed"//&
         & c_null_char)
    call hl_gtk_table_attach(jb, time_external_cbo, 1_c_int, 1_c_int)

    junk = gtk_label_new("Display zero:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 2_c_int)

    time_zero_sb = hl_gtk_spin_button_new(-huge(1_c_int), huge(1_c_int), &
         & initial_value = int(axstyle%tzero, c_int), tooltip=&
         & "Enter an offset, 0 on the axis will be shown as this value"//&
         & c_new_line//"in output units"//c_null_char)
    call hl_gtk_table_attach(jb, time_zero_sb, 1_c_int, 2_c_int)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb)

    junk = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_time_quit), data=c_loc(iapply(2)), &
         & tooltip="Apply the changes and exit the dialogue"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_time_quit), data=c_loc(iapply(1)), &
         & tooltip="Exit the dialogue, without changing the options"//&
         & c_null_char)
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(time_window)

  end subroutine gr_time_menu

  recursive subroutine gr_time_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    logical, pointer :: apply
    type(graff_style), pointer :: axstyle
    integer(kind=int16) :: ival
    integer(kind=int16), parameter :: mask_int=6_int16, mask_ext=24_int16

    call c_f_pointer(data, apply)

    if (apply) then
       axstyle => pdefs%axsty(iaxis)

       ival = ishft(int(gtk_combo_box_get_active(time_internal_cbo), int16), &
            & 1)
       axstyle%time = merge_bits(ival, axstyle%time, mask_int)

       ival = ishft(int(gtk_combo_box_get_active(time_external_cbo), int16), &
            & 3)
       axstyle%time = merge_bits(ival, axstyle%time, mask_ext)

       axstyle%tzero = int(hl_gtk_spin_button_get_value(time_zero_sb), int32)

       axstyle%time = ibset(axstyle%time, time_bit)
    end if

    iaxis = 0

    call gtk_widget_destroy(time_window)

  end subroutine gr_time_quit
end module gr_axis_time_widgets
