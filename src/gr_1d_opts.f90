module gr_1d_opts
  use iso_c_binding
  use iso_fortran_env

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_check_menu_item_get_active, gtk_combo_box_get_active, &
       & gtk_container_add, gtk_frame_new, gtk_label_new, &
       & gtk_spin_button_get_value, FALSE

  use graff_types
  use graff_globals

  use gr_cb_common

  implicit none

contains
  function gr_1d_opts_new() result(fr)
    type(c_ptr) :: fr

    type(c_ptr) :: junk, mnu, table, smnu
    character(len=15), dimension(29) :: col_list = [character(len=15) :: &
         & 'Omit', 'White (bg)', 'Black', 'Red', 'Green', 'Blue', 'Cyan', &
         & 'Magenta', 'Yellow', 'Orange', '#7f ff 00', '#00 ff 7f', &
         & '#00 7f ff', '#7f 00 ff', 'Mauve', 'Dark Grey', 'Light Grey', &
         & 'Dark Red', 'Light Red', 'Dark Green', 'Light Green', 'Dark Blue', &
         & 'Light Blue', 'Dark Cyan', 'Light Cyan', 'Dark Magenta', &
         & 'Light Magenta', 'Dark Yellow', 'Light Yellow']
    character(len=20), dimension(15) :: sym_list = [character(len=20) :: &
         & 'No symbol', 'Plus', 'Asterisk', 'Dot', 'Diamond', 'Triangle', &
         & 'Square', 'Cross', 'Circle', 'Filled Diamond', 'Filled Triangle', &
         & 'Filled Square', 'Filled Circle', 'Down Triangle', &
         & 'Filled Down Triangle']
    character(len=6), dimension(6) :: line_list = ['______', '......', &
         & '_ _ _ ', '_._._.', '_...  ', '__  __']
    character(len=5), dimension(3) :: join_list = ['None ', 'Line ', 'Histo']
    character(len=10), dimension(3) :: coord_list = ['Rect      ', &
         & 'Polar     ', 'Polar (°)']

    fr = hl_gtk_box_new()
    table = hl_gtk_table_new()
    call hl_gtk_box_pack(fr, table, expand=FALSE)

    junk = gtk_label_new("Colour:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 0_c_int, yopts=0_c_int)

    colour_cbo = hl_gtk_combo_box_new(initial_choices=col_list, &
         & changed=c_funloc(gr_1d_set_colour), &
         & active=int(pdefs%data(pdefs%cset)%colour+1, c_int), &
         & tooltip="Select the colour for the plot trace"//c_null_char)
    call hl_gtk_table_attach(table, colour_cbo, 1_c_int, 0_c_int, yopts=0_c_int)

    junk = gtk_label_new("Symbol:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 1_c_int, yopts=0_c_int)

    symbol_cbo = hl_gtk_combo_box_new(initial_choices=sym_list, &
         & changed=c_funloc(gr_1d_set_symbol), &
         & active=int(pdefs%data(pdefs%cset)%psym, c_int),&
         & tooltip="Select the symbol for the plot points"//c_null_char)
    call hl_gtk_table_attach(table, symbol_cbo, 1_c_int, 1_c_int, yopts=0_c_int)

    junk = gtk_label_new("Style:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 2_c_int, yopts=0_c_int)

    style_cbo = hl_gtk_combo_box_new(initial_choices=line_list, &
         & changed=c_funloc(gr_1d_set_linestyle), &
         & active=int(pdefs%data(pdefs%cset)%line, c_int), &
         & tooltip="Select the linestyle for the plot"//c_null_char)
    call hl_gtk_table_attach(table, style_cbo, 1_c_int, 2_c_int, yopts=0_c_int)

    junk = gtk_label_new("Join:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 3_c_int, yopts=0_c_int)

    join_cbo = hl_gtk_combo_box_new(initial_choices=join_list, &
         & changed=c_funloc(gr_1d_set_join), &
         & active=int(pdefs%data(pdefs%cset)%pline, c_int), &
         & tooltip="Select the connection format for the plot"//c_null_char)
    call hl_gtk_table_attach(table, join_cbo, 1_c_int, 3_c_int, yopts=0_c_int)

    junk = gtk_label_new("Thickness:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 4_c_int, yopts=0_c_int)

    thick_ent = hl_gtk_spin_button_new(0._c_double, 16._c_double,&
         & 0.1_c_double, &
         & initial_value=real(pdefs%data(pdefs%cset)%thick, c_double), &
         & value_changed=c_funloc(gr_1d_set_thick), tooltip = &
         & "Set the line thickness for the trace"//c_null_char)
    call hl_gtk_table_attach(table, thick_ent, 1_c_int, 4_c_int, yopts=0_c_int)

    junk = gtk_label_new("Size:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 5_c_int, yopts=0_c_int)

    size_ent = hl_gtk_spin_button_new(0._c_double, 16._c_double,&
         & 0.1_c_double, &
         & initial_value=real(pdefs%data(pdefs%cset)%symsize, c_double), &
         & value_changed=c_funloc(gr_1d_set_size), tooltip = &
         & "Set the symbol size for the trace"//c_null_char)
    call hl_gtk_table_attach(table, size_ent, 1_c_int, 5_c_int, yopts=0_c_int)

    junk = gtk_label_new("Coords:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 6_c_int, yopts=0_c_int)

    csys_cbo = hl_gtk_combo_box_new(initial_choices=coord_list, &
         & changed=c_funloc(gr_1d_set_coord), &
         & active=int(pdefs%data(pdefs%cset)%mode, c_int), &
         & tooltip="Select the coordinate system for the plot"//c_null_char)
    call hl_gtk_table_attach(table, csys_cbo, 1_c_int, 6_c_int, yopts=0_c_int)

    mnu = hl_gtk_menu_new()
    call hl_gtk_table_attach(table, mnu, 0_c_int, 7_c_int, xspan=2_c_int,&
         & yopts=0_c_int)

    smnu = hl_gtk_menu_submenu_new(mnu, "Extras ▼"//c_null_char, &
         & tooltip="Extra settings"//c_null_char)

    xsort_id = hl_gtk_check_menu_item_new(smnu, &
         & "Sort X Axis?"//c_null_char, &
         & initial_state=f_c_logical(pdefs%data(pdefs%cset)%sort), &
         & toggled=c_funloc(gr_1d_set_sort), tooltip= &
         & "Select whether to sort the X axis before plotting"//c_null_char)
    clip_id = hl_gtk_check_menu_item_new(smnu, &
         & "Clip to box?"//c_null_char, &
         & initial_state=f_c_logical(.not. pdefs%data(pdefs%cset)%noclip), &
         & toggled=c_funloc(gr_1d_set_clip), tooltip= &
         & "Select whether to clip the data to the region"//c_null_char)
    mouse_id = hl_gtk_check_menu_item_new(smnu, &
         & "Mouse editing?"//c_null_char, &
         & initial_state=f_c_logical(pdefs%data(pdefs%cset)%medit), &
         & toggled=c_funloc(gr_1d_set_mouse), tooltip= &
         & "Select whether to allow editing with the mouse"//c_null_char)

  end function gr_1d_opts_new

  subroutine gr_1d_set_colour(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%colour = &
         & int(gtk_combo_box_get_active(widget), int16)-1_int16

    call gr_plot_draw(.true.)
  end subroutine gr_1d_set_colour

  subroutine gr_1d_set_symbol(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%psym = &
         & int(gtk_combo_box_get_active(widget), int16)

    call gr_plot_draw(.true.)
  end subroutine gr_1d_set_symbol

  subroutine gr_1d_set_linestyle(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%line = &
         & int(gtk_combo_box_get_active(widget), int16)

    call gr_plot_draw(.true.)
  end subroutine gr_1d_set_linestyle

  subroutine gr_1d_set_join(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%pline = &
         & int(gtk_combo_box_get_active(widget), int16)

    call gr_plot_draw(.true.)
  end subroutine gr_1d_set_join

  subroutine gr_1d_set_coord(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%mode = &
         &int(gtk_combo_box_get_active(widget), int16)

    call gr_plot_draw(.true.)
  end subroutine gr_1d_set_coord

  subroutine gr_1d_set_thick(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%thick = &
         & real(gtk_spin_button_get_value(widget), real32)

    call gr_plot_draw(.true.)
  end subroutine gr_1d_set_thick

  subroutine gr_1d_set_size(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%symsize = &
         & real(gtk_spin_button_get_value(widget), real32)

    call gr_plot_draw(.true.)
  end subroutine gr_1d_set_size

  subroutine gr_1d_set_sort(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%sort = &
         & c_f_logical(gtk_check_menu_item_get_active(widget))

    call gr_plot_draw(.true.)
  end subroutine gr_1d_set_sort

  subroutine gr_1d_set_clip(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%noclip = &
         & .not. c_f_logical(gtk_check_menu_item_get_active(widget))

    call gr_plot_draw(.true.)
  end subroutine gr_1d_set_clip

  subroutine gr_1d_set_mouse(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%medit = &
         & c_f_logical(gtk_check_menu_item_get_active(widget))
    call gr_draw_tips
    pdefs%chflag = .true.
    pdefs%transient%changes = pdefs%transient%changes + 1_int16
  end subroutine gr_1d_set_mouse

end module gr_1d_opts
    
