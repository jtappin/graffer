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

module gr_1d_opts
  ! Widgets and handlers for 1D dataset display options.

  use iso_c_binding
  use iso_fortran_env

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_check_menu_item_get_active, gtk_combo_box_get_active, &
       & gtk_label_new, gtk_spin_button_get_value, FALSE

  use graff_types
  use graff_globals

  use gr_cb_common
  use gr_colours
  use gr_colour_widgets

  use ieee_arithmetic, only: ieee_is_finite
  
  implicit none

  character(len=15), dimension(30), parameter, private :: col_list = &
       & [character(len=15) :: &
       & 'Omit', 'White (bg)', 'Black', 'Red', 'Green', 'Blue', 'Cyan', &
       & 'Magenta', 'Yellow', 'Orange', '#7f ff 00', '#00 ff 7f', &
       & '#00 7f ff', '#7f 00 ff', 'Mauve', 'Dark Grey', 'Light Grey', &
       & 'Dark Red', 'Light Red', 'Dark Green', 'Light Green', 'Dark Blue', &
       & 'Light Blue', 'Dark Cyan', 'Light Cyan', 'Dark Magenta', &
       & 'Light Magenta', 'Dark Yellow', 'Light Yellow', 'Custom']
  integer, parameter, private :: ccindex=size(col_list)-1

contains
  function gr_1d_opts_new() result(fr)
    type(c_ptr) :: fr

    ! Define the options panel for 1-D datasets.

    type(c_ptr) :: junk, mnu, table, smnu
    character(len=20), dimension(19) :: sym_list = [character(len=20) :: &
         & 'No symbol', 'Plus', 'Asterisk', 'Dot', 'Diamond', 'Triangle', &
         & 'Square', 'Cross', 'Circle', 'Filled Diamond', 'Filled Triangle', &
         & 'Filled Square', 'Filled Circle', 'Down Triangle', &
         & 'Filled Down Triangle', 'Hexagon', 'Filled Hexagon', &
         & 'Horizontal', 'Vertical']
    character(len=6), dimension(6) :: line_list = ['______', '......', &
         & '_ _ _ ', '_._._.', '_...  ', '__  __']
    character(len=5), dimension(3) :: join_list = ['None ', 'Line ', 'Histo']
    character(len=10), dimension(3) :: coord_list = ['Rect      ', &
         & 'Polar     ', 'Polar (°)']

    integer(kind=c_int) :: isel
    integer, target, dimension(2) :: plimit = [1, 2]
    character(len=32) :: mmtext
    
    fr = hl_gtk_box_new()
    table = hl_gtk_table_new()
    call hl_gtk_box_pack(fr, table, expand=FALSE)

    ! Colour selection
    
    junk = gtk_label_new("Colour:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 0_c_int, yopts=0_c_int)

    if (pdefs%data(pdefs%cset)%colour == -2) then
       isel = ccindex
    else
       isel = pdefs%data(pdefs%cset)%colour+1
    end if
    
    colour_cbo = hl_gtk_combo_box_new(initial_choices=col_list, &
         & changed=c_funloc(gr_1d_set_colour), &
         & active=isel, &
         & tooltip="Select the colour for the plot trace"//c_null_char)
    call hl_gtk_table_attach(table, colour_cbo, 1_c_int, 0_c_int, yopts=0_c_int)
    custom_colour_index = ccindex

    ! Joining mode
    
    junk = gtk_label_new("Join:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 1_c_int, yopts=0_c_int)

    join_cbo = hl_gtk_combo_box_new(initial_choices=join_list, &
         & changed=c_funloc(gr_1d_set_join), &
         & active=int(pdefs%data(pdefs%cset)%pline, c_int), &
         & tooltip="Select the connection format for the plot"//c_null_char)
    call hl_gtk_table_attach(table, join_cbo, 1_c_int, 1_c_int, yopts=0_c_int)

    ! Line style
    
    junk = gtk_label_new("Style:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 2_c_int, yopts=0_c_int)

    style_cbo = hl_gtk_combo_box_new(initial_choices=line_list, &
         & changed=c_funloc(gr_1d_set_linestyle), &
         & active=int(pdefs%data(pdefs%cset)%line, c_int), &
         & tooltip="Select the linestyle for the plot"//c_null_char)
    call hl_gtk_table_attach(table, style_cbo, 1_c_int, 2_c_int, yopts=0_c_int)

    ! Line thickness
    
    junk = gtk_label_new("Thickness:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 3_c_int, yopts=0_c_int)

    thick_ent = hl_gtk_spin_button_new(0._c_double, 16._c_double,&
         & 0.1_c_double, &
         & initial_value=real(pdefs%data(pdefs%cset)%thick, c_double), &
         & value_changed=c_funloc(gr_1d_set_thick), tooltip = &
         & "Set the line thickness for the trace"//c_null_char)
    call hl_gtk_table_attach(table, thick_ent, 1_c_int, 3_c_int, yopts=0_c_int)

    ! Plot symbol
    
    junk = gtk_label_new("Symbol:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 4_c_int, yopts=0_c_int)

    symbol_cbo = hl_gtk_combo_box_new(initial_choices=sym_list, &
         & changed=c_funloc(gr_1d_set_symbol), &
         & active=int(pdefs%data(pdefs%cset)%psym, c_int),&
         & tooltip="Select the symbol for the plot points"//c_null_char)
    call hl_gtk_table_attach(table, symbol_cbo, 1_c_int, 4_c_int, yopts=0_c_int)

    ! Symbol size
    
    junk = gtk_label_new("Size:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 5_c_int, yopts=0_c_int)

    size_ent = hl_gtk_spin_button_new(0._c_double, 16._c_double,&
         & 0.1_c_double, &
         & initial_value=real(pdefs%data(pdefs%cset)%symsize, c_double), &
         & value_changed=c_funloc(gr_1d_set_size), tooltip = &
         & "Set the symbol size for the trace"//c_null_char)
    call hl_gtk_table_attach(table, size_ent, 1_c_int, 5_c_int, yopts=0_c_int)

    ! Rectangular / polar coordinate system
    
    junk = gtk_label_new("Coords:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 6_c_int, yopts=0_c_int)

    csys_cbo = hl_gtk_combo_box_new(initial_choices=coord_list, &
         & changed=c_funloc(gr_1d_set_coord), &
         & active=int(pdefs%data(pdefs%cset)%mode, c_int), &
         & tooltip="Select the coordinate system for the plot"//c_null_char)
    call hl_gtk_table_attach(table, csys_cbo, 1_c_int, 6_c_int, yopts=0_c_int)

    ! Plotting limits.

    junk = gtk_label_new("Min value:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 7_c_int, yopts=0_c_int)

    min_ent = hl_gtk_entry_new(editable=TRUE, &
         & activate=c_funloc(gr_set_1d_plimit), data = c_loc(plimit(1)), &
         & focus_out_event = c_funloc(gr_set_1d_plimit_e), &
         & data_focus_out = c_loc(plimit(1)), size=75_c_int,&
         & tooltip="Set the minimum value to plot in the dataset."//c_null_char)
    call hl_gtk_table_attach(table, min_ent, 1_c_int, 7_c_int, yopts=0_c_int)
    if (ieee_is_finite(pdefs%data(pdefs%cset)%min_val)) then
       write(mmtext, "(1pg0.5)") pdefs%data(pdefs%cset)%min_val
       call gtk_entry_set_text(min_ent, trim(mmtext)//c_null_char)
    end if
    
    junk = gtk_label_new("Max value:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 8_c_int, yopts=0_c_int)

    max_ent = hl_gtk_entry_new(editable=TRUE, &
         & activate=c_funloc(gr_set_1d_plimit), data = c_loc(plimit(2)), &
         & focus_out_event = c_funloc(gr_set_1d_plimit_e), &
         & data_focus_out = c_loc(plimit(2)), size=75_c_int,&
         & tooltip="Set the maximum value to plot in the dataset."//c_null_char)
    call hl_gtk_table_attach(table, max_ent, 1_c_int, 8_c_int, yopts=0_c_int)
    if (ieee_is_finite(pdefs%data(pdefs%cset)%max_val)) then
       write(mmtext, "(1pg0.5)") pdefs%data(pdefs%cset)%max_val
       call gtk_entry_set_text(max_ent, trim(mmtext)//c_null_char)
    end if
 
    
    ! Extra settings
    
    mnu = hl_gtk_menu_new()
    call hl_gtk_table_attach(table, mnu, 0_c_int, 9_c_int, xspan=2_c_int,&
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

    integer(kind=int16) :: icol
    
    ! Set the colour for the trace

    if (.not. gui_active) return

    icol = int(gtk_combo_box_get_active(widget), int16)
    if (icol == ccindex) then
       call gr_colour_define(gr_window, colour_cbo, &
            & pdefs%data(pdefs%cset)%colour, &
            & pdefs%data(pdefs%cset)%c_vals, &
            & origin=-1)
    else
       pdefs%data(pdefs%cset)%colour = icol-1_int16
       pdefs%data(pdefs%cset)%c_vals = 0_int16
       call gr_plot_draw(.true.)
    end if
  end subroutine gr_1d_set_colour

  subroutine gr_1d_set_symbol(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set the symbol for the points

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%psym = &
         & int(gtk_combo_box_get_active(widget), int16)

    call gr_plot_draw(.true.)
  end subroutine gr_1d_set_symbol

  subroutine gr_1d_set_linestyle(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set the linestyle for the trace

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%line = &
         & int(gtk_combo_box_get_active(widget), int16)

    call gr_plot_draw(.true.)
  end subroutine gr_1d_set_linestyle

  subroutine gr_1d_set_join(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set the joining option

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%pline = &
         & int(gtk_combo_box_get_active(widget), int16)

    call gr_plot_draw(.true.)
  end subroutine gr_1d_set_join

  subroutine gr_1d_set_coord(widget, data) bind(c)
    type(c_ptr), value :: widget, data
    logical :: log_valid
    integer :: i
    
    ! Select rectangular or polar coordinates.

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%mode = &
         &int(gtk_combo_box_get_active(widget), int16)
    
    log_valid = all(pdefs%data%mode == 0)
    if (.not. log_valid) pdefs%axtype(:) = 0

    do i = 1,3
       call gtk_check_menu_item_set_active(log_chb(i), &
            & int(pdefs%axtype(i), c_int))
       call gtk_widget_set_sensitive(log_chb(i), f_c_logical(log_valid))
    end do
    
    call gr_plot_draw(.true.)
  end subroutine gr_1d_set_coord

  subroutine gr_1d_set_thick(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set the line thickness

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%thick = &
         & real(gtk_spin_button_get_value(widget), real32)

    call gr_plot_draw(.true.)
  end subroutine gr_1d_set_thick

  subroutine gr_1d_set_size(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set the symbol size

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%symsize = &
         & real(gtk_spin_button_get_value(widget), real32)

    call gr_plot_draw(.true.)
  end subroutine gr_1d_set_size

  subroutine gr_1d_set_sort(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set whether to sort the X axis

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%sort = &
         & c_f_logical(gtk_check_menu_item_get_active(widget))

    call gr_plot_draw(.true.)
  end subroutine gr_1d_set_sort

  subroutine gr_1d_set_clip(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set clipping to viewport

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%noclip = &
         & .not. c_f_logical(gtk_check_menu_item_get_active(widget))

    call gr_plot_draw(.true.)
  end subroutine gr_1d_set_clip

  subroutine gr_1d_set_mouse(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set mouse editing.

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%medit = &
         & c_f_logical(gtk_check_menu_item_get_active(widget))
    call gr_draw_tips
    call gr_set_changed(.true.)
  end subroutine gr_1d_set_mouse

  function gr_set_1d_plimit_e(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, event, data

    call gr_set_1d_plimit(widget, data)
    rv = FALSE
  end function gr_set_1d_plimit_e

  subroutine gr_set_1d_plimit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    integer, pointer :: limit
    character(len=32) :: tval
    real(kind=real64) :: val, current
    integer :: ios
    
    ! Set plotting limit

    if (.not. gui_active) return

    call c_f_pointer(data, limit)

    if (limit == 1) then
       current = pdefs%data(pdefs%cset)%min_val
    else
       current = pdefs%data(pdefs%cset)%max_val
    end if
    
    call hl_gtk_entry_get_text(widget, tval)
    if (len_trim(tval) == 0) then
       val = d_nan()
    else
       read(tval, *, iostat=ios) val
       if (ios /= 0) then
          if (ieee_is_finite(current)) then
             write(tval, "(1pg0.5)") current
             call gtk_entry_set_text(widget, trim(tval)//c_null_char)
          else
             call gtk_entry_set_text(widget, c_null_char)
          end if
          return
       end if
    end if
    if (limit == 1) then
       pdefs%data(pdefs%cset)%min_val = val
    else
       pdefs%data(pdefs%cset)%max_val = val
    end if

    call gr_plot_draw(.true.)
  end subroutine gr_set_1d_plimit
  
end module gr_1d_opts
    
