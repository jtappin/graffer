! Copyright (C) 2013
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

module gr_general
  use iso_c_binding

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_entry_set_text, gtk_label_new, &
       & gtk_spin_button_get_value, TRUE, FALSE

  use graff_globals
  use graff_types
  use gr_cb_common

  use gr_plot

  use gr_general_widgets

  implicit none

contains
  function gr_general_new() result(fr)
    type(c_ptr) :: fr

    ! Define the general options panel

    type(c_ptr) :: junk, t, jb

    fr = hl_gtk_box_new()
    junk = gtk_label_new("General Settings"//c_null_char)
    call hl_gtk_box_pack(fr, junk, expand=FALSE)

    t = hl_gtk_table_new()
    call hl_gtk_box_pack(fr, t, expand=FALSE)

    junk = gtk_label_new("Title:"//c_null_char)
    call hl_gtk_table_attach(t, junk, 0_c_int, 0_c_int, yopts=0)

    title_box = hl_gtk_entry_new(editable=TRUE, &
         & activate=c_funloc(gr_set_title), &
         & focus_out_event=c_funloc(gr_set_title_e), &
         & value = trim(pdefs%title)//c_null_char, &
         & tooltip="Set a title for the plot"//c_null_char)
    call hl_gtk_table_attach(t, title_box, 1_c_int, 0_c_int, xspan=3_c_int, &
         & yopts=0)

    junk = gtk_label_new("Subtitle:"//c_null_char)
    call hl_gtk_table_attach(t, junk, 0_c_int, 1_c_int, yopts=0)

    subtitle_box = hl_gtk_entry_new(editable=TRUE, &
         & activate=c_funloc(gr_set_stitle), &
         & focus_out_event=c_funloc(gr_set_stitle_e), &
         & value = trim(pdefs%subtitle)//c_null_char, &
         & tooltip="Set a subtitle for the plot"//c_null_char)
    call hl_gtk_table_attach(t, subtitle_box, 1_c_int, 1_c_int, xspan=3_c_int,&
         & yopts=0)

    junk = gtk_label_new("Charsize:"//c_null_char)
    call hl_gtk_table_attach(t, junk, 0_c_int, 2_c_int, yopts=0)

    charsize_spin = hl_gtk_spin_button_new(0._c_double, 100._c_double,&
         & 0.01_c_double, initial_value=real(pdefs%charsize, c_double), &
         & value_changed=c_funloc(gr_set_csize), tooltip=&
         & "Set overall character size for the plot"//c_null_char)
    call hl_gtk_table_attach(t, charsize_spin, 1_c_int, 2_c_int, yopts=0)

    junk = gtk_label_new("Line width:"//c_null_char)
    call hl_gtk_table_attach(t, junk, 2_c_int, 2_c_int)

    linewidth_spin =  hl_gtk_spin_button_new(0._c_double, 20._c_double,&
         & 0.01_c_double, initial_value=real(pdefs%axthick, c_double), &
         & value_changed=c_funloc(gr_set_axthick), tooltip=&
         & "Set the thickness for plot axes"//c_null_char)
    call hl_gtk_table_attach(t, linewidth_spin, 3_c_int, 2_c_int, yopts=0)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_table_attach(t, jb, 0_c_int, 3_c_int, xspan=4_c_int, yopts=0)

    junk = hl_gtk_button_new("Corners..."//c_null_char, &
         & clicked = c_funloc(gr_set_corners), tooltip=&
         & "Set plot corners or aspect ratio"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    junk = hl_gtk_button_new("Key..."//c_null_char, &
         & clicked = c_funloc(gr_set_key), tooltip=&
         & "Configure a plot key or legend"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    junk = hl_gtk_button_new("Comment..."//c_null_char, &
         & clicked = c_funloc(gr_set_comment), tooltip=&
         & "Set plot corners or aspect ratio"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

  end function gr_general_new

  subroutine gr_general_set()

    ! Set widgets programmatically

    if (.not. c_associated(title_box)) return

    gui_active = .false.

    call gtk_entry_set_text(title_box, trim(pdefs%title)//c_null_char)
    call gtk_entry_set_text(subtitle_box, trim(pdefs%subtitle)//c_null_char)

    call hl_gtk_spin_button_set_value(charsize_spin, &
         & real(pdefs%charsize, c_double))
    call hl_gtk_spin_button_set_value(linewidth_spin, &
         & real(pdefs%axthick, c_double))

    gui_active = .true.

  end subroutine gr_general_set

  subroutine gr_set_title(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Change plot title

    if (.not. gui_active) return

    call hl_gtk_entry_get_text(widget, pdefs%title)

    call gr_plot_draw(.true.)
  end subroutine gr_set_title
  function gr_set_title_e(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, data, event

    call gr_set_title(widget, data)
    rv = FALSE
  end function gr_set_title_e

  subroutine gr_set_stitle(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Change plot subtitle

    if (.not. gui_active) return

    call hl_gtk_entry_get_text(widget, pdefs%subtitle)

    call gr_plot_draw(.true.)
  end subroutine gr_set_stitle
  function gr_set_stitle_e(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, data, event

    call gr_set_title(widget, data)
    rv = FALSE
  end function gr_set_stitle_e

  subroutine gr_set_csize(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set character size

    if (.not. gui_active) return

    pdefs%charsize = real(gtk_spin_button_get_value(widget), real32)

    call gr_plot_draw(.true.)
  end subroutine gr_set_csize

  subroutine gr_set_axthick(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set thickness for axes.

    if (.not. gui_active) return

    pdefs%axthick = real(gtk_spin_button_get_value(widget), real32)
    call gr_plot_draw(.true.)
  end subroutine gr_set_axthick

  subroutine gr_set_corners(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set up viewport

    call gr_corner_menu

  end subroutine gr_set_corners

  subroutine gr_set_key(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set up key.

    call gr_key_menu

  end subroutine gr_set_key

  subroutine gr_set_comment(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Add comments.

    call gr_comment_menu

  end subroutine gr_set_comment

end module gr_general
