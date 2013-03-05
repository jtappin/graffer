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

module gr_gui
  use iso_c_binding
  use iso_fortran_env

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_container_add, gtk_frame_new, gtk_label_new, gtk_main, &
       & gtk_widget_show_all, gtk_notebook_set_current_page, TRUE, FALSE

  use g, only: g_timeout_add_seconds

  use graff_globals
  use graff_types

  use gr_menubar
  use gr_general
  use gr_axis
  use gr_ds_selector
  use gr_ds_data
  use gr_1d_opts
  use gr_2d_opts
  use gr_mode
  use gr_drawing
  use gr_plot

  use gr_cb_common

  implicit none

contains
  subroutine gr_make_gui(version, infile)
    character(len=*), intent(in) :: version, infile

    ! Overall gui setup

    type(c_ptr) :: base, lh, junk, jb, gaccel, g_d_notebook, &
         & global_base, ds_base, ynb, topbase

    integer(kind=c_int) :: nbi, tid, iseconds
!!$    integer(kind=int16) :: zformat

    gui_active = .false.

    gr_window = hl_gtk_window_new(title="Graffer V"//trim(version)//&
         & ": "//trim(infile)//c_null_char, destroy=c_funloc(gr_exit), &
         & resizable=FALSE, accel_group=gaccel)

    topbase = hl_gtk_box_new()
    call gtk_container_add(gr_window, topbase)

    base = hl_gtk_box_new(horizontal = TRUE)
    call hl_gtk_box_pack(topbase, base)

    ! The Left Hand column has all the top-level controls.
    lh = hl_gtk_box_new()
    call hl_gtk_box_pack(base, lh, expand=FALSE)

    junk = gr_menubar_new(gaccel)
    call hl_gtk_box_pack(lh, junk, expand=FALSE)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(lh, jb, expand=FALSE)

    junk = gtk_label_new("Name:"//c_null_char)
    call hl_gtk_box_pack(jb, junk, expand=FALSE)
    name_id = hl_gtk_entry_new(value=&
         & trim(infile)//c_null_char, editable=FALSE)
    call hl_gtk_box_pack(jb, name_id)

    g_d_notebook = hl_gtk_notebook_new(switch_page=c_funloc(gr_redraw_cb))
    call hl_gtk_box_pack(lh, g_d_notebook)

    ! global base has the overall Graffer settings

    global_base = hl_gtk_box_new()
    nbi = hl_gtk_notebook_add_page(g_d_notebook, &
         & global_base, label="Global"//c_null_char)

    junk = gr_general_new()
    call hl_gtk_box_pack(global_base, junk, expand=false)

    junk = gr_axis_new(1)
    call hl_gtk_box_pack(global_base, junk, expand=false)

    y2_check = hl_gtk_check_button_new("Enable secondary Y-axis?"&
         & //c_null_char, &
         & toggled=c_funloc(gr_enable_y2), &
         & initial_state=f_c_logical(pdefs%y_right), tooltip=&
         & "Select whether to show a secondary Y axis"//c_null_char)
    call hl_gtk_box_pack(global_base, y2_check, expand=false)

    ynb=hl_gtk_notebook_new()
    call hl_gtk_box_pack(global_base, ynb, expand=false)

    junk = gr_axis_new(2)
    nbi = hl_gtk_notebook_add_page(ynb, junk, label="Main"//c_null_char)

    y2_tab = gr_axis_new(3, sensitive=pdefs%y_right)
    nbi = hl_gtk_notebook_add_page(ynb, y2_tab, label="Secondary"//c_null_char)

    junk = gtk_label_new(c_null_char)
    call hl_gtk_box_pack(global_base, junk)

    ! Dataset specific settings

    ds_base = hl_gtk_box_new()
    nbi = hl_gtk_notebook_add_page(g_d_notebook, ds_base, &
         & label="Datasets"//c_null_char)

    junk = gr_ds_selector_new()
    call hl_gtk_box_pack(ds_base, junk)

    display_nb = hl_gtk_notebook_new(show_tabs=FALSE)
    call hl_gtk_box_pack(ds_base, display_nb)

    junk = gr_1d_opts_new()
    nbi = hl_gtk_notebook_add_page(display_nb, junk)

    junk = gr_2d_opts_new()
    nbi = hl_gtk_notebook_add_page(display_nb, junk)

    junk = gr_ds_data_new()
    call hl_gtk_box_pack(ds_base, junk, expand=FALSE)

    junk = gtk_label_new(c_null_char)
    call hl_gtk_box_pack(ds_base, junk)

    !  Mode etc.

    junk = gr_mode_new()
    call hl_gtk_box_pack(lh, junk)


    ! Last but not least, the drawing area

    gr_drawing_area = gr_drawing_new()
    call hl_gtk_box_pack(base, gr_drawing_area)

    ! And a message box

    gr_infobar = hl_gtk_info_bar_new(["OK"], &
         & response=c_funloc(gr_infobar_clr), &
         & type=GTK_MESSAGE_WARNING)
    call hl_gtk_box_pack(topbase, gr_infobar)

!    zformat = pdefs%data(pdefs%cset)%zdata%format
    call gtk_widget_show_all(gr_window)
!    pdefs%data(pdefs%cset)%zdata%format = zformat

    ! Showing the right page(s) must follow showing the widgets
    select case (pdefs%data(pdefs%cset)%type)
    case(-4,9)
       call gtk_notebook_set_current_page(display_nb, 1)
       call gtk_notebook_set_current_page(fmt_nbook, &
            & int(pdefs%data(pdefs%cset)%zdata%format, c_int))
    case(-3:8)
       call gtk_notebook_set_current_page(display_nb, 0)
    end select

    ! Since there are a few programmatically called handlers that normally
    ! modify the data, but not in setup, we clear the changed flags
    ! manually

    gui_active = .true.

    pdefs%transient%changes = 0_int16
    pdefs%chflag = .false.

    call gr_plot_open
    call gr_plot_draw(.false.)

    iseconds = int(pdefs%opts%auto_delay, c_int)
    tid = g_timeout_add_seconds(iseconds, c_funloc(gr_auto_save), c_null_ptr)

    call gtk_main()

  end subroutine gr_make_gui
end module gr_gui
