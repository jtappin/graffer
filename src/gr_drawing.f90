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

module gr_drawing
  ! The drawing surface and its handlers.

  use iso_c_binding
  use iso_fortran_env

  use gtk_hl
  use gtk_draw_hl
  use gtk_sup
  use gtk, only: FALSE

  use gdk_events

  use gdk, only: gdk_keyval_from_name, gdk_window_fullscreen, &
       & gdk_window_get_state, gdk_window_unfullscreen

  use gtk, only: gtk_entry_set_text, gtk_widget_get_window, &
       & gtk_widget_grab_focus, gtk_widget_queue_draw, FALSE, &
       & GDK_ENTER_NOTIFY, GDK_WINDOW_STATE_FULLSCREEN

  use graff_types
  use graff_globals

  use gr_plot
  use gr_plot_tools
  use gr_cb_common
  use gr_drawing_buttons
  use gr_msg

  implicit none

  integer(kind=c_int), private :: nx, ny

contains
  function gr_drawing_new() result(area)
    type(c_ptr) :: area

    ! Create the drawing area and attach the needed events.

    area = hl_gtk_drawing_area_new(size=sysopts%geometry, &
         & has_alpha=FALSE,  &
         & button_press_event=c_funloc(gr_draw_button), &
         & button_release_event=c_funloc(gr_draw_button), &
         & motion_event=c_funloc(gr_draw_motion), &
         & key_press_event=c_funloc(gr_draw_key), &
         & leave_event=c_funloc(gr_draw_enter), &
         & enter_event=c_funloc(gr_draw_enter), &
         & size_allocate=c_funloc(gr_draw_resize))

    call hl_gtk_drawing_area_get_size(area, width=nx, height=ny)
  end function gr_drawing_new

  subroutine gr_draw_resize(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Actions on resizing

    call gr_plot_close()
    call hl_gtk_drawing_area_resize(widget)
    call hl_gtk_drawing_area_get_size(widget, width=nx, height=ny)
    call gr_plot_open()
    call gr_plot_draw(.false.)

    call gr_ds_device
  end subroutine gr_draw_resize

  function gr_draw_button(widget, event, gdata) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, event, gdata

    ! Actions on a button click

    type(gdkeventbutton), pointer :: fevent
    call c_f_pointer(event, fevent)

    if (pdefs%transient%mode == 0) then
       call gr_drawing_plot(fevent)
    else
       call gr_drawing_text(fevent)
    end if

    rv = FALSE
  end function gr_draw_button

  function gr_draw_motion(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, event, data

    ! Actions on cursor motion

    type(gdkeventmotion), pointer :: fevent
    real(kind=c_double) :: xw, yw
    real(kind=plflt) :: xmin, xmax, ymin, ymax, xh, yh

    character(len=20) :: tv
    logical :: xstatus

    call c_f_pointer(event, fevent)
    call gr_plot_coords_d_w(fevent%x, fevent%y, xw, yw)

    write(tv, "(g0.5)") xw
    call gtk_entry_set_text(cursor_position(1), trim(tv)//c_null_char)
    write(tv, "(g0.5)") yw
    call gtk_entry_set_text(cursor_position(2), trim(tv)//c_null_char)

    if (pdefs%transient%hairs) then
       call plxormod(.true., xstatus)
       if (xstatus) then
          if (pdefs%transient%mode == 1) then
             call gr_plot_linesty(2_int16)
             call gr_plot_transform(full=.true._int8)

             call gr_plot_coords_n_w(0._plflt, 0._plflt, xmin, ymin, &
                  & nolog=.true.)
             call gr_plot_coords_n_w(1._plflt, 1._plflt, xmax, ymax, &
                  & nolog=.true.)

          else
             call gr_plot_linesty(0_int16)
             call gr_plot_transform(full=.false._int8)

             call gr_plot_coords_v_w(0._plflt, 0._plflt, xmin, ymin, &
                  & nolog=.true.)
             call gr_plot_coords_v_w(1._plflt, 1._plflt, xmax, ymax, &
                  & nolog=.true.)
          end if

          if (xprev > 0. .or. yprev > 0.) then
             call gr_plot_coords_n_w(xprev, yprev, xh, yh, nolog=.true.)
             call pljoin(xh, ymin, xh, ymax)
             call pljoin(xmin, yh, xmax, yh)
          end if

          call gr_plot_coords_d_w(fevent%x, fevent%y, xh, yh, nolog=.true.)
          call pljoin(xh, ymin, xh, ymax)
          call pljoin(xmin, yh, xmax, yh)

          call gr_plot_coords_d_n(fevent%x, fevent%y, xprev, yprev)

          call plxormod(.false., xstatus)
          call gtk_widget_queue_draw(widget)
       end if
    end if

    rv = FALSE
  end function gr_draw_motion

  function gr_draw_key(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, event, data

    ! Actions on keyboard

    integer(kind=c_int) :: key_f, key_esc, key_f11, wstate
    type(c_ptr) :: gdk_win
    type(gdkeventkey), pointer :: fevent

    rv = FALSE

    key_f = gdk_keyval_from_name("f"//c_null_char)
    key_f11 = gdk_keyval_from_name("F11"//c_null_char)
    key_esc = gdk_keyval_from_name("Escape"//c_null_char)

    gdk_win = gtk_widget_get_window(gr_window)

    if (.not. c_associated(gdk_win)) then
       call gr_message(&
            & "gr_draw_key: No GDK window associated with top level GTK window")
       return
    end if

    wstate = gdk_window_get_state(gdk_win)

    call c_f_pointer(event, fevent)

    if (fevent%keyval == key_f .or. fevent%keyval == key_f11) then
       if (iand(wstate, GDK_WINDOW_STATE_FULLSCREEN) == 0) then
          call gdk_window_fullscreen(gdk_win)
       else
          call gdk_window_unfullscreen(gdk_win)
       end if
       call gr_plot_draw(.false.)
    else if (fevent%keyval == key_esc .and. &
         & iand(wstate, GDK_WINDOW_STATE_FULLSCREEN) /= 0) then
       call gdk_window_unfullscreen(gdk_win)
       call gr_plot_draw(.false.)
    end if

  end function gr_draw_key

  function gr_draw_enter(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, event, data

    ! Actions on cursor enter/exit.

    type(gdkeventcrossing), pointer :: fevent

    call c_f_pointer(event, fevent)
    if (fevent%type == GDK_ENTER_NOTIFY) then
       call gtk_widget_grab_focus(widget)
    else
       call gtk_widget_grab_focus(gr_window)
    end if

    rv = FALSE
  end function gr_draw_enter

end module gr_drawing

