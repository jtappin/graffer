! Copyright (C) 2020
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

module gr_colour_widgets
  ! Widget for defining a custom colour.

  use iso_fortran_env
  use iso_c_binding

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_container_add, gtk_label_new, gtk_widget_destroy, &
       & gtk_widget_show_all, gtk_widget_queue_draw, TRUE, FALSE

  use g, only: g_timeout_add
  
  use plplot

  use gr_colours
  use gr_plot
  
  implicit none
  
  type(c_ptr), private :: colour_window, colour_show, &
       & red_level, green_level, blue_level

  integer(kind=int16), dimension(3), private :: rgb_new

  integer(kind=int16), pointer :: source_index
  integer(kind=int16), dimension(:), pointer :: source_raw
  integer(kind=c_int), private :: original_index
  
  type(c_ptr), private :: pd_menu
  type(c_funptr), private :: timer_cb
  
contains
  subroutine gr_colour_define(parent, pull_down, o_index, o_raw, &
       & origin, updater)
    type(c_ptr), intent(in) :: parent, pull_down
    integer(kind=int16), target :: o_index
    integer(kind=int16), dimension(3), target :: o_raw
    integer, intent(in), optional :: origin
    type(c_funptr), intent(in), optional :: updater
    
    type(c_ptr) :: base, jb, junk
    logical, dimension(2), target :: iapply = [.false., .true.]
    integer, dimension(3), target :: cref = [1, 2, 3]

    source_index => o_index
    source_raw => o_raw
    pd_menu = pull_down
    original_index = o_index
    if (present(origin)) original_index = original_index - origin
    if (present(updater)) then
       timer_cb = updater
    else
       timer_cb = c_null_funptr
    end if
    
    if (o_index == -2) then
       rgb_new = o_raw
    else
       call gr_colour_triple(o_index, rgb_new)
    end if

    colour_window = hl_gtk_window_new("Colour define"//c_null_char, &
         & destroy=c_funloc(gr_col_quit), data_destroy=c_loc(iapply(1)), &
         & parent=parent, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(colour_window, base)

    colour_show = hl_gtk_drawing_area_new(size=[150_c_int, 25_c_int])
    call hl_gtk_box_pack(base, colour_show)

    jb = hl_gtk_table_new()
    call hl_gtk_box_pack(base, jb)
    
    junk = gtk_label_new("Red:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 0_c_int)
    
    red_level = hl_gtk_spin_button_new(0_c_int, 255_c_int, &
         & initial_value = int(rgb_new(1), c_int), &
         & value_changed = c_funloc(gr_col_update), &
         & data = c_loc(cref(1)), &
         & tooltip="Set the red level in the colour"//c_null_char)
    call hl_gtk_table_attach(jb, red_level, 1_c_int, 0_c_int)
    
    junk = gtk_label_new("Green:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int)
    green_level = hl_gtk_spin_button_new(0_c_int, 255_c_int, &
         & initial_value = int(rgb_new(2), c_int), &
         & value_changed = c_funloc(gr_col_update), &
         & data = c_loc(cref(2)), &
         & tooltip="Set the green level in the colour"//c_null_char)
    call hl_gtk_table_attach(jb, green_level, 1_c_int, 1_c_int)

    junk = gtk_label_new("Blue:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 2_c_int)
    
    blue_level = hl_gtk_spin_button_new(0_c_int, 255_c_int, &
         & initial_value = int(rgb_new(3), c_int), &
         & value_changed = c_funloc(gr_col_update), &
         & data = c_loc(cref(3)), &
         & tooltip="Set the blue level in the colour"//c_null_char)
    call hl_gtk_table_attach(jb, blue_level, 1_c_int, 2_c_int)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    junk = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_col_quit), data=c_loc(iapply(2)), &
         & tooltip="Apply the changes and quit the dialogue"//c_null_char)
    call hl_gtk_box_pack(jb, junk)
    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_col_quit), data=c_loc(iapply(1)), &
         & tooltip="Quit the dialogue, without making changes"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(colour_window)
 
    call gr_set_plw(colour_show)
    
    call gr_custom_line(rgb_new)
    
    call plfill([0._plflt, 1._plflt, 1._plflt, 0._plflt], &
         & [0._plflt, 0._plflt, 1._plflt, 1._plflt])
    call gtk_widget_queue_draw(colour_show)

  end subroutine gr_colour_define


  subroutine gr_col_update(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    integer(kind=c_int), pointer :: index
    integer(kind=int16) :: cival
 
    call c_f_pointer(data, index)
    
    cival = int(hl_gtk_spin_button_get_value(widget), int16)

    rgb_new(index) = cival

    call gr_set_plw(colour_show)
    
    call gr_custom_line(rgb_new)
    
    call plfill([0._plflt, 1._plflt, 1._plflt, 0._plflt], &
         & [0._plflt, 0._plflt, 1._plflt, 1._plflt])
    call gtk_widget_queue_draw(colour_show)
    
  end subroutine gr_col_update

  recursive subroutine gr_col_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    logical, pointer :: apply
    integer(kind=c_int) :: junk
    
    call c_f_pointer(data, apply)


    if (.not. associated(source_index)) return

    if (apply) then
       source_index = -2_int16
       source_raw = rgb_new
    else
       call gtk_combo_box_set_active(pd_menu, original_index)
    end if
   
    nullify(source_index, source_raw)
    
    call gtk_widget_destroy(colour_window)

    if (apply) then
       if (c_associated(timer_cb)) then
          junk = g_timeout_add(100_c_int, timer_cb, c_null_ptr)
       else
          call gr_set_plw(update=apply)
          call gr_plot_draw(.true.)
       end if
    end if
  end subroutine gr_col_quit
end module gr_colour_widgets
