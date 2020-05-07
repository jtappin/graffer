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
       & gtk_widget_show_all, TRUE, FALSE

  use plplot

  use gr_colours
  use gr_plot
  
  implicit none
  
  type(c_ptr), private :: colour_window, colour_show, &
       & red_level, green_level, blue_level

  integer(kind=int16), private :: rnew, gnew, bnew
  logical, private :: update_colour
  
contains
  subroutine gr_colour_define(parent, r, g, b, update)
    type(c_ptr), intent(in) :: parent
    
    integer(kind=int16), intent(inout) :: r, g, b
    logical, intent(out) :: update
    
    type(c_ptr) :: base, jb, junk, jbb
    logical, dimension(2), target :: iapply = [.false., .true.]
    integer, dimension(3), target :: cref = [1, 2, 3]

    rnew = r
    gnew = g
    bnew = b
    
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
         & initial_value = int(r, c_int), &
         & value_changed = c_funloc(gr_col_update), &
         & data = c_loc(cref(1)), &
         & tooltip="Set the red level in the colour"//c_null_char)
    call hl_gtk_table_attach(jb, red_level, 1_c_int, 0_c_int)
    
    junk = gtk_label_new("Green:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int)
    green_level = hl_gtk_spin_button_new(0_c_int, 255_c_int, &
         & initial_value = int(g, c_int), &
         & value_changed = c_funloc(gr_col_update), &
         & data = c_loc(cref(2)), &
         & tooltip="Set the green level in the colour"//c_null_char)
    call hl_gtk_table_attach(jb, green_level, 1_c_int, 1_c_int)

    junk = gtk_label_new("Blue:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 2_c_int)
    
    blue_level = hl_gtk_spin_button_new(0_c_int, 255_c_int, &
         & initial_value = int(b, c_int), &
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
 
    call gr_set_clw(r,g,b)

    if (update_colour) then
       r = rnew
       g = gnew
       b = bnew
    end if
    update = update_colour
    
  end subroutine gr_colour_define

  subroutine gr_set_clw(r,g,b)
    ! Select and initialize the colour window.

    integer(kind=int16), intent(in) :: r, g, b
    call gr_plot_close()

    call gr_plot_open(area=colour_show)
    call plbop
    call plvpor(0._plflt, 1._plflt, 0._plflt, 1._plflt)

    call gr_custom_line(r, g, b)
    
    call plfill([0._plflt, 1._plflt, 1._plflt, 0._plflt], &
         & [0._plflt, 0._plflt, 1._plflt, 1._plflt])

  end subroutine gr_set_clw

  subroutine gr_col_update(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    integer(kind=c_int), pointer :: index
    integer(kind=int16) :: cival
    
    call c_f_pointer(data, index)

    
    cival = int(hl_gtk_spin_button_get_value(widget), int16)

    select case (index)
    case(1)
       rnew = cival
    case(2)
       gnew = cival
    case(3)
       bnew = cival
    end select
    
    call gr_custom_line(rnew, gnew, bnew)
    
    call plfill([0._plflt, 1._plflt, 1._plflt, 0._plflt], &
         & [0._plflt, 0._plflt, 1._plflt, 1._plflt])
  end subroutine gr_col_update

  subroutine gr_col_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    logical, pointer :: apply

    call c_f_pointer(data, apply)

    call gtk_widget_destroy(colour_window)

    update_colour = apply
  end subroutine gr_col_quit
end module gr_colour_widgets
