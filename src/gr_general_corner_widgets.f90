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

module gr_general_corner_widgets
  ! Widgets to set the corners of the plto viewport.

  use iso_c_binding
  use iso_fortran_env

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_combo_box_get_active, gtk_container_add, gtk_frame_new, &
       & gtk_label_new, gtk_toggle_button_get_active, gtk_widget_destroy, &
       & gtk_widget_set_sensitive, gtk_widget_show_all, TRUE, FALSE


  use graff_types
  use graff_globals

  use gr_plot

  implicit none

  type(c_ptr), private :: corn_window, corn_c_frame, corn_a_frame, &
       & corn_iso_but
  type(c_ptr), dimension(4), private :: corn_corn_sb
  type(c_ptr), dimension(2), private :: corn_asp_sb
  integer(kind=c_int), private :: corn_method
  logical, private :: iso_flag

contains
  subroutine gr_corner_menu

    ! Set viewport for plotting

    type(c_ptr) :: base, jb, junk
    logical, dimension(2), target :: iapply = [.false., .true.]

    if (pdefs%aspect(1) <= 0. .and. pdefs%position(1) <= 0.) then
       corn_method = 0_c_int
    else if (pdefs%aspect(1) <= 0.) then
       corn_method = 1_c_int
    else
       corn_method = 2_c_int
    end if

    iso_flag = pdefs%isotropic

    corn_window = hl_gtk_window_new("Viewport specification"//c_null_char, &
         & destroy=c_funloc(gr_corn_quit), data_destroy=c_loc(iapply(1)), &
         & parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(corn_window, base)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb)

    junk = gtk_label_new("Determine position by:"//c_null_char)
    call hl_gtk_box_pack(jb, junk, expand=FALSE)

    junk = hl_gtk_combo_box_new(initial_choices=&
         & ['Automatic   ', 'Corners     ', 'Aspect Ratio'], &
         & changed=c_funloc(gr_corn_method), &
         & active=corn_method, tooltip = &
         & "Select how to determine corners"//c_null_char)

    call hl_gtk_box_pack(jb, junk)

    corn_c_frame = gtk_frame_new("Set corner positions"//c_null_char)
    call gtk_widget_set_sensitive(corn_c_frame, &
         &f_c_logical(corn_method == 1 .and. .not. pdefs%isotropic))

    call hl_gtk_box_pack(base, corn_c_frame)

    jb = hl_gtk_table_new()
    call gtk_container_add(corn_c_frame, jb)

    junk = gtk_label_new("Lower Left: X:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 0_c_int)

    corn_corn_sb(1) = hl_gtk_spin_button_new(0._c_double, 1._c_double, &
         & 0.001_c_double, initial_value=real(pdefs%position(1), c_double), &
         & tooltip = &
         & "Set the X coordinate of the lower left corner of the viewport"&
         & //c_null_char)
    call hl_gtk_table_attach(jb, corn_corn_sb(1), 1_c_int, 0_c_int)

    junk = gtk_label_new("Y:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 2_c_int, 0_c_int)

    corn_corn_sb(2) = hl_gtk_spin_button_new(0._c_double, 1._c_double, &
         & 0.001_c_double, initial_value=real(pdefs%position(2), c_double), &
         & tooltip = &
         & "Set the Y coordinate of the lower left corner of the viewport"&
         & //c_null_char)
    call hl_gtk_table_attach(jb, corn_corn_sb(2), 3_c_int, 0_c_int)

    junk = gtk_label_new("Upper Right: X:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int)

    corn_corn_sb(3) = hl_gtk_spin_button_new(0._c_double, 1._c_double, &
         & 0.001_c_double, initial_value=real(pdefs%position(3), c_double), &
         & tooltip = &
         & "Set the X coordinate of the upper right corner of the viewport"&
         & //c_null_char)
    call hl_gtk_table_attach(jb, corn_corn_sb(3), 1_c_int, 1_c_int)

    junk = gtk_label_new("Y:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 2_c_int, 1_c_int)

    corn_corn_sb(4) = hl_gtk_spin_button_new(0._c_double, 1._c_double, &
         & 0.001_c_double, initial_value=real(pdefs%position(4), c_double), &
         & tooltip = &
         & "Set the Y coordinate of the upper right corner of the viewport"&
         & //c_null_char)
    call hl_gtk_table_attach(jb, corn_corn_sb(4), 3_c_int, 1_c_int)

    corn_a_frame = gtk_frame_new("Set aspect ratio"//c_null_char)
    call gtk_widget_set_sensitive(corn_a_frame, &
         & f_c_logical(corn_method == 2 .and. .not. pdefs%isotropic))

    call hl_gtk_box_pack(base, corn_a_frame)

    jb = hl_gtk_table_new()
    call gtk_container_add(corn_a_frame, jb)

    junk = gtk_label_new("Aspect Ratio:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 0_c_int)

    corn_asp_sb(1) = hl_gtk_spin_button_new(0._c_double, 20._c_double, &
         & 0.001_c_double, initial_value=real(pdefs%aspect(1), c_double), &
         & tooltip="Set the aspect ratio of the viewport"//c_null_char)
    call hl_gtk_table_attach(jb, corn_asp_sb(1), 1_c_int, 0_c_int)

    junk = gtk_label_new("Margin:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 2_c_int, 0_c_int)

    corn_asp_sb(2) = hl_gtk_spin_button_new(0._c_double, 0.5_c_double, &
         & 0.001_c_double, initial_value=real(pdefs%aspect(2), c_double), &
         & tooltip="Set the minimum margin of the viewport"//c_null_char)
    call hl_gtk_table_attach(jb, corn_asp_sb(2), 3_c_int, 0_c_int)

    corn_iso_but = hl_gtk_check_button_new("Force isotropic axes"//&
         & c_null_char, toggled=c_funloc(gr_corn_iso), tooltip=&
         & "Toggle whether to force isotropic axis scaling"//c_null_char, &
         & initial_state=f_c_logical(pdefs%isotropic))

    call hl_gtk_box_pack(base, corn_iso_but, expand=FALSE)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    junk = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_corn_quit), data=c_loc(iapply(2)), &
         & tooltip="Apply the settings and destroy the menu"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_corn_quit), data=c_loc(iapply(1)), &
         & tooltip="Destroy the menu without applying the settings"//&
         & c_null_char)
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(corn_window)

  end subroutine gr_corner_menu

  recursive subroutine gr_corn_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Quit viewport setting

    logical, pointer :: apply
    integer :: i
    call c_f_pointer(data, apply)

    if (apply) then
       pdefs%isotropic = iso_flag

       if (.not. pdefs%isotropic) then
          select case(corn_method)
          case(0)
             pdefs%aspect = 0._real32
             pdefs%position = 0._real32
          case(1)
             pdefs%aspect = 0._real32
             do i = 1, 4
                pdefs%position(i) = &
                     & real(hl_gtk_spin_button_get_value(corn_corn_sb(i)), &
                     & real32)
             end do
          case(2)
             pdefs%position = 0._real32
             do i = 1, 2
                pdefs%aspect(i) = &
                     & real(hl_gtk_spin_button_get_value(corn_asp_sb(i)), &
                     & real32)
             end do
          end select
       end if

       call gr_plot_draw(.true.)
    end if

    call gtk_widget_destroy(corn_window)

  end subroutine gr_corn_quit

  subroutine gr_corn_method(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Select method to define viewport

    corn_method = gtk_combo_box_get_active(widget)

    call gtk_widget_set_sensitive(corn_c_frame, &
         & f_c_logical(.not. iso_flag .and. corn_method == 1))
    call gtk_widget_set_sensitive(corn_a_frame, &
         & f_c_logical(.not. iso_flag .and. corn_method == 2))

  end subroutine gr_corn_method

  subroutine gr_corn_iso(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Toggle isotropic axes.

    iso_flag = c_f_logical(gtk_toggle_button_get_active(widget))

    call gtk_widget_set_sensitive(corn_c_frame, &
         & f_c_logical(.not. iso_flag .and. corn_method == 1))
    call gtk_widget_set_sensitive(corn_a_frame, &
         & f_c_logical(.not. iso_flag .and. corn_method == 2))

  end subroutine gr_corn_iso
end module gr_general_corner_widgets

    
