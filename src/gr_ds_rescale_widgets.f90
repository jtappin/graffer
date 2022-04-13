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

module gr_ds_rescale_widgets
  ! Widgets & handlers to scale and/or shift datasets.

  use iso_fortran_env
  use iso_c_binding

  use gtk_hl
  use gtk, only: gtk_container_add, gtk_entry_set_text, gtk_label_new, &
       & gtk_widget_destroy, gtk_widget_show_all, TRUE, FALSE, &
       & GTK_MESSAGE_WARNING, GTK_BUTTONS_OK

  use graff_types
  use graff_globals
  use gr_utils

  use gr_plot
  use gr_ds_tools
  use gr_msg

  implicit none

  type(c_ptr), private :: rs_window
  type(c_ptr), private, dimension(3) :: rs_shifts, rs_scales, rs_divide

contains
  subroutine gr_ds_rescale

    ! Shift/scale a dataset.

    logical, dimension(2), target :: iapply = [.false., .true.]
    type(c_ptr) :: base, junk, jb
    integer(kind=c_int) :: iresp

    if (pdefs%data(pdefs%cset)%ndata == 0) then
       iresp = hl_gtk_message_dialog_show( &
            & ["EMPTY DATASET                        ",&
            &  "The current dataset does not contain ",&
            &  "any data, cannot rescale or shift it."], &
            & GTK_BUTTONS_OK, title="Empty dataset"//c_null_char, &
            & type=GTK_MESSAGE_WARNING, parent=gr_window)
       return
    end if

    rs_window = hl_gtk_window_new("Rescale dataset"//c_null_char, &
         & destroy=c_funloc(gr_ds_rs_quit), data_destroy=c_loc(iapply(1)), &
         & parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(rs_window, base)

    jb = hl_gtk_table_new()
    call hl_gtk_box_pack(base, jb)

    junk = gtk_label_new("X: Scaling"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 0_c_int)
    rs_scales(1) = hl_gtk_entry_new(editable=TRUE, tooltip = &
         & "Set a scaling factor for the X values"//c_null_char)
    call hl_gtk_table_attach(jb, rs_scales(1), 1_c_int, 0_c_int)

    rs_divide(1) = hl_gtk_check_button_new("Divide"//c_null_char, &
         & initial_state = FALSE, tooltip = &
         & "Check to divide rather than multiply the values"//c_null_char)
    call hl_gtk_table_attach(jb, rs_divide(1), 2_c_int, 0_c_int)

    junk = gtk_label_new("Shift"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 3_c_int, 0_c_int)
    rs_shifts(1) = hl_gtk_entry_new(editable=TRUE, tooltip = &
         & "Set a shift for the X values"//c_null_char)
    call hl_gtk_table_attach(jb, rs_shifts(1), 4_c_int, 0_c_int)

    junk = gtk_label_new("Y: Scaling"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int)
    rs_scales(2) = hl_gtk_entry_new(editable=TRUE, tooltip = &
         & "Set a scaling factor for the Y values"//c_null_char)
    call hl_gtk_table_attach(jb, rs_scales(2), 1_c_int, 1_c_int)

    rs_divide(2) = hl_gtk_check_button_new("Divide"//c_null_char, &
         & initial_state = FALSE, tooltip = &
         & "Check to divide rather than multiply the values"//c_null_char)
    call hl_gtk_table_attach(jb, rs_divide(2), 2_c_int, 1_c_int)

    junk = gtk_label_new("Shift"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 3_c_int, 1_c_int)
    rs_shifts(2) = hl_gtk_entry_new(editable=TRUE, tooltip = &
         & "Set a shift for the Y values"//c_null_char)
    call hl_gtk_table_attach(jb, rs_shifts(2), 4_c_int, 1_c_int)

    if (pdefs%data(pdefs%cset)%type == 9) then
       junk = gtk_label_new("Z: Scaling"//c_null_char)
       call hl_gtk_table_attach(jb, junk, 0_c_int, 2_c_int)
       rs_scales(3) = hl_gtk_entry_new(editable=TRUE, tooltip = &
            & "Set a scaling factor for the Z values"//c_null_char)
       call hl_gtk_table_attach(jb, rs_scales(3), 1_c_int, 2_c_int)

       rs_divide(3) = hl_gtk_check_button_new("Divide"//c_null_char, &
            & initial_state = FALSE, tooltip = &
            & "Check to divide rather than multiply the values"//c_null_char)
       call hl_gtk_table_attach(jb, rs_divide(3), 2_c_int, 2_c_int)

       junk = gtk_label_new("Shift"//c_null_char)
       call hl_gtk_table_attach(jb, junk, 3_c_int, 2_c_int)
       rs_shifts(3) = hl_gtk_entry_new(editable=TRUE, tooltip = &
            & "Set a shift for the Z values"//c_null_char)
       call hl_gtk_table_attach(jb, rs_shifts(3), 4_c_int, 2_c_int)
    else
       rs_scales(3) = c_null_ptr
       rs_divide(3) = c_null_ptr
       rs_shifts(3) = c_null_ptr
    end if

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)
    junk = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_ds_rs_quit), data=c_loc(iapply(2)))
    call hl_gtk_box_pack(jb, junk)

    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_ds_rs_quit), data=c_loc(iapply(1)))
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(rs_window)

  end subroutine gr_ds_rescale

  recursive subroutine gr_ds_rs_quit(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    ! Quit DS scaling tool

    logical, pointer :: apply
    type(graff_data), pointer :: data
    logical :: ok
    real(kind=real64), dimension(3) :: scales, shifts
    logical, dimension(3) :: divide
    
    integer :: i, ios
    character(len=40) :: text
    character(len=120) :: iom
    character(len=160), dimension(2) :: err_string

    call c_f_pointer(gdata, apply)

    if (apply) then
       data => pdefs%data(pdefs%cset)
       ok = .true.

       scales = 1._real64
       shifts = 0._real64

       do i = 1, 3
          if (.not. c_associated(rs_scales(i))) cycle
          call hl_gtk_entry_get_text(rs_scales(i), text)
          divide(i) = c_f_logical(gtk_toggle_button_get_active(rs_divide(i)))
          
          if (len_trim(text) > 0) then
             read(text, *, iostat=ios, iomsg=iom) scales(i)
             if (ios /= 0) then
                write(err_string, "(A,i0/t10,a)") &
                     & "gr_ds_rs_quit: Error reading scale #", &
                     & i, trim(iom)
                call gr_message(err_string)
                ok = .false.
                call gtk_entry_set_text(rs_scales(i), "Invalid"//c_null_char)
             else if (divide(i)) then
                scales(i) = 1._real64/scales(i)
             end if
          end if
    
          call hl_gtk_entry_get_text(rs_shifts(i), text)
          if (len_trim(text) > 0) then
             read(text, *, iostat=ios, iomsg=iom) shifts(i)
             if (ios /= 0) then
                write(err_string, "(A,i0/t10,a)") &
                     & "gr_ds_rs_quit: Error reading scale #", &
                     & i, trim(iom)
                call gr_message(err_string)
                ok = .false.
                call gtk_entry_set_text(rs_shifts(i), "Invalid"//c_null_char)
             end if
          end if
       end do

       if (.not. ok) return

       if (data%type == 9) then
          if (scales(1) /= 1._real64) data%zdata%x = data%zdata%x * scales(1)
          if (scales(2) /= 1._real64) data%zdata%y = data%zdata%y * scales(2)
          if (scales(3) /= 1._real64) data%zdata%z = data%zdata%z * scales(3)

          if (shifts(1) /= 0._real64) data%zdata%x = data%zdata%x + shifts(1)
          if (shifts(2) /= 0._real64) data%zdata%y = data%zdata%y + shifts(2)
          if (shifts(3) /= 0._real64) data%zdata%z = data%zdata%z + shifts(3)
       else
          if (scales(1) /= 1._real64) data%xydata%x = &
               & data%xydata%x * scales(1)
          if (scales(2) /= 1._real64) data%xydata%y = &
               & data%xydata%y * scales(2)

          if (shifts(1) /= 0._real64) data%xydata%x = &
               & data%xydata%x + shifts(1)
          if (shifts(2) /= 0._real64) data%xydata%y = &
               & data%xydata%y + shifts(2)

          if (nx_errors(data%type) /= 0 .and. (scales(1) /= 1._real64) &
               & data%xydata%x_err = data%xydata%x_err * scales(1)
          if (ny_errors(data%type) /= 0 .and. scales(2) /= 1._real64) &
                data%xydata%y_err = data%xydata%y_err * scales(2)

       end if
       call gr_plot_draw(.true.)
    end if

    call gtk_widget_destroy(rs_window)

  end subroutine gr_ds_rs_quit
end module gr_ds_rescale_widgets
