! Copyright (C) 2013-2021
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

module gr_general_comment_widgets
  ! Widget to add a global comment to a Graffer file.

  use iso_c_binding
  use iso_fortran_env

  use gtk_hl
  use gtk_sup
  use gtk, only: gtk_container_add, gtk_widget_destroy, gtk_widget_show_all, &
       & TRUE, FALSE

  use graff_types
  use graff_globals
  use graff_init

  implicit none

  type(c_ptr), private :: comm_window, comm_view

contains
  subroutine gr_comment_menu

    ! Add comments to the file.

    type(c_ptr) :: base, junk, jb, sbox
    logical, dimension(2), target :: iapply = [.false., .true.]

    comm_window = hl_gtk_window_new("Remarks"//c_null_char, &
         & destroy=c_funloc(gr_comment_quit), data_destroy=c_loc(iapply(1)), &
         & parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(comm_window, base)

    comm_view = hl_gtk_text_view_new(sbox, ssize=[600_c_int, 300_c_int])
    if (allocated(pdefs%remarks)) call hl_gtk_text_view_insert(comm_view, &
         & pdefs%remarks, replace=TRUE)

    call hl_gtk_box_pack(base, sbox)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    junk = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_comment_quit), data=c_loc(iapply(2)))
    call hl_gtk_box_pack(jb, junk)

    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_comment_quit), data=c_loc(iapply(1)))
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(comm_window)

  end subroutine gr_comment_menu

  recursive subroutine gr_comment_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Quit adding comments.

    logical, pointer :: apply
    integer(kind=c_int) :: nch

    call c_f_pointer(data, apply)

    if (apply) then
       call hl_gtk_text_view_get_info(comm_view, nchars=nch)
       if (allocated(pdefs%remarks)) deallocate(pdefs%remarks)

       if (nch /= 0) call hl_gtk_text_view_get_text(comm_view, pdefs%remarks)
       call gr_set_changed(.true.)
    end if

    call gtk_widget_destroy(comm_window)

  end subroutine gr_comment_quit
end module gr_general_comment_widgets
