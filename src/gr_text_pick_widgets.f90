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

module gr_text_pick_widgets

  use iso_c_binding
  use iso_fortran_env

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_container_add, gtk_main, gtk_main_level, gtk_main_quit, &
       & gtk_widget_destroy, gtk_widget_show_all, TRUE, FALSE, &
       & GTK_POLICY_NEVER

  use graff_types
  use graff_globals

  implicit none

  type(c_ptr), private :: tp_window, tp_pick
  integer(kind=c_int), private :: selection

contains

  function gr_text_pick() result(isel)
    integer :: isel

    ! Pick a text annotation

    type(c_ptr) :: base, jb, junk, sbox
    logical, dimension(2), target :: iapply = [.false., .true.]
    integer(kind=c_int) :: i
    type(graff_text), pointer :: text
    character(len=*), dimension(3), parameter :: systems = ["World     ", &
         & "Normalized", "Viewport  "]

    selection = -1

    tp_window = hl_gtk_window_new("Select annotation"//c_null_char, &
         & destroy=c_funloc(gr_tp_quit), data_destroy=c_loc(iapply(1)), &
         & parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(tp_window, base)

    tp_pick = hl_gtk_listn_new(sbox, types=[g_type_int, g_type_string, &
         & g_type_string, g_type_double, g_type_double, g_type_string], &
         & titles=["#     ", "ID    ", "System", "X     ", "Y     ", &
         & "Text  "], hscroll_policy=GTK_POLICY_NEVER, height=300_c_int)

    call hl_gtk_listn_ins(tp_pick, count=int(pdefs%ntext, c_int))
    do i = 1, pdefs%ntext
       text => pdefs%text(i)

       call hl_gtk_listn_set_cell(tp_pick, i-1_c_int, 0_c_int, ivalue=i)
       call hl_gtk_listn_set_cell(tp_pick, i-1_c_int, 1_c_int, &
            & svalue=trim(text%id)//c_null_char)
       call hl_gtk_listn_set_cell(tp_pick, i-1_c_int, 2_c_int, &
            & svalue=trim(systems(text%norm+1))//c_null_char)
       call hl_gtk_listn_set_cell(tp_pick, i-1_c_int, 3_c_int, &
            & dvalue=text%x)
       call hl_gtk_listn_set_cell(tp_pick, i-1_c_int, 4_c_int, &
            & dvalue=text%y)
       call hl_gtk_listn_set_cell(tp_pick, i-1_c_int, 5_c_int, &
            & svalue=trim(text%text)//c_null_char)
    end do

    call hl_gtk_box_pack(base, sbox)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    junk = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_tp_quit), data=c_loc(iapply(2)))
    call hl_gtk_box_pack(jb, junk)
    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_tp_quit), data=c_loc(iapply(1)))
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(tp_window)
    call hl_gtk_listn_set_selection(tp_pick)

    ! May need this?
    call gtk_main()

    isel = selection+1
  end function gr_text_pick

  recursive subroutine gr_tp_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    logical, pointer :: apply
    integer(kind=c_int) :: nsel
    integer(kind=c_int), dimension(:), allocatable :: isel

    call c_f_pointer(data, apply)

    if (apply) then
       nsel = hl_gtk_listn_get_selections(tp_pick, indices = isel)
       if (nsel == 1) then
          selection = isel(1)
       else
          selection = -1
       end if
    end if
    call gtk_widget_destroy(tp_window)
    if (gtk_main_level() > 1) call gtk_main_quit()

  end subroutine gr_tp_quit
end module gr_text_pick_widgets
