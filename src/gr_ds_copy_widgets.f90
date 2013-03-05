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

module gr_ds_copy_widgets
  use iso_fortran_env
  use iso_c_binding

  use gtk_hl
  use gtk, only: gtk_container_add, gtk_widget_destroy, &
       & gtk_widget_set_sensitive, gtk_widget_show_all, TRUE, FALSE, &
       & GTK_POLICY_NEVER

  use graff_types
  use graff_globals
  use gr_utils

  use gr_plot
  use gr_cb_common
  use gr_ds_tools

  implicit none

  type(c_ptr), private :: cf_window, cf_list, cf_dobut
  integer(kind=int16), private :: cf_from
  logical, private :: copy_active

contains
  subroutine gr_ds_copy_from_menu(class)
    integer, intent(in) :: class

    ! Menu for dataset copying

    logical, dimension(2), target :: iapply = [.false., .true.]
    type(c_ptr) :: base, jb, junk, sbox
    integer(kind=c_int) :: i, j

    copy_active = .false.

    cf_window = hl_gtk_window_new("Copy from dataset"//c_null_char, &
         & destroy=c_funloc(gr_ds_cf_quit), data_destroy=c_loc(iapply(1)), &
         & parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(cf_window, base)

    cf_list = hl_gtk_listn_new(sbox, hscroll_policy=GTK_POLICY_NEVER, &
         & changed=c_funloc(gr_ds_cf_pick), &
         & titles = [character(len=11) :: &
         & 'DS #', 'Type', 'N', 'N2', 'Description'], &
         &  types = [g_type_int, g_type_string, g_type_int, &
         & g_type_int, g_type_string], height=300_c_int)

    call hl_gtk_box_pack(base, sbox)

    j = 0
    do i = 1_c_int, int(pdefs%nsets, c_int)
       if (i == pdefs%cset) cycle
       select case (class)
       case(1)
          if (pdefs%data(i)%type < 0 .or. pdefs%data(i)%type == 9) cycle
       case(2)
          if (pdefs%data(i)%type /= 9) cycle
       case(3)
          if (pdefs%data(i)%type >= 0) cycle
       end select
       call hl_gtk_listn_ins(cf_list)
       call hl_gtk_listn_set_cell(cf_list, j, 0, ivalue=i)
       call hl_gtk_listn_set_cell(cf_list, j, 1, &
            & svalue = trim(typecodes(pdefs%data(i)%type))//c_null_char)
       call hl_gtk_listn_set_cell(cf_list, j, 2, &
            & ivalue = int(pdefs%data(i)%ndata, c_int))
       call hl_gtk_listn_set_cell(cf_list, j, 3, &
            & ivalue = int(pdefs%data(i)%ndata2, c_int))
       call hl_gtk_listn_set_cell(cf_list, j, 4, &
            & svalue = trim(pdefs%data(i)%descript)//c_null_char)
       j = j+1
    end do

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)
    cf_dobut = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_ds_cf_quit), data=c_loc(iapply(2)), &
         & sensitive=FALSE)
    call hl_gtk_box_pack(jb, cf_dobut)

    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_ds_cf_quit), data=c_loc(iapply(1)))
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(cf_window)
    call hl_gtk_listn_set_selection(cf_list, -1_c_int)

    copy_active = .true.

  end subroutine gr_ds_copy_from_menu

  subroutine gr_ds_cf_pick(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Callback to select source

    integer(kind=c_int), dimension(:), allocatable :: isel
    integer(kind=c_int) :: nsel, ifrom

    if (.not. copy_active) return

    nsel = hl_gtk_listn_get_selections(c_null_ptr, indices=isel, &
         & selection=widget)

    if (nsel == 1) then
       call hl_gtk_listn_get_cell(cf_list, isel(1), 0_c_int, ivalue=ifrom)
       cf_from = int(ifrom, int16)
       call gtk_widget_set_sensitive(cf_dobut, TRUE)
    else
       call gtk_widget_set_sensitive(cf_dobut, FALSE)
    end if
  end subroutine gr_ds_cf_pick

  recursive subroutine gr_ds_cf_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Quit copying menus

    logical, pointer :: apply

    call c_f_pointer(data, apply)

    if (apply) then
       call gr_ds_copy(from=cf_from, to=pdefs%cset)
       call gr_plot_draw(.true.)
    end if

    call gtk_widget_destroy(cf_window)
  end subroutine gr_ds_cf_quit
end module gr_ds_copy_widgets
