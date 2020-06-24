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

module  gr_ds_reorder_widgets
  ! Widgets & handlers to change the order of datasets.

  use iso_fortran_env
  use iso_c_binding

  use gtk_hl
  use gtk, only: gtk_container_add, gtk_label_new, gtk_label_set_text, &
       & gtk_widget_destroy, gtk_widget_set_sensitive, gtk_widget_show_all, &
       & TRUE, FALSE, GTK_POLICY_NEVER

  use graff_types
  use graff_globals
  use gr_utils

  use gr_plot
  use gr_cb_common
  use gr_ds_tools

  implicit none

  type(c_ptr), private :: sort_window, sort_list, sort_dobut, sort_action
  integer(kind=c_int), private :: sort_from, sort_to
  logical, private :: sort_active

contains
  subroutine gr_ds_sort_menu
    
    ! DS sorter menu

    type(c_ptr) :: base, junk, jb, sbox
    logical, target, dimension(2) :: iapply = [.false., .true.]
    integer :: i

    sort_from = -1_c_int
    sort_to = -1_c_int
    sort_active = .false.

    sort_window = hl_gtk_window_new("Dataset sorter"//c_null_char, &
         & destroy=c_funloc(gr_ds_sort_quit), &
         & parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(sort_window, base)

    sort_action = gtk_label_new("Dataset to move"//c_null_char)
    call hl_gtk_box_pack(base, sort_action, expand=FALSE)

    sort_list = hl_gtk_listn_new(sbox, hscroll_policy=GTK_POLICY_NEVER, &
         & titles = [character(len=11) :: &
         & 'DS #', 'Type', 'N', 'N2', 'Description'], &
         &  types = [g_type_int, g_type_string, g_type_int, &
         & g_type_int, g_type_string], height=300_c_int, &
         & changed=c_funloc(gr_ds_sort_pick))

    call hl_gtk_box_pack(base, sbox)

    call hl_gtk_listn_ins(sort_list, count=int(pdefs%nsets, c_int))

    do i = 1_c_int, int(pdefs%nsets, c_int)
       call hl_gtk_listn_set_cell(sort_list, i-1, 0, ivalue=i)
       call hl_gtk_listn_set_cell(sort_list, i-1, 1, &
            & svalue = trim(typecodes(pdefs%data(i)%type))//c_null_char)
       call hl_gtk_listn_set_cell(sort_list, i-1, 2, &
            & ivalue = int(pdefs%data(i)%ndata, c_int))
       if (pdefs%data(i)%type == 9 .or. pdefs%data(i)%type == -4) &
            & call hl_gtk_listn_set_cell(sort_list, i-1, 3, &
            & ivalue = int(pdefs%data(i)%ndata2, c_int))
       call hl_gtk_listn_set_cell(sort_list, i-1, 4, &
            & svalue = trim(pdefs%data(i)%descript)//c_null_char)
    end do

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    sort_dobut = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_ds_sort_do), data=c_loc(iapply(2)), &
         & sensitive=FALSE)
    call hl_gtk_box_pack(jb, sort_dobut)

    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_ds_sort_do), data=c_loc(iapply(1)))
    call hl_gtk_box_pack(jb, junk)

    junk = hl_gtk_button_new("Quit"//c_null_char, &
         & clicked=c_funloc(gr_ds_sort_quit))
    call hl_gtk_box_pack(base, junk, expand=FALSE)

    call gtk_widget_show_all(sort_window)
    call hl_gtk_listn_set_selection(sort_list, -1_c_int)

    sort_active = .true.

  end subroutine gr_ds_sort_menu

  recursive subroutine gr_ds_sort_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Quit sorter

    call gtk_widget_destroy(sort_window)

  end subroutine gr_ds_sort_quit

  recursive subroutine gr_ds_sort_pick(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Actions on selecting DS to move

    integer(kind=c_int), dimension(:), allocatable :: isel
    integer(kind=c_int) :: nsel, index

    if (.not. sort_active) return

    sort_active = .false.

    nsel = hl_gtk_listn_get_selections(c_null_ptr, &
         & selection=widget, indices=isel)
    if (nsel /= 1) return

    call hl_gtk_listn_get_cell(sort_list, isel(1), 0, ivalue=index)

    if (sort_from < 0) then
       sort_from = index
       call hl_gtk_listn_rem(sort_list, row=isel(1))
       call hl_gtk_listn_ins(sort_list, 0_c_int)
       call hl_gtk_listn_set_cell(sort_list, 0_c_int, 0_c_int, ivalue=0_c_int)
       call hl_gtk_listn_set_cell(sort_list, 0_c_int, 1_c_int, &
            & svalue = "<Dummy>"//c_null_char)
       call hl_gtk_listn_set_cell(sort_list, 0_c_int, 4_c_int, &
            & svalue = "Beginning"//c_null_char)
       call gtk_label_set_text(sort_action, "Place after"//c_null_char)
    else
       sort_to = index
       call gtk_widget_set_sensitive(sort_dobut, TRUE)
    end if

    sort_active = .true.
  end subroutine gr_ds_sort_pick

  subroutine gr_ds_sort_do(widget, data) bind(c)
    type(c_ptr), value :: widget, data
    
    ! Apply the change.

    logical, pointer :: apply
    integer(kind=c_int) :: i
    integer :: kidx
    integer(kind=int32), dimension(:), allocatable :: ktmp
    
    call c_f_pointer(data, apply)

    if (apply) then
       call gr_ds_move(int(sort_from, int16), int(sort_to, int16))
       if (allocated(pdefs%key%list)) then
          kidx = first(pdefs%key%list == sort_from)
          if (kidx > 0) then
             pdefs%key%list(kidx) = sort_to
             allocate(ktmp(size(pdefs%key%list)))
             call sort(pdefs%key%list, ktmp)
             pdefs%key%list=ktmp
             deallocate(ktmp)
          end if
       end if
    end if

    sort_active = .false.

    call hl_gtk_listn_rem(sort_list)
    call hl_gtk_listn_ins(sort_list, count=int(pdefs%nsets, c_int))

    do i = 1_c_int, int(pdefs%nsets, c_int)
       call hl_gtk_listn_set_cell(sort_list, i-1, 0, ivalue=i)
       call hl_gtk_listn_set_cell(sort_list, i-1, 1, &
            & svalue = trim(typecodes(pdefs%data(i)%type))//c_null_char)
       call hl_gtk_listn_set_cell(sort_list, i-1, 2, &
            & ivalue = int(pdefs%data(i)%ndata, c_int))
       if (pdefs%data(i)%type == 9 .or. pdefs%data(i)%type == -4) &
            & call hl_gtk_listn_set_cell(sort_list, i-1, 3, &
            & ivalue = int(pdefs%data(i)%ndata2, c_int))
       call hl_gtk_listn_set_cell(sort_list, i-1, 4, &
            & svalue = trim(pdefs%data(i)%descript)//c_null_char)
    end do
    call hl_gtk_listn_set_selection(sort_list, -1_c_int)
    call gtk_label_set_text(sort_action, "Dataset to move"//c_null_char)
    call gtk_widget_set_sensitive(sort_dobut, FALSE)

    call gr_plot_draw(.true.)

    sort_from = -1
    sort_to = -1
    sort_active = .true.
  end subroutine gr_ds_sort_do

end module gr_ds_reorder_widgets
