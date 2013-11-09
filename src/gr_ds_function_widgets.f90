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

module gr_ds_function_widgets
  ! Widgets & handlers to define functions.

  use iso_fortran_env
  use iso_c_binding

  use gtk_hl
  use gtk, only: gtk_container_add, gtk_label_new, &
       & gtk_notebook_set_current_page, gtk_widget_destroy, &
       & gtk_widget_show_all, TRUE, FALSE

  use graff_types
  use graff_globals
  use gr_utils

  use gr_plot
  use gr_cb_common
  use gr_ds_tools

  implicit none

  type(c_ptr), private :: fun_window
  type(c_ptr), dimension(2), private :: fun_entry, neval_spin
  type(c_ptr), dimension(2,2), private :: fun_range_entry
  integer(kind=int16), private :: fun_type

contains
  subroutine gr_ds_fun_menu(type)
    integer(kind=int16), intent(in) :: type

    ! Input / edit a function dataset

    logical, dimension(2), target :: iapply = [.false., .true.]
    type(c_ptr) :: base, jb, junk
    type(graff_data), pointer :: data
    integer(kind=c_int) :: iy
    character(len=32) :: text

    data => pdefs%data(pdefs%cset)
    if (data%ndata == 0) data%ndata = 25
    if (data%ndata2 == 0 .and. type == -4 ) data%ndata2 = 25

    fun_type = type

    fun_window = hl_gtk_window_new("Define a function"//c_null_char,&
         & destroy=c_funloc(gr_ds_fun_quit), data_destroy=c_loc(iapply(1)), &
         & parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(fun_window, base)

    jb = hl_gtk_table_new()
    call hl_gtk_box_pack(base, jb, expand=FALSE)
    select case (type)
    case(-1)
       junk = gtk_label_new("Function y=f(x) :"//c_null_char)
    case(-2)
       junk = gtk_label_new("Function x=f(y) :"//c_null_char)
    case(-3)
       junk = gtk_label_new("Function x=f(t) :"//c_null_char)
    case(-4)
       junk = gtk_label_new("Function z=f(x,y) :"//c_null_char)
    end select
    call hl_gtk_table_attach(jb, junk, 0_c_int, 0_c_int, xopts=0)

    fun_entry(1) = hl_gtk_entry_new(value=trim(data%funct%funct(1))//&
         & c_null_char)
    call hl_gtk_table_attach(jb, fun_entry(1), 1_c_int, 0_c_int, xspan=2)

    if (type == -3) then
       junk = gtk_label_new("Function y=f(t) :"//c_null_char)
       call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int, xopts=0)

       fun_entry(2) = hl_gtk_entry_new(value=trim(data%funct%funct(2))//&
            & c_null_char)
       call hl_gtk_table_attach(jb, fun_entry(2), 1_c_int, 1_c_int, xspan=2)
       iy = 1_c_int
    else
       iy = 0_c_int
    end if

    select case (type)
    case(-1)
       junk = gtk_label_new("X range:"//c_null_char)
    case(-2)
       junk = gtk_label_new("Y range:"//c_null_char)
    case(-3)
       junk = gtk_label_new("T range:"//c_null_char)
    case(-4)
       junk = gtk_label_new("X range:"//c_null_char)
    end select
    call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int+iy, xopts=0)

    write(text, "(g0.5)") data%funct%range(1,1)
    fun_range_entry(1,1) = hl_gtk_entry_new(value=trim(text)//c_null_char)
    call hl_gtk_table_attach(jb, fun_range_entry(1,1), 1_c_int, 1_c_int+iy)

    write(text, "(g0.5)") data%funct%range(2,1)
    fun_range_entry(2,1) = hl_gtk_entry_new(value=trim(text)//c_null_char)
    call hl_gtk_table_attach(jb, fun_range_entry(2,1), 2_c_int, 1_c_int+iy)

    if (type == -4) then
       junk = gtk_label_new("Y range:"//c_null_char)
       call hl_gtk_table_attach(jb, junk, 0_c_int, 2_c_int+iy, xopts=0)
       write(text, "(g0.5)") data%funct%range(1,2)
       fun_range_entry(1,2) = hl_gtk_entry_new(value=trim(text)//c_null_char)
       call hl_gtk_table_attach(jb, fun_range_entry(1,2), 1_c_int, 2_c_int+iy)

       write(text, "(g0.5)") data%funct%range(2,2)
       fun_range_entry(2,2) = hl_gtk_entry_new(value=trim(text)//c_null_char)
       call hl_gtk_table_attach(jb, fun_range_entry(2,2), 2_c_int, 2_c_int+iy)
    end if

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)
    junk = gtk_label_new("Number of evaluations:"//c_null_char)
    call hl_gtk_box_pack(jb, junk, expand=FALSE)

    neval_spin(1) = hl_gtk_spin_button_new(1_c_int, 10000_c_int, &
         & initial_value = int(data%ndata))
    call hl_gtk_box_pack(jb, neval_spin(1), expand=FALSE)
    if (type == -4) then
       neval_spin(2) = hl_gtk_spin_button_new(1_c_int, 10000_c_int, &
            & initial_value = int(data%ndata2))
       call hl_gtk_box_pack(jb, neval_spin(2), expand=FALSE)
    end if

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)
    junk = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_ds_fun_quit), data=c_loc(iapply(2)))
    call hl_gtk_box_pack(jb, junk)
    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_ds_fun_quit), data=c_loc(iapply(1)))
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(fun_window)
  end subroutine gr_ds_fun_menu

  recursive subroutine gr_ds_fun_quit(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    ! Quit function editor

    logical, pointer :: apply
    type(graff_data), pointer :: data
    integer :: i, ios
    character(len=32) :: text
    real(kind=real64) :: v

    call c_f_pointer(gdata, apply)

    if (apply) then
       data => pdefs%data(pdefs%cset)

       data%type = fun_type

       call hl_gtk_entry_get_text(fun_entry(1), text=data%funct%funct(1))
       data%ndata = int(hl_gtk_spin_button_get_value(neval_spin(1)), int32)

       if (fun_type == -3) then
          call hl_gtk_entry_get_text(fun_entry(2), text=data%funct%funct(2))
       else
          data%funct%funct(2) = ''
       end if
       if (fun_type == -4) then
          data%ndata2 = int(hl_gtk_spin_button_get_value(neval_spin(2)), int32)
       else
          data%ndata2 = 0_int32
       end if

       do i = 1, 2
          call hl_gtk_entry_get_text(fun_range_entry(i,1), text=text)
          read(text, *, iostat=ios) v
          if (ios == 0) data%funct%range(i,1) = v
          if (fun_type == -4) then
             call hl_gtk_entry_get_text(fun_range_entry(i,2), text=text)
             read(text, *, iostat=ios) v
             if (ios == 0) data%funct%range(i,2) = v
          end if
       end do
       data%funct%evaluated = .false.

       if (data%type == -4 .or. data%type == 9) then
          call  gtk_notebook_set_current_page(display_nb, 1)
       else
          call  gtk_notebook_set_current_page(display_nb, 0)
       end if
       call gr_plot_draw(.true.)
    end if

    call gtk_widget_destroy(fun_window)

  end subroutine gr_ds_fun_quit

end module gr_ds_function_widgets
