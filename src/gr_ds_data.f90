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

module gr_ds_data
  use iso_fortran_env
  use iso_c_binding

  use gtk_hl
  use gtk_sup
  use gtk, only: gtk_combo_box_get_active, gtk_label_new, TRUE, FALSE, &
       & GTK_RESPONSE_YES, GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO

  use graff_types
  use graff_globals

  use gr_cb_common
  use gr_ds_tools
  use gr_ds_widgets

  implicit none

contains
  function gr_ds_data_new() result(base)
    type(c_ptr) :: base

    ! Define the dataset data modification panel

    type(c_ptr) :: mnu, smnu, jmnu, junk
    integer(kind=int16), dimension(4), target :: ftypes = &
         & [-1_int16, -2_int16, -3_int16, -4_int16]

    base = hl_gtk_box_new(horizontal=TRUE)

    mnu = hl_gtk_menu_new()
    call hl_gtk_box_pack(base, mnu, expand=FALSE)

    ! Regular data

    smnu = hl_gtk_menu_submenu_new(mnu, "XY Data ▼"//c_null_char, &
         & tooltip="Add/modify data values"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "From file ..."//c_null_char, &
         & activate=c_funloc(gr_ds_file), &
         & tooltip="Read data from a file"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Edit values ..."//c_null_char, &
         & activate=c_funloc(gr_ds_edit), &
         & tooltip="Edit the values in the current dataset"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Copy ..."//c_null_char, &
         & activate=c_funloc(gr_ds_copy_from), &
         & tooltip="Copy data from another dataset"//c_null_char)


    jmnu = hl_gtk_menu_submenu_new(smnu, "2D Datasets"//c_null_char, &
         & tooltip="Data for 2-D datasets"//c_null_char)

    junk = hl_gtk_menu_item_new(jmnu, "From file ..."//c_null_char, &
         & activate=c_funloc(gr_ds_file_2d), &
         & tooltip="Read data from a file"//c_null_char)

    junk = hl_gtk_menu_item_new(jmnu, "Copy ..."//c_null_char, &
         & activate=c_funloc(gr_ds_copy_from_2d), &
         & tooltip="Copy data from another dataset"//c_null_char)

    ds_rescale_id = hl_gtk_menu_item_new(smnu, "Rescale Current"//c_null_char, &
         & activate=c_funloc(gr_ds_rescale_cb), &
         & tooltip="Scale and/or shift the current dataset"//c_null_char, &
         & sensitive=f_c_logical(pdefs%data(pdefs%cset)%type >= 0))


    ! Functions

    smnu = hl_gtk_menu_submenu_new(mnu, "Function ▼"//c_null_char, &
         & tooltip="Set/edit functions"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "y = f(x) ..."//c_null_char, &
         & activate=c_funloc(gr_ds_fun), data=c_loc(ftypes(1)), &
         & tooltip="Function with Y as a function of X"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "x = f(y) ..."//c_null_char, &
         & activate=c_funloc(gr_ds_fun), data=c_loc(ftypes(2)), &
         & tooltip="Function with X as a function of Y"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "x = f(t), y = g(t) ..."//c_null_char, &
         & activate=c_funloc(gr_ds_fun), data=c_loc(ftypes(3)), &
         & tooltip="Function with X & Y as a functions of T"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "z = f(x,y) ..."//c_null_char, &
         & activate=c_funloc(gr_ds_fun), data=c_loc(ftypes(4)), &
         & tooltip="Function with Z as a function of X & Y"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "From file ..."//c_null_char, &
         & activate=c_funloc(gr_ds_file_fn), &
         & tooltip="Read function from a file"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Copy ..."//c_null_char, &
         & activate=c_funloc(gr_ds_copy_from_fn), &
         & tooltip="Copy function from another dataset"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Fit dataset ..."//c_null_char, &
         & activate=c_funloc(gr_ds_fit), &
         & tooltip="Make a fit to a dataset."//c_null_char)

    ! Axis selection

    junk = gtk_label_new("Y-axis:"//c_null_char)
    call hl_gtk_box_pack(base, junk, expand=FALSE)

    ds_y_axis_cbo = hl_gtk_combo_box_new(initial_choices=&
         & ["Main ","Right"], changed=c_funloc(gr_ds_pick_y), &
         & active=int(pdefs%data(pdefs%cset)%y_axis, c_int), &
         & tooltip="Select which Y axis to use"//c_null_char, &
         & sensitive=f_c_logical(pdefs%y_right))
    call hl_gtk_box_pack(base, ds_y_axis_cbo, expand=FALSE)

  end function gr_ds_data_new

  subroutine gr_ds_file(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    ! Read 1-D data from a file

    character(len=256), dimension(:), allocatable :: files
    type(graff_data), pointer :: data
    integer(kind=c_int) :: iresp

    data => pdefs%data(pdefs%cset)

    if ((data%type < 0 .or. data%type == 9) .and. data%ndata > 0) then

       iresp = hl_gtk_message_dialog_show(&
            & ['TYPE CHANGE                       ', &
            &  'Current data set is a function    ', &
            &  'or a 2-D dataset, reading 1-D data', &
            &  'will overwrite it.                ', &
            &  'Do you really want to do this?    '], &
            & GTK_BUTTONS_YES_NO, &
            & type=GTK_MESSAGE_QUESTION, title= &
            & "Overwrite warning"//c_null_char, &
            & parent = gr_window)

       if (iresp /= GTK_RESPONSE_YES) return

    end if

    iresp = hl_gtk_file_chooser_show(files, &
         & filter=['*.dat'], all=TRUE, edit_filters=TRUE, &
         & current=TRUE, create=FALSE, parent=gr_window)

    if (.not. c_f_logical(iresp)) return

    call gr_ds_xy_read(files(1))

  end subroutine gr_ds_file

  subroutine gr_ds_edit(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    ! Edit the data in the editor

    type(graff_data), pointer :: data
    integer(kind=c_int) :: iresp

    data => pdefs%data(pdefs%cset)

    if ((data%type < 0 .or. data%type == 9) .and. data%ndata > 0) then

       iresp = hl_gtk_message_dialog_show(&
            & ['TYPE CHANGE                       ', &
            &  'Current data set is a function    ', &
            &  'or a 2-D dataset, reading 1-D data', &
            &  'will overwrite it.                ', &
            &  'Do you really want to do this?    '], &
            & GTK_BUTTONS_YES_NO, &
            & type=GTK_MESSAGE_QUESTION, title= &
            & "Overwrite warning"//c_null_char, &
            & parent = gr_window)

       if (iresp /= GTK_RESPONSE_YES) return

    end if

    call gr_ds_editor

  end subroutine gr_ds_edit

  subroutine gr_ds_copy_from(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    ! Copy data from another dataset.

    type(graff_data), pointer :: data
    integer(kind=c_int) :: iresp

    data => pdefs%data(pdefs%cset)

    if ((data%type < 0 .or. data%type == 9) .and. data%ndata > 0) then

       iresp = hl_gtk_message_dialog_show(&
            & ['TYPE CHANGE                       ', &
            &  'Current data set is a function    ', &
            &  'or a 2-D dataset, reading 1-D data', &
            &  'will overwrite it.                ', &
            &  'Do you really want to do this?    '], &
            & GTK_BUTTONS_YES_NO, &
            & type=GTK_MESSAGE_QUESTION, title= &
            & "Overwrite warning"//c_null_char, &
            & parent = gr_window)

       if (iresp /= GTK_RESPONSE_YES) return
    end if

    call gr_ds_copy_from_menu(1)

  end subroutine gr_ds_copy_from

  subroutine gr_ds_file_2d(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    ! Read 2-D data from a file.

    character(len=256), dimension(:), allocatable :: files
    type(graff_data), pointer :: data
    integer(kind=c_int) :: iresp

    data => pdefs%data(pdefs%cset)

    if (data%type /= 9 .and. data%ndata > 0) then

       iresp = hl_gtk_message_dialog_show(&
            & ['TYPE CHANGE                       ', &
            &  'Current data set is a function    ', &
            &  'or a 1-D dataset, reading 2-D data', &
            &  'will overwrite it.                ', &
            &  'Do you really want to do this?    '], &
            & GTK_BUTTONS_YES_NO, &
            & type=GTK_MESSAGE_QUESTION, title= &
            & "Overwrite warning"//c_null_char, &
            & parent = gr_window)

       if (iresp /= GTK_RESPONSE_YES) return
    end if

    iresp = hl_gtk_file_chooser_show(files, &
         & filter=['*.dat'], all=TRUE, edit_filters=TRUE, &
         & current=TRUE, create=FALSE, parent=gr_window)

    if (.not. c_f_logical(iresp)) return

    call gr_ds_z_read(files(1))

  end subroutine gr_ds_file_2d

  subroutine gr_ds_copy_from_2d(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    ! Copy data from a 2-D dataset

    type(graff_data), pointer :: data
    integer(kind=c_int) :: iresp

    data => pdefs%data(pdefs%cset)

    if (data%type /= 9 .and. data%ndata > 0) then

       iresp = hl_gtk_message_dialog_show(&
            & ['TYPE CHANGE                       ', &
            &  'Current data set is a function    ', &
            &  'or a 2-D dataset, reading 1-D data', &
            &  'will overwrite it.                ', &
            &  'Do you really want to do this?    '], &
            & GTK_BUTTONS_YES_NO, &
            & type=GTK_MESSAGE_QUESTION, title= &
            & "Overwrite warning"//c_null_char, &
            & parent = gr_window)

       if (iresp /= GTK_RESPONSE_YES) return
    end if

    call gr_ds_copy_from_menu(2)

  end subroutine gr_ds_copy_from_2d

  subroutine gr_ds_rescale_cb(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Scale/shift data.

    call gr_ds_rescale

  end subroutine gr_ds_rescale_cb

  subroutine gr_ds_fun(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    ! Edit a function

    integer(kind=int16), pointer :: ftype
    type(graff_data), pointer :: data
    integer(kind=c_int) :: iresp

    call c_f_pointer(gdata, ftype)
    data => pdefs%data(pdefs%cset)

    if (data%type >= 0 .and. data%ndata > 0) then

       iresp = hl_gtk_message_dialog_show(&
            & ['TYPE CHANGE                       ', &
            &  'Current data set is not a function', &
            &  'reading function data             ', &
            &  'will overwrite it.                ', &
            &  'Do you really want to do this?    '], &
            & GTK_BUTTONS_YES_NO, &
            & type=GTK_MESSAGE_QUESTION, title= &
            & "Overwrite warning"//c_null_char, &
            & parent = gr_window)

       if (iresp /= GTK_RESPONSE_YES) return

    end if

    call gr_ds_fun_menu(ftype)

  end subroutine gr_ds_fun

  subroutine gr_ds_file_fn(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    ! Read a function from a file.

    type(graff_data), pointer :: data
    integer(kind=c_int) :: iresp
    character(len=256), dimension(:), allocatable :: files

    data => pdefs%data(pdefs%cset)

    if (data%type >= 0 .and. data%ndata > 0) then

       iresp = hl_gtk_message_dialog_show(&
            & ['TYPE CHANGE                       ', &
            &  'Current data set is not a function', &
            &  'reading function data             ', &
            &  'will overwrite it.                ', &
            &  'Do you really want to do this?    '], &
            & GTK_BUTTONS_YES_NO, &
            & type=GTK_MESSAGE_QUESTION, title= &
            & "Overwrite warning"//c_null_char, &
            & parent = gr_window)

       if (iresp /= GTK_RESPONSE_YES) return

    end if

    iresp = hl_gtk_file_chooser_show(files, &
         & filter=['*.dat'], all=TRUE, edit_filters=TRUE, &
         & current=TRUE, create=FALSE, parent=gr_window)

    if (.not. c_f_logical(iresp)) return

    call gr_ds_fun_read(files(1))

    call gr_plot_draw(.true.)

  end subroutine gr_ds_file_fn

  subroutine gr_ds_copy_from_fn(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    ! Copy from another function dataset.

    type(graff_data), pointer :: data
    integer(kind=c_int) :: iresp

    data => pdefs%data(pdefs%cset)

    if (data%type >= 0 .and. data%ndata > 0) then

       iresp = hl_gtk_message_dialog_show(&
            & ['TYPE CHANGE                         ', &
            &  'Current data set is not a function  ', &
            &  'copying a function will overwrite it', &
            &  'Do you really want to do this?      '], &
            & GTK_BUTTONS_YES_NO, &
            & type=GTK_MESSAGE_QUESTION, title= &
            & "Overwrite warning"//c_null_char, &
            & parent = gr_window)

       if (iresp /= GTK_RESPONSE_YES) return
    end if

    call gr_ds_copy_from_menu(3)

  end subroutine gr_ds_copy_from_fn

  subroutine gr_ds_fit(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    ! Do a fit to another dataset

    type(graff_data), pointer :: data
    integer(kind=c_int) :: iresp

    data => pdefs%data(pdefs%cset)

    if (data%type >= 0 .and. data%ndata > 0) then

       iresp = hl_gtk_message_dialog_show(&
            & ['TYPE CHANGE                         ', &
            &  'Current data set is not a function  ', &
            &  'copying a function will overwrite it', &
            &  'Do you really want to do this?      '], &
            & GTK_BUTTONS_YES_NO, &
            & type=GTK_MESSAGE_QUESTION, title= &
            & "Overwrite warning"//c_null_char, &
            & parent = gr_window)

       if (iresp /= GTK_RESPONSE_YES) return
    end if

    call gr_fit_menu

  end subroutine gr_ds_fit

  subroutine gr_ds_pick_y(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Select Y axis.

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%y_axis = &
         & int(gtk_combo_box_get_active(widget), int16)

    call gr_plot_draw(.true.)
  end subroutine gr_ds_pick_y

end module gr_ds_data
