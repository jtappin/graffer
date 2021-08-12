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

module gr_ds_selector
  ! Basic widgets for selecting datasets (next, prev etc.)

  use iso_c_binding
  use iso_fortran_env

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_label_new, gtk_toggle_button_get_active, TRUE, FALSE, &
       & GTK_RESPONSE_YES, GTK_MESSAGE_QUESTION, GTK_ORIENTATION_HORIZONTAL, &
       & GTK_BUTTONS_YES_NO

  use graff_globals
  use graff_types
  use gr_cb_common
  use gr_ds_tools
  use gr_ds_widgets
  
  implicit none

contains

  function gr_ds_selector_new() result(fr)
    type(c_ptr) :: fr

    ! Define the dataset selection panel

    type(c_ptr) :: jb, mnu, smnu, junk
    integer(kind=int16), dimension(2), target :: direct = [-1_int16, 1_int16]
    character(len=32) :: typet
    
    fr = hl_gtk_box_new()

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(fr, jb, expand=FALSE)


    junk = hl_gtk_button_new("Next"//c_null_char, &
         & clicked=c_funloc(gr_ds_advance), data=c_loc(direct(2)), &
         & tooltip="Go to the next dataset"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    junk = hl_gtk_button_new("Previous"//c_null_char, &
         & clicked=c_funloc(gr_ds_advance), data=c_loc(direct(1)), &
         & tooltip="Go to the previous dataset"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    junk = hl_gtk_button_new("New"//c_null_char, &
         & clicked=c_funloc(gr_ds_new_cb), &
         & tooltip="Create a new dataset"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    mnu = hl_gtk_menu_new(orientation=GTK_ORIENTATION_HORIZONTAL)
    call hl_gtk_box_pack(jb, mnu)

    smnu = hl_gtk_menu_submenu_new(mnu, "Other ▼"//c_null_char, &
         & tooltip="Other dataset options"//c_null_char)

    ds_rescale_id = hl_gtk_menu_item_new(smnu, "Rescale Current"//c_null_char, &
         & activate=c_funloc(gr_ds_rescale_cb), &
         & tooltip="Scale and/or shift the current dataset"//c_null_char, &
         & sensitive=f_c_logical(pdefs%data(pdefs%cset)%type >= 0))

    ds_transpose_id =  hl_gtk_menu_item_new(smnu, "Transpose"//c_null_char, &
         & activate=c_funloc(gr_ds_transpose_cb), &
         & tooltip="Exchange X & Y axes of the current dataset"//c_null_char, &
         & sensitive=f_c_logical(pdefs%data(pdefs%cset)%type >= 0))
    
    junk = hl_gtk_menu_item_new(smnu, "Erase"//c_null_char, &
         & activate=c_funloc(gr_ds_erase_cb), tooltip=&
         & "Erase the data of the current dataset"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Erase all"//c_null_char, &
         & activate=c_funloc(gr_ds_erase_all_cb), tooltip=&
         & "Erase the contents of the current dataset"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Delete"//c_null_char, &
         & activate=c_funloc(gr_ds_delete_cb), tooltip=&
         & "Delete the current dataset"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Write..."//c_null_char, &
         & activate=c_funloc(gr_ds_write_cb), tooltip=&
         & "Write the current dataset to an ASCII file"//c_null_char)

    ds_as_data_id = hl_gtk_menu_item_new(smnu, &
         & "Write as data..."//c_null_char, &
         & activate=c_funloc(gr_ds_write_data_cb), tooltip=&
         & "Write the current dataset as data to an ASCII file"//c_null_char, &
         & sensitive=f_c_logical(pdefs%data(pdefs%cset)%type < 0))

    junk = hl_gtk_menu_item_new(smnu, "Copy"//c_null_char, &
         & activate=c_funloc(gr_ds_copy_cb), tooltip=&
         & "Copy the current dataset to a new one"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Select..."//c_null_char, &
         & activate=c_funloc(gr_ds_select_cb), tooltip=&
         & "Select a dataset"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Sort..."//c_null_char, &
         & activate=c_funloc(gr_ds_sort), tooltip=&
         & "Reorder the datasets"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Merge..."//c_null_char, &
         & activate=c_funloc(gr_ds_merge_cb), tooltip=&
         & "Merge two datasets"//c_null_char)



    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(fr, jb, expand=FALSE)

    junk = gtk_label_new("DS Name:"//c_null_char)
    call hl_gtk_box_pack(jb, junk, expand=FALSE)

    ds_name_id = hl_gtk_entry_new(editable=TRUE, &
         & activate=c_funloc(gr_ds_rename), size=100_c_int, &
         & focus_out_event=c_funloc(gr_ds_rename_e), &
         & value=trim(pdefs%data(pdefs%cset)%descript)//c_null_char)
    call hl_gtk_box_pack(jb, ds_name_id)

    junk = gtk_label_new("#:"//c_null_char)
    call hl_gtk_box_pack(jb, junk, expand=FALSE)

    ds_idx_id = hl_gtk_spin_button_new(1_c_int, int(pdefs%nsets, c_int), &
         & initial_value=int(pdefs%cset,c_int), &
         & value_changed=c_funloc(gr_ds_advance), tooltip=&
         & "Move through the available datasets"//c_null_char, &
         & wrap = TRUE)
    call hl_gtk_box_pack(jb, ds_idx_id)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(fr, jb, expand=FALSE)

    junk = gtk_label_new("Type:"//c_null_char)
    call hl_gtk_box_pack(jb, junk, expand=FALSE)
    
    ds_type_id =hl_gtk_entry_new(editable=FALSE, &
         & size=34_c_int, value = &
         & trim(typedescrs(pdefs%data(pdefs%cset)%type))//c_null_char)
    call hl_gtk_box_pack(jb, ds_type_id, expand=FALSE)
    
    junk = hl_gtk_check_button_new("Only show current DS"//c_null_char, &
         & toggled=c_funloc(gr_ds_current_only), initial_state=&
         & f_c_logical(transient%current_only), &
         & tooltip="Toggle display of only the current dataset"//c_null_char)
    call hl_gtk_box_pack(jb, junk, expand=FALSE)

  end function gr_ds_selector_new

  recursive subroutine gr_ds_advance(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Make a different dataset current

    integer(kind=int16), pointer :: direct
    integer(kind=int16) :: cnew

    if (.not. gui_active) return

    if (c_associated(data)) then
       call c_f_pointer(data, direct)
       cnew = pdefs%cset + direct
       if (cnew < 1) then
          cnew = pdefs%nsets
       else if (cnew > pdefs%nsets) then
          cnew = 1_int16
       end if
    else
       cnew = int(hl_gtk_spin_button_get_value(widget), int16)
    end if

    if (cnew /= pdefs%cset) then
       call gr_set_values_dataset(select = cnew)
       if (transient%current_only) call gr_plot_draw(.false.)
    end if
    
  end subroutine gr_ds_advance

  subroutine gr_ds_new_cb(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Create a new dataset

    call gr_ds_new(.true.)
    call gr_plot_draw(.true.)

  end subroutine gr_ds_new_cb

  subroutine gr_ds_select_cb(widget, data) bind(c)
    type(c_ptr), value :: widget, data
    
    ! Select a new current dataset

    call gr_ds_select
  
  end subroutine gr_ds_select_cb

  subroutine gr_ds_merge_cb(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Append one datset to another

    call gr_ds_merge
    
  end subroutine gr_ds_merge_cb


  subroutine gr_ds_sort(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Reorder datasets.

    call gr_ds_sort_menu

  end subroutine gr_ds_sort


  subroutine gr_ds_erase_cb(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Erase the current dataset

    integer(kind=c_int) :: iresp

    iresp = hl_gtk_message_dialog_show(&
         & ["This will destroy all data in", &
         &  "the current dataset          ",&
         &  "Do you want to continue?     "],&
         & GTK_BUTTONS_YES_NO, type=GTK_MESSAGE_QUESTION, &
         & parent=gr_window)

    if (iresp /= GTK_RESPONSE_YES) return

    call gr_ds_erase(data_only=.true.)
    call gr_plot_draw(.true.)

  end subroutine gr_ds_erase_cb
  
  subroutine gr_ds_erase_all_cb(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Erase the current dataset

    integer(kind=c_int) :: iresp

    iresp = hl_gtk_message_dialog_show(&
         & ["This will destroy all data and ", &
         &  "settings in the current dataset", &
         &  "Do you want to continue?       "],&
         & GTK_BUTTONS_YES_NO, type=GTK_MESSAGE_QUESTION, &
         & parent=gr_window)

    if (iresp /= GTK_RESPONSE_YES) return

    call gr_ds_erase(data_only=.false.)
    call gr_plot_draw(.true.)

  end subroutine gr_ds_erase_all_cb

  subroutine gr_ds_delete_cb(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Delete the current dataset.

    integer(kind=c_int) :: iresp

    iresp = hl_gtk_message_dialog_show(&
         & ["This will completely remove", &
         &  "the current dataset        ",&
         &  "Do you want to continue?   "],&
         & GTK_BUTTONS_YES_NO, type=GTK_MESSAGE_QUESTION, &
         & parent=gr_window)

    if (iresp /= GTK_RESPONSE_YES) return

    call gr_ds_delete
    call gr_plot_draw(.true.)

  end subroutine gr_ds_delete_cb

  subroutine gr_ds_write_cb(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Write the current dataset

    integer(kind=c_int) :: iresp
    character(len=256), dimension(:), allocatable :: files
    character(len=150) :: deffile
    integer :: idx

    deffile = pdefs%name
    idx = index(deffile, '.', back=.true.)
    if (idx == 0) idx = len_trim(deffile)+1

    write(deffile(idx:), "('_',I0,'.dat')") pdefs%cset-1


    iresp = hl_gtk_file_chooser_show(files, &
         & filter=['*.dat'], all=TRUE, edit_filters=TRUE, &
         & current=TRUE, create=TRUE, parent=gr_window, &
         & initial_file = trim(deffile)//c_null_char, &
         & initial_dir=trim(pdefs%dir)//c_null_char)

    if (c_f_logical(iresp)) call gr_ds_write(files(1))
  end subroutine gr_ds_write_cb

  subroutine gr_ds_write_data_cb(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Write the current (function) dataset as data values.
    integer(kind=c_int) :: iresp
    character(len=256), dimension(:), allocatable :: files
    character(len=150) :: deffile
    integer :: idx

    deffile = pdefs%name
    idx = index(deffile, '.', back=.true.)
    if (idx == 0) idx = len_trim(deffile)+1

    write(deffile(idx:), "('_',I0,'.dat')") pdefs%cset-1

    iresp = hl_gtk_file_chooser_show(files, &
         & filter=['*.dat'], all=TRUE, edit_filters=TRUE, &
         & current=TRUE, create=TRUE, parent=gr_window, &
         & initial_file = trim(deffile)//c_null_char, &
         & initial_dir=trim(pdefs%dir)//c_null_char)

    if (c_f_logical(iresp)) call gr_ds_write(files(1), as_data=.true.)
  end subroutine gr_ds_write_data_cb

  subroutine gr_ds_copy_cb(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Copy the current dataset to a new one.

    integer(kind=int16) current_ds

    current_ds = pdefs%cset

    call gr_ds_new(.true.)
    call gr_ds_copy(current_ds, append=' (copy)')
    call gr_plot_draw(.true.)

  end subroutine gr_ds_copy_cb

  subroutine gr_ds_rename(widget, data) bind(c)
    type(c_ptr), value :: widget, data
    character(len=120) :: newdesc
    
    ! Change the name of the current dataset.

    call hl_gtk_entry_get_text(widget, newdesc)
    if (newdesc /= pdefs%data(pdefs%cset)%descript) then
       pdefs%data(pdefs%cset)%descript = newdesc
       call gr_plot_draw(.true.)
    end if
    
  end subroutine gr_ds_rename
  function gr_ds_rename_e(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, event, data

    call gr_ds_rename(widget, data)
    rv = FALSE
  end function gr_ds_rename_e

  subroutine gr_ds_current_only(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Toggle display of current dataset only.

    if (.not. gui_active) return

    transient%current_only = &
         & c_f_logical(gtk_toggle_button_get_active(widget))

    call gr_plot_draw(.false.)
  end subroutine gr_ds_current_only

  subroutine gr_ds_rescale_cb(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Scale/shift data.

    call gr_ds_rescale

  end subroutine gr_ds_rescale_cb

  subroutine gr_ds_transpose_cb(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Scale/shift data.

    call gr_ds_transpose

  end subroutine gr_ds_transpose_cb


end module gr_ds_selector
