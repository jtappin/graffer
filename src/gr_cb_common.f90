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

! Callback routines that may be needed in several modules.
! Also widgets that need to be accessed outside their own module

module gr_cb_common
  ! Event handlers that are shared between multiple widgets.
  ! Or which need access to widgets that are from a different
  ! part of the code.

  use iso_c_binding
  use iso_fortran_env

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_check_menu_item_set_active, gtk_combo_box_set_active, &
       & gtk_dialog_add_button, gtk_dialog_run, gtk_entry_set_text, &
       & gtk_main_quit, gtk_notebook_set_current_page, &
       & gtk_toggle_button_get_active, gtk_toggle_button_set_active, &
       & gtk_widget_destroy, gtk_widget_hide, gtk_widget_set_sensitive, &
       & gtk_widget_set_tooltip_text, gtk_widget_show_all, &
       & gtk_window_set_title, TRUE, GTK_RESPONSE_DELETE_EVENT, &
       & GTK_RESPONSE_CANCEL, GTK_RESPONSE_YES, GTK_RESPONSE_NO, &
       & GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE, GTK_MESSAGE_ERROR

  use graff_types
  use graff_globals
  use gr_utils
  use gr_file
  use gr_plot
  use graff_version
  use gr_msg

  use ieee_arithmetic, only: ieee_is_finite
  
  use plplot, only: pi => pl_pi

  implicit none

  ! Widgets (N.B. The top-level window and the drawing area are in globals)

  ! Top - level gui

  type(c_ptr) :: y2_tab, y2_check
  type(c_ptr) :: name_id                ! The file name display
  type(c_ptr) :: display_nb             ! The notebook for 1d/2d

  ! 1 D dataset options

  type(c_ptr) :: colour_cbo, symbol_cbo, style_cbo, join_cbo, thick_ent, &
       & size_ent, csys_cbo, xsort_id, clip_id, mouse_id, min_ent, max_ent

  integer(kind=c_int) :: custom_colour_index
  
  ! 2 D dataset options

  type(c_ptr) :: fmt_nbook

  type(c_ptr) :: clevel_cbo, cldist_cbo, cfmt_cbo, clevel_view, &
       & ccol_view, csty_view, &
       & clevels_entry, cthick_view, clabel_entry, &
       & clabel_off_entry, cchsize_entry

  type(c_ptr) :: cg_table_pick=c_null_ptr, cg_missing_entry, & 
       & cg_gamma_entry, cg_invert_but, cg_smooth_but, &
       & gc_smooth_l_sb, cg_log_cbo

  type(c_ptr), dimension(2) :: cg_range_entry

  ! Axis options

  type(c_ptr), dimension(3) :: lbox, log_chb, &
       & exact_chb, ext_chb, ax_chb, bax_chb, minor_chb, ann_chb, time_chb, &
       & origin_grp, grid_grp, rot_chb

  type(c_ptr), dimension(2,3) :: rbox

  ! DS data

  type(c_ptr) :: ds_y_axis_cbo, ds_rescale_id

  ! DS selector

  type(c_ptr) :: ds_name_id, ds_idx_id, ds_as_data_id

  ! General

  type(c_ptr) :: title_box, subtitle_box, charsize_spin, linewidth_spin

  ! Mode

  type(c_ptr), dimension(2) :: cursor_position

  ! Control to inactivate callbacks when updating programatically

  logical :: gui_active

contains

  subroutine gr_save_cb(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Callback routine to save a Graffer file.

    logical, pointer :: current
    integer(kind=c_int) :: ipick
    character(len=256), dimension(:), allocatable :: files
    logical :: ok, ascii

    call c_f_pointer(data, current)

    if (.not. current) then
       ipick = hl_gtk_file_chooser_show(files, &
            & filter=["*.grf"], filter_name=["Graffer Files"], &
            & edit_filters=TRUE, create=TRUE, confirm_overwrite=TRUE, &
            & parent=gr_window, initial_dir=trim(pdefs%dir)//c_null_char, &
            & initial_file=trim(pdefs%name)//c_null_char, &
            & title="Save Graffer file"//c_null_char)
       if (.not. c_f_logical(ipick)) return
       ascii = .false.
       call split_fname(files(1), pdefs%name, pdefs%dir)
    else
       ascii = pdefs%is_ascii
    end if

    call gr_write(ok, ascii=ascii)
    call gr_set_values_global(.true.)
    if (ok) call gr_set_changed(.false.)

  end subroutine gr_save_cb

  subroutine gr_save_asc_cb(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Callback routine to save a Graffer file (forced ASCII mode)

    integer(kind=c_int) :: ipick
    character(len=256), dimension(:), allocatable :: files
    logical :: ok

    ipick = hl_gtk_file_chooser_show(files, &
         & filter=["*.grf"], filter_name=["Graffer Files"], &
         & edit_filters=TRUE, create=TRUE, confirm_overwrite=TRUE, &
         & parent=gr_window, initial_dir=trim(pdefs%dir)//c_null_char, &
         & initial_file=trim(pdefs%name)//c_null_char, &
         & title="Save Graffer file"//c_null_char)
    if (.not. c_f_logical(ipick)) return
    call split_fname(files(1), pdefs%name, pdefs%dir)

    call gr_write(ok, ascii=.true.)
    call gr_set_values_global(.true.)
    if (ok) call gr_set_changed(.false.)

  end subroutine gr_save_asc_cb

  recursive subroutine gr_exit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Callback to quit Graffer

    integer(kind=c_int) :: iresp
    type(c_ptr) :: unsaved_msg, junk
    logical :: ok

    if (pdefs%chflag) then
       unsaved_msg = hl_gtk_message_dialog_new( &
            & ["The current Graffer object is unsaved", &
            &  "Do you want to save it?              "], &
            & GTK_BUTTONS_NONE, type=GTK_MESSAGE_QUESTION, &
            & parent=gr_window, title="Unsaved file"//c_null_char)

       junk = gtk_dialog_add_button(unsaved_msg, &
            & "Save"//c_null_char, GTK_RESPONSE_YES)
       call gtk_widget_set_tooltip_text(junk, &
            & "Save the file and then exit"//c_null_char)

       junk = gtk_dialog_add_button(unsaved_msg, &
            & "Discard"//c_null_char, GTK_RESPONSE_NO)
       call gtk_widget_set_tooltip_text(junk, &
            & "Exit, discarding any changes"//c_null_char)

       junk = gtk_dialog_add_button(unsaved_msg, &
            & "Cancel"//c_null_char, GTK_RESPONSE_CANCEL)
       call gtk_widget_set_tooltip_text(junk, &
            & "Do not exit"//c_null_char)

       call gtk_widget_show_all(unsaved_msg)
       iresp = gtk_dialog_run(unsaved_msg)
       call gtk_widget_destroy(unsaved_msg)

       select case(iresp)
       case(GTK_RESPONSE_CANCEL, GTK_RESPONSE_DELETE_EVENT)
          return
       case(GTK_RESPONSE_YES)
          call gr_write(ok)
       case(GTK_RESPONSE_NO)
          call gr_set_changed(.false.)
       case default
          call gr_message("gr_exit: Invalid response code", &
               & type=GTK_MESSAGE_ERROR)
          return
       end select
    end if

    call gtk_main_quit()
    call gtk_widget_destroy(gr_window)

  end subroutine gr_exit

  subroutine gr_enable_y2(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Callback to enable/disable secondary Y-axis.

    integer(kind=c_int) :: isel

    if (.not. gui_active) return

    isel = gtk_toggle_button_get_active(widget)
    call gtk_widget_set_sensitive(y2_tab, isel)
    call gtk_widget_set_sensitive(ds_y_axis_cbo, isel)

    pdefs%y_right = c_f_logical(isel)

    call gr_plot_draw(.true.)

  end subroutine gr_enable_y2

  subroutine gr_redraw_cb(widget, page, npage, data) bind(c)
    type(c_ptr), value :: widget, page, data
    integer(kind=c_int), value :: npage

    if (.not. gui_active) return

    call gr_plot_draw(.false.)

  end subroutine gr_redraw_cb

  subroutine gr_set_values_global(minimal)
    logical, optional, intent(in) :: minimal

    ! Set the widget values for global properties.

    integer :: i, j
    character(len=32) :: text
    character(len=8) :: gr_sversion
    logical :: log_valid
    
    gui_active = .false.

    call graffer_version%string(gr_sversion)
    call gtk_entry_set_text(name_id, &
         & trim(pdefs%dir)//trim(pdefs%name)//c_null_char)
    call gtk_window_set_title(gr_window, "Graffer V"//trim(gr_sversion)//&
         & ": "//trim(pdefs%dir)//trim(pdefs%name)//c_null_char)

    if (present(minimal)) then
       if (minimal) then
          gui_active = .true.
          return
       end if
    end if

    call gtk_entry_set_text(title_box, trim(pdefs%title)//c_null_char)
    call gtk_entry_set_text(subtitle_box, trim(pdefs%subtitle//c_null_char))
    call hl_gtk_spin_button_set_value(charsize_spin, &
         & real(pdefs%charsize, c_double))
    call hl_gtk_spin_button_set_value(linewidth_spin, &
         & real(pdefs%axthick, c_double))

    log_valid = all(pdefs%data%mode == 0)
    if (.not. log_valid) pdefs%axtype(:) = 0
    
    do i = 1, 3
       call gtk_entry_set_text(lbox(i), trim(pdefs%axtitle(i))//c_null_char)
       do j = 1, 2
          write(text, "(1pg0.5)") pdefs%axrange(j,i)
          call gtk_entry_set_text(rbox(j,i), trim(text)//c_null_char)
       end do

       call gtk_check_menu_item_set_active(log_chb(i), &
            & int(pdefs%axtype(i), c_int))
       call gtk_widget_set_sensitive(log_chb(i), f_c_logical(log_valid))

       call gtk_check_menu_item_set_active(exact_chb(i), &
            & f_c_logical(btest(pdefs%axsty(i)%idl, exact_bit)))
       call gtk_check_menu_item_set_active(ext_chb(i), &
            & f_c_logical(btest(pdefs%axsty(i)%idl, extend_bit)))
       call gtk_check_menu_item_set_active(ax_chb(i), &
            & f_c_logical(btest(pdefs%axsty(i)%idl, axis_bit)))
       call gtk_check_menu_item_set_active(bax_chb(i), &
            & f_c_logical(btest(pdefs%axsty(i)%idl, box_bit)))

       call gtk_check_menu_item_set_active(minor_chb(i), &
            & f_c_logical(pdefs%axsty(i)%minor /= 0))

       call gtk_check_menu_item_set_active(ann_chb(i), &
            & f_c_logical(btest(pdefs%axsty(i)%extra, annot_bit)))
       if (i /= 1) call gtk_check_menu_item_set_active(rot_chb(i), &
            & f_c_logical(btest(pdefs%axsty(i)%extra, yrot_bit)))

       call gtk_check_menu_item_set_active(time_chb(i), &
            & f_c_logical(btest(pdefs%axsty(i)%time, time_bit)))

       select case(iand(pdefs%axsty(i)%extra, 10_int16))
       case(0,8)
          call hl_gtk_radio_menu_group_set_select(origin_grp(i), 0)
       case(2)
          call hl_gtk_radio_menu_group_set_select(origin_grp(i), 1)
       case(10)
          call hl_gtk_radio_menu_group_set_select(origin_grp(i), 2)
       end select

       call hl_gtk_radio_menu_group_set_select(grid_grp(i), &
            & int(pdefs%axsty(i)%grid, c_int))
    end do

    call gtk_toggle_button_set_active(y2_check, &
         &f_c_logical(pdefs%y_right))

    call gtk_widget_set_sensitive(ds_y_axis_cbo, f_c_logical(pdefs%y_right))
    call hl_gtk_spin_button_set_range_int(ds_idx_id, &
         & upper=int(pdefs%nsets, c_int))

    gui_active = .true.

  end subroutine gr_set_values_global

  subroutine gr_set_values_dataset(select)
    integer(kind=int16), intent(in), optional :: select

    ! Set the widget values for dataset-specific properties.

    type(graff_data), pointer :: data
    character(len=32), dimension(:), allocatable :: vtext
    character(len=32) :: stext
    integer :: i

    gui_active = .false.

    if (present(select)) pdefs%cset = select

    data => pdefs%data(pdefs%cset)

    if (data%type == -4 .or. data%type == 9) then
       call  gtk_notebook_set_current_page(display_nb, 1)
    else
       call  gtk_notebook_set_current_page(display_nb, 0)
    end if

    if (data%colour == -2) then
       call gtk_combo_box_set_active(colour_cbo, custom_colour_index)
    else
       call gtk_combo_box_set_active(colour_cbo, &
            & int(data%colour, c_int)+1_c_int)
    end if
    call gtk_combo_box_set_active(symbol_cbo, int(data%psym, c_int))
    call gtk_combo_box_set_active(style_cbo, int(data%line, c_int))
    call gtk_combo_box_set_active(join_cbo, int(data%pline, c_int))
    call gtk_combo_box_set_active(csys_cbo, int(data%mode, c_int))

    call hl_gtk_spin_button_set_value(thick_ent, real(data%thick, c_double))
    call hl_gtk_spin_button_set_value(size_ent, real(data%symsize, c_double))

    if (ieee_is_finite(data%min_val)) then
       write(stext, "(1pg0.5)") data%min_val
       call gtk_entry_set_text(min_ent, trim(stext)//c_null_char)
    else
       call gtk_entry_set_text(min_ent, c_null_char)
    end if
    if (ieee_is_finite(data%max_val)) then
       write(stext, "(1pg0.5)") data%max_val
       call gtk_entry_set_text(max_ent, trim(stext)//c_null_char)
    else
       call gtk_entry_set_text(max_ent, c_null_char)
    end if
    
    call gtk_check_menu_item_set_active(xsort_id, f_c_logical(data%sort))
    call gtk_check_menu_item_set_active(clip_id, f_c_logical(.not. data%noclip))
    call gtk_check_menu_item_set_active(mouse_id, f_c_logical(data%medit))

    call gtk_notebook_set_current_page(fmt_nbook, &
         & int(data%zdata%format, c_int))

    call gtk_combo_box_set_active(clevel_cbo, &
         & f_c_logical(data%zdata%set_levels))
    call gtk_combo_box_set_active(cldist_cbo, &
         & int(data%zdata%lmap, c_int))
    call gtk_combo_box_set_active(cfmt_cbo,&
         & int(data%zdata%fill, c_int))
    call hl_gtk_spin_button_set_value(clevels_entry, int(data%zdata%n_levels))
    call gtk_widget_set_sensitive(clevels_entry, &
         & f_c_logical(.not. data%zdata%set_levels))
     call gtk_widget_set_sensitive(cldist_cbo, &
         & f_c_logical(.not. data%zdata%set_levels))
    call gtk_widget_set_sensitive(clevel_view, &
         & f_c_logical(data%zdata%set_levels))
    
    if (allocated(data%zdata%levels)) then
       allocate(vtext(size(data%zdata%levels)))
       write(vtext, "(1pg0.5)") data%zdata%levels
       call hl_gtk_text_view_insert(clevel_view, vtext, replace=TRUE)
       deallocate(vtext)
    else
       call hl_gtk_text_view_insert(clevel_view, [''], replace=TRUE)
    end if
    if (allocated(data%zdata%style)) then
       allocate(vtext(size(data%zdata%style)))
       write(vtext, "(i0)") data%zdata%style
       call hl_gtk_text_view_insert(csty_view, vtext, replace=TRUE)
       deallocate(vtext)
    else
       call hl_gtk_text_view_insert(csty_view, [''], replace=TRUE)
    end if
    if (allocated(data%zdata%colours)) then
       allocate(vtext(size(data%zdata%colours)))
       write(vtext, "(i0)") data%zdata%colours
       call hl_gtk_text_view_insert(ccol_view, vtext, replace=TRUE)
       deallocate(vtext)
    else
       call hl_gtk_text_view_insert(ccol_view, [''], replace=TRUE)
    end if
    if (allocated(data%zdata%thick)) then
       allocate(vtext(size(data%zdata%thick)))
       write(vtext, "(1pg0.5)") data%zdata%thick
       call hl_gtk_text_view_insert(cthick_view, vtext, replace=TRUE)
       deallocate(vtext)
    else
       call hl_gtk_text_view_insert(cthick_view, [''], replace=TRUE)
    end if

    call hl_gtk_spin_button_set_value(clabel_entry, &
         & int(data%zdata%label, c_int))
 
    call hl_gtk_spin_button_set_value(clabel_off_entry, &
         & int(data%zdata%label_off, c_int))
    call hl_gtk_spin_button_set_value(cchsize_entry, &
         & real(data%zdata%charsize, c_double))

    call hl_gtk_listn_set_selection(cg_table_pick, &
         & int(data%zdata%ctable, c_int))

    write(stext, "(1pg0.5)") data%zdata%missing
    call gtk_entry_set_text(cg_missing_entry, trim(stext)//c_null_char)

    call hl_gtk_spin_button_set_value(cg_gamma_entry, &
         & real(data%zdata%gamma, c_double))

    call gtk_combo_box_set_active(cg_log_cbo, int(data%zdata%ilog, c_int))
    call gtk_toggle_button_set_active(cg_invert_but, &
         & f_c_logical(data%zdata%invert))
    call gtk_toggle_button_set_active(cg_smooth_but, &
         & f_c_logical(data%zdata%smooth))
    call gtk_widget_set_sensitive(gc_smooth_l_sb, &
         &  f_c_logical(data%zdata%smooth))
    
    call hl_gtk_spin_button_set_value(gc_smooth_l_sb, &
         &  int(data%zdata%shade_levels, c_int))
    
    do i = 1, 2
       write(stext, "(1pg0.5)") data%zdata%range(i)
       call gtk_entry_set_text(cg_range_entry(i), trim(stext)//c_null_char)
    end do

    call gtk_combo_box_set_active(ds_y_axis_cbo, int(data%y_axis, c_int))
    call gtk_widget_set_sensitive(ds_rescale_id, &
         & f_c_logical(data%type >= 0))

    call gtk_entry_set_text(ds_name_id, trim(data%descript)//c_null_char)
    call hl_gtk_spin_button_set_value(ds_idx_id, int(pdefs%cset, c_int))
    call gtk_widget_set_sensitive(ds_as_data_id, f_c_logical(data%type < 0))

    call gr_ds_device

    call gr_draw_tips

    gui_active = .true.

  end subroutine gr_set_values_dataset

  subroutine gr_ds_device

    ! Convert the points of the current dataset to device coordinates
    ! (for mouse editing etc.)

    type(graff_data), pointer :: data
    real(kind=real64), dimension(:), allocatable :: x, y
    real(kind=real64) :: scale
    integer :: i

    data => pdefs%data(pdefs%cset)

    if (allocated(pdefs%transient%x_dev)) deallocate(pdefs%transient%x_dev)
    if (allocated(pdefs%transient%y_dev)) deallocate(pdefs%transient%y_dev)

    if (data%type < 0 .or. data%type == 9) return
    if (data%ndata == 0) return

    allocate(x(data%ndata), y(data%ndata))

    if (data%mode == 0) then
       x = data%xydata(1,:)
       y = data%xydata(2,:)
    else
       if (data%mode == 1) then
          scale = 1._real64
       else
          scale = pi / 180._real64
       end if

       x = data%xydata(1,:) * cos(data%xydata(2,:)*scale)
       y = data%xydata(1,:) * sin(data%xydata(2,:)*scale)
    end if

    allocate(pdefs%transient%x_dev(data%ndata), &
         & pdefs%transient%y_dev(data%ndata))

    do i = 1, data%ndata
       call gr_plot_coords_w_d(x(i), y(i), pdefs%transient%x_dev(i), &
            &  pdefs%transient%y_dev(i))
    end do
  end subroutine gr_ds_device

  function gr_auto_save(data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: data

    ! Make an autosave file.

    logical :: ok

    if (pdefs%transient%changes > 0) then
       call gr_write(ok, auto=.TRUE.)
       pdefs%transient%changes = 0
    end if

    rv = TRUE
  end function gr_auto_save

  subroutine gr_draw_tips

    ! Adjust the tooltips on the drawing area

    type(graff_data), pointer :: data

    if (.not. c_associated(gr_drawing_area)) return

    if (pdefs%transient%mode == 1) then
       call gtk_widget_set_tooltip_text(gr_drawing_area, &
            & "Left = Add new annotation"//c_new_line//&
            & "Middle = Edit an existing annotation"//c_new_line//&
            & "Right = Delete an annotation"//c_null_char)
    else
       data => pdefs%data(pdefs%cset)

       if (data%type < 0 .or. data%type == 9 .or. .not. data%medit) then
          call gtk_widget_set_tooltip_text(gr_drawing_area, c_null_char)
       else
          call gtk_widget_set_tooltip_text(gr_drawing_area, &
               & 'Left = add point, Middle = edit point, Right = delete point' &
               & //c_new_line// &
               & 'C-Left = insert point in closest segment,'//c_new_line//&
               & 'S-Left = add at nearer end,'//c_new_line//&
               & 'C|S on any release = cancel.'//c_null_char)
       end if
    end if
  end subroutine gr_draw_tips

  subroutine gr_infobar_clr(widget, id, data) bind(c)
    type(c_ptr), value :: widget, data
    integer(kind=c_int), value :: id

    call gtk_widget_hide(widget)

  end subroutine gr_infobar_clr

end module gr_cb_common
