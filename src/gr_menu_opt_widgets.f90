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

module gr_menu_opt_widgets
  ! Menus to configure global options.

  use iso_fortran_env
  use iso_c_binding

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_combo_box_get_active, gtk_combo_box_set_active, &
       & gtk_container_add, gtk_label_new, gtk_toggle_button_get_active, &
       & gtk_widget_destroy, gtk_widget_show_all, TRUE, FALSE

  use graff_types
  use graff_globals
  use gr_utils

  use gr_text_utils
  use gr_plot

  implicit none

  type(c_ptr), private :: opt_window, opt_view_cbo, opt_2d_but, &
       & opt_mouse_but, opt_ffam_cbo, opt_fnt_cbo, opt_delete_f_but, &
       & opt_gdl_entry
  character(len=16), dimension(:), allocatable, private :: viewnames

contains
  subroutine gr_options_menu

    type(c_ptr) :: base, jb, junk
    integer :: iviewer, i
    logical, dimension(2), target :: iapply = [.false., .true.]

    if (.not. allocated(viewnames)) call gr_find_viewers(viewnames)
    call gr_text_init

    iviewer = 0
    if (sysopts%pdfviewer /= '') then
       do i = 1, size(viewnames)
          if (sysopts%pdfviewer == viewnames(i)) then
             iviewer = i
             exit
          end if
       end do
       if (iviewer == 0) then
         if (gr_find_program(sysopts%pdfviewer)) &
               & iviewer = size(viewnames)+1
       end if
    end if

    opt_window = hl_gtk_window_new("Graffer options"//c_null_char, &
         & destroy=c_funloc(gr_opt_quit), data_destroy=c_loc(iapply(1)), &
         & parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(opt_window, base)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    junk = gtk_label_new("PDF help viewer:"//c_null_char)
    call hl_gtk_box_pack(jb, junk, expand=FALSE)

    opt_view_cbo = hl_gtk_combo_box_new(has_entry=TRUE, &
         & initial_choices=viewnames, tooltip= &
         & "Select a viewer for PDF help files"//c_null_char)

    if (iviewer > size(viewnames)) &
         & call hl_gtk_combo_box_add_text(opt_view_cbo, &
         & trim(sysopts%pdfviewer)//c_null_char)

    call gtk_combo_box_set_active(opt_view_cbo, iviewer-1)
    call hl_gtk_box_pack(jb, opt_view_cbo)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    junk = gtk_label_new("GDL/IDL command:"//c_null_char)
    call hl_gtk_box_pack(jb, junk, expand=FALSE)

    opt_gdl_entry = hl_gtk_entry_new(value=&
         & gr_get_gdl_command()//c_null_char, &
         & changed = c_funloc(gr_opt_gdl), &
         & focus_out_event = c_funloc(gr_opt_gdl_e), &
         & tooltip="Set the command or path for the GDL or IDL interpreter" &
         & //c_null_char)
    call hl_gtk_box_pack(jb, opt_gdl_entry)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    opt_2d_but = hl_gtk_check_button_new("Hide 2-D datasets"//c_null_char,&
         & initial_state=f_c_logical(sysopts%s2d), tooltip=&
         & "Toggle whether to show or hide 2-D datasets"//c_new_line//&
         & "(useful if rendering is slow)"//c_null_char)
    call hl_gtk_box_pack(jb, opt_2d_but)

    opt_mouse_but = hl_gtk_check_button_new("Mouse editing"//c_null_char, &
         & initial_state=f_c_logical(sysopts%mouse), tooltip=&
         & "Toggle whether mouse editing of data is allowed by default"//&
         & c_null_char)
    call hl_gtk_box_pack(jb, opt_mouse_but)

    opt_delete_f_but = hl_gtk_check_button_new("Delete funct files"&
         & //c_null_char,&
         & initial_state=f_c_logical(sysopts%delete_function_files), &
         & tooltip="Select whether to delete temporary files used "//&
         & "in evaluating functions"//c_null_char)
    call hl_gtk_box_pack(jb, opt_delete_f_but)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    junk = gtk_label_new("Font type:"//c_null_char)
    call hl_gtk_box_pack(jb, junk, expand=FALSE)
    opt_ffam_cbo = hl_gtk_combo_box_new(initial_choices=font_names, &
         & active=int(pdefs%hardset%font_family, c_int)-1_c_int, &
         & tooltip="Select the font family for the default font"//c_null_char)
    call hl_gtk_box_pack(jb, opt_ffam_cbo)

    junk = gtk_label_new("Font style:"//c_null_char)
    call hl_gtk_box_pack(jb, junk, expand=FALSE)

    opt_fnt_cbo = hl_gtk_combo_box_new(initial_choices=font_styles, &
         & active=int(pdefs%hardset%font_wg_sl, c_int)-1_c_int, &
         & tooltip = "Select weight and shape for default font"//c_null_char)
    call hl_gtk_box_pack(jb, opt_fnt_cbo, 3_c_int, 7_c_int)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    junk = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_opt_quit), data=c_loc(iapply(2)), &
         & tooltip = "Apply the changes and remove the window"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_opt_quit), data=c_loc(iapply(1)), &
         & tooltip = "Remove the window, without applying the changes."&
         & //c_null_char)
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(opt_window)

  end subroutine gr_options_menu

  subroutine gr_opt_gdl(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    character(len=150) :: cline
    logical :: ok
    
    call hl_gtk_entry_get_text(widget, text=cline)
    ok = gr_have_gdl(cline)
    if (.not. ok) &
         & call gtk_entry_set_text(widget, gr_get_gdl_command()//c_null_char)
  end subroutine gr_opt_gdl

  function gr_opt_gdl_e(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, event, data

    call gr_opt_gdl(widget, data)

    rv = FALSE
  end function gr_opt_gdl_e
  
  recursive subroutine gr_opt_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Quit global option settings.

    logical, pointer :: apply
    integer(kind=c_int) :: vsel

    call c_f_pointer(data, apply)

    if (apply) then
       sysopts%s2d = c_f_logical(gtk_toggle_button_get_active(opt_2d_but))
       sysopts%mouse = &
            & c_f_logical(gtk_toggle_button_get_active(opt_mouse_but))
       sysopts%delete_function_files = &
            & c_f_logical(gtk_toggle_button_get_active(opt_delete_f_but))

       vsel = hl_gtk_combo_box_get_active(opt_view_cbo, &
            & ftext=sysopts%pdfviewer)
       pdefs%hardset%font_family = &
            & int(gtk_combo_box_get_active(opt_ffam_cbo), int16)+1_int16
       pdefs%hardset%font_wg_sl  = &
            & int(gtk_combo_box_get_active(opt_fnt_cbo), int16)+1_int16
  
       call gr_plot_draw(.true.)
    end if

    call gtk_widget_destroy(opt_window)

  end subroutine gr_opt_quit

end module gr_menu_opt_widgets
