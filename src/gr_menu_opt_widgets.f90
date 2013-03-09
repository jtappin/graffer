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

module gr_menu_opt_widgets
  use iso_fortran_env
  use iso_c_binding

  use gtk_hl
  use gtk_sup

  use g, only: g_find_program_in_path

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
       & opt_mouse_but, opt_ffam_cbo, opt_fnt_cbo, opt_delete_f_but
  character(len=16), dimension(:), allocatable, private :: viewnames

contains
  subroutine gr_options_menu

    ! Menus to configure global options.

    type(c_ptr) :: base, jb, junk, views
    integer :: iviewer, i
    logical, dimension(2), target :: iapply = [.false., .true.]

    if (.not. allocated(viewnames)) call gr_find_viewers(viewnames)
    iviewer = 0
    if (pdefs%opts%pdfviewer /= '') then
       do i = 1, size(viewnames)
          if (pdefs%opts%pdfviewer == viewnames(i)) then
             iviewer = i
             exit
          end if
       end do
       if (iviewer == 0) then
          views = g_find_program_in_path(trim(pdefs%opts%pdfviewer)//&
               & c_null_char)
          if (c_associated(views)) iviewer = size(viewnames)+1
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
         & trim(pdefs%opts%pdfviewer)//c_null_char)

    call gtk_combo_box_set_active(opt_view_cbo, iviewer-1)
    call hl_gtk_box_pack(jb, opt_view_cbo)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    opt_2d_but = hl_gtk_check_button_new("Hide 2-D datasets"//c_null_char,&
         & initial_state=f_c_logical(pdefs%opts%s2d), tooltip=&
         & "Toggle whether to show or hide 2-D datasets"//c_new_line//&
         & "(useful if rendering is slow)"//c_null_char)
    call hl_gtk_box_pack(jb, opt_2d_but)

    opt_mouse_but = hl_gtk_check_button_new("Mouse editing"//c_null_char, &
         & initial_state=f_c_logical(pdefs%opts%mouse), tooltip=&
         & "Toggle whether mouse editing of data is allowed by default"//&
         & c_null_char)
    call hl_gtk_box_pack(jb, opt_mouse_but)

    opt_delete_f_but = hl_gtk_check_button_new("Delete funct files"&
         & //c_null_char,&
         & initial_state=f_c_logical(pdefs%opts%delete_function_files), &
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

  recursive subroutine gr_opt_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Quit global option settings.

    logical, pointer :: apply
    integer(kind=c_int) :: vsel

    call c_f_pointer(data, apply)

    if (apply) then
       pdefs%opts%s2d = c_f_logical(gtk_toggle_button_get_active(opt_2d_but))
       pdefs%opts%mouse = &
            & c_f_logical(gtk_toggle_button_get_active(opt_mouse_but))
       pdefs%opts%delete_function_files = &
            & c_f_logical(gtk_toggle_button_get_active(opt_delete_f_but))

       vsel = hl_gtk_combo_box_get_active(opt_view_cbo, &
            & ftext=pdefs%opts%pdfviewer)
       pdefs%hardset%font_family = &
            & int(gtk_combo_box_get_active(opt_ffam_cbo), int16)+1_int16
       pdefs%hardset%font_wg_sl  = &
            & int(gtk_combo_box_get_active(opt_fnt_cbo), int16)+1_int16

       call gr_plot_draw(.true.)
    end if

    call gtk_widget_destroy(opt_window)

  end subroutine gr_opt_quit

end module gr_menu_opt_widgets
