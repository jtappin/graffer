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

module gr_text_widgets
  ! Widget for editing text annotations.

  use iso_fortran_env
  use iso_c_binding

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_combo_box_get_active, gtk_container_add, gtk_label_new, &
       & gtk_widget_destroy, gtk_widget_queue_draw, gtk_widget_show_all, TRUE, &
       & FALSE

  use plplot

  use graff_types
  use graff_globals
  use graff_init

  use gr_plot
  use gr_text_utils
  use gr_colours
  use gr_colour_widgets
  
  implicit none

  type(c_ptr), private :: text_window, text_draw, text_entry, text_id_entry, &
       & text_cs_sb, text_clr_cbo, text_algn_sb, text_sys_cbo, text_axis_cbo, &
       & text_ori_sb, text_ffam_cbo, text_fnt_cbo

  type(c_ptr), dimension(2), private :: text_xy_sb

  type(graff_text), pointer, private :: text
  logical, private :: is_new, text_ready
  integer(kind=int16), private :: csys
  real(kind=plflt), private :: csd

  character(len=15), dimension(29), private, parameter :: &
       & col_list = [character(len=15) :: &
       & 'White (bg)', 'Black', 'Red', 'Green', 'Blue', 'Cyan', &
       & 'Magenta', 'Yellow', 'Orange', '#7f ff 00', '#00 ff 7f', &
       & '#00 7f ff', '#7f 00 ff', 'Mauve', 'Dark Grey', 'Light Grey', &
       & 'Dark Red', 'Light Red', 'Dark Green', 'Light Green', 'Dark Blue', &
       & 'Light Blue', 'Dark Cyan', 'Light Cyan', 'Dark Magenta', &
       & 'Light Magenta', 'Dark Yellow', 'Light Yellow', 'Custom']

  integer, parameter, private :: ccindex=size(col_list)-1
  integer(kind=int16), private :: current_colour, c_red, c_green, c_blue

contains

  subroutine gr_text_menu(index, x, y)
    integer, intent(in), optional :: index
    real(kind=c_double), intent(in), optional :: x, y

    ! Editor for text annotations

    type(c_ptr) :: base, jb, junk, jbb
    logical, dimension(2), target :: iapply = [.false., .true.]
    real(kind=c_double), target, dimension(3) :: align=[0._c_double, &
         & 0.5_c_double, 1.0_c_double]
    real(kind=c_double) :: xs, ys
    real(kind=c_double) :: xcmin, xcmax, xcstep,  ycmin, ycmax, ycstep
    real(kind=plflt) :: css
    integer(kind=c_int) :: isel
    
    text_ready = .false.
    call gr_text_init

    call plgchr(csd, css)

    if (present(index)) then
       text => pdefs%text(index)
       is_new = .false.
    else
       allocate(text)
       call gr_pdefs_text_init(text)
       is_new = .true.
    end if

    if (present(x) .and. present(y)) then
       select case (text%norm)
       case(0)
          call gr_plot_coords_d_w(x, y, xs, ys)
       case(1)
          call gr_plot_coords_d_n(x, y, xs, ys)
       case(2)
          call gr_plot_coords_d_v(x, y, xs, ys)
       end select
    else 
       xs = text%x
       ys = text%y
    end if
    select case (text%norm)
    case(1)
       xcmin = 0._c_double
       ycmin = 0._c_double
       xcmax = 1._c_double
       ycmax = 1._c_double
       xcstep = 0.001_c_double
       ycstep = 0.001_c_double
    case(0)
       call gr_plot_coords_n_w(0._c_double, 0._c_double, xcmin, ycmin)
       call gr_plot_coords_n_w(1._c_double, 1._c_double, xcmax, ycmax)
       xcstep = 10._c_double ** int(log10((xcmax-xcmin)/1000._c_double))
       ycstep = 10._c_double ** int(log10((ycmax-ycmin)/1000._c_double))
    case(2)
       call gr_plot_coords_n_v(0._c_double, 0._c_double, xcmin, ycmin)
       call gr_plot_coords_n_v(1._c_double, 1._c_double, xcmax, ycmax)
       xcstep = 0.001_c_double
       ycstep = 0.001_c_double
    end select
    csys = text%norm

    text_window = hl_gtk_window_new("Text creator/editor"//c_null_char, &
         & destroy=c_funloc(gr_text_quit), data_destroy=c_loc(iapply(1)), &
         & parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(text_window, base)

    text_draw = hl_gtk_drawing_area_new(size=[320_c_int, 25_c_int])
    call hl_gtk_box_pack(base, text_draw)

    jb = hl_gtk_table_new()
    call hl_gtk_box_pack(base, jb)

    junk = gtk_label_new("Text:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 0_c_int)

    text_entry = hl_gtk_entry_new(value=trim(text%text)//c_null_char,&
         & changed = c_funloc(gr_text_update), tooltip= &
         & "Edit or enter the text string"//c_null_char)
    call hl_gtk_table_attach(jb, text_entry, 1_c_int, 0_c_int, xspan=3_c_int)

    junk = gtk_label_new("Coordinates:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int)

    text_sys_cbo = hl_gtk_combo_box_new(initial_choices=[&
         & 'World     ', 'Normalized', 'Viewport  '], &
         & active=int(text%norm, c_int), &
         & changed=c_funloc(gr_text_csys), tooltip=&
         & "Select the coordinate system for the text string"//c_null_char)
    call hl_gtk_table_attach(jb, text_sys_cbo, 1_c_int, 1_c_int)

    if (pdefs%y_right) then
       junk = gtk_label_new("Y axis:"//c_null_char)
       call hl_gtk_table_attach(jb, junk, 2_c_int, 1_c_int)

       text_axis_cbo = hl_gtk_combo_box_new(initial_choices=[ &
            & 'Main     ', 'Secondary'], active=int(text%axis, c_int), &
            & sensitive=f_c_logical(text%norm == 0), &
            & changed=c_funloc(gr_text_yax), tooltip=&
            & "Select the Y axis for text in data coordinates"//c_null_char)
       call hl_gtk_table_attach(jb, text_axis_cbo, 3_c_int, 1_c_int)
    end if

    junk = gtk_label_new("Position X:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 2_c_int)

    text_xy_sb(1) = hl_gtk_spin_button_new(xcmin, xcmax, xcstep, &
         & initial_value = xs, &
         & tooltip="Set the X coordinate of the text anchor"//c_null_char)
    call hl_gtk_table_attach(jb, text_xy_sb(1), 1_c_int, 2_c_int)

    junk = gtk_label_new("Position Y:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 2_c_int, 2_c_int)

    text_xy_sb(2) = hl_gtk_spin_button_new(ycmin, ycmax, ycstep, &
         & initial_value = ys, &
         & tooltip="Set the Y coordinate of the text anchor"//c_null_char)
    call hl_gtk_table_attach(jb, text_xy_sb(2), 3_c_int, 2_c_int)

    junk = gtk_label_new("ID:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 3_c_int)

    text_id_entry = hl_gtk_entry_new(value=trim(text%id)//c_null_char,&
         & changed = c_funloc(gr_text_update), tooltip= &
         & "Edit or enter an identifying tag for the text string"//c_null_char)
    call hl_gtk_table_attach(jb, text_id_entry, 1_c_int, 3_c_int, &
         & xspan=2_c_int)

    junk = gtk_label_new("Charsize:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 4_c_int)

    text_cs_sb = hl_gtk_spin_button_new(0._c_double, 10._c_double, &
         & 0.01_c_double, initial_value=real(text%size, c_double), &
         & value_changed=c_funloc(gr_text_update), tooltip= &
         & "Set the character size for the text string"//c_null_char)
    call hl_gtk_table_attach(jb, text_cs_sb, 1_c_int, 4_c_int)

    junk = gtk_label_new("Colour:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 2_c_int, 4_c_int)

    current_colour = text%colour
    if (text%colour == -2) then
       isel = ccindex
       c_red = text%c_vals(1)
       c_green = text%c_vals(2)
       c_blue = text%c_vals(3)
    else
       isel = text%colour
       call gr_colour_triple(text%colour, c_red, c_green, c_blue)
    end if
    text_clr_cbo = hl_gtk_combo_box_new(initial_choices=col_list, &
         & active=isel, &
         & changed=c_funloc(gr_text_set_colour), tooltip= &
         & "Select the colour for the text string"//c_null_char)
    call hl_gtk_table_attach(jb, text_clr_cbo, 3_c_int, 4_c_int)

    junk = gtk_label_new("Alignment:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 5_c_int)

    jbb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_table_attach(jb, jbb, 1_c_int, 5_c_int, xspan=2_c_int)

    junk = hl_gtk_button_new("Left"//c_null_char, &
         & clicked=c_funloc(gr_text_align), data=c_loc(align(1)), &
         & tooltip = "Anchor text at its left"//c_null_char)
    call hl_gtk_box_pack(jbb, junk)

    junk = hl_gtk_button_new("Centre"//c_null_char, &
         & clicked=c_funloc(gr_text_align), data=c_loc(align(2)), &
         & tooltip = "Anchor text at its centre"//c_null_char)
    call hl_gtk_box_pack(jbb, junk)

    junk = hl_gtk_button_new("Right"//c_null_char, &
         & clicked=c_funloc(gr_text_align), data=c_loc(align(3)), &
         & tooltip = "Anchor text at its right"//c_null_char)
    call hl_gtk_box_pack(jbb, junk)

    text_algn_sb = hl_gtk_spin_button_new(0._c_double, 1._c_double, &
         & 0.001_c_double, initial_value=real(text%align, c_double), &
         & tooltip = "Set the alignment of the text"//c_null_char)
    call hl_gtk_table_attach(jb, text_algn_sb, 3_c_int, 5_c_int)

    junk = gtk_label_new("Orientation:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 6_c_int)
    text_ori_sb = hl_gtk_spin_button_new(0._c_double, 360._c_double, &
         & 0.1_c_double, initial_value=real(text%orient, c_double), &
         & tooltip="Text orientation degrees anti-clockwise from +x"&
         & //c_null_char)
    call hl_gtk_table_attach(jb, text_ori_sb, 1_c_int, 6_c_int)

    junk = gtk_label_new("Font type:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 7_c_int)
    text_ffam_cbo = hl_gtk_combo_box_new(initial_choices=font_names, &
         & active=int(text%ffamily, c_int)-1_c_int, &
         & changed=c_funloc(gr_text_update))
    call hl_gtk_table_attach(jb, text_ffam_cbo, 1_c_int, 7_c_int)

    junk = gtk_label_new("Font style:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 2_c_int, 7_c_int)
    text_fnt_cbo = hl_gtk_combo_box_new(initial_choices=font_styles, &
         & active=int(text%font, c_int)-1_c_int, &
         & changed=c_funloc(gr_text_update))
    call hl_gtk_table_attach(jb, text_fnt_cbo, 3_c_int, 7_c_int)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    junk = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_text_quit), data=c_loc(iapply(2)), &
         & tooltip="Apply the changes and quit the dialogue"//c_null_char)
    call hl_gtk_box_pack(jb, junk)
    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_text_quit), data=c_loc(iapply(1)), &
         & tooltip="Quit the dialogue, without making changes"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(text_window)
    call gr_set_plw(.true.)

    text_ready=.true.
    if (text%text /= '') call gr_text_update(c_null_ptr, c_null_ptr)

  end subroutine gr_text_menu

  subroutine gr_set_plw(preview, update)
    logical, intent(in) :: preview
    logical, intent(in), optional :: update

    ! Select which plot window

    logical :: changed

    call gr_plot_close()
    if (preview) then
       call gr_plot_open(area=text_draw)
       call plbop
       call plschr(csd, 1._plflt)
       call plvpor(0._plflt, 1._plflt, 0._plflt, 1._plflt)
       call plwind(0._plflt, 1._plflt, 0._plflt, 1._plflt)
    else
       if (present(update)) then
          changed = update
       else 
          changed = .false.
       end if
       call gr_plot_open()
       call gr_plot_draw(changed)
    end if
  end subroutine gr_set_plw

  recursive subroutine gr_text_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Quit the annotation editor.

    logical, pointer :: apply

    call c_f_pointer(data, apply)

    call gr_set_plw(.false., update=apply)
    if (apply) then
       call hl_gtk_entry_get_text(text_entry, text=text%text)
       text%norm = int(gtk_combo_box_get_active(text_sys_cbo), int16)
       if (pdefs%y_right) &
            & text%axis = int(gtk_combo_box_get_active(text_axis_cbo), int16)
       text%x = hl_gtk_spin_button_get_value(text_xy_sb(1))
       text%y = hl_gtk_spin_button_get_value(text_xy_sb(2))
       call hl_gtk_entry_get_text(text_id_entry, text=text%id)
       text%size = real(hl_gtk_spin_button_get_value(text_cs_sb), real32)
!!$       text%colour = int(gtk_combo_box_get_active(text_clr_cbo), int16) - &
!!$            & 1_int16
       text%colour = current_colour
       if (current_colour == -2) then
          text%c_vals = [c_red, c_green, c_blue]
       else
          text%c_vals = 0_int16
       end if
       text%align = real(hl_gtk_spin_button_get_value(text_algn_sb), real32)
       text%orient = real(hl_gtk_spin_button_get_value(text_ori_sb), real32)
       text%ffamily = int(gtk_combo_box_get_active(text_ffam_cbo), int16) + &
            & 1_int16
       text%font = int(gtk_combo_box_get_active(text_fnt_cbo), int16) + &
            & 1_int16

       if (is_new) call gr_add_text(text)
    end if

    call gtk_widget_destroy(text_window)
    text_ready = .false.
    if (is_new) then 
       deallocate(text)
       is_new = .false.
    end if
  end subroutine gr_text_quit

  subroutine gr_text_set_colour(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    integer :: icol
    integer(kind=int16) :: r,g,b
    logical :: update
    
    icol = gtk_combo_box_get_active(widget)
    if (icol == ccindex) then
       icol = -2
       if (current_colour == -2) then
          r = c_red
          g = c_green
          b = c_blue
       else
          call gr_colour_triple(current_colour, r, g, b)
       end if
       call gr_colour_define(text_window, r, g, b, update)
       if (update) then
          c_red = r
          c_green = g
          c_blue = b
          current_colour = -2

          call gr_text_update(widget, data)
       end if
    else
       current_colour = icol
       call gr_text_update(widget, data)
    end if

  end subroutine gr_text_set_colour
  
  subroutine gr_text_update(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Update the preview.

    integer :: icol, ffamily, font
    real(kind=plflt) :: cs
    character(len=265) :: txt

    if (.not. text_ready) return

    call hl_gtk_entry_get_text(text_entry, text=txt)
!    icol = gtk_combo_box_get_active(text_clr_cbo) 
    if (icol == -1) return
    cs = hl_gtk_spin_button_get_value(text_cs_sb)
    ffamily = gtk_combo_box_get_active(text_ffam_cbo)+1
    if (ffamily < 1 .or. ffamily > size(font_list)) return
    font = gtk_combo_box_get_active(text_fnt_cbo)+1
    if (font < 1 .or. font > size(font_shape)) return

    call plschr(0._plflt, cs)

    if (current_colour >= 0) then
       call plcol0(int(current_colour, int32))
    else
       call gr_custom_line(c_red, c_green, c_blue)
    end if
    call plsfont(font_list(ffamily), font_shape(font), &
         & font_weight(font))
    call plclear()
    call plptex(0.05_plflt, 0.5_plflt, 1._plflt, 0._plflt, 0._plflt, txt)

    call gtk_widget_queue_draw(text_draw)
  end subroutine gr_text_update

  subroutine gr_text_csys(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Select annotation's coordinate system.

    integer :: newsys
    real(kind=c_double) :: xcmin, xcmax, xcstep,  ycmin, ycmax, ycstep
    real(kind=c_double) :: xc, yc, x, y

    newsys = gtk_combo_box_get_active(widget)
    if (newsys == csys) return

    call gr_set_plw(.false.)

    xc = hl_gtk_spin_button_get_value(text_xy_sb(1))
    yc = hl_gtk_spin_button_get_value(text_xy_sb(2))

    select case(newsys)
    case(1)
       xcmin = 0._c_double
       ycmin = 0._c_double
       xcmax = 1._c_double
       ycmax = 1._c_double
       xcstep = 0.001_c_double
       ycstep = 0.001_c_double
       select case (csys)
       case(0)
          call gr_plot_coords_w_n(xc, yc, x, y)
       case(2)
          call gr_plot_coords_v_n(xc, yc, x, y)
       end select
    case(0)
       call gr_plot_coords_n_w(0._c_double, 0._c_double, xcmin, ycmin)
       call gr_plot_coords_n_w(1._c_double, 1._c_double, xcmax, ycmax)
       xcstep = 10._c_double ** int(log10((xcmax-xcmin)/1000._c_double))
       ycstep = 10._c_double ** int(log10((ycmax-ycmin)/1000._c_double))
       select case (csys)
       case(1)
          call gr_plot_coords_n_w(xc, yc, x, y)
       case(2) 
          call gr_plot_coords_v_w(xc, yc, x, y)
       end select
    case(2)
       call gr_plot_coords_n_v(0._c_double, 0._c_double, xcmin, ycmin)
       call gr_plot_coords_n_v(1._c_double, 1._c_double, xcmax, ycmax)
       xcstep = 0.001_c_double
       ycstep = 0.001_c_double
       select case (csys)
       case(0)
          call gr_plot_coords_w_v(xc, yc, x, y)
       case(1)
          call gr_plot_coords_n_v(xc, yc, x, y)
       end select
    end select

    call hl_gtk_spin_button_set_range(text_xy_sb(1), lower=xcmin, &
         & upper=xcmax, step=xcstep)
    call hl_gtk_spin_button_set_range(text_xy_sb(2), lower=ycmin, &
         & upper=ycmax, step=ycstep)
    call hl_gtk_spin_button_set_value(text_xy_sb(1), x)
    call hl_gtk_spin_button_set_value(text_xy_sb(2), y)

    csys = int(newsys, int16)
    call gr_set_plw(.true.)
  end subroutine gr_text_csys

  subroutine gr_text_yax(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Select Y axis for world coordinates.

    real(kind=c_double) :: x,y, xv, yv
    integer :: oldax, newax

    if (csys /= 0) return  ! shouldn't happen
    oldax = pdefs%transform%world_selected
    newax = gtk_combo_box_get_active(widget)+1

    if (oldax == newax) return

    x = hl_gtk_spin_button_get_value(text_xy_sb(1))
    y = hl_gtk_spin_button_get_value(text_xy_sb(2))

    call gr_set_plw(.false.)
    call gr_plot_coords_w_v(x, y, xv, yv)
    call gr_plot_coords_v_w(xv, yv, x, y, y_axis=newax)
    call hl_gtk_spin_button_set_value(text_xy_sb(1), x)
    call hl_gtk_spin_button_set_value(text_xy_sb(2), y)
    call gr_set_plw(.true.)

  end subroutine gr_text_yax

  subroutine gr_text_align(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set text alignment, common options.

    real(kind=c_double), pointer :: align

    call c_f_pointer(data, align)
    call hl_gtk_spin_button_set_value(text_algn_sb, align)

  end subroutine gr_text_align
end module gr_text_widgets
