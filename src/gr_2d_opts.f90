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

module gr_2d_opts
  ! Widgets and handlers for 2D dataset display options.

  use iso_c_binding
  use iso_fortran_env

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_combo_box_get_active, gtk_entry_set_text, gtk_label_new, &
       & gtk_toggle_button_get_active, gtk_widget_set_sensitive, TRUE, FALSE, &
       & GTK_POLICY_NEVER

  use graff_types
  use graff_globals
  use gr_colours

  use gr_cb_common

  implicit none

contains

  function gr_2d_opts_new() result(fr)
    type(c_ptr) :: fr

    ! Display options for 2-D datasets.

    type(c_ptr) :: table, junk, sbox, jb
    integer(kind=c_int) :: nbi
    type(graff_zdata), pointer :: zdata
    character(len=32), dimension(:), allocatable :: txtvals
    integer :: i, nccol
    integer, dimension(2), target :: idx = [1, 2]
    character(len=32) :: textval
    integer(kind=int16) :: zformat

    zdata => pdefs%data(pdefs%cset)%zdata
    zformat = zdata%format

    fr = hl_gtk_box_new()

    fmt_nbook = hl_gtk_notebook_new(switch_page=c_funloc(gr_2d_set_fmt))
    call hl_gtk_box_pack(fr, fmt_nbook, expand=FALSE)

    ! The contour page

    table = hl_gtk_table_new()
    nbi = hl_gtk_notebook_add_page(fmt_nbook, table, &
         & label="Contoured"//c_null_char)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_table_attach(table, jb, 0_c_int, 0_c_int, xspan=3_c_int)

    clevel_cbo = hl_gtk_combo_box_new(initial_choices=&
         & ["Automatic", "Explicit "], changed=c_funloc(gr_2d_set_ct_rule), &
         & active=f_c_logical(zdata%set_levels), tooltip=&
         & "Select automatic or explicit contour levels"//c_null_char)
    call hl_gtk_box_pack(jb, clevel_cbo)

    cldist_cbo = hl_gtk_combo_box_new(initial_choices= &
         & ['Linear     ', 'Logarithmic', 'Square Root'], &
         & changed = c_funloc(gr_2d_set_clog), &
         & active = int(zdata%lmap, c_int), &
         & tooltip = "Select log/linear/sqrt contour levels."//c_null_char)
    call hl_gtk_box_pack(jb, cldist_cbo)
    
    cfmt_cbo = hl_gtk_combo_box_new(initial_choices=&
         & ["Outline ", "Filled  ", "Downhill"], &
         & changed=c_funloc(gr_2d_set_ct_fmt), &
         & active=int(zdata%fill, c_int), tooltip=&
         & "Select type of contours to use"//c_null_char)
    call hl_gtk_box_pack(jb, cfmt_cbo)

    junk = gtk_label_new("Levels"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 1_c_int)

    clevel_view = hl_gtk_text_view_new(scroll=sbox, &
         & hscroll_policy=GTK_POLICY_NEVER, editable=TRUE,&
         & focus_out_event=c_funloc(gr_2d_set_clevels), &
         & ssize=[-1_c_int, 100_c_int], &
         & sensitive=f_c_logical(zdata%set_levels), &
         & tooltip="Set explicit contour levels"//c_null_char)
    call hl_gtk_table_attach(table, sbox, 0_c_int, 2_c_int, xpad=5_c_int)
    if (allocated(zdata%levels)) then
       allocate(txtvals(size(zdata%levels)))
       write(txtvals, "(g0.5)") zdata%levels
       call hl_gtk_text_view_insert(clevel_view, txtvals, replace=TRUE)
       deallocate(txtvals)
    end if

    junk = gtk_label_new("Colours"//c_null_char)
    call hl_gtk_table_attach(table, junk, 1_c_int, 1_c_int)

    ccol_view = hl_gtk_text_view_new(scroll=sbox, &
         & hscroll_policy=GTK_POLICY_NEVER, editable=TRUE,&
         & focus_out_event=c_funloc(gr_2d_set_ccols), &
         & ssize=[-1_c_int, 100_c_int], &
         & tooltip="Set contour colours"//c_null_char)
    call hl_gtk_table_attach(table, sbox, 1_c_int, 2_c_int, xpad=5_c_int)
    if (allocated(zdata%colours)) then
       nccol = size(zdata%colours)
       allocate(txtvals(nccol))
       do i = 1, nccol
          if (zdata%colours(i) == -2) then
             write(txtvals(i), "(3i5)") zdata%raw_colours(:,i)
          else
             write(txtvals(i), "(i0)") zdata%colours(i)
          end if
       end do
       call hl_gtk_text_view_insert(ccol_view, txtvals, replace=TRUE)
       deallocate(txtvals)
    end if

    junk = gtk_label_new("Styles"//c_null_char)
    call hl_gtk_table_attach(table, junk, 2_c_int, 1_c_int)

    csty_view = hl_gtk_text_view_new(scroll=sbox, &
         & hscroll_policy=GTK_POLICY_NEVER, editable=TRUE,&
         & focus_out_event=c_funloc(gr_2d_set_csty), &
         & ssize=[-1_c_int, 100_c_int], &
         & tooltip="Set contour linestyles"//c_null_char)
    call hl_gtk_table_attach(table, sbox, 2_c_int, 2_c_int, xpad=5_c_int)
    if (allocated(zdata%style)) then
       allocate(txtvals(size(zdata%style)))
       write(txtvals, "(i0)") zdata%style
       call hl_gtk_text_view_insert(csty_view, txtvals, replace=TRUE)
       deallocate(txtvals)
    end if

    junk = gtk_label_new("Thicknesses"//c_null_char)
    call hl_gtk_table_attach(table, junk, 2_c_int, 3_c_int)

    cthick_view = hl_gtk_text_view_new(scroll=sbox, &
         & hscroll_policy=GTK_POLICY_NEVER, editable=TRUE,&
         & focus_out_event=c_funloc(gr_2d_set_cthick), &
         & ssize=[-1_c_int, 100_c_int], &
         & tooltip="Set contour line thicknesses"//c_null_char)
    call hl_gtk_table_attach(table, sbox, 2_c_int, 4_c_int, xpad=5_c_int)
    if (allocated(zdata%thick)) then
       allocate(txtvals(size(zdata%thick)))
       write(txtvals, "(g0.5)") zdata%thick
       call hl_gtk_text_view_insert(cthick_view, txtvals, replace=TRUE)
       deallocate(txtvals)
    end if

    jb = hl_gtk_table_new()
    call hl_gtk_table_attach(table, jb, 0_c_int, 4_c_int, xspan=2_c_int)

    junk = gtk_label_new("# Levels:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 0_c_int)

    clevels_entry = hl_gtk_spin_button_new(1_c_int, 100_c_int, &
         & initial_value=int(zdata%n_levels, c_int), &
         & value_changed=c_funloc(gr_2d_set_cnlevels), &
         & sensitive=f_c_logical(.not. zdata%set_levels), &
         & tooltip = "Set the number of contours"//c_null_char)
    call hl_gtk_table_attach(jb, clevels_entry, 1_c_int, 0_c_int)

    junk = gtk_label_new("Label"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int)

    clabel_entry = hl_gtk_spin_button_new(0_c_int, 100_c_int, &
         & initial_value=int(zdata%label, c_int), &
         & value_changed=c_funloc(gr_2d_set_clabel), &
         & tooltip="Set the contour labelling frequency (0=Off)"//c_null_char)
    call hl_gtk_table_attach(jb, clabel_entry, 1_c_int, 1_c_int)

    junk = gtk_label_new('Label offset'//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 2_c_int)
    clabel_off_entry = hl_gtk_spin_button_new(0_c_int, 99_c_int, &
         & initial_value = int(zdata%label_off, c_int), &
         & value_changed = c_funloc(gr_2d_set_clabel_off), &
         & tooltip = "Set the first contour to label."//c_null_char)
    call hl_gtk_table_attach(jb, clabel_off_entry, 1_c_int, 2_c_int)

    junk = gtk_label_new("Charsize"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 3_c_int)

    cchsize_entry = hl_gtk_spin_button_new(0._c_double, 100._c_double, &
         & 0.01_c_double, initial_value=real(zdata%charsize, c_double), &
         & value_changed=c_funloc(gr_2d_set_csize), &
         & tooltip="Set the character size for contour labels"//c_null_char)
    call hl_gtk_table_attach(jb, cchsize_entry, 1_c_int, 3_c_int)

    ! The Greyscale/colour page

    table = hl_gtk_table_new()
    nbi = hl_gtk_notebook_add_page(fmt_nbook, table, label="Colour/Grey")

    cg_table_pick = hl_gtk_listn_new(sbox, hscroll_policy=GTK_POLICY_NEVER, &
         & titles=['Colour Table'], changed=c_funloc(gr_2d_set_table), &
         & ncols=1_c_int, height=200_c_int, tooltip=&
         & "Select a colour table"//c_null_char)
    call hl_gtk_table_attach(table, sbox, 0_c_int, 0_c_int, &
         & xspan=4_c_int)
    call hl_gtk_listn_ins(cg_table_pick, count=gr_ct_get_ntables())
    do i = 0, gr_ct_get_ntables()-1
       call hl_gtk_listn_set_cell(cg_table_pick, &
            & int(i, c_int), 0_c_int, &
            & svalue = trim(gr_ct_get_name(i))//c_null_char)
    end do
    call hl_gtk_listn_set_selection(cg_table_pick, int(zdata%ctable, c_int))

    junk = gtk_label_new("Range"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 1_c_int, &
         & xspan=4_c_int)

    junk = gtk_label_new("Min:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 2_c_int)

    junk = gtk_label_new("Max:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 2_c_int, 2_c_int)

    do i = 1, 2
       write(textval, "(g0.5)") zdata%range(i)
       cg_range_entry(i) = hl_gtk_entry_new(editable=TRUE, &
            & value=trim(adjustl(textval))//c_null_char, &
            & activate=c_funloc(gr_2d_set_range), data=c_loc(idx(i)), &
            & focus_out_event=c_funloc(gr_2d_set_range_e), &
            & data_focus_out=c_loc(idx(i)), tooltip=&
            & "Set the range to map (equal is auto)"//c_null_char, &
            & size=80_c_int)
       call hl_gtk_table_attach(table, cg_range_entry(i), &
            & int(2*(i-1)+1, c_int), 2_c_int)
    end do

    junk = gtk_label_new("Missing:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 0_c_int, 3_c_int)

    write(textval, "(g0.5)") zdata%missing
    cg_missing_entry = hl_gtk_entry_new(editable=TRUE, &
         & value=trim(adjustl(textval))//c_null_char, &
         & activate=c_funloc(gr_2d_set_missing),  &
         & focus_out_event=c_funloc(gr_2d_set_missing_e), &
         & tooltip="Set missing data value (0. is disabled)"//c_null_char, &
         & size=80_c_int)
    call hl_gtk_table_attach(table, cg_missing_entry, 1_c_int, 3_c_int)

    junk = gtk_label_new("Gamma:"//c_null_char)
    call hl_gtk_table_attach(table, junk, 2_c_int, 3_c_int)

    cg_gamma_entry =  hl_gtk_spin_button_new(0._c_double, 5._c_double, &
         & 0.01_c_double, initial_value=real(zdata%gamma, c_double), &
         & value_changed=c_funloc(gr_2d_set_gamma), tooltip = &
         & "Set the gamma value for the colour mapping"//c_null_char)
    call hl_gtk_table_attach(table, cg_gamma_entry, 3_c_int, 3_c_int)


    cg_log_cbo = hl_gtk_combo_box_new(initial_choices = &
         & ['Linear     ', 'Logarithmic', 'Square Root'], &
         & changed=c_funloc(gr_2d_set_log), &
         & active=int(zdata%ilog, c_int), &
         & tooltip="Select log/linear/sqrt colour mapping"//c_null_char)
    call hl_gtk_table_attach(table, cg_log_cbo, 0_c_int, 4_c_int, &
         & xspan=2_c_int)

    cg_invert_but = hl_gtk_check_button_new("Invert colours?"//c_null_char, &
         & toggled=c_funloc(gr_2d_set_invert), &
         & initial_state=f_c_logical(zdata%invert), &
         & tooltip="Toggle inversion of colour mapping"//c_null_char)
    call hl_gtk_table_attach(table, cg_invert_but, 2_c_int, 4_c_int,&
         & xspan=2_c_int)

    cg_smooth_but = hl_gtk_check_button_new("Smooth display?"//c_null_char, &
         & toggled=c_funloc(gr_2d_set_smooth), &
         & initial_state=f_c_logical(zdata%smooth), &
         & tooltip="Select smooth display of colour image (slower)"&
         & //c_null_char)
    call hl_gtk_table_attach(table, cg_smooth_but, 0_c_int, 5_c_int, &
         & xspan=2_c_int)

    junk = gtk_label_new("Levels"//c_null_char)
    call hl_gtk_table_attach(table, junk, 2_c_int, 5_c_int)

    gc_smooth_l_sb = hl_gtk_spin_button_new(3_c_int, 256_c_int, &
         & initial_value=int(zdata%shade_levels, c_int), &
         & value_changed=c_funloc(gr_2d_set_sm_levels), tooltip=&
         & "Set how many levels for smooth colour display"//c_null_char, &
         & sensitive=f_c_logical(zdata%invert))
    call hl_gtk_table_attach(table, gc_smooth_l_sb, 3_c_int, 5_c_int)

    ! Hidden dataset

    junk = hl_gtk_box_new()
    nbi = hl_gtk_notebook_add_page(fmt_nbook, junk, &
         & label="Hidden"//c_null_char)

    zdata%format=zformat

  end function gr_2d_opts_new

  subroutine gr_2d_set_fmt(book, page, number, data) bind(c)
    type(c_ptr), value :: book, page, data
    integer(kind=c_int), value :: number

    ! Set display style. Contour/colour/none

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%zdata%format = int(number, int16)

    if (c_associated(gr_drawing_area)) call gr_plot_draw(.true.)
  end subroutine gr_2d_set_fmt

  ! Contour handlers

  subroutine gr_2d_set_ct_rule(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Automatic / explicit contour levels.

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%zdata%set_levels = &
         & c_f_logical(gtk_combo_box_get_active(widget))

    call gtk_widget_set_sensitive(clevel_view, &
         & f_c_logical(pdefs%data(pdefs%cset)%zdata%set_levels))
    call gtk_widget_set_sensitive(clevels_entry, &
         & f_c_logical(.not. pdefs%data(pdefs%cset)%zdata%set_levels))
    call gtk_widget_set_sensitive(cldist_cbo, &
         & f_c_logical(.not. pdefs%data(pdefs%cset)%zdata%set_levels))

    call gr_plot_draw(.true.)
  end subroutine gr_2d_set_ct_rule
  subroutine gr_2d_set_clog(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set log colour mapping

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%zdata%lmap = &
         & int(gtk_combo_box_get_active(widget), int16)

    call gr_plot_draw(.true.)
  end subroutine gr_2d_set_clog


  subroutine gr_2d_set_ct_fmt(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Contour format (open/filled)

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%zdata%fill = &
         & int(gtk_combo_box_get_active(widget), int8)

    call gr_plot_draw(.true.)
  end subroutine gr_2d_set_ct_fmt

  function gr_2d_set_clevels(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, event, data

    ! Contour levels.

    character(len=32), dimension(:), allocatable :: text
    real(kind=real64), dimension(:), allocatable :: levels
    integer :: nlevels, ios, i, j
    logical :: rewrite
    type(graff_zdata), pointer :: zdata

    rv = FALSE
    if (.not. gui_active) return

    call hl_gtk_text_view_get_text(widget, text)
    nlevels = count(text /= '')
    if (nlevels == 0) return
    allocate(levels(nlevels))

    zdata => pdefs%data(pdefs%cset)%zdata

    i = 1
    rewrite = .false.
    do j = 1, size(text)
       if (text(j) == '') cycle
       read(text(j), *, iostat=ios) levels(i)
       if (ios /= 0) then
          rewrite = .true.
          cycle
       end if
       i = i+1
    end do
    nlevels = i-1

    if (allocated(zdata%levels)) deallocate(zdata%levels)
    allocate(zdata%levels(nlevels))
    zdata%levels(:) = levels(:nlevels)
    zdata%n_levels = int(nlevels, int16)
        
    if (rewrite) then
       deallocate(text)
       allocate(text(nlevels))
       write(text, "(g0.5)") zdata%levels
       call hl_gtk_text_view_insert(widget, text, replace=TRUE)
    end if

    call gr_plot_draw(.true.)
  end function gr_2d_set_clevels

  function gr_2d_set_ccols(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, event, data

    ! Contour colours

    character(len=32), dimension(:), allocatable :: text
    character(len=12), dimension(:), allocatable :: subtext

    integer(kind=int16), dimension(:), allocatable :: colours
    integer(kind=int16), dimension(:,:), allocatable :: raw_colours

    integer :: ncols, ios, i, j, nsub
    logical :: rewrite
    type(graff_zdata), pointer :: zdata

    rv = FALSE
    if (.not. gui_active) return

    call hl_gtk_text_view_get_text(widget, text)
    ncols = count(text /= '')
    if (ncols == 0) return

    allocate(colours(ncols), raw_colours(3,ncols))

    zdata => pdefs%data(pdefs%cset)%zdata

    i = 1
    rewrite = .false.
    do j = 1, size(text)
       if (text(j) == '') cycle
       call split(text(j), ' ,	', subtext, count=nsub)
       if (nsub < 3) then
          read(text(j), *, iostat=ios) colours(i)
          if (ios /= 0) then
             rewrite = .true.
             cycle
          end if
          raw_colours(:,i) = 0_int16
       else
          read(text(j), *, iostat=ios) raw_colours(:,i)
          if (ios /= 0) then
             rewrite = .true.
             cycle
          end if
          colours(i) = -2
       end if
       i = i+1
    end do
    ncols = i-1

    if (allocated(zdata%colours)) deallocate(zdata%colours)
    if (allocated(zdata%raw_colours)) deallocate(zdata%raw_colours)
    allocate(zdata%colours(ncols), zdata%raw_colours(3, ncols))
    zdata%colours(:) = colours(:ncols)
    zdata%raw_colours(:,:) = raw_colours(:,:ncols)
    zdata%n_cols = int(ncols, int16)

    if (rewrite) then
       deallocate(text)
       allocate(text(ncols))
       do i = 1, ncols
          if (zdata%colours(i) == -2) then
             write(text(i), "(3i5)") zdata%raw_colours(:,i)
          else
             write(text(i), "(i0)") zdata%colours(i)
          end if
       end do
       call hl_gtk_text_view_insert(widget, text, replace=TRUE)
    end if

    call gr_plot_draw(.true.)
  end function gr_2d_set_ccols

  function gr_2d_set_csty(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, event, data

    ! Contour linestyles

    character(len=32), dimension(:), allocatable :: text
    integer(kind=int16), dimension(:), allocatable :: styles
    integer :: nsty, ios, i, j
    logical :: rewrite
    type(graff_zdata), pointer :: zdata

    rv = FALSE
    if (.not. gui_active) return

    call hl_gtk_text_view_get_text(widget, text)
    nsty = count(text /= '')
    if (nsty == 0) return
    allocate(styles(nsty))

    zdata => pdefs%data(pdefs%cset)%zdata

    i = 1
    rewrite = .false.
    do j = 1, size(text)
       if (text(j) == '') cycle
       read(text(j), *, iostat=ios) styles(i)
       if (ios /= 0) then
          rewrite = .true.
          cycle
       end if
       i = i+1
    end do
    nsty = i-1

    if (allocated(zdata%style)) deallocate(zdata%style)
    allocate(zdata%style(nsty))
    zdata%style(:) = styles(:nsty)
    zdata%n_sty = int(nsty, int16)

    if (rewrite) then
       deallocate(text)
       allocate(text(nsty))
       write(text, "(i0)") zdata%style
       call hl_gtk_text_view_insert(widget, text, replace=TRUE)
    end if

    call gr_plot_draw(.true.)
  end function gr_2d_set_csty

  function gr_2d_set_cthick(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, event, data

    ! Contour thicknesses

    character(len=32), dimension(:), allocatable :: text
    real(kind=real32), dimension(:), allocatable :: thick
    integer :: nthick, ios, i, j
    logical :: rewrite
    type(graff_zdata), pointer :: zdata

    rv = FALSE
    if (.not. gui_active) return

    call hl_gtk_text_view_get_text(widget, text)
    nthick = count(text /= '')
    if (nthick == 0) return
    allocate(thick(nthick))

    zdata => pdefs%data(pdefs%cset)%zdata

    i = 1
    rewrite = .false.
    do j = 1, size(text)
       if (text(j) == '') cycle
       read(text(j), *, iostat=ios) thick(i)
       if (ios /= 0) then
          rewrite = .true.
          cycle
       end if
       i = i+1
    end do
    nthick = i-1

    if (allocated(zdata%thick)) deallocate(zdata%thick)
    allocate(zdata%thick(nthick))
    zdata%thick(:) = thick(:nthick)
    zdata%n_thick = int(nthick, int16)

    if (rewrite) then
       deallocate(text)
       allocate(text(nthick))
       write(text, "(g0.5)") zdata%thick
       call hl_gtk_text_view_insert(widget, text, replace=TRUE)
    end if

    call gr_plot_draw(.true.)
  end function gr_2d_set_cthick

  subroutine gr_2d_set_cnlevels(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Number of contours

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%zdata%n_levels = &
         & int(hl_gtk_spin_button_get_value(widget), int16)
        
    call gr_plot_draw(.true.)
  end subroutine gr_2d_set_cnlevels

  subroutine gr_2d_set_clabel(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Contour labelling frequency

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%zdata%label = &
         & int(hl_gtk_spin_button_get_value(widget), int16)

    call gr_plot_draw(.true.)
  end subroutine gr_2d_set_clabel

  subroutine gr_2d_set_clabel_off(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Contour labelling offset
    
    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%zdata%label_off = &
         & int(hl_gtk_spin_button_get_value(widget), int16)

    call gr_plot_draw(.true.)
  end subroutine gr_2d_set_clabel_off

  subroutine gr_2d_set_csize(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Label character size

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%zdata%charsize = &
         & real(hl_gtk_spin_button_get_value(widget), real32)

    call gr_plot_draw(.true.)
  end subroutine gr_2d_set_csize

  ! Colour-grey handlers

  subroutine gr_2d_set_table(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Select colour table.

    integer(kind=c_int), dimension(:), allocatable :: selected
    integer(kind=c_int) :: nsel

    if (.not. gui_active) return

    nsel = hl_gtk_listn_get_selections(C_NULL_PTR, selected, selection=widget)
    if (nsel == 0) return

    pdefs%data(pdefs%cset)%zdata%ctable = int(selected(1), int16)

    if (c_associated(gr_drawing_area)) call gr_plot_draw(.true.)
  end subroutine gr_2d_set_table

  subroutine gr_2d_set_range(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set range for colour display

    integer, pointer :: idx
    character(len=32) :: text
    real(kind=real64) :: value
    integer :: ios
    type(graff_zdata), pointer :: zdata

    if (.not. gui_active) return

    zdata => pdefs%data(pdefs%cset)%zdata
    call c_f_pointer(data, idx)

    call hl_gtk_entry_get_text(widget, text)
    read(text, *, iostat=ios) value
    if (ios /= 0) then
       write(text, "(g0.5)") zdata%range(idx)
       call gtk_entry_set_text(widget, trim(adjustl(text))//c_null_char)
    else
       zdata%range(idx) = value
    end if
    call gr_plot_draw(.true.)
  end subroutine gr_2d_set_range
  function gr_2d_set_range_e(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, event, data

    call gr_2d_set_range(widget, data)

    rv = FALSE

  end function gr_2d_set_range_e

  subroutine gr_2d_set_missing(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Missing data values.

    character(len=32) :: text
    real(kind=real64) :: value
    integer :: ios
    type(graff_zdata), pointer :: zdata

    if (.not. gui_active) return

    zdata => pdefs%data(pdefs%cset)%zdata

    call hl_gtk_entry_get_text(widget, text)
    read(text, *, iostat=ios) value
    if (ios /= 0) then
       write(text, "(g0.5)") zdata%missing
       call gtk_entry_set_text(widget, trim(adjustl(text))//c_null_char)
    else
       zdata%missing = value
    end if
    call gr_plot_draw(.true.)
  end subroutine gr_2d_set_missing

  function gr_2d_set_missing_e(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, event, data

    call gr_2d_set_missing(widget, data)

    rv = FALSE

  end function gr_2d_set_missing_e

  subroutine gr_2d_set_gamma(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Colour table gamma setting

    character(len=32) :: text
    real(kind=real32) :: value
    integer :: ios
    type(graff_zdata), pointer :: zdata

    if (.not. gui_active) return

    zdata => pdefs%data(pdefs%cset)%zdata

    call hl_gtk_entry_get_text(widget, text)
    read(text, *, iostat=ios) value
    if (ios /= 0) then
       write(text, "(g0.5)") zdata%gamma
       call gtk_entry_set_text(widget, trim(adjustl(text))//c_null_char)
    else
       zdata%gamma = value
    end if
    call gr_plot_draw(.true.)
  end subroutine gr_2d_set_gamma

  subroutine gr_2d_set_log(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set log colour mapping

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%zdata%ilog = &
         & int(gtk_combo_box_get_active(widget), int16)

    call gr_plot_draw(.true.)
  end subroutine gr_2d_set_log

  subroutine gr_2d_set_invert(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set inverted colour table

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%zdata%invert = &
         & c_f_logical(gtk_toggle_button_get_active(widget))

    call gr_plot_draw(.true.)
  end subroutine gr_2d_set_invert

  subroutine gr_2d_set_smooth(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set smooth (plshades) or blocky (plimage) display

    if (.not. gui_active) return

    pdefs%data(pdefs%cset)%zdata%smooth = &
         & c_f_logical(gtk_toggle_button_get_active(widget))
    call gtk_widget_set_sensitive(gc_smooth_l_sb, &
         & gtk_toggle_button_get_active(widget))
    call gr_plot_draw(.true.)
  end subroutine gr_2d_set_smooth

  subroutine gr_2d_set_sm_levels(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set how many levels to use for plshades display.

    pdefs%data(pdefs%cset)%zdata%shade_levels = &
         & int(hl_gtk_spin_button_get_value(widget), int32)

    call gr_plot_draw(.true.)

  end subroutine gr_2d_set_sm_levels
end module gr_2d_opts
