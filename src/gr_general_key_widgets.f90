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

module gr_general_key_widgets
  ! Widget for adding a key to the plot.

  use iso_fortran_env
  use iso_c_binding

  use gtk_hl
  use gtk_sup

    use gtk, only: gtk_combo_box_get_active, gtk_container_add, gtk_label_new, &
        & gtk_toggle_button_get_active, gtk_toggle_button_set_active, &
        & gtk_widget_destroy, gtk_widget_set_sensitive, gtk_widget_show_all, &
        & TRUE, FALSE

  use graff_types
  use graff_globals

  use gr_plot
  use gr_plot_tools

  implicit none

  type(c_ptr), private :: key_window, key_base, key_csize_sb, key_cols_sb, &
       & key_side_but, key_points_sb, key_title_entry, key_enable_but, &
       & key_frame_but
  type(c_ptr), dimension(2,2), private :: key_bound_sb
  type(c_ptr), dimension(:), allocatable :: key_ds_but

  integer(kind=int16), private :: knorm
  integer, dimension(:), allocatable, target :: key_indices
  logical, dimension(:), allocatable :: key_use

  type(graff_key), private :: key_save
  logical, private :: reenter
  
contains
  subroutine gr_key_menu

    ! Set up a key or legend.

    integer, dimension(3), target :: iapply = [0, 1, 2]
    Type(c_ptr) :: base, junk, jb, cbase, stab
    real(kind=c_double) :: xcmin, xcmax, xcstep,  ycmin, ycmax, ycstep
    logical, dimension(:), allocatable :: usable
    integer :: n1d, i, j
    integer(kind=c_int) :: ix, iy
    character(len=120) :: klabel

    key_save = pdefs%key
    reenter = .false.
    
    allocate(usable(pdefs%nsets), key_use(pdefs%nsets))
    usable = pdefs%data%type >= -3 .and. pdefs%data%type <= 8
    n1d = count(usable)

    key_use(:) = .false.
    if (allocated(pdefs%key%list)) key_use(pdefs%key%list+1) = .true.
    if (pdefs%key%csize == 0._real64) pdefs%key%csize = 1._real64
       
    allocate(key_indices(n1d))
    j = 1
    do i = 1, pdefs%nsets
       if (.not. usable(i)) cycle
       key_indices(j) = i
       j = j+1
    end do

    knorm = pdefs%key%norm

    select case (knorm)
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

    key_window = hl_gtk_window_new("Key definition"//c_null_char, &
         & destroy=c_funloc(gr_key_quit), data_destroy=c_loc(iapply(1)), &
         & parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(key_window, base)

    key_enable_but = hl_gtk_check_button_new("Draw a key on the plot?"&
         & //c_null_char, &
         & toggled=c_funloc(gr_key_enable), &
         & initial_state=f_c_logical(pdefs%key%use), tooltip=&
         & "Switch the key display on/off"//c_null_char)

    call hl_gtk_box_pack(base, key_enable_but, expand=FALSE)

    key_base = hl_gtk_box_new(horizontal=TRUE)
    call gtk_widget_set_sensitive(key_base, f_c_logical(pdefs%key%use))
    call hl_gtk_box_pack(base, key_base)

    cbase = hl_gtk_box_new()
    call hl_gtk_box_pack(key_base, cbase)

    jb = hl_gtk_table_new()
    call hl_gtk_box_pack(cbase, jb)

    junk = gtk_label_new("Coordinate System:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 0_c_int)


    junk = hl_gtk_combo_box_new(initial_choices=&
         & ["Data    ",  "Normal  ", "Viewport"], &
         & changed=c_funloc(gr_key_norm), active=int(knorm, c_int), &
         & tooltip="Choose the coordinates in which to fix the key"//&
         & c_null_char)
    call hl_gtk_table_attach(jb, junk, 1_c_int, 0_c_int)

    junk = gtk_label_new("Lower left: X:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int)

    key_bound_sb(1,1) = hl_gtk_spin_button_new(xcmin, xcmax, xcstep, &
         & initial_value=pdefs%key%x(1), tooltip = &
         & "Enter the X coordinate of the lower left corner"//c_null_char)
    call hl_gtk_table_attach(jb, key_bound_sb(1,1), 1_c_int, 1_c_int)

    junk = gtk_label_new("Y:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 2_c_int, 1_c_int)

    key_bound_sb(2,1) = hl_gtk_spin_button_new(ycmin, ycmax, ycstep, &
         & initial_value=pdefs%key%y(1), tooltip = &
         & "Enter the Y coordinate of the lower left corner"//c_null_char)
    call hl_gtk_table_attach(jb, key_bound_sb(2,1), 3_c_int, 1_c_int)

    junk = gtk_label_new("Upper right: X:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 2_c_int)

    key_bound_sb(1,2) = hl_gtk_spin_button_new(xcmin, xcmax, xcstep, &
         & initial_value=pdefs%key%x(2), tooltip = &
         & "Enter the X coordinate of the upper right corner"//c_null_char)
    call hl_gtk_table_attach(jb, key_bound_sb(1,2), 1_c_int, 2_c_int)

    junk = gtk_label_new("Y:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 2_c_int, 2_c_int)

    key_bound_sb(2,2) = hl_gtk_spin_button_new(ycmin, ycmax, ycstep, &
         & initial_value=pdefs%key%y(2), tooltip = &
         & "Enter the Y coordinate of the upper right corner"//c_null_char)
    call hl_gtk_table_attach(jb, key_bound_sb(2,2), 3_c_int, 2_c_int)

    jb = hl_gtk_table_new()
    call hl_gtk_box_pack(cbase, jb)

    junk = gtk_label_new("Character size:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 0_c_int)

    key_csize_sb = hl_gtk_spin_button_new(0._c_double, 10._c_double, &
         & 0.01_c_double, initial_value=real(pdefs%key%csize, c_double), &
         & tooltip="Set the character size for the legends"//c_null_char)
    call hl_gtk_table_attach(jb, key_csize_sb, 1_c_int, 0_c_int)

    if (pdefs%y_right) then
       key_side_but = hl_gtk_check_button_new("Indicate the Y axis"//&
            & c_null_char, initial_state=f_c_logical(pdefs%key%side), &
            & tooltip="Select whether to indicate which traces associate "//&
            & "with which Y axis"//c_null_char)
       call hl_gtk_table_attach(jb, key_side_but, 2_c_int, 0_c_int, &
            & xspan=2_c_int)
    end if

    junk = gtk_label_new("Number of columns:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int)

    key_cols_sb = hl_gtk_spin_button_new(1_c_int, 5_c_int, &
         & initial_value=int(pdefs%key%cols, c_int), tooltip=&
         & "Number of columns to use to display the key"//c_null_char)
    call hl_gtk_table_attach(jb, key_cols_sb, 1_c_int, 1_c_int)

    junk = gtk_label_new("Number of points:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 2_c_int, 1_c_int)

    key_points_sb = hl_gtk_spin_button_new(1_c_int, 2_c_int, &
         & initial_value=2_c_int-f_c_logical(pdefs%key%one_point), tooltip=&
         & "Number of points to  use to display the key traces"//c_null_char, &
         & wrap=TRUE)
    call hl_gtk_table_attach(jb, key_points_sb, 3_c_int, 1_c_int)


    key_frame_but = hl_gtk_check_button_new("Draw a box around the key?"&
         & //c_null_char, initial_state = f_c_logical(pdefs%key%frame), &
         & tooltip="Toggle drawing a frame around the key area"//c_null_char)
    call hl_gtk_table_attach(jb, key_frame_but, 0_c_int, 2_c_int, &
         & xspan=2_c_int)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(cbase, jb)

    junk = gtk_label_new("Key title:"//c_null_char)
    call hl_gtk_box_pack(jb, junk, expand=FALSE)

    key_title_entry = hl_gtk_entry_new(value=&
         & trim(pdefs%key%title)//c_null_char, tooltip=&
         & "Enter a title for the key"//c_null_char)
    call hl_gtk_box_pack(jb, key_title_entry)


    stab = hl_gtk_table_new(homogeneous=TRUE)
    call hl_gtk_box_pack(key_base, stab)

    allocate(key_ds_but(n1d))

    do i = 1, n1d
       iy = int(mod(i-1,10),c_int)
       ix = int((i-1)/10, c_int)

       write(klabel, "(i0,') ',2a)") key_indices(i), &
            &trim(pdefs%data(key_indices(i))%descript), c_null_char
       key_ds_but(i) = hl_gtk_check_button_new(trim(klabel), &
            & toggled=c_funloc(gr_key_add), data=c_loc(key_indices(i)), &
            & initial_state=f_c_logical(key_use(key_indices(i))), &
            & tooltip="Select/deslect individual datasets"//c_null_char)
       call hl_gtk_table_attach(stab, key_ds_but(i), ix, iy)
    end do

    if (n1d < 10) then
       iy = n1d+1
    else
       iy = 10
    end if
    ix = (n1d-1)/10 + 1

    junk = hl_gtk_button_new("All"//c_null_char, &
         & clicked=c_funloc(gr_key_all), tooltip=&
         & "Select all eligable datasets"//c_null_char)
    call hl_gtk_table_attach(stab, junk, 0_c_int, iy, xspan=ix)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb)

    junk = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_key_quit), data=c_loc(iapply(2)), &
         & tooltip="Apply the changes and destroy the widget"//c_null_char)
    call hl_gtk_box_pack(jb, junk)
    junk = hl_gtk_button_new("Update"//c_null_char, &
         & clicked=c_funloc(gr_key_quit), data=c_loc(iapply(3)), &
         & tooltip="Apply the changes but do not destroy the widget"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    junk = hl_gtk_button_new("Cancel "//c_null_char, &
         & clicked=c_funloc(gr_key_quit), data=c_loc(iapply(1)), &
         & tooltip="Destroy the widget without applying the changes"//&
         & c_null_char)
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(key_window)

  end subroutine gr_key_menu

  recursive subroutine gr_key_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Quit setting up key

    integer, pointer :: apply
    integer :: nuse, i, j

    if (reenter) return

    reenter = .true.
    call c_f_pointer(data, apply)
    
    if (apply /= 0) then
       pdefs%key%use = c_f_logical(gtk_toggle_button_get_active(key_enable_but))

       pdefs%key%norm = knorm

       pdefs%key%x(1) = hl_gtk_spin_button_get_value(key_bound_sb(1,1))
       pdefs%key%y(1) = hl_gtk_spin_button_get_value(key_bound_sb(2,1))
       pdefs%key%x(2) = hl_gtk_spin_button_get_value(key_bound_sb(1,2))
       pdefs%key%y(2) = hl_gtk_spin_button_get_value(key_bound_sb(2,2))

       pdefs%key%csize =  hl_gtk_spin_button_get_value(key_csize_sb)
       if (pdefs%y_right) then
          pdefs%key%side = &
               & c_f_logical(gtk_toggle_button_get_active(key_side_but))
       else
          pdefs%key%side = .false.
       end if
       pdefs%key%cols = int(hl_gtk_spin_button_get_value(key_cols_sb), int16)

       if (hl_gtk_spin_button_get_value(key_points_sb) == 1) then
          pdefs%key%one_point = .true.
       else
          pdefs%key%one_point = .false.
       end if

       pdefs%key%frame = &
            & c_f_logical(gtk_toggle_button_get_active(key_frame_but))

       call hl_gtk_entry_get_text(key_title_entry, pdefs%key%title)

       if (allocated(pdefs%key%list)) deallocate(pdefs%key%list)
       nuse = count(key_use)
       allocate(pdefs%key%list(nuse))

       j = 1
       do i = 1, size(key_use)
          if (.not. key_use(i)) cycle
          pdefs%key%list(j) = i-1
          j = j+1
       end do

    else
       pdefs%key = key_save
    end if 
    call gr_plot_draw(.true.)

    if (apply /= 2) then
       if (allocated(key_ds_but)) deallocate(key_ds_but, key_indices, key_use)
       if (allocated(key_save%list)) deallocate(key_save%list)
       call gtk_widget_destroy(key_window)
    end if
    reenter = .false.
  end subroutine gr_key_quit

  subroutine gr_key_enable(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Switch key on / off

    call gtk_widget_set_sensitive(key_base, &
         & gtk_toggle_button_get_active(widget))

  end subroutine gr_key_enable

  subroutine gr_key_norm(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Select coordinate system to position key

    integer(kind=int16) :: lnorm
    real(kind=c_double) :: xcmin, xcmax, xcstep,  ycmin, ycmax, ycstep
    real(kind=c_double), dimension(2,2) :: xy_o, xy_n
    integer :: i, j

    lnorm = int(gtk_combo_box_get_active(widget), int16)

    if (lnorm == knorm) return

    do i = 1,2
       do j = 1,2
          xy_o(i,j) = hl_gtk_spin_button_get_value(key_bound_sb(i,j))
       end do
    end do

    select case (knorm)
    case(0)
       select case (lnorm)
       case(1)
          call gr_plot_coords_w_n(xy_o(1,1), xy_o(2,1), xy_n(1,1), xy_n(2,1))
          call gr_plot_coords_w_n(xy_o(1,2), xy_o(2,2), xy_n(1,2), xy_n(2,2))
       case(2)
          call gr_plot_coords_w_v(xy_o(1,1), xy_o(2,1), xy_n(1,1), xy_n(2,1))
          call gr_plot_coords_w_v(xy_o(1,2), xy_o(2,2), xy_n(1,2), xy_n(2,2))
       end select
    case(1)
       select case (lnorm)
       case(0)
          call gr_plot_coords_n_w(xy_o(1,1), xy_o(2,1), xy_n(1,1), xy_n(2,1))
          call gr_plot_coords_n_w(xy_o(1,2), xy_o(2,2), xy_n(1,2), xy_n(2,2))
       case(2)
          call gr_plot_coords_n_v(xy_o(1,1), xy_o(2,1), xy_n(1,1), xy_n(2,1))
          call gr_plot_coords_n_v(xy_o(1,2), xy_o(2,2), xy_n(1,2), xy_n(2,2))
       end select
    case(2)
       select case (lnorm)
       case(0)
          call gr_plot_coords_v_w(xy_o(1,1), xy_o(2,1), xy_n(1,1), xy_n(2,1))
          call gr_plot_coords_v_w(xy_o(1,2), xy_o(2,2), xy_n(1,2), xy_n(2,2))
        case(1)
          call gr_plot_coords_v_n(xy_o(1,1), xy_o(2,1), xy_n(1,1), xy_n(2,1))
          call gr_plot_coords_v_n(xy_o(1,2), xy_o(2,2), xy_n(1,2), xy_n(2,2))
       end select
    end select

    knorm = lnorm

        select case (knorm)
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

    do j = 1, 2
       call hl_gtk_spin_button_set_range(key_bound_sb(1,j), &
            & lower=xcmin, upper=xcmax, step=xcstep)
       call hl_gtk_spin_button_set_value(key_bound_sb(1,j), &
            & xy_n(1,j))
       call hl_gtk_spin_button_set_range(key_bound_sb(2,j), &
            & lower=ycmin, upper=ycmax, step=ycstep)
       call hl_gtk_spin_button_set_value(key_bound_sb(2,j), &
            & xy_n(2,j))
    end do

  end subroutine gr_key_norm

  subroutine gr_key_add(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Add a DS to the key

    integer, pointer :: dsno

    call c_f_pointer(data, dsno)
    key_use(dsno) = c_f_logical(gtk_toggle_button_get_active(widget))

  end subroutine gr_key_add

  subroutine gr_key_all(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Add all suitable datasets to the key.

    integer :: i

    do i = 1, size(key_indices)
       call gtk_toggle_button_set_active(key_ds_but(i), TRUE)
       key_use(key_indices(i)) = .true.
    end do
  end subroutine gr_key_all

end module gr_general_key_widgets
