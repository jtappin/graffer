module gr_menu_hc_widgets

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

  implicit none

  type(c_ptr), private :: hc_window, hc_paper_cbo, hc_ls_but, hc_name_entry, &
       & hc_view_cbo, hc_lp_entry, hc_col_but, hc_ts_but
  type(c_ptr), dimension(2), private :: hc_psize_sb, hc_off_sb

  real, dimension(2,2), parameter, private :: phys_size = &
       & reshape([21.0, 29.7, 21.59, 27.94], [2, 2])

  character(len=16), dimension(:), allocatable, private :: viewnames

contains
  subroutine gr_hc_menu

    ! Menus to configure hard copy options.

    type(c_ptr) :: base, junk, jb, views
    type(graff_hard), pointer :: hardset
    logical, dimension(2), target :: iapply = [.false., .true.]
    integer(kind=c_int) :: iviewer, i
    integer :: pdot

    hardset => pdefs%hardset

    if (.not. allocated(viewnames)) call gr_find_viewers(viewnames, eps=.true.)

    iviewer = 0
    if (hardset%viewer(1) /= '') then
       do i = 1, size(viewnames)
          if (hardset%viewer(1) == viewnames(i)) then
             iviewer = i
             exit
          end if
       end do
       if (iviewer == 0) then
          views = g_find_program_in_path(trim(hardset%viewer(1))//c_null_char)
          if (c_associated(views)) iviewer = size(viewnames)+1
       end if
    end if

    if (hardset%name == '') then
       pdot = index(pdefs%name, '.', back=.true.)
       if (pdot == 0) then
          hardset%name = pdefs%name
       else
          hardset%name = pdefs%name(:pdot-1)
       end if
    end if

    hc_window = hl_gtk_window_new("Hardcopy options"//c_null_char,&
         & destroy=c_funloc(gr_hc_quit), data_destroy=c_loc(iapply(1)), &
         & parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(hc_window, base)

    jb = hl_gtk_table_new()
    call hl_gtk_box_pack(base, jb)

    hc_paper_cbo = hl_gtk_combo_box_new(initial_choices=[&
         & 'A4    ', 'Letter'], active=int(hardset%psize, c_int), &
         & tooltip = "Set paper size for PS and PDF outputs"//c_null_char)
    call hl_gtk_table_attach(jb, hc_paper_cbo, 0_c_int, 0_c_int)

    junk = gtk_label_new("Size: X (cm)"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 1_c_int, 0_c_int)

    hc_psize_sb(1) = hl_gtk_spin_button_new(0._c_double, 100._c_double, &
         & 0.01_c_double, initial_value=real(hardset%size(1), c_double), &
         & tooltip=&
         & "Set the X dimension of the 'paper' for hardcopies"//c_null_char)
    call hl_gtk_table_attach(jb, hc_psize_sb(1), 2_c_int, 0_c_int)

    junk = gtk_label_new("Y (cm)"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 3_c_int, 0_c_int)

    hc_psize_sb(2) = hl_gtk_spin_button_new(0._c_double, 100._c_double, &
         & 0.01_c_double, initial_value=real(hardset%size(2), c_double), &
         &  tooltip=&
         & "Set the Y dimension of the 'paper' for hardcopies"//c_null_char)
    call hl_gtk_table_attach(jb, hc_psize_sb(2), 4_c_int, 0_c_int)

    junk = hl_gtk_button_new("Centre"//c_null_char, &
         & clicked=c_funloc(gr_hc_centre), tooltip= &
         & "Centre the plot on the physical page."//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int)

    junk = gtk_label_new("Offset: X (cm)"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 1_c_int, 1_c_int)

    hc_off_sb(1) = hl_gtk_spin_button_new(0._c_double, 100._c_double, &
         & 0.01_c_double, initial_value=real(hardset%off(1), c_double), &
         & tooltip=&
         & "Set the X offset of the 'paper' for hardcopies"//c_null_char)
    call hl_gtk_table_attach(jb, hc_off_sb(1), 2_c_int, 1_c_int)

    junk = gtk_label_new("Y (cm)"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 3_c_int, 1_c_int)

    hc_off_sb(2) = hl_gtk_spin_button_new(0._c_double, 100._c_double, &
         & 0.01_c_double, initial_value=real(hardset%off(2), c_double), &
         &  tooltip=&
         & "Set the Y offset of the 'paper' for hardcopies"//c_null_char)
    call hl_gtk_table_attach(jb, hc_off_sb(2), 4_c_int, 1_c_int)

    hc_ls_but = hl_gtk_check_button_new("Use landscape orientation"//&
         & c_null_char, &
         & toggled=c_funloc(gr_hc_orient), &
         & initial_state=f_c_logical(hardset%orient), tooltip=&
         & "Select landscape or portrait mode for physical paper plots"&
         & //c_null_char)
    call hl_gtk_table_attach(jb, hc_ls_but, 0_c_int, 2_c_int, xspan=2_c_int)

    hc_col_but =  hl_gtk_check_button_new("Make colour plots"//c_null_char, &
         & initial_state=f_c_logical(hardset%colour), tooltip=&
         & "Select colour or black & white outputs"//c_null_char)
    call hl_gtk_table_attach(jb, hc_col_but, 2_c_int, 2_c_int, xspan=2_c_int)

    hc_ts_but = hl_gtk_check_button_new("Add timestamp"//c_null_char, &
         & initial_state=f_c_logical(hardset%timestamp), tooltip=&
         & "Select whether to add a timestamp to the output plot"//c_null_char)
    call hl_gtk_table_attach(jb, hc_ts_but, 0_c_int, 3_c_int, xspan=2_c_int)

    jb = hl_gtk_table_new()
    call hl_gtk_box_pack(base, jb)

    junk = gtk_label_new("File stem:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 0_c_int)

    hc_name_entry = hl_gtk_entry_new(value=trim(hardset%name)//c_null_char, &
         & tooltip="Enter the name stem for the output file"//c_null_char)
    call hl_gtk_table_attach(jb, hc_name_entry, 1_c_int, 0_c_int, &
         & xspan=3_c_int)

    junk = gtk_label_new("Print cmd:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int)

    hc_lp_entry = hl_gtk_entry_new(value=trim(hardset%action(1))//c_null_char,&
         & tooltip="Enter the command to print PS output"//c_null_char)
    call hl_gtk_table_attach(jb, hc_lp_entry, 1_c_int, 1_c_int)

    junk = gtk_label_new("View cmd:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 2_c_int, 1_c_int)

    hc_view_cbo = hl_gtk_combo_box_new(has_entry=TRUE, &
         & initial_choices=viewnames, tooltip= &
         & "Select a viewer for EPS files"//c_null_char)

    call hl_gtk_combo_box_add_text(hc_view_cbo, "<none>"//c_null_char, &
         & at_start=TRUE)
    if (iviewer > size(viewnames)) &
         & call hl_gtk_combo_box_add_text(hc_view_cbo, &
         & trim(hardset%viewer(1))//c_null_char)

    call gtk_combo_box_set_active(hc_view_cbo, iviewer)
    call hl_gtk_table_attach(jb, hc_view_cbo, 3_c_int, 1_c_int)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    junk = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_hc_quit), data=c_loc(iapply(2)), &
         & tooltip = "Apply the changes and remove the window"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_hc_quit), data=c_loc(iapply(1)), &
         & tooltip = "Remove the window, without applying the changes."&
         & //c_null_char)
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(hc_window)

  end subroutine gr_hc_menu

  recursive subroutine gr_hc_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Exit hardcopy settings

    logical, pointer :: apply
    type(graff_hard), pointer :: hardset
    integer :: i
    integer(kind=c_int) :: vsel
    character(len=120) :: vtext

    call c_f_pointer(data, apply)

    if (apply) then
       hardset => pdefs%hardset
       hardset%psize = int(gtk_combo_box_get_active(hc_paper_cbo), int8)
       do i = 1, 2
          hardset%size(i) = real(hl_gtk_spin_button_get_value(hc_psize_sb(i)),&
               & real32)
          hardset%off(i) = real(hl_gtk_spin_button_get_value(hc_off_sb(i)), &
               & real32)
       end do

       hardset%orient = c_f_logical(gtk_toggle_button_get_active(hc_ls_but))
       hardset%colour = c_f_logical(gtk_toggle_button_get_active(hc_col_but))
       hardset%timestamp = c_f_logical(gtk_toggle_button_get_active(hc_ts_but))

       call hl_gtk_entry_get_text(hc_name_entry, hardset%name)
       call hl_gtk_entry_get_text(hc_lp_entry, hardset%action(1))

       vsel = hl_gtk_combo_box_get_active(hc_view_cbo, ftext=vtext)
       if (vsel == 0) then
          hardset%viewer(1) = ''
       else
          hardset%viewer(1) = vtext
       end if
       pdefs%chflag = .true.
       pdefs%transient%changes = pdefs%transient%changes + 1_int16
    end if

    call gtk_widget_destroy(hc_window)

  end subroutine gr_hc_quit


  subroutine gr_hc_centre(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Centre the plotting area on the page.

    integer(kind=c_int) :: ipap
    logical :: iland
    real(kind=c_double) :: off, sz
    integer :: i, j

    ipap = gtk_combo_box_get_active(hc_paper_cbo)+1
    iland = c_f_logical(gtk_toggle_button_get_active(hc_ls_but))

    do i = 1, 2
       if (iland) then
          j = 3-i
       else
          j = i
       end if
       sz = hl_gtk_spin_button_get_value(hc_psize_sb(i))
       off = (phys_size(j,ipap) - sz)/2._c_double
       call hl_gtk_spin_button_set_value(hc_off_sb(i), max(off, 0._c_double))
    end do

  end subroutine gr_hc_centre
  subroutine gr_hc_orient(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Swap page orientation.

    real(kind=c_double), dimension(2) :: tmp

    tmp(1) = hl_gtk_spin_button_get_value(hc_psize_sb(1))
    tmp(2) = hl_gtk_spin_button_get_value(hc_psize_sb(2))
    call hl_gtk_spin_button_set_value(hc_psize_sb(1), tmp(2))
    call hl_gtk_spin_button_set_value(hc_psize_sb(2), tmp(1))

    tmp(1) = hl_gtk_spin_button_get_value(hc_off_sb(1))
    tmp(2) = hl_gtk_spin_button_get_value(hc_off_sb(2))
    call hl_gtk_spin_button_set_value(hc_off_sb(1), tmp(2))
    call hl_gtk_spin_button_set_value(hc_off_sb(2), tmp(1))

  end subroutine gr_hc_orient

   
end module gr_menu_hc_widgets
