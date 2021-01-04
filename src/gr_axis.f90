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

module gr_axis
  ! Widgets and handlers for primary axis settings options.

  use iso_c_binding
  use iso_fortran_env

  use gtk_hl

  use g, only: g_object_set_data, g_object_get_data

  use gtk, only: gtk_check_menu_item_get_active, gtk_entry_set_text, &
       & gtk_label_new, gtk_widget_set_sensitive, TRUE, FALSE

  use graff_types
  use graff_globals
  use gr_utils
  use gr_plot

  use gr_cb_common
  use gr_axis_widgets
  use gr_axis_autoscale

  implicit none

  character(len=2), dimension(3), parameter :: axname = ['X ','Y ', 'Yr']

contains
  function gr_axis_new(axis, sensitive) result(fr)
    type(c_ptr) :: fr
    integer, intent(in), target :: axis
    logical(kind=int8), intent(in), optional :: sensitive

    ! Define an Axis settings panel

    type(c_ptr) :: junk, t, menu, jmnu, smnu
    integer, dimension(2), target :: minmax=[1, 2]
    logical, dimension(2), target :: ishrink=[.false., .true.]

    character(len=32) :: rtext

    fr = hl_gtk_box_new()
    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(fr, f_c_logical(sensitive))

    junk = gtk_label_new(axname(axis)//"-Axis"//c_null_char)
    call hl_gtk_box_pack(fr, junk, expand=FALSE)
    t = hl_gtk_table_new()
    call hl_gtk_box_pack(fr, t, expand=FALSE)

    ! Axis label
    
    junk = gtk_label_new(trim(axname(axis))//" Label:"//c_null_char)
    call hl_gtk_table_attach(t, junk, 0, 0, yopts=0)

    lbox(axis) = hl_gtk_entry_new(editable=TRUE, &
         & activate=c_funloc(gr_set_label), data=c_loc(axis), &
         & focus_out_event=c_funloc(gr_set_label_e), &
         & data_focus_out=c_loc(axis), &
         & value=trim(pdefs%axtitle(axis))//c_null_char, &
         & tooltip="Set the title for the "//trim(axname(axis))//&
         & " axis"//c_null_char)
    call hl_gtk_table_attach(t, lbox(axis), 1, 0, xspan=3, yopts=0)

    ! Axis options
    
    menu = hl_gtk_menu_new()
    call hl_gtk_table_attach(t, menu, 1, 1, xspan=2, yopts=0)

    jmnu = hl_gtk_menu_submenu_new(menu, &
         & trim(axname(axis))//" Style ▼"//c_null_char, &
         & tooltip="Set style options for the "//trim(axname(axis))//&
         & "axis"//c_null_char)

    log_chb(axis) = hl_gtk_check_menu_item_new(jmnu, &
         & "Logarithmic"//c_null_char, toggled=c_funloc(gr_set_log), &
         & data=c_loc(axis), &
         & initial_state=f_c_logical(pdefs%axtype(axis) /= 0), &
         & tooltip = "Select Log or linear axis"//c_null_char)

    exact_chb(axis) = hl_gtk_check_menu_item_new(jmnu, &
         & "Exact Range"//c_null_char, toggled = c_funloc(gr_set_exact), &
         & data=c_loc(axis), &
         & initial_state=f_c_logical(btest(pdefs%axsty(axis)%idl, exact_bit)), &
         & tooltip = "Select exact or automatic scaling"//c_null_char)

    ext_chb(axis) =  hl_gtk_check_menu_item_new(jmnu, &
         & "Extended Range"//c_null_char, toggled=c_funloc(gr_set_extend), &
         & data=c_loc(axis), &
         & initial_state=f_c_logical(btest(pdefs%axsty(axis)%idl, &
         & extend_bit)), &
         & tooltip = "Select extended axis range"//c_null_char)

    ax_chb(axis) = hl_gtk_check_menu_item_new(jmnu, &
         & "Supress Axes"//c_null_char, toggled=c_funloc(gr_set_ax_draw), &
         & data=c_loc(axis), &
         & initial_state=f_c_logical(btest(pdefs%axsty(axis)%idl, axis_bit)), &
         & tooltip = "Switch drawing of axis on/off"//c_null_char)

    bax_chb(axis) = hl_gtk_check_menu_item_new(jmnu, &
         & "Suppress Box Axis"//c_null_char, &
         & toggled=c_funloc(gr_set_bax_draw), &
         & data=c_loc(axis), &
         & initial_state=f_c_logical(btest(pdefs%axsty(axis)%idl, box_bit)), &
         & tooltip = "Switch drawing of box axis on/off"//c_null_char)

    minor_chb(axis) = hl_gtk_check_menu_item_new(jmnu, &
         & "Minor ticks"//c_null_char, toggled=c_funloc(gr_set_minor), &
         & data = c_loc(axis), &
         & initial_state=f_c_logical(pdefs%axsty(axis)%minor /= 1), &
         & tooltip = "Toggle display of minor tick marks"//c_null_char)

    ann_chb(axis) = hl_gtk_check_menu_item_new(jmnu, &
         & "Suppress annotation"//c_null_char, toggled=c_funloc(gr_set_annot), &
         & data = c_loc(axis), &
         & initial_state=f_c_logical(btest(pdefs%axsty(axis)%extra, &
         & annot_bit)), &
         &  tooltip="Switch axis labelling on/off"//c_null_char)

    if (axis /= 1) then
       rot_chb(axis) = hl_gtk_check_menu_item_new(jmnu, &
            & "Label along axis"//c_null_char, &
            & toggled=c_funloc(gr_set_ax_rot), &
            & data = c_loc(axis), initial_state=&
            & f_c_logical(btest(pdefs%axsty(axis)%extra, yrot_bit)), &
            & tooltip="Switch putting Y labels parallel to the axis"//&
            & c_null_char)
    end if

    time_chb(axis) = hl_gtk_check_menu_item_new(jmnu, &
         & "Time labels"//c_null_char, toggled=c_funloc(gr_set_time), &
         & data = c_loc(axis), &
         & initial_state=f_c_logical(btest(pdefs%axsty(axis)%time, time_bit)), &
         & tooltip="Switch axis time format labelling on/off"//c_null_char)

    smnu = hl_gtk_menu_submenu_new(jmnu, "Origin Axis"//c_null_char, &
         & tooltip="Set display of an axis through the origin"//c_null_char)

    origin_grp(axis) = c_null_ptr
    junk = hl_gtk_radio_menu_item_new(origin_grp(axis), smnu, &
         & "Off"//c_null_char, toggled=c_funloc(gr_set_origin), &
         & data=c_loc(axis), tooltip="No axis at the origin"//c_null_char)
    junk = hl_gtk_radio_menu_item_new(origin_grp(axis), smnu, &
         & "On"//c_null_char, toggled=c_funloc(gr_set_origin), &
         & data=c_loc(axis), tooltip="A plain axis at the origin"//c_null_char)
    junk = hl_gtk_radio_menu_item_new(origin_grp(axis), smnu, &
         & "Full"//c_null_char, toggled=c_funloc(gr_set_origin), &
         & data=c_loc(axis), tooltip="A labelled axis at the origin"//&
         & c_null_char)

    select case(iand(pdefs%axsty(axis)%extra, 10_int16))
    case(0,8)
       call hl_gtk_radio_menu_group_set_select(origin_grp(axis), 0)
    case(2)
       call hl_gtk_radio_menu_group_set_select(origin_grp(axis), 1)
    case(10)
       call hl_gtk_radio_menu_group_set_select(origin_grp(axis), 2)
    end select

    smnu = hl_gtk_menu_submenu_new(jmnu, "Grid style"//c_null_char, &
         tooltip = "Set a grid style"//c_null_char)

    grid_grp(axis) = c_null_ptr
    junk = hl_gtk_radio_menu_item_new(grid_grp(axis), smnu, &
         & "None"//c_null_char, toggled=c_funloc(gr_set_grid), &
         & data=c_loc(axis))
    junk = hl_gtk_radio_menu_item_new(grid_grp(axis), smnu, &
         & "______"//c_null_char, toggled=c_funloc(gr_set_grid), &
         & data=c_loc(axis))
    junk = hl_gtk_radio_menu_item_new(grid_grp(axis), smnu, &
         & "......"//c_null_char, toggled=c_funloc(gr_set_grid), &
         & data=c_loc(axis))
    junk = hl_gtk_radio_menu_item_new(grid_grp(axis), smnu, &
         & "_ _ _ "//c_null_char, toggled=c_funloc(gr_set_grid), &
         & data=c_loc(axis))
    junk = hl_gtk_radio_menu_item_new(grid_grp(axis), smnu, &
         & "_._._."//c_null_char, toggled=c_funloc(gr_set_grid), &
         & data=c_loc(axis))
    junk = hl_gtk_radio_menu_item_new(grid_grp(axis), smnu, &
         & "_...  "//c_null_char, toggled=c_funloc(gr_set_grid), &
         & data=c_loc(axis))
    junk = hl_gtk_radio_menu_item_new(grid_grp(axis), smnu, &
         & "__  __"//c_null_char, toggled=c_funloc(gr_set_grid), &
         & data=c_loc(axis))

    call hl_gtk_radio_menu_group_set_select(grid_grp(axis), &
         & int(pdefs%axsty(axis)%grid, c_int))

    smnu = hl_gtk_menu_submenu_new(jmnu, "Autoscale"//c_null_char, &
         tooltip = "Autoscale the axis"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Extend"//c_null_char, &
         & activate=c_funloc(gr_auto_e), data=c_loc(axis), &
         & tooltip = "Extend the axis to accomodate the data"//c_null_char)
    call g_object_set_data(junk, "shrink"//c_null_char, c_loc(ishrink(1)))
    call g_object_set_data(junk, "visible"//c_null_char, c_loc(ishrink(1)))
    call g_object_set_data(junk, "function"//c_null_char, c_loc(ishrink(2)))

    junk = hl_gtk_menu_item_new(smnu, "Extend or shrink"//c_null_char, &
         & activate=c_funloc(gr_auto_e), data=c_loc(axis), &
         & tooltip = "Extend or shrink the axis to fit the data"//c_null_char)
    call g_object_set_data(junk, "shrink"//c_null_char, c_loc(ishrink(2)))
    call g_object_set_data(junk, "visible"//c_null_char, c_loc(ishrink(1)))
    call g_object_set_data(junk, "function"//c_null_char, c_loc(ishrink(2)))

    junk = hl_gtk_menu_item_new(smnu, "X-Y data only"//c_null_char, &
         & activate=c_funloc(gr_auto_e), data=c_loc(axis), &
         & tooltip = "Extend or shrink the axis to fit the data"//c_null_char)
    call g_object_set_data(junk, "shrink"//c_null_char, c_loc(ishrink(2)))
    call g_object_set_data(junk, "visible"//c_null_char, c_loc(ishrink(1)))
    call g_object_set_data(junk, "function"//c_null_char, c_loc(ishrink(1)))

    junk = hl_gtk_menu_item_new(smnu, "Visible only"//c_null_char, &
         & activate=c_funloc(gr_auto_e), data=c_loc(axis), &
         & tooltip = "Extend or shrink the axis to fit the visible data"//c_null_char)
    call g_object_set_data(junk, "shrink"//c_null_char, c_loc(ishrink(1)))
    call g_object_set_data(junk, "visible"//c_null_char, c_loc(ishrink(2)))
    call g_object_set_data(junk, "function"//c_null_char, c_loc(ishrink(2)))

    junk = hl_gtk_menu_item_new(jmnu, "Advanced"//c_null_char, &
         & activate=c_funloc(gr_axis_adv), data=c_loc(axis), &
         & tooltip = "Advanced axis settings"//c_null_char)

    ! Axis range
    
    junk= gtk_label_new(axname(axis)//" Min:"//c_null_char)
    call hl_gtk_table_attach(t, junk, 0,2, yopts=0)

    write(rtext, "(1pg0.5)") pdefs%axrange(1,axis)
    rbox(1, axis) = hl_gtk_entry_new(editable=TRUE, &
         & activate=c_funloc(gr_set_range), data=c_loc(axis), & 
         & focus_out_event=c_funloc(gr_set_range_e), &
         & data_focus_out=c_loc(axis), value=trim(rtext)//c_null_char, &
         & size=75_c_int, tooltip="Set lower limit for axis"//c_null_char)
    call g_object_set_data(rbox(1, axis), "minmax"//c_null_char, &
         & c_loc(minmax(1)))
    call hl_gtk_table_attach(t, rbox(1,axis), 1,2, yopts=0)

    junk= gtk_label_new("Max:"//c_null_char)
    call hl_gtk_table_attach(t, junk, 2,2, yopts=0)

    write(rtext, "(1pg0.5)") pdefs%axrange(2,axis)
    rbox(2, axis) = hl_gtk_entry_new(editable=TRUE, &
         & activate=c_funloc(gr_set_range), data=c_loc(axis), &  ! Needs tweak
         & focus_out_event=c_funloc(gr_set_range_e), &
         & data_focus_out=c_loc(axis), value=trim(rtext)//c_null_char, &
         & size=75_c_int, tooltip="Set upper limit for axis"//c_null_char)
    call g_object_set_data(rbox(2, axis), "minmax"//c_null_char, &
         & c_loc(minmax(2)))
    call hl_gtk_table_attach(t, rbox(2,axis), 3,2, yopts=0)

  end function gr_axis_new

  subroutine gr_set_label(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set the axis label

    integer, pointer :: axis
    character(len=120) :: title

    if (.not. gui_active) return

    call c_f_pointer(data, axis)
    call hl_gtk_entry_get_text(widget, title)

    pdefs%axtitle(axis) = title

    call gr_plot_draw(.true.)
  end subroutine gr_set_label
  function gr_set_label_e(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, event, data

    call gr_set_label(widget, data)

    rv = FALSE
  end function gr_set_label_e

  subroutine gr_set_log(widget, wdata) bind(c)
    type(c_ptr), value :: widget, wdata

    ! Set log/linear scaling

    integer, pointer :: axis
    type(graff_data), pointer :: data
    integer :: i
    
    if (.not. gui_active) return

    call c_f_pointer(wdata, axis)
    pdefs%axtype(axis) = int(gtk_check_menu_item_get_active(widget), int16)

    ! A change of axis type requires new evaluation locations so flag
    ! functions as not-evaluated.
    
    do i = 1, pdefs%nsets
       data => pdefs%data(i)
       if (data%type == -1 .or. data%type == -4) data%funct%evaluated = .false.
       if ((data%type == -2 .or. data%type == -4) .and. &
            & (.not. pdefs%y_right .or. data%y_axis == axis-1)) &
            & data%funct%evaluated = .false.
    end do
!!$    if (minval(pdefs%axrange(:,axis)) <= 0.) &
!!$         & call gtk_widget_set_sensitive(widget, FALSE)
    
    call gr_plot_draw(.true.)
  end subroutine gr_set_log

  subroutine gr_set_exact(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set exact/rounded scaling

    integer, pointer :: axis

    if (.not. gui_active) return

    call c_f_pointer(data, axis)

    if (c_f_logical(gtk_check_menu_item_get_active(widget))) then
       pdefs%axsty(axis)%idl = ibset(pdefs%axsty(axis)%idl, exact_bit)
    else
       pdefs%axsty(axis)%idl = ibclr(pdefs%axsty(axis)%idl, exact_bit)
    end if
    call gr_plot_draw(.true.)
  end subroutine gr_set_exact

  subroutine gr_set_extend(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set extended axis range

    integer, pointer :: axis

    if (.not. gui_active) return

    call c_f_pointer(data, axis)

    if (c_f_logical(gtk_check_menu_item_get_active(widget))) then
       pdefs%axsty(axis)%idl = ibset(pdefs%axsty(axis)%idl, extend_bit)
    else
       pdefs%axsty(axis)%idl = ibclr(pdefs%axsty(axis)%idl, extend_bit)
    end if
    call gr_plot_draw(.true.)
  end subroutine gr_set_extend

  subroutine gr_set_ax_draw(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set drawing of axis

    integer, pointer :: axis

    if (.not. gui_active) return

    call c_f_pointer(data, axis)

    if (c_f_logical(gtk_check_menu_item_get_active(widget))) then
       pdefs%axsty(axis)%idl = ibset(pdefs%axsty(axis)%idl, axis_bit)
    else
       pdefs%axsty(axis)%idl = ibclr(pdefs%axsty(axis)%idl, axis_bit)
    end if
    call gr_plot_draw(.true.)
  end subroutine gr_set_ax_draw

  subroutine gr_set_bax_draw(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set drawing of box axis.

    integer, pointer :: axis

    if (.not. gui_active) return

    call c_f_pointer(data, axis)

    if (c_f_logical(gtk_check_menu_item_get_active(widget))) then
       pdefs%axsty(axis)%idl = ibset(pdefs%axsty(axis)%idl, box_bit)
    else
       pdefs%axsty(axis)%idl = ibclr(pdefs%axsty(axis)%idl, box_bit)
    end if
    call gr_plot_draw(.true.)
  end subroutine gr_set_bax_draw

  subroutine gr_set_minor(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Enable/disable minor ticks

    integer, pointer :: axis

    if (.not. gui_active) return

    call c_f_pointer(data, axis)
    if (c_f_logical(gtk_check_menu_item_get_active(widget))) then
       pdefs%axsty(axis)%minor = 0
    else
       pdefs%axsty(axis)%minor = 1
    end if
    call gr_plot_draw(.true.)
  end subroutine gr_set_minor

  subroutine gr_set_annot(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Enable/disable annotations.

    integer, pointer :: axis

    if (.not. gui_active) return

    call c_f_pointer(data, axis)

    if (c_f_logical(gtk_check_menu_item_get_active(widget))) then
       pdefs%axsty(axis)%extra = ibset(pdefs%axsty(axis)%extra, annot_bit)
    else
       pdefs%axsty(axis)%extra = ibclr(pdefs%axsty(axis)%extra, annot_bit)
    end if
    call gr_plot_draw(.true.)
  end subroutine gr_set_annot

  subroutine gr_set_ax_rot(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Y-axis annotation orientation.

    integer, pointer :: axis

    if (.not. gui_active) return

    call c_f_pointer(data, axis)

    if (c_f_logical(gtk_check_menu_item_get_active(widget))) then
       pdefs%axsty(axis)%extra = ibset(pdefs%axsty(axis)%extra, yrot_bit)
    else
       pdefs%axsty(axis)%extra = ibclr(pdefs%axsty(axis)%extra, yrot_bit)
    end if
    call gr_plot_draw(.true.)
  end subroutine gr_set_ax_rot

  subroutine gr_set_time(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set time labelling

    integer, pointer :: axis

    if (.not. gui_active) return

    call c_f_pointer(data, axis)

    if (c_f_logical(gtk_check_menu_item_get_active(widget))) then
       pdefs%axsty(axis)%time = ibset(pdefs%axsty(axis)%time, time_bit)
       call gr_time_menu(axis)
    else
       pdefs%axsty(axis)%time = ibclr(pdefs%axsty(axis)%time, time_bit)
    end if
    call gr_plot_draw(.true.)
  end subroutine gr_set_time

  subroutine gr_set_origin(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set origin axis drawing

    integer, pointer :: axis
    integer(kind=c_int) :: idx

    if (.not. gui_active) return

    call c_f_pointer(data, axis)

    idx = hl_gtk_radio_menu_group_get_select(origin_grp(axis))

    select case (idx)
    case(0)
       pdefs%axsty(axis)%extra = ibclr(pdefs%axsty(axis)%extra, origin_bit)
       pdefs%axsty(axis)%extra = ibclr(pdefs%axsty(axis)%extra, full_bit)
    case(1)
       pdefs%axsty(axis)%extra = ibset(pdefs%axsty(axis)%extra, origin_bit)
       pdefs%axsty(axis)%extra = ibclr(pdefs%axsty(axis)%extra, full_bit)
    case(2)
       pdefs%axsty(axis)%extra = ibset(pdefs%axsty(axis)%extra, origin_bit)
       pdefs%axsty(axis)%extra = ibset(pdefs%axsty(axis)%extra, full_bit)
    end select
    call gr_plot_draw(.true.)
  end subroutine gr_set_origin

  subroutine gr_set_grid(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set grid line style.

    integer, pointer :: axis

    if (.not. gui_active) return

    call c_f_pointer(data, axis)

    pdefs%axsty(axis)%grid = &
         & int(hl_gtk_radio_menu_group_get_select(grid_grp(axis)), int16)

    call gr_plot_draw(.true.)
  end subroutine gr_set_grid

  subroutine gr_auto_e(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Autoscale axis.

    integer, pointer :: axis
    logical, pointer :: ishrink, ivisible, ifunction
    type(c_ptr) :: cshrink, cvisible, cfunction

    if (.not. gui_active) return

    call c_f_pointer(data, axis)
    
    cshrink = g_object_get_data(widget, "shrink"//c_null_char)
    call c_f_pointer(cshrink, ishrink)
    cvisible = g_object_get_data(widget, "visible"//c_null_char)
    call c_f_pointer(cvisible, ivisible)
    cfunction = g_object_get_data(widget, "function"//c_null_char)
    call c_f_pointer(cfunction, ifunction)
    
    call gr_autoscale(axis, shrink=ishrink, visible=ivisible, &
         & functions=ifunction)

    call gr_plot_draw(.true.)
  end subroutine gr_auto_e

  subroutine gr_axis_adv(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Advanced options.

    integer, pointer :: axis

    call c_f_pointer(data, axis)

    call gr_axis_menu(axis)

  end subroutine gr_axis_adv

  subroutine gr_set_range(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set axis range

    type(c_ptr) :: cmm
    integer, pointer :: axis, mm
    character(len=32) :: text
    real(kind=real64) :: val
    integer :: ios
    integer(kind=int16) :: i

    if (.not. gui_active) return

    call c_f_pointer(data, axis)
    cmm = g_object_get_data(widget, 'minmax'//c_null_char)
    call c_f_pointer(cmm, mm)

    call hl_gtk_entry_get_text(widget, text)
    read(text, *, iostat=ios) val
    if (ios /= 0) then
       write(text, "(1pg0.5)") pdefs%axrange(mm,axis)
       call gtk_entry_set_text(widget, trim(text)//c_null_char)
    else if (val /= pdefs%axrange(mm,axis)) then
       pdefs%axrange(mm,axis) = val
       do i = 1, pdefs%nsets
          select case (pdefs%data(i)%type)
          case(-1) 
             if (axis == 1 .and.&
                  & pdefs%data(i)%funct%range(1,1) == &
                  & pdefs%data(i)%funct%range(2,1)) &
                  & pdefs%data(i)%funct%evaluated = .false.
          case(-2)
             if (((axis == 2 .and. pdefs%data(i)%y_axis == 0) .or.&
                  & (axis == 2 .and. pdefs%data(i)%y_axis == 1) .and. &
                  & pdefs%data(i)%funct%range(1,1) == &
                  & pdefs%data(i)%funct%range(2,1))) &
                  & pdefs%data(i)%funct%evaluated = .false.
          case(-4)
             if ((axis == 1 .and. &
                  & pdefs%data(i)%funct%range(1,1) == &
                  & pdefs%data(i)%funct%range(2,1)) .or.  &
                  & (((axis == 2 .and. pdefs%data(i)%y_axis == 0) .or.&
                  & (axis == 2 .and. pdefs%data(i)%y_axis == 1)) .and. &
                  & pdefs%data(i)%funct%range(1,2) == &
                  & pdefs%data(i)%funct%range(2,2))) &
                  & pdefs%data(i)%funct%evaluated = .false.
          end select
       end do
    end if

!!$    if (minval(pdefs%axrange(:,axis)) > 0.) then
!!$       call gtk_widget_set_sensitive(log_chb(axis), TRUE)
!!$    else
    if (minval(pdefs%axrange(:,axis)) <= 0. .and. &
         & pdefs%axtype(axis) == 1) then
       call hl_gtk_info_bar_message(gr_infobar, &
            & "Setting a zero or negative limit for a log axis"//c_null_char)
       return
!!$    else
!!$       call gtk_widget_set_sensitive(log_chb(axis), FALSE)
    end if
    call gr_plot_draw(.true.)
  end subroutine gr_set_range
  function gr_set_range_e(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, event, data

    call gr_set_range(widget, data)
    rv = FALSE
  end function gr_set_range_e
end module gr_axis
