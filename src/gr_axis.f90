module gr_axis
  use iso_c_binding
  use iso_fortran_env

  use gtk_hl

  use g, only: g_object_set_data, g_object_get_data

  use gtk, only: gtk_check_menu_item_get_active, gtk_container_add, &
       & gtk_entry_set_text, &
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
         & sensitive = f_c_logical(pdefs%axrange(1,axis) > 0), &
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
         & initial_state=f_c_logical(pdefs%axsty(axis)%minor /= 0), &
         & tooltip = "Toggle display of minor tick marks"//c_null_char)

    ann_chb(axis) = hl_gtk_check_menu_item_new(jmnu, &
         & "Suppress annotation"//c_null_char, toggled=c_funloc(gr_set_annot), &
         & data = c_loc(axis), &
         & initial_state=f_c_logical(btest(pdefs%axsty(axis)%extra, &
         & annot_bit)), &
         &  tooltip="Switch axis labelling on/off"//c_null_char)

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

    junk = hl_gtk_menu_item_new(smnu, "Extend or shrink"//c_null_char, &
         & activate=c_funloc(gr_auto_e), data=c_loc(axis), &
         & tooltip = "Extend or shrink the axis to fit the data"//c_null_char)
    call g_object_set_data(junk, "shrink"//c_null_char, c_loc(ishrink(2)))

    junk = hl_gtk_menu_item_new(jmnu, "Advanced"//c_null_char, &
         & activate=c_funloc(gr_axis_adv), data=c_loc(axis), &
         & tooltip = "Advanced axis settings"//c_null_char)

    junk= gtk_label_new(axname//" Min:"//c_null_char)
    call hl_gtk_table_attach(t, junk, 0,2, yopts=0)

    write(rtext, "(g0.5)") pdefs%axrange(1,axis)
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

    write(rtext, "(g0.5)") pdefs%axrange(2,axis)
    rbox(2, axis) = hl_gtk_entry_new(editable=TRUE, &
         & activate=c_funloc(gr_set_range), data=c_loc(axis), &  ! Needs tweak
         & focus_out_event=c_funloc(gr_set_range_e), &
         & data_focus_out=c_loc(axis), value=trim(rtext)//c_null_char, &
         & size=75_c_int, tooltip="Set lower limit for axis"//c_null_char)
    call g_object_set_data(rbox(2, axis), "minmax"//c_null_char, &
         & c_loc(minmax(2)))
    call hl_gtk_table_attach(t, rbox(2,axis), 3,2, yopts=0)

  end function gr_axis_new

  subroutine gr_set_label(widget, data) bind(c)
    type(c_ptr), value :: widget, data

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

  subroutine gr_set_log(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    integer, pointer :: axis

    if (.not. gui_active) return

    call c_f_pointer(data, axis)
    pdefs%axtype(axis) = int(gtk_check_menu_item_get_active(widget), int16)

    call gr_plot_draw(.true.)
 end subroutine gr_set_log

  subroutine gr_set_exact(widget, data) bind(c)
    type(c_ptr), value :: widget, data

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

    integer, pointer :: axis

    if (.not. gui_active) return

    call c_f_pointer(data, axis)

    pdefs%axsty(axis)%minor = &
         & int(gtk_check_menu_item_get_active(widget), int16)
    call gr_plot_draw(.true.)
  end subroutine gr_set_minor

  subroutine gr_set_annot(widget, data) bind(c)
    type(c_ptr), value :: widget, data

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

  subroutine gr_set_time(widget, data) bind(c)
    type(c_ptr), value :: widget, data

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

    integer, pointer :: axis

    if (.not. gui_active) return

    call c_f_pointer(data, axis)

    pdefs%axsty(axis)%grid = &
         & int(hl_gtk_radio_menu_group_get_select(grid_grp(axis)), int16)

    call gr_plot_draw(.true.)
  end subroutine gr_set_grid

  subroutine gr_auto_e(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    integer, pointer :: axis
    logical, pointer :: ishrink
    type(c_ptr) :: cshrink

    if (.not. gui_active) return

    call c_f_pointer(data, axis)
    cshrink = g_object_get_data(widget, "shrink"//c_null_char)
    call c_f_pointer(cshrink, ishrink)

    call gr_autoscale(axis, shrink=ishrink)

    call gr_plot_draw(.true.)
  end subroutine gr_auto_e

  subroutine gr_axis_adv(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    integer, pointer :: axis

    call c_f_pointer(data, axis)

    call gr_axis_menu(axis)

  end subroutine gr_axis_adv

  subroutine gr_set_range(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    type(c_ptr) :: cmm
    integer, pointer :: axis, mm
    character(len=32) :: text
    real(kind=real64) :: val
    integer :: ios

    if (.not. gui_active) return

    call c_f_pointer(data, axis)
    cmm = g_object_get_data(widget, 'minmax'//c_null_char)
    call c_f_pointer(cmm, mm)

    call hl_gtk_entry_get_text(widget, text)
    read(text, *, iostat=ios) val
    if (ios /= 0) then
       write(text, "(g0.5)") pdefs%axrange(mm,axis)
       call gtk_entry_set_text(widget, trim(text)//c_null_char)
    else
       pdefs%axrange(mm,axis) = val
    end if
    call gtk_widget_set_sensitive(log_chb(axis), &
         & f_c_logical(minval(pdefs%axrange(:,axis)) > 0.))

    call gr_plot_draw(.true.)
  end subroutine gr_set_range
  function gr_set_range_e(widget, event, data) bind(c) result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: widget, event, data

    call gr_set_range(widget, data)
    rv = FALSE
  end function gr_set_range_e
end module gr_axis
