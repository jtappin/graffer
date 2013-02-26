module gr_ds_merge_widgets
  use iso_fortran_env
  use iso_c_binding

  use gtk_hl
  use gtk, only: gtk_container_add, gtk_label_new, gtk_widget_destroy, &
       & gtk_widget_set_sensitive, gtk_widget_show_all, TRUE, FALSE, &
       & GTK_POLICY_NEVER

  use graff_types
  use graff_globals
  use gr_utils

  use gr_plot
  use gr_cb_common
  use gr_ds_tools

  implicit none

  type(c_ptr), private :: merge_window, merge_list1, merge_list2, merge_dobut
  integer(kind=c_int), private :: merge_target, merge_source

contains
  subroutine gr_ds_merge

    type(c_ptr) :: base, hbase, junk, jb, sbox
    logical, target, dimension(2) :: iapply = [.false., .true.]
    integer(kind=c_int) :: i, j

    merge_target = 0_c_int
    merge_source = 0_c_int

    merge_window = hl_gtk_window_new("Merge datasets"//c_null_char,&
         & destroy=c_funloc(gr_ds_merge_quit), data_destroy=c_loc(iapply(1)), &
         & parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(merge_window, base)

    hbase =  hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, hbase)

    jb = hl_gtk_box_new()
    call hl_gtk_box_pack(hbase, jb)

    junk = gtk_label_new("Dataset to extend"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    merge_list1 = hl_gtk_listn_new(sbox, hscroll_policy=GTK_POLICY_NEVER, &
         & titles = [character(len=11) :: &
         & 'DS #', 'Type', 'N', 'Description'], &
         &  types = [g_type_int, g_type_string, g_type_int, &
         & g_type_string], height=300_c_int, &
         & changed=c_funloc(gr_ds_merge_set_t))
    call hl_gtk_box_pack(jb, sbox)

    j = 0
    do i = 1_c_int, int(pdefs%nsets, c_int)
       if (pdefs%data(i)%type < 0 .or. pdefs%data(i)%type == 9) cycle
       call hl_gtk_listn_ins(merge_list1)
       call hl_gtk_listn_set_cell(merge_list1, j, 0, ivalue=i)
       call hl_gtk_listn_set_cell(merge_list1, j, 1, &
            & svalue = trim(typecodes(pdefs%data(i)%type))//c_null_char)
       call hl_gtk_listn_set_cell(merge_list1, j, 2, &
            & ivalue = int(pdefs%data(i)%ndata, c_int))
       call hl_gtk_listn_set_cell(merge_list1, j, 3, &
            & svalue = trim(pdefs%data(i)%descript)//c_null_char)
       j = j+1
    end do

    jb = hl_gtk_box_new()
    call hl_gtk_box_pack(hbase, jb)

    junk = gtk_label_new("Dataset to append"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    merge_list2 = hl_gtk_listn_new(sbox, hscroll_policy=GTK_POLICY_NEVER, &
         & titles = [character(len=11) :: &
         & 'DS #', 'Type', 'N', 'Description'], &
         &  types = [g_type_int, g_type_string, g_type_int, &
         & g_type_string], height=300_c_int, &
         & changed=c_funloc(gr_ds_merge_set_s))
    call hl_gtk_box_pack(jb, sbox)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    merge_dobut = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_ds_merge_quit), data=c_loc(iapply(2)), &
         & sensitive=FALSE)
    call hl_gtk_box_pack(jb, merge_dobut)

    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_ds_merge_quit), data=c_loc(iapply(1)))
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(merge_window)

  end subroutine gr_ds_merge

  subroutine gr_ds_merge_set_t(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    integer(kind=c_int) :: nsel
    integer(kind=c_int), dimension(:), allocatable :: isel
    integer(kind=c_int) :: type, i, j

    nsel = hl_gtk_listn_get_selections(merge_list1, indices=isel)
    if (nsel /= 1) return

    call hl_gtk_listn_rem(merge_list2)
    call gtk_widget_set_sensitive(merge_dobut, FALSE)

    call hl_gtk_listn_get_cell(merge_list1, isel(1), 0, ivalue=merge_target)
    type = pdefs%data(merge_target)%type

    j = 0
    do i = 1_c_int, int(pdefs%nsets, c_int)
       if (pdefs%data(i)%type /= type) cycle
       call hl_gtk_listn_ins(merge_list2)
       call hl_gtk_listn_set_cell(merge_list2, j, 0, ivalue=i)
       call hl_gtk_listn_set_cell(merge_list2, j, 1, &
            & svalue = trim(typecodes(pdefs%data(i)%type))//c_null_char)
       call hl_gtk_listn_set_cell(merge_list2, j, 2, &
            & ivalue = int(pdefs%data(i)%ndata, c_int))
       call hl_gtk_listn_set_cell(merge_list2, j, 3, &
            & svalue = trim(pdefs%data(i)%descript)//c_null_char)
       j = j+1
    end do
  end subroutine gr_ds_merge_set_t

  subroutine gr_ds_merge_set_s(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    integer(kind=c_int) :: nsel
    integer(kind=c_int), dimension(:), allocatable :: isel

    nsel = hl_gtk_listn_get_selections(merge_list2, indices=isel)
    if (nsel /= 1) return
    call hl_gtk_listn_get_cell(merge_list2, isel(1), 0, ivalue=merge_source)
    call gtk_widget_set_sensitive(merge_dobut, TRUE)
  end subroutine gr_ds_merge_set_s

  recursive subroutine gr_ds_merge_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    logical, pointer :: apply

    call c_f_pointer(data, apply)

    if (apply) then
       call gr_ds_append(int(merge_source, int16), int(merge_target, int16))

       if (merge_target == pdefs%cset)  call gr_set_values_dataset()
       call gr_plot_draw(.true.)
    end if

    call gtk_widget_destroy(merge_window)

  end subroutine gr_ds_merge_quit
end module gr_ds_merge_widgets
