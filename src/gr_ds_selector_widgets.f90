module gr_ds_selector_widgets
  use iso_fortran_env
  use iso_c_binding

  use gtk_hl
  use gtk, only: gtk_container_add, gtk_widget_destroy, gtk_widget_show_all, &
       & TRUE, FALSE, GTK_POLICY_NEVER

  use graff_types
  use graff_globals
  use gr_utils

  use gr_plot
  use gr_cb_common
  use gr_ds_tools

  implicit none

  type(c_ptr), private :: sel_window, sel_list

contains

  subroutine gr_ds_select

    ! Select a DS as current.

    type(c_ptr) :: base, sbox, jb, junk
    logical, target, dimension(2) :: iapply = [.false., .true.]
    integer(kind=c_int) :: i

    sel_window = hl_gtk_window_new("Select dataset"//c_null_char, &
         & destroy=c_funloc(gr_ds_sel_quit) ,data_destroy=c_loc(iapply(1)), &
         & parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(sel_window, base)

    sel_list = hl_gtk_listn_new(sbox, hscroll_policy=GTK_POLICY_NEVER, &
         & titles = [character(len=11) :: &
         & 'DS #', 'Type', 'N', 'N2', 'Description'], &
         &  types = [g_type_int, g_type_string, g_type_int, &
         & g_type_int, g_type_string], height=300_c_int)

    call hl_gtk_box_pack(base, sbox)

    call hl_gtk_listn_ins(sel_list, count=int(pdefs%nsets, c_int))

    do i = 1_c_int, int(pdefs%nsets, c_int)
       call hl_gtk_listn_set_cell(sel_list, i-1, 0, ivalue=i)
       call hl_gtk_listn_set_cell(sel_list, i-1, 1, &
            & svalue = trim(typecodes(pdefs%data(i)%type))//c_null_char)
       call hl_gtk_listn_set_cell(sel_list, i-1, 2, &
            & ivalue = int(pdefs%data(i)%ndata, c_int))
       if (pdefs%data(i)%type == 9 .or. pdefs%data(i)%type == -4) &
            & call hl_gtk_listn_set_cell(sel_list, i-1, 3, &
            & ivalue = int(pdefs%data(i)%ndata2, c_int))
       call hl_gtk_listn_set_cell(sel_list, i-1, 4, &
            & svalue = trim(pdefs%data(i)%descript)//c_null_char)
    end do

    call hl_gtk_listn_set_selection(sel_list, int(pdefs%cset-1, c_int))

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    junk = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_ds_sel_quit), data=c_loc(iapply(2)))
    call hl_gtk_box_pack(jb, junk)
    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_ds_sel_quit), data=c_loc(iapply(1)))
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(sel_window)

  end subroutine gr_ds_select

  recursive subroutine gr_ds_sel_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Quit selector

    logical, pointer :: apply
    integer(kind=c_int) :: nsel
    integer(kind=c_int), dimension(:), allocatable :: isel

    call c_f_pointer(data, apply)

    if (apply) then
       nsel = hl_gtk_listn_get_selections(sel_list, indices=isel)
       if (nsel > 0) then
          call gr_set_values_dataset(int(isel(1)+1, int16))
          call gr_plot_draw(.true.)
       end if
    end if

    call gtk_widget_destroy(sel_window)

  end subroutine gr_ds_sel_quit

end module gr_ds_selector_widgets
