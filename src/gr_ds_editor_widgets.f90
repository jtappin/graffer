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

module gr_ds_editor_widgets
  ! Editor for X-Y datasets

  use iso_fortran_env
  use iso_c_binding

  use gtk_hl
  use gtk, only: gtk_container_add, gtk_label_new, gtk_widget_destroy, &
       & gtk_widget_set_sensitive, gtk_widget_show_all, &
       & gtk_combo_box_get_active, TRUE, FALSE, &
       & GTK_POLICY_NEVER

  use graff_types
  use graff_globals
  use gr_utils

  use gr_plot
  use gr_cb_common
  use gr_ds_tools

  implicit none

  type(c_ptr), private :: edit_window, error_grp, value_view, opt_cbo
  type(c_ptr), dimension(9), private :: error_buts

contains

  subroutine gr_ds_editor

    type(graff_data), pointer :: data
    character(len=40) :: title
    logical, dimension(2), target :: iupdate = [.false., .true.]
    type(c_ptr) :: base, junk, sbox, jb, mnu, smnu
    character(len=150), dimension(:), allocatable :: values
    integer :: i, nfields, nxe, nye
    character(len=11), dimension(9), parameter :: errnames = &
         & [character(len=11) :: 'None', '±Y', '-Y +Y', '±X', '-X +X', &
         & '±X ±Y', '±X -Y +Y', '-X +X ±Y', '-X +X -Y +Y']
    character(len=13), dimension(9), parameter :: errnames_p = &
         & [character(len=13) :: 'None', '±Θ', '-Θ +Θ', '±R', '-R +R', &
         & '±R ±Θ', '±R -Θ +Θ', '-R +R ±Θ', '-R +R -Θ +Θ']

    character(len=7), dimension(3), parameter :: err_actions = &
         &[character(len=7) :: 'Abort', 'Re-edit', 'Skip']
    
    data => pdefs%data(pdefs%cset)

    write(title, "('Dataset Editor, DS #', i0)") pdefs%cset
    edit_window = hl_gtk_window_new(trim(title)//c_null_char, &
         & destroy=c_funloc(gr_editor_quit), &
         & data_destroy=c_loc(iupdate(1)), &
         & parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(edit_window, base)

    junk = gtk_label_new(trim(title)//c_null_char)
    call hl_gtk_box_pack(base, junk, expand=FALSE)

    if (data%descript /= '') then
       junk = gtk_label_new(trim(data%descript)//c_null_char)
       call hl_gtk_box_pack(base, junk, expand=FALSE)
    end if

    if (allocated(data%xydata%x)) then
       nxe = nx_errors(data%type)
       nye = ny_errors(data%type)
       allocate(values(data%ndata))
       nfields = 2 + nxe + nye
       if (nxe > 0 .and. nye > 0) then
          do i = 1, data%ndata
             write(values(i), "(6(1pg0,2x))") data%xydata%x(i), &
                  & data%xydata%y(i), data%xydata%x_err(:,i), &
                  & data%xydata%y_err(:,i) 
          end do
       else if (nxe > 0) then
          do i = 1, data%ndata
             write(values(i), "(6(1pg0,2x))") data%xydata%x(i), &
                  & data%xydata%y(i), data%xydata%x_err(:,i)
          end do
       else if (nye > 0) then
          do i = 1, data%ndata
             write(values(i), "(6(1pg0,2x))") data%xydata%x(i), &
                  & data%xydata%y(i), data%xydata%y_err(:,i) 
          end do
       else
          do i = 1, data%ndata
             write(values(i), "(6(1pg0,2x))") data%xydata%x(i), &
                  & data%xydata%y(i)
          end do
       end if
    else
       allocate(values(1))
       values(1) = ''
       nfields = 2
    end if

    value_view = hl_gtk_text_view_new(scroll=sbox, &
         & initial_text=values, ssize=[-1_c_int, 400_c_int], &
         & changed=c_funloc(gr_edit_update), &
         & hscroll_policy=GTK_POLICY_NEVER)

    call hl_gtk_box_pack(base, sbox)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb)

    mnu = hl_gtk_menu_new()
    call hl_gtk_box_pack(jb, mnu) ! , expand=FALSE)
    
    smnu = hl_gtk_menu_submenu_new(mnu, "Error bars ▼"//c_null_char)

    error_grp = c_null_ptr
    if (data%mode == 0) then
       do i = 1, 9
          error_buts(i) = hl_gtk_radio_menu_item_new(error_grp, &
               & smnu, trim(errnames(i))//c_null_char)
       end do
    else
       do i = 1, 9
          error_buts(i) = hl_gtk_radio_menu_item_new(error_grp, &
               & smnu, trim(errnames_p(i))//c_null_char)
       end do
    end if
    call gr_edit_err_setup(nfields, init=.true.)

    junk = gtk_label_new("Error action:"//c_null_char)
    call hl_gtk_box_pack(jb, junk)
    
    opt_cbo = hl_gtk_combo_box_new(initial_choices=err_actions, &
         & active=1_c_int, tooltip= &
         & "Action on invalid data."//c_null_char)
    call hl_gtk_box_pack(jb, opt_cbo)
 
    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    junk = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_editor_quit), data=c_loc(iupdate(2)))
    call hl_gtk_box_pack(jb, junk)

    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_editor_quit), data=c_loc(iupdate(1)))
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(edit_window)

  end subroutine gr_ds_editor

  subroutine gr_edit_err_setup(nfields, init)
    integer, intent(in) :: nfields
    logical, intent(in) :: init

    ! Set valid error options for no of cols.

    integer(kind=c_int), dimension(9) :: smask
    integer :: i
    integer(kind=c_int) :: isel

    if (init) then
       isel = int(pdefs%data(pdefs%cset)%type, c_int)
       if (isel > 8 .or. isel < 0) isel = 0_c_int
       call hl_gtk_radio_menu_group_set_select(error_grp, isel)
    else
       isel = hl_gtk_radio_menu_group_get_select(error_grp)
    end if

    smask = FALSE
    select case(nfields)
    case(1,2)
       smask(1) = TRUE
       if (isel /= 0) &
            & call hl_gtk_radio_menu_group_set_select(error_grp, 0_c_int)
    case(3)
       smask([2,4]) = TRUE
       if (isel /= 1 .and. isel /= 3) &
            & call hl_gtk_radio_menu_group_set_select(error_grp, 1_c_int)
    case(4)
       smask([3,5,6]) = TRUE
       if (isel /= 2 .and. isel /= 4 .and. isel /= 5) &
            & call hl_gtk_radio_menu_group_set_select(error_grp, 2_c_int)
    case(5)
       if (isel /= 6 .and. isel /= 7) &
            & call hl_gtk_radio_menu_group_set_select(error_grp, 6_c_int)
       smask([7,8]) = TRUE
    case(6)
       smask(9) = TRUE
       if (isel /= 8) &
            & call hl_gtk_radio_menu_group_set_select(error_grp, 8_c_int)
    end select

    do i = 1,9
       call gtk_widget_set_sensitive(error_buts(i), smask(i))
    end do

  end subroutine gr_edit_err_setup

  subroutine gr_edit_update(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Callback when contents changed.

    integer :: nfields, i
    character(len=200), dimension(:), allocatable :: cvals
    character(len=32), dimension(:), allocatable :: fields

    call hl_gtk_text_view_get_text(c_null_ptr, cvals, buffer=widget)
    do i =1, size(cvals)
       if (cvals(i) /= '') then
          call split(cvals(i), " 	,", fields, count=nfields)
          exit
       end if
    end do
    call gr_edit_err_setup(nfields, .false.)

  end subroutine gr_edit_update

  recursive subroutine gr_editor_quit(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    ! Quit the editor.

    logical, pointer :: iupdate
    character(len=200), dimension(:), allocatable :: cvals
    real(kind=real64), dimension(:,:), allocatable :: xyvals
    integer :: nlines, nfields, nxe, nye
    logical :: ok, keep
    type(graff_data), pointer :: data
    integer(kind=c_int) :: ieopt
    
    call c_f_pointer(gdata, iupdate)

    keep = .false.
    
    if (iupdate) then
       ieopt = gtk_combo_box_get_active(opt_cbo)
       
       call hl_gtk_text_view_get_text(value_view, cvals)
       call gr_xy_decode(cvals, xyvals, nlines, nfields, ok, &
            & skip = ieopt == 2)

       if (ok) then
          data => pdefs%data(pdefs%cset)

          if (allocated(data%xydata%x)) deallocate(data%xydata%x)
          if (allocated(data%xydata%y)) deallocate(data%xydata%y)
          if (allocated(data%xydata%x_err)) deallocate(data%xydata%x_err)
          if (allocated(data%xydata%y_err)) deallocate(data%xydata%y_err)
         
          if (allocated(data%zdata%x)) deallocate(data%zdata%x)
          if (allocated(data%zdata%y)) deallocate(data%zdata%y)
          if (allocated(data%zdata%z)) deallocate(data%zdata%z)

          data%ndata = nlines
          data%type = int(hl_gtk_radio_menu_group_get_select(error_grp), int16)
          call gtk_entry_set_text(ds_type_id, &
               & trim(typedescrs(data%type))//c_null_char)

          nxe = nx_errors(data%type)
          nye = ny_errors(data%type)
          allocate(data%xydata%x(nlines),data%xydata%y(nlines))
          
          data%xydata%x = xyvals(1,:)
          data%xydata%y = xyvals(2,:)
          if (nxe > 0) then
             allocate(data%xydata%x_err(nxe,nlines))
             data%xydata%x_err = xyvals(3:3+nxe-1,:)
          end if
          if (nye > 0) then
             allocate(data%xydata%y_err(nye,nlines))
             data%xydata%y_err = xyvals(3+nxe:,:)
          end if
          call gr_plot_draw(.true.)
       else
          keep = ieopt == 1
       end if
    end if

    if (.not. keep) call gtk_widget_destroy(edit_window)
    
  end subroutine gr_editor_quit

end module gr_ds_editor_widgets
