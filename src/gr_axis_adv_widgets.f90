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

module gr_axis_adv_widgets
  use iso_fortran_env
  use iso_c_binding

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_container_add, gtk_label_new, gtk_widget_destroy, &
       & gtk_widget_show_all, TRUE, FALSE

  use graff_types
  use graff_globals
  use gr_plot

  implicit none

  type(c_ptr), private :: adv_window, adv_major_entry, adv_format_entry, &
       & adv_minor_sb
  integer, private :: iaxis

contains
  subroutine gr_axis_menu(axis)
    integer, intent(in) :: axis

    ! Advanced axis settings.

    type(c_ptr) :: base, jb, junk
    character(len=2), dimension(3), parameter :: axnames = ['X ','Y ','Yr']
    logical, dimension(2), target :: iapply = [.false., .true.]
    type(graff_style), pointer :: axstyle
    character(len=32) :: tvalue
    real(kind=real64) :: tickstep
    integer :: p10
    axstyle => pdefs%axsty(axis)
    iaxis = axis

    if (axstyle%major /= 0 .and. axstyle%xmajor == 0.) then
       tickstep = abs(pdefs%axrange(2,axis)- pdefs%axrange(1,axis))/&
            & real(axstyle%major)
       p10 = floor(log10(tickstep))
       tickstep = tickstep/real(p10,real64)
       if (tickstep < 1.5) then
          tickstep = 1.0_real64
       else if (tickstep < 2.5) then
          tickstep = 2.0_real64
       else if (tickstep < 4.0) then
          tickstep = 3.0_real64
       else if (tickstep < 7.0) then
          tickstep = 5.0_real64
       else
          tickstep = 10.0_real64
       end if
       axstyle%xmajor = tickstep * 10._real64 ** p10
    end if

    adv_window = hl_gtk_window_new("Advanced "//trim(axnames(axis))//&
         & "-axis settings"//c_null_char, destroy=c_funloc(gr_adv_quit), &
         & data_destroy=c_loc(iapply(1)), parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(adv_window, base)

    jb = hl_gtk_table_new()
    call hl_gtk_box_pack(base, jb)

    junk = gtk_label_new("Major tick spacing:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 0_c_int)

    write(tvalue, "(g0.5)") axstyle%xmajor
    adv_major_entry = hl_gtk_entry_new(value=&
         & trim(adjustl(tvalue))//c_null_char, &
         & tooltip="Enter the spacing between major ticks"//c_new_line//&
         & "or 0 to use the default"//c_null_char)
    call hl_gtk_table_attach(jb, adv_major_entry, 1_c_int, 0_c_int)

    junk = gtk_label_new("Number of minor intervals:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int)

    adv_minor_sb = hl_gtk_spin_button_new(0_c_int, 50_c_int, &
         & initial_value = int(axstyle%minor, c_int), &
         & tooltip="Set the number of minor intervals between major ticks"&
         & //c_new_line//"0 = default, 1 = no minor ticks"//c_null_char)
    call hl_gtk_table_attach(jb, adv_minor_sb, 1_c_int, 1_c_int)

    junk = gtk_label_new("Label format:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 2_c_int)

    adv_format_entry = hl_gtk_entry_new(value=&
         & trim(axstyle%format)//c_null_char, tooltip=&
         & "Enter a format for axis labels (a valid Fortran format"//&
         & c_new_line//" in parentheses e.g. (f4.2)), blank to use default"//&
         & c_null_char)
    call hl_gtk_table_attach(jb, adv_format_entry, 1_c_int, 2_c_int)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb, expand=FALSE)

    junk = hl_gtk_button_new("Apply"//c_null_char, &
         & clicked=c_funloc(gr_adv_quit), data=c_loc(iapply(2)), &
         & tooltip="Apply the changes and exit the dialogue"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_adv_quit), data=c_loc(iapply(1)), &
         & tooltip="Exit the dialogue, without making changes"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(adv_window)
  end subroutine gr_axis_menu

  recursive subroutine gr_adv_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Quit advanced axis settings.

    logical, pointer :: apply
    type(graff_style), pointer :: axstyle
    character(len=32) :: tvalue
    integer :: ios
    real(kind=real64) :: xm
    character(len=120) :: iom

    call c_f_pointer(data, apply)

    if (apply) then
       axstyle => pdefs%axsty(iaxis)

       call hl_gtk_entry_get_text(adv_major_entry, text=tvalue)
       if (len_trim(tvalue) > 0) then
          read(tvalue, *, iostat=ios, iomsg=iom) xm
          if (ios /= 0) then
             write(error_unit, "(a/t10,a)") &
                  & "gr_adv_quit: Failed to read major tick spacing", trim(iom)
          else
             axstyle%xmajor = xm
          end if
       end if

       axstyle%minor = int(hl_gtk_spin_button_get_value(adv_minor_sb), int16)
       call hl_gtk_entry_get_text(adv_format_entry, text=tvalue)
       if (tvalue /= '') then
          tvalue = adjustl(tvalue)
          if (tvalue(1:1) /= '(') &
               & tvalue = '('//trim(tvalue)
          if (tvalue(len_trim(tvalue):len_trim(tvalue)) /= ')') &
               & tvalue = trim(tvalue)//')'
       end if
       axstyle%format = tvalue

       call gr_plot_draw(.true.)
    end if

    iaxis = 0
    call gtk_widget_destroy(adv_window)

  end subroutine gr_adv_quit

end module gr_axis_adv_widgets
