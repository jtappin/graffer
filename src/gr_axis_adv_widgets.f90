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

module gr_axis_adv_widgets
  ! Widgets and handlers for advanced axis settings.

  use iso_fortran_env
  use iso_c_binding

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_container_add, gtk_label_new, gtk_widget_destroy, &
       & gtk_widget_show_all, TRUE, FALSE

  use graff_types
  use graff_globals
  use gr_plot
  use gr_cb_common

  implicit none

  type(c_ptr), private :: adv_window, adv_major_entry, adv_format_entry, &
       & adv_minor_sb, adv_log2_sb, adv_log5_sb, adv_logx_sb
  
  integer, private :: iaxis
  integer(kind=int32), private, dimension(3) :: log_bands
contains
  subroutine gr_axis_menu(axis)
    integer, intent(in) :: axis

    ! Advanced axis settings.

    type(graff_style), pointer :: axstyle
    type(c_ptr) :: base, jb, junk
    character(len=2), dimension(3), parameter :: axnames = ['X ','Y ','Yr']
    logical, dimension(2), target :: iapply = [.false., .true.]

    axstyle => pdefs%axsty(axis)
    iaxis = axis

    log_bands = axstyle%log_bands
    
    adv_window = hl_gtk_window_new("Advanced "//trim(axnames(axis))//&
         & "-axis settings"//c_null_char, destroy=c_funloc(gr_adv_quit), &
         & data_destroy=c_loc(iapply(1)), parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(adv_window, base)

    jb = hl_gtk_table_new()
    call hl_gtk_box_pack(base, jb)

    if (pdefs%axtype(iaxis) == 0) then    ! Linear options
       junk = gtk_label_new("Major ticks:"//c_null_char)
       call hl_gtk_table_attach(jb, junk, 0_c_int, 0_c_int)

       adv_major_entry = hl_gtk_spin_button_new(0_c_int, &
            & 30_c_int, initial_value= int(axstyle%major, c_int), &
            & tooltip="Enter the number of major ticks"//c_new_line//&
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

    else             ! Log options
       junk =  gtk_label_new("Max decades for full ticks:"//c_null_char)
       call hl_gtk_table_attach(jb, junk, 0_c_int, 0_c_int)

       adv_log2_sb = hl_gtk_spin_button_new(1_c_int, 100_c_int, &
            & initial_value = int(log_bands(1), c_int), &
            & value_changed = c_funloc(gr_adv_log2_ch), tooltip = &
            & "Set the number of decades at which to switch from" // &
            & c_new_line // "major ticks every decade to every 2 decades." // &
            & c_null_char)
       call hl_gtk_table_attach(jb, adv_log2_sb, 1_c_int, 0_c_int)

       junk =  gtk_label_new("Max decades for 100 steps:"//c_null_char)
       call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int)

       adv_log5_sb = hl_gtk_spin_button_new(int(log_bands(1), c_int)+1, &
            & 100_c_int, initial_value = int(log_bands(2), c_int), &
            & value_changed = c_funloc(gr_adv_log5_ch), tooltip = &
            & "Set the number of decades at which to switch from" // &
            & c_new_line // "major ticks every 2 decades to every 5 decades." // &
            & c_null_char)
       call hl_gtk_table_attach(jb, adv_log5_sb, 1_c_int, 1_c_int)
       
       junk =  gtk_label_new("Max decades for 10,000 steps:"//c_null_char)
       call hl_gtk_table_attach(jb, junk, 0_c_int, 2_c_int)

       adv_logx_sb = hl_gtk_spin_button_new(int(log_bands(2), c_int)+1, &
            & 100_c_int, initial_value = int(log_bands(3), c_int), &
            & value_changed = c_funloc(gr_adv_logx_ch), tooltip = &
            & "Set the number of decades at which to switch from" // &
            & c_new_line // "major ticks every 5 decades to free-form." // &
            & c_null_char)
       
       call hl_gtk_table_attach(jb, adv_logx_sb, 1_c_int, 2_c_int)
    end if

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

    call c_f_pointer(data, apply)

    if (apply) then
       axstyle => pdefs%axsty(iaxis)

       if (pdefs%axtype(iaxis) == 0) then
          axstyle%major = int(hl_gtk_spin_button_get_value(adv_major_entry), &
               & int16)
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
       else
          axstyle%log_bands = log_bands
       end if
       call gr_plot_draw(.true.)
    end if

    iaxis = 0
    call gtk_widget_destroy(adv_window)

  end subroutine gr_adv_quit

  subroutine gr_adv_log2_ch(widget, data) bind(c)
    type(c_ptr), value :: widget, data


    log_bands(1) = int(gtk_spin_button_get_value(widget))

    if (log_bands(2) <= log_bands(1)) then
       log_bands(2) = log_bands(1)+1
       call hl_gtk_spin_button_set_int(adv_log5_sb, &
            & int(log_bands(1)+1,c_int))
       if (log_bands(3) <= log_bands(2)) then
          log_bands(3) = log_bands(2)+1
          call hl_gtk_spin_button_set_int(adv_logx_sb, &
               & int(log_bands(2)+1,c_int))
          call hl_gtk_spin_button_set_range_int(adv_logx_sb, &
               & int(log_bands(2)+1,c_int))
       end if
    end if
    call hl_gtk_spin_button_set_range_int(adv_log5_sb, &
         & int(log_bands(1)+1,c_int))
  end subroutine gr_adv_log2_ch

  subroutine gr_adv_log5_ch(widget, data) bind(c)
    type(c_ptr), value :: widget, data


    log_bands(2) = int(gtk_spin_button_get_value(widget))

    if (log_bands(3) <= log_bands(2)) then
       log_bands(3) = log_bands(2)+1
       call hl_gtk_spin_button_set_int(adv_logx_sb, &
            & int(log_bands(2)+1,c_int))
    end if
    call hl_gtk_spin_button_set_range_int(adv_logx_sb, &
         & int(log_bands(2)+1,c_int))

  end subroutine gr_adv_log5_ch
  
  subroutine gr_adv_logx_ch(widget, data) bind(c)
    type(c_ptr), value :: widget, data


    log_bands(3) = int(gtk_spin_button_get_value(widget))

  end subroutine gr_adv_logx_ch
  
end module gr_axis_adv_widgets
