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

program graffer
  ! Main program for Graffer.

  use iso_c_binding

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_init, TRUE

  use graff_types
  use graff_globals
  use graff_version
  use gr_colours
  use gr_file

  use gr_gui
  use gr_opt_init
  use graff_init

  implicit none

  character(len=240) :: input
  character(len=240), dimension(:), allocatable :: flist

  integer(kind=c_int) :: ipick
  logical :: ok, isdir
  character(len=8) :: gr_sversion

  call gtk_init()

  call graffer_version%set(5, 0)
  call graffer_version%string(gr_sversion)

  call gr_read_rc

  ! Get the file to open/create and determine which.

  call gr_parse_command(input, isdir)

  if (input == '') then
     ipick = hl_gtk_file_chooser_show(flist, filter=['*.grf'], &
          & filter_name=['Graffer Files'], all=TRUE, edit_filters=TRUE, &
          & current=TRUE)
     if (c_f_logical(ipick)) then
        input = flist(1)
     else
        stop
     end if
  else if (isdir) then
     ipick = hl_gtk_file_chooser_show(flist, filter=['*.grf'], &
          & filter_name=['Graffer Files'], all=TRUE, edit_filters=TRUE, &
          & initial_dir=trim(input)//c_null_char)
     if (c_f_logical(ipick)) then
        input = flist(1)
     else
        stop
     end if
  end if

  call gr_pdefs_init
  call gr_ct_init()

  if (file_exists(input)) then 
     call gr_read(input, ok)
     if (.not. ok) stop
  else
     call split_fname(input, pdefs%name, pdefs%dir)
  end if

  call gr_make_gui(gr_sversion, input)

end program graffer
