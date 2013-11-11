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

module gr_opt_init
  ! Set default options, and read configuration files.

  use iso_fortran_env

  use gtk_sup

  use graff_types
  use gr_utils

  implicit none

  type(graff_opts) :: default_options

contains
  subroutine gr_read_rc

    ! Get global options from resource files.

    character(len=*), dimension(*), parameter :: sysetc = &
         & ['/usr/local/etc/', '/etc/          ']
    character(len=*), parameter :: sysrc='graffer.rc', persrc='.grafferrc'
    character(len=512) :: home

    integer :: i
    logical :: ok
    
    do i = 1, size(sysetc)
       if (gr_is_file(trim(sysetc(i))//trim(sysrc))) then
          ok = gr_read_rc_file(trim(sysetc(i))//trim(sysrc))
          if (ok) exit
       end if
    end do

    call gr_home_dir(home)
    if (gr_is_file(trim(home)//trim(persrc))) &
         & ok = gr_read_rc_file(trim(home)//trim(persrc))

  end subroutine gr_read_rc

  function gr_read_rc_file(file) result(ok)
    logical :: ok
    character(len=*), intent(in) :: file

    ! Read a resource file.

    integer :: unit, ios, psep, pcom, px
    character(len=120) :: iom
    character(len=160) :: inln, keyval
    character(len=32) :: key
    integer :: ival
    integer, dimension(2) :: ival2

    ok = .true.

    open(newunit=unit, file=file, &
         & form='formatted', status='old', action='read', &
         & iostat=ios, iomsg=iom)

    if (ios /= 0) then
       write(error_unit, "(2a/t10,a)") "gr_read_rc_file: Failed to open ",&
            & trim(file), trim(iom)
       ok = .false.
       return
    end if

    do
       read(unit, "(A)", iostat=ios, iomsg=iom) inln
       if (ios == iostat_end) exit
       if (ios /= 0) then
          write(error_unit, "(2a/t10,a)") &
               & "gr_read_rc_file: Failed to read from: ", &
               & trim(file), trim(iom)
          ok = .false.
          exit
       end if

       pcom = index(inln, "#")
       if (pcom /= 0) inln = inln(:pcom-1)
       if (len_trim(inln) == 0) cycle

       psep = index(inln, ':')
       if (psep == 0) then
          write(error_unit, "(2a/t10,a)") &
               & "gr_read_rc_file: Invalid line in file:", trim(file), &
               & trim(inln)
          cycle
       end if
       key = lowcase(adjustl(inln(:psep-1)))
       keyval = trim(inln(psep+1:))

       select case (key)
       case('autosave')
          read(keyval, *, iostat=ios, iomsg=iom) ival
          if (ios /= 0) then
             write(error_unit, "(2a/t10,a)") &
                  & "gr_read_rc_file: Invalid Autosave setting in file:", &
                  & trim(file), trim(keyval)
          else
             default_options%auto_delay = ival
          end if
       case('supp2d')
          read(keyval, *, iostat=ios, iomsg=iom) ival
          if (ios /= 0) then
             write(error_unit, "(2a/t10,a)") &
                  & "gr_read_rc_file: Invalid Supp2D setting in file:", &
                  & trim(file), trim(keyval)
          else
             default_options%s2d = c_f_logical(ival)
          end if
       case('mouseedit')
          read(keyval, *, iostat=ios, iomsg=iom) ival
          if (ios /= 0) then
             write(error_unit, "(2a/t10,a)") &
                  & "gr_read_rc_file: Invalid MouseEdit setting in file:", &
                  & trim(file), trim(keyval)
          else
             default_options%mouse = c_f_logical(ival)
          end if
       case('colourmenu')
          read(keyval, *, iostat=ios, iomsg=iom) ival
          if (ios /= 0) then
             write(error_unit, "(2a/t10,a)") &
                  & "gr_read_rc_file: Invalid ColourMenu setting in file:", &
                  & trim(file), trim(keyval)
          else
             default_options%colour_menu = c_f_logical(ival)
             write(error_unit, "(a)") &
                  & "Warning: colour menu is ignored in the Fortran version"
          end if
       case('delete')
          read(keyval, *, iostat=ios, iomsg=iom) ival
          if (ios /= 0) then
             write(error_unit, "(2a/t10,a)") &
                  & "gr_read_rc_file: Invalid Delete setting in file:", &
                  & trim(file), trim(keyval)
          else
             default_options%delete_function_files = c_f_logical(ival)
          end if

       case("colourdir")
          default_options%colour_dir = trim(adjustl(keyval))
       case("colourname")
          default_options%colour_stem = trim(adjustl(keyval))

       case('pdfview')
          default_options%pdfviewer = trim(adjustl(keyval))

       case('gdl', 'idl')
          default_options%gdl_command = trim(adjustl(keyval))

       case('geometry')
          px = scan(keyval, 'xX')
          if (px > 0) keyval(px:px) = ' '
          read(keyval, *, iostat=ios, iomsg=iom) ival2
                    if (ios /= 0) then
             write(error_unit, "(2a/t10,a)") &
                  & "gr_read_rc_file: Invalid Geometry setting in file:", &
                  & trim(file), trim(keyval)
          else
             default_options%geometry = ival2
          end if
       case default
          write(error_unit, "(2a/t10,a)") &
               & "gr_read_rc_file: Unknown item in file:", &
               & trim(file), trim(inln)
       end select
    end do

    close(unit)

  end function gr_read_rc_file

  subroutine gr_parse_command(file, isdir)
    character(len=*), intent(out) :: file
    logical, intent(out) :: isdir

    ! Get global options from the command line.

    character(len=len(file)) :: argv, keyval
    character(len=32) :: key
    integer :: nargs, i, poseq, status, px
    integer(kind=int32) :: ival
    logical :: arg_plus
    integer :: ios
    character(len=120) :: iom
    logical :: helped

    nargs = command_argument_count()

    file = ''
    isdir=.false.
    helped = .false.

    i = 1
    do 
       if (i > nargs) exit
       call get_command_argument(i, argv)
       poseq = index(argv, '=')
       if (poseq > 0) then
          key = trim(argv(:poseq-1))
          keyval = trim(argv(poseq+1:))
       else
          key = trim(argv(:len(key)))
          keyval = ''
       endif

       select case (key)
       case('-h', '--help')
          call gr_cmd_help
          stop

       case('-a','--autosave')
          if (keyval == '') then
             call  get_command_argument(i+1, keyval, &
                  & status=status)
             arg_plus = .true.
          else
             status = 0
          end if
          if (status /= 0) then
             write(error_unit, "(a/t10,a)") &
                  & "gr_parse_command: Failed to get a value for key: ",&
                  &  trim(key)
          else
             read(keyval, *, iostat=ios, iomsg=iom) ival
             if (ios /= 0) then
                write(error_unit, "(a/t10,a)") &
                     & "gr_parse_command: Value for autosave delay is "//&
                     & "not valid", trim(keyval)
             else 
                if (arg_plus) i = i+1
                default_options%auto_delay = ival
             end if
          end if

       case('-m','--mouse')
          default_options%mouse = .true.
       case('-nom', '--nomouse')
          default_options%mouse = .false.

       case('-s2', '--suppress-2d')
          default_options%s2d = .true.
       case('-nos2', '--nosuppress-2d')
          default_options%s2d = .false.

       case('-d', '--delete')
          default_options%delete_function_files = .true.
       case('-nod', '--nodelete')
          default_options%delete_function_files = .false.

       case('-p', '--pdf')
          if (keyval == '') then
             call  get_command_argument(i+1, keyval, &
                  & status=status)
             arg_plus = .true.
          else
             status = 0
          end if
          if (status /= 0) then
             write(error_unit, "(a/t10,a)") &
                  & "gr_parse_command: Failed to get a value for key: ",&
                  &  trim(key)
          else
             if (arg_plus) i = i+1
             if (gr_find_program(keyval)) then
                default_options%pdfviewer =trim(keyval)
             else 
                write(error_unit, "(a/t10,a)") &
                     & "gr_parse_command: Pdf viewer not found in path", &
                     & trim(keyval)
             end if
          end if

       case('--gdl', '--idl')
          if (keyval == '') then
             call  get_command_argument(i+1, keyval, &
                  & status=status)
             arg_plus = .true.
          else
             status = 0
          end if
          if (status /= 0) then
             write(error_unit, "(a/t10,a)") &
                  & "gr_parse_command: Failed to get a value for key: ",&
                  &  trim(key)
          else
             if (arg_plus) i = i+1
             if (gr_find_program(keyval)) then
                default_options%gdl_command = trim(keyval)
             else
                write(error_unit, "(a/t10,a)") &
                     & "gr_parse_command: gdl/idl command not found in path", &
                     & trim(keyval)
             end if
          end if

       case('-ct', '--colour-table')
          if (keyval == '') then
             call  get_command_argument(i+1, keyval, &
                  & status=status)
             arg_plus = .true.
          else
             status = 0
          end if
          if (status /= 0) then
             write(error_unit, "(a/t10,a)") &
                  & "gr_parse_command: Failed to get a value for key: ",&
                  &  trim(key)
          else
             if (arg_plus) i = i+1
             default_options%colour_stem =trim(keyval)
          end if

       case('-cd', '--colour-dir')
          if (keyval == '') then
             call  get_command_argument(i+1, keyval, &
                  & status=status)
             arg_plus = .true.
          else
             status = 0
          end if
          if (status /= 0) then
             write(error_unit, "(a/t10,a)") &
                  & "gr_parse_command: Failed to get a value for key: ",&
                  &  trim(key)
          else
             if (arg_plus) i = i+1
             default_options%colour_dir =trim(keyval)
          end if

       case('-g', '--geometry')
          if (keyval == '') then
             call  get_command_argument(i+1, keyval, &
                  & status=status)
             arg_plus = .true.
          else
             status = 0
          end if
          if (status /= 0) then
             write(error_unit, "(a/t10,a)") &
                  & "gr_parse_command: Failed to get a value for key: ",&
                  &  trim(key)
          else
             if (arg_plus) i = i+1
             px = scan(keyval, 'xX')
             if (px == 0) then
                write(error_unit, "(a)") &
                     & "gr_parse_command: geometry setting has the form"//&
                     & " <x>x<y>",&
                     & "        e.g. 600x600"
             else
                read(keyval(:px-1), *, iostat=ios, iomsg=iom) ival
                if (ios /= 0) then
                   write(error_unit, "(a/t10,a)") &
                        & "gr_parse_command: Value for x size is "//&
                        & "not valid", trim(keyval)
                else
                   default_options%geometry(1) = ival
                end if
                read(keyval(px+1:), *, iostat=ios, iomsg=iom) ival
                if (ios /= 0) then
                   write(error_unit, "(a/t10,a)") &
                        & "gr_parse_command: Value for y size is "//&
                        & "not valid", trim(keyval)
                else
                   default_options%geometry(2) = ival
                end if
             end if
          end if

       case default
          if (i /= nargs) then
             write(error_unit, "(2a)") &
                  & "gr_parse_command: Unknown option: ", trim(key)
             if (.not. helped) then
                call gr_cmd_help
                helped = .true.
             end if
          else
             file = trim(argv)
             isdir = gr_is_dir(file)
             if (file == '.' .or. file == '..') isdir = .true.
          end if
       end select

       i = i+1
    end do 
  end subroutine gr_parse_command

  subroutine gr_cmd_help
    print "(A)", &
         & "Usage:",&
         & "         graffer [<opts>] [<file>]",&
         & "",&
         & "-h, --help            : Print this help text and exit",&
         & "-m, --mouse           : Enable editing data with the mouse by default", &
         & "-nom --nomouse        : Disable editing data with the mouse by default", &
         & "                        Default behaviour", &
         & "-s2 --suppress-2d     : Suppress display of 2-D datasets", &
         & "-nos2 --nosuppress-2d : Enable display of 2-D datasets", &
         & "                        Default behaviour", &
         & "-d --delete           : Delete files generated to evaluate functions", &
         & "-nod --nodelete       : Keep files generated to evaluate functions", &
         & "                        Default behaviour", &
         & "-p --pdf <cmd>        : Specify a PDF viewer for the help files",&
         & "-g --geometry <x>x<y> : Specify the drawing window geometry", &
         & "-a --autosave <time>  : Specify the delay between autosaves (s)", &
         & "-ct --colour-table <name>: Specify the filename stem for the colour", &
         & "                        table files (default 'c_tables')", &
         & "-cd --colour-dir <dir>: Specify where the colour tables are installed", &
         & "--gdl --idl <cmd>     : Specify the gdl or idl command to use", &
         & "                        default ${PREFIX}/share/graffer.", &
         & "", &
         & "<file>                : The graffer file to open or a directory to search"
  end subroutine gr_cmd_help
end module gr_opt_init
