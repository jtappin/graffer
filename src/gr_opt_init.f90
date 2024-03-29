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

module gr_opt_init
  ! Set default options, and read configuration files.

  use iso_fortran_env

  use gtk_sup

  use graff_types
  use gr_utils
  use gr_eval

  use graff_globals
  
  implicit none

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
    real(kind=real32) :: rval
    real(kind=plflt) :: r8val
    
    integer, dimension(2) :: ival2
    logical :: found

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
          read(keyval, *, iostat=ios, iomsg=iom) rval
          if (ios /= 0) then
             write(error_unit, "(2a/t10,a)") &
                  & "gr_read_rc_file: Invalid Autosave setting in file:", &
                  & trim(file), trim(keyval)
          else
             sysopts%auto_delay = rval
          end if
       case('supp2d')
          sysopts%s2d = truth(keyval, default=sysopts%s2d, status=ios)
          if (ios /= 0) then
             write(error_unit, "(2a/t10,a)") &
                  & "gr_read_rc_file: Invalid Supp2D setting in file:", &
                  & trim(file), trim(keyval)
          end if
       case('mouseedit')
          sysopts%mouse = truth(keyval, default=sysopts%mouse, status=ios)
          if (ios /= 0) then
             write(error_unit, "(2a/t10,a)") &
                  & "gr_read_rc_file: Invalid MouseEdit setting in file:", &
                  & trim(file), trim(keyval)
          end if

       case('delete')
          sysopts%delete_function_files = truth(keyval, default = &
               & sysopts%delete_function_files, status=ios)
          if (ios /= 0) then
             write(error_unit, "(2a/t10,a)") &
                  & "gr_read_rc_file: Invalid Delete setting in file:", &
                  & trim(file), trim(keyval)
          end if

       case("colourdir")
          sysopts%colour_dir = trim(adjustl(keyval))
       case("colourname")
          sysopts%colour_stem = trim(adjustl(keyval))

       case('pdfview')
          sysopts%pdfviewer = trim(adjustl(keyval))

       case('gdl', 'idl')
          found = gr_have_gdl(adjustl(keyval))
          if (.not. found) then
             write(error_unit, "(a/t10,a)") &
                  & "gr_parse_command: gdl/idl command not found in path", &
                  & trim(keyval)
          end if

       case('geometry')
          px = scan(keyval, 'xX')
          if (px > 0) keyval(px:px) = ' '
          read(keyval, *, iostat=ios, iomsg=iom) ival2
          if (ios /= 0) then
             write(error_unit, "(2a/t10,a)") &
                  & "gr_read_rc_file: Invalid Geometry setting in file:", &
                  & trim(file), trim(keyval)
          else
             sysopts%geometry = ival2
          end if

       case('scale')
          read(keyval, *, iostat=ios, iomsg=iom) r8val
          if (ios /= 0) then
             write(error_unit, "(2a/t10,a)") &
                  & "gr_read_rc_file: Invalid Scale setting in file:", &
                  & trim(file), trim(keyval)
          else
             sysopts%charscale = r8val
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
    real(kind=real32) :: rval
    real(kind=plflt) :: r8val
    logical :: arg_plus
    integer :: ios
    character(len=120) :: iom
    logical :: helped, found

    nargs = command_argument_count()

    file = ''
    isdir=.false.
    helped = .false.

    if (nargs == 0) return

    call get_command_argument(nargs, file)

    if (trim(file) == '-h' .or. trim(file) == '--help') then
       call gr_cmd_help
       stop                       ! After help don't want to do
    end if
    isdir = gr_is_dir(file)
    if (file == '.' .or. file == '..') isdir = .true.

    i = 1
    do 
       if (i > nargs-1) exit
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
          stop                       ! After help don't want to do
          ! anything else.

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
             read(keyval, *, iostat=ios, iomsg=iom) rval
             if (ios /= 0) then
                write(error_unit, "(a/t10,a)") &
                     & "gr_parse_command: Value for autosave delay is "//&
                     & "not valid", trim(keyval)
             else 
                if (arg_plus) i = i+1
                sysopts%auto_delay = rval
             end if
          end if

       case('-cs', '--charscale')
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
             read(keyval, *, iostat=ios, iomsg=iom) r8val
             if (ios /= 0) then
                write(error_unit, "(a/t10,a)") &
                     & "gr_parse_command: Value for character scaling is "//&
                     & "not valid", trim(keyval)
             else 
                if (arg_plus) i = i+1
                sysopts%charscale = r8val
             end if
          end if

       case('-m','--mouse')
          sysopts%mouse = .true.
       case('-nom', '--nomouse')
          sysopts%mouse = .false.

       case('-s2', '--suppress-2d')
          sysopts%s2d = .true.
       case('-nos2', '--nosuppress-2d')
          sysopts%s2d = .false.

       case('-d', '--delete')
          sysopts%delete_function_files = .true.
       case('-nod', '--nodelete')
          sysopts%delete_function_files = .false.

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
                sysopts%pdfviewer =trim(keyval)
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
             found = gr_have_gdl(keyval)
             if (.not. found) then
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
             sysopts%colour_stem =trim(keyval)
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
             sysopts%colour_dir =trim(keyval)
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
                   sysopts%geometry(1) = ival
                end if
                read(keyval(px+1:), *, iostat=ios, iomsg=iom) ival
                if (ios /= 0) then
                   write(error_unit, "(a/t10,a)") &
                        & "gr_parse_command: Value for y size is "//&
                        & "not valid", trim(keyval)
                else
                   sysopts%geometry(2) = ival
                end if
             end if
          end if

       case default
          write(error_unit, "(2a)") &
               & "gr_parse_command: Unknown option: ", trim(key)
          if (.not. helped) then
             call gr_cmd_help
             helped = .true.
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
         & "                        default ${PREFIX}/share/graffer.", &
         & "--gdl --idl <cmd>     : Specify the gdl or idl command to use,", &
         & "                        default: search $PATH for 'gdl' then 'idl'.", &
         & "", &
         & "<file>                : The graffer file to open or a directory to search", &
         & "                        if not given, then a dialogue is opened in the", &
         & "                        current directory."
  end subroutine gr_cmd_help
end module gr_opt_init
