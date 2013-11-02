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

module gr_os_dependent
  ! Module to provide wrappers for glib functions that are gtk2/gtk3 dependent.
  ! And plplot routines with different API's depending on version.
  ! Essentially this should be the only module needing #ifdef and the
  ! C-preprocessor.

  use iso_fortran_env

#ifdef GTK3
  use gtk_os_dependent, only: g_file_test, g_find_program_in_path, &
       & g_get_home_dir, g_get_current_dir
#else
  use g, only: g_file_test, g_find_program_in_path, &
       & g_get_home_dir, g_get_current_dir
#endif

  use gtk, only: G_FILE_TEST_IS_REGULAR, G_FILE_TEST_IS_DIR

  use gtk_sup

#ifdef HAVE_PLWIDTH
  use plplot, only: plwidth, plflt, plsdev
#else
  use plplot, only: plwid, plflt, plsdev
#endif

contains
  function gr_find_program(name, path)
    logical :: gr_find_program
    character(len=*), intent(in) :: name
    character(len=*), intent(out), optional :: path

    type(c_ptr) :: executable

    executable = g_find_program_in_path(trim(name)//c_null_char)
    gr_find_program = c_associated(executable)

    if (present(path)) then
       if (gr_find_program) then
          call c_f_string(executable, path)
       else
          path = ''
       end if
    end if
  end function gr_find_program

  subroutine gr_home_dir(home)
    character(len=*), intent(out) :: home

    type(c_ptr) :: chome

    chome = g_get_home_dir()
    call c_f_string(chome, home)
  end subroutine gr_home_dir

  subroutine gr_current_dir(dir)
    character(len=*), intent(out) :: dir

    type(c_ptr) :: cdir

    cdir = g_get_current_dir()
    call c_f_string(cdir, dir)
  end subroutine gr_current_dir

  function gr_is_file(file)
    logical :: gr_is_file
    character(len=*), intent(in) :: file

    gr_is_file = c_f_logical(g_file_test(trim(file)//c_null_char, &
         & G_FILE_TEST_IS_REGULAR))

  end function gr_is_file

  function gr_is_dir(file)
    logical :: gr_is_dir
    character(len=*), intent(in) :: file

    gr_is_dir = c_f_logical(g_file_test(trim(file)//c_null_char, &
         & G_FILE_TEST_IS_DIR))

  end function gr_is_dir

  subroutine gr_pl_eps
          ! epscairo was introduced at the same revision as plwidth
          ! so we are lazy and only do the one test.
#ifdef HAVE_PLWIDTH
    call plsdev("epscairo")
#else
    call plsdev("epsqt")
#endif
  end subroutine gr_pl_eps

  subroutine gr_pl_width(width)
    real(kind=real32), intent(in) :: width
#ifdef HAVE_PLWIDTH
    call plwidth(real(width, plflt))
#else
    call plwid(nint(width))
#endif
  end subroutine gr_pl_width

end module gr_os_dependent
    
