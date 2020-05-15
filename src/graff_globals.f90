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

module graff_globals
  ! Global variables needed by many different routines.

  use iso_c_binding

  use graff_types
  use graff_version

  implicit none

  ! The main data structure

  type(graff_pdefs), target :: pdefs

  ! Widgets that may be needed outside the GUI-specifics

  type(c_ptr) :: gr_window = c_null_ptr		! The top level window
  type(c_ptr) :: gr_drawing_area = c_null_ptr	! The main plotting surface
  type(c_ptr) :: gr_infobar = c_null_ptr	! Messages window
  type(c_ptr) :: xhair_but = c_null_ptr         ! Cross hairs toggle


end module graff_globals
