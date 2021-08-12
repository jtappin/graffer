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

module gr_idl_types
  use iso_fortran_env

  implicit none

  ! Translation of IDL type codes to equivalent Fortran types.

  integer(kind=int32), parameter :: idl_nocode = -1  ! Not set

  integer(kind=int32), parameter :: idl_undefined = 0   ! Undefined
  integer(kind=int32), parameter :: idl_byte = 1   ! Byte
  integer(kind=int32), parameter :: idl_int = 2   ! Integer
  integer(kind=int32), parameter :: idl_long = 3   ! Longword integer
  integer(kind=int32), parameter :: idl_float = 4   ! Floating point
  integer(kind=int32), parameter :: idl_double = 5   ! Double-precision floating
  integer(kind=int32), parameter :: idl_complex = 6   ! Complex floating
  integer(kind=int32), parameter :: idl_string = 7   ! String
  integer(kind=int32), parameter :: idl_struct = 8   ! Structure
  integer(kind=int32), parameter :: idl_dcomplex = 9  ! Double-precision complex
  integer(kind=int32), parameter :: idl_pointer = 10   ! Pointer
  integer(kind=int32), parameter :: idl_objref = 11   ! Object reference (list)
  integer(kind=int32), parameter :: idl_uint = 12   ! Unsigned Integer
  integer(kind=int32), parameter :: idl_ulong = 13   ! Unsigned Longword Integer
  integer(kind=int32), parameter :: idl_long64 = 14   ! 64-bit Integer
  integer(kind=int32), parameter :: idl_ulong64 = 15   ! Unsigned 64-bit Integer

end module gr_idl_types
