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
  integer(kind=int32), parameter :: idl_objref = 11   ! Object reference
  integer(kind=int32), parameter :: idl_uint = 12   ! Unsigned Integer
  integer(kind=int32), parameter :: idl_ulong = 13   ! Unsigned Longword Integer
  integer(kind=int32), parameter :: idl_long64 = 14   ! 64-bit Integer
  integer(kind=int32), parameter :: idl_ulong64 = 15   ! Unsigned 64-bit Integer

end module gr_idl_types
