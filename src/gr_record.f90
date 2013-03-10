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

module gr_record
  use iso_fortran_env, only: int8, int16, int32, real32, real64, &
       & iostat_end

  use gtk, only: GTK_MESSAGE_ERROR

  use gr_idl_types
  use gr_utils
  use gr_msg

  implicit none

  type :: graffer_record
     character(len=3) :: tag
     integer(kind=int32) :: tcode = idl_nocode
     integer(kind=int32) :: ndims = -1
     integer(kind=int32), dimension(:), allocatable :: dims, length

     integer(kind=int8) :: b_val
     integer(kind=int16) :: i_val
     integer(kind=int32) :: l_val
     real(kind=real32) :: r_val
     real(kind=real64) :: d_val
     
     integer(kind=int8), dimension(:), allocatable :: ba_val
     integer(kind=int16), dimension(:), allocatable :: ia_val
     integer(kind=int32), dimension(:), allocatable :: la_val
     real(kind=real32), dimension(:), allocatable :: ra_val
     real(kind=real64), dimension(:), allocatable :: da_val
     
     integer(kind=int8), dimension(:,:), allocatable :: baa_val
     real(kind=real64), dimension(:,:), allocatable :: daa_val
   contains
     private
     procedure, public :: read => gr_read_rec
     procedure, public :: put => gr_put_rec

     procedure :: gr_get_int
     procedure :: gr_get_long
     procedure :: gr_get_float
     procedure :: gr_get_double
     procedure :: gr_get_logical
     procedure :: gr_get_byte
     procedure :: gr_get_string

     procedure :: gr_get_int_a
     procedure :: gr_get_long_a
     procedure :: gr_get_float_a
     procedure :: gr_get_double_a
     procedure :: gr_get_string_a

     procedure :: gr_get_double_aa

     procedure :: gr_set_null
     procedure :: gr_set_int
     procedure :: gr_set_long
     procedure :: gr_set_float
     procedure :: gr_set_double
     procedure :: gr_set_logical
     procedure :: gr_set_byte
     procedure :: gr_set_string

     procedure :: gr_set_int_a
     procedure :: gr_set_long_a
     procedure :: gr_set_float_a
     procedure :: gr_set_double_a
     procedure :: gr_set_string_a

     procedure :: gr_set_double_aa

     generic, public :: get_value => gr_get_int, gr_get_long, &
          & gr_get_float, gr_get_double, gr_get_logical, gr_get_string, &
          & gr_get_int_a, gr_get_long_a, gr_get_float_a, gr_get_double_a, &
          & gr_get_string_a, gr_get_double_aa, gr_get_byte

     generic, public :: set_value => gr_set_int, gr_set_long, &
          & gr_set_float, gr_set_double, gr_set_logical, gr_set_string, &
          & gr_set_int_a, gr_set_long_a, gr_set_float_a, gr_set_double_a, &
          & gr_set_string_a, gr_set_double_aa, gr_set_byte, gr_set_null

     procedure, public :: get_dimensions => gr_get_dimensions
     procedure, public :: get_tag => gr_get_tag
  end type graffer_record

  character(len=160), dimension(2), private :: error_str

contains

  function gr_read_rec(this, unit, swap) result(status)
    integer :: status
    class(graffer_record), intent(out) :: this
    integer, intent(in) :: unit
    logical, intent(in), optional :: swap

    ! Read a data record from a binary Graffer file.

    logical :: isopen, swap_end
    integer :: ios
    character(len=120) :: iomsg
    integer :: i

    status = 0
    inquire(unit=unit, opened=isopen)
    if (.not. isopen) then
       write(error_str, "(A,i0,a)") &
            & "GR_READ_REC: Unit: ", unit, " is not open"
       call gr_message(error_str(1), type=GTK_MESSAGE_ERROR)
       status = 1
    end if

    if (present(swap)) then
       swap_end = swap
    else
       swap_end=.false.
    end if

    read(unit, iostat=ios, iomsg=iomsg) this%tag, this%tcode, this%ndims
    if (ios == iostat_end) then   ! end of file (only allowable here)
       status = -1
       return
    end if
    if (ios /= 0) then
       write(error_str, "(A)") "GR_READ_REC: Failed to read tag data", &
            & trim(iomsg)
       call gr_message(error_str)
       status = 1
       return
    end if
    if (swap_end) then
       call byte_swap(this%tcode)
       call byte_swap(this%ndims)
    end if
    this%tag = upcase(this%tag)

    if (this%tcode == idl_undefined) return  ! A null tag
    ! no further data present. 

    if (this%ndims > 0) then
       allocate(this%dims(this%ndims))
       read(unit, iostat=ios, iomsg=iomsg) this%dims
       if (ios /= 0) then
          write(error_str, "(2A/a)") &
               & "GR_READ_REC: Failed to read dimensions for: ", this%tag, &
               & trim(iomsg)
          call gr_message(error_str)
          status = 1
          return
       end if
       if (swap_end) call byte_swap(this%dims)
    end if

    if (this%tcode == idl_string) then
       if (this%ndims == 1) then
          allocate(this%length(this%dims(1)))
       else if (this%ndims == 0) then
          allocate(this%length(1))
       else
          call gr_message( &
               & "GR_READ_REC: only 1-D string arrays are handled")
          status = 1
          return
       end if

       read(unit, iostat=ios, iomsg=iomsg) this%length
       if (ios /= 0) then
          write(error_str, "(2A/a)") &
               & "GR_READ_REC: Failed to read lengths for: ", this%tag, &
               & trim(iomsg)
          call gr_message(error_str)
          status = 1
          return
       end if
       if (swap_end) call byte_swap(this%length)
    end if

    select case (this%ndims)
    case(0)      ! Scalars
       select case (this%tcode)
       case(idl_byte)
          read(unit, iostat=ios, iomsg=iomsg) this%b_val
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_READ_REC: Failed to read byte scalar", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
       case(idl_int)
          read(unit, iostat=ios, iomsg=iomsg) this%i_val
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_READ_REC: Failed to read integer scalar", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
          if (swap_end) call byte_swap(this%i_val)
       case(idl_long)
          read(unit, iostat=ios, iomsg=iomsg) this%l_val
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_READ_REC: Failed to read long scalar", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
          if (swap_end) call byte_swap(this%l_val)
       case(idl_float)
          read(unit, iostat=ios, iomsg=iomsg) this%r_val
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_READ_REC: Failed to read float scalar", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
          if (swap_end) call byte_swap(this%r_val)
       case(idl_double)
          read(unit, iostat=ios, iomsg=iomsg) this%d_val
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_READ_REC: Failed to read double scalar", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
          if (swap_end) call byte_swap(this%d_val)
       case(idl_string)
          allocate(this%ba_val(this%length(1)))
          read(unit, iostat=ios, iomsg=iomsg) this%ba_val
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_READ_REC: Failed to read string scalar", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
       case default
          write(error_str, "(A, i0)") &
               & "GR_READ_REC: Unknown/inappropriate type code, ", this%tcode
          call gr_message(error_str(1))
          status = 2
          return
       end select

    case(1)    ! 1-D arrays

       select case (this%tcode)
       case (idl_byte)
          allocate(this%ba_val(this%dims(1)))
          read(unit, iostat=ios, iomsg=iomsg) this%ba_val
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_READ_REC: Failed to read byte array", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
       case (idl_int)
          allocate(this%ia_val(this%dims(1)))
          read(unit, iostat=ios, iomsg=iomsg) this%ia_val
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_READ_REC: Failed to read int array", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
          if (swap_end) call byte_swap(this%ia_val)
       case (idl_long)
          allocate(this%la_val(this%dims(1)))
          read(unit, iostat=ios, iomsg=iomsg) this%la_val
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_READ_REC: Failed to read long array", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
          if (swap_end) call byte_swap(this%la_val)
       case (idl_float)
          allocate(this%ra_val(this%dims(1)))
          read(unit, iostat=ios, iomsg=iomsg) this%ra_val
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_READ_REC: Failed to read float array", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
          if (swap_end) call byte_swap(this%ra_val)
       case (idl_double)
          allocate(this%da_val(this%dims(1)))
          read(unit, iostat=ios, iomsg=iomsg) this%da_val
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_READ_REC: Failed to read double array", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
          if (swap_end) call byte_swap(this%da_val)
       case (idl_string)
          allocate(this%baa_val(maxval(this%length), this%dims(1)))

          do i = 1, this%dims(1)
             read(unit, iostat=ios, iomsg=iomsg) &
                  & this%baa_val(:this%length(i), i)
             if (ios /= 0) then
                write(error_str, "(A)") &
                     & "GR_READ_REC: Failed to read string array", &
                     & trim(iomsg)
                call gr_message(error_str)
                status = 1
                return
             end if
          end do
       case default
          write(error_str, "(A, i0)") &
               & "GR_READ_REC: Unknown/inappropriate type code, ", this%tcode
          call gr_message(error_str(1))
          status = 2
          return
       end select

    case (2)   ! 2-D arrays (only double supported)

       select case (this%tcode)
       case(idl_double)
          allocate(this%daa_val(this%dims(1), this%dims(2)))
          read(unit, iostat=ios, iomsg=iomsg) this%daa_val
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_READ_REC: Failed to read double array", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
          if (swap_end) call byte_swap(this%daa_val)
       case default
          write(error_str, "(A, i0)") &
               & "GR_READ_REC: Unknown/inappropriate type code, ", this%tcode
          call gr_message(error_str(1))
          status = 2
          return
       end select
    case default
       write(error_str, "(A, i0)") &
            & "GR_READ_REC: Inappropriate dimensionality, ", this%ndims
          call gr_message(error_str(1))
       status = 2
       return

    end select

    status = 0

  end function gr_read_rec

  subroutine gr_get_int(this, ival, status)
    class(graffer_record), intent(in) :: this
    integer(kind=int16), intent(out) :: ival
    integer, intent(out) :: status

    ! Get an int(2) value from a record.

    status = 0

    if (this%ndims /= 0) then
       call gr_message("GR_GET_INT: Try to read scalar from array")
       status = 2
       return
    end if

    select case (this%tcode)
    case(idl_int)
       ival = this%i_val
    case(idl_long)
       ival = int(this%l_val, int16)
       status = 4
    case(idl_float)
       ival = int(this%r_val, int16)
       status = 4
    case(idl_double) 
       ival = int(this%d_val, int16)
       status = 4
    case(idl_byte)
       ival = int(this%b_val, int16)
    case default
       write(error_str, "(A, I0)") "GR_GET_INT: Unknown type code: ", &
            & this%tcode
       call gr_message(error_str(1))
       status = 2
    end select
  end subroutine gr_get_int

  subroutine gr_get_long(this, lval, status)
    class(graffer_record), intent(in) :: this
    integer(kind=int32), intent(out) :: lval
    integer, intent(out) :: status

    ! get a long(4) value from a record.

    status = 0

    if (this%ndims /= 0) then
       call gr_message("GR_GET_LONG: Try to read scalar from array")
       status = 2
       return
    end if

    select case (this%tcode)
    case(idl_long)
       lval = this%l_val
    case(idl_int)
       lval = int(this%i_val, int32)
    case(idl_float)
       lval = int(this%r_val, int32)
       status = 4
    case(idl_double) 
       lval = int(this%d_val, int32)
       status = 4
    case(idl_byte)
       lval = int(this%b_val, int32)
    case default
       write(error_str, "(A, I0)") "GR_GET_LONG: Unknown type code: ", &
            & this%tcode
          call gr_message(error_str(1))
       status = 2
    end select
  end subroutine gr_get_long

  subroutine gr_get_float(this, rval, status)
    class(graffer_record), intent(in) :: this
    real(kind=real32), intent(out) :: rval
    integer, intent(out) :: status

    ! Get a float(4) value from a record

    status = 0

    if (this%ndims /= 0) then
       call gr_message("GR_GET_REAL: Try to read scalar from array")
       status = 2
       return
    end if

    select case (this%tcode)
    case(idl_float)
       rval = this%r_val
    case(idl_double) 
       rval = real(this%d_val, real32)
       status = 4
    case(idl_long)
       rval = real(this%l_val, real32)
       status = 4
    case(idl_int)
       rval = real(this%i_val, real32)
    case(idl_byte)
       rval = real(this%b_val, real32)
    case default
       write(error_str, "(A, I0)") "GR_GET_FLOAT: Unknown type code: ", &
            & this%tcode
       call gr_message(error_str(1))
       status = 2
    end select
  end subroutine gr_get_float

  subroutine gr_get_double(this, dval, status)
    class(graffer_record), intent(in) :: this
    real(kind=real64), intent(out) :: dval
    integer, intent(out) :: status

    ! get a double(8) value from a record

    status = 0

    if (this%ndims /= 0) then
       call gr_message("GR_GET_DOUBLE: Try to read scalar from array")
       status = 2
       return
    end if

    select case (this%tcode)
    case(idl_double) 
       dval = this%d_val
    case(idl_float)
       dval = real(this%r_val, real64)
    case(idl_long)
       dval = real(this%l_val, real64)
    case(idl_int)
       dval = real(this%i_val, real64)
    case(idl_byte)
       dval = real(this%b_val, real64)
    case default
       write(error_str, "(A, I0)") "GR_GET_DOUBLE: Unknown type code: ", &
            & this%tcode
       call gr_message(error_str(1))
       status = 2
    end select
  end subroutine gr_get_double

  subroutine gr_get_string(this, sval, status)
    class(graffer_record), intent(in) :: this
    character(len=*), intent(out) :: sval
    integer, intent(out) :: status

    ! Get a string value from a record.

    integer :: i

    status = 0

    if (this%ndims /= 0) then
       call gr_message("GR_GET_STRING: Try to read scalar from array")
       status = 2
       return
    end if

    select case (this%tcode)
    case (idl_string)
       sval = ''
       if (len(sval) < this%length(1)) status = 4

       do i = 1, min(len(sval), this%length(1))
          sval(i:i) = char(this%ba_val(i))
       end do
       if (len(sval) > this%length(1)) sval(this%length(1)+1:) = ' '
    case default
       write(error_str, "(A, I0)") "GR_GET_STRING: Unknown type code: ", &
            & this%tcode
       call gr_message(error_str(1))
       status = 2
    end select
  end subroutine gr_get_string

  subroutine gr_get_logical(this, tval, status)
    class(graffer_record), intent(in) :: this
    logical(kind=int8), intent(out) :: tval
    integer, intent(out) :: status

    ! get a logical(1) value from a record.

    status = 0

    if (this%ndims /= 0) then
       call gr_message("GR_GET_LOGICAL: Try to read scalar from array")
       status = 2
       return
    end if

    select case (this%tcode)
    case(idl_byte)
       tval = this%b_val /= 0
    case(idl_int)
       tval = this%i_val /= 0
       status = 4
    case(idl_long)
       tval = this%l_val /= 0
       status = 4
    case(idl_float)
       tval = this%r_val /= 0
       status = 4
    case(idl_double) 
       tval = this%d_val /= 0
       status = 4
    case default
       write(error_str, "(A, I0)") "GR_GET_LOGICAL: Unknown type code: ", &
            & this%tcode
       call gr_message(error_str(1))
       status = 2
    end select
  end subroutine gr_get_logical

  subroutine gr_get_byte(this, bval, status)
    class(graffer_record), intent(in) :: this
    integer(kind=int8), intent(out) :: bval
    integer, intent(out) :: status

    ! get a byte value from a record.

    status = 0

    if (this%ndims /= 0) then
       call gr_message("GR_GET_BYTE: Try to read scalar from array")
       status = 2
       return
    end if

    select case (this%tcode)
    case(idl_byte)
       bval = this%b_val
    case(idl_int)
       bval = int(this%i_val, int8)
       status=4
    case(idl_long)
       bval = int(this%l_val, int8)
       status = 4
    case(idl_float)
       bval = int(this%r_val, int8)
       status = 4
    case(idl_double) 
       bval = int(this%d_val, int8)
       status = 4
    case default
       write(error_str, "(A, I0)") "GR_GET_BYTE: Unknown type code: ", &
            & this%tcode
       call gr_message(error_str(1))
       status = 2
    end select
  end subroutine gr_get_byte


  subroutine gr_get_int_a(this, ival, status)
    class(graffer_record), intent(in) :: this
    integer(kind=int16), dimension(:), intent(out) :: ival
    integer, intent(out) :: status

    ! get an int(2) array from a record.

    integer(kind=int16) :: sival
    integer :: mxi, sz

    if (this%ndims == 0) then
       call gr_get_int(this, sival, status)
       ival = sival
       return
    else if (this%ndims == 2) then
       call gr_message("GR_GET_INT_A: Try to read 1D array from 2-D")
       status = 2
       return
    end if

    sz = size(ival)
    mxi = min(this%dims(1), sz)
    if (sz /= this%dims(1)) then
       write(error_str, "(A,i0,a,i0,a)") &
            & "GR_GET_INT_A: output size (",sz,&
            & ") not equal to data size (",this%dims(1),")"
       call gr_message(error_str(1))
       status = 4
    end if

    select case (this%tcode)
    case(idl_int)
       ival(:mxi) = this%ia_val(:mxi)
    case(idl_long)
       ival(:mxi) = int(this%la_val(:mxi), int16)
       status = 4
    case(idl_float)
       ival(:mxi) = int(this%ra_val(:mxi), int16)
       status = 4
    case(idl_double) 
       ival(:mxi) = int(this%da_val(:mxi), int16)
       status = 4
    case(idl_byte)
       ival(:mxi) = int(this%ba_val(:mxi), int16)
    case default
       write(error_str, "(A, I0)") "GR_GET_INT_A: Unknown type code: ", &
            & this%tcode
       call gr_message(error_str(1))
       status = 2
    end select
  end subroutine gr_get_int_a

  subroutine gr_get_long_a(this, lval, status)
    class(graffer_record), intent(in) :: this
    integer(kind=int32), dimension(:), intent(out) :: lval
    integer, intent(out) :: status

    ! Get a long(4) array from a record.

    integer(kind=int32) :: slval
    integer :: mxi, sz

    if (this%ndims == 0) then
       call gr_get_long(this, slval, status)
       lval = slval
       return
    else if (this%ndims == 2) then
       call gr_message("GR_GET_LONG_A: Try to read 1D array from 2-D")
       status = 2
       return
    end if

    sz = size(lval)
    mxi = min(this%dims(1), sz)
    if (sz /= this%dims(1)) then
       write(error_str, "(A,i0,a,i0,a)") &
            & "GR_GET_LONG_A: output size (",sz,&
            & ") not equal to data size (",this%dims(1),")"
       call gr_message(error_str(1))
       status = 4
    end if

    select case (this%tcode)
    case(idl_long)
       lval(:mxi) = this%la_val(:mxi)
    case(idl_int)
       lval(:mxi) = int(this%ia_val(:mxi), int32)
    case(idl_float)
       lval(:mxi) = int(this%ra_val(:mxi), int32)
       status = 4
    case(idl_double) 
       lval(:mxi) = int(this%da_val(:mxi), int32)
       status = 4
    case(idl_byte)
       lval(:mxi) = int(this%ba_val(:mxi), int32)
    case default
       write(error_str, "(A, I0)") "GR_GET_LONG_A: Unknown type code: ", &
            & this%tcode
       call gr_message(error_str(1))
       status = 2
    end select
  end subroutine gr_get_long_a

  subroutine gr_get_float_a(this, rval, status)
    class(graffer_record), intent(in) :: this
    real(kind=real32), dimension(:), intent(out) :: rval
    integer, intent(out) :: status

    ! get a float(4) array from a record

    real(kind=real32) :: srval
    integer :: mxi, sz

    if (this%ndims == 0) then
       call gr_get_float(this, srval, status)
       rval = srval
       return
    else if (this%ndims == 2) then
       call gr_message("GR_GET_FLOAT_A: Try to read 1D array from 2-D")
       status = 2
       return
    end if

    sz = size(rval)
    mxi = min(this%dims(1), sz)
    if (sz /= this%dims(1)) then
       write(error_str, "(A,i0,a,i0,a)") &
            & "GR_GET_FLOAT_A: output size (",sz,&
            & ") not equal to data size (",this%dims(1),")"
       call gr_message(error_str(1))
       status = 4
    end if

    select case (this%tcode)
    case(idl_float)
       rval(:mxi) = this%ra_val(:mxi)
    case(idl_double) 
       rval(:mxi) = real(this%da_val(:mxi), real32)
       status = 4
    case(idl_int)
       rval(:mxi) = real(this%ia_val(:mxi), real32)
    case(idl_long)
       rval(:mxi) = real(this%la_val(:mxi), real32)
       status = 4
    case(idl_byte)
       rval(:mxi) = real(this%ba_val(:mxi), real32)
    case default
       write(error_str, "(A, I0)") "GR_GET_FLOAT_A: Unknown type code: ", &
            & this%tcode
       call gr_message(error_str(1))
       status = 2
    end select
  end subroutine gr_get_float_a

  subroutine gr_get_double_a(this, dval, status)
    class(graffer_record), intent(in) :: this
    real(kind=real64), dimension(:), intent(out) :: dval
    integer, intent(out) :: status

    ! Get a double(8) array from a record

    real(kind=real64) :: sdval
    integer :: mxi, sz

    if (this%ndims == 0) then
       call gr_get_double(this, sdval, status)
       dval = sdval
       return
    else if (this%ndims == 2) then
       call gr_message("GR_GET_DOUBLE_A: Try to read 1D array from 2-D")
       status = 2
       return
    end if

    sz = size(dval)
    mxi = min(this%dims(1), sz)
    if (sz /= this%dims(1)) then
       write(error_str, "(A,i0,a,i0,a)") &
            & "GR_GET_DOUBLE_A: output size (",sz,&
            & ") not equal to data size (",this%dims(1),")"
       call gr_message(error_str(1))
       status = 4
    end if

    select case (this%tcode)
    case(idl_double) 
       dval(:mxi) = this%da_val(:mxi)
    case(idl_float)
       dval(:mxi) = real(this%la_val(:mxi), real64)
    case(idl_int)
       dval(:mxi) = real(this%ia_val(:mxi), real64)
    case(idl_long)
       dval(:mxi) = real(this%la_val(:mxi), real64)
       status = 4
    case(idl_byte)
       dval(:mxi) = real(this%ba_val(:mxi), real64)
    case default
       write(error_str, "(A, I0)") "GR_GET_DOUBLE_A: Unknown type code: ", &
            & this%tcode
       call gr_message(error_str(1))
       status = 2
    end select
  end subroutine gr_get_double_a

  subroutine gr_get_string_a(this, sval, status)
    class(graffer_record), intent(in) :: this
    character(len=*), intent(out), dimension(:) :: sval
    integer, intent(out) :: status

    ! Get a string array from a record.

    integer :: i, j
    integer :: mxi, sz
    character(len=len(sval)) :: ssval

    status = 0

    sz = size(sval)
    mxi = min(this%dims(1), sz)

    if (this%ndims == 0) then
       call gr_get_string(this, ssval, status)
       sval = ssval
       return
    else if (this%ndims == 2) then
       call gr_message("GR_GET_STRING_A: Try to read 1D array from 2-D")
       status = 2
       return
    end if

    select case (this%tcode)
    case (idl_string)
       do j = 1, mxi
          sval(j) = ''
          if (len(sval) < this%length(j)) status = 4

          do i = 1, min(len(sval), this%length(j))
             sval(j)(i:i) = char(this%baa_val(i,j))
          end do
       if (len(sval) > this%length(j)) sval(j)(this%length(j)+1:) = ' '
       end do
    case default
       write(error_str, "(A, I0)") "GR_GET_STRING_A: Unknown type code: ", &
            & this%tcode
       call gr_message(error_str(1))
       status = 2
    end select
  end subroutine gr_get_string_a

  subroutine gr_get_double_aa(this, dval, status)
    class(graffer_record), intent(in) :: this
    real(kind=real64), dimension(:,:), intent(out) :: dval
    integer, intent(out) :: status

    ! Get a 2D double(8) array from a record.

    real(kind=real64) :: sdval
    integer :: mxi, mxj
    integer, dimension(2) :: sz

    sz = shape(dval)

    if (this%ndims == 0) then
       call gr_get_double(this, sdval, status)
       dval = sdval
       return
    else if (this%ndims == 1) then
       if (sz(2) == 1) then
          call gr_get_double_a(this, dval(:,1), status)
       else if (sz(1) == 1) then
          call gr_get_double_a(this, dval(1,:), status)
       else
          call gr_message("GR_GET_DOUBLE_AA: Try to read 2D array from 1-D")
          status = 2
          return
       end if
    else
       mxi = min(this%dims(1), sz(1))
       mxj = min(this%dims(2), sz(2))

       if (any(sz /= this%dims)) then
          write(error_str, "(A,i0,1x,i0,a,i0,1x,i0,a)") &
               & "GR_GET_DOUBLE_AA: output size (",sz,&
               & ") not equal to data size (",this%dims,")"
          call gr_message(error_str(1))
          status = 4
       end if

       select case (this%tcode)
       case(idl_double) 
          dval(:mxi,:mxj) = this%daa_val(:mxi,:mxj)
       case default
          write(error_str, "(A, I0)") "GR_GET_DOUBLE_AA: Unknown type code: ", &
               & this%tcode
          call gr_message(error_str(1))
          status = 2
       end select
    end if
  end subroutine gr_get_double_aa

  function gr_get_dimensions(this, dims) result(ndims)
    integer(kind=int32) :: ndims
    class(graffer_record), intent(in) :: this
    integer(kind=int32), dimension(:), allocatable, intent(out) :: dims

    ! Get the number of dimensions of a record.

    ndims = this%ndims
    if (ndims /= 0) then
       allocate(dims(ndims))
       dims = this%dims
    end if
  end function gr_get_dimensions

  function gr_get_tag(this) result(tag)
    character(len=3) :: tag
    class(graffer_record), intent(in) :: this

    ! Get the record's tag.

    tag = this%tag
  end function gr_get_tag

  subroutine gr_set_null(this, tag)
    class(graffer_record), intent(out) :: this
    character(len=3), intent(in) :: tag

    ! Set a record to type null

    ! N.B. No bundled write, otherwise  it cannot be distinguished from
    ! a long without a write.

    this%tcode = idl_undefined
    this%tag = tag
    this%ndims = 0
  end subroutine gr_set_null

  subroutine gr_set_int(this, tag, ival, unit)
    class(graffer_record), intent(out) :: this
    character(len=3), intent(in) :: tag
    integer(kind=int16), intent(in) :: ival
    integer, intent(in), optional :: unit

    ! Set an int(2) value for a record/

    integer :: status

    this%tcode = idl_int
    this%tag = tag
    this%ndims = 0

    this%i_val = ival
    if (present(unit)) call this%put(unit, status)
  end subroutine gr_set_int
  subroutine gr_set_long(this, tag, lval, unit)
    class(graffer_record), intent(out) :: this
    character(len=3), intent(in) :: tag
    integer(kind=int32), intent(in) :: lval
    integer, intent(in), optional :: unit

    ! Set a long(4) value for a record.

    integer :: status

    this%tcode = idl_long
    this%tag = tag
    this%ndims = 0

    this%l_val = lval
    if (present(unit)) call this%put(unit, status)
  end subroutine gr_set_long
  subroutine gr_set_float(this, tag, rval, unit)
    class(graffer_record), intent(out) :: this
    character(len=3), intent(in) :: tag
    real(kind=real32), intent(in) :: rval
    integer, intent(in), optional :: unit

    ! Set a float(4) value for a record

    integer :: status

    this%tcode = idl_float
    this%tag = tag
    this%ndims = 0

    this%r_val = rval
    if (present(unit)) call this%put(unit, status)
  end subroutine gr_set_float
  subroutine gr_set_double(this, tag, dval, unit)
    class(graffer_record), intent(out) :: this
    character(len=3), intent(in) :: tag
    real(kind=real64), intent(in) :: dval
    integer, intent(in), optional :: unit

    ! Set a double(8) value for a record.

    integer :: status

    this%tcode = idl_double
    this%tag = tag
    this%ndims = 0

    this%d_val = dval
    if (present(unit)) call this%put(unit, status)
  end subroutine gr_set_double
  subroutine gr_set_logical(this, tag, tval, unit)
    class(graffer_record), intent(out) :: this
    character(len=3), intent(in) :: tag
    logical(kind=int8), intent(in) :: tval
    integer, intent(in), optional :: unit

    ! Set a logical(1) value for a record.

    integer :: status

    this%tcode = idl_byte
    this%tag = tag
    this%ndims = 0

    this%b_val = transfer(tval, this%b_val)
    if (present(unit)) call this%put(unit, status)
  end subroutine gr_set_logical
  subroutine gr_set_byte(this, tag, bval, unit)
    class(graffer_record), intent(out) :: this
    character(len=3), intent(in) :: tag
    integer(kind=int8), intent(in) :: bval
    integer, intent(in), optional :: unit

    ! Set a byte value for a record.

    integer :: status

    this%tcode = idl_byte
    this%tag = tag
    this%ndims = 0

    this%b_val = bval
    if (present(unit)) call this%put(unit, status)
  end subroutine gr_set_byte
  subroutine gr_set_string(this, tag, sval, unit)
    class(graffer_record), intent(out) :: this
    character(len=3), intent(in) :: tag
    character(len=*), intent(in) :: sval
    integer, intent(in), optional :: unit

    ! Set a string value for a record.

    integer :: i
    integer :: status

    this%tcode = idl_string
    this%tag = tag
    this%ndims = 0

    allocate(this%length(1))
    this%length(1) = len_trim(sval)
    
    allocate(this%ba_val(this%length(1)))
    do i = 1, len_trim(sval)
       this%ba_val(i) = ichar(sval(i:i), int8)
    end do
    if (present(unit)) call this%put(unit, status)
  end subroutine gr_set_string
  
  subroutine gr_set_int_a(this, tag, ival, unit)
    class(graffer_record), intent(out) :: this
    character(len=3), intent(in) :: tag
    integer(kind=int16), intent(in), dimension(:) :: ival
    integer, intent(in), optional :: unit

    ! Set an int(2) array for a record.

    integer :: status

    this%tcode = idl_int
    this%tag = tag
    this%ndims = 1
    allocate(this%dims(1))
    this%dims(1) = size(ival)

    allocate(this%ia_val(this%dims(1)))
    this%ia_val = ival
    if (present(unit)) call this%put(unit, status)
  end subroutine gr_set_int_a
  subroutine gr_set_long_a(this, tag, lval, unit)
    class(graffer_record), intent(out) :: this
    character(len=3), intent(in) :: tag
    integer(kind=int32), intent(in), dimension(:) :: lval
    integer, intent(in), optional :: unit

    ! Set a long(4) array for a record.

    integer :: status

    this%tcode = idl_long
    this%tag = tag
    this%ndims = 1
    allocate(this%dims(1))
    this%dims(1) = size(lval)

    allocate(this%la_val(this%dims(1)))
    this%la_val = lval
    if (present(unit)) call this%put(unit, status)
  end subroutine gr_set_long_a
  subroutine gr_set_float_a(this, tag, rval, unit)
    class(graffer_record), intent(out) :: this
    character(len=3), intent(in) :: tag
    real(kind=real32), intent(in), dimension(:) :: rval
    integer, intent(in), optional :: unit

    ! Set a float(4) array for a record.

    integer :: status

    this%tcode = idl_float
    this%tag = tag
    this%ndims = 1
    allocate(this%dims(1))
    this%dims(1) = size(rval)

    allocate(this%ra_val(this%dims(1)))
    this%ra_val = rval
    if (present(unit)) call this%put(unit, status)
  end subroutine gr_set_float_a
  subroutine gr_set_double_a(this, tag, dval, unit)
    class(graffer_record), intent(out) :: this
    character(len=3), intent(in) :: tag
    real(kind=real64), intent(in), dimension(:) :: dval
    integer, intent(in), optional :: unit

    ! Set a double(8) array for a record.

    integer :: status

    this%tcode = idl_double
    this%tag = tag
    this%ndims = 1
    allocate(this%dims(1))
    this%dims(1) = size(dval)

    allocate(this%da_val(this%dims(1)))
    this%da_val = dval
    if (present(unit)) call this%put(unit, status)
  end subroutine gr_set_double_a
  subroutine gr_set_string_a(this, tag, sval, unit)
    class(graffer_record), intent(out) :: this
    character(len=3), intent(in) :: tag
    character(len=*), intent(in), dimension(:) :: sval
    integer, intent(in), optional :: unit

    ! Set a string array for a record.

    integer :: i, j, lmax
    integer :: status

    this%tcode = idl_string
    this%tag = tag
    this%ndims = 1
    allocate(this%dims(1))
    this%dims(1) = size(sval)

    allocate(this%length(size(sval)))
    this%length = len_trim(sval)
    lmax = maxval(this%length)
    allocate(this%baa_val(lmax, this%dims(1)))
    do j = 1, size(sval)
       do i = 1, this%length(j)
          this%baa_val(i,j) = ichar(sval(j)(i:i), int8)
       end do
    end do
    if (present(unit)) call this%put(unit, status)
  end subroutine gr_set_string_a

  subroutine gr_set_double_aa(this, tag, dval, unit)
    class(graffer_record), intent(out) :: this
    character(len=3), intent(in) :: tag
    real(kind=real64), intent(in), dimension(:,:) :: dval
    integer, intent(in), optional :: unit

    ! Set a double(8) 2D array for a record.

    integer :: status

    this%tcode = idl_double
    this%tag = tag
    this%ndims = 2
    allocate(this%dims(2))
    this%dims = shape(dval)

    allocate(this%daa_val(this%dims(1), this%dims(2)))
    this%daa_val = dval

    if (present(unit)) call this%put(unit, status)
  end subroutine gr_set_double_aa
  
  subroutine gr_put_rec(this, unit, status)
    class(graffer_record), intent(in) :: this
    integer, intent(in) :: unit
    integer, intent(out) :: status

    ! write a record to a binary Graffer file.

    logical :: isopen
    integer :: ios, i
    character(len=120) :: iomsg

    inquire(unit=unit, opened=isopen)
    if (.not. isopen) then
       write(error_str , "(A,i0,a)") &
            & "GR_PUT_REC: Unit: ", unit, " is not open"
       call gr_message(error_str(1), type=GTK_MESSAGE_ERROR)
       status = 1
    end if

    write(unit, iostat=ios, iomsg=iomsg) this%tag, to_little(this%tcode), &
         & to_little(this%ndims)
    if (ios /= 0) then
       write(error_str, "(A)") "GR_PUT_REC: Failed to write tag header", &
            & trim(iomsg)
       call gr_message(error_str)
       status = 1
       return
    end if

    if (this%ndims > 0) then
       write(unit, iostat=ios, iomsg=iomsg) to_little(this%dims)
       if (ios /= 0) then
          write(error_str, "(A)") "GR_PUT_REC: Failed to write dimensions", &
               & trim(iomsg)
          call gr_message(error_str)
          status = 1
          return
       end if
    end if
    if (this%tcode == idl_string) then
       write(unit, iostat=ios, iomsg=iomsg) to_little(this%length)
       if (ios /= 0) then
          write(error_str, "(A)") "GR_READ_REC: Failed to write lengths", &
               & trim(iomsg)
          call gr_message(error_str)
          status = 1
          return
       end if
    end if

    select case (this%ndims)
    case(0)      ! Scalars
       select case (this%tcode)
       case(idl_undefined)
          ! Nothing to write
       case(idl_byte)
          write(unit, iostat=ios, iomsg=iomsg) this%b_val
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_PUT_REC: Failed to write byte scalar", &
                  & trim(iomsg)
             call gr_message(error_str)
            status = 1
             return
          end if
       case(idl_int)
          write(unit, iostat=ios, iomsg=iomsg) to_little(this%i_val)
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_PUT_REC: Failed to write integer scalar", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
       case(idl_long)
          write(unit, iostat=ios, iomsg=iomsg) to_little(this%l_val)
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_PUT_REC: Failed to write long scalar", &
                  & trim(iomsg)
             call gr_message(error_str)
            status = 1
             return
          end if
       case(idl_float)
          write(unit, iostat=ios, iomsg=iomsg) to_little(this%r_val)
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_PUT_REC: Failed to write float scalar", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
       case(idl_double)
          write(unit, iostat=ios, iomsg=iomsg) to_little(this%d_val)
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_PUT_REC: Failed to write double scalar", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
       case(idl_string)
          write(unit, iostat=ios, iomsg=iomsg) this%ba_val
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_PUT_REC: Failed to write string scalar", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
       end select

    case(1)    ! 1-D arrays
       select case (this%tcode)
       case (idl_byte)
          write(unit, iostat=ios, iomsg=iomsg) this%ba_val
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_PUT_REC: Failed to write byte array", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
       case (idl_int)
          write(unit, iostat=ios, iomsg=iomsg) to_little(this%ia_val)
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_PUT_REC: Failed to write int array", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
       case (idl_long)
          write(unit, iostat=ios, iomsg=iomsg) to_little(this%la_val)
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_PUT_REC: Failed to write long array", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
       case (idl_float)
          write(unit, iostat=ios, iomsg=iomsg) to_little(this%ra_val)
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_PUT_REC: Failed to write float array", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
       case (idl_double)
          write(unit, iostat=ios, iomsg=iomsg) to_little(this%da_val)
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_PUT_REC: Failed to write double array", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
       case (idl_string)
          do i = 1, this%dims(1)
             write(unit, iostat=ios, iomsg=iomsg) &
                  & this%baa_val(:this%length(i), i)
             if (ios /= 0) then
                write(error_str, "(A)") &
                     & "GR_PUT_REC: Failed to write string array", &
                     & trim(iomsg)
                call gr_message(error_str)
                status = 1
                return
             end if
          end do
       end select

    case (2)   ! 2-D arrays (only double supported)

       select case (this%tcode)
       case(idl_double)
          write(unit, iostat=ios, iomsg=iomsg) to_little(this%daa_val)
          if (ios /= 0) then
             write(error_str, "(A)") &
                  & "GR_PUT_REC: Failed to write double array", &
                  & trim(iomsg)
             call gr_message(error_str)
             status = 1
             return
          end if
       end select
    end select

    status = 0 
  end subroutine gr_put_rec
end module gr_record
