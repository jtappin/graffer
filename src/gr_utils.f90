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

module gr_utils
  ! Utility routines for the Fortran version of GRAFFER

  ! Also widely needed constants

  use iso_fortran_env
  use iso_c_binding
  use ieee_arithmetic
  
  use gtk_sup

  use gr_msg

  use g, only: g_find_program_in_path, g_get_home_dir, &
       & g_file_test, g_get_current_dir
  use gtk, only: G_FILE_TEST_IS_REGULAR, G_FILE_TEST_IS_DIR

  implicit none

  interface byte_swap
     module procedure swap_8    ! A dummy
     module procedure swap_16
     module procedure swap_32
     module procedure swap_r32
     module procedure swap_r64
  end interface byte_swap

  interface to_little
     module procedure little_8
     module procedure little_16
     module procedure little_32
     module procedure little_r32
     module procedure little_r64
  end interface to_little

  interface count_lines
     module procedure count_lines_file
     module procedure count_lines_unit
  end interface count_lines

  
  integer(kind=int8), parameter, private, dimension(2) :: z1 = [0_int8, 1_int8]
  integer(kind=int16), parameter, private :: blv = transfer(z1, 0_int16)
  logical, parameter :: is_big_endian = blv == 1_int16

  private :: swap_8, swap_16, swap_32, swap_r32, swap_r64
  private :: little_8, little_16, little_32, little_r32, little_r64
  private :: count_lines_file, count_lines_unit
   
  ! Character limits

  integer, parameter, private :: lcmin = iachar('a'), lcmax = iachar('z')
  integer, parameter, private :: ucmin = iachar('A'), ucmax = iachar('Z')
  integer, parameter, private :: case_diff = iachar('A')-iachar('a')

contains

  ! Byte swapping routines.

  elemental subroutine swap_8(i8, o8)
    integer(kind=int8), intent(inout) :: i8
    integer(kind=int8), intent(out), optional :: o8

    if (present(o8)) o8 = i8
  end subroutine swap_8

  elemental function little_8(i8)
    integer(kind=int8) :: little_8
    integer(kind=int8), intent(in) :: i8

    little_8 = i8

  end function little_8

  elemental subroutine swap_16(i16, o16)
    integer(kind=int16), intent(inout) :: i16
    integer(kind=int16), intent(out), optional :: o16

    integer(kind=int8), dimension(2) :: h8
    integer(kind=int8) :: tmp

    h8 = transfer(i16, h8)
    tmp = h8(1)
    h8(1) = h8(2)
    h8(2) = tmp

    if (present(o16)) then
       o16 = transfer(h8, o16)
    else
       i16 = transfer(h8, i16)
    end if

  end subroutine swap_16

  elemental function little_16(i16)
    integer(kind=int16) :: little_16
    integer(kind=int16), intent(in) :: i16

    integer(kind=int16) :: ii16

    if (is_big_endian) then
       ii16 = i16
       call swap_16(ii16, little_16)
    else
       little_16 = i16
    end if

  end function little_16

  elemental subroutine swap_32(i32, o32)
    integer(kind=int32), intent(inout) :: i32
    integer(kind=int32), intent(out), optional :: o32

    integer(kind=int8), dimension(4) :: h8
    integer(kind=int8) :: tmp
    integer :: i

    h8 = transfer(i32, h8)

    do i = 1, 2
       tmp = h8(i)
       h8(i) = h8(5-i)
       h8(5-i) = tmp
    end do

    if (present(o32)) then
       o32 = transfer(h8, o32)
    else
       i32 = transfer(h8, i32)
    end if

  end subroutine swap_32

  elemental function little_32(i32)
    integer(kind=int32) :: little_32
    integer(kind=int32), intent(in) :: i32

    integer(kind=int32) :: ii32

    if (is_big_endian) then
       ii32 = i32
       call swap_32(ii32, little_32)
    else
       little_32 = i32
    end if

  end function little_32

  elemental subroutine swap_r32(r32, x32)
    real(kind=real32), intent(inout) :: r32
    real(kind=real32), intent(out), optional :: x32

    integer(kind=int8), dimension(4) :: h8
    integer(kind=int8) :: tmp
    integer :: i

    h8 = transfer(r32, h8)

    do i = 1, 2
       tmp = h8(i)
       h8(i) = h8(5-i)
       h8(5-i) = tmp
    end do

    if (present(x32)) then
       x32 = transfer(h8, x32)
    else
       r32 = transfer(h8, r32)
    end if
  end subroutine swap_r32

  elemental function little_r32(r32)
    real(kind=real32) :: little_r32
    real(kind=real32), intent(in) :: r32

    real(kind=real32) :: rr32

    if (is_big_endian) then
       rr32 = r32
       call swap_r32(rr32, little_r32)
    else
       little_r32 = r32
    end if

  end function little_r32

  elemental subroutine swap_r64(r64, x64)
    real(kind=real64), intent(inout) :: r64
    real(kind=real64), intent(out), optional :: x64

    integer(kind=int8), dimension(8) :: h8
    integer(kind=int8) :: tmp
    integer :: i

    h8 = transfer(r64, h8)

    do i = 1, 4
       tmp = h8(i)
       h8(i) = h8(9-i)
       h8(9-i) = tmp
    end do

    r64 = transfer(h8, r64)

    if (present(x64)) then
       x64 = transfer(h8, x64)
    else
       r64 = transfer(h8, r64)
    end if
  end subroutine swap_r64

  elemental function little_r64(r64)
    real(kind=real64) :: little_r64
    real(kind=real64), intent(in) :: r64

    real(kind=real64) :: rr64

    if (is_big_endian) then
       rr64 = r64
       call swap_r64(rr64, little_r64)
    else
       little_r64 = r64
    end if

  end function little_r64

  ! Case swapping

  elemental function upcase(str)
    character(len=*), intent(in) :: str
    character(len=len(str)) :: upcase

    integer :: i,ic

    upcase = str
    do i = 1, len_trim(str)
       ic = iachar(str(i:i))
       if (ic >= lcmin .and. ic <= lcmax) upcase(i:i) = achar(ic+case_diff)
    end do
  end function upcase

  elemental function lowcase(str)
    character(len=*), intent(in) :: str
    character(len=len(str)) :: lowcase

    integer :: i,ic

    lowcase = str
    do i = 1, len_trim(str)
       ic = iachar(str(i:i))
       if (ic >= ucmin .and. ic <= ucmax) lowcase(i:i) = achar(ic-case_diff)
    end do
  end function lowcase

  ! Character test.

  elemental function is_letters(str)
    logical :: is_letters
    character(len=*), intent(in) :: str

    integer :: i, ic

    is_letters = .true.

    do i = 1, len_trim(str)
       ic = iachar(str(i:i))
       if ((ic < ucmin .or. ic > ucmax) .and. &
            & (ic < lcmin .or. ic > lcmax)) then
          is_letters = .false.
          exit
       end if
    end do

  end function is_letters
  
  function file_exists(file)
    logical :: file_exists
    character(len=*), intent(in) :: file

    ! Test if a file exists.

    inquire(file=file, exist=file_exists)

  end function file_exists

  subroutine split_fname(fullname, name, dir)
    character(len=*), intent(in) :: fullname
    character(len=*), intent(out) :: name, dir

    ! Split a file name into directory and filename.

    integer :: ps

    ps = index(fullname, '/', back=.true.)
    name = trim(fullname(ps+1:))

    if (ps == 0) then
       call gr_current_dir(dir)
       if (index(dir,'/', back=.true.) < len_trim(dir)) &
            & dir = trim(dir)//'/'
    else
       dir = fullname(:ps)
    end if
  end subroutine split_fname

  function count_lines_file(file, quiet) result(count_lines)
    integer :: count_lines
    character(len=*), intent(in) :: file
    logical, intent(in), optional :: quiet

    !+
    ! COUNT_LINES
    !	Count the lines in a file.
    !-

    integer :: lunit, ios
    character :: in    ! only need 1 char as it will skip the rest of the line
    character(len=200) :: iom
    logical :: iprint

    if (present(quiet)) then
       iprint = .not. quiet
    else
       iprint = .true.
    end if

    count_lines=0
    open(newunit=lunit, file=file, action='read', form='formatted', &
         & iostat=ios, iomsg=iom, status='old')
    if (ios /= 0) then
       if (iprint) then
          write(error_unit, *) "COUNT_LINES:: ", trim(file)
          write(error_unit, *) "              ", trim(iom)
       end if
       count_lines = -1
    else
       do
          read(lunit, '(A)', iostat=ios) in
          if (ios /= 0) exit
          count_lines = count_lines+1
       end do

       close(lunit)
    end if

  end function count_lines_file
  function count_lines_unit(unit) result(count_lines)
    integer :: count_lines
    integer, intent(in) :: unit

    !+
    ! COUNT_LINES
    !	Count the lines in a file.
    !-

    integer :: ios
    character :: in    ! only need 1 char as it will skip the rest of the line
    logical :: isopen

    count_lines=0
    inquire(unit=unit, opened=isopen)
    if (.not. isopen) then
       count_lines = -1
    else 
       rewind(unit)   ! just make sure
       do
          read(unit, '(A)', iostat=ios) in
          if (ios /= 0) exit
          count_lines = count_lines+1
       end do

       rewind(unit)

    end if

  end function count_lines_unit

  subroutine split(str, list, sta, count, maxlen, istrunc)

    !+
    ! SPLIT
    !   Split a string at any of the list of characters
    !-

    character(len=*), intent(in) :: str, list
    character(len=*), dimension(:), allocatable, intent(out) :: sta
    integer, optional, intent(out) :: count, maxlen
    logical, optional, intent(out) :: istrunc

    integer :: i, n, maxl
    integer :: idx0, idx1

    ! Firstly, find how many substrings there will be (and also if any 
    ! will be truncated)

    n = 0
    idx0 = 1
    idx1 = 0
    maxl=0
    do 
       idx0=idx1+verify(str(idx1+1:),list)
       if (idx0 == idx1) exit     ! No more characters that are not dividers.
       idx1 = idx0+scan(str(idx0:), list)-2
       n = n+1
       if (idx1 < idx0) then      ! Substring extends to end of string.
          maxl=max(maxl,len(str)-idx0+1)
          exit
       else
          maxl=max(maxl,idx1-idx0+1)
       end if
    end do

    if (present(count)) count=n
    if (present(maxlen)) maxlen=maxl
    if (present(istrunc)) then
       istrunc = maxl > len(sta)
    else if (maxl > len(sta)) then
       write(error_unit,*) "SPLIT:: Some substrings truncated"
       write(error_unit,*) "SPLIT:: MAXLEN:",maxl," LEN:",len(sta)
    end if


    ! Allocate the output array and extract the substrings to it.

    allocate(sta(n))

    idx0 = 1
    idx1 = 0
    do i = 1, n
       idx0=idx1+verify(str(idx1+1:),list)
       idx1 = idx0+scan(str(idx0:), list)-2
       if (idx1 < idx0) then
          sta(i) = str(idx0:)
       else
          sta(i) = str(idx0:idx1)
       end if
    end do

  end subroutine split

  subroutine gr_find_viewers(list, eps)
    character(len=*), dimension(:), allocatable, intent(out) :: list
    logical, intent(in), optional :: eps

    ! Find PDF or EPS viewers.

    logical :: isps
    logical, dimension(:), allocatable :: found
    integer :: i, j

    character(len=10), dimension(5), target :: epsviewers = &
         & [character(len=10) :: 'okular', 'evince', 'gv', &
         & 'kghostview', 'ghostview']
    character(len=10), dimension(7), target :: pdfviewers = &
         & [character(len=10) :: 'acroread',  'okular', &
         & 'evince', 'gv', 'kpdf', 'xpdf', 'kghostview']

    character(len=10), dimension(:), pointer :: viewers

    if (present(eps)) then
       isps = eps
    else
       isps = .false.
    end if

    if (isps) then
       viewers => epsviewers
    else
       viewers => pdfviewers
    end if

    allocate(found(size(viewers)))
    do i = 1, size(viewers)
       found(i) = gr_find_program(viewers(i))
    end do

    if (count(found) == 0) then
       call gr_message("gr_find_viewers: No suitable viewers found")
       return
    end if

    allocate(list(count(found)))
    j = 1
    do i = 1, size(found)
       if (.not. found(i)) cycle
       list(j) = viewers(i)
       j = j+1
    end do
  end subroutine gr_find_viewers

  subroutine gr_date(date, ut, time)
    character(len=*), intent(out) :: date
    logical, intent(in), optional :: ut
    integer, dimension(8), intent(in), optional :: time

    ! Convert a date to a string.

    integer, dimension(8) :: tvals
    logical :: gmt
    integer :: dh, dm

    if (len(date) < 19) then
       write(error_unit, *) "gr_date: output buffer too small (< 19)"
       date = ''
       return
    end if

    if (present(ut)) then
       gmt = ut
    else
       gmt = .false.
    end if

    if (present(time)) then
       tvals = time
    else
       call date_and_time(values=tvals)
    end if

    dh = tvals(4) / 60
    dm = tvals(4) - 60*dh
    if (gmt) then
       tvals(5) = tvals(5) - dh
       tvals(6) = tvals(6) - dm

       if (tvals(6) < 0) then
          tvals(6) = tvals(6) + 60
          tvals(5) = tvals(5) - 1
       else if (tvals(6) >= 60) then
          tvals(6) = tvals(6) - 60
          tvals(5) = tvals(5) + 1
       end if

       if (tvals(5) < 0) then
          tvals(5) = tvals(5) + 24
          tvals(3) = tvals(3) - 1
       else if (tvals(6) >= 24) then
          tvals(5) = tvals(5) - 24
          tvals(3) = tvals(3) + 1
       end if

       if (tvals(3) < 1) then
          tvals(3) = tvals(3) + mdays(tvals(1), tvals(2)-1)
          tvals(2) = tvals(2) - 1
       else if (tvals(3) > mdays(tvals(1), tvals(2))) then
          tvals(3) = tvals(3) - mdays(tvals(1), tvals(2))
          tvals(2) = tvals(2) + 1
       end if
       if (tvals(2) < 1) then
          tvals(2) = tvals(2) + 12
          tvals(1) = tvals(1) - 1
       else if (tvals(2) > 12) then
          tvals(2) = tvals(2) - 12
          tvals(1) = tvals(1) + 1
       end if
    end if

    write(date, "(I4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2)") &
         & tvals(1:3), tvals(5:7)

    if (len(date) >= 23) write(date(20:), "('.',I3.3)") tvals(8)
    if (len(date) >= 28 .and. .not. gmt) &
         & write(date(24:), "(sp,i3.2,ss,i2.2)") dh, abs(mod(tvals(4),60))

  contains
    function mdays(y, m)
      integer :: mdays
      integer, intent(in) :: y, m

      integer, parameter, dimension(12) :: lengths =&
           & [31,28,31,30,31,30,31,31,30,31,30,31]

      mdays = lengths(m)
      if (m == 2 .and. mod(y, 4) == 0 .and. &
           & (mod(y, 100) /= 0 .or. mod(y, 400) == 0)) mdays = mdays+1
    end function mdays
  end subroutine gr_date

  ! Miscellaneous glib interfaces with "fortranization"

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

  function first(l) result(p1)
    integer :: p1
    logical, intent(in), dimension(:) :: l

    do p1 = 1, size(l)
       if (l(p1)) return
    end do
    p1=0
  end function first
  function last(l) result(pl)
    integer :: pl
    logical, intent(in), dimension(:) :: l

    do pl = size(l),1,-1
       if (l(pl)) return
    end do
    pl=0
  end function last

  function truth(str, default, status)
    logical(kind=int8) :: truth
    character(len=*), intent(in) :: str
    logical(kind=int8), intent(in), optional :: default
    integer, intent(out), optional :: status

    status = 0
    select case(lowcase(trim(adjustl(str))))
    case('t', 'true', 'y', 'yes', '1')
       truth = .true.
    case('f', 'false', 'n', 'no', '0' )
       truth = .false.
    case default
       write(error_unit, *) "TRUTH:: Warning: Invalid input ", trim(str)
       status = 1
       if (present(default)) then
          truth = default
       else
          truth = .false.
       end if
    end select
  end function truth

  pure function d_inf()
    real(kind=real64) :: d_inf

    d_inf = ieee_value(d_inf, ieee_positive_inf)
  end function d_inf
  
  pure function d_nan()
    real(kind=real64) :: d_nan

    d_nan = ieee_value(d_nan, ieee_quiet_nan)
  end function d_nan
end module gr_utils
