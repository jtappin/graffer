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

module gr_colours
  use iso_fortran_env
  use plplot

  use graff_globals
  use gr_msg
  use gr_utils

  implicit none

  character(len=32), dimension(:), allocatable :: table_names
  integer, dimension(:), allocatable, private :: red, green, blue
  integer, private :: ntables = 0, ncolours = 0
  integer, private :: ct_unit = -1, ct_index=-1
  logical, private :: ct_is_open
  character(len=20), private :: ct_fmt

  character(len=*), dimension(2), parameter, private :: ctdirs = &
       & ['/usr/local/share/graffer/',&
       &  '/usr/share/graffer/      ']

contains
  subroutine gr_ct_init(basename)
    character(len=*), intent(in), optional :: basename

    ! Initialize the colour table system.

    character(len=300) :: header, datafile
    integer :: unit, ios, i
    character(len=120) :: iom
    character(len=160), dimension(2) :: err_string

    if (present(basename)) then
       header = trim(basename)//".info"
       datafile = trim(basename)//".tab"
    else 
       if (pdefs%opts%colour_stem == '') pdefs%opts%colour_stem = 'c_tables'
       if (pdefs%opts%colour_dir == '') then
          do i = 1, size(ctdirs)
             if (file_exists(trim(ctdirs(i))//trim(pdefs%opts%colour_stem)// &
                  & ".tab")) then
                pdefs%opts%colour_dir=ctdirs(i)
                exit
             end if
          end do
       else if (index(pdefs%opts%colour_dir, '/', back=.true.) /= &
            & len_trim(pdefs%opts%colour_dir)) then
          pdefs%opts%colour_dir = trim(pdefs%opts%colour_dir)//'/'
       end if

       header = trim(pdefs%opts%colour_dir)//trim(pdefs%opts%colour_stem)//&
            & ".info"
       datafile = trim(pdefs%opts%colour_dir)//trim(pdefs%opts%colour_stem)//&
            & ".tab"
    end if

    if (file_exists(header) .and. file_exists(datafile)) then
       open(newunit=unit, file=header, form='formatted', status='old', &
            & action='read', iostat=ios, iomsg=iom)
       if (ios /= 0) then
          write(err_string, "(2A/t10,a)") &
               & "gr_ct_init: Failed to open header file: ", &
               & trim(header), trim(iom)
          call gr_message(err_string)
          call gr_ct_simple
          return
       end if

       call gr_ct_close

       read(unit, *) ntables, ncolours

       allocate(table_names(ntables), red(ncolours), green(ncolours), &
            & blue(ncolours))

       do i = 1, ntables
          read(unit,"(A)") table_names(i)
       end do

       close(unit)

       open(newunit=ct_unit, file=datafile, form='formatted', action='read', &
            & status='old', access='direct', recl=ncolours*6, iostat=ios, &
            & iomsg=iom)
       if (ios /= 0) then
          write(err_string, "(2A/t10,a)") &
               & "gr_ct_init: Failed to open table file: ", &
               & trim(datafile), trim(iom)
          call gr_message(err_string)
          call gr_ct_simple
          return
       end if
       ct_is_open = .true.

       write(ct_fmt, "('(',i0,'z2)')") 3*ncolours
    else
       call gr_message("gr_colours: header or data file not found")
       call gr_ct_simple
    end if
  end subroutine gr_ct_init

  subroutine gr_ct_simple

    ! Simple greyscale table if the colour table files are not opened

    integer :: i

    call gr_ct_close
    ntables = 1
    ncolours = 256
    allocate(table_names(ntables), red(ncolours), green(ncolours), &
         & blue(ncolours))
    table_names(1) = "Simple Greyscale"
    do i = 1, 256
       red(i) = i-1
       green(i) = i-1
       blue(i) = i-1
    end do
    ct_unit=-1
    ct_is_open = .true.
  end subroutine gr_ct_simple
  subroutine gr_ct_close

    ! Close the colour table files.

    if (ct_is_open) close(ct_unit)
    ct_is_open = .false.
    if (allocated(table_names)) deallocate(table_names, red, green, blue)
    ct_index = -1
  end subroutine gr_ct_close

  subroutine gr_ct_get(index, load, r, g, b, invert, gamma)
    integer, intent(in) :: index
    logical, intent(in) :: load
    integer, dimension(:), intent(out), optional :: r, g, b
    logical(kind=int8), intent(in), optional :: invert
    real(kind=real32), intent(in), optional :: gamma

    ! Select, and load or return a colour table.

    integer, dimension(:), allocatable :: map, rr, gg, bb
    integer :: ios, i
    character(len=120) :: iom
    logical :: reversed
    real(kind=real32) :: xgamma
    character(len=160), dimension(2) :: err_string

    if (present(invert)) then
       reversed = invert
    else
       reversed = .false.
    end if

    if (present(gamma)) then
       xgamma = gamma
    else
       xgamma = 1.0
    end if

    if (.not. ct_is_open .or. ct_unit==-1) then
       if (.not. ct_is_open) call gr_ct_init()
       ct_index = -1
    end if
    if (index /= ct_index) then
       ct_index = min(max(index,0), ntables-1)
       if (ct_unit /= -1) then
          read(ct_unit, ct_fmt, rec=ct_index+1, iostat=ios, iomsg=iom) red, &
               & green, blue
          if (ios /= 0) then
             write(err_string, "(A,i0/t10,a)") "gr_ct_get: Read failed: #", &
                  & ct_index, trim(iom)
             call gr_message(err_string)
             return
          end if
       end if
    end if


    allocate(rr(ncolours), gg(ncolours), bb(ncolours))
    if (xgamma /= 1.) then
       allocate(map(ncolours))
       map = int(ncolours*([(real(i), i = 0, ncolours-1)]/&
            & real(ncolours))**xgamma) + 1
       rr = red(map)
       gg = green(map)
       bb = blue(map)
    else
       rr = red
       gg = green
       bb = blue
    end if

    if (load) then
       if (reversed) then
          call plscmap1(rr(size(rr):1:-1), gg(size(rr):1:-1), &
               & bb(size(rr):1:-1))
       else
          call plscmap1(rr, gg, bb)
       end if
    end if

    if (present(r)) then
       if (size(r) == ncolours) then
          r = rr
       else
          do i = 1, size(r)
             r(i) = rr(1 + ((i-1)*ncolours)/size(r))
          end do
       end if
       if (reversed) r = r(size(r):1:-1)
    end if

    if (present(g)) then
       if (size(g) == ncolours) then
          g = gg
       else
          do i = 1, size(g)
             g(i) = gg(1 + ((i-1)*ncolours)/size(g))
          end do
       end if
       if (reversed) g = g(size(g):1:-1)
    end if

    if (present(b)) then
       if (size(b) == ncolours) then
          b = bb
       else
          do i = 1, size(b)
             b(i) = bb(1 + ((i-1)*ncolours)/size(b))
          end do
       end if
       if (reversed) b = b(size(b):1:-1)
    end if

  end subroutine gr_ct_get
  function gr_ct_get_ntables()
    integer :: gr_ct_get_ntables

    ! Return how many tables are defined

    gr_ct_get_ntables = ntables
  end function gr_ct_get_ntables

  function gr_ct_get_ncolours()
    integer :: gr_ct_get_ncolours

    ! Return the size of the colour tables

    gr_ct_get_ncolours = ncolours
  end function gr_ct_get_ncolours

  function gr_ct_get_table()
    integer :: gr_ct_get_table

    ! Return the index of the current table.

    gr_ct_get_table = ct_index
  end function gr_ct_get_table
end module gr_colours
