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
  ! Colour table management. (Alternative version)

  use iso_fortran_env
  use plplot

  use gtk, only: GTK_MESSAGE_ERROR, GTK_MESSAGE_WARNING

  use graff_globals
  use gr_msg
  use gr_utils

  implicit none

  integer, parameter, private :: table_size=256

  type :: gr_colour_table
     character(len=32) :: name
     integer(kind=int32), dimension(table_size) :: red, green, blue
  end type gr_colour_table

  type(gr_colour_table), dimension(:), allocatable :: tables
  integer, private :: ntables = 0
  integer(kind=int32), parameter, private :: mask8 = Z'FF'

  character(len=*), dimension(*), parameter, private :: ctdirs = &
       & ['/usr/local/share/graffer/',&
       &  '/usr/share/graffer/      ',&
       &  '/opt/graffer/data/       ']
  character(len=*), parameter, private :: ctname='colour.table'

  private :: find_ct

contains
  subroutine gr_ct_init(basename, append, dirname, table_list)
    character(len=*), intent(in), optional :: basename, dirname
    logical, intent(in), optional :: append
    type(c_ptr), intent(in), optional :: table_list

    character(len=300) :: datafile
    integer :: unit, ios, i, istable
    character(len=120) :: iom
    character(len=160), dimension(2) :: err_string
    logical :: iapp
    integer(kind=int8) :: ntabf
    type(gr_colour_table), dimension(:), allocatable :: tmp_tables
    integer(kind=int8), dimension(table_size) :: rtmp, gtmp, btmp

    if (present(append)) then
       iapp = append
    else
       iapp = .false.
    end if

    call find_ct(datafile, basename, dirname)
    if (datafile == '') then
       call gr_message("Failed to find colour tables", type=GTK_MESSAGE_ERROR)
       print *, iapp, ntables
       if (.not. iapp .and. ntables == 0) call gr_ct_simple
       print *, iapp, ntables
       return
    end if

    print *, trim(datafile)
    open(newunit=unit, file=datafile, form='unformatted', status='old', &
         & action='read', iostat=ios, iomsg=iom, access='stream')
    if (ios /= 0) then
       write(err_string, "(2A/t10,a)") &
            & "gr_ct_init: Failed to open table file: ", &
            & trim(datafile), trim(iom)
       call gr_message(err_string, type=GTK_MESSAGE_ERROR)
       call gr_ct_simple
       return
    end if

    read(unit) ntabf
    print *, ntabf
    if (ntabf == 0) then
       call gr_message("Empty colour tables", type=GTK_MESSAGE_WARNING)
       if (.not. iapp .and. ntables == 0) call gr_ct_simple
       return
    end if

    if (ntables > 0 .and. iapp) then
       call move_alloc(tables, tmp_tables)
       ntables = ntables + ntabf
    else
       if (allocated(tables)) deallocate(tables)
       ntables = ntabf
    end if


    allocate(tables(ntables))

    if (allocated(tmp_tables)) then
       tables(:size(tmp_tables)) = tmp_tables
       istable = size(tmp_tables)+1
       deallocate(tmp_tables)
    else
       istable = 1
    end if

    print *, istable, ntables

    do i = istable, ntables
       read(unit, iostat=ios, iomsg=iom) tables(i)%name, rtmp, gtmp, btmp
       if (ios /= 0) then
          write(err_string, "(a,i0,2a/t10,a)") &
               & "gr_ct_init: Failed to read table #: ", i, ' from', &
               & trim(datafile), trim(iom)
          call gr_message(err_string, type=GTK_MESSAGE_ERROR)
          exit
       end if
       print *, i, tables(i)%name
       print *, rtmp

       ! Note we must do the IAND otherwise values >= 128 will appear
       ! negative, since Fortran has no unsigned types/attributes.

       tables(i)%red = iand(int(rtmp, int32), mask8)
       tables(i)%green = iand(int(gtmp, int32), mask8)
       tables(i)%blue = iand(int(btmp, int32), mask8)

    end do

    close(unit)

    if (present(table_list)) then
       if (iapp) then
          call hl_gtk_listn_ins(table_list, count=ntables-istable+1)
          do i = istable, ntables
             call hl_gtk_listn_set_cell(table_list, &
                  & int(i-1, c_int), 0_c_int, &
                  & svalue = trim(tables(i)%name)//c_null_char)
          end do
       else
          call hl_gtk_listn_rem(table_list)
          do i = 1, ntables
             call hl_gtk_listn_set_cell(table_list, &
                  & int(i-1, c_int), 0_c_int, &
                  & svalue = trim(tables(i)%name)//c_null_char)
          end do
       end if
    end if

  end subroutine gr_ct_init

  subroutine find_ct(datafile, basename, dirname)
    character(len=*), intent(out) :: datafile
    character(len=*), intent(in), optional :: basename, dirname

    integer :: i

    if (present(basename)) then
       datafile = basename
    else if (pdefs%opts%colour_stem /= '') then
       datafile = pdefs%opts%colour_stem
    else 
       datafile='colours.table'
    end if

    ! Search order is:
    ! 1) Dirname argument
    ! 2) pdefs location
    ! 3) current directory
    ! 4) System locations

    if (present(dirname)) then
       if (file_exists(trim(dirname)//'/'//trim(datafile))) then
          datafile = trim(dirname)//'/'//trim(datafile)
          return
       end if
    end if

    if (pdefs%opts%colour_dir /= '') then
       if (file_exists(trim(pdefs%opts%colour_dir)//'/'//trim(datafile))) then
          datafile = trim(pdefs%opts%colour_dir)//'/'//trim(datafile)
          return
       end if
    end if

    if (file_exists('./'//trim(datafile))) then
       datafile = './'//trim(datafile)
       return
    end if

    do i = 1, size(ctdirs)
       if (file_exists(trim(ctdirs(i))//trim(datafile))) then
          datafile = ctdirs(i)//trim(datafile)
          return
       end if
    end do

    datafile = ''
  end subroutine find_ct

  subroutine gr_ct_simple

    ! Simple greyscale table if the colour table files are not opened

    integer :: i

    ntables = 1
    allocate(tables(1))

    tables(1)%name = "Simple Greyscale"
    do i = 1, table_size
       tables(1)%red(i) = i-1
       tables(1)%green(i) = i-1
       tables(1)%blue(i) = i-1
    end do

  end subroutine gr_ct_simple

  subroutine gr_ct_get(index, load, r, g, b, invert, gamma)
    integer, intent(in) :: index
    logical, intent(in) :: load
    integer, dimension(:), intent(out), optional :: r, g, b
    logical(kind=int8), intent(in), optional :: invert
    real(kind=real32), intent(in), optional :: gamma

    ! Select, and load or return a colour table.

    integer, dimension(table_size) :: map, rr, gg, bb
    integer :: ios, i
    character(len=120) :: iom
    logical :: reversed
    real(kind=real32) :: xgamma
    character(len=160), dimension(2) :: err_string
    integer :: idx

    if (present(invert)) then
       reversed = invert
    else
       reversed = .false.
    end if

    idx = min(max(index,0),ntables-1)
    if (idx /= index) then
       call gr_message("Table number out of range, using 0", &
            & type=GTK_MESSAGE_WARNING)
       idx = 0
    end if

    if (present(gamma)) then
       xgamma = gamma
    else
       xgamma = 1.0
    end if

    if (xgamma /= 1.) then
       map = int(table_size*([(real(i), i = 0, table_size-1)]/&
            & real(table_size))**xgamma) + 1
       rr = tables(idx+1)%red(map)
       gg = tables(idx+1)%green(map)
       bb = tables(idx+1)%blue(map)
    else
       rr = tables(idx+1)%red
       gg = tables(idx+1)%green
       bb = tables(idx+1)%blue
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
       if (size(r) == table_size) then
          r = rr
       else
          do i = 1, size(r)
             r(i) = rr(1 + ((i-1)*table_size)/size(r))
          end do
       end if
       if (reversed) r = r(size(r):1:-1)
    end if

    if (present(g)) then
       if (size(g) == table_size) then
          g = gg
       else
          do i = 1, size(g)
             g(i) = gg(1 + ((i-1)*table_size)/size(g))
          end do
       end if
       if (reversed) g = g(size(g):1:-1)
    end if

    if (present(b)) then
       if (size(b) == table_size) then
          b = bb
       else
          do i = 1, size(b)
             b(i) = bb(1 + ((i-1)*table_size)/size(b))
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

  function gr_ct_get_name(index)
    character(len=32) :: gr_ct_get_name
    integer, intent(in) :: index

    if (index >= 0 .and. index < ntables) then
       gr_ct_get_name = tables(index+1)%name
    else
       gr_ct_get_name = "Invalid table number"
    end if

  end function gr_ct_get_name

end module gr_colours
