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

module gr_ds_tools
  ! Routines to read/write datasets to files

  use iso_fortran_env
  use iso_c_binding

  use gtk, only: gtk_notebook_set_current_page, GTK_MESSAGE_ERROR

  use graff_types
  use graff_globals
  use gr_utils

  use gr_plot
  use gr_eval

  use gr_cb_common
  use graff_init
  use gr_msg

  implicit none

  character(len=160), dimension(2), private :: err_string
contains
  subroutine gr_ds_xy_read(file)
    character(len=*), intent(in) :: file

    ! Read an XY dataset from a file

    type(graff_data), pointer :: data
    integer :: unit, ios, nlines, ntags, idx
    character(len=120) :: iom
    character(len=200), dimension(:), allocatable :: inln
    character(len=20) :: errtag
    integer :: tp, nte, nt, i, nxe, nye
    real(kind=real64), dimension(:,:), allocatable :: xyvals
    logical :: ok

    data => pdefs%data(pdefs%cset)

    open(newunit=unit, file=file, action='read', status='old', &
         & form='formatted', iostat=ios, iomsg=iom)
    if (ios /= 0) then
       write(err_string, "(2a/t10,a)") "gr_ds_xy_read: Failed to open file: ", &
            & trim(file), trim(iom)
       call gr_message(err_string, type=GTK_MESSAGE_ERROR)
       return
    end if

    nlines = count_lines(unit)

    allocate(inln(nlines))

    do i = 1, nlines
       read(unit,"(a)", iostat=ios, iomsg=iom) inln(i)
       if (ios /= 0) then
          write(err_string, "(2a/t10,a)") &
               & "gr_ds_xy_read: Error reading file: ", &
               & trim(file), trim(iom)
          call gr_message(err_string, type=GTK_MESSAGE_ERROR)
          return
       end if
    end do

    close(unit)

    ntags = count(index(inln, "#") /= 0)

    if (ntags > 0) then
       do i = nlines,1,-1
          idx = index(inln(i), "#")
          if (idx /= 0) then
             errtag = adjustl(trim(inln(i)(idx+1:)))
             exit
          end if
       end do
       where(index(inln, "#") /= 0) inln = ''
    else
       errtag = ''
    end if

    call gr_xy_decode(inln, xyvals, nlines, nt, ok)
    if (.not. ok) return

    select case (upcase(errtag))
    case('Y')
       tp = 1
       nte = 3
    case('X')
       tp = 3
       nte = 3
    case('YY')
       tp = 2
       nte = 4
    case('XX')
       tp = 4
       nte = 4
    case('XY')
       tp = 5
       nte = 4
    case('XYY')
       tp = 6
       nte = 5
    case('XXY')
       tp = 7
       nte = 5
    case('XXYY')
       tp = 8
       nte = 6
    case('')
       nte = nt
       select case (nt)
       case(2)
          tp = 0
       case(3)
          tp = 1
       case(4)
          tp = 5
       case(5)
          tp = 6
       case(6)
          tp = 8
       end select
    case default
       write(err_string, "(2A)") "gr_ds_xy_read: Invalid errors code: ", &
            & trim(errtag)
       call gr_message(err_string, type=GTK_MESSAGE_ERROR)
       return
    end select

    nxe = nx_errors(tp)
    nye = ny_errors(tp)
    
    if (nt /= nte) then
       write(err_string, "(A,I0,A/3a,i0,a)") &
            & "gr_ds_xy_read: number of columns(", nt, ") does not match", &
            & "that for error code ", trim(errtag), " (", nte,")"
       call gr_message(err_string, type=GTK_MESSAGE_ERROR)
       return
    end if

    if (allocated(data%xydata%x)) deallocate(data%xydata%x)
    if (allocated(data%xydata%y)) deallocate(data%xydata%y)
    if (allocated(data%xydata%x_err)) deallocate(data%xydata%x_err)
    if (allocated(data%xydata%y_err)) deallocate(data%xydata%y_err)
    
    if (allocated(data%zdata%x)) deallocate(data%zdata%x)
    if (allocated(data%zdata%y)) deallocate(data%zdata%y)
    if (allocated(data%zdata%z)) deallocate(data%zdata%z)

    data%ndata = nlines
    data%type = int(tp, int16)
    call gtk_entry_set_text(ds_type_id, &
         & trim(typedescrs(data%type))//c_null_char)

    call  gtk_notebook_set_current_page(display_nb, 0)

    allocate(data%xydata%x(nlines), data%xydata%y(nlines))
    if (nxe /= 0) allocate(data%xydata%x_err(nxe,nlines))
    if (nye /= 0) allocate(data%xydata%y_err(nye,nlines))

    data%xydata%x = xyvals(1,:)
    data%xydata%y = xyvals(2,:)
    if (nxe /= 0) data%xydata%x_err = xyvals(3:3+nxe-1,:)
    if (nye /= 0) data%xydata%y_err = xyvals(3+nxe:,:)

    deallocate(xyvals, inln)

    call gr_plot_draw(.true.)
    
  end subroutine gr_ds_xy_read

  subroutine gr_ds_z_read(file)
    character(len=*), intent(in) :: file

    ! Read a 2D dataset from a file.

    integer :: nx, ny
    integer :: unit, ios
    character(len=120) :: iom
    type(graff_data), pointer :: data

    real(kind=plflt), dimension(:,:), allocatable :: x,y,z
    logical(kind=int8) :: x2, y2

    data => pdefs%data(pdefs%cset)

    open(newunit=unit, file=file, status='old', action='read', &
         & form='formatted', iostat=ios, iomsg=iom)
    if (ios /= 0) then
       write(err_string, "(2a/t10,a)") "gr_ds_z_read: Failed to open file: ", &
            & trim(file), trim(iom)
       call gr_message(err_string, type=GTK_MESSAGE_ERROR)
       return
    end if

    read(unit, *, iostat=ios, iomsg=iom) nx, ny
    if (ios /= 0) then
       write(err_string, "(2a/t10,a)") &
            & "gr_ds_z_read: Failed to read size from: ", &
            & trim(file), trim(iom)
       call gr_message(err_string, type=GTK_MESSAGE_ERROR)
       close(unit)
       return
    end if


    x2 = nx < 0
    y2 = ny < 0

    nx = abs(nx)
    ny = abs(ny)

    allocate(z(nx, ny))

    if (x2) then
       allocate(x(nx, ny))
    else     
       allocate(x(nx, 1))
    end if
    if (y2) then
       allocate(y(nx, ny))
    else     
       allocate(y(1, ny))
    end if

    read(unit, *, iostat=ios, iomsg=iom) x
    if (ios /= 0) then
       write(err_string, "(2a/t10,a)") &
            & "gr_ds_z_read: Failed to read X values from: ", &
            & trim(file), trim(iom)
       call gr_message(err_string, type=GTK_MESSAGE_ERROR)
       close(unit)
       return
    end if
    read(unit, *, iostat=ios, iomsg=iom) y
    if (ios /= 0) then
       write(err_string, "(2a/t10,a)") &
            & "gr_ds_z_read: Failed to read Y values from: ", &
            & trim(file), trim(iom)
       call gr_message(err_string, type=GTK_MESSAGE_ERROR)
       close(unit)
       return
    end if
    read(unit, *, iostat=ios, iomsg=iom) z
    if (ios /= 0) then
       write(err_string, "(2a/t10,a)") &
            & "gr_ds_z_read: Failed to read Z values from: ", &
            & trim(file), trim(iom)
       call gr_message(err_string, type=GTK_MESSAGE_ERROR)
       close(unit)
       return
    end if

    if (allocated(data%zdata%x)) deallocate(data%zdata%x)
    if (allocated(data%zdata%y)) deallocate(data%zdata%y)
    if (allocated(data%zdata%z)) deallocate(data%zdata%z)
    
    if (allocated(data%xydata%x)) deallocate(data%xydata%x)
    if (allocated(data%xydata%y)) deallocate(data%xydata%y)
    if (allocated(data%xydata%x_err)) deallocate(data%xydata%x_err)
    if (allocated(data%xydata%y_err)) deallocate(data%xydata%y_err)

    data%type = 9
    data%ndata = nx
    data%ndata2 = ny
    data%zdata%x_is_2d = x2
    data%zdata%y_is_2d = y2
    call gtk_entry_set_text(ds_type_id, &
         & trim(typedescrs(data%type))//c_null_char)

    call move_alloc(x, data%zdata%x)
    call move_alloc(y, data%zdata%y)
    call move_alloc(z, data%zdata%z)
    call gtk_notebook_set_current_page(display_nb, 1)

    call gr_plot_draw(.true.)
    
  end subroutine gr_ds_z_read

  subroutine gr_ds_fun_read(file)
    character(len=*), intent(in) :: file

    ! Read a function dataset from a file.

    character(len=40) :: code
    integer(kind=int16) :: type
    real(kind=real64), dimension(2,2) :: range
    integer(kind=int32) :: neval, neval2
    character(len=256), dimension(2) :: funct
    integer :: unit, ios
    character(len=120) :: iom
    type(graff_data), pointer :: data

    open(newunit=unit, file=file, status='old', action='read', &
         & form='formatted', iostat=ios, iomsg=iom)
    if (ios /= 0) then
       write(err_string, "(2a/t10,a)") &
            & "gr_ds_fun_read: Failed to open file: ", &
            & trim(file), trim(iom)
       call gr_message(err_string, type=GTK_MESSAGE_ERROR)
       return
    end if

    read(unit, "(A)", iostat=ios, iomsg=iom) code
    if (ios /= 0) then
       write(err_string, "(a/t10,a)") &
            & "gr_ds_fun_read: Failed to read type code: ", &
            &  trim(iom)
       call gr_message(err_string, type=GTK_MESSAGE_ERROR)
       return
    end if
    code = upcase(adjustl(code))
    select case (code)
    case('Y')
       type = -1
    case('X')
       type = -2
    case('XY')
       type = -3
    case('Z')
       type = -4
    case default
       write(err_string, "(2a)") "gr_ds_fun_read: invalid type code ", &
            & trim(code)
       call gr_message(err_string(1), type=GTK_MESSAGE_ERROR)
       return
    end select

    if (type == -4) then
       read(unit, *, iostat=ios, iomsg=iom) range
       if (ios /= 0) then
          write(err_string, "(a/t10,a)") &
               & "gr_ds_fun_read: failed to read range", &
               &  trim(iom)
          call gr_message(err_string, type=GTK_MESSAGE_ERROR)
          return
       end if
       read(unit, *, iostat=ios, iomsg=iom) neval, neval2
       if (ios /= 0) then
          write(err_string, "(a/t10,a)") &
               & "gr_ds_fun_read: Failed to read evaluation counts", &
               & trim(iom)
          call gr_message(err_string, type=GTK_MESSAGE_ERROR)
          return
       end if
    else
       read(unit, *, iostat=ios, iomsg=iom) range(:,1)
       if (ios /= 0) then
          write(err_string, "(a/t10,a)") &
               & "gr_ds_fun_read: failed to read range", &
               &  trim(iom)
          call gr_message(err_string, type=GTK_MESSAGE_ERROR)
          return
       end if
       range(:,2) = 0._real64
       read(unit, *, iostat=ios, iomsg=iom) neval
       if (ios /= 0) then
          write(err_string, "(a/t10,a)") &
               & "gr_ds_fun_read: Failed to read evaluation count", &
               & trim(iom)
          call gr_message(err_string, type=GTK_MESSAGE_ERROR)
          return
       end if
       neval2 = 0_int32
    end if

    read(unit, "(A)", iostat=ios, iomsg=iom) funct(1)
    if (ios /= 0) then
       write(err_string, "(a/t10,a)") &
            & "gr_ds_fun_read: Failed to read function definition", &
            & trim(iom)
       call gr_message(err_string, type=GTK_MESSAGE_ERROR)
       return
    end if
    if (type == -3) then
       read(unit, "(A)", iostat=ios, iomsg=iom) funct(2)
       if (ios /= 0) then
          write(err_string, "(a/t10,a)") &
               & "gr_ds_fun_read: Failed to read function definition", &
               & trim(iom)
          call gr_message(err_string, type=GTK_MESSAGE_ERROR)
          return
       end if
    else
       funct(2) = ''
    end if

    close(unit)

    data => pdefs%data(pdefs%cset)

    if (allocated(data%xydata%x)) deallocate(data%xydata%x)
    if (allocated(data%xydata%y)) deallocate(data%xydata%y)
    if (allocated(data%xydata%x_err)) deallocate(data%xydata%x_err)
    if (allocated(data%xydata%y_err)) deallocate(data%xydata%y_err)
    
    if (allocated(data%zdata%x)) deallocate(data%zdata%x)
    if (allocated(data%zdata%y)) deallocate(data%zdata%y)
    if (allocated(data%zdata%z)) deallocate(data%zdata%z)

    data%funct%evaluated = .false.
    data%funct%range = range
    data%funct%funct = funct
    data%ndata = neval
    data%ndata2 = neval2
    data%type = type
    call gtk_entry_set_text(ds_type_id, &
         & trim(typedescrs(data%type))//c_null_char)

    if (data%type == -4 .or. data%type == 9) then
       call  gtk_notebook_set_current_page(display_nb, 1)
    else
       call  gtk_notebook_set_current_page(display_nb, 0)
    end if

    call gr_plot_draw(.true.)

  end subroutine gr_ds_fun_read

  subroutine gr_xy_decode(lines, xyvals, nlines, nfields, ok, skip)
    character(len=*), dimension(:), intent(in) :: lines
    real(kind=int64), dimension(:,:), allocatable, intent(out) :: xyvals
    integer, intent(out) :: nlines, nfields
    logical, intent(out) :: ok
    logical, intent(in), optional :: skip
    
    ! Decode an XY dataset.

    integer :: i, j, ios, nskip
    character(len=120) :: iom
    character(len=32), dimension(:), allocatable :: fields 
    logical :: start_flag, skip_bad
    real(kind=int64), dimension(:,:), allocatable :: xytemp
    
    character(len=*), parameter :: white_space = ' 	'
    
    if (present(skip)) then
       skip_bad = skip
    else
       skip_bad = .false.
    end if

    nlines = count(verify(lines, white_space) /= 0)
    if (nlines == 0) then
       call gr_message("gr_xy_decode: No non-empty lines.")
       ok = .false.
       return
    end if

    j = 1
    start_flag = .true.
    nskip = 0
    do i = 1, size(lines)
       if (verify(lines(i), white_space) == 0) cycle
       if (start_flag) then
          call split(lines(i), " 	,", fields, count=nfields)
          allocate(xyvals(max(nfields, 2), nlines))
          start_flag = .false.
       end if

       if (nfields == 1) then
          xyvals(1,j) = real(j-1, real64)
          read(lines(i), *, iostat=ios, iomsg=iom) xyvals(2,j)
       else
          read(lines(i), *, iostat=ios, iomsg=iom) xyvals(:,j)
       end if
       if (ios /= 0) then
          if (skip_bad) then
             nskip = nskip+1
             cycle
          else
             write(err_string, "(a,i0/a)") &
                  & "gr_xy_decode: Failed to read line ",i, &
                  & trim(iom)
             call gr_message(err_string, type=GTK_MESSAGE_ERROR)
             ok  = .false.
             return
          end if
       end if
       j = j+1
    end do

    ok = .true.

    if (nskip /= 0) then
       if (j == 1) then
          err_string(1) = "gr_xy_decode: no valid lines found"
          err_string(2) = ''
          call gr_message(err_string, type=GTK_MESSAGE_ERROR)
          ok = .false.
       else
          call move_alloc(xyvals, xytemp)
          allocate(xyvals(max(nfields, 2), j-1))
          xyvals = xytemp(:,:j-1)
          nlines = j-1
          deallocate(xytemp)
          write(err_string, "(a,i0,a/a)") "gr_xy_decode: skipped ", nskip, &
               &" undecodable lines.", ""
          call gr_message(err_string, type=GTK_MESSAGE_WARNING)
       end if
    end if
  end subroutine gr_xy_decode

  subroutine gr_ds_write(file, as_data)
    character(len=*), intent(in) :: file
    logical, intent(in), optional :: as_data

    ! Write a dataset to a file.

    integer :: unit, ios, i
    character(len=120) :: iom
    type(graff_data), pointer :: data
    integer :: nx, ny, etype, status, nxe, nye

    data => pdefs%data(pdefs%cset)

    etype = data%type
    if (present(as_data) .and. etype < 0) then
       if (as_data) then
          if (etype == -4) then
             etype = 9
          else
             etype = 0
          end if
          status = gr_evaluate(pdefs%cset)
          if (status /= 0) then
             call gr_message(&
                  & "gr_ds_write: Failed to evaluate function to write as data", &
                  & type=GTK_MESSAGE_ERROR)
             return
          end if
       end if
    end if

    open(newunit=unit, file=file, action='write', form='formatted', &
         & iostat=ios, iomsg=iom)
    if (ios /= 0) then
       write(err_string, "(2a/t10,a)") &
            & "gr_ds_write: Failed to open: ",trim(file), &
            & trim(iom)
       call gr_message(err_string, type=GTK_MESSAGE_ERROR)
       return
    end if

    if (etype < 0) then
       select case (etype)
       case(-1)
          write(unit, "(A)") "Y"
       case(-2)
          write(unit, "(A)") "X"
       case(-3)
          write(unit, "(A)") "XY"
       case(-4)
          write(unit, "(A)") "Z"
       end select

       if (etype == -3) then
          write(unit, *) data%funct%range
       else
          write(unit, *) data%funct%range(:,1)
       end if

       if (etype == -4) then
          write(unit, *) data%ndata, data%ndata2
       else
          write(unit, *) data%ndata
       end if

       if (etype == -3) then
          write(unit, "(A)") data%funct%funct
       else
          write(unit, "(A)") data%funct%funct(1)
       end if

    else if (etype == 9) then
       nx = data%ndata
       if (data%zdata%x_is_2d) nx = -nx
       ny = data%ndata2
       if (data%zdata%y_is_2d) ny = -ny
       write(unit, *) nx, ny
       write(unit, "(6(1pg0,2x))") data%zdata%x
       write(unit, "(6(1pg0,2x))") data%zdata%y
       write(unit, "(6(1pg0,2x))") data%zdata%z

    else
       select case (etype)
       case(0)
       case(1)
          write(unit, "(A)") "#Y"
       case(2)
          write(unit, "(A)") "#YY"
       case(3)
          write(unit, "(A)") "#X"
       case(4)
          write(unit, "(A)") "#XX"
       case(5)
          write(unit, "(A)") "#XY"
       case(6)
          write(unit, "(A)") "#XYY"
       case(7)
          write(unit, "(A)") "#XXY"
       case(8)
          write(unit, "(A)") "#XXYY"
       end select

       nxe = nx_errors(etype)
       nye = ny_errors(etype)
       if (nxe > 0 .and. nye > 0) then
          do i = 1, data%ndata
             write(unit, "(6(1pg0,2x))") data%xydata%x(i), data%xydata%y(i), &
                  &  data%xydata%x_err(:,i), data%xydata%y_err(:,i)
          end do
       else if (nxe > 0) then
          do i = 1, data%ndata
             write(unit, "(6(1pg0,2x))") data%xydata%x(i), data%xydata%y(i), &
                  &  data%xydata%x_err(:,i)
          end do
       else if (nye > 0) then
          do i = 1, data%ndata
             write(unit, "(6(1pg0,2x))") data%xydata%x(i), data%xydata%y(i), &
                  & data%xydata%y_err(:,i)
          end do
       else
          do i = 1, data%ndata
             write(unit, "(6(1pg0,2x))") data%xydata%x(i), data%xydata%y(i)
          end do
       end if
    end if

    close(unit)
  end subroutine gr_ds_write


  subroutine gr_ds_new(make_current)
    logical, intent(in) :: make_current

    ! Create a new dataset

    type(graff_data), dimension(:), allocatable :: datas

    allocate(datas(pdefs%nsets+1))
    datas(:pdefs%nsets) = pdefs%data
    deallocate(pdefs%data)
    call move_alloc(datas, pdefs%data)

    pdefs%nsets = pdefs%nsets+1_int16
    if (make_current) pdefs%cset = pdefs%nsets

    if (c_associated(ds_idx_id)) &
         & call hl_gtk_spin_button_set_range_int(ds_idx_id, &
         & upper=int(pdefs%nsets, c_int))

    call gr_pdefs_data_init(index=pdefs%nsets)
    call gr_set_values_dataset()
  end subroutine gr_ds_new

  subroutine gr_ds_copy(from, to, source, destination, copy_format, &
       & move, no_housekeeping, append)
    integer(kind=int16), intent(in), optional :: from, to
    type(graff_data), target, intent(inout), optional :: source
    type(graff_data), target, intent(out), optional :: destination
    logical, intent(in), optional :: copy_format, move, no_housekeeping
    character(len=*), intent(in), optional :: append
    
    ! Make a copy of a dataset

    integer(kind=int16) :: dest
    type(graff_data), pointer :: data_from, data_to
    logical :: fcopy, realloc, update_hk
    integer :: nn
    integer, dimension(2) :: nn2

    if (present(source)) then
       data_from => source
       if (present(from)) call gr_message( &
            & "gr_ds_copy: SOURCE and FROM both present, using SOURCE")
    else if (present(from)) then
       data_from => pdefs%data(from)
    else
       call gr_message("gr_ds_copy: Must specify FROM or SOURCE", &
            & type=GTK_MESSAGE_ERROR)
       return
    end if

    if (present(destination)) then
       if (present(to)) call gr_message( &
            & "gr_ds_copy: DESTINATION and TO both present, using DESTINATION")

       if (present(copy_format)) then
          fcopy = copy_format
       else
          fcopy = .true.
       end if

       data_to => destination
    else
       if (present(to)) then
          dest = to
       else
          dest = pdefs%cset
       end if

       if (present(copy_format)) then
          fcopy = copy_format
       else
          fcopy = present(source)
       end if

       data_to => pdefs%data(dest)
    end if

    if (present(move)) then
       realloc = move
    else
       realloc = .false.
    end if
    if (realloc) fcopy = .true.

    if (present(no_housekeeping)) then
       update_hk = .not. no_housekeeping
    else
       update_hk = .true.
    end if
    
    ! First we clear the target

    call gr_pdefs_data_init(dataset=data_to, minimal=.true.)

    ! Copies common to all

    data_to%ndata = data_from%ndata
    data_to%ndata2 = data_from%ndata2
    data_to%type = data_from%type
    data_to%mode = data_from%mode

    data_to%y_axis = data_from%y_axis

    ! We don't copy the formatting options unless requested
    if (fcopy) then
       if (present(append)) then
          data_to%descript = trim(data_from%descript) // trim(append)
       else
          data_to%descript = data_from%descript
       end if
       
       data_to%pline = data_from%pline
       data_to%psym = data_from%psym
       data_to%symsize = data_from%symsize
       data_to%line = data_from%line
       data_to%colour = data_from%colour
       data_to%c_vals = data_from%c_vals
       data_to%thick = data_from%thick
       data_to%min_val = data_from%min_val
       data_to%max_val = data_from%max_val
       data_to%sort = data_from%sort
       data_to%noclip = data_from%noclip
       data_to%medit = data_from%medit

       if (data_from%type == 9 .or. data_from%type == -4) then
          data_to%zdata%format = data_from%zdata%format
          data_to%zdata%set_levels = data_from%zdata%set_levels
          data_to%zdata%n_levels = data_from%zdata%n_levels
          data_to%zdata%lmap = data_from%zdata%lmap
          data_to%zdata%n_cols = data_from%zdata%n_cols
          data_to%zdata%n_sty = data_from%zdata%n_sty
          data_to%zdata%n_thick = data_from%zdata%n_thick
          if (allocated(data_from%zdata%levels)) then
             if (realloc) then
                call move_alloc(data_from%zdata%levels, &
                     & data_to%zdata%levels)
             else
                nn = size(data_from%zdata%levels)
                allocate(data_to%zdata%levels(nn))
                data_to%zdata%levels(:) = data_from%zdata%levels
             end if
          end if
          if (allocated(data_from%zdata%thick)) then
             if (realloc) then
                call move_alloc(data_from%zdata%thick, &
                     & data_to%zdata%thick)
             else
                nn = size(data_from%zdata%thick)
                allocate(data_to%zdata%thick(nn))
                data_to%zdata%thick(:) = data_from%zdata%thick
             end if
          end if
          if (allocated(data_from%zdata%style)) then
             if (realloc) then
                call move_alloc(data_from%zdata%style, &
                     &  data_to%zdata%style)
             else
                nn = size(data_from%zdata%style)
                allocate(data_to%zdata%style(nn))
                data_to%zdata%style(:) = data_from%zdata%style
             end if
          end if
          if (allocated(data_from%zdata%colours)) then
             if (realloc) then
                call move_alloc(data_from%zdata%colours, &
                     & data_to%zdata%colours)
             else
                nn = size(data_from%zdata%colours)
                allocate(data_to%zdata%colours(nn))
                data_to%zdata%colours(:) = data_from%zdata%colours
             end if
          end if
          if (allocated(data_from%zdata%raw_colours)) then
             if (realloc) then
                call move_alloc(data_from%zdata%raw_colours, &
                     & data_to%zdata%raw_colours)
             else
                nn2 = shape(data_from%zdata%raw_colours)
                allocate(data_to%zdata%raw_colours(nn2(1),nn2(2)))
                data_to%zdata%raw_colours(:,:) = data_from%zdata%raw_colours
             end if
          end if
          data_to%zdata%range = data_from%zdata%range
          data_to%zdata%missing = data_from%zdata%missing
          data_to%zdata%pxsize = data_from%zdata%pxsize
          data_to%zdata%charsize = data_from%zdata%charsize
          data_to%zdata%gamma = data_from%zdata%gamma
          data_to%zdata%label = data_from%zdata%label
          data_to%zdata%label_off = data_from%zdata%label_off
          data_to%zdata%ctable = data_from%zdata%ctable
          data_to%zdata%fill = data_from%zdata%fill
          data_to%zdata%ilog = data_from%zdata%ilog
          data_to%zdata%invert = data_from%zdata%invert
          data_to%zdata%smooth = data_from%zdata%smooth
          data_to%zdata%shade_levels = data_to%zdata%shade_levels
       end if
    else if (present(append)) then
       data_to%descript = trim(data_from%descript) // trim(append)
    end if

    select case (data_to%type)
    case(0:8)
       if (realloc) then
          call move_alloc(data_from%xydata%x, data_to%xydata%x)
          call move_alloc(data_from%xydata%y, data_to%xydata%y)
          if (allocated(data_from%xydata%x_err)) &
               & call move_alloc(data_from%xydata%x_err, data_to%xydata%x_err)
          if (allocated(data_from%xydata%y_err)) &
               & call move_alloc(data_from%xydata%y_err, data_to%xydata%y_err)
      else
          nn = data_from%ndata
          allocate(data_to%xydata%x(nn), data_to%xydata%y(nn))
          
          data_to%xydata%x = data_from%xydata%x
          data_to%xydata%y = data_from%xydata%y

          if (nx_errors(data_to%type) /= 0) then
             allocate(data_to%xydata%x_err(nx_errors(data_to%type),nn))
             data_to%xydata%x_err = data_from%xydata%x_err
          end if
          if (ny_errors(data_to%type) /= 0) then
             allocate(data_to%xydata%y_err(ny_errors(data_to%type),nn))
             data_to%xydata%y_err = data_from%xydata%y_err
          end if
          
       end if
    case(:-1)
       data_to%funct = data_from%funct
       if (data_to%funct%evaluated) then
          if (data_to%type == -4) then
             if (realloc) then
                call move_alloc(data_from%zdata%x, data_to%zdata%x)
                call move_alloc(data_from%zdata%y, data_to%zdata%y)
                call move_alloc(data_from%zdata%z, data_to%zdata%z)
             else
                nn2 = shape(data_from%zdata%x)
                allocate(data_to%zdata%x(nn2(1),nn2(2)))
                nn2 = shape(data_from%zdata%y)
                allocate(data_to%zdata%y(nn2(1),nn2(2)))
                nn2 = shape(data_from%zdata%z)
                allocate(data_to%zdata%z(nn2(1),nn2(2)))
                data_to%zdata%x(:,:) = data_from%zdata%x
                data_to%zdata%y(:,:) = data_from%zdata%y
                data_to%zdata%z(:,:) = data_from%zdata%z
             end if
             data_to%zdata%x_is_2d = data_from%zdata%x_is_2d
             data_to%zdata%y_is_2d = data_from%zdata%y_is_2d
          else
             if (realloc) then
                call move_alloc(data_from%xydata%x, data_to%xydata%x)
                call move_alloc(data_from%xydata%y, data_to%xydata%y)
             else
                nn = data_from%ndata
                allocate(data_to%xydata%x(nn), data_to%xydata%y(nn))
                data_to%xydata%x = data_from%xydata%x
                data_to%xydata%y = data_from%xydata%y
            end if
          end if
       end if
    case(9)
       if (realloc) then
          call move_alloc(data_from%zdata%x, data_to%zdata%x)
          call move_alloc(data_from%zdata%y, data_to%zdata%y)
          call move_alloc(data_from%zdata%z, data_to%zdata%z)
       else
          nn2 = shape(data_from%zdata%x)
          allocate(data_to%zdata%x(nn2(1),nn2(2)))
          nn2 = shape(data_from%zdata%y)
          allocate(data_to%zdata%y(nn2(1),nn2(2)))
          nn2 = shape(data_from%zdata%z)
          allocate(data_to%zdata%z(nn2(1),nn2(2)))
          data_to%zdata%x(:,:) = data_from%zdata%x
          data_to%zdata%y(:,:) = data_from%zdata%y
          data_to%zdata%z(:,:) = data_from%zdata%z
       end if
       data_to%zdata%x_is_2d = data_from%zdata%x_is_2d
       data_to%zdata%y_is_2d = data_from%zdata%y_is_2d
    end select

    if (update_hk) then
       call gr_set_values_dataset()
       if (pdefs%data(pdefs%cset)%type == -4 .or. &
            & pdefs%data(pdefs%cset)%type == 9) then
          call  gtk_notebook_set_current_page(display_nb, 1)
       else
          call  gtk_notebook_set_current_page(display_nb, 0)
       end if
    end if
    
  end subroutine gr_ds_copy

  subroutine gr_ds_append(index, append_to)
    integer(kind=int16), intent(in) :: index
    integer(kind=int16), intent(in), optional :: append_to

    ! Append one dataset to another.

    integer(kind=int16) :: dest
    integer :: ndata, ndata1, nxe, nye
    real(kind=real64), allocatable, dimension(:,:) :: xyetmp
    real(kind=real64), allocatable, dimension(:) :: xytmp

    if (present(append_to)) then
       dest = append_to
    else
       dest = pdefs%cset
    end if

    if (pdefs%data(index)%ndata == 0) return

    if (pdefs%data(dest)%ndata == 0) then
       call gr_ds_copy(from=index, to=dest, copy_format=.false.)
       return
    end if

    if (pdefs%data(dest)%type /= pdefs%data(index)%type) then
       write(err_string, "(a/t10,a,i0,a,i0)") &
            & "gr_ds_append: Datasets to concatenate must have the same types",&
            & "Source: ", pdefs%data(index)%type, " Destination: ", &
            & pdefs%data(dest)%type
       call gr_message(err_string, type=GTK_MESSAGE_ERROR)
       return
    end if
    if (pdefs%data(dest)%type == 9 .or. pdefs%data(dest)%type < 0) then
      call gr_message( &
            & "gr_ds_append: Datasets to concatenate must be X-Y data", &
            & type=GTK_MESSAGE_ERROR)
       return
    end if

    ndata = pdefs%data(dest)%ndata + pdefs%data(index)%ndata
    ndata1 = pdefs%data(dest)%ndata
    nxe = nx_errors(pdefs%data(index)%type)
    nye = ny_errors(pdefs%data(index)%type)

    allocate(xytmp(ndata))
    xytmp(:ndata1) = pdefs%data(dest)%xydata%x
    xytmp(ndata1+1:) = pdefs%data(index)%xydata%x
    deallocate(pdefs%data(dest)%xydata%x)
    call move_alloc(xytmp, pdefs%data(dest)%xydata%x)

    allocate(xytmp(ndata))
    xytmp(:ndata1) = pdefs%data(dest)%xydata%y
    xytmp(ndata1+1:) = pdefs%data(index)%xydata%y
    deallocate(pdefs%data(dest)%xydata%y)
    call move_alloc(xytmp, pdefs%data(dest)%xydata%y)

    if (nxe > 0) then
       allocate(xyetmp(nxe, ndata))
       xyetmp(:,:ndata1) = pdefs%data(dest)%xydata%x_err
       xyetmp(:,ndata1+1:) = pdefs%data(index)%xydata%x_err
       deallocate(pdefs%data(dest)%xydata%x_err)
       call move_alloc(xyetmp, pdefs%data(dest)%xydata%x_err)
    end if
    if (nye > 0) then
       allocate(xyetmp(nye, ndata))
       xyetmp(:,:ndata1) = pdefs%data(dest)%xydata%y_err
       xyetmp(:,ndata1+1:) = pdefs%data(index)%xydata%y_err
       deallocate(pdefs%data(dest)%xydata%y_err)
       call move_alloc(xyetmp, pdefs%data(dest)%xydata%y_err)
    end if
     
    pdefs%data(dest)%ndata = ndata

  end subroutine gr_ds_append

  subroutine gr_ds_erase(index, data_only)
    integer(kind=int16), intent(in), optional :: index
    logical, intent(in), optional :: data_only
    
    ! Erase the contents of a dataset

    integer(kind=int16) :: idx

    if (present(index)) then
       idx = index
    else
       idx = pdefs%cset
    end if

    call gr_pdefs_data_init(index=idx, minimal=data_only)

  end subroutine gr_ds_erase

  subroutine gr_ds_delete(index)
    integer(kind=int16), intent(in), optional :: index

    ! Delete a dataset.

    integer(kind=int16) :: idx, nset0
    type(graff_data), dimension(:), allocatable :: datasets
    integer(kind=int16) :: i, j
    integer :: nkey
    logical, dimension(:), allocatable :: iskey

    if (present(index)) then
       idx = index
    else
       idx = pdefs%cset
    end if

    nset0 = pdefs%nsets

    if (idx < 0 .or. idx > pdefs%nsets) return
    if (pdefs%nsets == 1) then
       call gr_ds_erase
       return
    end if

    allocate(datasets(pdefs%nsets-1))

    do i = 1, idx-1_int16
       call gr_ds_copy(i, destination=datasets(i), move=.true.)
    end do
    do i = idx+1_int16, pdefs%nsets
       call gr_ds_copy(i, destination=datasets(i-1), move=.true.)
    end do

    deallocate(pdefs%data)
    call move_alloc(datasets, pdefs%data)
    pdefs%nsets = pdefs%nsets - 1_int16
    if (pdefs%cset == idx) then
       pdefs%cset = max(pdefs%cset-1_int16, 1_int16)
       call gr_set_values_dataset()
    end if

    if (allocated(pdefs%key%list)) then
       allocate(iskey(nset0))
       iskey = .false.
       iskey(pdefs%key%list+1) = .true.

       if (iskey(idx)) then
          nkey = size(pdefs%key%list)-1
       else
          nkey = size(pdefs%key%list)
       end if

       deallocate(pdefs%key%list)
       if (nkey >= 1) then
          allocate(pdefs%key%list(nkey))
          j=1_int16
          do i = 1, idx-1_int16
             if (iskey(i)) then
                pdefs%key%list(j) = i-1
                j = j+1_int16
             end if
          end do
          do i = idx+1_int16, nset0
             if (iskey(i)) then
                pdefs%key%list(j) = i-2
                j = j+1_int16
             end if
          end do
       end if
    end if

  end subroutine gr_ds_delete

  subroutine gr_ds_move(index, after)
    integer(kind=int16), intent(in) :: index, after

    ! Reorder datasets

    type(graff_data) :: tmpdata
    integer(kind=int16) :: i, j
    logical, dimension(:), allocatable :: iskey
    logical :: tmpkey

    if (index <= 0 .or. index > pdefs%nsets) return
    if (after < 0 .or. after > pdefs%nsets) return
    if (after == index-1_int16 .or. after == index) return

    allocate(iskey(pdefs%nsets))
    iskey = .false.
    if (allocated(pdefs%key%list)) iskey(pdefs%key%list+1) = .true.

    call gr_ds_copy(index, destination=tmpdata, move=.true., &
         & no_housekeeping=.true.)
    tmpkey = iskey(index)

    if (after > index) then
       do i = index+1_int16, after
          call gr_ds_copy(i, to=i-1_int16, move=.true., &
               & no_housekeeping=.true.)
          iskey(i-1) = iskey(i)
       end do
       call gr_ds_copy(source=tmpdata, to=after, move=.true.)
       iskey(after) = tmpkey
    else
       do i = index-1_int16, after+1_int16, -1_int16
          call gr_ds_copy(i, to=i+1_int16, move=.true., &
               & no_housekeeping=.true.)
          iskey(i+1) = iskey(i)
       end do
       call gr_ds_copy(source=tmpdata, to=after+1_int16, move=.true.)
       iskey(after+1) = tmpkey
    end if

    if (any(iskey)) then
       j = 1_int16
       do i = 1_int16, pdefs%nsets
          if (.not. iskey(i)) cycle
          pdefs%key%list(j) = i-1_int16
          j = j + 1_int16
       end do
    end if

  end subroutine gr_ds_move

  subroutine gr_ds_transpose
    type(graff_data), pointer :: data
    real(kind=real64), allocatable, dimension(:,:) :: xyetmp
    real(kind=plflt), dimension(:,:), allocatable :: x,y,z
    real(kind=real64), allocatable, dimension(:) :: xytmp
    integer, dimension(2) :: sz
    logical(kind=int8) :: x2, y2
    integer :: nx, ny

    data => pdefs%data(pdefs%cset)

    if (data%type < 0) return

    if ( data%type == 9) then
       if (.not. allocated(data%zdata%z)) return
       sz = shape(data%zdata%z)
       allocate(z(sz(2),sz(1)))
       z = transpose(data%zdata%z)
       nx = sz(2)
       ny = sz(1)
       
       sz = shape(data%zdata%x)
       allocate(y(sz(2), sz(1)))
       y = transpose(data%zdata%x)
       y2 = sz(2) /= 1
       
       sz = shape(data%zdata%y)
       allocate(x(sz(2), sz(1)))
       x = transpose(data%zdata%y)
       x2 = sz(1) /= 1

       deallocate(data%zdata%x)
       deallocate(data%zdata%y)
       deallocate(data%zdata%z)
       
       data%ndata = nx
       data%ndata2 = ny
       data%zdata%x_is_2d = x2
       data%zdata%y_is_2d = y2
       
       call move_alloc(x, data%zdata%x)
       call move_alloc(y, data%zdata%y)
       call move_alloc(z, data%zdata%z)
    else
       call move_alloc(data%xydata%x, xytmp)
       call move_alloc(data%xydata%y, data%xydata%x)
       call move_alloc(xytmp, data%xydata%y)
       
       select case (data%type)
       case(0)              ! No error bars
       case(1,2)              ! Y errors, become X
          call move_alloc(data%xydata%y_err, data%xydata%x_err)
          data%type = data%type + 2_int16

       case(3, 4)              ! X errors, become Y
          call move_alloc(data%xydata%x_err, data%xydata%y_err)
          data%type = data%type - 2_int16

       case(5,8)              ! XY errors exchange
          call move_alloc(data%xydata%x_err, xyetmp)
          call move_alloc(data%xydata%y_err, data%xydata%x_err)
          call move_alloc(xyetmp, data%xydata%y_err)

       case(6)              ! XYY → XXY
          call move_alloc(data%xydata%x_err, xyetmp)
          call move_alloc(data%xydata%y_err, data%xydata%x_err)
          call move_alloc(xyetmp, data%xydata%y_err)
          data%type = 7

       case(7)              ! XXY → XYY
          call move_alloc(data%xydata%x_err, xyetmp)
          call move_alloc(data%xydata%y_err, data%xydata%x_err)
          call move_alloc(xyetmp, data%xydata%y_err)
          data%type = 6

       case default
          write(err_string, "(A,i0)") "gr_ds_transpose: Invalid type code: ", &
            & data%type
          call gr_message(err_string, type=GTK_MESSAGE_ERROR)
          return
       end select
    end if
    call gr_plot_draw(.true.)
  end subroutine gr_ds_transpose
  
end module gr_ds_tools
