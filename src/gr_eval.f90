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

module gr_eval
  use iso_fortran_env
  use iso_c_binding

  use gtk_sup
  use gtk_hl

  use g, only: g_find_program_in_path

  use graff_globals
  use gr_msg

  implicit none

  logical :: is_init=.false.
  logical :: gdl_found=.false.
  character(len=100) :: gdl_command

contains

  function gr_have_gdl()
    logical :: gr_have_gdl

    ! Find 'gdl' or 'idl' command

    integer :: status
    character(len=150) :: gdl_default_command
    type(c_ptr) :: executable

    if (is_init) then
       gr_have_gdl = gdl_found
       return
    end if

    call get_environment_variable("GRAFFER_GDL", &
         & value=gdl_default_command, status=status)
    if (status == 0) then
       executable = &
            & g_find_program_in_path(trim(gdl_default_command)//c_null_char)
       if (c_associated(executable)) then
          call c_f_string(executable, gdl_command)
          write(error_unit, "(2a)") "Found ${GRAFFER_GDL} at: ", &
               & trim(gdl_command)
          is_init = .true.
          gdl_found = .true.
          gr_have_gdl = .true.
          return
       end if
    end if

    executable = g_find_program_in_path("gdl"//c_null_char)
    if (c_associated(executable)) then
       call c_f_string(executable, gdl_command)
       write(error_unit, "(2a)") "Found 'gdl' at: ", trim(gdl_command)
       is_init = .true.
       gdl_found = .true.
       gr_have_gdl = .true.
       return
    end if

    executable = g_find_program_in_path("idl"//c_null_char)
    if (c_associated(executable)) then
       call c_f_string(executable, gdl_command)
       write(error_unit, "(2a)") "Found 'idl' at: ", trim(gdl_command)
       is_init = .true.
       gdl_found = .true.
       gr_have_gdl = .true.
       return
    end if

    write(error_unit, "(A)") "Failed to find 'gdl' or 'idl' in ${PATH}"
    call gr_message("Failed to find 'gdl' or 'idl' in ${PATH}")

    gdl_found = .false.
    gr_have_gdl =.false.
    is_init = .true.
    gdl_command = ''
  end function gr_have_gdl

  function gr_evaluate(dsidx, dataset)
    integer :: gr_evaluate
    integer, intent(in), optional :: dsidx
    type(graff_data), optional, intent(inout), target :: dataset

    ! Evalutate a function and store the evaluations.

    real(kind=real64), dimension(:), allocatable :: x, y
    real(kind=real64), dimension(2) :: frange
    real(kind=real64), dimension(:,:), allocatable :: z
    real(kind=real64), dimension(2) :: xr, yr
    integer :: status
    type(graff_data), pointer :: data
    character(len=120) :: err_buffer

    if (present(dataset)) then
       data => dataset
    else if (present(dsidx)) then
       data => pdefs%data(dsidx)
    else
       data => pdefs%data(pdefs%cset)
    end if

    if (data%funct%evaluated) then
       gr_evaluate = 0
       return
    end if

    select case(data%type)
    case(-1)
       frange = data%funct%range(:,1)
       if (frange(1) == frange(2)) frange = pdefs%axrange(:,1)
       call gr_eval_fx(data%funct%funct(1), &
            & data%ndata, frange, &
            & x, y, dsidx, status)
    case(-2)
       frange = data%funct%range(:,1)
       if (frange(1) == frange(2)) then
          if (pdefs%y_right .and. data%y_axis == 1) then
             frange = pdefs%axrange(:,3)
          else
             frange = pdefs%axrange(:,2)
          end if
       end if
       call gr_eval_fy(data%funct%funct(1), &
            & data%ndata, frange, &
            & x, y, dsidx, status)
    case(-3)
       frange = data%funct%range(:,1)
       if (frange(1) == frange(2)) frange = [0._real64, 1._real64]
       call gr_eval_ft(data%funct%funct, &
            & data%ndata, frange, &
            & x, y, dsidx, status)
    case(-4)
       xr = data%funct%range(:,1)
       if (xr(1) == xr(2)) xr = pdefs%axrange(:,1)
       yr = data%funct%range(:,2)
       if (yr(1) == yr(2)) then
          if (pdefs%y_right .and. data%y_axis == 1) then
             yr = pdefs%axrange(:,3)
          else
             yr = pdefs%axrange(:,2)
          end if
       end if
       call gr_eval_fz(data%funct%funct(1), &
            & data%ndata, data%ndata2, &
            & xr, yr, x, y, z, dsidx, status)

    end select

    gr_evaluate = status

    if (status /= 0) then
       write(err_buffer, "(a,i0)") &
            & "gr_evaluate: Failed to evaluate function for dataset #", &
            & dsidx
       call gr_message(err_buffer)
       return
    end if

    if (allocated(data%xydata)) &
         & deallocate(data%xydata)
    if (allocated(data%zdata%x)) &
         & deallocate(data%zdata%x)
    if (allocated(data%zdata%y)) &
         & deallocate(data%zdata%y)
    if (allocated(data%zdata%y)) &
         & deallocate(data%zdata%y)

    if (data%type == -4) then
       allocate(data%zdata%x(size(x),1))
       allocate(data%zdata%y(1, size(y)))

       data%zdata%x(:,1) = x
       data%zdata%y(1,:) = y
       data%zdata%x_is_2d = .false.
       data%zdata%y_is_2d = .false.

       call move_alloc(z, data%zdata%z)
    else
       allocate(data%xydata(2, size(x)))

       data%xydata(1,:) = x
       data%xydata(2,:) = y
    end if

    data%funct%evaluated = .true.

  end function gr_evaluate

  subroutine gr_eval_fx(fun, n, xr, x, y, dsidx, status)
    character(len=*), intent(in) :: fun
    integer(kind=int32), intent(in) :: n
    real(kind=real64), dimension(2), intent(in) :: xr
    real(kind=real64), dimension(:), allocatable, intent(out) :: x, y
    integer, intent(in) :: dsidx
    integer, intent(out) :: status

    ! Evaluate a function y = f(x)

    integer :: cstatus, ios
    integer :: punit, dunit
    character(len=150) :: pfile, dfile

    if (.not. gr_have_gdl()) then
       status=-1
       return
    end if

    call gr_make_gdl_names(dsidx, pfile, dfile)

    open(newunit=punit, file=pfile, form='formatted', action='write')
    write(punit, "(a,g0,a,i0,a,g0,a,i0,a)") 'x = ',xr(1), &
         & '+dindgen(',n,')*',xr(2)-xr(1), &
         & '/double(',n-1,')'
    write(punit, "(2a)") 'y = ', trim(fun)
    write(punit, "(3a)") 'openw, 1, "', trim(dfile), '"'
    write(punit, "(a)") 'writeu, 1, x, y'
    write(punit, "(a)") 'close, 1'
    write(punit, "(a)") 'exit'
    close(punit)

    call execute_command_line(trim(gdl_command)//" "//pfile//&
         & ' > /dev/null 2> /dev/null', &
         & exitstat=status, cmdstat=cstatus)
    if (status /= 0) return
    if (cstatus /= 0) then
       status=-cstatus
       return
    end if
    allocate(x(n),y(n))
    open(newunit=dunit, file=dfile, form='unformatted', action='read', &
         & access='stream', iostat=ios)
    if (ios /= 0) then
       status = ios
       return
    end if
    read(dunit, iostat=ios) x, y
    if (ios /= 0) status = ios

    close(dunit)

    if (pdefs%opts%delete_function_files) &
         & call execute_command_line("rm "//trim(pfile)//' '//trim(dfile))

  end subroutine gr_eval_fx

  subroutine gr_eval_fy(fun, n, yr, x, y, dsidx, status)
    character(len=*), intent(in) :: fun
    integer(kind=int32), intent(in) :: n
    real(kind=real64), dimension(2), intent(in) :: yr
    real(kind=real64), dimension(:), allocatable, intent(out) :: x, y
    integer, intent(in) :: dsidx
    integer, intent(out) :: status

    ! Evaluate a function x = f(y)

    integer :: cstatus, ios
    integer :: punit, dunit
    character(len=150) :: pfile, dfile

    if (.not. gr_have_gdl()) then
       status=-1
       return
    end if

    call gr_make_gdl_names(dsidx, pfile, dfile)

    open(newunit=punit, file=pfile, form='formatted', action='write')
    write(punit, "(a,g0,a,i0,a,g0,a,i0,a)") 'y = ',yr(1), &
         & '+dindgen(',n,')*',yr(2)-yr(1), &
         & '/double(',n-1,')'
    write(punit, "(2a)") 'x = ', trim(fun)
    write(punit, "(3a)") 'openw, 1, "', trim(dfile), '"'
    write(punit, "(a)") 'writeu, 1, x, y'
    write(punit, "(a)") 'close, 1'
    write(punit, "(a)") 'exit'
    close(punit)

    call execute_command_line(trim(gdl_command)//" "//trim(pfile)//&
         & ' > /dev/null 2>/dev/null', &
         & exitstat=status, cmdstat=cstatus)
    if (status /= 0) return
    if (cstatus /= 0) then
       status = -cstatus
       return
    end if

    allocate(x(n),y(n))
    open(newunit=dunit, file=dfile, form='unformatted', action='read', &
         & access='stream', iostat=ios)
    if (ios /= 0) then
       status=ios
       return
    end if

    read(dunit, iostat=ios) x, y
    if (ios /= 0) status=ios

    close(dunit)

    if (pdefs%opts%delete_function_files) &
         & call execute_command_line("rm "//trim(pfile)//' '//trim(dfile))

  end subroutine gr_eval_fy

  subroutine gr_eval_ft(fun, n, tr, x, y, dsidx, status)
    character(len=*), intent(in), dimension(2) :: fun
    integer(kind=int32), intent(in) :: n
    real(kind=real64), dimension(2), intent(in) :: tr
    real(kind=real64), dimension(:), allocatable, intent(out) :: x, y
    integer, intent(in) :: dsidx
    integer, intent(out) :: status

    ! Evaluate a function x = f(t), y = g(t)

    integer :: cstatus, ios
    integer :: punit, dunit
    character(len=150) :: pfile, dfile

    if (.not. gr_have_gdl()) then
       status=-1
       return
    end if

    call gr_make_gdl_names(dsidx, pfile, dfile)

    open(newunit=punit, file=pfile, form='formatted', action='write')
    write(punit, "(a,g0,a,i0,a,g0,a,i0,a)") 't = ',tr(1), &
         & '+dindgen(',n,')*',tr(2)-tr(1), &
         & '/double(',n-1,')'
    write(punit, "(2a)") 'x = ', trim(fun(1))
    write(punit, "(2a)") 'y = ', trim(fun(2))
    write(punit, "(3a)") 'openw, 1, "', trim(dfile), '"'
    write(punit, "(a)") 'writeu, 1, x, y'
    write(punit, "(a)") 'close, 1'
    write(punit, "(a)") 'exit'
    close(punit)

    call execute_command_line(trim(gdl_command)//" "//trim(pfile)//&
         & ' > /dev/null 2> /dev/null', &
         & exitstat=status, cmdstat=cstatus)
    if (status /= 0) return
    if (cstatus /= 0) then
       status = -cstatus
       return
    end if

    allocate(x(n),y(n))
    open(newunit=dunit, file=dfile, form='unformatted', action='read', &
         & access='stream', iostat=ios)
    if (ios /= 0) then
       status = ios
       return
    end if

    read(dunit, iostat=ios) x, y
    if (ios /= 0) status=ios
    close(dunit)

    if (pdefs%opts%delete_function_files) &
         & call execute_command_line("rm "//trim(pfile)//' '//trim(dfile))

  end subroutine gr_eval_ft

  subroutine gr_eval_fz(fun, nx, ny, xr, yr, x, y, z, dsidx, status)
    character(len=*), intent(in) :: fun
    integer(kind=int32), intent(in) :: nx, ny
    real(kind=real64), dimension(2), intent(in) :: xr, yr
    real(kind=real64), dimension(:), allocatable, intent(out) :: x, y
    real(kind=real64), dimension(:,:), allocatable, intent(out) :: z
    integer, intent(in) :: dsidx
    integer, intent(out) :: status

    ! Evaluate a function z = f(x,y)

    integer :: cstatus, ios
    integer :: punit, dunit
    character(len=150) :: pfile, dfile

    if (.not. gr_have_gdl()) then
       status=-1
       return
    end if

    call gr_make_gdl_names(dsidx, pfile, dfile)

    open(newunit=punit, file=pfile, form='formatted', action='write')
    write(punit, "(a,g0,a,i0,a,g0,a,i0,a)") 'x1 = ',xr(1), &
         & '+dindgen(',nx,')*',xr(2)-xr(1), &
         & '/double(',nx-1,')'
    write(punit, "(a,i0,a)") 'x = x1[*,intarr(',ny,')]'
    write(punit, "(a,g0,a,i0,a,g0,a,i0,a)") 'y1 = ',yr(1), &
         & '+dindgen(1,',ny,')*',yr(2)-yr(1), &
         & '/double(',ny-1,')'
    write(punit, "(a,i0,a)") 'y = y1[intarr(',nx,'),*]'
    write(punit, "(2a)") 'z = ', trim(fun)
    write(punit, "(3a)") 'openw, 1, "', trim(dfile), '"'
    write(punit, "(a)") 'writeu, 1, x1, y1, z'
    write(punit, "(a)") 'close, 1'
    write(punit, "(a)") 'exit'
    close(punit)

    call execute_command_line(trim(gdl_command)//" "//trim(pfile)//&
         & ' > /dev/null 2>/dev/null', &
         & exitstat=status, cmdstat=cstatus)
    if (status /= 0) return
    if (cstatus /= 0) then
       status = -cstatus
       return
    end if

    allocate(x(nx),y(ny),z(nx,ny))
    open(newunit=dunit, file=dfile, form='unformatted', action='read', &
         & access='stream', iostat=ios)
    if (ios /= 0) then
       status=ios
       return
    end if

    read(dunit, iostat=ios) x, y, z
    if (ios /= 0) status=ios

    close(dunit)

    if (pdefs%opts%delete_function_files) &
         & call execute_command_line("rm "//trim(pfile)//' '//trim(dfile))

  end subroutine gr_eval_fz

  subroutine gr_make_gdl_names(dsidx, pfile, dfile)
    integer, intent(in) :: dsidx
    character(len=*), intent(out) :: pfile, dfile

    ! Generate the names for the program and its output.

    integer :: idx

    pfile = pdefs%name
    dfile = pdefs%name

    idx = index(pfile, '.', back=.true.)
    if (idx == 0) idx = len_trim(pfile)+1

    write(pfile(idx:), "('_',i0,'.pro')") dsidx-1
    write(dfile(idx:), "('_',i0,'.fs')") dsidx-1

  end subroutine gr_make_gdl_names
end module gr_eval
