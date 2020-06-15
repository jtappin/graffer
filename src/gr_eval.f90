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

module gr_eval
  ! Function evaluation.

  use iso_fortran_env
  use iso_c_binding

  use gtk_sup
  use gtk_hl

  use gtk, only: GTK_MESSAGE_ERROR, GTK_MESSAGE_INFO
  
  use graff_globals
  use gr_msg
  use gr_utils

  implicit none

  logical, private :: is_init=.false.
  logical, private :: gdl_found=.false.
  character(len=150), private :: gdl_command = ''

  private :: gr_eval_fx, gr_eval_fy, gr_eval_ft, gr_eval_fz
  
contains
  function gr_get_gdl_command()
    character(len=len(gdl_command)) :: gr_get_gdl_command
    
    logical :: status
    
    if (.not. is_init) status = gr_have_gdl()
    
    gr_get_gdl_command = trim(gdl_command)
    
  end function gr_get_gdl_command
  
  function gr_have_gdl(command)
    logical :: gr_have_gdl
    character(len=*), intent(in), optional :: command
    ! Find 'gdl' or 'idl' command

    integer :: status
    character(len=150) :: gdl_default_command
    logical :: ok
    
    if (present(command)) then
       ok = gr_find_program(command)
       if (ok) then
          gdl_command = command
          is_init = .true.
       end if
       gr_have_gdl = ok
       return
    end if

    if (is_init) then
       gr_have_gdl = gdl_found
       return
    end if

   
    if (gdl_command /= '') then
       if (gr_find_program(gdl_command)) then
          call gr_message("Found gdl/idl at: "// &
               & trim(gdl_command), type=GTK_MESSAGE_INFO)
          is_init = .true.
          gdl_found = .true.
          gr_have_gdl = .true.
          return
       end if
    end if

    call get_environment_variable("GRAFFER_GDL", &
         & value=gdl_default_command, status=status)
    if (status == 0) then
       if (gr_find_program(gdl_default_command, path=gdl_command)) then
          call gr_message("Found ${GRAFFER_GDL} at: "// &
               & trim(gdl_command), type=GTK_MESSAGE_INFO)
          is_init = .true.
          gdl_found = .true.
          gr_have_gdl = .true.
          return
       end if
    end if

    if (gr_find_program("gdl", path=gdl_command)) then
       call gr_message("Found 'gdl' at: "// trim(gdl_command), &
            & type=GTK_MESSAGE_INFO)
       is_init = .true.
       gdl_found = .true.
       gr_have_gdl = .true.
       return
    end if

    if (gr_find_program("idl", path=gdl_command)) then
       call gr_message("Found 'idl' at: "//trim(gdl_command), &
            & type=GTK_MESSAGE_INFO)
       is_init = .true.
       gdl_found = .true.
       gr_have_gdl = .true.
       return
    end if

    call gr_message("Failed to find 'gdl' or 'idl' in ${PATH}")

    gdl_found = .false.
    gr_have_gdl =.false.
    is_init = .true.
    gdl_command = ''
  end function gr_have_gdl

  function gr_evaluate(dsidx)
    integer :: gr_evaluate
    integer(kind=int16), intent(in) :: dsidx

    ! Evalutate a function and store the evaluations.

    real(kind=real64), dimension(:), allocatable :: x, y
    real(kind=real64), dimension(2) :: frange
    real(kind=real64), dimension(:,:), allocatable :: z
    real(kind=real64), dimension(2) :: xr, yr
    integer :: status
    type(graff_data), pointer :: data
    character(len=120) :: err_buffer
    logical :: y_is_log
    
    data => pdefs%data(dsidx)

    if (data%funct%evaluated) then
       gr_evaluate = 0
       return
    end if

    select case(data%type)
    case(-1)
       frange = data%funct%range(:,1)
       if (frange(1) == frange(2)) frange = pdefs%axrange(:,1)
       call gr_eval_fx(data%funct%funct(1), &
            & data%ndata, frange, pdefs%axtype(1) == 1, dsidx, &
            & x, y, status)
       
    case(-2)
       frange = data%funct%range(:,1)

       if (pdefs%y_right .and. data%y_axis == 1) then
          if (frange(1) == frange(2)) frange = pdefs%axrange(:,3)
          y_is_log = pdefs%axtype(3) == 1
       else
          if (frange(1) == frange(2)) frange = pdefs%axrange(:,2)
          y_is_log = pdefs%axtype(2) == 1
       end if

       call gr_eval_fy(data%funct%funct(1), &
            & data%ndata, frange, y_is_log, dsidx, &
            & x, y, status)
       
    case(-3)
       frange = data%funct%range(:,1)
       if (frange(1) == frange(2)) frange = [0._real64, 1._real64]
       call gr_eval_ft(data%funct%funct, &
            & data%ndata, frange, &
            & dsidx, x, y, status)
       
    case(-4)
       xr = data%funct%range(:,1)
       if (xr(1) == xr(2)) xr = pdefs%axrange(:,1)
       
       yr = data%funct%range(:,2)
       if (pdefs%y_right .and. data%y_axis == 1) then
          if (yr(1) == yr(2)) yr = pdefs%axrange(:,3)
          y_is_log = pdefs%axtype(3) == 1
       else
          if (yr(1) == yr(2))  yr = pdefs%axrange(:,2)
          y_is_log = pdefs%axtype(2) == 1
       end if

       call gr_eval_fz(data%funct%funct(1), &
            & data%ndata, data%ndata2, &
            & xr, yr, pdefs%axtype(1) == 1, y_is_log, &
            & dsidx, x, y, z, status)

    end select

    gr_evaluate = status

    if (status /= 0) then
       write(err_buffer, "(a,i0)") &
            & "gr_evaluate: Failed to evaluate function for dataset #", &
            & dsidx
       call gr_message(err_buffer, type=GTK_MESSAGE_ERROR)
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

  subroutine gr_eval_fx(fun, n, xr, x_is_log, dsidx, x, y, status)
    character(len=*), intent(in) :: fun
    integer(kind=int32), intent(in) :: n
    real(kind=real64), dimension(2), intent(in) :: xr
    logical, intent(in) :: x_is_log
    integer(kind=int16), intent(in) :: dsidx
    real(kind=real64), dimension(:), allocatable, intent(out) :: x, y
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

    ! For log axes, the evaluation points should be uniformly distributed
    ! in log(x)
    
    if (x_is_log) then
       write(punit, "(a,g0,a,i0,a,g0,a,i0,a)") 'x = ',xr(1), &
            & '* exp(dindgen(',n,')*alog(',xr(2)/xr(1), &
            & ')/double(',n-1,'))'
    else
       write(punit, "(a,g0,a,i0,a,g0,a,i0,a)") 'x = ',xr(1), &
            & '+dindgen(',n,')*(',xr(2)-xr(1), &
            & ')/double(',n-1,')'
    end if
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

    if (sysopts%delete_function_files) &
         & call execute_command_line("rm "//trim(pfile)//' '//trim(dfile))

  end subroutine gr_eval_fx

  subroutine gr_eval_fy(fun, n, yr, y_is_log, dsidx, x, y, status)
    character(len=*), intent(in) :: fun
    integer(kind=int32), intent(in) :: n
    real(kind=real64), dimension(2), intent(in) :: yr
    logical, intent(in) :: y_is_log
    integer(kind=int16), intent(in) :: dsidx
    real(kind=real64), dimension(:), allocatable, intent(out) :: x, y
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
    
    ! For log axes, the evaluation points should be uniformly distributed
    ! in log(y)
    
    if (y_is_log) then
       write(punit, "(a,g0,a,i0,a,g0,a,i0,a)") 'y = ',yr(1), &
            & '* exp(dindgen(',n,')*alog(',yr(2)/yr(1), &
            & ')/double(',n-1,'))'
    else
       write(punit, "(a,g0,a,i0,a,g0,a,i0,a)") 'y = ',yr(1), &
            & '+dindgen(',n,')*(',yr(2)-yr(1), &
            & ')/double(',n-1,')'
    end if
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

    if (sysopts%delete_function_files) &
         & call execute_command_line("rm "//trim(pfile)//' '//trim(dfile))

  end subroutine gr_eval_fy

  subroutine gr_eval_ft(fun, n, tr, dsidx, x, y, status)
    character(len=*), intent(in), dimension(2) :: fun
    integer(kind=int32), intent(in) :: n
    real(kind=real64), dimension(2), intent(in) :: tr
    integer(kind=int16), intent(in) :: dsidx
    real(kind=real64), dimension(:), allocatable, intent(out) :: x, y
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
         & '+dindgen(',n,')*(',tr(2)-tr(1), &
         & ')/double(',n-1,')'
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

    if (sysopts%delete_function_files) &
         & call execute_command_line("rm "//trim(pfile)//' '//trim(dfile))

  end subroutine gr_eval_ft

  subroutine gr_eval_fz(fun, nx, ny, xr, yr, x_is_log, y_is_log, dsidx, &
       & x, y, z, status)
    character(len=*), intent(in) :: fun
    integer(kind=int32), intent(in) :: nx, ny
    real(kind=real64), dimension(2), intent(in) :: xr, yr
    logical, intent(in) :: x_is_log, y_is_log
    integer(kind=int16), intent(in) :: dsidx
    real(kind=real64), dimension(:), allocatable, intent(out) :: x, y
    real(kind=real64), dimension(:,:), allocatable, intent(out) :: z
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

    ! For log axes, the evaluation points should be uniformly distributed
    ! in log(x)

    if (x_is_log) then
       write(punit, "(a,g0,a,i0,a,g0,a,i0,a)") 'x1 = ',xr(1), &
            & '* exp(dindgen(',nx,')*alog(',xr(2)/xr(1), &
            & ')/double(',nx-1,'))'
    else
       write(punit, "(a,g0,a,i0,a,g0,a,i0,a)") 'x1 = ',xr(1), &
            & '+dindgen(',nx,')*(',xr(2)-xr(1), &
            & ')/double(',nx-1,')'
    end if
    write(punit, "(a,i0,a)") 'x = x1[*,intarr(',ny,')]'

    ! For log axes, the evaluation points should be uniformly
    ! distributed in log(y)

    if (y_is_log) then
       write(punit, "(a,g0,a,i0,a,g0,a,i0,a)") 'y1 = ',yr(1), &
            & '* exp(dindgen(1,',ny,')*alog(',yr(2)/yr(1), &
            & ')/double(',ny-1,'))'
    else
       write(punit, "(a,g0,a,i0,a,g0,a,i0,a)") 'y1 = ',yr(1), &
            & '+dindgen(1,',ny,')*(',yr(2)-yr(1), &
            & ')/double(',ny-1,')'
    end if
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

    if (sysopts%delete_function_files) &
         & call execute_command_line("rm "//trim(pfile)//' '//trim(dfile))

  end subroutine gr_eval_fz

  subroutine gr_make_gdl_names(dsidx, pfile, dfile, ostem)
    integer(kind=int16), intent(in) :: dsidx
    character(len=*), intent(out) :: pfile, dfile
    character(len=*), intent(out), optional :: ostem

    ! Generate the names for the program and its output.

    integer :: idx

    pfile = pdefs%name
    dfile = pdefs%name

    idx = index(pfile, '.', back=.true.)
    if (idx == 0) idx = len_trim(pfile)+1

    write(pfile(idx:), "('_',i0,'.pro')") dsidx-1
    write(dfile(idx:), "('_',i0,'.fs')") dsidx-1
    if (present(ostem)) then
       ostem(:idx-1) = pdefs%name(:idx-1)
       write(ostem(idx:), "('_',i0)") dsidx-1
    end if
    
  end subroutine gr_make_gdl_names
end module gr_eval
