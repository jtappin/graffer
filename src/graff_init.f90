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

module graff_init
  ! Set initial default values in a graffer data structure.

  use iso_fortran_env

  use graff_types
  use graff_version
  use graff_globals
  use gr_opt_init

  implicit none

contains
  subroutine gr_pdefs_init

    ! Initialize a Graffer data structure

    integer :: i

    pdefs%name = ''
    pdefs%dir = ''
    pdefs%title = ''
    pdefs%subtitle = ''
    pdefs%charsize = 1._real32
    pdefs%axthick = 1._real32
    pdefs%position = 0._real32
    pdefs%aspect = 0._real32
    pdefs%isotropic = .false.
    pdefs%match = .false.
    pdefs%axrange(1,:) = 0._real64
    pdefs%axrange(2,:) = 1._real64
    pdefs%axtitle = ''
    pdefs%axtype = 0_int16

    do i = 1, 3
       pdefs%axsty(i)%idl = 0_int16
       pdefs%axsty(i)%extra = 0_int16
       pdefs%axsty(i)%grid = 0_int16
       pdefs%axsty(i)%time = 0_int16
       pdefs%axsty(i)%tzero = 0_int32
       pdefs%axsty(i)%minor = 0_int16
       pdefs%axsty(i)%major = 0_int16
       pdefs%axsty(i)%xmajor = 0._real64
       pdefs%axsty(i)%format = ""
    end do

    pdefs%ctable = 0_int16
    pdefs%gamma = 1._real32
    pdefs%nsets = 1_int16
    pdefs%cset = 1_int16
    pdefs%ntext = 0_int16

    pdefs%chflag = .false.
    pdefs%short_colour = .false.
    pdefs%is_ascii = .false.

    if (allocated(pdefs%data)) deallocate(pdefs%data)
    allocate(pdefs%data(1))
    call gr_pdefs_data_init_all
    if (allocated(pdefs%text)) deallocate(pdefs%text)
    if (allocated(pdefs%remarks)) deallocate(pdefs%remarks)

    call gr_pdefs_text_init(pdefs%text_options)

    pdefs%transient%current_only = .false.
    if (allocated(pdefs%transient%x_dev)) deallocate(pdefs%transient%x_dev)
    if (allocated(pdefs%transient%y_dev)) deallocate(pdefs%transient%y_dev)

    ! Hardcopy defaults (note that not all of these are relevant to
    ! plplot's way of doing hardcopy).

    pdefs%hardset%colour = .true.
    pdefs%hardset%eps = .false.
    pdefs%hardset%orient = .true.
    pdefs%hardset%psize = 0_int8
    pdefs%hardset%timestamp = .false.
    pdefs%hardset%cmyk = .false.
    pdefs%hardset%font_family = 1_int16
    pdefs%hardset%font_wg_sl = 1_int16

    pdefs%hardset%size = [23., 18.]
    pdefs%hardset%off = [3.35, 1.5]
    
    pdefs%hardset%action = ''
    pdefs%hardset%viewer = ''
    pdefs%hardset%name = ''

    pdefs%hardset%psdev = ''
    pdefs%hardset%epsdev = ''
    pdefs%hardset%pdfdev = ''
    pdefs%hardset%svgdev = '' 

    pdefs%opts = default_options

    pdefs%key%x = 0._real64
    pdefs%key%y = 0._real64

    pdefs%key%csize = 1._real64
    pdefs%key%norm = 2_int16
    pdefs%key%cols= 1_int16

    if (allocated(pdefs%key%list)) deallocate(pdefs%key%list)
    pdefs%key%frame = .false.
    pdefs%key%one_point = .false.
    pdefs%key%use = .false.
    pdefs%key%side = .false.
    pdefs%key%title = ''

    pdefs%transform%is_initialized = .false.
    
  end subroutine gr_pdefs_init

  subroutine gr_pdefs_data_init_all

    ! Initialize all datasets in a Graffer structure.

    integer(kind=int16) :: i

    do i = 1_int16, pdefs%nsets
       call gr_pdefs_data_init(index=i)
    end do
  end subroutine gr_pdefs_data_init_all


  subroutine gr_pdefs_data_init(dataset, index)
    type(graff_data), intent(inout), optional, target :: dataset
    integer(kind=int16), intent(in), optional :: index

    ! Initialize a Graffer dataset.

    type(graff_data), pointer :: data

    if (present(dataset)) then
       data => dataset
    else if (present(index)) then
       data => pdefs%data(index)
    else
       write(error_unit, "(A)") &
            & "gr_pdefs_data_init: Must specify dataset or index"
       return
    end if

    data%ndata = 0_int32
    data%ndata2 = 0_int32
    data%type = 0_int16
    data%mode = 0_int16
    data%descript = ''
    data%pline = 1_int16
    data%psym = 0_int16
    data%symsize = 1._real32
    data%line = 0_int16
    data%colour = 1_int16
    data%thick = 1._real32
    data%y_axis = 0_int16
    data%sort = .false.
    data%noclip = .false.

    if (present(dataset)) then
       data%medit = .false.
       data%zdata%gamma = 1._real32
       data%zdata%ctable = 0_int16
    else
       data%medit = pdefs%opts%mouse
       data%zdata%gamma = pdefs%gamma
       data%zdata%ctable = pdefs%ctable
    end if

    data%zdata%format = 0
    data%zdata%set_levels = .false.
    data%zdata%n_levels = 6_int16
    data%zdata%n_cols = 0_int16
    data%zdata%n_sty = 0_int16
    data%zdata%n_thick = 0_int16

    data%zdata%range = 0._real64
    data%zdata%missing = 0._real64
    data%zdata%pxsize = 0.1_real32
    data%zdata%charsize = 1._real32
    data%zdata%label = 0_int16
    data%zdata%fill = 0_int8
    data%zdata%ilog = .false.
    data%zdata%invert = .false.
    data%zdata%smooth = .false.
    data%zdata%shade_levels = 256

    if (allocated(data%xydata)) deallocate(data%xydata)

    if (allocated(data%zdata%x)) deallocate(data%zdata%x)
    if (allocated(data%zdata%y)) deallocate(data%zdata%y)
    if (allocated(data%zdata%z)) deallocate(data%zdata%z)

    if (allocated(data%zdata%levels)) deallocate(data%zdata%levels)
    if (allocated(data%zdata%thick)) deallocate(data%zdata%thick)
    if (allocated(data%zdata%style)) deallocate(data%zdata%style)
    if (allocated(data%zdata%colours)) deallocate(data%zdata%colours)

    data%funct%range = 0._real64
    data%funct%funct = ''
    data%funct%evaluated = .false.

  end subroutine gr_pdefs_data_init

  subroutine gr_pdefs_text_init_all

    ! Initialize all text annotations in a Graffer structure.

    integer :: i

    do i = 1, pdefs%ntext
       call gr_pdefs_text_init(pdefs%text(i))
    end do
  end subroutine gr_pdefs_text_init_all


  subroutine gr_pdefs_text_init(text)
    type(graff_text), intent(inout) :: text

    ! Initialize a Graffer text annotation.

    text%id = ''
    text%text = ''
    text%colour = 1_int16
    text%size = 1._real32
    text%orient = 0._real32
    text%align = 0._real32

    text%ffamily = 1_int16
    text%font = 1_int16
    text%thick = 1._real32
    text%x = 0._real64
    text%y = 0._real64
    text%norm = 2_int16
    text%axis = 0_int16

  end subroutine gr_pdefs_text_init

  subroutine gr_set_changed(state, auto)
    logical, intent(in) :: state
    logical, intent(in), optional :: auto

    logical :: auto_only
    character(len=8) :: gr_sversion
    call graffer_version%string(gr_sversion)

    if (state) then
       pdefs%chflag = .true.
       pdefs%transient%changes = pdefs%transient%changes + 1_int16
       call gtk_window_set_title(gr_window, "Graffer V"//trim(gr_sversion)//&
            & ": "//trim(pdefs%dir)//trim(pdefs%name)//&
            & " (Modified)"//c_null_char)
    else
       if (present(auto)) then
          auto_only = auto
       else
          auto_only = .false.
       end if

       pdefs%transient%changes = 0_int16
       if (.not. auto_only) then
          pdefs%chflag = .false.
          call gtk_window_set_title(gr_window, "Graffer V"//trim(gr_sversion)//&
               & ": "//trim(pdefs%dir)//trim(pdefs%name)//c_null_char)
       end if
    end if

  end subroutine gr_set_changed
end module graff_init
