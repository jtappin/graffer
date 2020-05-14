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

module graff_types
  ! Module for the data structures for the Fortran
  ! version of Graffer

  use iso_fortran_env
  use iso_c_binding, only: c_int
  use plplot, only: plflt

  implicit none

  ! Axis style settings
  type :: graff_style
     integer(kind=int16) :: idl=0_int16, extra=0_int16, &
          & grid=0_int16, time=0_int16
     integer(kind=int32) :: tzero=0_int32
     integer(kind=int16) :: minor=0_int16, major=0_int16
     character(len=40) :: format=''
     real(kind=real64), dimension(:), allocatable :: values
  end type graff_style

  type :: window_spec
     real(kind=plflt), dimension(4,2) :: world
     integer :: world_selected
     real(kind=plflt), dimension(4) :: viewport
     real(kind=plflt) :: vp_aspect
     logical :: viewport_enabled, is_initialized = .false.
  end type window_spec

  ! Data for a 2-D (z,y) dataset
  type :: graff_zdata
     real(kind=real64), dimension(:,:), allocatable :: x, y, z
     logical(kind=int8) :: x_is_2d, y_is_2d
     integer(kind=int16) :: format=0
     logical(kind=int8) :: set_levels=.false.
     integer(kind=int16) :: n_levels=0_int16, lmap=0_int16, &
          & n_cols=0_int16, &
          & n_sty=0_int16, n_thick=0_int16
     real(kind=real64), dimension(:), allocatable :: thick
     real(kind=real64), dimension(:), allocatable :: levels
     integer(kind=int16), dimension(:), allocatable :: style
     integer(kind=int16), dimension(:), allocatable :: colours
     integer(kind=int16), dimension(:,:), allocatable :: raw_colours
     real(kind=real64), dimension(2) :: range=0._real64
     real(kind=real64) :: missing=0._real64
     real(kind=real64) :: pxsize=0._real64, charsize=0._real64, gamma=0._real64
     integer(kind=int16) :: label=0_int16, label_off = 0_int16, &
          & ctable=0_int16
     integer(kind=int8) :: fill=0_int8
     integer(kind=int16) :: ilog=0_int16
     logical(kind=int8) ::  invert=.false., smooth=.false.
     integer(kind=int32) :: shade_levels
  end type graff_zdata

  ! Function specific stuff
  type :: graff_fdata
     real(kind=real64), dimension(2,2) :: range
     character(len=256), dimension(2) :: funct
     logical :: evaluated = .FALSE.
  end type graff_fdata

  ! Data for a general dataset
  type :: graff_data
     integer(kind=int32) :: ndata=0_int32, ndata2=0_int32
     integer(kind=int16) :: type=0_int16, mode=0_int16
     real(kind=real64), dimension(:,:), allocatable :: xydata
     type(graff_zdata) :: zdata
     type(graff_fdata) :: funct
     character(len=120) :: descript=''
     integer(kind=int16) :: pline=0_int16, psym=0_int16
     real(kind=real64) :: symsize=0._real64
     integer(kind=int16) :: line=0_int16, colour=0_int16
     integer(kind=int16), dimension(3) :: c_vals=0_int16
     real(kind=real64) :: thick=0._real64, min_val=0._real64, &
          & max_val=0._real64
     integer(kind=int16) :: y_axis=0_int16
     logical(kind=int8) :: sort=.false., noclip=.false., medit=.false.
  end type graff_data

  ! Text annotation
  type :: graff_text
     character(len=40) :: id=''
     character(len=256) :: text=''
     integer(kind=int16) :: colour=0_int16
     integer(kind=int16), dimension(3) :: c_vals = 0_int16
     real(kind=real64) :: size=0._real64, orient=0._real64, align=0._real64
     integer(kind=int16) :: ffamily=0_int16, font=0_int16
     real(kind=real64) :: thick=0._real64
     real(kind=real64) :: x=0._real64, y=0._real64
     integer(kind=int16) :: norm=0_int16, axis=0_int16
  end type graff_text

  ! A Key display
  type :: graff_key
     real(kind=real64), dimension(2) :: x=0._real64, y=0._real64
     real(kind=real64) :: csize=1._real64
     integer(kind=int16) :: norm=2_int16, cols=1_int16
     integer(kind=int32), dimension(:), allocatable :: list
     logical(kind=int8) :: frame=.false., one_point=.false., &
          & use=.false., side=.false.
     character(len=120) :: title=''
  end type graff_key

  ! Transient properties
  type :: graff_trans
     real(kind=real64), dimension(2) :: opos = 0._real64
     integer(kind=int32) :: imove = 0_int32
     integer(kind=int16) :: mode=0_int16, changes=0_int16, colmin=0_int16
     logical(kind=int8) :: hairs=.true., opflag=.false., &
          & backup=.false., current_only=.false.
     real(kind=real64), dimension(:), allocatable :: x_dev, y_dev
  end type graff_trans

  ! Hardcopy options
  type :: graff_hard
     logical(kind=int8) :: colour=.true., eps=.false., orient=.false., &
          & timestamp=.false., cmyk=.false.
     integer(kind=int8) :: psize=0_int8
     integer(kind=int16) :: font_family=1_int16, font_wg_sl=1_int16
     real(kind=real64), dimension(2) :: size=0._real64, off=0._real64
     character(len=120), dimension(2) :: action='', viewer='', &
          & pdfviewer=''
     character(len=120) :: name=''
     character(len=16) :: psdev='', epsdev='', pdfdev='', svgdev=''
  end type graff_hard

  ! General options
  type :: graff_opts
     real(kind=real32) :: auto_delay=300._real32
     logical(kind=int8) :: s2d=.false., mouse=.false., colour_menu=.false., &
          & delete_function_files=.false.
     character(len=120) :: pdfviewer=''
     character(len=150) :: gdl_command=''
     character(len=256) :: colour_dir=''
     character(len=80) :: colour_stem=''
     integer(kind=c_int), dimension(2) :: geometry = [600, 600]
  end type graff_opts

  ! Top level plot structure
  type :: graff_pdefs
     integer(kind=int16), dimension(2) :: version=0_int16
     character(len=120) :: name='', dir=''
     character(len=120) :: title='', subtitle=''
     real(kind=real64) :: charsize=0._real64, axthick=0._real64
     integer(kind=int16) :: fontopt = 0_int16
     real(kind=real64), dimension(4) :: position=0._real64
     real(kind=real64), dimension(2) :: aspect=0._real64
     logical(kind=int8) :: isotropic=.false., match=.false.
     logical(kind=int8) :: y_right=.false.
     real(kind=real64), dimension(2,3) :: axrange=0._real64
     character(len=120), dimension(3) :: axtitle=''
     integer(kind=int16), dimension(3) :: axtype=0_int16
     type(graff_style), dimension(3) :: axsty
     type(window_spec) :: transform
     integer(kind=int16) :: ctable=0_int16
     real(kind=real64) :: gamma=0._real64
     integer(kind=int16) :: nsets=0_int16, cset=0_int16
     type(graff_data), dimension(:), allocatable :: data
     integer(kind=int16) :: ntext=0_int16
     type(graff_text), dimension(:), allocatable :: text
     type(graff_text) :: text_options
     type(graff_key) :: key
     character(len=120), dimension(:), allocatable :: remarks
     !     type(graff_ids) :: ids
     type(graff_hard) :: hardset
     type(graff_opts) :: opts
     character(len=120) :: ds_dir=''
     logical(kind=int8) :: chflag=.false., short_colour=.false., &
          & is_ascii=.false.
     type(graff_trans) :: transient
  end type graff_pdefs

  character(len=6), dimension(-4:9), parameter :: typecodes = &
       & ['f(x,y)', 'f(t)  ', 'f(y)  ', 'f(x)  ', 'XY    ', 'XYy   ', &
       & 'XYyy  ', 'XYx   ', 'XYxx  ', 'XYxy  ', 'XYxyy ', 'XYxxy ', &
       & 'XYxxyy', 'Z     ']

end module graff_types
