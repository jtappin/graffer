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

module gr_shading
  ! Module to present simplified shading interface. Formerly had
  ! version dependent APIs.
  ! Also provies a float32 interface to plwidth.

  use iso_fortran_env
  use gr_plot_utils

  use plplot, only: plwidth, plflt, plsdev, plshade, plshades

  interface gr_plshade
     module procedure gr_plshade1
     module procedure gr_plshade2
  end interface gr_plshade
  interface gr_plshades
     module procedure gr_plshades1
     module procedure gr_plshades2
  end interface gr_plshades

contains

  subroutine gr_pl_width(width)
    real(kind=real32), intent(in) :: width
    call plwidth(real(width, plflt))
  end subroutine gr_pl_width

  subroutine gr_plshade1(z, xmin, xmax, ymin, ymax, clevel, ccol, x1, y1)
    real(kind=plflt), intent(in), dimension(:,:) :: z
    real(kind=plflt), intent(in) :: xmin, xmax, ymin, ymax, clevel, ccol
    real(kind=plflt), intent(in), dimension(:) :: x1, y1

    call plshade(z, xmin, xmax, ymin, ymax, &
         & clevel, huge(0._plflt), 0, ccol, 1._plflt, &
         & 0, 0._plflt, 0, 0._plflt, .true., x1, y1)

  end subroutine gr_plshade1

  subroutine gr_plshade2(z, xmin, xmax, ymin, ymax, clevel, ccol, x2, y2)
    real(kind=plflt), intent(in), dimension(:,:) :: z
    real(kind=plflt), intent(in) :: xmin, xmax, ymin, ymax, clevel, ccol
    real(kind=plflt), intent(in), dimension(:,:) :: x2, y2

    call plshade(z, xmin, xmax, ymin, ymax, &
         & clevel, huge(0._plflt), 0, ccol, 1._plflt, &
         & 0, 0._plflt, 0, 0._plflt, .true., x2, y2)

  end subroutine gr_plshade2

  subroutine gr_plshades1(z, xmin, xmax, ymin, ymax, clevels, x1, y1)
    real(kind=plflt), intent(in), dimension(:,:) :: z
    real(kind=plflt), intent(in) :: xmin, xmax, ymin, ymax
    real(kind=plflt), intent(in), dimension(:) :: clevels, x1, y1

    call plshades(z, xmin, xmax, ymin, ymax, clevels, 0._plflt, &
            & 0, 0._plflt, .true., x1, y1)

  end subroutine gr_plshades1

  subroutine gr_plshades2(z, xmin, xmax, ymin, ymax, clevels, x2, y2)
    real(kind=plflt), intent(in), dimension(:,:) :: z
    real(kind=plflt), intent(in) :: xmin, xmax, ymin, ymax
    real(kind=plflt), intent(in), dimension(:) :: clevels
    real(kind=plflt), intent(in), dimension(:,:) :: x2, y2

    call plshades(z, xmin, xmax, ymin, ymax, clevels, 0._plflt, &
            & 0, 0._plflt, .true., x2, y2)

  end subroutine gr_plshades2

end module gr_shading
