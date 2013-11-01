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

module gr_text_utils

  use iso_fortran_env

  use graff_types
  use graff_globals

  use plplot

  implicit none

  ! Font defintions

  ! Temporarily (5.9.10) the font definition constants are not.
  ! integer, parameter, dimension(*) :: font_list = [PL_FCI_SANS, PL_FCI_SERIF, &
  !      & PL_FCI_MONO, PL_FCI_SCRIPT, PL_FCI_SYMBOL]
  ! integer, parameter, dimension(*) :: font_weight = [PL_FCI_MEDIUM, &
  !      & PL_FCI_BOLD, PL_FCI_MEDIUM, PL_FCI_BOLD, PL_FCI_MEDIUM, PL_FCI_BOLD]
  ! integer, parameter, dimension(*) :: font_shape = [PL_FCI_UPRIGHT, &
  !      & PL_FCI_UPRIGHT, PL_FCI_ITALIC, PL_FCI_ITALIC, PL_FCI_OBLIQUE, &
  !      & PL_FCI_OBLIQUE]
  integer, protected, dimension(5) :: font_list
  integer, protected, dimension(6) :: font_weight
  integer, protected, dimension(6) :: font_shape
  logical, private :: text_is_init=.false.

  character(len=*), dimension(*), parameter :: font_names = ['Sans Serif',  &
       & 'Serif     ', 'Monospaced', 'Script    ', 'Symbol    ']
  character(len=*), dimension(*), parameter :: font_styles = &
       & ['Medium        ', 'Bold          ', 'Italic Medium ', &
       & 'Italic Bold   ', 'Oblique Medium', 'Oblique Bold  ']

contains
  subroutine gr_text_init

    if (text_is_init) return

    font_list = [PL_FCI_SANS, PL_FCI_SERIF, &
         & PL_FCI_MONO, PL_FCI_SCRIPT, PL_FCI_SYMBOL]
    font_weight = [PL_FCI_MEDIUM, &
         & PL_FCI_BOLD, PL_FCI_MEDIUM, PL_FCI_BOLD, PL_FCI_MEDIUM, PL_FCI_BOLD]
    font_shape = [PL_FCI_UPRIGHT, &
         & PL_FCI_UPRIGHT, PL_FCI_ITALIC, PL_FCI_ITALIC, PL_FCI_OBLIQUE, &
         & PL_FCI_OBLIQUE]
    text_is_init=.true.
  end subroutine gr_text_init

  subroutine gr_add_text(text)
    type(graff_text), intent(in) :: text

    ! Add a text annotation to a file

    type(graff_text), dimension(:), allocatable :: tmp_text

    if (pdefs%ntext == 0) then
       if (.not. allocated(pdefs%text)) allocate(pdefs%text(1)) 
       pdefs%text(1) = text
       pdefs%ntext = 1_int16
    else
       allocate(tmp_text(pdefs%ntext+1))
       tmp_text(:pdefs%ntext) = pdefs%text
       tmp_text(pdefs%ntext+1) = text
       deallocate(pdefs%text)
       call move_alloc(tmp_text, pdefs%text)
       pdefs%ntext = pdefs%ntext + 1_int16
    end if
  end subroutine gr_add_text

  subroutine gr_delete_text(index)
    integer, intent(in) :: index

    ! Delete a text annotation

    integer :: i, j
    type(graff_text), dimension(:), allocatable :: tmp_text

    if (index < 1 .or. index > pdefs%ntext) return

    if (pdefs%ntext == 1) then
       deallocate(pdefs%text)
       pdefs%ntext = 0_int16
    else
       allocate(tmp_text(pdefs%ntext-1))

       j = 1
       do i = 1, pdefs%ntext
          if (i == index) cycle
          tmp_text(j) = pdefs%text(i)
          j = j+1
       end do
       deallocate(pdefs%text)
       call move_alloc(tmp_text, pdefs%text)
       pdefs%ntext = pdefs%ntext - 1_int16
    end if
  end subroutine gr_delete_text
end module gr_text_utils
