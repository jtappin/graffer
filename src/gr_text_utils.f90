module gr_text_utils

  use iso_fortran_env

  use graff_types
  use graff_globals

  use plplot

  implicit none

  integer, parameter, dimension(*) :: font_list = [PL_FCI_SANS, PL_FCI_SERIF, &
       & PL_FCI_MONO, PL_FCI_SCRIPT, PL_FCI_SYMBOL]
  integer, dimension(*), parameter :: font_weight = [PL_FCI_MEDIUM, &
       & PL_FCI_BOLD, PL_FCI_MEDIUM, PL_FCI_BOLD, PL_FCI_MEDIUM, PL_FCI_BOLD]
  integer, dimension(*), parameter :: font_shape = [PL_FCI_UPRIGHT, &
       & PL_FCI_UPRIGHT, PL_FCI_ITALIC, PL_FCI_ITALIC, PL_FCI_OBLIQUE, &
       & PL_FCI_OBLIQUE]

  character(len=*), dimension(*), parameter :: font_names = ['Sans Serif',  &
       & 'Serif     ', 'Monospaced', 'Script    ', 'Symbol    ']
  character(len=*), dimension(*), parameter :: font_styles = &
       & ['Medium        ', 'Bold          ', 'Italic Medium ', &
       & 'Italic Bold   ', 'Oblique Medium', 'Oblique Bold  ']

contains
  subroutine gr_add_text(text)
    type(graff_text), intent(in) :: text

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
