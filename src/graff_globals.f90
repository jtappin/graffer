module graff_globals
  use iso_c_binding

  use graff_types
  use graff_version

  implicit none

  ! The main data structure

  type(graff_pdefs), target :: pdefs

  ! Widgets that may be needed outside the GUI-specifics

  type(c_ptr) :: gr_window = c_null_ptr		! The top level window
  type(c_ptr) :: gr_drawing_area = c_null_ptr	! The main plotting surface

end module graff_globals
