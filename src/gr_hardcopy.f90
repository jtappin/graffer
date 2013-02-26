module gr_hardcopy
  use iso_c_binding
  use iso_fortran_env

  use gr_plot
  use graff_types
  use graff_globals

  implicit none

  type(c_ptr) :: hc_window

contains

  subroutine gr_hardcopy_config

    type(c_ptr) :: base, jb, junk
    type(graff_hard), pointer :: hopts
    logical, dimension(2), target :: iapply = [.false., .true.]

    hopts => pdefs%hardset

    hc_window = hl_gtk_window_new("Hardcopy configuration"//c_null_char, &
         & destroy=c_funloc(gr_hc_quit), data_destroy=c_loc(iapply(1)), &
         & parent = gr_window)

    
