program graffer
  use iso_c_binding

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_init, TRUE

  use graff_types
  use graff_globals
  use graff_version
  use gr_colours
  use gr_file

  use gr_gui
  use gr_opt_init
  use graff_init

  implicit none

  character(len=240) :: input
  character(len=240), dimension(:), allocatable :: flist

  integer(kind=c_int) :: ipick
  logical :: ok, isdir
  character(len=8) :: gr_sversion

  call graffer_version%set(4, 7)
  call graffer_version%string(gr_sversion)

  !! TEMPORARY
  !!  call gr_ct_init('/home/jtappin/idl/graffer-dev/f-graffer/data/c_tables')
  call gr_ct_init('/data/software/graffer-dev/f-graffer/data/c_tables')
  !!  call gr_ct_init('/home/james/Dev/Graffer/f-graffer/data/c_tables')

  ! Get the file to open/create and determine which.

  call gtk_init()

  call gr_parse_command(input, isdir)

  if (input == '') then
     ipick = hl_gtk_file_chooser_show(flist, filter=['*.grf'], &
          & filter_name=['Graffer Files'], all=TRUE, edit_filters=TRUE, &
          & current=TRUE)
     if (c_f_logical(ipick)) then
        input = flist(1)
     else
        stop
     end if
  else if (isdir) then
     ipick = hl_gtk_file_chooser_show(flist, filter=['*.grf'], &
          & filter_name=['Graffer Files'], all=TRUE, edit_filters=TRUE, &
          & initial_dir=trim(input)//c_null_char)
     if (c_f_logical(ipick)) then
        input = flist(1)
     else
        stop
     end if
  end if

  call gr_pdefs_init(pdefs)
  if (file_exists(input)) then 
     call gr_read(pdefs, input, ok)
     if (.not. ok) stop
  else
     call split_fname(input, pdefs%name, pdefs%dir)
  end if

  call gr_make_gui(gr_sversion, input)

end program graffer
