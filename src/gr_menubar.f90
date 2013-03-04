module gr_menubar
  use iso_c_binding
  use iso_fortran_env

  use gtk_hl
  use gtk_sup
  use gdk_pixbuf_hl
  use gtk_draw_hl

  use gtk, only: gtk_dialog_add_button, gtk_dialog_run, gtk_widget_destroy, &
       & gtk_widget_set_tooltip_text, gtk_widget_show_all, TRUE, &
       & GDK_SHIFT_MASK, GDK_CONTROL_MASK, GDK_MOD1_MASK, &
       & GTK_RESPONSE_DELETE_EVENT, GTK_RESPONSE_CANCEL, GTK_RESPONSE_YES, &
       & GTK_RESPONSE_NO, GTK_MESSAGE_QUESTION, GTK_PACK_DIRECTION_LTR, &
       & GTK_BUTTONS_NONE

  use graff_types
  use graff_globals
  use graff_version
  use gr_utils
  use gr_file
  use graff_init

  use gr_cb_common

  use gr_menu_widgets

  implicit none

contains
  function gr_menubar_new(accel) result(mbar)
    type(c_ptr) :: mbar
    type(c_ptr), intent(in) :: accel

    ! Create the Graffer menubar

    type(c_ptr) :: smnu, junk, jmnu
    logical, dimension(2), target :: use_current = [.true., .false.]
    character(len=3), dimension(3), target :: devices=['ps ','eps', 'pdf']
    character(len=4), dimension(3), target :: imtypes = ['png ', 'tiff', 'jpeg']
    character(len=3), dimension(4), target :: helptype = &
         & ['ug ', 'fmt', 'abt', 'gfa']

    mbar = hl_gtk_menu_new(GTK_PACK_DIRECTION_LTR)

    ! The file menu

    smnu = hl_gtk_menu_submenu_new(mbar, "File ▼"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Save"//c_null_char, &
         & activate=c_funloc(gr_save_cb), data=c_loc(use_current(1)), &
         & accel_key="s"//c_null_char, accel_group=accel, &
         & tooltip="Save the file to its current name"//c_null_char)
    junk = hl_gtk_menu_item_new(smnu, "Save as"//c_null_char, &
         & activate=c_funloc(gr_save_cb), data=c_loc(use_current(2)), &
         & accel_key="s"//c_null_char, accel_group=accel, &
         & accel_mods=ior(GDK_CONTROL_MASK, GDK_SHIFT_MASK), &
         & tooltip="Save the file to a new name"//c_null_char)
    junk = hl_gtk_menu_item_new(smnu, "Save as (ASCII)"//c_null_char, &
         & activate=c_funloc(gr_save_asc_cb), data=c_loc(use_current(2)), &
         & tooltip="Save the file to a new name, in ascii format."//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Open"//c_null_char, &
         & activate=c_funloc(gr_open_cb), data=c_loc(use_current(1)), &
         & accel_key="o"//c_null_char, accel_group=accel, &
         & tooltip="Open an existing file"//c_null_char)
    junk = hl_gtk_menu_item_new(smnu, "New"//c_null_char, &
         & activate=c_funloc(gr_open_cb), data=c_loc(use_current(2)), &
         & accel_key="n"//c_null_char, accel_group=accel, &
         & tooltip="Create a new file"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Quit"//c_null_char, &
         & activate=c_funloc(gr_exit), &
         & accel_key="q"//c_null_char, accel_group=accel, &
         & tooltip="Exit from Graffer"//c_null_char)

    ! Hard copy

    smnu = hl_gtk_menu_submenu_new(mbar, "Hardcopy ▼"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Options"//c_null_char, &
         & activate=c_funloc(gr_hc_opts), &
         & tooltip="Set options for hard copy"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Print (PS)"//c_null_char, &
         & activate=c_funloc(gr_print), data=c_loc(devices(1)), &
         & accel_key='p'//c_null_char, accel_group=accel, &
         & tooltip="Print to a PS file (and printer)"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Print (EPS)"//c_null_char, &
         & activate=c_funloc(gr_print), data=c_loc(devices(2)), &
         & accel_key='e'//c_null_char, accel_group=accel, &
         & tooltip="Print to an EPS file (and view)"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "Print (PDF)"//c_null_char, &
         & activate=c_funloc(gr_print), data=c_loc(devices(3)), &
         & accel_key='p'//c_null_char, &
         & accel_mods=ior(GDK_CONTROL_MASK, GDK_SHIFT_MASK), &
         & accel_group=accel, &
         & tooltip="Print to a PDF file"//c_null_char)


    jmnu = hl_gtk_menu_submenu_new(smnu, "Dump screen"//c_null_char)

    junk = hl_gtk_menu_item_new(jmnu, "PNG"//c_null_char, &
         & activate=c_funloc(gr_dump), data=c_loc(imtypes(1)), &
         & accel_key='p'//c_null_char, &
         & accel_mods=ior(GDK_CONTROL_MASK, GDK_MOD1_MASK), &
         & accel_group=accel, tooltip=&
         & "Dump screen to a PNG file"//c_null_char)

    junk = hl_gtk_menu_item_new(jmnu, "TIFF"//c_null_char, &
         & activate=c_funloc(gr_dump), data=c_loc(imtypes(2)), &
         & accel_key='t'//c_null_char, &
         & accel_mods=ior(GDK_CONTROL_MASK, GDK_MOD1_MASK), &
         & accel_group=accel, tooltip=&
         & "Dump screen to a TIFF file"//c_null_char)

    junk = hl_gtk_menu_item_new(jmnu, "JPEG"//c_null_char, &
         & activate=c_funloc(gr_dump), data=c_loc(imtypes(3)), &
         & accel_key='j'//c_null_char, &
         & accel_mods=ior(GDK_CONTROL_MASK, GDK_MOD1_MASK), &
         & accel_group=accel, tooltip=&
         & "Dump screen to a JPEG file"//c_null_char)


    junk = hl_gtk_menu_item_new(mbar, "Options..."//c_null_char, &
         & activate=c_funloc(gr_set_opts), tooltip=&
         & "Set special options for this file."//c_null_char)

    smnu = hl_gtk_menu_submenu_new(mbar, "Help ▼"//c_null_char, &
         & tooltip="Help displays"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "User Guide"//c_null_char, &
         & activate=c_funloc(gr_help), data=c_loc(helptype(1)), &
         & accel_key='h'//c_null_char, accel_group=accel, &
         & tooltip="Show the user guide"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "File Format"//c_null_char, &
         & activate=c_funloc(gr_help), data=c_loc(helptype(2)), &
         & tooltip="Show the file format description"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "About Graffer"//c_null_char, &
         & activate=c_funloc(gr_help_abt), data=c_loc(helptype(3)), &
         & tooltip="Show info about Graffer"//c_null_char)

    junk = hl_gtk_menu_item_new(smnu, "About Gtk-Fortran"//c_null_char, &
         & activate=c_funloc(gr_help_abt), data=c_loc(helptype(4)), &
         & tooltip="Show info about Gtk-Fortran"//c_null_char)

  end function gr_menubar_new

  subroutine gr_open_cb(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Callback to open/create a file

    logical, pointer :: is_new
    logical :: ok
    integer(kind=c_int) :: new_file, ipick, iresp
    character(len=256), dimension(:), allocatable :: files
    type(c_ptr) :: unsave_msg, junk
    character(len=8) :: gr_sversion

    call c_f_pointer(data, is_new)
    new_file = f_c_logical(is_new)

    ipick = hl_gtk_file_chooser_show(files, &
         & filter=["*.grf"], filter_name=["Graffer Files"], &
         & edit_filters=TRUE, create=new_file, confirm_overwrite=TRUE, &
         & parent=gr_window, initial_dir=trim(pdefs%dir)//c_null_char, &
         & initial_file=trim(pdefs%name)//c_null_char, &
         & title="Open a graffer file"//c_null_char)

    if (.not. c_f_logical(ipick)) return

    if (pdefs%chflag) then
       unsave_msg = hl_gtk_message_dialog_new( &
            & ["The current Graffer object is unsaved", &
            &  "Do you want to save it?              "], &
            & GTK_BUTTONS_NONE, type=GTK_MESSAGE_QUESTION, &
            & parent=gr_window, title="Unsaved file"//c_null_char)

       junk = gtk_dialog_add_button(unsave_msg, &
            & "Save"//c_null_char, GTK_RESPONSE_YES)
       call gtk_widget_set_tooltip_text(junk, &
            & "Save the file before opening the new one"//c_null_char)

       junk = gtk_dialog_add_button(unsave_msg, &
            & "Discard"//c_null_char, GTK_RESPONSE_NO)
       call gtk_widget_set_tooltip_text(junk, &
            & "Open the new file, discarding any changes to the old"//&
            & c_null_char)

       junk = gtk_dialog_add_button(unsave_msg, &
            & "Cancel"//c_null_char, GTK_RESPONSE_CANCEL)
       call gtk_widget_set_tooltip_text(junk, &
            & "Do not open the new file"//c_null_char)

       call gtk_widget_show_all(unsave_msg)
       iresp = gtk_dialog_run(unsave_msg)
       call gtk_widget_destroy(unsave_msg)

       select case(iresp)
       case(GTK_RESPONSE_CANCEL, GTK_RESPONSE_DELETE_EVENT)
          return
       case(GTK_RESPONSE_YES)
          call gr_write(pdefs, ok)
       case(GTK_RESPONSE_NO)
       case default
          write(error_unit, "(A)") "gr_exit: Invalid response code"
          return
       end select
    end if

    gui_active = .false.
    if (file_exists(files(1))) then
       call gr_read(pdefs, files(1), ok)
    else
       call gr_pdefs_init(pdefs)
       call split_fname(files(1), pdefs%name, pdefs%dir)
    end if
    call graffer_version%string(gr_sversion)

!!$    call gtk_window_set_title(gr_window, "Graffer V"//trim(gr_sversion)//&
!!$         & ": "//trim(files(1))//c_null_char)
    gui_active = .true.
    call gr_set_values_global()
    call gr_set_values_dataset()

    call gr_plot_draw(.false.)

  end subroutine gr_open_cb

  subroutine gr_hc_opts(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set hardcopy options

    call gr_hc_menu

  end subroutine gr_hc_opts
  subroutine gr_print(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Make a hardcopy of the plot.

    character(len=3) :: device

    call c_f_string(data, device)

    call gr_plot_close()
    call gr_plot_open(device)
    call gr_plot_draw(.false.)
    call gr_plot_close
    call gr_plot_open()
    call gr_plot_draw(.false.)

  end subroutine gr_print
  subroutine gr_dump(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Dump the current screen.

    character(len=4) :: filetype
    type(c_ptr) :: pixb
    integer :: pdot

    call c_f_string(data, filetype)

    pixb = hl_gtk_drawing_area_get_gdk_pixbuf(gr_drawing_area)
    if (pdefs%hardset%name == '') then
       pdot = index(pdefs%name, '.', back=.true.)
       if (pdot == 0) then
          pdefs%hardset%name = pdefs%name
       else
          pdefs%hardset%name = pdefs%name(:pdot-1)
       end if
    end if
    call hl_gdk_pixbuf_save(pixb, trim(pdefs%hardset%name)//'.'//&
         & trim(filetype)//c_null_char)

  end subroutine gr_dump

  subroutine gr_set_opts(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Set global options.

    call gr_options_menu

  end subroutine gr_set_opts
  subroutine gr_help(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Display help texts

    character(len=3), pointer :: type
    character(len=120), dimension(:), allocatable :: pdflist
    integer(kind=c_int) :: iresp
    character(len=*), parameter, dimension(2) :: docdir = &
         & ['/usr/local/share/doc/graffer', &
         &  '/usr/share/doc/graffer      ']
    integer :: i

    call c_f_pointer(data, type)

    if (pdefs%opts%pdfviewer == "") then
       call gr_find_viewers(pdflist)
       if (.not. allocated(pdflist)) then
          iresp = hl_gtk_message_dialog_show( &
               & ["No Viewers Found                      ", &
               &  "No PDF viewers were found in your path", &
               &  "cannot display help files             "], &
               & GTK_BUTTONS_OK, type=GTK_MESSAGE_WARNING, &
               & parent=gr_window)
          return
       end if
       pdefs%opts%pdfviewer = pdflist(1)
    end if

    if (type == 'ug') then
       do i = 1, size(docdir)
          if (file_exists(trim(docdir(i))//"/Graffer.pdf")) then
             call execute_command_line(pdefs%opts%pdfviewer//' '//&
                  & trim(docdir(i))//'/Graffer.pdf', wait=.false.)
             return
          end if
       end do
    else
       do i = 1, size(docdir)
          if (file_exists(trim(docdir(i))//"/Format.pdf")) then
             call execute_command_line(pdefs%opts%pdfviewer//' '//&
                  & trim(docdir(i))//'/Format.pdf', wait=.false.)
             return
          end if
       end do
    end if

    iresp = hl_gtk_message_dialog_show(&
         & ["File not found                        ",&
         &  "Failed to find the requested help file", &
         &  "in any expected places, please check  ", &
         &  "your installation                     "], &
         & GTK_BUTTONS_OK, type=GTK_MESSAGE_ERROR, &
         & parent=gr_window)

  end subroutine gr_help
  subroutine gr_help_abt(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Display about dialogues.

    character(len=3), pointer :: type

    call c_f_pointer(data, type)

    if (type == 'gfa') then
       call hl_gtk_about_dialog_gtk_fortran(gr_window)
    else
       call hl_gtk_about_dialog_show(name='Graffer',&
            & authors=['James Tappin'], &
            & license="GNU GPL 3"//C_NULL_CHAR, &
            & comments= &
            & "Graffer is tool for plotting and tuning data plots."//&
            & "It can generate publication-ready plots of many types"//&
            & "of data or functions"//c_null_char, &
            & parent=gr_window)
    end if
  end subroutine gr_help_abt
end module gr_menubar
