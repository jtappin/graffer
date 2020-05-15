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

module gr_file
  ! Core graffer file handling routines

  use iso_fortran_env
  use iso_c_binding, only: c_int

  use gtk_hl

  use gtk, only: GTK_RESPONSE_YES, GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO, &
       & GTK_MESSAGE_ERROR

  use gr_record
  use graff_types
  use graff_globals
  use gr_utils
  use graff_version
  use graff_init
  use gr_msg
  use gr_file_ascii

  implicit none

  integer, private :: gr_unit=0
  logical, private :: swap_end

  character(len=160), dimension(2), private :: error_str
contains

  function gr_open(file, version, ascii, name, dir, date)
    integer :: gr_open
    character(len=*), intent(in) :: file
    integer(kind=int16), intent(out), dimension(2) :: version
    logical, intent(out) :: ascii
    character(len=*), intent(out), optional :: name,dir,date

    ! Open a Graffer file for reading.

    integer :: ios, status
    character(len=120) :: iom

    character(len=7) :: rs
    character(len=200) :: ldir, lname, ltime
    character(len=len(file)) :: dir1, name1
    character(len=len(file)+2) :: autofile

    integer, dimension(13) :: svals
    integer :: mtas, mtrq
    logical :: autoflag
    integer(kind=c_int) :: iresp

    call split_fname(file, name1, dir1)
    autofile = trim(dir1)//'#'//trim(name1)//'#'

    autoflag = .false.
    ! WARNING:
    !   STAT is a GNU extension for portability remove this stuff. (down to
    !   "END of GNU extension") Can probably be replaced with something using
    !   GIO routines.
    if (file_exists(autofile)) then
       call stat(file, svals)
       mtrq = svals(10)
       call stat(autofile, svals)
       mtas = svals(10)
       if (mtas > mtrq) then
          iresp = hl_gtk_message_dialog_show( &
               & ['NEWER AUTOSAVE                                   ', &
               &  'There is an auto save file which is newer than   ', &
               &  'the saved file. Do you want to open that instead?'], &
               &  GTK_BUTTONS_YES_NO, type = GTK_MESSAGE_QUESTION, &
               &  title = "Use autosave?"//c_null_char)
          autoflag = iresp == GTK_RESPONSE_YES
       end if
    end if
    ! END of GNU extension.

    if (autoflag) then
       open(newunit=gr_unit, file=autofile, access='stream', &
            & form='unformatted', &
            & status='old', action='read', iostat=ios, iomsg=iom)
    else
       open(newunit=gr_unit, file=file, access='stream', form='unformatted', &
            & status='old', action='read', iostat=ios, iomsg=iom)
    end if

    if (ios /= 0) then
       write(error_str, "(2A/a)") "GR_OPEN: Failed to open file: ", file, &
            & trim(iom)
       call gr_message(error_str, type=GTK_MESSAGE_ERROR)
       gr_open = 0
       return
    end if

    read(gr_unit, iostat=ios, iomsg=iom) rs
    if (ios /= 0) then
       write(error_str, "(2A/a)") "GR_OPEN: Failed to read file", file, &
            & trim(iom)
       call gr_message(error_str, type=GTK_MESSAGE_ERROR)
       gr_open = 0
       return
    end if

    if (rs == 'Graffer') then
       call gr_close
       gr_open = gr_open_asc(file, gr_unit, version, name, dir, date)
       ascii = .true.
       return
    else
       ascii = .false.
    end if

    if (rs /= 'GRAFFER') then
       call gr_message("GR_OPEN: Not a Graffer file.", type=GTK_MESSAGE_ERROR)
       gr_open = 0
       call gr_close
       return
    end if

    read(gr_unit, iostat=ios, iomsg=iom) version
    if (version(1) > 255) then  ! Probably swapped
       call byte_swap(version)
       if (version(1) > 255) then
          call gr_message("GR_OPEN: Invalid version data", &
               & type=GTK_MESSAGE_ERROR)
          gr_open = 0
          call gr_close
          return
       end if
       swap_end = .true.
    else
       swap_end=.false.
    end if

    if (version(1) < 4) then
       write(error_str, "(A,i0,'.',i0)") &
            & "GR_OPEN: Only V4 (and above) files are supported, found:", &
            & version
       call gr_message(error_str(1), type=GTK_MESSAGE_ERROR)
       gr_open = 0
       call gr_close
       return
    end if

    call gr_str_read(ldir, status)
    if (status == 1) then
       gr_open = 0
       call gr_close
       return
    end if
    if (present(dir)) dir = ldir

    call gr_str_read(lname, status)
    if (status == 1) then
       gr_open = 0
       call gr_close
       return
    end if
    if (present(name)) name = lname

    call gr_str_read(ltime, status)
    if (status == 1) then
       gr_open = 0
       call gr_close
       return
    end if
    if (present(date)) date = ltime

    if (autoflag) then
       gr_open = -1
    else
       gr_open = 1
    end if

  end function gr_open

  subroutine gr_close

    ! Close a Graffer file.

    if (gr_unit == 0) return
    close(gr_unit)
    gr_unit = 0
  end subroutine gr_close

  ! Raw string (needed in file header)

  subroutine gr_str_read(string, status)
    character(len=*), intent(out) :: string
    integer, intent(out) :: status

    ! Get a string from the header of a Graffer file

    integer(kind=int32) :: slen
    integer ::ios
    character(len=120) iom
    integer(kind=int8), dimension(:), allocatable :: tail

    status = 0
    if (gr_unit == 0) then   ! No open file
       call gr_message("GR_STR_READ: No open file", type=GTK_MESSAGE_ERROR)
       status = 1
       return
    end if

    read(gr_unit, iostat=ios, iomsg=iom) slen
    if (ios /= 0) then
       write(error_str, "(A)") "GR_STR_READ: Failed to read length", trim(iom)
       call gr_message(error_str, type=GTK_MESSAGE_ERROR)
       status = 1
       return
    end if
    if (swap_end) call byte_swap(slen)
    if (slen > len(string)) then
       allocate(tail(slen-len(string)))
       read(gr_unit, iostat=ios, iomsg=iom) string, tail
       status = 4
    else
       read(gr_unit, iostat=ios, iomsg=iom) string(:slen)
       if (slen < len(string)) string(slen+1:) = ' '
    end if
    if (ios /= 0) then
       write(error_str, "(A)") "GR_STR_READ: Failed to read string", trim(iom)
       call gr_message(error_str, type=GTK_MESSAGE_ERROR)
       status = 1
    end if
 
  end subroutine gr_str_read


  ! The main read routine

  subroutine gr_read(file, ok)
    character(len=*), intent(in) :: file
    logical, intent(out) :: ok

    ! Read a Graffer file.

    logical :: is_ascii

    logical :: dflag, tflag, ctflag
    type(graffer_record) :: rec
    integer :: status

    integer(kind=int32) :: ndims
    integer(kind=int32), dimension(:), allocatable :: dims

    integer(kind=int16) :: nds, ntext
    integer :: rset, tset

    dflag = .false.
    tflag = .false.
    ctflag = .false.

    ok = .true.

    status = gr_open(file, pdefs%version, is_ascii, name=pdefs%name,&
         & dir=pdefs%dir)
    if (status == 0) then
       ok = .false.
       return
    end if

    call gr_pdefs_init
    call split_fname(file, pdefs%name, pdefs%dir)
    if (status < 0) call gr_set_changed(.true.)

    if (is_ascii) then
       call gr_get_asc(gr_unit)
       if (pdefs%version(2) <= 6) call gr_font_remap
       call gr_close
       return
    end if

    do
       status = rec%read(gr_unit, swap_end)
       if (status == -1) exit
       if (status == 1) then
          call gr_message("GR_READ: Failed to read a tag "//rec%tag, &
               & type=GTK_MESSAGE_ERROR)
          exit
       end if

       select case(rec%get_tag())
          ! The G keys are general graffer keys
          ! GT - plot title
          ! GS - Plot subtitle
          ! GC - Annotation charcter size
          ! GA - line thickness for AXES
          ! GP - Positions of corners
          ! GR - Aspect of plot.
          ! GI - Is plot isotropic?
          ! GHA - Use H/C aspect on screen
       case('GT')
          call rec%get_value(pdefs%title, status)
       case('GS')
          call rec%get_value(pdefs%subtitle, status)
       case('GC')
          call rec%get_value(pdefs%charsize, status)
       case ('GA')
          call rec%get_value(pdefs%axthick,status)
       case ('GP')
          call rec%get_value(pdefs%position,status)
       case ('GR')
          call rec%get_value(pdefs%aspect,status)
       case ('GI')
          call rec%get_value(pdefs%isotropic,status)
       case ('GHA')
          call rec%get_value(pdefs%match,status)
       case('GF')
          call rec%get_value(pdefs%fontopt, status)
          
          ! The X, Y and R keys are items relating
          ! to the X, Y and right-hand Y axes
          ! respectively  
          ! XR, YR, RR - axis range
          ! XL, YL, RL - axis log/linear
          ! XSI, YSI, RSI - axis style (the IDL
          !                 STYLE key) 
          ! XSE, YSE, RSE - extra style items
          ! XSG, YSG, RSG - Grid linestyle (IDL
          !                 linesyle+1) 
          ! XST (YST, RST) - Time labelling options
          ! XSZ (YSZ, RSZ) - Time zero.
          ! XT, YT, RT - Axis label.
          ! YIR - There is a right-hand y-axis

       case ('XR ')
          call rec%get_value(pdefs%axrange(:,1), status)
       case ('XL ')
          call rec%get_value(pdefs%axtype(1), status)
       case ('XSI')
          call rec%get_value(pdefs%axsty(1)%idl, status)
       case ('XSE')
          call rec%get_value(pdefs%axsty(1)%extra, status)
          if (iand(pdefs%axsty(1)%extra, 1_int16) == 1_int16) then 
             pdefs%axsty(1)%minor = 1_int16
             pdefs%axsty(1)%extra = ior(pdefs%axsty(1)%extra, not(1_int16))
          end if
       case ('XMN')
          call rec%get_value(pdefs%axsty(1)%minor, status)
       case ('XMJ')
          call rec%get_value(pdefs%axsty(1)%major, status)
       case ('XFM')
          call rec%get_value(pdefs%axsty(1)%format, status)

       case('XVL')
          if (allocated(pdefs%axsty(1)%values)) &
               & deallocate(pdefs%axsty(1)%values)
          ndims = rec%get_dimensions(dims)
          allocate(pdefs%axsty(1)%values(dims(1)))
          call rec%get_value(pdefs%axsty(1)%values, status)
       case ('XSG')
          call rec%get_value(pdefs%axsty(1)%grid, status)
       case ('XST')
          call rec%get_value(pdefs%axsty(1)%time, status)
       case ('XSZ')
          call rec%get_value(pdefs%axsty(1)%tzero, status)
       case ('XT ')
          call rec%get_value(pdefs%axtitle(1), status)

       case ('YR ')
          call rec%get_value(pdefs%axrange(:,2), status)
       case ('YL ')
          call rec%get_value(pdefs%axtype(2), status)
       case ('YSI')
          call rec%get_value(pdefs%axsty(2)%idl, status)
       case ('YSE')
          call rec%get_value(pdefs%axsty(2)%extra, status)
          if (iand(pdefs%axsty(2)%extra, 1_int16) == 1_int16) then 
             pdefs%axsty(2)%minor = 1_int16
             pdefs%axsty(2)%extra = ior(pdefs%axsty(2)%extra, not(1_int16))
          end if
       case ('YMN')
          call rec%get_value(pdefs%axsty(2)%minor, status)
       case ('YMJ')
          call rec%get_value(pdefs%axsty(2)%major, status)
       case ('YFM')
          call rec%get_value(pdefs%axsty(2)%format, status)
       case('YVL')
          if (allocated(pdefs%axsty(2)%values)) &
               & deallocate(pdefs%axsty(2)%values)
          ndims = rec%get_dimensions(dims)
          allocate(pdefs%axsty(2)%values(dims(1)))
          call rec%get_value(pdefs%axsty(2)%values, status)
       case ('YSG')
          call rec%get_value(pdefs%axsty(2)%grid, status)
       case ('YST')
          call rec%get_value(pdefs%axsty(2)%time, status)
       case ('YSZ')
          call rec%get_value(pdefs%axsty(2)%tzero, status)
       case ('YT ')
          call rec%get_value(pdefs%axtitle(2), status)
       case ('YIR')
          call rec%get_value(pdefs%y_right, status)

       case ('RR ')
          call rec%get_value(pdefs%axrange(:,3), status)
       case ('RL ')
          call rec%get_value(pdefs%axtype(3), status)
       case ('RSI')
          call rec%get_value(pdefs%axsty(3)%idl, status)
       case ('RSE')
          call rec%get_value(pdefs%axsty(3)%extra, status)
          if (iand(pdefs%axsty(3)%extra, 1_int16) == 1_int16) then 
             pdefs%axsty(3)%minor = 1_int16
             pdefs%axsty(3)%extra = ior(pdefs%axsty(3)%extra, not(1_int16))
          end if
       case ('RMN')
          call rec%get_value(pdefs%axsty(3)%minor, status)
       case ('RMJ')
          call rec%get_value(pdefs%axsty(3)%major, status)
       case ('RFM')
          call rec%get_value(pdefs%axsty(3)%format, status)
       case('RVL')
          if (allocated(pdefs%axsty(3)%values)) &
               & deallocate(pdefs%axsty(3)%values)
          ndims = rec%get_dimensions(dims)
          allocate(pdefs%axsty(3)%values(dims(1)))
          call rec%get_value(pdefs%axsty(3)%values, status)
       case ('RSG')
          call rec%get_value(pdefs%axsty(3)%grid, status)
       case ('RST')
          call rec%get_value(pdefs%axsty(3)%time, status)
       case ('RSZ')
          call rec%get_value(pdefs%axsty(3)%tzero, status)
       case ('RT ')
          call rec%get_value(pdefs%axtitle(3), status)

          ! ZT - specifies the colour table to
          !      be used by the image format for
          !      displaying 2-D data
          ! ZG - The gamma value for same.

       case('ZT ')
          call rec%get_value(pdefs%ctable, status)
          ctflag = .true.
       case ('ZG ')
          call rec%get_value(pdefs%gamma, status)
          if (pdefs%gamma == 0.) pdefs%gamma = 1.

          ! DN - total number of datasets in the
          !      file. This MUST come before any
          !      datasets are defined.
          ! DC - The currently selected dataset
          !      (N.B. This is ZERO-based in the file
          !      but ONE-Based inside the Fortran Code).

       case('DN ')
          call rec%get_value(pdefs%nsets, status)
          nds = max(pdefs%nsets, 1_int16)
          if (allocated(pdefs%data)) deallocate(pdefs%data)
          allocate(pdefs%data(nds))
          call gr_pdefs_data_init_all
          dflag = .true.

       case ('DC ')
          call rec%get_value(pdefs%cset, status)
          pdefs%cset = pdefs%cset+1_int16

          ! TN - The total number of text
          !      strings in the file. This must
          !      come before any strings are
          !      actually defined.

       case('TN ')
          call rec%get_value(pdefs%ntext, status)
          ntext = max(pdefs%ntext, 1_int16)
          if (allocated(pdefs%text)) deallocate(pdefs%text)
          allocate(pdefs%text(ntext))
          call gr_pdefs_text_init_all
          tflag = .true.
          ! DS - Start the definition of a
          !      dataset.

       case('DS ')
          call rec%get_value(rset, status)
          rset = rset+1
          if (dflag .and. rset >= 1 .and. rset <= pdefs%nsets) then
             call gr_read_ds(pdefs%data(rset), status)
          else
             status = 2
          end if

          ! TS - start the definition of a text
          !      string 
          ! TTS - start the definition of the
          !       text template (current default
          !       text options).

       case('TS ')
          call rec%get_value(tset, status)
          tset = tset+1
          if (tflag .and. tset >= 1 .and. tset <= pdefs%ntext) then
             call gr_read_txt(pdefs%text(tset), status)
          else 
             status = 2
          end if

       case('TTS')
          call gr_read_txt(pdefs%text_options, status)

          ! The H options refer to the options
          ! for genration PostScript hardcopy
          ! files.
          ! HC - Colour or monchrome
          ! HE - Eps or normal
          ! HO - landscape or portrait
          !      (Orientation)
          ! HY - Use CMYK colour model or not.
          ! HP - Paper size (A4 or letter)
          ! HT - whether to put a timestapm on
          !      the plot.
          ! HS - size x & y in cm.
          ! HD - Page offset in cm.
          ! HAB - The spooling command (up to
          !       the filename)
          ! HAA - Any part of the spooling
          !       command which follows the
          !       filename. 
          ! HVB - The view command (up to
          !       the filename)
          ! HVA - Any part of the view
          !       command which follows the
          !       filename. 
          ! HPB - The pdf view command (up to
          !       the filename)
          ! HPA - Any part of the pdf view
          !       command which follows the
          !       filename. 
          ! HF - Font family.
          ! HWS - Font weight and slant (bit 0 is
          !       on for bold, bit 1 for
          !       oblique/italic)
          ! HFN - Plot file name
          ! HPS - PS device
          ! HEP - EPS device
          ! HPD - PDF device
          ! HSV - SVG device

       case ('HC ')
          call rec%get_value(pdefs%hardset%colour, status)
       case ('HE ')
          call rec%get_value(pdefs%hardset%eps, status)
       case ('HO ')
          call rec%get_value(pdefs%hardset%orient, status)
       case ('HY ')
          call rec%get_value(pdefs%hardset%cmyk, status)
       case ('HP ')
          call rec%get_value(pdefs%hardset%psize, status)
       case ('HT ')
          call rec%get_value(pdefs%hardset%timestamp, status)
       case ('HS ')
          call rec%get_value(pdefs%hardset%size, status)
       case ('HD ')
          call rec%get_value(pdefs%hardset%off, status)

       case ('HAB')
          call rec%get_value(pdefs%hardset%action(1), status)
       case ('HAA')
          call rec%get_value(pdefs%hardset%action(2), status)
       case ('HVB')
          call rec%get_value(pdefs%hardset%viewer(1), status)
       case ('HVA')
          call rec%get_value(pdefs%hardset%viewer(2), status)
       case ('HPB')
          call rec%get_value(pdefs%hardset%pdfviewer(1), status)
       case ('HPA')
          call rec%get_value(pdefs%hardset%pdfviewer(2), status)

       case ('HF ')
          call rec%get_value(pdefs%hardset%font_family, status)
       case ('HWS')
          call rec%get_value(pdefs%hardset%font_wg_sl, status)
       case ('HFN')
          call rec%get_value(pdefs%hardset%name, status)

       case ('HPS')
          call rec%get_value(pdefs%hardset%psdev, status)
       case ('HEP')
          call rec%get_value(pdefs%hardset%epsdev, status)
       case ('HPD')
          call rec%get_value(pdefs%hardset%pdfdev, status)
       case ('HSV')
          call rec%get_value(pdefs%hardset%svgdev, status)

          ! The K tags relate to the plotting of
          ! a key on the plot.
          ! KU - Plot a key
          ! KX - X coordinates of the corners
          ! KY - Y coordinates of the corners
          ! KN - System they are given in.
          ! KC - How many columns
          ! KS - Character size
          ! KF - Frame?
          ! KT - Title of key
          ! KNL - Number of items in key (only
          !       used in an ascii save
          ! KL - The indices of the datasets to
          !      display
          ! KP - Whether to plot 1 or 2 points.

       case ('KU ')
          call rec%get_value(pdefs%key%use, status)
       case ('KX ')
          call rec%get_value(pdefs%key%x, status)
       case ('KY ')
          call rec%get_value(pdefs%key%y, status)
       case ('KS ')
          call rec%get_value(pdefs%key%csize, status)
       case ('KN ')
          call rec%get_value(pdefs%key%norm, status)
       case ('KC ')
          call rec%get_value(pdefs%key%cols, status)
       case ('KF ')
          call rec%get_value(pdefs%key%frame, status)
       case ('KP ')
          call rec%get_value(pdefs%key%one_point, status)
       case ('KT ')
          call rec%get_value(pdefs%key%title, status)
       case('KL ')
          if (allocated(pdefs%key%list)) deallocate(pdefs%key%list)
          ndims = rec%get_dimensions(dims)
          allocate(pdefs%key%list(dims(1)))
          call rec%get_value(pdefs%key%list, status)

          ! REM - Remarks attached to the file

       case('REM')
          if (allocated(pdefs%remarks)) deallocate(pdefs%remarks)
          ndims = rec%get_dimensions(dims)
          allocate(pdefs%remarks(dims(1)))
          call rec%get_value(pdefs%remarks, status)

          ! This probably means that the file is
          ! corrupted. 

       case default
          call gr_message("GR_READ: unknown or obsolete tag: "// &
               & rec%get_tag()//" Skipping")
          status = 2

       end select

       select case (status)
       case(1)
          call gr_message("GR_READ: I/O error", type=GTK_MESSAGE_ERROR)
          ok = .false.
          exit
       case(2)
          call gr_message("GR_READ: possibly invalid file")
 
       case(4)
          call gr_message("GR_READ: possible loss of precision: "// &
               & rec%get_tag())
       end select
    end do

    call gr_close

    if (pdefs%version(2) <= 6) call gr_font_remap

    call gr_set_changed(.false.)
    pdefs%transient%backup = .false.
    pdefs%is_ascii = .false.

  end subroutine gr_read

  subroutine gr_read_txt(txt_s, status)
    type(graff_text), intent(inout) :: txt_s
    integer, intent(out) :: status

    ! Read a text annotation from a Graffer file

    logical :: ffflag
    type(graffer_record) :: rec

    ffflag = .false.
    status = 0

    do
       status = rec%read(gr_unit, swap_end)
       if (status == -1) exit
       if (status == 1) then
          call gr_message("GR_READ_TXT: Failed to read a tag "//rec%tag, &
               & type=GTK_MESSAGE_ERROR)
          exit
       end if

       select case(rec%get_tag())

          ! Recognized tags for Text items
          ! C - colour
          ! CV - Custom colour.
          ! S - character size
          ! O - orientation (degrees
          !     anticlockwise from the normal
          !     L->R orientation.
          ! A - justification (from 0 (left
          !     aligned) to 1 (right aligned))
          ! FF - Font type
          ! F - Font (IDL font number).
          ! W - line thickness to draw with.
          ! X,Y - position of anchor (not used
          !       in template).
          ! N - is the above in normalized or
          !     data coordinates.
          ! T - The actual string (not used in
          !     template)
          ! TID - An ID string for the annotation.
          ! AX - Which Y axis to use (if
          !      multiple axes are in use)
          ! TE, TTE - End

       case ('C')
          call rec%get_value(txt_s%colour, status)
       case('CV')
          call rec%get_value(txt_s%c_vals, status)
       case ('S')
          call rec%get_value(txt_s%size, status)
       case ('O')
          call rec%get_value(txt_s%orient, status)
       case ('A')
          call rec%get_value(txt_s%align, status)
       case('FF')
          call rec%get_value(txt_s%ffamily, status)
          ffflag = .true.

       case('F')
          call rec%get_value(txt_s%font, status)

       case ('W')
          call rec%get_value(txt_s%thick, status)
       case ('X')
          call rec%get_value(txt_s%x, status)
       case ('Y')
          call rec%get_value(txt_s%y, status)
       case ('N')
          call rec%get_value(txt_s%norm, status)

       case ('T')
          call rec%get_value(txt_s%text, status)
       case ('TID')
          call rec%get_value(txt_s%id, status)

       case ('AX')
          call rec%get_value(txt_s%axis, status)

       case('TE')
          return
       case('TTE')
          return

       case default
          call gr_message("Unknown text tag: "// &
               & rec%get_tag()//" Skipping.")
          status = 2
       end select

    end do
  end subroutine gr_read_txt

  subroutine gr_read_ds(ds, status)
    type(graff_data), intent(inout) :: ds
    integer, intent(out) :: status

    ! Read an individual dataset from a Graffer file.

    logical :: nflag, nflag2
    logical :: tflag
    logical :: x2flag, y2flag
    type(graffer_record) :: rec

    integer, parameter, dimension(*) :: elements = [2, 3, 4, 3, 4, 4, 5, 5, 6]

    integer(kind=int32) :: ndims
    integer(kind=int32), dimension(:), allocatable :: dims
    integer(kind=int32) :: asize

    do
       status = rec%read(gr_unit, swap_end)
       if (status == -1) exit
       if (status == 1) then
          call gr_message("GR_READ_DS: Failed to read a tag "//rec%get_tag(), &
               & type=GTK_MESSAGE_ERROR)
          exit
       end if

       ndims = rec%get_dimensions(dims)
       
       select case(rec%get_tag())
          ! Recognised tags:
          ! J - Joining option
          ! P - symbol
          ! S - symbol size
          ! L - line style
          ! C - colour
          ! CV - Custom colour
          ! W - thickness (width)
          ! O - sorted? (Order)
          ! D - description
          ! N - number of points (or
          !     evaluations)
          ! N2- Number of points in y-direction
          !     for 2-D data.
          ! T - type
          ! M - Mode
          ! K - noclip (both C & N are already
          !     bagged)
          ! E - Mouse editing
          ! R - function range
          ! F, FX, FY - function specifiers
          ! VS, VE - start & end XY data.
          ! DE - end dataset

       case ('J')
          call rec%get_value(ds%pline, status)
       case ('P')
          call rec%get_value(ds%psym, status)
       case ('S')
          call rec%get_value(ds%symsize, status)
       case ('L')
          call rec%get_value(ds%line, status)
       case ('C')
          call rec%get_value(ds%colour, status)
       case ('CV')
          call rec%get_value(ds%c_vals, status)
       case ('W')
          call rec%get_value(ds%thick, status)
       case ('O')
          call rec%get_value(ds%sort, status)
       case ('K')
          call rec%get_value(ds%noclip, status)
       case ('E')
          call rec%get_value(ds%medit, status)
       case ('D')
          call rec%get_value(ds%descript, status)
       case('MN')
          call rec%get_value(ds%min_val, status)
       case('MX')
          call rec%get_value(ds%max_val, status)
          
       case('N')
          call rec%get_value(ds%ndata, status)
          if (ds%ndata < 0) then 
             x2flag = .true.
             ds%ndata = abs(ds%ndata)
          else
             x2flag = .false.
          endif
          nflag = .true.

       case('N2')
          call rec%get_value(ds%ndata2, status)
          if (ds%ndata2 < 0) then
             y2flag = .true.
             ds%ndata2 = abs(ds%ndata2)
          else
             y2flag = .false.
          end if
          nflag2 = .true.

       case('T')
          call rec%get_value(ds%type, status)
          tflag = .true.
       case ('M')
          call rec%get_value(ds%mode, status)

       case ('Y')
          call rec%get_value(ds%y_axis, status)

       case ('ZF')
          call rec%get_value(ds%zdata%format, status)

       case('ZNL')
          call rec%get_value(ds%zdata%n_levels, status)
          ds%zdata%set_levels = .false.

       case('ZL')
          if (allocated(ds%zdata%levels)) deallocate(ds%zdata%levels)
          if (.not. allocated(dims)) then
             asize = 1
          else
             asize = dims(1)
          end if
          allocate(ds%zdata%levels(asize))
          call rec%get_value(ds%zdata%levels, status)
          ds%zdata%n_levels = int(asize, int16)
          ds%zdata%set_levels = .true.

       case('ZC')
          if (allocated(ds%zdata%colours)) deallocate(ds%zdata%colours)
          if (.not. allocated(dims)) then
             asize = 1
          else
             asize = dims(1)
          end if
          allocate(ds%zdata%colours(asize))
          call rec%get_value(ds%zdata%colours, status)
          
          if (rec%tcode == idl_objref) then
             if (allocated(ds%zdata%raw_colours)) &
                  & deallocate(ds%zdata%raw_colours)
             allocate(ds%zdata%raw_colours(3,asize))
             call rec%get_value(ds%zdata%raw_colours, status)
          end if
          ds%zdata%n_cols = int(asize, int16)

       case('ZCR')
          if (allocated(ds%zdata%raw_colours)) &
               & deallocate(ds%zdata%raw_colours)
          allocate(ds%zdata%raw_colours(dims(1), dims(2)))
          call rec%get_value(ds%zdata%raw_colours, status)
            
       case ('ZCT')
          call rec%get_value(ds%zdata%ctable, status)

       case ('ZCG')
          call rec%get_value(ds%zdata%gamma, status)
          if (ds%zdata%gamma == 0.) ds%zdata%gamma = 1.

       case('ZS')
          if (allocated(ds%zdata%style)) deallocate(ds%zdata%style)
          if (.not. allocated(dims)) then
             asize = 1
          else
             asize = dims(1)
          end if
          allocate(ds%zdata%style(asize))
          call rec%get_value(ds%zdata%style, status)
          ds%zdata%n_sty = int(asize, int16)

       case('ZT')
          if (allocated(ds%zdata%thick)) deallocate(ds%zdata%thick)
          if (.not. allocated(dims)) then
             asize = 1
          else
             asize = dims(1)
          end if
          allocate(ds%zdata%thick(asize))
          call rec%get_value(ds%zdata%thick, status)
          ds%zdata%n_thick = int(asize, int16)

       case ('ZCF')
          call rec%get_value(ds%zdata%fill, status)
       case ('ZLI')
          call rec%get_value(ds%zdata%label, status)
       case ('ZLO')
          call rec%get_value(ds%zdata%label_off, status)
       case ('ZCS')
          call rec%get_value(ds%zdata%charsize, status)
       case('ZLM')
          call rec%get_value(ds%zdata%lmap, status)
          
       case ('ZR')
          call rec%get_value(ds%zdata%range, status)
       case ('ZP')
          call rec%get_value(ds%zdata%pxsize, status)

       case ('ZIL')
          call rec%get_value(ds%zdata%ilog, status)
       case ('ZIN')
          call rec%get_value(ds%zdata%invert, status)
       case('ZSM')
          call rec%get_value(ds%zdata%smooth, status)
       case('ZSN')
          call rec%get_value(ds%zdata%shade_levels, status)
       case ('ZM')
          call rec%get_value(ds%zdata%missing, status)

       case ('R')
          if (ds%type == -4) then
             call rec%get_value(ds%funct%range, status)
          else
             call rec%get_value(ds%funct%range(:,1), status)
          end if

       case ('F','FX')
          call rec%get_value(ds%funct%funct(1), status)
       case ('FY')
          call rec%get_value(ds%funct%funct(2), status)

       case ('VS')
          if (allocated(ds%xydata)) deallocate(ds%xydata)
          allocate(ds%xydata(dims(1), dims(2)))
          call rec%get_value(ds%xydata, status)
          ds%ndata = dims(2)

       case('ZXS')
          if (allocated(ds%zdata%x)) deallocate(ds%zdata%x)
          if (ndims == 1) then
             ds%zdata%x_is_2d = .false.
             allocate(ds%zdata%x(dims(1),1))
          else
             ds%zdata%x_is_2d = dims(2) > 1
             allocate(ds%zdata%x(dims(1),dims(2)))
          end if
          call rec%get_value(ds%zdata%x, status)

       case('ZYS')
          if (allocated(ds%zdata%y)) deallocate(ds%zdata%y)
          if (ndims == 1) then
             ds%zdata%y_is_2d = .false.
             allocate(ds%zdata%y(1,dims(1)))
          else
             ds%zdata%y_is_2d = dims(1) > 1
             allocate(ds%zdata%y(dims(1),dims(2)))
          end if
          call rec%get_value(ds%zdata%y, status)
       case('ZZS')
          if (allocated(ds%zdata%z)) deallocate(ds%zdata%z)
          allocate(ds%zdata%z(dims(1),dims(2)))
          call rec%get_value(ds%zdata%z, status)

       case ('DE')
          exit

       case default
          call gr_message("GR_READ_DS: Unknown DS tag: "// &
               & rec%get_tag()//", Skipping.")
          status = 2

       end select
    end do

    ! If min_val and max_val are both zero then they should both be NaN

    if (ds%min_val == 0._real64 .and. ds%max_val == 0._real64) then
       ds%min_val = d_nan
       ds%max_val = d_nan
    end if

  end subroutine gr_read_ds

  subroutine gr_write(ok, auto, ascii)
    logical, intent(out) :: ok
    logical, optional, intent(in) :: auto, ascii

    ! Write a Graffer file.

    integer :: ios, unit
    character(len=100) :: iom
    logical :: autosave, use_ascii
    type(graffer_record) :: rec
    character(len=28) :: date
    type(graff_data), pointer :: gdata
    type(graff_text), pointer :: gtext
    character(len=242) :: outfile, autofile

    integer(kind=int32) :: i, n1, n2
    integer :: status

    if (present(auto)) then
       autosave = auto
    else 
       autosave = .false.
    end if

    if (autosave) then
       outfile = trim(pdefs%dir)//'#'//trim(pdefs%name)//'#'
    else
       outfile = trim(pdefs%dir)//trim(pdefs%name)
       autofile = trim(pdefs%dir)//'#'//trim(pdefs%name)//'#'
       if (.not. pdefs%transient%backup .and. file_exists(outfile)) then
          call execute_command_line("cp "//trim(outfile)//" "//&
               & trim(outfile)//"~")
          pdefs%transient%backup = .true.
       end if
       if (present(ascii)) then
          use_ascii = ascii
       else
          use_ascii = pdefs%is_ascii
       end if
       if (use_ascii) then
          call gr_save_asc(ok)
          return
       end if
    end if

    open(newunit=unit, file=outfile, status='replace', &
         & form='unformatted', action='write', access='stream', &
         & iostat=ios, iomsg=iom)
    if (ios /= 0) then
       write(error_str, "(A)") "GR_WRITE:: Failed to open file", trim(iom)
       call gr_message(error_str, type=GTK_MESSAGE_ERROR)
       ok = .false.
       return
    end if

    call gr_date(date)
    write(unit) "GRAFFER", graffer_version%ints(), &
         & to_little(len_trim(pdefs%dir)), &
         & trim(pdefs%dir), to_little(len_trim(pdefs%name)), &
         & trim(pdefs%name), &
         & to_little(len_trim(date)), trim(date)

    call rec%set_value('GT ', pdefs%title, unit)
    call rec%set_value('GS ', pdefs%subtitle, unit)
    call rec%set_value('GC ', pdefs%charsize, unit)
    call rec%set_value('GA ', pdefs%axthick, unit)
    call rec%set_value('GP ', pdefs%position, unit)
    call rec%set_value('GR ', pdefs%aspect, unit)
    call rec%set_value('GI ', pdefs%isotropic , unit)
    call rec%set_value('GHA', pdefs%match, unit)

    ! X-axis information

    call rec%set_value('XR ', pdefs%axrange(:,1), unit)
    call rec%set_value('XL ', pdefs%axtype(1), unit)
    call rec%set_value('XSI', pdefs%axsty(1)%idl, unit)
    call rec%set_value('XSE', pdefs%axsty(1)%extra, unit)
    call rec%set_value('XSG', pdefs%axsty(1)%grid, unit)
    call rec%set_value('XST', pdefs%axsty(1)%time, unit)
    call rec%set_value('XSZ', pdefs%axsty(1)%tzero, unit)
    call rec%set_value('XMJ', pdefs%axsty(1)%major, unit)
    call rec%set_value('XFM', pdefs%axsty(1)%format, unit)
    call rec%set_value('XMN', pdefs%axsty(1)%minor, unit)
    if (allocated(pdefs%axsty(1)%values)) &
         &  call rec%set_value('XVL', pdefs%axsty(1)%values, unit)
    call rec%set_value('XT ', pdefs%axtitle(1), unit)

    ! Y-axis information

    call rec%set_value('YIR', pdefs%y_right, unit)
    call rec%set_value('YR ', pdefs%axrange(:,2), unit)
    call rec%set_value('YL ', pdefs%axtype(2), unit)
    call rec%set_value('YSI', pdefs%axsty(2)%idl, unit)
    call rec%set_value('YSE', pdefs%axsty(2)%extra, unit)
    call rec%set_value('YSG', pdefs%axsty(2)%grid, unit)
    call rec%set_value('YST', pdefs%axsty(2)%time, unit)
    call rec%set_value('YSZ', pdefs%axsty(2)%tzero, unit)
    call rec%set_value('YMJ', pdefs%axsty(2)%major, unit)
    call rec%set_value('YFM', pdefs%axsty(2)%format, unit)
    call rec%set_value('YMN', pdefs%axsty(2)%minor, unit)
    if (allocated(pdefs%axsty(2)%values)) &
         & call rec%set_value('YVL', pdefs%axsty(2)%values, unit)
    call rec%set_value('YT ', pdefs%axtitle(2), unit)

    ! Secondary Y-axis information

    call rec%set_value('RR ', pdefs%axrange(:,3), unit)
    call rec%set_value('RL ', pdefs%axtype(3), unit)
    call rec%set_value('RSI', pdefs%axsty(3)%idl, unit)
    call rec%set_value('RSE', pdefs%axsty(3)%extra, unit)
    call rec%set_value('RSG', pdefs%axsty(3)%grid, unit)
    call rec%set_value('RST', pdefs%axsty(3)%time, unit)
    call rec%set_value('RSZ', pdefs%axsty(3)%tzero, unit)
    call rec%set_value('RMJ', pdefs%axsty(3)%major, unit)
    call rec%set_value('RFM', pdefs%axsty(3)%format, unit)
    call rec%set_value('RMN', pdefs%axsty(3)%minor, unit)
    if (allocated(pdefs%axsty(3)%values)) &
         & call rec%set_value('RVL', pdefs%axsty(3)%values, unit)
    call rec%set_value('RT ', pdefs%axtitle(3), unit)

    ! Colour table for displayed Z data
    call rec%set_value('ZT ', pdefs%ctable, unit)
    call rec%set_value('ZG ', pdefs%gamma, unit)

    ! Number of data sets.
    call rec%set_value('DN ', pdefs%nsets, unit)
    call rec%set_value('DC ', pdefs%cset-1_int16, unit)

    !	Output each dataset

    do i = 1, pdefs%nsets
       gdata => pdefs%data(i)
       n1 = gdata%ndata
       n2 = gdata%ndata2

       call rec%set_value('DS ', i-1, unit)
       call rec%set_value('D  ', gdata%descript, unit)
       call rec%set_value('T  ', gdata%type, unit)
       call rec%set_value('M  ', gdata%mode, unit)
       call rec%set_value('Y  ', gdata%y_axis, unit)
       call rec%set_value('N  ', n1, unit)
       call rec%set_value('N2 ', n2, unit)

       call rec%set_value('J  ', gdata%pline, unit)
       call rec%set_value('P  ', gdata%psym, unit)
       call rec%set_value('S  ', gdata%symsize, unit)
       call rec%set_value('L  ', gdata%line, unit)
       call rec%set_value('C  ', gdata%colour, unit)
       call rec%set_value('CV ', gdata%c_vals, unit)
       call rec%set_value('W  ', gdata%thick, unit)
       call rec%set_value('O  ', gdata%sort, unit)
       call rec%set_value('K  ', gdata%noclip, unit)
       call rec%set_value('E  ', gdata%medit, unit)
       call rec%set_value('MN ', gdata%min_val, unit)
       call rec%set_value('MX ', gdata%max_val, unit)
       
       select case(gdata%type)
       case(0:8)             ! X-Y types
          if (allocated(gdata%xydata)) &
               & call rec%set_value('VS ', gdata%xydata, unit)

       case(9)               ! 2-D data
          call rec%set_value('ZXS', gdata%zdata%x, unit)
          call rec%set_value('ZYS', gdata%zdata%y, unit)
          call rec%set_value('ZZS', gdata%zdata%z, unit)
       case(-1,-2)       ! Functions
          call rec%set_value('R  ', gdata%funct%range(:,1), unit)
          call rec%set_value('F  ', gdata%funct%funct(1), unit)

       case(-4)       ! 2 D Functions
          call rec%set_value('R  ', gdata%funct%range, unit)
          call rec%set_value('F  ', gdata%funct%funct(1), unit)

       case(-3)             ! Parametric functions
          call rec%set_value('R  ', gdata%funct%range(:,1), unit)
          call rec%set_value('FX ', gdata%funct%funct(1), unit)
          call rec%set_value('FY ', gdata%funct%funct(2), unit)
       end select

       if (gdata%type == 9 .or. gdata%type == -4) then
          call rec%set_value('ZF ', gdata%zdata%format, unit)

          call rec%set_value('ZCF', gdata%zdata%fill, unit)
          call rec%set_value('ZLI', gdata%zdata%label, unit)
          call rec%set_value('ZLO', gdata%zdata%label_off, unit)
          call rec%set_value('ZCS', gdata%zdata%charsize, unit)
          call rec%set_value('ZLM', gdata%zdata%lmap, unit)
          
          call rec%set_value('ZCT', gdata%zdata%ctable, unit)
          call rec%set_value('ZCG', gdata%zdata%gamma, unit)

          if (gdata%zdata%set_levels .and. &
               & allocated(gdata%zdata%levels)) then
             call rec%set_value('ZL ', gdata%zdata%levels, unit)
          else
             call rec%set_value('ZNL', gdata%zdata%n_levels, unit)
          end if
          if (gdata%zdata%n_cols > 0)  then
             call rec%set_value('ZC ',gdata%zdata%colours , unit)
             if (allocated(gdata%zdata%raw_colours)) &
                  & call rec%set_value('ZCR',gdata%zdata%raw_colours , unit)
          end if
          if (gdata%zdata%n_sty > 0)  &
               & call rec%set_value('ZS ', gdata%zdata%style, unit)
          if (gdata%zdata%n_thick > 0)  &
               & call rec%set_value('ZT ', gdata%zdata%thick, unit)

          call rec%set_value('ZR ', gdata%zdata%range, unit)
          call rec%set_value('ZP ', gdata%zdata%pxsize, unit)
          call rec%set_value('ZIL', gdata%zdata%ilog, unit)
          call rec%set_value('ZIN', gdata%zdata%invert, unit)
          call rec%set_value('ZSM', gdata%zdata%smooth, unit)
          call rec%set_value('ZSN', gdata%zdata%shade_levels, unit)
          call rec%set_value('ZM ', gdata%zdata%missing, unit)
       end if
       call rec%set_value('DE ')
       call rec%put(unit, status)
    end do

    !	Note: don't save the widget ids.

    call rec%set_value('TN ', pdefs%ntext, unit)
    ! Number of text items

    do i = 1, pdefs%ntext
       gtext => pdefs%text(i)
       call rec%set_value('TS ', i-1, unit)
       call rec%set_value('TID', gtext%id, unit)
       call rec%set_value('T  ', gtext%text, unit)
       call rec%set_value('X  ', gtext%x, unit)
       call rec%set_value('Y  ', gtext%y, unit)
       call rec%set_value('N  ', gtext%norm, unit)
       call rec%set_value('AX ', gtext%axis, unit)
       call rec%set_value('C  ', gtext%colour, unit)
       call rec%set_value('CV ', gtext%c_vals, unit)
       call rec%set_value('S  ', gtext%size, unit)
       call rec%set_value('O  ', gtext%orient, unit)
       call rec%set_value('A  ', gtext%align, unit)
       call rec%set_value('FF ', gtext%ffamily, unit)
       call rec%set_value('F  ', gtext%font, unit)
       call rec%set_value('W  ', gtext%thick, unit)
       call rec%set_value('TE ')
       call rec%put(unit, status)
    end do

    !	The text template (no need to dump the text string or position
    !	here as it 
    !	must be the null string, only included to simplify coding)

    call rec%set_value('TTS')
    call rec%put(unit, status)
    call rec%set_value('C  ', pdefs%text_options%colour, unit)
    call rec%set_value('CV ', pdefs%text_options%c_vals, unit)
    call rec%set_value('S  ', pdefs%text_options%size, unit)
    call rec%set_value('O  ', pdefs%text_options%orient, unit)
    call rec%set_value('A  ', pdefs%text_options%align, unit)
    call rec%set_value('F  ', pdefs%text_options%font, unit)
    call rec%set_value('W  ', pdefs%text_options%thick, unit)
    call rec%set_value('N  ', pdefs%text_options%norm, unit)
    call rec%set_value('TTE')
    call rec%put(unit, status)

    !	Specify the key information

    call rec%set_value('KU ', pdefs%key%use, unit)
    call rec%set_value('KX ', pdefs%key%x, unit)
    call rec%set_value('KY ', pdefs%key%y, unit)
    call rec%set_value('KN ', pdefs%key%norm, unit)
    call rec%set_value('KC ', pdefs%key%cols, unit)
    call rec%set_value('KF ', pdefs%key%frame, unit)
    call rec%set_value('KP ', pdefs%key%one_point, unit)
    call rec%set_value('KS ', pdefs%key%csize, unit)
    call rec%set_value('KT ', pdefs%key%title, unit)

    if (allocated(pdefs%key%list)) &
         & call rec%set_value('KL ', pdefs%key%list, unit)

    !	Any remarks associated with the file.

    if (allocated(pdefs%remarks)) &
         & call rec%set_value('REM', pdefs%remarks, unit)

    ! The hardcopy options

    call rec%set_value('HC ', pdefs%hardset%colour, unit)
    call rec%set_value('HE ', pdefs%hardset%eps, unit)
    call rec%set_value('HO ', pdefs%hardset%orient, unit)
    call rec%set_value('HY ', pdefs%hardset%cmyk, unit)
    call rec%set_value('HP ', pdefs%hardset%psize, unit)
    call rec%set_value('HT ', pdefs%hardset%timestamp, unit)
    call rec%set_value('HS ', pdefs%hardset%size, unit)
    call rec%set_value('HD ', pdefs%hardset%off, unit)

    call rec%set_value('HAB', pdefs%hardset%action(1), unit)
    call rec%set_value('HAA', pdefs%hardset%action(2), unit)
    call rec%set_value('HVB', pdefs%hardset%viewer(1), unit)
    call rec%set_value('HVA', pdefs%hardset%viewer(2), unit)
    call rec%set_value('HPB', pdefs%hardset%pdfviewer(1), unit)
    call rec%set_value('HPA', pdefs%hardset%pdfviewer(2), unit)

    call rec%set_value('HF ', pdefs%hardset%font_family, unit)
    call rec%set_value('HWS', pdefs%hardset%font_wg_sl, unit)
    call rec%set_value('HFN', pdefs%hardset%name, unit)
    call rec%set_value('HPS', pdefs%hardset%psdev, unit)
    call rec%set_value('HEP', pdefs%hardset%epsdev, unit)
    call rec%set_value('HPD', pdefs%hardset%pdfdev, unit)
    call rec%set_value('HSV', pdefs%hardset%svgdev, unit)

    close(unit)

    call gr_set_changed(.false., auto=autosave)
    if (.not. autosave) then
       pdefs%is_ascii = .false.
       if (file_exists(autofile)) then
          open(newunit=unit, file=autofile)
          close(unit, status='delete')
       end if
    end if

  end subroutine gr_write

  subroutine gr_font_remap

    ! Approximate font mapping

    integer(kind=int16), parameter, dimension(3:20,-1:1) :: &
         & ffam = reshape( int( &
         & [1,5,1,2,5,2,5,5,2,4,4,2,2,1,2,2,1,5,&
         &  1,1,1,1,2,2,5,5,3,3,2,2,2,2,1,4,4,1,&
         &  1,1,1,1,2,2,5,5,3,3,3,3,2,2,1,1,1,1], int16), [18,3]), &
         & fshp = reshape( int( &
         & [1,1,2,1,2,3,1,1,1,1,2,1,1,1,2,4,1,1,&
         &  1,2,1,6,1,4,1,1,1,5,1,3,2,4,1,1,2,1,&
         &  1,2,3,4,1,3,1,1,1,3,2,4,2,4,1,1,1,1], int16), [18,3])
    integer(kind=int16) tff, tfsh
    type(graff_text), pointer :: text
    integer :: i

    if (pdefs%version(2) > 6) return

    if (pdefs%hardset%font_wg_sl >= 3) then
       tff = pdefs%hardset%font_family
       tfsh = pdefs%hardset%font_wg_sl
       pdefs%hardset%font_family = ffam(tfsh,tff)
       pdefs%hardset%font_wg_sl = fshp(tfsh,tff)
    end if

    do i = 1, pdefs%ntext
       text => pdefs%text(i)
       if (text%font >= 3) then
          tff = text%ffamily
          tfsh = text%font
          text%ffamily = ffam(tfsh,tff)
          text%font = fshp(tfsh,tff)
       end if
    end do

    text => pdefs%text_options
    if (text%font >= 3) then
       tff = text%ffamily
       tfsh = text%font
       text%ffamily = ffam(tfsh,tff)
       text%font = fshp(tfsh,tff)
    end if

  end subroutine gr_font_remap
end module gr_file
