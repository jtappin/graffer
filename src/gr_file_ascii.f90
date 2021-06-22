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

 module gr_file_ascii
   ! Routines for accessing ASCII Graffer files.

  use iso_fortran_env

  use gtk_sup
  use gtk, only: GTK_MESSAGE_ERROR

  use graff_types
  use graff_globals
  use gr_utils
  use gr_msg
  use graff_init

  implicit none

  interface gr_log_val
     module procedure gr_log_val_s, gr_log_val_a
  end interface gr_log_val
  interface gr_byt_val
     module procedure gr_byt_val_s, gr_byt_val_a
  end interface gr_byt_val
  interface gr_int_val
     module procedure gr_int_val_s, gr_int_val_a
  end interface gr_int_val
  interface gr_lon_val
     module procedure gr_lon_val_s, gr_lon_val_a
  end interface gr_lon_val
  interface gr_flt_val
     module procedure gr_flt_val_s, gr_flt_val_a
  end interface gr_flt_val
  interface gr_dbl_val
     module procedure gr_dbl_val_s, gr_dbl_val_a
  end interface gr_dbl_val

  character(len=160), dimension(3), private :: error_str

contains
  subroutine gr_str_val(line, tag, val)
    character(len=*), intent(in) :: line, tag
    character(len=*), intent(out) :: val

    ! Extract a string from a line of an ASCII graffer file.

    integer :: pos

    pos = index(line, trim(tag)//':')
    if (pos == 0) then
       val = ''
    else
       val = line(pos+len_trim(tag)+1:)
    end if
  end subroutine gr_str_val

  function gr_log_val_s(string)
    logical :: gr_log_val_s
    character(len=*), intent(in) :: string

    ! Convert a Tag value to a logical

    integer(kind=int8) :: ilg
    integer :: ios
    character(len=120) :: iom

    read(string, *, iostat=ios, iomsg=iom) ilg
    if (ios /= 0) then
       write(error_str, "(a/t10,a/t10,a)") &
            & "gr_log_val_s: Failed to read logical value from:",&
            & trim(string), trim(iom)
       call gr_message(error_str)
       gr_log_val_s = .false.
    else
       gr_log_val_s = c_f_logical(int(ilg))
    end if
  end function gr_log_val_s

  function gr_log_val_a(string, num)
    logical, dimension(:), allocatable :: gr_log_val_a
    character(len=*), intent(in) :: string
    integer, intent(in) :: num

    ! Convert Tag values to logicals

    integer(kind=int8), dimension(:), allocatable :: ilg
    integer :: ios
    character(len=120) :: iom


    allocate(gr_log_val_a(num), ilg(num))
    read(string, *, iostat=ios, iomsg=iom) ilg
    if (ios /= 0) then
       write(error_str, "(a/t10,a/t10,a)") &
            & "gr_log_val_a: Failed to read logical value from:",&
            & trim(string), trim(iom)
       call gr_message(error_str)
       gr_log_val_a = .false.
    else
       gr_log_val_a = ilg == 0
    end if
  end function gr_log_val_a

  function gr_byt_val_s(string)
    integer(kind=int8) :: gr_byt_val_s
    character(len=*), intent(in) :: string

    ! Convert a tag value to a byte

    integer :: ios
    character(len=120) :: iom

    read(string, *, iostat=ios, iomsg=iom) gr_byt_val_s
    if (ios /= 0) then
       write(error_str, "(a/t10,a/t10,a)") &
            & "gr_byt_val_s: Failed to read int8 value from:",&
            & trim(string), trim(iom)
       call gr_message(error_str)
    end if
  end function gr_byt_val_s

  function gr_byt_val_a(string, num)
    integer(kind=int8), dimension(:), allocatable :: gr_byt_val_a
    character(len=*), intent(in) :: string
    integer, intent(in) :: num

    ! Convert tag values to bytes

    integer :: ios
    character(len=120) :: iom

    allocate(gr_byt_val_a(num))
    read(string, *, iostat=ios, iomsg=iom) gr_byt_val_a
    if (ios /= 0) then
       write(error_str, "(a/t10,a/t10,a)") &
            & "gr_byt_val_a: Failed to read int8 value from:",&
            & trim(string), trim(iom)
       call gr_message(error_str)
    end if
  end function gr_byt_val_a

  function gr_int_val_s(string)
    integer(kind=int16) :: gr_int_val_s
    character(len=*), intent(in) :: string

    ! Convert a tag value to an int (2byte)

    integer :: ios
    character(len=120) :: iom

    read(string, *, iostat=ios, iomsg=iom) gr_int_val_s
    if (ios /= 0) then
       write(error_str, "(a/t10,a/t10,a)") &
            & "gr_int_val_s: Failed to read int16 value from:",&
            & trim(string), trim(iom)
       call gr_message(error_str)
    end if
  end function gr_int_val_s

  function gr_int_val_a(string, num)
    integer(kind=int16), dimension(:), allocatable :: gr_int_val_a
    character(len=*), intent(in) :: string
    integer, intent(in) :: num

    ! Convert tag values to ints (2byte)

    integer :: ios
    character(len=120) :: iom


    allocate(gr_int_val_a(num))
    read(string, *, iostat=ios, iomsg=iom) gr_int_val_a
    if (ios /= 0) then
       write(error_str, "(a/t10,a/t10,a)") &
            & "gr_int_val_a: Failed to read int16 value from:",&
            & trim(string), trim(iom)
       call gr_message(error_str)
    end if
  end function gr_int_val_a

  function gr_lon_val_s(string)
    integer(kind=int32) :: gr_lon_val_s
    character(len=*), intent(in) :: string

    ! Convert a tag value to a long (4byte)

    integer :: ios
    character(len=120) :: iom

    read(string, *, iostat=ios, iomsg=iom) gr_lon_val_s
    if (ios /= 0) then
       write(error_str, "(a/t10,a/t10,a)") &
            & "gr_lon_val_s: Failed to read int32 value from:",&
            & trim(string), trim(iom)
       call gr_message(error_str)
    end if
  end function gr_lon_val_s

  function gr_lon_val_a(string, num)
    integer(kind=int32), dimension(:), allocatable :: gr_lon_val_a
    character(len=*), intent(in) :: string
    integer, intent(in) :: num

    ! Convert tag values to longs (4byte)

    integer :: ios
    character(len=120) :: iom

    allocate(gr_lon_val_a(num))
    read(string, *, iostat=ios, iomsg=iom) gr_lon_val_a
    if (ios /= 0) then
       write(error_str, "(a/t10,a/t10,a)") &
            & "gr_lon_val_a: Failed to read int32 value from:",&
            & trim(string), trim(iom)
       call gr_message(error_str)
    end if
  end function gr_lon_val_a

  function gr_flt_val_s(string)
    real(kind=real32) :: gr_flt_val_s
    character(len=*), intent(in) :: string

    ! Convert a tag value to a float (4byte)

    integer :: ios
    character(len=120) :: iom

    read(string, *, iostat=ios, iomsg=iom) gr_flt_val_s
    if (ios /= 0) then
       write(error_str, "(a/t10,a/t10,a)") &
            & "gr_flt_val_s: Failed to read real32 value from:",&
            & trim(string), trim(iom)
       call gr_message(error_str)
    end if
  end function gr_flt_val_s

  function gr_flt_val_a(string, num)
    real(kind=real32), dimension(:), allocatable :: gr_flt_val_a
    character(len=*), intent(in) :: string
    integer, intent(in) :: num

    ! Convert tag values to floats (4byte)

    integer :: ios
    character(len=120) :: iom

    allocate(gr_flt_val_a(num))
    read(string, *, iostat=ios, iomsg=iom) gr_flt_val_a
    if (ios /= 0) then
       write(error_str, "(a/t10,a/t10,a)") &
            & "gr_flt_val_a: Failed to read real32 value from:",&
            & trim(string), trim(iom)
       call gr_message(error_str)
    end if
  end function gr_flt_val_a

  function gr_dbl_val_s(string)
    real(kind=real64) :: gr_dbl_val_s
    character(len=*), intent(in) :: string

    ! Convert a tag value to a double (8byte)

    integer :: ios
    character(len=120) :: iom

    read(string, *, iostat=ios, iomsg=iom) gr_dbl_val_s
    if (ios /= 0) then
       write(error_str, "(a/t10,a/t10,a)") &
            & "gr_dbl_val_s: Failed to read real64 value from:",&
            & trim(string), trim(iom)
       call gr_message(error_str)
    end if
  end function gr_dbl_val_s

  function gr_dbl_val_a(string, num)
    real(kind=real64), dimension(:), allocatable :: gr_dbl_val_a
    character(len=*), intent(in) :: string
    integer, intent(in) :: num

    ! Convert tag values to doubles (8byte)

    integer :: ios
    character(len=120) :: iom

    allocate(gr_dbl_val_a(num))
    read(string, *, iostat=ios, iomsg=iom) gr_dbl_val_a
    if (ios /= 0) then
       write(error_str, "(a/t10,a/t10,a)") &
            & "gr_dbl_val_a: Failed to read real64 value from:",&
            & trim(string), trim(iom)
       call gr_message(error_str)
    end if
  end function gr_dbl_val_a

  function gr_open_asc(file, unit, version, name, dir, date)
    integer :: gr_open_asc
    character(len=*), intent(in) :: file
    integer, intent(out) :: unit
    integer(kind=int16), intent(out), dimension(2) :: version
    character(len=*), intent(out), optional :: name,dir,date

    ! Open an ASCII Graffer file.

    character(len=200) :: ldir, lname
    character(len=256) :: inln, tmp
    integer :: ios, pos
    character(len=120) :: iom

    open(newunit=unit, file=file, form='formatted', &
            & status='old', action='read', iostat=ios, iomsg=iom)
    if (ios /= 0) then
       write(error_str, "(A)") "GR_OPEN_ASC: Failed to open file", trim(iom)
       call gr_message(error_str(:2), type=GTK_MESSAGE_ERROR)
       gr_open_asc = 0
       return
    end if

    read(unit, "(A)") inln

    pos = index(inln,'V') + 1
    read(inln(pos:pos+1), *) version(1)
    read(inln(pos+3:pos+4), *) version(2)

    if (present(name) .or. present(dir)) then
       pos = index(inln,':')
       tmp = inln(pos+1:)
       pos = index(tmp, ':')
       tmp = adjustl(tmp(:pos-1))
       call split_fname(tmp, lname, ldir)
       if (present(name)) name = lname
       if (present(dir)) dir = ldir
    end if

    if (present(date)) then
       pos = index(inln, '@')
       date = adjustl(inln(pos+1:))
    end if

    gr_open_asc = 1
  end function gr_open_asc
    
  subroutine gr_get_asc(unit)
    integer, intent(in) :: unit

    ! Read an ASCII Graffer file

    integer :: ios, ntag, itag, nrem, i
    character(len=120) :: iom
    logical :: dflag, tflag
    integer :: nxt, nyt, nrt, rset, tset, n_key_list

    character(len=256) :: inln
    character(len=256), dimension(:), allocatable :: tag_val
    type(graff_data), pointer :: data
    type(graff_text), pointer :: text

    nxt = 0
    nyt = 0
    nrt = 0

    dflag = .false.
    tflag = .false.

    do
       read(unit, '(a)', iostat=ios, iomsg=iom) inln
       if (ios == iostat_end) exit
       if (ios /= 0) then
          write(error_str, "(a/t10,a)") &
               & "gr_get_asc: Error reading from ascii file", trim(iom)
          call gr_message(error_str(:2), type=GTK_MESSAGE_ERROR)
          close(unit)
          return
       end if

       call split(inln, ':', tag_val, count=ntag)

       do itag = 1, ntag, 2
          select case(trim(tag_val(itag)))
          case('GT')
             call gr_str_val(inln, 'GT', pdefs%title)
             exit
          case('GS')
             call gr_str_val(inln, 'GS', pdefs%subtitle)
             exit
          case('GC')
             pdefs%charsize = gr_flt_val(tag_val(itag+1))
          case('GA')
             pdefs%axthick = gr_flt_val(tag_val(itag+1))
          case('GP')
             pdefs%position = gr_flt_val(tag_val(itag+1), 4)
          case('GR')
             pdefs%aspect = gr_flt_val(tag_val(itag+1), 2)
          case('GI')
             pdefs%isotropic = gr_log_val(tag_val(itag+1))
          case('GHA')
             pdefs%match = gr_log_val(tag_val(itag+1))


          case('XR')
             pdefs%axrange(:,1) = gr_dbl_val(tag_val(itag+1), 2)
          case('XL')
             pdefs%axtype(1) = gr_int_val(tag_val(itag+1))
          case('XSI')
             pdefs%axsty(1)%idl = gr_int_val(tag_val(itag+1))
          case('XSE')
             pdefs%axsty(1)%extra = gr_int_val(tag_val(itag+1))
             if (btest(pdefs%axsty(1)%extra, 0)) then
                pdefs%axsty(1)%minor = 1
                pdefs%axsty(1)%extra = ibclr(pdefs%axsty(1)%extra, 0)
             end if

          case('XMJ')
             pdefs%axsty(1)%major = gr_int_val(tag_val(itag+1))
          case('XMN')
             pdefs%axsty(1)%minor = gr_int_val(tag_val(itag+1))
          case('XNV')
             nxt = gr_int_val(tag_val(itag+1))
          case('XFM')
             call gr_str_val(inln, 'XFM', pdefs%axsty(1)%format)
             exit
          case('XLL')
             pdefs%axsty(1)%log_bands =gr_int_val(tag_val(itag+1), 3)
             
          case('XVL')
             if (nxt == 0) exit
             pdefs%axsty(1)%values = gr_dbl_val(tag_val(itag+1), nxt)

          case('XSG')
             pdefs%axsty(1)%grid = gr_int_val(tag_val(itag+1))
          case('XST')
             pdefs%axsty(1)%time = gr_int_val(tag_val(itag+1))
          case('XSZ')
             pdefs%axsty(1)%tzero = gr_int_val(tag_val(itag+1))
          case('XT')
             call gr_str_val(inln, 'XT', pdefs%axtitle(1))
             exit

          case('YR')
             pdefs%axrange(:,2) = gr_dbl_val(tag_val(itag+1), 2)
          case('YL')
             pdefs%axtype(2) = gr_int_val(tag_val(itag+1))
          case('YSI')
             pdefs%axsty(2)%idl = gr_int_val(tag_val(itag+1))
          case('YSE')
             pdefs%axsty(2)%extra = gr_int_val(tag_val(itag+1))
             if (btest(pdefs%axsty(2)%extra, 0)) then
                pdefs%axsty(2)%minor = 1
                pdefs%axsty(2)%extra = ibclr(pdefs%axsty(2)%extra, 0)
             end if

          case('YMJ')
             pdefs%axsty(2)%major = gr_int_val(tag_val(itag+1))
          case('YMN')
             pdefs%axsty(2)%minor = gr_int_val(tag_val(itag+1))
          case('YNV')
             nxt = gr_int_val(tag_val(itag+1))
          case('YFM')
             call gr_str_val(inln, 'YFM', pdefs%axsty(2)%format)
             exit
          case('YLL')
             pdefs%axsty(2)%log_bands =gr_int_val(tag_val(itag+1), 3)
             
          case('YVL')
             if (nxt == 0) exit
             pdefs%axsty(2)%values = gr_dbl_val(tag_val(itag+1), nxt)

          case('YSG')
             pdefs%axsty(2)%grid = gr_int_val(tag_val(itag+1))
          case('YST')
             pdefs%axsty(2)%time = gr_int_val(tag_val(itag+1))
          case('YSZ')
             pdefs%axsty(2)%tzero = gr_int_val(tag_val(itag+1))
          case('YT')
             call gr_str_val(inln, 'YT', pdefs%axtitle(2))
             exit

          case('RR')
             pdefs%axrange(:,3) = gr_dbl_val(tag_val(itag+1), 2)
          case('RL')
             pdefs%axtype(3) = gr_int_val(tag_val(itag+1))
          case('RSI')
             pdefs%axsty(3)%idl = gr_int_val(tag_val(itag+1))
          case('RSE')
             pdefs%axsty(3)%extra = gr_int_val(tag_val(itag+1))
             if (btest(pdefs%axsty(3)%extra, 0)) then
                pdefs%axsty(3)%minor = 1
                pdefs%axsty(3)%extra = ibclr(pdefs%axsty(3)%extra, 0)
             end if

          case('RMJ')
             pdefs%axsty(3)%major = gr_int_val(tag_val(itag+1))
          case('RMN')
             pdefs%axsty(3)%minor = gr_int_val(tag_val(itag+1))
          case('RNV')
             nxt = gr_int_val(tag_val(itag+1))
          case('RFM')
             call gr_str_val(inln, 'RFM', pdefs%axsty(3)%format)
             exit
          case('RLL')
             pdefs%axsty(3)%log_bands =gr_int_val(tag_val(itag+1), 3)
             
          case('RVL')
             if (nxt == 0) exit
             pdefs%axsty(3)%values = gr_dbl_val(tag_val(itag+1), nxt)

          case('RSG')
             pdefs%axsty(3)%grid = gr_int_val(tag_val(itag+1))
          case('RST')
             pdefs%axsty(3)%time = gr_int_val(tag_val(itag+1))
          case('RSZ')
             pdefs%axsty(3)%tzero = gr_int_val(tag_val(itag+1))
          case('RT')
             call gr_str_val(inln, 'RT', pdefs%axtitle(3))
             exit
          case('YIR')
             pdefs%y_right = gr_log_val(tag_val(itag+1))

          case('ZT')
             pdefs%ctable = gr_int_val(tag_val(itag+1))
          case('ZG')
             pdefs%gamma = gr_flt_val(tag_val(itag+1))
             if (pdefs%gamma == 0.) pdefs%gamma = 1.0

          case('DN')
             if (dflag) call gr_message( &
                  & "gr_get_asc: Datasets have already been defined"// &
                  & "these will be destroyed.")

             pdefs%nsets = gr_int_val(tag_val(itag+1))
             if (allocated(pdefs%data)) deallocate(pdefs%data)
             if (pdefs%nsets > 0) allocate(pdefs%data(pdefs%nsets))
             dflag = .true.

          case('DC')
             pdefs%cset = gr_int_val(tag_val(itag+1))+1_int16

          case('TN')
             if (tflag) call gr_message( &
                  & "gr_get_asc: Text strings have already been defined"// &
                  & "these will be destroyed.")

             pdefs%ntext = gr_int_val(tag_val(itag+1))
             if (allocated(pdefs%text)) deallocate(pdefs%text)
             if (pdefs%ntext > 0) allocate(pdefs%text(pdefs%ntext))
             tflag = .true.

          case('DS')
             rset = gr_int_val(tag_val(itag+1)) + 1_int16
             if (.not. dflag) then 
                call gr_message( &
                     & "gr_get_asc: Dataset read before number of sets defined", &
                     & type=GTK_MESSAGE_ERROR)
                stop
             end if

             data => pdefs%data(rset)
             call gr_get_ds_asc(data, unit)
             exit

          case('TS')
             tset = gr_int_val(tag_val(itag+1))+1
             if (.not. tflag) then 
                call gr_message( &
                     & "gr_get_asc: Text string read before number of sets"//&
                     & " defined", type=GTK_MESSAGE_ERROR)
                stop
             end if
             text => pdefs%text(tset)
             call gr_get_txt_asc(text, unit)
             exit

          case ('TTS')
             call gr_get_txt_asc(pdefs%text_options, unit)
             exit

          case('HC')
             pdefs%hardset%colour = gr_log_val(tag_val(itag+1))
          case('HE')
             pdefs%hardset%eps = gr_log_val(tag_val(itag+1))
          case('HO')
             pdefs%hardset%orient = gr_log_val(tag_val(itag+1))
          case('HP')
             pdefs%hardset%psize = gr_byt_val(tag_val(itag+1))
          case('HY')
             pdefs%hardset%cmyk = gr_log_val(tag_val(itag+1))
          case('HT')
             pdefs%hardset%timestamp = gr_log_val(tag_val(itag+1))
          case('HS' )
             pdefs%hardset%size = gr_flt_val(tag_val(itag+1), 2)
          case('HD' )
             pdefs%hardset%off = gr_flt_val(tag_val(itag+1), 2)

          case('HAB')
             call gr_str_val(inln, 'HAB',  pdefs%hardset%action(1))
             exit

          case('HAA')
             call gr_str_val(inln, 'HAA', pdefs%hardset%action(2))
             exit

          case('HVB')
             call gr_str_val(inln, 'HVB',  pdefs%hardset%viewer(1) )
             exit

          case('HVA')
             call gr_str_val(inln, 'HVA', pdefs%hardset%viewer(2))
             exit

          case('HPB')
             call gr_str_val(inln, 'HPB', pdefs%hardset%pdfviewer(1) )
             exit

          case('HPA')
             call gr_str_val(inln, 'HPA', pdefs%hardset%pdfviewer(2))
             exit

 
          case('HF')
             pdefs%hardset%font_family = gr_int_val(tag_val(itag+1))
          case('HWS')
             pdefs%hardset%font_wg_sl = gr_int_val(tag_val(itag+1))
          case('HFN')
             call gr_str_val(inln, 'HFN', pdefs%hardset%name)
             exit
          case('HPS')
             call gr_str_val(inln, 'HPS', pdefs%hardset%psdev)
             exit
          case('HEP')
             call gr_str_val(inln, 'HEP', pdefs%hardset%epsdev)
             exit
          case('HPD')
             call gr_str_val(inln, 'HPD', pdefs%hardset%pdfdev)
             exit
          case('HSV')
             call gr_str_val(inln, 'HSV', pdefs%hardset%svgdev)
             exit

          case('KU')
             pdefs%key%use = gr_log_val(tag_val(itag+1))
          case('KX')
             pdefs%key%x = gr_dbl_val(tag_val(itag+1), 2)
          case('KY')
             pdefs%key%y = gr_dbl_val(tag_val(itag+1), 2)
          case('KS')
             pdefs%key%csize = gr_dbl_val(tag_val(itag+1))
          case('KN')
             pdefs%key%norm = gr_int_val(tag_val(itag+1))
          case('KC')
             pdefs%key%cols = gr_int_val(tag_val(itag+1))
          case('KF')
             pdefs%key%frame = gr_log_val(tag_val(itag+1))
          case('KP')
             pdefs%key%one_point = gr_log_val(tag_val(itag+1))
          case('KT')
             call gr_str_val(inln, 'KT', pdefs%key%title)
             exit

          case('KLN')
             n_key_list = gr_lon_val(tag_val(itag+1))
          case('KL')
             if (allocated(pdefs%key%list)) deallocate(pdefs%key%list)
             allocate(pdefs%key%list(n_key_list))
             pdefs%key%list = gr_lon_val(tag_val(itag+1), n_key_list)
          case('REM')
             nrem = gr_lon_val(tag_val(itag+1))
             if (allocated(pdefs%remarks)) deallocate(pdefs%remarks)
             allocate(pdefs%remarks(nrem))
             do i = 1, nrem
                read(unit, "(a)") pdefs%remarks(i)
             end do


          end select
       end do
    end do

    call gr_set_changed(.false.)
    transient%backup = .false.
    pdefs%is_ascii = .true.

  end subroutine gr_get_asc

  subroutine gr_get_ds_asc(data, unit)
    type(graff_data), intent(inout) :: data
    integer, intent(in) :: unit

    ! Read an individual dataset from an ASCII Graffer file

    character(len=256) :: inln
    logical :: nflag, nflag2, tflag, dflag, jflag
    logical, dimension(4) :: cflag
    character(len=256), dimension(:), allocatable :: tag_val

    integer, parameter, dimension(0:*) :: elements = [2,3,4,3,4,4,5,5,6]
    integer :: ios, ntags, itag, ncols
    character(len=120) :: iom

    nflag = .false.
    nflag2 = .false.
    tflag = .false.
    dflag = .false.
    cflag = .false.
    jflag = .false.

    main: do
       read(unit, "(a)", iostat=ios, iomsg=iom) inln
       if (ios == iostat_end) return
       if (ios /= 0) then
          write(error_str, "(a/a)") &
               & "gr_get_ds_asc: error reading dataset from ascii", trim(iom)
          call gr_message(error_str)
          return
       end if

       call split(inln,':',tag_val, count=ntags)

       do itag = 1, ntags, 2
          select case(trim(tag_val(itag)))
          case('J')
             data%pline = gr_int_val(tag_val(itag+1))
             jflag = .true.

          case('P')
             data%psym = gr_int_val(tag_val(itag+1))
          case('S')
             data%symsize = gr_flt_val(tag_val(itag+1))
          case('L')
             data%line = gr_int_val(tag_val(itag+1))
          case('C')
             data%colour = gr_int_val(tag_val(itag+1))
          case('CV')
             data%c_vals = gr_int_val(tag_val(itag+1), 3)
          case('W')
             data%thick = gr_flt_val(tag_val(itag+1))
          case('O')
             data%sort = gr_log_val(tag_val(itag+1))
          case('K')
             data%noclip = gr_log_val(tag_val(itag+1))
          case('E')
             data%medit = gr_log_val(tag_val(itag+1))
          case('D')
             call gr_str_val(inln, 'D',  data%descript)
             exit

          case('MN')
             data%min_val = gr_dbl_val(tag_val(itag+1))
          case('MX')
             data%max_val = gr_dbl_val(tag_val(itag+1))
             
          case('N')
             data%ndata = gr_lon_val(tag_val(itag+1))
             nflag = .true.

          case('N2')
             data%ndata2 = gr_lon_val(tag_val(itag+1))
             nflag2 = .true.

          case('T')
             data%type = gr_int_val(tag_val(itag+1))
             tflag = .true.

          case('Y')
             data%y_axis = gr_int_val(tag_val(itag+1))

          case('M')
             data%mode = gr_int_val(tag_val(itag+1))

          case('ZF')
             data%zdata%format = gr_int_val(tag_val(itag+1))

          case('ZNL')
             data%zdata%n_levels = abs(gr_int_val(tag_val(itag+1)))
             cflag(1) = .true.

          case('ZL')
             if (cflag(1)) then 
                if (allocated(data%zdata%levels)) deallocate(data%zdata%levels)
                allocate(data%zdata%levels(data%zdata%n_levels))
                data%zdata%levels = &
                     & gr_dbl_val(tag_val(itag+1), int(data%zdata%n_levels))
             else
                call gr_message( "gr_get_ds_asc: contour level "// &
                     & "given without count - ignored")
             end if
          case('ZLL')
             if (cflag(1)) then 
                if (allocated(data%zdata%levels)) deallocate(data%zdata%levels)
                allocate(data%zdata%levels(data%zdata%n_levels))
                read(unit, *) data%zdata%levels 
                exit
             else
                call gr_message("gr_get_ds_asc: contour level "// &
                     & "given without count - ignored")
             end if

          case('ZNC')
             data%zdata%n_cols = gr_int_val(tag_val(itag+1))
             cflag(2) = .true.

          case('ZC')
             if (cflag(2)) then 
                if (allocated(data%zdata%colours)) &
                     & deallocate(data%zdata%colours)
                allocate(data%zdata%colours(data%zdata%n_cols))
                data%zdata%colours = gr_int_val(tag_val(itag+1), &
                     & int(data%zdata%n_cols))
             else 
                call gr_message("gr_get_ds_asc:  Contour colour "// &
                     & "list given without count - ignored")
             end if

          case('ZCR')
             if (cflag(2)) then 
                if (allocated(data%zdata%raw_colours)) &
                     & deallocate(data%zdata%raw_colours)
                allocate(data%zdata%raw_colours(3,data%zdata%n_cols))
                read(unit, *) data%zdata%raw_colours
                exit
             else 
                call gr_message("gr_get_ds_asc:  Contour colour "// &
                     & "list given without count - ignored")
             end if

          case('ZCL')
             if (cflag(2)) then 
                if (allocated(data%zdata%colours)) &
                     & deallocate(data%zdata%colours)
                allocate(data%zdata%colours(data%zdata%n_cols))
                read(unit, *) data%zdata%colours
                exit
             else 
                call gr_message("gr_get_ds_asc:  Contour colour "// &
                     & "list given without count - ignored")
             end if

          case('ZCT')
             data%zdata%ctable = gr_int_val(tag_val(itag+1))

          case('ZCG')
             data%zdata%gamma = gr_flt_val(tag_val(itag+1))
             if (data%zdata%gamma == 0.) data%zdata%gamma = 1.0

          case('ZNS')
             data%zdata%n_sty = gr_int_val(tag_val(itag+1))
             cflag(3) = .true.

          case('ZS')
             if (cflag(3)) then
                if (allocated(data%zdata%style)) deallocate(data%zdata%style)
                allocate(data%zdata%style(data%zdata%n_sty))
                data%zdata%style = gr_int_val(tag_val(itag+1), &
                     & int(data%zdata%n_sty))
             else 
                call gr_message("gr_get_ds_asc: Contour style "// &
                     & "list given without count - ignored")
             end if
          case('ZSL')
             if (cflag(3)) then
                if (allocated(data%zdata%style)) deallocate(data%zdata%style)
                allocate(data%zdata%style(data%zdata%n_sty))
                read(unit, *) data%zdata%style
                exit
             else 
                call gr_message("gr_get_ds_asc: Contour style "// &
                     & "list given without count - ignored")
             end if

          case('ZNT')
             data%zdata%n_thick = gr_int_val(tag_val(itag+1))
             cflag(4) = .true.

          case('ZT')
             if (cflag(4)) then
                if (allocated(data%zdata%thick)) deallocate(data%zdata%thick)
                allocate(data%zdata%thick(data%zdata%n_thick))
                data%zdata%thick = gr_int_val(tag_val(itag+1), &
                     & int(data%zdata%n_thick))
             else
                call gr_message("gr_get_ds_asc: Contour thickness "//&
                     & "list given without count - ignored")
             end if
         case('ZTL')
             if (cflag(4)) then
                if (allocated(data%zdata%thick)) deallocate(data%zdata%thick)
                allocate(data%zdata%thick(data%zdata%n_thick))
                read(unit, *) data%zdata%thick
                exit
             else
                call gr_message("gr_get_ds_asc: Contour thickness "//&
                     & "list given without count - ignored")
             end if

          case('ZCF')
             data%zdata%fill = gr_byt_val(tag_val(itag+1))
          case('ZLI')
             data%zdata%label = gr_int_val(tag_val(itag+1))
          case('ZLO')
             data%zdata%label_off = gr_int_val(tag_val(itag+1))
          case('ZCS')
             data%zdata%charsize = gr_flt_val(tag_val(itag+1))
          case('ZLM')
             data%zdata%lmap = gr_int_val(tag_val(itag+1))
             
          case('ZR')
             data%zdata%range = gr_dbl_val(tag_val(itag+1), 2)
          case('ZP')
             data%zdata%pxsize = gr_flt_val(tag_val(itag+1))
          case('ZIL')
             data%zdata%ilog = gr_int_val(tag_val(itag+1))
          case('ZIN')
             data%zdata%invert = gr_log_val(tag_val(itag+1))
          case('ZSM')
             data%zdata%smooth = gr_log_val(tag_val(itag+1))
          case('ZSN')
             data%zdata%shade_levels = gr_lon_val(tag_val(itag+1))
          case('ZM')
             data%zdata%missing = gr_dbl_val(tag_val(itag+1))

          case('R')
             if (.not. tflag) then 
                call gr_message("gr_get_ds_asc: Range found "// &
                     & "before type defined - ignored")
             else if (data%type >= 0) then 
                call gr_message( &
                     & "gr_get_ds_asc: Range found in XY data set - ignored")
             else if (data%type == -4) then 
                data%funct%range = &
                     & reshape(gr_dbl_val(tag_val(itag+1), 4), [2,2])
             else 
                data%funct%range(:,1) = gr_dbl_val(tag_val(itag+1), 2)
             end if
          case('F')
             if (.not. tflag) then 
                call gr_message("gr_get_ds_asc: Function found "// &
                     & "before type defined - ignored")
             else if (data%type >= 0) then 
                call gr_message("gr_get_ds_asc: Function found "// &
                     & "in XY data set - ignored")
             else if (data%type == -3) then 
                call gr_message("gr_get_ds_asc: Plain function "// &
                     & "found in parametric set - ignored")
             else 
                call gr_str_val(inln, 'F', data%funct%funct(1))
                exit
             end if
          case('FX')
             if (.not. tflag) then 
                call gr_message("gr_get_ds_asc: Function found "// &
                     & "before type defined - ignored")
             else if (data%type >= 0) then 
                call gr_message("gr_get_ds_asc: Function found "// &
                     & "in XY data set - ignored")
             else if (data%type /= -3) then
               call gr_message("gr_get_ds_asc: X function found "// &
                     & "in plain function - ignored")
             else 
                call gr_str_val(inln, 'FX', data%funct%funct(1))
                exit
             end if
          case('FY')
             if (.not. tflag) then 
                call gr_message("gr_get_ds_asc: Function found "// &
                     & "before type defined - ignored")
             else if (data%type >= 0) then 
                call gr_message("gr_get_ds_asc: Function found "// &
                     & "in XY data set - ignored")
             else if (data%type /= -3) then
                call gr_message("gr_get_ds_asc: Y function found "// &
                     & "in plain function - ignored")
             else 
                call gr_str_val(inln, 'FY', data%funct%funct(2))
                exit
             end if

          case('VS')
             if (.not. tflag) then
                call gr_message("gr_get_ds_asc: Data found "// &
                     & "before type defined - ignored")
                do
                   read(unit, '(a)') inln
                   if (index(inln, "VE:") > 0) exit
                end do
             else if (data%type < 0) then
                call gr_message("gr_get_ds_asc: Data found "// &
                     & "in function dataset - ignored")
                do
                   read(unit, '(a)') inln
                   if (index(inln, "VE:") > 0) exit
                end do
             else if (data%type > 8) then
                call gr_message("gr_get_ds_asc: 1-D Data "// &
                     & "found in 2-D dataset - ignored")
                do
                   read(unit, '(a)') inln
                   if (index(inln, "VE:") > 0) exit
                end do
             else
                ncols = gr_int_val(tag_val(itag+1))
                if (ncols /= elements(data%type)) &
                     & call gr_message("gr_get_ds_asc: WARNING "// &
                     & "Data columns wrong could get corrupt DS")
                if (allocated(data%xydata)) deallocate(data%xydata)
                allocate(data%xydata(elements(data%type), data%ndata))
                read(unit, *) data%xydata
                read(unit, "(a)") inln
                if (index(inln, 'VE:') == 0) then 
                   call gr_message("gr_get_ds_asc: WARNING Data "// &
                        & "rows wrong could get corrupt DS")
                end if
             end if

          case('ZXS')
             if (.not. tflag) then
                call gr_message("gr_get_ds_asc: Data found "// &
                     & "before type defined - ignored")
                do
                   read(unit, '(a)') inln
                   if (index(inln, "ZXE:") > 0) exit
                end do
             else if (data%type /= 9) then
                call gr_message("gr_get_ds_asc: 2-D Data found "//&
                     & "in function or 1-D dataset - ignored")
                do
                   read(unit, '(a)') inln
                   if (index(inln, "ZXE:") > 0) exit
                end do
             else
                if (allocated(data%zdata%x)) deallocate(data%zdata%x)
                if (data%zdata%x_is_2d) then 
                   allocate(data%zdata%x(data%ndata, data%ndata2))
                else
                   allocate(data%zdata%x(data%ndata, 1))
                end if
                read(unit, *) data%zdata%x
                read(unit, "(a)") inln
                if (index(inln, 'ZXE:') == 0) call gr_message( &
                     & "gr_get_ds_asc: WARNING Data X count wrong "// &
                     & "could get corrupt DS")
             end if

          case('ZYS')
             if (.not. tflag) then
                call gr_message("gr_get_ds_asc: Data found "// &
                     & "before type defined - ignored")
                do
                   read(unit, '(a)') inln
                   if (index(inln, "ZYE:") > 0) exit
                end do
             else if (data%type /= 9) then
               call gr_message("gr_get_ds_asc: 2-D Data found "//&
                     & "in function or 1-D dataset - ignored")
                do
                   read(unit, '(a)') inln
                   if (index(inln, "ZYE:") > 0) exit
                end do
             else
                if (allocated(data%zdata%y)) deallocate(data%zdata%y)
                if (data%zdata%y_is_2d) then 
                   allocate(data%zdata%y(data%ndata, data%ndata2))
                else
                   allocate(data%zdata%y(1, data%ndata2))
                end if
                read(unit, *) data%zdata%y
                read(unit, "(a)") inln
                if (index(inln, 'ZYE:') == 0) call gr_message( &
                     & "gr_get_ds_asc: WARNING Data Y count wrong "// &
                     & "could get corrupt DS")
             end if

          case('ZZS')
             if (.not. tflag) then
                call gr_message("gr_get_ds_asc: Data found "// &
                     & "before type defined - ignored")
                do
                   read(unit, '(a)') inln
                   if (index(inln, "ZZE:") > 0) exit
                end do
             else if (data%type /= 9) then
                call gr_message("gr_get_ds_asc: 2-D Data found "//&
                     & "in function or 1-D dataset - ignored")
                do
                   read(unit, '(a)') inln
                   if (index(inln, "ZZE:") > 0) exit
                end do
             else
                if (allocated(data%zdata%z)) deallocate(data%zdata%z)
                allocate(data%zdata%z(data%ndata, data%ndata2))
                read(unit, *) data%zdata%z
                read(unit, "(a)") inln
                if (index(inln, 'ZZE:') == 0) call gr_message( &
                     & "gr_get_ds_asc: WARNING Data Z count wrong "// &
                     & "could get corrupt DS")
             end if

          case('ZX2')
             data%zdata%x_is_2d = gr_log_val(tag_val(itag+1))
          case('ZY2')
             data%zdata%y_is_2d = gr_log_val(tag_val(itag+1))

          case('DE')
             exit main

          case default
             call gr_message( &
                  & "gr_get_ds_asc: Unknown Dataset tag "//tag_val(itag)//&
                  & " - ignored")
          end select
       end do
    end do main

    ! If min_val and max_val are both zero then they should both be NaN

    if (data%min_val == 0._real64 .and. data%max_val == 0._real64) then
       data%min_val = d_nan()
       data%max_val = d_nan()
    end if

 
    if (.not. jflag) then
       select case(data%psym)
       case(10)
          data%pline = 2
          data%psym = 0
       case(0)
          data%pline = 1
       case(:-1)
          data%pline = 1
          data%psym = abs(data%psym)
       case default
          data%pline = 0
       end select
    end if
  end subroutine gr_get_ds_asc

  subroutine gr_get_txt_asc(text, unit)
    type(graff_text), intent(inout) :: text
    integer, intent(in) :: unit

    ! Read a text annotation from an ASCII Graffer file.

    character(len=256) :: inln
    character(len=256), dimension(:), allocatable :: tag_val
    logical :: ffflag
    integer :: ios, itag, ntags
    character(len=120) :: iom

    ffflag = .false.

    do
       read(unit, "(a)", iostat=ios, iomsg=iom) inln
       if (ios == iostat_end) return
       if (ios /= 0) then
          write(error_str, "(a/t10,a)") &
               & "gr_get_txt_asc: error reading dataset from ascii", trim(iom)
          call gr_message(error_str(:2), type=GTK_MESSAGE_ERROR)
          return
       end if

       call split(inln,':',tag_val, count=ntags)

       do itag = 1, ntags, 2
          select case(trim(tag_val(itag)))

          case('C')
             text%colour = gr_int_val(tag_val(itag+1))
          case('CV')
             text%c_vals = gr_int_val(tag_val(itag+1), 3)
          case('S')
             text%size = gr_flt_val(tag_val(itag+1))
          case('O')
             text%orient = gr_flt_val(tag_val(itag+1))
          case('A')
             text%align = gr_flt_val(tag_val(itag+1))
          case('FF')
             text%ffamily = gr_int_val(tag_val(itag+1))
             ffflag = .true.

          case('F')
             text%font = gr_int_val(tag_val(itag+1))
             ! Possible IDL remapping goes here

          case('W')
             text%thick = gr_flt_val(tag_val(itag+1))
          case('X')
             text%x = gr_dbl_val(tag_val(itag+1))
          case('Y')
             text%y = gr_dbl_val(tag_val(itag+1))

          case('N')
             text%norm = gr_int_val(tag_val(itag+1))

          case('T')
             call gr_str_val(inln, 'T', text%text)
             exit

          case('TID')
             call gr_str_val(inln, 'TID', text%id)
             exit

          case('AX')
             text%axis = gr_int_val(tag_val(itag+1))

          case('TE', 'TTE')
             return

          case default
             call gr_message( &
                  & "gr_get_txt_asc: Unknown text tag "//tag_val(itag)//&
                  & " - ignored")

          end select
       end do
    end do
  end subroutine gr_get_txt_asc

  subroutine gr_save_asc(ok)
    logical, intent(out) :: ok

    ! Write an ASCII Graffer file.

    character(len=256) :: outfile
    integer :: ios, unit, nvals, i
    character(len=120) :: iom, vfmt
    character(len=28) :: date
    type(graff_data), pointer :: data
    type(graff_text), pointer :: text

    ok = .true.

    outfile = trim(pdefs%dir)//trim(pdefs%name)

    if (.not. transient%backup .and. file_exists(outfile)) then
       call execute_command_line("cp "//trim(outfile)//" "//trim(outfile)//"~")
       transient%backup = .true.
    end if

    open(newunit=unit, file=outfile, form='formatted', action='write', &
         & status='replace', iostat=ios, iomsg=iom)
    if (ios /= 0) then
       write(error_str, "(2a/t10,a)") &
            & "gr_save_asc: Failed to open output file: ", trim(outfile), &
            & trim(iom)
       call gr_message(error_str, type=GTK_MESSAGE_ERROR)
       ok = .false.
       return
    end if

    call gr_date(date)
    write(unit, "('Graffer V',I2,'.',I2.2,': ', 2A,' : @',A)") &
         & pdefs%version, trim(pdefs%dir), trim(pdefs%name), date !, time, zone

    write(unit, "(2a)") "GT:", trim(pdefs%title)
    write(unit, "(2a)") "GS:", trim(pdefs%subtitle)

    write(unit, "(a,f8.4,a,f7.3)") 'GC:', pdefs%charsize, ':GA:', pdefs%axthick
    write(unit, "(a,4f8.5)") "GP:", pdefs%position
    write(unit, "(a,F9.5,f8.5)") "GR:", pdefs%aspect
    write(unit, "(2(a,i1))") "GI:", f_c_logical(pdefs%isotropic), ":GHA:", &
         &f_c_logical(pdefs%match)

    write(unit, "(a,2(g0,' '))") "XR:",pdefs%axrange(:,1)
    write(unit, "(a,i0,5(a,i0))") "XL:", pdefs%axtype(1), ":XSI:", &
         & pdefs%axsty(1)%idl, ":XSE:", pdefs%axsty(1)%extra, &
         & ":XSG:", pdefs%axsty(1)%grid, ":XST:", pdefs%axsty(1)%time, &
         & ":XSZ:", pdefs%axsty(1)%tzero
    write(unit, "(a,i0,a,i0)") "XMJ:", pdefs%axsty(1)%major, &
         &  ":XMN:", pdefs%axsty(1)%minor
    write(unit, "(2a)") "XFM:", trim(pdefs%axsty(1)%format)
    write(unit, "(a,3(i0,' '))") "XLL:", pdefs%axsty(1)%log_bands
    
    if (allocated(pdefs%axsty(1)%values)) then
       nvals = size(pdefs%axsty(1)%values)
       write(unit, "(a,i0)") "XNV:", nvals
       write(vfmt, "('(a,',i0,'(g0,1x))')") nvals
       write(unit, vfmt) "XVL:", pdefs%axsty(1)%values
    end if

    write(unit, "(2a)") "XT:", pdefs%axtitle(1)

    write(unit, "(a,i0)") 'YIR:', f_c_logical(pdefs%y_right)

    write(unit, "(a,2(g0,' '))") "YR:",pdefs%axrange(:,2)
    write(unit, "(a,i0,5(a,i0))") "YL:", pdefs%axtype(2), ":YSI:", &
         & pdefs%axsty(2)%idl, ":YSE:", pdefs%axsty(2)%extra, &
         & ":YSG:", pdefs%axsty(2)%grid, ":YST:", pdefs%axsty(2)%time, &
         & ":YSZ:", pdefs%axsty(2)%tzero
    write(unit, "(a,i0,a,i0)") "YMJ:", pdefs%axsty(2)%major, &
         & ":YMN:", pdefs%axsty(2)%minor
    write(unit, "(2a)") "YFM:", trim(pdefs%axsty(2)%format)
    write(unit, "(a,3(i0,' '))") "YLL:", pdefs%axsty(2)%log_bands
    
    if (allocated(pdefs%axsty(2)%values)) then
       nvals = size(pdefs%axsty(2)%values)
       write(unit, "(a,i0)") "YNV:", nvals
       write(vfmt, "('(a,',i0,'(g0,1x))')") nvals
       write(unit, vfmt) "YVL:", pdefs%axsty(2)%values
    end if

    write(unit, "(2a)") "YT:", pdefs%axtitle(2)

    write(unit, "(a,2(g0,' '))") "RR:",pdefs%axrange(:,3)
    write(unit, "(a,i0,5(a,i0))") "RL:", pdefs%axtype(3), ":RSI:", &
         & pdefs%axsty(3)%idl, ":RSE:", pdefs%axsty(3)%extra, &
         & ":RSG:", pdefs%axsty(3)%grid, ":RST:", pdefs%axsty(3)%time, &
         & ":RSZ:", pdefs%axsty(3)%tzero
    write(unit, "(a,i0,a,i0)") "RMJ:", pdefs%axsty(3)%major&
         &, ":RMN:", pdefs%axsty(3)%minor
    write(unit, "(2a)") "RFM:", trim(pdefs%axsty(3)%format)
    write(unit, "(a,3(i0,' '))") "RLL:", pdefs%axsty(3)%log_bands
    
    if (allocated(pdefs%axsty(3)%values)) then
       nvals = size(pdefs%axsty(3)%values)
       write(unit, "(a,i0)") "RNV:", nvals
       write(vfmt, "('(a,',i0,'(g0,1x))')") nvals
       write(unit, vfmt) "RVL:", pdefs%axsty(3)%values
    end if

    write(unit, "(2a)") "RT:", pdefs%axtitle(3)

    write(unit, "(a,i0,a,f7.3)") 'ZT:', pdefs%ctable, ':ZG:', pdefs%gamma
    write(unit, "(2(a,i0))") 'DN:', pdefs%nsets, ':DC:', pdefs%cset-1_int16

    do i = 1, pdefs%nsets
       data => pdefs%data(i)

       write(unit, "(a,I0)") "DS:", i-1
       write(unit, "(2a)") "D:", trim(data%descript)
       write(unit, "(5(a,i0))") "T:", data%type, ":M:", data%mode, &
            & ":Y:", data%y_axis, ":N:", data%ndata, ":N2:", data%ndata2

       write(unit, "(2(a,i0),a,f8.3)") "J:", data%pline, ":P:", &
            & data%psym, ":S:", data%symsize
       write(unit, "(2(a,i0),a,f8.4,3(a,i0))") "L:", data%line, &
            & ":C:", data%colour, ":W:", data%thick, &
            & ":O:", f_c_logical(data%sort), ":K:", f_c_logical(data%noclip), &
            & ":E:", f_c_logical(data%medit)
       if (data%colour == -2) write(unit, "(a,3i5)") 'CV:', data%c_vals
       write(unit, "(2(a,g0))") "MN:", data%min_val, ":MX:", data%max_val
       
       if (data%ndata > 0) then
          select case(data%type)
          case(0:8)
             nvals = size(data%xydata, 1)
             write(vfmt, "('(',I0,'(g0,1x))')") nvals
             write(unit, "(a,i0)") "VS:", nvals
             write(unit, vfmt) data%xydata
             write(unit, "(a)") "VE:"

          case(9)
             write(unit, "(2a,i0)") "ZX2:", f_c_logical(data%zdata%x_is_2d), &
                  & ":ZY2:", f_c_logical(data%zdata%y_is_2d)

             write(unit, "(a)") "ZXS:"
             write(unit, "(6(g0,1x))") data%zdata%x
             write(unit, "(a)") "ZXE:"
             write(unit, "(a)") "ZYS:"
             write(unit, "(6(g0,1x))") data%zdata%y
             write(unit, "(a)") "ZYE:"
             write(unit, "(a)") "ZZS:"
             write(unit, "(6(g0,1x))") data%zdata%z
             write(unit, "(a)") "ZZE:"

          case(-1,-2)
             write(unit, "(a,2(g0,1x))") "R:", data%funct%range(:,1)
             write(unit, "(2a)") "F:", trim(data%funct%funct(1))
          case(-3)
             write(unit, "(a,2(g0,1x))") "R:", data%funct%range(:,1)
             write(unit, "(2a)") "FX:", trim(data%funct%funct(1)), &
                  & "FY:", trim(data%funct%funct(2))
          case(-4)
             write(unit, "(a,4(g0,1x))") "R:", data%funct%range
             write(unit, "(2a)") "F:", trim(data%funct%funct(1))
          end select

          if (data%type == -4 .or. data%type == 9) then
             write(unit, "(5(a,i0))") "ZF:", data%zdata%format, &
                  & ':ZNL:', data%zdata%n_levels, &
                  & ':ZNC:', data%zdata%n_cols, &
                  & ':ZNS:', data%zdata%n_sty, &
                  & ':ZNT:', data%zdata%n_thick
             write(unit, "(5(a,i0))") "ZCF:", data%zdata%fill, &
                  & ":ZCT:", data%zdata%ctable, &
                  & ":ZLI:", data%zdata%label, &
                  & ":ZLO:", data%zdata%label_off, &
                  & ":ZLM:", data%zdata%lmap
             write(unit, "(2(a,f7.3))") ":ZCS:", data%zdata%charsize, &
                  & ":ZCG:", data%zdata%gamma

             if (data%zdata%set_levels .and. allocated(data%zdata%levels)) then
                if (data%zdata%n_levels <= 5) then
                   write(unit, "(a,5(g0,1x))") 'ZL:', data%zdata%levels
                else
                   write(unit, "(a)") 'ZLL:'
                   write(unit, "(5(g0,1x))") data%zdata%levels
                end if
             end if

             if (allocated(data%zdata%colours)) then
                if (data%zdata%n_cols <= 20) then
                   write(unit, "(a,20i4)") "ZC:", data%zdata%colours
                else
                   write(unit, "(a)") "ZCL:"
                   write(unit, "(20i4)") data%zdata%colours
                end if
             end if

             if (allocated(data%zdata%raw_colours)) then
                write(unit, "(a)") "ZCR:"
                write(unit, "(15i5)") data%zdata%raw_colours
             end if
             
             if (allocated(data%zdata%style)) then
                if (data%zdata%n_sty <= 20) then
                   write(unit, "(a,20i4)") "ZS:", data%zdata%style
                else
                   write(unit, "(a)") "ZSL:"
                   write(unit, "(20i4)") data%zdata%style
                end if
             end if

             if (allocated(data%zdata%thick)) then
                if (data%zdata%n_cols <= 10) then
                   write(unit, "(a,10f7.3)") "ZT:", data%zdata%thick
                else
                   write(unit, "(a)") "ZTL:"
                   write(unit, "(10f7.3)") data%zdata%thick
                end if
             end if

             write(unit, "(a,2(g0,1x),a,g0)") "ZR:", data%zdata%range, &
                  & ":ZM:", data%zdata%missing
             write(unit, "(a,f7.3, 4(a,i0))") "ZP:", data%zdata%pxsize, &
                  & ":ZIL:", data%zdata%ilog, ":ZIN:", &
                  & f_c_logical(data%zdata%invert), ':ZSM:', &
                  & f_c_logical(data%zdata%smooth), ':ZSN:', &
                  & data%zdata%shade_levels
          end if
       end if
       write(unit, "(a)") "DE:"
    end do

    write(unit, "(a,i0)") "TN:", pdefs%ntext

    do i = 1, pdefs%ntext
       text => pdefs%text(i)

       write(unit, "(a,i0)") "TS:", i-1
       write(unit, "(2a)") "TID:", text%id
       write(unit, "(2a)") "T:", text%text

       write(unit, "(2(a,g0),2(a,i0))") "X:", text%x, ':Y:', text%y, &
            & ":N:", text%norm, ":AX:", text%axis

       write(unit, "(a,i0,a,f8.3,a,f9.4, a,f8.5)") "C:", text%colour, &
            & ":S:", text%size, ":O:", text%orient, ":A:", text%align
       if (text%colour == -2) write(unit, "(a,3i5)") 'CV:', text%c_vals
       
       write(unit, "(2(a,i0), a,f7.2)") "FF:", text%ffamily, &
            & ":F:", text%font, ":W:", text%thick
       write(unit, "(a)") "TE:"
    end do

    text => pdefs%text_options
    write(unit, "(a,i0)") "TTS:"
    write(unit, "(a,i0,a,f8.3,a,f9.4, a,f8.5)") "C:", text%colour, &
         & ":S:", text%size, ":O:", text%orient, ":A:", text%align
    if (text%colour == -2) write(unit, "(a,3i5)") 'CV:', text%c_vals
    
     write(unit, "(2(a,i0), a,f7.2)") "FF:", text%ffamily, &
         & ":F:", text%font, ":W:", text%thick
    write(unit, "(a)") "TTE:"

    write(unit, "(5(a,i0))") "KU:", f_c_logical(pdefs%key%use), &
         & ":KN:", pdefs%key%norm, ":KC:", pdefs%key%cols, &
         & ":KF:", f_c_logical(pdefs%key%frame), ":KP:", &
         & f_c_logical(pdefs%key%one_point)

    write(unit, "(2(a,2(g0,1x)),a,g0)") "KX:", pdefs%key%x, ":KY:", &
         & pdefs%key%y, ":KS:", pdefs%key%csize

    write(unit, "(2a)") "KT:", trim(pdefs%key%title)

    if (allocated(pdefs%key%list)) then
       write(unit, "(a,i0)") "KLN:", size(pdefs%key%list)
       write(vfmt, "('(a,',i0,'(i0,1x))')") size(pdefs%key%list)
       write(unit, vfmt) "KL:", pdefs%key%list
    end if

    write(unit, "(6(a,i0))") "HC:", f_c_logical(pdefs%hardset%colour), &
         & ":HE:", f_c_logical(pdefs%hardset%eps), &
         & ":HP:", pdefs%hardset%psize, &
         & ":HO:", f_c_logical(pdefs%hardset%orient), ":HT:", &
         & f_c_logical(pdefs%hardset%timestamp), ":HY:", &
         & f_c_logical(pdefs%hardset%cmyk)

    write(unit, "(2(a,2(g0,1x)))") "HS:", pdefs%hardset%size, ":HD:", &
         &pdefs%hardset%off

    write(unit, "(2a)") "HAB:", pdefs%hardset%action(1), &
         & "HAA:", pdefs%hardset%action(2), &
         & "HVB:", pdefs%hardset%viewer(1), &
         & "HVA:", pdefs%hardset%viewer(2), &
         & "HPB:", pdefs%hardset%pdfviewer(1), &
         & "HPA:", pdefs%hardset%pdfviewer(2)

    write(unit, "(2(a,i0))") "HF:", pdefs%hardset%font_family, &
         & ":HWS:", pdefs%hardset%font_wg_sl
    write(unit, "(2a)") "HFN:", trim(pdefs%hardset%name)
    write(unit, "(2a)") "HPS:", trim(pdefs%hardset%psdev)
    write(unit, "(2a)") "HEP:", trim(pdefs%hardset%epsdev)
    write(unit, "(2a)") "HPD:", trim(pdefs%hardset%pdfdev)
    write(unit, "(2a)") "HSV:", trim(pdefs%hardset%svgdev)

    if (allocated(pdefs%remarks)) then
       write(unit, "(a,i0)") "REM:", size(pdefs%remarks)
       write(unit, "(a)") pdefs%remarks
    end if

    close(unit)

    call gr_set_changed(.false.)
    pdefs%is_ascii = .true.

  end subroutine gr_save_asc
end module gr_file_ascii
