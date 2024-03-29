! Copyright (C) 2013-2021
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

module gr_plot
  ! Top level plotting routines.

  use iso_fortran_env
  use iso_c_binding

  use plplot
  use plplot_extra

  use gtk_hl
  use gtk_draw_hl
  use gtk_sup

  use gtk, only: gtk_widget_queue_draw, gtk_widget_set_sensitive

  use graff_types
  use graff_globals
  use gr_plot_utils
  use gr_plot_tools
  use gr_plot_procs
  use graff_init
  use gr_colours
  
  use gr_eval
  use gr_sort
  use gr_text_utils
  use gr_shading
  use gr_utils
  
  use ieee_arithmetic, only: ieee_is_finite
  
  implicit none

  real(kind=plflt), parameter :: cm2pt = 72._plflt/2.54_plflt

  type(c_ptr), private :: cc = c_null_ptr  ! The cairo context for the drawing
  type(c_ptr), private :: plotting_area = c_null_ptr ! The drawing area
  logical, private :: gr_plot_is_open = .false., gr_is_widget

  real(kind=plflt) :: xprev, yprev
!!  character(len=80) :: selected_device
  character(len=160), private :: error_str

  character(len=120), private :: local_name
  
contains

  subroutine gr_plot_open(device, area)
    character(len=*), intent(in), optional :: device
    type(c_ptr), optional :: area

    ! Open a plplot stream for plotting.

    integer(kind=c_int) :: width, height
    character(len=20) :: geometry
    character(len=16) :: driver
    character(len=120) :: dname

    integer :: pdot
    real(kind=plflt) :: page_aspect
    type(graff_hard), pointer :: hardset
    logical :: isd
    integer :: plrc

    if (gr_plot_is_open) return

    hardset => pdefs%hardset

    plrc = plparseopts(PL_PARSE_SKIP)

    call gr_line_colours()

    if (present(device)) then
       if (hardset%name == '') then
          pdot = index(pdefs%name, '.', back=.true.)
          if (pdot == 0) then
             local_name = pdefs%name
          else
             local_name = pdefs%name(:pdot-1)
          end if
       else
          pdot = index(hardset%name, '.', back=.true.)
          if (pdot == 0) then
             local_name = hardset%name
          else
             local_name = hardset%name(:pdot-1)
          end if

          pdot = index(local_name, '/', back=.true.)
          
          ! If the name has a directory, check that is exists and if
          ! not then strip it.
          if (pdot /= 0) then
             dname = local_name(:pdot)
             inquire(file=dname, exist=isd)
             if (.not. isd) then
                local_name = local_name(pdot+1:)
                hardset%name = local_name
                if (index(pdefs%dir, '/', back=.true.) == &
                     & len_trim(pdefs%dir)) then
                   local_name = trim(pdefs%dir) // trim(local_name)
                else
                   local_name = trim(pdefs%dir) // '/' // trim(local_name)
                end if
             end if
          end if
       end if

       select case (device)
       case('ps')
          if (hardset%psdev == '') then
             call gr_default_device('ps', driver)
          else
             driver = hardset%psdev
          end if
!!$          if (hardset%psize == 0) then     ! A4
!!$             page_aspect = 297._plflt/210._plflt
!!$          else
!!$             page_aspect = 11._plflt/8.5_plflt
!!$          end if
          call plsdev(driver)
!!$          if (hardset%orient) then
!!$             call plsdidev (0._plflt, page_aspect, 0._plflt, 0._plflt)
!!$          else
!!$             call plsdidev (0._plflt, 1./page_aspect, 0._plflt, 0._plflt)
!!$          end if
          page_aspect = hardset%size(1)/hardset%size(2)

          call plsori(1)
          call plsdidev(0._plflt, page_aspect, 0._plflt, 0._plflt)

          ! Since the drivers seem to be ignoring the offset,
          ! at least pro tem we will not use it.
          
          call plspage(0._plflt, 0._plflt, &
               & int(hardset%size(2)*cm2pt), &
               & int(hardset%size(1)*cm2pt), &
               & 0, 0)
!!$               & int(hardset%off(2)*cm2pt), &
!!$               & int(hardset%off(1)*cm2pt))

          call plsfnam(trim(local_name)//'.ps')
          
       case('eps')
          if (hardset%epsdev == '') then
             call gr_default_device('eps', driver)
          else
             driver = hardset%epsdev
          end if
          page_aspect = hardset%size(1)/hardset%size(2)

          call plsdev(driver)

          ! This slightly bizarre looking page set up is needed to avoid
          ! rotated plots.

          call plsori(1)
          call plsdidev(0._plflt, page_aspect, 0._plflt, 0._plflt)
          call plspage(0._plflt, 0._plflt, &
               & int(hardset%size(2)*cm2pt), &
               & int(hardset%size(1)*cm2pt), &
               & 0, 0)
          call plsfnam(trim(local_name)//'.eps')
          
       case('pdf')
          if (hardset%pdfdev == '') then
             call gr_default_device('pdf', driver)
          else
             driver = hardset%pdfdev
          end if

          if (hardset%psize == 0) then     ! A4
             page_aspect = 297._plflt/210._plflt
          else
             page_aspect = 11._plflt/8.5_plflt
          end if
          call plsdev(driver)
!!$          if (hardset%orient) then
!!$             call plsdidev (0._plflt, page_aspect, 0._plflt, 0._plflt)
!!$          else
!!$             call plsdidev (0._plflt, 1./page_aspect, 0._plflt, 0._plflt)
!!$          end if

          ! Since the drivers seem to be ignoring the offset,
          ! at least pro tem we will not use it.
           
          call plspage(0._plflt, 0._plflt, &
               & int(hardset%size(1)*cm2pt), &
               & int(hardset%size(2)*cm2pt), &
               & 0, 0)
!!$               & int(hardset%off(1)*cm2pt), &
!!$               & int(hardset%off(2)*cm2pt))
          call plsfnam(trim(local_name)//'.pdf')

       case('epdf')
          if (hardset%pdfdev == '') then
             call gr_default_device('pdf', driver)
          else
             driver = hardset%pdfdev
          end if

          page_aspect = hardset%size(1)/hardset%size(2)

          call plsdev(driver)
          call plsori(0)
!!$          call plsdidev(0._plflt, page_aspect, 0._plflt, 0._plflt)
          call plspage(0._plflt, 0._plflt, &
               & int(hardset%size(1)*cm2pt), &
               & int(hardset%size(2)*cm2pt), &
               & 0, 0)
          call plsfnam(trim(local_name)//'.pdf')
          
       case('svg')
          if (hardset%svgdev == '') then
             call gr_default_device('svg', driver)
          else
             driver = hardset%svgdev
          end if
          page_aspect = hardset%size(1)/hardset%size(2)

          call plsdev(driver)
          call plsori(0)
!!$          call plsdidev(0._plflt, page_aspect, 0._plflt, 0._plflt)
          call plspage(0._plflt, 0._plflt, &
               & int(hardset%size(1)*cm2pt), &
               & int(hardset%size(2)*cm2pt), &
               & 0, 0)
          call plsfnam(trim(local_name)//'.svg')
       end select

       call plscolor(f_c_logical(hardset%colour))
       call plinit()

       gr_is_widget = .false.
       transient%selected_device = device
    else
       if (present(area)) then
          plotting_area = area
       else
          plotting_area = gr_drawing_area
       end if

       if (.not. c_associated(plotting_area)) return

       call plsdev("extcairo")
       plrc = plsetopt("drvopt", "set_background=1")

       call hl_gtk_drawing_area_get_size(plotting_area, &
            & width=width, height=height)
       write(geometry, "(I0,'x',I0)") width, height
       plrc = plsetopt("geometry", geometry)

       call plscolor(1)
       call plinit()
       cc = hl_gtk_drawing_area_cairo_new(plotting_area)
       call pl_cmd(PLESC_DEVINIT, cc)
       gr_is_widget = .true.
!!$       call plxormod(.false., status)
!!$       call gtk_widget_set_sensitive(xhair_but, f_c_logical(status))
       transient%selected_device = 'widget'
    end if
    call plfontld(1)
    gr_plot_is_open = .true.

!    call plsesc(ichar('!'))

  end subroutine gr_plot_open

  subroutine gr_set_plw(drawing, update, csize)
    type(c_ptr), intent(in), optional :: drawing
    logical, intent(in), optional :: update
    real(kind=plflt), intent(in), optional :: csize

    ! Select which plot window

    logical :: changed

    call gr_plot_close()
    if (present(drawing)) then
       call gr_plot_open(area=drawing)
       call plbop
       if (present(csize)) call plschr(csize, sysopts%charscale)    !1._plflt)
       call plvpor(0._plflt, 1._plflt, 0._plflt, 1._plflt)
       call plwind(0._plflt, 1._plflt, 0._plflt, 1._plflt)
    else
       if (present(update)) then
          changed = update
       else 
          changed = .false.
       end if
       call gr_plot_open()
       call gr_plot_draw(changed)
    end if
  end subroutine gr_set_plw


  subroutine gr_plot_close()

    ! Close a plplot stream and dispose of the output.

    type(graff_hard), pointer :: hardset
    integer(kind=c_int) :: iresp
    
    hardset => pdefs%hardset


    if (.not. gr_plot_is_open) return
    if (hardset%timestamp .and. .not. gr_is_widget) &
         & call gr_stamp
    if (gr_is_widget) call hl_gtk_drawing_area_cairo_destroy(cc)
    call plend
    gr_plot_is_open = .false.

    if (transient%selected_device == 'ps' .and. hardset%action(1) /= '') then
       if (hardset%prompt(1)) then
          iresp = hl_gtk_message_dialog_show( &
               & ["Print or view file with " // &
               &  trim(hardset%action(1))//"?"], &
               & GTK_BUTTONS_YES_NO, type=GTK_MESSAGE_QUESTION)
       else
          iresp = GTK_RESPONSE_YES
       end if
       if (iresp == GTK_RESPONSE_YES) &
            & call execute_command_line(trim(hardset%action(1))//' '//&
            & trim(local_name)//'.ps '//trim(hardset%action(2)))

    else if (transient%selected_device == 'eps' .and. &
         & hardset%viewer(1) /= '') then
       if (hardset%prompt(2)) then
          iresp = hl_gtk_message_dialog_show( &
               & ["View file with "// &
               &  trim(hardset%viewer(1))//"?"], &
               & GTK_BUTTONS_YES_NO, type=GTK_MESSAGE_QUESTION)
       else
          iresp = GTK_RESPONSE_YES
       end if
       if (iresp == GTK_RESPONSE_YES) &
            & call execute_command_line(trim(hardset%viewer(1))//' '//&
            & trim(local_name)//'.eps '//&
            & trim(hardset%viewer(2)), &
            & wait=.false.)

    else if ((transient%selected_device == 'pdf' .or. &
         & transient%selected_device == 'epdf') .and. &
         & hardset%pdfviewer(1) /= '') then
       if (hardset%prompt(3)) then
          iresp = hl_gtk_message_dialog_show( &
               & ["View file with "// &
               & trim(hardset%pdfviewer(1))//"?"], &
               & GTK_BUTTONS_YES_NO, type=GTK_MESSAGE_QUESTION)
       else
          iresp = GTK_RESPONSE_YES
       end if
       if (iresp == GTK_RESPONSE_YES) &
            & call execute_command_line(trim(hardset%pdfviewer(1))//' '//&
            & trim(local_name)//'.pdf '//&
            & trim(hardset%pdfviewer(2)), &
            & wait=.false.)
    end if

    transient%selected_device = ''
  end subroutine gr_plot_close

  subroutine gr_plot_draw(ichange)
    logical, intent(in) :: ichange

    ! Plot a Graffer file.

    real(kind=plflt) :: x0, x1, y0, y1, r0, r1
    integer :: i
    logical :: ok

    if (.not. gr_plot_is_open) call gr_plot_open
    if (.not. gr_plot_is_open) return

    ! N.B. Don't cancel any other change flag that may have been set.
    if (ichange) call gr_set_changed(.true.)
    
    if (pdefs%hardset%font_family <= 0 .or. &
         & pdefs%hardset%font_family > size(font_list)) &
         & pdefs%hardset%font_family = 1_int16
    if (pdefs%hardset%font_wg_sl <= 0 .or. &
         & pdefs%hardset%font_wg_sl > size(font_shape)) &
         &pdefs%hardset%font_wg_sl = 1_int16

    call plsfont(font_list(pdefs%hardset%font_family), &
         & font_shape(pdefs%hardset%font_wg_sl), &
         & font_weight(pdefs%hardset%font_wg_sl))

    ok = .true.
    call gr_axis_range(1, x0, x1, ok)
    call gr_axis_range(2, y0, y1, ok)
    pdefs%transform%world(:,1) = [x0, x1, y0, y1]
    if (pdefs%y_right) then
       call gr_axis_range(3, r0, r1, ok)
       pdefs%transform%world(:,2) = [x0, x1, r0, r1]
    end if

    if (.not. ok) then
       call gr_message("gr_plot_draw: Invalid axis ranges")
       return
    end if

    call gr_viewport(pdefs%transform%viewport, pdefs%transform%vp_aspect)
    pdefs%transform%viewport_enabled=.false.
    pdefs%transform%is_initialized = .true.

    call plbop()

    call gr_plot_transform(index=1, full=.false._int8)

    ! First the datasets

    do i = 1, pdefs%nsets
       if (transient%current_only .and. &
            & (i /= pdefs%cset .or. transient%mode == 1)) cycle

       select case (pdefs%data(i)%type)
       case(0:8)
          call gr_1dd_plot(i)
       case(-3:-1)
          call gr_1df_plot(i)
       case(9)
          if (.not. sysopts%s2d .or. .not. gr_is_widget) call gr_2dd_plot(i)
       case(-4)
          if (.not. sysopts%s2d .or. .not. gr_is_widget) call gr_2df_plot(i)
       case default
          write(error_str, "(a,i0,a)")"gr_plot_draw: ",&
               &  pdefs%data(i)%type, "is invalid"
          call gr_message(error_str)
       end select
    end do

    ! The text annotations

    if (.not. transient%current_only .or. transient%mode == 1) then
       do i = 1, pdefs%ntext
          call gr_text_draw(i, anchor=gr_is_widget .and. &
               & transient%mode == 1)
       end do
    end if

    ! Draw the axes last

    call gr_axis_plot

    if (pdefs%key%use) call gr_key_draw

    call gtk_widget_queue_draw(plotting_area)

    ! Set the proper world transform for the cursor tracking
    call gr_plot_transform(dataset=0, full=.false._int8)

    xprev = -1._plflt
    yprev = -1._plflt

  end subroutine gr_plot_draw

  subroutine gr_axis_plot

    ! Plot the axes.

    character(len=20) :: xopt, yopt, ropt
    integer :: xminor, yminor, rminor
    real(kind=plflt) :: xmajor, ymajor, rmajor
    character(len=241) :: fxstring   ! Gets the subtitle as well.
    character(len=120) :: fystring, ftstring
    
    !    external :: gr_format_labels

    call gr_axis_box(1, xopt, xmajor, xminor)
    call gr_axis_box(2, yopt, ymajor, yminor)
    if (pdefs%y_right) call gr_axis_box(3, ropt, rmajor, rminor)

    if (any(pdefs%axsty%format /= '') .or. any(pdefs%axsty%is_big_log)) &
         & call plslabelfunc(gr_format_labels)

    call plcol0(1)
    call plwidth(pdefs%axthick)
    call gr_plot_linesty(0_int16)
    call plschr(0._plflt, real(pdefs%charsize, plflt)*sysopts%charscale)
    call gr_plot_transform(index=1, full=.false._int8)

!    call plsesc(ichar('#'))
    call plbox(xopt, xmajor, xminor, yopt, ymajor, yminor)
!    call plsesc(ichar('!'))

    if (pdefs%axsty(1)%grid /= 0) then
       call gr_plot_linesty(pdefs%axsty(1)%grid-1_int16, &
            & scale=sqrt(pdefs%axthick))
       call plbox("g", xmajor, 0, "", 0._plflt, 0)
    end if
    if (pdefs%axsty(2)%grid /= 0) then
       call gr_plot_linesty(pdefs%axsty(2)%grid-1_int16, &
            & scale=sqrt(pdefs%axthick))
       call plbox("", 0._plflt, 0, "g", ymajor, 0)
    end if

    fxstring=''
    if (len_trim(pdefs%axtitle(1)) > 0) then
       if (len_trim(pdefs%subtitle) > 0) then
          call gr_ip_convert(trim(pdefs%axtitle(1))//c_new_line//trim(pdefs%subtitle), &
               & fxstring)
       else
          call gr_ip_convert(pdefs%axtitle(1), fxstring)
       end if
    end if

    fystring = ''
    if (len_trim(pdefs%axtitle(2)) > 0) &
         &  call gr_ip_convert(pdefs%axtitle(2), fystring)

    ftstring=''
    if (len_trim(pdefs%title) > 0) &
         &  call gr_ip_convert(pdefs%title, ftstring)
    
!!$    call pllab(trim(pdefs%axtitle(1))//c_new_line//trim(pdefs%subtitle), &
!!$         & pdefs%axtitle(2), pdefs%title)
    call pllab(fxstring, fystring, ftstring)
    
    if (pdefs%y_right) then
       call plcol0(1)
       call plwidth(pdefs%axthick)
       call gr_plot_linesty(0_int16)

       ! The RH Y axis

       call gr_plot_transform(index=2, full=.false._int8)

!       call plsesc(ichar('#'))
       call plbox("", 0._plflt, 0, ropt, rmajor, rminor)
!       call plsesc(ichar('!'))

       if (pdefs%axsty(3)%grid /= 0) then
          call gr_plot_linesty(pdefs%axsty(3)%grid-1_int16, &
            & scale=sqrt(pdefs%axthick))
          call plbox("", 0._plflt, 0, "g", rmajor, 0)
       end if
       if (len_trim(pdefs%axtitle(3)) > 0) then
          fystring=''
          call gr_ip_convert(pdefs%axtitle(3), fystring)
          call plmtex("r", 3._plflt, 0.5_plflt, 0.5_plflt, fystring)
       end if
    end if

  end subroutine gr_axis_plot

  subroutine gr_1dd_plot(index)
    integer, intent(in) :: index

    ! Plot an X-Y dataset

    real(kind=plflt), allocatable, dimension(:) :: x, y
    real(kind=plflt), allocatable, dimension(:) :: xh, yh
    integer(kind=int32), allocatable, dimension(:) :: idx
    
    type(graff_data), pointer :: data

    integer :: i
    logical :: xlog, ylog, issort
    real(kind=plflt), parameter :: dtor = pl_pi/180._plflt
    real(kind=plflt) :: scale
    integer :: nseg, iseg
    integer, dimension(:,:), allocatable :: isegb
    logical, dimension(:), allocatable :: invalid
    
    logical :: sflag
    data => pdefs%data(index)
    if (.not. allocated(data%xydata%x)) return

    call gr_plot_transform(dataset=index, full=data%noclip)

    xlog = pdefs%axtype(1) == 1
    if (pdefs%y_right .and. data%y_axis == 1) then
       ylog = pdefs%axtype(3) == 1
    else
       ylog = pdefs%axtype(2) == 1
    end if

    if (data%sort) then
       allocate(x(data%ndata), idx(data%ndata))
       call sort(data%xydata%x, x, idx)
       deallocate(x)
       issort = .true.
    else
       issort = .false.
    end if

    nseg = 1     ! single segment
    
    allocate(x(data%ndata), y(data%ndata))
    
    allocate(invalid(data%ndata))
    invalid(:) = .false.
    
    if (data%mode == 0) then
       if (issort) then
          x = data%xydata%x(idx)
          y = data%xydata%y(idx)
       else
          x = data%xydata%x(:)
          y = data%xydata%y(:)
       end if
       
       if (xlog) x = log10(x)
       
       if (ieee_is_finite(data%min_val)) &
            & invalid = invalid .or. y < data%min_val
       if (ieee_is_finite(data%max_val)) &
            & invalid = invalid .or. y > data%max_val
       nseg=1 + count(invalid)  ! Upper limit
       allocate(isegb(2, nseg))
       
       if (nseg > 1) then
          sflag = .true.
          iseg = 1
          do i = 1, data%ndata
             if (sflag .and. .not. invalid(i)) then
                isegb(1,iseg) = i
                sflag = .false.
             else if (.not. sflag .and. invalid(i)) then
                isegb(2,iseg) = i-1
                sflag = .true.
                iseg = iseg+1
             end if
          end do
          if (sflag) then     ! We ended on an invalid point
             nseg=iseg-1
          else                ! We ended on a valid point
             isegb(2,iseg) = data%ndata
             nseg = iseg
          end if
       else
          isegb(1,1) = 1
          isegb(2,1) = data%ndata
       end if
       if (ylog) y = log10(y)
    else
       if (data%mode == 1) then
          scale = 1._plflt
       else
          scale = dtor
       end if
       
       allocate(isegb(2,1))
       isegb(1,1) = 1
       isegb(2,1) = data%ndata
       
       x = data%xydata%x * cos(data%xydata%y*scale)
       if (xlog) x = log10(x)
       y = data%xydata%x * sin(data%xydata%y*scale)
       if (ylog) y = log10(y)
    end if
    
    if (data%colour == -1) return

    if (data%colour == -2) then
       call gr_custom_line(data%c_vals)
    else
       call plcol0(int(data%colour))
    end if
    
    call plwidth(data%thick)
    call gr_plot_linesty(data%line, scale=sqrt(data%thick))

    ! Moved error bars  before the line so that
    ! isegb doesn't get changed by histogram mode
    
    if (data%type > 0) then
       if (data%mode == 0) then
          do i = 1, nseg
             call gr_plot_xy_errors(index,isegb(:,i))
          end do
       else
          call gr_plot_rt_errors(index)
       end if
    end if

    select case (data%pline)
    case(1)
       do i = 1, nseg
          call plline(x(isegb(1,i):isegb(2,i)), y(isegb(1,i):isegb(2,i)))
       end do
    case(2)
       allocate(xh(2*size(x)), yh(2*size(x)))
       do i = 1, size(x)
          yh(2*i-1:2*i) = y(i)
       end do
       xh(1) = x(1)
       xh(size(xh)) = x(size(x))
       do i = 1, size(x)-1
          xh(2*i:2*i+1) = (x(i)+x(i+1))/2._real64
       end do
       isegb(1,:) = 2*isegb(1,:)-1
       isegb(2,:) = 2*isegb(2,:)
       do i = 1, nseg
          call plline(xh(isegb(1,i):isegb(2,i)), yh(isegb(1,i):isegb(2,i)))
       end do
       deallocate(xh,yh)
    end select


    if (data%psym /= 0) &
         & call gr_plot_symbol(x, y, data%psym, &
         & data%symsize, use = .not. invalid) 

    deallocate(x,y)
        
  end subroutine gr_1dd_plot

  subroutine gr_1df_plot(index)
    integer, intent(in) :: index

    ! Plot an X-Y function

    integer :: status

    status = gr_evaluate(int(index, int16))
    if (status == 0) call gr_1dd_plot(index)

  end subroutine gr_1df_plot

  subroutine gr_2dd_plot(index)
    integer, intent(in) :: index

    ! Plot a 2D dataset

    select case (pdefs%data(index)%zdata%format)
    case(0)
       call gr_contour(index)
    case(1)
       if (pdefs%data(index)%zdata%smooth) then
          call gr_shade_smooth(index)
       else
          call gr_shade(index)
       end if
    case(2)
    case default
       write(error_str, "(A,i0)") "gr_2dd_plot: Invalid format setting: ",&
            & pdefs%data(index)%zdata%format
       call gr_message(error_str)
    end select
  end subroutine gr_2dd_plot

  subroutine gr_2df_plot(index)
    integer, intent(in) :: index

    ! Plot a 2D function

    integer :: status

    status = gr_evaluate(int(index, int16))
    if (status == 0) call gr_2dd_plot(index)

  end subroutine gr_2df_plot

end module gr_plot
