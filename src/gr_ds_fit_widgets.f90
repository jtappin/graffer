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

module gr_ds_fit_widgets
  use iso_fortran_env
  use iso_c_binding

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_container_add, gtk_label_new, gtk_widget_destroy, &
       & gtk_widget_show_all, TRUE, FALSE, GTK_POLICY_NEVER


  use graff_types
  use graff_globals

  use gr_fitting
  use gr_plot
  use gr_cb_common

  implicit none

  type(c_ptr), private :: fit_window, fit_list, fit_type_cbo, fit_order_sb, &
       & fit_dir_cbo, fit_soln_entry, fit_prob_entry, fit_wt_but, fit_neval_sb
  type(graff_fdata), private :: oldfun
  integer(kind=int16), private :: oldtype

contains
  subroutine gr_fit_menu

    ! Configure fitting of datasets

    type(c_ptr) :: base, junk, jb, sbox, b1
    integer :: nxy
    integer(kind=c_int) :: i, j

    nxy = count(pdefs%data%type >= 0 .and. pdefs%data%type <= 8)
    if (nxy == 0) then
       write(error_unit, "(A)") "gr_fit_menu: No fittable datasets found"
       return
    end if

    if (pdefs%data(pdefs%cset)%type < 0) then
       oldfun = pdefs%data(pdefs%cset)%funct
       oldtype = pdefs%data(pdefs%cset)%type
    else 
       oldtype = 10
    end if

    fit_window = hl_gtk_window_new("Fitting"//c_null_char, &
         & destroy=c_funloc(gr_fit_quit), &
         & parent=gr_window, modal=TRUE)

    base = hl_gtk_box_new()
    call gtk_container_add(fit_window, base)

    b1 = hl_gtk_box_new(horizontal = TRUE)
    call hl_gtk_box_pack(base, b1)

    fit_list = hl_gtk_listn_new(sbox, hscroll_policy=GTK_POLICY_NEVER, &
         & titles = [character(len=11) :: &
         & 'DS #', 'Type', 'N', 'Description'], &
         &  types = [g_type_int, g_type_string, g_type_int, &
         & g_type_string], height=300_c_int, &
         & changed = c_funloc(gr_fit_select))

    j = 0
    do i = 1_c_int, int(pdefs%nsets, c_int)
       if (pdefs%data(i)%type < 0 .or. pdefs%data(i)%type == 9) cycle
       if (i == pdefs%cset) cycle

       call hl_gtk_listn_ins(fit_list)
       call hl_gtk_listn_set_cell(fit_list, j, 0, ivalue=i)
       call hl_gtk_listn_set_cell(fit_list, j, 1, &
            & svalue = trim(typecodes(pdefs%data(i)%type))//c_null_char)
       call hl_gtk_listn_set_cell(fit_list, j, 2, &
            & ivalue = int(pdefs%data(i)%ndata, c_int))
       call hl_gtk_listn_set_cell(fit_list, j, 3, &
            & svalue = trim(pdefs%data(i)%descript)//c_null_char)
       j = j+1
    end do

    call hl_gtk_box_pack(b1, sbox)

    jb = hl_gtk_table_new()
    call hl_gtk_box_pack(b1, jb)

    junk = gtk_label_new("Fit type:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 0_c_int, &
         & yopts=0_c_int)

    fit_type_cbo = hl_gtk_combo_box_new(initial_choices=&
         & ["Polynomial      ", &
         &  "Exponential     ", &
         &  "Logarithmic     ", &
         &  "Power law       ", &
         &  "Piecewise linear"], active=-1_c_int, tooltip = &
         & "Select the type of fit to make"//c_null_char)
    call hl_gtk_table_attach(jb, fit_type_cbo, 1_c_int, 0_c_int, &
         & yopts=0_c_int)

    junk = gtk_label_new("Order:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int, &
         & yopts=0_c_int)

    fit_order_sb = hl_gtk_spin_button_new(1_c_int, 20_c_int, &
         & initial_value=1_c_int, tooltip=&
         & "Set the degree of the fitted polynomial"//c_null_char)
    call hl_gtk_table_attach(jb, fit_order_sb, 1_c_int, 1_c_int, &
         & yopts=0_c_int)

    junk = gtk_label_new("Direction:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 2_c_int, &
         & yopts=0_c_int)

    fit_dir_cbo = hl_gtk_combo_box_new(initial_choices = &
         & ['y = f(x)', 'x = f(y)'], &
         & active=f_c_logical(pdefs%data(pdefs%cset)%type == -2), &
         & tooltip="Choose whether to fit Y as a function of X or vice-versa"&
         & //c_null_char, changed=c_funloc(gr_fit_dir_ch))
    call hl_gtk_table_attach(jb, fit_dir_cbo, 1_c_int, 2_c_int, &
         & yopts=0_c_int)

    fit_wt_but = hl_gtk_check_button_new("Use error bars for weighting?"//&
         & c_null_char, sensitive=FALSE, initial_state=TRUE, tooltip=&
         & "Toggle whether to weight the points by their errors"//c_null_char)
    call hl_gtk_table_attach(jb, fit_wt_but, 0_c_int, 3_c_int, xspan=2_c_int, &
         & yopts=0_c_int)

    junk = gtk_label_new("Evaluations:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 4_c_int, &
         & yopts=0_c_int)

    fit_neval_sb = hl_gtk_spin_button_new(1_c_int, 10000_c_int, &
         & initial_value=25_c_int, tooltip=&
         & "How many evaluations to make for the display"//c_null_char)
    call hl_gtk_table_attach(jb, fit_neval_sb, 1_c_int, 4_c_int, &
         & yopts=0_c_int)

    jb = hl_gtk_table_new()
    call hl_gtk_box_pack(base, jb)

    junk = gtk_label_new("Fit:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 0_c_int, xopts=0_c_int)
    fit_soln_entry = hl_gtk_entry_new(editable=FALSE)
    call hl_gtk_table_attach(jb, fit_soln_entry, 1_c_int, 0_c_int)

    junk = gtk_label_new("Prob chance:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int, xopts=0_c_int)
    fit_prob_entry = hl_gtk_entry_new(editable=FALSE)
    call hl_gtk_table_attach(jb, fit_prob_entry, 1_c_int, 1_c_int)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb)

    junk = hl_gtk_button_new("Update"//c_null_char, &
         & clicked=c_funloc(gr_fit_update), tooltip=&
         & "Compute the fit, and show the result"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    if (pdefs%data(pdefs%cset)%type < 0) then
       junk = hl_gtk_button_new("Cancel"//c_null_char, &
            & clicked=c_funloc(gr_fit_clear), tooltip=&
            & "Clear the fit, restore the previous function, "//&
            & "quit the dialogue"//c_null_char)
       call hl_gtk_box_pack(jb, junk)
    end if

    junk = hl_gtk_button_new("Quit"//c_null_char, &
         & clicked=c_funloc(gr_fit_quit), tooltip=&
         & "Quit the dialogue."//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    call gtk_widget_show_all(fit_window)

  end subroutine gr_fit_menu

  recursive subroutine gr_fit_quit(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Quit fitting menu

    call gtk_widget_destroy(fit_window)
  end subroutine gr_fit_quit

  subroutine gr_fit_select(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    ! Select DS to be fitted

    integer(kind=c_int), dimension(:), allocatable :: isel
    integer(kind=c_int) :: nsel, idx, iyax, iftype, idir
    type(graff_data), pointer :: data

    nsel = hl_gtk_listn_get_selections(c_null_ptr, selection=widget, &
         & indices = isel)

    if (nsel /= 1) return

    call hl_gtk_listn_get_cell(fit_list, isel(1), 0_c_int, ivalue=idx)

    data => pdefs%data(idx)
    if (pdefs%y_right) then
       iyax = data%y_axis + 2
    else
       iyax = 2
    end if

    idir = gtk_combo_box_get_active(fit_dir_cbo)
    if (idir == 0) then
       iftype = (2*pdefs%axtype(1) + pdefs%axtype(iyax))
    else
       iftype = (pdefs%axtype(1) + 2*pdefs%axtype(iyax))
    end if
    call gtk_combo_box_set_active(fit_type_cbo, iftype)
       
    call gtk_widget_set_sensitive(fit_wt_but, f_c_logical(data%type > 0))
  end subroutine gr_fit_select

  subroutine gr_fit_dir_ch(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Select X->Y or Y->X

    integer(kind=c_int) :: itype, idir

    idir =  gtk_combo_box_get_active(widget)
    itype = gtk_combo_box_get_active(fit_type_cbo)
    if (itype == 2) then
       itype = 3
    else if (itype == 3) then
       itype = 2
    end if
    call gtk_combo_box_set_active(fit_type_cbo, itype)
  end subroutine gr_fit_dir_ch

  subroutine gr_fit_clear(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    ! Cancel changes

    if (oldtype < 0) then
       pdefs%data(pdefs%cset)%funct = oldfun
       pdefs%data(pdefs%cset)%type = oldtype
       call gtk_widget_destroy(fit_window)
    end if

  end subroutine gr_fit_clear

  subroutine gr_fit_update(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    ! Compute the fit.

    real(kind=real64), dimension(:), allocatable :: coeffs
    real(kind=real64), dimension(:), allocatable, target :: xr, yr
    real(kind=real64), dimension(:), allocatable :: wt
    real(kind=real64), dimension(:), pointer :: x, y
    real(kind=real64) :: xbar, ybar

    integer :: order, neval,i,ipos
    integer(kind=int16) :: oftype
    integer(kind=c_int), dimension(:), allocatable :: isel
    integer(kind=c_int) :: itype, idir, nsel, idx

    logical :: use_wt
    character(len=10) :: var
    character(len=5) :: wrap0, wrap1
    character(len=256) :: fstring
    character(len=32) :: termstr
    type(graff_data), pointer :: fit_ds, curr_ds

    nsel = hl_gtk_listn_get_selections(fit_list, &
         & indices = isel)
    if (nsel /= 1) return

    call hl_gtk_listn_get_cell(fit_list, isel(1), 0_c_int, ivalue=idx)

    fit_ds => pdefs%data(idx)
    curr_ds => pdefs%data(pdefs%cset)

    order = int(hl_gtk_spin_button_get_value(fit_order_sb))
    use_wt = c_f_logical(gtk_toggle_button_get_active(fit_wt_but)) .and. &
         & fit_ds%type > 0
    neval = int(hl_gtk_spin_button_get_value(fit_neval_sb))
    idir = gtk_combo_box_get_active(fit_dir_cbo)
    itype = gtk_combo_box_get_active(fit_type_cbo)
    allocate(x(fit_ds%ndata), y(fit_ds%ndata), coeffs(order+1))
    xr = fit_ds%xydata(1,:)
    yr = fit_ds%xydata(2,:)

    if (use_wt) then
       xbar = sum(abs(xr))/real(size(xr), real64)
       ybar = sum(abs(yr))/real(size(yr), real64)

       allocate(wt(fit_ds%ndata))

       select case (fit_ds%type)
       case(1)
          wt = ybar/fit_ds%xydata(3,:)
       case(2)
          wt = 2.*ybar / (fit_ds%xydata(3,:) + fit_ds%xydata(4,:))
       case(3)
          wt = xbar/fit_ds%xydata(3,:)
       case(4)
          wt = 2.*xbar / (fit_ds%xydata(3,:) + fit_ds%xydata(4,:))
       case(5)
          wt = sqrt((xbar/fit_ds%xydata(3,:))**2 + &
               & (ybar/fit_ds%xydata(4,:))**2)
       case(6)
          wt = sqrt((xbar/fit_ds%xydata(3,:))**2 + &
               & (2.*ybar/(fit_ds%xydata(4,:)+fit_ds%xydata(5,:)))**2)
       case(7)
          wt = sqrt((2.*xbar/(fit_ds%xydata(3,:)+fit_ds%xydata(4,:)))**2 + &
               & (ybar/fit_ds%xydata(5,:))**2)
       case(8)
          wt = sqrt((2.*xbar/(fit_ds%xydata(3,:)+fit_ds%xydata(4,:)))**2 + &
              & (2.*ybar/(fit_ds%xydata(5,:)+fit_ds%xydata(5,:)))**2)
       end select
    end if

    if (idir == 0) then
       oftype = -1
       x => xr
       y => yr
       var = 'x'
    else
       oftype = -2
       x => yr
       y => xr
       var = 'y'
    end if

    wrap0 = ''
    wrap1 =''
    select case (itype)
    case(0)
    case(1)
       y = log(y)
       wrap0 = 'exp('
       wrap1 = ')'
    case(2)
       x = log(x)
       var = 'alog('//trim(var)//')'
    case(3)
       x = log(x)
       y = log(y)
       var = 'alog('//trim(var)//')'
       wrap0 = 'exp('
       wrap1 = ')'
    case(4)
       print *, "Not yet implemented"
       return
    end select

    if (use_wt) then
       coeffs = poly_fit(x, y, order, weights=wt)
    else
       coeffs = poly_fit(x, y, order)
    end if

    fstring = wrap0

    write(termstr,"(g0.8)") coeffs(1)
    termstr=adjustl(termstr)
    fstring = trim(fstring)//termstr

    write(termstr,"(sp,g0.8)") coeffs(2)
    termstr=adjustl(termstr)
    termstr = trim(termstr)//"*"//var
    fstring = trim(fstring)//termstr

    do i = 2, order
       write(termstr,"(sp,g0.8)") coeffs(i+1)
       termstr=adjustl(termstr)
       ipos = len_trim(termstr)+1
       write(termstr(ipos:), "('*',a,'^',i0)") trim(var), i
       fstring = trim(fstring)//termstr
    end do

    fstring = trim(fstring)//wrap1

    call gtk_entry_set_text(fit_soln_entry, trim(fstring)//c_null_char)

    curr_ds%type = oftype
    curr_ds%ndata = neval

    curr_ds%funct%range = 0._real64
    curr_ds%funct%funct(1) = fstring
    curr_ds%funct%funct(2) = ''
    curr_ds%funct%evaluated = .false.

    if (allocated(curr_ds%xydata)) deallocate(curr_ds%xydata)
    call  gtk_notebook_set_current_page(display_nb, 0)

    call gr_plot_draw(.true.)
  end subroutine gr_fit_update
end module gr_ds_fit_widgets
