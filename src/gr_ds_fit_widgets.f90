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

module gr_ds_fit_widgets
  ! Fitting a function to a dataset.

  use, intrinsic :: iso_fortran_env
  use, intrinsic :: iso_c_binding
  use, intrinsic :: ieee_arithmetic

  use gtk_hl
  use gtk_sup

  use gtk, only: gtk_combo_box_get_active, gtk_combo_box_set_active, &
       & gtk_container_add, gtk_entry_set_text, gtk_label_new, &
       & gtk_notebook_set_current_page, gtk_toggle_button_get_active, &
       & gtk_widget_destroy, gtk_widget_set_sensitive, gtk_widget_show_all, &
       & TRUE, FALSE, GTK_POLICY_NEVER

  use graff_types
  use graff_globals

  use gr_fitting
  use gr_plot
  use gr_cb_common
  use gr_msg
  use gr_ds_tools
  
  implicit none

  type(c_ptr), private :: fit_window, fit_list, fit_type_cbo, fit_order_sb, &
       & fit_dir_cbo, fit_soln_entry, fit_prob_entry, fit_wt_but, &
       & fit_neval_sb, fit_chi2_entry

  type(graff_data), private :: old_ds
  
  logical, private :: have_changed

contains
  subroutine gr_fit_menu

    ! Configure fitting of datasets

    type(c_ptr) :: base, junk, jb, sbox, b1
    integer :: nxy
    integer(kind=c_int) :: i, j

    nxy = count(pdefs%data%type >= 0 .and. pdefs%data%type <= 8)
    if (nxy == 0) then
       call gr_message("gr_fit_menu: No fittable datasets found")
       return
    end if

    call gr_ds_copy(from=pdefs%cset, destination = old_ds)
    
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
         &  "Power law       "], active=-1_c_int, tooltip = &
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
    call hl_gtk_table_attach(jb, fit_soln_entry, 1_c_int, 0_c_int, xspan=3)

    junk = gtk_label_new("Chi**2/DF:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 0_c_int, 1_c_int, xopts=0_c_int)
    fit_chi2_entry = hl_gtk_entry_new(editable=FALSE)
    call hl_gtk_table_attach(jb, fit_chi2_entry, 1_c_int, 1_c_int)
    junk = gtk_label_new("Prob:"//c_null_char)
    call hl_gtk_table_attach(jb, junk, 2_c_int, 1_c_int, xopts=0_c_int)
    fit_prob_entry = hl_gtk_entry_new(editable=FALSE)
    call hl_gtk_table_attach(jb, fit_prob_entry, 3_c_int, 1_c_int)

    jb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jb)

    junk = hl_gtk_button_new("Update"//c_null_char, &
         & clicked=c_funloc(gr_fit_update), tooltip=&
         & "Compute the fit, and show the result"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

    junk = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(gr_fit_clear), tooltip=&
         & "Clear the fit, restore the previous function, "//&
         & "quit the dialogue"//c_null_char)
    call hl_gtk_box_pack(jb, junk)

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

    call gr_plot_draw(have_changed)

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

    call gr_ds_copy(source=old_ds, to=pdefs%cset, move=.true.)
    have_changed = .false.
    call gtk_widget_destroy(fit_window)
 
  end subroutine gr_fit_clear

  subroutine gr_fit_update(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    ! Compute the fit.

    real(kind=real64), dimension(:), allocatable :: coeffs
    real(kind=real64), dimension(:), allocatable, target :: xr, yr
    real(kind=real64), dimension(:), allocatable :: wt
    real(kind=real64), dimension(:), pointer :: x, y
    real(kind=real64) :: chi2, prob

    integer :: order, neval,i,ipos, nf_data, j
    integer(kind=int16) :: oftype
    integer(kind=c_int), dimension(:), allocatable :: isel
    integer(kind=c_int) :: itype, idir, nsel, idx

    logical :: use_wt, nan_flag
    character(len=10) :: var
    character(len=5) :: wrap0, wrap1
    character(len=256) :: fstring
    character(len=32) :: termstr
    type(graff_data), pointer :: fit_ds, curr_ds
    logical, dimension(:), allocatable :: valid_data

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

    allocate(coeffs(order+1))

    ! Need to eliminate any NaN values otherwise fit will be NaN.

    nan_flag=any(.not. (ieee_is_finite(fit_ds%xydata%x) .and. &
         & ieee_is_finite(fit_ds%xydata%y)))

    if (nan_flag) then
       allocate(valid_data(fit_ds%ndata))
       valid_data = ieee_is_finite(fit_ds%xydata%x) .and. &
            & ieee_is_finite(fit_ds%xydata%y)
       nf_data = count(valid_data)
       allocate(xr(nf_data), yr(nf_data))
       j = 1
       do i = 1, fit_ds%ndata
          if (.not. valid_data(i)) cycle
          xr(j) = fit_ds%xydata%x(i)
          yr(j) = fit_ds%xydata%y(i)
          j = j+1
       end do
    else
       allocate(xr(fit_ds%ndata), yr(fit_ds%ndata))
       xr = fit_ds%xydata%x
       yr = fit_ds%xydata%y
    end if

    select case (itype)
    case(1)                ! Exponential fit, -ve Y values invalid
       if (any(yr <= 0.)) then
          call gr_message("Exp fit: Y contains negative or zero values, using polynomial")
          itype = 0
          call gtk_combo_box_set_active(fit_type_cbo, 0_c_int)
       end if

    case(2)                ! Log fit, -ve X values are invalid
       if (any(xr <= 0.)) then
          call gr_message("Log fit: X contains negative or zero values, using polynomial")
          itype = 0
          call gtk_combo_box_set_active(fit_type_cbo, 0_c_int)
       end if

    case(3)                ! Power law, -ve X or Y values are invalid
       if (any(xr <= 0.) .or. any(yr <= 0)) then
          call gr_message("Power fit: X or Y contains negative or zero values, using polynomial")
          itype = 0
          call gtk_combo_box_set_active(fit_type_cbo, 0_c_int)
       end if
    end select

    if (use_wt) then
       if (nan_flag) then
          allocate(wt(nf_data))

          j = 1
          select case (fit_ds%type)
          case(1)
             do i = 1, fit_ds%ndata
                if (.not. valid_data(i)) cycle
                wt(j) = 1._real64/fit_ds%xydata%y_err(1,i)
                j = j+1
             end do
          case(2)
             do i = 1, fit_ds%ndata
                if (.not. valid_data(i)) cycle
                wt(j) = 2._real64 / (fit_ds%xydata%y_err(1,i) + &
                     & fit_ds%xydata%y_err(2,i))
                j = j+1
             end do
          case(3)
             do i = 1, fit_ds%ndata
                if (.not. valid_data(i)) cycle
                wt(j) = 1._real64/fit_ds%xydata%x_err(1,i)
                j = j+1
             end do
          case(4)
             do i = 1, fit_ds%ndata
                if (.not. valid_data(i)) cycle
                wt(j) = 2._real64 / (fit_ds%xydata%x_err(1,i) + &
                     & fit_ds%xydata%x_err(2,i))
                j = j+1
             end do
          case(5)
             do i = 1, fit_ds%ndata
                if (.not. valid_data(i)) cycle
                wt(j) = sqrt((1._real64/fit_ds%xydata%x_err(1,i))**2 + &
                     & (1._real64/fit_ds%xydata%y_err(1,i))**2)
                j = j+1
             end do
          case(6)
             do i = 1, fit_ds%ndata
                if (.not. valid_data(i)) cycle
                wt(j) = sqrt((1._real64/fit_ds%xydata%x_err(1,i))**2 + &
                     & (2._real64/(fit_ds%xydata%y_err(1,i)+ &
                     & fit_ds%xydata%y_err(2,i)))**2)
                j = j+1
             end do
          case(7)
             do i = 1, fit_ds%ndata
                if (.not. valid_data(i)) cycle
                wt(j) = sqrt((2._real64/(fit_ds%xydata%x_err(1,i)+ &
                     & fit_ds%xydata%x_err(2,i)))**2 + &
                     & (1._real64/fit_ds%xydata%y_err(1,i))**2)
                j = j+1
             end do
          case(8)
             do i = 1, fit_ds%ndata
                if (.not. valid_data(i)) cycle
                wt(j) = sqrt((2._real64/(fit_ds%xydata%x_err(1,i)+ &
                     & fit_ds%xydata%x_err(2,i)))**2 + &
                     & (2._real64/(fit_ds%xydata%y_err(1,i)+ &
                     & fit_ds%xydata%y_err(2,i)))**2)
                j = j+1
             end do
          end select
       else
          allocate(wt(fit_ds%ndata))

          select case (fit_ds%type)
          case(1)
             wt = 1._real64/fit_ds%xydata%y_err(1,:)
          case(2)
             wt = 2._real64 / (fit_ds%xydata%y_err(1,:) + &
                  & fit_ds%xydata%y_err(2,:))
          case(3)
             wt = 1._real64/fit_ds%xydata%x_err(1,:)
          case(4)
             wt = 2._real64 / (fit_ds%xydata%x_err(1,:) + &
                  & fit_ds%xydata%x_err(2,:))
          case(5)
             wt = sqrt((1._real64/fit_ds%xydata%x_err(1,:))**2 + &
                  & (1._real64/fit_ds%xydata%y_err(1,:))**2)
          case(6)
             wt = sqrt((1._real64/fit_ds%xydata%x_err(1,:))**2 + &
                  & (2._real64/(fit_ds%xydata%y_err(1,:)+ &
                  & fit_ds%xydata%y_err(2,:)))**2)
          case(7)
             wt = sqrt((2._real64/(fit_ds%xydata%x_err(1,:)+ &
                  & fit_ds%xydata%x_err(2,:)))**2 + &
                  & (1._real64/fit_ds%xydata%y_err(1,:))**2)
          case(8)
             wt = sqrt((2._real64/(fit_ds%xydata%x_err(1,:)+ &
                  & fit_ds%xydata%x_err(2,:)))**2 + &
                  & (2._real64/(fit_ds%xydata%y_err(1,:)+ &
                  & fit_ds%xydata%y_err(2,:)))**2)
          end select
       endif
       where (.not. ieee_is_finite(wt)) wt = 0._real64
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
       call gr_message("Piecewise fitting is not yet implemented.")
       return
    end select

    if (use_wt) then
       coeffs = poly_fit(x, y, order, weights=wt, chi2=chi2)
    else
       coeffs = poly_fit(x, y, order, chi2=chi2)
    end if

    fstring = wrap0

    write(termstr,"(1pg0.8)") coeffs(1)
    termstr=adjustl(termstr)
    fstring = trim(fstring)//termstr

    write(termstr,"(sp,1pg0.8)") coeffs(2)
    termstr=adjustl(termstr)
    termstr = trim(termstr)//"*"//var
    fstring = trim(fstring)//termstr

    do i = 2, order
       write(termstr,"(sp,1pg0.8)") coeffs(i+1)
       termstr=adjustl(termstr)
       ipos = len_trim(termstr)+1
       write(termstr(ipos:), "('*',a,'^',i0)") trim(var), i
       fstring = trim(fstring)//termstr
    end do

    fstring = trim(fstring)//wrap1

    call gtk_entry_set_text(fit_soln_entry, trim(fstring)//c_null_char)

    write(termstr, "(1pg0)") chi2/(size(x)-1-order)
    call gtk_entry_set_text(fit_chi2_entry, trim(termstr)//c_null_char)
    prob = 1. - gammap(real(size(x)-1-order, real64)/2., chi2/2.)
    write(termstr, "(1pg0)") prob
    call gtk_entry_set_text(fit_prob_entry, trim(termstr)//c_null_char)

    curr_ds%type = oftype
    curr_ds%ndata = neval

    curr_ds%funct%range = 0._real64
    curr_ds%funct%funct(1) = fstring
    curr_ds%funct%funct(2) = ''
    curr_ds%funct%evaluated = .false.
    call gtk_entry_set_text(ds_type_id, &
         & trim(typedescrs(curr_ds%type))//c_null_char)

    if (allocated(curr_ds%xydata%x)) deallocate(curr_ds%xydata%x)
    if (allocated(curr_ds%xydata%y)) deallocate(curr_ds%xydata%y)
    if (allocated(curr_ds%xydata%x_err)) deallocate(curr_ds%xydata%x_err)
    if (allocated(curr_ds%xydata%y_err)) deallocate(curr_ds%xydata%y_err)
    
    call  gtk_notebook_set_current_page(display_nb, 0)

    call gr_plot_draw(.false.)
    have_changed = .true.
  end subroutine gr_fit_update
end module gr_ds_fit_widgets
