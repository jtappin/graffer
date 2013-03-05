; Copyright (C) 2013
; James Tappin

; This is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3, or (at your option)
; any later version.

; This software is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License along with
; this program; see the files COPYING3 and COPYING.RUNTIME respectively.
; If not, see <http://www.gnu.org/licenses/>.

pro Gr_autoscale, pdefs, xaxis=xaxis, yaxis=yaxis, $
                  ignore=ignore

;+
; GR_AUTOSCALE
;	Autoscale a GRAFFER axis to its data.
;
; Usage:
;	gr_autoscale, pdefs, [/xaxis|/yaxis]
;
; Arguments:
;	pdefs	struct	in/out	The GRAFFER data structure
;
; Keywords:
;	xaxis	?	input	If set scale the X-axis
;	yaxis	?	input	If set scale the Y-axis (2 for secondary)
;	ignore	?	input	If set, then disregard the current
;				settings altogther, the default is
;				only to extend axis ranges.
;
; History:
;	Original (borrowing much code from PLOT_OBJECT): 9/8/96; SJT
;	Convert handles to pointers: 27/6/05; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;-

widgets = pdefs.ids.graffer ne 0

if (not keyword_set(xaxis) and not keyword_set(yaxis)) then begin
    graff_msg, pdefs.ids.message, "Must specify either XAXIS or YAXIS " + $
      "key"
    return
endif else if (keyword_set(xaxis) and keyword_set(yaxis)) then begin
    graff_msg, pdefs.ids.message, "Cannot autoscale both axes at once"
    return
endif

if n_elements(yaxis) eq 0 then yaxis = 0

range = [!Values.f_infinity, -!Values.f_infinity]

if (keyword_set(xaxis)) then begin
    
    if (not keyword_set(ignore)) then range = pdefs.xrange
    for i = 0, pdefs.nsets-1 do begin
        
        if ((*pdefs.data)[i].ndata eq 0) then continue ; Trying to autoscale
                                ; on an empty data set is silly
        
        if (pdefs.y_right and (*pdefs.data)[i].y_axis eq 1) then $
          yrange = pdefs.yrange_r $
        else yrange = pdefs.yrange
        if ((*pdefs.data)[i].mode ne 0) then $
          gr_as_xa, (*pdefs.data)[i], pdefs.xrange, yrange, range $ 
        else gr_as_xr, (*pdefs.data)[i], yrange, pdefs.ytype, range
        
    endfor
    
    pdefs.xrange = range
    if (widgets) then begin
        widget_control, pdefs.ids.xmin, set_value = range(0)
        widget_control, pdefs.ids.xmax, set_value = range(1)
    endif

endif else if yaxis eq 2 and pdefs.y_right then begin

    if (not keyword_set(ignore)) then range = pdefs.yrange_r
    
    for i = 0, pdefs.nsets-1 do begin
        
        if ((*pdefs.data)[i].ndata eq 0) then continue ; Trying to autoscale
                                ; on an empty data set is silly
        if (*pdefs.data)[i].y_axis eq 0 then continue
        if ((*pdefs.data)[i].mode ne 0) then $
          gr_as_ya, (*pdefs.data)[i], pdefs.xrange, pdefs.yrange_r, range $ 
        else gr_as_yr, (*pdefs.data)[i], pdefs.xrange, pdefs.xtype, range

    endfor
    
    pdefs.yrange_r = range
    if (widgets) then begin
        widget_control, pdefs.ids.ymin_r, set_value = range(0)
        widget_control, pdefs.ids.ymax_r, set_value = range(1)
    endif
    
endif else begin
    
    if (not keyword_set(ignore)) then range = pdefs.yrange
    
    for i = 0, pdefs.nsets-1 do begin
        
        if ((*pdefs.data)[i].ndata eq 0) then continue ; Trying to autoscale
                                ; on an empty data set is silly
        if pdefs.y_right and (*pdefs.data)[i].y_axis eq 1 then continue
        if ((*pdefs.data)[i].mode ne 0) then $
          gr_as_ya, (*pdefs.data)[i], pdefs.xrange, pdefs.yrange, range $ 
        else gr_as_yr, (*pdefs.data)[i], pdefs.xrange, pdefs.xtype, range

    endfor
    
    pdefs.yrange = range
    if (widgets) then begin
        widget_control, pdefs.ids.ymin, set_value = range(0)
        widget_control, pdefs.ids.ymax, set_value = range(1)
    endif
    
endelse

end
