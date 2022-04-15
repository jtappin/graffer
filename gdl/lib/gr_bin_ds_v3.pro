; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_bin_ds_v3, data, nset, ilu, msgid, version

;+
; GR_BIN_DS, swap = swap
;	Read an individual dataset from a binary V2.x GRAFFER file.
;
; Arguments:
;	data	struct	in/out	The graffer dataset structure
;	nset	int	input	The serial number of the current
;				dataset.
;	ilu	int	input	File unit number to read
;	msgid	long	input	ID of message window (if created).
;	version	int[2]	input	Version of dataset begin read.	
;
;
; History:
;	Original (binary version): 15/1/97; SJT
;	Add version argument: 14/5/99; SJT
;	Replace handles with pointers: 27/5/05; SJT
;	Convert data to doubles: 30/6/05; SJT
;	Support colour inversion: 26/6/07; SJT
;	Add local colour tables for 2D datasets: 17/11/11; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;	Remove obsolete swap key: 6/1/12; SJT
;-

  tag = '   '

  nflag = 0b
  nflag2 = 0b
  tflag = 0b
  dflag = 0b
  jflag = 0b

  if version[0] eq 2 then begin
     single = 1b
     n_is_int = version[1] le 1
  endif else begin
     single = 0b
     n_is_int = 0b
  endelse

  elements = [2, 3, 4, 3, 4, 4, 5, 5, 6]
  data[nset].min_val = !values.d_nan
  data[nset].max_val = !values.d_nan

  while (not eof(ilu)) do begin
     
     readu, ilu, tag
     
                                ; Recognised tags:
                                ; J - Joining option
                                ; P - symbol
                                ; S - symbol size
                                ; L - line style
                                ; C - colour
                                ; W - thickness (width)
                                ; O - sorted? (Order)
                                ; D - description
                                ; N - number of points (or
                                ;     evaluations)
                                ; N2- Number of points in y-direction
                                ;     for 2-D data.
                                ; T - type
                                ; M - Mode
                                ; K - noclip (both C & N are already
                                ;     bagged)
                                ; E - Mouse editing
                                ; R - function range
                                ; F, FX, FY - function specifiers
                                ; VS, VE - start & end XY data.
                                ; DE - end dataset

     case (strtrim(tag)) of
        
        'J': begin
           data[nset].pline = gr_int_rd(ilu, 1)
           jflag = 1b
        end
        
        'P': data[nset].psym = gr_int_rd(ilu, 1)
        'S': data[nset].symsize = gr_flt_rd(ilu, 1)
        'L': data[nset].line = gr_int_rd(ilu, 1)
        'C': data[nset].colour = gr_int_rd(ilu, 1)
        'W': data[nset].thick = double(gr_int_rd(ilu, 1))
        'O': data[nset].sort = gr_byt_rd(ilu, 1)
        'K': data[nset].noclip = gr_byt_rd(ilu, 1)
        'E': data[nset].medit = gr_byt_rd(ilu, 1)
        'D': data[nset].descript = gr_str_rd(ilu)

        
        'N': begin
           if (n_is_int) then $
              data[nset].ndata = gr_int_rd(ilu, 1) $
           else $
              data[nset].ndata = gr_lon_rd(ilu, 1)
           nflag = 1b
        end
        'N2': begin
           if (n_is_int) then $
              data[nset].ndata2 = gr_int_rd(ilu, 1) $
           else $
              data[nset].ndata2 = gr_lon_rd(ilu, 1)
           nflag2 = 1b
        end
        'T': begin
           data[nset].type = gr_int_rd(ilu, 1)
           tflag = 1b
        end
        'M': data[nset].mode = gr_int_rd(ilu, 1)
        
        'Y': data[nset].y_axis = gr_int_rd(ilu, 1)

        'ZF':  data[nset].zopts.format = gr_int_rd(ilu, 1)
        
        'ZNL': data[nset].zopts.n_levels = abs(gr_int_rd(ilu, 1))
        'ZL': begin
           levels = gr_dbl_rd(ilu, data[nset].zopts.n_levels, single $
                              = single)
           data[nset].zopts.levels = ptr_new(levels)
           data[nset].zopts.set_levels = 1b
        end

        'ZNC': data[nset].zopts.n_cols = gr_int_rd(ilu, 1)
        'ZC': begin
           cols = gr_int_rd(ilu, data[nset].zopts.n_cols)
           data[nset].zopts.colours = ptr_new(cols, /extract)
           data[nset].zopts.raw_colours = $
              ptr_new(intarr(3, data[nset].zopts.n_cols))
        end

        'ZCT': data[nset].zopts.ctable = gr_int_rd(ilu, 1)

        'ZCG': data[nset].zopts.gamma = gr_flt_rd(ilu, 1)

        'ZNS': data[nset].zopts.n_sty = gr_int_rd(ilu, 1)
        'ZS': begin
           stys = gr_int_rd(ilu, data[nset].zopts.n_sty)
           data[nset].zopts.style = ptr_new(stys)
        end
        
        'ZNT': data[nset].zopts.n_thick = gr_int_rd(ilu, 1)
        'ZT': begin
           thick = double(gr_int_rd(ilu, data[nset].zopts.n_thick))
           data[nset].zopts.thick = ptr_new(thick)
        end
        
        'ZCF': data[nset].zopts.fill = gr_byt_rd(ilu, 1)
        'ZLI': data[nset].zopts.label = $
           gr_int_rd(ilu, 1)
        
        'ZR': data[nset].zopts.range = gr_dbl_rd(ilu, 2, single = $
                                                 single)
        'ZP': data[nset].zopts.pxsize = gr_flt_rd(ilu, 1)
        
        'ZIL': data[nset].zopts.ilog = gr_byt_rd(ilu, 1)
        'ZIN': data[nset].zopts.invert = gr_byt_rd(ilu, 1)

        'R': begin
           if (data[nset].type eq -4) then $
              xydata.range = gr_dbl_rd(ilu, 4, single = single)  $
           else $
              xydata.range = gr_dbl_rd(ilu, 2, single = single) 
        end
        'F': xydata.funct = gr_str_rd(ilu)
        'FX': xydata.funct(0) = gr_str_rd(ilu)
        'FY': xydata.funct(1) = gr_str_rd(ilu)
        
        'VS': begin
           ncols = gr_int_rd(ilu, 1)
           readu, ilu, xyvals
           if single then xyvals = double(xyvals)
           xydata.x = ptr_new(reform(xyvals[0, *]))
           xydata.y = ptr_new(reform(xyvals[1, *]))
           if nerr[0] ne 0 then $
              xydata.x_err = ptr_new(xyvals[2:1+nerr[0], *]))
           if nerr[1] ne 0 then $
              xydata.y_err = ptr_new(xyvals[2+nerr[0]:*, *]))

           readu, ilu, tag
        end
        'ZXS': begin
           if xydata.x_is_2d then $
              xv = gr_dbl_rd(ilu, [data[nset].ndata, $
                                   data[nset].ndata2]) $
           else $ 
              xv = gr_dbl_rd(ilu, data[nset].ndata, $
                             single = single)
           xydata.x = ptr_new(xv)
           readu, ilu, tag
        end
        'ZYS': begin
           if xydata.y_is_2d then $
              yv = gr_dbl_rd(ilu, [data[nset].ndata, $
                                   data[nset].ndata2]) $
           else $ 
              yv = gr_dbl_rd(ilu, data[nset].ndata2, single = single)
           xydata.y = ptr_new(yv)
           readu, ilu, tag
        end
        'ZZS': begin
           zv = gr_dbl_rd(ilu, [data[nset].ndata, $
                                data[nset].ndata2], $
                          single = single)
           xydata.z = ptr_new(double(zv))
           readu, ilu, tag
        end

        'ZX2': if ptr_valid(xydata.x) then graff_msg, msgid, $
           "WARNING: 2-D X data requested after X data " + $
           "acquired" $
        else xydata.x_is_2d = gr_byt_rd(ilu, 1)
        'ZY2': if ptr_valid(xydata.x) then graff_msg, msgid, $
           "WARNING: 2-D X data requested after X data " + $
           "acquired" $
        else xydata.y_is_2d = gr_byt_rd(ilu, 1)
        
        'DE': goto, ds_read
        
        Else: begin
           graff_msg, msgid, "Unknown DS tag: " + $
                      tag + "Ignoring."
           point_lun, -ilu, iptr
           print, iptr
        end
     endcase
     
     
     if (nflag and tflag and not dflag) then begin
        dflag = 1b
        case data[nset].type of
           -4: xydata = {graff_zfunct}
           -3: xydata = {graff_pfunct}
           -2: xydata = {graff_funct}
           -1: xydata = {graff_funct}
           9: if (nflag2) then begin
              xydata = {graff_zdata}
           endif else dflag = 0b
           
           Else: xydata = {graff_xydata}
        endcase
     endif
     
     New_line:
     
  endwhile

Ds_read:

; This is for old versions that did not have the join setting but used
; raw IDL PSYM settings

  if (not jflag) then begin
     if (data[nset].psym eq 10) then begin
        data[nset].pline = 2
        data[nset].psym = 0
     endif else if (data[nset].psym eq 0) then begin
        data[nset].pline = 1
     endif else if (data[nset].psym lt 0) then begin
        data[nset].pline = 1
        data[nset].psym = abs(data[nset].psym)
     endif else data[nset].pline = 0
  endif

  data[nset].xydata = ptr_new(xydata)

end
