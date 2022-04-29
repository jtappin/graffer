; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_bin_ds, data, nset, ilu, msgid

;+
; GR_BIN_DS
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
; Keywords:
;	swap	If set, then the file is byte-swapped relative to the
;		platform
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
;	V4 version: 6/1/12; SJT
;	Eliminate goto and redundant code: 11/1/12; SJT
; 	Add min & max values: 4/3/15; SJT
; 	Fix init of min & max: 2/6/15; SJT
;	Add non-linear contour level maps: 12/10/16; SJT
;	Add labelling offset: 2/5/17; SJT
;-

  tag = '   '

  nflag = 0b
  nflag2 = 0b
  tflag = 0b
  dflag = 0b
  eflag = 0b

  elements = [2, 3, 4, 3, 4, 4, 5, 5, 6]

  data[nset].min_val = !values.d_nan
  data[nset].max_val = !values.d_nan

  while (not eof(ilu)) do begin
     
     graff_get_rec, ilu, tag, value, tcode, nvals = nvals, ndims = ndims
     
                                ; Recognised tags:
                                ; J - Joining option
                                ; P - symbol
                                ; S - symbol size
                                ; L - line style
                                ; C - colour
                                ; CV - custom colour
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
                                ; MN - Min value to plot.
                                ; MX - Max value to plot.
                                ; E - Mouse editing
                                ; R - function range
                                ; F, FX, FY - function specifiers
                                ; VS, VE - start & end XY data.
                                ; DE - end dataset

     case (strtrim(tag)) of
        
        'J': data[nset].pline = value
        
        'P': data[nset].psym = value
        'S': data[nset].symsize = value
        'L': data[nset].line = value
        'C': data[nset].colour = value
        'CV': data[nset].c_vals = value
        'W': data[nset].thick = value
        'O': data[nset].sort = value
        'K': data[nset].noclip = value
        'E': data[nset].medit = value
        'D': data[nset].descript = value

        
        'N': begin
           data[nset].ndata = value
           nflag = 1b
        end
        'N2': begin
           data[nset].ndata2 = value
           nflag2 = 1b
        end
        'T': begin
           data[nset].type = value
           tflag = 1b
        end
        'M': data[nset].mode = value
        
        'MN': data[nset].min_val = value
        'MX': data[nset].max_val = value

        'Y': data[nset].y_axis = value

        'ZF':  data[nset].zopts.format = value
        
        'ZNL': begin
           data[nset].zopts.n_levels = value
           data[nset].zopts.set_levels = 0b
        end
        'ZLM': data[nset].zopts.lmap = value

        'ZL': begin
           data[nset].zopts.levels = ptr_new(double(value))
           data[nset].zopts.n_levels = nvals
           data[nset].zopts.set_levels = 1b
        end

        'ZC': begin
           if tcode eq 11 then begin
              cols = intarr(nvals)
              rcols = intarr(3, nvals)
              for j = 0, nvals-1 do begin
                 if n_elements(value[j]) eq 1 then $
                    cols[j] = fix(value[j]) $
                 else begin
                    rcols[*, j] = fix(value)
                    cols[j] = -2
                 endelse
              endfor
              data[nset].zopts.colours = ptr_new(cols)
              data[nset].zopts.raw_colours = ptr_new(rcols)
           endif else data[nset].zopts.colours = ptr_new(fix(value))
           data[nset].zopts.n_cols = nvals
        end
        'ZCR': begin
           if data[nset].zopts.n_cols eq 0 then $
              graff_msg, msgid, $
                         "Raw colours found before indexed list."
           data[nset].zopts.raw_colours = ptr_new(fix(value))
        end
        
        'ZCT': data[nset].zopts.ctable = value

        'ZCG': data[nset].zopts.gamma = value

        'ZS': begin
           data[nset].zopts.style = ptr_new(fix(value))
           data[nset].zopts.n_sty = nvals
        end

        'ZT': begin
           data[nset].zopts.thick = ptr_new(double(value))
           data[nset].zopts.n_thick = nvals
        end

        'ZCF': begin
           data[nset].zopts.fill = value
           if data[nset].zopts.fill gt 1 then begin
              graff_msg, msgid, $
                         "Downhill tick contours no longer " + $
                         "supported, using standard."
              data[nset].zopts.fill = 0
           endif
        end
        'ZLI': data[nset].zopts.label = value
        'ZLO': data[nset].zopts.label_off = value
        'ZCS': data[nset].zopts.charsize = value

        'ZR': data[nset].zopts.range = value
        'ZP': data[nset].zopts.pxsize = value
        
        'ZIL': data[nset].zopts.ilog = value
        'ZIN': data[nset].zopts.invert = value
        'ZSM': data[nset].zopts.smooth = value
        'ZSN': data[nset].zopts.shade_levels = value
        'ZM': data[nset].zopts.missing = value

        'R': xydata.range = value

        'F': xydata.funct = value
        'FX': xydata.funct(0) = value
        'FY': xydata.funct(1) = value
        
        'VS': begin
           xyvals = double(value)
           nerr = gr_n_errors(data[nset].type)
           sv = size(xyvals)
           if sv[1] ne 2+nerr[0]+nerr[1] then begin
              graff_msg, msgid, $
                         "Number of data columns does not match DS type."
              continue
           endif
           xydata.x = ptr_new(reform(xyvals[0, *]))
           xydata.y = ptr_new(reform(xyvals[1, *]))
           if nerr[0] ne 0 then $
              xydata.x_err = ptr_new(xyvals[2:1+nerr[0], *])
           if nerr[1] ne 0 then $
              xydata.y_err = ptr_new(xyvals[2+nerr[0]:*, *])
        end

        'VX': begin
           xv = double(value)
           xydata.x = ptr_new(xv)
        end
        'VY': begin
           yv = double(value)
           xydata.y = ptr_new(yv)
        end
        'VXE': begin
           xev = double(value)
           xydata.x_err = ptr_new(xev)
        end
        'VYE': begin
           yev = double(value)
           xydata.y_err = ptr_new(yev)
        end
        
        'ZXS': begin
           xv = double(value)
           xydata.x_is_2d = ndims eq 2
           xydata.x = ptr_new(xv)
        end
        'ZYS': begin
           yv = double(value)
           xydata.y_is_2d = ndims eq 2
           xydata.y = ptr_new(yv)
        end
        'ZZS': begin
           zv = double(value)
           xydata.z = ptr_new(zv)
        end
        
        'DE': eflag = 1b
        
        Else: begin
           graff_msg, msgid, "Unknown DS tag: " + $
                      tag + " Ignoring."
        end
     endcase
     
     if eflag then break

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

  data[nset].xydata = ptr_new(xydata)

end
