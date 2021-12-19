; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; NAME:
;	gr_curr_menu
;
;
; PURPOSE:
;	Menus for the options of the current dataset.
;
;
; CATEGORY:
;	graffer
;
;
; CALLING SEQUENCE:
;	gr_curr_menu,base,pdefs
;
;
; INPUTS:
;	base	long	The parent widget_base for the menus
;	pdefs	struct	The graffer master data structure
;
;
; MODIFICATION HISTORY:
;	Extracted from graff_one: 4/7/05; SJT
;-

pro gr_curr_menu, base, pdefs

jb = widget_base(base, $
                 /column)

                                ; Plot a function or read data

gr_ds_create, jb, pdefs
                                ; Plotting symbols etc.

optbb = widget_base(jb, $
                    /frame)

gr_ds_menus, optbb, pdefs
gr_z_menus, optbb, pdefs


end
