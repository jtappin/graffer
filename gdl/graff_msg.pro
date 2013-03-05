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

pro Graff_msg, message, help=help

;+
; GRAFF_MSG
;	Display a message in the graffer message box
;
; Usage:
;	graff_msg, mwid, message
;
; Arguments:
;	mwid	long	input	Widget ID of message box
;	message	string	input	The message
;
; History:
;	Original: 18/8/95; SJT
;	Change to take widget ID as first argument: 12/5/95; SJT
;-

print, message

end
