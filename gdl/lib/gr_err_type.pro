;+
; GR_ERR_TYPE
;	Return the implied type from the dimenmsions of the error
;	limits.
;
; Usage:
;	type = gr_err_type(nxe, nye)
;	or
;	type = gr_err_type(nerr)
;
; Returns:
;	The type code for the numbers of error limits.
;
; Arguments:
;	nxe	long	The number of X errors.
;	nye	long	The number of Y errors.
;	nerr	long	A 2-element array with the number of X & Y
;			errors.
;
; History:
;	Original: 28/4/22; SJT
;-

function gr_err_type, nxe, nye

  typemap = [[0, 3, 4], $
             [1, 5, 7], $
             [2, 6, 8]]

  if n_params() eq 2 then return, typemap[nxe, nye] $
  else return, typemap[nxe[0], nxe[1]]

end
