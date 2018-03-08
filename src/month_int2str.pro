;----------------------------------------------------
; Author:  Jeffrey.R.Hall@jpl.nasa.gov (x4-4149)
; Written: 1998
; Project: MISR, David.J.Diner@jpl.nasa.gov (x4-6319)
;----------------------------------------------------

function month_int2str, month_int
	;-------------------------------------------
	; Convert month number into 3 character name.
	; Also converts string version of month number 
	; (where 1-9 have a leading zero).
	;-------------------------------------------
	if month_int eq 1 then month_str = 'Jan'
	if month_int eq 2 then month_str = 'Feb'
	if month_int eq 3 then month_str = 'Mar'
	if month_int eq 4 then month_str = 'Apr'
	if month_int eq 5 then month_str = 'May'
	if month_int eq 6 then month_str = 'Jun'
	if month_int eq 7 then month_str = 'Jul'
	if month_int eq 8 then month_str = 'Aug'
	if month_int eq 9 then month_str = 'Sep'
	if month_int eq 10 then month_str = 'Oct'
	if month_int eq 11 then month_str = 'Nov'
	if month_int eq 12 then month_str = 'Dec'
	return, month_str
end
