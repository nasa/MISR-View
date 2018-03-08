;----------------------------------------------------
; Author:  Jeffrey.R.Hall@jpl.nasa.gov (x4-4149)
; Written: 1998
; Project: MISR, David.J.Diner@jpl.nasa.gov (x4-6319)
;----------------------------------------------------

function month_str2intstr, month_str
	;-------------------------------------------
	; Convert month name into string version of 
	; integer (where 1-9 have a leading zero).
	;-------------------------------------------
	if month_str eq 'Jan' then month_intstr = '01'
	if month_str eq 'Feb' then month_intstr = '02'
	if month_str eq 'Mar' then month_intstr = '03'
	if month_str eq 'Apr' then month_intstr = '04'
	if month_str eq 'May' then month_intstr = '05'
	if month_str eq 'Jun' then month_intstr = '06'
	if month_str eq 'Jul' then month_intstr = '07'
	if month_str eq 'Aug' then month_intstr = '08'
	if month_str eq 'Sep' then month_intstr = '09'
	if month_str eq 'Oct' then month_intstr = '10'
	if month_str eq 'Nov' then month_intstr = '11'
	if month_str eq 'Dec' then month_intstr = '12'
	return, month_intstr
end
