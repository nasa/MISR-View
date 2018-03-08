;----------------------------------------------------
; Author:  Jeffrey.R.Hall@jpl.nasa.gov (x4-4149)
; Written: 1998
; Project: MISR, David.J.Diner@jpl.nasa.gov (x4-6319)
;----------------------------------------------------

@month_str2intstr

function systime2datetime
	;-------------------------------------------
	; Convert system time into numerical string:
	; from: 	Fri Apr 17 13:25:53 PDT 1998
	; to:		19980417132553
	;		yyyymmddhhmmss
	; This format is required for the julianday 
	; function.
	;-------------------------------------------
	now = systime()
	year = strmid( now, 20, 4 )
	month = strmid( now, 4, 3 )
	month = month_str2intstr( month )
	day = strmid( now, 8, 2 )
	hour = strmid( now, 11, 2 )
	minute = strmid( now, 14, 2 )
	second = strmid( now, 17, 2 )
	datetime = year + month + day + hour + minute + second
	return, datetime
end
