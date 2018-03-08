;----------------------------------------------------
; Author:  Jeffrey.R.Hall@jpl.nasa.gov (x4-4149)
; Written: 1998
; Project: MISR, David.J.Diner@jpl.nasa.gov (x4-6319)
;----------------------------------------------------

@julianday

function most_recent_orbit, date, Nref, JNref
	;-------------------------------------------
	; calculate mostRecentOrbit from date.
	; "date" must be given in the format:
	;	yyyymmddhhmmss
	; Example:
	;	19980415165839
	; Otherwise, (if date is given as -1), then 
	; unix system time is used.
	; 	(Thu Apr 16 16:58:39 PDT 1998)
	;-------------------------------------------
	if date eq -1 then $
		date = systime2datetime()
	J = julianday( date )

	;-------------------------------------------
	; Equation from memo "EOS orbit information" 
	; by Dave Diner, March 9, 1998.
	;-------------------------------------------
	N = long( 233.0 / 16.0 * ( J - JNref ) ) + Nref

	return, N
end
