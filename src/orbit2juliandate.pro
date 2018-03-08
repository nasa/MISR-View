;----------------------------------------------------
; Author:  Jeffrey.R.Hall@jpl.nasa.gov (x4-4149)
; Written: 1998
; Project: MISR, David.J.Diner@jpl.nasa.gov (x4-6319)
;----------------------------------------------------

function orbit2juliandate, N, Nref, JNref
	JN = JNref + 16.0 / 233.0 * ( N - Nref )
	return, JN
end
