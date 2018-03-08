;----------------------------------------------------
; Author:  Charles.K.Thompson@jpl.nasa.gov (x4-9602)
;  and Jeffrey.R.Hall@jpl.nasa.gov (x4-4149)
; Written: 1998
; Project: MISR, David.J.Diner@jpl.nasa.gov (x4-6319)
;----------------------------------------------------

FUNCTION orbit2path, orbit, Nref, PNref

	;-------------------------------------------
	; Updated equation from Veljko 7-21-99.
	;-------------------------------------------
	N = LONG( orbit )
	M = 16L * ( N - Nref )
	pathPrime = ( M - 233L * FIX( M / 233.0 ) ) + PNref

	;-------------------------------------------
	; Minor modification to Veljko's equation by 
	; J Hall Dec 9, 1999
	;-------------------------------------------
	WHILE pathPrime GT 233L DO $
		pathPrime = pathPrime - 233L
	WHILE pathPrime LE 0L DO $
		pathPrime = pathPrime + 233L

	RETURN, pathPrime
END

;	;ORIGINAL VERSION, DECLARED INCORRECT BY DAVE DINER, JUNE 1999.
;	;-------------------------------------------
;	; Equations from memo "EOS orbit information" 
;	; by Dave Diner, March 9, 1998.
;	;-------------------------------------------
;	N = long( orbit )
;	M = 16L * ( N - Nref ) + PNref
;	path = M - ( 233L * long( ( float( M ) - 1.0 ) / 233.0 ) )
