FUNCTION lon2path, longitude, pathOne_Longitude, PNref
	;-------------------------------------------
	; Convert longitude (at equator) to path number.
	; FROM DAVE DINER'S FORMULA MEMO OF 9MAR98.
	; Coded by Jeff Hall 7JUL99.
	;-------------------------------------------
	L		= longitude		;longitude.
	theta		= 0.0			;latitude always at equator.
	S		= 16.0*360.0/233.0	;~24.721
	LrefTheta	= pathOne_Longitude
	deltaL		= LrefTheta - L
	IF deltaL < 0.0 THEN deltaL = deltaL + 360.0
	Pprime		= INTARR( 360 )
	FOR i = 0, 360 - 1 DO $
		Pprime[i] = MIN( ABS( deltaL - [i - PNref] * (S / 16.0) ) )
	Pprime		= (WHERE( Pprime EQ MIN( Pprime ) ))[0]
	newPathNumber	= Pprime - 233 * FIX( (Pprime - 1) / 233 )
;print,'lon2path:  longitude of path at equator = ',longitude
;print,'lon2path:  pathOne_Longitude = ',pathOne_Longitude
;print,'lon2path:  PNref = ',PNref
;print,'lon2path:  (formula result) newPathNumber = ',newPathNumber
;print,''

	RETURN, newPathNumber
end

