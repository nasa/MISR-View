;+
;NAME:
;	julianday
;PURPOSE:
;	Calculate the Julian Day of the specified date and time (to the second).
;	Internal calculations are double precision floating point.
;CALLING SEQUENCE:
;	result = julianday( '19970306114733' )
;	This returns the decimal Julian Day for March 6, 1997, at 11:47:32 UT.
;INPUT:
;	'yyyymmddhhmmss' - dateTime 14 character string.
;       The string is all numbers, but still needs to be input as a string.
;OPTIONAL INPUT KEYWORDS:
;	None.
;OUTPUT:
;	double( 0 )
;	Double precision floating point scalar Julian Day representation.
;	Precise to the second at the 5th decimal place.  eg, format='(d24.5)'
;	Remember these are decimal days, so seconds are resolved at the 5th 
;	decimal place which can sometimes jump by 2.  (There are 100000 
;	possibilities at the 5th decimal place, but only 86400 seconds are 
;	in a day, so about 8% of the time the 5th decimal will jump by 2.)
;HISTORY:
;	Written March 1997 by Jeffrey R. Hall, jeff@basis.jpl.nasa.gov
;COPYRIGHT:
;	California Institute of Technology
;	Jet Propulsion Laboratory
;	1996, 1997, 2001
;-

function julianday, dateTime
	; From "Atronomical Algorithms" by Jean Meeus (1991, Willmann-Bell)
	; Implemented in IDL by Jeff Hall, Jet Propulsion Laboratory, March 1997.

   	jd = double( 0 )

	y  = double( strmid( dateTime,  0, 4 ) )
	mo = double( strmid( dateTime,  4, 2 ) )
	d  = double( strmid( dateTime,  6, 2 ) )
	h  = double( strmid( dateTime,  8, 2 ) )
	mi = double( strmid( dateTime, 10, 2 ) )
	s  = double( strmid( dateTime, 12, 2 ) )
	d  = d + h/24.0 + mi/60.0/24.0 + s/60.0/60.0/24.0
	if ( mo eq 1 ) or ( mo eq 2 ) then begin
		y = y - 1.0
		mo = mo + 12.0
	endif

	A = long( y / 100.0 )
	B = 2.0 - A + long( A / 4.0 )

	jd = long( 365.25 * ( y + 4716 ) ) + long( 30.6001 * $
		( mo + 1.0 ) ) + d + B - 1524.5

	return, jd
end
