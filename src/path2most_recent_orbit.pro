;----------------------------------------------------
; path2most_recent_orbit.pro
;
; Determine the most recent orbit number that matches 
; the specified path number.
;
; Author:  Jeffrey.R.Hall@jpl.nasa.gov (x4-4149)
; Written: 1998
; Project: MISR, David.J.Diner@jpl.nasa.gov (x4-6319)
;----------------------------------------------------

@most_recent_orbit

function path2most_recent_orbit, path, Nref, JNref, PNref	;(PNref optional)

;print,'path2most_recent_orbit:  path = ',path
	if n_params() eq 3 then PNref = path

	;-------------------------------------------
	; Orient the calculation by getting the most 
	; recent orbit nuber.
	;-------------------------------------------
	mostRecentOrbit = most_recent_orbit( -1, Nref, JNref )

	;-------------------------------------------
	; Generate list of path numbers 
	; corresponding to the most recent 233 
	; orbits.
	;-------------------------------------------
	pathsOfLast233Orbits = lonarr( 233 )
	for i = 0, 233 - 1 do $
		pathsOfLast233Orbits[i] = orbit2path( mostRecentOrbit - i, Nref, PNref )

	;-------------------------------------------
	; Find the indices where the list of path 
	; numbers matches the path number.
	;-------------------------------------------
	orbitIndices = where( pathsOfLast233Orbits eq path )

	;-------------------------------------------
	; The largest matching index equals the 
	; number-of-paths-ago on which the matching 
	; path occured, so subtract that index value 
	; from the most recent orbit number to 
	; obtain the most recent orbit number 
	; corresponding to the path in question.
	;-------------------------------------------
	orbit = mostRecentOrbit - max( orbitIndices )

	return, orbit

; ALTERNATE METHOD:  Most recent orbit on initial path:
;	N = ( long( 233.0 / 16.0 * ( J - JNref ) ) / 233L ) * 233 + Nref


end
