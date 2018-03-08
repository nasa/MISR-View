;---------------------------------------------------------------------------
; where_is.pro
;
; (Version of "where_not.pro".)
;
; PURPOSE:	Designed to solve the problem of finding good data values 
;		when there is an arbitrary number of "bad" data flag values.
; METHOD:	Constructs a WHERE statement including each element 
;		of the flagValues array as an explicit condition, then 
;		EXECUTEs it and returns the statement's result.
; INPUTS:	array of data
;		array of flagValues
; OUTPUTS:	Returns indices into data where flagValues are *FOUND*.
;		-1 indicates that WHERE found matches (ie, "good" data).
;		-2 indicates an error occured in EXECUTE during either 
;		the compile or execution of the WHERE statement.
; EXAMPLE:	data=[2,3,4,5,6,7,8]
;		flagValues=[2,4,6,8]
;		print,data[where_is(data,flagValues)]
;		       0           2           4           6
; WRITTEN BY:	jeffrey.r.hall@jpl.nasa.gov
;		charles.k.thompson@jpl.nasa.gov
;		22 Mar 1999
;		for MISR
;---------------------------------------------------------------------------
FUNCTION where_is, data, flagValues

FOR i = 0, N_ELEMENTS( flagValues ) - 1 DO BEGIN
	IF i EQ 0 THEN $
		whereStatement = 'whereNotResult = WHERE( data EQ ' + STRTRIM( STRING( flagValues[ i ] ), 2 ) $
	ELSE $
		whereStatement = whereStatement + ' OR data EQ ' + STRTRIM( STRING( flagValues[ i ] ), 2 )
	IF i EQ N_ELEMENTS( flagValues ) - 1 THEN $
		whereStatement = whereStatement + ' )'
ENDFOR

IF EXECUTE( whereStatement ) THEN $
	RETURN, whereNotResult $
ELSE $
	RETURN, -2	; This (-2) will probably never happen.

END
