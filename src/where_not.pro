;---------------------------------------------------------------------------
; where_not.pro
;
; PURPOSE:	Designed to solve the problem of finding good data values 
;		when there is an arbitrary number of "bad" data flag values.
; METHOD:	Constructs a WHERE statement including each element 
;		of the flagValues array as an explicit condition, then 
;		EXECUTEs it and returns the statement's result.
; INPUTS:	array of data
;		array of flagValues
; OUTPUTS:	Returns indices into data where flagValues are *NOT FOUND*.
;		-1 indicates that WHERE found no matches (no "good" data).
;		-2 indicates an error occured in EXECUTE during either 
;		the compile or execution of the WHERE statement.
; EXAMPLE:	data=[2,3,4,5,6,7,8]
;		flagValues=[2,4,6,8]
;		print,data[where_not(data,flagValues)]
;		       3       5       7
; WRITTEN BY:	jeffrey.r.hall@jpl.nasa.gov
;		23 Feb 1999
;		for MISR
;---------------------------------------------------------------------------
FUNCTION where_not, data, flagValues

;;;ckt, apr99
; had to convert flagValues to DOUBLE due to fact that stringifying BYTES causes problem
; e.g., string(0B) does NOT equal 0

flagValues	= DOUBLE(flagValues)

FOR i = 0, N_ELEMENTS( flagValues ) - 1 DO BEGIN
	IF i EQ 0 THEN $
		whereStatement = 'whereNotResult = WHERE( data NE ' + STRTRIM( STRING( flagValues[ i ] ), 2 ) $
	ELSE $
		whereStatement = whereStatement + ' AND data NE ' + STRTRIM( STRING( flagValues[ i ] ), 2 )
	IF i EQ N_ELEMENTS( flagValues ) - 1 THEN $
		whereStatement = whereStatement + ' )'
ENDFOR

IF EXECUTE( whereStatement ) THEN $
	RETURN, whereNotResult $
ELSE $
	RETURN, -2

END
