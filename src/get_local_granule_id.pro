;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ get_local_granule_id @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION get_local_granule_id, filename
	ptr	= misr_get_meta_info( filename, METADATA_NAME = 'coremetadata' )
	
	IF LONG(TOTAL(PTR_VALID(ptr))) NE N_ELEMENTS(ptr) THEN BEGIN
;print,'could not find COREMETADATA'
		eMsg	= 'Could not find metadata: COREMETADATA '
		res	= DIALOG_MESSAGE( eMsg, /ERROR )
		PTR_FREE, ptr
		RETURN, ''
	ENDIF
	
	str1	= STRUPCASE(STRING(*(ptr[0])))
	PTR_FREE, ptr
	
	idx1	= STRPOS( str1, 'GRANULEID' )
	IF idx1[0] LT 0 THEN RETURN, ''
	idx2	= STRPOS( str1, 'GRANULEID', idx1[0] + 1 )
	
	IF idx2[0] LT 0 THEN RETURN, ''
	str2	= STRMID( str1, idx1[0], idx2[0] - idx1[0] + 1 )

	idx3	= STRPOS( str2, 'VALUE' )
	IF idx3[0] LT 0 THEN RETURN, ''
	
	idx4	= STRPOS( str2, '=', idx3[0] )
	IF idx4[0] LT 0 THEN RETURN, ''
	
	idx5	= STRPOS( str2, '"', idx4[0] )
	IF idx5[0] LT 0 THEN RETURN, ''
	
	idx6	= STRPOS( str2, '"', idx5[0] + 1 )
	IF idx6[0] LT 0 THEN RETURN, ''
	
	IF idx6[0] - idx5[0] - 1 LE 0 THEN RETURN, ''
	
	str3	= STRMID( str2, idx5[0] + 1, idx6[0] - idx5[0] - 1 )
	
	RETURN, str3
END
; get_local_granule_id
