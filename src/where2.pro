FUNCTION where2, data, flag_values, INVERSE = inverse
	valid_types	= [1,2,3,4,5,7,12,13,14,15]
	IDX		= WHERE(valid_types EQ SIZE(data,/TYPE), cnt)
	IF cnt LE 0 THEN BEGIN
		res	= DIALOG_MESSAGE([	$
			'where2: incoming data type ('+STRTRIM(SIZE(data,/TYPE),2)+')',	$
			'not supported!'],/ERROR)
		RETURN,[-1L]
	ENDIF

	flag_values	= [ flag_values ]
	idx		= [-1L]
	;
	; What is this (below)?  We want to make a copy of the
	; data for the purpose of creating a mask, but we want the
	; mask to be a byte array of all zeros.  With all supported
	; data types OTHER THAN STRING, this is easy.  However, in the
	; case of STRING, we have to assume that the contents of the
	; string array may be not numerical (e.g., 'entry1','entry2', etc.)
	; To get around this, we append a character '0' to the beginning
	; of each element of the array, then multiply it by zero before converting
	; it to byte.  In the example above, we would now have '0entry1', 0entry2',
	; and IDL does not throw any error messages when strings like this are
	; multiplied by numbers.
	;
	datacopy	= BYTE(('0'+data)*0)

	IF NOT KEYWORD_SET(inverse) THEN BEGIN
		FOR i = ULONG64(0), ULONG64(N_ELEMENTS(flag_values) - 1) DO BEGIN
			idx	= WHERE( data EQ flag_values[i], cnt )
			IF cnt GT 0 THEN datacopy[idx] = 1B
		ENDFOR
		RETURN, WHERE(datacopy GT 0)
	ENDIF ELSE BEGIN
		FOR i = ULONG64(0), ULONG64(N_ELEMENTS(flag_values) - 1) DO BEGIN
			idx	= WHERE( data NE flag_values[i] AND datacopy EQ i, cnt )
			IF cnt GT 0 THEN datacopy[idx] = datacopy[idx] + 1B
		ENDFOR
		RETURN, WHERE(datacopy GE N_ELEMENTS(flag_values))
	ENDELSE

END
; where2
