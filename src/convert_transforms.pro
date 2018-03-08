@convert_transform_to_function.pro

PRO convert_transforms
;1234567890123456789012345678901234567890123456789012345678901234567890123456789
	CD, CURRENT = cdir
	cdir	= STRTRIM(cdir,2)
	IF STRMID(cdir,STRLEN(cdir)-1,1) NE PATH_SEP() THEN cdir = cdir + PATH_SEP()
	tfiles	= FILE_SEARCH(cdir+'*.transform',COUNT=tcnt)
	IF tcnt LE 0 THEN RETURN
	FOR i=0,tcnt-1 DO BEGIN
		CLOSE, /ALL
		convert_transform_to_function,tfiles[i]
	ENDFOR
END
; convert_transforms
