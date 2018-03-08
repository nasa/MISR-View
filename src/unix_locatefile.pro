;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ unix_locatefile @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION unix_locatefile, qualifiedDir, filename, cnt
	TRUE		= 1
	FALSE		= 0
	returnVal	= ''
	cnt		= 0
	
	IF NOT KEYWORD_SET(dirDivider) THEN dirDivider = '/'
	qualifiedDir	= STRTRIM(qualifiedDir,2)
	filename	= STRTRIM(filename,2)
	endDividerExists=						$
	   STRMID(qualifiedDir,STRLEN(qualifiedDir)-1,1) EQ dirDivider
	IF NOT endDividerExists THEN qualifiedDir = qualifiedDir +	$
	   dirDivider

	;-----------------------------------------------------------------------
	;search in current directory for filename
	;-----------------------------------------------------------------------
	res	= FINDFILE(qualifiedDir+filename,COUNT = cnt)
	IF cnt GT 0 THEN RETURN, qualifiedDir + filename

	;-----------------------------------------------------------------------
	;force directories to have slash appended to end
	;-----------------------------------------------------------------------
	SPAWN,'ls -lF ' + qualifiedDir + '| grep drw', res
	found	= FALSE
	idx		= 0
	WHILE NOT found AND idx LT N_ELEMENTS(res) DO BEGIN
		sep		= STR_SEP(res[idx],' ')
		tmpDir	= STRTRIM(sep[N_ELEMENTS(sep)-1],2)
		;---------------------------------------------------------------
		; make sure we're dealing with a directory
		;---------------------------------------------------------------
		IF STRMID(tmpDir,STRLEN(tmpDir)-1,1) EQ dirDivider THEN BEGIN
			dir2Check	= qualifiedDir+tmpDir
			returnVal	= unix_locatefile(dir2Check,filename,cnt)
			IF returnVal NE '' THEN found	= TRUE
		ENDIF
		idx	= idx + 1
	ENDWHILE

	RETURN, returnVal
END
; unix_locatefile
