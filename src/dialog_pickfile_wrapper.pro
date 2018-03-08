@GetDirectoryDivider.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ dialog_pickfile_wrapper @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION dialog_pickfile_wrapper,						$
				_Extra = e,					$
				WRITE = w,					$
				READ = r,					$
				DIR_ONLY = dir_only,				$
				GET_PATH = gp ; called out separately because
					      ; it is not a member of the structure e
					      ; even if the user specifies it (probably because
					      ; it returns information as opposed
					      ; to setting information
					 
	routine_name	= '========== misr_return_fill_values =========='
	
	IF SIZE(e,/TYPE) NE 0 THEN BEGIN     
		extra_tag_names	= STRTRIM(STRUPCASE(TAG_NAMES(e)),2)
		r_idx		= WHERE( extra_tag_names EQ 'MUST_EXIST', r_cnt )
		IF KEYWORD_SET( r ) AND r_cnt LE 0 THEN					$
			e = CREATE_STRUCT(e,'MUST_EXIST',1,'READ',1)
		IF KEYWORD_SET( w ) THEN						$
			e = CREATE_STRUCT(e,'WRITE',1)
	ENDIF
	
	CD, CURRENT = cur_dir
	
	WHILE 1 DO BEGIN



	;=======================================================================
	; basic error catch mechanism
	;=======================================================================
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		reason	= [ 'Problem with specified directory... may not exist...' ]
		res	= DIALOG_MESSAGE( reason, /INFORMATION )
		CD, cur_dir
	ENDIF
	
	IF SIZE(e,/TYPE) NE 0 THEN BEGIN     
		filename	= DIALOG_PICKFILE( _Extra = e, GET_PATH = return_path )
	ENDIF ELSE BEGIN
		filename	= DIALOG_PICKFILE( GET_PATH = return_path )
	ENDELSE
	gp		= return_path
	
	CD, return_path
	
	;=======================================================================
	; In IDL 5.4, if "Cancel" is pressed on the DIALOG_PICKFILE interface,
	; a null string is returned.  In this case, just return the null string.
	;=======================================================================
	IF filename[0] EQ '' THEN RETURN, ''
	
	;=======================================================================
	; In IDL 5.4, if "OK" is pressed on the DIALOG_PICKFILE interface, one of
	; two types of strings are returned.  If the user has specified a
	; filename and then pressed "OK", the returned string is a fully-qualified
	; name of the file selected.  IF, however, the user has pressed "OK" and
	; NOT entered anything, the return string is NOT null and is only
	; the fully-qualified directory path WITHOUT a filename attached.
	;=======================================================================
	n_files				= N_ELEMENTS( filename )
	respecify	= 0

	IF n_files EQ 1 THEN BEGIN
		existing_file			= FINDFILE( filename, COUNT = file_cnt )
		only_directory_specified	= STRTRIM(gp,2) EQ STRTRIM(filename,2)
		IF NOT only_directory_specified THEN BEGIN
			filename_only			= STRMID(filename,STRLEN(gp))
			pos				= STRPOS( filename_only, GetDirectoryDivider() )
			IF pos GE 0 THEN BEGIN
				respecify	= 1
				reason		= [ 'Problem with fully-qualified file name:',	$
						    filename,					$
						    'Be careful when using special characters',	$
						    'for directory references.' ]
			ENDIF
		ENDIF
	ENDIF ELSE BEGIN
		FOR i = 0, n_files - 1 DO BEGIN
			existing_file			= FINDFILE( filename[i], COUNT = cnt )
			IF i EQ 0 THEN file_cnt = cnt ELSE file_cnt = file_cnt + cnt
		ENDFOR
	ENDELSE
	
	CASE 1 OF
		file_cnt EQ 1: BEGIN
			END
		only_directory_specified: BEGIN
			IF KEYWORD_SET(dir_only) THEN RETURN, filename
			respecify	= 1
			reason		= [ 'No file name entered.' ]
			END
		file_cnt GE 1 AND n_files GE 1 AND file_cnt NE n_files: BEGIN ;works even with /MULTIPLE_FILES
			respecify	= 1
			reason		= [ 'Selected file(s) map to a different number of file names.' ]
			END
		file_cnt EQ 0: BEGIN
			END
		ELSE: BEGIN
			END
	ENDCASE
	
	IF NOT respecify THEN RETURN, filename
	
	res	= DIALOG_MESSAGE( reason, /INFORMATION )
	
	ENDWHILE
	
				
END
; dialog_pickfile_wrapper
