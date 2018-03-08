;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@ misr_get_default_transform_file @@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION misr_get_default_transform_file, local_granule_id
	COMMON MISRVIEWDATA, set_font, default_transform_directory
	
	ON_IOERROR, BAD
	
	file2use	= ''
	local_granule_id	= STRTRIM(STRUPCASE(local_granule_id),2)
;print,'default_transform_directory=',default_transform_directory
	default_transforms_file	= FINDFILE(				$
		default_transform_directory +				$
		'misr_view_DEFAULT_TRANSFORMS',				$
		COUNT = cnt )
;print,'---------> cnt=',cnt
	IF cnt LE 0 THEN RETURN, ''
	
	default_transforms_file	= STRTRIM( default_transforms_file[0], 2 )
	
	OPENR, lun, default_transforms_file, /GET_LUN
	str		= ''
	done		= 0
	WHILE NOT EOF(lun) AND NOT done DO BEGIN
		READF, lun, str
		str	= STRTRIM(str,2)
;print,'str = ',str
		IF str NE '' AND STRMID(str,0,1) NE ';' THEN BEGIN
			sep_str	= STR_SEP( str, '=' )
;print,'sep_str = ',sep_str
			IF N_ELEMENTS(sep_str) NE 2 THEN BEGIN
				msg	= [ 'Problem encountered while reading',	$
					    default_transforms_file ]
				reply	= DIALOG_MESSAGE( msg, /ERROR )
				done	= 1
			ENDIF
			entry2check	= STRTRIM(STRUPCASE(sep_str[0]),2)
;print,'entry2check = ',entry2check
;print,'local_granule_id = ',local_granule_id
			IF STRPOS( local_granule_id, entry2check ) GE 0 AND NOT done AND STRTRIM(sep_str[1],2) NE '' THEN BEGIN
				done		= 1
				file2use	= default_transform_directory+STRTRIM( sep_str[1], 2 )
				file2use_exists	= FINDFILE( file2use, COUNT = cnt )
				IF cnt LE 0 THEN BEGIN
					msg		= [ 'Cannot locate', file2use ]
					reply		= DIALOG_MESSAGE( msg, /ERROR )
					done		= 1
					file2use	= ''
				ENDIF
			ENDIF
		ENDIF
	ENDWHILE
	
	GOTO, DONE
	
BAD:	reply	= DIALOG_MESSAGE( [ 'Error encountered while attempting to read',	$
				     default_transforms_file,				$
				     !ERR_STRING ], /ERROR )
	
DONE:	FREE_LUN, lun
		
;print,'file2use = ',file2use
	RETURN, file2use
	
END
; misr_get_default_transform_file
