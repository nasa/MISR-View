;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ get_make_info_eh @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO get_make_info_eh, event
	WIDGET_CONTROL, event.id, GET_UVALUE = widget_type
	WIDGET_CONTROL, event.top, GET_UVALUE = info_ptr
	
	CASE STRUPCASE(STRTRIM(widget_type,2)) OF
		'SELECT_SHELL_DIR': BEGIN
			WIDGET_CONTROL, (*info_ptr).cw3, GET_VALUE = script_str
			script_str	= script_str[0]
			tmp	= DIALOG_PICKFILE(TITLE = 'Select the SHELL SCRIPTS directory', /DIRECTORY, GET_PATH = dir, PATH = script_str )
			IF STRTRIM(dir,2) EQ '' THEN RETURN
			WIDGET_CONTROL, (*info_ptr).cw3, SET_VALUE = dir
			END
		'SELECT_RELEASE_DIR': BEGIN
			WIDGET_CONTROL, (*info_ptr).cw4, GET_VALUE = dir_str
			dir_str	= dir_str[0]
			tmp	= DIALOG_PICKFILE(TITLE = 'Select the RELEASE directory', /DIRECTORY, GET_PATH = dir, PATH = dir_str )
			IF STRTRIM(dir,2) EQ '' THEN RETURN
			WIDGET_CONTROL, (*info_ptr).cw4, SET_VALUE = dir
			END
		'SELECT_ANCILLARY_DIR': BEGIN
			WIDGET_CONTROL, (*info_ptr).cw2, GET_VALUE = ancillary_str
			ancillary_str	= ancillary_str[0]
			tmp	= DIALOG_PICKFILE(TITLE = 'Select the ANCILLARY FILES directory', /DIRECTORY, GET_PATH = dir, PATH = ancillary_str )
			IF STRTRIM(dir,2) EQ '' THEN RETURN
			WIDGET_CONTROL, (*info_ptr).cw2, SET_VALUE = dir
			END
		'VERSION': BEGIN
			WIDGET_CONTROL, event.id, GET_VALUE = release_str
			release_str	= release_str[0]
;;;ckt,sep2004			current_dir	= (*info_ptr).dir
;;;ckt,sep2004			IF STRMID(current_dir,STRLEN(current_dir)-1,1) NE PATH_SEP() THEN	$
;;;ckt,sep2004				current_dir	= current_dir + PATH_SEP()
;;;ckt,sep2004			IF (*info_ptr).is_beta THEN						$
;;;ckt,sep2004				new_dir	= current_dir + 'V' + STRTRIM(release_str,2) + '_beta_release/'	$
;;;ckt,sep2004			ELSE									$
;;;ckt,sep2004				new_dir	= current_dir + 'V' + STRTRIM(release_str,2) + '_release/'
;;;ckt,sep2004			WIDGET_CONTROL, (*info_ptr).cw4, SET_VALUE = new_dir
			END
		'OK': BEGIN
			bad_info	= 0
			
			WIDGET_CONTROL, (*info_ptr).cw1, GET_VALUE = release_str
			release_str	= release_str[0]
			IF STRTRIM(release_str,2) EQ '' THEN bad_info = 1
			
			WIDGET_CONTROL, (*info_ptr).cw2, GET_VALUE = ancillary_str
			ancillary_str	= ancillary_str[0]
			IF STRTRIM(ancillary_str,2) EQ '' THEN bad_info = 1
			
			script_str	= ''
			IF (*info_ptr).is_beta THEN WIDGET_CONTROL, (*info_ptr).cw3, GET_VALUE = script_str
			script_str	= script_str[0]
			IF STRTRIM(script_str,2) EQ '' AND (*info_ptr).is_beta THEN bad_info = 1
			
			WIDGET_CONTROL, (*info_ptr).cw4, GET_VALUE = dir_str
			dir_str	= dir_str[0]
			IF STRTRIM(dir_str,2) EQ '' THEN bad_info = 1
			
			IF bad_info THEN BEGIN
				msg	= 'Null entry encountered... check text entries!'
				res	= DIALOG_MESSAGE( msg, /ERROR )
				RETURN
			ENDIF
			ancillary_str	= STRTRIM(ancillary_str,2)
			script_str	= STRTRIM(script_str,2)
			dir_str		= STRTRIM(dir_str,2)
			IF STRMID(ancillary_str,STRLEN(ancillary_str)-1,1) NE '/' THEN	$
				ancillary_str	= ancillary_str + '/'
			IF (*info_ptr).is_beta THEN BEGIN
				IF STRMID(script_str,STRLEN(script_str)-1,1) NE '/' THEN	$
					script_str	= script_str + '/'
			ENDIF
			IF STRMID(dir_str,STRLEN(dir_str)-1,1) NE '/' THEN	$
				dir_str	= dir_str + '/'
			(*info_ptr).version_string	= release_str
			(*info_ptr).ancillary_file_dir	= ancillary_str
			(*info_ptr).script_dir		= script_str
			(*info_ptr).dir			= dir_str
			WIDGET_CONTROL, event.top, /DESTROY
			END
		'CANCEL': BEGIN
			(*info_ptr).cancel_pressed	= 1
			WIDGET_CONTROL, event.top, /DESTROY
			END
		ELSE:
	ENDCASE
END
;get_make_info_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ get_make_info @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION get_make_info, ancillary_file_dir, script_dir, release_dir, BETA = beta
	IF KEYWORD_SET(beta) THEN do_beta = 1 ELSE do_beta = 0
	
	b	= WIDGET_BASE( TITLE = 'Enter/Verify The Information Below',	$
			/COLUMN, /BASE_ALIGN_CENTER,				$
			GROUP_LEADER = WIDGET_BASE(), /MODAL )
	IF do_beta THEN	$
		cw1	=							$
			CW_FIELD( b, /ROW, TITLE = 'MISR_VIEW Release Version (beta generated automatically):',	$
				UVALUE = 'version', /ALL_EVENTS )		$
	ELSE									$
		cw1	=							$
			CW_FIELD( b, /ROW, TITLE = 'MISR_VIEW Release Version:',$
				UVALUE = 'version', /ALL_EVENTS )
	
	cw2_base= WIDGET_BASE(						$
			b,						$
			/ROW )
	cw2	= CW_FIELD( cw2_base, /ROW, TITLE = 'Ancillary File Directory:', 	$
			VALUE = ancillary_file_dir, UVALUE = 'ancillary' )
	cw2_btn	= WIDGET_BUTTON(					$
			cw2_base,					$
			VALUE = 'Select...',				$
			UVALUE = 'select_ancillary_dir' )
	cw3	= (-1L)


	IF do_beta THEN BEGIN
		cw3_base= WIDGET_BASE(						$
				b,						$
				/ROW )
		cw3	= CW_FIELD( cw3_base, /ROW, TITLE = 'BETA Shell Script Directory:', 	$
				VALUE = script_dir, UVALUE = 'shell' )
		cw3_btn	= WIDGET_BUTTON(					$
				cw3_base,					$
				VALUE = 'Select...',				$
				UVALUE = 'select_shell_dir' )
		cw4_base= WIDGET_BASE(						$
				b,						$
				/ROW )
		cw4	= CW_FIELD( cw4_base, /ROW, TITLE = 'BETA Release Directory:', 	$
				VALUE = release_dir, UVALUE = 'release', XSIZE=100 )
		cw4_btn	= WIDGET_BUTTON(					$
				cw4_base,					$
				VALUE = 'Select...',				$
				UVALUE = 'select_release_dir' )
	ENDIF ELSE BEGIN
		cw4_base= WIDGET_BASE(						$
				b,						$
				/ROW )
		cw4	= CW_FIELD( cw4_base, /ROW, TITLE = 'Release Directory:', 	$
				VALUE = release_dir, UVALUE = 'release', XSIZE=100 )
		cw4_btn	= WIDGET_BUTTON(					$
				cw4_base,					$
				VALUE = 'Select...',				$
				UVALUE = 'select_release_dir' )
	ENDELSE
	sb	= WIDGET_BASE( b, /ROW, /BASE_ALIGN_CENTER )
	ok	= WIDGET_BUTTON( sb, VALUE = 'OK', UVALUE = 'ok' )
	cancel	= WIDGET_BUTTON( sb, VALUE = 'Cancel', UVALUE = 'cancel' )
	WIDGET_CONTROL, b, /REALIZE
	
	info_ptr	= PTR_NEW( {						$
					ancillary_file_dir:ancillary_file_dir,	$
					script_dir:script_dir, dir:release_dir,	$
					version_string:'',			$
					cw1:cw1, cw2:cw2, cw3:cw3, cw4:cw4,	$
					cancel_pressed:0, is_beta:do_beta }, /NO_COPY )
	WIDGET_CONTROL, b, SET_UVALUE = info_ptr
	XMANAGER, 'get_make_info', b, EVENT_HANDLER = 'get_make_info_eh'
	
	struct2return	= *info_ptr
	PTR_FREE, info_ptr
	RETURN, struct2return
END
; get_make_info

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ make @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO make, BETA = beta

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when 
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== make =========='
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		e_msg	= [											$
				routine_name,									$
				'Error Index: ' + STRTRIM( error_status, 2 ),					$
				'Error Message: ' + !ERR_STRING,						$
				'',										$
				'Suggestion: If attempting to read or write a file, then check permissions.',	$
				'',										$
				'Returning...' ]
		result	= DIALOG_MESSAGE( e_msg, /ERROR )
		RETURN
	ENDIF
	
	IF KEYWORD_SET(beta) THEN do_beta = 1 ELSE do_beta = 0
	
	
	;=======================================================================
	; Create a function that checks to make sure that the save files
	; containing routines and variables were created at the same time
	;=======================================================================
	time_tag		= STRTRIM(ROUND(SYSTIME(/JULIAN)),2)
	verify_func		= 'verify_save_files'
	OPENW, lun, verify_func+'.pro', /GET_LUN
	PRINTF, lun, 'FUNCTION '+verify_func+', time_tag2check'
	PRINTF, lun, '	time_tag	= '+time_tag
	PRINTF, lun, '	IF time_tag NE time_tag2check THEN RETURN, 0'
	PRINTF, lun, '	RETURN, 1'
	PRINTF, lun, 'END'
	PRINTF, lun, '; '+verify_func
	FREE_LUN, lun
	
	;=======================================================================
	; Convert transform files to functions
	;=======================================================================
	convert_transforms

;;;ckt,sep2004	idl_save_version	= STRTRIM(STRING(!VERSION.RELEASE),2)
;;;ckt,sep2004	SAVE, /VARIABLES, FILENAME = 'idl_save_version.sav'

	ancillary_file_dir	= '/data/vis/vesa/misr_grid_viewer/ancillary_files/'
	
	IF do_beta THEN BEGIN
		script_dir	= '/data/vis/vesa/misr_grid_viewer/'
		release_dir	= '/data/vis/vesa/misr_grid_viewer/'
	ENDIF ELSE BEGIN
		;===============================================================
		; There is no script directory for an actual release.
		;===============================================================
		script_dir	= ''
		release_dir	= '/data/vis/vesa/misr_grid_viewer/release/'
	ENDELSE

	info_struct		= get_make_info( ancillary_file_dir, script_dir, release_dir, BETA = do_beta )
	IF info_struct.cancel_pressed THEN RETURN
	
	version_string		= 'misr_view' + info_struct.version_string
	ancillary_file_dir	= info_struct.ancillary_file_dir
	script_dir		= info_struct.script_dir
	release_dir		= info_struct.dir
		
	;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	; Grab all files in current directory with suffixes ".pro" and ".PRO"
	;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	file_list1	= FINDFILE( "*.pro", COUNT = cnt1 )
	file_list2	= FINDFILE( "*.PRO", COUNT = cnt2 )
	file_list	= STRTRIM( [ file_list1, file_list2 ], 2 )
	
	;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	; Locate entries for misr_view.pro and GEOREF_IMAGE.PRO; these two modules
	; need to be compiled after all others
	;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	idx1	= WHERE( file_list EQ 'GEOREF_IMAGE.PRO', cnt1 )
	idx2	= WHERE( file_list EQ 'misr_view.pro', cnt2 )
	idx3	= WHERE( file_list EQ 'make.pro', cnt3 )
	
;;;ckt,mar2000	pos1	= STRPOS( file_list, 'GEOREF_IMAGE.PRO' )
;;;ckt,mar2000	pos2	= STRPOS( file_list, 'misr_view.pro' )
;;;ckt,mar2000	pos3	= STRPOS( file_list, 'make.pro' )
;;;ckt,mar2000	idx1	= WHERE( pos1 GE 0, cnt1 )
;;;ckt,mar2000	idx2	= WHERE( pos2 GE 0, cnt2 )
;;;ckt,mar2000	idx3	= WHERE( pos3 GE 0, cnt2 )
	
	temp1	= file_list[N_ELEMENTS(file_list)-2]
	temp2	= file_list[N_ELEMENTS(file_list)-1]
	file_list[N_ELEMENTS(file_list)-2]	= file_list[idx1[0]]
	file_list[N_ELEMENTS(file_list)-1]	= file_list[idx2[0]]
	file_list[idx1[0]]			= temp1
	file_list[idx2[0]]			= temp2
	
	IF idx3[0] EQ 0 THEN BEGIN
		file_list	= file_list[1:N_ELEMENTS(file_list)-1]
	ENDIF
	
	IF idx3[0] EQ N_ELEMENTS(file_list)-1 THEN BEGIN
		file_list	= file_list[0:N_ELEMENTS(file_list)-2]
	ENDIF
	
	IF idx3[0] GT 0 AND idx3[0] LT N_ELEMENTS(file_list)-1 THEN BEGIN
		file_list	= [ file_list[0:idx3[0]-1],file_list[idx3[0]+1:N_ELEMENTS(file_list)-1]]
	ENDIF
	
	file_list	= '@' + file_list
	
	OPENW, lun, 'compile_all', /GET_LUN
	FOR i = 0, N_ELEMENTS(file_list) - 1 DO PRINTF, lun, file_list[i]
	CLOSE, lun
	FREE_LUN, lun
	
	compile_list	= STRARR(100)
	ctr		= 0
	FOR j = 0, N_ELEMENTS(file_list), 10 DO BEGIN
		compile_list[ctr]	= 'compile' + STRTRIM(ctr+1,2)
	
		OPENW, lun, 'compile' + STRTRIM(ctr+1,2) + '.pro', /GET_LUN
	
		FOR i = j, MIN([N_ELEMENTS(file_list) - 1,j+9]) DO PRINTF, lun, file_list[i]
	
		PRINTF,lun,'PRO compile' + STRTRIM(ctr+1,2)
		PRINTF,lun,'END'
	
		CLOSE, lun
		FREE_LUN, lun
		ctr	= ctr + 1
	ENDFOR
	
	compile_list	= compile_list[0:ctr-1]
	
	FOR i = 0, ctr-1 DO RESOLVE_ROUTINE, compile_list[i]
	
	RESOLVE_ALL
	
;;;ckt,sep2004	save_filename	= version_string + '.sav'
	save_filename	= 'misr_view.sav'
	SAVE, FILENAME = save_filename, /ROUTINES
	
	FOR i = 0, ctr-1 DO SPAWN, 'rm ' + compile_list[i] + '.pro'
	
	
	locImg = READ_TIFF( ancillary_file_dir + 'backImg.tiff', r, g, b )
	
	asciiCornerFile		= ''
	headerSize		= 0L
	blockCornerFileName	= GETENV('AGP_BLOCK_CORNERS')
	
	IF blockCornerFileName NE '' THEN BEGIN
		OPENR, lun, blockCornerFileName, /GET_LUN
		headerSize = 3L
		asciiCornerFile = STRARR( 233L * 180L * 2L + headerSize )
		READF, lun, asciiCornerFile, FORMAT = '(a255)'
		CLOSE,lun
		FREE_LUN,lun
	ENDIF ELSE BEGIN
		blockCornerFileName	= DIALOG_PICKFILE(TITLE='Locate and select the AGP block corner file')
		IF blockCornerFileName NE '' THEN BEGIN
			OPENR, lun, blockCornerFileName, /GET_LUN
			headerSize = 3L
			asciiCornerFile = STRARR( 233L * 180L * 2L + headerSize )
			READF, lun, asciiCornerFile, FORMAT = '(a255)'
			CLOSE,lun
			FREE_LUN,lun
		ENDIF
	ENDELSE
	
	OPENR, lun, ancillary_file_dir + 'etop25.raw', /GET_LUN
	elevationMap	= BYTARR( 864, 432 )
	READU, lun, elevationMap
	CLOSE, lun
	FREE_LUN, lun
		
	version	= READ_TIFF(ancillary_file_dir + 'misr_number_letter.TIF')
	req_img	= READ_TIFF(ancillary_file_dir + 'require.TIF')
	logo	= READ_TIFF(ancillary_file_dir + 'misr_view_logo.TIF')
	
	;====================================================================
	; Special save for beta version; also copy idl_save_version.sav to beta
	; directory
	;====================================================================
	IF do_beta THEN BEGIN
                idl_save_version	= STRTRIM(STRING(!VERSION.RELEASE),2)
		misr_version		= info_struct.version_string + 'beta'
		SAVE, /VARIABLES, FILENAME = 'ancillary_variables.sav'
;;;;;;		SPAWN, 'mkdir ' + release_dir
		SPAWN, 'mv ancillary_variables.sav ' + release_dir
;;;ckt,sep2004	SPAWN, 'cp idl_save_version.sav ' + release_dir
		SPAWN, 'mv * ' + release_dir
		SPAWN, 'cp '+ ancillary_file_dir + '* ' + release_dir
	ENDIF ELSE BEGIN
                idl_save_version	= STRTRIM(STRING(!VERSION.RELEASE),2)
		misr_version		= info_struct.version_string
		SAVE, /VARIABLES, FILENAME = 'ancillary_variables.sav'
		SPAWN, 'mkdir ' + release_dir + 'external/'
		SPAWN, 'mkdir ' + release_dir + 'src/'
		SPAWN, 'mkdir ' + release_dir + 'ancillary/'
		SPAWN, 'mv ancillary_variables.sav ' + release_dir + 'external/'
		SPAWN, 'mv *.pro ' + release_dir + 'src/'
		SPAWN, 'mv *.PRO ' + release_dir + 'src/'
		SPAWN, 'mv compile_all ' + release_dir + 'src/'
		SPAWN, 'cp *.transform ' + release_dir + 'src/'
		SPAWN, 'mv *.transform ' + release_dir + 'external/'
		SPAWN, 'cp misr_view_DEFAULT_TRANSFORMS ' + release_dir + 'src/'
		SPAWN, 'mv misr_view_DEFAULT_TRANSFORMS ' + release_dir + 'external/'
		SPAWN, 'mv 00* ' + release_dir + 'src/'
		SPAWN, 'cp ' + release_dir + 'doc/00README_misr_view* ' + release_dir + 'external/'
		SPAWN, 'mv misr_view.sav ' + release_dir + 'external/'
		IF blockCornerFileName NE '' THEN SPAWN,'cp '+blockCornerFileName+' '+release_dir +'ancillary/'
		SPAWN, 'cp '+ ancillary_file_dir + '* ' + release_dir + 'ancillary/'
		SPAWN, 'mkdir ' + release_dir + 'external/ug'+STRTRIM(misr_version,2)+'_html/'
		SPAWN, 'cp ' + release_dir + 'doc/ug'+STRTRIM(misr_version,2)+'_html/* ' + release_dir + 'external/ug'+STRTRIM(misr_version,2)+'_html/'
                SPAWN, 'touch '+ release_dir + 'external/IDL_VERSION_'+idl_save_version+'.REQUIRED'
	ENDELSE

	SPAWN,'pwd',current_dir
print,'current_dir=',current_dir
	sep	= STR_SEP(STRTRIM(current_dir,2),PATH_SEP())
	IF sep[N_ELEMENTS(sep)-1] EQ 'staging' AND NOT do_beta THEN SPAWN, 'rm *'
	
;;;ckt,sep2004	misr_version		= info_struct.version_string
;;;ckt,sep2004	SAVE, /VARIABLES, FILENAME = 'ancillary_variables.sav'
		
	;====================================================================
	; Create scripts for BETA ONLY!!!!!
	;====================================================================
	
	IF do_beta THEN BEGIN
		OPENW, lun, script_dir + 'misr_view.batch', /GET_LUN
;;;ckt,apr2001	PRINTF, lun, "restore,'" + save_filename + "'"
		PRINTF, lun, '.compile compile_all'
		PRINTF, lun, 'misr_view'
		PRINTF, lun, 'exit'
		CLOSE, lun
		FREE_LUN, lun
	
		SPAWN, 'pwd', current_dir
		
		OPENW, lun, script_dir + 'misr_view.sh', /GET_LUN
		PRINTF, lun, '#! /bin/sh'
		PRINTF, lun, ''
;;;ckt,apr2001	PRINTF, lun, 'cd ' + STRTRIM(current_dir,2) + '/local'
		PRINTF, lun, 'cd ' + release_dir
;;;ckt,apr2001	PRINTF, lun, 'idl ' + script_dir + 'misr_view.batch > /dev/null 2>&1'
		PRINTF, lun, 'idl ' + script_dir + 'misr_view.batch'
		CLOSE, lun
		FREE_LUN, lun
	
		SPAWN, 'chmod 755 ' + script_dir + '*'
	ENDIF
	
;;;ckt,sep2004	SPAWN, 'mkdir local'
;;;ckt,sep2004	SPAWN, 'mkdir external'
;;;ckt,sep2004	SPAWN, 'mkdir src'
;;;ckt,sep2004	SPAWN, 'mkdir doc'
	
;;;ckt,sep2004	SPAWN, 'tar cvf external_' + version_string + '.tar *.sav *.doc, *.ps, *.html, *.txt, *.pdf, 00README_misr_view'
	
;;;ckt,sep2004	SPAWN, 'mv external_' + version_string + '.tar external/'
;;;ckt,sep2004	SPAWN, 'mv *.sav local/'
;;;ckt,sep2004	SPAWN, 'mv *.pro src/'
;;;ckt,sep2004	SPAWN, 'mv *.PRO src/'
;;;ckt,sep2004	SPAWN, 'mv 00README_misr_view src/'
;;;ckt,sep2004	SPAWN, 'mv make_misr_view.batch src/'
;;;ckt,sep2004	SPAWN, 'mv make_misr_view.sh src/'
;;;ckt,sep2004	SPAWN, 'mv *.doc doc/'
;;;ckt,sep2004	SPAWN, 'mv *.html doc/'
;;;ckt,sep2004	SPAWN, 'mv *.ps doc/'
;;;ckt,sep2004	SPAWN, 'mv *.txt doc/'
	
;;;ckt,sep2004	OPENW, lun, 'update_agp_block_corner_info.pro', /GET_LUN
;;;ckt,sep2004	PRINTF, lun, 'PRO update_agp_block_corner_info'
;;;ckt,sep2004	PRINTF, lun, "	SPAWN, 'cp local/*.sav .'"
;;;ckt,sep2004	PRINTF, lun, "	RESTORE, 'ancillary_variables.sav'"
;;;ckt,sep2004	PRINTF, lun, "	asciiCornerFile	= ''"
;;;ckt,sep2004	PRINTF, lun, "	blockCornerFileName	= GETENV('AGP_BLOCK_CORNERS')"
;;;ckt,sep2004	PRINTF, lun, "	IF blockCornerFileName NE '' THEN BEGIN"
;;;ckt,sep2004	PRINTF, lun, '		OPENR, lun, blockCornerFileName, /GET_LUN'
;;;ckt,sep2004	PRINTF, lun, '		headerSize = 3L'
;;;ckt,sep2004	PRINTF, lun, '		asciiCornerFile = STRARR( 233L * 180L * 2L + headerSize )'
;;;ckt,sep2004	PRINTF, lun, "		READF, lun, asciiCornerFile, FORMAT = '(a255)'"
;;;ckt,sep2004	PRINTF, lun, '		CLOSE,lun'
;;;ckt,sep2004	PRINTF, lun, '		FREE_LUN,lun'
;;;ckt,sep2004	PRINTF, lun, '	ENDIF'
;;;ckt,sep2004	
;;;ckt,sep2004	idl_save_version	= STRTRIM(STRING(!VERSION.RELEASE),2)
;;;ckt,sep2004
;;;ckt,sep2004	PRINTF, lun, "	SAVE, /VARIABLES, FILENAME = 'ancillary_variables.sav'"
;;;ckt,sep2004	PRINTF, lun, "	SPAWN, 'tar cvf external_' + version_string + '.tar *.sav *.doc, *.ps, *.html, *.txt, *.pdf'"
;;;ckt,sep2004	PRINTF, lun, "	SPAWN, 'mv external_' + version_string + '.tar external/'"
;;;ckt,sep2004	PRINTF, lun, "	SPAWN, 'mv *.sav local/'"
;;;ckt,sep2004	PRINTF, lun, 'END'
;;;ckt,sep2004	CLOSE, lun
;;;ckt,sep2004	FREE_lun, lun
	
;;;ckt,sep2004	OPENW, lun, '00README', /GET_LUN
;;;ckt,sep2004	PRINTF, lun, '@@@@@@@@@@@@@@@@@@@@@@@@@@@ 00README_directory_setup @@@@@@@@@@@@@@@@@@@@@@@@@@@'
;;;ckt,sep2004	PRINTF, lun, ''
;;;ckt,sep2004	PRINTF, lun, 'DATE GENERATED: ' + SYSTIME()
;;;ckt,sep2004	PRINTF, lun, ''
;;;ckt,sep2004	PRINTF, lun, ''
;;;ckt,sep2004	PRINTF, lun, '--------------------------------------------------------------------------------'
;;;ckt,sep2004	PRINTF, lun, '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> VERY IMPORTANT <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<'
;;;ckt,sep2004	PRINTF, lun, 'The MISR_VIEW SAVE files in the local subdirectories which contain MISR_VIEW'
;;;ckt,sep2004	PRINTF, lun, 'routines will be compatible ONLY WHEN RUNNING IDL ' + !VERSION.RELEASE
;;;ckt,sep2004	PRINTF, lun, '--------------------------------------------------------------------------------'
;;;ckt,sep2004	PRINTF, lun, ''
;;;ckt,sep2004	PRINTF, lun, ''
;;;ckt,sep2004	PRINTF, lun, 'This README explains the contents of this directory and its sub-directories'
;;;ckt,sep2004	PRINTF, lun, 'Please refer to the file 00README_misr_view located in the src/ directory'
;;;ckt,sep2004	PRINTF, lun, 'for any important information regarding the operation of MISR_VIEW.'
;;;ckt,sep2004	PRINTF, lun, ''
;;;ckt,sep2004	PRINTF, lun, 'Contents of the current directory:'
;;;ckt,sep2004	PRINTF, lun, ''
;;;ckt,sep2004	PRINTF, lun, '	update_agp_block_corner_info.pro: IDL procedure that should be run whenever'
;;;ckt,sep2004	PRINTF, lun, '		the AGP_BLOCK_CORNERS file has been updated.  This routine will update'
;;;ckt,sep2004	PRINTF, lun, '		the SAVE and TAR files located in the local/ and external/'
;;;ckt,sep2004	PRINTF, lun, '		subdirectories, respectively.'
;;;ckt,sep2004	PRINTF, lun, ''
;;;ckt,sep2004	PRINTF, lun, '	update_misr_view.batch: shell script which runs update_agp_block_corner_info.pro'
;;;ckt,sep2004	PRINTF, lun, ''
;;;ckt,sep2004	PRINTF, lun, '	update_misr_view.sh: shell script which runs invokes IDL and runs update_misr_view.batch'
;;;ckt,sep2004	PRINTF, lun, ''
;;;ckt,sep2004	PRINTF, lun, '	local/: subdirectory that contains the SAVE files used to invoke MISR_VIEW'
;;;ckt,sep2004	PRINTF, lun, '		locally through the shell scripts misr_view.sh and misr_view.batch.'
;;;ckt,sep2004	PRINTF, lun, ''
;;;ckt,sep2004	PRINTF, lun, '	external/: subdirectory that contains a TAR file containing everything that is'
;;;ckt,sep2004	PRINTF, lun, '		necessary to run MISR_VIEW externally.  Also includes all documentation.'
;;;ckt,sep2004	PRINTF, lun, ''
;;;ckt,sep2004	PRINTF, lun, '	src/: subdirectory that contains all source files for the current version of'
;;;ckt,sep2004	PRINTF, lun, '		MISR_VIEW.'
;;;ckt,sep2004	PRINTF, lun, ''
;;;ckt,sep2004	PRINTF, lun, '	doc/: subdirectory that contains all documentation for the current version'
;;;ckt,sep2004	PRINTF, lun, '		of MISR_VIEW.'
;;;ckt,sep2004	PRINTF, lun, ''
;;;ckt,sep2004	PRINTF, lun, 'Any questions regarding the information contained herein should be addressed'
;;;ckt,sep2004	PRINTF, lun, 'to Charles Thompson (Charles.K.Thompson@jpl.nasa.gov) or Jeffrey R. Hall'
;;;ckt,sep2004	PRINTF, lun, '(Jeffrey.R.Hall@jpl.nasa.gov).'
;;;ckt,sep2004	CLOSE, lun
;;;ckt,sep2004	FREE_lun, lun
END
; make

