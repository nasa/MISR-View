@GetDirectoryDivider
@get_local_granule_id.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ list_bad_files_eh @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO list_bad_files_eh, event
	WIDGET_CONTROL, event.top, /DESTROY
END
; list_bad_files_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ list_bad_files @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO list_bad_files, file_strarr, group_leader, EXTRA_INFO = extra_info
	b	= WIDGET_BASE( TITLE = 'Problem File information', /COLUMN, /ALIGN_CENTER, /BASE_ALIGN_CENTER, GROUP_LEADER = group_leader )
	lbl1	= WIDGET_LABEL( b, VALUE = 'The files below were excluded from misr_view', /ALIGN_CENTER )
	lbl2	= WIDGET_LABEL( b, VALUE = 'for the reasons in parentheses', /ALIGN_CENTER )
	IF KEYWORD_SET(extra_info) THEN	$
		lbl3	= WIDGET_LABEL( b, VALUE = extra_info, /ALIGN_CENTER )
	list1	= WIDGET_LIST( b, VALUE = file_strarr, SCR_XSIZE = 600, SCR_YSIZE = 300, /ALIGN_CENTER, EVENT_PRO = 'list_bad_files_eh' )
	dismiss	= WIDGET_BUTTON( b, VALUE = 'Dismiss', /ALIGN_CENTER, EVENT_PRO = 'list_bad_files_eh' )
	WIDGET_CONTROL, b, /REALIZE
	XMANAGER, 'Problem File information', b, EVENT_HANDLER = 'list_bad_files_eh'
END
; list_bad_files


;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ parseCatalog @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION parseCatalog, catalogName, FILE_ONLY = file_only, statePtr
	routineName	= '----- parseCatalog -----'
	returnMsg	= 'Problem with catalog file... returning...'

	;=======================================================================
	; basic error catch mechanism
	;=======================================================================
	CATCH, errorStatus
	IF errorStatus NE 0 THEN BEGIN
		eIndex	= 'Error Index:' + 				$
				STRTRIM( STRING(errorStatus), 2 )
		eMsg	= 'Error Message: ' + !ERR_STRING
		res	= DIALOG_MESSAGE(									$
				[ routineName,									$
				  eIndex,									$
				  eMsg,										$
				'',										$
				  returnMsg,									$
				'',										$
				'Suggestion 1: If attempting to read or write a file, then check permissions.',	$
				'',										$
				'Suggestion 2: Make sure file is valid catalog file.',				$
				''										$
				 ],										$
				  /ERROR )
		RETURN, STRARR(1,6)
	ENDIF
	
	productInfo		= STRARR( 100, 6 )
	curLen			= 100L
	inputFromCatalog	= STRARR( curLen )
	curLine			= 0L
	str			= ''
	finished		= 0
	early_abort		= 0
	
	IF KEYWORD_SET( file_only ) THEN BEGIN
		is_file_only	= 1
	ENDIF ELSE BEGIN
		is_file_only	= 0
		OPENR, catlun, catalogName, /GET_LUN
		IF EOF( catlun ) THEN BEGIN
			CLOSE, catlun
			RETURN, productInfo[ 0, * ]
		ENDIF
	ENDELSE
	
	WHILE NOT finished DO BEGIN
		bad_current_file	= 0
		IF file_only THEN str = catalogName ELSE READF, catlun, str
		
		productName		= 'COMMENTLINE'
		IF STRMID(STRTRIM(str,2),0,1) EQ ';' OR STRMID(STRTRIM(str,2),0,1) EQ '' THEN comment_line = 1 ELSE comment_line = 0
		res		= (-1)
		grid_list	= ''
		IF NOT comment_line THEN res = EOS_GD_INQGRID( str, grid_list )
		
		IF (res LT 0 OR grid_list EQ '') AND NOT comment_line THEN BEGIN
			IF is_file_only THEN BEGIN
				msg	= [											$
					'The following file appears to NOT',							$
					'be a valid MISR HDF-EOS GRID file:',							$
					'',											$
					str,											$
					'',											$
					'Perhaps an error has been made in a file selection...',				$
					'Should further processing be aborted?' ]
			ENDIF ELSE BEGIN
				msg	= [											$
					'The selected MISR catalog file below:',						$
					'',											$
					catalogName,										$
					'',											$
					'contains the following entry that appears to NOT',					$
					'be a valid MISR HDF-EOS GRID file:',							$
					'',											$
					str,											$
					'',											$
					'Perhaps an error has been made in the catalog selection,',				$
					'or there could be an isolated problem with the data file',				$
					'entry itself (bad file name, incorrect directory location, etc.)...',			$
					'Should further processing be aborted?' ]
			ENDELSE
			res	= DIALOG_MESSAGE( msg, /QUESTION )
			IF STRTRIM(STRUPCASE(res),2) EQ 'YES' THEN finished = 1
			bad_current_file	= 1
			productName		= 'NOTHDFEOSFILE'
		ENDIF
		
		IF NOT bad_current_file AND NOT comment_line THEN BEGIN
;print,'str = ',str
			sepStr		= STR_SEP( str, GetDirectoryDivider() )
;print,'sepStr = ', sepStr
			granule_id	= get_local_granule_id( str )
;print,'granule_id=',granule_id
			fName		= sepStr[ N_ELEMENTS( sepStr ) - 1 ]
;print,'fName=',fName
			qualifiedDir	= STRMID( str, 0, STRLEN( str ) - STRLEN( fName ) )
;print,'qualifiedDir=',qualifiedDir
;print,'	granule_id	= ', granule_id	
			;===============================================================
			; ADDED DECEMBER 2000: routine get_local_granule_id (called above)
			; which returns "LOCALGRANULEID" from coremetadata.  In theory, files
			; can now be named anything.  (ckt, dec2000)
			;===============================================================
			IF granule_id NE '' THEN fSep = STR_SEP( granule_id, '_' ) ELSE fSep = STR_SEP( fName, '_' )
		
;;;ckt,dec2000		fSep		= STR_SEP( fName, '_' )

			IF N_ELEMENTS(fSep) GE 3 THEN productName = fSep[ 2 ] ELSE productName = 'UNKNOWN'

			found_file	= FINDFILE(str)
			IF found_file[0] EQ '' THEN productName = 'FILENOTFOUND'
;print,'productName = ',productName
		ENDIF

		CASE productName OF
		
			;---------------------------------------------------------------
			; MISR_AM1_GP_GMP_Pmmm_Onnnnnn_vv.hdf
			;---------------------------------------------------------------
			'GP': BEGIN	; GEOMETRIC PARAMETER FILES
				p_str		= STRMID(fSep[4],1,3)
				o_str		= STRMID(fSep[5],1,6)
;print,'############# o_str, Nref, PNref = ',o_str, (*statePtr).Nref, (*statePtr).PNref
;print,'orbit2path(o_str, Nref, PNref) = ',orbit2path(o_str, (*statePtr).Nref, (*statePtr).PNref )
				sync_path	= '00' + STRTRIM(STRING(orbit2path(o_str, (*statePtr).Nref, (*statePtr).PNref )),2)
				sync_path	= STRMID( sync_path, STRLEN(sync_path)-3, 3 )
;print,'sync_path = ',sync_path
;print,'p_str = ',p_str
				CASE 1 OF
					N_ELEMENTS(fSep) LT 6:		bad_read	= 1
					NOT is_valid_number(p_str):  	bad_read	= 1
					NOT is_valid_number(o_str):  	bad_read	= 1
					STRLEN(STRTRIM(p_str,2)) NE 3:  bad_read	= 1
					STRLEN(STRTRIM(o_str,2)) NE 6:  bad_read	= 1
					sync_path NE p_str:		bad_read	= 2
					ELSE:				bad_read	= 0
				ENDCASE
				
				IF bad_read LE 0 THEN BEGIN
					productInfo[curLine,0]	= productName+'_'+fSep[3]	;product
					productInfo[curLine,1]	= STRMID(fSep[4],1,3)		;path
					productInfo[curLine,2]	= STRMID(fSep[5],1,6)		;orbit
					productInfo[curLine,3]	= ''				;camera
				ENDIF
				END
		
			'GRP': BEGIN	; GEORECTIFIED RADIANCE PRODUCT FILES (FROM PGE-2)
			;---------------------------------------------------------------
			; MISR_AM1_GRP_TERRAIN_GM_Pmmm_Onnnnnn_cc_vv.hdf
			; MISR_AM1_GRP_ELLIPSOID_GM_Pmmm_Onnnnnn_cc_vv.hdf
			; MISR_AM1_GRP_RCCM_GM_Pmmm_Onnnnnn_cc_vv.hdf
			;
			; mmm		= path number
			; nnnnnn	= absolute orbit number
			; cc		= camera identifier
			; vv		= version number
			;---------------------------------------------------------------
				p_str	= STRMID(fSep[5],1,3)
				o_str	= STRMID(fSep[6],1,6)
;print,'############# o_str, Nref, PNref = ',o_str, (*statePtr).Nref, (*statePtr).PNref
;print,'orbit2path(o_str, Nref, PNref) = ',orbit2path(o_str, (*statePtr).Nref, (*statePtr).PNref )
				sync_path	= '00' + STRTRIM(STRING(orbit2path(o_str, (*statePtr).Nref, (*statePtr).PNref )),2)
				sync_path	= STRMID( sync_path, STRLEN(sync_path)-3, 3 )
;print,'sync_path = ',sync_path
;print,'p_str = ',p_str
				CASE 1 OF
					N_ELEMENTS(fSep) LT 7:		bad_read	= 1
					NOT is_valid_number(p_str):  	bad_read	= 1
					NOT is_valid_number(o_str):  	bad_read	= 1
					STRLEN(STRTRIM(p_str,2)) NE 3:  bad_read	= 1
					STRLEN(STRTRIM(o_str,2)) NE 6:  bad_read	= 1
					sync_path NE p_str:		bad_read	= 2
					ELSE:				bad_read	= 0
				ENDCASE
				
				IF bad_read LE 0 THEN BEGIN
					productInfo[curLine,0]	= productName+'_'+fSep[3]	;product
					productInfo[curLine,3]	= fSep[7]			;camera
					productInfo[curLine,1]	= p_str				;path
					productInfo[curLine,2]	= o_str				;orbit
				ENDIF
				
				END
			'PGRP': BEGIN	; GEORECTIFIED RADIANCE PRODUCT FILES (GROM PGE-1)
			;---------------------------------------------------------------
			; MISR_AM1_PGRP_TERRAIN_GM_Pmmm_Onnnnnn_cc_vv.hdf
			; MISR_AM1_PGRP_ELLIPSOID_GM_Pmmm_Onnnnnn_cc_vv.hdf
			; MISR_AM1_PGRP_RCCM_GM_Pmmm_Onnnnnn_cc_vv.hdf
			;
			; mmm		= path number
			; nnnnnn	= absolute orbit number
			; cc		= camera identifier
			; vv		= version number
			;---------------------------------------------------------------
				p_str	= STRMID(fSep[5],1,3)
				o_str	= STRMID(fSep[6],1,6)
;print,'############# o_str, Nref, PNref = ',o_str, (*statePtr).Nref, (*statePtr).PNref
;print,'orbit2path(o_str, Nref, PNref) = ',orbit2path(o_str, (*statePtr).Nref, (*statePtr).PNref )
				sync_path	= '00' + STRTRIM(STRING(orbit2path(o_str, (*statePtr).Nref, (*statePtr).PNref )),2)
				sync_path	= STRMID( sync_path, STRLEN(sync_path)-3, 3 )
;print,'sync_path = ',sync_path
;print,'p_str = ',p_str
				CASE 1 OF
					N_ELEMENTS(fSep) LT 7:		bad_read	= 1
					NOT is_valid_number(p_str):  	bad_read	= 1
					NOT is_valid_number(o_str):  	bad_read	= 1
					STRLEN(STRTRIM(p_str,2)) NE 3:  bad_read	= 1
					STRLEN(STRTRIM(o_str,2)) NE 6:  bad_read	= 1
					sync_path NE p_str:		bad_read	= 2
					ELSE:				bad_read	= 0
				ENDCASE
				
				IF bad_read LE 0 THEN BEGIN
					productInfo[curLine,0]	= productName+'_'+fSep[3]	;product
					productInfo[curLine,3]	= fSep[7]			;camera
					productInfo[curLine,1]	= p_str				;path
					productInfo[curLine,2]	= o_str				;orbit
				ENDIF
				
				END
			
			'AGP': BEGIN	; ANCILLARY GEOGRAPHIC PRODUCT FILES
			;---------------------------------------------------------------
			; MISR_AM1_AGP_Pmmm_vv.hdf
			;
			; mmm		= path number
			; vv		= version number
			;---------------------------------------------------------------
				p_str	= STRMID(fSep[3],1,3)
				CASE 1 OF
					N_ELEMENTS(fSep) LT 4:		bad_read	= 1
					NOT is_valid_number(p_str):  	bad_read	= 1
					STRLEN(STRTRIM(p_str,2)) NE 3:  bad_read	= 1
					ELSE:				bad_read	= 0
				ENDCASE
				
				IF bad_read LE 0 THEN BEGIN
					productInfo[curLine,0]	= productName			;product
					productInfo[curLine,1]	= p_str				;path
					productInfo[curLine,2]	= ''				;orbit
					productInfo[curLine,3]	= ''				;camera
				ENDIF
				END
			
			'AS': BEGIN	; AEROSOL/SURFACE
			;---------------------------------------------------------------
			; MISR_AM1_AS_?????_Pmmm_Onnnnnn_vv.hdf
			; MISR_AM1_AS_LANDSFC1_?????_Pmmm_Onnnnnn_vv.hdf
			; MISR_AM1_AS_LANDSFC2_?????_Pmmm_Onnnnnn_vv.hdf
			; MISR_AM1_AS_LANDSFC3_?????_Pmmm_Onnnnnn_vv.hdf
			; MISR_AM1_AS_OCEANSFC_?????_Pmmm_Onnnnnn_vv.hdf
			;
			; mmm		= path number
			; nnnnnn	= absolute orbit number
			; vv		= version number
			;---------------------------------------------------------------
				p_str	= STRMID(fSep[4],1,3)
				o_str	= STRMID(fSep[5],1,6)
				sync_path	= '00' + STRTRIM(STRING(orbit2path(o_str, (*statePtr).Nref, (*statePtr).PNref )),2)
				sync_path	= STRMID( sync_path, STRLEN(sync_path)-3, 3 )
				CASE 1 OF
					N_ELEMENTS(fSep) LT 6:		bad_read	= 1
					NOT is_valid_number(p_str):  	bad_read	= 1
					NOT is_valid_number(o_str):  	bad_read	= 1
					STRLEN(STRTRIM(p_str,2)) NE 3:  bad_read	= 1
					STRLEN(STRTRIM(o_str,2)) NE 6:  bad_read	= 1
					sync_path NE p_str:		bad_read	= 2
					ELSE:				bad_read	= 0
				ENDCASE
				
				IF bad_read LE 0 THEN BEGIN
					productInfo[curLine,0]	= productName			;product
					productInfo[curLine,1]	= p_str				;path
					productInfo[curLine,2]	= o_str				;orbit
					productInfo[curLine,3]	= ''				;camera
				ENDIF
				END
				
			'TC': BEGIN	; LEVEL 2 TOA/CLOUD PRODUCT FILES
			;---------------------------------------------------------------
			; MISR_AM1_TC_?????_Pmmm_Onnnnnn_vv.hdf
			;
			; mmm		= path number
			; nnnnnn	= absolute orbit number
			; vv		= version number
			;---------------------------------------------------------------
				p_str	= STRMID(fSep[4],1,3)
				o_str	= STRMID(fSep[5],1,6)
				sync_path	= '00' + STRTRIM(STRING(orbit2path(o_str, (*statePtr).Nref, (*statePtr).PNref )),2)
				sync_path	= STRMID( sync_path, STRLEN(sync_path)-3, 3 )
				CASE 1 OF
					N_ELEMENTS(fSep) LT 6:		bad_read	= 1
					NOT is_valid_number(p_str):  	bad_read	= 1
					NOT is_valid_number(o_str):  	bad_read	= 1
					STRLEN(STRTRIM(p_str,2)) NE 3:  bad_read	= 1
					STRLEN(STRTRIM(o_str,2)) NE 6:  bad_read	= 1
					sync_path NE p_str:		bad_read	= 2
					ELSE:				bad_read	= 0
				ENDCASE
				
				IF bad_read LE 0 THEN BEGIN
					productInfo[curLine,0]	= productName			;product
					productInfo[curLine,1]	= p_str				;path
					productInfo[curLine,2]	= o_str				;orbit
					productInfo[curLine,3]	= ''				;camera
				ENDIF
				END
			'FILENOTFOUND'	: bad_read = 3
			'NOTHDFEOSFILE'	: bad_read = 4
			'COMMENTLINE'	: bad_read = (-1)
			ELSE		: bad_read = 1
		ENDCASE
		
		CASE 1 OF
			bad_read EQ 0: BEGIN
				productInfo[curLine,4]	= qualifiedDir	; qualified directory name with "/" at end of string
				productInfo[curLine,5]	= fName		; file name
				curLine = curLine + 1
				END
			bad_read GT 0: BEGIN
				reasons		= [ 'mangled file name', 'bad path/orbit sync', 'file not found', 'not HDF-EOS GRID file' ]
				reason_str	= reasons[bad_read-1]
				IF TOTAL(SIZE(reason_strarr)) LE 1 THEN							$
					reason_strarr = [str+' ('+reason_str+')']					$
				ELSE											$
					reason_strarr = [reason_strarr, str+' ('+reason_str+')']
				bad_read	= 0
				END
			ELSE:
		ENDCASE

		IF curLine GE curLen THEN BEGIN
			tmpCat				= productInfo
			len				= N_ELEMENTS( tmpCat[ *, 0 ] )
			productInfo			= STRARR( len + 100, 6 )
			productInfo[ 0:len - 1, * ]	= tmpCat
			curLen				= curLen + 100L
		ENDIF
		
		IF file_only THEN BEGIN
			finished	= 1
		ENDIF ELSE BEGIN
			IF EOF( catlun ) THEN finished = 1
;print,'after READF #2, str = ', str
		ENDELSE
;print,'==============================='
;print,'productInfo[curLine,4]=',productInfo[curLine-1,4]
;print,'productInfo[curLine,5]=',productInfo[curLine-1,5]
;print,'==============================='
	ENDWHILE

	IF NOT file_only THEN CLOSE, catlun
;print,'TOTAL(SIZE(reason_strarr))=',TOTAL(SIZE(reason_strarr))

	IF TOTAL(SIZE(reason_strarr)) GT 1 THEN	BEGIN
		IF curLine LE 0 THEN BEGIN
			list_bad_files, reason_strarr, (*statePtr).parent, EXTRA_INFO = '(NOTE: There are no files in active data list!)'
			RETURN, productInfo[ 0, * ]
		ENDIF ELSE BEGIN
			list_bad_files, reason_strarr, (*statePtr).parent
			RETURN, productInfo[ 0:curLine - 1, * ]
		ENDELSE
;;;ckt,sep2000		str2exec	= 'res=dialog_message(["The files below were excluded from misr_view","for the reasons in parentheses",""'
;;;ckt,sep2000		FOR i = 0, N_ELEMENTS(reason_strarr)-1 DO str2exec = str2exec + ',reason_strarr[' + strtrim(string(i),2) + ']'
;;;ckt,sep2000		str2exec	= str2exec + '],/INFORMATION)'
;;;ckt,sep2000		success		= EXECUTE(str2exec)
	ENDIF
	
	IF curLine LE 0 THEN BEGIN
;;;ckt,sep2000		res	= DIALOG_MESSAGE( ['No files in active data list!'],/INFORMATION)
		RETURN, productInfo[ 0, * ]
	ENDIF
	
	RETURN, productInfo[ 0:curLine - 1, * ]
END
; parseCatalog
