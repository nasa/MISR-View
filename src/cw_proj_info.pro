@GetCorrectFont.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ check_projection_input_kill @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO check_projection_input_kill, top
	WIDGET_CONTROL, top, GET_UVALUE = ptr
	(*ptr).continue = 0
END
; check_projection_input_kill

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ display_projection_input_eh @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO display_projection_input_eh, event
	WIDGET_CONTROL, event.top, GET_UVALUE = ptr
	WIDGET_CONTROL, event.id, GET_UVALUE = widget_name
	CASE STRUPCASE(STRTRIM(widget_name,2)) OF
		'CONTINUE':  val = 1
		'RESPECIFY': val = 0
		ELSE:
	ENDCASE
	WIDGET_CONTROL, event.top, /DESTROY
	(*ptr).continue = val
END
; display_projection_input_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ display_projection_input @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION display_projection_input, strarr2check, group_leader, font_type
	n_entries	= N_ELEMENTS( strarr2check[0,*] )
	b		= WIDGET_BASE( TITLE = 'Reprojection Parameter Selection Review',	$
					/COLUMN, GROUP_LEADER = group_leader, /MODAL,		$
					EVENT_PRO = 'display_projection_input_eh', KILL_NOTIFY = 'check_projection_input_kill' )
	l1		= WIDGET_LABEL( b, VALUE = 'Please review the selected projection parameters below:', /ALIGN_CENTER, FONT = font_type )
	FOR i = 0, n_entries-1 DO lbl = WIDGET_LABEL( b, VALUE = strarr2check[0,i] + strarr2check[1,i], /ALIGN_LEFT, FONT = font_type )
	b2		= WIDGET_BASE( b, /ROW, /FRAME, /BASE_ALIGN_CENTER, /ALIGN_CENTER )
	con		= WIDGET_BUTTON( b2, VALUE = 'Continue', UVALUE = 'continue', FONT = font_type )
	redo		= WIDGET_BUTTON( b2, VALUE = 'Respecify', UVALUE = 'respecify', FONT = font_type )
	WIDGET_CONTROL, b, /REALIZE
	ptr		= PTR_NEW( { strarr2check:strarr2check, continue:1 }, /NO_COPY )
	WIDGET_CONTROL, b, SET_UVALUE = ptr
	XMANAGER, 'display_projection_input', b,  EVENT_HANDLER = 'display_projection_input_eh'
	val2return	= (*ptr).continue
	PTR_FREE, ptr
	RETURN, val2return
END
; display_projection_input

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ restore_parameter_file @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO restore_parameter_file, filename, ptr

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when 
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== restore_parameter_file =========='
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


	OPENR, lun, filename, /GET_LUN
	s	= ''
	emsg	= ['PROBLEMS ENCOUNTERED WITH FILE '+filename]
	proj_fnd= 0
	first_parallel_encountered	= 0
	second_parallel_encountered	= 0
	res_set				= 0
	wdims_set			= 0
	scale_set			= 0
	limit_set			= 0
	error				= 0
	WHILE NOT EOF(lun) AND NOT error DO BEGIN
		READF, lun, s
		sarr	= STR_SEP(s,'=')
		IF N_ELEMENTS(sarr) LT 2 THEN BEGIN
			error	= 1
			emsg	= [emsg,'problem with line: '+s]
		ENDIF ELSE BEGIN
			IF NOT proj_fnd THEN BEGIN
				IF STRUPCASE(STRTRIM(sarr[0],2)) NE 'PROJECTION' THEN BEGIN
					sarr[0]	= 'ERROR'
				ENDIF ELSE BEGIN
					idx				= WHERE( STRUPCASE((*ptr).PROJECTIONS) EQ STRUPCASE(STRTRIM(sarr[1],2)), cnt )
					IF cnt GT 0 THEN BEGIN
						WIDGET_CONTROL, (*ptr).projection_base_stack[(*ptr).current_projection_idx], MAP = 0
						(*ptr).current_projection_idx	= idx[0]
						WIDGET_CONTROL, (*ptr).projection_base_stack[(*ptr).current_projection_idx], MAP = 1
						(*ptr).current_projection_idx	= idx[0]
						WIDGET_CONTROL, (*ptr).proj_droplist, SET_DROPLIST_SELECT = (*ptr).current_projection_idx
						proj_fnd			= 1
					ENDIF ELSE BEGIN
						error	= 1
						emsg	= [emsg,'problem with line: '+s+' (unknown projection: '+sarr[1]+')']
					ENDELSE
				ENDELSE
			ENDIF
			CASE STRUPCASE(STRTRIM(sarr[0],2)) OF
				'CENTER_LATITUDE': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF FLOAT((STRTRIM(sarr[1],2))) GE -90.0 AND FLOAT((STRTRIM(sarr[1],2))) LE 90.0 THEN	$
							WIDGET_CONTROL, (*ptr).clat_cwf, SET_VALUE = FLOAT((STRTRIM(sarr[1],2)))
					ENDIF ELSE BEGIN
						error	= 1
						emsg	= [emsg,'problem with line: '+s]
					ENDELSE
					END
				'CENTER_LONGITUDE': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF FLOAT((STRTRIM(sarr[1],2))) GE -180.0 AND FLOAT((STRTRIM(sarr[1],2))) LE 180.0 THEN	$
							WIDGET_CONTROL, (*ptr).clon_cwf, SET_VALUE = FLOAT((STRTRIM(sarr[1],2)))
					ENDIF ELSE BEGIN
						error	= 1
						emsg	= [emsg,'problem with line: '+s]
					ENDELSE
					END
				'CENTER_ROTATION': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF FLOAT((STRTRIM(sarr[1],2))) GE -180.0 AND FLOAT((STRTRIM(sarr[1],2))) LE 180.0 THEN	$
							WIDGET_CONTROL, (*ptr).crot_cwf, SET_VALUE = FLOAT((STRTRIM(sarr[1],2)))
					ENDIF ELSE BEGIN
						error	= 1
						emsg	= [emsg,'problem with line: '+s]
					ENDELSE
					END
				'MINIMUM_LONGITUDE': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF FLOAT((STRTRIM(sarr[1],2))) GE -180.0 AND FLOAT((STRTRIM(sarr[1],2))) LE 180.0 THEN	$
							WIDGET_CONTROL, (*ptr).min_lon_lbl, SET_VALUE = FLOAT((STRTRIM(sarr[1],2)))
						(*ptr).current_scale_lim_idx	= 1
					ENDIF ELSE BEGIN
						error	= 1
						emsg	= [emsg,'problem with line: '+s]
					ENDELSE
					END
				'MINIMUM_LATITUDE': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN	
						IF FLOAT((STRTRIM(sarr[1],2))) GE -90.0 AND FLOAT((STRTRIM(sarr[1],2))) LE 90.0 THEN	$
							WIDGET_CONTROL, (*ptr).min_lat_lbl, SET_VALUE = FLOAT((STRTRIM(sarr[1],2)))
						(*ptr).current_scale_lim_idx	= 1
					ENDIF ELSE BEGIN
						error	= 1
						emsg	= [emsg,'problem with line: '+s]
					ENDELSE
					END
				'MAXIMUM_LONGITUDE': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF FLOAT((STRTRIM(sarr[1],2))) GE -180.0 AND FLOAT((STRTRIM(sarr[1],2))) LE 180.0 THEN	$
							WIDGET_CONTROL, (*ptr).max_lon_lbl, SET_VALUE = FLOAT((STRTRIM(sarr[1],2)))
						(*ptr).current_scale_lim_idx	= 1
					ENDIF ELSE BEGIN
						error	= 1
						emsg	= [emsg,'problem with line: '+s]
					ENDELSE
					END
				'MAXIMUM_LATITUDE': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						WIDGET_CONTROL, (*ptr).min_lat_lbl, GET_VALUE = min_val
						IF FLOAT((STRTRIM(sarr[1],2))) GE -90.0 AND FLOAT((STRTRIM(sarr[1],2))) LE 90.0 AND FLOAT((STRTRIM(sarr[1],2))) GT min_val THEN	$
							WIDGET_CONTROL, (*ptr).max_lat_lbl, SET_VALUE = FLOAT((STRTRIM(sarr[1],2)))
						(*ptr).current_scale_lim_idx	= 1
					ENDIF ELSE BEGIN
						error	= 1
						emsg	= [emsg,'problem with line: '+s+' (max_lon < min_lon)']
					ENDELSE
					END
				'SCALE': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF FLOAT((STRTRIM(sarr[1],2))) LT 1E6 OR FLOAT((STRTRIM(sarr[1],2))) GT 500E6 THEN 	$
							WIDGET_CONTROL, (*ptr).scale_cwf, SET_VALUE = FLOAT((STRTRIM(sarr[1],2)))
						(*ptr).current_scale_lim_idx	= 0
						scale_set	= 1
					ENDIF ELSE BEGIN
						error	= 1
						emsg	= [emsg,'problem with line: '+s]
					ENDELSE
					END
				'CENTRAL_AZIMUTH': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF (*ptr).param_array[ (*ptr).current_projection_idx, (*ptr).CENTRAL_AZIMUTH_IDX, (*ptr).RELEVANCE_IDX ] THEN BEGIN
							IF FLOAT((STRTRIM(sarr[1],2))) GE -180.0 AND FLOAT((STRTRIM(sarr[1],2))) LE 180.0 THEN	$
								WIDGET_CONTROL, WIDGET_INFO( WIDGET_INFO( (*ptr).projection_base_stack[(*ptr).current_projection_idx], /CHILD ), /CHILD ),	$
									SET_VALUE = FLOAT((STRTRIM(sarr[1],2)))
						ENDIF
					ENDIF
					END
				'SAT_P_DISTANCE': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF (*ptr).param_array[ (*ptr).current_projection_idx, (*ptr).SAT_P_IDX, (*ptr).RELEVANCE_IDX ] THEN BEGIN
							IF FLOAT((STRTRIM(sarr[1],2))) GE -1.0 AND FLOAT((STRTRIM(sarr[1],2))) LE 5000.0 THEN	$
								WIDGET_CONTROL, WIDGET_INFO( (*ptr).satp_base1[(*ptr).current_projection_idx], /CHILD ),	$
									SET_VALUE = FLOAT((STRTRIM(sarr[1],2)))
						ENDIF
					ENDIF
					END
				'SAT_P_OMEGA': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF (*ptr).param_array[ (*ptr).current_projection_idx, (*ptr).SAT_P_IDX, (*ptr).RELEVANCE_IDX ] THEN BEGIN
							IF FLOAT((STRTRIM(sarr[1],2))) GE 0.0 AND FLOAT((STRTRIM(sarr[1],2))) LE 90.0 THEN BEGIN
								p2		= WIDGET_INFO( WIDGET_INFO( (*ptr).satp_base1[(*ptr).current_projection_idx], /CHILD ), /SIBLING )
								WIDGET_CONTROL, p2, SET_VALUE = FLOAT((STRTRIM(sarr[1],2)))
							ENDIF
						ENDIF
					ENDIF
					END
				'SAT_P_GAMMA': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF (*ptr).param_array[ (*ptr).current_projection_idx, (*ptr).SAT_P_IDX, (*ptr).RELEVANCE_IDX ] THEN BEGIN
							IF FLOAT((STRTRIM(sarr[1],2))) GE -180.0 AND FLOAT((STRTRIM(sarr[1],2))) LE 180.0 THEN BEGIN
								p3		= WIDGET_INFO( p2, /SIBLING )
								WIDGET_CONTROL, p3, SET_VALUE = FLOAT((STRTRIM(sarr[1],2)))
							ENDIF
						ENDIF
					ENDIF
					END
				'STANDARD_PARALLEL_1': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF (*ptr).param_array[ (*ptr).current_projection_idx, (*ptr).STANDARD_PARALLELS_IDX, (*ptr).RELEVANCE_IDX ] THEN BEGIN
							IF FLOAT((STRTRIM(sarr[1],2))) GE -90.0 AND FLOAT((STRTRIM(sarr[1],2))) LE 90.0 THEN BEGIN
								widget_control, widget_info(widget_info((*ptr).parallel_base1[(*ptr).current_projection_idx],/child),/sibling),SET_VALUE = FLOAT((STRTRIM(sarr[1],2)))
								first_parallel_encountered	= 1
							ENDIF
						ENDIF
					ENDIF
					END
				'STANDARD_PARALLEL_2': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF (*ptr).param_array[ (*ptr).current_projection_idx, (*ptr).STANDARD_PARALLELS_IDX, (*ptr).RELEVANCE_IDX ] THEN BEGIN
							IF FLOAT((STRTRIM(sarr[1],2))) GE -90.0 AND FLOAT((STRTRIM(sarr[1],2))) LE 90.0 THEN BEGIN
								WIDGET_CONTROL,  WIDGET_INFO(WIDGET_INFO(WIDGET_INFO((*ptr).parallel_base1[(*ptr).current_projection_idx],/child),/sibling),/sibling),SET_VALUE = FLOAT((STRTRIM(sarr[1],2)))
								second_parallel_encountered	= 1
							ENDIF
						ENDIF
					ENDIF
					END
				'RESOLUTION': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF FLOAT((STRTRIM(sarr[1],2))) GT 0.0 AND FLOAT((STRTRIM(sarr[1],2))) LT 10000.0 THEN BEGIN
							WIDGET_CONTROL, (*ptr).res_cwf, SET_VALUE = FLOAT((STRTRIM(sarr[1],2)))
							res_set	= 1
						ENDIF
					ENDIF
					END
				'WINDOW_WIDTH': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF FLOAT((STRTRIM(sarr[1],2))) GE 50 AND FLOAT((STRTRIM(sarr[1],2))) LE 8000 THEN BEGIN
							WIDGET_CONTROL, (*ptr).height_lbl, SET_VALUE = FIX((STRTRIM(sarr[1],2)))
							IF NOT res_set THEN wdims_set	= 1
						ENDIF
					ENDIF
					END
				'WINDOW_HEIGHT': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF FLOAT((STRTRIM(sarr[1],2))) GE 50 AND FLOAT((STRTRIM(sarr[1],2))) LE 8000 THEN BEGIN
							WIDGET_CONTROL, (*ptr).width_lbl, SET_VALUE = FIX((STRTRIM(sarr[1],2)))
							IF NOT res_set THEN wdims_set	= 1
						ENDIF
					ENDIF
					END
				'ISOTROPIC': BEGIN
					IF STRUPCASE(STRTRIM(sarr[1],2)) EQ 'YES' THEN BEGIN
						WIDGET_CONTROL, (*ptr).isotropic_btn, SET_BUTTON = 1
						(*ptr).isotropic	= 1
					ENDIF ELSE BEGIN
						WIDGET_CONTROL, (*ptr).isotropic_btn, SET_BUTTON = 0
						(*ptr).isotropic	= 0
					ENDELSE
					END
				'SEAM_FILLING': BEGIN
					IF STRUPCASE(STRTRIM(sarr[1],2)) EQ 'YES' THEN BEGIN
						WIDGET_CONTROL, (*ptr).fill_seam_btn, SET_BUTTON = 1
						WIDGET_CONTROL, (*ptr).fill_seam_options_btn, /SENSITIVE
						(*ptr).do_seam_filling	= 1
					ENDIF ELSE BEGIN
						WIDGET_CONTROL, (*ptr).fill_seam_btn, SET_BUTTON = 0
						(*ptr).do_seam_filling	= 0
						WIDGET_CONTROL, (*ptr).fill_seam_options_btn, SENSITIVE=0
					ENDELSE
					END
				'BOX_WIDTH': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF FIX((STRTRIM(sarr[1],2))) GE 3 AND FIX((STRTRIM(sarr[1],2))) LE 7 AND FIX((STRTRIM(sarr[1],2))) MOD 2 NE 0 THEN BEGIN
							(*ptr).fill_seam_info[0]	= FLOAT((STRTRIM(sarr[1],2)))
						ENDIF
					ENDIF
					END
				'BOX_PCT': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF FLOAT((STRTRIM(sarr[1],2))) GE 0.0 AND FLOAT((STRTRIM(sarr[1],2))) LE 100.0 THEN BEGIN
							(*ptr).fill_seam_info[1]	= FLOAT((STRTRIM(sarr[1],2)))/100.0
						ENDIF
					ENDIF
					END
				'RESOLVE_OVERLAP_IDX': BEGIN
					IF is_valid_number(STRTRIM(sarr[1],2)) THEN BEGIN
						IF FIX((STRTRIM(sarr[1],2))) GE 0 AND FIX((STRTRIM(sarr[1],2))) LE 4 THEN BEGIN
							WIDGET_CONTROL, (*ptr).overlap_wd, SET_DROPLIST_SELECT = FIX((STRTRIM(sarr[1],2)))
							(*ptr).overlap_idx	= FIX((STRTRIM(sarr[1],2)))
						ENDIF
					ENDIF
					END
				'CREATE_OVERLAY_MAP': BEGIN
					IF STRUPCASE(STRTRIM(sarr[1],2)) EQ 'YES' THEN BEGIN
						WIDGET_CONTROL, (*ptr).overlay_btn, SET_BUTTON = 1
						(*ptr).overlay	= 1
					ENDIF ELSE BEGIN
						WIDGET_CONTROL, (*ptr).overlay_btn, SET_BUTTON = 0
						(*ptr).overlay	= 0
					ENDELSE
					END
				'PROJECTION':
				ELSE: BEGIN
					error	= 1
					emsg	= [emsg,'problem with line: '+s+' (projection name not first parameter in file)']
					END
			ENDCASE
		ENDELSE
	ENDWHILE

	FREE_LUN, lun
	
	IF error THEN BEGIN
		note	= [ 'Since an error in the projection parameter file was encountered',	$
			    'it is best to either manually fix the error in the',	$
			    'file or generate another projection parameter file using the',		$
			    'controls on the projection interface and saving',		$
			    'the control settings with the Save Parameters To File button.' ]
		emsg	= [emsg,note]
		res	= DIALOG_MESSAGE(emsg, /ERROR)
		RETURN
	ENDIF
	
	WIDGET_CONTROL, (*ptr).scale_lim_barr[(*ptr).current_scale_lim_idx EQ 0], MAP = 0
	WIDGET_CONTROL, (*ptr).scale_lim_barr[(*ptr).current_scale_lim_idx], MAP = 1
	IF res_set THEN BEGIN
		WIDGET_CONTROL, (*ptr).res_windim_barr[0], MAP = 0
		WIDGET_CONTROL, (*ptr).res_windim_barr[1], MAP = 1
		WIDGET_CONTROL, (*ptr).res_btn, SET_BUTTON = 1
		(*ptr).current_res_windim_idx	= 1
	ENDIF ELSE BEGIN
		WIDGET_CONTROL, (*ptr).res_windim_barr[1], MAP = 0
		WIDGET_CONTROL, (*ptr).res_windim_barr[0], MAP = 1
		WIDGET_CONTROL, (*ptr).dim_btn, SET_BUTTON = 1
		(*ptr).current_res_windim_idx	= 0
	ENDELSE
	
	IF scale_set THEN BEGIN
		WIDGET_CONTROL, (*ptr).scale_lim_barr[0], MAP = 1
		WIDGET_CONTROL, (*ptr).scale_lim_barr[1], MAP = 0
		WIDGET_CONTROL, (*ptr).scale_btn, SET_BUTTON = 1
		(*ptr).current_scale_lim_idx	= 0
	ENDIF ELSE BEGIN
		WIDGET_CONTROL, (*ptr).scale_lim_barr[0], MAP = 0
		WIDGET_CONTROL, (*ptr).scale_lim_barr[1], MAP = 1
		WIDGET_CONTROL, (*ptr).limit_btn, SET_BUTTON = 1
		(*ptr).current_scale_lim_idx	= 1
	ENDELSE
	
	IF first_parallel_encountered THEN BEGIN
		WIDGET_CONTROL, WIDGET_INFO((*ptr).parallel_base1[(*ptr).current_projection_idx],/CHILD), SET_DROPLIST_SELECT = 1
		WIDGET_CONTROL,WIDGET_INFO( WIDGET_INFO((*ptr).parallel_base1[(*ptr).current_projection_idx],/CHILD),/SIBLING), SENSITIVE = 1
		WIDGET_CONTROL,WIDGET_INFO( WIDGET_INFO( WIDGET_INFO((*ptr).parallel_base1[(*ptr).current_projection_idx],/CHILD),/SIBLING),/SIBLING), SENSITIVE = 0
	ENDIF
	
	IF second_parallel_encountered AND first_parallel_encountered THEN BEGIN
		WIDGET_CONTROL, WIDGET_INFO((*ptr).parallel_base1[(*ptr).current_projection_idx],/CHILD), SET_DROPLIST_SELECT = 2
		WIDGET_CONTROL,WIDGET_INFO( WIDGET_INFO((*ptr).parallel_base1[(*ptr).current_projection_idx],/CHILD),/SIBLING), SENSITIVE = 1
		WIDGET_CONTROL,WIDGET_INFO( WIDGET_INFO( WIDGET_INFO((*ptr).parallel_base1[(*ptr).current_projection_idx],/CHILD),/SIBLING),/SIBLING), SENSITIVE = 1
	ENDIF
	
END
; restore_parameter_file

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ save_parameter_file @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO save_parameter_file, filename, ptr

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when 
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== save_parameter_file =========='
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

	
	OPENW, lun, filename, /GET_LUN
	proj	= STRUPCASE((*ptr).PROJECTIONS[(*ptr).current_projection_idx])
	IF proj EQ 'TRANSVERSE MERCATOR' THEN proj = 'TRANSVERSE_MERCATOR'
	IF proj EQ 'GOODES HOMOLOSINE' THEN proj = 'GOODESHOMOLOSINE'
	PRINTF, lun, 'projection=',proj
	
	WIDGET_CONTROL, (*ptr).clat_cwf, GET_VALUE = cen_lat
	WIDGET_CONTROL, (*ptr).clon_cwf, GET_VALUE = cen_lon
	WIDGET_CONTROL, (*ptr).crot_cwf, GET_VALUE = cen_rot
	
	PRINTF, lun, 'center_latitude='+STRTRIM(cen_lat,2)
	PRINTF, lun, 'center_longitude='+STRTRIM(cen_lon,2)
	PRINTF, lun, 'center_rotation='+STRTRIM(cen_rot,2)
	
	IF (*ptr).current_scale_lim_idx THEN BEGIN
		WIDGET_CONTROL, (*ptr).min_lon_lbl, GET_VALUE = min_lon
		WIDGET_CONTROL, (*ptr).max_lon_lbl, GET_VALUE = max_lon
		WIDGET_CONTROL, (*ptr).min_lat_lbl, GET_VALUE = min_lat
		WIDGET_CONTROL, (*ptr).max_lat_lbl, GET_VALUE = max_lat
				
		PRINTF, lun, 'minimum_longitude='+STRTRIM(min_lon,2)
		PRINTF, lun, 'minimum_latitude='+STRTRIM(min_lat,2)
		PRINTF, lun, 'maximum_longitude='+STRTRIM(max_lon,2)
		PRINTF, lun, 'maximum_latitude='+STRTRIM(max_lat,2)
	ENDIF ELSE BEGIN
		WIDGET_CONTROL, (*ptr).scale_cwf, GET_VALUE = out_scale
		PRINTF, lun, 'scale='+STRTRIM(out_scale,2)
	ENDELSE
	
	IF (*ptr).param_array[ (*ptr).current_projection_idx, (*ptr).CENTRAL_AZIMUTH_IDX, (*ptr).RELEVANCE_IDX ] THEN BEGIN
		WIDGET_CONTROL, WIDGET_INFO( WIDGET_INFO( (*ptr).projection_base_stack[(*ptr).current_projection_idx], /CHILD ), /CHILD ), GET_VALUE = caz
		PRINTF, lun, 'central_azimuth='+STRTRIM(caz,2)
	ENDIF
				
	IF (*ptr).param_array[ (*ptr).current_projection_idx, (*ptr).SAT_P_IDX, (*ptr).RELEVANCE_IDX ] THEN BEGIN
		p1		= WIDGET_INFO( (*ptr).satp_base1[(*ptr).current_projection_idx], /CHILD )
		p2		= WIDGET_INFO( p1, /SIBLING )
		p3		= WIDGET_INFO( p2, /SIBLING )
		WIDGET_CONTROL, p1, GET_VALUE = f
		WIDGET_CONTROL, p2, GET_VALUE = o
		WIDGET_CONTROL, p3, GET_VALUE = g
		
		PRINTF, lun, 'sat_p_distance='+STRTRIM(f,2)
		PRINTF, lun, 'sat_p_omega='+STRTRIM(o,2)
		PRINTF, lun, 'sat_p_gamma='+STRTRIM(g,2)
	ENDIF
		
	IF (*ptr).param_array[ (*ptr).current_projection_idx, (*ptr).STANDARD_PARALLELS_IDX, (*ptr).RELEVANCE_IDX ] THEN BEGIN
		CASE (*ptr).param_array[(*ptr).current_projection_idx,(*ptr).STANDARD_PARALLELS_IDX,(*ptr).N_ENTRIES_USED_IDX] OF
			0:
			1: BEGIN
				widget_control, widget_info(widget_info((*ptr).parallel_base1[(*ptr).current_projection_idx],/child),/sibling),GET_VALUE = p1
				PRINTF, lun, 'standard_parallel_1='+STRTRIM(p1,2)
				END
			2: BEGIN
				WIDGET_CONTROL, WIDGET_INFO(WIDGET_INFO((*ptr).parallel_base1[(*ptr).current_projection_idx],/child),/sibling),GET_VALUE = p1
				WIDGET_CONTROL,  WIDGET_INFO(WIDGET_INFO(WIDGET_INFO((*ptr).parallel_base1[(*ptr).current_projection_idx],/child),/sibling),/sibling),GET_VALUE = p2
				PRINTF, lun, 'standard_parallel_1='+STRTRIM(p1,2)
				PRINTF, lun, 'standard_parallel_2='+STRTRIM(p2,2)
				END
			ELSE:
		ENDCASE
	ENDIF
			
	final_height	= (-1)
	final_width	= final_height
	final_res	= (*ptr).default_res
	IF (*ptr).current_res_windim_idx THEN BEGIN
		WIDGET_CONTROL, (*ptr).res_cwf, GET_VALUE = res
		final_res	= res
		PRINTF, lun, 'resolution='+STRTRIM(res,2)
	ENDIF ELSE BEGIN
		WIDGET_CONTROL, (*ptr).height_lbl, GET_VALUE = hhh
		WIDGET_CONTROL, (*ptr).width_lbl, GET_VALUE = www
;;;ckt,may2001		strarr2check	= [ [strarr2check], ['Width Of Projection Window: ', STRTRIM(www,2)] ]
;;;ckt,may2001		strarr2check	= [ [strarr2check], ['Height Of Projection Window: ', STRTRIM(hhh,2)] ]
		final_width	= www
		final_height	= hhh
		PRINTF, lun, 'window_width='+STRTRIM(www,2)
		PRINTF, lun, 'window_height='+STRTRIM(hhh,2)
	ENDELSE
			
	PRINTF, lun, 'isotropic='+STRTRIM((['No','Yes'])[(*ptr).isotropic],2)
	PRINTF, lun, 'seam_filling='+STRTRIM((['No','Yes'])[(*ptr).do_seam_filling],2)
	
	IF (*ptr).do_seam_filling THEN BEGIN
		PRINTF, lun, 'box_width='+STRTRIM(FIX((*ptr).fill_seam_info[0]),2)
		PRINTF, lun, 'box_pct='+STRTRIM((*ptr).fill_seam_info[1]*100.0,2)
	ENDIF
	
	PRINTF, lun, 'resolve_overlap_idx='+STRTRIM((*ptr).overlap_idx,2)
	PRINTF, lun, 'create_overlay_map='+STRTRIM((['No','Yes'])[(*ptr).overlay],2)

	IF final_height LT 0 THEN BEGIN
		IF (*ptr).current_scale_lim_idx THEN BEGIN
			use_polar	= (STRUPCASE((*ptr).PROJECTIONS[(*ptr).current_projection_idx]) EQ 'LAMBERT' OR	$
					  STRUPCASE((*ptr).PROJECTIONS[(*ptr).current_projection_idx]) EQ 'AZIMUTHAL') AND	$
					  (cen_lat EQ -90.0 OR cen_lat EQ 90.0)
			s	= return_resolution_info(final_res,[min_lat,min_lon,max_lat,max_lon], POLAR=use_polar)
			final_height	= s.nlines
			final_width	= s.nsamps
		ENDIF ELSE BEGIN
			; should never get to this point.
			final_height	= 500
			final_width	= 500
		ENDELSE
	ENDIF
	
	FREE_LUN, lun
END
; save_parameter_file

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ check_projection_input @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION check_projection_input, strarr2check, group_leader, font_type
	id_strarr	= REFORM( strarr2check[0,*] )
	idx		= WHERE( STRPOS(id_strarr, 'Center Latitude') GE 0, cnt )
	clat		= FLOAT(strarr2check[1,idx[0]])
	idx		= WHERE( STRPOS(id_strarr, 'Center Longitude') GE 0, cnt )
	clon		= FLOAT(strarr2check[1,idx[0]])
	idx		= WHERE( STRPOS(id_strarr, 'Center Rotation') GE 0, cnt )
	crot		= FLOAT(strarr2check[1,idx[0]])
	bad_str		= ['PROBLEMS WITH PROJECTION PARAMETER SPECIFICATIONS']
	suggest_str	= ['POTENTIAL PROBLEMS WITH PROJECTION PARAMETER SPECIFICATIONS']
	IF clat LT -90.0 OR clat GT 90.0 THEN bad_str = [bad_str,'center latitude ('+STRTRIM(clat,2)+') is out of range (-90.0->90.0)']
	IF clon LT -180.0 OR clon GT 180.0 THEN bad_str = [bad_str,'center longitude ('+STRTRIM(clon,2)+') is out of range (-180.0->180.0)']
	IF crot LT -180.0 OR crot GT 180.0 THEN bad_str = [bad_str,'center rotation ('+STRTRIM(crot,2)+') is out of range (-180.0->180.0)']
	
	idx		= WHERE( STRPOS(id_strarr, 'Scale Of Projection') GE 0, cnt )
	scale_exists	= 0
	IF cnt GT 0 THEN BEGIN
		scale		= FLOAT(strarr2check[1,idx[0]])
		IF scale LT 1E6 OR scale GT 500E6 THEN bad_str = [bad_str,'scale ('+STRTRIM(scale,2)+') is out of range (1E6->500E6)']
		scale_exists	= 1
	ENDIF ELSE BEGIN
		idx		= WHERE( STRPOS(id_strarr, 'Left Longitude') GE 0, cnt )
		left_lon	= FLOAT(strarr2check[1,idx[0]])
		idx		= WHERE( STRPOS(id_strarr, 'Right Longitude') GE 0, cnt )
		right_lon	= FLOAT(strarr2check[1,idx[0]])
		idx		= WHERE( STRPOS(id_strarr, 'Lower Latitude') GE 0, cnt )
		lower_lat	= FLOAT(strarr2check[1,idx[0]])
		idx		= WHERE( STRPOS(id_strarr, 'Upper Latitude') GE 0, cnt )
		upper_lat	= FLOAT(strarr2check[1,idx[0]])
		
		IF lower_lat LT -90.0 OR lower_lat GT 90.0 THEN bad_str = [bad_str,'lower latitude ('+STRTRIM(lower_lat,2)+') is out of range (-90.0->90.0)']
		IF upper_lat LT -90.0 OR upper_lat GT 90.0 THEN bad_str = [bad_str,'lower latitude ('+STRTRIM(upper_lat,2)+') is out of range (-90.0->90.0)']
		IF left_lon LT -180.0 OR left_lon GT 180.0 THEN bad_str = [bad_str,'left longitude ('+STRTRIM(left_lon,2)+') is out of range (-180.0->180.0)']
		IF right_lon LT -180.0 OR right_lon GT 180.0 THEN bad_str = [bad_str,'right longitude ('+STRTRIM(right_lon,2)+') is out of range (-180.0->180.0)']

		IF lower_lat GE upper_lat THEN bad_str = [bad_str,'lower latitude ('+STRTRIM(lower_lat,2)+') >= upper latitude ('+STRTRIM(upper_lat,2)+')']
		
		IF left_lon GT right_lon AND ( clon LT left_lon AND clon GT right_lon ) THEN suggest_str = [suggest_str,'center longitude ('+STRTRIM(clon,2)+	$
			') is outside longitudinal limits ('+STRTRIM(left_lon,2)+'->'+STRTRIM(right_lon,2)+')']
		IF right_lon GT left_lon AND ( clon LT left_lon OR clon GT right_lon ) THEN suggest_str = [suggest_str,'center longitude ('+STRTRIM(clon,2)+	$
			') is outside longitudinal limits ('+STRTRIM(left_lon,2)+'->'+STRTRIM(right_lon,2)+')']
		IF clat LT lower_lat OR clat GT upper_lat THEN suggest_str = [suggest_str,'center latitude ('+STRTRIM(clat,2)+	$
			') is outside latitudinal limits ('+STRTRIM(lower_lat,2)+'->'+STRTRIM(upper_lat,2)+')']
	ENDELSE
	
	caz		= 0.0
	idx		= WHERE( STRPOS(id_strarr, 'Central Azimuth') GE 0, cnt )
	IF cnt GT 0 THEN caz = FLOAT(strarr2check[1,idx[0]])
	IF cnt gt 0 AND (caz LT -180.0 OR caz GT 180.0) THEN bad_str = [bad_str,'central azimuth ('+STRTRIM(caz,2)+') is out of range (-180.0->180.0)']
	
	rad		= 6000000.0
	idx		= WHERE( STRPOS(id_strarr, 'Equatorial Radius') GE 0, cnt )
	IF cnt GT 0 THEN rad = FLOAT(strarr2check[1,idx[0]])
	IF cnt gt 0 AND ((ABS(rad-6000000.0) GT 1000000.0) OR rad LE 0.0) THEN bad_str = [bad_str,'bad value for equatorial radius ('+STRTRIM(rad,2)+')']
	
	ecc		= 1.0
	idx		= WHERE( STRPOS(id_strarr, 'Eccentricity Squared') GE 0, cnt )
	IF cnt GT 0 THEN ecc = FLOAT(strarr2check[1,idx[0]])
	IF cnt gt 0 AND ecc LE 0.0 OR ecc GT 1.0 THEN bad_str = [bad_str,'bad value for eccentricity squared ('+STRTRIM(ecc,2)+')']
	
	cmer		= 1.0
	idx		= WHERE( STRPOS(id_strarr, 'Scale On Central Meridian') GE 0, cnt )
	IF cnt GT 0 THEN cmer = FLOAT(strarr2check[1,idx[0]])
	IF cnt gt 0 AND cmer LE 0.0 OR ABS(cmer-1.0) GT 1.0 THEN bad_str = [bad_str,'bad value for scale on central meridian ('+STRTRIM(cmer,2)+')']
	dper		= 1.0
	idx		= WHERE( STRPOS(id_strarr, 'Distance From Perspective Point') GE 0, cnt )
	IF cnt GT 0 THEN dper = FLOAT(strarr2check[1,idx[0]])
	IF cnt gt 0 AND dper LT 1.0 OR dper GT 5000.0 THEN bad_str = [bad_str,'distance from perspective point to center of globe ('+STRTRIM(dper,2)+') is out of range (1.0->50.0)']
	
	omega		= 0.0
	idx		= WHERE( STRPOS(id_strarr, 'Omega') GE 0, cnt )
	IF cnt GT 0 THEN omega = FLOAT(strarr2check[1,idx[0]])
	IF cnt gt 0 AND omega LT 0.0 OR omega GT 90.0 THEN bad_str = [bad_str,'omega ('+STRTRIM(omega,2)+') is out of range (0.0->90.0)']
	
	gamma		= 0.0
	idx		= WHERE( STRPOS(id_strarr, 'Omega') GE 0, cnt )
	IF cnt GT 0 THEN gamma = FLOAT(strarr2check[1,idx[0]])
	IF cnt gt 0 AND (gamma LT -180.0 OR gamma GT 180.0) THEN bad_str = [bad_str,'gamma ('+STRTRIM(gamma,2)+') is out of range (-180.0->180.0)']
	
	sp1		= 0.0
	idx1		= WHERE( STRPOS(id_strarr, 'Standard Parallel 1') GE 0, cnt1 )
	IF cnt1 GT 0 THEN sp1 = FLOAT(strarr2check[1,idx1[0]])
	
	sp2		= 0.0
	idx2		= WHERE( STRPOS(id_strarr, 'Standard Parallel 2') GE 0, cnt2 )
	IF cnt2 GT 0 THEN sp2 = FLOAT(strarr2check[1,idx2[0]])
	
	CASE 1 OF
		cnt1 GT 0 AND cnt2 LE 0: BEGIN
			IF sp1 LT -90.0 OR sp1 GT 90.0 THEN bad_str = [bad_str,'standard parallel 1 ('+STRTRIM(sp1,2)+') is out of range (-90.0->90.0)']
			END
		cnt1 GT 0 AND cnt2 GT 0: BEGIN
			IF sp1 LT -90.0 OR sp1 GT 90.0 THEN bad_str = [bad_str,'standard parallel 1 ('+STRTRIM(sp1,2)+') is out of range (-90.0->90.0)']
			IF sp2 LT -90.0 OR sp2 GT 90.0 THEN bad_str = [bad_str,'standard parallel 2 ('+STRTRIM(sp2,2)+') is out of range (-90.0->90.0)']
			IF sp1 GE sp2 THEN bad_str = [bad_str,'standard parallel 1 ('+STRTRIM(sp2,2)+') >= standard parallel 2 ('+STRTRIM(sp2,2)+')']
			END
		ELSE:
	ENDCASE
	
	wres		= 1.0
	idx		= WHERE( STRPOS(id_strarr, 'Resolution For Projection Window') GE 0, cnt )
	IF cnt GT 0 THEN wres = FLOAT(strarr2check[1,idx[0]])
	IF cnt gt 0 AND wres LE 0.0 OR wres GE 10000.0 THEN bad_str = [bad_str,'resolution ('+STRTRIM(wres,2)+') is out of range (0.0 < res < 10000.0)']
	IF scale_exists AND cnt gt 0 THEN bad_str = [bad_str,'resolution can only be specified when geographic extents (not scale) are specified']
	www		= 500
	idx		= WHERE( STRPOS(id_strarr, 'Width Of Projection Window') GE 0, cnt )
	IF cnt GT 0 THEN www = FIX(strarr2check[1,idx[0]])
	IF cnt gt 0 AND www LT 50 OR www GT 8000 THEN bad_str = [bad_str,'window width ('+STRTRIM(wres,2)+') is out of range (50 <= width <= 8000)']
	
	hhh		= 500
	idx		= WHERE( STRPOS(id_strarr, 'Height Of Projection Window') GE 0, cnt )
	IF cnt GT 0 THEN hhh = FIX(strarr2check[1,idx[0]])
	IF cnt gt 0 AND hhh LT 50 OR hhh GT 8000 THEN bad_str = [bad_str,'window height ('+STRTRIM(wres,2)+') is out of range (50 <= height <= 8000)']
	
	IF N_ELEMENTS(bad_str) LE 1 AND N_ELEMENTS(suggest_str) LE 1 THEN RETURN, 1
	
	b		= WIDGET_BASE( TITLE = 'Reprojection Parameter Problems',	$
					/COLUMN, GROUP_LEADER = group_leader, /MODAL,	$
					KILL_NOTIFY = 'check_projection_input_kill',	$
					EVENT_PRO = 'display_projection_input_eh' )
	IF N_ELEMENTS(bad_str) GT 1 THEN BEGIN
		l1		= WIDGET_LABEL( b, VALUE = bad_str[0], /ALIGN_CENTER, FONT = font_type )
		FOR i = 1, N_ELEMENTS(bad_str)-1 DO lbl = WIDGET_LABEL( b, VALUE = bad_str[i], /ALIGN_LEFT, FONT = font_type )
	ENDIF
	dum	= WIDGET_LABEL( b, VALUE = '' )
	IF N_ELEMENTS(suggest_str) GT 1 THEN BEGIN
		l1		= WIDGET_LABEL( b, VALUE = suggest_str[0], /ALIGN_CENTER, FONT = font_type )
		FOR i = 1, N_ELEMENTS(suggest_str)-1 DO lbl = WIDGET_LABEL( b, VALUE = suggest_str[i], /ALIGN_LEFT, FONT = font_type )
	ENDIF
			
	b2		= WIDGET_BASE( b, /ROW, /FRAME, /BASE_ALIGN_CENTER, /ALIGN_CENTER )
	IF N_ELEMENTS(bad_str) LE 1 THEN con = WIDGET_BUTTON( b2, VALUE = 'Continue', UVALUE = 'continue', FONT = font_type )
	redo		= WIDGET_BUTTON( b2, VALUE = 'Respecify', UVALUE = 'respecify', FONT = font_type )
	WIDGET_CONTROL, b, /REALIZE
	ptr		= PTR_NEW( { strarr2check:strarr2check, continue:1 }, /NO_COPY )
	WIDGET_CONTROL, b, SET_UVALUE = ptr
	val2return	= (*ptr).continue
	XMANAGER, 'check_projection_input', b,  EVENT_HANDLER = 'display_projection_input_eh'
	PTR_FREE, ptr
	RETURN, val2return
END
; check_projection_input

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ confirm_parameter_integrity @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION confirm_parameter_integrity, ptr, group_leader, DISPLAY_PARAMS = display_params
	WIDGET_CONTROL, (*ptr).clat_cwf, GET_VALUE = cen_lat
	WIDGET_CONTROL, (*ptr).clon_cwf, GET_VALUE = cen_lon
	WIDGET_CONTROL, (*ptr).crot_cwf, GET_VALUE = cen_rot
			
	IF (*ptr).set_center_lon_lat_rot[ (*ptr).current_projection_idx, (*ptr).USE_LONLATROT_IDX ] THEN BEGIN
		req_lon	= (*ptr).set_center_lon_lat_rot[(*ptr).current_projection_idx, (*ptr).LON_IDX ]
		req_lat	= (*ptr).set_center_lon_lat_rot[ (*ptr).current_projection_idx, (*ptr).LAT_IDX ]
		req_rot	= (*ptr).set_center_lon_lat_rot[ (*ptr).current_projection_idx, (*ptr).ROT_IDX ]
		WIDGET_CONTROL, (*ptr).clat_cwf, GET_VALUE = cen_lat
		WIDGET_CONTROL, (*ptr).clon_cwf, GET_VALUE = cen_lon
		WIDGET_CONTROL, (*ptr).crot_cwf, GET_VALUE = cen_rot
				
		IF cen_lat NE req_lat OR cen_lon NE req_lon OR cen_rot NE req_rot THEN BEGIN
			msg	= [									$
					'The current projection (' +					$
					STRUPCASE((*ptr).PROJECTIONS[(*ptr).current_projection_idx])+') ',	$
					'requires the center latitude, center longitude, and angle',	$
					'of rotation to be ' +						$
					STRTRIM(req_lat,2) + ' degrees, ' + STRTRIM(req_lon,2) +	$
					' degrees, and ' + STRTRIM(req_rot,2) + ' degrees,',		$
					'respectively.  Do you want to continue with this projection',	$
					'with these values changed accordingly?' ]
			res	= DIALOG_MESSAGE( msg, /QUESTION )
			IF STRTRIM(STRUPCASE(res),2) EQ 'NO' THEN RETURN, { failure:1 }
		ENDIF
		cen_lat	= req_lat
		cen_lon	= req_lon
		cen_rot	= req_rot
	ENDIF
					
	proj	= STRUPCASE((*ptr).PROJECTIONS[(*ptr).current_projection_idx])
			
	strarr2check	= [ 'Projection Name: ', proj ]
	strarr2check	= [ [strarr2check], ['Center Latitude Of Projection (deg): ', STRTRIM(cen_lat,2)] ]
	strarr2check	= [ [strarr2check], ['Center Longitude Of Projection (deg): ', STRTRIM(cen_lon,2)] ]
	strarr2check	= [ [strarr2check], ['Center Rotation Of Projection (deg): ', STRTRIM(cen_rot,2)] ]
			
	IF proj EQ 'TRANSVERSE MERCATOR' THEN proj = 'TRANSVERSE_MERCATOR'
	IF proj EQ 'GOODES HOMOLOSINE' THEN proj = 'GOODESHOMOLOSINE'

;===== part of process to remove need for EXECUTE =======
;======== original statement not commented out ==========
extra_struct	= CREATE_STRUCT(proj,1)
	proj	= '/'+proj
;===================== ckt, aug2004 =====================
;========================================================
			
	IF (*ptr).current_scale_lim_idx THEN BEGIN
		WIDGET_CONTROL, (*ptr).min_lon_lbl, GET_VALUE = min_lon
		WIDGET_CONTROL, (*ptr).max_lon_lbl, GET_VALUE = max_lon
		WIDGET_CONTROL, (*ptr).min_lat_lbl, GET_VALUE = min_lat
		WIDGET_CONTROL, (*ptr).max_lat_lbl, GET_VALUE = max_lat
		strarr2check	= [ [strarr2check], ['Left Longitude Of Projection (deg): ', STRTRIM(min_lon,2)] ]
		strarr2check	= [ [strarr2check], ['Right Longitude Of Projection (deg): ', STRTRIM(max_lon,2)] ]
		strarr2check	= [ [strarr2check], ['Lower Latitude Of Projection (deg): ', STRTRIM(min_lat,2)] ]
		strarr2check	= [ [strarr2check], ['Upper Latitude Of Projection (deg): ', STRTRIM(max_lat,2)] ]
				
	ENDIF ELSE BEGIN
		WIDGET_CONTROL, (*ptr).scale_cwf, GET_VALUE = out_scale
		strarr2check	= [ [strarr2check], ['Scale Of Projection: 1:', STRTRIM(out_scale,2)] ]
	ENDELSE

;===== part of process to remove need for EXECUTE =======
;======== original statement not commented out ==========
	str2exec	= 'MAP_SET,'+STRTRIM(cen_lat,2)+','+STRTRIM(cen_lon,2)+','+STRTRIM(cen_rot,2) + ',' + proj
;===================== ckt, aug2004 =====================
;========================================================

	IF (*ptr).param_array[ (*ptr).current_projection_idx, (*ptr).CENTRAL_AZIMUTH_IDX, (*ptr).RELEVANCE_IDX ] THEN BEGIN
		WIDGET_CONTROL, WIDGET_INFO( WIDGET_INFO( (*ptr).projection_base_stack[(*ptr).current_projection_idx], /CHILD ), /CHILD ), GET_VALUE = caz
	
;===== part of process to remove need for EXECUTE =======
;======== original statement not commented out ==========
extra_struct	= CREATE_STRUCT('CENTRAL_AZIMUTH',FLOAT(caz),extra_struct)
		str2exec	= str2exec+',CENTRAL_AZIMUTH='+STRTRIM(caz,2)
;===================== ckt, aug2004 =====================
;========================================================
		
		strarr2check	= [ [strarr2check], ['Central Azimuth Of Projection (deg): ', STRTRIM(caz,2)] ]
	ENDIF
		
	IF (*ptr).param_array[ (*ptr).current_projection_idx, (*ptr).ELLIPSOID_IDX, (*ptr).RELEVANCE_IDX ] THEN BEGIN
		p1		= WIDGET_INFO( WIDGET_INFO( (*ptr).ellip_base1[(*ptr).current_projection_idx], /CHILD ), /SIBLING )
		p2		= WIDGET_INFO( p1, /SIBLING )
		p3		= WIDGET_INFO( p2, /SIBLING )
		WIDGET_CONTROL, p1, GET_VALUE = r
		WIDGET_CONTROL, p2, GET_VALUE = e
		WIDGET_CONTROL, p3, GET_VALUE = m
		rr	= STR_SEP(r,':')
		r	= FLOAT(rr[1])
		ee	= STR_SEP(e,':')
		e	= FLOAT(ee[1])
		mm	= STR_SEP(m,':')
		m	= FLOAT(mm[1])
		
;===== part of process to remove need for EXECUTE =======
;======== original statement not commented out ==========
extra_struct	= CREATE_STRUCT('ELLIPSOID',[r,e,m],extra_struct)
		str2exec	=str2exec+',ELLIPSOID=['+STRTRIM(r,2)+','+STRTRIM(e,2)+','+STRTRIM(m,2)+']'
;===================== ckt, aug2004 =====================
;========================================================
		
		strarr2check	= [ [strarr2check], ['Equatorial Radius For Ellipsoid (m): ', STRTRIM(r,2)] ]
		strarr2check	= [ [strarr2check], ['Eccentricity Squared For Ellipsoid: ', STRTRIM(e,2)] ]
		strarr2check	= [ [strarr2check], ['Scale On Central Meridian For Ellipsoid: ', STRTRIM(m,2)] ]
	ENDIF
		
	IF (*ptr).param_array[ (*ptr).current_projection_idx, (*ptr).SAT_P_IDX, (*ptr).RELEVANCE_IDX ] THEN BEGIN
		p1		= WIDGET_INFO( (*ptr).satp_base1[(*ptr).current_projection_idx], /CHILD )
		p2		= WIDGET_INFO( p1, /SIBLING )
		p3		= WIDGET_INFO( p2, /SIBLING )
		WIDGET_CONTROL, p1, GET_VALUE = f
		WIDGET_CONTROL, p2, GET_VALUE = o
		WIDGET_CONTROL, p3, GET_VALUE = g
		
;===== part of process to remove need for EXECUTE =======
;======== original statement not commented out ==========
extra_struct	= CREATE_STRUCT('SAT_P',[FLOAT(f),FLOAT(o),FLOAT(g)],extra_struct)
		str2exec	= str2exec+',SAT_P=['+STRTRIM(f,2)+','+STRTRIM(o,2)+','+STRTRIM(g,2)+']'
;===================== ckt, aug2004 =====================
;========================================================
		
		strarr2check	= [ [strarr2check], ['Distance From Perspective Point To Center Of Globe (radii): ', STRTRIM(f,2)] ]
		strarr2check	= [ [strarr2check], ['Omega, Downward Tilt Of Camera (deg): ', STRTRIM(o,2)] ]
		strarr2check	= [ [strarr2check], ['Gamma, Rotation Of Projection Plane (deg): ', STRTRIM(g,2)] ]
	ENDIF
		
	IF (*ptr).param_array[ (*ptr).current_projection_idx, (*ptr).STANDARD_PARALLELS_IDX, (*ptr).RELEVANCE_IDX ] THEN BEGIN
		CASE (*ptr).param_array[(*ptr).current_projection_idx,(*ptr).STANDARD_PARALLELS_IDX,(*ptr).N_ENTRIES_USED_IDX] OF
			0:
			1: BEGIN
				widget_control, widget_info(widget_info((*ptr).parallel_base1[(*ptr).current_projection_idx],/child),/sibling),GET_VALUE = p1
				
;===== part of process to remove need for EXECUTE =======
;======== original statement not commented out ==========
extra_struct	= CREATE_STRUCT('STANDARD_PARALLELS',[FLOAT(p1)],extra_struct)
				str2exec	= str2exec+',STANDARD_PARALLELS=['+STRTRIM(p1,2)+']'
;===================== ckt, aug2004 =====================
;========================================================
				
				strarr2check	= [ [strarr2check], ['Standard Parallel 1 For Projection (deg): ', STRTRIM(p1,2)] ]
				END
			2: BEGIN
				WIDGET_CONTROL, WIDGET_INFO(WIDGET_INFO((*ptr).parallel_base1[(*ptr).current_projection_idx],/child),/sibling),GET_VALUE = p1
				WIDGET_CONTROL,  WIDGET_INFO(WIDGET_INFO(WIDGET_INFO((*ptr).parallel_base1[(*ptr).current_projection_idx],/child),/sibling),/sibling),GET_VALUE = p2
				
;===== part of process to remove need for EXECUTE =======
;======== original statement not commented out ==========
extra_struct	= CREATE_STRUCT('STANDARD_PARALLELS',[FLOAT(p1),FLOAT(p2)],extra_struct)
				str2exec	= str2exec+',STANDARD_PARALLELS=['+STRTRIM(p1,2)+','+STRTRIM(p2,2)+']'
;===================== ckt, aug2004 =====================
;========================================================
				
				strarr2check	= [ [strarr2check], ['Standard Parallel 1 For Projection (deg): ', STRTRIM(p1,2)] ]
				strarr2check	= [ [strarr2check], ['Standard Parallel 2 For Projection (deg): ', STRTRIM(p2,2)] ]
				END
			ELSE:
		ENDCASE
	ENDIF
			
	final_height	= (-1)
	final_width	= final_height
	final_res	= (*ptr).default_res
	IF (*ptr).current_res_windim_idx THEN BEGIN
		WIDGET_CONTROL, (*ptr).res_cwf, GET_VALUE = res
		strarr2check	= [ [strarr2check], ['Resolution For Projection Window (km): ', STRTRIM(res,2)] ]
		final_res	= res
	ENDIF ELSE BEGIN
		WIDGET_CONTROL, (*ptr).height_lbl, GET_VALUE = hhh
		WIDGET_CONTROL, (*ptr).width_lbl, GET_VALUE = www
		strarr2check	= [ [strarr2check], ['Width Of Projection Window: ', STRTRIM(www,2)] ]
		strarr2check	= [ [strarr2check], ['Height Of Projection Window: ', STRTRIM(hhh,2)] ]
		final_width	= www
		final_height	= hhh
	ENDELSE
			
	strarr2check	= [ [strarr2check], ['Isotropic Map?: ', (['No','Yes'])[(*ptr).isotropic] ] ]
	strarr2check	= [ [strarr2check], ['Perform Seam Filling?: ', (['No','Yes'])[(*ptr).do_seam_filling] ] ]
	
	IF (*ptr).do_seam_filling THEN BEGIN
		strarr2check	= [ [strarr2check], ['Box Width: ', STRTRIM(FIX((*ptr).fill_seam_info[0]),2) ] ]
		strarr2check	= [ [strarr2check], ['Percentage Of Good Pixels Required In Box : ', STRTRIM((*ptr).fill_seam_info[1]*100.0,2) ] ]
	ENDIF
	
	strarr2check	= [ [strarr2check], ['Resolve Overlap By Using: ',(*ptr).OVERLAP_OPTIONS[(*ptr).overlap_idx] ] ]
	
	strarr2check	= [ [strarr2check], ['Create Overlay Map?: ', (['No','Yes'])[(*ptr).overlay] ] ]
	;
	;
	; check all input values to see if they make sense
	;
	;
			
	IF KEYWORD_SET(display_params) THEN BEGIN
		good_flag	= display_projection_input( strarr2check, group_leader, (*ptr).font_type )
		IF NOT good_flag THEN RETURN, { failure:1 }
	ENDIF
						
	good_flag	= check_projection_input( strarr2check, group_leader, (*ptr).font_type )
	
	
			
	IF NOT good_flag THEN RETURN, { failure:1 }
			
	IF (*ptr).current_scale_lim_idx THEN BEGIN
	
;===== part of process to remove need for EXECUTE =======
;======== original statements not commented out ==========

scale_limit_keyword	= 'LIMIT'
		IF STRUPCASE((*ptr).PROJECTIONS[(*ptr).current_projection_idx]) EQ 'SATELLITE' THEN BEGIN
			IF min_lon LT max_lon THEN BEGIN
				extent_str	= 'LIMIT = [ '+ STRTRIM((max_lat+min_lat)/2.0,2)	+ ',' + STRTRIM(min_lon,2) + ',' +	$
								STRTRIM(max_lat,2)			+ ',' + STRTRIM((max_lon+min_lon)/2.0,2) + ',' +		$
	                  					STRTRIM((max_lat+min_lat)/2.0,2)	+ ',' + STRTRIM(max_lon,2) + ',' +	$
	                     					STRTRIM(min_lat,2)			+ ',' + STRTRIM((max_lon+min_lon)/2.0,2) + ']'
scale_limit_value	= FLOAT([(max_lat+min_lat)/2.0,min_lon,max_lat,(max_lon+min_lon)/2.0,(max_lat+min_lat)/2.0,max_lon,min_lat,(max_lon+min_lon)/2.0])
			ENDIF ELSE BEGIN
				extent1		= 180.0 - min_lon
				extent2		= 180.0 + max_lon
				half_extent	= (extent1 + extent2)/2.0
				mid_lon		= min_lon + half_extent
				IF mid_lon GT 180.0 THEN mid_lon = mid_lon - 360.0
				IF tmp GT 180.0 THEN tmp = tmp - 360.0
				extent_str	= 'LIMIT = [ '+	STRTRIM((max_lat+min_lat)/2.0,2)	+ ',' + STRTRIM(min_lon,2) + ',' +	$
								STRTRIM(max_lat,2)			+ ',' + STRTRIM(mid_lon) + ',' +	$
                   						STRTRIM((max_lat+min_lat)/2.0,2)	+ ',' + STRTRIM(max_lon,2) + ',' +	$
	                     					STRTRIM(min_lat,2)			+ ',' + STRTRIM(mid_lon) + ']'
scale_limit_value	= FLOAT([(max_lat+min_lat)/2.0,min_lon,max_lat,mid_lon,(max_lat+min_lat)/2.0,max_lon,min_lat,mid_lon])
			ENDELSE
		ENDIF ELSE BEGIN
			extent_str	= 'LIMIT=['+STRTRIM(min_lat,2)+','+STRTRIM(min_lon,2)+','+STRTRIM(max_lat,2)+','+STRTRIM(max_lon,2)+']'
scale_limit_value	= FLOAT([min_lat,min_lon,max_lat,max_lon])
		ENDELSE
	ENDIF ELSE BEGIN
scale_limit_keyword	= 'SCALE'
		extent_str	= 'SCALE = '+STRTRIM(out_scale,2)
scale_limit_value	= FLOAT(out_scale)
	ENDELSE
		
extra_struct	= CREATE_STRUCT(scale_limit_keyword,scale_limit_value,extra_struct)

;===================== ckt, aug2004 =====================
;========================================================
	
	
	
	
	
;===== part of process to remove need for EXECUTE =======
;======== original statements not commented out ==========

	IF (*ptr).isotropic OR NOT (*ptr).current_scale_lim_idx THEN str2exec = str2exec + ',/ISOTROPIC'
IF (*ptr).isotropic OR NOT (*ptr).current_scale_lim_idx THEN extra_struct = CREATE_STRUCT('ISOTROPIC',1,extra_struct)

;===================== ckt, aug2004 =====================
;========================================================


;===== part of process to remove need for EXECUTE =======
;======== original statements not commented out ==========
	
extra_struct	= CREATE_STRUCT('NOBORDER',1,extra_struct)
	str2exec		= str2exec+','+extent_str+',/NOBORDER
	
;===================== ckt, aug2004 =====================
;========================================================

	IF final_height LT 0 THEN BEGIN
		IF (*ptr).current_scale_lim_idx THEN BEGIN
			use_polar	= (STRUPCASE((*ptr).PROJECTIONS[(*ptr).current_projection_idx]) EQ 'LAMBERT' OR	$
					  STRUPCASE((*ptr).PROJECTIONS[(*ptr).current_projection_idx]) EQ 'AZIMUTHAL') AND	$
					  (cen_lat EQ -90.0 OR cen_lat EQ 90.0)
			s	= return_resolution_info(final_res,[min_lat,min_lon,max_lat,max_lon], POLAR=use_polar)
			final_height	= s.nlines
			final_width	= s.nsamps
		ENDIF ELSE BEGIN
			; should never get to this point.
			final_height	= 500
			final_width	= 500
		ENDELSE
	ENDIF
	;
	; need to pass back:
	; 
	; map_set string
	; window_dims
	; string array of all parameter settings
	; seam filling method (if any)
	; box width
	; pct of good pixels required in box
	;
	;
	;
	
	RETURN, {								$
			failure			: 0,				$
			
			map_set_str		: str2exec,			$
			
;===== part of process to remove need for EXECUTE =======
;======== original statements not commented out ==========
extra_struct	: extra_struct,						$
cen_lat		: cen_lat,						$
cen_lon		: cen_lon,						$
cen_rot		: cen_rot,						$
;===================== ckt, aug2004 =====================
;========================================================

			window_width		: final_width,			$
			window_height		: final_height,			$
			projection_name		: STRUPCASE((*ptr).PROJECTIONS[(*ptr).current_projection_idx]), $
			parameter_listing	: strarr2check,			$
			fill_seams		: (*ptr).do_seam_filling,	$
			seam_fill_parameters	: (*ptr).fill_seam_info,	$
			create_overlay_image	: (*ptr).overlay,		$
			overlap_idx		: (*ptr).overlap_idx }
			
END
; confirm_parameter_integrity

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ get_new_filter_info_eh @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO get_new_filter_info_eh, event
	WIDGET_CONTROL, event.id, GET_UVALUE = widget_name
	WIDGET_CONTROL, event.top, GET_UVALUE = ptr
	CASE STRTRIM(STRUPCASE(widget_name),2) OF
		'DEFAULTS': BEGIN
			WIDGET_CONTROL, WIDGET_INFO(event.top, /CHILD), SET_VALUE = (*ptr).fill_seam_info[2]
			WIDGET_CONTROL, WIDGET_INFO(WIDGET_INFO(event.top, /CHILD),/SIBLING), SET_VALUE = (*ptr).fill_seam_info[3]*100.0
			END
		'OK': BEGIN
			WIDGET_CONTROL, WIDGET_INFO(event.top, /CHILD), GET_VALUE = new_width
			IF new_width LT 3 OR new_width GT 7 OR new_width MOD 2 EQ 0 THEN BEGIN
				res	= DIALOG_MESSAGE( [	'Problem With Box Width Input!',			$
								'The only valid entries are odd numbers between',	$
								'3 and 7 inclusive' ], /ERROR )
				RETURN
			ENDIF
			WIDGET_CONTROL, WIDGET_INFO(WIDGET_INFO(event.top, /CHILD),/SIBLING), GET_VALUE = new_pct
			IF new_pct LT 0.0 OR new_pct GT 100.0 THEN BEGIN
				res	= DIALOG_MESSAGE( [	'Problem With Good Pixel Percentage Input!',			$
								'The only valid entries are numbers between',	$
								'0.0 and 100.0 inclusive' ], /ERROR )
				RETURN
			ENDIF
			(*ptr).fill_seam_info[0]	= new_width
			(*ptr).fill_seam_info[1]	= new_pct/100.0
			WIDGET_CONTROL, event.top, /DESTROY
			END
		'CANCEL': BEGIN
			WIDGET_CONTROL, event.top, /DESTROY
			END
		ELSE:
	ENDCASE
END
; get_new_filter_info_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ get_new_filter_info @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO get_new_filter_info, ptr, group_leader
	b	= WIDGET_BASE( TITLE = 'Box Filter Parameters', /MODAL,		$
			GROUP_LEADER = group_leader, /BASE_ALIGN_CENTER,	$
			/ALIGN_CENTER, /COLUMN, EVENT_PRO = 'get_new_filter_info_eh' )
	box_width_cwf	= CW_FIELD( b, FONT = (*ptr).font_type,  VALUE = FIX((*ptr).fill_seam_info[0]),	$
				/INTEGER, TITLE = 'Width Of Averaging Box (3->7):', UVALUE = 'box_width')
	pct_cwf	= CW_FIELD( b, FONT = (*ptr).font_type, VALUE = (*ptr).fill_seam_info[1]*100.0,	$
				TITLE = 'Required Percentage Of Good Pixels Within Averaging Box(0.0->100.0):', UVALUE = 'pct', /FLOATING )
	def	= WIDGET_BUTTON( b, VALUE = 'use defaults', FONT = (*ptr).font_type, UVALUE = 'defaults' )
	b2	= WIDGET_BASE( b, /ROW, /BASE_ALIGN_CENTER, /ALIGN_CENTER )
	ok	= WIDGET_BUTTON( b2, VALUE = 'OK', FONT = (*ptr).font_type, UVALUE = 'ok' )
	cancel	= WIDGET_BUTTON( b2, VALUE = 'Cancel', FONT = (*ptr).font_type, UVALUE = 'cancel' )
	WIDGET_CONTROL, b, /REALIZE
	WIDGET_CONTROL, b, SET_UVALUE = ptr
	XMANAGER, 'get_new_filter_info', b, EVENT_HANDLER = 'get_new_filter_info_eh'
END
;get_new_filter_info

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ cw_proj_info_eh @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO cw_proj_info_eh, event

	WIDGET_CONTROL, event.id, GET_UVALUE = widget_name
	WIDGET_CONTROL, event.top, GET_UVALUE = ptr

	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		confirm_struct	= confirm_parameter_integrity(ptr,event.top)
		msg	= [										$
				'ERROR INDEX: ' + STRTRIM(error_status,2),				$
				'ERROR MESSAGE: ' + STRTRIM(!ERROR_STATE.MSG,2),			$
				'',									$
				'This message may have occurred if the dimensions of the output',	$
				'projection image are too large.  Unless explicitly specified,',	$
				'window dimensions are determined by the specified projection',		$
				'resolution.',								$
				'',									$
				'OR',									$
				'',									$
				'This message may have occured if a file read or write is attempted',	$
				'and the file protections are insufficient.  Check file permissions.',	$
				'',									$
				'' ]

		IF NOT confirm_struct.failure THEN BEGIN
		
			IF (*ptr).current_res_windim_idx THEN BEGIN
				WIDGET_CONTROL, (*ptr).res_cwf, GET_VALUE = res
				msg	= [ msg,							$
						'Given the current resolution of ' + STRTRIM(res,2)+' km,' ]
			ENDIF
			
			msg	= [ msg,								$
					'the current projection window dimensions are:',		$
					'     ',							$
					'     Window Width: '+STRTRIM(confirm_struct.window_width,2),	$
					'     Window Height: '+STRTRIM(confirm_struct.window_height,2) ]
		ENDIF
		res	= DIALOG_MESSAGE( msg, /ERROR )
		RETURN
	ENDIF

		
	widget_name	= STRUPCASE(STRTRIM(widget_name,2))
;print,'widget_name=',widget_name
	CASE 1 OF
		STRPOS( widget_name, 'EXTENT_HELP' ) GE 0: BEGIN
			msg	= [														$
					'Projection extent refers to the how much of the earth is mapped to the projection window.',		$
					'There are two ways to specify extent, scale and limits.',						$
					'     ',												$
					'Specifying extent by limits requires that left longitude, right longitude, lower latitude,',		$
					'and upper latitude bounding points be specified.  As usual, latitude values should',			$
					'be between -90.0 and 90.0 degrees inclusive, while longitude values should be between -180.0',		$
					'and 180.0 degrees inclusive.',										$
					'     ',												$
					'While the lower latitude bounds must always be less that the upper latitude bounds, the left',		$
					'longitude bounds can be either greater than or less than the right longitude bounds.',			$
					'     ',												$
					'Specifying map extents by scale will construct an isotropic map with the specified scale at',		$
					'a ratio of 1:scale.  Typical scales for global maps are between 1:100 million and 1:200 million.',	$
					'For continents, typiocal scale is around 1:50 million.' ]
			XDISPLAYFILE, dumfile, DONE_BUTTON = 'Done', TEXT = msg, /MODAL
			END
		STRPOS( widget_name, 'CENTER_HELP' ) GE 0: BEGIN
			msg	= [												$
					'Projection center refers to the center of the projection plane.',			$
					'     ',										$
					'Center latitude is the latitude of the point on the earth"s surface',			$
					'to be mapped to the center of the projection plane.  Valid values are',		$
					'between -90.0 degrees and 90.0 degrees inclusive.',					$
					'     ',										$
					'Center longitude is the longitude of the point on the earth"s surface',		$
					'to be mapped to the center of the projection plane.  Valid values are',		$
					'between -180.0 degrees and 180.0 degrees inclusive.',					$
					'     ',										$
					'Rotation is the angle through which the North direction should be rotated',		$
					'around line L between the earth"s center and the point (center_lat, center_lon).',	$
					'Rotation is measured in degrees with the positive direction being clockwise rotation',	$
					'around line L.  Valid values are between -180.0 degrees and 180.0 degrees inclusive.' ]
			XDISPLAYFILE, dumfile, DONE_BUTTON = 'Done', TEXT = msg, /MODAL
			END
		STRPOS( widget_name, 'TYPE_HELP' ) GE 0: BEGIN
			msg	= [												$
					'For information on each of the projections listed here, please consult the IDL',	$
					'User"s Guide under the procedure MAP_SET.  Currently, the only projection that',	$
					'is problematic is the SATELLITE projection.' ]
			XDISPLAYFILE, dumfile, DONE_BUTTON = 'Done', TEXT = msg, /MODAL
			END
		STRPOS( widget_name, 'WINDOW_HELP' ) GE 0: BEGIN
			msg	= [														$
					'Projection window refers to how the dimensions of the output window for the projected image are',	$
					'determined.  There are two ways to specify window size, explicitly setting the window width and',	$
					'height, or setting the window resolution.',								$
					'     ',												$
					'If using the resolution option, window dimensions are determined by using a simple routine to',	$
					'determine a reasonable estimate for input data resolution, then using that value and the',		$
					'longitudinal and latitudinal extents of the input data to determine window sizing.' ]
			XDISPLAYFILE, dumfile, DONE_BUTTON = 'Done', TEXT = msg, /MODAL
			END
		STRPOS( widget_name, 'OTHER_HELP' ) GE 0: BEGIN
			msg	= [														$
					'Other settings refers to options that the user can specify to occur when reprojecting',		$
					'data.',												$
					'     ',												$
					'If more than one image is to be reprojected into the output projection window, ',			$
					'any image overlap that occurs during the reprojection process can be resolved by',			$
					'selecting one of the options under the "Resolve Multiple Image Overlap..." droplist.',			$
					'     ',												$
					'The "Apply Seam Filling Algorithm" check box can be set so that any seams between ',			$
					'reprojected images can be filled.  There are options for the "averaging fill box" width and ',		$
					'percentage of good pixels that must be within the box before averaging occurs.  Generally',		$
					'speaking, the default values are good enough for most applications.',					$
					'     ',												$
					'The "Use Isotropic Scaling" check box forces the output reprojected image to have the same',		$
					'scale in both the latitudinal and longitudinal directions.',						$
					'     ',												$
					'The "Create TIFF Overlay Image" check box will allow the user to create an image with the ',		$
					'same projection parameters and dimensions as the reprojected data.  This overlay image can then',	$
					'be combined with the reprojected data to annotate with grid lines and continental outlines,',		$
					'if within the reprojected area.' ]
			XDISPLAYFILE, dumfile, DONE_BUTTON = 'Done', TEXT = msg, /MODAL

			END
		STRPOS( widget_name, 'SAVE' ) GE 0: BEGIN
			confirm_struct	= confirm_parameter_integrity(ptr,event.top)
			IF confirm_struct.failure THEN RETURN
;;;ckt,apr2001			filename	= DIALOG_PICKFILE( TITLE = 'Enter the name of the projection parameter save file' )
			filename	= dialog_pickfile_wrapper( TITLE = 'Enter the name of the projection parameter save file' )
			IF filename EQ '' THEN RETURN
			save_parameter_file, filename, ptr
			END
		STRPOS( widget_name, 'RESTORE' ) GE 0: BEGIN
;;;ckt,apr2001			filename	= DIALOG_PICKFILE( TITLE = 'Enter the name of the projection parameter save file' )
			filename	= dialog_pickfile_wrapper( /MUST_EXIST, TITLE = 'Enter the name of the projection parameter save file' )
			IF filename EQ '' THEN RETURN
			restore_parameter_file, filename, ptr
			END
		STRPOS( widget_name, 'FILL_SEAM_OPTIONS' ) GE 0: BEGIN
			get_new_filter_info, ptr, event.top
			END
		STRPOS( widget_name, 'OVERLAP' ) GE 0: BEGIN
			(*ptr).overlap_idx	= event.index
			END
		STRPOS( widget_name, 'OVERLAY' ) GE 0: BEGIN
			(*ptr).overlay	= event.select
			END
		STRPOS( widget_name, 'ISOTROPIC' ) GE 0: BEGIN
			(*ptr).isotropic	= event.select
			END
		STRPOS( widget_name, 'FILL_SEAMS' ) GE 0: BEGIN
			(*ptr).do_seam_filling	= event.select
			WIDGET_CONTROL, (*ptr).fill_seam_options_btn, SENSITIVE = (*ptr).do_seam_filling
			END
		STRPOS( widget_name, 'USECENTERDEFAULTS' ) GE 0: BEGIN
			WIDGET_CONTROL, (*ptr).clat_cwf, SET_VALUE = (*ptr).center_defaults[0]
			WIDGET_CONTROL, (*ptr).clon_cwf, SET_VALUE = (*ptr).center_defaults[1]
			WIDGET_CONTROL, (*ptr).crot_cwf, SET_VALUE = (*ptr).center_defaults[2]
		END
		STRPOS( widget_name, 'USELIMITDEFAULTS' ) GE 0: BEGIN
			WIDGET_CONTROL, (*ptr).min_lon_lbl, SET_VALUE = (*ptr).limit_defaults[1]
			WIDGET_CONTROL, (*ptr).max_lon_lbl, SET_VALUE = (*ptr).limit_defaults[3]
			WIDGET_CONTROL, (*ptr).min_lat_lbl, SET_VALUE = (*ptr).limit_defaults[0]
			WIDGET_CONTROL, (*ptr).max_lat_lbl, SET_VALUE = (*ptr).limit_defaults[2]
		END
		STRPOS( widget_name, 'USESCALEDEFAULT' ) GE 0: BEGIN
			WIDGET_CONTROL, (*ptr).scale_cwf, SET_VALUE = (*ptr).default_scale
		END
		STRPOS( widget_name, 'USEPROJDEFAULT' ) GE 0: BEGIN
			idx	= WHERE( STRUPCASE((*ptr).PROJECTIONS) EQ STRUPCASE((*ptr).default_proj_name), cnt )
			IF cnt GT 0 THEN BEGIN
				WIDGET_CONTROL, (*ptr).proj_droplist, SET_DROPLIST_SELECT = idx[0]
				WIDGET_CONTROL, (*ptr).projection_base_stack[(*ptr).current_projection_idx], MAP = 0
				WIDGET_CONTROL, (*ptr).projection_base_stack[idx[0]], MAP = 1
				(*ptr).current_projection_idx	= idx[0]
			ENDIF
		END
		STRPOS( widget_name, 'USECENAZDEFAULTS' ) GE 0: BEGIN
			WIDGET_CONTROL, WIDGET_INFO( WIDGET_INFO( (*ptr).projection_base_stack[(*ptr).current_projection_idx], /CHILD ), /CHILD ),	$
						SET_VALUE = (*ptr).cen_az_default
		END
		STRPOS( widget_name, 'USEELLIPDEFAULTS' ) GE 0: BEGIN
			p1		= WIDGET_INFO( WIDGET_INFO( (*ptr).ellip_base1[(*ptr).current_projection_idx], /CHILD ), /SIBLING )
			p2		= WIDGET_INFO( p1, /SIBLING )
			p3		= WIDGET_INFO( p2, /SIBLING )
			WIDGET_CONTROL, p1, SET_VALUE = 'Equatorial Radius (m):         '+STRTRIM((*ptr).ellip_default[0],2)
			WIDGET_CONTROL, p2, SET_VALUE = 'Eccentricity Squared:          '+STRTRIM((*ptr).ellip_default[1],2)
			WIDGET_CONTROL, p3, SET_VALUE = 'Center Meridian Scale:         '+STRTRIM((*ptr).ellip_default[2],2)
		END
		STRPOS( widget_name, 'USESATPDEFAULTS' ) GE 0: BEGIN
			p1		= WIDGET_INFO( (*ptr).satp_base1[(*ptr).current_projection_idx], /CHILD )
			p2		= WIDGET_INFO( p1, /SIBLING )
			p3		= WIDGET_INFO( p2, /SIBLING )
			WIDGET_CONTROL, p1, SET_VALUE = (*ptr).satp_default[0]
			WIDGET_CONTROL, p2, SET_VALUE = (*ptr).satp_default[1]
			WIDGET_CONTROL, p3, SET_VALUE = (*ptr).satp_default[2]
		END
		STRPOS( widget_name, 'USEPARDEFAULTS' ) GE 0: BEGIN
			WIDGET_CONTROL, WIDGET_INFO(WIDGET_INFO((*ptr).parallel_base1[(*ptr).current_projection_idx],/child),/sibling),	$
				SET_VALUE = (*ptr).parallel_default[0]
			WIDGET_CONTROL,  WIDGET_INFO(WIDGET_INFO(WIDGET_INFO((*ptr).parallel_base1[(*ptr).current_projection_idx],/child),/sibling),/sibling),	$
				SET_VALUE = (*ptr).parallel_default[0]
		END
		STRPOS( widget_name, 'PROJ_DLIST' ) GE 0: BEGIN
			WIDGET_CONTROL, (*ptr).projection_base_stack[(*ptr).current_projection_idx], MAP = 0
			WIDGET_CONTROL, (*ptr).projection_base_stack[event.index], MAP = 1
			(*ptr).current_projection_idx	= event.index
			END
		STRPOS( widget_name, 'PARALLEL_DROP' ) GE 0: BEGIN
			p1	= WIDGET_INFO( event.id, /SIBLING )
			p2	= WIDGET_INFO( p1, /SIBLING )
			CASE event.index OF
				0: BEGIN
					WIDGET_CONTROL, p1, SENSITIVE = 0
					WIDGET_CONTROL, p2, SENSITIVE = 0
					(*ptr).param_array[(*ptr).current_projection_idx,(*ptr).STANDARD_PARALLELS_IDX,(*ptr).N_ENTRIES_USED_IDX]	= 0
					END
				1: BEGIN
					WIDGET_CONTROL, p1, SENSITIVE = 1
					WIDGET_CONTROL, p2, SENSITIVE = 0
					(*ptr).param_array[(*ptr).current_projection_idx,(*ptr).STANDARD_PARALLELS_IDX,(*ptr).N_ENTRIES_USED_IDX]	= 1
					END
				2: BEGIN
					WIDGET_CONTROL, p1, SENSITIVE = 1
					WIDGET_CONTROL, p2, SENSITIVE = 1
					(*ptr).param_array[(*ptr).current_projection_idx,(*ptr).STANDARD_PARALLELS_IDX,(*ptr).N_ENTRIES_USED_IDX]	= 2
					END
				ELSE:
			ENDCASE
			END
		STRPOS( widget_name, 'DISMISS' ) GE 0: BEGIN
			WIDGET_CONTROL, event.top, /DESTROY
			END
		STRPOS( widget_name, 'MAIN_DROP' ) GE 0: BEGIN
			WIDGET_CONTROL, (*ptr).main_base_arr[(*ptr).current_main_drop_idx], MAP = 0
			WIDGET_CONTROL, (*ptr).main_base_arr[event.index], MAP = 1
			(*ptr).current_main_drop_idx	= event.index
			END
		STRPOS( widget_name, 'LIMIT' ) GE 0: BEGIN
			WIDGET_CONTROL, (*ptr).scale_lim_barr[(*ptr).current_scale_lim_idx], MAP = 0
			IF event.select THEN (*ptr).current_scale_lim_idx = 1 ELSE (*ptr).current_scale_lim_idx = 0
			WIDGET_CONTROL, (*ptr).scale_lim_barr[(*ptr).current_scale_lim_idx], MAP = 1
			END
		STRPOS( widget_name, 'SCALE' ) GE 0: BEGIN
			WIDGET_CONTROL, (*ptr).scale_lim_barr[(*ptr).current_scale_lim_idx], MAP = 0
			IF event.select THEN (*ptr).current_scale_lim_idx = 0 ELSE (*ptr).current_scale_lim_idx = 1
			WIDGET_CONTROL, (*ptr).scale_lim_barr[(*ptr).current_scale_lim_idx], MAP = 1
			END
		STRPOS( widget_name, 'DATARES' ) GE 0: BEGIN
			WIDGET_CONTROL, (*ptr).res_windim_barr[(*ptr).current_res_windim_idx], MAP = 0
			IF event.select THEN (*ptr).current_res_windim_idx = 1 ELSE (*ptr).current_res_windim_idx = 0
			WIDGET_CONTROL, (*ptr).res_windim_barr[(*ptr).current_res_windim_idx], MAP = 1
			END
		STRPOS( widget_name, 'USEWINDIMDEFAULTS' ) GE 0: BEGIN
;print,'inside wh event portion'
			WIDGET_CONTROL, (*ptr).height_lbl, SET_VALUE = (*ptr).wh_defaults[0]
			WIDGET_CONTROL, (*ptr).width_lbl, SET_VALUE = (*ptr).wh_defaults[1]
			END
		STRPOS( widget_name, 'WINDIM' ) GE 0: BEGIN
			WIDGET_CONTROL, (*ptr).res_windim_barr[(*ptr).current_res_windim_idx], MAP = 0
			IF event.select THEN (*ptr).current_res_windim_idx = 0 ELSE (*ptr).current_res_windim_idx = 1
			WIDGET_CONTROL, (*ptr).res_windim_barr[(*ptr).current_res_windim_idx], MAP = 1
			END
		STRPOS( widget_name, 'USERESDEFAULT' ) GE 0: BEGIN
			WIDGET_CONTROL, (*ptr).res_cwf, SET_VALUE = (*ptr).res_default
			END
		STRPOS( widget_name, 'PREVIEW' ) GE 0: BEGIN
			confirm_struct	= confirm_parameter_integrity(ptr,event.top)
			IF confirm_struct.failure THEN RETURN
			w		= FLOAT(confirm_struct.window_width)
			h		= FLOAT(confirm_struct.window_height)
			screen_wh	= GET_SCREEN_SIZE()
			IF w GT h THEN BEGIN
				out_w	= FIX(screen_wh[0])
				out_h	= (h/w)*screen_wh[0]
				IF out_h GT screen_wh[1] THEN BEGIN
					out_h	= FIX(screen_wh[1])
					out_w	= (w/h)*screen_wh[1]
				ENDIF
			ENDIF ELSE BEGIN
				out_h	= FIX(screen_wh[1])
				out_w	= (w/h)*screen_wh[1]
				IF out_w GT screen_wh[0] THEN BEGIN
					out_w	= FIX(screen_wh[0])
					out_h	= (h/w)*screen_wh[0]
				ENDIF
			ENDELSE
			res	= DIALOG_MESSAGE( [								$
							'A preview window will appear for several seconds',	$
							'which will display continental outlines and lat/lon',	$
							'grid lines in the selected projection using the',	$
							'selected projection parameters.  The window will',	$
							'not necessarily be the same size as the actual',	$
							'output window, but it will have the same',		$
							'width/height aspect ratio.' ], /INFORMATION )
			last_win	= !D.WINDOW
;print,'confirm_struct.map_set_str=',confirm_struct.map_set_str
			WINDOW, /FREE, XSIZE = out_w, YSIZE = out_h, TITLE = 'Projection Preview Window ('+	$
					STRUPCASE((*ptr).PROJECTIONS[(*ptr).current_projection_idx]) +')'
			tmpw	= !D.WINDOW
			
extra_struct	= confirm_struct.extra_struct
extra_struct	= CREATE_STRUCT('CONTINENTS',1,'GRID',1,'LABEL',1,extra_struct)
CALL_PROCEDURE, 'MAP_SET',confirm_struct.cen_lat,confirm_struct.cen_lon,confirm_struct.cen_rot, _Extra = extra_struct
;;;ckt,aug2004			success		= EXECUTE(confirm_struct.map_set_str+',/CONTINENTS,/GRID,/LABEL')
			
			WAIT, 5
			IF !D.WINDOW EQ tmpw THEN WDELETE, !D.WINDOW
			WSET, last_win
				
			END
		STRPOS( widget_name, 'REPROJECT' ) GE 0: BEGIN
			confirm_struct	= confirm_parameter_integrity(ptr,event.top,/DISPLAY_PARAMS)
			IF confirm_struct.failure THEN RETURN
			
			last_win	= !D.WINDOW
			WINDOW, /FREE, /PIXMAP, XSIZE = confirm_struct.window_width, YSIZE = confirm_struct.window_height
			tmpw		= !D.WINDOW
			
			IF confirm_struct.create_overlay_image THEN BEGIN
;;;ckt,apr2001				filename	= DIALOG_PICKFILE( TITLE = 'Enter the name of the TIFF overlay image to be saved' )
				filename	= dialog_pickfile_wrapper( TITLE = 'Enter the name of the TIFF overlay image to be saved' )
				done		= 0
				WHILE STRTRIM(filename,2) EQ '' AND NOT done DO BEGIN
					res	= DIALOG_MESSAGE( [						$
									'No filename has been entered for the',	$
									'overlay image file...  do you still',	$
									'want to save an overlay image for',	$
									'the current projection parameters?' ],	$
							/QUESTION )
					IF STRUPCASE(STRTRIM(res,2)) EQ 'NO' THEN				$
						done = 1							$
					ELSE									$
;;;ckt,apr2001						filename	= DIALOG_PICKFILE( TITLE = 'Enter the name of the overlay image to be saved' )
						filename	= dialog_pickfile_wrapper( TITLE = 'Enter the name of the overlay image to be saved' )
				ENDWHILE
				
				IF STRTRIM(filename,2) NE '' THEN BEGIN
					WSET, tmpw
					
					
extra_struct	= confirm_struct.extra_struct
extra_struct	= CREATE_STRUCT('CONTINENTS',1,'GRID',1,extra_struct)
CALL_PROCEDURE, 'MAP_SET',confirm_struct.cen_lat,confirm_struct.cen_lon,confirm_struct.cen_rot, _Extra = extra_struct
;;;ckt,aug2004					success	= EXECUTE(confirm_struct.map_set_str+',/CONTINENTS,/GRID')
					
					
					img	= TVRD()
					WRITE_TIFF, filename, REVERSE(img,2), 1
				ENDIF
			ENDIF
			
			IF !D.WINDOW EQ tmpw AND !D.WINDOW NE last_win THEN WDELETE, !D.WINDOW
			WSET, last_win
			
			WIDGET_CONTROL, event.top, /DESTROY
			(*ptr).cancel_pressed	= 0
			(*ptr).info_ptr		= PTR_NEW( confirm_struct, /NO_COPY )
			END
		ELSE:
	ENDCASE
	
END

PRO cw_proj_info_kill, main_base
END

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@              @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ cw_proj_info @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@              @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION cw_proj_info,								$
			top_base,						$
			LIMIT = limit,						$
			CEN_LAT = cen_lat,					$
			CEN_LON = cen_lon,					$
			CEN_ROT = cen_rot,					$
			SCALE = scl,						$
			PROJECTION_NAME = pname,				$
			RESOLUTION = in_res,					$
			WINDOW_DIMS = wdims
	
	default_clat		= 0.0
	default_clon		= 0.0
	default_crot		= 0.0
	default_min_lat		= -90.0
	default_min_lon		= -179.99
	default_max_lat		= 90.0
	default_max_lon		= 179.99
	default_proj_name	= 'CYLINDRICAL'
	default_scale		= 1.0E6
	
	default_res		= 1.0
	default_width		= 500
	default_height		= 500
	
	IF KEYWORD_SET(cen_lat) THEN default_clat = cen_lat
	IF KEYWORD_SET(cen_lon) THEN default_clon = cen_lon
	IF KEYWORD_SET(cen_rot) THEN default_crot = cen_rot
	scl_set	= 0
	lim_set	= 0
	IF KEYWORD_SET(scl) THEN BEGIN
		scl		= 1
		default_scale	= scl
	ENDIF
	IF KEYWORD_SET(limit) THEN BEGIN
		lim_set		= 1
		default_min_lat	= limit[0]
		default_min_lon	= limit[1]
		default_max_lat	= limit[2]
		default_max_lon	= limit[3]
	ENDIF
	
	res_set	= 0
	win_set	= 0
	IF KEYWORD_SET(in_res) THEN BEGIN
		res_set		= 1
		default_res	= in_res
	ENDIF
	
	IF KEYWORD_SET(wdims) AND NOT res_set THEN BEGIN
		win_set		= 1
		default_width	= in_res[0]
		default_height	= in_res[1]
	ENDIF
	
	b		= WIDGET_BASE(						$
					/COLUMN,				$
					GROUP_LEADER = top_base,		$
					TITLE = 'Data Reprojection Interface',	$
					EVENT_PRO = 'cw_proj_info_eh',		$
					KILL_NOTIFY = 'cw_proj_info_kill',	$
					TLB_FRAME_ATTR = 9,			$
					/MODAL,					$
					/BASE_ALIGN_CENTER )
					
	font_type	= GetCorrectFont( 'courier2bold' )
	main_drop_txt	= [ 'PROJECTION CENTER', 'PROJECTION EXTENT', 'PROJECTION TYPE', 'PROJECTION WINDOW', 'OTHER SETTINGS' ]
	main_droplist	= WIDGET_DROPLIST( b, TITLE = 'PROJECTION PARAMETERS:', FONT = font_type, $
						UVALUE = 'main_drop', VALUE = main_drop_txt )
	main_bboard	= WIDGET_BASE( b )
	main_base_arr	= LONARR(5)
	
	main_base_arr[0]= WIDGET_BASE(				$
					main_bboard,		$
					/COLUMN,		$
					/BASE_ALIGN_LEFT,	$
					/FRAME,			$
					MAP = 0,		$
					/ALIGN_LEFT )
	lab1base	= WIDGET_BASE(				$
					main_base_arr[0],	$
					/ROW,			$
					/BASE_ALIGN_CENTER,	$
					/ALIGN_CENTER )
	lab1		= WIDGET_LABEL(				$
					lab1base,		$
					FONT = font_type,	$
					/ALIGN_CENTER,		$
					VALUE = 'PROJECTION CENTER' )
	center_help_btn	= WIDGET_BUTTON(			$
					lab1base,		$
					FONT = font_type,	$
					VALUE = '?',		$
					UVALUE = 'center_help' )
	sub_b1		= WIDGET_BASE(				$
					main_base_arr[0],	$
					/ROW,			$
					/BASE_ALIGN_LEFT,	$
					/ALIGN_LEFT )
	clat_title	= 'Center Latitude (deg): '
	clat_cwf	= CW_FIELD(				$
					sub_b1,			$
					/FLOATING,		$
					VALUE = default_clat,		$
					TITLE = clat_title,	$
					FONT = font_type,	$
					UVALUE = 'center_lat' )
	sub_b2		= WIDGET_BASE(				$
					main_base_arr[0],	$
					/ROW,			$
					/BASE_ALIGN_LEFT,	$
					/ALIGN_LEFT )
	clon_title	= 'Center Longitude (deg):'
	clon_cwf	= CW_FIELD(				$
					sub_b2,			$
					/FLOATING,		$
					VALUE = default_clon,		$
					FONT = font_type,	$
					TITLE = clon_title,	$
					UVALUE = 'center_lon' )
	sub_b3		= WIDGET_BASE(				$
					main_base_arr[0],	$
					/ROW,			$
					/BASE_ALIGN_LEFT,	$
					/ALIGN_LEFT )
	crot_title	= 'Rotation (deg):        '
	crot_cwf	= CW_FIELD(				$
					sub_b3,			$
					/FLOATING,		$
					VALUE = default_crot,		$
					FONT = font_type,	$
					TITLE = crot_title,	$
					UVALUE = 'center_rot' )
	cdef_btn	= WIDGET_BUTTON(			$
					main_base_arr[0],	$
					VALUE = 'use defaults',	$
					FONT = font_type,	$
					/ALIGN_CENTER,		$
					UVALUE = 'usecenterdefaults' )
					
	center_defaults	= [ default_clat, default_clon, default_crot ]
				
				
	main_base_arr[1]= WIDGET_BASE(				$
					main_bboard,		$
					/COLUMN,		$
					/BASE_ALIGN_LEFT,	$
					/FRAME,			$
					MAP = 0,		$
					/ALIGN_LEFT )
	lab2base	= WIDGET_BASE(				$
					main_base_arr[1],	$
					/ROW,			$
					/BASE_ALIGN_CENTER,	$
					/ALIGN_CENTER )
	lab2		= WIDGET_LABEL(				$
					lab2base,		$
					FONT = font_type,	$
					/ALIGN_CENTER,		$
					VALUE = 'PROJECTION EXTENT' )
	extent_help_btn	= WIDGET_BUTTON(			$
					lab2base,		$
					FONT = font_type,	$
					VALUE = '?',		$
					UVALUE = 'extent_help' )
	lim_txt		= 'Specify Extents By:'
	sub_b1		= WIDGET_BASE(				$
					main_base_arr[1],	$
					/ROW,			$
					/BASE_ALIGN_CENTER,	$
					/ALIGN_CENTER )
	sub_b01		= WIDGET_BASE(				$
					sub_b1,			$
					/ROW,			$
					/BASE_ALIGN_CENTER )
	lim_lab		= WIDGET_LABEL(				$
					sub_b01,		$
					FONT = font_type,	$
					/ALIGN_CENTER,		$
					VALUE = lim_txt )
	sub_b11		= WIDGET_BASE(				$
					sub_b1,			$
					/ROW,			$
					/BASE_ALIGN_CENTER,	$
					/EXCLUSIVE )
	scale_btn	= WIDGET_BUTTON(			$
					sub_b11,		$
					FONT = font_type,	$
					UVALUE = 'scale',	$
					VALUE = 'Scale' )
	limit_btn	= WIDGET_BUTTON(			$
					sub_b11,		$
					FONT = font_type,	$
					UVALUE = 'limit',	$
					VALUE = 'Limits' )
	bboard		= WIDGET_BASE( main_base_arr[1] )
	scale_lim_barr	= LONARR(2)
	scale_lim_barr[0]					$
			= WIDGET_BASE(				$
					bboard,			$
					/COLUMN,			$
					/ALIGN_LEFT,		$
					/BASE_ALIGN_LEFT,	$
					MAP = scl_set )
	scale_cwf	= CW_FIELD(				$
				scale_lim_barr[0],		$
				TITLE = 'Scale = 1:             ',		$
				FONT = font_type,		$
				VALUE = default_scale,			$
				/FLOATING )
	scaledef_btn	= WIDGET_BUTTON(			$
					scale_lim_barr[0],	$
					VALUE = 'use default',	$
					FONT = font_type,	$
					/ALIGN_CENTER,		$
					UVALUE = 'usescaledefault' )
	scale_lim_barr[1]					$
			= WIDGET_BASE(				$
					bboard,			$
					/COLUMN,		$
					MAP = (lim_set OR (NOT lim_set AND NOT scl_set)),		$
					/ALIGN_LEFT,		$
					/BASE_ALIGN_LEFT )
					
	min_lon_lbl	= CW_FIELD(						$
				scale_lim_barr[1],				$
				TITLE = 'Left Longitude (deg):  ',		$
				FONT = font_type,				$
				VALUE = default_min_lon,			$
				/FLOATING,					$
				UVALUE = 'min_lon' )
	max_lon_lbl	= CW_FIELD(						$
				scale_lim_barr[1],				$
				TITLE = 'Right Longitude (deg): ',		$
				FONT = font_type,				$
				VALUE = default_max_lon,			$
				/FLOATING,					$
				UVALUE = 'max_lon' )
	min_lat_lbl	= CW_FIELD(						$
				scale_lim_barr[1],				$
				TITLE = 'Lower Latitude (deg):  ',		$
				FONT = font_type,				$
				VALUE = default_min_lat,			$
				/FLOATING,					$
				UVALUE = 'min_lat' )
	max_lat_lbl	= CW_FIELD(						$
				scale_lim_barr[1],				$
				TITLE = 'Upper Latitude (deg):  ',		$
				FONT = font_type,				$
				VALUE = default_max_lat,			$
				/FLOATING,					$
				UVALUE = 'max_lat' )
					
	limitdef_btn	= WIDGET_BUTTON(			$
					scale_lim_barr[1],	$
					VALUE = 'use defaults',	$
					FONT = font_type,	$
					/ALIGN_CENTER,		$
					UVALUE = 'uselimitdefaults' )
					
	limit_defaults	= [default_min_lat, default_min_lon, default_max_lat, default_max_lon ]
	
	main_base_arr[2]= WIDGET_BASE(				$
					main_bboard,		$
					/COLUMN,		$
					/BASE_ALIGN_LEFT,	$
					/FRAME,			$
					/ALIGN_LEFT )
	lab3base	= WIDGET_BASE(				$
					main_base_arr[2],	$
					/ROW,			$
					/BASE_ALIGN_CENTER,	$
					/ALIGN_CENTER )
	lab3		= WIDGET_LABEL(				$
					lab3base,		$
					FONT = font_type,	$
					/ALIGN_CENTER,		$
					VALUE = 'PROJECTION TYPE' )
	proj_txt		= 'Projection Name:'
	sub_b1		= WIDGET_BASE(				$
					main_base_arr[2],	$
					/ROW,			$
					/BASE_ALIGN_LEFT,	$
					/ALIGN_LEFT )
	PROJECTIONS	= [					$
				'Aitoff',			$
				'Albers',			$
				'Azimuthal',			$
				'Conic',			$
				'Cylindrical',			$
				'Goodes Homolosine',		$
				'Gnomic',			$
				'Hammer',			$
				'Lambert',			$
				'Mercator',			$
				'Miller',			$
				'Mollweide',			$
				'Orthographic',			$
				'Robinson',			$
				'Satellite',			$
				'Sinusoidal',			$
				'Stereographic',		$
				'Transverse Mercator' ]

	proj_droplist	= WIDGET_DROPLIST(			$
					sub_b1,			$
					FONT = font_type,	$
					/ALIGN_LEFT,		$
					TITLE = proj_txt,	$
					UVALUE = 'proj_dlist',	$
					VALUE = PROJECTIONS )
					
	type_help_btn	= WIDGET_BUTTON(			$
					sub_b1,			$
					FONT = font_type,	$
					VALUE = '?',		$
					UVALUE = 'type_help' )
	projdef_btn	= WIDGET_BUTTON(			$
					main_base_arr[2],	$
					FONT = font_type,	$
					/ALIGN_CENTER,		$
					VALUE = 'use default',		$
					UVALUE = 'useprojdefault' )
	proj_base_arr	= LONARR( N_ELEMENTS(PROJECTIONS) )
	;
	;
	;(*,0,*) = CENTRAL_AZIMUTH
	;(*,1,*) = ELLIPSOID
	;(*,2,*) = SAT_P
	;(*,3,*) = STANDARD_PARALLELS
	;
	;
	;(*,*,0) = are projection parameters relevant to current projection?
	;(*,*,1) = number of entries for this parameter that are used 
	;(*,*,2:4) entries for this parameter
	; CENTRAL_AZIMUTH
	; ELLIPSOID
	; SAT_P
	; STANDARD_PARALLELS
	;
	RELEVANCE_IDX		= 0
	N_ENTRIES_USED_IDX	= 1
	ENTRY1_IDX		= 2
	ENTRY2_IDX		= 3
	ENTRY3_IDX		= 4
	
	CENTRAL_AZIMUTH_IDX	= 0
	ELLIPSOID_IDX		= 1
	SAT_P_IDX		= 2
	STANDARD_PARALLELS_IDX	= 3
	
	AITOFF_IDX		= 0
	ALBERS_IDX 		= 1
	AZIMUTHAL_IDX		= 2
	CONIC_IDX		= 3
	CYLINDRICAL_IDX		= 4
	GOODESHOMOLOSINE_IDX	= 5
	GNOMIC_IDX		= 6
	HAMMER_IDX		= 7
	LAMBERT_IDX		= 8
	MERCATOR_IDX		= 9
	MILLER_CYLINDRICAL_IDX	= 10
	MOLLWEIDE_IDX		= 11
	ORTHOGRAPHIC_IDX	= 12
	ROBINSON_IDX		= 13
	SATELLITE_IDX		= 14
	SINUSOIDAL_IDX		= 15
	STEREOGRAPHIC_IDX	= 16
	TRANSVER_MERCATOR_IDX	= 17
	
	USE_LONLATROT_IDX	= 0
	LON_IDX			= 1
	LAT_IDX			= 2
	ROT_IDX			= 3
	
	param_array			= FLTARR( N_ELEMENTS(PROJECTIONS),4, 5 )
	set_center_lon_lat_rot		= FLTARR( N_ELEMENTS(PROJECTIONS), 4 )
	
	param_array[ ALBERS_IDX, STANDARD_PARALLELS_IDX, RELEVANCE_IDX ]		= 1.0
	
	param_array[ CONIC_IDX, STANDARD_PARALLELS_IDX, RELEVANCE_IDX ]			= 1.0
	param_array[ CONIC_IDX, ELLIPSOID_IDX, RELEVANCE_IDX ]				= 1.0
	param_array[ CONIC_IDX, ELLIPSOID_IDX, N_ENTRIES_USED_IDX ]			= 3.0
	param_array[ CONIC_IDX, ELLIPSOID_IDX, ENTRY1_IDX ]				= 6378206.4
	param_array[ CONIC_IDX, ELLIPSOID_IDX, ENTRY2_IDX ]				= 0.00676866
	param_array[ CONIC_IDX, ELLIPSOID_IDX, ENTRY3_IDX ]				= 0.9996
	
	param_array[ CYLINDRICAL_IDX, CENTRAL_AZIMUTH_IDX, RELEVANCE_IDX ]		= 1.0
	param_array[ CYLINDRICAL_IDX, CENTRAL_AZIMUTH_IDX, N_ENTRIES_USED_IDX ]		= 1.0
	param_array[ CYLINDRICAL_IDX, CENTRAL_AZIMUTH_IDX, ENTRY1_IDX ]			= 0.0
	
	set_center_lon_lat_rot[ GOODESHOMOLOSINE_IDX, USE_LONLATROT_IDX ]		= 1.0
	set_center_lon_lat_rot[ GOODESHOMOLOSINE_IDX, LON_IDX ]				= 0.0
	set_center_lon_lat_rot[ GOODESHOMOLOSINE_IDX, LAT_IDX ]				= 0.0
	set_center_lon_lat_rot[ GOODESHOMOLOSINE_IDX, ROT_IDX ]				= 0.0
		
	param_array[ MERCATOR_IDX, CENTRAL_AZIMUTH_IDX, RELEVANCE_IDX ]			= 1.0
	param_array[ MERCATOR_IDX, CENTRAL_AZIMUTH_IDX, N_ENTRIES_USED_IDX ]		= 1.0
	param_array[ MERCATOR_IDX, CENTRAL_AZIMUTH_IDX, ENTRY1_IDX ]			= 0.0
	
	param_array[ MILLER_CYLINDRICAL_IDX, CENTRAL_AZIMUTH_IDX, RELEVANCE_IDX ]	= 1.0
	param_array[ MILLER_CYLINDRICAL_IDX, CENTRAL_AZIMUTH_IDX, N_ENTRIES_USED_IDX ]	= 1.0
	param_array[ MILLER_CYLINDRICAL_IDX, CENTRAL_AZIMUTH_IDX, ENTRY1_IDX ]		= 0.0
	
	param_array[ MOLLWEIDE_IDX, CENTRAL_AZIMUTH_IDX, RELEVANCE_IDX ]		= 1.0
	param_array[ MOLLWEIDE_IDX, CENTRAL_AZIMUTH_IDX, N_ENTRIES_USED_IDX ]		= 1.0
	param_array[ MOLLWEIDE_IDX, CENTRAL_AZIMUTH_IDX, ENTRY1_IDX ]			= 0.0
	
	param_array[ SATELLITE_IDX, SAT_P_IDX, RELEVANCE_IDX ]				= 1.0
	param_array[ SATELLITE_IDX, SAT_P_IDX, N_ENTRIES_USED_IDX ]			= 3.0
	param_array[ SATELLITE_IDX, SAT_P_IDX, ENTRY1_IDX ]				= 2.0
	param_array[ SATELLITE_IDX, SAT_P_IDX, ENTRY2_IDX ]				= 0.0
	param_array[ SATELLITE_IDX, SAT_P_IDX, ENTRY3_IDX ]				= 0.0
	
	param_array[ SINUSOIDAL_IDX, CENTRAL_AZIMUTH_IDX, RELEVANCE_IDX ]		= 1.0
	param_array[ SINUSOIDAL_IDX, CENTRAL_AZIMUTH_IDX, N_ENTRIES_USED_IDX ]		= 1.0
	param_array[ SINUSOIDAL_IDX, CENTRAL_AZIMUTH_IDX, ENTRY1_IDX ]			= 0.0
	
	param_array[ TRANSVER_MERCATOR_IDX, ELLIPSOID_IDX, RELEVANCE_IDX ]		= 1.0
	param_array[ TRANSVER_MERCATOR_IDX, ELLIPSOID_IDX, N_ENTRIES_USED_IDX ]		= 3.0
	param_array[ TRANSVER_MERCATOR_IDX, ELLIPSOID_IDX, ENTRY1_IDX ]			= 6378206.4
	param_array[ TRANSVER_MERCATOR_IDX, ELLIPSOID_IDX, ENTRY2_IDX ]			= 0.00676866
	param_array[ TRANSVER_MERCATOR_IDX, ELLIPSOID_IDX, ENTRY3_IDX ]			= 0.9996
		
	proj_base_arr	= WIDGET_BASE( main_base_arr[2] )
	pb		= LONARR(N_ELEMENTS(PROJECTIONS))
	az_base		= pb
	ellip_base1	= pb
	satp_base1	= pb
	parallel_base1	= pb
	
	cen_az_default	= 0.0
	ellip_default	= [6378206.4,0.00676866,0.9996]
	satp_default	= [2.0,0.0,0.0]
	parallel_default= [0.0,0.0]
	
	idx	= WHERE( STRUPCASE(PROJECTIONS) EQ STRUPCASE(default_proj_name), cnt )
	IF cnt GT 0 THEN proj_idx	= idx[0] ELSE proj_idx	= 0
	
	FOR i = 0, N_ELEMENTS(PROJECTIONS) - 1 DO BEGIN
		pb[i]			= WIDGET_BASE(			$
						proj_base_arr,		$
						/COLUMN,		$
						MAP = (i EQ proj_idx),		$
						/BASE_ALIGN_LEFT )
		IF param_array[ i, CENTRAL_AZIMUTH_IDX, RELEVANCE_IDX ] THEN BEGIN
;print,'creating base for i = ', i
			az_base[i]	= WIDGET_BASE(pb[i],/COLUMN,/ALIGN_LEFT,/FRAME,/BASE_ALIGN_LEFT)
			cen_az_cwf = CW_FIELD( az_base[i], UVALUE = 'central_az_'+STRTRIM(i,2),	$
				VALUE = param_array[ i, CENTRAL_AZIMUTH_IDX, ENTRY1_IDX ], /FLOATING, 	$
				TITLE = 'Central Azimuth (deg):', FONT = font_type )
				
			cenazdef_btn	= WIDGET_BUTTON(			$
					az_base[i],				$
					VALUE = 'use default',			$
					FONT = font_type,			$
					/ALIGN_CENTER,				$
					UVALUE = 'usecenazdefaults' )
		ENDIF
		
		ELLIPSOID_MODEL_NAMES = [ 'Clarke 1866' ]
		
		
		IF param_array[ i, ELLIPSOID_IDX, RELEVANCE_IDX ] THEN BEGIN
			ellip_base1[i]	= WIDGET_BASE(pb[i],/COLUMN,/ALIGN_LEFT,/FRAME,/BASE_ALIGN_LEFT)
			overlap_wd	= WIDGET_DROPLIST(					$
						ellip_base1[i], 				$
						UVALUE = 'ellipsoid_model',				$
						FONT = font_type,	$
						VALUE = ELLIPSOID_MODEL_NAMES,				$
						TITLE = 'Ellipsoid Model:' )
						
			eqr_lbl = WIDGET_LABEL( ellip_base1[i], UVALUE = 'ellipsoid_rad_'+STRTRIM(i,2),	$
				VALUE = 'Equatorial Radius (m):         '+STRTRIM(param_array[ i, ELLIPSOID_IDX, ENTRY1_IDX ],2), FONT = font_type )
				
			ecc_lbl = WIDGET_LABEL( ellip_base1[i], UVALUE = 'ellipsoid_rad_'+STRTRIM(i,2),	$
				VALUE = 'Eccentricity Squared:          '+STRTRIM(param_array[ i, ELLIPSOID_IDX, ENTRY2_IDX ],2), FONT = font_type )

			scl_lbl = WIDGET_LABEL( ellip_base1[i], UVALUE = 'ellipsoid_rad_'+STRTRIM(i,2),	$
				VALUE = 'Center Meridian Scale:         '+STRTRIM(param_array[ i, ELLIPSOID_IDX, ENTRY3_IDX ],2), FONT = font_type )

			ellipdef_btn	= WIDGET_BUTTON(			$
					ellip_base1[i],				$
					VALUE = 'use defaults',			$
					FONT = font_type,			$
					/ALIGN_CENTER,				$
					UVALUE = 'useellipdefaults' )
		ENDIF
		
		IF param_array[ i, SAT_P_IDX, RELEVANCE_IDX ] THEN BEGIN
;print,'creating base for i = ', i
			satp_base1[i]	= WIDGET_BASE(pb[i],/COLUMN,/ALIGN_LEFT,/FRAME,/BASE_ALIGN_LEFT)
			f_cwf = CW_FIELD( satp_base1[i], UVALUE = 'satp_f_'+STRTRIM(i,2),	$
				VALUE = param_array[ i, SAT_P_IDX, ENTRY1_IDX ], /FLOATING, 	$
				TITLE = 'F (radii):           ', FONT = font_type )
			omega_cwf = CW_FIELD( satp_base1[i], UVALUE = 'satp_omega_'+STRTRIM(i,2),	$
				VALUE = param_array[ i, SAT_P_IDX, ENTRY2_IDX ], /FLOATING, 	$
				TITLE = 'Omega (deg):         ', FONT = font_type )
			gamma_cwf = CW_FIELD( satp_base1[i], UVALUE = 'satp_gamma_'+STRTRIM(i,2),	$
				VALUE = param_array[ i, SAT_P_IDX, ENTRY3_IDX ], /FLOATING, 	$
				TITLE = 'Gamma (deg):         ', FONT = font_type )
			satpdef_btn	= WIDGET_BUTTON(			$
					satp_base1[i],				$
					VALUE = 'use defaults',			$
					FONT = font_type,			$
					/ALIGN_CENTER,				$
					UVALUE = 'usesatpdefaults' )
		ENDIF
		
		IF param_array[ i, STANDARD_PARALLELS_IDX, RELEVANCE_IDX ] THEN BEGIN
			parallel_base1[i]	= WIDGET_BASE(pb[i],/COLUMN,/ALIGN_LEFT,/FRAME,/BASE_ALIGN_LEFT)
			parallel_drop	= WIDGET_DROPLIST( parallel_base1[i], TITLE = 'Standard Parallels To Use:', FONT = font_type, $
						UVALUE = 'parallel_drop_'+STRTRIM(i,2), VALUE = SINDGEN(3) )
			parallel1_cwf = CW_FIELD( parallel_base1[i], UVALUE = 'parallel1_'+STRTRIM(i,2),	$
				VALUE = param_array[ i, STANDARD_PARALLELS_IDX, ENTRY1_IDX ], /FLOATING, 	$
				TITLE = 'Parallel 1 (deg):', FONT = font_type )
			parallel2_cwf = CW_FIELD( parallel_base1[i], UVALUE = 'parallel2_'+STRTRIM(i,2),	$
				VALUE = param_array[ i, STANDARD_PARALLELS_IDX, ENTRY2_IDX ], /FLOATING, 	$
				TITLE = 'Parallel 2 (deg):', FONT = font_type )
			WIDGET_CONTROL, parallel1_cwf, SENSITIVE = 0
			WIDGET_CONTROL, parallel2_cwf, SENSITIVE = 0
			pardef_btn	= WIDGET_BUTTON(			$
					parallel_base1[i],			$
					VALUE = 'use defaults',			$
					FONT = font_type,			$
					/ALIGN_CENTER,				$
					UVALUE = 'usepardefaults' )
		ENDIF
	ENDFOR
	
	;(*,*,0) = are projection parameters relevant to current projection?
	;(*,*,1) = number of entries for this parameter that are used 
	;CENTRAL_AZIMUTH
	;ELLIPSOID
	;SAT_P
	;STANDARD_PARALLELS
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	main_base_arr[3]= WIDGET_BASE(				$
					main_bboard,		$
					/COLUMN,		$
					/BASE_ALIGN_LEFT,	$
					/FRAME,			$
					MAP = 0,		$
					/ALIGN_LEFT )
	lab2base	= WIDGET_BASE(				$
					main_base_arr[3],	$
					/ROW,			$
					/BASE_ALIGN_CENTER,	$
					/ALIGN_CENTER )
	lab2		= WIDGET_LABEL(				$
					lab2base,		$
					FONT = font_type,	$
					/ALIGN_CENTER,		$
					VALUE = 'PROJECTION WINDOW' )
	extent_help_btn	= WIDGET_BUTTON(			$
					lab2base,		$
					FONT = font_type,	$
					VALUE = '?',		$
					UVALUE = 'window_help' )
	dim_txt		= 'Specify By:'
	sub_b1		= WIDGET_BASE(				$
					main_base_arr[3],	$
					/ROW,			$
					/BASE_ALIGN_CENTER,	$
					/ALIGN_CENTER )
	sub_b01		= WIDGET_BASE(				$
					sub_b1,			$
					/ROW,			$
					/BASE_ALIGN_CENTER )
	lim_lab		= WIDGET_LABEL(				$
					sub_b01,		$
					FONT = font_type,	$
					/ALIGN_CENTER,		$
					VALUE = dim_txt )
	sub_b11		= WIDGET_BASE(				$
					sub_b1,			$
					/ROW,			$
					/BASE_ALIGN_CENTER,	$
					/EXCLUSIVE )
	dim_btn	= WIDGET_BUTTON(			$
					sub_b11,		$
					FONT = font_type,	$
					UVALUE = 'windim',	$
					VALUE = 'Width/Height' )
	res_btn	= WIDGET_BUTTON(			$
					sub_b11,		$
					FONT = font_type,	$
					UVALUE = 'datares',	$
					VALUE = 'Resolution' )
	bboard		= WIDGET_BASE( main_base_arr[3] )
	res_windim_barr	= LONARR(2)
	res_windim_barr[1]					$
			= WIDGET_BASE(				$
					bboard,			$
					/COLUMN,			$
					/ALIGN_LEFT,		$
					/BASE_ALIGN_LEFT,	$
					MAP = (res_set OR (NOT res_set AND NOT win_set) ) )
	res_cwf	= CW_FIELD(				$
				res_windim_barr[1],		$
				TITLE = 'Output Resolution (km):',		$
				FONT = font_type,		$
				VALUE = default_res,			$
				/FLOATING )
	resdef_btn	= WIDGET_BUTTON(			$
					res_windim_barr[1],	$
					VALUE = 'use default',	$
					FONT = font_type,	$
					/ALIGN_CENTER,		$
					UVALUE = 'useresdefault' )
	res_windim_barr[0]					$
			= WIDGET_BASE(				$
					bboard,			$
					/COLUMN,		$
					MAP = win_set,		$
					/ALIGN_LEFT,		$
					/BASE_ALIGN_LEFT )
					
	width_lbl	= CW_FIELD(						$
				res_windim_barr[0],				$
				TITLE = 'Window Width:          ',		$
				FONT = font_type,				$
				VALUE = default_width,				$
				/INTEGER,					$
				UVALUE = 'width' )
	
	
	
	height_lbl	= CW_FIELD(						$
				res_windim_barr[0],				$
				TITLE = 'Window Height:         ',		$
				FONT = font_type,				$
				VALUE = default_height,				$
				/INTEGER,					$
				UVALUE = 'height' )
	
	windef_btn	= WIDGET_BUTTON(			$
					res_windim_barr[0],	$
					VALUE = 'use defaults',	$
					FONT = font_type,	$
					/ALIGN_CENTER,		$
					UVALUE = 'usewindimdefaults' )
	

	wh_defaults	= [ default_width, default_height ]
	res_default	= default_res
	
	
	
	
	
	main_base_arr[4]= WIDGET_BASE(				$
					main_bboard,		$
					/COLUMN,		$
					/BASE_ALIGN_LEFT,	$
					/FRAME,			$
					MAP = 0,		$
					/ALIGN_LEFT )
	
	
	
	lab2base	= WIDGET_BASE(				$
					main_base_arr[4],	$
					/ROW,			$
					/BASE_ALIGN_CENTER,	$
					/ALIGN_CENTER )
	lab2		= WIDGET_LABEL(				$
					lab2base,		$
					FONT = font_type,	$
					/ALIGN_CENTER,		$
					VALUE = 'OTHER SETTINGS' )
	other_help_btn	= WIDGET_BUTTON(			$
					lab2base,		$
					FONT = font_type,	$
					VALUE = '?',		$
					UVALUE = 'other_help' )
	OVERLAP		= [							$
				'first value encountered',				$
				'last value encountered',				$
				'highest DN value',				$
				'lowest DN value',				$
				'average' ]
				
	sub_b4		= WIDGET_BASE(				$
					main_base_arr[4],	$
					/COLUMN,			$
					/FRAME,			$
					/BASE_ALIGN_CENTER,	$
					/ALIGN_CENTER )
	sub_b4a_lbl	= WIDGET_LABEL(						$
				sub_b4, 					$
				FONT = font_type,	$
				VALUE = ' Resolve Multiple Image Overlap By Using: ' )
	
	overlap_wd	= WIDGET_DROPLIST(					$
				sub_b4, 					$
				UVALUE = 'overlap',				$
				FONT = font_type,	$
				VALUE = OVERLAP,				$
				TITLE = '' )
				
	sub_base4	= WIDGET_BASE(						$
				main_base_arr[4], 				$
				/ROW,					$
				/BASE_ALIGN_CENTER )
	sub_base4a	= WIDGET_BASE(						$
				sub_base4, 				$
				/ROW,					$
				/BASE_ALIGN_CENTER,				$
				/NONEXCLUSIVE )
				
	fill_seam_btn	= WIDGET_BUTTON(					$
				sub_base4a,					$
				FONT = font_type,	$
				VALUE = 'Apply Seam Filling Algorithm',		$
				UVALUE = 'fill_seams' )
	sub_base4b	= WIDGET_BASE(						$
				sub_base4, 				$
				/ROW,					$
				/BASE_ALIGN_CENTER )
	fill_seam_options_btn	= WIDGET_BUTTON(					$
				sub_base4b,					$
				FONT = font_type,	$
				VALUE = 'Options',		$
				SENSITIVE = 0,	$
				UVALUE = 'fill_seam_options' )
	
				
	WIDGET_CONTROL, fill_seam_btn, SET_BUTTON = 0
	
	sub_base5	= WIDGET_BASE(						$
				main_base_arr[4], 				$
				/ROW,					$
				/BASE_ALIGN_CENTER )
	sub_base5a	= WIDGET_BASE(						$
				sub_base5, 				$
				/ROW,					$
				/BASE_ALIGN_CENTER,				$
				/NONEXCLUSIVE )
				
	isotropic_btn	= WIDGET_BUTTON(					$
				sub_base5a,					$
				FONT = font_type,	$
				VALUE = 'Use Isotropic Scaling',		$
				UVALUE = 'isotropic' )
	
	sub_base6	= WIDGET_BASE(						$
				main_base_arr[4], 				$
				/ROW,					$
				/BASE_ALIGN_CENTER )
	sub_base6a	= WIDGET_BASE(						$
				sub_base6, 				$
				/ROW,					$
				/BASE_ALIGN_CENTER,				$
				/NONEXCLUSIVE )
				
	overlay_btn	= WIDGET_BUTTON(					$
				sub_base6a,					$
				FONT = font_type,				$
				VALUE = 'Create TIFF Overlay Image',			$
				UVALUE = 'overlay' )
	
	
	base4		= WIDGET_BASE(				$
					b,			$
					/COLUMN,		$
					/BASE_ALIGN_CENTER,	$
					/FRAME,			$
					/ALIGN_CENTER )
	preview_btn	= WIDGET_BUTTON(					$
					base4,					$
					VALUE = '     Preview Projection     ',$
					FONT = font_type,			$
					UVALUE = 'preview' )
	reproject_btn	= WIDGET_BUTTON(					$
					base4,					$
					VALUE = '       Do Reprojection      ',$
					FONT = font_type,			$
					UVALUE = 'reproject' )
	save_btn	= WIDGET_BUTTON(					$
					base4,					$
					VALUE = '   Save Parameters To File  ',$
					FONT = font_type,			$
					UVALUE = 'save' )
	save_btn	= WIDGET_BUTTON(					$
					base4,					$
					VALUE = 'Restore Parameters From File',	$
					FONT = font_type,			$
					UVALUE = 'restore' )
	dismiss_btn	= WIDGET_BUTTON(					$
					base4,					$
					VALUE = '           Cancel           ',	$
					FONT = font_type,			$
					UVALUE = 'dismiss' )
	WIDGET_CONTROL, b, /REALIZE
	
	WIDGET_CONTROL, main_droplist, SET_DROPLIST_SELECT = 2
	
	WIDGET_CONTROL, proj_droplist, SET_DROPLIST_SELECT = proj_idx
	
	WIDGET_CONTROL, limit_btn, SET_BUTTON = 1
	
	WIDGET_CONTROL, res_btn, SET_BUTTON = 1
	
	ptr	= PTR_NEW( {									$
				projection_base_stack	: pb,					$
				scale_btn		: scale_btn,				$
				limit_btn		: limit_btn,				$
				az_base			: az_base,				$
				ellip_base1		: ellip_base1,				$
				satp_base1		: satp_base1,				$
				parallel_base1		: parallel_base1,			$
				param_array		: param_array,				$
				set_center_lon_lat_rot	: set_center_lon_lat_rot,		$
				proj_droplist		: proj_droplist,			$
				PROJECTIONS		: PROJECTIONS,				$
				
				RELEVANCE_IDX		: RELEVANCE_IDX,			$
				N_ENTRIES_USED_IDX	: N_ENTRIES_USED_IDX,			$
				ENTRY1_IDX		: ENTRY1_IDX,				$
				ENTRY2_IDX		: ENTRY2_IDX,				$
				ENTRY3_IDX		: ENTRY3_IDX,				$
	
				CENTRAL_AZIMUTH_IDX	: CENTRAL_AZIMUTH_IDX,			$
				ELLIPSOID_IDX		: ELLIPSOID_IDX,			$
				SAT_P_IDX		: SAT_P_IDX,				$
				STANDARD_PARALLELS_IDX	: STANDARD_PARALLELS_IDX,		$
	
				AITOFF_IDX		: AITOFF_IDX,				$
				ALBERS_IDX		: ALBERS_IDX,				$
				AZIMUTHAL_IDX		: AZIMUTHAL_IDX,			$
				CONIC_IDX		: CONIC_IDX,				$
				CYLINDRICAL_IDX		: CYLINDRICAL_IDX,			$
				GOODESHOMOLOSINE_IDX	: GOODESHOMOLOSINE_IDX,			$
				GNOMIC_IDX		: GNOMIC_IDX,				$
				HAMMER_IDX		: HAMMER_IDX,				$
				LAMBERT_IDX		: LAMBERT_IDX,				$
				MERCATOR_IDX		: MERCATOR_IDX,				$
				MILLER_CYLINDRICAL_IDX	: MILLER_CYLINDRICAL_IDX,		$
				MOLLWEIDE_IDX		: MOLLWEIDE_IDX,			$
				ORTHOGRAPHIC_IDX	: ORTHOGRAPHIC_IDX,			$
				ROBINSON_IDX		: ROBINSON_IDX,				$
				SATELLITE_IDX		: SATELLITE_IDX,			$
				SINUSOIDAL_IDX		: SINUSOIDAL_IDX,			$
				STEREOGRAPHIC_IDX	: STEREOGRAPHIC_IDX,			$
				TRANSVER_MERCATOR_IDX	: TRANSVER_MERCATOR_IDX,		$
	
				USE_LONLATROT_IDX	: USE_LONLATROT_IDX,			$
				LON_IDX			: LON_IDX,				$
				LAT_IDX			: LAT_IDX,				$
				ROT_IDX			: ROT_IDX,				$
				
				cen_az_default		: cen_az_default,			$
				ellip_default		: ellip_default,			$
				satp_default		: satp_default,				$
				parallel_default	: parallel_default,			$
				limit_defaults		: limit_defaults,			$
				default_scale		: default_scale,			$
				default_proj_name	: default_proj_name,			$
				center_defaults		: center_defaults,			$
				res_btn			: res_btn,				$
				dim_btn			: dim_btn,				$
				wh_defaults		: wh_defaults,				$
				res_default		: res_default,				$
				res_cwf			: res_cwf,				$
				height_lbl		: height_lbl,				$
				width_lbl		: width_lbl,				$
				res_windim_barr		: res_windim_barr,			$
				current_main_drop_idx	: 2,					$
				scale_lim_barr		: scale_lim_barr,			$
				current_scale_lim_idx	: 1,					$
				current_res_windim_idx	: 1,					$
				main_base_arr		: main_base_arr,			$
				scale_cwf		: scale_cwf,				$
				min_lon_lbl		: min_lon_lbl,				$
				max_lon_lbl		: max_lon_lbl,				$
				min_lat_lbl		: min_lat_lbl,				$
				max_lat_lbl		: max_lat_lbl,				$
				clat_cwf		: clat_cwf,				$
				clon_cwf		: clon_cwf,				$
				crot_cwf		: crot_cwf,				$
				font_type		: font_type,				$
				isotropic		: 0,					$
				overlay			: 0,					$
				OVERLAP_OPTIONS		: OVERLAP,				$
				overlap_wd		: overlap_wd,				$
				fill_seam_info		: [5.0,0.25,5.0,0.25],			$
				;  the 2-element array corresponds to (box width), (pct of good pixels req'd)
				overlap_idx		: 0,					$
				fill_seam_options_btn	: fill_seam_options_btn,		$
				map_set_str		: '',					$
				do_seam_filling		: 0,					$
				isotropic_btn		: isotropic_btn,			$
				fill_seam_btn		: fill_seam_btn,			$
				main_droplist		: main_droplist,			$
				overlay_btn		: overlay_btn,				$
				current_projection_idx	: proj_idx,				$
				cancel_pressed		: 1,					$
				default_res		: default_res,				$
				info_ptr		: PTR_NEW() }, /NO_COPY )
					
	WIDGET_CONTROL, b, SET_UVALUE = ptr
	XMANAGER, 'cw_proj_info', b, EVENT_HANDLER = 'cw_proj_info_eh'
	return_struct	= { failure:1 }
	IF PTR_VALID((*ptr).info_ptr) THEN return_struct = *((*ptr).info_ptr)
	PTR_FREE, (*ptr).info_ptr
	PTR_FREE, ptr
	RETURN, return_struct
END
; cw_proj_info
