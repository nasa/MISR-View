@GetDirectoryDivider.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ transform_save @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION transform_save, ptr, NO_REMINDER = no_reminder

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== transform_save =========='
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
		RETURN, 0
	ENDIF



	filename	= (*ptr).current_filename
	IF KEYWORD_SET(no_reminder) THEN silent = 1 ELSE silent = 0
	IF filename EQ '' THEN BEGIN
		IF NOT silent THEN BEGIN
			msg	= [							$
					'The current file has no name.',		$
					'Do you want to save it?' ]
			reply	= DIALOG_MESSAGE( msg, /QUESTION, /CANCEL )
			IF STRTRIM(STRUPCASE(reply),2) EQ 'CANCEL' THEN RETURN, 0
			IF STRTRIM(STRUPCASE(reply),2) EQ 'NO' THEN RETURN, 1
		ENDIF
		done	= 0
		WHILE NOT done DO BEGIN
			title		= 'Save Transform File As...'
			filename	= dialog_pickfile_wrapper(		$
							TITLE = title,		$
							FILTER = '*.transform',	$
							GET_PATH = path )
;ckt,apr2001			filename	= DIALOG_PICKFILE(			$
;ckt,apr2001							TITLE = title,		$
;ckt,apr2001							FILTER = '*.transform',	$
;ckt,apr2001							GET_PATH = path )
			IF filename EQ '' THEN RETURN, 0
			existing_file	= FINDFILE( filename, COUNT = cnt )
			IF cnt GT 0 THEN BEGIN
				msg	= [						$
						'Are you sure you want to overwrite',	$
						filename + '?' ]
				reply	= DIALOG_MESSAGE( msg, /QUESTION )
				IF STRTRIM(STRUPCASE(reply),2) EQ 'CANCEL' THEN RETURN, 0
				IF STRTRIM(STRUPCASE(reply),2) EQ 'YES' THEN done = 1
			ENDIF ELSE BEGIN
				done	= 1
			ENDELSE
		ENDWHILE
	ENDIF ELSE BEGIN
		IF NOT silent THEN BEGIN
			msg	= [							$
					'Save changes to',				$
					filename + '?']
			reply	= DIALOG_MESSAGE( msg, /QUESTION )
			IF STRTRIM(STRUPCASE(reply),2) EQ 'CANCEL' THEN RETURN, 0
			IF STRTRIM(STRUPCASE(reply),2) EQ 'NO' THEN RETURN, 1
		ENDIF
	ENDELSE

	OPENW, lun, filename, /GET_LUN
	WIDGET_CONTROL, (*ptr).text_area_id, GET_VALUE = content_array
	FOR i = 0, N_ELEMENTS(content_array) - 1 DO		$
		PRINTF, lun, content_array[i]
	FREE_LUN, lun
	(*ptr).changes_made	= 0
	(*ptr).current_filename	= filename
	WIDGET_CONTROL, (*ptr).save_item, SENSITIVE = 0

	RETURN, 1
END
; transform_save

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ transform_editor_eh @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO transform_editor_eh, event

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== transform_editor_eh =========='
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


	event_structure_name	= STRTRIM(STRUPCASE(TAG_NAMES(event,/STRUCTURE_NAME)),2)
	IF event_structure_name EQ 'WIDGET_BASE' THEN				$
		widget_name	= 'TLB'						$
	ELSE									$
		WIDGET_CONTROL, event.id, GET_UVALUE = widget_name

	WIDGET_CONTROL, event.top, GET_UVALUE = ptr

	CASE STRUPCASE(STRTRIM(widget_name,2)) OF
		'TLB': BEGIN
;print,'resize event'
;print,event.x,event.y
			WIDGET_CONTROL, event.id, SCR_XSIZE = event.x, SCR_YSIZE = event.y
			WIDGET_CONTROL, (*ptr).text_area_id, SCR_XSIZE = event.x, SCR_YSIZE = event.y-(*ptr).button_base_height
			END
		'OPENITEM': BEGIN
			continue	= 1
			IF (*ptr).changes_made THEN continue = transform_save( ptr )
			IF NOT continue THEN RETURN

			title		= 'Select a Transform File'
			filename	= dialog_pickfile_wrapper(		$
						TITLE = title,			$
						/MUST_EXIST,			$
						FILTER = '*.transform' )
;ckt,apr2001			filename	= DIALOG_PICKFILE(			$
;ckt,apr2001						TITLE = title,			$
;ckt,apr2001						FILTER = '*.transform' )
			IF filename EQ '' THEN RETURN
			OPENR, lun, filename, /GET_LUN
			str		= ''
			WHILE NOT EOF(lun) DO BEGIN
				READF, lun, str
				IF SIZE( content_array, /TYPE ) EQ 0 THEN	$
					content_array	= [ str ]		$
				ELSE						$
					content_array	= [ content_array, str ]
			ENDWHILE
			FREE_LUN, lun
			WIDGET_CONTROL, (*ptr).text_area_id, SET_VALUE = content_array
			(*ptr).current_filename	= filename
			WIDGET_CONTROL, event.top, TLB_SET_TITLE = 'Transform Editor ('+filename+')'
			END
		'TEXTAREA': BEGIN
			IF event_structure_name NE 'WIDGET_TEXT_SEL' THEN BEGIN
				(*ptr).changes_made = 1
				WIDGET_CONTROL, (*ptr).save_item, /SENSITIVE
			ENDIF
			END
		'TEMPLATEITEM': BEGIN
			continue	= 1
			IF (*ptr).changes_made THEN continue = transform_save( ptr )
			IF NOT continue THEN RETURN
			special_str	= ';' + STRTRIM(OBJ_CLASS((*ptr).obj),2) + '_TRANSFORM_FILE'
			template_txt	= [						$
						special_str,				$
						';',					$
						';START_IMAGE_DATA_TRANSFORM_COMMENTS',	$
						';END_IMAGE_DATA_TRANSFORM_COMMENTS',	$
						';',					$
						';START_IMAGE_DATA_TRANSFORM_SETUP',	$
						';END_IMAGE_DATA_TRANSFORM_SETUP',	$
						';',					$
						';START_IMAGE_DATA_TRANSFORM_EQUATION',	$
						'transformed_data = source_data',	$
						';END_IMAGE_DATA_TRANSFORM_EQUATION' ]
			WIDGET_CONTROL, (*ptr).text_area_id, SET_VALUE = template_txt
			(*ptr).changes_made = 1
			WIDGET_CONTROL, (*ptr).save_item, /SENSITIVE
			WIDGET_CONTROL, event.top, TLB_SET_TITLE = 'Transform Editor (UNTITLED)'
			END
		'SAVEITEM': BEGIN
			continue = transform_save( ptr, /NO_REMINDER )
			IF NOT continue THEN RETURN
			WIDGET_CONTROL, event.top, TLB_SET_TITLE = 'Transform Editor ('+(*ptr).current_filename+')'
			END
		'SAVEASITEM': BEGIN
			tmp_current_filename	= (*ptr).current_filename
			(*ptr).current_filename	= ''
			continue = transform_save( ptr, /NO_REMINDER )
			IF NOT continue THEN BEGIN
				(*ptr).current_filename	= tmp_current_filename
				RETURN
			ENDIF
			WIDGET_CONTROL, event.top, TLB_SET_TITLE = 'Transform Editor ('+(*ptr).current_filename+')'
			END
		'EXITITEM': BEGIN
			continue	= 1
			IF (*ptr).changes_made THEN continue = transform_save( ptr )
			IF NOT continue THEN RETURN
			IF (*ptr).current_filename NE '' THEN BEGIN
				msg	= [ 'Do you want to use the file',	$
					    (*ptr).current_filename,		$
					    'in the Transform Information Interface?' ]
				reply	= DIALOG_MESSAGE( msg, /QUESTION )
				IF STRUPCASE(STRTRIM(reply,2)) EQ 'YES' THEN (*ptr).file2pass = (*ptr).current_filename
			ENDIF
			WIDGET_CONTROL, event.top, /DESTROY
			END
		'CHECKSYNTAX': BEGIN
			IF (*ptr).current_filename NE '' AND NOT (*ptr).changes_made THEN BEGIN
				ret	= ((*ptr).obj)->ReadTransformFile((*ptr).current_filename)
				IF NOT ret.failure THEN BEGIN
					msg	= [ 'Transform file ' + STRTRIM((*ptr).current_filename,2)+' appears to have proper syntax.' ]
					reply	= DIALOG_MESSAGE( msg, /INFORMATION )
				ENDIF
			ENDIF ELSE BEGIN
				msg	= [ 'Save current contents of transform editor first.' ]
				reply	= DIALOG_MESSAGE( msg, /INFORMATION )
			ENDELSE
			END
		ELSE:
	ENDCASE
END
; transform_editor_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ transform_editor_kill @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO transform_editor_kill, tlb
END
; transform_editor_kill

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::TransformEditor @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::TransformEditor, group_leader, TRANSFORM_FILE = transform_file

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== IMAGE_DATA::TransformEditor =========='
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
		RETURN, ''
	ENDIF


	DEVICE, GET_SCREEN_SIZE = screen_size
	screen_xdim1	= screen_size[0] * 0.50
	screen_ydim1	= screen_size[1] * 0.50
	screen_xdim2	= screen_xdim1 * 0.75
	screen_ydim2	= screen_ydim1 * 0.75
	non_title_font	= GetCorrectFont( 'courier2bold' )

	tlb		= WIDGET_BASE(						$
					TITLE = 'Transform Editor',		$
					/COLUMN,				$
					GROUP_LEADER = group_leader,		$
					/MODAL,					$
					EVENT_PRO = 'transform_editor_eh',	$
					KILL_NOTIFY = 'transform_editor_kill',	$
					/TLB_SIZE_EVENTS,			$
					UVALUE = 'tlb' )
	button_base	= WIDGET_BASE(						$
					tlb,					$
					/ROW )
	file_menu	= WIDGET_BUTTON(					$
					button_base,				$
					VALUE = 'File',				$
					/MENU,					$
					FONT = non_title_font )

	template_item	= WIDGET_BUTTON(					$
					file_menu,				$
					VALUE = 'Template',			$
					UVALUE = 'templateitem',		$
					FONT = non_title_font )
	open_item	= WIDGET_BUTTON(					$
					file_menu,				$
					VALUE = 'Open...',			$
					UVALUE = 'openitem',			$
					FONT = non_title_font )

	save_item	= WIDGET_BUTTON(					$
					file_menu,				$
					VALUE = 'Save',				$
					UVALUE = 'saveitem',			$
					FONT = non_title_font )

	saveas_item	= WIDGET_BUTTON(					$
					file_menu,				$
					VALUE = 'Save As...',			$
					UVALUE = 'saveasitem',			$
					FONT = non_title_font )

	exit_item	= WIDGET_BUTTON(					$
					file_menu,				$
					VALUE = 'Exit',				$
					UVALUE = 'exititem',			$
					FONT = non_title_font )

	options_menu	= WIDGET_BUTTON(					$
					button_base,				$
					VALUE = 'Options',			$
					/MENU,					$
					FONT = non_title_font )
	syntax_item	= WIDGET_BUTTON(					$
					options_menu,				$
					VALUE = 'Check Syntax',			$
					UVALUE = 'checksyntax',			$
					FONT = non_title_font )

	text_area_id	= WIDGET_TEXT(						$
					tlb,					$
					/ALL_EVENTS,				$
					/EDITABLE,				$
					UVALUE = 'textarea',			$
					/WRAP,					$
					/SCROLL,				$
					SCR_XSIZE = screen_xdim1,			$
					SCR_YSIZE = screen_ydim1 )
	WIDGET_CONTROL, tlb, /REALIZE

	WIDGET_CONTROL, save_item, SENSITIVE = 0

	IF KEYWORD_SET(transform_file) THEN BEGIN
		OPENR, lun, transform_file, /GET_LUN
		str	= ''
		WHILE NOT EOF(lun) DO BEGIN
			READF, lun, str
			IF SIZE(contents,/TYPE) EQ 0 THEN			$
				contents	= [str]				$
			ELSE							$
				contents	= [ contents, str ]
		ENDWHILE
		FREE_LUN, lun
		tmp		= STR_SEP( transform_file, GetDirectoryDivider() )
		file_title	= tmp[N_ELEMENTS(tmp)-1]
		current_filename= transform_file
	ENDIF ELSE BEGIN
		special_str	= ';' + STRTRIM(OBJ_CLASS(SELF),2) + '_TRANSFORM_FILE'
		contents	= [						$
					special_str,				$
					';',					$
					';START_IMAGE_DATA_TRANSFORM_COMMENTS',	$
					';END_IMAGE_DATA_TRANSFORM_COMMENTS',	$
					';',					$
					';START_IMAGE_DATA_TRANSFORM_SETUP',	$
					';END_IMAGE_DATA_TRANSFORM_SETUP',	$
					';',					$
					';START_IMAGE_DATA_TRANSFORM_EQUATION',	$
					'transformed_data = source_data',	$
					';END_IMAGE_DATA_TRANSFORM_EQUATION' ]
		file_title	= 'UNTITLED'
		current_filename= ''
	ENDELSE

	WIDGET_CONTROL, text_area_id, SET_VALUE = contents
	WIDGET_CONTROL, tlb, TLB_SET_TITLE = 'Transform Editor ('+file_title+')'

	geom			= WIDGET_INFO( button_base, /GEOMETRY )

	button_base_height	= geom.YSIZE + (2*geom.MARGIN)

	ptr		= PTR_NEW(								$
					{							$
						text_area_id		: text_area_id,		$
						changes_made		: 0,			$
						save_item		: save_item,		$
						current_filename	: current_filename,	$
						button_base_height	: button_base_height,	$
						file2pass		: '',			$
						obj			: SELF },		$
					/NO_COPY )

	WIDGET_CONTROL, tlb, SET_UVALUE = ptr

	XMANAGER, 'Transform Editor', tlb, EVENT_HANDLER = 'transform_editor_eh'

	file2pass	= (*ptr).file2pass
	PTR_FREE, ptr

	RETURN, file2pass
END
; IMAGE_DATA::TransformEditor

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@ IMAGE_DATA::Set_Default_Transform @@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO IMAGE_DATA::Set_Default_Transform
END
; IMAGE_DATA::Set_Default_Transform

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@ IMAGE_DATA::ReturnLastGoodTransform @@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnLastGoodTransform
	info2return	= ['']
	IF PTR_VALID(SELF.last_good_transform_formula_ptr) THEN		$
		info2return = *(SELF.last_good_transform_formula_ptr)

	RETURN, info2return
END
; IMAGE_DATA::ReturnLastGoodTransform

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@ IMAGE_DATA::ReturnLastGoodTransformInfo @@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnLastGoodTransformInfo
	info2return	= ['']
	IF PTR_VALID(SELF.last_good_transform_info_ptr) THEN		$
		info2return = *(SELF.last_good_transform_info_ptr)

	RETURN, info2return
END
; IMAGE_DATA::ReturnLastGoodTransformInfo

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::ReturnTransform @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnTransform
	IF PTR_VALID(SELF.data_transform_formula_ptr) THEN RETURN, *(SELF.data_transform_formula_ptr) ELSE RETURN, ['']
END
; IMAGE_DATA::ReturnTransform

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::ReturnTransformInfo @@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnTransformInfo
	IF PTR_VALID(SELF.data_transform_info_ptr) THEN RETURN, *(SELF.data_transform_info_ptr) ELSE RETURN, ['']
END
; IMAGE_DATA::ReturnTransformInfo

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::SetTransform @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO IMAGE_DATA::SetTransform, filename, CLEAR_TRANSFORM = clear_transform, SUCCESS = success

	success	= 1

	IF KEYWORD_SET(clear_transform) THEN BEGIN
		PTR_FREE, SELF.data_transform_formula_ptr
		PTR_FREE, SELF.data_transform_info_ptr
		SELF.current_transform_file	= ''

		SELF.valid_data_max	= SELF->GetMaxVal(				$
								0,			$
								SELF.global_width-1,	$
								0,			$
								SELF.global_height-1,	$
								SELF.global_width,	$
								SELF.global_height,	$
								/RECALCULATE_GLOBAL )
		SELF.valid_data_min	= SELF->GetMinVal(				$
								0,			$
								SELF.global_width-1,	$
								0,			$
								SELF.global_height-1,	$
								SELF.global_width,	$
								SELF.global_height,	$
								/RECALCULATE_GLOBAL )

		SELF.scaling_params.min_val_to_byte	= SELF.valid_data_min
		SELF.scaling_params.max_val_to_byte	= SELF.valid_data_max

		RETURN
	ENDIF

	transform_file	= STRTRIM( filename, 2 )
	IF transform_file EQ '' THEN RETURN

	transform_file	= FINDFILE( transform_file )
	IF transform_file[0] EQ '' THEN BEGIN
		msg	= [ 'IMAGE_DATA::SetTransform: Could not find',	$
			    transform_file ]
		reply	= DIALOG_MESSAGE( msg, /ERROR )
		success	= 0
		RETURN
	ENDIF
	transform_file	= transform_file[0]

	ret		= SELF->ReadTransformFile( transform_file, SILENT = SELF.vm_used )
	IF NOT ret.failure THEN BEGIN
		SELF.current_transform_file	= transform_file
		PTR_FREE, SELF.data_transform_formula_ptr
		PTR_FREE, SELF.data_transform_info_ptr
		SELF.data_transform_formula_ptr	= PTR_NEW( ret.trans )
		SELF.data_transform_info_ptr	= PTR_NEW( ret.trans_info )
	ENDIF ELSE BEGIN
		success	= 0
		RETURN
	ENDELSE

	SELF.valid_data_max	= SELF->GetMaxVal(				$
							0,			$
							SELF.global_width-1,	$
							0,			$
							SELF.global_height-1,	$
							SELF.global_width,	$
							SELF.global_height,	$
							/RECALCULATE_GLOBAL )
	SELF.valid_data_min	= SELF->GetMinVal(				$
							0,			$
							SELF.global_width-1,	$
							0,			$
							SELF.global_height-1,	$
							SELF.global_width,	$
							SELF.global_height,	$
							/RECALCULATE_GLOBAL )

	SELF.scaling_params.min_val_to_byte	= SELF.valid_data_min
	SELF.scaling_params.max_val_to_byte	= SELF.valid_data_max

END
; IMAGE_DATA::SetTransform

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::ReadTransformFile @@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReadTransformFile, filename, SILENT = silent

	ON_IOERROR, BAD


	IF SELF.vm_used THEN BEGIN
		msg	= [						$
		'PLEASE NOTE: As this application is currently',	$
		'being run using the IDL Virtual Machine, there is',	$
		'only limited functionality the user has with',		$
		'using transform files.  Please note the following:',	$
		'',							$
		'- The transform files that are provided',		$
		'  (".transform" suffix) are the only transforms that',	$
		'  are available to the user; these transforms have',	$
		'  been converted to functions and are part of the',	$
		'  compiled code.',					$
		'',							$
		'- The ability to edit or create new transforms from',	$
		'  within the application has been disabled.',		$
		'',							$
		'- Although the user can use an external text editor',	$
		'  to create a transform file, the file will not be',	$
		'  recognized and, in fact, may cause an error if the',	$
		'  user attempts to use it as a valid transform.',	$
		'',							$
		'- The user should NOT change the names of the',	$
		'  supplied transform files.  The application keys off',$
		'  of the name of the transform file to call its ',	$
		'  functional analog.  Modifying the transform file',	$
		'  names will cause problems.',				$
		'',							$
		'- The user can use an external text editor to view',	$
		'  the source of the transform files that are',		$
		'  provided; again, any changes the user makes to the',	$
		'  source transform file will have no effect.' ]
		IF NOT KEYWORD_SET(silent) THEN				$
			res= DIALOG_MESSAGE(msg,/INFORMATION)
		msg	= ['Not viewable using IDL VM... use text editor to view source transform file']
		RETURN, { failure:0, trans:msg, trans_info:msg, trans_setup:msg }
	ENDIF



	trans_info_strarr	= ['']
	trans_strarr		= ['']
	trans_setup_strarr	= ['']
	OPENR, lun, filename, /GET_LUN
	str		= ''
	bad_file	= 0
	READF, lun, str
	str			= STRTRIM(str,2)
	IF STRLEN(str) GE STRLEN(';IMAGE_DATA_TRANSFORM_FILE') THEN BEGIN
		end_str	= STRMID(str,STRLEN(str)-STRLEN('IMAGE_DATA_TRANSFORM_FILE'),STRLEN('IMAGE_DATA_TRANSFORM_FILE'))
;print,'end_str=',end_str
		IF end_str EQ 'IMAGE_DATA_TRANSFORM_FILE' THEN BEGIN
			proper_header_str	= ';'+STRTRIM(OBJ_CLASS(SELF),2)+'_TRANSFORM_FILE'
;print,'str=',str
;print,'proper_header_str=',proper_header_str
			IF str NE proper_header_str AND str NE ';IMAGE_DATA_TRANSFORM_FILE' THEN BEGIN
				msg	= [ 'It appears that this transform file has been created for a',			$
					    'class of IMAGE_DATA (' +								$
					    STRMID(str,1,STRLEN(str)-STRLEN(';_TRANSFORM_FILE')) +			$
					    ') which is different than the current',						$
					    'IMAGE_DATA class ('+STRTRIM(OBJ_CLASS(SELF),2)+').  This',				$
					    'may cause problems if there are unsupported methods.  Continue?' ]
				reply	= DIALOG_MESSAGE( msg, /QUESTION )
				IF STRUPCASE(STRTRIM(reply,2)) EQ 'NO' THEN bad_file = 1
			ENDIF
		ENDIF ELSE BEGIN
			msg	= [								$
					'The file ' + filename,					$
					'does not appear to be a valid transform file.',	$
					'Please consult the misr_view user guide for',		$
					'details on creating transform files.' ]
			res	= DIALOG_MESSAGE( msg, /ERROR )
			bad_file	= 1
		ENDELSE
	ENDIF ELSE BEGIN
		msg	= [								$
				'The file ' + filename,					$
				'does not appear to be a valid transform file.',	$
				'Please consult the misr_view user guide for',		$
				'details on creating transform files.' ]
		res	= DIALOG_MESSAGE( msg, /ERROR )
		bad_file= 1
	ENDELSE

	IF bad_file THEN BEGIN
		FREE_LUN, lun
		RETURN, { failure:1, trans:trans_strarr, trans_info:trans_info_strarr, trans_setup:trans_setup_strarr }
	ENDIF

	trans_info_strarr	= STRARR(100)
	trans_strarr		= STRARR(100)
	trans_setup_strarr	= STRARR(100)
	trans_info_ctr		= 0
	trans_ctr		= 0
	trans_setup_ctr		= 0

	transform_comment_open	= 0
	transform_setup_open	= 0
	transform_eqn_open	= 0
	error_encountered	= 0
	setup_encountered	= 0
	msg			= [ 'Error encountered reading transform file' ]

	WHILE NOT EOF(lun) AND NOT error_encountered DO BEGIN
		READF, lun, str
		str	= STRTRIM( str, 2 )
		CASE 1 OF
			str EQ ';START_IMAGE_DATA_TRANSFORM_COMMENTS': BEGIN
				IF NOT error_encountered THEN BEGIN
					IF					$
						NOT transform_comment_open AND	$
						NOT transform_setup_open AND	$
						NOT transform_eqn_open THEN BEGIN
						transform_comment_open	= 1
					ENDIF ELSE BEGIN
						error_encountered	= 1
						msg			= [ 'start/end keywords out of order' ]
					ENDELSE
				ENDIF
				END
			str EQ ';END_IMAGE_DATA_TRANSFORM_COMMENTS':BEGIN
				IF NOT error_encountered THEN BEGIN
					IF					$
						transform_comment_open AND	$
						NOT transform_setup_open AND	$
						NOT transform_eqn_open THEN BEGIN
						transform_comment_open	= 0
					ENDIF ELSE BEGIN
						error_encountered	= 1
						msg			= [ 'start/end keywords out of order' ]
					ENDELSE
				ENDIF
				END
			str EQ ';START_IMAGE_DATA_TRANSFORM_SETUP': BEGIN
				IF NOT error_encountered THEN BEGIN
					IF					$
						NOT transform_comment_open AND	$
						NOT transform_setup_open AND	$
						NOT transform_eqn_open THEN BEGIN
						transform_setup_open	= 1
					ENDIF ELSE BEGIN
						error_encountered	= 1
						msg			= [ 'start/end keywords out of order' ]
					ENDELSE
				ENDIF
				END
			str EQ ';END_IMAGE_DATA_TRANSFORM_SETUP': BEGIN
				IF NOT error_encountered THEN BEGIN
					IF					$
						NOT transform_comment_open AND	$
						transform_setup_open AND	$
						NOT transform_eqn_open THEN BEGIN
						transform_setup_open	= 0
					ENDIF ELSE BEGIN
						error_encountered	= 1
						msg			= [ 'start/end keywords out of order' ]
					ENDELSE
				ENDIF
				END
			str EQ ';START_IMAGE_DATA_TRANSFORM_EQUATION': BEGIN
				IF NOT error_encountered THEN BEGIN
					IF					$
						NOT transform_comment_open AND	$
						NOT transform_setup_open AND	$
						NOT transform_eqn_open THEN BEGIN
						transform_eqn_open	= 1
					ENDIF ELSE BEGIN
						error_encountered	= 1
						msg			= [ 'start/end keywords out of order' ]
					ENDELSE
				ENDIF
				END
			str EQ ';END_IMAGE_DATA_TRANSFORM_EQUATION': BEGIN
				IF NOT error_encountered THEN BEGIN
					IF					$
						NOT transform_comment_open AND	$
						NOT transform_setup_open AND	$
						transform_eqn_open THEN BEGIN
						transform_eqn_open	= 0
					ENDIF ELSE BEGIN
						error_encountered	= 1
						msg			= [ 'start/end keywords out of order' ]
					ENDELSE
				ENDIF
				END
			STRMID(str,0,1) EQ ';': BEGIN
				IF transform_comment_open AND NOT error_encountered THEN BEGIN
					trans_info_strarr[trans_info_ctr]	= STRMID(str,1)
					trans_info_ctr				= trans_info_ctr + 1
				ENDIF
				END
			ELSE: BEGIN
				CASE 1 OF
					STRLEN(str) EQ 0: BEGIN
						END
					transform_comment_open: BEGIN
						error_encountered	= 1
						msg			= [ 'non-comment line encountered',	$
									    'in comment section' ]
						END
					transform_eqn_open: BEGIN
						trans_strarr[trans_ctr]	= str
						trans_ctr		= trans_ctr + 1
						END
					transform_setup_open: BEGIN
						trans_setup_strarr[trans_setup_ctr]	= str
						trans_setup_ctr				= trans_setup_ctr + 1
						setup_encountered			= 1
						END
					ELSE: BEGIN
						error_encountered	= 1
						msg			= [ 'non-comment line encountered outside',	$
									    'of setup or equation sections' ]
						END
				ENDCASE
				END
		ENDCASE
	ENDWHILE

	GOTO, DONE

BAD:	msg			= [ 'Error encountered while attempting to read',			$
				     filename,								$
				     !ERR_STRING ]
	error_encountered	= 1

DONE:	FREE_LUN, lun

	IF trans_ctr LE 0 AND NOT error_encountered THEN BEGIN
		error_encountered	= 1
		msg			= [ 'no transform specified' ]
	ENDIF

	IF NOT error_encountered THEN BEGIN
		IF					$
			transform_comment_open OR	$
			transform_setup_open OR		$
			transform_eqn_open THEN BEGIN
			error_encountered	= 1
			msg			= [ 'missing start/end keyword' ]
		ENDIF
	ENDIF

	IF error_encountered THEN BEGIN
		ret	= DIALOG_MESSAGE( msg, /ERROR )
		RETURN, { failure:1, trans:trans_strarr, trans_info:trans_info_strarr, trans_setup:trans_setup_strarr }
	ENDIF

	IF setup_encountered THEN BEGIN
		source_data	= (*(SELF.img_stack_ptr))
		setup_varname_strarr	= STRARR( trans_setup_ctr )
		setup_var_ptrarr	= PTRARR( trans_setup_ctr )
		i			= 0
		WHILE i LT trans_setup_ctr AND NOT error_encountered DO BEGIN
			tmpstr		= trans_setup_strarr[i]
			sep_str		= STR_SEP( tmpstr, '=' )
			IF N_ELEMENTS(sep_str) NE 2 THEN BEGIN
				error_encountered	= 1
				msg			= [ 'bad line in setup portion of transform file:',	$
							    tmpstr ]
			ENDIF ELSE BEGIN

				success = EXECUTE( tmpstr )

				IF NOT success THEN BEGIN
					error_encountered	= 1
					msg			= [ 'bad line in setup portion of transform file:',	$
								    tmpstr ]
				ENDIF ELSE BEGIN
					setup_varname_strarr[i]	= STRTRIM( sep_str[0], 2 )


;;;ckt,sep2004 setup_var_ptrarr[i]	= CALL_FUNCTION('PTR_NEW',setup_varname_strarr[i])
					str2exec		= 'setup_var_ptrarr[i]=PTR_NEW('+setup_varname_strarr[i]+')'
					success			= EXECUTE( str2exec )




					IF NOT success THEN BEGIN
						error_encountered	= 1
						msg			= [ 'bad line in setup portion of transform file:',	$
									    tmpstr ]
					ENDIF
				ENDELSE
			ENDELSE
			i		= i + 1
		ENDWHILE

		IF error_encountered THEN BEGIN
			ret	= DIALOG_MESSAGE( msg, /ERROR )
			PTR_FREE, setup_var_ptrarr
			RETURN, { failure:1, trans:trans_strarr, trans_info:trans_info_strarr, trans_setup:trans_setup_strarr }
		ENDIF

		i	= 0

		WHILE i LT trans_ctr AND NOT error_encountered DO BEGIN
			t		= trans_strarr[i]
			done		= 0
			start_pos	= 0
			WHILE NOT done AND NOT error_encountered DO BEGIN
				pos1	= STRPOS( t, '<<', start_pos )
				IF pos1 GE 0 THEN BEGIN
					pos2	= STRPOS( t, '>>', pos1 )
					IF pos2 GE pos1+2 AND NOT (pos1 EQ 0 AND pos2 EQ STRLEN(t)-2) THEN BEGIN
						current_varname		= STRCOMPRESS( STRTRIM( STRMID( t, pos1+2,pos2-pos1-2 ), 2 ), /REMOVE_ALL )
						IF STRLEN( current_varname ) GT 0 THEN BEGIN

							str2chars	= STRARR(STRLEN(current_varname))
							FOR char_ctr = 0, N_ELEMENTS(str2chars)-1 DO str2chars[char_ctr] = STRMID(current_varname,char_ctr,1)

							bracket_idx1			= WHERE( str2chars EQ '[', bracket_cnt1 )
							bracket_idx2			= WHERE( str2chars EQ ']', bracket_cnt2 )
							bad_brackets_encountered	= 0
							bracket_value_str		= ''
							IF 								$
								(bracket_cnt1 GT 0 AND bracket_cnt2 EQ 0) OR		$ ;unmatched forward brackets
								(bracket_cnt1 EQ 0 AND bracket_cnt2 GT 0) OR		$ ;unmatched backward brackets
								bracket_cnt1 GT 1 OR					$ ;too many forward brackets
								bracket_cnt2 GT 1 OR					$ ;too many backward brackets
								((bracket_cnt1 EQ 1 AND bracket_cnt2 EQ 1) AND 		$
								  bracket_idx1[0] GE bracket_idx2[0]) OR		$ ;out-of-order brackets
								bracket_idx1[0] EQ 0 OR					$ ;forward bracket is before variable name
								((bracket_cnt1 EQ 1 AND bracket_cnt2 EQ 1) AND 		$
								  bracket_idx1[0]+1 GE bracket_idx2[0])			$ ;nothing between brackets (e.g., '[]')
							THEN bad_brackets_encountered = 1

							IF NOT bad_brackets_encountered AND bracket_cnt1 EQ 1 AND bracket_cnt2 EQ 1 THEN BEGIN
								bracket_value_str	= STRMID( current_varname, bracket_idx1[0], bracket_idx2[0]-bracket_idx1[0]+1 )
								current_varname		= STRMID( current_varname, 0, bracket_idx1[0] )
							ENDIF


							idx	= WHERE( setup_varname_strarr EQ current_varname, cnt )
							IF cnt GT 0 AND bad_brackets_encountered LE 0 THEN BEGIN
								;=============================================
								; add parentheses to avoid strings like
								; x_var+-1 ( instead, x_var+(-1) )
								;=============================================

								val2use	= (*(setup_var_ptrarr[idx[0]]))[0]

								IF N_ELEMENTS(*(setup_var_ptrarr[idx[0]])) GT 1 AND bracket_value_str EQ '' THEN BEGIN
									error_encountered	= 1
									msg			= [		$
										'variable in equation',	$
										'must be scalar (1 element)',	$
										current_varname + ' currently',	$
										'consists of ' + STRTRIM(N_ELEMENTS(*(setup_var_ptrarr[idx[0]])),2) + ' elements' ]
									done			= 1
								ENDIF

								IF N_ELEMENTS(*(setup_var_ptrarr[idx[0]])) GT 1 AND bracket_value_str NE '' THEN BEGIN

									str2exec	= 'val2use = (*(setup_var_ptrarr[idx[0]]))' + bracket_value_str
									success		= EXECUTE( str2exec )



									IF success LT 1 THEN BEGIN
										error_encountered	= 1
										msg			= [			$
											'problem parsing variable:',		$
											current_varname+bracket_value_str ]
										done			= 1
									ENDIF
								ENDIF

								IF error_encountered LT 1 THEN BEGIN

									var2use	= '('+STRTRIM(val2use,2)+')'

									CASE 1 OF
										pos1 EQ 0: BEGIN
											;=============================================
											; if variable to be used is NOT a scalar,
											; only access first element, as this
											; substitution routine only works with
											; scalar replacements.
											;=============================================
											new_t		= var2use+STRMID(t,pos2+2)
											start_pos	= STRLEN(var2use)
											END
										pos2 EQ STRLEN(t)-2: BEGIN
											new_t		= STRMID(t,0,pos1)+var2use
											start_pos	= STRLEN(STRMID(t,0,pos1)+var2use)
											END
										ELSE: BEGIN
											new_t		= STRMID(t,0,pos1)+var2use+STRMID(t,pos2+2)
											start_pos	= STRLEN(STRMID(t,0,pos1)+var2use)
											END
									ENDCASE
									t		= new_t
								ENDIF
							ENDIF ELSE BEGIN
								error_encountered	= 1
								msg			= [ 'unknown variable in equation',	$
											    'portion of transform file:',	$
											    current_varname ]
								done			= 1
							ENDELSE
						ENDIF ELSE BEGIN
							error_encountered	= 1
							msg			= [ 'bad line in equation portion of transform file:',	$
										    t ]
							done			= 1
						ENDELSE
					ENDIF ELSE BEGIN
						error_encountered	= 1
						msg			= [ 'bad line in equation portion of transform file:',	$
									    t ]
						done			= 1
					ENDELSE
				ENDIF ELSE BEGIN
					done	= 1
				ENDELSE
				trans_strarr[i]	= t
			ENDWHILE
			i	= i + 1
		ENDWHILE
		PTR_FREE, setup_var_ptrarr
	ENDIF

	IF error_encountered THEN BEGIN
		ret	= DIALOG_MESSAGE( msg, /ERROR )
		RETURN, { failure:1, trans:trans_strarr, trans_info:trans_info_strarr, trans_setup:trans_setup_strarr }
	ENDIF

	valid			= SELF->CheckTransformValidity( trans_strarr, /SILENT )

	IF trans_ctr LE 0 THEN trans_ctr = 1
	IF trans_info_ctr LE 0 THEN trans_info_ctr = 1
	IF trans_setup_ctr LE 0 THEN trans_setup_ctr = 1

	IF NOT valid THEN BEGIN
		msg	= [ 'Error in transform equation.  Please look over the',	$
			    'equation below and make the necessary corrections',	$
			    'in the transform file.  Consult the user guide for',	$
			    'more information',						$
			    '',								$
			    trans_strarr[0:trans_ctr-1] ]
		ret	= DIALOG_MESSAGE( msg, /ERROR )
		RETURN, { failure:1, trans:trans_strarr, trans_info:trans_info_strarr, trans_setup:trans_setup_strarr }
	ENDIF

	RETURN, {									$
			failure		: 0,						$
			trans		: trans_strarr[0:trans_ctr-1],			$
			trans_info	: trans_info_strarr[0:trans_info_ctr-1],	$
			trans_setup	: trans_setup_strarr[0:trans_setup_ctr-1] }

END
; IMAGE_DATA::ReadTransformFile

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@ IMAGE_DATA::ValidTransformExists @@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ValidTransformExists
	RETURN, PTR_VALID(SELF.data_transform_formula_ptr)
END
; IMAGE_DATA::ValidTransformExists

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@ IMAGE_DATA::CheckTransformValidity @@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::CheckTransformValidity, trans_str, SILENT = silent
	idx		= WHERE( trans_str NE '', cnt)
	IF cnt LE 0 THEN RETURN, 0
	transform_lines	= trans_str[idx]

	n_lines		= N_ELEMENTS(transform_lines)
	transform_good	= 1
	line_ctr	= 0
	source_data	= ((*(SELF.img_stack_ptr))[*,*,0])

	WHILE transform_good AND line_ctr LT n_lines DO BEGIN
		str2exec	= transform_lines[line_ctr]
;print,'str2exec=',str2exec
		success		= EXECUTE(str2exec)
;print,'--------> success = ',success
		IF NOT success THEN transform_good = 0
		line_ctr	= line_ctr + 1
	ENDWHILE

	IF NOT transform_good THEN BEGIN
		IF NOT KEYWORD_SET(silent) THEN					$
			msg	= DIALOG_MESSAGE( [				$
					'Problem with current transform',	$
					'Please check the transform code',	$
					'carefully for errors, or clear',	$
					'the transform entirely in order',	$
					'to avoid having this message reappear.' ], /ERROR )
		RETURN, 0
	ENDIF

	RETURN, 1
END
; IMAGE_DATA::CheckTransformValidity

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@ IMAGE_DATA::ReturnDefaultTransformFile @@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnDefaultTransformFile
	RETURN, SELF.default_transform_file
END
; IMAGE_DATA::ReturnDefaultTransformFile

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@ DisplayTransformInfo_eh @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO DisplayTransformInfo_eh, event
	WIDGET_CONTROL, event.id, GET_UVALUE = uval
	WIDGET_CONTROL, event.top, GET_UVALUE = info_ptr

	obj	= (*info_ptr).obj

	CASE STRLOWCASE(uval) OF
		'apply': BEGIN
			WIDGET_CONTROL, (*info_ptr).trans_txt, GET_VALUE = trans_txt_strarr
			trans_txt_strarr	= STRTRIM(trans_txt_strarr,2)
			IF N_ELEMENTS(trans_txt_strarr) LE 1 AND STRLEN(trans_txt_strarr[0]) LE 0 THEN BEGIN
				obj->SetTransform, (*info_ptr).transform_file2use, /CLEAR_TRANSFORM
				(*info_ptr).update	= 1
			ENDIF ELSE BEGIN
				obj->SetTransform, (*info_ptr).transform_file2use, SUCCESS = success
				IF NOT success THEN BEGIN
					msg	= [ 'Transform file not applied.' ]
					reply	= DIALOG_MESSAGE( msg, /INFORMATION )
				ENDIF ELSE BEGIN
					(*info_ptr).update	= 1
				ENDELSE
			ENDELSE
			END
		'dismiss': BEGIN
			WIDGET_CONTROL, event.top, /DESTROY
			END
		'recalltransform': BEGIN
			transform_file	= dialog_pickfile_wrapper( TITLE = 'Select a transform file', /MUST_EXIST, FILTER = '*.transform' )
;ckt,apr2001			transform_file	= DIALOG_PICKFILE( TITLE = 'Select a transform file', FILTER = '*.transform' )
			IF transform_file EQ '' THEN RETURN
			transform_info	= obj->ReadTransformFile(transform_file)
			IF transform_info.failure THEN RETURN
			(*info_ptr).transform_file2use	= transform_file
			WIDGET_CONTROL, (*info_ptr).transform_file_lbl, SET_VALUE = (*info_ptr).transform_file2use
			WIDGET_CONTROL, (*info_ptr).trans_txt, SET_VALUE = transform_info.trans
			WIDGET_CONTROL, (*info_ptr).info_txt, SET_VALUE = transform_info.trans_info
			END
		'defaulttransform': BEGIN
			default_transform_file	= obj->ReturnDefaultTransformFile()
			transform_info	= obj->ReadTransformFile(default_transform_file)
			IF transform_info.failure THEN RETURN
			(*info_ptr).transform_file2use	= default_transform_file
			WIDGET_CONTROL, (*info_ptr).transform_file_lbl, SET_VALUE = (*info_ptr).transform_file2use
			WIDGET_CONTROL, (*info_ptr).trans_txt, SET_VALUE = transform_info.trans
			WIDGET_CONTROL, (*info_ptr).info_txt, SET_VALUE = transform_info.trans_info
			END
		'transformeditor': BEGIN
			transform_file	= obj->TransformEditor( event.top, TRANSFORM_FILE = (*info_ptr).transform_file2use )
			IF transform_file EQ '' THEN RETURN
			transform_info	= obj->ReadTransformFile(transform_file)
			IF transform_info.failure THEN RETURN
			(*info_ptr).transform_file2use	= transform_file
			WIDGET_CONTROL, (*info_ptr).transform_file_lbl, SET_VALUE = (*info_ptr).transform_file2use
			WIDGET_CONTROL, (*info_ptr).trans_txt, SET_VALUE = transform_info.trans
			WIDGET_CONTROL, (*info_ptr).info_txt, SET_VALUE = transform_info.trans_info
			END
		'cleartransform': BEGIN
			WIDGET_CONTROL, (*info_ptr).trans_txt,SET_VALUE = ['']
			WIDGET_CONTROL, (*info_ptr).info_txt, SET_VALUE = ['']
			WIDGET_CONTROL, (*info_ptr).transform_file_lbl, SET_VALUE = 'None'
			(*info_ptr).transform_file2use	= ''
			END
		ELSE:
	ENDCASE
END
; DisplayTransformInfo_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ DisplayTransformInfo_Kill @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO DisplayTransformInfo_Kill, tlb
END
; DisplayTransformInfo_Kill

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@ IMAGE_DATA::DisplayTransformInfo @@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::DisplayTransformInfo, GROUP_LEADER = group_leader, DATA_DESCRIPTION = data_description
	IF NOT KEYWORD_SET(group_leader) THEN group_leader = WIDGET_BASE()
	IF NOT KEYWORD_SET(data_description) THEN data_description = 'None Available'
	tlb	= WIDGET_BASE(								$
				GROUP_LEADER = group_leader,				$
				/MODAL,							$
				/COLUMN,						$
				/BASE_ALIGN_CENTER, 					$
				EVENT_PRO = 'DisplayTransformInfo_eh',			$
				KILL_NOTIFY = 'DisplayTransformInfo_Kill',		$
				TITLE = 'Transformation Information Interface' )
	lbl1	= WIDGET_LABEL(								$
				tlb,							$
				VALUE = 'Data Description',				$
				FONT = GetCorrectFont('courier2bold') )
	lbl2	= WIDGET_LABEL(								$
				tlb,							$
				VALUE = data_description,				$
				FONT = GetCorrectFont('courier3bold') )
	lbl3	= WIDGET_LABEL(								$
				tlb,							$
				VALUE = '____________________',				$
				FONT = GetCorrectFont('courier2bold') )

	transform_file_base								$
		= WIDGET_BASE(								$
				tlb, /ROW, /ALIGN_CENTER, /BASE_ALIGN_CENTER )
	transform_file_lbl1								$
		= WIDGET_LABEL(								$
				transform_file_base, VALUE = 'Current Transform File:',	$
				FONT = GetCorrectFont('courier2bold') )
	IF SELF.current_transform_file EQ '' THEN file2use = 'None' ELSE file2use = SELF.current_transform_file
	transform_file_lbl2								$
		= WIDGET_LABEL(								$
				transform_file_base, VALUE = file2use,			$
				/DYNAMIC_RESIZE,					$
				FONT = GetCorrectFont('courier2bold') )


	lbl33	= WIDGET_LABEL(								$
				tlb,							$
				VALUE = '____________________',				$
				FONT = GetCorrectFont('courier2bold') )

	lbl4	= WIDGET_LABEL(								$
				tlb,							$
				VALUE = 'Transform Commands',				$
				FONT = GetCorrectFont('courier2bold') )
	IF PTR_VALID(SELF.data_transform_formula_ptr) THEN trans = *(SELF.data_transform_formula_ptr) ELSE trans = ['']

	txt1		= WIDGET_TEXT(							$
					tlb,						$
					VALUE = trans,					$
					SCR_XSIZE = 600,				$
					XSIZE = 1000,					$
					SCR_YSIZE = 200,				$
					YSIZE = 300,					$
					/SCROLL,					$
					UVAL = 'text',					$
					FONT = GetCorrectFont('courier2bold') )
	lbl6	= WIDGET_LABEL(								$
				tlb,							$
				VALUE = '____________________',				$
				FONT = GetCorrectFont('courier2bold') )
	lbl7	= WIDGET_LABEL(								$
				tlb,							$
				VALUE = 'Transform Information',			$
				FONT = GetCorrectFont('courier2bold') )
;PRINT,'--------- PTR_VALID(SELF.data_transform_info_ptr)? = ',PTR_VALID(SELF.data_transform_info_ptr)
	IF PTR_VALID(SELF.data_transform_info_ptr) THEN trans_info = *(SELF.data_transform_info_ptr) ELSE trans_info = ['']
	txt2		= WIDGET_TEXT(							$
					tlb,						$
					VALUE = trans_info,				$
					SCR_XSIZE = 600,				$
					XSIZE = 1000,					$
					SCR_YSIZE = 200,				$
					UVAL = 'text',					$
					YSIZE = 300,					$
					/SCROLL,					$
					FONT = GetCorrectFont('courier2bold') )

	sub_base1	= WIDGET_BASE( tlb, /ROW, /BASE_ALIGN_CENTER, /FRAME )
	default		= WIDGET_BUTTON( sub_base1, VALUE = 'Default Transform', UVALUE = 'defaulttransform', FONT = GetCorrectFont('courier2bold') )
	recall		= WIDGET_BUTTON( sub_base1, VALUE = 'Recall Transform...', UVALUE = 'recalltransform', FONT = GetCorrectFont('courier2bold') )
	clear		= WIDGET_BUTTON( sub_base1, VALUE = 'Clear Transform', UVALUE = 'cleartransform', FONT = GetCorrectFont('courier2bold') )

	editor		= WIDGET_BUTTON( sub_base1, VALUE = 'Transform Editor', UVALUE = 'transformeditor', FONT = GetCorrectFont('courier2bold') )

	sub_base2	= WIDGET_BASE( tlb, /ROW, /BASE_ALIGN_CENTER, /FRAME )
	apply		= WIDGET_BUTTON( sub_base2, VALUE = 'Set', UVALUE = 'apply', FONT = GetCorrectFont('courier2bold') )
	dismiss		= WIDGET_BUTTON( sub_base2, VALUE = 'Dismiss', UVALUE = 'dismiss', FONT = GetCorrectFont('courier2bold') )

	IF SELF.default_transform_file EQ '' THEN WIDGET_CONTROL, default, SENSITIVE = 0



;;;ckt,sep2004
;;;ckt,sep2004
	IF SELF.vm_used THEN WIDGET_CONTROL, editor, SENSITIVE = 0


	WIDGET_CONTROL, tlb, DEFAULT_BUTTON = dismiss
	WIDGET_CONTROL, tlb, /REALIZE

	info_ptr	= PTR_NEW( {								$
					trans_txt		: txt1,				$
					info_txt		: txt2,				$
					transform_file2use	: SELF.current_transform_file,	$
					obj			: SELF,				$
					transform_file_lbl	: transform_file_lbl2,		$
					editor_base		: (-1L),			$
					update			: 0				$
					}, /NO_COPY )

	WIDGET_CONTROL, tlb, SET_UVALUE = info_ptr
	XMANAGER, 'Transformation Information Interface', tlb, EVENT_HANDLER = 'DisplayTransformInfo_eh'
	update_flag	= (*info_ptr).update
	IF WIDGET_INFO((*info_ptr).editor_base, /VALID_ID) THEN WIDGET_CONTROL, (*info_ptr).editor_base, /DESTROY

	PTR_FREE, info_ptr

	RETURN, update_flag
END
; IMAGE_DATA::DisplayTransformInfo

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::TransformData @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::TransformData, source_data


;;;ckt,sep2004
;;;ckt,sep2004
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		msg	= DIALOG_MESSAGE( [					$
					'Problem with current transform',	$
					'Returning data without performing',	$
					'transform.  Transform will be',	$
					'cleared from memory to avoid',		$
					'having this message reappear.' ], /ERROR )
		PTR_FREE, SELF.data_transform_formula_ptr
		PTR_FREE, SELF.data_transform_info_ptr
		RETURN, source_data
	ENDIF




;help,source_data
size_sourcedata	= SIZE(source_data)
;print,'size_sourcedata = ',size_sourcedata
	IF NOT PTR_VALID(SELF.data_transform_formula_ptr) THEN RETURN, source_data
	IF N_ELEMENTS(*(SELF.data_transform_formula_ptr)) LE 1 AND			$
		STRLEN(STRTRIM((*(SELF.data_transform_formula_ptr))[0],2)) LE 0 THEN RETURN, source_data



;;;ckt,sep2004
;;;ckt,sep2004
	IF SELF.vm_used THEN BEGIN
		sep1		= STR_SEP(SELF.current_transform_file,PATH_SEP())
		sep2		= STR_SEP(sep1[N_ELEMENTS(sep1)-1],'.')
		trans_func	= sep2[0]+'_'+sep2[1]
;res=dialog_message(['transfunc = '+trans_func],/information)
		transformed_data= CALL_FUNCTION(trans_func,SELF,source_data)
	ENDIF ELSE BEGIN


	transform_lines	= *(SELF.data_transform_formula_ptr)

	n_lines		= N_ELEMENTS(transform_lines)
	transform_good	= 1
	line_ctr	= 0

	WHILE transform_good AND line_ctr LT n_lines DO BEGIN
		str2exec	= transform_lines[line_ctr]
;print,'str2exec=',str2exec
		success		= EXECUTE(str2exec)
		IF NOT success THEN transform_good = 0
		line_ctr	= line_ctr + 1
	ENDWHILE

	IF NOT transform_good THEN BEGIN
		msg	= DIALOG_MESSAGE( [					$
					'Problem with current transform',	$
					'Returning data without performing',	$
					'transform.  Transform will be',	$
					'cleared from memory to avoid',		$
					'having this message reappear.' ], /ERROR )
		PTR_FREE, SELF.data_transform_formula_ptr
		PTR_FREE, SELF.data_transform_info_ptr
;PRINT,'+++++++++ PTR_VALID(SELF.data_transform_formula_ptr)? = ',PTR_VALID(SELF.data_transform_formula_ptr)
;PRINT,'+++++++++ PTR_VALID(SELF.data_transform_info_ptr)? = ',PTR_VALID(SELF.data_transform_info_ptr)
		RETURN, source_data
	ENDIF


;;;ckt,sep2004
;;;ckt,sep2004
	ENDELSE




size_transdata	= SIZE(transformed_data)
;print,'size_transdata = ',size_transdata
;
; check to see if dimensions of transformed data are the same as source data
; if not, print message and return source data
;
;;help,transformed_data
msg	= ['']

IF N_ELEMENTS(size_sourcedata) NE N_ELEMENTS(size_transdata) AND N_ELEMENTS(transformed_data) NE 1 AND N_ELEMENTS(source_data) NE 1 THEN BEGIN

	msg	= [								$
			'Current transform changes the dimensions of the',	$
			'input data; this is not allowed.  Data returned',	$
			'will be the original source data and the transform',	$
			'will be cleared from memory so that this message',	$
			'does not reappear.' ]
ENDIF

IF msg[0] EQ '' AND N_ELEMENTS(transformed_data) NE 1 AND N_ELEMENTS(source_data) NE 1 THEN BEGIN
	done	= 0
	i	= 0
	WHILE i LE size_sourcedata[0] DO BEGIN
;help,size_sourcedata
;print,'i=',i
		IF size_sourcedata[i] NE size_transdata[i] AND NOT done THEN	BEGIN
			msg	= [								$
				'Current transform changes the dimensions of the',	$
				'input data; this is not allowed.  Data returned',	$
				'will be the original source data and the transform',	$
				'will be cleared from memory so that this message',	$
				'does not reappear.' ]
			done	= 1
		ENDIF
		i	= i + 1
	ENDWHILE
ENDIF

IF msg[0] NE '' THEN BEGIN
	res	= DIALOG_MESSAGE( msg, /ERROR )
	PTR_FREE, SELF.data_transform_formula_ptr
	PTR_FREE, SELF.data_transform_info_ptr
	RETURN, source_data
ENDIF

	RETURN, transformed_data
END
; IMAGE_DATA::TransformData

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::Return_Display_Struct @@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::Return_Display_Struct,						$
					display_lower_left_gh,				$
					display_lower_left_gv,				$
					display_upper_right_gh,				$
					display_upper_right_gv,				$
					display_size_h,					$
					display_size_v,					$
					zoom_factor

;print,'*(SELF.global_h_off_stack_ptr)=',*(SELF.global_h_off_stack_ptr)
;print,'display_lower_left_gh=',display_lower_left_gh
;print,'*(SELF.global_v_off_stack_ptr)=',*(SELF.global_v_off_stack_ptr)
;print,'display_lower_left_gv=',display_lower_left_gv
;print,'*(SELF.global_h_off_stack_ptr)=',*(SELF.global_h_off_stack_ptr)
;print,'SELF.slice_size_h=',SELF.slice_size_h
;print,'display_upper_right_gh=',display_upper_right_gh
;print,'*(SELF.global_v_off_stack_ptr)=',*(SELF.global_v_off_stack_ptr)
;print,'display_upper_right_gv=',display_upper_right_gv
;print,'SELF.slice_size_v=',SELF.slice_size_v

IF display_lower_left_gh GT display_upper_right_gh THEN display_lower_left_gh = display_upper_right_gh
IF display_lower_left_gv GT display_upper_right_gv THEN display_lower_left_gv = display_upper_right_gv


	;==============================================================
	; Look for one of the following four conditions
	;==============================================================
	slice_idx	= WHERE(							$
				;======================================
				; CHECK #1
				;======================================
				(							$
				;======================================
				; slice_lower_left_horizontal_coord LT
				; display_lower_left_horizontal_coord
				;======================================
				*(SELF.global_h_off_stack_ptr) LT			$
				display_lower_left_gh AND				$
				;======================================
				; slice_lower_left_vertical_coord LT
				; display_lower_left_vertical_coord
				;======================================
				*(SELF.global_v_off_stack_ptr) LT			$
				display_lower_left_gv AND				$
				;======================================
				; slice_lower_left_horizontal_coord +
				; slice_size_horizontal GT
				; display_lower_left_horizontal_coord
				;======================================
				*(SELF.global_h_off_stack_ptr) + SELF.slice_size_h GT	$
				display_lower_left_gh AND				$
				;======================================
				; slice_lower_left_vertical_coord +
				; slice_size_vertical GT
				; display_lower_left_vertical_coord
				;======================================
				*(SELF.global_v_off_stack_ptr) + SELF.slice_size_v GT	$
				display_lower_left_gv ) OR				$
				;======================================
				; CHECK #2
				;======================================
				(							$
				;======================================
				; slice_lower_left_horizontal_coord GE
				; display_lower_left_horizontal_coord
				;======================================
				*(SELF.global_h_off_stack_ptr) GE			$
				display_lower_left_gh AND				$
				;======================================
				; slice_lower_left_vertical_coord LT
				; display_lower_left_vertical_coord
				;======================================
				*(SELF.global_v_off_stack_ptr) LT			$
				display_lower_left_gv AND				$
				;======================================
				; slice_lower_left_horizontal_coord LT
				; display_lower_left_horizontal_coord +
				; display_size_horizontal
				;======================================
				*(SELF.global_h_off_stack_ptr) LT			$
				display_lower_left_gh + display_size_h AND		$
				;======================================
				; slice_lower_left_vertical_coord +
				; slice_size_vertical GT
				; display_lower_left_vertical_coord
				;======================================
				*(SELF.global_v_off_stack_ptr) + SELF.slice_size_v GT	$
				display_lower_left_gv ) OR				$
				;======================================
				; CHECK #3
				;======================================
				(							$
				;======================================
				; slice_lower_left_horizontal_coord LT
				; display_lower_left_horizontal_coord
				;======================================
				*(SELF.global_h_off_stack_ptr) LT			$
				display_lower_left_gh AND				$
				;======================================
				; slice_lower_left_vertical_coord GE
				; display_lower_left_vertical_coord
				;======================================
				*(SELF.global_v_off_stack_ptr) GE			$
				display_lower_left_gv AND				$
				;======================================
				; slice_lower_left_horizontal_coord +
				; slice_size_horizontal GT
				; display_lower_left_horizontal_coord
				;======================================
				*(SELF.global_h_off_stack_ptr) + SELF.slice_size_h GT	$
				display_lower_left_gh AND				$
				;======================================
				; slice_lower_left_vertical_coord LT
				; display_lower_left_vertical_coord +
				; display_size_vertical
				;======================================
				*(SELF.global_v_off_stack_ptr) LT			$
				display_lower_left_gv + display_size_v ) OR		$
				;======================================
				; CHECK #4
				;======================================
				(							$
				;======================================
				; slice_lower_left_horizontal_coord GE
				; display_lower_left_horizontal_coord
				;======================================
				*(SELF.global_h_off_stack_ptr) GE			$
				display_lower_left_gh AND				$
				;======================================
				; slice_lower_left_vertical_coord GE
				; display_lower_left_vertical_coord
				;======================================
				*(SELF.global_v_off_stack_ptr) GE			$
				display_lower_left_gv AND				$
				;======================================
				; slice_lower_left_horizontal_coord LT
				; display_lower_left_horizontal_coord +
				; display_size_horizontal
				;======================================
				*(SELF.global_h_off_stack_ptr) LT			$
				display_lower_left_gh + display_size_h AND		$
				;======================================
				; slice_lower_left_vertical_coord LT
				; display_lower_left_vertical_coord +
				; display_size_vertical
				;======================================
				*(SELF.global_v_off_stack_ptr) LT			$
				display_lower_left_gv + display_size_v ), slice_cnt )

	;==============================================================
	; If no slices are to be displayed, return empty structure
	;==============================================================
;print,'slice_cnt = ',slice_cnt
	IF slice_cnt LE 0 THEN BEGIN
		RETURN, {														$
			slice_idx			: (-1L),									$
			n_slices			: 0L,										$
			slice_h_start			: 0L,										$
			slice_h_end			: 0L,										$
			slice_v_start			: 0L,										$
			slice_v_end			: 0L,										$
			slice_h_lower_left_display_pos	: 0L,										$
			slice_v_lower_left_display_pos	: 0L,										$
			tile_inc_h			: 0L,										$
			tile_inc_v			: 0L,										$
			tile_out_hs			: 0L,										$
			tile_out_he			: 0L,										$
			tile_out_vs			: 0L,										$
			tile_out_ve			: 0L,										$
			tile_out_hsz			: 0L,										$
			tile_out_vsz			: 0L,										$
			out_h_sz			: LONG(FLOAT(display_upper_right_gh-display_lower_left_gh+1)*zoom_factor),	$
			out_v_sz			: LONG(FLOAT(display_upper_right_gv-display_lower_left_gv+1)*zoom_factor) }
	ENDIF


	slice_h_lower_left_display_pos	= LONARR( slice_cnt )
	slice_v_lower_left_display_pos	= LONARR( slice_cnt )
	slice_h_start			= LONARR( slice_cnt )
	slice_h_end			= LONARR( slice_cnt ) + SELF.slice_size_h - 1L
	slice_v_start			= LONARR( slice_cnt )
	slice_v_end			= LONARR( slice_cnt ) + SELF.slice_size_v - 1L

	;==============================================================
	; Calculate starting slice pixel horizontally
	;==============================================================
	diff_h	= (*(SELF.global_h_off_stack_ptr))[ slice_idx ] - display_lower_left_gh
	idx1	= WHERE( diff_h LT 0L, cnt1 )
	idx2	= WHERE( diff_h GT 0L, cnt2 )

	IF cnt1 GT 0 THEN slice_h_start[ idx1 ]				= ABS( diff_h[ idx1 ] )
	IF cnt2 GT 0 THEN slice_h_lower_left_display_pos[ idx2 ]	= diff_h[ idx2 ]

	;==============================================================
	; Calculate starting slice pixel vertically
	;==============================================================
	diff_v	= (*(SELF.global_v_off_stack_ptr))[ slice_idx ] - display_lower_left_gv
	idx1	= WHERE( diff_v LT 0L, cnt1 )
	idx2	= WHERE( diff_v GT 0L, cnt2 )

	IF cnt1 GT 0 THEN slice_v_start[ idx1 ]				= ABS( diff_v[ idx1 ] )
	IF cnt2 GT 0 THEN slice_v_lower_left_display_pos[ idx2 ]	= diff_v[ idx2 ]
;print,''
;print,'========================================================='
;print,'IMAGE_DATA::Return_Display_Struct:'
;print,''
;print,' display_lower_left_gv = '
;print, display_lower_left_gv
;print,''
;print,' (*(SELF.global_v_off_stack_ptr))[ slice_idx ] ='
;print,(*(SELF.global_v_off_stack_ptr))[ slice_idx ]
;print,''
;print,' diff_v = '
;print, diff_v
;print,''
;print,'========================================================='
;print,''

	;==============================================================
	; Calculate ending slice pixel horizontally
	;==============================================================
	diff_h	= (*(SELF.global_h_off_stack_ptr))[ slice_idx ] + (SELF.slice_size_h - 1)	$
			- display_upper_right_gh
	idx1	= WHERE( diff_h GT 0L, cnt1 )

	IF cnt1 GT 0 THEN slice_h_end[ idx1 ] =						$
		slice_h_end[ idx1 ] - diff_h[ idx1 ]

	;==============================================================
	; Calculate ending slice pixel vertically
	;==============================================================
	diff_v	= (*(SELF.global_v_off_stack_ptr))[ slice_idx ] + (SELF.slice_size_v - 1)	$
			- display_upper_right_gv
	idx1	= WHERE( diff_v GT 0L, cnt1 )

	IF cnt1 GT 0 THEN slice_v_end[ idx1 ] =						$
		slice_v_end[ idx1 ] - diff_v[ idx1 ]
;print,''
;print,'========================================================='
;print,'IMAGE_DATA::Return_Display_Struct:'
;print,''
;print,' display_upper_right_gv = '
;print, display_upper_right_gv
;print,''
;print,' (*(SELF.global_v_off_stack_ptr))[ slice_idx ] ='
;print,(*(SELF.global_v_off_stack_ptr))[ slice_idx ]
;print,''
;print,' diff_v = '
;print, diff_v
;print,''
;print,'========================================================='
;print,''
;print,''
;print,'========================================================='
;print,'IMAGE_DATA::Return_Display_Struct:'
;print,''
;print,'slice_h_start = '
;print,slice_h_start
;print,''
;print,'slice_v_start = '
;print,slice_v_start
;print,''
;print,'slice_h_end = '
;print,slice_h_end
;print,''
;print,'slice_v_end = '
;print,slice_v_end
;print,''
;print,'zoom_factor = '
;print,zoom_factor
;print,''
;print,'slice_h_lower_left_display_pos = '
;print,slice_h_lower_left_display_pos
;print,''
;print,'slice_v_lower_left_display_pos = '
;print,slice_v_lower_left_display_pos
;print,''
;print,'========================================================='
;print,''

	tile_inc_h	= FIX(FLOAT( slice_h_end - slice_h_start ) * zoom_factor) + MAX( [ (ROUND(zoom_factor) - 1L), 0L ] )
	tile_inc_v	= FIX(FLOAT( slice_v_end - slice_v_start ) * zoom_factor) + MAX( [ (ROUND(zoom_factor) - 1L), 0L ] )

	tile_out_hs	= FIX( FLOAT( slice_h_lower_left_display_pos ) * zoom_factor )
	tile_out_he	= tile_out_hs + tile_inc_h
	tile_out_vs	= FIX( FLOAT( slice_v_lower_left_display_pos ) * zoom_factor )
	tile_out_ve	= tile_out_vs + tile_inc_v

	tile_out_hsz	= tile_out_he - tile_out_hs + 1L
	tile_out_vsz	= tile_out_ve - tile_out_vs + 1L

	max_h_pos_idx	= WHERE( tile_out_hs + tile_inc_h EQ MAX( tile_out_hs + tile_inc_h ) )
	max_v_pos_idx	= WHERE( tile_out_vs + tile_inc_v EQ MAX( tile_out_vs + tile_inc_v ) )
	out_h_sz	= tile_out_hs[ max_h_pos_idx[0] ] + tile_inc_h[ max_h_pos_idx[0] ] + 1L
	out_v_sz	= tile_out_vs[ max_v_pos_idx[0] ] + tile_inc_v[ max_v_pos_idx[0] ] + 1L

;print,'slice_idx = ',slice_idx
;print,''
;print,'========================================================='
;print,'IMAGE_DATA::Return_Display_Struct:'
;print,''
;print,'tile_inc_h = '
;print,tile_inc_h
;print,''
;print,'tile_inc_v = '
;print,tile_inc_v
;print,''
;print,'tile_out_hs = '
;print,tile_out_hs
;print,''
;print,'tile_out_he = '
;print,tile_out_he
;print,''
;print,'tile_out_vs = '
;print,tile_out_vs
;print,''
;print,'tile_out_ve = '
;print,tile_out_ve
;print,''
;print,'out_h_sz = '
;print,out_h_sz
;print,''
;print,'out_v_sz = '
;print,out_v_sz
;print,''
;print,'tile_out_hs = '
;print,tile_out_hs
;print,''
;print,'tile_out_he = '
;print,tile_out_he
;print,''
;print,'tile_out_vs = '
;print,tile_out_vs
;print,''
;print,'tile_out_ve = '
;print,tile_out_ve
;print,''
;print,'========================================================='
;print,''
	RETURN, {										$
			slice_idx			: slice_idx,				$
			n_slices			: slice_cnt,				$
			slice_h_start			: slice_h_start,			$
			slice_h_end			: slice_h_end,				$
			slice_v_start			: slice_v_start,			$
			slice_v_end			: slice_v_end,				$
			slice_h_lower_left_display_pos	: slice_h_lower_left_display_pos,	$
			slice_v_lower_left_display_pos	: slice_v_lower_left_display_pos,	$
			tile_inc_h			: tile_inc_h,				$
			tile_inc_v			: tile_inc_v,				$
			tile_out_hs			: tile_out_hs,				$
			tile_out_he			: tile_out_he,				$
			tile_out_vs			: tile_out_vs,				$
			tile_out_ve			: tile_out_ve,				$
			tile_out_hsz			: tile_out_hsz,				$
			tile_out_vsz			: tile_out_vsz,				$
			out_h_sz			: out_h_sz,				$
			out_v_sz			: out_v_sz }
END
; IMAGE_DATA::Return_Display_Struct

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::ReturnUnbytescaledTile @@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnUnbytescaledTile, stack_idx
	; min_val_to_byte and max_val_to_byte are already calculated, as
	; is indices for data less than min_val_to_byte, indices for
	; data greater than max_val_to_byte, and indices for missing
	; data
	RETURN, SELF->TransformData((*(SELF.img_stack_ptr))[*,*,stack_idx])

END
; IMAGE_DATA::ReturnUnbytescaledTile

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::ReturnUnbytescaledImage @@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnUnbytescaledImage,				$
				global_image_coord_xs,			$
				global_image_coord_xe,			$
				global_image_coord_ys,			$
				global_image_coord_ye,			$
				view_width,				$
				view_height,				$
				DATA_TYPE = data_type,			$
				BACKGROUND_VALUE = back_val,		$
				NATIVE_RES = native_res
	;
	; The keyword NATIVE_RES takes precedence over all other keywords
	; related to output image size IF zoom_factor is calculated to be 1.0


	in_h		= global_image_coord_xe - global_image_coord_xs + 1L
	in_v		= global_image_coord_ye - global_image_coord_ys + 1L

	zoom_factor	= FLOAT( view_width ) / FLOAT( in_h )

	ret		= SELF->Return_Display_Struct(			$
					global_image_coord_xs,		$
					global_image_coord_ys,		$
					global_image_coord_xe,		$
					global_image_coord_ye,		$
					in_h,				$
					in_v,				$
					zoom_factor )

	IF KEYWORD_SET( data_type ) THEN BEGIN

		CASE 1 OF
			data_type EQ 1	: unscaled_img = BYTARR(	ret.out_h_sz, ret.out_v_sz )
			data_type EQ 2	: unscaled_img = INTARR(	ret.out_h_sz, ret.out_v_sz )
			data_type EQ 3	: unscaled_img = LONARR(	ret.out_h_sz, ret.out_v_sz )
			data_type EQ 4	: unscaled_img = FLTARR(	ret.out_h_sz, ret.out_v_sz )
			data_type EQ 5	: unscaled_img = DBLARR(	ret.out_h_sz, ret.out_v_sz )
			data_type EQ 6	: unscaled_img = COMPLEXARR(	ret.out_h_sz, ret.out_v_sz )
			data_type EQ 7	: unscaled_img = STRARR(	ret.out_h_sz, ret.out_v_sz )
			data_type EQ 9	: unscaled_img = DCOMPLEXARR(	ret.out_h_sz, ret.out_v_sz )
			data_type EQ 12	: unscaled_img = UINTARR(	ret.out_h_sz, ret.out_v_sz )
			data_type EQ 13	: unscaled_img = ULONARR(	ret.out_h_sz, ret.out_v_sz )
			data_type EQ 14	: unscaled_img = LON64ARR(	ret.out_h_sz, ret.out_v_sz )
			data_type EQ 15	: unscaled_img = ULON64ARR(	ret.out_h_sz, ret.out_v_sz )
		ENDCASE

	ENDIF ELSE BEGIN

		unscaled_img = BYTARR(	ret.out_h_sz, ret.out_v_sz )

	ENDELSE

	back_val_keyword_set	= 0

	IF KEYWORD_SET(back_val) THEN BEGIN
		unscaled_img	= unscaled_img + back_val
		back_val_keyword_set	= 1
		backval2use		= back_val
	ENDIF

	FOR i = 0, ret.n_slices - 1 DO											$
		unscaled_img[ ret.tile_out_hs[i]:ret.tile_out_he[i], ret.tile_out_vs[i]:ret.tile_out_ve[i] ]	=	$
			unscaled_img[ ret.tile_out_hs[i]:ret.tile_out_he[i], ret.tile_out_vs[i]:ret.tile_out_ve[i] ] +	$
			CONGRID( (SELF->ReturnUnbytescaledTile( ret.slice_idx[i] ))[ ret.slice_h_start[i]:ret.slice_h_end[i],	$
			ret.slice_v_start[i]:ret.slice_v_end[i] ], ret.tile_out_hsz[i], ret.tile_out_vsz[i] )


	IF KEYWORD_SET(native_res) AND zoom_factor EQ 1.0 AND SELF.original_data_size_factor NE 1.0 THEN BEGIN
		tmpsz	= SIZE( unscaled_img )
		new_xsize	= FLOAT(tmpsz[1]) / SELF.original_data_size_factor
		new_ysize	= FLOAT(tmpsz[2]) / SELF.original_data_size_factor
		IF ROUND(new_xsize) NE new_xsize OR ROUND(new_ysize) NE new_ysize THEN BEGIN
			msg	= [											$
					'Cannot create an image for the current dataset',				$
					'because at least one of the individual tiles that',				$
					'make up the image has horizontal and/or vertical',				$
					'offset values that are less than 1.0 for the data',				$
					'in its native resolution.  The data will be saved out',			$
					'to an image with dimensions ' + STRTRIM( CEIL(new_xsize),2),			$
					'wide by ' + STRTRIM(CEIL(new_ysize),2) + ' high.  Please note that the',	$
					'actual dimensions of the image should be ' + STRTRIM( new_xsize, 2 ),		$
					'wide by ' + STRTRIM(new_ysize,2) + 'high' ]
			ret	= DIALOG_MESSAGE( msg, /INFORMATION )
		ENDIF
		unscaled_img	= CONGRID( TEMPORARY(unscaled_img), CEIL(new_xsize), CEIL(new_ysize) )
	ENDIF

	RETURN, unscaled_img
END
; IMAGE_DATA::ReturnUnbytescaledImage

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::ReturnScalingParameters @@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnScalingParameters
	RETURN, SELF.scaling_params
END
; IMAGE_DATA::ReturnScalingParameters

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::ReturnByteScaledTile @@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnByteScaledTile, stack_idx
	; min_val_to_byte and max_val_to_byte are already calculated, as
	; is indices for data less than min_val_to_byte, indices for
	; data greater than max_val_to_byte, and indices for missing
	; data


	exponent	= 1.0D
;print,'SELF.scaling_params.gamma = ',SELF.scaling_params.gamma
	IF SELF.scaling_params.gamma GT 0 THEN	$
		exponent	= 1.0D / SELF.scaling_params.gamma

	spread		=						$
		SELF.scaling_params.max_val_to_byte -			$
		SELF.scaling_params.min_val_to_byte

	IF spread EQ 0 THEN spread = 1

	mask255	= NOT( (*(SELF.scaling_params.above_mask_ptr))[*,*,stack_idx] ) * 255B

;print,'IMAGE_DATA::ReturnByteScaledTile -- SELF.scaling_params.min_val_to_byte = ',SELF.scaling_params.min_val_to_byte
;print,'IMAGE_DATA::ReturnByteScaledTile -- SELF.scaling_params.max_val_to_byte = ',SELF.scaling_params.max_val_to_byte
	ByteScaledTile = 							$
		( BYTSCL( ((( SELF->TransformData(((*(SELF.img_stack_ptr))[*,*,stack_idx])) -		$
			SELF.scaling_params.min_val_to_byte) /			$
			spread) *						$
			(*(SELF.scaling_params.below_mask_ptr))[*,*,stack_idx]) ^ exponent * spread +		$
			SELF.scaling_params.min_val_to_byte,			$
			MIN = SELF.scaling_params.min_val_to_byte,		$
			MAX = SELF.scaling_params.max_val_to_byte ) *		$
			(*(SELF.scaling_params.above_mask_ptr))[*,*,stack_idx] + mask255 ) *	$
			(*(SELF.scaling_params.missing_mask_ptr))[*,*,stack_idx]

	RETURN, ByteScaledTile

END
; IMAGE_DATA::ReturnByteScaledTile


;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@ IMAGE_DATA::ReturnUnbytescaledStackStructure @@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnUnbytescaledStackStructure, RESOLUTION = newRes, NATIVE_RES = native_res
;
; If both RESOLUTION and NATIVE_RES keywords are set, NATIVE_RES takes precedence
;

	IF KEYWORD_SET( newRes ) THEN BEGIN
;print,''
;print,'keyword newRes is set'
		;-----------------------------------------------------
		; If data is being sized down use Cubic interpolation
		; in CONGRID, and use Nearest Neighbor for sizing up.
		;-----------------------------------------------------
		IF newRes[0] LT SELF.slice_size_h $
		OR newRes[1] LT SELF.slice_size_v THEN $
			ex = 'CUBIC = -0.5'
	ENDIF ELSE BEGIN
		newRes		= [SELF.slice_size_h,SELF.slice_size_v]
	ENDELSE

	native_res_keyword_set	= 0
	IF KEYWORD_SET( native_res ) THEN BEGIN
		native_res_keyword_set	= 1
		newRes		= [								$
					SELF.slice_size_h / SELF.original_data_size_factor,	$
					SELF.slice_size_v / SELF.original_data_size_factor ]
	ENDIF
;print,'--------------->>>>>>>>>> newRes=',newRes
;print,''
;print,'SELF->GetNumberType() = ',SELF->GetNumberType()
	CASE SELF->GetNumberType() OF
		1	: unscaledStack = BYTARR( SELF.n_slices, newRes[0], newRes[1] )
		2	: unscaledStack = INTARR( SELF.n_slices, newRes[0], newRes[1] )
		3	: unscaledStack = LONARR( SELF.n_slices, newRes[0], newRes[1] )
		4	: unscaledStack = FLTARR( SELF.n_slices, newRes[0], newRes[1] )
		5	: unscaledStack = DBLARR( SELF.n_slices, newRes[0], newRes[1] )
		6	: unscaledStack = COMPLEXARR( SELF.n_slices, newRes[0], newRes[1] )
		7	: unscaledStack = STRARR( SELF.n_slices, newRes[0], newRes[1] )
		9	: unscaledStack = DCOMPLEXARR( SELF.n_slices, newRes[0], newRes[1] )
		12	: unscaledStack = UINTARR( SELF.n_slices, newRes[0], newRes[1] )
		13	: unscaledStack = ULONARR( SELF.n_slices, newRes[0], newRes[1] )
		14	: unscaledStack = LON64ARR( SELF.n_slices, newRes[0], newRes[1] )
		15	: unscaledStack = ULON64ARR( SELF.n_slices, newRes[0], newRes[1] )
	ENDCASE

	h_offsets	= LONARR( SELF.n_slices )
	v_offsets	= LONARR( SELF.n_slices )

	IF native_res_keyword_set THEN BEGIN
		h_offsets	= FLOAT(h_offsets)
		v_offsets	= FLOAT(v_offsets)
	ENDIF

	;-----------------------------------------------------
	; Run CONGRID regardless.  This simplifies the code
	; and shouldn't take any more time if newRes is equal
	; to the native resolution.
	; Also, scale the offsets to match the new resolution
	; (scale factor goes to 1.0 when there's no change.)
	;-----------------------------------------------------
	FOR slice = 0, SELF.n_slices - 1 DO BEGIN
;print,''
;print,'newRes[0], newRes[1] = ',newRes[0],newres[1]
;print,''
		unscaledStack[slice,*,*]	= CONGRID( SELF->ReturnUnbytescaledTile( slice ), newRes[0], newRes[1], _EXTRA = ex )
		h_offsets[slice]	= (*(SELF.global_h_off_stack_ptr))[slice] * newRes[0] / SELF.slice_size_h
		v_offsets[slice]	= (*(SELF.global_v_off_stack_ptr))[slice] * newRes[1] / SELF.slice_size_v
	ENDFOR

;print,'SELF.n_slices = ',SELF.n_slices
;print,'(*(SELF.global_h_off_stack_ptr)) = ',(*(SELF.global_h_off_stack_ptr))
;print,'(*(SELF.global_v_off_stack_ptr)) = ',(*(SELF.global_v_off_stack_ptr))
;print,'h_offsets = ',h_offsets
;print,'v_offsets = ',v_offsets


;help,unscaledStack
;print,''
	RETURN, {				$
		unscaledStack : unscaledStack,	$
		h_offsets : h_offsets,		$
		v_offsets : v_offsets		$
		}
END
; IMAGE_DATA::ReturnUnbytescaledStackStructure

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@ IMAGE_DATA::ReturnOriginalSizeFactor @@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnOriginalSizeFactor
	RETURN, SELF.original_data_size_factor
END
; IMAGE_DATA::ReturnOriginalSizeFactor

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::ReturnByteScaledStackStructure @@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnByteScaledStackStructure, RESOLUTION = newRes, NATIVE_RES = native_res

	IF KEYWORD_SET( newRes ) THEN BEGIN
;print,''
;print,'keyword newRes is set'
		;-----------------------------------------------------
		; If data is being sized down use Cubic interpolation
		; in CONGRID, and use Nearest Neighbor for sizing up.
		;-----------------------------------------------------
		IF newRes[0] LT SELF.slice_size_h $
		OR newRes[1] LT SELF.slice_size_v THEN $
			ex = 'CUBIC = -0.5'
	ENDIF ELSE BEGIN
		newRes = [SELF.slice_size_h,SELF.slice_size_v]
	ENDELSE

	native_res_keyword_set	= 0
	IF KEYWORD_SET( native_res ) THEN BEGIN
		native_res_keyword_set	= 1
		newRes		= [								$
					SELF.slice_size_h / SELF.original_data_size_factor,	$
					SELF.slice_size_v / SELF.original_data_size_factor ]
	ENDIF

	scaledStack	= BYTARR( SELF.n_slices, newRes[0], newRes[1] )
	h_offsets	= LONARR( SELF.n_slices )
	v_offsets	= LONARR( SELF.n_slices )
	IF native_res_keyword_set THEN BEGIN
		h_offsets	= FLOAT(h_offsets)
		v_offsets	= FLOAT(v_offsets)
	ENDIF

	;-----------------------------------------------------
	; Run CONGRID regardless.  This simplifies the code
	; and shouldn't take any more time if newRes is equal
	; to the native resolution.
	; Also, scale the offsets to match the new resolution
	; (scale factor goes to 1.0 when there's no change.)
	;-----------------------------------------------------
	FOR slice = 0, SELF.n_slices - 1 DO BEGIN
;print,''
;print,'newRes[0], newRes[1] = ',newRes[0],newres[1]
;print,''
		scaledStack[slice,*,*]	= CONGRID( SELF->ReturnByteScaledTile( slice ), newRes[0], newRes[1], _EXTRA = ex )
		h_offsets[slice]	= (*(SELF.global_h_off_stack_ptr))[slice] * newRes[0] / SELF.slice_size_h
		v_offsets[slice]	= (*(SELF.global_v_off_stack_ptr))[slice] * newRes[1] / SELF.slice_size_v
	ENDFOR

;print,'SELF.n_slices = ',SELF.n_slices
;print,'(*(SELF.global_h_off_stack_ptr)) = ',(*(SELF.global_h_off_stack_ptr))
;print,'(*(SELF.global_v_off_stack_ptr)) = ',(*(SELF.global_v_off_stack_ptr))
;print,'h_offsets = ',h_offsets
;print,'v_offsets = ',v_offsets


	RETURN, {				$
		scaledStack : scaledStack,	$
		h_offsets : h_offsets,		$
		v_offsets : v_offsets		$
		}

END
; IMAGE_DATA::ReturnByteScaledStackStructure

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::ReturnByteScaledImage @@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnByteScaledImage,				$
				global_image_coord_xs,			$
				global_image_coord_xe,			$
				global_image_coord_ys,			$
				global_image_coord_ye,			$
				view_width,				$
				view_height,		$
				NATIVE_RES = native_res

	;
	; The keyword NATIVE_RES takes precedence over all other keywords
	; related to output image size IF zoom_factor is calculated to be 1.0

	in_h		= global_image_coord_xe - global_image_coord_xs + 1L
	in_v		= global_image_coord_ye - global_image_coord_ys + 1L
;print,'global_image_coord_xe=',global_image_coord_xe
;print,'global_image_coord_xs=',global_image_coord_xs
;print,'global_image_coord_ye=',global_image_coord_ye
;print,'global_image_coord_ys=',global_image_coord_ys
;print,'view_width=',view_width

	zoom_factor	= FLOAT( view_width ) / FLOAT( in_h )

	ret		= SELF->Return_Display_Struct(			$
					global_image_coord_xs,		$
					global_image_coord_ys,		$
					global_image_coord_xe,		$
					global_image_coord_ye,		$
					in_h,				$
					in_v,				$
					zoom_factor )
;print,'======================'
;print,'IMAGE_DATA::ReturnByteScaledImage: ret.out_h_sz, ret.out_v_sz = ',ret.out_h_sz, ret.out_v_sz
;print,'======================'
	scaled_img	= BYTARR( ret.out_h_sz, ret.out_v_sz )

	IF ret.n_slices LE 0 THEN RETURN, scaled_img

	FOR i = 0, ret.n_slices - 1 DO BEGIN
;if ret.tile_out_he[i] GE ret.out_h_sz OR ret.tile_out_ve[i] GE ret.out_v_sz then begin
;print,'--------------------- i = ',i
;print,'ret.out_h_sz=',ret.out_h_sz
;print,'ret.out_v_sz=',ret.out_v_sz
;print,'zoom_factor = ',zoom_factor
;print,'ret.tile_out_hs=',ret.tile_out_hs[i]
;print,'ret.tile_out_he=',ret.tile_out_he[i]
;print,'ret.tile_out_vs=',ret.tile_out_vs[i]
;print,'ret.tile_out_ve=',ret.tile_out_ve[i]
;print,'ret.slice_h_start[i]=',ret.slice_h_start[i]
;print,'ret.slice_h_end[i]=',ret.slice_h_end[i]
;print,'ret.slice_v_start[i]=',ret.slice_v_start[i]
;print,'ret.slice_v_end[i]=',ret.slice_v_end[i]
;print,'ret.tile_out_hsz[i]=',ret.tile_out_hsz[i]
;print,'ret.tile_out_vsz[i]=',ret.tile_out_vsz[i]
;endif
;print,'retrieving slice #' + strtrim(string(i+1),2) + ' out of ' + strtrim(string(ret.n_slices),2)

		CASE 1 OF
			ret.slice_h_start[i] EQ ret.slice_h_end[i] AND		$
			ret.slice_v_start[i] EQ ret.slice_v_end[i]: BEGIN
				pix	= (SELF->ReturnByteScaledTile(		$
						ret.slice_idx[i] ))[		$
						ret.slice_h_start[i]:ret.slice_h_end[i],ret.slice_v_start[i]:ret.slice_v_end[i] ]
				out_pix	= BYTARR( ret.tile_out_hsz[i], ret.tile_out_vsz[i] ) + pix[0]
				END
			ret.slice_v_start[i] EQ ret.slice_v_end[i]: BEGIN
				pix	= CONGRID( (SELF->ReturnByteScaledTile(	$
						ret.slice_idx[i] ))[		$
						ret.slice_h_start[i]:ret.slice_h_end[i],ret.slice_v_start[i]:ret.slice_v_end[i] ], $
						ret.tile_out_hsz[i], ret.tile_out_vsz[i] )
				out_pix	= BYTARR( ret.tile_out_hsz[i], ret.tile_out_vsz[i] )
				FOR v = 0, ret.tile_out_vsz[i] - 1 DO out_pix[ *, v ] = pix
				END
			ELSE: BEGIN
				out_pix	= CONGRID( (SELF->ReturnByteScaledTile(	$
						ret.slice_idx[i] ))[ 		$
						ret.slice_h_start[i]:ret.slice_h_end[i],ret.slice_v_start[i]:ret.slice_v_end[i] ], $
						ret.tile_out_hsz[i], ret.tile_out_vsz[i] )
				END
		ENDCASE

		scaled_img[ ret.tile_out_hs[i]:ret.tile_out_he[i], ret.tile_out_vs[i]:ret.tile_out_ve[i] ]	=	$
			scaled_img[ ret.tile_out_hs[i]:ret.tile_out_he[i], ret.tile_out_vs[i]:ret.tile_out_ve[i] ] +	$
			out_pix
	ENDFOR

	IF KEYWORD_SET(native_res) AND zoom_factor EQ 1.0 AND SELF.original_data_size_factor NE 1.0 THEN BEGIN
		tmpsz	= SIZE( scaled_img )
		new_xsize	= FLOAT(tmpsz[1]) / SELF.original_data_size_factor
		new_ysize	= FLOAT(tmpsz[2]) / SELF.original_data_size_factor
		IF ROUND(new_xsize) NE new_xsize OR ROUND(new_ysize) NE new_ysize THEN BEGIN
			msg	= [											$
					'Cannot create an image for the current dataset',				$
					'because at least one of the individual tiles that',				$
					'make up the image has horizontal and/or vertical',				$
					'offset values that are less than 1.0 for the data',				$
					'in its native resolution.  The data will be saved out',			$
					'to an image with dimensions ' + STRTRIM( CEIL(new_xsize),2),			$
					'wide by ' + STRTRIM(CEIL(new_ysize),2) + ' high.  Please note that the',	$
					'actual dimensions of the image should be ' + STRTRIM( new_xsize, 2 ),		$
					'wide by ' + STRTRIM(new_ysize,2) + 'high' ]
			ret	= DIALOG_MESSAGE( msg, /INFORMATION )
		ENDIF
		scaled_img	= CONGRID( TEMPORARY(scaled_img), CEIL(new_xsize), CEIL(new_ysize) )
	ENDIF

	RETURN, scaled_img
END
; IMAGE_DATA::ReturnByteScaledImage

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@ IMAGE_DATA::ReturnClosestXYMatchForDualValues @@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnClosestXYMatchForDualValues, value1,		$
						img_data_obj2, value2
	;=======================================================================
	; assumption: both objects are IDENTICAL in dimensions and number of
	; slices
	;=======================================================================
	ret1		= SELF->Return_Display_Struct(			$
					0,				$
					0,				$
					SELF.global_width-1,		$
					SELF.global_height-1,		$
					SELF.global_width,		$
					SELF.global_height,		$
					1.0 )

	ret2		= SELF->Return_Display_Struct(				$
					0,					$
					0,					$
					img_data_obj2->GetImageWidth()-1,	$
					img_data_obj2->GetImageHeight()-1,	$
					img_data_obj2->GetImageWidth(),		$
					img_data_obj2->GetImageHeight(),	$
					1.0 )
	img_stack_ptr2	= img_data_obj2->ReturnSliceStackPtr()

	FOR i = 0, ret1.n_slices - 1 DO BEGIN
		diff1		= ABS( SELF->TransformData(((*(SELF.img_stack_ptr))[*,*,ret1.slice_idx[i]])) - value1 )
		diff2		= ABS( img_data_obj2->TransformData(((*(img_stack_ptr2))[*,*,ret2.slice_idx[i]])) - value2 )
		global_found	= 0
		IF i LE 0 THEN BEGIN
			global_min	= MIN( diff1 + diff2 )
			global_min_idx	= WHERE( (diff1 + diff2) EQ global_min, cnt )
			global_found	= 1
		ENDIF ELSE BEGIN
			min	= MIN( diff1 + diff2 )
			min_idx	= WHERE( (diff1 + diff2) EQ min, cnt )
			IF min LT global_min THEN BEGIN
				global_min	= min
				global_min_idx	= min_idx
				global_found	= 1
			ENDIF
		ENDELSE

		IF global_found THEN BEGIN
			slice_idx	= ret1.slice_idx[i]
			x_pos		= global_min_idx MOD SELF.slice_size_h
			y_pos		= global_min_idx / SELF.slice_size_h
		ENDIF
	ENDFOR

	;=======================================================================
	; If either of the calculated indices are 0, it may
	; indicate that the lon/lat may be OFF the image
	; (beyond its bounds).  If this is the case, check to
	; see if the input lon or lat falls between the
	; "side" lon or lat value and one pixel inward.
	;
	;=======================================================================
	onMap	= 0
	IF ( x_pos[0] LE 0 ) THEN BEGIN
	     IF (value1 GE SELF->TransformData(((*(SELF.img_stack_ptr))[*,*,slice_idx[0]])[x_pos[0],y_pos[0]]) ) AND		$
	        (value1 LT SELF->TransformData(((*(SELF.img_stack_ptr))[*,*,slice_idx[0]])[x_pos[0]+1,y_pos[0]]) ) THEN		$
	        onMap = 1										$
	     ELSE											$
	        onMap = 0
	ENDIF

	IF ( y_pos[0] LE 0 ) THEN BEGIN
	     IF (value2 GE img_data_obj2->TransformData(((*(img_stack_ptr2))[*,*,slice_idx[0]])[x_pos[0],y_pos[0]]) ) AND			$
	        (value2 LT img_data_obj2->TransformData(((*(img_stack_ptr2))[*,*,slice_idx[0]])[x_pos[0],y_pos[0]+1]) ) THEN		$
	        onMap = 1						$
	     ELSE							$
	        onMap = 0
	ENDIF

	IF ( x_pos[0] GE SELF.slice_size_h-1 ) THEN BEGIN
	     IF (value1 LE SELF->TransformData(((*(SELF.img_stack_ptr))[*,*,slice_idx[0]])[x_pos[0],y_pos[0]]) ) AND		$
	        (value1 GT SELF->TransformData(((*(SELF.img_stack_ptr))[*,*,slice_idx[0]])[x_pos[0]-1,y_pos[0]]) ) THEN		$
	        onMap = 1						$
	     ELSE							$
	        onMap = 0
	ENDIF

	IF ( y_pos[0] GE SELF.slice_size_v-1 ) THEN BEGIN
	     IF (value2 LE img_data_obj2->TransformData(((*(img_stack_ptr2))[*,*,slice_idx[0]])[x_pos[0],y_pos[0]]) ) AND			$
	        (value2 GT img_data_obj2->TransformData(((*(img_stack_ptr2))[*,*,slice_idx[0]])[x_pos[0],y_pos[0]-1]) ) THEN		$
	        onMap = 1						$
	     ELSE							$
	        onMap = 0
	ENDIF

	IF ( NOT onMap AND						$
	     x_pos[0] GT 0 AND						$
	     x_pos[0] LT SELF.slice_size_h-1 AND	$
	     y_pos[0] GT 0 AND						$
	     y_pos[0] LT SELF.slice_size_v-1 ) THEN	$
	   onMap	= 1

	;
	; If the lon/lat pair is OFF the lon/lat imagery, return appropriate flag
	;
	IF NOT onMap THEN RETURN, [ -1L, -1L ]

	RETURN, [ (*SELF.global_h_off_stack_ptr)[slice_idx[0]] + x_pos[0],		$
		(*SELF.global_v_off_stack_ptr)[slice_idx[0]] + y_pos[0] ]
END
; IMAGE_DATA::ReturnClosestXYMatchForDualValues

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::ReturnSliceStackPtr @@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnSliceStackPtr
	RETURN, SELF.img_stack_ptr
END
;IMAGE_DATA::ReturnSliceStackPtr

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@ IMAGE_DATA::ReturnImageTileValueAtXY @@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnImageTileValueAtXY, 				$
					global_x_image_coord,		$
					global_y_image_coord
	ret	= SELF->Return_Display_Struct(				$
					global_x_image_coord,		$
					global_y_image_coord,		$
					global_x_image_coord,		$
					global_y_image_coord,		$
					SELF.global_width,		$
					SELF.global_height,		$
					1.0 )
	IF ret.slice_idx[0] LT 0 THEN BEGIN
		PRINT,'NO DATA AT ' + STRTRIM(global_x_image_coord,2) +	$
			', ' + STRTRIM(global_y_image_coord,2)
		RETURN, 0L
	ENDIF
	;==============================================================
	; slice_idx should always be a single element!
	;==============================================================
	RETURN, (*(SELF.img_stack_id_ptr))[ ret.slice_idx[0] ]

END
; IMAGE_DATA::ReturnImageTileValueAtXY

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@ IMAGE_DATA::ReturnImageTileLocationValueAtXY @@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnImageTileLocationValueAtXY, 			$
					global_x_image_coord,		$
					global_y_image_coord

	ret		= SELF->Return_Display_Struct(			$
					global_x_image_coord,		$
					global_y_image_coord,		$
					global_x_image_coord,		$
					global_y_image_coord,		$
					1,				$
					1,				$
					1.0 )
	RETURN,									$
		[ FIX( ret.slice_h_start / SELF.original_data_size_factor ),	$
		  FIX( ret.slice_v_start / SELF.original_data_size_factor ) ]
END
; IMAGE_DATA::ReturnImageTileLocationValueAtXY

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@ IMAGE_DATA::ReturnImageValueAtXY @@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::ReturnImageValueAtXY, 				$
					global_x_image_coord,		$
					global_y_image_coord
	ret		= SELF->Return_Display_Struct(			$
					global_x_image_coord,		$
					global_y_image_coord,		$
					global_x_image_coord,		$
					global_y_image_coord,		$
					1,				$
					1,				$
					1.0 )

	IF ret.n_slices LE 0 THEN RETURN, { value:0, bad_value:1 }

	in_hs	= ret.slice_h_start[0]
	in_he	= ret.slice_h_end[0]
	in_vs	= ret.slice_v_start[0]
	in_ve	= ret.slice_v_end[0]
	RETURN, { value:SELF->TransformData(((*(SELF.img_stack_ptr))[*,*,ret.slice_idx[0]])[ in_hs:in_he, in_vs:in_ve ]), bad_value:0 }
END
;IMAGE_DATA::ReturnImageValueAtXY

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::CloneObject @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::CloneObject, XSIZE = xsize, YSIZE = ysize
RETURN, 0
END
; IMAGE_DATA::CloneObject

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::ResizeImage @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO IMAGE_DATA::ResizeImage, new_x_size, new_y_size
END
; IMAGE_DATA::ResizeImage

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::CalculateHistogram @@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::CalculateHistogram,				$
				global_image_coord_xs,			$
				global_image_coord_xe,			$
				global_image_coord_ys,			$
				global_image_coord_ye,			$
				_EXTRA = e


	in_h		= global_image_coord_xe - global_image_coord_xs + 1L
	in_v		= global_image_coord_ye - global_image_coord_ys + 1L

	ret		= SELF->Return_Display_Struct(			$
					global_image_coord_xs,		$
					global_image_coord_ys,		$
					global_image_coord_xe,		$
					global_image_coord_ye,		$
					in_h,				$
					in_v,				$
					1.0 )

;------------------------------------------------------------------------------------
; NOTE: HISTOGRAM has a problem with the new IDL data types, such as LONG64 and ULONG
; UPDATE: binsize=1 (the default) can cause HISTOGRAM to generate too many bins.
; SOLUTION: set binsize correctly.  implemented?????
;------------------------------------------------------------------------------------
	FOR i = 0,ret.n_slices - 1 DO BEGIN
		in_hs	= ret.slice_h_start[ i ]
		in_he	= ret.slice_h_end[ i ]
		in_vs	= ret.slice_v_start[ i ]
		in_ve	= ret.slice_v_end[ i ]
		current_slice	= SELF->TransformData(((*(SELF.img_stack_ptr))[*,*,ret.slice_idx[i]])[ in_hs:in_he, in_vs:in_ve ])
		current_slice	= DOUBLE(current_slice)
;print,''
;print,'PTR_VALID( SELF.missing_data_value_ptr ) = ',PTR_VALID( SELF.missing_data_value_ptr )
		IF PTR_VALID( SELF.missing_data_value_ptr ) THEN BEGIN
;print,'in IMAGE_DATA, SELF.missing_data_value_ptr = ',*(SELF.missing_data_value_ptr)
			missing_mask	= ((*(SELF.scaling_params.missing_mask_ptr))[*,*,ret.slice_idx[i]])[ in_hs:in_he, in_vs:in_ve ]
;help,missing_mask
;print,'min(missing_mask),max(missing_mask) = ',min(missing_mask),max(missing_mask)
			not_missing_idx	= WHERE( missing_mask GT 0, cnt )
;print,'cnt = ',cnt
			IF cnt GT 0 THEN BEGIN
				IF i LE 0 OR SIZE(hist,/TYPE) EQ 0 THEN	hist = HISTOGRAM( current_slice[not_missing_idx], _EXTRA = e )	$
				ELSE		hist = hist + HISTOGRAM( current_slice[not_missing_idx], _EXTRA = e )
			ENDIF
		ENDIF ELSE BEGIN
			IF i LE 0 THEN	hist = HISTOGRAM( current_slice, _EXTRA = e )	$
			ELSE		hist = hist + HISTOGRAM( current_slice, _EXTRA = e )
		ENDELSE
	ENDFOR

	IF SIZE( hist, /TYPE ) EQ 0 THEN $
		hist = LONG([-1])

;help,hist
;print,'min(hist),max(hist) = ',min(hist),max(hist)
;print,''

	RETURN, hist

END
; IMAGE_DATA::CalculateHistogram

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::GetNumberType @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::GetNumberType
;;;ckt,dec1999imgptrarr	RETURN, SIZE( (*((*(SELF.img_stack_ptr))[0])), /TYPE )
;
; NOTE: Number type that is returned is determined BEFORE any transformation takes place!
;
	RETURN, SIZE( SELF->TransformData( (*(SELF.img_stack_ptr)) ), /TYPE )
END
; IMAGE_DATA::GetNumberType

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::GetTileSize @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::GetTileSize, NATIVE_RES = native_res
	IF KEYWORD_SET(native_res) THEN					$
		RETURN, [ SELF.slice_size_h/SELF.original_data_size_factor, SELF.slice_size_v/SELF.original_data_size_factor ]	$
	ELSE								$
		RETURN, [ SELF.slice_size_h, SELF.slice_size_v ]
END
; IMAGE_DATA::GetTileSize

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::GetImageWidth @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::GetImageWidth
	RETURN, SELF.global_width
END
; IMAGE_DATA::GetImageWidth

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::GetImageHeight @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::GetImageHeight
	RETURN, SELF.global_height
END
; IMAGE_DATA::GetImageHeight

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::SetDisplayGamma @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO IMAGE_DATA::SetDisplayGamma, new_value
	SELF.scaling_params.gamma	= new_value
END
; IMAGE_DATA::SetDisplayGamma

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::GetDisplayGamma @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::GetDisplayGamma
	RETURN, SELF.scaling_params.gamma
END
; IMAGE_DATA::GetDisplayGamma

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@ IMAGE_DATA::GetDisplayMinVal2Byte @@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::GetDisplayMinVal2Byte
	RETURN, SELF.scaling_params.min_val_to_byte
END
; IMAGE_DATA::GetDisplayMinVal2Byte

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@ IMAGE_DATA::GetDisplayMaxVal2Byte @@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::GetDisplayMaxVal2Byte
	RETURN, SELF.scaling_params.max_val_to_byte
END
; IMAGE_DATA::GetDisplayMaxVal2Byte

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::GetMinVal @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::GetMinVal,						$
				global_image_coord_xs,			$
				global_image_coord_xe,			$
				global_image_coord_ys,			$
				global_image_coord_ye,			$
				view_width,				$
				view_height,				$
				RECALCULATE_GLOBAL = recalc_global


	IF								$
		global_image_coord_xs EQ 0 AND				$
		global_image_coord_xe EQ SELF.global_width - 1 AND	$
		global_image_coord_ys EQ 0 AND				$
		global_image_coord_ye EQ SELF.global_height - 1 AND	$
		NOT KEYWORD_SET(recalc_global) THEN	$
		RETURN, SELF.valid_data_min

	in_h		= global_image_coord_xe - global_image_coord_xs + 1L
	in_v		= global_image_coord_ye - global_image_coord_ys + 1L

	ret		= SELF->Return_Display_Struct(			$
					global_image_coord_xs,		$
					global_image_coord_ys,		$
					global_image_coord_xe,		$
					global_image_coord_ye,		$
					in_h,				$
					in_v,				$
					1.0 )
	found_good_val	= 0
	FOR i = 0,ret.n_slices - 1 DO BEGIN
		in_hs	= ret.slice_h_start[ i ]
		in_he	= ret.slice_h_end[ i ]
		in_vs	= ret.slice_v_start[ i ]
		in_ve	= ret.slice_v_end[ i ]
		current_slice	= SELF->TransformData(((*(SELF.img_stack_ptr))[*,*,ret.slice_idx[i]])[ in_hs:in_he, in_vs:in_ve ])
		IF PTR_VALID( SELF.missing_data_value_ptr ) THEN BEGIN
			missing_mask	= ((*(SELF.scaling_params.missing_mask_ptr))[*,*,ret.slice_idx[i]])[ in_hs:in_he, in_vs:in_ve ]
			not_missing_idx	= WHERE( missing_mask GT 0, cnt )
			IF cnt GT 0 THEN BEGIN
				IF i LE 0 OR NOT found_good_val THEN min = MIN( current_slice[not_missing_idx] )	$
				ELSE		                     min = MIN( [ min, current_slice[not_missing_idx] ] )
				found_good_val	= 1
			ENDIF
		ENDIF ELSE BEGIN
			IF i LE 0 OR NOT found_good_val THEN min = MIN( current_slice )	$
			ELSE		min	= MIN( [ min, current_slice[*] ] )
			found_good_val	= 1
		ENDELSE
	ENDFOR

	IF found_good_val THEN RETURN, min

	msg	= [							$
			'could not find any valid data values in',	$
			'specified region... returning global minimum value' ]
	res	= DIALOG_MESSAGE( msg, /INFORMATION )


	RETURN, SELF.valid_data_min
END
; IMAGE_DATA::GetMinVal

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::GetMaxVal @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::GetMaxVal,						$
				global_image_coord_xs,			$
				global_image_coord_xe,			$
				global_image_coord_ys,			$
				global_image_coord_ye,			$
				view_width,				$
				view_height,				$
				RECALCULATE_GLOBAL = recalc_global


	in_h		= global_image_coord_xe - global_image_coord_xs + 1L
	in_v		= global_image_coord_ye - global_image_coord_ys + 1L

	IF								$
		global_image_coord_xs EQ 0 AND				$
		global_image_coord_xe EQ SELF.global_width - 1 AND	$
		global_image_coord_ys EQ 0 AND				$
		global_image_coord_ye EQ SELF.global_height - 1 AND	$
		NOT KEYWORD_SET(recalc_global) THEN			$
		RETURN, SELF.valid_data_max

	ret		= SELF->Return_Display_Struct(			$
					global_image_coord_xs,		$
					global_image_coord_ys,		$
					global_image_coord_xe,		$
					global_image_coord_ye,		$
					in_h,				$
					in_v,				$
					1.0 )
	found_good_val	= 0
	FOR i = 0,ret.n_slices - 1 DO BEGIN
		in_hs	= ret.slice_h_start[ i ]
		in_he	= ret.slice_h_end[ i ]
		in_vs	= ret.slice_v_start[ i ]
		in_ve	= ret.slice_v_end[ i ]
		current_slice	= SELF->TransformData(((*(SELF.img_stack_ptr))[*,*,ret.slice_idx[i]])[ in_hs:in_he, in_vs:in_ve ])
		IF PTR_VALID( SELF.missing_data_value_ptr ) THEN BEGIN
			missing_mask	= ((*(SELF.scaling_params.missing_mask_ptr))[*,*,ret.slice_idx[i]])[ in_hs:in_he, in_vs:in_ve ]
			not_missing_idx	= WHERE( missing_mask GT 0, cnt )

			IF cnt GT 0 THEN BEGIN
				IF i LE 0 OR NOT found_good_val THEN max = MAX( current_slice[not_missing_idx] )	$
				ELSE		max	= MAX( [ max, current_slice[not_missing_idx] ] )
				found_good_val	= 1
			ENDIF
;print,'in PTR_VALID side of things, max = ',max
		ENDIF ELSE BEGIN
			IF i LE 0 OR NOT found_good_val THEN max = MAX( current_slice )	$
			ELSE		                     max = MAX( [ max, current_slice[*] ] )
			found_good_val	= 1
;print,'in the NOT PTR_VALID side of things, max = ',max
		ENDELSE
	ENDFOR


	IF found_good_val THEN RETURN, max

	msg	= [							$
			'could not find any valid data values in',	$
			'specified region... returning global maximum value' ]
	res	= DIALOG_MESSAGE( msg, /INFORMATION )


	RETURN, SELF.valid_data_max
END
; IMAGE_DATA::GetMaxVal

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@ IMAGE_DATA::SetDisplayMinVal2Byte @@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO IMAGE_DATA::SetDisplayMinVal2Byte, new_value
	SELF.scaling_params.min_val_to_byte	= new_value
	(*(SELF.scaling_params.below_mask_ptr))	= (*(SELF.scaling_params.below_mask_ptr))*0B+1B
	idx	= WHERE( SELF->TransformData((*(SELF.img_stack_ptr))) LT SELF.scaling_params.min_val_to_byte, cnt )
	IF cnt GT 0 THEN (*(SELF.scaling_params.below_mask_ptr))[ idx ] = 0B
END
; IMAGE_DATA::SetDisplayMinVal2Byte

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@ IMAGE_DATA::SetDisplayMaxVal2Byte @@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO IMAGE_DATA::SetDisplayMaxVal2Byte, new_value
	SELF.scaling_params.max_val_to_byte	= new_value
	(*(SELF.scaling_params.above_mask_ptr))	= (*(SELF.scaling_params.above_mask_ptr))*0B+1B
	idx	= WHERE( SELF->TransformData((*(SELF.img_stack_ptr))) GT SELF.scaling_params.max_val_to_byte, cnt )
	IF cnt GT 0 THEN (*(SELF.scaling_params.above_mask_ptr))[ idx ] = 0B
END
; IMAGE_DATA::SetDisplayMaxVal2Byte

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@ IMAGE_DATA::Return_Missing_Data_Ptr @@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::Return_Missing_Data_Ptr
	RETURN, SELF.missing_data_value_ptr
END
; IMAGE_DATA::Return_Missing_Data_Ptr

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@ IMAGE_DATA::Return_Missing_Data_Minmax_Ptr @@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::Return_Missing_Data_Minmax_Ptr
	RETURN, SELF.missing_data_value_ptr
END
; IMAGE_DATA::Return_Missing_Data_Minmax_Ptr

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@ IMAGE_DATA::Continuous_Missing_Data_Vals_Exist @@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::Continuous_Missing_Data_Vals_Exist
	RETURN, SELF.continuous_missing_data_vals_exist
END
; IMAGE_DATA::Continuous_Missing_Data_Vals

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@ IMAGE_DATA::Discrete_Missing_Data_Vals_Exist @@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::Discrete_Missing_Data_Vals_Exist
	RETURN, SELF.discrete_missing_data_vals_exist
END
; IMAGE_DATA::Discrete_Missing_Data_Vals_Exist

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@ IMAGE_DATA::Return_Number_Of_Tiles @@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::Return_Number_Of_Tiles
	RETURN, SELF.n_slices
END
; IMAGE_DATA::Return_Number_Of_Tiles

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@ IMAGE_DATA::Return_Data_Description @@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::Return_Data_Description
print,'-----> RETURNING DATA DESCRIPTION = ',SELF.data_description
	RETURN, SELF.data_description
END
; IMAGE_DATA::Return_Data_Description

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::Return_Data_Units @@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::Return_Data_Units
	RETURN, SELF.data_units
END
; IMAGE_DATA::Return_Data_Units



;;;ckt,sep2004
;;;ckt,sep2004
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::VM_Used @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO IMAGE_DATA::VM_Used
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		CATCH, /CANCEL
		SELF.vm_used	= 1
		RETURN
	ENDIF
	success	= EXECUTE('a=1')
	SELF.vm_used	= 0
END
; IMAGE_DATA::VM_Used
;;;ckt,sep2004
;;;ckt,sep2004




;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::Return_Source_Data @@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::Return_Source_Data
	RETURN, (*(SELF.img_stack_ptr))
END
; IMAGE_DATA::Return_Source_Data

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::CLEANUP @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO IMAGE_DATA::CLEANUP
	PTR_FREE, SELF.img_stack_ptr
	PTR_FREE, SELF.img_stack_id_ptr
	PTR_FREE, SELF.global_h_off_stack_ptr
	PTR_FREE, SELF.global_v_off_stack_ptr
	PTR_FREE, SELF.missing_data_value_ptr
	PTR_FREE, SELF.scaling_params.above_mask_ptr
	PTR_FREE, SELF.scaling_params.below_mask_ptr
	PTR_FREE, SELF.scaling_params.missing_mask_ptr
	PTR_FREE, SELF.data_transform_formula_ptr
	PTR_FREE, SELF.data_transform_info_ptr
	PTR_FREE, SELF.last_good_transform_formula_ptr
	PTR_FREE, SELF.last_good_transform_info_ptr
END
; IMAGE_DATA::CLEANUP

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA::INIT @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION IMAGE_DATA::INIT,									$
				img_stack,							$
				global_h_off_stack,						$
				global_v_off_stack,						$
				STACK_DIMENSION_IDX = stack_dim_idx,				$
				MISSING_DATA_VALUES = missing_data_vals,			$
				MISSING_DATA_VALUE_RANGE = missing_data_val_range,		$
				DISPLAY_GAMMA = gamma,						$
				DISPLAY_MIN_VALUE_TO_BYTE = min_val_to_byte,			$
				DISPLAY_MAX_VALUE_TO_BYTE = max_val_to_byte,			$
				DEFAULT_DATA_TRANSFORM_FILE = default_data_transform_file,	$
				SOURCE_FILE = source_file,					$
				;
				; The following keyword was added to allow the retrieval of tiles
				; within an IMAGE_DATA object to be retrieved at their true
				; original resolution.  Tiles may come into an IMAGE_DATA object
				; NOT at their original resolution due to horizonatal and vertical
				; offsets being less than one.
				;
				ORIG_DATA_RESIZE_FACTOR = orig_data_resize_factor,		$
				TILE_ID = tile_id


	success					= 1
	failure					= 0
;print,'in IMAGE_DATA::INIT, global_h_off_stack=',global_h_off_stack
;print,'in IMAGE_DATA::INIT, global_v_off_stack=',global_v_off_stack

;res=dialog_message(['made it to here o1'],/information)
;;;ckt,sep2004
;;;ckt,sep2004
	SELF->VM_Used
;res=dialog_message(['made it to here o1b'],/information)

IF KEYWORD_SET(source_file) THEN SELF.source_file = source_file
	SELF.original_data_size_factor	= 1.0
	IF KEYWORD_SET(orig_data_resize_factor) THEN SELF.original_data_size_factor	= orig_data_resize_factor

	IF NOT KEYWORD_SET( stack_dim_idx ) THEN stack_dim_idx = 0

	SELF.stack_dim_idx	= stack_dim_idx




;
;
; IMPORTANT!!!!!!!!!!!!!!!!!!!!!!!!!!!
; IMPORTANT!!!!!!!!!!!!!!!!!!!!!!!!!!!
; IMPORTANT!!!!!!!!!!!!!!!!!!!!!!!!!!!
; IMPORTANT!!!!!!!!!!!!!!!!!!!!!!!!!!!
;
;
IF SELF.stack_dim_idx EQ 0 THEN BEGIN
;move stack dimension to 3rd dimension.
END
; IMPORTANT!!!!!!!!!!!!!!!!!!!!!!!!!!!
; IMPORTANT!!!!!!!!!!!!!!!!!!!!!!!!!!!
; IMPORTANT!!!!!!!!!!!!!!!!!!!!!!!!!!!
; IMPORTANT!!!!!!!!!!!!!!!!!!!!!!!!!!!
; IMPORTANT!!!!!!!!!!!!!!!!!!!!!!!!!!!
; IMPORTANT!!!!!!!!!!!!!!!!!!!!!!!!!!!

	;=======================================================================
	; The following code checks for the existence of one of the missing data
	; keywords.  MISSING_DATA_VALUES is used for a group of discrete
	; missing values; MISSING_DATA_VALUE_RANGE is used to specify the min and
	; max of a continuous range of missing data values ( [ min_val, max_val ] ).
	; If both keywords are set (which is wrong in the first place), the
	; continuous keyword will override the discrete keyword.
	;=======================================================================
	SELF.discrete_missing_data_vals_exist	= 0
	SELF.continuous_missing_data_vals_exist	= 0

	IF KEYWORD_SET(missing_data_vals) THEN BEGIN
		SELF.missing_data_value_ptr = PTR_NEW(missing_data_vals,/NO_COPY)
		SELF.discrete_missing_data_vals_exist	= 1
	ENDIF ELSE BEGIN
		IF KEYWORD_SET(missing_data_val_range) THEN BEGIN
			SELF.missing_data_value_ptr = PTR_NEW(missing_data_val_range,/NO_COPY)
			SELF.continuous_missing_data_vals_exist	= 1
		ENDIF

	ENDELSE

	;==============================================================
	; assumption: no slices overlap any other slices in either the
	; horizontal or vertical directions
	;==============================================================
;print,'(SIZE(img_stack))=',(SIZE(img_stack))
	IF (SIZE(img_stack))[0] LE 2 THEN					$
		SELF.n_slices	= 1						$
	ELSE									$
		SELF.n_slices	= (SIZE(img_stack))[SELF.stack_dim_idx+1]
;help,img_stack
;print,'min,max of img_stack = ',min(img_stack),max(img_stack)
;print,'SELF.stack_dim_idx=',SELF.stack_dim_idx
;print,'in IMAGE_DATA::INIT, SELF.n_slices=', SELF.n_slices

	tile_ids	= STRTRIM( LINDGEN(SELF.n_slices) + 1, 2 )

	IF KEYWORD_SET(tile_id) THEN							$
		tile_ids[0:MIN([N_ELEMENTS(tile_ids), N_ELEMENTS(tile_id)])-1]	= STRTRIM(tile_id,2)

	SELF.img_stack_id_ptr	= PTR_NEW( tile_ids, /NO_COPY )

	;==============================================================
	; determine global width and height of mosaicked image
	;==============================================================
	v_sort_idx	= SORT( global_v_off_stack )
	top_v_off	= global_v_off_stack[ v_sort_idx[SELF. n_slices - 1 ] ]

	CASE SELF.stack_dim_idx OF
		0: BEGIN
			IF (SIZE(img_stack))[0] LE 2 THEN			$
				slice	= img_stack				$
			ELSE							$
				slice	= img_stack[ v_sort_idx[ SELF.n_slices - 1 ], *, * ]
			END
		2: slice	= img_stack[ *, *, v_sort_idx[ SELF.n_slices - 1 ] ]
		ELSE:
	ENDCASE
;help,slice
	;==============================================================
	; assumption: all slices are of the same v-h dimensions
	;==============================================================
	SELF.slice_size_h	= (SIZE(REFORM(slice)))[1]
	SELF.slice_size_v	= (SIZE(REFORM(slice)))[2]
;print,'SELF.slice_size_h,SELF.slice_size_v = ',SELF.slice_size_h,SELF.slice_size_v
	;==============================================================
	; assumption: global_v_off_stack is 0-based
	;==============================================================
	SELF.global_height	= top_v_off + SELF.slice_size_v

	h_sort_idx	= SORT( global_h_off_stack )
;print,'global_h_off_stack=',global_h_off_stack
	right_h_off	= global_h_off_stack[ h_sort_idx[ SELF.n_slices - 1 ] ]
;print,'right_h_off=',	right_h_off
	;==============================================================
	; assumption: global_h_off_stack is 0-based
	;==============================================================
	SELF.global_width	= right_h_off + SELF.slice_size_h
;print,'	right_h_off + SELF.slice_size_h = ',right_h_off + SELF.slice_size_h


	missing_mask	= BYTARR( SELF.slice_size_h, SELF.slice_size_v, SELF.n_slices )

;res=dialog_message(['made it to here o2'],/information)

	IF PTR_VALID( SELF.missing_data_value_ptr ) THEN BEGIN
;res=dialog_message(['made it to here o2a'],/information)
		;==============================================================
		; DISCRETE MISSING DATA VALUES
		;==============================================================
		IF SELF.discrete_missing_data_vals_exist THEN BEGIN
;res=dialog_message(['made it to here o3'],/information)
			IF PTR_VALID(SELF.data_transform_formula_ptr) THEN							$
;;;ckt,sep2004				not_missing_idx	= where_not( SELF->TransformData(img_stack), *(SELF.missing_data_value_ptr) )	$
				not_missing_idx	= where2( SELF->TransformData(img_stack), *(SELF.missing_data_value_ptr), /INVERSE )	$
			ELSE													$
;;;ckt,sep2004				not_missing_idx	= where_not( img_stack, *(SELF.missing_data_value_ptr) )
				not_missing_idx	= where2( img_stack, *(SELF.missing_data_value_ptr), /INVERSE )
;res=dialog_message(['made it to here o4'],/information)

			IF not_missing_idx[0] EQ -2L THEN BEGIN
				;-------------------------------------------------------
				; indicates the EXECUTE() statement in where_not failed.
				;-------------------------------------------------------

				IF PTR_VALID(SELF.data_transform_formula_ptr) THEN					$
					tmp_min		= MIN( SELF->TransformData(img_stack), MAX = tmp_max )		$
				ELSE											$
					tmp_min		= MIN( img_stack, MAX = tmp_max )

				tmpidx		= WHERE( *(SELF.missing_data_value_ptr) GE tmp_min AND			$
							*(SELF.missing_data_value_ptr) LE tmp_max, tmpcnt )
				IF tmpcnt LE 0 THEN BEGIN
					not_missing_idx	= WHERE( img_stack EQ img_stack )
				ENDIF ELSE BEGIN
					missing_mask	= (TEMPORARY(missing_mask)*0B) + 1B
					FOR i = 0, tmpcnt-1 DO BEGIN
						curval	= (*(SELF.missing_data_value_ptr))[tmpidx[i]]
						FOR j = 0, SELF.n_slices - 1 DO BEGIN


							IF PTR_VALID(SELF.data_transform_formula_ptr) THEN		$
								tmpimg	= SELF->TransformData(img_stack[*,*,j])		$
							ELSE								$
								tmpimg	= img_stack[*,*,j]


							tmpidx2	= WHERE( tmpimg EQ curval, tmpcnt2 )
							IF tmpcnt2 GT 0 THEN BEGIN
								tmpmm			= missing_mask[*,*, j]
								tmpmm[tmpidx2]		= 0B
								missing_mask[*,*, j]	= tmpmm
							ENDIF
						ENDFOR
					ENDFOR
					not_missing_idx	= WHERE( missing_mask GT 0B )
				ENDELSE
			ENDIF

		;==============================================================
		; CONTINUOUS MISSING DATA VALUES
		;==============================================================
		ENDIF ELSE BEGIN
;print,'*(SELF.missing_data_value_ptr) = ',*(SELF.missing_data_value_ptr)
			IF PTR_VALID(SELF.data_transform_formula_ptr) THEN							$
				not_missing_idx	= WHERE(									$
					( SELF->TransformData(img_stack) LT *(SELF.missing_data_value_ptr)[0] ) OR		$
					( SELF->TransformData(img_stack) GT *(SELF.missing_data_value_ptr)[1] ) )		$
			ELSE													$
				not_missing_idx	= WHERE(									$
					( img_stack LT (*(SELF.missing_data_value_ptr))[0] ) OR					$
					( img_stack GT (*(SELF.missing_data_value_ptr))[1] ) )
		ENDELSE

		IF not_missing_idx[0] NE -1L THEN  missing_mask[ not_missing_idx ] = 1B

		IF not_missing_idx[0] EQ -1L THEN BEGIN
		;==============================================================
		; If this part of the code is encountered, it means that ALL
		; of the data is tagged as missing (not very useful), so min
		; and max are meaningless.
		;==============================================================
			IF PTR_VALID(SELF.data_transform_formula_ptr) THEN					$
				global_min	= MIN( SELF->TransformData(img_stack), MAX = global_max )	$
			ELSE											$
				global_min	= MIN( img_stack, MAX = global_max )

		ENDIF ELSE BEGIN
			IF PTR_VALID(SELF.data_transform_formula_ptr) THEN							$
				global_min	= MIN( SELF->TransformData(img_stack[ not_missing_idx ]), MAX = global_max )	$
			ELSE													$
				global_min	= MIN( img_stack[ not_missing_idx ], MAX = global_max )
		ENDELSE
	ENDIF ELSE BEGIN
		IF PTR_VALID(SELF.data_transform_formula_ptr) THEN						$
			global_min	= MIN( SELF->TransformData(img_stack), MAX = global_max )		$
		ELSE												$
				global_min	= MIN( img_stack, MAX = global_max )

		missing_mask	= missing_mask*0B+1B
	ENDELSE


	SELF.scaling_params.missing_mask_ptr	= PTR_NEW(missing_mask,/NO_COPY)

	SELF.valid_data_min			= global_min
	SELF.valid_data_max			= global_max
	SELF.scaling_params.min_val_to_byte	= global_min
	SELF.scaling_params.max_val_to_byte	= global_max
	IF KEYWORD_SET(min_val_to_byte) THEN SELF.scaling_params.min_val_to_byte = min_val_to_byte
	IF KEYWORD_SET(max_val_to_byte) THEN SELF.scaling_params.max_val_to_byte = max_val_to_byte

	below_mask	= BYTARR( SELF.slice_size_h, SELF.slice_size_v, SELF.n_slices )

IF PTR_VALID(SELF.data_transform_formula_ptr) THEN								$
	idx		= WHERE( SELF->TransformData(img_stack) LT SELF.scaling_params.min_val_to_byte, cnt )	$
ELSE														$
	idx		= WHERE( img_stack LT SELF.scaling_params.min_val_to_byte, cnt )


	IF cnt GT 0 THEN below_mask[ idx ] = 0B
	SELF.scaling_params.below_mask_ptr	= PTR_NEW(below_mask,/NO_COPY)

	above_mask	= BYTARR( SELF.slice_size_h, SELF.slice_size_v, SELF.n_slices )

IF PTR_VALID(SELF.data_transform_formula_ptr) THEN								$
	idx		= WHERE( SELF->TransformData(img_stack) GT SELF.scaling_params.max_val_to_byte, cnt )	$
ELSE														$
	idx		= WHERE( img_stack GT SELF.scaling_params.max_val_to_byte, cnt )

	IF cnt GT 0 THEN above_mask[ idx ] = 0B
	SELF.scaling_params.above_mask_ptr	= PTR_NEW(above_mask,/NO_COPY)

	SELF.img_stack_ptr	= PTR_NEW(img_stack,/NO_COPY)

	SELF.global_h_off_stack_ptr	= PTR_NEW( global_h_off_stack, /NO_COPY )
	SELF.global_v_off_stack_ptr	= PTR_NEW( global_v_off_stack, /NO_COPY )

	SELF.scaling_params.gamma	= 1.0D
	IF KEYWORD_SET( gamma ) THEN SELF.scaling_params.gamma = gamma

	IF KEYWORD_SET(default_data_transform_file) THEN BEGIN
		SELF->SetTransform, default_data_transform_file, SUCCESS = transform_successful
		IF transform_successful THEN BEGIN
			SELF.default_transform_file	= default_data_transform_file
			SELF.current_transform_file	= SELF.default_transform_file
		ENDIF
	ENDIF ELSE BEGIN
		SELF->Set_Default_Transform
		SELF.current_transform_file		= SELF.default_transform_file
	ENDELSE
;res=dialog_message(['returtning from init'],/information)

	RETURN, success

END
; IMAGE_DATA::INIT

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ IMAGE_DATA__DEFINE @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO IMAGE_DATA__DEFINE

	scaling_params	= {	scaling_params,					$
				gamma			: 0.0D,			$
				min_val_to_byte		: 0.0D,			$
				max_val_to_byte		: 0.0D,			$
				missing_mask_ptr	: PTR_NEW(),		$
				above_mask_ptr		: PTR_NEW(),		$
				below_mask_ptr		: PTR_NEW(),		$
				MISSING_MASK_IDX	: 0B,			$
				BELOW_MASK_IDX		: 0B,			$
				ABOVE_MASK_IDX		: 0B }

	IMAGE_DATA = {								$
		IMAGE_DATA,							$
			img_stack_ptr			: PTR_NEW(),		$
			img_stack_id_ptr		: PTR_NEW(),		$
			global_h_off_stack_ptr		: PTR_NEW(),		$
			global_v_off_stack_ptr		: PTR_NEW(),		$
			missing_data_value_ptr		: PTR_NEW(),		$
			data_transform_formula_ptr	: PTR_NEW(),		$
			data_transform_info_ptr		: PTR_NEW(),		$
			last_good_transform_formula_ptr	: PTR_NEW(),		$
			last_good_transform_info_ptr	: PTR_NEW(),		$
			default_transform_file		: '',			$
			current_transform_file		: '',			$
			scaling_params			: scaling_params,	$
			stack_dim_idx			: 0,			$
			global_width			: 0,			$
			global_height			: 0,			$
			slice_size_h			: 0L,			$
			slice_size_v			: 0L,			$
			valid_data_min			: 0.0D,			$
			valid_data_max			: 0.0D,			$
			discrete_missing_data_vals_exist: 0,			$
			continuous_missing_data_vals_exist:0,			$
			source_file			: '',			$
			data_description		: '',			$
			data_units			: '',			$
			original_data_size_factor	: 0.0,			$
			n_slices			: 0L,			$
;;;ckt,sep2004
;;;ckt,sep2004
			vm_used				: 0 }
END
; IMAGE_DATA__DEFINE
