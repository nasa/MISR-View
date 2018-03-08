;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ scroll_option_eh @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO scroll_option_eh, event
	WIDGET_CONTROL, event.id, GET_VALUE = button_name
	CASE STRUPCASE(button_name) OF
		'OK': BEGIN
			WIDGET_CONTROL, event.top, GET_UVALUE = ptr
			WIDGET_CONTROL, (*ptr).hcwf, GET_VALUE = hoff
			WIDGET_CONTROL, (*ptr).vcwf, GET_VALUE = voff
			IF hoff LT 0 OR voff LT 0 THEN BEGIN
				msg	= 'Offsets cannot be less than 0!'
				res	= DIALOG_MESSAGE( msg, /ERROR )
				RETURN
			ENDIF
			IF hoff GE (*ptr).scroll_struct.scroll_window_x OR		$
				voff GE (*ptr).scroll_struct.scroll_window_y THEN BEGIN
				msg	= [					$
						'Offsets cannot be greater',	$
						'than or equal to the ',	$
						'dimensions of the scroll',	$
						'window!' ]
				res	= DIALOG_MESSAGE( msg, /ERROR )
				RETURN
			ENDIF
			(*ptr).scroll_struct.display_offset_x	= hoff
			(*ptr).scroll_struct.display_offset_y	= voff
			WIDGET_CONTROL, event.top, /DESTROY
			END
		'CANCEL': BEGIN
			WIDGET_CONTROL, event.top, GET_UVALUE = ptr
			(*ptr).cancel_pressed	= 1
			WIDGET_CONTROL, event.top, /DESTROY
			END
		ELSE:
	ENDCASE
END
; scroll_option_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ show_scroll_options @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION show_scroll_options, scroll_struct
	opt_base	= WIDGET_BASE(						$
					/COLUMN,				$
					/BASE_ALIGN_CENTER,			$
					/MODAL,					$
					GROUP_LEADER =				$
					(*(scroll_struct.window_info_ptr)).tlb,	$
					EVENT_PRO = 'scroll_option_eh' )
					
	sval		= 'Lower Left Horizontal Offset:'
	hcwf		= CW_FIELD(						$
					opt_base,				$
					/LONG,					$
					TITLE = sval,				$
					VALUE = scroll_struct.display_offset_x )
					
	sval		= 'Lower Left Vertical Offset:'
	vcwf		= CW_FIELD(						$
					opt_base,				$
					/LONG,					$
					TITLE = sval,				$
					VALUE = scroll_struct.display_offset_y )
					
	btn_base	= WIDGET_BASE(						$
					opt_base,				$
					/ALIGN_CENTER,				$
					/BASE_ALIGN_CENTER,			$
					/ROW )
	ok		= WIDGET_BUTTON(					$
					btn_base,				$
					VALUE = 'OK' )
	cancel		= WIDGET_BUTTON(					$
					btn_base,				$
					VALUE = 'Cancel' )
					
	WIDGET_CONTROL, opt_base, /REALIZE
	
	ptr	= PTR_NEW( {							$
				hcwf		: hcwf,				$
				vcwf		: vcwf,				$
				cancel_pressed	: 0,				$
				scroll_struct	: scroll_struct } )
	
	WIDGET_CONTROL, opt_base, SET_UVALUE = ptr
	
	XMANAGER, 'show_scroll_options', opt_base, EVENT_HANDLER = 'scroll_option_eh'
	
	scroll_struct	= (*ptr).scroll_struct
	PTR_FREE, ptr
	
	RETURN, scroll_struct
					
END
; show_scroll_options

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ confirm_settings_eh @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO confirm_settings_eh, event

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when 
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== confirm_settings_eh (scroll_tool) =========='
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


	WIDGET_CONTROL, event.id, GET_UVALUE = uvalue

	CASE 1 OF
		STRUPCASE( uvalue ) EQ 'HDTV' : BEGIN
			WIDGET_CONTROL, event.top, GET_UVALUE = write_scroll_path_info
;print,'before:  write_scroll_path_info.video_frame_size = ',write_scroll_path_info.video_frame_size
			write_scroll_path_info.video_frame_size = [1920,1080]
;print,'after:  write_scroll_path_info.video_frame_size = ',write_scroll_path_info.video_frame_size
			WIDGET_CONTROL, event.top, SET_UVALUE = write_scroll_path_info
		END

		STRUPCASE( uvalue ) EQ 'NTSC' : BEGIN
			WIDGET_CONTROL, event.top, GET_UVALUE = write_scroll_path_info
;print,'before:  write_scroll_path_info.video_frame_size = ',write_scroll_path_info.video_frame_size
			write_scroll_path_info.video_frame_size = [720,486]
;print,'after:  write_scroll_path_info.video_frame_size = ',write_scroll_path_info.video_frame_size
			WIDGET_CONTROL, event.top, SET_UVALUE = write_scroll_path_info
		END

		STRUPCASE( uvalue ) EQ 'YES' : BEGIN

			WIDGET_CONTROL, event.top, GET_UVALUE = write_scroll_path_info
			WIDGET_CONTROL, event.top, /DESTROY
			WIDGET_CONTROL, /HOURGLASS

			first			= write_scroll_path_info.first
			last			= write_scroll_path_info.last
			direction		= write_scroll_path_info.direction
			time			= write_scroll_path_info.time
			time_inc		= write_scroll_path_info.time_inc
			block_x_size		= write_scroll_path_info.block_x_size
			block_y_size		= write_scroll_path_info.block_y_size
			video_frame_size	= write_scroll_path_info.video_frame_size
			scroll_struct		= write_scroll_path_info.scroll_struct
			pathFileName		= write_scroll_path_info.pathFileName
			mosaicFileName		= write_scroll_path_info.mosaicFileName

;print,'ok:  write_scroll_path_info.video_frame_size = ',write_scroll_path_info.video_frame_size

			;--------------------------------------------------
			; Write out ASCII path file.
			;--------------------------------------------------
			OPENW, lun, pathFileName, /GET_LUN
			FOR i = first, last, direction DO BEGIN
				;--------------------------------------------------
				; First time through time goes to zero (start).
				;--------------------------------------------------
				time	= time + time_inc
				;--------------------------------------------------
				; [xcenter,ycenter]=[0,0] anchored to lower left hand corner.
				;--------------------------------------------------
				xcenter	= scroll_struct.x_off[i] + block_x_size / 2
				ycenter	= scroll_struct.y_off[i] + block_y_size / 2
				PRINTF, lun,					$
					STRING( time ),				$
					' ' + STRING( xcenter ),		$
					' ' + STRING( ycenter ),		$
					' ' + STRING( video_frame_size[0] ),	$
					' ' + STRING( video_frame_size[1] )
			ENDFOR
			FREE_LUN, lun

			;--------------------------------------------------
			; Write out mosaic image @ agreed upon resolution.
			; Prepare imagery in the WRITE_TIFF command so as 
			; to avoid data copy.
			;--------------------------------------------------
			outXSize	= scroll_struct.mosaicOutputSize[0]
			outYSize	= scroll_struct.mosaicOutputSize[1]
			CASE 1 OF
				OBJ_VALID( scroll_struct.dataObjArr[0] )							$ ;RED ONLY (B/W)
				AND NOT OBJ_VALID( scroll_struct.dataObjArr[1] )						$
				AND NOT OBJ_VALID( scroll_struct.dataObjArr[2] ) : BEGIN
					validObj = scroll_struct.dataObjArr[0]
					WRITE_TIFF, mosaicFileName, REVERSE( validObj -> ReturnBytescaledImage(		$
						0, validObj -> GetImageWidth()-1,					$
						0, validObj -> GetImageHeight()-1,					$
						outXSize, outYSize ), 2 ), 1
				END

				NOT OBJ_VALID( scroll_struct.dataObjArr[0] )						$ ;GRN ONLY (B/W)
				AND OBJ_VALID( scroll_struct.dataObjArr[1] )						$
				AND NOT OBJ_VALID( scroll_struct.dataObjArr[2] ) : BEGIN
					validObj = scroll_struct.dataObjArr[1]
					WRITE_TIFF, mosaicFileName, REVERSE( validObj -> ReturnBytescaledImage(		$
						0, validObj -> GetImageWidth()-1,					$
						0, validObj -> GetImageHeight()-1,					$
						outXSize, outYSize ), 2 ), 1
				END

				NOT OBJ_VALID( scroll_struct.dataObjArr[0] )						$ ;BLU ONLY (B/W)
				AND NOT OBJ_VALID( scroll_struct.dataObjArr[1] )						$
				AND OBJ_VALID( scroll_struct.dataObjArr[2] ) : BEGIN
					validObj = scroll_struct.dataObjArr[2]
					WRITE_TIFF, mosaicFileName, REVERSE( validObj -> ReturnBytescaledImage(		$
						0, validObj -> GetImageWidth()-1,					$
						0, validObj -> GetImageHeight()-1,					$
						outXSize, outYSize ), 2 ), 1
				END

				OBJ_VALID( scroll_struct.dataObjArr[0] )							$ ;RED+GRN
				AND OBJ_VALID( scroll_struct.dataObjArr[1] )						$
				AND NOT OBJ_VALID( scroll_struct.dataObjArr[2] ) :						$
					WRITE_TIFF, mosaicFileName, 1, PLANARCONFIG = 2,				$
						RED = REVERSE( scroll_struct.dataObjArr[0] -> ReturnBytescaledImage(	$
							0, scroll_struct.dataObjArr[0] -> GetImageWidth()-1,		$
							0, scroll_struct.dataObjArr[0] -> GetImageHeight()-1,		$
							outXSize, outYSize ), 2 ),					$
						GRE = REVERSE( scroll_struct.dataObjArr[1] -> ReturnBytescaledImage(	$
							0, scroll_struct.dataObjArr[1] -> GetImageWidth()-1,		$
							0, scroll_struct.dataObjArr[1] -> GetImageHeight()-1,		$
							outXSize, outYSize ), 2 ),					$
						BLU = BYTARR( outXSize, outYSize )

				OBJ_VALID( scroll_struct.dataObjArr[0] )							$ ;RED+BLU
				AND NOT OBJ_VALID( scroll_struct.dataObjArr[1] )						$
				AND OBJ_VALID( scroll_struct.dataObjArr[2] ) :						$
					WRITE_TIFF, mosaicFileName, 1, PLANARCONFIG = 2,				$
						RED = REVERSE( scroll_struct.dataObjArr[0] -> ReturnBytescaledImage(	$
							0, scroll_struct.dataObjArr[0] -> GetImageWidth()-1,		$
							0, scroll_struct.dataObjArr[0] -> GetImageHeight()-1,		$
							outXSize, outYSize ), 2 ),					$
						GRE = BYTARR( outXSize, outYSize ),					$
						BLU = REVERSE( scroll_struct.dataObjArr[2] -> ReturnBytescaledImage(	$
							0, scroll_struct.dataObjArr[2] -> GetImageWidth()-1,		$
							0, scroll_struct.dataObjArr[2] -> GetImageHeight()-1,		$
							outXSize, outYSize ), 2 )

				NOT OBJ_VALID( scroll_struct.dataObjArr[0] )						$ ;GRN+BLU
				AND OBJ_VALID( scroll_struct.dataObjArr[1] )						$
				AND OBJ_VALID( scroll_struct.dataObjArr[2] ) :						$
					WRITE_TIFF, mosaicFileName, 1, PLANARCONFIG = 2,				$
						RED = BYTARR( outXSize, outYSize ),					$
						GRE = REVERSE( scroll_struct.dataObjArr[1] -> ReturnBytescaledImage(	$
							0, scroll_struct.dataObjArr[1] -> GetImageWidth()-1,		$
							0, scroll_struct.dataObjArr[1] -> GetImageHeight()-1,		$
							outXSize, outYSize ), 2 ),					$
						BLU = REVERSE( scroll_struct.dataObjArr[2] -> ReturnBytescaledImage(	$
							0, scroll_struct.dataObjArr[2] -> GetImageWidth()-1,		$
							0, scroll_struct.dataObjArr[2] -> GetImageHeight()-1,		$
							outXSize, outYSize ), 2 )

				OBJ_VALID( scroll_struct.dataObjArr[0] )							$ ;RED+GRN+BLU
				AND OBJ_VALID( scroll_struct.dataObjArr[1] )						$
				AND OBJ_VALID( scroll_struct.dataObjArr[2] ) :						$
					WRITE_TIFF, mosaicFileName, 1, PLANARCONFIG = 2,				$
						RED = REVERSE( scroll_struct.dataObjArr[0] -> ReturnBytescaledImage(	$
							0, scroll_struct.dataObjArr[0] -> GetImageWidth()-1,		$
							0, scroll_struct.dataObjArr[0] -> GetImageHeight()-1,		$
							outXSize, outYSize ), 2 ),					$
						GRE = REVERSE( scroll_struct.dataObjArr[1] -> ReturnBytescaledImage(	$
							0, scroll_struct.dataObjArr[1] -> GetImageWidth()-1,		$
							0, scroll_struct.dataObjArr[1] -> GetImageHeight()-1,		$
							outXSize, outYSize ), 2 ),					$
						BLU = REVERSE( scroll_struct.dataObjArr[2] -> ReturnBytescaledImage(	$
							0, scroll_struct.dataObjArr[2] -> GetImageWidth()-1,		$
							0, scroll_struct.dataObjArr[2] -> GetImageHeight()-1,		$
							outXSize, outYSize ), 2 )

				ELSE : res = DIALOG_MESSAGE( 'CANNOT SAVE IMAGE DATA MOSAIC TO FILE, NO VALID DATA OBJECTS', /ERROR )
			ENDCASE
		END

		STRUPCASE( uvalue ) EQ 'NO' : BEGIN
			WIDGET_CONTROL, event.top, /DESTROY
			RETURN
		END

		ELSE : BEGIN
		END

	ENDCASE

END
; confirm_settings_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ write_scroll_path_file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO write_scroll_path_file, control_struct, scroll_struct

	;----------------------------------------------------------------------------
	;*** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** ***
	;
	; Much of the code for write_scroll_path_file is located in confirm_settings_eh.
	;
	;*** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** ***
	;----------------------------------------------------------------------------

	;--------------------------------------------------
	; Get filenames and other settings.
	;--------------------------------------------------
	pathFileName	= dialog_pickfile_wrapper( TITLE = 'ASCII path' )
;;;ckt,apr2001	pathFileName	= DIALOG_PICKFILE( TITLE = 'ASCII path' )
	IF blankFileName( pathFileName ) THEN BEGIN
		err = DIALOG_MESSAGE( [						$
			'A blank file name was selected.',			$
			'Please press the Write Path File button again.' ],	$
			/ERROR )
		RETURN
	ENDIF
;;;ckt,apr2001	mosaicFileName	= DIALOG_PICKFILE( TITLE = 'TIFF image' )
	mosaicFileName	= dialog_pickfile_wrapper( TITLE = 'TIFF image' )
	IF blankFileName( mosaicFileName ) THEN	BEGIN
		err = DIALOG_MESSAGE( [						$
			'A blank file name was selected.',			$
			'Please press the Write Path File button again.' ],	$
			/ERROR )
		RETURN
	ENDIF
	curWin		= !D.WINDOW
	WSET, scroll_struct.block[0]
	block_x_size	= !D.X_SIZE
	block_y_size	= !D.Y_SIZE
	WSET, curWin
	;--------------------------------------------------
	; Calculate centers, x and y center differencs, and 
	; averages of differences.  The larger average 
	; represents one measure of the motion direction 
	; vector which decides whether the direction is 
	; predominately in the x or y axis.  Predominance 
	; determines whether block_x_size or block_y_size 
	; is used in the calculation of time_inc.  
	;--------------------------------------------------
	xcenter		= scroll_struct.x_off + block_x_size / 2
	ycenter		= scroll_struct.y_off + block_y_size / 2
	ndiff		= N_ELEMENTS( xcenter ) - 1
	xdiff		= LONARR( ndiff )
	ydiff		= LONARR( ndiff )
	FOR i = 0, ndiff - 1 DO BEGIN
		xdiff[i]	= ABS( xcenter[i] - xcenter[i+1] )
		ydiff[i]	= ABS( ycenter[i] - ycenter[i+1] )
	ENDFOR
	xavg		= FLOAT( TOTAL( xdiff ) ) / FLOAT( ndiff )
	yavg		= FLOAT( TOTAL( ydiff ) ) / FLOAT( ndiff )
	IF xavg GE yavg THEN		$
		block_size	= xavg	$
	ELSE				$
		block_size	= yavg
	;--------------------------------------------------
	; Now, calculate time_inc using block_size as 
	; computed above.
	;--------------------------------------------------
	time_inc	= *(control_struct.avg_realtime_inc_ptr) * ( DOUBLE( block_size ) / DOUBLE( ABS( scroll_struct.scroll_inc ) ) )
	time		= (-1.0d) * time_inc
	IF scroll_struct.scroll_inc GT 0 THEN BEGIN
		;--------------------------------------------------
		; South to North (forward).
		;--------------------------------------------------
		first		= 0
		last		= N_ELEMENTS( scroll_struct.block ) - 1
		direction	= 1
	ENDIF ELSE BEGIN
		;--------------------------------------------------
		; North to South (backward).
		;--------------------------------------------------
		first		= N_ELEMENTS( scroll_struct.block ) - 1
		last		= 0
		direction	= (-1)
	ENDELSE

	;--------------------------------------------------
	; Have user verify current settings.
	;--------------------------------------------------
	default_video_frame_size = [1920,1080]
	seconds		= time
	FOR i = first, last, direction DO $
		seconds = seconds + time_inc
	nblocks		= ABS( first - last ) + 1
	confirm_tlb	= WIDGET_BASE(				$
				TITLE = 'Confirm Settings',	$
				/COLUMN,			$
				UVALUE = default_video_frame_size )
	label_01	= WIDGET_LABEL(				$
				confirm_tlb,			$
				/ALIGN_LEFT,			$
				VALUE = 'Please confirm settings:' )
	label_02	= WIDGET_LABEL(				$
				confirm_tlb,			$
				/ALIGN_LEFT,			$
				VALUE = '' )
	label_03	= WIDGET_LABEL(				$
				confirm_tlb,			$
				/ALIGN_LEFT,			$
				VALUE = 'Path file name          = ' + pathFileName )
	label_04	= WIDGET_LABEL(				$
				confirm_tlb,			$
				/ALIGN_LEFT,			$
				VALUE = 'Mosaic TIFF file name   = ' + mosaicFileName )
	label_05	= WIDGET_LABEL(				$
				confirm_tlb,			$
				/ALIGN_LEFT,			$
				VALUE = 'Total number of seconds = ' + STRTRIM( seconds, 2 ) )
	label_06	= WIDGET_LABEL(				$
				confirm_tlb,			$
				/ALIGN_LEFT,			$
				VALUE = 'Total number of blocks  = ' + STRTRIM( nblocks, 2 ) )
	tv_base		= WIDGET_BASE(				$
				confirm_tlb,			$
				/ALIGN_LEFT,			$
				/EXCLUSIVE,			$
				/COLUMN )
	HDTV_button	= WIDGET_BUTTON(			$
				tv_base,			$
				VALUE = 'HDTV (1920x1080)',	$
				UVALUE = 'HDTV' )
	NTSC_button	= WIDGET_BUTTON(			$
				tv_base,			$
				VALUE = 'NTSC (720x486)',	$
				UVALUE = 'NTSC' )
	confirm_base	= WIDGET_BASE(				$
				confirm_tlb,			$
				/ALIGN_LEFT, /ROW, /FRAME )
	confirm_label	= WIDGET_LABEL(				$
				confirm_base,			$
				VALUE = 'Confirm Settings:' )
	yesno_base	= WIDGET_BASE(				$
				confirm_base,			$
				/ROW )
	yes_button	= WIDGET_BUTTON(			$
				yesno_base,			$
				VALUE = 'Yes',			$
				UVALUE = 'YES' )
	no_button	= WIDGET_BUTTON(			$
				yesno_base,			$
				VALUE = 'No',			$
				UVALUE = 'NO' )
	WIDGET_CONTROL, confirm_tlb, /REALIZE
	WIDGET_CONTROL, HDTV_button, /SET_BUTTON
	write_scroll_path_info = {							$
			video_frame_size	: default_video_frame_size,	$
			time_inc		: time_inc,			$
			time			: time,				$
			first			: first,			$
			last			: last,				$
			direction		: direction,			$
			pathFileName		: pathFileName,			$
			mosaicFileName		: mosaicFileName,		$
			block_x_size		: block_x_size,			$
			block_y_size		: block_y_size,			$
			scroll_struct		: scroll_struct			}
	WIDGET_CONTROL, confirm_tlb, SET_UVALUE = write_scroll_path_info
	XMANAGER, 'confirm settings', confirm_tlb, EVENT_HANDLER = 'confirm_settings_eh'

	;-------------------------------------------------------------------------------
	; The remaining code for write_scroll_path_file is located in confirm_settings_eh.
	;-------------------------------------------------------------------------------

END
; write_scroll_path_file

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ get_mpeg_info_eh @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO get_mpeg_info_eh, event
	WIDGET_CONTROL, event.id, GET_UVALUE = widget_name
	WIDGET_CONTROL, event.top, GET_UVALUE = ptr
	
	CASE STRUPCASE(widget_name) OF
		'DIRECTION': BEGIN
			sval	= 'Starting Frame (%): ' + (*ptr).seval[event.value]
			WIDGET_CONTROL, (*ptr).s_lbl, SET_VALUE = sval
			eval	= 'Ending Frame (%): ' + (*ptr).seval[event.value EQ 0]
			WIDGET_CONTROL, (*ptr).e_lbl, SET_VALUE = eval
			(*ptr).forward	= event.value EQ 0
			END
		'FILENAME': BEGIN
;;;ckt,apr2001			res	= DIALOG_PICKFILE( TITLE = 'Enter the name for the output MPEG file' )
			res	= dialog_pickfile_wrapper( TITLE = 'Enter the name for the output MPEG file' )
			IF res NE '' THEN BEGIN
				(*ptr).filename = res
				WIDGET_CONTROL, (*ptr).out_txt, SET_VALUE = res
			ENDIF
			END
		'CREATE': BEGIN
			WIDGET_CONTROL, event.top, /DESTROY
			END
		'CANCEL': BEGIN
			(*ptr).cancel_pressed	= 1
			WIDGET_CONTROL, event.top, /DESTROY
			END
		ELSE:
	ENDCASE
END
; get_mpeg_info_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ get_mpeg_info @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION get_mpeg_info, scroll_struct
	m_base	= WIDGET_BASE(							$
				TITLE = 'MPEG Generation',			$
				/COLUMN,					$
				/MODAL,						$
				GROUP_LEADER =					$
					(*(scroll_struct.window_info_ptr)).tlb,	$
				/BASE_ALIGN_CENTER,				$
				EVENT_PRO = 'get_mpeg_info_eh' )
	fb_cwbg	= CW_BGROUP(							$
				m_base,						$
				[ 'Forward', 'Reverse' ],			$
				/COLUMN,					$
				/EXCLUSIVE,					$
				/FRAME,						$
				/RETURN_INDEX,					$
				SET_VALUE = 0,					$
				UVALUE = 'direction' )
	position= ROUND(( FLOAT( scroll_struct.start_scroll_idx ) / FLOAT( N_ELEMENTS( scroll_struct.interp_x_offsets ) ) ) * 100.0)
	sval	= STRTRIM(STRING(position),2)
	s_lbl	= WIDGET_LABEL(							$
				m_base,						$
				/DYNAMIC_RESIZE,				$
				/ALIGN_CENTER,					$
				VALUE = 'Starting Frame (%): ' + sval )
	position= ROUND(( FLOAT( scroll_struct.end_scroll_idx ) / FLOAT( N_ELEMENTS( scroll_struct.interp_x_offsets ) ) ) * 100.0)
	eval	= STRTRIM(STRING(position),2)
	e_lbl	= WIDGET_LABEL(							$
				m_base,						$
				/DYNAMIC_RESIZE,				$
				/ALIGN_CENTER,					$
				VALUE = 'Ending Frame (%): ' + eval )
	out_base= WIDGET_BASE(							$
				m_base,						$
				/ALIGN_CENTER,					$
				/BASE_ALIGN_CENTER,				$
				/ROW )
	out_lbl	= WIDGET_LABEL(							$
				out_base,					$
				VALUE = 'Name of Output MPEG File: ',		$
				/ALIGN_CENTER )
	out_txt	= WIDGET_TEXT(							$
				out_base,					$
				VALUE = 'NOT SET',				$
				/ALIGN_CENTER )
	out_btn	= WIDGET_BUTTON(						$
				out_base,					$
				/ALIGN_CENTER,					$
				VALUE = 'Specify Filename',			$
				UVALUE = 'filename' )
	btn_base= WIDGET_BASE(							$
				m_base,						$
				/ROW,						$
				/ALIGN_CENTER,					$
				/BASE_ALIGN_CENTER )
	make	= WIDGET_BUTTON(						$
				btn_base,					$
				/ALIGN_CENTER,					$
				VALUE = 'Create MPEG',				$
				UVALUE = 'create' )
	cancel	= WIDGET_BUTTON(						$
				btn_base,					$
				/ALIGN_CENTER,					$
				VALUE = 'Cancel',				$
				UVALUE = 'cancel' )
				
	WIDGET_CONTROL, m_base, /REALIZE
	
	ptr	= PTR_NEW( {							$
				s_lbl		: s_lbl,			$
				e_lbl		: e_lbl,			$
				out_txt		: out_txt,			$
				filename	: '',				$
				forward		: 1,				$
				seval		: [ sval, eval ],		$
				cancel_pressed	: 0 } )
	WIDGET_CONTROL, m_base, SET_UVALUE = ptr
	
	XMANAGER, 'get_mpeg_info', m_base, EVENT_HANDLER = 'get_mpeg_info_eh'
	
	s	= { forward: (*ptr).forward, cancel_pressed:(*ptr).cancel_pressed, filename:(*ptr).filename }
	PTR_FREE, ptr
	
	RETURN, s
	
END
; get_mpeg_info

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ scroll_controls_eh @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO scroll_controls_eh, event
	widget_type	= TAG_NAMES( event, /STRUCTURE_NAME )
	
	CASE STRUPCASE(widget_type) OF
		'WIDGET_TIMER': BEGIN
			update_scroll_display, event.id
			WIDGET_CONTROL, event.id, GET_UVALUE = scroll_struct
			position	= ROUND(( FLOAT( scroll_struct.xy_idx ) / FLOAT( N_ELEMENTS( scroll_struct.interp_x_offsets ) ) ) * 100.0)
			WIDGET_CONTROL, (*(scroll_struct.window_info_ptr)).scroll_controls_base, GET_UVALUE = control_struct
			WIDGET_CONTROL, control_struct.position_slider, SET_VALUE = position
			
			IF NOT scroll_struct.make_mpeg THEN BEGIN
				CASE 1 OF
					scroll_struct.xy_idx GE scroll_struct.end_scroll_idx: BEGIN
						WIDGET_CONTROL, control_struct.for_btn, SENSITIVE = 0
						WIDGET_CONTROL, control_struct.stop_btn, SENSITIVE = 0
						WIDGET_CONTROL, control_struct.back_btn, /SENSITIVE
						END
					scroll_struct.xy_idx LE scroll_struct.start_scroll_idx: BEGIN
						WIDGET_CONTROL, control_struct.for_btn, /SENSITIVE
						WIDGET_CONTROL, control_struct.stop_btn, SENSITIVE = 0
						WIDGET_CONTROL, control_struct.back_btn, SENSITIVE = 0
						END
					ELSE: BEGIN
						CASE 1 OF
							scroll_struct.scroll_inc GT 0 AND NOT scroll_struct.done: BEGIN
								WIDGET_CONTROL, control_struct.for_btn, SENSITIVE = 0
								WIDGET_CONTROL, control_struct.stop_btn, /SENSITIVE
								WIDGET_CONTROL, control_struct.back_btn, /SENSITIVE
								END
							scroll_struct.scroll_inc LE 0 AND NOT scroll_struct.done: BEGIN
								WIDGET_CONTROL, control_struct.for_btn, /SENSITIVE
								WIDGET_CONTROL, control_struct.stop_btn, /SENSITIVE
								WIDGET_CONTROL, control_struct.back_btn, SENSITIVE = 0
								END
							ELSE: BEGIN
								WIDGET_CONTROL, control_struct.for_btn, /SENSITIVE
								WIDGET_CONTROL, control_struct.stop_btn, SENSITIVE = 0
								WIDGET_CONTROL, control_struct.back_btn, /SENSITIVE
							END
						ENDCASE
						END
				ENDCASE
			
				IF NOT scroll_struct.done THEN BEGIN
;print,'updating path timing information'
					; Update path timing information.
					;
					now					= SYSTIME( /SECONDS )
					new					= now - *(control_struct.prev_time_ptr)
					avg					= DOUBLE( *(control_struct.avg_count_ptr) )
					old					= *(control_struct.avg_realtime_inc_ptr)
					*(control_struct.avg_realtime_inc_ptr)	= ( old * avg + new ) / ( avg + 1 )
					*(control_struct.avg_count_ptr)		= avg + 1
					*(control_struct.prev_time_ptr)		= now
;print,'now = ',now
;print,'new = ',new
;print,'avg = ',avg
;print,'old = ',old
;print,'*(control_struct.avg_realtime_inc_ptr) = ',*(control_struct.avg_realtime_inc_ptr)
;print,'*(control_struct.avg_count_ptr) = ',*(control_struct.avg_count_ptr)
;print,'*(control_struct.prev_time_ptr) = ',*(control_struct.prev_time_ptr)

					WIDGET_CONTROL, control_struct.position_slider, SENSITIVE = 0
					WIDGET_CONTROL, control_struct.clear_end_btn, SENSITIVE = 0
					time_inc			= scroll_struct.time_inc
					WIDGET_CONTROL, (*(scroll_struct.window_info_ptr)).tlb, TIMER = time_inc
				ENDIF ELSE BEGIN
					WIDGET_CONTROL, control_struct.position_slider, /SENSITIVE
					WIDGET_CONTROL, control_struct.clear_end_btn, /SENSITIVE
				ENDELSE
			ENDIF ELSE BEGIN
				IF NOT WIDGET_INFO(scroll_struct.msg_base, /VALID_ID) THEN					$
					scroll_struct.msg_base	= get_msg_base((*(scroll_struct.window_info_ptr)).tlb)

				WIDGET_CONTROL, scroll_struct.msg_base, /MAP
				
				WIDGET_CONTROL, scroll_struct.msg_base, GET_UVALUE = cancel_status
				
				IF STRUPCASE(cancel_status) EQ 'CANCEL' THEN BEGIN
					MPEG_CLOSE, scroll_struct.mpeg_obj
					OBJ_DESTROY, scroll_struct.mpeg_obj
					WIDGET_CONTROL, scroll_struct.msg_base, MAP = 0
					scroll_struct.make_mpeg	= 0
					WIDGET_CONTROL, event.id, SET_UVALUE = scroll_struct
					WIDGET_CONTROL, control_struct.control_base, /SENSITIVE
					WIDGET_CONTROL, scroll_struct.msg_base, SET_UVALUE = 'NO_CANCEL'
					scroll_struct.done	= 1
					WIDGET_CONTROL, event.id, SET_UVALUE = scroll_struct
					WIDGET_CONTROL, (*(scroll_struct.window_info_ptr)).tlb, TIMER = scroll_struct.time_inc
					RETURN
				ENDIF
				
				MPEG_PUT, scroll_struct.mpeg_obj,								$
					WINDOW = (*(scroll_struct.window_info_ptr)).scroll_window_id,				$
					FRAME = scroll_struct.mpeg_frame_ctr, /ORDER
					
				scroll_struct.mpeg_frame_ctr	= scroll_struct.mpeg_frame_ctr + 1
								
				IF scroll_struct.done THEN BEGIN
					scroll_struct.make_mpeg	= 0
					MPEG_SAVE, scroll_struct.mpeg_obj, FILENAME = scroll_struct.mpeg_filename
;print,'finished saving mpeg'
					MPEG_CLOSE, scroll_struct.mpeg_obj
;print,'finished closing mpeg'
					OBJ_DESTROY, scroll_struct.mpeg_obj
;print,'about to map msg gui to 0'
					WIDGET_CONTROL, scroll_struct.msg_base, MAP = 0
;print,'finished mapping msg gui to 0'
					WIDGET_CONTROL, control_struct.control_base, /SENSITIVE
				ENDIF
				
				WIDGET_CONTROL, (*(scroll_struct.window_info_ptr)).tlb, TIMER = scroll_struct.time_inc
								
				WIDGET_CONTROL, event.id, SET_UVALUE = scroll_struct
				
			ENDELSE
			END
		'WIDGET_BASE': BEGIN
			;----------------------------------------------
			; Resize.
			;----------------------------------------------
			WIDGET_CONTROL, event.id, GET_UVALUE = scroll_window_struct
			
			IF scroll_window_struct.make_mpeg THEN BEGIN
				res	= DIALOG_MESSAGE( 'Cannot resize window during MPEG creation!', /ERROR )
				new_draw_xsize				= (*(scroll_window_struct.window_info_ptr)).base_oldx - (*(scroll_window_struct.window_info_ptr)).margin * 2
				new_draw_ysize				= (*(scroll_window_struct.window_info_ptr)).base_oldy - (*(scroll_window_struct.window_info_ptr)).margin * 2
				WIDGET_CONTROL, (*(scroll_window_struct.window_info_ptr)).tlb, XSIZE = (*(scroll_window_struct.window_info_ptr)).base_oldx
				WIDGET_CONTROL, (*(scroll_window_struct.window_info_ptr)).tlb, YSIZE = (*(scroll_window_struct.window_info_ptr)).base_oldy
				WIDGET_CONTROL, (*(scroll_window_struct.window_info_ptr)).draw_widget_id, XSIZE = new_draw_xsize
				WIDGET_CONTROL, (*(scroll_window_struct.window_info_ptr)).draw_widget_id, YSIZE = new_draw_ysize
				RETURN
			ENDIF

			IF event.x NE (*(scroll_window_struct.window_info_ptr)).base_oldx $
			OR event.y NE (*(scroll_window_struct.window_info_ptr)).base_oldy THEN BEGIN
;print,''
;print,'********************************************************************************
;print,'                RESIZING'
;print,'********************************************************************************
;print,''
				;---------------------------------------------------------------
				; Subtract double the margin value to account for both sides 
				; (top & bottom) or (left & right).
				;---------------------------------------------------------------
				new_draw_xsize				= event.x - (*(scroll_window_struct.window_info_ptr)).margin * 2
				new_draw_ysize				= event.y - (*(scroll_window_struct.window_info_ptr)).margin * 2
;				scroll_window_struct.n_display_blks	= new_draw_ysize / scroll_window_struct.block_size_y
				WIDGET_CONTROL, (*(scroll_window_struct.window_info_ptr)).tlb, XSIZE = event.x
				WIDGET_CONTROL, (*(scroll_window_struct.window_info_ptr)).tlb, YSIZE = event.y
				WIDGET_CONTROL, (*(scroll_window_struct.window_info_ptr)).draw_widget_id, XSIZE = new_draw_xsize
				WIDGET_CONTROL, (*(scroll_window_struct.window_info_ptr)).draw_widget_id, YSIZE = new_draw_ysize
				WDELETE, (*(scroll_window_struct.window_info_ptr)).pixmap_id
				WINDOW, /FREE, /PIXMAP, XSIZE = new_draw_xsize, YSIZE = new_draw_ysize
				(*(scroll_window_struct.window_info_ptr)).pixmap_id	= !D.WINDOW
				(*(scroll_window_struct.window_info_ptr)).base_oldx	= event.x
				(*(scroll_window_struct.window_info_ptr)).base_oldy	= event.y
				scroll_window_struct.scroll_window_x	= new_draw_xsize
				scroll_window_struct.scroll_window_y	= new_draw_ysize
;print,'new draw x, y size = ',new_draw_xsize,new_draw_ysize
				WIDGET_CONTROL, event.id, SET_UVALUE = scroll_window_struct
				scroll_display,								$
					(*(scroll_window_struct.window_info_ptr)).pixmap_id,		$
					scroll_window_struct.interp_x_offsets[scroll_window_struct.xy_idx],	$
					scroll_window_struct.interp_y_offsets[scroll_window_struct.xy_idx],	$
					scroll_window_struct.x_off,					$
					scroll_window_struct.y_off,					$
					scroll_window_struct.block,					$
					scroll_window_struct.display_offset_x,				$
					scroll_window_struct.display_offset_y
					
				update_scroll_display, (*(scroll_window_struct.window_info_ptr)).tlb
				RETURN
			ENDIF
			WIDGET_CONTROL, event.id, SET_UVALUE = scroll_window_struct
		END
		'WIDGET_BUTTON': BEGIN
			WIDGET_CONTROL, event.id, GET_UVALUE = button_name
			CASE STRUPCASE(button_name) OF
				'DISMISS': BEGIN
					WIDGET_CONTROL, event.top, GET_UVALUE = control_struct
					WIDGET_CONTROL, event.top, /DESTROY
					WIDGET_CONTROL, control_struct.tlb, /DESTROY
					END
				'BACKWARD': BEGIN
					WIDGET_CONTROL, event.top, GET_UVALUE = control_struct
					WIDGET_CONTROL, control_struct.tlb, GET_UVALUE = scroll_struct
					scroll_struct.scroll_inc	= ABS( scroll_struct.scroll_inc ) * (-1)
					IF scroll_struct.done THEN BEGIN
						scroll_struct.done	= 0
						WIDGET_CONTROL, (*(scroll_struct.window_info_ptr)).tlb, TIMER = scroll_struct.time_inc
					ENDIF
					WIDGET_CONTROL, event.id, SENSITIVE = 0
					WIDGET_CONTROL, control_struct.for_btn, /SENSITIVE
					WIDGET_CONTROL, control_struct.stop_btn, /SENSITIVE
					WIDGET_CONTROL, control_struct.position_slider, SENSITIVE = 0
					WIDGET_CONTROL, control_struct.tlb, SET_UVALUE = scroll_struct

					; Reinitialize path timing information.
					;
					*(control_struct.avg_realtime_inc_ptr)	= 0.0d
					*(control_struct.avg_count_ptr)		= 0L
					*(control_struct.prev_time_ptr)		= SYSTIME( /SECONDS )
					END
				'STOP': BEGIN
					WIDGET_CONTROL, event.top, GET_UVALUE = control_struct
					WIDGET_CONTROL, control_struct.tlb, GET_UVALUE = scroll_struct
					scroll_struct.done	= 1
					WIDGET_CONTROL, event.id, SENSITIVE = 0
					WIDGET_CONTROL, control_struct.back_btn, /SENSITIVE
					WIDGET_CONTROL, control_struct.for_btn, /SENSITIVE
					WIDGET_CONTROL, control_struct.position_slider, /SENSITIVE
					WIDGET_CONTROL, control_struct.tlb, SET_UVALUE = scroll_struct
					END
				'FORWARD': BEGIN
					WIDGET_CONTROL, event.top, GET_UVALUE = control_struct
					WIDGET_CONTROL, control_struct.tlb, GET_UVALUE = scroll_struct
					scroll_struct.scroll_inc	= ABS( scroll_struct.scroll_inc )
					IF scroll_struct.done THEN BEGIN
						scroll_struct.done	= 0
						WIDGET_CONTROL, (*(scroll_struct.window_info_ptr)).tlb, TIMER = scroll_struct.time_inc
					ENDIF
					WIDGET_CONTROL, event.id, SENSITIVE = 0
					WIDGET_CONTROL, control_struct.back_btn, /SENSITIVE
					WIDGET_CONTROL, control_struct.stop_btn, /SENSITIVE
					WIDGET_CONTROL, control_struct.position_slider, SENSITIVE = 0
					WIDGET_CONTROL, control_struct.tlb, SET_UVALUE = scroll_struct

					; Reinitialize path timing information.
					;
					*(control_struct.avg_realtime_inc_ptr)	= 0.0d
					*(control_struct.avg_count_ptr)		= 0L
					*(control_struct.prev_time_ptr)		= SYSTIME( /SECONDS )
					END
				'PATH': BEGIN
					WIDGET_CONTROL, event.top, GET_UVALUE = control_struct
					WIDGET_CONTROL, control_struct.tlb, GET_UVALUE = scroll_struct
					write_scroll_path_file, control_struct, scroll_struct
					WIDGET_CONTROL, control_struct.tlb, SET_UVALUE = scroll_struct
					END
				'MPEG': BEGIN


	;=======================================================================
	; basic error catch mechanism, generally for file read or write when 
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== scroll_controls_eh (MPEG) =========='
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


					WIDGET_CONTROL, event.top, GET_UVALUE = control_struct
					WIDGET_CONTROL, control_struct.tlb, GET_UVALUE = scroll_struct
					
					scroll_struct.done		= 0
					scroll_struct.make_mpeg	= 1
					
					res			= get_mpeg_info(scroll_struct)
					
					IF NOT res.cancel_pressed AND res.filename NE '' THEN BEGIN
						scroll_struct.mpeg_filename	= res.filename
						IF res.forward THEN BEGIN
							scroll_struct.xy_idx	=		$
								scroll_struct.start_scroll_idx
							scroll_struct.scroll_inc	= ABS(scroll_struct.scroll_inc)
						ENDIF ELSE BEGIN
							scroll_struct.xy_idx	=		$
								scroll_struct.end_scroll_idx
						
							scroll_struct.scroll_inc	= ABS(scroll_struct.scroll_inc)*(-1)
						ENDELSE
						
						scroll_display,							$
							(*(scroll_struct.window_info_ptr)).pixmap_id,		$
							scroll_struct.interp_x_offsets[scroll_struct.xy_idx],		$
							scroll_struct.interp_y_offsets[scroll_struct.xy_idx],		$
							scroll_struct.x_off,					$
							scroll_struct.y_off,					$
							scroll_struct.block,					$
							scroll_struct.display_offset_x,				$
							scroll_struct.display_offset_y
							
						WIDGET_CONTROL, control_struct.control_base, SENSITIVE = 0
						scroll_struct.mpeg_obj	= MPEG_OPEN([scroll_struct.scroll_window_x,	$
											scroll_struct.scroll_window_y])
						scroll_struct.mpeg_frame_ctr	= 0L
						WIDGET_CONTROL, (*(scroll_struct.window_info_ptr)).tlb, TIMER = scroll_struct.time_inc
						
					ENDIF
					
					WIDGET_CONTROL, control_struct.tlb, SET_UVALUE = scroll_struct
					END
				'SET1': BEGIN
					WIDGET_CONTROL, event.top, GET_UVALUE = control_struct
					WIDGET_CONTROL, control_struct.tlb, GET_UVALUE = scroll_struct
					IF scroll_struct.xy_idx GT scroll_struct.end_scroll_idx THEN BEGIN
						msg	= [						$
								'Starting frame index cannot be',	$
								'greater than ending frame index!' ]
						res	= DIALOG_MESSAGE( msg, /ERROR )
					ENDIF ELSE BEGIN
						scroll_struct.start_scroll_idx	= scroll_struct.xy_idx
						WIDGET_CONTROL, control_struct.position_slider, GET_VALUE = pct
						spct	= 'Set Starting Position (' + STRTRIM(STRING(pct),2) + '%)'
						WIDGET_CONTROL, control_struct.set_end1_btn, SET_VALUE = spct
					ENDELSE
					
					WIDGET_CONTROL, control_struct.tlb, SET_UVALUE = scroll_struct
					END
				'SET2': BEGIN
					WIDGET_CONTROL, event.top, GET_UVALUE = control_struct
					WIDGET_CONTROL, control_struct.tlb, GET_UVALUE = scroll_struct
					IF scroll_struct.xy_idx LT scroll_struct.start_scroll_idx THEN BEGIN
						msg	= [					$
								'Ending frame index cannot be',	$
								'less than starting frame index!' ]
						res	= DIALOG_MESSAGE( msg, /ERROR )
					ENDIF ELSE BEGIN
						scroll_struct.end_scroll_idx	= scroll_struct.xy_idx
						WIDGET_CONTROL, control_struct.position_slider, GET_VALUE = pct
						spct	= 'Set Ending Position (' + STRTRIM(STRING(pct),2) + '%)'
						WIDGET_CONTROL, control_struct.set_end2_btn, SET_VALUE = spct
					ENDELSE
					WIDGET_CONTROL, control_struct.tlb, SET_UVALUE = scroll_struct
					END
				'CLEAR': BEGIN
					WIDGET_CONTROL, event.top, GET_UVALUE = control_struct
					WIDGET_CONTROL, control_struct.tlb, GET_UVALUE = scroll_struct
					scroll_struct.start_scroll_idx	= 0L
					scroll_struct.end_scroll_idx		= N_ELEMENTS(scroll_struct.interp_x_offsets)-1
					spct	= 'Set Starting Position (0%)'
					WIDGET_CONTROL, control_struct.set_end1_btn, SET_VALUE = spct
					spct	= 'Set Ending Position (100%)'
					WIDGET_CONTROL, control_struct.set_end2_btn, SET_VALUE = spct
					IF scroll_struct.xy_idx GE scroll_struct.end_scroll_idx THEN BEGIN
						WIDGET_CONTROL, control_struct.for_btn, SENSITIVE = 0
						WIDGET_CONTROL, control_struct.stop_btn, SENSITIVE = 0
						WIDGET_CONTROL, control_struct.position_slider, /SENSITIVE
						WIDGET_CONTROL, control_struct.back_btn, /SENSITIVE
					ENDIF
					IF scroll_struct.xy_idx LE scroll_struct.start_scroll_idx THEN BEGIN
						WIDGET_CONTROL, control_struct.for_btn, /SENSITIVE
						WIDGET_CONTROL, control_struct.stop_btn, SENSITIVE = 0
						WIDGET_CONTROL, control_struct.back_btn, SENSITIVE = 0
						WIDGET_CONTROL, control_struct.position_slider, /SENSITIVE
					ENDIF
					IF scroll_struct.xy_idx GT scroll_struct.start_scroll_idx AND scroll_struct.xy_idx LT scroll_struct.end_scroll_idx THEN BEGIN
						WIDGET_CONTROL, control_struct.for_btn, /SENSITIVE
						WIDGET_CONTROL, control_struct.stop_btn, SENSITIVE = 0
						WIDGET_CONTROL, control_struct.back_btn, /SENSITIVE
						WIDGET_CONTROL, control_struct.position_slider, /SENSITIVE
					ENDIF
					WIDGET_CONTROL, control_struct.tlb, SET_UVALUE = scroll_struct
					END
				'OPTION': BEGIN
					WIDGET_CONTROL, event.top, GET_UVALUE = control_struct
					WIDGET_CONTROL, control_struct.tlb, GET_UVALUE = scroll_struct
					scroll_struct	= show_scroll_options( scroll_struct )
					WIDGET_CONTROL, control_struct.tlb, SET_UVALUE = scroll_struct
;print,'scroll_struct.xy_idx=',scroll_struct.xy_idx
;print,'n_elements(scroll_struct.interp_x_offsets) = ',n_elements(scroll_struct.interp_x_offsets)
					scroll_display,						$
						(*(scroll_struct.window_info_ptr)).pixmap_id,	$
						scroll_struct.interp_x_offsets[scroll_struct.xy_idx],	$
						scroll_struct.interp_y_offsets[scroll_struct.xy_idx],	$
						scroll_struct.x_off,				$
						scroll_struct.y_off,				$
						scroll_struct.block,				$
						scroll_struct.display_offset_x,			$
						scroll_struct.display_offset_y

;;;JZH Dec99				update_scroll_display, (*(scroll_struct.window_info_ptr)).tlb
					END
				ELSE:
			ENDCASE
			END
		'WIDGET_SLIDER': BEGIN
			WIDGET_CONTROL, event.id, GET_UVALUE = slider_name
			CASE STRUPCASE(slider_name) OF
				'POSITION': BEGIN
					WIDGET_CONTROL, event.top, GET_UVALUE = control_struct
					WIDGET_CONTROL, control_struct.tlb, GET_UVALUE = scroll_struct

					tmp_idx			= ROUND((FLOAT( event.value ) / 100.0) * FLOAT( N_ELEMENTS( scroll_struct.interp_x_offsets ) - 1 ))

					CASE 1 OF
						tmp_idx LT scroll_struct.start_scroll_idx: val2use = scroll_struct.start_scroll_idx
						tmp_idx GT scroll_struct.end_scroll_idx: val2use = scroll_struct.end_scroll_idx
						ELSE: val2use	= tmp_idx
					ENDCASE
					
					pos	= ROUND((FLOAT(val2use)/FLOAT(N_ELEMENTS(scroll_struct.interp_x_offsets)))*100.0)
					
					WIDGET_CONTROL, control_struct.position_slider, SET_VALUE = pos

					scroll_struct.xy_idx	= val2use
					IF scroll_struct.xy_idx GE scroll_struct.end_scroll_idx THEN BEGIN
						WIDGET_CONTROL, control_struct.for_btn, SENSITIVE = 0
						WIDGET_CONTROL, control_struct.stop_btn, SENSITIVE = 0
						WIDGET_CONTROL, control_struct.position_slider, /SENSITIVE
						WIDGET_CONTROL, control_struct.back_btn, /SENSITIVE
					ENDIF
					IF scroll_struct.xy_idx LE scroll_struct.start_scroll_idx THEN BEGIN
						WIDGET_CONTROL, control_struct.for_btn, /SENSITIVE
						WIDGET_CONTROL, control_struct.stop_btn, SENSITIVE = 0
						WIDGET_CONTROL, control_struct.back_btn, SENSITIVE = 0
						WIDGET_CONTROL, control_struct.position_slider, /SENSITIVE
					ENDIF
					IF scroll_struct.xy_idx GT scroll_struct.start_scroll_idx AND scroll_struct.xy_idx LT scroll_struct.end_scroll_idx THEN BEGIN
						WIDGET_CONTROL, control_struct.for_btn, /SENSITIVE
						WIDGET_CONTROL, control_struct.stop_btn, SENSITIVE = 0
						WIDGET_CONTROL, control_struct.back_btn, /SENSITIVE
						WIDGET_CONTROL, control_struct.position_slider, /SENSITIVE
					ENDIF
					WIDGET_CONTROL, control_struct.tlb, SET_UVALUE = scroll_struct
					scroll_display,						$
						(*(scroll_struct.window_info_ptr)).pixmap_id,	$
						scroll_struct.interp_x_offsets[scroll_struct.xy_idx],	$
						scroll_struct.interp_y_offsets[scroll_struct.xy_idx],	$
						scroll_struct.x_off,				$
						scroll_struct.y_off,				$
						scroll_struct.block,				$
						scroll_struct.display_offset_x,			$
						scroll_struct.display_offset_y
					
					update_scroll_display, (*(scroll_struct.window_info_ptr)).tlb
					END
				'SPEED': BEGIN
					WIDGET_CONTROL, event.top, GET_UVALUE = control_struct
					WIDGET_CONTROL, control_struct.tlb, GET_UVALUE = scroll_struct
					
					IF event.value GT 50 THEN BEGIN
						scroll_struct.time_inc	= scroll_struct.min_time_inc
						scroll_struct.scroll_inc	= event.value - 49
					ENDIF ELSE BEGIN
						scroll_struct.scroll_inc	= 1
						scroll_struct.time_inc	= (FLOAT(50 - event.value) / 100.0) + scroll_struct.min_time_inc
					ENDELSE
					
					WIDGET_CONTROL, control_struct.tlb, SET_UVALUE = scroll_struct

					; Reinitialize path timing information.
					;
					*(control_struct.avg_realtime_inc_ptr)	= 0.0d
					*(control_struct.avg_count_ptr)		= 0L
					*(control_struct.prev_time_ptr)		= SYSTIME( /SECONDS )
					END
				ELSE:
			ENDCASE
			END
		ELSE:
	ENDCASE
	
END
; scroll_controls_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ setup_scroll_controls_base @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION setup_scroll_controls_base, tlb
	control_base	= WIDGET_BASE(					$
					/COLUMN,			$
					/FLOATING,			$
					GROUP_LEADER = tlb,		$
					TITLE = 'Scroll Controls',		$
					/BASE_ALIGN_CENTER,		$
					EVENT_PRO = 'scroll_controls_eh' )
					
	slider_base		= WIDGET_BASE(				$
					control_base,			$
					/ROW,				$
					/ALIGN_CENTER,			$
					/BASE_ALIGN_CENTER )
					
	position_slider		= WIDGET_SLIDER(			$
					slider_base,			$
					/FRAME,				$
					/ALIGN_CENTER,			$
					VALUE = 0,			$
					TITLE = 'Scroll Position (%)',	$
					UVALUE = 'position',		$
					/DRAG )
	speed_slider		= WIDGET_SLIDER(			$
					slider_base,			$
					/FRAME,				$
					/ALIGN_CENTER,			$
					VALUE = 50,			$
					MINIMUM = 1,			$
					TITLE = 'Scroll Speed (%)',	$
					UVALUE = 'speed' )
					
	vcr_base		= WIDGET_BASE(				$
					control_base,			$
					/ROW,				$
					/ALIGN_CENTER,			$
					/BASE_ALIGN_CENTER )
					
	bitmap			= BYTARR(32,32)+255B
	FOR y = 11, 20 DO bitmap[16-(y-11):16+(y-11),y] = 0B
	fbitmap			= REVERSE( ROT( bitmap, 90 ), 1 )
	bbitmap			= ROT( bitmap, 90 )
	sbitmap			= BYTARR(32,32)+255B
	sbitmap[6:25,6:25]	= 0B
	
	back_btn		= WIDGET_BUTTON(			$
					vcr_base,			$
					/ALIGN_CENTER,			$
					VALUE = CVTTOBM(bbitmap),	$
					UVALUE = 'backward' )
					
	stop_btn		= WIDGET_BUTTON(			$
					vcr_base,			$
					/ALIGN_CENTER,			$
					VALUE = CVTTOBM(sbitmap),	$
					SENSITIVE = 0,			$
					UVALUE = 'stop' )
	
	for_btn			= WIDGET_BUTTON(			$
					vcr_base,			$
					/ALIGN_CENTER,			$
					VALUE = CVTTOBM(fbitmap),	$
					UVALUE = 'forward' )
					
	sval			= 'Set Starting Position (0%)'
	set_end1_btn		= WIDGET_BUTTON(			$
					control_base,			$
					VALUE = sval,			$
					UVALUE = 'set1',		$
					/DYNAMIC_RESIZE,		$
					/ALIGN_CENTER )
	
	sval			= 'Set Ending Position (100%)'
	set_end2_btn		= WIDGET_BUTTON(			$
					control_base,			$
					VALUE = sval,			$
					/DYNAMIC_RESIZE,		$
					UVALUE = 'set2',		$
					/ALIGN_CENTER )
	clear_end_btn		= WIDGET_BUTTON(			$
					control_base,			$
					VALUE = 'Clear Settings',	$
					UVALUE = 'clear',		$
					/ALIGN_CENTER )
					
	path_btn		= WIDGET_BUTTON(			$
					control_base,			$
					VALUE = 'Write Path File',	$
					UVALUE = 'path',		$
					/ALIGN_CENTER )
	mpeg_btn		= WIDGET_BUTTON(			$
					control_base,			$
					VALUE = 'Write MPEG...',	$
					UVALUE = 'mpeg',		$
					/ALIGN_CENTER )
	option_btn		= WIDGET_BUTTON(			$
					control_base,			$
					VALUE = 'Options...',		$
					UVALUE = 'option',		$
					/ALIGN_CENTER )
	dismiss_btn		= WIDGET_BUTTON(			$
					control_base,			$
					VALUE = 'Dismiss',		$
					UVALUE = 'dismiss',		$
					/ALIGN_CENTER )

	avg_realtime_inc_ptr	= PTR_NEW( 0.0d )
	avg_count_ptr		= PTR_NEW( 0L )
	prev_time_ptr		= PTR_NEW( SYSTIME( /SECONDS ) )

	control_struct		= {						$
				tlb			: tlb,			$
				control_base		: control_base,		$
				position_slider		: position_slider,	$
				speed_slider		: speed_slider,		$
				back_btn		: back_btn,		$
				stop_btn		: stop_btn,		$
				for_btn			: for_btn,		$
				mpeg_btn		: mpeg_btn,		$
				path_btn		: path_btn,		$
				option_btn		: option_btn,		$
				clear_end_btn		: clear_end_btn,	$
				set_end1_btn		: set_end1_btn,		$
				set_end2_btn		: set_end2_btn,		$
				dismiss_btn		: dismiss_btn,		$
				avg_realtime_inc_ptr	: avg_realtime_inc_ptr,	$
				avg_count_ptr		: avg_count_ptr,	$
				prev_time_ptr		: prev_time_ptr }

	WIDGET_CONTROL, control_base, SET_UVALUE = control_struct
	
	RETURN, control_base
END
; setup_scroll_controls_base

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ return_scroll_window @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION return_scroll_window, XSIZE = init_xsize, YSIZE = init_ysize, GROUP_LEADER = group_leader,	$
			N_FRAMES = n_frames
	DEVICE, GET_SCREEN_SIZE = screenSize
	IF NOT KEYWORD_SET(init_xsize) THEN init_xsize	= screenSize[0]*0.6
	IF NOT KEYWORD_SET(init_ysize) THEN init_ysize	= init_xsize*0.8

	;---------------------------------------------------------------
	; The WIDGET_DRAW must be slightly smaller than the WIDGET_BASE.
	; Assign a margin value for the current operating system.  This 
	; portion of the code was developed on an SGI, so the default 
	; margin value is 3 as required by IRIX.  Other systems may 
	; differ.
	;---------------------------------------------------------------
	CASE 1 OF
		!VERSION.OS EQ 'A/UX'		: margin = 3
		!VERSION.OS EQ 'AIX'		: margin = 3
		!VERSION.OS EQ 'DG/UX'		: margin = 3
		!VERSION.OS EQ 'hp-ux'		: margin = 3
		!VERSION.OS EQ 'IRIX'		: margin = 3
		!VERSION.OS EQ 'MacOS'		: margin = 3
		!VERSION.OS EQ 'OSF'		: margin = 3
		!VERSION.OS EQ 'RISC/os'	: margin = 3
		!VERSION.OS EQ 'sunos'		: margin = 3
		!VERSION.OS EQ 'vms'		: margin = 3
		!VERSION.OS EQ 'Win32'		: margin = 3
		ELSE				: margin = 3
	ENDCASE
	
	IF KEYWORD_SET(group_leader) THEN				$
		tlb		= WIDGET_BASE(				$
						TITLE = 'Scroll Window',	$
						/ROW,			$
						GROUP_LEADER =		$
							group_leader,	$
						/BASE_ALIGN_CENTER,	$
						/TLB_SIZE_EVENTS )	$
	ELSE								$
		tlb		= WIDGET_BASE(				$
						TITLE = 'Scroll Window',	$
						/ROW,			$
						/BASE_ALIGN_CENTER,	$
						/TLB_SIZE_EVENTS )
						
	draw_widget	= WIDGET_DRAW(					$
					tlb,				$
					RETAIN = 2,			$
					XSIZE = init_xsize,		$
					YSIZE = init_ysize )
					
	scroll_controls_base	= setup_scroll_controls_base(tlb)
	
	WIDGET_CONTROL, tlb, /REALIZE
	WIDGET_CONTROL, scroll_controls_base, /REALIZE
	WIDGET_CONTROL, draw_widget, GET_VALUE = scroll_window_id
	
	WINDOW, /FREE, /PIXMAP, XSIZE = init_xsize, YSIZE = init_ysize
	pixmap_id = !D.WINDOW

	window_info_ptr = PTR_NEW( {					$
			tlb		: tlb				,$
			scroll_controls_base : scroll_controls_base		,$
			margin		: margin			,$
			draw_widget_id	: draw_widget			,$
			scroll_window_id	: scroll_window_id			,$
			pixmap_id	: pixmap_id			,$
			base_oldx	: init_xsize + margin * 2	,$	; Allow for the margin size around the draw widget.
			base_oldy	: init_ysize + margin * 2	} )	; Allow for the margin size around the draw widget.
	
	XMANAGER, 'scroll_tool', tlb, EVENT_HANDLER = 'scroll_controls_eh', /NO_BLOCK
	
	RETURN, window_info_ptr
END
; return_scroll_window

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ convert_blocks_to_pixmaps @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION convert_blocks_to_pixmaps, rgb_ptrarr
	valid_idx	= WHERE( PTR_VALID(rgb_ptrarr), valid_cnt )
	x		= ( SIZE( (*(rgb_ptrarr[valid_idx[0]]))[*,*,0] ) )[1]	;old pre IMAGE_DATA object way
	y		= ( SIZE( (*(rgb_ptrarr[valid_idx[0]]))[*,*,0] ) )[2]	;old pre IMAGE_DATA object way
	n		= N_ELEMENTS( (*(rgb_ptrarr[valid_idx[0]]))[*,0,0] )
	x		= N_ELEMENTS( (*(rgb_ptrarr[valid_idx[0]]))[0,*,0] )
	y		= N_ELEMENTS( (*(rgb_ptrarr[valid_idx[0]]))[0,0,*] )
	
	id		= LONARR(n)
	w		= !D.WINDOW
	
	FOR i = 0, n-1 DO BEGIN
		WINDOW, /FREE, /PIXMAP, XSIZE = x, YSIZE = y
		id[i] = !D.WINDOW
		FOR j = 0, valid_cnt - 1 DO					$
			TV, (*(rgb_ptrarr[valid_idx[j]]))[i,*,*],		$
				CHANNEL = (valid_idx[j] + 1) * (valid_cnt GT 1)
	ENDFOR
	
	RETURN, id
END
; convert_blocks_to_pixmaps

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ calculate_lower_xy_coords @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION calculate_lower_xy_coords,						$
					in_x_off, in_y_off,			$
					img_size_x, img_size_y,			$
					block_size_x, block_size_y
	n_blocks		= N_ELEMENTS( in_x_off )

	out_x_coords_ptrarr		= PTRARR(n_blocks)
	out_y_coords_ptrarr		= PTRARR(n_blocks)
	n_total_coords			= 0L

	FOR i = 1, n_blocks - 1 DO BEGIN
		diff_x				= ABS(in_x_off[i] - in_x_off[i-1])
		diff_y				= ABS(in_y_off[i] - in_y_off[i-1])
		max_elem			= MAX([diff_x,diff_y]) + 1
		out_x_off			= CONGRID( [ in_x_off[i-1], in_x_off[i] ], max_elem, /INTERP, /MINUS_ONE )
		out_y_off			= CONGRID( [ in_y_off[i-1], in_y_off[i] ], max_elem, /INTERP, /MINUS_ONE )
		out_x_coords_ptrarr[i-1]	= PTR_NEW(out_x_off[0:max_elem-2], /NO_COPY)
		out_y_coords_ptrarr[i-1]	= PTR_NEW(out_y_off[0:max_elem-2], /NO_COPY)
		n_total_coords			= n_total_coords + max_elem - 1
	ENDFOR

	IF img_size_x GT img_size_y THEN BEGIN
		out_x_off			= CONGRID( [ in_x_off[n_blocks-1], in_x_off[n_blocks-1] + block_size_x - 1 ], block_size_x, /INTERP, /MINUS_ONE )
		out_y_off			= CONGRID( [ in_y_off[n_blocks-1], in_y_off[n_blocks-1] ], block_size_x, CUBIC=(-0.5), /MINUS_ONE )
	ENDIF ELSE BEGIN
		out_y_off			= CONGRID( [ in_y_off[n_blocks-1], in_y_off[n_blocks-1] + block_size_y - 1 ], block_size_y, /INTERP, /MINUS_ONE )
		out_x_off			= CONGRID( [ in_x_off[n_blocks-1], in_x_off[n_blocks-1] ], block_size_y, CUBIC=(-0.5), /MINUS_ONE )
	ENDELSE

	out_x_coords_ptrarr[n_blocks-1]	= PTR_NEW(out_x_off[0:max_elem-2], /NO_COPY)
	out_y_coords_ptrarr[n_blocks-1]	= PTR_NEW(out_y_off[0:max_elem-2], /NO_COPY)
	n_total_coords			= n_total_coords + max_elem - 1
	
	interp_x_offsets			= LONARR(n_total_coords)
	interp_y_offsets			= LONARR(n_total_coords)
	ctr					= 0L

	FOR i = 0, n_blocks - 1 DO BEGIN
		n				= N_ELEMENTS(*(out_x_coords_ptrarr[i]))
		interp_x_offsets[ctr:ctr+n-1]	= *(out_x_coords_ptrarr[i])
		interp_y_offsets[ctr:ctr+n-1]	= *(out_y_coords_ptrarr[i])
		ctr				= ctr+n
	ENDFOR
	
	PTR_FREE, out_x_coords_ptrarr
	PTR_FREE, out_y_coords_ptrarr

	RETURN, { interp_x_offsets:interp_x_offsets, interp_y_offsets:interp_y_offsets }

END
;calculate_lower_xy_coords

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ get_msg_base_eh @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO get_msg_base_eh, event
	WIDGET_CONTROL, event.top, SET_UVALUE = 'CANCEL'
END
; get_msg_base_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ get_msg_base @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION get_msg_base, tlb
	top	= WIDGET_BASE(							$
				GROUP_LEADER = tlb,				$
				MAP = 0,					$
				/COLUMN,					$
				TITLE = 'Creating MPEG File',			$
				UVALUE = 'NO_CANCEL',				$
				/BASE_ALIGN_CENTER,				$
				EVENT_PRO = 'get_msg_base_eh' )
	lbl	= WIDGET_LABEL(							$
				top,						$
				/DYNAMIC_RESIZE,				$
				/ALIGN_CENTER,					$
				VALUE = 'Creating MPEG file... please wait...' )
	cancel	= WIDGET_BUTTON(						$
				top,						$
				/ALIGN_CENTER,					$
				VALUE = 'Cancel MPEG Creation',			$
				UVALUE = 'cancel' )
	
	WIDGET_CONTROL, top, /REALIZE
	
	XMANAGER, 'get_msg_base', top, EVENT_HANDLER = 'get_msg_base_eh'
	
	RETURN, top
END
; get_msg_base

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ scroll_tool @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO scroll_tool, dataObjArr, RESOLUTION = resolution, GROUP_LEADER = group_leader

	rgb_ptrarr = PTRARR( 3 )

	;--------------------------------------------------
	; h_offsets, v_offsets and nblocks are the same for 
	; all three channels, so these will get set by the 
	; last valid dataObj.
	;--------------------------------------------------
	IF OBJ_VALID( dataObjArr[0] ) THEN BEGIN
		red_bytscl_stack_struct	= dataObjArr[0] -> ReturnByteScaledStackStructure( RESOLUTION = resolution )
		h_offsets		= red_bytscl_stack_struct.h_offsets
		v_offsets		= red_bytscl_stack_struct.v_offsets
		nblocks			= N_ELEMENTS( red_bytscl_stack_struct.scaledStack[*,0,0] )
		rgb_ptrarr[0]		= PTR_NEW( red_bytscl_stack_struct.scaledStack, /NO_COPY )
		validObj		= dataObjArr[0]
	ENDIF
	IF OBJ_VALID( dataObjArr[1] ) THEN BEGIN
		grn_bytscl_stack_struct	= dataObjArr[1] -> ReturnByteScaledStackStructure( RESOLUTION = resolution )
		h_offsets		= grn_bytscl_stack_struct.h_offsets
		v_offsets		= grn_bytscl_stack_struct.v_offsets
		nblocks			= N_ELEMENTS( grn_bytscl_stack_struct.scaledStack[*,0,0] )
		rgb_ptrarr[1]		= PTR_NEW( grn_bytscl_stack_struct.scaledStack, /NO_COPY )
		validObj		= dataObjArr[1]
	ENDIF
	IF OBJ_VALID( dataObjArr[2] ) THEN BEGIN
		blu_bytscl_stack_struct	= dataObjArr[2] -> ReturnByteScaledStackStructure( RESOLUTION = resolution )
		h_offsets		= blu_bytscl_stack_struct.h_offsets
		v_offsets		= blu_bytscl_stack_struct.v_offsets
		nblocks			= N_ELEMENTS( blu_bytscl_stack_struct.scaledStack[*,0,0] )
		rgb_ptrarr[2]		= PTR_NEW( blu_bytscl_stack_struct.scaledStack, /NO_COPY )
		validObj		= dataObjArr[2]
	ENDIF

	;--------------------------------------------------
	; Calculate mosaic output size.
	;--------------------------------------------------
	mosaicOutputSize = LONARR( 2 )							;[x,y]
	outResRatio = resolution[0] / FLOAT( ( validObj -> GetTileSize() )[0] )
	mosaicOutputSize[0] = ROUND( outResRatio * validObj -> GetImageWidth() )	;x
	mosaicOutputSize[1] = ROUND( outResRatio * validObj -> GetImageHeight() )	;y
	
	IF nblocks LT 2 THEN BEGIN
		msg	= 'Scroll tool requires at least 2 blocks of data'
		res	= DIALOG_MESSAGE(msg,/INFORMATION)
		RETURN
	ENDIF

	block	= convert_blocks_to_pixmaps( rgb_ptrarr )

	n_blocks2display	= 4
	use_block_pixmaps	= 1

	x_off			= h_offsets
	y_off			= v_offsets
	block_size_x		= resolution[0]
	block_size_y		= resolution[1]
	full_img_size_x		= MAX(x_off) + block_size_x
	full_img_size_y		= MAX(y_off) + block_size_y
	interp_struct		= calculate_lower_xy_coords(x_off,y_off,full_img_size_x,full_img_size_y,block_size_x,block_size_y)
	
	DEVICE, GET_SCREEN_SIZE = screen_size
	
	scroll_window_x		= 512
	scroll_window_y		= 512
	
	IF KEYWORD_SET(group_leader) THEN BEGIN
		groupLeader		= group_leader
		window_info_ptr		= return_scroll_window(			$
						XSIZE = scroll_window_x,		$
						YSIZE = scroll_window_y,		$
						GROUP_LEADER = group_leader )
	ENDIF ELSE BEGIN
		groupLeader		= 0L
		window_info_ptr		= return_scroll_window(			$
						XSIZE = scroll_window_x,		$
						YSIZE = scroll_window_y  )
	ENDELSE
					

	WSET, (*window_info_ptr).scroll_window_id
	ERASE
	
	interp_x_offsets	= interp_struct.interp_x_offsets
	interp_y_offsets	= interp_struct.interp_y_offsets
	xy_idx			= 0L
	scroll_inc			= 1L
	lower_left_y		= LONG(interp_y_offsets[xy_idx])
	lower_left_x		= LONG(interp_x_offsets[xy_idx])
	min_time_inc		= 0.01
	time_inc		= min_time_inc
	display_offset_x	= 0L
	display_offset_y	= 0L
	
	scroll_display,								$
		(*window_info_ptr).pixmap_id,					$
		lower_left_x,							$
		lower_left_y,							$
		x_off,								$
		y_off,								$
		block,								$
		display_offset_x,						$
		display_offset_y
		
	scroll_struct	= {							$
				x_off			: x_off,		$
				y_off			: y_off,		$
				interp_x_offsets	: interp_x_offsets,	$
				interp_y_offsets	: interp_y_offsets,	$
				block			: block,		$
				display_offset_x	: display_offset_x,	$
				display_offset_y	: display_offset_y,	$
				xy_idx			: xy_idx,		$
				msg_base		: get_msg_base((*window_info_ptr).tlb),	$
				mpeg_obj		: OBJ_NEW(),		$
				mpeg_filename		: '',			$
				scroll_inc			: scroll_inc,		$
				time_inc		: time_inc,		$
				min_time_inc		: min_time_inc,		$
				mpeg_frame_ctr		: 0L,			$
				scroll_window_x		: scroll_window_x,		$
				scroll_window_y		: scroll_window_y,		$
				window_info_ptr		: window_info_ptr,	$
				start_scroll_idx		: 0L,			$
				make_mpeg		: 0,			$
				end_scroll_idx		: N_ELEMENTS(interp_x_offsets)-1,	$
				done			: 1B,			$
				groupLeader		: groupLeader,		$
				dataObjArr		: dataObjArr,		$
				mosaicOutputSize	: mosaicOutputSize }
								
	WIDGET_CONTROL, (*window_info_ptr).tlb, SET_UVALUE = scroll_struct
	
	update_scroll_display, (*window_info_ptr).tlb
	
END
; scroll_tool

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ update_scroll_display @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO update_scroll_display, tlb
	WIDGET_CONTROL, tlb, GET_UVALUE = scroll_struct
		
	WSET, (*(scroll_struct.window_info_ptr)).scroll_window_id
	DEVICE, COPY = [							$
		0,								$
		0,								$
		scroll_struct.scroll_window_x,					$
		scroll_struct.scroll_window_y,					$
		0,								$
		0,								$
		(*(scroll_struct.window_info_ptr)).pixmap_id ]
		
	scroll_struct.xy_idx	= scroll_struct.xy_idx + scroll_struct.scroll_inc
	
	IF scroll_struct.xy_idx GT scroll_struct.end_scroll_idx OR scroll_struct.xy_idx LT scroll_struct.start_scroll_idx THEN BEGIN
		scroll_struct.xy_idx	= MIN( [ MAX( [ scroll_struct.start_scroll_idx, scroll_struct.xy_idx ] ), scroll_struct.end_scroll_idx ] )
		scroll_struct.done	= 1B
	ENDIF ELSE BEGIN
		scroll_display,							$
			(*(scroll_struct.window_info_ptr)).pixmap_id,		$
			scroll_struct.interp_x_offsets[scroll_struct.xy_idx],		$
			scroll_struct.interp_y_offsets[scroll_struct.xy_idx],		$
			scroll_struct.x_off,					$
			scroll_struct.y_off,					$
			scroll_struct.block,					$
			scroll_struct.display_offset_x,				$
			scroll_struct.display_offset_y
	ENDELSE

	WIDGET_CONTROL, tlb, SET_UVALUE = scroll_struct
END
; update_scroll_display

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ scroll_display @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO scroll_display,									$
			scroll_window_pixmap_id,						$
			scroll_window_lower_left_x,					$
			scroll_window_lower_left_y,					$
			lower_left_x_block_offsets,					$
			lower_left_y_block_offsets,					$
			pixmap_id_block_stack,						$
			lower_left_x_start,						$
			lower_left_y_start
			
	current_window		= !D.WINDOW
	
	WSET, pixmap_id_block_stack[0]
	block_size_x		= !D.X_SIZE
	block_size_y		= !D.Y_SIZE
	
	
	WSET, scroll_window_pixmap_id
	
	ERASE
	
	; obtain full dimensions of scroll window
	;
	scroll_window_x		= !D.X_SIZE
	scroll_window_y		= !D.Y_SIZE
	
	; account for any offset of the lower left corner (user-defined)
	;
	actual_scroll_window_x	= scroll_window_x - lower_left_x_start
	actual_scroll_window_y	= scroll_window_y - lower_left_y_start
	
	; determine which blocks are to be displayed
	;
	xy_idx		= WHERE(											$
				lower_left_y_block_offsets GT ( scroll_window_lower_left_y - block_size_y ) AND		$
				lower_left_y_block_offsets LT ( scroll_window_lower_left_y + actual_scroll_window_y ) AND	$
				lower_left_x_block_offsets GT ( scroll_window_lower_left_x - block_size_x ) AND		$
				lower_left_x_block_offsets LT ( scroll_window_lower_left_x + actual_scroll_window_x ), xy_cnt )

	IF xy_cnt LE 0 THEN RETURN
	
	n_blocks	= N_ELEMENTS( xy_idx )
	
	; display blocks to pixmap
	;
	FOR i = 0, n_blocks - 1 DO BEGIN
		x_start_dest	= lower_left_x_block_offsets[xy_idx[i]] - scroll_window_lower_left_x + lower_left_x_start
		y_start_dest	= lower_left_y_block_offsets[xy_idx[i]] - scroll_window_lower_left_y + lower_left_y_start
		
		DEVICE, SET_GRAPHICS_FUNCTION = 6
		DEVICE, COPY = [					$
			0,						$
			0,						$
			block_size_x,					$
			block_size_y,					$
			x_start_dest,					$
			y_start_dest,					$
			pixmap_id_block_stack[ xy_idx[i] ] ]
		; SET_GRAPHICS_FUNCTION back to it's default value (GXcopy).
		DEVICE, SET_GRAPHICS_FUNCTION = 3
	ENDFOR
		
	WSET, current_window
	
END
; scroll_display
