@GetCorrectFont.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ create_color_bar @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO create_color_bar,								$
			parent_base,						$
			bottom,							$
			ncolors,						$
			min_data_value,						$
			max_data_value,						$
			width,							$
			height,							$
			desc,							$
			units
			
;print,'creating color bar with following characteristics:'
;print,'bottom=',bottom
;print,'ncolors=',ncolors
;print,'min_data_value=',min_data_value
;print,'max_data_value=',max_data_value
;print,'width=',width
;print,'height=',height
;print,'desc=',desc
;print,'units=',units
END
; create_color_bar

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ create_color_bar_setup_eh @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO create_color_bar_setup_eh, event
	WIDGET_CONTROL, event.id, GEt_UVALUE = widget_name
	WIDGET_CONTROL, event.top, GEt_UVALUE = ptr
	
	CASE STRUPCASE(STRTRIM(widget_name,2)) OF
		'ATTACHUNITS': BEGIN
			(*ptr).attach_units	= event.select
			WIDGET_CONTROL, (*ptr).units_cwf, SENSITIVE = (*ptr).attach_units
			END
		'DISMISS': BEGIN
			WIDGET_CONTROL, event.top, /DESTROY
			END
		'CREATE': BEGIN
			screen_dims	= GET_SCREEN_SIZE()
			WIDGET_CONTROL, (*ptr).width_cwf, GET_VALUE = width
			WIDGET_CONTROL, (*ptr).height_cwf, GET_VALUE = height
			WIDGET_CONTROL, (*ptr).desc_cwf, GET_VALUE = desc
			units	= ''
			IF (*ptr).attach_units THEN				$
				WIDGET_CONTROL, (*ptr).units_cwf, GET_VALUE = units
			IF width LE 0 OR height LE 0 OR width GT screen_dims[0] OR height GT screen_dims[1] THEN BEGIN
				msg	= [					$
						'Problem with width and/or height entries',	$
						'Width of color bar must be greater than',	$
						'0 and less than ' + STRTRIM(screen_dims[0],2),	$
						'and width must be greater than 0 and less',	$
						'than ' + STRTRIM(screen_dims[1],2) + '.' ]
				res	= DIALOG_MESSAGE( msg, /ERROR )
				RETURN
			ENDIF
			create_color_bar,					$
					(*ptr).parent_base,			$
					(*ptr).bottom,				$
					(*ptr).ncolors,				$
					(*ptr).min_data_value,			$
					(*ptr).max_data_value,			$
					width,					$
					height,					$
					desc,					$
					units
			END
		ELSE:
	ENDCASE
END
; create_color_bar_setup_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ create_color_bar_setup @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO create_color_bar_setup,							$
			parent_base,						$
			bottom,							$
			ncolors,						$
			min_data_value,						$
			max_data_value,						$
			data_description_string,				$
			DIMS	= dims

	default_width		= 300
	default_height		= 200
	
	IF KEYWORD_SET(dims) THEN BEGIN
		default_width	= dims[0]
		default_height	= dims[1]
	ENDIF
	
	b		= WIDGET_BASE(						$
					/COLUMN,				$
					GROUP_LEADER = parent_base,		$
					TITLE = 'Color Bar Setup',		$
					TLB_FRAME_ATTR = 9,			$
					/MODAL,					$
					/BASE_ALIGN_CENTER )
					
	font_type	= GetCorrectFont( 'courier2bold' )


	width_title	= 'Color Bar Width:'
	width_cwf	= CW_FIELD(						$
					b,					$
					/INTEGER,				$
					VALUE = default_width,			$
					TITLE = width_title,			$
					FONT = font_type,			$
					UVALUE = 'colorbarwidth' )
	height_title	= 'Color Bar Height:'
	height_cwf	= CW_FIELD(						$
					b,					$
					/INTEGER,				$
					VALUE = default_height,			$
					TITLE = height_title,			$
					FONT = font_type,			$
					UVALUE = 'colorbarheight' )

	desc_title	= 'Data Description:'
	desc_cwf	= CW_FIELD(						$
					b,					$
					/STRING,				$
					VALUE = data_description_string,	$
					XSIZE = 10,				$
					TITLE = desc_title,			$
					FONT = font_type,			$
					UVALUE = 'colorbardesc' )

	sub_base1	= WIDGET_BASE(						$
					b, 					$
					/FRAME,					$
					/COLUMN,				$
					/BASE_ALIGN_CENTER )
	sub_base1a	= WIDGET_BASE(						$
					sub_base1, 				$
					/ROW,					$
					/BASE_ALIGN_CENTER,			$
					/NONEXCLUSIVE )
				
	unit_btn	= WIDGET_BUTTON(					$
					sub_base1a,				$
					FONT = font_type,			$
					VALUE = 'Attach Units',			$
					UVALUE = 'attachunits' )
	units_title	= 'Units String:'
	units_cwf	= CW_FIELD(						$
					sub_base1,				$
					/STRING,				$
					VALUE = '',				$
					TITLE = units_title,			$
					FONT = font_type,			$
					UVALUE = 'unitsdesc' )
	WIDGET_CONTROL, units_cwf, SENSITIVE = 0

	sub_base2	= WIDGET_BASE(						$
					b,					$
					/ROW,					$
					/BASE_ALIGN_CENTER,			$
					/FRAME,					$
					/ALIGN_CENTER )
	create_btn	= WIDGET_BUTTON(					$
					sub_base2,				$
					VALUE = 'Create Color Bar',		$
					FONT = font_type,			$
					UVALUE = 'create' )
	dismiss_btn	= WIDGET_BUTTON(					$
					sub_base2,				$
					VALUE = 'Dismiss',			$
					FONT = font_type,			$
					UVALUE = 'dismiss' )
	WIDGET_CONTROL, b, /REALIZE
	
	WIDGET_CONTROL, units_cwf, SENSITIVE = 0
	
	ptr	= PTR_NEW( {							$
				width_cwf	: width_cwf,			$
				height_cwf	: height_cwf,			$
				desc_cwf	: desc_cwf,			$
				units_cwf	: units_cwf,			$
				attach_units	: 0,				$
				parent_base	: parent_base,			$
				bottom		: bottom,			$
				ncolors		: ncolors,			$
				min_data_value	: min_data_value,		$
				max_data_value	: max_data_value }, /NO_COPY )
				
	WIDGET_CONTROL, b, SET_UVALUE = ptr
		
	XMANAGER, 'create color bar setup', b, EVENT_HANDLER = 'create_color_bar_setup_eh'
	
	PTR_FREE, ptr
	
END
; create_color_bar_setup
