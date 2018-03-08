;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ get_init_o_p_d_info_kill @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO get_init_o_p_d_info_kill, tlb
END
; get_init_o_p_d_info_kill

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ get_init_o_p_d_info @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO get_init_o_p_d_info, event
	WIDGET_CONTROL, event.id, GET_UVALUE = widget_type
	WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
	
	CASE widget_type OF
		'year': BEGIN
			WIDGET_CONTROL, event.id, GET_VALUE = active_year
			WIDGET_CONTROL, (*infoPtr).year_menu, SET_VALUE = active_year
			(*infoPtr).active_year	= active_year
			END
		'month': BEGIN
			WIDGET_CONTROL, event.id, GET_VALUE = active_month
			WIDGET_CONTROL, (*infoPtr).month_menu, SET_VALUE = active_month
			(*infoPtr).active_month	= active_month
			WIDGET_CONTROL, (*infoPtr).day_buttons[28], /SENSITIVE
			WIDGET_CONTROL, (*infoPtr).day_buttons[29], /SENSITIVE
			WIDGET_CONTROL, (*infoPtr).day_buttons[30], /SENSITIVE
			IF active_month EQ 'April' OR				$
			   active_month EQ 'June' OR				$
			   active_month EQ 'September' OR			$
			   active_month EQ 'November' THEN BEGIN
				WIDGET_CONTROL, (*infoPtr).day_buttons[30], SENSITIVE = 0
			ENDIF
			
			IF active_month EQ 'February' THEN BEGIN
				WIDGET_CONTROL, (*infoPtr).day_buttons[28], SENSITIVE = 0
				WIDGET_CONTROL, (*infoPtr).day_buttons[29], SENSITIVE = 0
				WIDGET_CONTROL, (*infoPtr).day_buttons[30], SENSITIVE = 0
			ENDIF
			END
		'day': BEGIN
			WIDGET_CONTROL, event.id, GET_VALUE = active_day
			WIDGET_CONTROL, (*infoPtr).day_menu, SET_VALUE = active_day
			(*infoPtr).active_day		= active_day
			END
		'hour': BEGIN
			WIDGET_CONTROL, event.id, GET_VALUE = active_hour
			WIDGET_CONTROL, (*infoPtr).hour_menu, SET_VALUE = active_hour
			(*infoPtr).active_hour		= active_hour
			END
		'minute': BEGIN
			WIDGET_CONTROL, event.id, GET_VALUE = active_minute
			WIDGET_CONTROL, (*infoPtr).minute_menu, SET_VALUE = active_minute
			(*infoPtr).active_minute	= active_minute
			END
		'seconds': BEGIN
			WIDGET_CONTROL, event.id, GET_VALUE = active_seconds
			WIDGET_CONTROL, (*infoPtr).seconds_menu, SET_VALUE = active_seconds
			(*infoPtr).active_seconds	= active_seconds
			END
		'ok': BEGIN
			WIDGET_CONTROL, (*infoPtr).orbit_cw, GET_VALUE = active_orbit
			WIDGET_CONTROL, (*infoPtr).path_cw, GET_VALUE = active_path
			IF active_orbit LE 0 THEN BEGIN
				msg	= 'Orbit values must be greater than 0!'
				res	= DIALOG_MESSAGE( msg, /ERROR )
				RETURN
			ENDIF
			IF active_path LE 0 OR active_path GT 233 THEN BEGIN
				msg	= 'Path values must be between 1 and 233 inclusive!'
				res	= DIALOG_MESSAGE( msg, /ERROR )
				RETURN
			ENDIF
			(*infoPtr).active_orbit	= active_orbit
			(*infoPtr).active_path	= active_path

			WIDGET_CONTROL, (*infoPtr).year_menu, GET_VALUE = active_year
			WIDGET_CONTROL, (*infoPtr).month_menu, GET_VALUE = active_month
			WIDGET_CONTROL, (*infoPtr).day_menu, GET_VALUE = active_day
			WIDGET_CONTROL, (*infoPtr).hour_menu, GET_VALUE = active_hour
			WIDGET_CONTROL, (*infoPtr).minute_menu, GET_VALUE = active_minute
			WIDGET_CONTROL, (*infoPtr).seconds_menu, GET_VALUE = active_seconds

;print,'active_path = ',active_path
;print,'active_orbit = ',active_orbit
;print,'active_year = ',active_year
;print,'active_month = ',active_month
;print,'active_day = ',active_day
;print,'active_hour = ',active_hour
;print,'active_minute = ',active_minute
;print,'active_seconds = ',active_seconds

			*((*infoPtr).INITIAL_VALUES_STRUCT_PTR) = {			$
						initial_path	: active_path		,$
						initial_orbit	: active_orbit		,$
						initial_year	: active_year		,$
						initial_month	: active_month		,$
						initial_day	: active_day		,$
						initial_hour	: active_hour		,$
						initial_minute	: active_minute		,$
						initial_seconds	: active_seconds	$
						}

;help,(*infoPtr).INITIAL_VALUES_STRUCT_PTR
;if ptr_valid( (*infoPtr).INITIAL_VALUES_STRUCT_PTR ) then begin
;help,*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)
;if size(*((*infoPtr).INITIAL_VALUES_STRUCT_PTR),/type) eq 8 then begin
;print,tag_names( *((*infoPtr).INITIAL_VALUES_STRUCT_PTR) )
;for i=0,7 do print,(*(*infoPtr).INITIAL_VALUES_STRUCT_PTR).(i)
;endif
;endif
			WIDGET_CONTROL, event.top, /DESTROY
			END
		'defaults': BEGIN
			*((*infoPtr).INITIAL_VALUES_STRUCT_PTR) =	$
					{				$
					initial_path	: '100'		,$
					initial_orbit	: '1000'	,$
					initial_year	: '2000'	,$
					initial_month	: 'February'	,$
					initial_day	: '25'		,$
					initial_hour	: '00'		,$
					initial_minute	: '25'		,$
					initial_seconds	: '05'		$
					}

			(*infoPtr).active_orbit		= (*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)).initial_orbit
			(*infoPtr).active_path		= (*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)).initial_path
			(*infoPtr).active_year		= (*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)).initial_year
			(*infoPtr).active_month		= (*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)).initial_month
			(*infoPtr).active_day		= (*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)).initial_day
			(*infoPtr).active_hour		= (*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)).initial_hour
			(*infoPtr).active_minute	= (*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)).initial_minute
			(*infoPtr).active_seconds	= (*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)).initial_seconds

			WIDGET_CONTROL, (*infoPtr).orbit_cw, SET_VALUE = (*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)).initial_orbit
			WIDGET_CONTROL, (*infoPtr).path_cw, SET_VALUE = (*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)).initial_path
			WIDGET_CONTROL, (*infoPtr).year_menu, SET_VALUE = (*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)).initial_year
			WIDGET_CONTROL, (*infoPtr).month_menu, SET_VALUE = (*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)).initial_month
			WIDGET_CONTROL, (*infoPtr).day_menu, SET_VALUE = (*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)).initial_day
			WIDGET_CONTROL, (*infoPtr).hour_menu, SET_VALUE = (*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)).initial_hour
			WIDGET_CONTROL, (*infoPtr).minute_menu, SET_VALUE = (*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)).initial_minute
			WIDGET_CONTROL, (*infoPtr).seconds_menu, SET_VALUE = (*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)).initial_seconds

;print,'active_path = ',(*infoPtr).active_path
;print,'active_orbit = ',(*infoPtr).active_orbit
;print,'active_year = ',(*infoPtr).active_year
;print,'active_month = ',(*infoPtr).active_month
;print,'active_day = ',(*infoPtr).active_day
;print,'active_hour = ',(*infoPtr).active_hour
;print,'active_minute = ',(*infoPtr).active_minute
;print,'active_seconds = ',(*infoPtr).active_seconds

;help,(*infoPtr).INITIAL_VALUES_STRUCT_PTR
;if ptr_valid( (*infoPtr).INITIAL_VALUES_STRUCT_PTR ) then begin
;help,*((*infoPtr).INITIAL_VALUES_STRUCT_PTR)
;if size(*((*infoPtr).INITIAL_VALUES_STRUCT_PTR),/type) eq 8 then begin
;print,tag_names( *((*infoPtr).INITIAL_VALUES_STRUCT_PTR) )
;for i=0,7 do print,(*(*infoPtr).INITIAL_VALUES_STRUCT_PTR).(i)
;endif
;endif
			END
		ELSE:
	ENDCASE

END
; get_init_o_p_d_info

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@ misr_get_initial_orbit_path_date_info @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION misr_get_initial_orbit_path_date_info,			$
	INITIAL_VALUES_STRUCT_PTR = INITIAL_VALUES_STRUCT_PTR,	$
	NO_GUI = NO_GUI,					$
	DEFAULTS = DEFAULTS

	year_names	= [	'1997', '1998', '1999', $
				'2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', $
				'2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020' ]
	month_names	= [ 'January',						$
			    'February',						$
			    'March',						$
			    'April',						$
			    'May',						$
			    'June',						$
			    'July',						$
			    'August',						$
			    'September',					$
			    'October',						$
			    'November',						$
			    'December' ]
	day_names	= STRING( INDGEN(31) + 1 )
	hour_names	= STRING( INDGEN(24) )
	minute_names	= STRING( INDGEN(12) * 5 )
	seconds_names	= STRING( INDGEN(12) * 5 )
	year_buttons	= LONARR( N_ELEMENTS( year_names ) )
	day_buttons	= LONARR(31)
	month_buttons	= LONARR(12)
	hour_buttons	= LONARR(24)
	minute_buttons	= LONARR(12)
	seconds_buttons	= LONARR(12)

	;------------------------------------------------------------------
	; This keyword is used for passing in new intialization values when 
	; the reinitialize option is invoked by the user after startup.
	;------------------------------------------------------------------
;help,INITIAL_VALUES_STRUCT_PTR
;if ptr_valid(INITIAL_VALUES_STRUCT_PTR) then begin
;help,*INITIAL_VALUES_STRUCT_PTR
;if size(*INITIAL_VALUES_STRUCT_PTR,/type) eq 8 then begin
;print,tag_names(*INITIAL_VALUES_STRUCT_PTR)
;for i=0,7 do print,(*INITIAL_VALUES_STRUCT_PTR).(i)
;endif
;endif
	IF NOT KEYWORD_SET( INITIAL_VALUES_STRUCT_PTR ) OR KEYWORD_SET( DEFAULTS ) THEN BEGIN
;print,'K E Y W O R D   N O T   S E T   !!!!'
		INITIAL_VALUES_STRUCT_PTR = PTR_NEW(			$
					{				$
					initial_path	: '100'		,$
					initial_orbit	: '1000'	,$
					initial_year	: '2000'	,$
					initial_month	: 'February'	,$
					initial_day	: '25'		,$
					initial_hour	: '00'		,$
					initial_minute	: '25'		,$
					initial_seconds	: '05'		$
					} )
;print,'inside if'
;help,INITIAL_VALUES_STRUCT_PTR
;help,*INITIAL_VALUES_STRUCT_PTR
;print,'leaving if'
	ENDIF

;help,INITIAL_VALUES_STRUCT_PTR
;help,*INITIAL_VALUES_STRUCT_PTR

	initial_path	= (*INITIAL_VALUES_STRUCT_PTR).initial_path
	initial_orbit	= (*INITIAL_VALUES_STRUCT_PTR).initial_orbit
	initial_year	= (*INITIAL_VALUES_STRUCT_PTR).initial_year
	initial_month	= (*INITIAL_VALUES_STRUCT_PTR).initial_month
	initial_day	= (*INITIAL_VALUES_STRUCT_PTR).initial_day
	initial_hour	= (*INITIAL_VALUES_STRUCT_PTR).initial_hour
	initial_minute	= (*INITIAL_VALUES_STRUCT_PTR).initial_minute
	initial_seconds	= (*INITIAL_VALUES_STRUCT_PTR).initial_seconds
	
IF NOT KEYWORD_SET( NO_GUI ) THEN BEGIN

	str		= 'Set Initial Orbit-Path-Date Information'
	
	tlb		= WIDGET_BASE( GROUP_LEADER= WIDGET_BASE(), /COLUMN,	$
				TITLE = str, /MODAL, /BASE_ALIGN_CENTER,	$
				KILL_NOTIFY = 'get_init_o_p_d_info_kill' )

	spacer		= WIDGET_LABEL( tlb, VALUE = ' ' )
	label		= WIDGET_LABEL( tlb, VALUE = 'Ascending Orbit GMT Initialization' )
	label		= WIDGET_LABEL( tlb, VALUE = '(Orbit starts at night side equator crossing)' )
	label		= WIDGET_LABEL( tlb, VALUE = '_____________________________________________' )
	spacer		= WIDGET_LABEL( tlb, VALUE = ' ' )
				
	year_base	= WIDGET_BASE( tlb, /ROW, /BASE_ALIGN_CENTER, /FRAME )
	year_label	= WIDGET_LABEL( year_base, VALUE = 'Initial Year:' )
	year_menu	= WIDGET_BUTTON( year_base, /MENU, VALUE = initial_year, /DYNAMIC_RESIZE )
	FOR i = 0, N_ELEMENTS( year_names ) - 1 DO year_buttons[i]	=	$
		WIDGET_BUTTON( year_menu, VALUE = year_names[i],		$
				UVALUE = 'year', /DYNAMIC_RESIZE )
	
	month_base	= WIDGET_BASE( tlb, /ROW, /BASE_ALIGN_CENTER, /FRAME )
	month_label	= WIDGET_LABEL( month_base, VALUE = 'Initial Month:' )
	month_menu	= WIDGET_BUTTON( month_base, /MENU, VALUE = initial_month, /DYNAMIC_RESIZE )
	FOR i = 0, 11 DO month_buttons[i]	=				$
		WIDGET_BUTTON( month_menu, VALUE = month_names[i],		$
				UVALUE = 'month', /DYNAMIC_RESIZE )
	
	day_base	= WIDGET_BASE( tlb, /ROW, /BASE_ALIGN_CENTER, /FRAME )
	day_label	= WIDGET_LABEL( day_base, VALUE = 'Initial Day:' )
	day_menu	= WIDGET_BUTTON( day_base, /MENU, VALUE = initial_day, /DYNAMIC_RESIZE )
	FOR i = 0, 30 DO $
		day_buttons[i]	= WIDGET_BUTTON( day_menu, VALUE = day_names[i], UVALUE = 'day', /DYNAMIC_RESIZE )
		
	hour_base	= WIDGET_BASE( tlb, /ROW, /BASE_ALIGN_CENTER, /FRAME )
	hour_label	= WIDGET_LABEL( hour_base, VALUE = 'Initial Hour:' )
	hour_menu	= WIDGET_BUTTON( hour_base, /MENU, VALUE = initial_hour, /DYNAMIC_RESIZE )
	FOR i = 0, 23 DO $
		hour_buttons[i]	= WIDGET_BUTTON( hour_menu, VALUE = hour_names[i], UVALUE = 'hour', /DYNAMIC_RESIZE )
	minute_base	= WIDGET_BASE( tlb, /ROW, /BASE_ALIGN_CENTER, /FRAME )
	minute_label	= WIDGET_LABEL( minute_base, VALUE = 'Initial Minute:' )
	minute_menu	= WIDGET_BUTTON( minute_base, /MENU, VALUE = initial_minute, /DYNAMIC_RESIZE )
	FOR i = 0, 11 DO $
		minute_buttons[i]	= WIDGET_BUTTON( minute_menu, VALUE = minute_names[i], UVALUE = 'minute', /DYNAMIC_RESIZE )
	seconds_base	= WIDGET_BASE( tlb, /ROW, /BASE_ALIGN_CENTER, /FRAME )
	seconds_label	= WIDGET_LABEL( seconds_base, VALUE = 'Initial Seconds:' )
	seconds_menu	= WIDGET_BUTTON( seconds_base, /MENU, VALUE = initial_seconds, /DYNAMIC_RESIZE )
	FOR i = 0, 11 DO $
		seconds_buttons[i]	= WIDGET_BUTTON( seconds_menu, VALUE = seconds_names[i], UVALUE = 'seconds', /DYNAMIC_RESIZE )
		
	orbit_base	= WIDGET_BASE( tlb, /ROW, /BASE_ALIGN_CENTER, /FRAME )
	orbit_label	= CW_FIELD( orbit_base, TITLE = 'Orbit #:', /INTEGER,	$
				VALUE = initial_orbit )
				
	str		= 'Corresponding Path:'
	path_base	= WIDGET_BASE( tlb, /ROW, /BASE_ALIGN_CENTER, /FRAME )
	path_label	= CW_FIELD( path_base, TITLE = str, /INTEGER, VALUE = initial_path )
	
	button_base	= WIDGET_BASE( tlb, /ROW, /BASE_ALIGN_CENTER )
	ok_button	= WIDGET_BUTTON( button_base, VALUE = 'OK', UVALUE = 'ok' )
	defaults	= WIDGET_BUTTON( button_base, VALUE = 'Defaults', UVALUE = 'defaults' )

	WIDGET_CONTROL, tlb, DEFAULT_BUTTON = ok_button
	
	WIDGET_CONTROL, tlb, /REALIZE

	
	infoPtr		= PTR_NEW( {						$
					active_path	:initial_path,		$
					active_orbit	:initial_orbit,		$
					active_year	:initial_year,		$
					active_day	:initial_day,		$
					active_month	:initial_month,		$
					active_minute	:initial_minute,	$
					active_hour	:initial_hour,		$
					active_seconds	:initial_seconds,	$
					year_names	:year_names,		$
					year_menu	:year_menu,		$
					year_buttons	:year_buttons,		$
					month_names	:month_names,		$
					month_menu	:month_menu,		$
					month_buttons	:month_buttons,		$
					day_buttons	:day_buttons,		$
					day_menu	:day_menu,		$
					hour_buttons	:hour_buttons,		$
					hour_menu	:hour_menu,		$
					minute_buttons	:minute_buttons,	$
					minute_menu	:minute_menu,		$
					seconds_buttons	:seconds_buttons,	$
					seconds_menu	:seconds_menu,		$
					orbit_cw	:orbit_label,		$
					path_cw		:path_label,		$
					INITIAL_VALUES_STRUCT_PTR : INITIAL_VALUES_STRUCT_PTR $
					}, /NO_COPY )
					
	WIDGET_CONTROL, tlb, SET_UVALUE = infoPtr
					
	XMANAGER, 'Get_Path_Orbit_Date_Info', tlb, EVENT_HANDLER = 'get_init_o_p_d_info'

ENDIF ELSE BEGIN ; endif not keyword_set( NO_GUI )

	infoPtr		= PTR_NEW( {						$
					active_path	:initial_path,		$
					active_orbit	:initial_orbit,		$
					active_year	:initial_year,		$
					active_day	:initial_day,		$
					active_month	:initial_month,		$
					active_minute	:initial_minute,	$
					active_hour	:initial_hour,		$
					active_seconds	:initial_seconds,	$
					month_names	:month_names,		$
					INITIAL_VALUES_STRUCT_PTR : INITIAL_VALUES_STRUCT_PTR $
					}, /NO_COPY )

ENDELSE
					
	
	idx		= WHERE( (*infoPtr).month_names EQ (*infoPtr).active_month) + 1
	str_mon		= '0' + STRTRIM(STRING(idx[0]),2)
	str_mon		= STRMID( str_mon, STRLEN(str_mon) - 2, 2 )
	str_day		= '0' + STRTRIM(STRING((*infoPtr).active_day),2)
	str_day		= STRMID( str_day, STRLEN(str_day) - 2, 2 )
	str_minute	= '0' + STRTRIM(STRING((*infoPtr).active_minute),2)
	str_minute	= STRMID( str_minute, STRLEN(str_minute) - 2, 2 )
	str_hour	= '0' + STRTRIM(STRING((*infoPtr).active_hour),2)
	str_hour	= STRMID( str_hour, STRLEN(str_hour) - 2, 2 )
	str_seconds	= '0' + STRTRIM(STRING((*infoPtr).active_seconds),2)
	str_seconds	= STRMID( str_seconds, STRLEN(str_seconds) - 2, 2 )
	
	init_date_val	= STRTRIM((*infoPtr).active_year,2) + str_mon + str_day + str_hour + str_minute + str_seconds
	init_orbit_val	= (*infoPtr).active_orbit
	init_path_val	= (*infoPtr).active_path
	
	PTR_FREE, infoPtr
;print,'	init_date_val, init_orbit_val, init_path_val = ',init_date_val, init_orbit_val, init_path_val
;print,'just before returning'
;print,'just before returning'
;print,'just before returning'
;print,'just before returning'
;print,'just before returning'
;print,'just before returning'
;help,INITIAL_VALUES_STRUCT_PTR
;if ptr_valid(INITIAL_VALUES_STRUCT_PTR) then begin
;help,*INITIAL_VALUES_STRUCT_PTR
;if size(*INITIAL_VALUES_STRUCT_PTR,/type) eq 8 then begin
;print,tag_names(*INITIAL_VALUES_STRUCT_PTR)
;for i=0,7 do print,(*INITIAL_VALUES_STRUCT_PTR).(i)
;endif
;endif
;print,'just before returning'
;print,'just before returning'
;print,'just before returning'
;print,'just before returning'
;print,'just before returning'
;print,'just before returning'
	RETURN, { init_date_val:init_date_val, init_orbit_val:init_orbit_val, init_path_val:init_path_val, $
			INITIAL_VALUES_STRUCT_PTR : INITIAL_VALUES_STRUCT_PTR }
	
END
; misr_get_initial_orbit_path_date_info
