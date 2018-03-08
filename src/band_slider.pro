PRO dismiss_eh, event
	WIDGET_CONTROL, event.top, GET_UVALUE = info_ptr
;;;ckt,dec1999	WSET, (*info_ptr).cur_win
	WIDGET_CONTROL, (*info_ptr).tlb, /DESTROY
END
; dismiss_eh

PRO reset_eh, event
	WIDGET_CONTROL, event.top, GET_UVALUE = info_ptr
	WSET, (*info_ptr).comp_pix
	DEVICE, SET_GRAPHICS_FUNCTION = (*info_ptr).src_copy
	DEVICE, COPY = [ 0, 0, (*info_ptr).image_xy[0], (*info_ptr).image_xy[1], 0, 0, (*info_ptr).pix_rgb[0] ]
	DEVICE, SET_GRAPHICS_FUNCTION = (*info_ptr).src_xor_dest_copy
	DEVICE, COPY = [ 0, 0, (*info_ptr).image_xy[0], (*info_ptr).image_xy[1], 0, 0, (*info_ptr).pix_rgb[1] ]
	DEVICE, COPY = [ 0, 0, (*info_ptr).image_xy[0], (*info_ptr).image_xy[1], 0, 0, (*info_ptr).pix_rgb[2] ]
	WSET, (*info_ptr).slider_id
	DEVICE, SET_GRAPHICS_FUNCTION = (*info_ptr).src_copy
	DEVICE, COPY = [ 0, 0, (*info_ptr).image_xy[0], (*info_ptr).image_xy[1], 0, 0, (*info_ptr).comp_pix ]
	
	(*info_ptr).image_origin	= [[0L,0L,0L],[0L,0L,0L]]
	(*info_ptr).previous_xy		= [ 0L, 0L ]
	(*info_ptr).prev_image_origin	= [[0L,0L,0L],[0L,0L,0L]]
	
	WIDGET_CONTROL, (*info_ptr).rgb_dx[3], SET_VALUE = '0'
	WIDGET_CONTROL, (*info_ptr).rgb_dy[3], SET_VALUE = '0'
	WIDGET_CONTROL, (*info_ptr).rgb_dx[4], SET_VALUE = '0'
	WIDGET_CONTROL, (*info_ptr).rgb_dy[4], SET_VALUE = '0'
	WIDGET_CONTROL, (*info_ptr).rgb_dx[5], SET_VALUE = '0'
	WIDGET_CONTROL, (*info_ptr).rgb_dy[5], SET_VALUE = '0'
END
;reset_eh

PRO repaint_slider_window, info_ptr, change_x, change_y
	active_idx	= WHERE( (*info_ptr).select_idx GT 0, cnt )
	IF cnt LE 0 THEN RETURN
	
	FOR j = 0, N_ELEMENTS(active_idx) - 1 DO BEGIN
		cur_plane_idx	= active_idx[j]
		WSET, (*info_ptr).comp_pix
		DEVICE, SET_GRAPHICS_FUNCTION = (*info_ptr).src_xor_dest_copy
		
		FOR i = 0, 1 DO BEGIN
			src_x	= MIN( [ ABS( MIN( [ (*info_ptr).image_origin[cur_plane_idx,0], 0L ] ) ),	$
					(*info_ptr).image_xy[0] - 1 ] )
			src_y	= MIN( [ ABS( MIN( [ (*info_ptr).image_origin[cur_plane_idx,1], 0L ] ) ),	$
					(*info_ptr).image_xy[1] - 1 ] )
			nx	= (*info_ptr).image_xy[0] - src_x
			ny	= (*info_ptr).image_xy[1] - src_y
			id	= (*info_ptr).pix_rgb[cur_plane_idx]
			dest_x	= MIN( [ MAX( [ 0L, (*info_ptr).image_origin[cur_plane_idx,0] ] ),		$
				(*info_ptr).image_xy[0] - 1 ] )
			dest_y	= MIN( [ MAX( [ 0L, (*info_ptr).image_origin[cur_plane_idx,1] ] ),		$
				(*info_ptr).image_xy[1] - 1 ] )
		
			DEVICE, COPY = [ src_x, src_y, nx, ny, dest_x, dest_y, id ]
		
			IF i LE 0 THEN BEGIN
				(*info_ptr).image_origin[cur_plane_idx,0]	=	$
					(*info_ptr).prev_image_origin[cur_plane_idx,0] + change_x
				(*info_ptr).image_origin[cur_plane_idx,1]	=	$
					(*info_ptr).prev_image_origin[cur_plane_idx,1] + change_y
			ENDIF
		ENDFOR
	
		WSET, (*info_ptr).slider_id
		DEVICE, SET_GRAPHICS_FUNCTION = (*info_ptr).src_copy
		DEVICE, COPY = [ 0, 0, (*info_ptr).image_xy[0], (*info_ptr).image_xy[1], 0, 0, (*info_ptr).comp_pix ]
	
		WIDGET_CONTROL, (*info_ptr).rgb_dx[cur_plane_idx+3], SET_VALUE = STRTRIM(STRING((*info_ptr).image_origin[cur_plane_idx,0]),2)
		WIDGET_CONTROL, (*info_ptr).rgb_dy[cur_plane_idx+3], SET_VALUE = STRTRIM(STRING((*info_ptr).image_origin[cur_plane_idx,1]),2)
	ENDFOR
	
END
; repaint_slider_window

PRO slider_draw_eh, event
	MOUSE_DOWN	= 0
	MOUSE_UP	= 1
	MOUSE_MOVE	= 2
	
	WIDGET_CONTROL, event.top, GET_UVALUE = info_ptr
	
	widget_type	= STRUPCASE( TAG_NAMES( event, /STRUCTURE_NAME ) )
	
;;;ckt,dec1999	IF widget_type EQ 'WIDGET_TIMER' THEN BEGIN
;;;ckt,dec1999		(*info_ptr).flush_move_events	= 0
;;;ckt,dec1999		RETURN
;;;ckt,dec1999	ENDIF
		
	CASE event.type OF
		MOUSE_DOWN: BEGIN
			(*info_ptr).mouse_status	= MOUSE_DOWN
			(*info_ptr).previous_xy		= [ event.x, event.y ]
			END
		MOUSE_UP: BEGIN
			(*info_ptr).mouse_status			= MOUSE_UP
			(*info_ptr).prev_image_origin[0,0]	= (*info_ptr).image_origin[0,0]
			(*info_ptr).prev_image_origin[0,1]	= (*info_ptr).image_origin[0,1]
			(*info_ptr).prev_image_origin[1,0]	= (*info_ptr).image_origin[1,0]
			(*info_ptr).prev_image_origin[1,1]	= (*info_ptr).image_origin[1,1]
			(*info_ptr).prev_image_origin[2,0]	= (*info_ptr).image_origin[2,0]
			(*info_ptr).prev_image_origin[2,1]	= (*info_ptr).image_origin[2,1]
			END
		MOUSE_MOVE: BEGIN
			IF (*info_ptr).mouse_status THEN RETURN
			IF TOTAL( (*info_ptr).select_idx ) LE 0 THEN RETURN
;;;ckt,dec1999			IF (*info_ptr).flush_move_events THEN RETURN
			change_x	= 0
			change_y	= 0
;print,'+++++ MOUSE_MOVE +++++   (*info_ptr).previous_xy = ',(*info_ptr).previous_xy
;print,'+++++ MOUSE_MOVE +++++   event.x, event.y = ',event.x, event.y
			IF (*info_ptr).cur_movement_idx NE 1 THEN			$
				change_x	= event.x - (*info_ptr).previous_xy[0]
			IF (*info_ptr).cur_movement_idx NE 0 THEN			$
				change_y	= event.y - (*info_ptr).previous_xy[1]
				
			repaint_slider_window, info_ptr, change_x, change_y
			
;;;ckt,dec1999			WIDGET_CONTROL, event.id, TIMER = 0.5
;;;ckt,dec1999			(*info_ptr).flush_move_events	= 1
			END
		ELSE:
	ENDCASE
	
END
; slider_draw_eh

FUNCTION rgb_btn_eh, event
	WIDGET_CONTROL, event.top, GET_UVALUE = info_ptr	
	WIDGET_CONTROL, (*info_ptr).rgb_btn, GET_VALUE = select_idx
	(*info_ptr).select_idx	= select_idx
END
; rgb_btn_eh

FUNCTION hvf_btn_eh, event
	WIDGET_CONTROL, event.top, GET_UVALUE = info_ptr
	hvf	= ['horizontal','vertical','free']
	(*info_ptr).cur_movement_idx	= event.value
	configure_buttons, (*info_ptr).fine_tuner_info_ptr, hvf[(*info_ptr).cur_movement_idx]
END
;hvf_btn_eh

FUNCTION fine_tune_eh, event
	widget_type	= STRTRIM(STRUPCASE(TAG_NAMES( event, /STRUCTURE_NAME )),2)
	
	change_x	= 0
	change_y	= 0
	
	CASE widget_type OF
		'WIDGET_BUTTON': BEGIN
			parent	= WIDGET_INFO( event.id, /PARENT )
			WIDGET_CONTROL, parent, GET_UVALUE = info_ptr
			WIDGET_CONTROL, event.id, GET_UVALUE = dxdy
			change_x	= (*info_ptr).inc_x * dxdy[0]
			change_y	= (*info_ptr).inc_y * dxdy[1]
			END
		ELSE: BEGIN
			parent	= WIDGET_INFO( event.id, /PARENT )
			WIDGET_CONTROL, parent, GET_UVALUE = info_ptr
			WIDGET_CONTROL, event.id, GET_UVALUE = inc_type
			CASE inc_type OF
				'INCX': BEGIN
					WIDGET_CONTROL, event.id, GET_VALUE = new_incx
					(*info_ptr).inc_x	= new_incx
					END
				'INCY': BEGIN
					WIDGET_CONTROL, event.id, GET_VALUE = new_incy
					(*info_ptr).inc_y	= new_incy
					END
				ELSE:
			ENDCASE
			END
	ENDCASE
	
	RETURN, { FINE_TUNER, id: event.id, top:event.top, handler:event.handler, change_x:change_x, change_y:change_y }
END
; fine_tune_eh

FUNCTION get_fine_tune_mover_tool, parent_base
	tlb	= WIDGET_BASE( parent_base, /COLUMN, /BASE_ALIGN_CENTER, EVENT_FUNC = 'fine_tune_eh' )
;print,'tlb = ',tlb	
	inc_x	= CW_FIELD( tlb, /COLUMN, /LONG, /ALL_EVENTS, VALUE = 1L, UVALUE = 'INCX', TITLE = 'Horizontal increment' )
	inc_y	= CW_FIELD( tlb, /COLUMN, /LONG, /ALL_EVENTS, VALUE = 1L, UVALUE = 'INCY', TITLE = 'Vertical increment' )
	
	base	= WIDGET_BASE( tlb, /GRID_LAYOUT, ROW = 3, /BASE_ALIGN_CENTER )
	
	bitmap	= BYTARR(32,32)+255B
	
	FOR y = 11, 20 do bitmap[16-(y-11):16+(y-11),y] = 0B
	
	bitmap_array	= BYTARR( 3,3,32,32 )
	button_array	= LONARR( 3, 3 )
	inc_xy_array	= LONARR( 3, 3, 2 )
	
	tmp			= BYTARR(32,32)+255B
	FOR i = 0, 15 DO BEGIN
		tmp[i,i:31-i]		= 255B / (i+1)
		tmp[i:31-i,i]		= 255B / (i+1)
		tmp[31-i,i:31-i]	= 255B / (i+1)
		tmp[i:31-i,31-i]	= 255B / (i+1)
	ENDFOR
	
	bitmap_array[1,1,*,*]	= tmp
	
	bitmap_array[1,2,*,*]	= bitmap
	inc_xy_array[1,2,*]	= [0L,(-1L)]
	
	bitmap_array[1,0,*,*]	= REVERSE( bitmap, 2 )
	inc_xy_array[1,0,*]	= [0L,1L]
	
	bitmap_array[0,2,*,*]	= ROT( bitmap, 45 )
	inc_xy_array[0,2,*]	= [(-1L),(-1L)]
	
	bitmap_array[0,0,*,*]	= REVERSE( ROT( bitmap, 45 ), 2 )
	inc_xy_array[0,0,*]	= [(-1),1L]
	
	bitmap_array[2,0,*,*]	= REVERSE( REVERSE( ROT( bitmap, 45 ), 2 ), 1 )
	inc_xy_array[2,0,*]	= [1L,1L]
	
	bitmap_array[2,2,*,*]	= REVERSE( ROT( bitmap, 45 ), 1 )
	inc_xy_array[2,2,*]	= [1L,(-1L)]
	
	bitmap_array[2,1,*,*]	= REVERSE( ROT( bitmap, 90 ), 1 )
	inc_xy_array[2,1,*]	= [1L,0L]
	
	bitmap_array[0,1,*,*]	= ROT( bitmap, 90 )
	inc_xy_array[0,1,*]	= [(-1L),0L]
	
	FOR i = 0, 2 DO BEGIN
		FOR j = 0, 2 DO BEGIN
;		print,'j,i = ',j,i
			button_array[j,i]	= WIDGET_BUTTON( base, /BITMAP,				$
							VALUE = CVTTOBM(REFORM(bitmap_array[j,i,*,*])),	$
							UVALUE = REFORM(inc_xy_array[j,i,*]) )
		ENDFOR
	ENDFOR
	
	WIDGET_CONTROL, button_array[1,1], SENSITIVE = 0
	
	info_ptr	= PTR_NEW( {						$
					button_array	: button_array,		$
					inc_x		: 1L,			$
					inc_y		: 1L }, /NO_COPY )
					
	WIDGET_CONTROL, base, SET_UVALUE = info_ptr
	WIDGET_CONTROL, tlb, SET_UVALUE = info_ptr
	
	RETURN, info_ptr
	
END
; get_fine_tune_mover_tool

PRO mover_eh, event
	IF event.change_x EQ 0 AND event.change_y EQ 0 THEN RETURN
	WIDGET_CONTROL, event.handler, GET_UVALUE =  info_ptr
	repaint_slider_window, info_ptr, event.change_x, event.change_y
	(*info_ptr).prev_image_origin[0,0]	= (*info_ptr).image_origin[0,0]
	(*info_ptr).prev_image_origin[0,1]	= (*info_ptr).image_origin[0,1]
	(*info_ptr).prev_image_origin[1,0]	= (*info_ptr).image_origin[1,0]
	(*info_ptr).prev_image_origin[1,1]	= (*info_ptr).image_origin[1,1]
	(*info_ptr).prev_image_origin[2,0]	= (*info_ptr).image_origin[2,0]
	(*info_ptr).prev_image_origin[2,1]	= (*info_ptr).image_origin[2,1]
END
; mover_eh

PRO configure_buttons, info_ptr, dir_str
	on_off_arr	= INTARR(3,3)
	
	CASE STRUPCASE(STRTRIM(dir_str,2)) OF
		'HORIZONTAL':	on_off_arr[*,1]	= 1
		'VERTICAL':	on_off_arr[1,*]	= 1
		'FREE':		on_off_arr[*,*]	= 1
		ELSE:
	ENDCASE
	
	on_off_arr[1,1]	= 0
	
	FOR i = 0, 2 DO FOR j = 0, 2 DO WIDGET_CONTROL, (*info_ptr).button_array[i,j], SENSITIVE = on_off_arr[i,j]
END
; configure_buttons

PRO save_eh, event

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when 
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== save_eh (band_slider) =========='
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


	fn	= dialog_pickfile_wrapper( TITLE = 'Enter name of output TIFF file' )
;;;ckt,apr2001	fn	= DIALOG_PICKFILE( TITLE = 'Enter name of output TIFF file' )
	IF fn EQ '' THEN RETURN
	WIDGET_CONTROL, event.top, GET_UVALUE = info_ptr
	WSET, (*info_ptr).slider_id
	img	= BYTARR(3,(*info_ptr).image_xy[0],(*info_ptr).image_xy[1])
	FOR i = 0, 2 DO img[i,*,*]	= TVRD(CHANNEL=i+1)
	WRITE_TIFF, fn, img
END
; save_eh

PRO band_slider, r, g, b, GROUP_LEADER = group_leader

	src_copy		= 3
	src_xor_dest_copy	= 6
		
	cur_win			= !D.WINDOW
	
	x			= (SIZE(r))[1]
	y			= (SIZE(r))[2]
	
	pix_rgb			= LONARR(3)
	
	WINDOW, /FREE, /PIXMAP, XSIZE = x, YSIZE = y
	pix_rgb[0]	= !D.WINDOW
	TV, r, CHANNEL = 1
	
	WINDOW, /FREE, /PIXMAP, XSIZE = x, YSIZE = y
	pix_rgb[1]	= !D.WINDOW
	TV, g, CHANNEL = 2
	
	WINDOW, /FREE, /PIXMAP, XSIZE = x, YSIZE = y
	pix_rgb[2]	= !D.WINDOW
	TV, b, CHANNEL = 3
	
	WINDOW, /FREE, /PIXMAP, XSIZE = x, YSIZE = y
	comp_pix	= !D.WINDOW
	
	DEVICE, SET_GRAPHICS_FUNCTION = src_xor_dest_copy
	
	FOR i = 0, 2 DO DEVICE, COPY = [ 0, 0, x, y, 0, 0, pix_rgb[i] ]
	
	sensitive_status	= BYTARR(3)
	IF MIN(r) NE MAX(r) THEN sensitive_status[0] = 1
	IF MIN(g) NE MAX(g) THEN sensitive_status[1] = 1
	IF MIN(b) NE MAX(b) THEN sensitive_status[2] = 1
	first_active_idx	= WHERE( sensitive_status GT 0 )
	first_active_idx	= first_active_idx[0]
	tmp			= BYTARR(3)
	tmp[first_active_idx[0]]= 1
	
	IF KEYWORD_SET(group_leader) THEN					$
		tlb	= WIDGET_BASE(						$
					TITLE = 'Band Slider Tool',		$
					/COLUMN,				$
					TLB_FRAME_ATTR = 1,			$
					GROUP_LEADER = group_leader )		$
	ELSE									$
		tlb	= WIDGET_BASE(						$
					TITLE = 'Band Slider Tool',		$
					TLB_FRAME_ATTR = 1,			$
					/COLUMN )

	draw	= WIDGET_DRAW( tlb, XSIZE = x, YSIZE = y, RETAIN = 2, /BUTTON_EVENTS, /MOTION_EVENTS, EVENT_PRO = 'slider_draw_eh' )
	base	= WIDGET_BASE(						$
				/COLUMN,				$
				/FLOATING,				$
				GROUP_LEADER = tlb,			$
				TITLE = 'Slider Controls',		$
				/BASE_ALIGN_CENTER )
	cw_base	= WIDGET_BASE( base, /ROW, /BASE_ALIGN_CENTER, /ALIGN_CENTER )
	rgb_btn	= CW_BGROUP(cw_base, [ 'red', 'green', 'blue' ], /NONEXCLUSIVE, IDS = rgb_btn_ids,	$
				EVENT_FUNCT = 'rgb_btn_eh', /COLUMN, SET_VALUE = tmp, FRAME = 3 )
	hvf_btn	= CW_BGROUP(cw_base, [ 'horizontal', 'vertical', 'free' ], /EXCLUSIVE, IDS = hvf_btn_ids,	$
				EVENT_FUNCT = 'hvf_btn_eh', /NO_RELEASE, /COLUMN, SET_VALUE = 0, FRAME = 3 )
	base1	= WIDGET_BASE( base, /COLUMN, /BASE_ALIGN_CENTER, FRAME = 3, EVENT_PRO = 'mover_eh' )
	title	= WIDGET_LABEL( base1, VALUE = 'fine tune movement' )
	fine_tuner_info_ptr	= get_fine_tune_mover_tool( base1 )
	
	base2	= WIDGET_BASE( base, /COLUMN, /BASE_ALIGN_CENTER )
	rgb_dx	= LONARR(6)
	rgb_dy	= LONARR(6)
	rgb_str	= [ '(Red)', '(Green)', '(Blue)' ]
	
	FOR i = 0,2 DO BEGIN
		b		= WIDGET_BASE( base2, /ROW, /BASE_ALIGN_CENTER )
		rgb_dx[i]	= WIDGET_LABEL( b, VALUE = 'DX ' + rgb_str[i] )
		rgb_dx[i+3]	= WIDGET_LABEL( b, VALUE = '0', /DYNAMIC_RESIZE )
		spacer		= WIDGET_LABEL( b, VALUE = '     ' )
		rgb_dy[i]	= WIDGET_LABEL( b, VALUE = 'DY ' + rgb_str[i] )
		rgb_dy[i+3]	= WIDGET_LABEL( b, VALUE = '0', /DYNAMIC_RESIZE )
	ENDFOR
	
	btn_base	= WIDGET_BASE( base, /ROW, /BASE_ALIGN_CENTER, /ALIGN_CENTER )
	
	reset	= WIDGET_BUTTON( btn_base, VALUE = 'Reset All Bands', EVENT_PRO = 'reset_eh', /ALIGN_CENTER )
	save	= WIDGET_BUTTON( btn_base, VALUE = 'Save As TIFF', EVENT_PRO = 'save_eh', /ALIGN_CENTER )
	dismiss	= WIDGET_BUTTON( btn_base, VALUE = 'Dismiss', EVENT_PRO = 'dismiss_eh', /ALIGN_CENTER )
	
	WIDGET_CONTROL, tlb, /REALIZE
	WIDGET_CONTROL, base, /REALIZE
	
	FOR i = 0, 2 DO WIDGET_CONTROL, rgb_btn_ids[i], SENSITIVE = sensitive_status[i]
			
	WIDGET_CONTROL, draw, GET_VALUE = slider_id
	
	configure_buttons, fine_tuner_info_ptr, 'horizontal'
		
	WSET, slider_id
	
	DEVICE, COPY = [ 0, 0, x, y, 0, 0, comp_pix ]
	
	image_origin	= LONARR( 3, 2 )
	
	info_ptr	= PTR_NEW( {							$
					slider_id		: slider_id,		$
					tlb			: tlb,			$
					cur_win			: cur_win,		$
					pix_rgb			: pix_rgb,		$
					comp_pix		: comp_pix,		$
					rgb_btn			: rgb_btn,		$
					cur_movement_idx	: 0L,			$
					select_idx		: tmp,			$
;;;ckt,dec1999					flush_move_events	: 0L,			$
					mouse_status		: 1,			$
					image_origin		: image_origin,		$
					prev_image_origin	: image_origin,		$
					previous_xy		: [ 0L, 0L ],		$
					image_xy		: [ x, y ],		$
					fine_tuner_info_ptr	: fine_tuner_info_ptr,	$
					rgb_dx			: rgb_dx,		$
					rgb_dy			: rgb_dy,		$
					src_xor_dest_copy	: src_xor_dest_copy,	$
					src_copy		: src_copy },		$
					/NO_COPY )
					
	WIDGET_CONTROL, tlb, SET_UVALUE = info_ptr
	WIDGET_CONTROL, base1, SET_UVALUE = info_ptr
	WIDGET_CONTROL, base, SET_UVALUE = info_ptr
					
	
	XMANAGER, 'slider_window', tlb
	
END
; band_slider
