;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ misr_display_meta_kill @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO misr_display_meta_kill, tlb
	WIDGET_CONTROL, tlb, GET_UVALUE = info_ptr
	IF *((*info_ptr).last_event_ptr) NE 'select_button_event' THEN	$
		*((*info_ptr).idx_ptr)	= (-1)
	IF WIDGET_INFO((*info_ptr).group_leader, /VALID_ID) THEN	$
			WIDGET_CONTROL, (*info_ptr).group_leader, /DESTROY
END
; misr_display_meta_kill

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ misr_display_meta_eh @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO misr_display_meta_eh, event
	widget_type	= TAG_NAMES(event,/STRUCTURE_NAME)
	WIDGET_CONTROL, event.top, GET_UVALUE = info_ptr
	
	IF STRUPCASE(widget_type) EQ 'WIDGET_BASE' THEN BEGIN
		*((*info_ptr).last_event_ptr) = 'resize_event'
		(*((*info_ptr).new_base_xy_ptr))[0]	= MAX([(*((*info_ptr).min_base_xy_ptr))[0],event.x])
		(*((*info_ptr).new_base_xy_ptr))[1]	= MAX([(*((*info_ptr).min_base_xy_ptr))[1],event.y])
		WIDGET_CONTROL, (*info_ptr).group_leader, /DESTROY
		RETURN
	ENDIF
	
	WIDGET_CONTROL, event.id, GET_UVALUE = event_type
	*((*info_ptr).last_event_ptr)	= event_type
	CASE STRLOWCASE(event_type) OF
		'list_event': BEGIN
			*((*info_ptr).idx_ptr) = event.index
			END
		'select_button_event': BEGIN
			WIDGET_CONTROL, (*info_ptr).group_leader, /DESTROY
			END
		'cancel_button_event': BEGIN
			*((*info_ptr).idx_ptr) = (-1)
			WIDGET_CONTROL, (*info_ptr).group_leader, /DESTROY
			END
		ELSE:
	ENDCASE
END
; misr_display_meta_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ misr_display_meta @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION misr_display_meta, data_listing, XSIZE = xsize,			$
			YSIZE = ysize, MIN_BASE_XY_SIZE = min_base_xy_size,	$
			CONSTANT_YSIZE = constant_ysize

	orig_data_list	= data_listing
	n		= 0L
	
	FOR i = 0, N_ELEMENTS(orig_data_list) - 1 DO			$
		n	= n +						$
			  N_ELEMENTS(*((*(orig_data_list[i]))[0])) +	$
			  N_ELEMENTS(*((*(orig_data_list[i]))[1]))
			  
	data_listing	= STRARR(n)
	
	ctr	= 0L
	FOR i = 0, N_ELEMENTS(orig_data_list) - 1 DO BEGIN
		data_listing[ctr]	= '* ' + *((*(orig_data_list[i]))[0])
		ctr			= ctr + 1
		n			= N_ELEMENTS(*((*(orig_data_list[i]))[1]))
		data_listing[ctr:ctr+n-1]					$
					= '          <' +			$
					  *((*(orig_data_list[i]))[1]) +	$
					  '>'
		ctr			= ctr + n
	ENDFOR

	group_leader	= WIDGET_BASE()
	
	b	= WIDGET_BASE( TITLE = 'Listing of Metadata',			$
			      	GROUP_LEADER = group_leader,			$
			     	/COLUMN,					$
			     	 /MODAL,					$
			      	/TLB_SIZE_EVENTS,				$
			      	KILL_NOTIFY = 'misr_display_meta_kill',		$
			      	/BASE_ALIGN_CENTER,				$
			      	EVENT_PRO = 'misr_display_meta_eh' )
	
	lbl_base= WIDGET_BASE( b, /ROW )
	lbl1	= WIDGET_LABEL( lbl_base, VALUE = '* Metadata Names' )
	lbl2	= WIDGET_LABEL( lbl_base, VALUE = '   <Field Listings>' )
	
	l_base	= WIDGET_BASE( b, /COLUMN )
		
	IF KEYWORD_SET(ysize) AND KEYWORD_SET(constant_ysize) THEN	$
		l	= WIDGET_LIST( l_base,				$
			      	 VALUE = data_listing,			$
			      	 SCR_YSIZE=ysize-constant_ysize,	$
			      	 FRAME = 3,				$
			      	 UVALUE = 'list_event' )		$
	ELSE								$
		l	= WIDGET_LIST( l_base,				$
			      	 VALUE = data_listing,			$
			      	 FRAME = 3,				$
			      	 UVALUE = 'list_event' )
			       
	bt_base	= WIDGET_BASE( b, /ROW )
	sel_bt	= WIDGET_BUTTON( bt_base,				$
				 VALUE = 'Select Highlighted Metadata',	$
				 UVALUE = 'select_button_event' )
	can_bt	= WIDGET_BUTTON( bt_base,				$
				 VALUE = 'Cancel',			$
				 UVALUE = 'cancel_button_event' )
				 
	WIDGET_CONTROL, b, /REALIZE
	
	base_info	= WIDGET_INFO(b,/GEOMETRY)
	list_info	= WIDGET_INFO(l,/GEOMETRY)
	IF NOT KEYWORD_SET(constant_ysize) THEN						$
		constant_ysize_ptr	= PTR_NEW(base_info.ysize-list_info.ysize)	$
	ELSE										$
		constant_ysize_ptr	= PTR_NEW(constant_ysize)
	IF NOT KEYWORD_SET(min_base_xy_size) THEN min_base_xy_size = [base_info.xsize,base_info.ysize]
	idx_ptr	= PTR_NEW((-1L),/NO_COPY)
	min_base_xy_ptr	= PTR_NEW(min_base_xy_size,/NO_COPY)
	tmpc		= 'null_event'
	last_event_ptr	= PTR_NEW(tmpc)
	new_base_xy_ptr	= PTR_NEW(LONARR(2))
	info_ptr	= PTR_NEW( { idx_ptr:idx_ptr,				$
				     listing:orig_data_list,		$
				     group_leader:group_leader,		$
				     last_event_ptr:last_event_ptr,	$
				     min_base_xy_ptr:min_base_xy_ptr,	$
				     new_base_xy_ptr:new_base_xy_ptr,	$
				     constant_ysize_ptr:constant_ysize_ptr } )
	WIDGET_CONTROL, b, SET_UVALUE = info_ptr
	
	XMANAGER, 'Metadata_Listing_Base', b, EVENT_HANDLER = 'misr_display_meta_eh'
	
	IF STRLOWCASE(*last_event_ptr) EQ 'resize_event' THEN BEGIN
		min_xsize = (*min_base_xy_ptr)[0]
		min_ysize = (*min_base_xy_ptr)[1]
		new_xsize = (*new_base_xy_ptr)[0]
		new_ysize = (*new_base_xy_ptr)[1]
		constant_ysize	= *constant_ysize_ptr
		PTR_FREE, min_base_xy_ptr
		PTR_FREE, new_base_xy_ptr
		PTR_FREE, idx_ptr
		PTR_FREE, last_event_ptr
		PTR_FREE, constant_ysize_ptr
		PTR_FREE, info_ptr
		data_name = misr_display_meta( orig_data_list,				$
					     XSIZE = new_xsize,				$
					     YSIZE = new_ysize,				$
					     MIN_BASE_XY_SIZE = [min_xsize,min_ysize],	$
					     CONSTANT_YSIZE = constant_ysize )
		RETURN, data_name
	ENDIF
	
	IF *idx_ptr LT 0 THEN BEGIN
		PTR_FREE, idx_ptr
		res	= depointer(orig_data_list)
		RETURN, PTRARR(1)
	ENDIF
	
	ctr		= 0L
	done		= 0
	i		= 0L
	
	WHILE i LT N_ELEMENTS(orig_data_list) AND NOT done DO BEGIN
		n	= N_ELEMENTS(*((*(orig_data_list[i]))[1]))
		IF *idx_ptr EQ ctr THEN BEGIN
			nm		= *((*(orig_data_list[i]))[0])
			fld_strarr	= *((*(orig_data_list[i]))[1])
			done		= 1				
		ENDIF ELSE BEGIN
			j	= 0L
			WHILE j LT n AND NOT done DO BEGIN
				IF *idx_ptr EQ ctr + 1 + j THEN BEGIN
					nm		= *((*(orig_data_list[i]))[0])
					fld_strarr	= [ (*((*(orig_data_list[i]))[1]))[j] ]
					done		= 1
				ENDIF	
				j		= j + 1L		
			ENDWHILE
		ENDELSE
		ctr	= ctr + n + 1
		i	= i + 1L
	ENDWHILE
	
	IF NOT done THEN BEGIN
		PTR_FREE, idx_ptr
		res = depointer(orig_data_list)
		RETURN, PTRARR(1)
	ENDIF
	
	PTR_FREE, idx_ptr
;print,'just before call to depointer'
	res	= depointer(orig_data_list)
;print,'just after call to depointer'
;print,'nm, fld_strarr = ',nm, fld_strarr
	RETURN, [ PTR_NEW(nm), PTR_NEW(fld_strarr) ]	
				
END
; misr_display_meta
