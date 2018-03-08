;===============================================================================
; show_preview_image_eh
;===============================================================================
PRO show_preview_image_eh, event
	WIDGET_CONTROL, event.id, GET_UVALUE = widget_name
	widget_name	= STRTRIM(STRUPCASE(widget_name),2)
	CASE widget_name OF
		'EXIT': WIDGET_CONTROL, event.top, /DESTROY
		ELSE:
	ENDCASE
END
; show_preview_image_eh

;===============================================================================
; show_preview_image
;===============================================================================
PRO show_preview_image, img, group_leader, filename
	dims	= SIZE(img,/DIMENSIONS)
	tlb	= WIDGET_BASE(							$
			TITLE = 'Preview Image: ' + filename,			$
			GROUP_LEADER = group_leader,				$
			/COLUMN,						$
			EVENT_PRO = 'show_preview_image_eh',			$
			MBAR = bar )
	actions	= WIDGET_BUTTON(						$
			bar,							$
			VALUE = 'Actions',					$
			/MENU )
	quit	= WIDGET_BUTTON(						$
			actions,						$
			VALUE = 'Exit',						$
			UVALUE = 'exit' )
	draw	= WIDGET_DRAW(							$
			tlb,							$
			XSIZE = dims[N_ELEMENTS(dims)-2],			$
			YSIZE = dims[N_ELEMENTS(dims)-1] )
	WIDGET_CONTROL, tlb, /REALIZE
	WIDGET_CONTROL, draw, GET_VALUE = draw_id
	curr	= !D.WINDOW
	WSET, draw_id
	TV, img, /TRUE
	WSET, curr
	XMANAGER, 'show_preview_image', tlb, EVENT_HANDLER = 'show_preview_image_eh'
END
; show_preview_image

;===============================================================================
; trim_text_image
;===============================================================================
FUNCTION trim_text_image, img
	dims	= SIZE(img,/DIMENSIONS)
	IF N_ELEMENTS(dims) GE 3 THEN BEGIN
		aspect	= FLOAT(dims[1])/dims[2]
		idx	= WHERE(						$
				REFORM(img[0,*,*]) GT 0 OR			$
				REFORM(img[1,*,*]) GT 0 OR			$
				REFORM(img[2,*,*]) GT 0, cnt )
		IF cnt LE 0 THEN RETURN, img[*,ROUND(10*aspect),0:9]
		idx_x	= idx MOD dims[1]
		idx_y	= idx / dims[1]
		min_x	= MIN(idx_x,MAX=max_x)
		min_y	= MIN(idx_y,MAX=max_y)
		RETURN, img[*,min_x:max_x,min_y:max_y]
	ENDIF ELSE BEGIN
		aspect	= FLOAT(dims[0])/dims[1]
		idx	= WHERE(img GT 0, cnt)
		IF cnt LE 0 THEN RETURN, img[ROUND(10*aspect),0:9]
		idx_x	= idx MOD dims[0]
		idx_y	= idx / dims[0]
		min_x	= MIN(idx_x,MAX=max_x)
		min_y	= MIN(idx_y,MAX=max_y)
		RETURN, img[min_x:max_x,min_y:max_y]
	ENDELSE

END
; trim_text_image

;===============================================================================
; trim_image
;===============================================================================
FUNCTION trim_image, img, BACKGROUND_VALUE = back_val
	IF NOT KEYWORD_SET(back_val) THEN back_value = 0 ELSE back_value = back_val

	dims	= SIZE(img, /DIMENSIONS)
	minx	= 0
	maxx	= dims[N_ELEMENTS(dims)-2]-1
	miny	= 0
	maxy	= dims[N_ELEMENTS(dims)-1]-1
	IF N_ELEMENTS(dims) EQ 3 THEN BEGIN
		;assumption = (3,x,y)
		xtotr	= TOTAL(REFORM(img[0,*,*]),2)
		ytotr	= TOTAL(REFORM(img[0,*,*]),1)
		xtotg	= TOTAL(REFORM(img[1,*,*]),2)
		ytotg	= TOTAL(REFORM(img[1,*,*]),1)
		xtotb	= TOTAL(REFORM(img[2,*,*]),2)
		ytotb	= TOTAL(REFORM(img[2,*,*]),1)
		idxx	= WHERE(						$
				xtotr NE back_value OR				$
				xtotg NE back_value OR				$
				xtotb NE back_value, cntx )
		idxy	= WHERE(						$
				ytotr NE back_value OR				$
				ytotg NE back_value OR				$
				ytotb NE back_value, cnty )
		IF cntx GT 0 THEN						$
			minx	= MIN([idxx],MAX=maxx)
		IF cnty GT 0 THEN						$
			miny	= MIN([idxy],MAX=maxy)
		img	= img[*,minx:maxx,miny:maxy]
	ENDIF ELSE BEGIN
		xtot	= TOTAL(img,2)
		ytot	= TOTAL(img,1)
		idxx	= WHERE( xtot NE back_value, cntx )
		idxy	= WHERE( ytot NE back_value, cnty )
		IF cntx GT 0 THEN						$
			minx	= MIN([idxx],MAX=maxx)
		IF cnty GT 0 THEN						$
			miny	= MIN([idxy],MAX=maxy)
		img	= img[minx:maxx,miny:maxy]
	ENDELSE

	RETURN, img
END
; trim_image

;===============================================================================
; create_save_image
;===============================================================================
FUNCTION create_save_image, ptr, PREVIEW = preview
	img_dims		= SIZE((*ptr).img, /DIMENSIONS)
	cbar_dims		= INTARR(3)
	main_txt_dims		= INTARR(3)
	second_txt_dims		= INTARR(3)
	tertiary_txt_dims	= INTARR(3)
	cbar_txt_dims		= INTARR(3)
	img2d			= 0
	at_least_one_annotation	= 0

	IF N_ELEMENTS(img_dims) LT 3 THEN BEGIN
		img2d		= 1
		img		= TRANSPOSE([[[(*ptr).img]],[[(*ptr).img]],[[(*ptr).img]]],[2,0,1])
		img_dims	= SIZE(img, /DIMENSIONS)
	ENDIF ELSE BEGIN
		img		= (*ptr).img
	ENDELSE



	IF (*ptr).add_cbar THEN BEGIN
		cbar_dims	= SIZE((*ptr).cbar, /DIMENSIONS)

		cbr		= REFORM(((*ptr).cbar)[0,*,*])
		cbg		= REFORM(((*ptr).cbar)[1,*,*])
		cbb		= REFORM(((*ptr).cbar)[2,*,*])
		idx	= WHERE(				$
				cbr GT 0 OR			$
				cbg GT 0 OR			$
				cbb GT 0, cnt )
		min_x	= 0 & max_x	= 0
		IF cnt GT 0 THEN BEGIN
			idx_x	= idx MOD cbar_dims[1]
			min_x	= MIN(idx_x,MAX=max_x)
		ENDIF


		WIDGET_CONTROL, (*ptr).units_cwf, GET_VALUE = cbar_title
		cbar_title_txt	= cbar_title[0]
		text_size	= FIX((*ptr).font_sizes[(*ptr).units_font_size_idx])
		cbar_txt_img	= trim_text_image(do_text(cbar_title_txt,text_size))

		cbar_txt_dims	= SIZE(cbar_txt_img,/DIMENSIONS)
		at_least_one_annotation	= 1

		IF (*ptr).resize_using_cbar THEN BEGIN
			cbar_xdims	= MAX([max_x-min_x+1,cbar_txt_dims[1]])
			ratio		= FLOAT(cbar_xdims) / FLOAT(img_dims[1])
			newimgr		= CONGRID(				$
						REFORM(img[0,*,*]),		$
						ROUND(ratio*img_dims[1]),	$
						ROUND(ratio*img_dims[2]),	$
						INTERP = (*ptr).interp_type EQ 0)
			newimgg		= CONGRID(				$
						REFORM(img[1,*,*]),		$
						ROUND(ratio*img_dims[1]),	$
						ROUND(ratio*img_dims[2]),	$
						INTERP = (*ptr).interp_type EQ 0)
			newimgb		= CONGRID(				$
						REFORM(img[2,*,*]),		$
						ROUND(ratio*img_dims[1]),	$
						ROUND(ratio*img_dims[2]),	$
						INTERP = (*ptr).interp_type EQ 0)

			img		= TRANSPOSE([[[newimgr]],[[newimgg]],[[newimgb]]],[2,0,1])
			img_dims	= SIZE(img, /DIMENSIONS)
		ENDIF
	ENDIF

	IF (*ptr).add_main_title THEN BEGIN
		IF (*ptr).title_array[(*ptr).main_title_wd_idx] EQ		$
			'User-Defined' THEN					$
			WIDGET_CONTROL,						$
				(*ptr).user_main_title,				$
				GET_VALUE = main_title_txt			$
		ELSE								$
			main_title_txt	=					$
				(*ptr).title_array[(*ptr).main_title_wd_idx]
		main_title_txt	= main_title_txt[0]

		text_size	= FIX((*ptr).font_sizes[(*ptr).main_font_size_idx])
		main_txt_img	= trim_text_image(do_text(main_title_txt,text_size))
		main_txt_dims	= SIZE(main_txt_img,/DIMENSIONS)
		at_least_one_annotation	= 1
	ENDIF

	IF (*ptr).add_second_title THEN BEGIN
		IF (*ptr).title_array[(*ptr).second_title_wd_idx] EQ		$
			'User-Defined' THEN					$
			WIDGET_CONTROL,						$
				(*ptr).user_second_title,			$
				GET_VALUE = second_title_txt			$
		ELSE								$
			second_title_txt	=				$
				(*ptr).title_array[(*ptr).second_title_wd_idx]
		second_title_txt	= second_title_txt[0]

		text_size	= FIX((*ptr).font_sizes[(*ptr).second_font_size_idx])
		second_txt_img	= trim_text_image(do_text(second_title_txt,text_size))
		second_txt_dims	= SIZE(second_txt_img,/DIMENSIONS)
		at_least_one_annotation	= 1
	ENDIF

	IF (*ptr).add_tertiary_title THEN BEGIN
		IF (*ptr).title_array[(*ptr).tertiary_title_wd_idx] EQ		$
			'User-Defined' THEN					$
			WIDGET_CONTROL,						$
				(*ptr).user_tertiary_title,			$
				GET_VALUE = tertiary_title_txt			$
		ELSE								$
			tertiary_title_txt	=				$
				(*ptr).title_array[(*ptr).tertiary_title_wd_idx]
		tertiary_title_txt	= tertiary_title_txt[0]

		text_size		= FIX((*ptr).font_sizes[(*ptr).tertiary_font_size_idx])
		tertiary_txt_img	= trim_text_image(do_text(tertiary_title_txt,text_size))
		tertiary_txt_dims	= SIZE(tertiary_txt_img,/DIMENSIONS)
		at_least_one_annotation	= 1
	ENDIF



	IF (*ptr).resize_using_title AND ((*ptr).add_main_title OR $
		(*ptr).add_second_title OR (*ptr).add_tertiary_title ) THEN BEGIN
		max_title_width	= MAX(	[									$
					main_txt_dims[N_ELEMENTS(main_txt_dims)-2],			$
					second_txt_dims[N_ELEMENTS(second_txt_dims)-2],			$
					tertiary_txt_dims[N_ELEMENTS(tertiary_txt_dims)-2] ] )
		ratio		= FLOAT(max_title_width) / FLOAT(img_dims[1])
		newimgr		= CONGRID(				$
					REFORM(img[0,*,*]),		$
					ROUND(ratio*img_dims[1]),	$
					ROUND(ratio*img_dims[2] ),	$
					INTERP = (*ptr).interp_type EQ 0 )
		newimgg		= CONGRID(				$
					REFORM(img[1,*,*]),		$
					ROUND(ratio*img_dims[1]),	$
					ROUND(ratio*img_dims[2] ),	$
					INTERP = (*ptr).interp_type EQ 0 )
		newimgb		= CONGRID(				$
					REFORM(img[2,*,*]),		$
					ROUND(ratio*img_dims[1]),	$
					ROUND(ratio*img_dims[2] ),	$
					INTERP = (*ptr).interp_type EQ 0 )

		img		= TRANSPOSE([[[newimgr]],[[newimgg]],[[newimgb]]],[2,0,1])
		img_dims	= SIZE(img, /DIMENSIONS)
	ENDIF




print,'img_dims=',	img_dims
print,'cbar_dims=',	cbar_dims
print,'main_txt_dims=',	main_txt_dims
print,'second_txt_dims=',	second_txt_dims
print,'tertiary_txt_dims=',	tertiary_txt_dims
print,'cbar_txt_dims=',	cbar_txt_dims
	max_text_ht	= MAX(	[									$
					main_txt_dims[N_ELEMENTS(main_txt_dims)-1],			$
					second_txt_dims[N_ELEMENTS(second_txt_dims)-1],			$
					tertiary_txt_dims[N_ELEMENTS(tertiary_txt_dims)-1],		$
					 cbar_txt_dims[N_ELEMENTS(cbar_txt_dims)-1] ] )

	padding		= max_text_ht / 4
	total_padding	= 0L
	IF (*ptr).add_cbar THEN							$
		total_padding	= padding + padding + padding ;twice as far away from image as from color bar

	IF (*ptr).add_tertiary_title THEN					$
		total_padding	= total_padding + padding
	IF (*ptr).add_second_title THEN						$
		total_padding	= total_padding + padding
	IF (*ptr).add_main_title THEN						$
		total_padding	= total_padding + padding

	IF at_least_one_annotation THEN BEGIN
		total_padding	= total_padding + padding ;to separate border of image from title
		total_padding	= total_padding + padding ;to separate border of image from color bar (if it exists) or image itself
	ENDIF ELSE BEGIN
		padding		= 0
	ENDELSE

	final_x_dim	= MAX(	[						$
					img_dims[N_ELEMENTS(img_dims)-2],				$
					cbar_dims[N_ELEMENTS(cbar_dims)-2],				$
					main_txt_dims[N_ELEMENTS(main_txt_dims)-2],			$
					second_txt_dims[N_ELEMENTS(second_txt_dims)-2],			$
					tertiary_txt_dims[N_ELEMENTS(tertiary_txt_dims)-2],		$
					cbar_txt_dims[N_ELEMENTS(cbar_txt_dims)-2] ] ) + padding + padding
	final_y_dim	= img_dims[N_ELEMENTS(img_dims)-1] +			$
			  cbar_dims[N_ELEMENTS(cbar_dims)-1] +			$
			  main_txt_dims[N_ELEMENTS(main_txt_dims)-1] +		$
			  second_txt_dims[N_ELEMENTS(second_txt_dims)-1] +	$
			  tertiary_txt_dims[N_ELEMENTS(tertiary_txt_dims)-1] +	$
			  cbar_txt_dims[N_ELEMENTS(cbar_txt_dims)-1] +		$
			  total_padding

	final_img	= BYTARR( 3, final_x_dim, final_y_dim )
help,final_img
	xs	= (-1)
	xe	= (-1)
	ys	= 0
	ye	= 0

	IF (*ptr).add_cbar THEN BEGIN
		xs				= (final_x_dim - cbar_dims[1]) / 2
		xe				= xs + cbar_dims[N_ELEMENTS(cbar_dims)-2] - 1
		ys				= ye+padding
		ye				= ys + cbar_dims[N_ELEMENTS(cbar_dims)-1] - 1
		final_img[*,xs:xe,ys:ye]	= (*ptr).cbar
		xs				= (final_x_dim - cbar_txt_dims[N_ELEMENTS(cbar_txt_dims)-2]) / 2
		xe				= xs + cbar_txt_dims[N_ELEMENTS(cbar_txt_dims)-2] - 1
		ys				= ye+padding
		ye				= ys + cbar_txt_dims[N_ELEMENTS(cbar_txt_dims)-1] - 1
		final_img[*,xs:xe,ys:ye]	= cbar_txt_img
		ye				= ye + padding
	ENDIF
	xs					= (final_x_dim - img_dims[N_ELEMENTS(img_dims)-2]) / 2
	xe					= xs + img_dims[N_ELEMENTS(img_dims)-2] - 1
	ys					= ye+padding
	ye					= ys + img_dims[N_ELEMENTS(img_dims)-1] - 1
	final_img[*,xs:xe,ys:ye]		= img
	IF (*ptr).add_tertiary_title THEN BEGIN
		xs				= (final_x_dim - tertiary_txt_dims[N_ELEMENTS(tertiary_txt_dims)-2]) / 2
		xe				= xs + tertiary_txt_dims[N_ELEMENTS(tertiary_txt_dims)-2] - 1
		ys				= ye+padding
		ye				= ys + tertiary_txt_dims[N_ELEMENTS(tertiary_txt_dims)-1] - 1
		final_img[*,xs:xe,ys:ye]	= tertiary_txt_img
	ENDIF
	IF (*ptr).add_second_title THEN BEGIN
		xs				= (final_x_dim - second_txt_dims[N_ELEMENTS(second_txt_dims)-2]) / 2
		xe				= xs + second_txt_dims[N_ELEMENTS(second_txt_dims)-2] - 1
		ys				= ye+padding
		ye				= ys + second_txt_dims[N_ELEMENTS(second_txt_dims)-1] - 1
		final_img[*,xs:xe,ys:ye]	= second_txt_img
	ENDIF
	IF (*ptr).add_main_title THEN BEGIN
		xs				= (final_x_dim - main_txt_dims[N_ELEMENTS(main_txt_dims)-2]) / 2
		xe				= xs + main_txt_dims[N_ELEMENTS(main_txt_dims)-2] - 1
		ys				= ye+padding
		ye				= ys + main_txt_dims[N_ELEMENTS(main_txt_dims)-1] - 1
		final_img[*,xs:xe,ys:ye]	= main_txt_img
	ENDIF

	IF KEYWORD_SET(preview) THEN BEGIN
		min_screen_dim	= MIN(GET_SCREEN_SIZE()*0.50)
		aspect		= FLOAT(final_x_dim) / final_y_dim
print,'final_x_dim,final_y_dim,aspect =',final_x_dim,final_y_dim,aspect
		IF aspect GT 1.0 THEN BEGIN
			preview_x_dim	= min_screen_dim
			preview_y_dim	= min_screen_dim*(1.0/aspect)
		ENDIF ELSE BEGIN
			preview_x_dim	= min_screen_dim*aspect
			preview_y_dim	= min_screen_dim
		ENDELSE
print,'preview_x_dim, preview_y_dim=',preview_x_dim, preview_y_dim
		RETURN, CONGRID( final_img,3,preview_x_dim, preview_y_dim )
	ENDIF

	RETURN, final_img
END
; create_save_image

;===============================================================================
; save_image_to_format
;===============================================================================
PRO save_image_to_format, filename, img, grayscale_wo_annotation, save_type
	CASE save_type OF
		'TIFF': BEGIN
			WRITE_TIFF, filename, REVERSE( img, (SIZE(img))[0] )
			END
		'JPEG': BEGIN
			IF grayscale_wo_annotation			$
			THEN BEGIN
				;--------------------------------------------------------------
				; Black and white JPEG image.
				;--------------------------------------------------------------
				WRITE_JPEG, filename, img, ORDER = 0, QUALITY = 100
			ENDIF ELSE BEGIN
				;--------------------------------------------------------------
				; Color JPEG image.
				; Either true or pseudo color.
				;--------------------------------------------------------------
				WRITE_JPEG, filename, img, ORDER = 0, QUALITY = 100, TRUE = 1
			ENDELSE
			END
		'VICAR': BEGIN
			IF grayscale_wo_annotation			$
			THEN BEGIN
				;--------------------------------------------------------------
				; Black and white VICAR image.
				;--------------------------------------------------------------
				write_vicar, filename + '_vicar_img', ROTATE( img, 7 )
			ENDIF ELSE BEGIN
				;--------------------------------------------------------------
				; Three VICAR images: R, G and B.
				; Either true or pseudo color.
				;--------------------------------------------------------------
				write_vicar, filename + '_vicar_red', ROTATE( REFORM( img[0,*,*] ), 7 )
				write_vicar, filename + '_vicar_grn', ROTATE( REFORM( img[1,*,*] ), 7 )
				write_vicar, filename + '_vicar_blu', ROTATE( REFORM( img[2,*,*] ), 7 )
			ENDELSE
			END
		ELSE:
	ENDCASE
END
; save_image_to_format

;===============================================================================
; save_image_gui_eh
;===============================================================================
;01234567890123456789012345678901234567890123456789012345678901234567890123456789
PRO save_image_gui_eh, event
	WIDGET_CONTROL, event.id, GET_UVALUE = widget_name
	widget_name	= STRTRIM(STRUPCASE(widget_name),2)
	WIDGET_CONTROL, event.top, GET_UVALUE = ptr

	CASE widget_name OF
		'LINEAR_INTERP': BEGIN
			(*ptr).interp_type	= event.select EQ 0
;print,'(*ptr).interp_type=',(*ptr).interp_type
			END
		'NEAR_NEIGHBOR': BEGIN
			(*ptr).interp_type	= event.select EQ 1
;print,'(*ptr).interp_type=',(*ptr).interp_type
			END
		'RESIZE_USING_CBAR': BEGIN
			(*ptr).resize_using_cbar	= event.select
			END
		'RESIZE_USING_TITLE': BEGIN
			(*ptr).resize_using_title	= event.select
			END
		'CONCAT_CBAR': BEGIN
			(*ptr).add_cbar	= event.select
			WIDGET_CONTROL, (*ptr).unit_base, SENSITIVE = event.select
			END
		'CONCAT_TITLE': BEGIN
			(*ptr).add_titles	= event.select
			(*ptr).add_main_title	= event.select AND			$
				(*ptr).title_array[(*ptr).main_title_wd_idx] NE		$
				'None'
			(*ptr).add_second_title						$
						= event.select AND			$
				(*ptr).title_array[(*ptr).second_title_wd_idx] NE	$
				'None'
			(*ptr).add_tertiary_title					$
						= event.select AND			$
				(*ptr).title_array[(*ptr).tertiary_title_wd_idx] NE	$
				'None'
			WIDGET_CONTROL, (*ptr).main_title_sub_base, SENSITIVE = event.select
			WIDGET_CONTROL, (*ptr).second_title_sub_base, SENSITIVE = event.select
			WIDGET_CONTROL, (*ptr).tertiary_title_sub_base, SENSITIVE = event.select
			WIDGET_CONTROL, (*ptr).resize_using_title_sub_base, SENSITIVE = event.select
			WIDGET_CONTROL,							$
				(*ptr).user_main_title,					$
				SENSITIVE = event.select AND				$
				(*ptr).title_array[(*ptr).main_title_wd_idx] EQ 'User-Defined'
			WIDGET_CONTROL,							$
				(*ptr).user_second_title,				$
				SENSITIVE = event.select AND				$
				(*ptr).title_array[(*ptr).second_title_wd_idx] EQ 'User-Defined'
			WIDGET_CONTROL,							$
				(*ptr).user_tertiary_title,				$
				SENSITIVE = event.select AND				$
				(*ptr).title_array[(*ptr).tertiary_title_wd_idx] EQ 'User-Defined'
			END
		'MAIN_TITLE_WD': BEGIN
			(*ptr).main_title_wd_idx	= event.index
			(*ptr).add_main_title	= 				$
				(*ptr).title_array[(*ptr).main_title_wd_idx] NE 'None' AND (*ptr).add_titles
			WIDGET_CONTROL,							$
				(*ptr).user_main_title,					$
				SENSITIVE = (*ptr).title_array[(*ptr).main_title_wd_idx] EQ 'User-Defined'
			END
		'SECOND_TITLE_WD': BEGIN
			(*ptr).second_title_wd_idx	= event.index
			(*ptr).add_second_title	= 				$
				(*ptr).title_array[(*ptr).second_title_wd_idx] NE 'None' AND (*ptr).add_titles
			WIDGET_CONTROL,							$
				(*ptr).user_second_title,				$
				SENSITIVE = (*ptr).title_array[(*ptr).second_title_wd_idx] EQ 'User-Defined'
			END
		'TERTIARY_TITLE_WD': BEGIN
			(*ptr).tertiary_title_wd_idx	= event.index
			(*ptr).add_tertiary_title	= 				$
				(*ptr).title_array[(*ptr).tertiary_title_wd_idx] NE 'None' AND (*ptr).add_titles
			WIDGET_CONTROL,							$
				(*ptr).user_tertiary_title,				$
				SENSITIVE = (*ptr).title_array[(*ptr).tertiary_title_wd_idx] EQ 'User-Defined'
			END
		'MAIN_TITLE_FONT_SIZE_WD': BEGIN
			(*ptr).main_font_size_idx	= event.index
			END
		'SECOND_TITLE_FONT_SIZE_WD': BEGIN
			(*ptr).second_font_size_idx	= event.index
			END
		'TERTIARY_TITLE_FONT_SIZE_WD': BEGIN
			(*ptr).tertiary_font_size_idx	= event.index
			END
		'UNITS_FONT_SIZE_WD': BEGIN
			(*ptr).units_font_size_idx	= event.index
			END
		'PREVIEW': BEGIN
			IF (*ptr).resize_using_cbar AND (*ptr).resize_using_title THEN BEGIN
				res	= DIALOG_MESSAGE( [					$
					'Select a MAXIMUM of ONE of the reszing options!' ], /INFORMATION )
				RETURN
			ENDIF
			show_preview_image, create_save_image(ptr,/PREVIEW), event.top, (*ptr).filename
			END
		'SAVE': BEGIN
			IF (*ptr).resize_using_cbar AND (*ptr).resize_using_title THEN BEGIN
				res	= DIALOG_MESSAGE( [					$
					'Select a MAXIMUM of ONE of the reszing options!' ], /INFORMATION )
				RETURN
			ENDIF
			original_dims		= SIZE((*ptr).img,/DIMENSIONS)
			original_is_grayscale	= N_ELEMENTS(original_dims) LT 3
			img			= create_save_image(ptr)
			new_dims		= SIZE(img,/DIMENSIONS)
			grayscale_wo_annotation	= 0

			IF								$
				original_is_grayscale AND				$
				original_dims[0] EQ new_dims[1] AND			$
				original_dims[1] EQ new_dims[2] THEN BEGIN
				grayscale_wo_annotation	= 1
				img			= REFORM(img[0,*,*])
			ENDIF

			save_image_to_format, (*ptr).filename, img, grayscale_wo_annotation, (*ptr).save_type

			PTR_FREE, ptr

			WIDGET_CONTROL, event.top, /DESTROY
			END
		'CANCEL': BEGIN
			PTR_FREE, ptr
			WIDGET_CONTROL, event.top, /DESTROY
			END
		ELSE:
	ENDCASE
END
; save_image_gui_eh

;===============================================================================
; save_image_gui_kill
;===============================================================================
PRO save_image_gui_kill, tlb
	WIDGET_CONTROL, tlb, GET_UVALUE = ptr
	PTR_FREE, ptr
END
; save_image_gui_kill

;===============================================================================
; save_image_gui
;===============================================================================
;01234567890123456789012345678901234567890123456789012345678901234567890123456789
PRO save_image_gui,								$
		group_leader,							$
		filename,							$
		colorbar_present,						$
		title_array,							$
		unit_str,							$
		img,								$
		cbar,								$
		save_type

	msg	= [								$
			'Do you want to add annotations (title, color bar)',	$
			'to the image to be saved?' ]
	ans	= DIALOG_MESSAGE(msg,/QUESTION)
	IF STRTRIM(STRUPCASE(ans),2) EQ 'NO' THEN BEGIN
		grayscale_wo_annotation	= N_ELEMENTS(SIZE(img,/DIMENSIONS)) LT 3
		save_image_to_format, filename, img, grayscale_wo_annotation, save_type
		RETURN
	ENDIF

	FOR i = 0, N_ELEMENTS(title_array)-1 DO BEGIN
		IF STRTRIM(title_array[i],2) EQ '' THEN				$
			title_array[i] = 'Title ' + STRTRIM(i+1,2)
	ENDFOR
	title_array	= [title_array,'User-Defined','None']
	font_sizes	= STRTRIM([12,18,24,32,36,40,45,48,52,56,64,72,80,96],2)
	default_font_idx							$
			= 6
	IF STRTRIM(unit_str,2) EQ '' THEN unit_str = 'Color Bar'

	tlb		= WIDGET_BASE(						$
				GROUP_LEADER = group_leader,			$
				/COLUMN,					$
				KILL_NOTIFY = 'save_image_gui_kill',		$
				TITLE = 'Save Image Options',			$
;;;				/MODAL,						$
				EVENT_PRO = 'save_image_gui_eh' )
	fname_lbl1	= WIDGET_LABEL(						$
				tlb,						$
				VALUE = 'File To Be Saved:' )
	fname_lbl2	= WIDGET_LABEL(						$
				tlb,						$
				VALUE = filename )
	fname_lbl3	= WIDGET_LABEL(						$
				tlb,						$
				VALUE = '' )
	units_cwf	= (-1L)
	unit_base	= (-1L)
	IF colorbar_present THEN BEGIN
		cbar_nonex_base							$
			= WIDGET_BASE(						$
				tlb,						$
				/COLUMN,					$
				/NONEXCLUSIVE )
		concat_cbar							$
			= WIDGET_BUTTON(					$
				cbar_nonex_base,				$
				VALUE = 'Concatenate Color Bar To Image',	$
				UVALUE = 'concat_cbar' )
				
		cbar_base	= WIDGET_BASE(					$
				tlb,						$
				/COLUMN )
		unit_base							$
			= WIDGET_BASE(						$
				cbar_base,					$
				SENSITIVE = 0,					$
				/FRAME,						$
				/COLUMN )
		units_cwf							$
			= CW_FIELD(						$
				unit_base,					$
				VALUE = unit_str,				$
				TITLE = 'Color Bar Title:',			$
				/STRING,					$
				/ROW,						$
				UVALUE = 'units_cwf' )
		units_font_size_wd						$
			= WIDGET_DROPLIST(					$
				unit_base,					$
				VALUE = font_sizes,				$
				TITLE = 'Color Bar Title Text Size',		$
				UVALUE = 'units_font_size_wd' )
		cbar_nonex_base2						$
			= WIDGET_BASE(						$
				unit_base,					$
				/COLUMN,					$
				/NONEXCLUSIVE )
		resize_img_using_cbar						$
			= WIDGET_BUTTON(					$
				cbar_nonex_base2,				$
				VALUE = 'Resize Image To Color Bar Width',	$
				UVALUE = 'resize_using_cbar' )
	ENDIF
	fname_lbl4	= WIDGET_LABEL(						$
				tlb,						$
				VALUE = '' )

	title_nonex_base							$
			= WIDGET_BASE(						$
				tlb,						$
				/COLUMN,					$
				/NONEXCLUSIVE )
	concat_title	= WIDGET_BUTTON(					$
				title_nonex_base,				$
				VALUE = 'Concatentate Title To Image',		$
				UVALUE = 'concat_title' )
	title_base	= WIDGET_BASE(						$
				tlb,						$
				/COLUMN )
	main_title_sub_base							$
			= WIDGET_BASE(						$
				title_base,					$
				SENSITIVE = 0,					$
				/FRAME,						$
				/COLUMN )
	main_title_wd	= WIDGET_DROPLIST(					$
				main_title_sub_base,				$
				VALUE = title_array,				$
				TITLE = 'Main Title',				$
				UVALUE = 'main_title_wd' )
	user_main_title	= WIDGET_TEXT(						$
				main_title_sub_base,				$
				VALUE = 'Main Title',				$
				/EDITABLE,					$
				SENSITIVE = 0,					$
				UVALUE = 'user_main_title' )
	main_title_font_size_wd							$
			= WIDGET_DROPLIST(					$
				main_title_sub_base,				$
				VALUE = font_sizes,				$
				TITLE = 'Main Title Text Size',			$
				UVALUE = 'main_title_font_size_wd' )
	second_title_sub_base							$
			= WIDGET_BASE(						$
				title_base,					$
				SENSITIVE = 0,					$
				/FRAME,						$
				/COLUMN )
	second_title_wd	= WIDGET_DROPLIST(					$
				second_title_sub_base,				$
				VALUE = title_array,				$
				TITLE = 'Secondary Title',			$
				UVALUE = 'second_title_wd' )
	user_second_title							$
			= WIDGET_TEXT(						$
				second_title_sub_base,				$
				SENSITIVE = 0,					$
				/EDITABLE,					$
				VALUE = 'Secondary Title',			$
				UVALUE = 'user_main_title' )
	second_title_font_size_wd						$
			= WIDGET_DROPLIST(					$
				second_title_sub_base,				$
				VALUE = font_sizes,				$
				TITLE = 'Secondary Title Text Size',		$
				UVALUE = 'second_title_font_size_wd' )
	tertiary_title_sub_base							$
			= WIDGET_BASE(						$
				title_base,					$
				SENSITIVE = 0,					$
				/FRAME,						$
				/COLUMN )
	tertiary_title_wd							$
			= WIDGET_DROPLIST(					$
				tertiary_title_sub_base,			$
				VALUE = title_array,				$
				TITLE = 'Tertiary Title',			$
				UVALUE = 'tertiary_title_wd' )
	user_tertiary_title							$
			= WIDGET_TEXT(						$
				tertiary_title_sub_base,			$
				VALUE = 'Tertiary Title',			$
				/EDITABLE,					$
				SENSITIVE = 0,					$
				UVALUE = 'user_main_title' )
	tertiary_title_font_size_wd						$
			= WIDGET_DROPLIST(					$
				tertiary_title_sub_base,			$
				VALUE = font_sizes,				$
				TITLE = 'Tertiary Title Text Size',		$
				UVALUE = 'tertiary_title_font_size_wd' )

	resize_using_title_sub_base						$
			= WIDGET_BASE(						$
				title_base,					$
				SENSITIVE = 0,					$
				/FRAME,						$
				/NONEXCLUSIVE,					$
				/COLUMN )
	resize_img_using_title							$
			= WIDGET_BUTTON(					$
				resize_using_title_sub_base,			$
				VALUE = 'Resize Image To Title Width',$
				UVALUE = 'resize_using_title' )
	fname_lbl5	= WIDGET_LABEL(						$
				tlb,						$
				VALUE = '' )
	resize_base	= WIDGET_BASE(						$
				tlb,						$
				/COLUMN )
	resize_lbl	= WIDGET_LABEL(						$
				resize_base,					$
				VALUE = 'Image Resize Method (if used): ' )
	resize_ex_base	= WIDGET_BASE(						$
				resize_base,					$
				/COLUMN,					$
				/EXCLUSIVE )
	lin_interp_btn	= WIDGET_BUTTON(					$
				resize_ex_base,					$
				VALUE = 'Linear Interpolation',			$
				UVALUE = 'linear_interp' )
	neighbor_btn	= WIDGET_BUTTON(					$
				resize_ex_base,					$
				VALUE = 'Nearest Neighbor',			$
				UVALUE = 'near_neighbor' )
	fname_lbl6	= WIDGET_LABEL(						$
				tlb,						$
				VALUE = '' )

	btn_base	= WIDGET_BASE(						$
				tlb,						$
				/ROW )
	preview_btn	= WIDGET_BUTTON(					$
				btn_base,					$
				VALUE = 'Preview...',				$
				UVALUE = 'preview' )
	save_btn	= WIDGET_BUTTON(					$
				btn_base,					$
				VALUE = 'Save Annotated Image',			$
				UVALUE = 'save' )
	cancel_btn	= WIDGET_BUTTON(					$
				btn_base,					$
				VALUE = 'Cancel',				$
				UVALUE = 'cancel' )

	WIDGET_CONTROL, tlb, /REALIZE

	IF colorbar_present THEN						$
		WIDGET_CONTROL, units_font_size_wd, SET_DROPLIST_SELECT = default_font_idx
	WIDGET_CONTROL, main_title_font_size_wd, SET_DROPLIST_SELECT = default_font_idx
	WIDGET_CONTROL, second_title_font_size_wd, SET_DROPLIST_SELECT = default_font_idx
	WIDGET_CONTROL, tertiary_title_font_size_wd, SET_DROPLIST_SELECT = default_font_idx
	WIDGET_CONTROL, neighbor_btn, /SET_BUTTON
	ptr		= PTR_NEW( {						$
				filename		: filename,		$
				add_titles		: 0,			$
				add_main_title		: 0,			$
				add_second_title	: 0,			$
				add_tertiary_title	: 0,			$
				add_cbar		: 0,			$
				units_cwf		: units_cwf,		$
				unit_base		: unit_base,		$
				unit_str		: unit_str,		$
				title_base		: title_base,		$
				title_array		: title_array,		$
				main_title_wd_idx	: 0,			$
				user_main_title		: user_main_title,	$
				second_title_wd_idx	: 0,			$
				user_second_title	: user_second_title,	$
				tertiary_title_wd_idx	: 0,			$
				user_tertiary_title	: user_tertiary_title,	$
				img			: trim_image(img),	$
				cbar			: cbar,			$
				save_type		: save_type,		$
				font_sizes		: font_sizes,		$
				main_title_sub_base	: main_title_sub_base,	$
				second_title_sub_base	: second_title_sub_base,   $
				tertiary_title_sub_base	: tertiary_title_sub_base, $
				resize_using_title_sub_base			$
							: resize_using_title_sub_base, $
				resize_using_cbar	: 0,			$
				resize_using_title	: 0,			$
				interp_type		: 1,			$
				units_font_size_idx	: default_font_idx,	$
				main_font_size_idx	: default_font_idx,	$
				second_font_size_idx	: default_font_idx,	$
				tertiary_font_size_idx	: default_font_idx }, /NO_COPY )
	WIDGET_CONTROL, tlb, SET_UVALUE = ptr

	XMANAGER,								$
		'save_image_gui',						$
		tlb,								$
		EVENT_HANDLER = 'save_image_gui_eh'
END
; save_image_gui
