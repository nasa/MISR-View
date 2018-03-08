@box_filter_float.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ do_reprojection @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION do_reprojection,							$
			lon_stack,						$
			lat_stack,						$
			img_stack,						$
			map_set_str,						$
			w_dims,							$
			fill_seams,						$
			box_filter_width,					$
			pct,							$
			min_good_data_val,					$
			max_good_data_val,					$
			missing_val,						$
			in_data_type

;help,img_stack
	IDL_DATA_TYPE_ARRAY	= [								$
					'UNDEFINED',						$
					'BYTARR',						$
					'INTARR',						$
					'LONARR',						$
					'FLTARR',						$
					'DBLARR',						$
					'NOT SUPPORTED',					$
					'STRARR',						$
					'NOT SUPPORTED',					$
					'NOT SUPPORTED',					$
					'NOT SUPPORTED',					$
					'NOT SUPPORTED',					$
					'UINTARR',						$
					'ULONARR',						$
					'LON64ARR',						$
					'ULON64ARR' ]

	window_width		= w_dims[0]
	window_height		= w_dims[1]
	type2use 		= in_data_type
	img_stack 		= convert_number( img_stack, type2use )
	bad_data 		= missing_val

final_img	= CALL_FUNCTION(IDL_DATA_TYPE_ARRAY[SIZE(img_stack, /TYPE)],window_width,window_height)+bad_data
;;;ckt,aug2004	str2exec		= 'final_img='+IDL_DATA_TYPE_ARRAY[SIZE(img_stack, /TYPE)]+'(window_width,window_height)+bad_data'
;;;ckt,aug2004	success			= EXECUTE( str2exec )

;;;ckt,aug2004	IF NOT success THEN BEGIN
;;;ckt,aug2004		res	= DIALOG_MESSAGE( [							$
;;;ckt,aug2004						'Problem with data type in do_projection...',	$
;;;ckt,aug2004						'Please check...' ], /ERROR )
;;;ckt,aug2004		RETURN, BYTARR( window_width, window_height )
;;;ckt,aug2004	ENDIF

	IF (SIZE(img_stack))[0] LE 2 THEN n_blocks = 1 ELSE n_blocks = (SIZE(img_stack))[1]

	old_win			= !D.WINDOW
;print,'=================== old_win = ',old_win
	WINDOW, /FREE, /PIXMAP, XSIZE = window_width, YSIZE = window_height
	pix_id			= !D.WINDOW
;print,'=================== pix_id = ',pix_id
;print,'!d.x_size,!d.y_size = ',!d.x_size,!d.y_size



;;; print,'map_set_str=',map_set_str
sep	= STR_SEP(map_set_str,',')
p1	= FLOAT(sep[1])
p2	= FLOAT(sep[2])
p3	= FLOAT(sep[3])



tmpsep	= sep[4:N_ELEMENTS(sep)-1]
tmpctr				= 0
last_str_with_equal_sign	= ''
FOR p = 0, N_ELEMENTS(tmpsep)-1 DO BEGIN
	IF STRPOS(tmpsep[p],'=') GE 0 OR STRPOS(tmpsep[p],'/') GE 0 THEN BEGIN
		IF last_str_with_equal_sign NE '' THEN BEGIN
			tmpsep[tmpctr]		= last_str_with_equal_sign
			tmpctr			= tmpctr + 1
		ENDIF
		last_str_with_equal_sign	= tmpsep[p]
	ENDIF ELSE BEGIN
		last_str_with_equal_sign	= last_str_with_equal_sign + ',' + tmpsep[p]
	ENDELSE
ENDFOR
sep[4:4+tmpctr-1]	= tmpsep[0:tmpctr-1]
count2use		= 4+tmpctr-1




FOR p = 4, count2use DO BEGIN
	str	= STRTRIM(sep[p],2)
;;; print,'str = ',str
	IF STRMID(str,0,1) EQ '/' THEN BEGIN
		keywd	= STRMID(str,1)
		IF p EQ 4 THEN BEGIN
			extra_struct	= CREATE_STRUCT(keywd,1)
		ENDIF ELSE BEGIN
			extra_struct	= CREATE_STRUCT(keywd,1,extra_struct)
		ENDELSE
	ENDIF ELSE BEGIN
		sep2	= STR_SEP(str,'=')
		keywd	= STRTRIM(sep2[0],2)

		IF N_ELEMENTS(STR_SEP(sep2[1],',')) GT 1 THEN BEGIN
			pos1	= STRPOS(sep2[1],'[')
			pos2	= STRPOS(sep2[1],']')
			IF pos2 LT 0 THEN pos2 = STRLEN(sep2[1])
			sep3	= STRMID(sep2[1],pos1+1,pos2-pos1-1)
			keyval	= FLOAT(STR_SEP(sep3,','))
		ENDIF ELSE BEGIN
			keyval	= FLOAT(STRTRIM(sep2[1],2))
		ENDELSE

		IF p EQ 4 THEN BEGIN
			extra_struct	= CREATE_STRUCT(keywd,keyval)
		ENDIF ELSE BEGIN
			extra_struct	= CREATE_STRUCT(keywd,keyval,extra_struct)
		ENDELSE
	ENDELSE
ENDFOR
CALL_PROCEDURE,'MAP_SET',p1,p2,p3,_Extra=extra_struct


;;;ckt,aug2004	success			= EXECUTE(map_set_str)

;;;ckt,aug2004	IF NOT success THEN BEGIN
;;;ckt,aug2004		res	= DIALOG_MESSAGE( [							$
;;;ckt,aug2004						'Problem with MAP_SET string...',		$
;;;ckt,aug2004						'Please check...' ], /ERROR )
;;;ckt,aug2004		RETURN, BYTARR( window_width, window_height )
;;;ckt,aug2004	ENDIF

	FOR j = 0, n_blocks - 1 DO BEGIN
		IF MIN(REFORM(img_stack[j,*,*])) NE MAX(REFORM(img_stack[j,*,*])) THEN BEGIN
			lon2use	= REFORM(lon_stack[j,*,*])
			lat2use	= REFORM(lat_stack[j,*,*])
			img2use	= REFORM(img_stack[j,*,*])
			img_w	= (SIZE(img2use))[1]
			img_h	= (SIZE(img2use))[2]
			lon_w	= (SIZE(lon2use))[1]
			lon_h	= (SIZE(lon2use))[2]
			lat_w	= (SIZE(lat2use))[1]
			lat_h	= (SIZE(lat2use))[2]
			IF img_w NE lon_w OR img_w NE lat_w OR img_h NE lon_h OR img_h NE lat_h THEN BEGIN
				width2use	= MAX([img_w,lon_w,lat_w])
				height2use	= MAX([img_h,lon_h,lat_h])
				img2use		= CONGRID( img2use, width2use, height2use, /INTERP, /MINUS_ONE )
				lat2use		= CONGRID( lat2use, width2use, height2use, /INTERP, /MINUS_ONE )
				undo_crossover	= 0
				crossover_idx	= WHERE( lon2use[0,*] GT lon2use[lon_w-1,*], crossover_cnt )
				IF crossover_cnt GT 0 THEN BEGIN
					lon_idx		= WHERE( lon2use LT 0.0, lon_cnt )
					IF lon_cnt GT 0 THEN lon2use[lon_idx]	= lon2use[lon_idx] + 360.0
					undo_crossover	= 1
				ENDIF
				lon2use		= CONGRID( lon2use, width2use, height2use, /INTERP, /MINUS_ONE )
				IF undo_crossover THEN BEGIN
					crossover_idx	= WHERE( lon2use GT 180.0, crossover_cnt )
					IF crossover_cnt GT 0 THEN lon2use[crossover_idx] = lon2use[crossover_idx] - 360.0
				ENDIF
			ENDIF
;print,'SIZE(img2use,/TYPE) = ',SIZE(img2use,/TYPE)

IF SIZE(img2use,/TYPE) EQ 5 THEN img2use = FLOAT(img2use)

;IF SIZE(lon2use,/TYPE) EQ 5 THEN lon2use = FLOAT(lon2use)
;IF SIZE(lat2use,/TYPE) EQ 5 THEN lat2use = FLOAT(lat2use)

;help,img2use
;help,lon2use
;help,lat2use
;print,'min,max of img2use = ',min(img2use),max(img2use),min(img2use[where(img2use ne min(img2use))])
;;;			img_xy_dims	= SIZE( img2use, /DIMENSIONS )

			out_img	= MAP_PATCH( img2use, lon2use, lat2use, XSTART = xs, YSTART = ys, MISSING = bad_data )
;;;			out_img		= MAP_PATCH(							$
;;;				img2use,								$
;;;				CONGRID( lon2use, img_xy_dims[0], img_xy_dims[1], /INTERP, /MINUS_ONE ),$
;;;				CONGRID( lat2use, img_xy_dims[0], img_xy_dims[1], /INTERP, /MINUS_ONE ),$
;;;				XSTART = xs,								$
;;;				YSTART = ys,								$
;;;				MISSING = bad_data )

;print,'min,max of out_img = ',min(out_img),max(out_img)
			x_start_final	= xs
			x_end_final	= MIN( [ xs+(SIZE(out_img))[1], window_width ] ) - 1
			y_start_final	= ys
			y_end_final	= MIN( [ ys+(SIZE(out_img))[2], window_height ] ) - 1
			x_start_out	= 0
			x_end_out	= x_end_final - x_start_final
			y_start_out	= 0
			y_end_out	= y_end_final - y_start_final

			tmp_img1	= final_img[ x_start_final:x_end_final, y_start_final:y_end_final ]
			tmp_img2	= out_img[ x_start_out:x_end_out, y_start_out:y_end_out ]

			idx1		= WHERE( tmp_img1 NE bad_data AND tmp_img2 NE bad_data, cnt1 )

			idx2		= WHERE( (tmp_img1 NE bad_data AND tmp_img2 EQ bad_data), cnt2 )

			idx3		= WHERE( (tmp_img1 EQ bad_data AND tmp_img2 NE bad_data), cnt3 )


new_img	= CALL_FUNCTION(IDL_DATA_TYPE_ARRAY[SIZE(img_stack, /TYPE)],x_end_final-x_start_final+1,y_end_final-y_start_final+1)+bad_data
;;;ckt,aug2004			str2exec	= 'new_img='+IDL_DATA_TYPE_ARRAY[SIZE(img_stack, /TYPE)]+'(x_end_final-x_start_final+1,y_end_final-y_start_final+1)+bad_data'
;;;ckt,aug2004			success		= EXECUTE(str2exec)

				;==================================================================================
				; the averaging step below assumes that only two tiles will ever coincide at a single pixel.
				;==========================================================================================
				IF cnt1 GT 0 THEN new_img[idx1]	= (tmp_img1[idx1]+tmp_img2[idx1])/2.0D
				IF cnt2 GT 0 THEN new_img[idx2] = tmp_img1[idx2]
				IF cnt3 GT 0 THEN new_img[idx3] = tmp_img2[idx3]
				final_img[x_start_final:x_end_final,y_start_final:y_end_final]	= new_img
		ENDIF
	ENDFOR

	good_idx		= WHERE(final_img NE bad_data,good_cnt)
	IF good_cnt LE 0 THEN RETURN, final_img

	min_good		= [ MIN(final_img[good_idx]) ]
	max_good		= [ MAX(final_img[good_idx]) ]
	min_good		= min_good_data_val[0]
	max_good		= max_good_data_val[0]
	miss_val		= bad_data
	idx			= WHERE( final_img LT min_good OR final_img GT max_good, cnt )
	IF cnt GT 0 THEN final_img[idx]	= bad_data
	filter_width		= box_filter_width
	pct_good_pixels_in_box	= pct

	IF fill_seams THEN BEGIN

			final_img		= box_filter_float(						$
									final_img,				$
									filter_width,				$
									min_good,				$
									max_good,				$
									pct_good_pixels_in_box )
			filter_width		= filter_width + 2
			min_good		= bad_data
			max_good		= bad_data

			final_img		= box_filter_float(						$
									final_img,				$
									filter_width,				$
									min_good,				$
									max_good,				$
									pct_good_pixels_in_box )
	ENDIF

	WDELETE, pix_id

	WSET, old_win
	RETURN, final_img
END
; do_reprojection
