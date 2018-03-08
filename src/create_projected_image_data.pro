;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ get_missing_val_info @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION get_missing_val_info, in_val

	data_type		= SIZE(in_val,/TYPE)
	orig_data_type		= data_type
	default_missing_val	= [ -999, -9999, -99999 ]
	
	CASE data_type OF
		1: one_type = 1B
		2: one_type = 1
		3: one_type = 1L
		4: one_type = 1.0
		5: one_type = 1.0D
		7: one_type = '1'
		12: one_type = 1U
		13: one_type = 1UL
		14: one_type = LONG64(1)
		15: one_type = ULONG64(1)
		ELSE: one_type = 1
	ENDCASE
	
	;=======================================================================
	; Force missing_val to be in_val - 1 (in_val is the minimum value of
	; the "good" data)
	;=======================================================================
	missing_val	= in_val - one_type
	
	;=======================================================================
	; If wraparound is encountered (such as if in_val = 0B and
	; missing_val = 0B - 1B = 255B) then promote data type and perform
	; same subtraction
	;=======================================================================
	IF missing_val GT in_val THEN BEGIN
		missing_val	= in_val
		IF data_type NE 14 AND data_type NE 15 AND data_type NE 5 AND data_type NE 7 THEN BEGIN
			CASE data_type OF
				1: BEGIN & in_val = FIX(in_val) & data_type = 2 & END
				2: BEGIN & in_val = LONG(in_val) & data_type = 3 & END
				3: BEGIN & in_val = LONG64(in_val) & data_type = 14 & END
				4: BEGIN & in_val = DOUBLE(in_val) & data_type = 5 & END
				12: BEGIN & in_val = LONG64(in_val) & data_type = 14 & END
				13: BEGIN & in_val = LONG64(in_val) & data_type = 14 & END
				ELSE:
			ENDCASE
			missing_val	= in_val - convert_number(1,data_type)
		ENDIF
	ENDIF
	
	done	= 0
	miss_ctr	= 0
	;=======================================================================
	; Set missing_val to one of the default missing data values.
	;=======================================================================
	WHILE NOT done DO BEGIN
		IF convert_number(default_missing_val[miss_ctr],data_type) LE default_missing_val[miss_ctr] THEN BEGIN
			IF default_missing_val[miss_ctr] LT missing_val THEN BEGIN
				missing_val = convert_number(default_missing_val[miss_ctr],data_type)
				done	= 1
			ENDIF
		ENDIF ELSE BEGIN
			done	= 1
		ENDELSE
		
		miss_ctr	= miss_ctr + 1
		IF miss_ctr EQ N_ELEMENTS(default_missing_val) THEN done = 1
	ENDWHILE
	
	RETURN, { missing_val: missing_val, data_type: data_type, promoted:(data_type NE orig_data_type) }
END
; get_missing_val_info

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ get_image_data_minmaxmissing @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION get_image_data_minmaxmissing, rgba_objarr
	n_georef_img	= N_ELEMENTS( rgba_objarr[0,*] )
	n_bands		= N_ELEMENTS( rgba_objarr[*,0] )
	FOR band_ctr = 0, n_bands - 1 DO BEGIN
		
		global_min_val		= 0.0D
		global_max_val		= 0.0D
		global_type_code	= SIZE(global_max_val,/TYPE)
		notfound	= 1
		FOR georef_ctr = 0, n_georef_img-1 DO BEGIN
			IF OBJ_VALID(rgba_objarr[ band_ctr, georef_ctr ]) THEN BEGIN
			
				min_val	= (rgba_objarr[ band_ctr, georef_ctr ])->GetMinVal(			$
							0,							$
							(rgba_objarr[band_ctr,georef_ctr])->GetImageWidth()-1,	$
							0,							$
							(rgba_objarr[band_ctr,georef_ctr])->GetImageHeight()-1,	$
							(rgba_objarr[band_ctr,georef_ctr])->GetImageWidth(),	$
							(rgba_objarr[band_ctr,georef_ctr])->GetImageHeight() )
				max_val	= (rgba_objarr[ band_ctr, georef_ctr ])->GetMaxVal(			$
							0,							$
							(rgba_objarr[band_ctr,georef_ctr])->GetImageWidth()-1,	$
							0,							$
							(rgba_objarr[band_ctr,georef_ctr])->GetImageHeight()-1,	$
							(rgba_objarr[band_ctr,georef_ctr])->GetImageWidth(),	$
							(rgba_objarr[band_ctr,georef_ctr])->GetImageHeight() )
;help,min_val
;help,max_val
				IF notfound THEN BEGIN
;;;ckt,apr2001					global_type_code= SIZE(min_val,/TYPE)
					global_type_code= (rgba_objarr[band_ctr,georef_ctr])->GetNumberType()
					global_min_val	= min_val
					global_max_val	= max_val
					notfound	= 0
				ENDIF ELSE BEGIN
					global_min_val	= MIN([min_val,global_min_val])
					global_max_val	= MAX([max_val,global_max_val])
				ENDELSE
			ENDIF
		ENDFOR
		
		s			= get_missing_val_info(convert_number(global_min_val,global_type_code))
		global_missing_val	= s.missing_val
		global_data_type_code	= s.data_type
		
		IF s.promoted THEN PRINT, '>>>>>>>>>>>>>>>>> Some data has been promoted to a number type with greater range to accomodate missing data values'
		
		IF band_ctr LE 0 THEN BEGIN
			rgba_global_min_val	= [ DOUBLE( global_min_val ) ]
			rgba_global_max_val	= [ DOUBLE( global_max_val ) ]
			rgba_missing_val	= [ DOUBLE( global_missing_val ) ]
			rgba_data_type_code	= [ global_data_type_code ]
		ENDIF ELSE BEGIN
			rgba_global_min_val	= [ rgba_global_min_val, DOUBLE( global_min_val ) ]
			rgba_global_max_val	= [ rgba_global_max_val, DOUBLE( global_max_val ) ]
			rgba_missing_val	= [ rgba_missing_val, DOUBLE( global_missing_val ) ]
			rgba_data_type_code	= [ rgba_data_type_code, global_data_type_code ]
		ENDELSE
		
	ENDFOR
	
	RETURN, { rgba_global_min_val:rgba_global_min_val, rgba_global_max_val:rgba_global_max_val, rgba_missing_val:rgba_missing_val, rgba_data_type_code:rgba_data_type_code }
END
; get_image_data_minmaxmissing

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ get_empty_proj_img @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION get_empty_proj_img, window_dims, type_code, missing_val
	CASE type_code OF
		1: m = BYTARR( window_dims[0], window_dims[1] ) + missing_val
		2: m = INTARR( window_dims[0], window_dims[1] ) + missing_val
		3: m = LONARR( window_dims[0], window_dims[1] ) + missing_val
		4: m = FLTARR( window_dims[0], window_dims[1] ) + missing_val
		5: m = DBLARR( window_dims[0], window_dims[1] ) + missing_val
		12: m = UINTARR( window_dims[0], window_dims[1] ) + missing_val
		13: m = ULONARR( window_dims[0], window_dims[1] ) + missing_val
		14: m = ULON64ARR( window_dims[0], window_dims[1] ) + missing_val
		15: m = LON64ARR( window_dims[0], window_dims[1] ) + missing_val
		ELSE: m = BYTARR( window_dims[0], window_dims[1] ) + missing_val
	ENDCASE
	
	RETURN, m
END
; get_empty_proj_img

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ create_projected_image_data @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION create_projected_image_data,						$
				rgba_objarr,					$
				lon_objarr,					$
				lat_objarr,					$
				map_set_str,					$
				fill_seams,					$
				window_dims,					$
				fill_seam_parameters,				$
				resolve_overlap_idx,				$
				group_leader

	;=======================================================================
	; rgba_objarr is dimensioned [ nb, ng ], where:
	;	nb	= number of bands
	;	ng	= number of GEOREF_IMAGE objects
	;=======================================================================
	
	SLICE_SIZE	= 256L

	n_georef_img	= N_ELEMENTS( rgba_objarr[0,*] )
	n_bands		= N_ELEMENTS( rgba_objarr[*,0] )
	n_lonlat_img	= N_ELEMENTS(lon_objarr)
	n_bands		= n_bands + 2
	tmp_rgba_objarr	= OBJARR(n_bands,n_georef_img)
	
	FOR i = 0, n_bands - 1 DO BEGIN
		FOR j = 0, n_georef_img - 1 DO BEGIN
			CASE 1 OF
				i LT n_bands - 2: tmp_rgba_objarr[i,j] = rgba_objarr[i,j]
				i EQ n_bands - 2: BEGIN
					tmp_rgba_objarr[i,j] = lon_objarr[(n_lonlat_img GT 1)*j]
					END
				ELSE: BEGIN
					tmp_rgba_objarr[i,j] = lat_objarr[(n_lonlat_img GT 1)*j]
					END
			ENDCASE
		ENDFOR
	ENDFOR
	
	rgba_objarr	= tmp_rgba_objarr
	
	out_img_objarr	= OBJARR(n_bands)
	
	minmaxmissing	= get_image_data_minmaxmissing(rgba_objarr)
	
	;==================================================================
	;
	; Progress bar set-up (from J.R. Hall)
	;
	;==================================================================
	; Set up progress bar.
	therm = widget_base( title = 'Reprojecting Data...', /column, group_leader = group_leader )
	progress = widget_draw( therm, xsize = 300, ysize = 25, retain = 2 )
	label = widget_label( therm, xsize = 300, value = '0%' )
	cancelB = widget_button( therm, value = 'cancel' )
	widget_control, therm, /realize
	widget_control, progress, get_value = win
	count = -1.0
	process = 0
	per = 0
	perPrevious = 0
	cancel_pressed = 0
	stop_showing = 0
	;==================================================================
	;
	; Progress bar set-up (from J.R. Hall)
	;
	;==================================================================
	
	progress_idx	= WHERE(OBJ_VALID(rgba_objarr) GT 0, progress_cnt )
	
	orig_unpadded_x_size	= 0
	orig_unpadded_y_size	= 0
	FOR band_ctr = 0, n_bands - 1 DO BEGIN
		georef_ctr		= 0
		good_img_encountered	= 0
		
		WHILE georef_ctr LT n_georef_img DO BEGIN
		
		
			;==================================================================
			;
			; Progress bar set-up (from J.R. Hall)
			;
			;==================================================================
			IF cancel_pressed then begin
				ans	= dialog_message(['Are you sure you want to cancel the reprojection process?'], /question )
				if strupcase(strtrim(ans,2)) EQ 'YES' THEN BEGIN
					OBJ_DESTROY, out_img_objarr
					IF widget_info(therm, /valid_id) then widget_control, therm, /destroy
					RETURN, { cancel_pressed : 1 }
				endif else begin
					cancel_pressed	= 0
				endelse
			ENDIF
			IF widget_info(therm, /valid_id) then begin
				; Update progress bar.
				count = count + OBJ_VALID( rgba_objarr[ band_ctr, georef_ctr ] )
				IF progress_cnt GT 0 THEN per = count / progress_cnt
				; Only update progress bar when a change is required.
				if per gt perPrevious then begin
					perPrevious = per
curwin	= !d.window
					if !d.name ne 'ps' then $
						wset, win
					polyfill, [ 0, per, per, 0 ], [ 0, 0, 1, 1 ], /normal, $
						color = 200
					if !d.name ne 'ps' then $
						wset, curwin
					widget_control, label, $
						set_value = strcompress( fix( per * 100 ) )+'%'
				endif
				quit = widget_event( cancelB, /nowait )
				if quit.id eq cancelB then  $
					cancel_pressed = 1
			ENDIF
			;==================================================================
			;
			; Progress bar set-up (from J.R. Hall)
			;
			;==================================================================
		
		
;print,'band_ctr,georef_ctr=',band_ctr,georef_ctr
			mval			= minmaxmissing.rgba_missing_val[band_ctr]
			add_mval_ptr		= PTR_NEW()
			
			;==================================================================
			; For now, the resulting IMAGE_DATA reprojected data will have
			; its missing data range (if any missing data exists) to whatever
			; setting is encountered in the FIRST valid IMAGE_DATA object.
			;==================================================================
			continuous_missing_data	= 0
			tcode			= minmaxmissing.rgba_data_type_code[band_ctr]
			mn			= minmaxmissing.rgba_global_min_val[band_ctr]
			mx			= minmaxmissing.rgba_global_max_val[band_ctr]
			out_proj		= get_empty_proj_img( window_dims, tcode, mval )
			IF OBJ_VALID( rgba_objarr[ band_ctr, georef_ctr ] ) THEN BEGIN
				add_mval_ptr		= (rgba_objarr[band_ctr,georef_ctr])->Return_Missing_Data_Ptr()
				continuous_missing_data	= (rgba_objarr[band_ctr,georef_ctr])->Continuous_Missing_Data_Vals_Exist()
				
				IF PTR_VALID( add_mval_ptr ) AND continuous_missing_data THEN mval = (*add_mval_ptr)[0]
				
;print,'create_projected_image_data:: Continuous_Missing_Data_Vals_Exist() ===== ', continuous_missing_data
;if ptr_valid(add_mval_ptr) then print,'added missing data values = ',*add_mval_ptr

				lon_o			= lon_objarr[(n_lonlat_img GT 1)*georef_ctr]
				lat_o			= lat_objarr[(n_lonlat_img GT 1)*georef_ctr]
				img_o			= rgba_objarr[band_ctr,georef_ctr]
				out_proj		= do_reprojection(								$
					(lon_o->ReturnUnbytescaledStackStructure(RESOLUTION=lon_o->GetTileSize())).unscaledStack,	$
					(lat_o->ReturnUnbytescaledStackStructure(RESOLUTION=lat_o->GetTileSize())).unscaledStack,	$
					(img_o->ReturnUnbytescaledStackStructure(RESOLUTION=img_o->GetTileSize())).unscaledStack,	$
					map_set_str,										$
					window_dims,										$
					fill_seams,										$
					FIX(fill_seam_parameters[0]),								$
					fill_seam_parameters[1],								$
					mn,											$
					mx,											$
					mval,											$
					tcode )
			ENDIF
					
			IF MAX(out_proj) GT mval THEN BEGIN
				IF NOT good_img_encountered THEN BEGIN
					mosaic_out_proj		= out_proj
					denom_img_rgb		= DBLARR( window_dims[0], window_dims[1] )
					numer_img_rgb		= DBLARR( window_dims[0], window_dims[1] )
					good_img_encountered	= 1
					IF PTR_VALID(add_mval_ptr) THEN BEGIN
						band_mval = [mval,*add_mval_ptr]
						is_continuous	= continuous_missing_data
					ENDIF ELSE BEGIN
						is_continuous	= continuous_missing_data
						band_mval	= mval
					ENDELSE
				ENDIF ELSE BEGIN
					idx1	= WHERE( (out_proj GE mn AND out_proj LE mx) AND (mosaic_out_proj LT mn OR mosaic_out_proj GT mx), cnt1 )
					IF cnt1 GT 0 THEN BEGIN
						IF resolve_overlap_idx NE 4 THEN BEGIN
							mosaic_out_proj[idx1]	= out_proj[idx1]
						ENDIF ELSE BEGIN
							numer_img_rgb[idx1]	= numer_img_rgb[idx1] + out_proj[idx1]
							denom_img_rgb[idx1]	= denom_img_rgb[idx1] + 1.0D
						ENDELSE
					ENDIF
					
					idx2	= WHERE( (out_proj GE mn AND out_proj LE mx) AND (mosaic_out_proj GE mn AND mosaic_out_proj LE mx), cnt2 )
					IF cnt2 GT 0 THEN BEGIN
						CASE resolve_overlap_idx OF
						
							;=======================
							; USE FIRST VALUE ENCOUNTERED
							;=======================
							0:
							
							
							;=======================
							; USE LAST VALUE ENCOUNTERED
							;=======================
							1: mosaic_out_proj[idx2] = out_proj[idx2]
							
							
							;=======================
							; USE HIGHEST DATA VALUE
							;=======================
							2: BEGIN
								idx3	= WHERE( out_proj[idx2] GT mosaic_out_proj[idx2], cnt3 )
								IF cnt3 GT 0 THEN BEGIN
									tmp				= mosaic_out_proj[idx2]
									tmp[idx3]			= (out_proj[idx2])[idx3]
									mosaic_out_proj[idx2]		= tmp
;;;ckt,may2001									(mosaic_out_proj[idx2])[idx3] = (out_proj[idx2])[idx3]
								ENDIF
								END
								
								
							;=======================
							; USE LOWEST DATA VALUE
							;=======================
							3: BEGIN
								idx3	= WHERE( out_proj[idx2] LT mosaic_out_proj[idx2], cnt3 )
;print,'help,(mosaic_out_proj[idx2])[idx3]'
;IF cnt3 GT 0 THEN help,(mosaic_out_proj[idx2])[idx3]
;print,'help,(out_proj[idx2])[idx3]'
;IF cnt3 GT 0 THEN help,(out_proj[idx2])[idx3]
								IF cnt3 GT 0 THEN BEGIN
									tmp				= mosaic_out_proj[idx2]
									tmp[idx3]			= (out_proj[idx2])[idx3]
									mosaic_out_proj[idx2]		= tmp
;;;ckt,may2001									(mosaic_out_proj[idx2])[idx3]	= (out_proj[idx2])[idx3]
								ENDIF
								END
								
								
							;=======================
							; USE AVERAGE
							;=======================
							4: BEGIN
								numer_img_rgb[ idx2 ]	= numer_img_rgb[ idx2 ] + out_proj[ idx2 ]
								denom_img_rgb[ idx2 ]	= denom_img_rgb[ idx2 ] + 1.0D
								END
								
								
							ELSE:
						ENDCASE
					ENDIF
				
				ENDELSE
			ENDIF
					
			georef_ctr	= georef_ctr + 1
			
			IF resolve_overlap_idx EQ 4 THEN BEGIN
				idx	= WHERE( denom_img_rgb GT 0.0D, cnt )
				IF cnt GT 0 THEN mosaic_out_proj[idx] = numer_img_rgb[idx] / denom_img_rgb[idx]
			ENDIF
			
		ENDWHILE
		IF good_img_encountered THEN BEGIN
			orig_unpadded_x_size		= (SIZE(mosaic_out_proj))[1]
			orig_unpadded_y_size		= (SIZE(mosaic_out_proj))[2]
			slice_stack_struct		= create_slice_stack( mosaic_out_proj, [SLICE_SIZE, SLICE_SIZE], MISSING_DATA_VALUES = band_mval )
			IF is_continuous THEN BEGIN
				sorted_missing_vals	= band_mval[ SORT( band_mval ) ]
				missing_range		= [ sorted_missing_vals[0], sorted_missing_vals[ N_ELEMENTS( sorted_missing_vals ) - 1 ] ]
				out_img_objarr[ band_ctr ]	= OBJ_NEW(								$
									'IMAGE_DATA',							$
										convert_number(slice_stack_struct.slice_stack,tcode),	$
										slice_stack_struct.x_offsets,				$
										slice_stack_struct.y_offsets,				$
										MISSING_DATA_VALUE_RANGE = missing_range,		$
										STACK_DIMENSION_IDX = 2 )
			ENDIF ELSE BEGIN
				out_img_objarr[ band_ctr ]	= OBJ_NEW(								$
									'IMAGE_DATA',							$
										convert_number(slice_stack_struct.slice_stack,tcode),	$
										slice_stack_struct.x_offsets,				$
										slice_stack_struct.y_offsets,				$
										MISSING_DATA_VALUES = band_mval,			$
										STACK_DIMENSION_IDX = 2 )
			ENDELSE
		ENDIF
	ENDFOR
	
	;==================================================================
	;
	; Progress bar set-up (from J.R. Hall)
	;
	;==================================================================
	; Destroy progress bar.
	IF widget_info(therm, /valid_id) then widget_control, therm, /destroy
	;==================================================================
	;
	; Progress bar set-up (from J.R. Hall)
	;
	;==================================================================
	IF orig_unpadded_x_size EQ 0 OR orig_unpadded_y_size EQ 0 THEN cancel = 1 ELSE cancel = 0
	RETURN, {											$
			rgba_image_data_objarr	: out_img_objarr[0:N_ELEMENTS(out_img_objarr)-3],	$
			lon_image_data_obj	: out_img_objarr[N_ELEMENTS(out_img_objarr)-2],		$
			lat_image_data_obj	: out_img_objarr[N_ELEMENTS(out_img_objarr)-1],		$
			cancel_pressed		: cancel,						$
			orig_unpadded_x_size	: orig_unpadded_x_size,					$
			orig_unpadded_y_size	: orig_unpadded_y_size					$
			 }
	
	RETURN, out_img_objarr
END
; create_projected_image_data
