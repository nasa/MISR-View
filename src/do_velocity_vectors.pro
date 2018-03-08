@GetCorrectFont.pro
@convert_pseudocolor2truecolor.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ velovect_subroutine @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO velovect_subroutine,		$
	length2use	= length2use,	$
	u2use		= u2use,	$
	v2use		= v2use,	$
	umin2use	= umin2use,	$
	umax2use	= umax2use,	$
	vmin2use	= vmin2use,	$
	vmax2use	= vmax2use,	$
	lon_idx		= lon_idx,	$
	lat_idx		= lat_idx,	$
	_EXTRA		= extra
;help,lon_idx
;help,lat_idx
;print,'========= lon_idx=',lon_idx
;print,'========= lat_idx=',lat_idx
	;--------------------------------------------------
	; VELOVECT trims the longer window dimension to 
	; equal the shorter dimension.  Account for this in 
	; the MAP_SET LIMIT.
	;--------------------------------------------------
	IF !d.x_size LT !d.y_size THEN BEGIN
		xlim		= 1.0
		ylim		= MIN( [FLOAT(!d.y_size)/!d.x_size,FLOAT(!d.x_size)/!d.y_size] )
	ENDIF ELSE BEGIN
		xlim		= MIN( [FLOAT(!d.y_size)/!d.x_size,FLOAT(!d.x_size)/!d.y_size] )
		ylim		= 1.0
	ENDELSE
;print,'xlim=',xlim
;print,'ylim=',ylim
	;--------------------------------------------------
	; Set up the plotting space for VELOVECT.
	;--------------------------------------------------
	MAP_SET,0.0,0.0,0.0,			$
		LIMIT=[0.0,0.0,xlim,ylim],	$
		/CYLINDRICAL,			$
		/ISOTROPIC,			$
		/NOBORDER,			$
		XMARGIN=0.0,YMARGIN=0.0,	$
		/NOERASE
		
	;--------------------------------------------------
	; Draw the arrows.
	;--------------------------------------------------
;print,'VELOVECT, LENGTH = ',length2use
;print,'VELOVECT, XRANGE = ',umin2use,umax2use
;print,'VELOVECT, YRANGE = ',vmin2use,vmax2use
;print,'u2use = '
;print,u2use
;print,'v2use = '
;print,v2use
	VELOVECT,				$
		u2use,				$
		v2use,				$
		lon_idx,			$
		lat_idx,			$
		/OVERPLOT,			$
		XRANGE = [umin2use,umax2use],	$
		YRANGE = [vmin2use,vmax2use],	$
		LENGTH = length2use,		$
		_EXTRA = extra
end
; velovect_subroutine

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ do_velocity_vectors_kill @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO do_velocity_vectors_kill, tlb
END
; do_velocity_vectors_kill

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ do_velocity_vectors_eh @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO do_velocity_vectors_eh, event
	WIDGET_CONTROL, event.id, GET_UVALUE = uval
	WIDGET_CONTROL, event.top, GET_UVALUE = info_ptr
	
	CASE STRLOWCASE(uval) OF
		'wd_u':		(*info_ptr).wd1_idx	= event.index
		'wd_v':		(*info_ptr).wd2_idx	= event.index
		'wd_color':	(*info_ptr).wd3_idx	= event.index
		'wd_thick':	(*info_ptr).wd4_idx	= event.index
		'wd_length':	(*info_ptr).wd5_idx	= event.index
		'reproject':	(*info_ptr).reproject	= event.select EQ 1
		'ok': BEGIN
			WIDGET_CONTROL, event.top, /DESTROY
			(*info_ptr).cancel_pressed	= 0
			END
		'cancel': BEGIN
			WIDGET_CONTROL, event.top, /DESTROY
			END
		ELSE:
	ENDCASE
END
; do_velocity_vectors_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ do_velocity_vectors @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO do_velocity_vectors, thisObj
	last_win		= !D.WINDOW
	font_type		= GetCorrectFont( 'courier2bold' )

	lon_img_data_obj_ptr	= thisObj->GetPtr2LonImg()
	lat_img_data_obj_ptr	= thisObj->GetPtr2LatImg()
	
	tlb	= WIDGET_BASE(							$
				GROUP_LEADER = thisObj->GetParentBase(),	$
				/MODAL,						$
				/COLUMN,					$
				/BASE_ALIGN_CENTER, 				$
				KILL_NOTIFY = 'do_velocity_vectors_kill',	$
				TITLE = 'Velocity Vector Setup' )
				
	msg1	= 'NOTE: Image Underneath Vectors Will'
	msg2	= 'Always Be All Displayed RGB Data Planes'
	lbl1	= WIDGET_LABEL( tlb, VALUE = msg1, /ALIGN_CENTER, FONT = font_type )
	lbl2	= WIDGET_LABEL( tlb, VALUE = msg2, /ALIGN_CENTER, FONT = font_type )
	wd_list1= [*(thisObj->Get_Data_Description_Ptr())]
	wd_list2= [*(thisObj->Get_Data_Description_Ptr())]
	
	colors2use	= [							$
				convert_pseudocolor2truecolor(255b,255b,255b),	$
				convert_pseudocolor2truecolor(255b,0b,0b),	$
				convert_pseudocolor2truecolor(255b,255b,0b) ]
	thick2use	= FINDGEN(6)/2.0+0.5
	len2use		= FINDGEN(5)/3.0+(1.0/3.0)
	
	wd_list3= ['WHITE','RED','YELLOW']
	wd_list4= STRMID(STRTRIM(FINDGEN(6)/2.0+0.5,2),0,3)
	wd_list5= ['SHORTEST', 'SHORT', 'MEDIUM', 'LONG', 'LONGEST']
	wd1		= WIDGET_DROPLIST(					$
				tlb, 						$
				UVALUE = 'wd_u',				$
				FONT = font_type,				$
				VALUE = wd_list1,				$
				TITLE = 'X-component (U):' )
	wd2		= WIDGET_DROPLIST(					$
				tlb, 						$
				FONT = font_type,				$
				UVALUE = 'wd_v',				$
				VALUE = wd_list2,				$
				TITLE = 'Y-component (V):' )
	wd3		= WIDGET_DROPLIST(					$
				tlb, 						$
				FONT = font_type,				$
				UVALUE = 'wd_color',				$
				VALUE = wd_list3,				$
				TITLE = 'Color To Draw Vectors:' )
	wd4		= WIDGET_DROPLIST(					$
				tlb, 						$
				FONT = font_type,				$
				UVALUE = 'wd_thick',				$
				VALUE = wd_list4,				$
				TITLE = 'Vector Line Thickness:', SENSITIVE = 0 )	; disable Line Thickness, doesn't seem to work.
	wd5		= WIDGET_DROPLIST(					$
				tlb, 						$
				FONT = font_type,				$
				UVALUE = 'wd_length',				$
				VALUE = wd_list5,				$
				TITLE = 'Relative Vector Line Length:' )	; disable Relative Length, so that length2use can be used.
	
	IF PTR_VALID(lon_img_data_obj_ptr) AND PTR_VALID(lat_img_data_obj_ptr) THEN BEGIN
		sub_base1	= WIDGET_BASE(						$
						tlb,					$
						/ROW,					$
						/BASE_ALIGN_CENTER,			$
						/FRAME,					$
						/NONEXCLUSIVE )
		project_btn	= WIDGET_BUTTON(					$
						sub_base1,				$
						FONT = font_type,			$
						VALUE = 'Reproject Image Data',		$
						UVALUE = 'reproject' )
		WIDGET_CONTROL, project_btn, SET_BUTTON = 0
	ENDIF
				
	sub_base2	= WIDGET_BASE( tlb, /ROW, /BASE_ALIGN_CENTER, /FRAME )
	ok		= WIDGET_BUTTON( sub_base2, VALUE = 'OK', UVALUE = 'ok', FONT = font_type )
	cancel		= WIDGET_BUTTON( sub_base2, VALUE = 'Cancel', UVALUE = 'cancel', FONT = font_type )
	
	WIDGET_CONTROL, tlb, DEFAULT_BUTTON = ok
	WIDGET_CONTROL, tlb, /REALIZE
	
	info_ptr	= PTR_NEW( {							$
					wd1_idx		:0,				$
					wd2_idx		:0,				$
					wd3_idx		:0,				$
					wd4_idx		:0,				$
					wd5_idx		:0,				$
					reproject	:0,				$
					cancel_pressed	:1				$
					}, /NO_COPY )
					
	WIDGET_CONTROL, tlb, SET_UVALUE = info_ptr
	
	XMANAGER, 'do_velocity_vectors', tlb, EVENT_HANDLER = 'do_velocity_vectors_eh'
	
	IF (*info_ptr).cancel_pressed THEN BEGIN
		PTR_FREE, info_ptr
		WSET, last_win
		RETURN
	ENDIF
	
	WIDGET_CONTROL, /HOURGLASS
	
	rgba_objarr	= OBJARR( 3 )

	rgba_objarr[0]		= thisObj->ReturnImageDataObj(RGB = 'RED')
	rgba_objarr[1]		= thisObj->ReturnImageDataObj(RGB = 'GREEN')
	rgba_objarr[2]		= thisObj->ReturnImageDataObj(RGB = 'BLUE')
	
	u_img_data_obj		= thisObj->ReturnImageDataObj(DATA_INDEX=(*info_ptr).wd1_idx)
	v_img_data_obj		= thisObj->ReturnImageDataObj(DATA_INDEX=(*info_ptr).wd2_idx)

	umin2use		= u_img_data_obj->GetMinVal(						$
								0,					$
								(u_img_data_obj->GetImageWidth())-1,	$
								0,					$
								(u_img_data_obj->GetImageHeight())-1,	$
								(u_img_data_obj->GetImageWidth()),	$
								(u_img_data_obj->GetImageHeight()) )
	umax2use		= u_img_data_obj->GetMaxVal(						$
								0,					$
								(u_img_data_obj->GetImageWidth())-1,	$
								0,					$
								(u_img_data_obj->GetImageHeight())-1,	$
								(u_img_data_obj->GetImageWidth()),	$
								(u_img_data_obj->GetImageHeight()) )
	vmin2use		= v_img_data_obj->GetMinVal(						$
								0,					$
								(u_img_data_obj->GetImageWidth())-1,	$
								0,					$
								(u_img_data_obj->GetImageHeight())-1,	$
								(u_img_data_obj->GetImageWidth()),	$
								(u_img_data_obj->GetImageHeight()) )
	vmax2use		= v_img_data_obj->GetMaxVal(						$
								0,					$
								(u_img_data_obj->GetImageWidth())-1,	$
								0,					$
								(u_img_data_obj->GetImageHeight())-1,	$
								(u_img_data_obj->GetImageWidth()),	$
								(u_img_data_obj->GetImageHeight()) )
	
	u_stack_struct		= u_img_data_obj->ReturnUnbytescaledStackStructure(/NATIVE_RES)
	v_stack_struct		= v_img_data_obj->ReturnUnbytescaledStackStructure(/NATIVE_RES)
	u			= u_stack_struct.unscaledStack
	v			= v_stack_struct.unscaledStack
	
;;;ckt,oct2001	u_tile_size		= u_img_data_obj->GetTileSize()
;;;ckt,oct2001	v_tile_size		= v_img_data_obj->GetTileSize()
	u_tile_size		= u_img_data_obj->GetTileSize(/NATIVE_RES)
	v_tile_size		= v_img_data_obj->GetTileSize(/NATIVE_RES)
	
	u_miss_ptr		= u_img_data_obj->Return_Missing_Data_Ptr()
	v_miss_ptr		= v_img_data_obj->Return_Missing_Data_Ptr()
	is_continuous_u		= u_img_data_obj->Continuous_Missing_Data_Vals_Exist()
	is_continuous_v		= v_img_data_obj->Continuous_Missing_Data_Vals_Exist()
;if ptr_valid(u_miss_ptr) then print,'u_miss_ptr=',*u_miss_ptr
;if ptr_valid(v_miss_ptr) then print,'v_miss_ptr=',*v_miss_ptr
	
	IF u_tile_size[0] NE v_tile_size[0] OR u_tile_size[1] NE v_tile_size[1] THEN BEGIN
		msg	= [								$
				'U and V components have differing dimensions...',	$
				'Please check...' ]
		res	= DIALOG_MESSAGE( msg, /ERROR )
		PTR_FREE, info_ptr
		WSET, last_win
		RETURN
	ENDIF
	
	uv_res			= [ u_tile_size[0], u_tile_size[1] ]
	
	good_idx			= WHERE( OBJ_VALID(rgba_objarr) GE 1, cnt )
	create_gray_scale_img	= TOTAL(OBJ_VALID(rgba_objarr)) LE 1
	res_w			= 0
	res_h			= 0
	FOR i = 0, 2 DO BEGIN
		IF OBJ_VALID(rgba_objarr[i]) THEN BEGIN
			tile_dims	= (rgba_objarr[i])->GetTileSize()
			res_w		= MAX([res_w,tile_dims[0]])
			res_h		= MAX([res_h,tile_dims[1]])
		ENDIF
	ENDFOR
	
	res2use			= [res_w,res_h]
	
	IF uv_res[0] GT res2use[0] OR uv_res[1] GT res2use[1] THEN BEGIN
		msg	= [								$
				'U and V components have larger dimensions',		$
				'than underlying imagery.' ]
		res	= DIALOG_MESSAGE( msg, /ERROR )
		PTR_FREE, info_ptr
		WSET, last_win
		RETURN
	ENDIF
	
	IF													$
		FLOAT(res2use[0])/FLOAT(uv_res[0]) NE FLOAT(FIX(FLOAT(res2use[0])/FLOAT(uv_res[0]))) OR		$
		FLOAT(res2use[1])/FLOAT(uv_res[1]) NE FLOAT(FIX(FLOAT(res2use[1])/FLOAT(uv_res[1]))) AND	$
		NOT (*info_ptr).reproject THEN BEGIN
		msg	= [								$
				'U and V components need to have dimensions',		$
				'which divide evenly into the corresponding',		$
				'dimensions of the underlying imagery.' ]
		res	= DIALOG_MESSAGE( msg, /ERROR )
		PTR_FREE, info_ptr
		WSET, last_win
		RETURN
	ENDIF
	
	IF uv_res[0] EQ res2use[0] OR uv_res[1] EQ res2use[1] THEN BEGIN
		msg	= [								$
				'Generally speaking, there is a sparse number',		$
				'of vectors to be mapped when compared to the',		$
				'number of pixels within the underlying imagery.',	$
				'Currently, the U and V parameters have a 1:1',		$
				'correspondence with the imagery.   Should',		$
				'processing continue?' ]
		res	= DIALOG_MESSAGE( msg, /QUESTION )
		IF STRUPCASE(STRTRIM(res,2)) EQ 'NO' THEN BEGIN
			PTR_FREE, info_ptr
			WSET, last_win
			RETURN
		ENDIF
	ENDIF
	
	vec_color2use	= colors2use[(*info_ptr).wd3_idx]
	
	extra_struct1	= {							$
				COLOR	: vec_color2use,			$
				DOTS	: 0,					$
;;;;;;;				LENGTH	: len2use[(*info_ptr).wd5_idx],		$	; Use length2use, instead.  JRH Sep 3, 2001.
				THICK	: thick2use[(*info_ptr).wd4_idx] }
	
	IF (*info_ptr).reproject THEN BEGIN
		first_time_through	= 1
		ret_ptr		= reprojection_tool([*lon_img_data_obj_ptr],[*lat_img_data_obj_ptr],rgba_objarr,GROUP_LEADER =thisObj->GetParentBase() )
		ret_struct	= *ret_ptr
		PTR_FREE, ret_ptr
		IF ret_struct.cancel_pressed THEN BEGIN
			PTR_FREE, info_ptr
			RETURN
		ENDIF
		FOR i = 0, 2 DO BEGIN
			IF OBJ_VALID(ret_struct.rgba_image_data_objarr[i]) THEN BEGIN
				scaling_params	= (rgba_objarr[i])->ReturnScalingParameters()
				iw	= (ret_struct.rgba_image_data_objarr[i])->GetImageWidth()
				ih	= (ret_struct.rgba_image_data_objarr[i])->GetImageHeight()
				unscaled_img	= (ret_struct.rgba_image_data_objarr[i])->ReturnUnbytescaledImage(		$
								0,								$
								(ret_struct.rgba_image_data_objarr[i])->GetImageWidth()-1,	$
								0,								$
								(ret_struct.rgba_image_data_objarr[i])->GetImageHeight()-1,	$
								(ret_struct.rgba_image_data_objarr[i])->GetImageWidth(),	$
								(ret_struct.rgba_image_data_objarr[i])->GetImageHeight(),	$
								DATA_TYPE = (ret_struct.rgba_image_data_objarr[i])->GetNumberType() )
				exponent	= 1.0D
				IF scaling_params.gamma GT 0 THEN	$
						exponent	= 1.0D / scaling_params.gamma
	
				spread		=						$
					scaling_params.max_val_to_byte - scaling_params.min_val_to_byte
		
				IF spread EQ 0 THEN spread = 1
				
				o	= rgba_objarr[i]
	
				below_mask	= BYTE(unscaled_img)*0B+1B
				tmpidx1		= WHERE( unscaled_img LT scaling_params.min_val_to_byte, tmpcnt1)
				IF tmpcnt1 GT 0 THEN below_mask[tmpidx1] = 0B
				above_mask	= BYTE(unscaled_img)*0B+1B
				tmpidx1		= WHERE( unscaled_img GT scaling_params.max_val_to_byte, tmpcnt1)
				IF tmpcnt1 GT 0 THEN above_mask[tmpidx1] = 0B
				mask255		= NOT( above_mask ) * 255B
				miss_mask	= BYTE(unscaled_img)*0B+1B
				miss_ptr	= o->Return_Missing_Data_Ptr()
				is_continuous2	= o->Continuous_Missing_Data_Vals_Exist()
				IF PTR_VALID(miss_ptr) THEN BEGIN
					IF is_continuous2 THEN BEGIN
						tmpidx1	= WHERE( unscaled_img LT (*miss_ptr)[0] OR unscaled_img GT (*miss_ptr)[1] )
						IF tmpidx1[0] GE 0 THEN miss_mask[tmpidx1] = 0B
					ENDIF ELSE BEGIN
						tmpidx1	= where2( unscaled_img, *miss_ptr )
						IF tmpidx1[0] GE 0 THEN miss_mask[tmpidx1] = 0B
					ENDELSE
				ENDIF
	
				scaled_img_with_padding = 														$
					( BYTSCL( (((unscaled_img-scaling_params.min_val_to_byte)/spread)*below_mask) ^ exponent * spread +				$
						scaling_params.min_val_to_byte,												$
						MIN = scaling_params.min_val_to_byte,											$
						MAX = scaling_params.max_val_to_byte ) *										$
						above_mask + mask255 ) *												$
						miss_mask
						
				scaled_img	= scaled_img_with_padding[0:ret_struct.orig_unpadded_x_size-1,0:ret_struct.orig_unpadded_y_size-1]
				iw		= (SIZE(scaled_img))[1]
				ih		= (SIZE(scaled_img))[2]
				
				IF first_time_through THEN BEGIN
					ss	= GET_SCREEN_SIZE()
					xvis	= MAX( [ 64, MIN( [ ss[0]/2, iw ] )-32 ] )
					yvis	= MAX( [ 64, MIN( [ ss[1]/2, ih ] )-32 ] )

					;---------------------------------------------------------------------------
					; special version of SLIDE_IMAGE modified for misr_view 4.0 2001 (IDL 5.4).
					;---------------------------------------------------------------------------
					SLIDE_IMAGE2,									$
						SLIDE_WINDOW = slide_win,						$
						GROUP = thisObj->GetParentBase(),					$
						RETAIN = 2,								$
						SHOW_FULL = 0,								$
						XSIZE = iw,								$
						YSIZE = ih,								$
						XVISIBLE = xvis,							$
						YVISIBLE = yvis,							$
						TITLE = 'Velocity Vector Overlay Result',				$
						/REGISTER,								$
						N_BANDS = TOTAL(OBJ_VALID(rgba_objarr)),				$
						VEC_COLOR = vec_color2use,						$
						VEC_INDEX = 0
							
					first_time_through	= 0
					WSET, slide_win
					
					
					
					
map_set_str	= ret_struct.map_set_string				
sep	= STR_SEP(map_set_str,',')
p1	= FLOAT(sep[1])
p2	= FLOAT(sep[2])
p3	= FLOAT(sep[3])
FOR p = 4, N_ELEMENTS(sep)-1 DO BEGIN
	str	= STRTRIM(sep[p],2)
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
		keyval	= FLOAT(STRTRIM(sep2[1],2))
		IF p EQ 4 THEN BEGIN
			extra_struct	= CREATE_STRUCT(keywd,keyval)
		ENDIF ELSE BEGIN
			extra_struct	= CREATE_STRUCT(keywd,keyval,extra_struct)
		ENDELSE
	ENDELSE
ENDFOR
CALL_PROCEDURE,'MAP_SET',p1,p2,p3,_Extra=extra_struct
;;;ckt,aug2004					success	= EXECUTE(ret_struct.map_set_string)



;print,'iw,ih=',iw,ih
;print,'map_set_string = ',ret_struct.map_set_string
				ENDIF
				
				IF create_gray_scale_img THEN BEGIN
					IF !D.N_COLORS GT 256 THEN BEGIN
						TV, scaled_img, CHANNEL = 1
						TV, scaled_img, CHANNEL = 2
						TV, scaled_img, CHANNEL = 3
					ENDIF ELSE BEGIN
						top_idx	= WHERE( scaled_img GE 255, top_cnt )
						IF top_cnt GT 0 THEN scaled_img[top_idx] = 254B
						scaled_img	= scaled_img + 1B
						TV, scaled_img
					ENDELSE
				ENDIF ELSE BEGIN
					TV, scaled_img, CHANNEL = i+1
				ENDELSE
				
				lon_stack	= (*lon_img_data_obj_ptr)->ReturnUnbytescaledStackStructure()
				lat_stack	= (*lat_img_data_obj_ptr)->ReturnUnbytescaledStackStructure()
				res2use		= (*lat_img_data_obj_ptr)->GetTileSize()
				
				p1		= uv_res[0]
				p2		= res2use[0] / uv_res[0]
				p3		= (res2use[0] / uv_res[0]) / 2
				lon_idx		= INDGEN(p1)*p2+p3
				p1		= uv_res[1]
				p2		= res2use[1] / uv_res[1]
				p3		= (res2use[1] / uv_res[1]) / 2
				lat_idx		= INDGEN(p1)*p2+p3
				
				n_tiles		= (rgba_objarr[good_idx[0]])->Return_Number_Of_Tiles()
				
				;===========================================================================
				; VELOVECT REQUIRES THAT MISSING VALUES BE GREATER THAN ALL VALID VALUES,
				; SO IF MISSING DATA VALUES ARE LESS THAN VALID DATA, THE MISSING VALUES
				; NEED TO BE CONVERTED TO VALUES GREATER THAN THE MAXIMUM VALID VALUE.
				; THE DEFAULT MISSING VALUE THAT WILL BE USED IS THE MAXIMUM OF THE
				; DEFAULT "U" MISSING DATA VALUE AND THE DEFAULT "V" MISSING DATA VALUE
				;===========================================================================
				IF PTR_VALID(u_miss_ptr) THEN BEGIN
					tmpidx	= where2( u, *u_miss_ptr, /INVERSE )
					IF tmpidx[0] NE -1 THEN BEGIN
						tmpidx2	= WHERE( *u_miss_ptr GT MAX(ABS(u[tmpidx])), tmpcnt2 )
						IF tmpcnt2 GT 0 THEN BEGIN
							u_default_missing_val2use	= (*u_miss_ptr)[tmpidx2[0]]
						ENDIF ELSE BEGIN
							u_default_missing_val2use	= MAX(ABS(u[tmpidx])) + 1.0
						ENDELSE
					ENDIF ELSE BEGIN
						u_default_missing_val2use	= MAX(ABS(*u_miss_ptr))
					ENDELSE
				ENDIF ELSE BEGIN
						u_default_missing_val2use	= MAX(ABS(u)) + 1.0
				ENDELSE
				
				IF PTR_VALID(v_miss_ptr) THEN BEGIN
					tmpidx	= where2( v, *v_miss_ptr, /INVERSE )
					IF tmpidx[0] NE -1 THEN BEGIN
						tmpidx2	= WHERE( *v_miss_ptr GT MAX(ABS(v[tmpidx])), tmpcnt2 )
						IF tmpcnt2 GT 0 THEN BEGIN
							v_default_missing_val2use	= (*v_miss_ptr)[tmpidx2[0]]
						ENDIF ELSE BEGIN
							v_default_missing_val2use	= MAX(ABS(v[tmpidx])) + 1.0
						ENDELSE
					ENDIF ELSE BEGIN
						v_default_missing_val2use	= MAX(ABS(*v_miss_ptr))
					ENDELSE
				ENDIF ELSE BEGIN
						v_default_missing_val2use	= MAX(ABS(v)) + 1.0
				ENDELSE
				
				default_missing_value2use	= MAX([u_default_missing_val2use,v_default_missing_val2use])
				
				
				;===================================================
				; Do this loop to determine global max_length such 
				; that missing data values are not considered.
				; Then, run it again (below) for actual processsing.
				;===================================================
				max_length	= DBLARR( n_tiles )
				FOR j = 0, n_tiles - 1 DO BEGIN
;help,lon_stack
					cur_lon_tile	= REFORM(lon_stack.unscaledStack[j,*,*])
					cur_lat_tile	= REFORM(lat_stack.unscaledStack[j,*,*])
					lons2use	= FLTARR(uv_res[0],uv_res[1])
					lats2use	= FLTARR(uv_res[0],uv_res[1])
					FOR k = 0, uv_res[0] - 1 DO BEGIN
						FOR l = 0, uv_res[1] - 1 DO BEGIN
							lons2use[k,l]	= cur_lon_tile[ lon_idx[k], lat_idx[l] ]
							lats2use[k,l]	= cur_lat_tile[ lon_idx[k], lat_idx[l] ]
						ENDFOR
					ENDFOR
					
					u2use = REFORM(u[j,*,*])*0+default_missing_value2use
					v2use = REFORM(v[j,*,*])*0+default_missing_value2use
;;;ckt,apr2001					IF PTR_VALID(u_miss_ptr) THEN u2use = REFORM(u[j,*,*])*0+(*u_miss_ptr)[N_ELEMENTS(*u_miss_ptr)-1]	$
;;;ckt,apr2001					ELSE u2use = REFORM(u[j,*,*])*0+9999
;;;ckt,apr2001					IF PTR_VALID(v_miss_ptr) THEN v2use = REFORM(v[j,*,*])*0+(*v_miss_ptr)[N_ELEMENTS(*v_miss_ptr)-1]	$
;;;ckt,apr2001					ELSE v2use = REFORM(v[j,*,*])*0+9999
					missval	= SQRT( (MAX(u2use))^2 + (MAX(u2use))^2 )
					;===========================================================================
					; VELOVECT REQUIRES THAT MISSING VALUES BE GREATER THAN ALL VALID VALUES,
					; SO IF MISSING DATA VALUES ARE LESS THAN VALID DATA, THE MISSING VALUES
					; NEED TO BE CONVERTED TO VALUES GREATER THAN THE MAXIMUM VALID VALUE.
					;===========================================================================
					tmp_struct	= CREATE_STRUCT( extra_struct1, 'MISSING', missval )
					tmpu	= REFORM(u[j,*,*])
					tmpv	= REFORM(v[j,*,*])
					tmpidx1	= LINDGEN(uv_res[0],uv_res[1])
					
					IF PTR_VALID(u_miss_ptr) THEN BEGIN
						IF is_continuous_u THEN BEGIN
							tmpidx1	= WHERE( tmpu LT (*u_miss_ptr)[0] OR tmpu GT (*u_miss_ptr)[1] )
						ENDIF ELSE BEGIN
							tmpidx1	= where2( tmpu, *u_miss_ptr, /INVERSE )
						ENDELSE
					ENDIF
					
					tmpidx2	= LINDGEN(uv_res[0],uv_res[1])
					
					IF PTR_VALID(v_miss_ptr) THEN BEGIN
						IF is_continuous_v THEN BEGIN
							tmpidx2	= WHERE( tmpv LT (*v_miss_ptr)[0] OR tmpv GT (*v_miss_ptr)[1] )
						ENDIF ELSE BEGIN
							tmpidx2	= where2( tmpv, *v_miss_ptr, /INVERSE )
						ENDELSE
					ENDIF
					
					tmpidx3	= where2( tmpidx1, tmpidx2 )
					IF tmpidx3[0] NE -1 THEN BEGIN
						u2use[tmpidx1[tmpidx3]]		= tmpu[tmpidx1[tmpidx3]]
						v2use[tmpidx1[tmpidx3]]		= tmpv[tmpidx1[tmpidx3]]
					ENDIF
					
					nx	= (size(u2use))[1]
					ny	= (size(u2use))[2]
					n_tot	= nx*ny
					u2	= fltarr(n_tot,n_tot) 
					v2	= fltarr(n_tot,n_tot)
					idx	= lindgen(nx*ny)*(nx*ny+1)
;print,'u2use = ',u2use
;print,'v2use = ',v2use
;print,'lons2use = ',lons2use
;print,'lats2use = ',lats2use
					u2[idx]	= u2use
					v2[idx]	= v2use
					idx	= where(u2 le -9999.0,cnt)
					lon2	= lons2use[lindgen(nx*ny)]
					lat2	= lats2use[lindgen(nx*ny)]

					;===================================================
					; 42.3107 always seems to be the max value.  
					; Make an assumption that it is an anomolous value, 
					; and disregard that value.
					;===================================================
					max_length[i]	=	MAX(								$
								SQRT(								$
								ABS( (u2use[WHERE( u2use LT 42.3106 OR u2use GT 42.3108 )])^2 ) +	$
								ABS( (v2use[WHERE( v2use LT 42.3106 OR v2use GT 42.3108 )])^2 ) ) )
				ENDFOR
				max_length	= MAX( max_length )

				;===================================================
				; Now that global max_length is calculated, run the 
				; loop again for real processing.
				;===================================================
				FOR j = 0, n_tiles - 1 DO BEGIN
;help,lon_stack
					cur_lon_tile	= REFORM(lon_stack.unscaledStack[j,*,*])
					cur_lat_tile	= REFORM(lat_stack.unscaledStack[j,*,*])
					lons2use	= FLTARR(uv_res[0],uv_res[1])
					lats2use	= FLTARR(uv_res[0],uv_res[1])
					FOR k = 0, uv_res[0] - 1 DO BEGIN
						FOR l = 0, uv_res[1] - 1 DO BEGIN
							lons2use[k,l]	= cur_lon_tile[ lon_idx[k], lat_idx[l] ]
							lats2use[k,l]	= cur_lat_tile[ lon_idx[k], lat_idx[l] ]
						ENDFOR
					ENDFOR
					
					u2use = REFORM(u[j,*,*])*0+default_missing_value2use
					v2use = REFORM(v[j,*,*])*0+default_missing_value2use
;;;ckt,apr2001					IF PTR_VALID(u_miss_ptr) THEN u2use = REFORM(u[j,*,*])*0+(*u_miss_ptr)[N_ELEMENTS(*u_miss_ptr)-1]	$
;;;ckt,apr2001					ELSE u2use = REFORM(u[j,*,*])*0+9999
;;;ckt,apr2001					IF PTR_VALID(v_miss_ptr) THEN v2use = REFORM(v[j,*,*])*0+(*v_miss_ptr)[N_ELEMENTS(*v_miss_ptr)-1]	$
;;;ckt,apr2001					ELSE v2use = REFORM(v[j,*,*])*0+9999
					missval	= SQRT( (MAX(u2use))^2 + (MAX(u2use))^2 )
					;===========================================================================
					; VELOVECT REQUIRES THAT MISSING VALUES BE GREATER THAN ALL VALID VALUES,
					; SO IF MISSING DATA VALUES ARE LESS THAN VALID DATA, THE MISSING VALUES
					; NEED TO BE CONVERTED TO VALUES GREATER THAN THE MAXIMUM VALID VALUE.
					;===========================================================================
					tmp_struct	= CREATE_STRUCT( extra_struct1, 'MISSING', missval )
					tmpu	= REFORM(u[j,*,*])
					tmpv	= REFORM(v[j,*,*])
					tmpidx1	= LINDGEN(uv_res[0],uv_res[1])
					
					IF PTR_VALID(u_miss_ptr) THEN BEGIN
						IF is_continuous_u THEN BEGIN
							tmpidx1	= WHERE( tmpu LT (*u_miss_ptr)[0] OR tmpu GT (*u_miss_ptr)[1] )
						ENDIF ELSE BEGIN
							tmpidx1	= where2( tmpu, *u_miss_ptr, /INVERSE )
						ENDELSE
					ENDIF
					
					tmpidx2	= LINDGEN(uv_res[0],uv_res[1])
					
					IF PTR_VALID(v_miss_ptr) THEN BEGIN
						IF is_continuous_v THEN BEGIN
							tmpidx2	= WHERE( tmpv LT (*v_miss_ptr)[0] OR tmpv GT (*v_miss_ptr)[1] )
						ENDIF ELSE BEGIN
							tmpidx2	= where2( tmpv, *v_miss_ptr, /INVERSE )
						ENDELSE
					ENDIF
					
					tmpidx3	= where2( tmpidx1, tmpidx2 )
					IF tmpidx3[0] NE -1 THEN BEGIN
						u2use[tmpidx1[tmpidx3]]		= tmpu[tmpidx1[tmpidx3]]
						v2use[tmpidx1[tmpidx3]]		= tmpv[tmpidx1[tmpidx3]]
					ENDIF
					
					nx	= (size(u2use))[1]
					ny	= (size(u2use))[2]
					n_tot	= nx*ny
					u2	= fltarr(n_tot,n_tot) 
					v2	= fltarr(n_tot,n_tot)
					idx	= lindgen(nx*ny)*(nx*ny+1)
;print,'u2use = ',u2use
;print,'v2use = ',v2use
;print,'lons2use = ',lons2use
;print,'lats2use = ',lats2use
					u2[idx]	= u2use
					v2[idx]	= v2use
					idx	= where(u2 le -9999.0,cnt)
					lon2	= lons2use[lindgen(nx*ny)]
					lat2	= lats2use[lindgen(nx*ny)]
;print,'min(lat)=',min(lat)
;print,'min(lon)=',min(lon)
;print,'max(lat)=',max(lat)
;print,'max(lon)]=',max(lon)

	
;	help,u2
;	help,v2
;	help,lon2
;	help,lat2
;	
;print,'======================================================'
;print,'u2 = '
;print,u2
;print,'======================================================'
;print,'v2 = '
;print,v2
;print,'======================================================'
;print,'lon2 = '
;print,lon2
;print,'======================================================'
;print,'lat2 = '
;print,lat2
	
					length2use = MAX( SQRT(							$
							ABS( (u2[WHERE( u2 LT 42.3106 OR u2 GT 42.3108 )])^2 ) +	$
							ABS( (v2[WHERE( v2 LT 42.3106 OR v2 GT 42.3108 )])^2 ) ) ) / max_length

;print,'VELOVECT, LENGTH = ',length2use
					; After being map projected arrows are very short, so multiply by 5.
					length2use = length2use * 5

;print,'VELOVECT, LENGTH = ',length2use
					; Scale by user-selected length ('SHORTEST', etc.)
					length2use = length2use * len2use[(*info_ptr).wd5_idx]
;print,'VELOVECT, LENGTH = ',length2use
					VELOVECT,			$
						u2,			$
						v2,			$
						lon2,			$
						lat2,			$
						/OVERPLOT,		$
						LENGTH = length2use,	$
						_Extra = tmp_struct
					
				ENDFOR
		
			ENDIF
		ENDFOR
	ENDIF ELSE BEGIN
		p1		= uv_res[0]
		p2		= res2use[0] / uv_res[0]
		p3		= (res2use[0] / uv_res[0]) / 2
		lon_idx		= INDGEN(p1)*p2+p3
		p1		= uv_res[1]
		p2		= res2use[1] / uv_res[1]
		p3		= (res2use[1] / uv_res[1]) / 2
		lat_idx		= INDGEN(p1)*p2+p3
;print,'lon_idx=',lon_idx
;print,'lat_idx=',lat_idx
		n_tiles		= (rgba_objarr[good_idx[0]])->Return_Number_Of_Tiles()
		first_time_through	= 1
		FOR j = 0, 2 DO BEGIN
			IF OBJ_VALID(rgba_objarr[j]) THEN BEGIN
;print,'--------------------==================================>>>>>>>>>>>>> res2use=',res2use
				img_stack_struct	= (rgba_objarr[j])->ReturnByteScaledStackStructure(RESOLUTION=res2use)
				blocks			= img_stack_struct.scaledStack
				xoffset			= img_stack_struct.h_offsets
				yoffset			= img_stack_struct.v_offsets
				IF first_time_through THEN BEGIN
					ss	= GET_SCREEN_SIZE()
					xvis	= MAX( [ 64, MIN( [ ss[0]/2,xoffset[(WHERE(xoffset EQ MAX(xoffset)))[0]]+res2use[0]] )-32 ] )
					yvis	= MAX( [ 64, MIN( [ ss[1]/2,yoffset[(WHERE(yoffset EQ MAX(yoffset)))[0]]+res2use[1]] )-32 ] )

					;---------------------------------------------------------------------------
					; special version of SLIDE_IMAGE modified for misr_view 4.0 2001 (IDL 5.4).
					;---------------------------------------------------------------------------
					SLIDE_IMAGE2,									$
						SLIDE_WINDOW = slide_win,						$
						GROUP = thisObj->GetParentBase(),					$
						RETAIN = 2,								$
						SHOW_FULL = 0,								$
						XSIZE = xoffset[(WHERE(xoffset EQ MAX(xoffset)))[0]]+res2use[0],	$
						YSIZE = yoffset[(WHERE(yoffset EQ MAX(yoffset)))[0]]+res2use[1],	$
						XVISIBLE = xvis,							$
						YVISIBLE = yvis,							$
						TITLE = 'Velocity Vector Overlay Result',				$
						/REGISTER,								$
						N_BANDS = TOTAL(OBJ_VALID(rgba_objarr)),				$
						VEC_COLOR = vec_color2use,						$
						VEC_INDEX = 0
							
;					WINDOW,											$
;						XSIZE = xoffset[(WHERE(xoffset EQ MAX(xoffset)))[0]]+res2use[0],		$
;						YSIZE = yoffset[(WHERE(yoffset EQ MAX(yoffset)))[0]]+res2use[1],		$
;						/FREE,										$
;						/PIXMAP
					first_time_through	= 0
;					pixmap_win		= !D.WINDOW
					WSET, slide_win
				ENDIF
				;--------------------------------------------------
				; TV all the blocks first so that the arrows may 
				; protrude beyond the block boundry without being 
				; partially erased by the next block being TV'ed.
				; Then draw all the arrows in a second FOR loop.
				;--------------------------------------------------
				IF create_gray_scale_img THEN BEGIN
					IF !D.N_COLORS GT 256 THEN BEGIN
						FOR i=0,n_tiles-1 DO TV, REFORM(blocks[i,*,*]), xoffset[i], yoffset[i], CHANNEL = 1
						FOR i=0,n_tiles-1 DO TV, REFORM(blocks[i,*,*]), xoffset[i], yoffset[i], CHANNEL = 2
						FOR i=0,n_tiles-1 DO TV, REFORM(blocks[i,*,*]), xoffset[i], yoffset[i], CHANNEL = 3
					ENDIF ELSE BEGIN
						FOR i=0,n_tiles-1 DO BEGIN
							scaled_img	= REFORM(blocks[i,*,*])
							top_idx	= WHERE( scaled_img GE 255, top_cnt )
							IF top_cnt GT 0 THEN scaled_img[top_idx] = 254B
							scaled_img	= scaled_img + 1B
							TV, scaled_img, xoffset[i], yoffset[i]
						ENDFOR
					ENDELSE
				ENDIF ELSE BEGIN
;print,'xoffset=',xoffset
;print,'yoffset=',yoffset
					FOR i=0,n_tiles-1 DO TV, REFORM(blocks[i,*,*]), xoffset[i], yoffset[i], CHANNEL = j+1
				ENDELSE

				;===========================================================================
				; VELOVECT REQUIRES THAT MISSING VALUES BE GREATER THAN ALL VALID VALUES,
				; SO IF MISSING DATA VALUES ARE LESS THAN VALID DATA, THE MISSING VALUES
				; NEED TO BE CONVERTED TO VALUES GREATER THAN THE MAXIMUM VALID VALUE.
				; THE DEFAULT MISSING VALUE THAT WILL BE USED IS THE MAXIMUM OF THE
				; DEFAULT "U" MISSING DATA VALUE AND THE DEFAULT "V" MISSING DATA VALUE
				;===========================================================================
				IF PTR_VALID(u_miss_ptr) THEN BEGIN
					tmpidx	= where2( u, *u_miss_ptr, /INVERSE )
					IF tmpidx[0] NE -1 THEN BEGIN
						tmpidx2	= WHERE( *u_miss_ptr GT MAX(ABS(u[tmpidx])), tmpcnt2 )
						IF tmpcnt2 GT 0 THEN BEGIN
							u_default_missing_val2use	= (*u_miss_ptr)[tmpidx2[0]]
						ENDIF ELSE BEGIN
							u_default_missing_val2use	= MAX(ABS(u[tmpidx])) + 1.0
						ENDELSE
					ENDIF ELSE BEGIN
						u_default_missing_val2use	= MAX(ABS(*u_miss_ptr))
					ENDELSE
				ENDIF ELSE BEGIN
						u_default_missing_val2use	= MAX(ABS(u)) + 1.0
				ENDELSE
				
				IF PTR_VALID(v_miss_ptr) THEN BEGIN
					tmpidx	= where2( v, *v_miss_ptr, /INVERSE )
					IF tmpidx[0] NE -1 THEN BEGIN
						tmpidx2	= WHERE( *v_miss_ptr GT MAX(ABS(v[tmpidx])), tmpcnt2 )
						IF tmpcnt2 GT 0 THEN BEGIN
							v_default_missing_val2use	= (*v_miss_ptr)[tmpidx2[0]]
						ENDIF ELSE BEGIN
							v_default_missing_val2use	= MAX(ABS(v[tmpidx])) + 1.0
						ENDELSE
					ENDIF ELSE BEGIN
						v_default_missing_val2use	= MAX(ABS(*v_miss_ptr))
					ENDELSE
				ENDIF ELSE BEGIN
						v_default_missing_val2use	= MAX(ABS(v)) + 1.0
				ENDELSE
				
				default_missing_value2use	= MAX([u_default_missing_val2use,v_default_missing_val2use])

				;===================================================
				; Do this loop to determine global max_length such 
				; that missing data values are not considered.
				; Then, run it again (below) for actual processsing.
				;===================================================
				max_length	= DBLARR( n_tiles )
;print,'n_tiles = ',n_tiles
;help,max_length
				FOR i=0,n_tiles-1 DO BEGIN

					u2use = REFORM(u[i,*,*])*0+default_missing_value2use
					v2use = REFORM(v[i,*,*])*0+default_missing_value2use
;;;ckt,apr2001					IF PTR_VALID(u_miss_ptr) THEN u2use = REFORM(u[i,*,*])*0+(*u_miss_ptr)[N_ELEMENTS(*u_miss_ptr)-1]	$
;;;ckt,apr2001					ELSE u2use = REFORM(u[i,*,*])*0+9999
;;;ckt,apr2001					IF PTR_VALID(v_miss_ptr) THEN v2use = REFORM(v[i,*,*])*0+(*v_miss_ptr)[N_ELEMENTS(*v_miss_ptr)-1]	$
;;;ckt,apr2001					ELSE v2use = REFORM(v[i,*,*])*0+9999
					missval	= SQRT( (MAX(u2use))^2 + (MAX(u2use))^2 )
					tmp_struct	= CREATE_STRUCT( extra_struct1, 'MISSING', missval )
					tmpu	= REFORM(u[i,*,*])
					tmpv	= REFORM(v[i,*,*])
					lonidx2use	= ( lon_idx + xoffset[i] ) / FLOAT( MAX( [!d.x_size,!d.y_size] ) )
					latidx2use	= ( lat_idx + yoffset[i] ) / FLOAT( MAX( [!d.x_size,!d.y_size] ) )
					tmpidx1	= LINDGEN(uv_res[0],uv_res[1])
					IF PTR_VALID(u_miss_ptr) THEN BEGIN
						IF is_continuous_u THEN BEGIN
							tmpidx1	= WHERE( tmpu LT (*u_miss_ptr)[0] OR tmpu GT (*u_miss_ptr)[1] )
						ENDIF ELSE BEGIN
							tmpidx1	= where2( tmpu, *u_miss_ptr, /INVERSE )
						ENDELSE
					ENDIF
					tmpidx2	= LINDGEN(uv_res[0],uv_res[1])
					IF PTR_VALID(v_miss_ptr) THEN BEGIN
						IF is_continuous_v THEN BEGIN
							tmpidx2	= WHERE( tmpv LT (*v_miss_ptr)[0] OR tmpv GT (*v_miss_ptr)[1] )
						ENDIF ELSE BEGIN
							tmpidx2	= where2( tmpv, *v_miss_ptr, /INVERSE )
						ENDELSE
					ENDIF
					tmpidx3	= where2( tmpidx1, tmpidx2 )
					IF tmpidx3[0] NE -1 THEN BEGIN
						u2use[tmpidx1[tmpidx3]]		= tmpu[tmpidx1[tmpidx3]]
						v2use[tmpidx1[tmpidx3]]		= tmpv[tmpidx1[tmpidx3]]
					ENDIF

					;===================================================
					; 42.3107 always seems to be the max value.  
					; Make an assumption that it is an anomolous value, 
					; and disregard that value.
					;===================================================
					max_length[i]	=	MAX( SQRT(							$
								u2use[WHERE( u2use LT 42.3106 OR u2use GT 42.3108 )]^2 +	$
								v2use[WHERE( v2use LT 42.3106 OR v2use GT 42.3108 )]^2 ) )
;					max_length[i]	=	MAX( SQRT( u2use^2 + v2use^2 ) )
;print,'i = ',i
;help,u2use
;print,'u2use = '
;print,u2use,format='(f64.32)'

;help,v2use
;print,'v2use = '
;print,v2use,format='(f64.32)'

;print,'i = ',i
;print,'max_length[i] = ',max_length[i]
				ENDFOR
				max_length	= MAX( max_length )
;print,'max_length = ',max_length
;help,max_length
				
				;===================================================
				; Now that global max_length is calculated, run the 
				; loop again for real processing.
				;===================================================
				FOR i=0,n_tiles-1 DO BEGIN

					u2use = REFORM(u[i,*,*])*0+default_missing_value2use
					v2use = REFORM(v[i,*,*])*0+default_missing_value2use
;;;ckt,apr2001					IF PTR_VALID(u_miss_ptr) THEN u2use = REFORM(u[i,*,*])*0+(*u_miss_ptr)[N_ELEMENTS(*u_miss_ptr)-1]	$
;;;ckt,apr2001					ELSE u2use = REFORM(u[i,*,*])*0+9999
;;;ckt,apr2001					IF PTR_VALID(v_miss_ptr) THEN v2use = REFORM(v[i,*,*])*0+(*v_miss_ptr)[N_ELEMENTS(*v_miss_ptr)-1]	$
;;;ckt,apr2001					ELSE v2use = REFORM(v[i,*,*])*0+9999
					missval	= SQRT( (MAX(u2use))^2 + (MAX(v2use))^2 )
					tmp_struct	= CREATE_STRUCT( extra_struct1, 'MISSING', missval )
					tmpu	= REFORM(u[i,*,*])
					tmpv	= REFORM(v[i,*,*])
					lonidx2use	= ( lon_idx + xoffset[i] ) / FLOAT( MAX( [!d.x_size,!d.y_size] ) )
					latidx2use	= ( lat_idx + yoffset[i] ) / FLOAT( MAX( [!d.x_size,!d.y_size] ) )
					tmpidx1	= LINDGEN(uv_res[0],uv_res[1])
					IF PTR_VALID(u_miss_ptr) THEN BEGIN
						IF is_continuous_u THEN BEGIN
							tmpidx1	= WHERE( tmpu LT (*u_miss_ptr)[0] OR tmpu GT (*u_miss_ptr)[1] )
						ENDIF ELSE BEGIN
							tmpidx1	= where2( tmpu, *u_miss_ptr, /INVERSE )
						ENDELSE
					ENDIF
					tmpidx2	= LINDGEN(uv_res[0],uv_res[1])
					IF PTR_VALID(v_miss_ptr) THEN BEGIN
						IF is_continuous_v THEN BEGIN
							tmpidx2	= WHERE( tmpv LT (*v_miss_ptr)[0] OR tmpv GT (*v_miss_ptr)[1] )
						ENDIF ELSE BEGIN
							tmpidx2	= where2( tmpv, *v_miss_ptr, /INVERSE )
						ENDELSE
					ENDIF
					tmpidx3	= where2( tmpidx1, tmpidx2 )
					IF tmpidx3[0] NE -1 THEN BEGIN
						u2use[tmpidx1[tmpidx3]]		= tmpu[tmpidx1[tmpidx3]]
						v2use[tmpidx1[tmpidx3]]		= tmpv[tmpidx1[tmpidx3]]
					ENDIF
;help,u2use
;help,v2use
;help,lonidx2use
;help,latidx2use

					length2use = MAX( SQRT(								$
							ABS( (u2use[WHERE( u2use LT 42.3106 OR u2use GT 42.3108 )])^2 ) +	$
							ABS( (v2use[WHERE( v2use LT 42.3106 OR v2use GT 42.3108 )])^2 ) ) ) / max_length
;print,'length2use = ',length2use
					; Scale by user-selected length ('SHORTEST', etc.)
					length2use = length2use * len2use[(*info_ptr).wd5_idx]
;print,'length2use = ',length2use

					velovect_subroutine,							$
						u2use		= u2use,					$
						v2use		= v2use,					$
						umin2use	= umin2use,					$
						umax2use	= umax2use,					$
						vmin2use	= vmin2use,					$
						vmax2use	= vmax2use,					$
						lon_idx		= lonidx2use,					$
						lat_idx		= latidx2use,					$
						length2use	= length2use,					$
						_extra		= tmp_struct
				ENDFOR
			ENDIF
		ENDFOR
	ENDELSE
	
	WSET, last_win
	PTR_FREE, info_ptr
END
; do_velocity_vectors
