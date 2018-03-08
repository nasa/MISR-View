@create_projected_image_data.pro
@do_reprojection.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ reprojection_tool @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION reprojection_tool,							$
			lon_data_objarr,					$
			lat_data_objarr,					$
			rgba_data_objarr,					$
			GROUP_LEADER = group_leader
			

	n_lon_obj	= N_ELEMENTS(lon_data_objarr)
	n_lat_obj	= N_ELEMENTS(lat_data_objarr)
	n_rgba		= N_ELEMENTS(rgba_data_objarr[0,*])

	IF n_lon_obj NE n_lat_obj THEN BEGIN
		msg	= [							$
				'Differing number of longitude and latitude',	$
				'objects passed into reprojection tool.' ]
		res	= DIALOG_MESSAGE( msg, /ERROR )
		RETURN, PTR_NEW()
	ENDIF
	
	IF (n_lon_obj NE n_rgba) AND (n_lon_obj NE 1) THEN BEGIN
		msg	= [							$
				'Either one set of lon/lat image data objects',	$
				'for all RGB objects or one set of lon/lat',	$
				'data objects per each RGB object must be specified.' ]
		res	= DIALOG_MESSAGE( msg, /ERROR )
		RETURN, PTR_NEW()
	ENDIF
						
	w		= (lon_data_objarr[0])->GetImageWidth()
	h		= (lon_data_objarr[0])->GetImageHeight()
	
	default_min_lon	= (lon_data_objarr[0])->GetMinVal(0,w-1,0,h-1,w,h)
	default_max_lon	= (lon_data_objarr[0])->GetMaxVal(0,w-1,0,h-1,w,h)
	default_min_lat	= (lat_data_objarr[0])->GetMinVal(0,w-1,0,h-1,w,h)
	default_max_lat	= (lat_data_objarr[0])->GetMaxVal(0,w-1,0,h-1,w,h)
					
	FOR i = 1, N_ELEMENTS(lon_data_objarr) - 1 DO BEGIN
		default_min_lon	= MIN( [ default_min_lon, (lon_data_objarr[i])->GetMinVal(0,w-1,0,h-1,w,h) ] )
		default_max_lon	= MAX( [ default_max_lon, (lon_data_objarr[i])->GetMaxVal(0,w-1,0,h-1,w,h) ] )
		default_min_lat	= MIN( [ default_min_lat, (lat_data_objarr[i])->GetMinVal(0,w-1,0,h-1,w,h) ] )
		default_max_lat	= MAX( [ default_max_lat, (lat_data_objarr[i])->GetMaxVal(0,w-1,0,h-1,w,h) ] )
	ENDFOR
	
	;================================================================================
	; No need to check for longitude wraparound since default_min_lon will
	; DEFINITELY be less than or equal to default_max_lon (by virtue of the statements
	; above).  In other words, there will never be a case where default_min_lon will be
	; greater than default_max_lon, so figuring out cen_lon doesn't require any
	; checking.
	;================================================================================
	cen_lat	= ( default_max_lat + default_min_lat ) / 2.0
	cen_lon	= ( default_max_lon + default_min_lon ) / 2.0
		
	max_h	= h
	max_w	= w
	FOR i = 0, N_ELEMENTS(rgba_data_objarr)-1 DO BEGIN
		IF OBJ_VALID(rgba_data_objarr[i]) THEN BEGIN
			max_h	= MAX([max_h,(rgba_data_objarr[i])->GetImageHeight()])
			max_w	= MAX([max_w,(rgba_data_objarr[i])->GetImageWidth()])
		ENDIF
	ENDFOR
	
	;--------------------------------
	; Average length of a degree of
	; latitude (km)
	;--------------------------------
	avg_length_lat	= 111.0
	
	kill_gl	= 0
	IF KEYWORD_SET(group_leader) THEN gl = group_leader ELSE kill_gl = 1
	IF kill_gl THEN group_leader = WIDGET_BASE()

	DEFAULT_RES	= ((default_max_lat-default_min_lat)*avg_length_lat) / FLOAT(max_h)
	
;print,'=====================DEFAULT_RES = ',DEFAULT_RES
;		{								$
;			failure			: 0,				$
;			map_set_str		: str2exec,			$
;			window_width		: final_width,			$
;			window_height		: final_height,			$
;			parameter_listing	: strarr2check,			$
;			fill_seams		: (*ptr).do_seam_filling,	$
;			seam_fill_parameters	: (*ptr).fill_seam_info,	$
;			create_overlay_image	: (*ptr).overlay,		$
;			overlap_idx		: (*ptr).overlap_idx }

	return_struct	= cw_proj_info(											$
					gl,										$
					LIMIT = [default_min_lat,default_min_lon,default_max_lat,default_max_lon],	$
					CEN_LAT = cen_lat,								$
					CEN_LON = cen_lon,								$
					RESOLUTION = DEFAULT_RES )
					
	IF return_struct.failure THEN RETURN, PTR_NEW( { cancel_pressed:1 }, /NO_COPY )
					
	struct1		= create_projected_image_data(							$
					rgba_data_objarr,						$
					lon_data_objarr,						$
					lat_data_objarr,						$
					return_struct.map_set_str,					$
					return_struct.fill_seams,					$
					[return_struct.window_width,return_struct.window_height],	$
					return_struct.seam_fill_parameters,				$
					return_struct.overlap_idx )
;print,'struct1.cancel_pressed = ',struct1.cancel_pressed				
	ptr2struct	= PTR_NEW( CREATE_STRUCT(							$
						struct1,						$
						'projname', return_struct.projection_name,		$
						'parameter_listing', return_struct.parameter_listing,	$
						'map_set_string', return_struct.map_set_str ), /NO_COPY )
		
	
	RETURN, ptr2struct
END
; reprojection_tool
