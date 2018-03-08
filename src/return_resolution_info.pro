FUNCTION return_resolution_info, avg_res_km, limit, POLAR = polar
	;--------------------------------
	; Average length of a degree of
	; latitude (km)
	;--------------------------------
	avg_length_lat		= 111.0
	;--------------------------------
	; NS_INC_DEGREES is equal to 1/2 the
	; resolution of an NSCAT cell (25km)
	; divided by the approximate length
	; of a degree of latitude expressed
	; in kilometers (111.0)
	;--------------------------------
	NS_INC_DEGREES	= 12.50 / avg_length_lat
	
	length_lon_km_start_0	= FLTARR( 91 ) * 0.0
	length_lon_km_start_0( 0:89 )	= [    							 $
		111.321,111.304,111.253,111.169,111.051,110.900,110.715,110.497,110.245,109.959, $
		109.641,109.289,108.904,108.486,108.036,107.553,107.036,106.487,105.906,105.294, $
		104.649,103.972,103.264,102.524,101.754,100.952,100.119, 99.257, 98.364, 97.441, $
		 96.488, 95.506, 94.495, 93.455, 92.387, 91.290, 90.166, 89.014, 87.835, 86.629, $
		 85.396, 84.137, 82.853, 81.543, 80.208, 78.849, 77.466, 76.058, 74.628, 73.174, $
		 71.698, 70.200, 68.680, 67.140, 65.578, 63.996, 62.395, 60.774, 59.135, 57.478, $
		 55.802, 54.110, 52.400, 50.675, 48.934, 47.177, 45.407, 43.622, 41.823, 40.012, $
		 38.188, 36.353, 34.506, 32.648, 30.781, 28.903, 27.017, 25.123, 23.220, 21.311, $
		 19.394, 17.472, 15.545, 13.612, 11.675,  9.735,  7.792,  5.846,  3.898,  1.949 ]
		 
	left_lon	= limit[1]
	right_lon	= limit[3]
	upper_lat	= limit[2]
	lower_lat	= limit[0]
	
	lat_extent_km	= (upper_lat-lower_lat)*avg_length_lat
	IF right_lon GT left_lon THEN BEGIN
		lon_extent_km	= (right_lon-left_lon)*length_lon_km_start_0[ABS(ROUND((upper_lat+lower_lat)/2.0))]
	ENDIF ELSE BEGIN
		lon_extent_km	= (180.0-right_lon)*length_lon_km_start_0[ABS(ROUND((upper_lat+lower_lat)/2.0))]
		lon_extent_km	= lon_extent_km+((180.0+left_lon)*length_lon_km_start_0[ABS(ROUND((upper_lat+lower_lat)/2.0))])
	ENDELSE
;print,'avg_res_km=',avg_res_km	
;print,'lat_extent_km=',	lat_extent_km
;print,'lon_extent_km=',	lon_extent_km

	nlines	= ROUND(lat_extent_km / avg_res_km)
	nsamps	= ROUND(lon_extent_km / avg_res_km)
;print,'nlines,nsamps=',	nlines,nsamps
	IF KEYWORD_SET(polar) THEN RETURN, { nlines:nlines, nsamps:nlines }	$
	ELSE RETURN, { nlines:nlines, nsamps:nsamps }
END
; return_resolution
