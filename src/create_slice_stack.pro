;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ create_slice_stack @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION create_slice_stack, img, slice_size_wh, MISSING_DATA_VALUES = mval
	missing_vals_exist			= 1
	IF KEYWORD_SET(mval) THEN missing_vals	= mval ELSE missing_vals_exist = 0
	IF missing_vals_exist THEN val2add = missing_vals[0] ELSE val2add = 0.0D
	img_wh					= [ (SIZE(img))[1], (SIZE(img))[2] ]
;print,'>>>>>>>>>>>> original img_wh = ',img_wh
	slice_w					= slice_size_wh[0]
	slice_h					= slice_size_wh[1]
	
	pad_w					= slice_w - (img_wh[0] MOD slice_w)
	IF pad_w GE slice_w THEN pad_w = 0
	pad_h					= slice_h - (img_wh[1] MOD slice_h)
	IF pad_h GE slice_h THEN pad_h = 0
;print,'pad_w,pad_h=',pad_w,pad_h
	new_img					= convert_number( DBLARR( img_wh[0]+pad_w, img_wh[1]+pad_h )+val2add, SIZE(img,/TYPE) )
	new_img[0:img_wh[0]-1,0:img_wh[1]-1]	= img
	img					= new_img
	img_wh					= [ (SIZE(img))[1], (SIZE(img))[2] ]

	FOR i = 0, img_wh[0]-1, slice_w DO BEGIN
		FOR j = 0, img_wh[1]-1, slice_h DO BEGIN
;print,'img_wh = ',img_wh
;print,'slice_h,slice_w = ',slice_h,slice_w
;print,'i,j = ',i,j
;print,'missing_vals_exist=',missing_vals_exist
			current_slice	= REFORM(img[i:i+slice_w-1,j:j+slice_h-1])
			skip_this_slice	= 0
			;=========================================================================
			; The following code has been commented out, but should be uncommented in
			; future versions, since it eliminates any "tiles" that have nothing but
			; missing data, thereby elminiating potentially large amounts of unneeded
			; tiles in a mosaicked image.  The problem is that, if this routine is called
			; for several different datasets that are coincident but have different areas
			; of missing data, it's possible to have inconsistencies in calculated mosaic
			; image sizes when they are calculated by the IMAGE_DATA object.  To remedy this,
			; this routine needs to routine needs to return the locations of the tiles that
			; contain valid data, then this information needs to be checked with other 
			; datasets that use this routine in order to cross-reference tiles with
			; valid data.  The union of all tile locations containing valid data should
			; be used for images that are to be considered conicident with one another.
			;
			; However, another argument for NOT eliminating "black space" tiles is when
			; a user wants to specify a reprojection extent for the purposes of manually
			; mosaicking images together.  By not eliminating tiles, each image represents
			; the extent that the user desires.
			;=========================================================================
;			IF missing_vals_exist THEN BEGIN
;				good_idx	= where2( current_slice, missing_vals )
;				IF good_idx[0] GE 0 AND N_ELEMENTS(good_idx) EQ (slice_w*slice_h) THEN skip_this_slice = 1
;			ENDIF
			IF NOT skip_this_slice THEN BEGIN
				IF ( i EQ 0 AND j EQ 0 ) OR ( SIZE( slice_stack, /TYPE) LE 0 ) THEN BEGIN
					slice_stack		= [current_slice]
					x_off			= [0L]
					y_off			= [0L]
				ENDIF ELSE BEGIN
					slice_stack		= [ [[slice_stack]], [[current_slice]] ]
					x_off			= [x_off, i]
					y_off			= [y_off, j]
				ENDELSE
			ENDIF
		ENDFOR
	ENDFOR
	
;	IF (SIZE(slice_stack))[0] GT 2 THEN slice_stack = TRANSPOSE( slice_stack, [2,0,1] )
	
	RETURN, { slice_stack:slice_stack, x_offsets:x_off, y_offsets:y_off }
END
; create_slice_stack
