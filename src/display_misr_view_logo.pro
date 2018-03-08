PRO display_misr_view_logo, version_str, versionPtr, req_imgPtr, logoPtr, SECONDS = secs, IDL_SAVE_VERSION = idl_save_version
	version	= *versionPtr
	req_img	= *req_imgPtr
	logo	= *logoPtr
	IF NOT KEYWORD_SET(secs) THEN secs = 3
	orig	= !D.WINDOW
	
	let_w	= 10
	let_h	= 15
;;;ckt,feb2000	version	= READ_TIFF('misr_number_letter.TIF')
	req_img_exists	= 0
	
	IF KEYWORD_SET( idl_save_version ) THEN BEGIN
		req_img_exists	= 1
;;;ckt,feb2000		req_img	= READ_TIFF('require.TIF')
		req_img	= REVERSE(req_img,3)
		x	= (SIZE(req_img))[2]
		y	= (SIZE(req_img))[3]
		n	= STRLEN(STRTRIM(idl_save_version,2))
		rx	= let_w*n+x
		rimg	= BYTARR( 3, rx, y )
		sy	= 5
		ey	= 19
		rimg[*,0:x-1,*]	= req_img
		curx	= x
		FOR i = 0, n - 1 DO BEGIN
			s	= STRMID(idl_save_version,i,1)
			CASE 1 OF
				s EQ '0': rimg[*,curx:curx+let_w-1,sy:ey] = version[*,60:60+let_w-1,*]
				s EQ '1': rimg[*,curx:curx+let_w-1,sy:ey] = version[*,70:70+let_w-1,*]
				s EQ '2': rimg[*,curx:curx+let_w-1,sy:ey] = version[*,80:80+let_w-1,*]
				s EQ '3': rimg[*,curx:curx+let_w-1,sy:ey] = version[*,90:90+let_w-1,*]
				s EQ '4': rimg[*,curx:curx+let_w-1,sy:ey] = version[*,100:100+let_w-1,*]
				s EQ '5': rimg[*,curx:curx+let_w-1,sy:ey] = version[*,110:110+let_w-1,*]
				s EQ '6': rimg[*,curx:curx+let_w-1,sy:ey] = version[*,120:120+let_w-1,*]
				s EQ '7': rimg[*,curx:curx+let_w-1,sy:ey] = version[*,130:130+let_w-1,*]
				s EQ '8': rimg[*,curx:curx+let_w-1,sy:ey] = version[*,140:140+let_w-1,*]
				s EQ '9': rimg[*,curx:curx+let_w-1,sy:ey] = version[*,150:150+let_w-1,*]
				s EQ '.': rimg[*,curx:curx+let_w-1,sy:ey] = version[*,160:160+let_w-1,*]
				s EQ ' ': rimg[*,curx:curx+let_w-1,sy:ey] = BYTARR(3,10,15)
				ELSE: BEGIN
					idx	= STRPOS( 'abcdefghijklmnopqrstuvwxyz', s )
					xs	= 170+(idx*10)
					rimg[*,curx:curx+let_w-1,sy:ey] = version[*,xs:xs+let_w-1,*]
					END
			ENDCASE
			curx	= curx + let_w
		ENDFOR
	ENDIF
	
	
;;;ckt,feb2000	logo	= READ_TIFF('misr_view_logo.TIF')
	logo	= REVERSE(logo,3)
	x	= (SIZE(logo))[2]
	y	= (SIZE(logo))[3]
	
	n		= STRLEN(STRTRIM(version_str,2))
	vx		= let_w*n+60
	vimg		= BYTARR( 3, vx, let_h )
	vimg[*,0:59,*]	= version[*,0:59,*]
	curx		= 60
	init_len	= 60
	
	FOR i = 0, n - 1 DO BEGIN
		s	= STRMID(version_str,i,1)
		CASE 1 OF
			s EQ '0': vimg[*,curx:curx+let_w-1,*] = version[*,60:60+let_w-1,*]
			s EQ '1': vimg[*,curx:curx+let_w-1,*] = version[*,70:70+let_w-1,*]
			s EQ '2': vimg[*,curx:curx+let_w-1,*] = version[*,80:80+let_w-1,*]
			s EQ '3': vimg[*,curx:curx+let_w-1,*] = version[*,90:90+let_w-1,*]
			s EQ '4': vimg[*,curx:curx+let_w-1,*] = version[*,100:100+let_w-1,*]
			s EQ '5': vimg[*,curx:curx+let_w-1,*] = version[*,110:110+let_w-1,*]
			s EQ '6': vimg[*,curx:curx+let_w-1,*] = version[*,120:120+let_w-1,*]
			s EQ '7': vimg[*,curx:curx+let_w-1,*] = version[*,130:130+let_w-1,*]
			s EQ '8': vimg[*,curx:curx+let_w-1,*] = version[*,140:140+let_w-1,*]
			s EQ '9': vimg[*,curx:curx+let_w-1,*] = version[*,150:150+let_w-1,*]
			s EQ '.': vimg[*,curx:curx+let_w-1,*] = version[*,160:160+let_w-1,*]
			s EQ ' ': vimg[*,curx:curx+let_w-1,*] = BYTARR(3,10,15)
			ELSE: BEGIN
				idx	= STRPOS( 'abcdefghijklmnopqrstuvwxyz', s )
				xs	= 170+(idx*10)
				vimg[*,curx:curx+let_w-1,*] = version[*,xs:xs+let_w-1,*]
				END
		ENDCASE
		curx	= curx + let_w
	ENDFOR
	
	logo[*, (x-vx)/2:(x-vx)/2+vx-1,60:60+let_h-1]	= vimg
	
	IF req_img_exists THEN logo[*, (x-rx)/2:(x-rx)/2+rx-1,5:5+20-1]	= rimg
		
	b	= WIDGET_BASE( /COLUMN, TLB_FRAME_ATTR = 1+2+4+8 )
	d	= WIDGET_DRAW( b, XSIZE = x, YSIZE = y, RETAIN = 2 )
	WIDGET_CONTROL, b, /REALIZE
	WIDGET_CONTROL, d, GET_VALUE = id
	WSET, id
			
	IF !D.N_COLORS LE 256 THEN BEGIN
		TVLCT, r_orig, g_orig, b_orig, /GET
		logo	= COLOR_QUAN( logo, 1, rr, gg, bb, CUBE = 6 )
		TVLCT, rr, gg, bb
		TV, logo
	ENDIF ELSE BEGIN
		TV, REFORM(logo[0,*,*]), CHANNEL = 1
		TV, REFORM(logo[1,*,*]), CHANNEL = 2
		TV, REFORM(logo[2,*,*]), CHANNEL = 3
	ENDELSE
	
	WIDGET_CONTROL, b, MAP = 1
	
	WAIT, secs

	IF WIDGET_INFO( b, /VALID_ID ) THEN	$
		WIDGET_CONTROL, b, /DESTROY	$
	ELSE					$
		display_misr_view_logo, version_str, versionPtr, req_imgPtr, logoPtr, SECONDS = 1, IDL_SAVE_VERSION = idl_save_version

	
	IF !D.N_COLORS LE 256 THEN TVLCT, r_orig, g_orig, b_orig
END
; display_misr_view_logo
