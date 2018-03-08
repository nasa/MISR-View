@build_data_menu.pro
@valid_airmisr_file.pro
@retrieve_airmisr_lonlat_grids.pro
@retrieve_airmisr_latlon_info.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ fill_data_button @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO fill_data_button, baseID, productInfo, selectedOrbit, selectedPath, $
			AIRMISR_FILE = airMisrFile

;print,''
;print,'*** *** *** *** *** *** *** ***'
;print,'*** *** fill_data_buton *** ***'
;print,'*** *** *** *** *** *** *** ***'
;print,''
                      
	WIDGET_CONTROL, baseID, GET_UVALUE = infoPtr

	;---------------------------------------------------------------------
	; Special case for AirMISR data
	;---------------------------------------------------------------------
	IF selectedOrbit LT 0 AND selectedPath LT 0 AND KEYWORD_SET(airMisrFile) THEN BEGIN
		fn	= STRARR(1)
		fn[0]	= airMisrFile
		WIDGET_CONTROL, (*infoPtr).dataMenu, /SENSITIVE
		build_data_menu, fn, infoPtr, /AIRMISR, DISABLE_BASE = WIDGET_INFO(baseID, /PARENT)
		RETURN
	ENDIF

	;---------------------------------------------------------------------
	; If productInfo is undefined, just return
	;---------------------------------------------------------------------
	IF SIZE( productInfo, /TYPE ) LE 0 THEN BEGIN
		WIDGET_CONTROL, (*infoPtr).dataMenu, SENSITIVE = 0
		RETURN
	ENDIF

	;---------------------------------------------------------------------
	; First off, stringify the path and orbit information
	; so that it resembles the path/orbit information in the
	; file names (i.e., MISR_AM1_GRP_TERRAIN_Pmmm_Onnnnnn_cc_vv.hdf)
	;---------------------------------------------------------------------
	orbitStr	= STRTRIM(STRING(selectedOrbit),2)
	orbitStr	= STRMID('00000'+orbitStr,STRLEN(orbitStr)-1,6)

	pathStr		= STRTRIM(STRING(selectedPath),2)
	pathStr		= STRMID('00'+pathStr,STRLEN(pathStr)-1,3)

	;---------------------------------------------------------------------
	; Now, locate the file name entries where path and/or orbit
	; numbers match those stringified above
	;---------------------------------------------------------------------
	orbitIdx	= WHERE(productInfo[*,2] EQ orbitStr, orbitCnt)
;;;ckt,aug2000	pathIdx		= WHERE(productInfo[*,1] EQ pathStr,  pathCnt)
	
	
pathIdx		 =WHERE(								$
			( productInfo[*,1] EQ pathStr AND productInfo[*,2] EQ '' ) OR	$
			( productInfo[*,1] EQ pathStr AND productInfo[*,2] EQ orbitStr ), pathCnt )

	;---------------------------------------------------------------------
	; Impose the requirement that AGP and GP_GMP files MUST exist for the
	; current path/orbit combo; if they do not, do not sensitize the data
	; button and return.  THIS IMPLIES THAT pathCnt and orbitCnt MUST 
	; ALWAYS BE GREATER THAN ZERO IN ORDER FOR THE DATA MENU BUTTON TO BE
	; SENSITIZED!
	;---------------------------------------------------------------------
;;;ckt, may1999	required_files_exist	= 0
;;;ckt, may1999	IF pathCnt GT 0 AND orbitCnt GT 0 THEN BEGIN
;;;ckt, may1999		agpIdx	= WHERE (productInfo[pathIdx,0] EQ 'AGP', agpCnt )
;;;ckt, may1999		gmpIdx	= WHERE( productInfo[pathIdx,0] EQ 'GP_GMP', gmpCnt )
;;;ckt, may1999		IF agpCnt GT 0 AND gmpCnt GT 0 THEN					$
;;;ckt, may1999			required_files_exist	= 1
;;;ckt, may1999	ENDIF
	
;;;ckt, may1999	IF NOT required_files_exist THEN BEGIN
;;;ckt, may1999		WIDGET_CONTROL, (*infoPtr).dataMenu, SENSITIVE = 0
;;;ckt, may1999		RETURN
;;;ckt, may1999	ENDIF

;;;ckt, jun1999	IF NOT pathCnt LE 0 AND orbitCnt LE 0 THEN BEGIN
	IF pathCnt LE 0 THEN BEGIN
		WIDGET_CONTROL, (*infoPtr).dataMenu, SENSITIVE = 0
		RETURN
	ENDIF
	
	;---------------------------------------------------------------------
	; If we are at this point, we know that the data button should be 
	; sensitized, so go ahead and do it.
	;---------------------------------------------------------------------
	WIDGET_CONTROL, (*infoPtr).dataMenu, SENSITIVE = 1
	
	;---------------------------------------------------------------------
	; Set up a string array dimensioned to the size of pathCnt; since every
	; file, including AGP files, contain path information, pathCnt should
	; account for all matched files.  If there are separate entries for
	; the same file within the catalog, these entries will be reflected in the
	; menu presented to the user.  In other words, the software does not detect
	; and eliminate files of the same name if they are in the source catalog.
	;---------------------------------------------------------------------
	fn	= STRARR( pathCnt )

	FOR i = 0, pathCnt - 1 DO BEGIN
		fn[ i ]	=							$
			STRTRIM( productInfo[ pathIdx[ i ], 4 ], 2 ) +		$ ; fully-qualified path
			STRTRIM( productInfo[ pathIdx[ i ], 5 ], 2 )		  ; name of file
;print,'STRTRIM( productInfo[ pathIdx[ i ], 4 ], 2 )=',STRTRIM( productInfo[ pathIdx[ i ], 4 ], 2 )
;print,'STRTRIM( productInfo[ pathIdx[ i ], 5 ], 2 )=',STRTRIM( productInfo[ pathIdx[ i ], 5 ], 2 )
;print,'=========================='
;print,'fn['+strtrim(i,2)+']= ' + fn[ i ]
;print,'=========================='
	ENDFOR

	build_data_menu, fn, infoPtr, DISABLE_BASE = WIDGET_INFO(baseID, /PARENT)

END
; fill_data_button
