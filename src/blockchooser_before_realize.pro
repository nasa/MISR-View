;----------------------------------------------------
; Author:  Jeffrey.R.Hall@jpl.nasa.gov (x4-4149)
; Written: 1998
; Updated: 1999-2000
; Project: MISR, David.J.Diner@jpl.nasa.gov (x4-6319)
;----------------------------------------------------

@month_int2str
@month_str2intstr
@systime2datetime
@orbit2path
@orbit2juliandate
@julianday
@most_recent_orbit
@path2most_recent_orbit
@misr_get_initial_orbit_path_date_info

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ reset_upperBase_buttons @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO reset_upperBase_buttons, statePtr, newPathNumber, newOrbitNumber
;print,'reset_upperBase_buttons:  newOrbitNumber = ',newOrbitNumber
	;-------------------------------------------
	; Set the new path number value and read in
	; the block corners for that path.
	;-------------------------------------------
	if newOrbitNumber eq 0 then $
		newOrbitNumber = path2most_recent_orbit( newPathNumber, (*statePtr).Nref, (*statePtr).JNref, (*statePtr).PNref )
;print,''
;print,'*** reset_upperBase_buttons ***'
;print,'newPathNumber = ',newPathNumber
;print,'newOrbitNumber = ',newOrbitNumber
;help,newOrbitNumber
;print,'(*statePtr).Nref = ',(*statePtr).Nref
;print,'(*statePtr).PNref = ',(*statePtr).PNref
	widget_control, (*statePtr).pathText, set_value = strtrim( newPathNumber, 2 )
	widget_control, (*statePtr).orbitListButton, $
		set_value = 'Orbit List, Path ' + strtrim( newPathNumber, 2 )
;print,'reset_upperBase_buttons:  newPathNumber = ',newPathNumber
;print,''
	if newPathNumber ne (*statePtr).previousPath then begin
		readNewBlockCornerFile, statePtr, LONG( newPathNumber )
		(*statePtr).previousPath = LONG( newPathNumber )
	endif
	unset_planes, statePtr, LONG( newOrbitNumber ), LONG( newPathNumber )

	widget_control, (*statePtr).orbitText, set_value = STRTRIM(STRING(LONG(newOrbitNumber)),2)
	(*statePtr).previousOrbit = LONG( newOrbitNumber )

	;-------------------------------------------
	; Put current cursor LONGITUDE (lonLat[0])
	; into map center LONGITUDE (LatLon[1]).
	;-------------------------------------------
	centerBlockNumber = N_ELEMENTS( (*statePtr).binaryCornerFile[*,0] ) / 2 - 1
	(*statePtr).mapCenterLatLon[1] = (*statePtr).binaryCornerFile[centerBlockNumber,1] mod 360.0
	if strupcase( (*statePtr).zoomStatus ) eq 'ZOOMED_IN' then begin
		;-------------------------------------------
		; Get the block numbers from the text
		; widgets.
		;-------------------------------------------
		widget_control, (*statePtr).whichBlocksText[0], get_value = upperBlock
		widget_control, (*statePtr).whichBlocksText[1], get_value = lowerBlock
		upperBlock = LONG( upperBlock[0] )
		lowerBlock = LONG( lowerBlock[0] )

		;-------------------------------------------
		; Determine which is the center block of the
		; range of chosen blocks.  For mid-latitude
		; zooms, the center point of this block will
		; be the center of the zoom.  For polar zooms,
		; this is ignored.
		;-------------------------------------------
		zoomBlock = upperBlock + (lowerBlock-upperBlock)/2 - 1

		;-------------------------------------------
		; Assign zoomblock's center point as the map
		; center point which will be used by map_set
		; in the "mapDraw" and "plotPath" routines.
		;-------------------------------------------
		(*statePtr).mapCenterLatLon = [ $
			(*statePtr).binaryCornerFile[zoomBlock,0], $
			(*statePtr).binaryCornerFile[zoomBlock,1] mod 360.0 ]
	endif
	(*statePtr).lonLat[0] = (*statePtr).mapCenterLatLon[1]

	;-------------------------------------------
	; Determine and set the date of this orbit.
	;-------------------------------------------
	setDate, statePtr, newOrbitNumber

	;-------------------------------------------
	; Determine and set the hour and minute of
	; the orbit.
	;-------------------------------------------
	setTime, statePtr, newOrbitNumber

	;-------------------------------------------
	; Redraw the map.
	;-------------------------------------------
	if strupcase( (*statePtr).zoomStatus ) eq 'ZOOMED_IN' then begin
		drawMap, statePtr, scale = 600E5
	endif else begin
		drawMap, statePtr
	endelse

	;-------------------------------------------
	; Redraw the path.
	;-------------------------------------------
	plotPath, statePtr

	refresh_line_buffer, statePtr

	if strupcase((*statePtr).chooseOrbitOrBlocksFlag) eq 'BLOCKS' then begin

plotBlockIdx = 16679875
IF (*statePtr).eight_bit_display THEN plotBlockIdx = (*statePtr).selectedBlockIdx

		plotBlocks, statePtr, plotBlockIdx

		;-------------------------------------------
		; Re-draw the block choosing lines in their
		; correct locations.
		;-------------------------------------------
		curWin = !d.window
		wset, (*statePtr).mapDrawWindowID
		device, copy = [			$
			0,				$
			0,				$
			(*statePtr).mapX,		$
			1,				$
			0,				$
			(*statePtr).oldLineValue[0],	$
			(*statePtr).linePixMap[0] ]
		device, copy = [			$
			0,				$
			0,				$
			(*statePtr).mapX,		$
			1,				$
			0,				$
			(*statePtr).oldLineValue[1],	$
			(*statePtr).linePixMap[0] ]
		wset, curWin
	endif
END
; reset_upperBase_buttons

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ unset_planes @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro unset_planes, statePtr, orbit, path, _extra = airMisrFile


;print,''
;print,''
;print,''
;print,''
;print,''
;print,'unset_planes'
;print,''
;print,''
;print,''
;print,''
;print,''

	widget_control, (*statePtr).lowerBase, get_uvalue = lowerBaseStatePtr

;;;ckt,aug2000	gotMemoryValue = 0.0

;jzh,feb2001 - commented out this IF statement to accomodate RECALL of stored data selections for AirMISR data.
;I can't see why this IF was here in the first place, maybe the reason will turn up later.  Seems to work for now.
;	if not (*statePtr).airMisrFlag then begin

		dataString = [ '', 'NOT SET', '', '' ]

		for i = 0, 5 do begin

			;--------------------------------------------
			; Unset each plane object's data information.
			;--------------------------------------------
			(*lowerBaseStatePtr).planeObjStructArray[i].fname = ''
			(*lowerBaseStatePtr).planeObjStructArray[i].grid = ''
			(*lowerBaseStatePtr).planeObjStructArray[i].field = ''
			(*lowerBaseStatePtr).planeObjStructArray[i].num_type = ''
			ptr_free, (*lowerBaseStatePtr).planeObjStructArray[i].extraDims

;;;ckt,aug2000				gotMemoryValue = gotMemoryValue + ((*lowerBaseStatePtr).planeObjArr)[i] ->  getMemoryValue()
			((*lowerBaseStatePtr).planeObjArr)[i] -> SetText, dataString
;;;ckt,aug2000				((*lowerBaseStatePtr).planeObjArr)[i] -> SetMemoryValue, 0.0
		endfor

;;;ckt,aug2000		widget_control, (*lowerBaseStatePtr).memCurrLabel, set_value = '0.0 MB'
;;;ckt,aug2000		widget_control, (*lowerBaseStatePtr).memCumLabel, set_value = '0.0 MB'
;;;ckt,aug2000		(*lowerBaseStatePtr).memCum = 0.0

;jzh,feb2001
;	endif

;;;ckt,aug2000	(*lowerBaseStatePtr).memAvail	= (*lowerBaseStatePtr).memAvail + gotMemoryValue
;;;ckt,aug2000	memAvailStr			= ConvertFloatingMem2Str( (*lowerBaseStatePtr).memAvail )
;;;ckt,aug2000	WIDGET_CONTROL, (*lowerBaseStatePtr).memAvailLabel, SET_VALUE = memAvailStr

	widget_control, (*lowerBaseStatePtr).dataLabelBase[1], map = 0
	widget_control, (*lowerBaseStatePtr).dataLabelBase[0], map = 1
        (*lowerBaseStatePtr).mappedDataLabelBaseIdx = 0
	widget_control, (*lowerBaseStatePtr).dataLabel[0], set_value = '        None Selected         '

	widget_control, (*lowerBaseStatePtr).setButton, sensitive = 0
	tlb = widget_info( (*statePtr).lowerBase, /parent )
	widget_control, tlb, get_uvalue = tlbPtr
	widget_control, (*tlbPtr).createID, sensitive = 0

;;;;jan99,ckt	if n_elements( (*statePtr).catContents ) ne 1 then $
;;;;jan99,ckt	if SIZE( *((*statePtr).catContentsPtr), /TYPE ) GT 0 THEN		$
           fill_data_button, (*statePtr).lowerBase, *((*statePtr).catContentsPtr),	$
;;;;jan99,ckt			(*statePtr).catContents,	$
				orbit,				$
				path,				$
				_extra = airMisrFile

end
; unset_planes

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ catalogButtonEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro catalogButtonEvent, event
	upperBase = widget_info( event.top, /child )
	widget_control, upperBase, get_uvalue = statePtr

	catTlb = widget_base( /column, RESOURCE_NAME = 'glbc' )
	catDone = widget_button( catTlb, value = 'Done', $
			event_pro = 'catDoneEvent', $
			RESOURCE_NAME = 'menu_level_0' )
	catTable = widget_table( catTlb, $
			value = rotate( (*((*statePtr).catContentsPtr))[*,*], 4 ), $
;;;jan99,ckt			value = rotate( (*statePtr).catContents[*,*], 4 ), $
			column_labels = ['Product','Path','Orbit','Camera','Directory'], $
			column_widths = [ 300, 100, 100, 100, 600 ], $
			/scroll, scr_xsize = 800 )

	widget_control, catTlb, /realize
end
; catalogButtonEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ catDoneEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro catDoneEvent, event
	catBase = widget_info( event.id, /parent )
	widget_control, catBase, /destroy
end
; catDoneEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ readOldBlockCornerFile @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;-------------------------------------------
;-------------------------------------------
;-------------------------------------------
; SUPERCEEDED BY "readNewBlockCornerFile !!!
;---------------------***-------------------
;-------------------------------------------
;-------------------------------------------
pro readOldBlockCornerFile, statePtr, pathNumber

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== readOldBlockCornerFile =========='
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		e_msg	= [											$
				routine_name,									$
				'Error Index: ' + STRTRIM( error_status, 2 ),					$
				'Error Message: ' + !ERR_STRING,						$
				'',										$
				'Suggestion: If attempting to read or write a file, then check permissions.',	$
				'',										$
				'Returning...' ]
		result	= DIALOG_MESSAGE( e_msg, /ERROR )
		RETURN
	ENDIF


	;-------------------------------------------
	; Only one path (swath) at a time is stored
	; in the state variable.
	;-------------------------------------------

	;-------------------------------------------
	; Open the file containing the orbit track
	; lat/lon values.
	;-------------------------------------------
	; Mark Bull, JPL 4-0439, constructs the
	; corner point files.  He puts these files
	; in this directory on jord.jpl.nasa.gov:
	; /data/geo_info/agp/block_file/
	; There is one file for each path (swath)
	; number.
	;-------------------------------------------
	blockCornerFileName = 'v2_180block_path' + $
		strtrim( pathNumber, 2 ) + $
		'.block_corners'
	openr, lun, blockCornerFileName, /get
	asciiCornerFile = strarr( 1440 )
	readf, lun, asciiCornerFile, format = '(a255)'
	close,lun
	free_lun,lun

	;-------------------------------------------
	; Loop over every line (1440 total) in the
	; block corner file, and check to see which
	; type of information is contained in that
	; line.  Type 'Block' contains the block number
	; which is used to point into the binaryCornerFile
	; array.  There are 5 other types- 'Center:',
	; 'UpperLeft:', 'UpperRight', 'LowerRight', and
	; 'LowerLeft:' that contain the lat and lon of
	; that item for each block.  All this information
	; is transferred from the ASCII file to the
	; binary array.  The binary array will be used
	; for plotting on the map.
	;-------------------------------------------
	j = -1
	for i = 0, 1440-1 do begin
		a = strtrim( asciiCornerFile[i], 2 )
		b = strcompress( a )
		c = str_sep( b, ' ' )
		sz = size(c)
		if sz[1] eq 3 then begin
			if c[0] eq 'Block' then begin
				j = LONG( c[2] ) - 1
			endif
			if c[0] eq 'Center:' then begin
				(*statePtr).binaryCornerFile[j,0] = float( c[1] )
				(*statePtr).binaryCornerFile[j,1] = float( c[2] )
			endif
			if c[0] eq 'UpperLeft:' then begin
				(*statePtr).binaryCornerFile[j,2] = float( c[1] )
				(*statePtr).binaryCornerFile[j,3] = float( c[2] )
			endif
			if c[0] eq 'UpperRight:' then begin
				(*statePtr).binaryCornerFile[j,4] = float( c[1] )
				(*statePtr).binaryCornerFile[j,5] = float( c[2] )
			endif
			if c[0] eq 'LowerRight:' then begin
				(*statePtr).binaryCornerFile[j,6] = float( c[1] )
				(*statePtr).binaryCornerFile[j,7] = float( c[2] )
			endif
			if c[0] eq 'LowerLeft:' then begin
				(*statePtr).binaryCornerFile[j,8] = float( c[1] )
				(*statePtr).binaryCornerFile[j,9] = float( c[2] )
			endif
		endif
	endfor

	;-------------------------------------------
	; Store the middle (closest to the equator)
	; block's longitude as the map center
	; longitude for map_set.
	;-------------------------------------------
	centerBlockNumber = N_ELEMENTS( (*statePtr).binaryCornerFile[*,0] ) / 2 - 1
	(*statePtr).mapCenterLatLon[1] = (*statePtr).binaryCornerFile[centerBlockNumber,1]

	;-------------------------------------------
	; Set up the map coordinate system.
	;-------------------------------------------
	curWin = !d.window
	wset, (*statePtr).mapDrawWindowID
	map_set, (*statePtr).mapCenterLatLon[0], $
		(*statePtr).mapCenterLatLon[1], $
		0.0, $
		/lambert, /noborder, xmargin = 0.0, ymargin = 0.0
	wset, curWin

	;-------------------------------------------
	; Convert all of the block center points into
	; device coordinates.  By using the Lambert
	; map projection, the blocks will receive
	; monotonically decending line numbers which
	; will be used to correlate the cursor with
	; the block numbers during block selection.
	;-------------------------------------------
	curWin = !d.window
	wset, (*statePtr).mapDrawWindowID
	(*statePtr).blockScreenLineNumbers = convert_coord( $
		(*statePtr).binaryCornerFile[*,1], $
		(*statePtr).binaryCornerFile[*,0], /data, /to_device )
	wset, curWin
end
; readOldBlockCornerFile	***Old***, superceeded by ***New***

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ readNewBlockCornerFile @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;-------------------------------------------
;-------------------------------------------
;-------------------------------------------
; SUPERCEEDES "readOldBlockCornerFile !!!
;------------------***----------------------
;-------------------------------------------
;-------------------------------------------
pro readNewBlockCornerFile, statePtr, path

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== readNewBlockCornerFile =========='
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		e_msg	= [											$
				routine_name,									$
				'Error Index: ' + STRTRIM( error_status, 2 ),					$
				'Error Message: ' + !ERR_STRING,						$
				'',										$
				'Suggestion: If attempting to read or write a file, then check permissions.',	$
				'',										$
				'Returning...' ]
		result	= DIALOG_MESSAGE( e_msg, /ERROR )
		RETURN
	ENDIF


	WIDGET_CONTROL, /HOURGLASS

;print,''
;print,''
;print,''&print,'readNewBlockCornerFile:  path = ',path&print,''
;st=systime(/sec)
;tt=st
	;-------------------------------------------
	; Only one path (swath) at a time is stored
	; in the state variable.
	;-------------------------------------------

;print,'readNewBlockCornerFile:  creating message'
	messageBase = widget_base( TITLE = 'Please Wait...' )
	messageLable = widget_label( messageBase, $
		value = 'Reading block corner file for path # '+strtrim(string(path),2) )
	widget_control, messageBase, /realize
;st=systime(/sec)-st&print,'seconds=',st&st=systime(/sec)&print,''

;print,'readNewBlockCornerFile:  reading ASCII file'
	;-------------------------------------------
	; Open the block corner file.
	;-------------------------------------------
	; Mike Bull, JPL 4-0439, constructs the
	; corner point file.  He puts the file
	; in this directory on jord.jpl.nasa.gov:
	; /data/geo_info/agp/block_file/
	; Block corners for all paths are contained
	; in just one file.
	;-------------------------------------------

;;;check environment variable for AGP_BLOCK_CORNERS location

blockCornerFileName = GETENV('AGP_BLOCK_CORNERS')
;print,'blockCornerFileName = ',blockCornerFileName
IF blockCornerFileName EQ '' THEN BEGIN
	asciiCornerFile	= *((*statePtr).agpPtr)
	headerSize	= (*statePtr).headerSize
	IF N_ELEMENTS(asciiCornerFile) LE 1 THEN BEGIN
		blockCornerFileName	= 'AGP_BLOCK_CORNERS'
		ret			= FINDFILE( blockCornerFileName, COUNT = cnt )
		IF cnt LE 0 THEN BEGIN
			msg	= [							$
					'AGP_BLOCK_CORNERS not found...',		$
					'This file is required to run misr_view.',	$
					'Make sure that this file is located within',	$
					'same directory as the rest of the misr_view',	$
					'files.  If running misr_view on a PC, make',	$
					'sure that you have set your current',		$
					'directory as detailed in the README file.' ]
			res	= DIALOG_MESSAGE( msg, /ERROR )
			RETURN
		ENDIF ELSE BEGIN
			OPENR, lun, blockCornerFileName, /GET_LUN
			headerSize = 3L
			asciiCornerFile = STRARR( 233L * 180L * 2L + headerSize )
			READF, lun, asciiCornerFile, FORMAT = '(a255)'
			CLOSE,lun
			FREE_LUN,lun
		ENDELSE
	ENDIF
ENDIF ELSE BEGIN
	OPENR, lun, blockCornerFileName, /GET_LUN
	headerSize = 3L
	asciiCornerFile = STRARR( 233L * 180L * 2L + headerSize )
	READF, lun, asciiCornerFile, FORMAT = '(a255)'
	CLOSE,lun
	FREE_LUN,lun
ENDELSE

;;;ckt,feb2000	blockCornerFileName = 'AGP_BLOCK_CORNERS'


;;;ckt,feb2000	OPENR, lun, blockCornerFileName, /GET_LUN
;;;ckt,feb2000	headerSize = 3L
;;;ckt,feb2000	asciiCornerFile = STRARR( 233L * 180L * 2L + headerSize )
;;;ckt,feb2000	READF, lun, asciiCornerFile, FORMAT = '(a255)'
;;;ckt,feb2000	CLOSE,lun
;;;ckt,feb2000	FREE_LUN,lun
;st=systime(/sec)-st&print,'seconds=',st&st=systime(/sec)&print,''

	;-------------------------------------------
	; Loop over 180 blocks.  The path number is
	; factored into the looping index so as to
	; hit only the data corresponding to the
	; desired path.
	; (THIS METHOD IS SOMEWHAT HARDWIRED TO THE
	; FORMATTING OF THE BLOCK CORNER FILE.  IF
	; THE FORMATTING CHANGES IN THE FUTURE, THEN
	; THIS LOOPING MAY HAVE TO CHANGE AS WELL.
	;-------------------------------------------
;print,'180L*(LONG(path)-1L) = ',180L*(LONG(path)-1L)
;print,'(180L*LONG(path))-1L = ',(180L*LONG(path))-1L
;st=systime(/sec)-st&print,'seconds=',st&st=systime(/sec)&print,''

;print,'readNewBlockCornerFile:  path = ',path
;print,'readNewBlockCornerFile:  LOOP START = 180L*(LONG(path)-1L) = ',180L*(LONG(path)-1L)
;print,'readNewBlockCornerFile:  LOOP  END  = (180L*LONG(path))-1L = ',(180L*LONG(path))-1L
	FOR i = 180L*(LONG(path)-1L), (180L*LONG(path))-1L DO BEGIN
;print,'readNewBlockCornerFile:  i = ',i

		;-------------------------------------------
		; Skipping over the header, pull out the lat
		; and lon combo (2 lines) for each block.
		;-------------------------------------------
		lat = asciiCornerFile[i+headerSize]
		lon = asciiCornerFile[i+headerSize+233L*180L]
		latElements = STR_SEP( STRCOMPRESS( lat ), ' ' )
		lonElements = STR_SEP( STRCOMPRESS( lon ), ' ' )

		;-------------------------------------------
		; The block number that is stored in the
		; third position (index=2) in the string
		; array (as separated by ' ') of each
		; original lat & lon string.  Block numbers
		; are one-based, whereas the indexing is
		; zero-based, so subtract 1.
		;-------------------------------------------
		blockNumber = latElements[2] - 1

		;-------------------------------------------
		; Center point.
		;-------------------------------------------
		(*statePtr).binaryCornerFile[blockNumber,0] = FLOAT( latElements[7] )
		(*statePtr).binaryCornerFile[blockNumber,1] = FLOAT( lonElements[7] )

		;-------------------------------------------
		; UpperLeft corner point.
		;-------------------------------------------
		(*statePtr).binaryCornerFile[blockNumber,2] = FLOAT( latElements[3] )
		(*statePtr).binaryCornerFile[blockNumber,3] = FLOAT( lonElements[3] )

		;-------------------------------------------
		; UpperRight corner point.
		;-------------------------------------------
		(*statePtr).binaryCornerFile[blockNumber,4] = FLOAT( latElements[4] )
		(*statePtr).binaryCornerFile[blockNumber,5] = FLOAT( lonElements[4] )

		;-------------------------------------------
		; LowerRight corner point.
		;-------------------------------------------
		(*statePtr).binaryCornerFile[blockNumber,6] = FLOAT( latElements[6] )
		(*statePtr).binaryCornerFile[blockNumber,7] = FLOAT( lonElements[6] )

		;-------------------------------------------
		; LowerLeft corner point.
		;-------------------------------------------
		(*statePtr).binaryCornerFile[blockNumber,8] = FLOAT( latElements[5] )
		(*statePtr).binaryCornerFile[blockNumber,9] = FLOAT( lonElements[5] )
	ENDFOR
;st=systime(/sec)-st&print,'seconds=',st&st=systime(/sec)&print,''

;print,'readNewBlockCornerFile:  getting center block location'
	;-------------------------------------------
	; Store the middle (closest to the equator,
	; block #90 [1-based]) block's longitude as
	; the map center longitude for map_set.
	;-------------------------------------------
	centerBlockNumber = N_ELEMENTS( (*statePtr).binaryCornerFile[*,0] ) / 2 - 1
	(*statePtr).mapCenterLatLon[1] = (*statePtr).binaryCornerFile[centerBlockNumber,1]
;print,'(*statePtr).mapCenterLatLon[1] = ',(*statePtr).mapCenterLatLon[1]

	if strupcase( (*statePtr).zoomStatus ) eq 'ZOOMED_IN' then begin
		;-------------------------------------------
		; Get the block numbers from the text
		; widgets.
		;-------------------------------------------
		widget_control, (*statePtr).whichBlocksText[0], get_value = upperBlock
		widget_control, (*statePtr).whichBlocksText[1], get_value = lowerBlock
		upperBlock = LONG( upperBlock[0] )
		lowerBlock = LONG( lowerBlock[0] )

		;-------------------------------------------
		; Determine which is the center block of the
		; range of chosen blocks.  For mid-latitude
		; zooms, the center point of this block will
		; be the center of the zoom.  For polar zooms,
		; this is ignored.
		;-------------------------------------------
		zoomBlock = upperBlock + (lowerBlock-upperBlock)/2 - 1

		;-------------------------------------------
		; Assign zoomblock's center point as the map
		; center point which will be used by map_set
		; in the "mapDraw" and "plotPath" routines.
		;-------------------------------------------
		(*statePtr).mapCenterLatLon = [ $
			(*statePtr).binaryCornerFile[zoomBlock,0], $
			(*statePtr).binaryCornerFile[zoomBlock,1] mod 360.0 ]
	endif

;st=systime(/sec)-st&print,'seconds=',st&st=systime(/sec)&print,''

;print,'readNewBlockCornerFile:  map_set'
	;-------------------------------------------
	; Set up the map coordinate system.
	;-------------------------------------------
	curWin = !d.window
	wset, (*statePtr).mapDrawWindowID
;print,'(*statePtr).zoomStatus = ',(*statePtr).zoomStatus
	if strupcase( (*statePtr).zoomStatus ) eq 'ZOOMED_IN' then begin

;print,'(*statePtr).mapCenterLatLon = ',(*statePtr).mapCenterLatLon
		map_set, (*statePtr).mapCenterLatLon[0], $
			(*statePtr).mapCenterLatLon[1], $
			0.0, $
			/lambert, /noborder, xmargin = 0.0, ymargin = 0.0, $
			scale = 600E5
	endif else begin
		map_set, (*statePtr).mapCenterLatLon[0], $
			(*statePtr).mapCenterLatLon[1], $
			0.0, $
			/lambert, /noborder, xmargin = 0.0, ymargin = 0.0
	endelse











	wset, curWin
;st=systime(/sec)-st&print,'seconds=',st&st=systime(/sec)&print,''

;print,'readNewBlockCornerFile:  convert_coord'
	;-------------------------------------------
	; Convert all of the block center points into
	; device coordinates.  By using the Lambert
	; map projection, the blocks will receive
	; monotonically decending line numbers which
	; will be used to correlate the cursor with
	; the block numbers during block selection.
	;-------------------------------------------
	curWin = !d.window
	wset, (*statePtr).mapDrawWindowID
	(*statePtr).blockScreenLineNumbers = convert_coord( $
		(*statePtr).binaryCornerFile[*,1], $
		(*statePtr).binaryCornerFile[*,0], /data, /to_device )
	wset, curWin

	(*statePtr).oldLineValue[0] = (*statePtr).blockScreenLineNumbers[1,(*statePtr).whichBlocks[0]-1]
	(*statePtr).oldLineValue[1] = (*statePtr).blockScreenLineNumbers[1,(*statePtr).whichBlocks[1]-1]
	if (*statePtr).oldLineValue[0] eq (*statePtr).oldLineValue[1] then $
		(*statePtr).oldLineValue[1] = (*statePtr).oldLineValue[0] - 1
;print,'readNewBlockCornerFile:  (*statePtr).oldLineValue[0] = ',(*statePtr).oldLineValue[0]
;print,'readNewBlockCornerFile:  (*statePtr).oldLineValue[1] = ',(*statePtr).oldLineValue[1]

	if widget_info( messageBase, /valid_id ) then $
		widget_control, messageBase, /destroy
;st=systime(/sec)-st&print,'seconds=',st&st=systime(/sec)&print,''
;print,'readNewBlockCornerFile:  leaving routine'
;print,'TOTAL SECONDS = ',systime(/sec)-tt&print,''
;print,''
;print,''
end
; readNewBlockCornerFile	***New***, superceeds ***Old***

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ grey_day @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro grey_day, statePtr

	;-------------------------------------------
	; Grey out the extra day buttons in short
	; months.  Also, choose an earier day if
	; currently selected day is invalid for the
	; newly selected month.
	;-------------------------------------------
	widget_control, (*statePtr).dayButton, get_value = day
	widget_control, (*statePtr).monthButton, get_value = month
	if month eq 'Apr' or month eq 'Jun' or month eq 'Sep' or month eq 'Nov' then begin
		widget_control, (*statePtr).allDayButtons[31-1], sensitive = 0
		widget_control, (*statePtr).allDayButtons[30-1], sensitive = 1
		widget_control, (*statePtr).allDayButtons[29-1], sensitive = 1

		if day eq 31 then $
			widget_control, (*statePtr).dayButton, set_value = '30'
	endif else begin
		widget_control, (*statePtr).allDayButtons[31-1], sensitive = 1
		widget_control, (*statePtr).allDayButtons[30-1], sensitive = 1
		widget_control, (*statePtr).allDayButtons[29-1], sensitive = 1
	endelse
	if month eq 'Feb' then begin
		widget_control, (*statePtr).allDayButtons[31-1], sensitive = 0
		widget_control, (*statePtr).allDayButtons[30-1], sensitive = 0
		if day eq 31 or day eq 30 then $
			day = '29'
		widget_control, (*statePtr).yearButton, get_value = year
		if ( year mod 4 ) ne 0 then begin
			widget_control, (*statePtr).allDayButtons[29-1], sensitive = 0
			if day eq 29 then $
				day = '28'
		endif
		widget_control, (*statePtr).dayButton, set_value = day

	endif
end
; grey_day

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ errorCheck_whichBlocks @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro errorCheck_whichBlocks, statePtr

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;  SPECIAL NOTE:
	;;  upperBlock IS THE NORTHERN-MOST AND HAS A LOWER NUMBER.  (startBlock)
	;;  lowerBlock IS THE SOUTHERN-MOST AND HAS A HIGHER NUMBER.  (endBlock)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;-------------------------------------------
	; Error check the text entries of the block
	; text widgets.  Do not allow the lower
	; block value to be less than the upper
	; block value.
	;-------------------------------------------
	widget_control, (*statePtr).whichBlocksText[0], get_value = upperBlock
	widget_control, (*statePtr).whichBlocksText[1], get_value = lowerBlock
	upperBlock = LONG( upperBlock[0] )
	lowerBlock = LONG( lowerBlock[0] )

	if upperBlock lt 1 then begin
		upperBlock = 1
		widget_control, (*statePtr).whichBlocksText[0], set_value = strtrim( upperBlock, 2 )
	endif
	if upperBlock gt 180 then begin
		upperBlock = 180
		widget_control, (*statePtr).whichBlocksText[0], set_value = strtrim( upperBlock, 2 )
	endif
	if lowerBlock gt 180 then begin
		lowerBlock = 180
		widget_control, (*statePtr).whichBlocksText[1], set_value = strtrim( lowerBlock, 2 )
	endif
	if lowerBlock lt upperBlock then begin
		lowerBlock = upperBlock
		widget_control, (*statePtr).whichBlocksText[1], set_value = strtrim( lowerBlock, 2 )
	endif

	(*statePtr).whichBlocks = [upperBlock,lowerBlock]
;These 2 lines of code only seem to screw things up.
;Leave them commented out unless they're determined to be useful.
;JRH Oct-18-1999.
;	(*statePtr).oldLineValue[0] = (*statePtr).blockScreenLineNumbers[1,(*statePtr).whichBlocks[0]-1]
;	(*statePtr).oldLineValue[1] = (*statePtr).blockScreenLineNumbers[1,(*statePtr).whichBlocks[1]-1]
	if (*statePtr).oldLineValue[0] eq (*statePtr).oldLineValue[1] then $
		(*statePtr).oldLineValue[1] = (*statePtr).oldLineValue[0] - 1

	;-------------------------------------------
	; Update latitude and longitude label values
	; for the UPPER block lat/lon label widget.
	;-------------------------------------------
	specimen = (*statePtr).binaryCornerFile[upperBlock-1,0]
	oneDecimal = LONG( ( specimen mod 1 ) * 10 )
	if oneDecimal lt 0 then oneDecimal = 0 - oneDecimal
	oneDecimalString = strtrim( oneDecimal, 2 )
	latString = strtrim( LONG( specimen ), 2 ) + '.' + oneDecimalString
	specimen = (*statePtr).binaryCornerFile[upperBlock-1,1] mod 360.0
	while specimen gt 180.0 do $
		specimen = specimen - 360.0
	while specimen le -180.0 do $
		specimen = specimen + 360.0
	oneDecimal = LONG( ( specimen mod 1 ) * 10 )
	if oneDecimal lt 0 then oneDecimal = 0 - oneDecimal
	oneDecimalString = strtrim( oneDecimal, 2 )
	lonString = strtrim( LONG( specimen), 2 ) + '.' + oneDecimalString
	widget_control, (*statePtr).latLimitValue[0], set_value = latString + ', ' + lonString
	;-------------------------------------------
	; Update latitude and longitude label values
	; for the LOWER block lat/lon label widget.
	;-------------------------------------------
	specimen = (*statePtr).binaryCornerFile[lowerBlock-1,0]
	oneDecimal = LONG( ( specimen mod 1 ) * 10 )
	if oneDecimal lt 0 then oneDecimal = 0 - oneDecimal
	oneDecimalString = strtrim( oneDecimal, 2 )
	latString = strtrim( LONG( specimen ), 2 ) + '.' + oneDecimalString
	specimen = (*statePtr).binaryCornerFile[lowerBlock-1,1] mod 360.0
	while specimen gt 180.0 do $
		specimen = specimen - 360.0
	while specimen le -180.0 do $
		specimen = specimen + 360.0
	oneDecimal = LONG( ( specimen mod 1 ) * 10 )
	if oneDecimal lt 0 then oneDecimal = 0 - oneDecimal
	oneDecimalString = strtrim( oneDecimal, 2 )
	lonString = strtrim( LONG( specimen), 2 ) + '.' + oneDecimalString
	widget_control, (*statePtr).latLimitValue[1], set_value = latString + ', ' + lonString

	;-------------------------------------------
	; Update the total number of blocks selected
	; and put that value in the text widget.
	;-------------------------------------------
	nblocks = LONG( (*statePtr).whichBlocks[1] ) - LONG( (*statePtr).whichBlocks[0] ) + 1
	zpad = ''
	if nblocks lt 100 then zpad = ' '
	if nblocks lt 10 then zpad = '  '
	numberOfBlocks = zpad + strtrim( nblocks, 2 )
	widget_control, (*statePtr).numBlocksValueLabel, set_value = numberOfBlocks
end
; errorCheck_whichBlocks

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ zoomButtonEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro zoomButtonEvent, event
	widget_control, /hourglass

	upperBase = widget_info( event.top, /child )
	widget_control, upperBase, get_uvalue = statePtr

	;-------------------------------------------
	; Error check the block numbers in the text
	; widgets.  Do this in case and invalid
	; value was entered WITHOUT being registered
	; by using the <CR> key.
	;-------------------------------------------
	errorCheck_whichBlocks, statePtr

	;-------------------------------------------
	; Get the block numbers from the text
	; widgets.
	;-------------------------------------------
	widget_control, (*statePtr).whichBlocksText[0], get_value = upperBlock
	widget_control, (*statePtr).whichBlocksText[1], get_value = lowerBlock
	upperBlock = LONG( upperBlock[0] )
	lowerBlock = LONG( lowerBlock[0] )

	;-------------------------------------------
	; Get the value of the zoom button which determines
	; whether zoom-in or zoom-out is to be performed.
	;-------------------------------------------
	widget_control, event.id, get_value = zoomRequest

	;-------------------------------------------
	; Get the path number from the widget label.
	;-------------------------------------------
	widget_control, (*statePtr).pathText, get_value = pathNumber

	;-------------------------------------------
	; Do a zoom-in.
	;
	; Polar zooms center on the pole, and mid-latitude
	; zooms center on the center block of the range of
	; chosen blocks.
	;-------------------------------------------
	case 1 of
		strupcase(zoomRequest) eq 'IN' and event.select : begin
			;-------------------------------------------
			; Check to see if any blocks have been chosen,
			; the default on startup is no chosen blocks.
			;-------------------------------------------
			widget_control, (*statePtr).numBlocksValueLabel, get_value = nBlocks
			;-------------------------------------------
			; If not, then return.
			;-------------------------------------------
			if nBlocks[0] eq 0 then return

			;-------------------------------------------
			; Determine which is the center block of the
			; range of chosen blocks.  For mid-latitude
			; zooms, the center point of this block will
			; be the center of the zoom.  For polar zooms,
			; this is ignored.
			;-------------------------------------------
			zoomBlock = upperBlock + (lowerBlock-upperBlock)/2 - 1

			;-------------------------------------------
			; Assign zoomblock's center point as the map
			; center point which will be used by map_set
			; in the "mapDraw" and "plotPath" routines.
			;-------------------------------------------
			(*statePtr).mapCenterLatLon = [ $
				(*statePtr).binaryCornerFile[zoomBlock,0], $
				(*statePtr).binaryCornerFile[zoomBlock,1] mod 360.0 ]

			;-------------------------------------------
			; Determine if a polar zoom is required, and
			; if so then which one, north or south pole.
			; Set the map center coordinates to the pole
			; around which the zoom will occur.  The
;;; USED TO BE TWOFOLD --- COULDN'T GET IT TO WORK CORRECTLY SO THE SECOND CRITERIA IS COMMENTED OUT.
			; criteria for polar zoomis twofold:  center
			; latitude (between blocks) must be within
			; 30 degrees of the pole (to catch the tail
			; ends of the obit) and the block number
			; must fall within the last 30 blocks of the
			; orbit tail.
			;-------------------------------------------

			blockNearestSouthPole = (where( (*statePtr).binaryCornerFile[*,0] $
						eq min( (*statePtr).binaryCornerFile[*,0] ) ))[0]
			blockNearestNorthPole = (where( (*statePtr).binaryCornerFile[*,0] $
						eq max( (*statePtr).binaryCornerFile[*,0] ) ))[0]

;;;;;
;;;;; UNDER DEVELOPMENT TO FINETUNE THE DECISION TO POLAR OR NOT TO POLAR ZOOM.
;;;;;
;print,''
;print,'(*statePtr).mapCenterLatLon[0] = ',(*statePtr).mapCenterLatLon[0]
;print,'upperBlock = ',upperBlock
;print,'lowerBlock = ',lowerBlock
;print,'blockNearestNouthPole = ',blockNearestNorthPole
;print,'blockNearestSouthPole = ',blockNearestSouthPole
			if (*statePtr).mapCenterLatLon[0] - 10.0 lt -90.0 $
			or ( lowerBlock ge blockNearestSouthPole $
			and upperBlock ge blockNearestSouthPole - 25 ) $
			then begin
;print,''
;print,'zooming to South Pole'
;print,''
				(*statePtr).mapCenterLatLon[0] = -90.0
				centerBlockNumber = N_ELEMENTS( (*statePtr).binaryCornerFile[*,0] ) / 2 - 1
				(*statePtr).mapCenterLatLon[1] = (*statePtr).binaryCornerFile[centerBlockNumber,1] mod 360.0
			endif
			if (*statePtr).mapCenterLatLon[0] + 10.0 gt 90.0 $
			or ( lowerBlock le blockNearestNorthPole $
			and upperBlock le blockNearestNorthPole + 25 ) $
			then begin
;print,''
;print,'zooming to North Pole'
;print,''
				(*statePtr).mapCenterLatLon[0] = 90.0
				centerBlockNumber = N_ELEMENTS( (*statePtr).binaryCornerFile[*,0] ) / 2 - 1
				(*statePtr).mapCenterLatLon[1] = (*statePtr).binaryCornerFile[centerBlockNumber,1] mod 360.0
			endif
;;;;;
;;;;; END DEVELOPMENT.  (Problem remains of having more blocks than will fit into the zoomed map.  User discretion advised.)
;;;;;

			;-------------------------------------------
			; Check to see if the polar case was found to
			; be required, if so then do a polar zoom.
			; If not, then do a mid-latitude zoom.
			;-------------------------------------------
			if (*statePtr).mapCenterLatLon[0] eq -90.0 $
			or (*statePtr).mapCenterLatLon[0] eq 90.0 then begin

				;-------------------------------------------
				; Zoom into polar regions.
				;
				; Polar zooms are always centered on the pole.
				; SCALE is set such that blocks will get plotted
				; at about the same resolution as with the
				; mid-latitude zooms.
				;-------------------------------------------

				drawMap, statePtr, scale = 600E5

				;-------------------------------------------
				; Redraw the path.
				;-------------------------------------------
				plotPath, statePtr

plotBlockIdx = 16679875
IF (*statePtr).eight_bit_display THEN plotBlockIdx = (*statePtr).selectedBlockIdx

				plotBlocks, statePtr, plotBlockIdx

			endif else begin

				;-------------------------------------------
				; Zoom into mid-latitudes.
				;
				; Mid-latitude zooms are always centered on
				; the center block of the range of chosen blocks.f
				; Limits are set to encompass a 50x50 degree square
				; which results in the blocks getting plotted
				; at about the same resolution as with the
				; polar zooms.
				;-------------------------------------------

				drawMap, statePtr, limit = [ $
					(*statePtr).binaryCornerFile[zoomBlock,0] - 25, $
					( (*statePtr).binaryCornerFile[zoomBlock,1] - 25 ) mod 360.0, $
					(*statePtr).binaryCornerFile[zoomBlock,0] + 25, $
					( (*statePtr).binaryCornerFile[zoomBlock,1] + 25 ) mod 360.0 ]

				;-------------------------------------------
				; Redraw the path.
				;-------------------------------------------
				plotPath, statePtr

plotBlockIdx = 16679875
IF (*statePtr).eight_bit_display THEN plotBlockIdx = (*statePtr).selectedBlockIdx

				plotBlocks, statePtr, plotBlockIdx
			endelse

			;-------------------------------------------
			; Convert list to zoomed coordinates.  The
			; list will be converted back to full-earth
			; coordinates when zoom-out occurs.
			; (Remember that the path (swath) selection
			; radio button is disabled during zoom-in.)
			;-------------------------------------------
			curWin = !d.window
			wset, (*statePtr).mapDrawWindowID
;print,'(*statePtr).binaryCornerFile[*,1] = ',(*statePtr).binaryCornerFile[*,1]
;print,'(*statePtr).binaryCornerFile[*,0] = ',(*statePtr).binaryCornerFile[*,0]
			(*statePtr).blockScreenLineNumbers = convert_coord( $
				(*statePtr).binaryCornerFile[*,1] mod 360.0, $
				(*statePtr).binaryCornerFile[*,0], $
				/data, /to_device )
			wset, curWin

			;-------------------------------------------
			; Draw block chooser lines on zoomed-in map.
			;-------------------------------------------
			curWin = !d.window
			wset, (*statePtr).mapDrawWindowID
;print,'(*statePtr).binaryCornerFile[upperBlock - 1,1] = ',(*statePtr).binaryCornerFile[upperBlock - 1,1]
;print,'(*statePtr).binaryCornerFile[upperBlock - 1,0] = ',(*statePtr).binaryCornerFile[upperBlock - 1,0]
			upperSampLine = convert_coord( $
				(*statePtr).binaryCornerFile[upperBlock - 1,1] mod 360.0, $
				(*statePtr).binaryCornerFile[upperBlock - 1,0], $
				/data, /to_device )
;print,'(*statePtr).binaryCornerFile[lowerBlock - 1,1] = ',(*statePtr).binaryCornerFile[lowerBlock - 1,1]
;print,'(*statePtr).binaryCornerFile[lowerBlock - 1,0] = ',(*statePtr).binaryCornerFile[lowerBlock - 1,0]
			lowerSampLine = convert_coord( $
				(*statePtr).binaryCornerFile[lowerBlock - 1,1] mod 360.0, $
				(*statePtr).binaryCornerFile[lowerBlock - 1,0], $
				/data, /to_device )
			wset, curWin
			;-------------------------------------------
			; Protect against accessing lines outside of
			; the screen space.
			;-------------------------------------------
			if upperSampLine[1] gt (*statePtr).mapY-1 then $
				upperSampLine[1] = (*statePtr).mapY-1
			if lowerSampLine[1] lt 0 then lowerSampLine[1] = 0
			if lowerSampLine[1] eq upperSampLine[1] then $
				lowerSampLine[1] = lowerSampLine[1] - 1
			(*statePtr).oldLineValue[0] = upperSampLine[1]
			(*statePtr).oldLineValue[1] = lowerSampLine[1]

			;-------------------------------------------
			; Re-save the line buffers for the block
			; chooser.
			;-------------------------------------------
			refresh_line_buffer, statePtr

			curWin = !d.window
			wset, (*statePtr).mapDrawWindowID
			device, copy = [ 0, 0, (*statePtr).mapX, 1, 0, upperSampLine[1], (*statePtr).linePixMap[0] ]
			device, copy = [ 0, 0, (*statePtr).mapX, 1, 0, lowerSampLine[1], (*statePtr).linePixMap[0] ]
			wset, curWin

			(*statePtr).zoomStatus = 'ZOOMED_IN'
		end

		;-------------------------------------------
		; Do a zoom-out back to full-globe map coverage.
		;-------------------------------------------
		strupcase(zoomRequest) eq 'OUT' and event.select : begin
			;-------------------------------------------
			; Re-set the map center coordinates back to
			; the block that is closest to the equator.
			;-------------------------------------------
			centerBlockNumber = N_ELEMENTS( (*statePtr).binaryCornerFile[*,0] ) / 2 - 1
			(*statePtr).mapCenterLatLon = [ 0, (*statePtr).binaryCornerFile[centerBlockNumber,1] mod 360.0 ]

			;-------------------------------------------
			; Re-draw the full-earth map.
			;-------------------------------------------
			drawMap, statePtr

			;-------------------------------------------
			; Re-plot the complete path (swath).
			;-------------------------------------------
			plotPath, statePtr

plotBlockIdx = 16679875
IF (*statePtr).eight_bit_display THEN plotBlockIdx = (*statePtr).selectedBlockIdx

			plotBlocks, statePtr, plotBlockIdx

			;-------------------------------------------
			; Convert list back to original full-earth
			; coordinates.
			;-------------------------------------------
			curWin = !d.window
			wset, (*statePtr).mapDrawWindowID
			(*statePtr).blockScreenLineNumbers = convert_coord( $
				(*statePtr).binaryCornerFile[*,1] mod 360.0, $
				(*statePtr).binaryCornerFile[*,0], /data, /to_device )
			wset, curWin

			;-------------------------------------------
			; Draw block chooser lines on zoomed-out map.
			; Subtract 1 from the block numbers obtained
			; from the text widgets becase these are 1-
			; based and the binaryCornerFile is 0-based.
			;-------------------------------------------
			curWin = !d.window
			wset, (*statePtr).mapDrawWindowID
			upperSampLine = convert_coord( $
				(*statePtr).binaryCornerFile[upperBlock-1,1] mod 360.0, $
				(*statePtr).binaryCornerFile[upperBlock-1,0], $
				/data, /to_device )
			lowerSampLine = convert_coord( $
				(*statePtr).binaryCornerFile[lowerBlock-1,1] mod 360.0, $
				(*statePtr).binaryCornerFile[lowerBlock-1,0], $
				/data, /to_device )
			wset, curWin
			if lowerSampLine[1] eq upperSampLine[1] then $
				lowerSampLine[1] = lowerSampLine[1] - 1
			(*statePtr).oldLineValue[0] = upperSampLine[1]
			(*statePtr).oldLineValue[1] = lowerSampLine[1]

			;-------------------------------------------
			; Re-save the line buffers for the block
			; chooser.
			;-------------------------------------------
			refresh_line_buffer, statePtr

			curWin = !d.window
			wset, (*statePtr).mapDrawWindowID
			device, copy = [ 0, 0, (*statePtr).mapX, 1, 0, upperSampLine[1], (*statePtr).linePixMap[0] ]
			device, copy = [ 0, 0, (*statePtr).mapX, 1, 0, lowerSampLine[1], (*statePtr).linePixMap[0] ]
			wset, curWin

			(*statePtr).zoomStatus = 'ZOOMED_OUT'
		end
		else : begin
			; "else" to prevent crash if no case match is found.
		end
	endcase
end
; zoomButtonEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ refresh_line_buffer @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro refresh_line_buffer, statePtr
;print,'refresh_line_buffer:  (*statePtr).oldLineValue[0] = ',(*statePtr).oldLineValue[0]
;print,'refresh_line_buffer:  (*statePtr).oldLineValue[1] = ',(*statePtr).oldLineValue[1]
	;-------------------------------------------
	; Re-save the block choosing lines now that
	; the map has been rotated to a new orbit
	; position.  Copy the lines from the newly
	; rotated map into the line buffers.
	;-------------------------------------------
	curWin = !d.window
	wset, (*statePtr).bufferPixMap[0]
	if (*statePtr).oldLineValue[0] le (*statePtr).mapX $
	and (*statePtr).oldLineValue[0] ge 0 $
	then $
		device, copy = [			$
			0,				$
			(*statePtr).oldLineValue[0],	$
			(*statePtr).mapX,		$
			1,				$
			0,				$
			0,				$
			(*statePtr).mapDrawWindowID ]

	wset, (*statePtr).bufferPixMap[1]
	if (*statePtr).oldLineValue[1] le (*statePtr).mapX $
	and (*statePtr).oldLineValue[1] ge 0 $
	then $
		device, copy = [			$
			0,				$
			(*statePtr).oldLineValue[1],	$
			(*statePtr).mapX,		$
			1,				$
			0,				$
			0,				$
			(*statePtr).mapDrawWindowID ]
	wset, curWin
end
; refresh_line_buffer

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ EightBit_RestoreColorMap @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO EightBit_RestoreColorMap, statePtr
   n_reserved_colors = N_ELEMENTS((*statePtr).red_reserved)
   reserved_r        = (*statePtr).red_reserved
   reserved_g        = (*statePtr).grn_reserved
   reserved_b        = (*statePtr).blu_reserved

;print,'EightBit_RestoreColorMap -- LOADCT, 0'
   LOADCT, 0
   TVLCT, r, g, b, /GET

   out_r = r
   out_g = g
   out_b = b

   out_r[n_reserved_colors:N_ELEMENTS(r)-1] = CONGRID(r, N_ELEMENTS(r) - n_reserved_colors)
   out_g[n_reserved_colors:N_ELEMENTS(r)-1] = CONGRID(g, N_ELEMENTS(g) - n_reserved_colors)
   out_b[n_reserved_colors:N_ELEMENTS(r)-1] = CONGRID(b, N_ELEMENTS(b) - n_reserved_colors)

   out_r[0:n_reserved_colors-1] = reserved_r
   out_g[0:n_reserved_colors-1] = reserved_g
   out_b[0:n_reserved_colors-1] = reserved_b

;print,'blockchooser_before_realize.pro EightBit_RestoreColorMap -- TVLCT, out_r, out_g, out_b'
;print,'blockchooser_before_realize.pro EightBit_RestoreColorMap -- out_r = ',out_r
;print,'blockchooser_before_realize.pro EightBit_RestoreColorMap -- out_g = ',out_g
;print,'blockchooser_before_realize.pro EightBit_RestoreColorMap -- out_b = ',out_b
   TVLCT, out_r, out_g, out_b
END
; EightBit_RestoreColorMap

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ drawMap @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro drawMap, statePtr, _extra = extra

	curWin = !d.window
	wset, (*statePtr).mapDrawWindowID

	cont_idx = 7528447
	IF (*statePtr).eight_bit_display THEN BEGIN
		EightBit_RestoreColorMap, statePtr
		ERASE
		cont_idx = (*statePtr).contIdx ;[255,223,114] (24_bit_index = 7528447)
	ENDIF

	;-------------------------------------------
	; Determine if extra is being used, if so
	; then determine what tag names it includes,
	; create an execute string that constructs a
	; new extra structure containing all desired
	; tags, and save the new extra structure in
	; the statePtr.
	; If extra is not being used, then put only
	; the MISR-generic keywords into the extra
	; structure, and save it in the statePtr.
	;
	; The pupose of storing the extra structure
	; in the statePtr is to make a phantom call
	; to MAP_SET after a GEOREF_IMAGE object is
	; instantiated.  This is necessary because
	; when the object realizes a new WIDGET_DRAW
	; the current MAP_SET settings get unset.
	;-------------------------------------------
	if (size(extra))[1] gt 0 then begin
	
;========== remove the EXECUTE =============
;ckt,aug2004
;ckt,aug2004		tNames = tag_names(extra)
;ckt,aug2004		str = ''
;ckt,aug2004		for i = 0, n_elements(tnames)-2 do $
;ckt,aug2004			str = str + tNames[i] + ':extra.' + tNames[i]
;ckt,aug2004		str = str + tNames[n_elements(tnames)-1] + ':extra.' + tNames[n_elements(tnames)-1]
;ckt,aug2004		str = 'extra = {lambert:1,noborder:1,xmargin:0.0,ymargin:0.0,'+str+'}'
;ckt,aug2004		success = execute(str)
;ckt,aug2004		(*statePtr).mapSetStruct = ptr_new( extra )
;===========================================
		
		
;========== replacement code =============
		tNames			= TAG_NAMES( extra )
		str			= ''
		extra			= CREATE_STRUCT(	$
						'lambert',1,	$
						'noborder',1,	$
						'xmargin',0.0,	$
						'ymargin',0.0,	$
						extra )
		(*statePtr).mapSetStruct= PTR_NEW( extra )
;===========================================

	endif else begin
		extra = {lambert:1,noborder:1,xmargin:0.0,ymargin:0.0}
		(*statePtr).mapSetStruct = ptr_new( extra )
	endelse

	;-------------------------------------------
	; Project and display world elevation (if
	; available).  Check availability by checking
	; to see if elevationMap has more than one
	; element.  If there is only 1 element, then
	; it's not available.
	;-------------------------------------------
	if n_elements( (*statePtr).elevationMap ) gt 1 then begin

		map_set, (*statePtr).mapCenterLatLon[0], $
			(*statePtr).mapCenterLatLon[1], $
			0.0, /noerase, $
			_extra = *((*statePtr).mapSetStruct)
		mappedEle = map_image( (*statePtr).elevationMap, stX, stY, /bilinear, compress = 2,	$
			MISSING = N_ELEMENTS( (*statePtr).red_reserved ) )
		tv, mappedEle, stX, stY
		map_set, (*statePtr).mapCenterLatLon[0], $
			(*statePtr).mapCenterLatLon[1], $
			0.0, $
			/horizon, /grid, /label, /continents, $
			con_color = cont_idx, $
			/noerase, _extra = extra

	endif else begin ;use erase

		;-------------------------------------------
		; Use a single call to map_set to set the
		; map coordinates and to draw the map.
		; If zooming, "extra" will contain either
		; the SCALE for polar zooms, or LIMITS for
		; mid-latitude zooms.
		;-------------------------------------------
		map_set, (*statePtr).mapCenterLatLon[0], $
			(*statePtr).mapCenterLatLon[1], $
			0.0, $
			/horizon, /grid, /label, /continents, $
			con_color = cont_idx, $
			_extra = extra
	endelse
;print,'drawMap:  (*statePtr).mapCenterLatLon[0],(*statePtr).mapCenterLatLon[1] = ',(*statePtr).mapCenterLatLon[0],(*statePtr).mapCenterLatLon[1]
	wset, curWin
end
; drawMap

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ plotPath @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro plotPath, statePtr

	curWin = !d.window
	wset, (*statePtr).mapDrawWindowID

;	;-------------------------------------------
;	; Error check the block numbers in the text
;	; widgets.  Do this in case and invalid
;	; value was entered WITHOUT being registered
;	; by using the <CR> key.
;	;-------------------------------------------
;	errorCheck_whichBlocks, statePtr

	block_idx = 16754005
	IF (*statePtr).eight_bit_display THEN $
		block_idx = (*statePtr).blockIdx 	;[85,165,255] (24_bit_index = 16754005)

	;-------------------------------------------
	; Draw the block outlines.
	;-------------------------------------------
	for i = 0, 179 do begin
		;-------------------------------------------
		; Draw lines connecting the upper and lower
		; left corners of each block.
		;-------------------------------------------
		plots, [ (*statePtr).binaryCornerFile[i,3] mod 360.0, $
			(*statePtr).binaryCornerFile[i,9] mod 360.0 ], $
			[ (*statePtr).binaryCornerFile[i,2], $
			(*statePtr).binaryCornerFile[i,8] ], $
			color = block_idx

		;-------------------------------------------
		; Draw lines connecting the upper and lower
		; right corners of each block.
		;-------------------------------------------
		plots, [ (*statePtr).binaryCornerFile[i,5] mod 360.0, $
			(*statePtr).binaryCornerFile[i,7] mod 360.0 ], $
			[ (*statePtr).binaryCornerFile[i,4], $
			(*statePtr).binaryCornerFile[i,6] ], $
			color = block_idx

		;-------------------------------------------
		; Draw line across top of each block.
		;-------------------------------------------
		plots, [ (*statePtr).binaryCornerFile[i,3] mod 360.0, $
			(*statePtr).binaryCornerFile[i,5] mod 360.0 ], $
			[ (*statePtr).binaryCornerFile[i,2], $
			(*statePtr).binaryCornerFile[i,4] ], $
			color = block_idx

		;-------------------------------------------
		; Draw line across bottom of each block.
		;-------------------------------------------
		plots, [ (*statePtr).binaryCornerFile[i,7] mod 360.0, $
			(*statePtr).binaryCornerFile[i,9] mod 360.0 ], $
			[ (*statePtr).binaryCornerFile[i,6], $
			(*statePtr).binaryCornerFile[i,8] ], $
			color = block_idx
	endfor

	wset, curWin
end
; plotPath

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ plotBlocks @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro plotBlocks, statePtr, colorIndex

	curWin = !d.window
	wset, (*statePtr).mapDrawWindowID

	;-------------------------------------------
	; Draw the block outlines.  "whichBlocks" is
	; 1 based, whereas "binaryCornerFile" is
	; 0 based, so subtract 1.
	;-------------------------------------------
	for i = LONG( (*statePtr).whichBlocks[0] ) - 1, LONG( (*statePtr).whichBlocks[1] ) - 1 do begin
		;-------------------------------------------
		; Draw lines connecting the upper and lower
		; left corners of each block.
		;-------------------------------------------
		plots, [ (*statePtr).binaryCornerFile[i,3] mod 360.0, $
			(*statePtr).binaryCornerFile[i,9] mod 360.0 ], $
			[ (*statePtr).binaryCornerFile[i,2], $
			(*statePtr).binaryCornerFile[i,8] ], $
			color = colorIndex

		;-------------------------------------------
		; Draw lines connecting the upper and lower
		; right corners of each block.
		;-------------------------------------------
		plots, [ (*statePtr).binaryCornerFile[i,5] mod 360.0, $
			(*statePtr).binaryCornerFile[i,7] mod 360.0 ], $
			[ (*statePtr).binaryCornerFile[i,4], $
			(*statePtr).binaryCornerFile[i,6] ], $
			color = colorIndex

		;-------------------------------------------
		; Draw line across top of each block.
		;-------------------------------------------
		plots, [ (*statePtr).binaryCornerFile[i,3] mod 360.0, $
			(*statePtr).binaryCornerFile[i,5] mod 360.0 ], $
			[ (*statePtr).binaryCornerFile[i,2], $
			(*statePtr).binaryCornerFile[i,4] ], $
			color = colorIndex

		;-------------------------------------------
		; Draw line across bottom of each block.
		;-------------------------------------------
		plots, [ (*statePtr).binaryCornerFile[i,7] mod 360.0, $
			(*statePtr).binaryCornerFile[i,9] mod 360.0 ], $
			[ (*statePtr).binaryCornerFile[i,6], $
			(*statePtr).binaryCornerFile[i,8] ], $
			color = colorIndex
	endfor

	wset, curWin
end
; plotBlocks

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ mapModeChooseEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro mapModeChooseEvent, event
	widget_control, /hourglass
	upperBase = widget_info( event.top, /child )
	widget_control, upperBase, get_uvalue = statePtr
;help,event.top
;help,upperBase
;help,statePtr

	if strupcase( (*statePtr).zoomStatus ) eq 'ZOOMED_IN' then begin
		widget_control, (*statePtr).zoomOutButton, /set_button
		zoomButtonEvent, { top:event.top, id:(*statePtr).zoomOutButton, handler:'zoomButtonEvent', select:1 }
	endif

	if event.id eq (*statePtr).mapModeChooseOrbitButton and event.select then begin

		;-------------------------------------------
		; Enable path (swath) choosing, and disable
		; block choosing.
		;-------------------------------------------
		widget_control, (*statePtr).pathAndOrbitChooseBase, sensitive = 1
		widget_control, (*statePtr).blocksChooseBase, sensitive = 0
		(*statePtr).chooseOrbitOrBlocksFlag = 'orbit'

		;-------------------------------------------
		; Erase the block choosing lines.
		;-------------------------------------------
		curWin = !d.window
		wset, (*statePtr).mapDrawWindowID
		device, copy = [			$
			0,				$
			0,				$
			(*statePtr).mapX,		$
			1,				$
			0,				$
			(*statePtr).oldLineValue[0],	$
			(*statePtr).bufferPixMap[0] ]
		device, copy = [			$
			0,				$
			0,				$
			(*statePtr).mapX,		$
			1,				$
			0,				$
			(*statePtr).oldLineValue[1],	$
			(*statePtr).bufferPixMap[1] ]
		wset, curWin

		;-------------------------------------------
		; Error check the block numbers in the text
		; widgets.  Do this in case and invalid
		; value was entered WITHOUT being registered
		; by using the <CR> key.
		;-------------------------------------------
		errorCheck_whichBlocks, statePtr

		;-------------------------------------------
		; Redraw the map.
		;-------------------------------------------
		drawmap, statePtr

		;-------------------------------------------
		; Redraw the path.
		;-------------------------------------------
		plotPath, statePtr

		refresh_line_buffer, statePtr
	endif

	if event.id eq (*statePtr).mapModeChooseBlocksButton and event.select then begin

		upperBase = widget_info( event.top, /child )
		widget_control, upperBase, get_uvalue = statePtr

		;-------------------------------------------
		; Enable block choosing, and disable path
		; (swath) choosing.
		;-------------------------------------------
		widget_control, (*statePtr).pathAndOrbitChooseBase, sensitive = 0
		widget_control, (*statePtr).blocksChooseBase, sensitive = 1
		(*statePtr).chooseOrbitOrBlocksFlag = 'blocks'

		;-------------------------------------------
		; Redraw the map.
		;-------------------------------------------
		drawmap, statePtr

		;-------------------------------------------
		; Redraw the path.
		;-------------------------------------------
		plotPath, statePtr

		;-------------------------------------------
		; Re-draw the block choosing lines in their
		; correct locations.
		;-------------------------------------------
		curWin = !d.window
		wset, (*statePtr).mapDrawWindowID
		device, copy = [			$
			0,				$
			0,				$
			(*statePtr).mapX,		$
			1,				$
			0,				$
			(*statePtr).oldLineValue[0],	$
			(*statePtr).linePixMap[0] ]
		device, copy = [			$
			0,				$
			0,				$
			(*statePtr).mapX,		$
			1,				$
			0,				$
			(*statePtr).oldLineValue[1],	$
			(*statePtr).linePixMap[0] ]
		wset, curWin

		plotBlockIdx = 16679875
		IF (*statePtr).eight_bit_display THEN $
			plotBlockIdx = (*statePtr).selectedBlockIdx

		plotBlocks, statePtr, plotBlockIdx

		widget_control, (*statePtr).whichBlocksText[0], $
			set_value = strtrim( (*statePtr).whichBlocks[0], 2 )
		widget_control, (*statePtr).whichBlocksText[1], $
			set_value = strtrim( (*statePtr).whichBlocks[1], 2 )

		;-------------------------------------------
		; Determine total number of blocks selected
		; and put that value in the text widget.
		;-------------------------------------------
		nblocks = LONG( (*statePtr).whichBlocks[1] ) - LONG( (*statePtr).whichBlocks[0] ) + 1
		zpad = ''
		if nblocks lt 100 then zpad = ' '
		if nblocks lt 10 then zpad = '  '
		numberOfBlocks = zpad + strtrim( nblocks, 2 )
		widget_control, (*statePtr).numBlocksValueLabel, set_value = numberOfBlocks
	endif
end
; mapModeChooseEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ misrChooseButtonEvent @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro misrChooseButtonEvent, event
	widget_control, /hourglass
	upperBase = widget_info( event.top, /child )
	widget_control, upperBase, get_uvalue = statePtr
	widget_control, (*statePtr).lowerBase, get_uvalue = lowerBaseStatePtr

	if event.id eq (*statePtr).misrChooseButton and event.select then begin

		;-------------------------------------------
		; Change billboards.
		;-------------------------------------------
		widget_control, (*statePtr).airMisrFileBase, map = 0
		widget_control, (*statePtr).chooseBase, map = 1

		;-------------------------------------------
		; Error check the block numbers in the text
		; widgets.  Do this in case and invalid
		; value was entered WITHOUT being registered
		; by using the <CR> key.
		;-------------------------------------------
		errorCheck_whichBlocks, statePtr

		;-------------------------------------------
		; Turn off the AirMISR flag.
		;-------------------------------------------
		(*statePtr).airMisrFlag = 0

		;-------------------------------------------
		; Put most recent orbit into text widget.
		;-------------------------------------------
		widget_control, (*statePtr).orbitText, get_value = orbit
		widget_control, (*statePtr).pathText, get_value = path
		unset_planes, statePtr, LONG( orbit[0] ), LONG( path[0] )

		;-------------------------------------------
		; Re-setup the data button by calling the
		; event handler for the orbit text widget.
		; This redraws the map and replots the path.
		;-------------------------------------------
		orbitTextEvent, {top:event.top, id:event.id, handler:event.handler}

		;-------------------------------------------
		; Restore default values to the along-track
		; and across-track resolution labels, and
		; activate the selector buttons.
		;-------------------------------------------
		widget_control, (*lowerBaseStatePtr).altResLabel, set_value = '1100m'
		widget_control, (*lowerBaseStatePtr).actResLabel, set_value = '1100m'
		widget_control, (*lowerBaseStatePtr).resBase1, sensitive = 1

widget_control,widget_info((*lowerBaseStatePtr).resBase1,/child),/sensitive
widget_control,widget_info(widget_info((*lowerBaseStatePtr).resBase1,/child),/child),/sensitive

		widget_control, (*lowerBaseStatePtr).resBase2, sensitive = 1

		;-------------------------------------------
		; Redraw the map.
		;-------------------------------------------
		drawMap, statePtr

		;-------------------------------------------
		; Redraw the path.
		;-------------------------------------------
		plotPath, statePtr

		if strupcase((*statePtr).chooseOrbitOrBlocksFlag) eq 'BLOCKS' then begin

			plotBlockIdx = 16679875
			IF (*statePtr).eight_bit_display THEN $
				plotBlockIdx = (*statePtr).selectedBlockIdx
			plotBlocks, statePtr, plotBlockIdx

			;-------------------------------------------
			; Re-draw the block choosing lines in their
			; correct locations.
			;-------------------------------------------
			curWin = !d.window
			wset, (*statePtr).mapDrawWindowID
;print,'misrChooseButtonEvent:  (*statePtr).oldLineValue[0] = ',(*statePtr).oldLineValue[0]
;print,'misrChooseButtonEvent:  (*statePtr).oldLineValue[1] = ',(*statePtr).oldLineValue[1]
			device, copy = [			$
				0,				$
				0,				$
				(*statePtr).mapX,		$
				1,				$
				0,				$
				(*statePtr).oldLineValue[0],	$
				(*statePtr).linePixMap[0] ]
			device, copy = [			$
				0,				$
				0,				$
				(*statePtr).mapX,		$
				1,				$
				0,				$
				(*statePtr).oldLineValue[1],	$
				(*statePtr).linePixMap[0] ]
			wset, curWin
		endif

	endif

	if event.id eq (*statePtr).airMisrChooseButton and event.select then begin

		;-------------------------------------------
		; Change billboards.
		;-------------------------------------------
		widget_control, (*statePtr).chooseBase, map = 0
		widget_control, (*statePtr).airMisrFileBase, map = 1

		unset_planes, statePtr, -1, -1

		;-------------------------------------------
		; Set the AirMisr flag.
		;-------------------------------------------
		(*statePtr).airMisrFlag = 1

		;-------------------------------------------
		; Put "not applicable" in the along-track
		; and across-track resolution labels, and
		; deactivate the selector buttons.
		;-------------------------------------------
		widget_control, (*lowerBaseStatePtr).altResLabel, set_value = 'n/a'
		widget_control, (*lowerBaseStatePtr).actResLabel, set_value = 'n/a'
		widget_control, (*lowerBaseStatePtr).resBase1, sensitive = 0
		widget_control, (*lowerBaseStatePtr).resBase2, sensitive = 0

		;-------------------------------------------
		; Turn off dataMenu button until a valid
		; AirMISR file is selected (or reselected if
		; necessary).  Button will be turned on by
		; build_data_menu.pro.
		;-------------------------------------------
		widget_control, (*lowerBaseStatePtr).dataMenu, sensitive = 0

		;-------------------------------------------
		; Redraw the map with no path graphics.  The
		; AirMISR locations will be plotted later
		; when the file is selected.
		;-------------------------------------------
		(*statePtr).mapCenterLatLon[0] = 0

		;-------------------------------------------
		; Reset back to "ZOOM OUT" mode.
		;-------------------------------------------
		if strupcase( (*statePtr).zoomStatus ) eq 'ZOOMED_IN' then begin
			widget_control, (*statePtr).zoomOutButton, /set_button
			zoomButtonEvent, { top:event.top, id:(*statePtr).zoomOutButton, handler:'zoomButtonEvent', select:1 }
		endif

		;-------------------------------------------
		; Redraw global map w/o path blocks.
		;-------------------------------------------
		drawMap, statePtr
	endif
end
; misrChooseButtonEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ airMisrFileButtonEvent @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro airMisrFileButtonEvent, event

	upperBase = widget_info( event.top, /child )
	widget_control, upperBase, get_uvalue = statePtr

	;-------------------------------------------
	; Have the user locate an AirMISR file and
	; make sure it exists and is in fact a valid
	; AirMISR file, otherwise return.
	; Store the user-selected directory in the
	; button's UVALUE and restore it to the
	; filter next time so the user won't have to
	; re-select it every time.
	;-------------------------------------------
	widget_control, event.id, get_uvalue = dir
;cktapr2001	airMisrFile = dialog_pickfile( path = dir, get_path = dir )
	airMisrFile = dialog_pickfile_wrapper( path = dir, get_path = dir, /must_exist )
	widget_control, event.id, set_uvalue = dir
;print,'airMisrFileButtonEvent:  airMisrFile = ***',airMisrFile,'***'
	if airMisrFile eq '' then $
		return
	found = findfile( airMisrFile, count = count )
;print,'airMisrFileButtonEvent:  count = ***',count,'***'
	if count eq 0 then $
		return
	valid = valid_airmisr_file( airMisrFile )
;print,'airMisrFileButtonEvent:  valid = ***',valid,'***'
	if not valid then $
		return

	;-------------------------------------------
	; Put the AirMISR filename into the text
	; widget, which has scroll bars to allow
	; seeing the entire filename.
	;-------------------------------------------
	widget_control, (*statePtr).airMisrFileText, set_value = strtrim( airMisrFile, 2 )

	;-------------------------------------------
	; Make sure that the selection of viewplanes
	; is unset.
	;-------------------------------------------
	unset_planes, statePtr, -1, -1, AIRMISR_FILE = airMisrFile

	;-------------------------------------------
	; Temporarily put the AirMISR coordinates in
	; the mapCenterLatLon variable to be used by
	; MAP_SET in the drawMap routine.  Then set
	; mapCenterLatLon back to MISR coordinates.
	;-------------------------------------------
	savedMapCenterLatLon = (*statePtr).mapCenterLatLon

	(*statePtr).airMisrFilename	= airMisrFile
	airmisr_latlon_info		= retrieve_airmisr_latlon_info( airMisrFile )

	(*statePtr).mapCenterLatLon	= [ 0, airmisr_latlon_info.center_latlon[1] ]	; <--- Use actual AirMISR center lon.


;;;ckt,jul1999	(*statePtr).airMisrLonLatPtr = ptr_new( retrieve_airmisr_lonlat( airMisrFile ) )
;;;ckt,jul1999		(*statePtr).mapCenterLatLon = [ 0, 0 ]						; <--- Use 0,0 until AirMISR lon available.
;;;	(*statePtr).mapCenterLatLon = [ 0, (*((*statePtr).airMisrLonLatPtr))[0] ]	; <--- Use actual AirMISR center lon.



;;; IT WILL BE NECESSARY TO ADD AIRMISR LOCATION PLOTTING CODE HERE (after drawMap).
;;; THIS SHOULD BE EASY -- LOOK AT MISR EXAMPLE IN PLOTPATH.
	drawMap, statePtr
	(*statePtr).mapCenterLatLon = savedMapCenterLatLon

end
; airMisrFileButtonEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ setDate @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro setDate, statePtr, orbit
	;-------------------------------------------
	; Determine and set the date of the orbit.
	;-------------------------------------------
	julianDate = orbit2juliandate( float( orbit ), (*statePtr).Nref, (*statePtr).JNref )
	;-------------------------------------------
	; Necessary to add 0.5 to julian day before
	; running caldat. (Maybe not???????)
	;-------------------------------------------
	caldat, julianDate, caldatMonth, caldatDay, caldatYear
	month = month_int2str( caldatMonth )
	day = strtrim( caldatDay, 2 )
	if caldatDay lt 10 then $
		day = '0' + day
	year = strtrim( caldatYear, 2 )
;print,'month=',month
;print,'day=',day
;print,'year=',year
	widget_control, (*statePtr).monthButton, set_value = month
	widget_control, (*statePtr).dayButton, set_value = day[0]
	widget_control, (*statePtr).yearButton, set_value = year[0]
end
; setDate

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ setTime @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro setTime, statePtr, orbit
	;-------------------------------------------
	; Determine and set the hour and minute of
	; the orbit.
	;-------------------------------------------
	julianDate = orbit2juliandate( float(orbit), (*statePtr).Nref, (*statePtr).JNref )
	caldat, julianDate, caldatMonth, caldatDay, caldatYear, caldatHour, caldatMinute, caldatSeconds

	if caldatHour lt 10 then zpad = '0' else zpad = ''
	widget_control, (*statePtr).hourButton, set_value = zpad + strtrim( caldatHour[0], 2 )
	if caldatMinute lt 10 then zpad = '0' else zpad = ''
	widget_control, (*statePtr).minuteButton, set_value = zpad + strtrim( caldatMinute[0], 2 )
	if caldatSeconds lt 10 then zpad = '0' else zpad = ''
	widget_control, (*statePtr).secondsButton, set_value = zpad + strtrim( LONG( caldatSeconds[0] ), 2 )
end
; setTime

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ mapDrawEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro mapDrawEvent, event
	;-------------------------------------------
	;-------------------------------------------
	;-------------------------------------------
	;
	; Handle draw widget events in map window.
	;
	;-------------------------------------------
	;-------------------------------------------
	;-------------------------------------------

	curWin = !d.window

	;-------------------------------------------
	; Fetch the state pointer from the top level
	; base.
	;-------------------------------------------
	upperBase = widget_info( event.top, /child )
	widget_control, upperBase, get_uvalue = statePtr

	IF TAG_NAMES( event, /STRUCTURE_NAME ) EQ 'WIDGET_TRACKING' THEN BEGIN
		IF event.ENTER THEN $
			EightBit_RestoreColorMap, statePtr
		WIDGET_CONTROL, event.top, GET_UVALUE = tlbPtr
		WIDGET_CONTROL, (*tlbPtr).tlb, GET_UVALUE = topPtr
		(*topPtr).lastActiveObj = OBJ_NEW()
		wset, curWin
		RETURN
	ENDIF

	;-------------------------------------------
	; Determine if the button is being pressed
	; or released.  If it's being pressed, then
	; set the mouse down indicator to 1.  If
	; it's being released, then set the mouse
	; down indicator to 0.
	;-------------------------------------------
	if event.type eq 0 then begin
		widget_control, (*statePtr).orbitText, get_value = orbit
		(*statePtr).oldOrbit = orbit[0]
		;-------------------------------------------
		; Third and forth indicies of oldLineValue
		; are used for the horizontal and vertical
		; positions of the crosshairs.  (First and
		; second indicies are reserved for the block
		; choosing lines.)
		;-------------------------------------------
		(*statePtr).oldLineValue[2] = event.y
		(*statePtr).oldLineValue[3] = event.x

			;-------------------------------------------
			; SAVE HORIZONTAL PORTION OF NEW CROSSHAIR
			; LOCATION.
			;-------------------------------------------
			curWinNew = !d.window
			wset, (*statePtr).bufferPixMap[2]
			device, copy = [			$
				0,				$
				event.y,			$
				(*statePtr).mapX,		$
				1,				$
				0,				$
				0,				$
				(*statePtr).mapDrawWindowID ]
			wset, curWinNew

			;-------------------------------------------
			; SAVE VERTICAL PORTION OF NEW CROSSHAIR
			; LOCATION.
			;-------------------------------------------
			curWinNew = !d.window
			wset, (*statePtr).bufferPixMap[3]
			device, copy = [			$
				event.x,			$
				0,				$
				1,				$
				(*statePtr).mapY,		$
				0,				$
				0,				$
				(*statePtr).mapDrawWindowID ]
			wset, curWinNew

		(*statePtr).mouseButtonIsDown = 1
	endif
	if event.type eq 1 then $
		(*statePtr).mouseButtonIsDown = 0

	;-------------------------------------------
	; Convert the cursor X Y location to map
	; coordinates.
	;-------------------------------------------
	curWin = !d.window
	wset, (*statePtr).mapDrawWindowID
	(*statePtr).lonLat = convert_coord( event.X, event.Y, /device, /to_data )
	wset, curWin

	;-------------------------------------------
	; Detect when the cursor is off the map
	; projection and return if that is the case.
	;-------------------------------------------
	if (where( finite( (*statePtr).lonLat ) eq 0 ))[0] gt -1 then begin
		widget_control, (*statePtr).cursorLatLonValue[0], set_value = 'off'
		widget_control, (*statePtr).cursorLatLonValue[1], set_value = 'map'
		wset, curWin
		return
	endif

	;-------------------------------------------
	; Shorten LATITUDE value to one decimal place,
	; convert to string, and put into label widget
	; to display the cursor's current location.
	;-------------------------------------------
	specimen = (*statePtr).lonLat[1]
	oneDecimal = LONG( ( specimen mod 1 ) * 10 )
	if oneDecimal lt 0 and specimen gt -1.0 and specimen lt 0 then $
		negSign = '-' else negSign = ''
	if oneDecimal lt 0 then oneDecimal = 0 - oneDecimal
	oneDecimalString = strtrim( oneDecimal, 2 )
	cursorLatLonValueString = negSign + strtrim( LONG( specimen ), 2 ) + '.' + oneDecimalString
	widget_control, (*statePtr).cursorLatLonValue[0], set_value = cursorLatLonValueString

	;-------------------------------------------
	; Shorten LONGITUDE value to one decimal place,
	; convert to string, and put into label widget
	; to display the cursor's current location.
	;-------------------------------------------
	specimen = (*statePtr).lonLat[0]
	while specimen gt 180.0 do $
		specimen = specimen - 360.0
	while specimen le -180.0 do $
		specimen = specimen + 360.0
	oneDecimal = LONG( ( specimen mod 1 ) * 10 )
	if oneDecimal lt 0 and specimen gt -1.0 and specimen lt 0 then $
		negSign = '-' else negSign = ''
	if oneDecimal lt 0 then oneDecimal = 0 - oneDecimal
	oneDecimalString = strtrim( oneDecimal, 2 )
	cursorLatLonValueString = negSign + strtrim( LONG( specimen ), 2 ) + '.' + oneDecimalString
	widget_control, (*statePtr).cursorLatLonValue[1], set_value = cursorLatLonValueString

	if (*statePtr).airMisrFlag then begin
		wset, curWin
		return
	endif

	;-------------------------------------------
	; Find cursor latitude at center longitude.
	; LONGITUDE IS FIXED AT CURRENT MAP CENTER.
	;-------------------------------------------
	curWin = !d.window
	wset, (*statePtr).mapDrawWindowID
	currentLonLat = convert_coord( (*statePtr).mapX / 2.0, event.Y, /device, /to_data )
	wset, curWin

	;-------------------------------------------
	; Check to see if cursor is beyond map area.
	; If it is, then return out of this routine.
	;-------------------------------------------
	if min( finite( currentLonLat ) ) eq 0 then begin
		wset, curWin
		return
	endif

	;-------------------------------------------
	; Determine which line (upper or lower) is
	; being dragged, always choose the line that
	; is closest to the cursor on mouse down.
	;-------------------------------------------
	if event.type eq 0 and ( (*statePtr).chooseOrbitOrBlocksFlag eq 'blocks' ) then begin
		breakPoint = (*statePtr).oldLineValue[0] + $
			( (*statePtr).oldLineValue[1] - $
			(*statePtr).oldLineValue[0] ) / 2
		if event.y lt breakPoint then $
			(*statePtr).whichLine = 1 $
		else $
			(*statePtr).whichLine = 0
	endif

	;-------------------------------------------
	; Do contents only if the mouse down
	; indicator is 1 (button is down), and if
	; the mouse is inside the map window
	; (draw_widget).
	;-------------------------------------------
	if ( event.type eq 0 $
	or ( (*statePtr).mouseButtonIsDown and event.type eq 2 ) $
	and ( event.y lt (*statePtr).mapY - 1 and event.y ge 0 ) ) $
	then begin

		case strupcase( (*statePtr).chooseOrbitOrBlocksFlag ) of

		'ORBIT' : begin

			;-------------------------------------------
			; The following several steps up to where
			; newPathNumber is calculated comprise a
			; correction for when the cursor is north or
			; south of the equator.  Without this
			; correction the oribit chooser will get the
			; correct orbit only when the cursor is on
			; the equator.
			;-------------------------------------------

			;-------------------------------------------
			; Determine which blocks comprise the main
			; body, exclude the orbit tails.  Use block
			; center latitude to determine which block
			; is closest to the pole.
			;-------------------------------------------
			blockNearestNorthPole = (where( (*statePtr).binaryCornerFile[*,0] $
						eq max( (*statePtr).binaryCornerFile[*,0] ) ))[0]
			blockNearestSouthPole = (where( (*statePtr).binaryCornerFile[*,0] $
						eq min( (*statePtr).binaryCornerFile[*,0] ) ))[0]
			orbitMainBody = (*statePtr).binaryCornerFile[ blockNearestNorthPole:blockNearestSouthPole, 0 ]

			;-------------------------------------------
			; Withing the main orbit body, find block
			; whose center is nearest to cursor
			; latitude.
			;-------------------------------------------
			diffs = abs( orbitMainBody - (*statePtr).lonLat[1] )
			blockNearestCursor = where( diffs eq min(diffs) )
			blockNearestCursor = blockNearestCursor[0] + blockNearestNorthPole

			;-------------------------------------------
			; Offset is the difference between the
			; nearest block and the equator block.
			;-------------------------------------------
			absLats = abs( (*statePtr).binaryCornerFile[*,0] )
			blockNearestEquator = where( absLats eq min(absLats) )
			blockNearestEquator = blockNearestEquator[0]

			;-------------------------------------------
			; Calculate the new path longitude (at
			; equator crossing) from cursor location.
			;-------------------------------------------
			if (*statePtr).binaryCornerFile[blockNearestCursor,0] lt 0 then $
				longitude = (*statePtr).lonLat[0] $
						+ abs( (*statePtr).binaryCornerFile[blockNearestEquator,1] $
						- (*statePtr).binaryCornerFile[blockNearestCursor,1] ) $
			else $
				longitude = (*statePtr).lonLat[0] $
						- abs( (*statePtr).binaryCornerFile[blockNearestEquator,1] $
						- (*statePtr).binaryCornerFile[blockNearestCursor,1] )

			;-------------------------------------------
			; Convert new path longitude to new path
			; number.
			;-------------------------------------------
			newPathNumber = lon2path( longitude, (*statePtr).pathOne_Longitude, (*statePtr).PNref )

			;-------------------------------------------
			; Put the new path number into the widget
			; label.
			;-------------------------------------------
			widget_control, (*statePtr).pathText, set_value = strtrim( newPathNumber, 2 )
			widget_control, (*statePtr).orbitListButton, $
				set_value = 'Orbit List, Path ' + strtrim( newPathNumber, 2 )

			;-------------------------------------------
			; Get the most recent orbit number that
			; matches the currently selected path number
			; and put that orbit number into the orbit
			; text widget.
			;-------------------------------------------
			widget_control, (*statePtr).pathText, get_value = path
			orbit = path2most_recent_orbit( path[0], (*statePtr).Nref, (*statePtr).JNref, (*statePtr).PNref )
;print,'mapDrawEvent:  path[0] = ',path[0]
;print,'mapDrawEvent:  orbit = ',orbit
			widget_control, (*statePtr).orbitText, set_value = strtrim( orbit, 2 )
			(*statePtr).previousOrbit = orbit

			;-------------------------------------------
			; Determine and set the date of this orbit.
			;-------------------------------------------
			setDate, statePtr, orbit

			;-------------------------------------------
			; Determine and set the hour and minute of
			; the orbit.
			;-------------------------------------------
			setTime, statePtr, orbit

			;-------------------------------------------
			; ERASE HORIZONTAL PORTION OF OLD CROSSHAIR.
			;-------------------------------------------
			wset, (*statePtr).mapDrawWindowID
			device, copy = [			$
				0,				$
				0,				$
				(*statePtr).mapX,		$
				1,				$
				0,				$
				(*statePtr).oldLineValue[2],	$
				(*statePtr).bufferPixMap[2] ]

			;-------------------------------------------
			; ERASE VERTICAL PORTION OF OLD C/H LOCATION.
			;-------------------------------------------
			wset, (*statePtr).mapDrawWindowID
			device, copy = [			$
				0,				$
				0,				$
				1,				$
				(*statePtr).mapY,		$
				(*statePtr).oldLineValue[3],	$
				0,				$
				(*statePtr).bufferPixMap[3] ]

			;-------------------------------------------
			; SAVE HORIZONTAL PORTION OF NEW C/H LOCATION.
			;-------------------------------------------
			wset, (*statePtr).bufferPixMap[2]
			device, copy = [			$
				0,				$
				event.y,			$
				(*statePtr).mapX,		$
				1,				$
				0,				$
				0,				$
				(*statePtr).mapDrawWindowID ]

			;-------------------------------------------
			; SAVE VERTICAL PORTION OF NEW C/H LOCATION.
			;-------------------------------------------
			wset, (*statePtr).bufferPixMap[3]
			device, copy = [			$
				event.x,			$
				0,				$
				1,				$
				(*statePtr).mapY,		$
				0,				$
				0,				$
				(*statePtr).mapDrawWindowID ]

			;-------------------------------------------
			; DRAW HORIZONTAL PORTION OF NEW C/H LOCATION.
			;-------------------------------------------
			wset, (*statePtr).mapDrawWindowID
			device, copy = [			$
				0,				$
				0,				$
				(*statePtr).mapX,		$
				1,				$
				0,				$
				event.y,			$
				(*statePtr).linePixMap[0] ]

			;-------------------------------------------
			; DRAW VERTICAL PORTION OF NEW C/H LOCATION.
			;-------------------------------------------
			wset, (*statePtr).mapDrawWindowID
			device, copy = [			$
				0,				$
				0,				$
				1,				$
				(*statePtr).mapY,		$
				event.x,			$
				0,				$
				(*statePtr).linePixMap[1] ]

			(*statePtr).oldLineValue[2] = event.y
			(*statePtr).oldLineValue[3] = event.x

		end

		'BLOCKS' : begin

			startBlock = 0
			endBlock = (size((*statePtr).blockScreenLineNumbers))[2] - 1

			;-------------------------------------------
			; If cursor is dragging one line and crosses
			; over the other line, then return.
			;-------------------------------------------
			if ( ( (*statePtr).whichLine eq 1 ) and ( event.y ge (*statePtr).oldLineValue[0] ) ) $
			or ( ( (*statePtr).whichLine eq 0 ) and ( event.y le (*statePtr).oldLineValue[1] ) ) $
			then begin
				wset, curWin
				return
			endif

			;-------------------------------------------
			; If map is in polar zoom mode, then limit
			; searching to the upper or lower half of
			; the block file.  This is necessary due to
			; the search algorithm sometimes finding a
			; block in the wrong hemisphere when the map
			; is in a polar zoom.  When the map is in a
			; polar zoom, there is no need to search the
			; portion of the block list corresponding to
			; the opposite hemisphere anyway, so this
			; search limiting will patch the bug in the
			; search algorithm.
			;-------------------------------------------
			if strupcase( (*statePtr).zoomStatus ) eq 'ZOOMED_IN' then begin
				if (*statePtr).mapCenterLatLon[0] eq 90.0 then $
					endBlock = endBlock / 2
				if (*statePtr).mapCenterLatLon[0] eq -90.0 then $
					startBlock = endblock / 2
			endif

			;-------------------------------------------
			; Determine which block number is closest to
			; the cursor location and put that value in
			; the appropriate element (0 or 1, as
			; determined by whichLine) of the
			; whichBlocks string array.
			;-------------------------------------------
			blockNumberArray = where( $
				abs( $
				(*statePtr).blockScreenLineNumbers[1,startBlock:endBlock] - event.y ) eq $
				min( $
				abs( $
				(*statePtr).blockScreenLineNumbers[1,startBlock:endBlock] - event.y ) ) )
			if blockNumberArray[0] eq -1 then begin
				wset, curWin
				return
			endif
			blockNumber = blockNumberArray[0] + startBlock

			;-------------------------------------------
			; Make the string version of the block number
			; 1-based, instead of zero-based.
			;-------------------------------------------
			blockNumberString = strtrim( blockNumber + 1, 2 )
			(*statePtr).whichBlocks[(*statePtr).whichLine] = blockNumberString

			;-------------------------------------------
			; Set the text widget before calling plotPath.
			;-------------------------------------------
			widget_control, (*statePtr).whichBlocksText[(*statePtr).whichLine], set_value = blockNumberString

			;-------------------------------------------
			; Determine total number of blocks selected
			; and put that value in the text widget.
			;-------------------------------------------
			nblocks = LONG( (*statePtr).whichBlocks[1] ) - LONG( (*statePtr).whichBlocks[0] ) + 1
			zpad = ''
			if nblocks lt 100 then zpad = ' '
			if nblocks lt 10 then zpad = '  '
			numberOfBlocks = zpad + strtrim( nblocks, 2 )
			widget_control, (*statePtr).numBlocksValueLabel, set_value = numberOfBlocks

			;-------------------------------------------
			; Erase old chosen blocks by redrawing all
			; of the blocks in the orbit, then highlight
			; the newly chosen blocks.
			;-------------------------------------------
			plotPath, statePtr

plotBlockIdx = 16679875
IF (*statePtr).eight_bit_display THEN plotBlockIdx = (*statePtr).selectedBlockIdx

			plotBlocks, statePtr, plotBlockIdx

			widget_control, (*statePtr).whichBlocksText[0], $
				set_value = strtrim( (*statePtr).whichBlocks[0], 2 )
			widget_control, (*statePtr).whichBlocksText[1], $
				set_value = strtrim( (*statePtr).whichBlocks[1], 2 )

			;-------------------------------------------
			; Get the path number from the widget label.
			;-------------------------------------------
			widget_control, (*statePtr).pathText, get_value = pathNumber

			;-------------------------------------------
			; Put the line's latitude and longitude values
			; in the appropriate labels.
			;-------------------------------------------
			specimen = (*statePtr).binaryCornerFile[blockNumber,0]
			oneDecimal = LONG( ( specimen mod 1 ) * 10 )
			if oneDecimal lt 0 then oneDecimal = 0 - oneDecimal
			oneDecimalString = strtrim( oneDecimal, 2 )
			latString = strtrim( LONG( specimen ), 2 ) + '.' + oneDecimalString
			specimen = (*statePtr).binaryCornerFile[blockNumber,1] mod 360.0
			while specimen gt 180.0 do $
				specimen = specimen - 360.0
			while specimen le -180.0 do $
				specimen = specimen + 360.0
			oneDecimal = LONG( ( specimen mod 1 ) * 10 )
			if oneDecimal lt 0 then oneDecimal = 0 - oneDecimal
			oneDecimalString = strtrim( oneDecimal, 2 )
			lonString = strtrim( LONG( specimen), 2 ) + '.' + oneDecimalString
			widget_control, (*statePtr).latLimitValue[(*statePtr).whichLine], $
				set_value = latString + ', ' + lonString

			;-------------------------------------------
			; If the cursor is dragged up beyond the
			; topmost block, or below the bottommost
			; block, then limit the draw line to those
			; corresponding limits.
			;-------------------------------------------
			newLine = event.y
; This almost works, but there's a problem with polar zooms when used after these checks occur.  FIX later, optional.
; Leave commented out until fixed, if that ever happens.
;			if newLine gt (*statePtr).blockScreenLineNumbers[1,0] then $
;				newLine = (*statePtr).blockScreenLineNumbers[1,0]
;			if newLine lt (*statePtr).blockScreenLineNumbers[1,179] then $
;				newLine = (*statePtr).blockScreenLineNumbers[1,179]

			if newLine ne (*statePtr).oldLineValue[(*statePtr).whichLine] then begin
				;-------------------------------------------
				; Erase the old line by copying the line
				; buffer into the old line position.
				;-------------------------------------------
				wset, (*statePtr).mapDrawWindowID
				device, copy = [			$
					0,				$
					0,				$
					(*statePtr).mapX,		$
					1,				$
					0,				$
					(*statePtr).oldLineValue[	$
						(*statePtr).whichLine],	$
					(*statePtr).bufferPixMap[	$
						(*statePtr).whichLine] ]

				;-------------------------------------------
				; Save the new line by copying it into the
				; line buffer.
				;-------------------------------------------
				wset, (*statePtr).bufferPixMap[(*statePtr).whichLine]
				device, copy = [			$
					0,				$
					newLine,			$
					(*statePtr).mapX,		$
					1,				$
					0,				$
					0,				$
					(*statePtr).mapDrawWindowID ]

				;-------------------------------------------
				; Draw the new line by copying the line
				; pixmap into the new line position.
				;-------------------------------------------
				wset, (*statePtr).mapDrawWindowID
				device, copy = [			$
					0,				$
					0,				$
					(*statePtr).mapX,		$
					1,				$
					0,				$
					newLine,			$
					(*statePtr).linePixMap[0] ]

				;-------------------------------------------
				; Store current line number as the old line.
				;-------------------------------------------
				(*statePtr).oldLineValue[(*statePtr).whichLine] = newLine
			endif
		end
		endcase
	endif

	;-------------------------------------------
	; Detect a mouse button release and re-draw
	; and re-plot for the coordinates of the
	; newly selected path (swath).
	;-------------------------------------------
	if event.type eq 1 and strupcase((*statePtr).chooseOrbitOrBlocksFlag) eq 'ORBIT' then begin

		widget_control, /hourglass

		;-------------------------------------------
		; Put current cursor LONGITUDE (lonLat[0])
		; into map center LONGITUDE (LatLon[1]).
		;-------------------------------------------
		(*statePtr).mapCenterLatLon[1] = (*statePtr).lonLat[0]

		;-------------------------------------------
		; This is the same block of code as above
		; which corrects for the cursor being north
		; or south of the equator.  Without this
		; correction the orbit chooser would have
		; only worked correctly when choosing on the
		; equator.
		;-------------------------------------------
		blockNearestNorthPole = (where( (*statePtr).binaryCornerFile[*,0] $
					eq max( (*statePtr).binaryCornerFile[*,0] ) ))[0]
		blockNearestSouthPole = (where( (*statePtr).binaryCornerFile[*,0] $
					eq min( (*statePtr).binaryCornerFile[*,0] ) ))[0]
		orbitMainBody = (*statePtr).binaryCornerFile[ blockNearestNorthPole:blockNearestSouthPole, 0 ]
		diffs = abs( orbitMainBody - (*statePtr).lonLat[1] )
		blockNearestCursor = where( diffs eq min(diffs) )
		blockNearestCursor = blockNearestCursor[0] + blockNearestNorthPole
		absLats = abs( (*statePtr).binaryCornerFile[*,0] )
		blockNearestEquator = where( absLats eq min(absLats) )
		blockNearestEquator = blockNearestEquator[0]
		if (*statePtr).lonLat[1] lt 0 then $
			(*statePtr).mapCenterLatLon[1] = (*statePtr).lonLat[0] $
					+ abs( (*statePtr).binaryCornerFile[blockNearestEquator,1] $
					- (*statePtr).binaryCornerFile[blockNearestCursor,1] ) $
		else $
			(*statePtr).mapCenterLatLon[1] = (*statePtr).lonLat[0] $
					- abs( (*statePtr).binaryCornerFile[blockNearestEquator,1] $
					- (*statePtr).binaryCornerFile[blockNearestCursor,1] )

		;-------------------------------------------
		; Fetch the new path number, read out the
		; block corners for the new path number, and
		; redraw the path.
		;-------------------------------------------
		widget_control, (*statePtr).pathText, get_value = pathNumber
		if pathNumber[0] ne (*statePtr).previousPath then begin
			readNewBlockCornerFile, statePtr, LONG( pathNumber[0] )
			(*statePtr).previousPath = LONG( pathNumber[0] )
		endif

		;-------------------------------------------
		; redraw the map.
		;-------------------------------------------
		if strupcase( (*statePtr).zoomStatus ) eq 'ZOOMED_IN' then begin
			drawMap, statePtr, scale = 600E5
		endif else begin
			drawMap, statePtr
		endelse

		;-------------------------------------------
		; redraw the path.
		;-------------------------------------------
		plotPath, statePtr

		refresh_line_buffer, statePtr

		;-------------------------------------------
		; Grey out the extra day buttons in short months.
		;-------------------------------------------
		grey_day, statePtr

		;-------------------------------------------
		; Do housekeeping in lower half of data
		; selection interface.
		;-------------------------------------------
		widget_control, (*statePtr).orbitText, get_value = orbit
		widget_control, (*statePtr).pathText, get_value = path
		if (*statePtr).oldOrbit ne orbit[0] then $
			unset_planes, statePtr, LONG( orbit[0] ), LONG( path[0] )
	endif

	wset, curWin

end
; mapDrawEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ whichBlocksTextEvent @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro whichBlocksTextEvent, event

	curWin = !d.window

	;-------------------------------------------
	; Fetch the state pointer from the top level
	; base.
	;-------------------------------------------
	upperBase = widget_info( event.top, /child )
	widget_control, upperBase, get_uvalue = statePtr

	;-------------------------------------------
	; Determine which block's text widget is
	; being edited.  Also check to make sure the
	; newly edited value is correctly larger or
	; smaller than the other block.  (The upper
	; block number must not be greater than the
	; lower block number.)  If the newly edited
	; block number is incorrect then reset it
	; back to its previous value and return.
	;-------------------------------------------
	(*statePtr).whichLine = (where( (*statePtr).whichBlocksText eq event.id ))[0]
	widget_control, event.id, get_value = blockNumber
	blockNumber = long( blockNumber[0] )
	otherLine = abs( (*statePtr).whichLine - 1 )
	widget_control, (*statePtr).whichBlocksText[otherLine], get_value = otherBlockNumber
	otherBlockNumber = long( otherBlockNumber[0] )
;print,'whichBlocksTextEvent:  (*statePtr).whichLine = ',(*statePtr).whichLine
;print,'whichBlocksTextEvent:  otherLine = ',otherLine
;print,'whichBlocksTextEvent:  blockNumber = ',blockNumber
;print,'whichBlocksTextEvent:  otherBlockNumber = ',otherBlockNumber
;print,'tag_names( event, /structure_name ) = ',tag_names( event, /structure_name )
	;-------------------------------------------
	; If the blocks are not correctly larger or
	; smaller than each other, then give the
	; user a short time to enter the correct
	; values.  For example, the user may start
	; with the default 90:90 range, and wish to
	; change the second value to 100 by first
	; erasing the "9" before entering "10" to
	; complete the value "100".  Allow this to
	; occur within the short time limit, after
	; which revert to previous value.
	;
	; Also make sure the value is within 1-180.
	;-------------------------------------------
	delay = 1.0 ;seconds
;print,'(*statePtr).whichLine = ',(*statePtr).whichLine
	if (*statePtr).whichLine eq 0 then begin
		if ( blockNumber gt otherBlockNumber ) or ( blockNumber lt 1 ) then begin
			if tag_names( event, /structure_name ) eq 'WIDGET_TIMER' then begin
				if blockNumber lt 1 then begin
					blockNumber = 1
				endif else begin
					widget_control, (*statePtr).whichBlocksText[(*statePtr).whichLine], $
						set_value = strtrim( (*statePtr).whichBlocks[(*statePtr).whichLine], 2 )
				endelse
			endif else begin
				widget_control, event.id, timer = delay
;print,'here'
			endelse
;print,'whichBlocksTextEvent:  (*statePtr).whichBlocks[',(*statePtr).whichLine,'] = ',(*statePtr).whichBlocks[(*statePtr).whichLine]
			wset, curWin
			return
		endif
	endif else begin
		if ( blockNumber lt otherBlockNumber ) or ( blockNumber gt 180 ) then begin
			if tag_names( event, /structure_name ) eq 'WIDGET_TIMER' then begin
				if blockNumber gt 180 then begin
					blockNumber = 180
				endif else begin
					widget_control, (*statePtr).whichBlocksText[(*statePtr).whichLine], $
						set_value = strtrim( (*statePtr).whichBlocks[(*statePtr).whichLine], 2 )
				endelse
			endif else begin
				widget_control, event.id, timer = delay
;print,'no, here'
			endelse
;print,'whichBlocksTextEvent:  (*statePtr).whichBlocks[',(*statePtr).whichLine,'] = ',(*statePtr).whichBlocks[(*statePtr).whichLine]
			wset, curWin
			return
		endif
	endelse

	;-------------------------------------------
	; If map is polar zoom mode, then limit
	; searching to the upper or lower half of
	; the block file.  This is necessary due to
	; the search algorithm sometimes finding a
	; block in the wrong hemisphere when the map
	; is in a polar zoom.  When the map is in a
	; polar zoom, there is no need to search the
	; portion of the block list corresponding to
	; the opposite hemisphere anyway, so this
	; search limiting will patch the bug in the
	; search algorithm.
	;-------------------------------------------
	startBlock = 0
	endBlock = (size((*statePtr).blockScreenLineNumbers))[2] - 1
	if strupcase( (*statePtr).zoomStatus ) eq 'ZOOMED_IN' then begin
		if (*statePtr).mapCenterLatLon[0] eq 90.0 then $
			endBlock = endBlock / 2
		if (*statePtr).mapCenterLatLon[0] eq -90.0 then $
			startBlock = endblock / 2
	endif

	;-------------------------------------------
	; Check to make sure the block number is
	; within the range 1-180, if not then set it
	; to 1 (if too low) or 180 (if too high.)
	;-------------------------------------------

	;-------------------------------------------
	; Check that the new block line number is
	; within the map window (if zoomed), if not
	; then reset the text widget with the old
	; value and return.
	;-------------------------------------------
	if (*statePtr).blockScreenLineNumbers[1,blockNumber-1] lt 0 $
	or (*statePtr).blockScreenLineNumbers[1,blockNumber-1] gt (*statePtr).mapX $
	then begin
		widget_control, (*statePtr).whichBlocksText[(*statePtr).whichLine], $
			set_value = strtrim( (*statePtr).whichBlocks[(*statePtr).whichLine], 2 )
		wset, curWin
		return
	endif

	;-------------------------------------------
	; Put the string value into the whichBlocks
	; variable.
	;-------------------------------------------
	blockNumberString = strtrim( blockNumber, 2 )
	(*statePtr).whichBlocks[(*statePtr).whichLine] = blockNumberString
;print,'whichBlocksTextEvent:  blockNumberString = ',blockNumberString
;print,'whichBlocksTextEvent:  (*statePtr).whichBlocks = ',(*statePtr).whichBlocks

	;-------------------------------------------
	; Determine total number of blocks selected
	; and put that value in the text widget.
	;-------------------------------------------
	nblocks = LONG( (*statePtr).whichBlocks[1] ) - LONG( (*statePtr).whichBlocks[0] ) + 1
	zpad = ''
	if nblocks lt 100 then zpad = ' '
	if nblocks lt 10 then zpad = '  '
	numberOfBlocks = zpad + strtrim( nblocks, 2 )
	widget_control, (*statePtr).numBlocksValueLabel, set_value = numberOfBlocks

;	widget_control, (*statePtr).whichBlocksText[(*statePtr).whichLine], $
;		set_value = strtrim( (*statePtr).whichBlocks[(*statePtr).whichLine], 2 )

	;-------------------------------------------
	; Get the path number from the widget label.
	;-------------------------------------------
	widget_control, (*statePtr).pathText, get_value = pathNumber

	;-------------------------------------------
	; Put the line's latitude and longitude values
	; in the appropriate labels.
	;-------------------------------------------
	specimen = (*statePtr).binaryCornerFile[blockNumber-1,0]
	oneDecimal = LONG( ( specimen mod 1 ) * 10 )
	if oneDecimal lt 0 then oneDecimal = 0 - oneDecimal
	oneDecimalString = strtrim( oneDecimal, 2 )
	latString = strtrim( LONG( specimen ), 2 ) + '.' + oneDecimalString
	specimen = (*statePtr).binaryCornerFile[blockNumber-1,1] mod 360.0
	while specimen gt 180.0 do $
		specimen = specimen - 360.0
	while specimen le -180.0 do $
		specimen = specimen + 360.0
	oneDecimal = LONG( ( specimen mod 1 ) * 10 )
	if oneDecimal lt 0 then oneDecimal = 0 - oneDecimal
	oneDecimalString = strtrim( oneDecimal, 2 )
	lonString = strtrim( LONG( specimen), 2 ) + '.' + oneDecimalString
	widget_control, (*statePtr).latLimitValue[(*statePtr).whichLine], set_value = latString + ', ' + lonString

	;-------------------------------------------
	; Erase the old line by copying the line
	; buffer into the old line position.
	;-------------------------------------------
	wset, (*statePtr).mapDrawWindowID
;print,''
;print,''
;print,'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
;print,''
;print,'device, copy #1, erase old line'
;print,''
;print,'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
;print,'(*statePtr).whichLine = ',(*statePtr).whichLine
;print,'(*statePtr).oldLineValue[(*statePtr).whichLine] = ',(*statePtr).oldLineValue[(*statePtr).whichLine]
;print,'(*statePtr).bufferPixMap[(*statePtr).whichLine] = ',(*statePtr).bufferPixMap[(*statePtr).whichLine]
;print,''
;print,''
	device, copy = [			$
		0,				$
		0,				$
		(*statePtr).mapX,		$
		1,				$
		0,				$
		(*statePtr).oldLineValue[	$
			(*statePtr).whichLine],	$
		(*statePtr).bufferPixMap[	$
			(*statePtr).whichLine] ]

	;-------------------------------------------
	; Store current line number as the old line.
	;-------------------------------------------
	(*statePtr).oldLineValue[(*statePtr).whichLine] = (*statePtr).blockScreenLineNumbers[1,blockNumber-1]
	if (*statePtr).oldLineValue[0] eq (*statePtr).oldLineValue[1] then $
		(*statePtr).oldLineValue[1] = (*statePtr).oldLineValue[0] - 1

	;-------------------------------------------
	; Error check the block numbers in the text
	; widgets.  Do this in case and invalid
	; value was entered WITHOUT being registered
	; by using the <CR> key.
	;-------------------------------------------
	errorCheck_whichBlocks, statePtr

	;-------------------------------------------
	; Save the new line by copying it into the
	; line buffer.
	;-------------------------------------------
	wset, (*statePtr).bufferPixMap[(*statePtr).whichLine]
	device, copy = [						$
		0,							$
		(*statePtr).oldLineValue[(*statePtr).whichLine],	$
		(*statePtr).mapX,					$
		1,							$
		0,							$
		0,							$
		(*statePtr).mapDrawWindowID ]

	;-------------------------------------------
	; Draw the new line by copying the line
	; pixmap into the new line position.
	;-------------------------------------------
	wset, (*statePtr).mapDrawWindowID
	device, copy = [						$
		0,							$
		0,							$
		(*statePtr).mapX,					$
		1,							$
		0,							$
		(*statePtr).oldLineValue[(*statePtr).whichLine],	$
		(*statePtr).linePixMap[0] ]

	;-------------------------------------------
	; Erase old chosen blocks by redrawing all
	; of the blocks in the orbit, then highlight
	; the newly chosen blocks.
	;-------------------------------------------
	plotPath, statePtr

	;-------------------------------------------
	; Highlight chosen block range in a
	; different color.
	;-------------------------------------------
	plotBlockIdx = 16679875
	IF (*statePtr).eight_bit_display THEN $
		plotBlockIdx = (*statePtr).selectedBlockIdx
	plotBlocks, statePtr, plotBlockIdx

	wset, curWin
end
; whichBlocksTextEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ mostRecentPathButtonEvent @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro mostRecentPathButtonEvent, event

	widget_control, /hourglass

	;-------------------------------------------
	; Fetch the state pointer from the top level
	; base.
	;-------------------------------------------
	upperBase = widget_info( event.top, /child )
	widget_control, upperBase, get_uvalue = statePtr

	widget_control, (*statePtr).pathText, get_value = oldPathNumber

	;-------------------------------------------
	; Determine the most recent orbit according
	; to the current unix system time, and set
	; path number and orbit number accordingly.
	;-------------------------------------------
	mostRecentOrbit = most_recent_orbit( -1, (*statePtr).Nref, (*statePtr).JNref )
	mostRecentPath = orbit2path( mostRecentOrbit, (*statePtr).Nref, (*statePtr).PNref )

	if mostRecentPath ne oldPathNumber[0] then begin

		newPathString = strtrim( mostRecentPath, 2 )
		newOrbitString = strtrim( mostRecentOrbit, 2 )
		widget_control, (*statePtr).pathText, set_value = newPathString
		widget_control, (*statePtr).orbitListButton, set_value = 'Orbit List, Path ' + newPathString
		if mostRecentPath ne (*statePtr).previousPath then begin
			readNewBlockCornerFile, statePtr, LONG( mostRecentPath )
			(*statePtr).previousPath = LONG( mostRecentPath )
		endif
		widget_control, (*statePtr).orbitText, set_value = newOrbitString
		(*statePtr).previousOrbit = LONG( newOrbitString )
		unset_planes, statePtr, LONG( mostRecentOrbit ), LONG( mostRecentPath )

		;-------------------------------------------
		; Determine and set the date of the new
		; orbit number.
		;-------------------------------------------
		setDate, statePtr, mostRecentOrbit

		;-------------------------------------------
		; Determine and set the hour and minute of
		; the orbit.
		;-------------------------------------------
		setTime, statePtr, mostRecentOrbit

		;-------------------------------------------
		; Put current cursor LONGITUDE (lonLat[0])
		; into map center LONGITUDE (LatLon[1]).
		;-------------------------------------------
		centerBlockNumber = N_ELEMENTS( (*statePtr).binaryCornerFile[*,0] ) / 2 - 1
		(*statePtr).mapCenterLatLon[1] = (*statePtr).binaryCornerFile[centerBlockNumber,1] mod 360.0
		(*statePtr).lonLat[0] = (*statePtr).mapCenterLatLon[1]

		;-------------------------------------------
		; Redraw the map.
		;-------------------------------------------
		if strupcase( (*statePtr).zoomStatus ) eq 'ZOOMED_IN' then begin
			drawMap, statePtr, scale = 600E5
		endif else begin
			drawMap, statePtr
		endelse

		;-------------------------------------------
		; Redraw the path.
		;-------------------------------------------
		plotPath, statePtr

		refresh_line_buffer, statePtr
	endif
end
; mostRecentPathButtonEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ pathTextEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro pathTextEvent, event

	widget_control, /hourglass

	;-------------------------------------------
	; Fetch the state pointer from the top level
	; base.
	;-------------------------------------------
	upperBase = widget_info( event.top, /child )
	widget_control, upperBase, get_uvalue = statePtr

	;-------------------------------------------
	; Get the new path number value.  Insure that
	; path number is between 1-233, if not then
	; set path text widget to corrected value.
	;-------------------------------------------
	widget_control, (*statePtr).pathText, get_value = newPathNumber
	path = newPathNumber[0]
	if path lt 1 then $
		while path lt 1 do $
			path = path + 233
	if path gt 233 then $
		while path gt 233 do $
			path = path - 233
	path = strtrim( path, 2 )

	if path ne (*statePtr).previousPath then begin

		(*statePtr).previousPath = LONG( path )

		widget_control, (*statePtr).pathText, set_value = path
		widget_control, (*statePtr).orbitListButton, set_value = 'Orbit List, Path ' + strtrim( path, 2 )

		orbit = path2most_recent_orbit( path, (*statePtr).Nref, (*statePtr).JNref, (*statePtr).PNref )

		;-------------------------------------------
		; Determine and set the date of this orbit.
		;-------------------------------------------
		setDate, statePtr, orbit

		;-------------------------------------------
		; Determine and set the hour and minute of
		; the orbit.
		;-------------------------------------------
		setTime, statePtr, orbit

		;-------------------------------------------
		; Put most recent orbit into text widget.
		;-------------------------------------------
		widget_control, (*statePtr).orbitText, set_value = strtrim( orbit, 2 )
		(*statePtr).previousOrbit = orbit
		unset_planes, statePtr, LONG( orbit ), LONG( path )

		;-------------------------------------------
		; If a new path has been chosen, then read
		; the corresponding new block corners from
		; block corner file into binaryCornerFile.
		;-------------------------------------------
		readNewBlockCornerFile, statePtr, LONG( path )

		;-------------------------------------------
		; Put current cursor LONGITUDE (lonLat[0])
		; into map center LONGITUDE (LatLon[1]).
		;-------------------------------------------
		centerBlockNumber = N_ELEMENTS( (*statePtr).binaryCornerFile[*,0] ) / 2 - 1
		(*statePtr).mapCenterLatLon[1] = (*statePtr).binaryCornerFile[centerBlockNumber,1] mod 360.0
		(*statePtr).lonLat[0] = (*statePtr).mapCenterLatLon[1]

		;-------------------------------------------
		; Redraw the map using the new longitude.
		;-------------------------------------------
		if strupcase( (*statePtr).zoomStatus ) eq 'ZOOMED_IN' then begin
			drawMap, statePtr, scale = 600E5
		endif else begin
			drawMap, statePtr
		endelse

		;-------------------------------------------
		; Redraw the path using new block corners.
		;-------------------------------------------
		plotPath, statePtr

		refresh_line_buffer, statePtr
	endif
end
; pathTextEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ orbitListButtonEvent @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro orbitListButtonEvent, event
	;-------------------------------------------
	; Create clickable orbit list.  Re-create
	; the list every time, rather than keeping
	; it around unnecessarily.
	;-------------------------------------------
	widget_control, /hourglass
	upperBase = widget_info( event.top, /child )
	widget_control, upperBase, get_uvalue = statePtr

	;-------------------------------------------
	; Fetch the current orbit value from its
	; text widget.
	;-------------------------------------------
	widget_control, (*statePtr).orbitText, get_value = orbit

;	;-------------------------------------------
;	; Find the most recent orbit number corresponding
;	; to the newly selected current date selection.
;	; Also find the corresponding path number.
;	;-------------------------------------------
;	widget_control, (*statePtr).yearButton, get_value = year
;	widget_control, (*statePtr).monthButton, get_value = month
;	widget_control, (*statePtr).dayButton, get_value = day
;	widget_control, (*statePtr).hourButton, get_value = hour
;	widget_control, (*statePtr).minuteButton, get_value = minute
;	dateTime = year + month_str2intstr( month ) + day + hour + minute + '00'
;	orbit = most_recent_orbit( dateTime, (*statePtr).Nref, (*statePtr).JNref )
;	path = orbit2path( orbit, (*statePtr).Nref, (*statePtr).PNref )
widget_control,(*statePtr).pathText,get_value=path
path=path[0]

	;-------------------------------------------
	; generate list of orbit numbers from
	; mission start (orbit# <= 233) to most
	; recent orbit.
	;-------------------------------------------
	orbit = path2most_recent_orbit( path, (*statePtr).Nref, (*statePtr).JNref, (*statePtr).PNref )
	count = orbit[0] / 233 + 1
	orbits = lonarr( count )
	for i = 0, count - 1 do $
		orbits[i] = orbit - i * 233L

	;-------------------------------------------
	; generate list of dates corresponding to
	; list of orbit numbers.
	;-------------------------------------------
	dates = strarr( count )
	for i = 0, count - 1 do begin
		Jdate = (*statePtr).JNref + 16.0 / 233.0 * ( orbits[i] - (*statePtr).Nref )
		;-------------------------------------------
		; Necessary to add 0.5 to julian day before
		; runing caldat. (Maybe not???????)
		;-------------------------------------------
		caldat, Jdate, caldatMonth, caldatDay, caldatYear
		mm = month_int2str( caldatMonth )
		dd = strtrim( caldatDay, 2 )
		if caldatDay lt 10 then $
			dd = '0' + dd
		dates[i] = mm + ' ' + dd + ' ' + strtrim( caldatYear, 2 )
	endfor

	;-------------------------------------------
	; Assign new values to orbit menu buttons.
	;-------------------------------------------
	orbitStringsForList = strarr( count )
	for i = 0, count - 1 do begin
		spacePad = ''
		if orbits[i] lt 10000000L then spacePad = ' '
		if orbits[i] lt 1000000L then spacePad = '  '
		if orbits[i] lt 100000L then spacePad = '   '
		if orbits[i] lt 10000L then spacePad = '    '
		if orbits[i] lt 1000L then spacePad = '     '
		if orbits[i] lt 100L then spacePad = '      '
		if orbits[i] lt 10L then spacePad = '       '
		newOrbitString = spacePad $
			+ strtrim( orbits[i], 2 ) $
			+ '     ' $
			+ strtrim( dates[i], 2 )
		orbitStringsForList[i] = newOrbitString
	endfor

	;-------------------------------------------
	; Create and realize the list widget.
	;-------------------------------------------
	orbitListBase = widget_base( group_leader = event.top, /column, /floating, /modal, $
					event_pro = 'orbitListEvent', title = 'Click on Selection' )
	orbitListLabel = widget_label( orbitListBase, value = 'Select an Orbit on Path ' + strtrim( path, 2 ) )
	ysize = min( [ 20, count ] )
	orbitList = widget_list( orbitListBase, value = orbitStringsForList, ysize = ysize, uvalue = 'ORBITLIST' )
	orbitListBaseForButtons = widget_base( orbitListBase, /row )
	orbitListAcceptButton = widget_button( orbitListBaseForButtons, value = 'Accept', uvalue = 'ACCEPT' )
	orbitListCancelButton = widget_button( orbitListBaseForButtons, value = 'Cancel', uvalue = 'CANCEL' )
	widget_control, orbitListBase, set_uvalue = { $
						statePtr		: statePtr, $
						orbitList		: orbitList, $
						orbitStringsForList	: orbitStringsForList }
	widget_control, orbitListBase, default_button = orbitListAcceptButton
	widget_control, orbitListBase, /realize
end
; orbitListButtonEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ orbitListEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro orbitListEvent, event
	widget_control, event.top, get_uvalue = uvalStruct
	statePtr		= uvalStruct.statePtr
	orbitList		= uvalStruct.orbitList
	orbitStringsForList	= uvalStruct.orbitStringsForList

	widgetType = tag_names( event, /structure_name )
	case 1 of
		strupcase( widgetType ) eq 'WIDGET_LIST' : begin
			;-------------------------------------------
			; Double clicking on a list element accepts
			; that list element's index.  Disregard a
			; single click by returning.  After getting
			; the selectedIndex, kill the list widget.
			;-------------------------------------------
			if event.clicks eq 2 then begin
				selectedIndex = event.index
				widget_control, event.top, /destroy
			endif else begin
				return
			endelse
		end
		strupcase( widgetType ) eq 'WIDGET_BUTTON' : begin
			widget_control, event.id, get_uvalue = buttonName

			;-------------------------------------------
			; Accept button accepts the currently
			; highlighted list element's index.  After
			; getting the selected index, then kill the
			; orbit list widget.
			;-------------------------------------------
			if strupcase( buttonName ) eq 'ACCEPT' then begin
				selectedIndex = widget_info( orbitList, /list_select )
				;-------------------------------------------
				; If nothing was highlighted, then return
				; without destroying the list widget.
				;-------------------------------------------
				if selectedIndex eq -1 then $
					return
				;-------------------------------------------
				; A valid selectedIndex is assumed now, so
				; destroy the list widget.
				;-------------------------------------------
				widget_control, event.top, /destroy
			endif

			;-------------------------------------------
			; Kill the list widget and return.
			;-------------------------------------------
			if strupcase( buttonName ) eq 'CANCEL' then begin
				widget_control, event.top, /destroy
				return
			endif
		end
	endcase

	;-------------------------------------------
	; Use the selected index to grab out the
	; selected orbit string.
	;-------------------------------------------
	selectedOrbitListElement = orbitStringsForList[selectedIndex]

	;-------------------------------------------
	; Parse out the selected orbit list string
	; and put the new orbit value into the orbit
	; text widget.  The orbit text widget is
	; where the value is stored for retrieval,
	; rather than putting it in the (*statePtr)
	; structure.
	;-------------------------------------------
	orbitChoiceString = str_sep( strtrim( strcompress( selectedOrbitListElement ), 2 ), ' ' )
	newOrbitString = orbitChoiceString[0]
	widget_control, (*statePtr).orbitText, set_value = newOrbitString
	(*statePtr).previousOrbit = LONG( newOrbitString )

	;-------------------------------------------
	; Get the current path number and call
	; the unset_planes routine.
	;-------------------------------------------
	widget_control, (*statePtr).pathText, get_value = pathString
	unset_planes, statePtr, long( newOrbitString ), long( pathString[0] )

	;-------------------------------------------
	; Determine and set the date of the orbit.
	;-------------------------------------------
	setDate, statePtr, long( newOrbitString )

	;-------------------------------------------
	; Determine and set the hour and minute of
	; the orbit.
	; (Not necessary to set new time, because
	; all orbits on a given path have the same
	; time, but do it anyway.)
	;-------------------------------------------
	setTime, statePtr, long( newOrbitString )
end
; orbitListEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ orbitTextEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro orbitTextEvent, event

	widget_control, /hourglass

	;-------------------------------------------
	; Fetch the state pointer from the top level
	; base.
	;-------------------------------------------
	upperBase = widget_info( event.top, /child )
	widget_control, upperBase, get_uvalue = statePtr

	;-------------------------------------------
	; Get the new orbit number value.
	;-------------------------------------------
	widget_control, (*statePtr).orbitText, get_value = newOrbitNumber
	newOrbitNumber = newOrbitNumber[0]

;print,'newOrbitNumber = ',newOrbitNumber
;print,'(*statePtr).previousOrbit = ',(*statePtr).previousOrbit
	if newOrbitNumber ne (*statePtr).previousOrbit then begin

		(*statePtr).previousOrbit = newOrbitNumber

		;-------------------------------------------
		; Determine and set the date of this orbit.
		;-------------------------------------------
		setDate, statePtr, newOrbitNumber

		;-------------------------------------------
		; Determine and set the hour and minute of
		; the orbit.
		;-------------------------------------------
		setTime, statePtr, newOrbitNumber

		;-------------------------------------------
		; Set the new path number value and read in
		; the block corners for that path.
		;-------------------------------------------
		newPathNumber = strtrim( orbit2path( newOrbitNumber, (*statePtr).Nref, (*statePtr).PNref ), 2 )
		widget_control, (*statePtr).pathText, set_value = newPathNumber
		widget_control, (*statePtr).orbitListButton, set_value = 'Orbit List, Path ' + strtrim( newPathNumber, 2 )
		if newPathNumber ne (*statePtr).previousPath then begin
			readNewBlockCornerFile, statePtr, LONG( newPathNumber )
			(*statePtr).previousPath = LONG( newPathNumber )
		endif
		unset_planes, statePtr, LONG( newOrbitNumber ), LONG( newPathNumber )

		;-------------------------------------------
		; Put current cursor LONGITUDE (lonLat[0])
		; into map center LONGITUDE (LatLon[1]).
		;-------------------------------------------
		centerBlockNumber = N_ELEMENTS( (*statePtr).binaryCornerFile[*,0] ) / 2 - 1
		(*statePtr).mapCenterLatLon[1] = (*statePtr).binaryCornerFile[centerBlockNumber,1] mod 360.0
		(*statePtr).lonLat[0] = (*statePtr).mapCenterLatLon[1]

		;-------------------------------------------
		; Redraw the path using the new longitude.
		;-------------------------------------------
		if strupcase( (*statePtr).zoomStatus ) eq 'ZOOMED_IN' then begin
			drawMap, statePtr, scale = 600E5
		endif else begin
			drawMap, statePtr
		endelse

		;-------------------------------------------
		; Redraw the path using new block corners.
		;-------------------------------------------
		plotPath, statePtr

		refresh_line_buffer, statePtr
	endif
end
; orbitTextEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ dateEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro dateEvent, event

	widget_control, /hourglass

	widget_control, event.id, get_value = newDate

	;-------------------------------------------
	; Get the state pointer.
	;-------------------------------------------
	upperBase = widget_info( event.top, /child )
	widget_control, upperBase, get_uvalue = statePtr

	;-------------------------------------------
	; Fetch the menu buttons value and put it
	; into that menu button's parent button.
	;-------------------------------------------
	widget_control, event.id, get_value = newTime
	timeParent = widget_info( event.id, /parent )
	widget_control, timeParent, set_value = newDate

	;-------------------------------------------
	; Grey out extra days in short months.
	;-------------------------------------------
	grey_day, statePtr

	;-------------------------------------------
	; Find the most recent orbit number corresponding
	; to the newly selected current date selection.
	; Also find the corresponding path number.
	;-------------------------------------------
	widget_control, (*statePtr).yearButton, get_value = year
	widget_control, (*statePtr).monthButton, get_value = month
	widget_control, (*statePtr).dayButton, get_value = day
	widget_control, (*statePtr).hourButton, get_value = hour
	widget_control, (*statePtr).minuteButton, get_value = minute
	widget_control, (*statePtr).secondsButton, get_value = seconds
	dateTime = year + month_str2intstr( month ) + day + hour + minute + seconds
	orbit = most_recent_orbit( dateTime, (*statePtr).Nref, (*statePtr).JNref )
	path = orbit2path( orbit, (*statePtr).Nref, (*statePtr).PNref )

	widget_control, (*statePtr).orbitText, set_value = strtrim( orbit, 2 )
	(*statePtr).previousOrbit = orbit
	widget_control, (*statePtr).pathText, set_value = strtrim( path, 2 )
	widget_control, (*statePtr).orbitListButton, set_value = 'Orbit List, Path ' + strtrim( path, 2 )
	if path ne (*statePtr).previousPath then begin
		readNewBlockCornerFile, statePtr, LONG( path )
		(*statePtr).previousPath = LONG( path )
	endif
	unset_planes, statePtr, LONG( orbit ), LONG( path )

	;-------------------------------------------
	; Put current cursor LONGITUDE (lonLat[0])
	; into map center LONGITUDE (LatLon[1]).
	;-------------------------------------------
	centerBlockNumber = N_ELEMENTS( (*statePtr).binaryCornerFile[*,0] ) / 2 - 1
	(*statePtr).mapCenterLatLon[1] = (*statePtr).binaryCornerFile[centerBlockNumber,1] mod 360.0
	(*statePtr).lonLat[0] = (*statePtr).mapCenterLatLon[1]

	;-------------------------------------------
	; Redraw the map.
	;-------------------------------------------
	if strupcase( (*statePtr).zoomStatus ) eq 'ZOOMED_IN' then begin
		drawMap, statePtr, scale = 600E5
	endif else begin
		drawMap, statePtr
	endelse

	;-------------------------------------------
	; Redraw the path.
	;-------------------------------------------
	plotPath, statePtr

	refresh_line_buffer, statePtr
end
; dateEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ timeEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro timeEvent, event

	widget_control, /hourglass

	;-------------------------------------------
	; Get the state pointer.
	;-------------------------------------------
	upperBase = widget_info( event.top, /child )
	widget_control, upperBase, get_uvalue = statePtr

	;-------------------------------------------
	; Fetch the menu button's value and put it
	; into that menu button's parent button.
	;-------------------------------------------
	widget_control, event.id, get_value = newTime
	timeParent = widget_info( event.id, /parent )
	widget_control, timeParent, set_value = newTime

	;-------------------------------------------
	; Find the most recent orbit number and
	; corresponding path number.
	;-------------------------------------------
	widget_control, (*statePtr).yearButton, get_value = year
	widget_control, (*statePtr).monthButton, get_value = month
	widget_control, (*statePtr).dayButton, get_value = day
	widget_control, (*statePtr).hourButton, get_value = hour
	widget_control, (*statePtr).minuteButton, get_value = minute
	widget_control, (*statePtr).secondsButton, get_value = seconds
	dateTime = year + month_str2intstr( month ) + day + hour + minute + seconds
	orbit = most_recent_orbit( dateTime, (*statePtr).Nref, (*statePtr).JNref )
	path = orbit2path( orbit, (*statePtr).Nref, (*statePtr).PNref )

	widget_control, (*statePtr).orbitText, set_value = strtrim( orbit, 2 )
	(*statePtr).previousOrbit = orbit
	widget_control, (*statePtr).pathText, set_value = strtrim( path, 2 )
	widget_control, (*statePtr).orbitListButton, set_value = 'Orbit List, Path ' + strtrim( path, 2 )
	if path ne (*statePtr).previousPath then begin
		readNewBlockCornerFile, statePtr, LONG( path )
		(*statePtr).previousPath = LONG( path )
	endif
	unset_planes, statePtr, LONG( orbit ), LONG( path )

	;-------------------------------------------
	; Put current cursor LONGITUDE (lonLat[0])
	; into map center LONGITUDE (LatLon[1]).
	;-------------------------------------------
	centerBlockNumber = N_ELEMENTS( (*statePtr).binaryCornerFile[*,0] ) / 2 - 1
	(*statePtr).mapCenterLatLon[1] = (*statePtr).binaryCornerFile[centerBlockNumber,1] mod 360.0
	(*statePtr).lonLat[0] = (*statePtr).mapCenterLatLon[1]

	;-------------------------------------------
	; Redraw the map.
	;-------------------------------------------
	if strupcase( (*statePtr).zoomStatus ) eq 'ZOOMED_IN' then begin
		drawMap, statePtr, scale = 600E5
	endif else begin
		drawMap, statePtr
	endelse

	;-------------------------------------------
	; Redraw the path.
	;-------------------------------------------
	plotPath, statePtr

	refresh_line_buffer, statePtr
end
; timeEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ writeMapToFile @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro writeMapToFile, event
	;-------------------------------------------
	; This capability provided mainly just to
	; easily generate a graphic.  It simply
	; copies the visual contents of the map
	; window and writes to a tiff file.
	;-------------------------------------------

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== writeMapToFile (blockchooser_before_realize) =========='
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		e_msg	= [											$
				routine_name,									$
				'Error Index: ' + STRTRIM( error_status, 2 ),					$
				'Error Message: ' + !ERR_STRING,						$
				'',										$
				'Suggestion: If attempting to read or write a file, then check permissions.',	$
				'',										$
				'Returning...' ]
		result	= DIALOG_MESSAGE( e_msg, /ERROR )
		RETURN
	ENDIF


	;-------------------------------------------
	; Get the state pointer.
	;-------------------------------------------
	upperBase = widget_info( event.top, /child )
	widget_control, upperBase, get_uvalue = statePtr

	;-------------------------------------------
	; Store the user-selected directory path in
	; the button's UVALUE and restore it to the
	; filter next time the button is pressed.
	;-------------------------------------------
	widget_control, event.id, get_uvalue = dir
;;;ckt,apr2001	tiffFileName = dialog_pickfile( path = dir, get_path = dir )
	tiffFileName = dialog_pickfile_wrapper( path = dir, get_path = dir )
	widget_control, event.id, set_uvalue = dir
	if tiffFileName ne '' then begin
		tiffImage = bytarr( 3, (*statePtr).mapX, (*statePtr).mapY )
		curWin = !d.window
		wset, (*statePtr).mapDrawWindowID
		tiffImage[0,*,*] = reverse(tvrd(ch = 1),2)
		tiffImage[1,*,*] = reverse(tvrd(ch = 2),2)
		tiffImage[2,*,*] = reverse(tvrd(ch = 3),2)
		write_tiff, tiffFileName, tiffImage
		wset, curWin
	endif
end
; writeMapToFile

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ blockChooser_before_realize @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;;jan99,ckt pro blockChooser_before_realize, upperBase, catContents, $
pro blockChooser_before_realize, upperBase, catContentsPtr, elevationMapPtr, agpPtr, $
                                 headerSize, EIGHT_BIT_DISPLAY = eight_bit_display

;;;jan99, ckt	if n_params() eq 1 then $
;;;jan99, ckt		catContents = 0

	font2use = GetCorrectFont('courier2bold')

	;-------------------------------------------
	; Create a top-level-base (tlb) for all the
	; widgets associated with path (swath) and
	; block choosing.  This tlb will be a child
	; of a base higer up the application tree.
	; (subject to change)
	;-------------------------------------------
	tlb		= widget_base( upperBase, /row, RESOURCE_NAME = 'glbc' )

	;-------------------------------------------
	; Create a sub-base just for the draw widget
	; map window.
	;-------------------------------------------
	mapBase		= widget_base( tlb, /column )

	;-------------------------------------------
	; Calculate height of map window (draw_widget)
	; to be 1.5 times the width.
	;-------------------------------------------
	mapX		= 400
	mapY		= LONG( mapX )

	;-------------------------------------------
	; Create pixmaps for the block choosing lines
	; (which will also be used for the horizontal
	; element of the crosshair) and for the
	; vertical element of the crosshair.  Colorize
	; these lines with the erase command.
	;-------------------------------------------
	linePixMap	= lonarr(2)
	window, xsize = mapX, ysize = 1, /pixmap, /free

lineIdx = 7397760
IF KEYWORD_SET(eight_bit_display) THEN lineIdx = 4

	erase, lineIdx	;60000
	linePixMap[0] = !d.window
	window, xsize = 1, ysize = mapY, /pixmap, /free
	erase, lineIdx	;50000
	linePixMap[1] = !d.window

	;-------------------------------------------
	; Create pixmaps for buffering, to store the
	; map lines where block choosing lines and
	; crosshairs are being drawn, for retrieval
	; and restore to erase drawn lines when they
	; are moved.
	;-------------------------------------------
	bufferPixMap = lonarr(4)
	window, xsize = mapX, ysize = 1, /pixmap, /free
	bufferPixMap[0] = !d.window
	window, xsize = mapX, ysize = 1, /pixmap, /free
	bufferPixMap[1] = !d.window
	window, xsize = mapX, ysize = 1, /pixmap, /free
	bufferPixMap[2] = !d.window
	window, xsize = 1, ysize = mapY, /pixmap, /free
	bufferPixMap[3] = !d.window

	;-------------------------------------------
	; Create the entire widget family.
	;-------------------------------------------

	mapDraw			= widget_draw( mapBase, $
					xsize = mapX, ysize = mapY, $
					/motion_events, /button_events, $
					/TRACKING_EVENTS, $
					event_pro = 'mapDrawEvent', retain = 2 )

	infoBase		= widget_base( tlb, /column )

	;-------------------------------------------
	; Radio buttons.
	;-------------------------------------------
	misrChooseLabelBase	= widget_base( infoBase, /row, /frame )
	misrChooseLabel		= widget_label( misrChooseLabelBase, value = 'Source Data:', font = font2use )
	misrAirMisrChooseBase	= widget_base( misrChooseLabelBase, /row, /exclusive, $
					event_pro = 'misrChooseButtonEvent' )
	misrChooseButton	= widget_button( misrAirMisrChooseBase, $
					value = 'MISR', font = font2use, $
					RESOURCE_NAME = 'menu_level_0' )
	airMisrChooseButton	= widget_button( misrAirMisrChooseBase, $
					value = 'AirMISR', font = font2use, $
					RESOURCE_NAME = 'menu_level_0' )
	widget_control, misrChooseButton, set_button = 1

	;-------------------------------------------
	; MISR billboard.  (AirMisr billboard gets
	; created later, see below.)
	;-------------------------------------------
	billboardBase		= widget_base( infoBase, /frame )

	chooseBase		= widget_base( billboardBase, / column )

	;-------------------------------------------
	; Set up radio buttons for choosing path or blocks.
	;-------------------------------------------
	mapModeButtonBase		= widget_base( chooseBase, /row, $
						event_pro = 'mapModeChooseEvent', $
						/exclusive )
	mapModeChooseOrbitButton	= widget_button( mapModeButtonBase, $
						value = 'Choose MISR Orbit ', font = font2use, $
						RESOURCE_NAME = 'menu_level_0' )
	mapModeChooseBlocksButton	= widget_button( mapModeButtonBase, $
						value='Choose MISR Blocks', font = font2use, $
						RESOURCE_NAME = 'menu_level_0' )
	widget_control, mapModeChooseBlocksButton, set_button = 1

	pathAndOrbitChooseBase	= widget_base( chooseBase, /column, /frame )
	dateTimeBase		= widget_base( pathAndOrbitChooseBase, /row )


;---------------------------------------------------------------------------------------------------
; gui for entering in initial path/orbit/date information until MISR flies.
;---------------------------------------------------------------------------------------------------
orbit_path_date_struct	= misr_get_initial_orbit_path_date_info( /NO_GUI, /DEFAULTS )
Nref	= LONG( orbit_path_date_struct.init_orbit_val )
PNref	= LONG( orbit_path_date_struct.init_path_val )

;print,'orbit_path_date_struct.init_date_val = ',orbit_path_date_struct.init_date_val
;help,orbit_path_date_struct.init_date_val

JNref	= julianday( orbit_path_date_struct.init_date_val )

;help,Nref
;help,PNref
;help,JNref
;print,'Nref,PNref,JNref = ',Nref,PNref,JNref
;===================================================================================================
;;;ckt, Apr99	Nref = 1 ; <---------------------------------------------Assumed value for now.
;;;ckt, Apr99	PNref = 1	; <--------------------------------------------Assumed value for now.
;;;ckt, Apr99	JNref = julianday( '19980429000000' ) ; <----------------Assumed value for now.
;===================================================================================================
	;-------------------------------------------
	; If catalog is available, then initialize
	; orbit to maximum available orbit number.
	; If catalog is NOT available, then initialize
	; orbit to most recent orbit.
	;-------------------------------------------
;;;jan99, ckt	if n_elements( catContents ) eq 1 then begin
	if SIZE(*catContentsPtr, /TYPE) LE 0 then begin
		initialOrbit = path2most_recent_orbit( PNref, Nref, JNref )
	endif else begin
;;;jan99, ckt		initialOrbit = long( max( catContents[*,2] ) )
		initialOrbit = long( max( (*catContentsPtr)[*,2] ) )
	endelse
;print,'initialOrbit = ',initialOrbit

	;-------------------------------------------
	; Get the initial orbit's path number, and
	; calculate initial date values.
	;-------------------------------------------
	initialPath = strtrim( orbit2path( initialOrbit, Nref, PNref ), 2 )
;print,'initialPath = ',initialPath
	julianDate = orbit2juliandate( initialOrbit, Nref, JNref )
	;-------------------------------------------
	; Necessary to add 0.5 to julian day before
	; running caldat. (Maybe not???????)
	;-------------------------------------------
	caldat, julianDate, caldatMonth, caldatDay, caldatYear, caldatHour, caldatMinute, caldatSeconds
;print,'caldatMonth, caldatDay, caldatYear, caldatHour, caldatMinute, caldatSeconds = ',caldatMonth, caldatDay, caldatYear, caldatHour, caldatMinute, caldatSeconds
;print,'caldatSeconds = ',caldatSeconds
	initialMonth	= month_int2str( caldatMonth )
	initialDay	= strtrim( caldatDay, 2 )
	if caldatDay lt 10 then $
		initialDay = '0' + initialDay
	initialYear	= strtrim( caldatYear, 2 )
	if caldatHour lt 10 then zpad = '0' else zpad = ''
	initialHour	= zpad + strtrim( caldatHour, 2 )
	if caldatMinute lt 10 then zpad = '0' else zpad = ''
	initialMinute	= zpad + strtrim( caldatMinute, 2 )
	if caldatSeconds lt 10 then zpad = '0' else zpad = ''
	initialSeconds	= zpad + strtrim( LONG( caldatSeconds ), 2 )

;	;-------------------------------------------
;	; Calculate initial time values.
;	;-------------------------------------------
;	juliantime = orbit2juliandate( initialOrbit, Nref, JNref ) mod 1
;print,'juliantime = ',juliantime
;	CALDAT, juliantime, caldatMonth, caldatDay, caldatYear, caldatHour, caldatMinute, caldatSecond
;print,'caldatMonth, caldatDay, caldatYear, caldatHour, caldatMinute, caldatSecond = ',caldatMonth, caldatDay, caldatYear, caldatHour, caldatMinute, caldatSecond
;
;	;-------------------------------------------
;	; Necessary to add 0.5 to julian day before
;	; mod'ing. (Maybe not???????)
;	;-------------------------------------------
;	initialHour = LONG( ( ( juliantime + 0.5 ) mod 1 ) * 24.0 )
;	;-------------------------------------------
;	; Necessary to add 0.5 to julian day and to
;	; julian hour before mod'ing. (Maybe not???????)
;	;-------------------------------------------
;	initialHour	= strtrim( initialHour, 2 )
;	if initialHour lt 10 then initialHour = '0' + initialHour
;	initialMinute	= LONG( ( ( ( ( juliantime + 0.5 ) mod 1 ) * 24.0 + 0.5 ) mod 1 ) * 60.0 )
;	initialMinute	= strtrim( initialMinute, 2 )
;	if initialMinute lt 10 then initialMinute = '0' + initialMinute
;	initialSeconds	= LONG( ( ( ( ( juliantime + 0.5 ) mod 1 ) * 24.0 + 0.5 ) mod 1 ) * 60.0 * 60 )
;	initialSeconds	= strtrim( initialSeconds, 2 )
;	if initialSeconds lt 10 then initialSeconds = '0' + initialSeconds
;
;print,'initialHour, initialMinute, initialSeconds = ',initialHour,' ',initialMinute,' ',initialSeconds

	;-------------------------------------------
	; (Having a separate base for the buttons
	; makes the buttons slightly less tall and
	; the entire button is active, whereas with
	; the tall unbased version there is an area
	; in the lower part of the button area that
	; remains inactive.)
	; Presumably, this is an IDL feature.
	;-------------------------------------------
	dateButtonBase	= widget_base( dateTimeBase, /row )
	dateLabel	= widget_label( dateButtonBase, font = font2use, value = 'Orbit Date:' )

	;-------------------------------------------
	; Create menu button with 12 months.
	;-------------------------------------------
	monthButton		= widget_button( dateButtonBase, $
					value = initialMonth, /menu, font = font2use, $
					event_pro = 'dateEvent', $
					RESOURCE_NAME = 'menu_level_0' )
	allMonthValues = [ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' ]
	allMonthValueButtons = lonarr(31)
	allMonthButtons = lonarr( 12 )
	for i = 0, 11 do $
		allMonthButtons[i] = widget_button( monthButton, $
					value = allMonthValues[i], font = font2use, $
					RESOURCE_NAME = 'menu_level_1' )

	;-------------------------------------------
	; Create menu button with 31 days.
	;-------------------------------------------
	dayButton		= widget_button( dateButtonBase, $
					value = initialDay, /menu, font = font2use, $
					event_pro = 'dateEvent', $
					RESOURCE_NAME = 'menu_level_0' )
	allDayValues = strtrim( byte( findgen( 31 ) ) + 1, 2 )
	allDayButtons = lonarr(31)
	allDayButtonBases = lonarr(31)
	for i = 0, 30 do begin
		if allDayValues[i] lt 10 then $
			allDayValues[i] = '0' + allDayValues[i]
		allDayButtons[i] = widget_button( dayButton, $
					value = allDayValues[i], font = font2use, $
					RESOURCE_NAME = 'menu_level_1' )
	endfor

	;-------------------------------------------
	; Create menu button with relevant years.
	;-------------------------------------------
	yearButton		= widget_button( dateButtonBase, $
					value = initialYear, /menu, font = font2use, $
					event_pro = 'dateEvent', $
					RESOURCE_NAME = 'menu_level_0' )
	caldat,systime(/julian),mm,dd,yy
	nYears			= yy - 1998 + 20
	allYearValues = strtrim( byte( findgen( nYears ) ) + 1998, 2 )		; <-- needs to reference mission start year.
	allYearButtons = lonarr( nYears )
	for i = 0, nYears-1 do $
		allYearButtons[i] = widget_button( YearButton, $
					value = allYearValues[i], font = font2use, $
					RESOURCE_NAME = 'menu_level_1' )
;	now = systime()
;	year = strmid( now, 20, 4 )
;	if year lt 1999 then $
;		widget_control, allYearButtons[1], sensitive = 0
;	if year lt 2000 then $
;		widget_control, allYearButtons[2], sensitive = 0
;	if year lt 2001 then $
;		widget_control, allYearButtons[3], sensitive = 0
;	if year lt 2002 then $
;		widget_control, allYearButtons[4], sensitive = 0
;	if year lt 2003 then $
;		widget_control, allYearButtons[5], sensitive = 0
;	if year lt 2004 then $
;		widget_control, allYearButtons[6], sensitive = 0
;	if year lt 2005 then $
;		widget_control, allYearButtons[7], sensitive = 0
;	if year lt 2006 then $
;		widget_control, allYearButtons[8], sensitive = 0
;	if year lt 2007 then $
;		widget_control, allYearButtons[9], sensitive = 0

	timeBase		= widget_base( dateTimeBase, /row )

	gmtLabel		= widget_label( timeBase, value = ' GMT:', font = font2use )

	;-------------------------------------------
	; (Having a separate base for the buttons
	; makes the buttons slightly less tall and
	; the entire button is active, whereas with
	; the tall unbased version there is an area
	; in the lower part of the button area that
	; remains inactive.)
	; Presumably, this is an IDL feature.
	;-------------------------------------------
	timeButtonBase		= widget_base( timeBase, /row )

	;-------------------------------------------
	; Create menu button with 24 hours.
	;-------------------------------------------
	hourButton		= widget_button( timeButtonBase, $
					value = initialHour, /menu, font = font2use, $
					event_pro = 'timeEvent', $
					RESOURCE_NAME = 'menu_level_0' )
	;-------------------------------------------
	; (Necessary to add zero, otherwise IDL
	; reduces the variable to an empty scaler.
	; Presumably, this is an IDL feature!)
	;-------------------------------------------
	allHourValues = strtrim( byte( findgen( 24 ) ) + 0, 2 )
	for i = 0, 23 do $
		if allHourValues[i] lt 10 then allHourValues[i] = '0' + allHourValues[i]
	allHourButtons = lonarr( 24 )
	for i = 0, 23 do $
		allHourButtons[i] = widget_button( HourButton, $
					value = allHourValues[i], font = font2use, $
					RESOURCE_NAME = 'menu_level_1' )

	;-------------------------------------------
	; Create menu button with 12 minute selections.
	;-------------------------------------------
	minuteButton		= widget_button( timeButtonBase, $
					value = initialMinute, /menu, font = font2use, $
					event_pro = 'timeEvent', $
					RESOURCE_NAME = 'menu_level_0' )
	allMinuteValues = strtrim( byte( findgen( 12 ) * 5 ) + 0, 2 )
	for i = 0, 11 do $
		if allMinuteValues[i] lt 10 then allMinuteValues[i] = '0' + allMinuteValues[i]
	allMinuteButtons = lonarr( 12 )
	for i = 0, 11 do $
		allMinuteButtons[i] = widget_button( MinuteButton, $
					value = allMinuteValues[i], $
					RESOURCE_NAME = 'menu_level_1' )

	;-------------------------------------------
	; Create menu button with 12 seconds selections.
	;-------------------------------------------
	secondsButton		= widget_button( timeButtonBase, $
					value = strtrim( initialSeconds, 2 ), /menu, font = font2use, $
					event_pro = 'timeEvent', $
					RESOURCE_NAME = 'menu_level_0' )
	allSecondsValues = strtrim( byte( findgen( 12 ) * 5 ) + 0, 2 )
	for i = 0, 11 do $
		if allSecondsValues[i] lt 10 then allSecondsValues[i] = '0' + allSecondsValues[i]
	allSecondsButtons = lonarr( 12 )
	for i = 0, 11 do $
		allSecondsButtons[i] = widget_button( SecondsButton, $
					value = allSecondsValues[i], font = font2use, $
					RESOURCE_NAME = 'menu_level_1' )

	;-------------------------------------------
	; Set up path choosing panel.
	;-------------------------------------------
	pathChooseBase		= widget_base( pathAndOrbitChooseBase, /row )
	pathNumberLabel		= widget_label( pathChooseBase, value = 'Path:', font = font2use )
	pathTextBase		= widget_base( pathChooseBase, /row )
;print,'initialPath being put into pathText on startup = ',initialPath
	pathText		= widget_text( pathTextBase, $
					value = initialPath, xsize = 4, /editable, font = font2use, $
					event_pro = 'pathTextEvent' )

	;-------------------------------------------
	; Create button that will create the
	; clickable orbit list.
	;-------------------------------------------
	; (Having a separate base for the button
	; makes the button slightly less tall and
	; the entire button is active, whereas with
	; the tall unbased version there is an area
	; in the lower part of the button area that
	; remains inactive.)
	; Presumably, this is an IDL feature.
	;-------------------------------------------
	orbitChooseBase		= widget_base( pathChooseBase, /row )
	orbitListLabel		= widget_label( orbitChooseBase, value = 'Orbit:', font = font2use )
	orbitString = strtrim( initialOrbit, 2 )
	orbitText		= widget_text( orbitChooseBase, $
					value = orbitString, font = font2use, $
					/editable, xsize = 7, $
					event_pro = 'orbitTextEvent' )
	case 1 of
		PNref lt 100 and PNref ge 10	: pathString = ' ' + strtrim( PNref, 2 )
		PNref lt 10			: pathString = '  ' + strtrim( PNref, 2 )
		else				: pathString = strtrim( PNref, 2 )
	endcase
	orbitListButtonBase	= widget_base( orbitChooseBase, /row )
	orbitListButton		= widget_button( orbitListButtonBase, $
					value = 'Orbit List, Path ' + pathString, font = font2use, $
					event_pro = 'orbitListButtonEvent', $
					RESOURCE_NAME = 'menu_level_0' )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Create button to display catalog contents, but disable it.  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	catalogButtonBase	= widget_base( pathChooseBase, /row )
;	catalogButton		= widget_button( catalogButtonBase, $
;					value = 'Catalog', $
;					event_pro = 'catalogButtonEvent', $
;					RESOURCE_NAME = 'menu_level_0' )
;;Catalog button doesn't do anything useful (except display static view of catalog contents), so disapear (map=0) for now.
;widget_control,catalogButtonBase,map=0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;  Someday perhaps, the catalog display feature can be made to do something useful.  ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;jan99, ckt	if n_elements( catContents ) eq 1 then $
;	if SIZE(*catContentsPtr,/TYPE) LE 0 then $
;		widget_control, catalogButton, sensitive = 0

	;-------------------------------------------
	; Set up "Choose Blocks" panel.
	;-------------------------------------------
	whichBlocks		= ['90','90']
	nBlocks			= '  1'
	latLimitValue		= fltarr(2)
	blocksChooseBase	= widget_base( chooseBase, /column, /frame )
	whichBlocksText		= lonarr( 2 )
	upperBlockBase		= widget_base( blocksChooseBase, /row )
	upperBlockLabel		= widget_label( upperBlockBase, value = 'Start Block:', font = font2use )
	whichBlocksText[0]	= widget_text( upperBlockBase, $
					value = whichBlocks[0], xsize = 5, /editable, /all_events, font = font2use, $
					event_pro = 'whichBlocksTextEvent' )
	upperLatLabel		= widget_label( upperBlockBase, value = ' Center lat,lon: ', font = font2use )
	latLimitValue[0]	= widget_label( upperBlockBase, $
					value = '', /dynamic_resize, font = font2use )
	lowerBlockBase		= widget_base( blocksChooseBase, /row )
	lowerBlockLabel		= widget_label( lowerBlockBase, value = 'End Block:  ', font = font2use )
	whichBlocksText[1]	= widget_text( lowerBlockBase, $
					value = whichBlocks[1], xsize = 5, /editable, /all_events, font = font2use, $
					event_pro = 'whichBlocksTextEvent' )
	lowerLatLabel		= widget_label( lowerBlockBase, value = ' Center lat,lon: ', font = font2use )
	latLimitValue[1]	= widget_label( lowerBlockBase, $
					value = '', /dynamic_resize, font = font2use )
	numBlocksBase		= widget_base( blocksChooseBase, /row )
	numBlocksLabel		= widget_label( numBlocksBase, $
					value = 'Number of Blocks:', font = font2use )
	numBlocksValueLabel	= widget_label( numBlocksBase, value = nBlocks, /dynamic_resize, font = font2use )

	twoblanks		= widget_label( numBlocksBase, value = '  ' )

	zoomButtonBase		= widget_base( numBlocksBase, /row, /frame )
	zoomLabel		= widget_label( zoomButtonBase, value = 'Map Zoom:', font = font2use )
	zoomRadioButtonBase	= widget_base( zoomButtonBase, /exclusive, /row, $
					event_pro = 'zoomButtonEvent' )
	zoomInButton		= widget_button( zoomRadioButtonBase, $
					value = 'In', font = font2use, $
					RESOURCE_NAME = 'menu_level_0' )
	zoomOutButton		= widget_button( zoomRadioButtonBase, $
					value = 'Out', font = font2use, $
					RESOURCE_NAME = 'menu_level_0' )
	widget_control, zoomOutButton, /set_button

	anotherBase		= widget_base( infoBase, /row )
;	writeMapToFileButton	= widget_button( anotherBase, $
;					value = 'Save Map as TIFF', $
;					event_pro = 'writeMapToFile', $
;					RESOURCE_NAME = 'menu_level_0' )	; UVALUE of this button stores user-selected path.
	cursorLatLon		= fltarr(2)
	cursorLatLonBase	= widget_base( anotherBase, /row, /frame )
	cursorLatLonLabel	= widget_label( cursorLatLonBase, $
					value = 'Cursor lat,lon: ', font = font2use )
	cursorLatLonValue = lonarr(2)
	cursorLatLonValue[0]	= widget_label( cursorLatLonBase, $
					value = 'off', /dynamic_resize, font = font2use )
	cursorCommaLabel	= widget_label( cursorLatLonBase, value = ',', font = font2use )
	cursorLatLonValue[1]	= widget_label( cursorLatLonBase, $
					value = 'map', /dynamic_resize, font = font2use )

	;-------------------------------------------
	; AirMisr billboard.
	;-------------------------------------------
	airMisrFileBase		= widget_base( billboardBase, /column )
	airMisrFileButtonBase	= widget_base( airMisrFileBase, /row )
	airMisrFileButton	= widget_button( airMisrFileButtonBase, $
					value = 'Find/Select AirMISR Data File', $
					event_pro = 'airMisrFileButtonEvent', font = font2use, $
					RESOURCE_NAME = 'menu_level_0' )	; UVALUE of this button stores user-selected path.
	airMisrFileTextLabel	= widget_label( airMisrFileBase, value = 'Selected File:', font = font2use )
	airMisrFileText		= widget_text( airMisrFileBase, $
					value = 'None Selected', $
					xsize = 36, ysize = 10, $
					/scroll, /no_newline, font = font2use )

	;-------------------------------------------
	; Read the elevation map to be used as image
	; underlay beneath the continental outlines
	; and lat/lon grid.  If the elevation map
	; not available, the program will continue
	; with no image underlay.
	;-------------------------------------------
	elevationMap	= *elevationMapPtr

;;;ckt,feb2000	openr, lun, 'etop25.raw', /get_lun, error = error
;;;ckt,feb2000	if error eq 0 then begin
;;;ckt,feb2000		elevationMap = bytarr(864,432)
;;;ckt,feb2000		readu, lun, elevationMap
;;;ckt,feb2000		close, lun
;;;ckt,feb2000		free_lun, lun
;;;ckt,feb2000	endif else $
;;;ckt,feb2000		elevationMap = 0

display_8bit = 0
reserved_r = [0]
reserved_g = [0]
reserved_b = [0]
contIdx          = 0
blockIdx         = 0
selectedBlockIdx = 0
selectionLineIdx = 0
IF KEYWORD_SET(eight_bit_display) THEN BEGIN
	display_8bit = 1
	;
	; idx 0 = background
	; idx 1 = continental outlines
	; idx 2 = block outlines
	; idx 3 = selected blocks
	; idx 4 = selection lines
	; idx 5 = for MISR_DATA_MENU_OBJECT
	; idx 6 = for MISR_DATA_MENU_OBJECT
	; idx 7 = for MISR_DATA_MENU_OBJECT
	; idx 8 = for MISR_DATA_MENU_OBJECT
	;
	reserved_r = [   0, 255,  85, 195, 128, 255,   0,   0, 255 ]
	reserved_g = [   0, 223, 165, 131, 225,   0, 255,   0,   0 ]
	reserved_b = [   0, 114, 255, 254, 112,   0,   0, 255, 255 ]
	contIdx          = 1
	blockIdx         = 2
	selectedBlockIdx = 3
	selectionLineIdx = 4
	IF (SIZE(elevationMap))[0] GT 0 THEN BEGIN
		ncolors		= MIN( [ !D.N_COLORS, 255 ] )
		elevationMap	= BYTSCL( elevationMap, TOP = ncolors - N_ELEMENTS(reserved_r) ) + N_ELEMENTS(reserved_r)
	ENDIF
ENDIF

	;-------------------------------------------
	; Create the state variable and associate a
	; pointer value which will be stored in a
	; base somewhere higher up the tree.
	; (subject to change)
	;-------------------------------------------
	statePtr = ptr_new( 							$
		{								$
		parent				: WIDGET_INFO(upperBase,/PARENT),$
		Nref				: Nref				,$
		PNref				: PNref				,$
		JNref				: JNref				,$
		binaryCornerFile		: fltarr( 180, 10 )		,$
		pathOne_Longitude		: 0.0				,$;initialized in blockchooser_after_realize.pro
		mapX				: mapX				,$
		mapY				: mapY				,$
		lowerBase			: 0L				,$;initialized in blockchooser_after_realize.pro
;;;jan99, ckt		catContents			: catContents		,$
		catContentsPtr			: catContentsPtr		,$
		elevationMap			: elevationMap			,$
		lonLat				: fltarr( 3 )			,$
		mapDraw				: mapDraw			,$
		mapDrawWindowID			: 0L				,$;initialized in blockchooser_after_realize.pro
		linePixMap			: linePixMap			,$
		bufferPixMap			: bufferPixMap			,$
		oldOrbit			: 0L				,$
		whichLine			: 0				,$
		;-------------------------------------------
		; Third and forth indicies of oldLineValue
		; are used for the horizontal and vertical
		; positions of the crosshairs.  (First and
		; second indicies are reserved for the block
		; choosing lines.)
		;-------------------------------------------
		oldLineValue			: [ mapY/2, mapY/2-1, 0, 0 ]	,$
		mouseButtonIsDown		: 0				,$
		chooseBase			: chooseBase			,$
		misrChooseButton		: misrChooseButton		,$
		airMisrChooseButton		: airMisrChooseButton		,$
		airMisrFileBase			: airMisrFileBase		,$
		airMisrFlag			: 0				,$
		airMisrFilename			: ''				,$
		airMisrFileText			: airMisrFileText		,$
		pathAndOrbitChooseBase		: pathAndOrbitChooseBase	,$
		blocksChooseBase		: blocksChooseBase		,$
		chooseOrbitOrBlocksFlag		: 'blocks'			,$
		cursorLatLonValue		: cursorLatLonValue		,$
		pathText			: pathText			,$
		previousPath			: 0L				,$;initialized in blockchooser_after_realize.pro
		previousOrbit			: 0L				,$;initialized in blockchooser_after_realize.pro
		orbitText			: orbitText			,$
		orbitList			: 0L				,$
		orbitListButton			: orbitListButton		,$
		dayButton			: dayButton			,$
		allDayButtons			: allDayButtons			,$
		monthButton			: monthButton			,$
		yearButton			: yearButton			,$
		hourButton			: hourButton			,$
		minuteButton			: minuteButton			,$
		secondsButton			: secondsButton			,$
		whichBlocks			: whichBlocks			,$
		whichBlocksText			: whichBlocksText		,$
		whichBlocksTimerFlag		: 0				,$
		latLimitValue			: latLimitValue			,$
		numBlocksValueLabel		: numBlocksValueLabel		,$
		blockScreenLineNumbers		: intarr( 3, 180 )		,$
		mapMode				: 0				,$
		mapCenterLatLon			: [ 0.0, 0.0 ]			,$
		zoomOutButton			: zoomOutButton			,$
		zoomStatus			: 'ZOOMED_OUT'			,$
		mapModeChooseOrbitButton	: mapModeChooseOrbitButton	,$
		mapModeChooseBlocksButton	: mapModeChooseBlocksButton	,$
		eight_bit_display		: display_8bit			,$
		red_reserved			: reserved_r			,$
		grn_reserved			: reserved_g			,$
		blu_reserved			: reserved_b			,$
		contIdx				: contIdx			,$
		blockIdx			: blockIdx			,$
		selectedBlockIdx		: selectedBlockIdx		,$
		selectionLineIdx		: selectionLineIdx		,$
		agpPtr				: agpPtr			,$
		headerSize			: headerSize			,$
		mapSetStruct			: ptr_new()			$
		} )

	;-------------------------------------------
	; Grey out the extra day buttons in short months.
	;-------------------------------------------
	grey_day, statePtr

	;-------------------------------------------
	; For now, the state pointer is stored in
	; the upperBase.
	;-------------------------------------------
	widget_control, upperBase, set_uvalue = statePtr
end
; blockChooser_before_realize

