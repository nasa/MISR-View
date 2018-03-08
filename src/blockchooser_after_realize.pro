;----------------------------------------------------
; Author:  Jeffrey.R.Hall@jpl.nasa.gov (x4-4149)
; Written: 1998
; Project: MISR, David.J.Diner@jpl.nasa.gov (x4-6319)
;----------------------------------------------------

pro blockchooser_after_realize, upperBase

	widget_control, upperBase, get_uvalue = statePtr

	;-------------------------------------------
	; Set billboards:  AirMISR off, MISR on.
	;-------------------------------------------
	widget_control, (*statePtr).airMisrFileBase, map = 0
	widget_control, (*statePtr).chooseBase, map = 1

	(*statePtr).lowerBase = widget_info( upperBase, /sibling )

	widget_control, (*statePtr).mapDraw, get_value = mapDrawWindowID
	(*statePtr).mapDrawWindowID = mapDrawWindowID

	widget_control, (*statePtr).pathAndOrbitChooseBase, sensitive=0
	widget_control, (*statePtr).blocksChooseBase, sensitive=1

	;-------------------------------------------
	; Read in coordinates for initial path.
	;-------------------------------------------
	readNewBlockCornerFile, statePtr, (*statePtr).PNref

	widget_control, (*statePtr).pathText, get_value=pathNumber
	(*statePtr).previousPath = pathNumber[0]
	widget_control, (*statePtr).orbitText, get_value=orbitNumber
	(*statePtr).previousOrbit = orbitNumber[0]
	centerBlockNumber = N_ELEMENTS( (*statePtr).binaryCornerFile[*,0] ) / 2 - 1
	(*statePtr).mapCenterLatLon[1] = (*statePtr).binaryCornerFile[centerBlockNumber,1] mod 360.0
	(*statePtr).pathOne_Longitude = (*statePtr).mapCenterLatLon[1]

	;-------------------------------------------
	; Set up the initial map in the draw widget window.
	;-------------------------------------------
	drawMap, statePtr

	;-------------------------------------------
	; Draw the default path onto the map.
	;-------------------------------------------
	plotPath, statePtr

	;-------------------------------------------
	; Highlight the default-selected blocks 
	; (probably 90:90 unless something's changed
	; elsewhere) onto the map.
	;-------------------------------------------
	plotBlockIdx = 16679875
	IF (*statePtr).eight_bit_display THEN $
		plotBlockIdx = (*statePtr).selectedBlockIdx
	plotBlocks, statePtr, plotBlockIdx

;print,'blockchooser_after_realize:  (*statePtr).oldLineValue[0] = ',(*statePtr).oldLineValue[0]
;print,'blockchooser_after_realize:  (*statePtr).oldLineValue[1] = ',(*statePtr).oldLineValue[1]
	;-------------------------------------------
	; Initialize the line buffers by copying the 
	; current positions into the buffers before 
	; drawing any lines into the draw widget.
	;-------------------------------------------
	wset, (*statePtr).bufferPixMap[0]
	device, copy=[					$
		0,					$
		(*statePtr).oldLineValue[0],		$
		(*statePtr).mapX,			$
		1,					$
		0,					$
		0,					$
		(*statePtr).mapDrawWindowID ]
	wset, (*statePtr).bufferPixMap[1]
	device, copy=[					$
		0,					$
		(*statePtr).oldLineValue[1],		$
		(*statePtr).mapX,			$
		1,					$
		0,					$
		0,					$
		(*statePtr).mapDrawWindowID ]

	;-------------------------------------------
	; Draw the block choosing lines in their
	; correct locations.
	;-------------------------------------------
	curWin = !d.window
	wset, (*statePtr).mapDrawWindowID
;print,'(*statePtr).oldLineValue[0] = ',(*statePtr).oldLineValue[0]
	device, copy = [			$
		0,				$
		0,				$
		(*statePtr).mapX,		$
		1,				$
		0,				$
		(*statePtr).oldLineValue[0],	$
		(*statePtr).linePixMap[0] ]
;print,'(*statePtr).oldLineValue[1] = ',(*statePtr).oldLineValue[1]
	device, copy = [			$
		0,				$
		0,				$
		(*statePtr).mapX,		$
		1,				$
		0,				$
		(*statePtr).oldLineValue[1],	$
		(*statePtr).linePixMap[0] ]
	wset, curWin

	;-------------------------------------------
	; Put the line's latitude and longitude values
	; in the STARTING block's center label.
	;-------------------------------------------
	specimen = (*statePtr).binaryCornerFile[(*statePtr).whichblocks[0],0]
	oneDecimal = fix( ( specimen mod 1 ) * 10 )
	if oneDecimal lt 0 then oneDecimal = 0 - oneDecimal
	oneDecimalString = strtrim( string( oneDecimal ), 2 )
	latString = strtrim( string( fix( specimen ) ), 2 ) + '.' + oneDecimalString
	specimen = (*statePtr).binaryCornerFile[(*statePtr).whichblocks[0],1] mod 360.0
	while specimen gt 180.0 do $
		specimen = specimen - 360.0
	while specimen le -180.0 do $
		specimen = specimen + 360.0
	oneDecimal = fix( ( specimen mod 1 ) * 10 )
	if oneDecimal lt 0 then oneDecimal = 0 - oneDecimal
	oneDecimalString = strtrim( string( oneDecimal ), 2 )
	lonString = strtrim( string( fix( specimen) ), 2 ) + '.' + oneDecimalString
	widget_control, (*statePtr).latLimitValue[0], set_value = latString + ', ' + lonString
	;-------------------------------------------
	; Put the line's latitude and longitude values
	; in the ENDING block's center label.
	;-------------------------------------------
	specimen = (*statePtr).binaryCornerFile[(*statePtr).whichblocks[1],0]
	oneDecimal = fix( ( specimen mod 1 ) * 10 )
	if oneDecimal lt 0 then oneDecimal = 0 - oneDecimal
	oneDecimalString = strtrim( string( oneDecimal ), 2 )
	latString = strtrim( string( fix( specimen ) ), 2 ) + '.' + oneDecimalString
	specimen = (*statePtr).binaryCornerFile[(*statePtr).whichblocks[1],1] mod 360.0
	while specimen gt 180.0 do $
		specimen = specimen - 360.0
	while specimen le -180.0 do $
		specimen = specimen + 360.0
	oneDecimal = fix( ( specimen mod 1 ) * 10 )
	if oneDecimal lt 0 then oneDecimal = 0 - oneDecimal
	oneDecimalString = strtrim( string( oneDecimal ), 2 )
	lonString = strtrim( string( fix( specimen) ), 2 ) + '.' + oneDecimalString
	widget_control, (*statePtr).latLimitValue[1], set_value = latString + ', ' + lonString
end
