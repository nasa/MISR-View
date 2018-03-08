;12345678901234567890123456789012345678901234567890123456789012345678901234
;+
;==========================================================================
;
;Module Name:	GetLowerBaseContents
;
;Call Protocol:	GetLowerBaseContents, parentBase
;
;==========================================================================
;
;		Jet Propulsion Laboratory (JPL)
;
;		Multi-angle Imaging SpectroRadiometer  (MISR)
;
;		Instrument(s) :	<instrument>
;
;		Subsystem(s) :	<subsystem(s)>
;
;	Copyright (c) 2001 California Institute of Technology
;	U.S. Government Sponsorship under NASA Contract NAS7-1270
;	is acknowledged.
;
;		Cognizant Programmer(s) :
;
;		Charles Thompson	Charles.K.Thompson@jpl.nasa.gov
;
;===========================================================================
;
;Description:
;
;	GetLowerBaseContents is part of special-purpose software for the
;	MISRTOOL IDL program being developed at JPL by Charles Thompson and
;	Jeff Hall.  This module sets up the lower half of the data selection
;	interface.  For more information on this, please refer to the
;	"misr_data_selection.pro" file.
;
;Input Parameters:
;
;	Type	Name		Units		Purpose
;	-------------------------------------------------------------------
;	    	parentBase		the top-level base ID
;
;Output Parameters:
;
;	Type	Name		Units	Purpose
;	-------------------------------------------------------------------
;	OBJECT	MISR_DATA_MENU_OBJECT	 none
;
;Globals:
;
;	Type	Name	Units	Purpose
;	-------------------------------------------------------------------
;	<type>	<name>	<units>	<purpose>
;
;Return Values:
;
;	Type	Name	Units	Purpose
;	-------------------------------------------------------------------
;	<none>
;
;Known Bugs and Limitations:
;
;	<bugs & limitations>
;
;Parent(s):
;
;	<parents, if any>
;
;Routines Called:
;
;	<routines called, if any>
;
;Files Accessed:
;
;	<files accessed, if any>
;
;Revision History:
;
;Notes:
;
;
;=============================================================================
;-
;
@GetCorrectFont.pro
@misr_data_menu_object.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ cpt_ev @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO cpt_ev, event
;---------------------------------------------
; Event handler for ChangePlaneText interface
; See ChangePlaneText for more details about
; the interface and its purpose
;---------------------------------------------

	;---------------------------------------------
	; Retrieve pointer to structure from event.top
	; and widget uvalue (see ChangePlaneTextcode
	; below for contents of structure)
	;---------------------------------------------
	WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
	WIDGET_CONTROL, event.id, GET_UVALUE = uVal

	;---------------------------------------------
	; Case statement handles all widgets which
	; generate events within the ChangePlaneText
	; interface (all buttons).  Compare the
	; uvalue of each button to see what part of
	; case statement to execute
	;---------------------------------------------
	CASE uVal OF
		;---------------------------------------------
		; Font pulldown menu
		;---------------------------------------------
		'fontpd': $
			BEGIN
				;----------------------------------------
				; Begin process of forming a string that
				; is bounded by two delimiter carets
				; ("<" and ">").  Since we want the
				; widget_label to remain a fixed number
				; of characters in length (rather than
				; dynamically resizing itself), check
				; the length of the current font string
				; selected against the maximum length
				; the widget_label can be.  Use this
				; difference to pad the current string
				; before displaying it in the
				; widget_label.  Then, call the method
				; SetFont to set the font in the
				; current widget_draw
				;----------------------------------------
				currLen		= STRLEN( event.value )
				str2Use		= '< '
				diffLen		= (*infoPtr).maxFontLen	$
							- currLen
				nSpacesBefore	= FIX( diffLen / 2 )
				nSpacesAfter	= ROUND( diffLen / 2 )
				FOR i = 1, nSpacesBefore DO		$
					str2Use = str2Use + ' '
				str2Use	= str2Use + STRTRIM( event.value, 2 )
				FOR i = 1, nSpacesAfter DO		$
					str2Use = str2Use + ' '
				str2Use	= str2Use + ' >'
				WIDGET_CONTROL, (*infoPtr).fontLabel,	$
					SET_VALUE = str2Use
				(*infoPtr).misrDataMenuObj->SetFont,	$
					STRTRIM( event.value, 2 ) +	$
					'*Bold'
			END
		'sizepd': $
			BEGIN
				;----------------------------------------
				; Begin process of forming a string that
				; is bounded by two delimiter carets
				; ("<" and ">").  Since we want the
				; widget_label to remain a fixed number
				; of characters in length (rather than
				; dynamically resizing itself), check
				; the length of the current size string
				; selected against the maximum length
				; the widget_label can be.  Use this
				; difference to pad the current string
				; before displaying it in the
				; widget_label.  Then, call the method
				; SetSize to set the font in the
				; current widget_draw
				;----------------------------------------
				currLen		= STRLEN( event.value )
				str2Use		= '< '
				diffLen		= (*infoPtr).maxSizeLen	$
							- currLen
				nSpacesBefore	= FIX( diffLen / 2 )
				nSpacesAfter	= ROUND( diffLen / 2 )
				FOR i = 1, nSpacesBefore DO		$
					str2Use = str2Use + ' '
				str2Use	= str2Use + STRTRIM( event.value, 2 )
				FOR i = 1, nSpacesAfter DO		$
					str2Use = str2Use + ' '
				str2Use	= str2Use + ' >'
				WIDGET_CONTROL, (*infoPtr).sizeLabel,	$
					SET_VALUE = str2Use
				(*infoPtr).misrDataMenuObj->SetSize,	$
					FIX(event.value)
			END
		'done':	$
			BEGIN
				;----------------------------------------
				; Kill the interface, clean up the pointer
				;----------------------------------------
				PTR_FREE, infoPtr
				WIDGET_CONTROL, event.top, /DESTROY
			END
		ELSE:
	ENDCASE
END
; cpt_ev

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ChangePlaneText @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO ChangePlaneText, topBase, misrDataMenuObj
;---------------------------------------------
; Interface which allows the user to change
; the text characteristics of the current
; MISR_DATA_MENU_OBJ object.
;---------------------------------------------

	;---------------------------------------------
	; Set up base and label describing current
	; MISR_DATA_MENU_OBJ (use GetTitle method)
	;---------------------------------------------
	cptBase		= WIDGET_BASE(					$
				GROUP_LEADER = topBase,			$
				/MODAL,					$
				/COLUMN,				$
				TITLE = 'Change Plane Text' )
	cptLabel1	= WIDGET_LABEL(					$
				cptBase,				$
				FONT = GetCorrectFont('courier2bold'),	$
				VALUE = 'Current Plane: ' +		$
				misrDataMenuObj->GetTitle() )

	;---------------------------------------------
	; Set up a base for the font and size pulldown
	; menus.  Create the pulldown menus with
	; some basic functionality (2 types of fonts
	; and a few size options).  Also, set up two
	; widget_labels which display the current font
	; and size (repectively) in use.  Delimit
	; these "current value" strings with the "<"
	; and ">" carets.  Use the MISR_DATA_MENU_OBJ
	; methods GetCurrentFont and GetCurrentSize
	; to retrieve information about the current
	; settings, and pad the strings with blanks
	; as necessary so that the final displayed
	; string is long enough to contain the
	; longest string that might ever be encountered.
	;---------------------------------------------
	btBase		= WIDGET_BASE(					$
				cptBase,				$
				/ALIGN_CENTER,				$
				/COLUMN,				$
				FRAME = 3 )
	fontDesc	= [						$
				'1\  Fonts  ',				$
				'0\ courier ',				$
				'2\helvetica' ]
	maxFontLen	= MAX( STRLEN(fontDesc) - 2 )
	fontBase	= WIDGET_BASE(					$
				btBase,					$
				/BASE_ALIGN_CENTER,			$
				/ROW )
	fontPd		= CW_PDMENU(					$
				fontBase,				$
				FONT = GetCorrectFont('courier2bold'),	$
				UVALUE = 'fontpd',			$
				fontDesc,				$
				/RETURN_NAME )
	currLen		= STRLEN( misrDataMenuObj->GetCurrentFont() )
	str2Use		= '< '
	diffLen		= maxFontLen - currLen
	nSpacesBefore	= FIX( diffLen / 2 )
	nSpacesAfter	= ROUND( diffLen / 2 )
	FOR i = 1, nSpacesBefore DO					$
		str2Use = str2Use + ' '
	str2Use	= str2Use + STRTRIM( misrDataMenuObj->GetCurrentFont(), 2 )
	FOR i = 1, nSpacesAfter DO					$
		str2Use = str2Use + ' '
	str2Use	= str2Use + ' >'
	fontLabel	= WIDGET_LABEL(					$
				fontBase,				$
				FONT = GetCorrectFont('courier2bold'),	$
				VALUE = str2Use )
	sizeBase	= WIDGET_BASE(					$
				btBase,					$
				/BASE_ALIGN_CENTER,			$
				/ROW )

	sizeDesc	= [						$
				'1\  Sizes  ',				$
				'0\    8    ',				$
				'0\   10    ',				$
				'0\   12    ',				$
				'0\   14    ',				$
				'2\   16    ' ]

	maxSizeLen	= MAX( STRLEN(sizeDesc) - 2 )
	sizePd		= CW_PDMENU(					$
				sizeBase,				$
				FONT = GetCorrectFont('courier2bold'),	$
				sizeDesc,				$
				UVALUE = 'sizepd',			$
				/RETURN_NAME )
	currLen		= STRLEN( misrDataMenuObj->GetCurrentTextSize() )
	str2Use		= '< '
	diffLen		= maxSizeLen - currLen
	nSpacesBefore	= FIX( diffLen / 2 )
	nSpacesAfter	= ROUND( diffLen / 2 )
	FOR i = 1, nSpacesBefore DO					$
		str2Use = str2Use + ' '
	str2Use	= str2Use + STRTRIM( misrDataMenuObj->GetCurrentTextSize(), 2 )
	FOR i = 1, nSpacesAfter DO					$
		str2Use = str2Use + ' '
	str2Use		= str2Use + ' >'
	sizeLabel	= WIDGET_LABEL(					$
				sizeBase,				$
				FONT = GetCorrectFont('courier2bold'),	$
				VALUE = str2Use )
	;---------------------------------------------
	; done button
	;---------------------------------------------
	doneBt		= WIDGET_BUTTON(				$
				cptBase,				$
				UVALUE = 'done',			$
				FONT = GetCorrectFont('courier2bold'),	$
				VALUE = 'Done' )

	WIDGET_CONTROL, cptBase, /REALIZE

	;---------------------------------------------
	; structure containing relevant information
	; to be used by the event handler
	;---------------------------------------------
	infoPtr	= PTR_NEW( {						$
			misrDataMenuObj		:misrDataMenuObj,	$
			maxFontLen		:maxFontLen,		$
			maxSizeLen		:maxSizeLen,		$
			sizeLabel		:sizeLabel,		$
			fontLabel		:fontLabel },		$
			/NO_COPY )
	WIDGET_CONTROL, cptBase, SET_UVALUE = infoPtr
	WIDGET_CONTROL, cptBase, DEFAULT_BUTTON = doneBt
	XMANAGER, 'ChangePlaneText', cptBase, EVENT_HANDLER = 'cpt_ev'
END
; ChangePlaneText

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ spo_ev @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO spo_ev, event
;---------------------------------------------
; Event handler for SetPlaneOptions interface
; See SetPlaneOptions for more details about
; the interface and its purpose
;---------------------------------------------

	;---------------------------------------------
	; Retrieve object reference and widget uvalue
	;---------------------------------------------
	WIDGET_CONTROL, event.top, GET_UVALUE = misrDataMenuObj
	WIDGET_CONTROL, event.id, GET_UVALUE = uVal

	;---------------------------------------------
	; Simple case statement with 4 options:
	; call MISR_DATA_MENU_OBJ methods ZoomIn
	; or ZoomOut, call the ChangePlaneText
	; interface, or kill the current
	; interface
	;---------------------------------------------
	CASE uVal OF
		'zoomin':	misrDataMenuObj->ZoomIn
		'zoomout':	misrDataMenuObj->ZoomOut
		'changetext':	ChangePlaneText, event.top, misrDataMenuObj
		'done':		WIDGET_CONTROL, event.top, /DESTROY
		ELSE:
	ENDCASE
END
; spo_ev

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ SetPlaneOptions @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO SetPlaneOptions, topBase, misrDataMenuObj
;---------------------------------------------
; Interface which allows the user to set
; various characteristics of the current
; MISR_DATA_MENU_OBJ object (zoom in, zoom out,
; text style)
;---------------------------------------------

	;---------------------------------------------
	; Set up a basic base with buttons for zooming
	; in, zooming out, changing text styles, and
	; quitting the current interface
	;---------------------------------------------
	spoBase		= WIDGET_BASE(					$
				TITLE = 'Text Display Options',		$
				GROUP_LEADER = topBase,			$
				/MODAL,					$
				/COLUMN )
	spoLabel1	= WIDGET_LABEL(					$
				spoBase,				$
				FONT = GetCorrectFont('courier2bold'),	$
				VALUE = 'Current Plane: ' +		$
				misrDataMenuObj->GetTitle() )
	btBase		= WIDGET_BASE(					$
				spoBase,				$
				/ALIGN_CENTER,				$
				/COLUMN,				$
				FRAME = 3 )
	zoomInBt	= WIDGET_BUTTON(				$
				btBase,					$
				UVALUE = 'zoomin',			$
				FONT = GetCorrectFont('courier2bold'),	$
				VALUE = 'Zoom In' )
	zoomOutBt	= WIDGET_BUTTON(				$
				btBase,					$
				UVALUE = 'zoomout',			$
				FONT = GetCorrectFont('courier2bold'),	$
				Value = 'Zoom Out' )
	chgFontBt	= WIDGET_BUTTON(				$
				btBase,					$
				FONT = GetCorrectFont('courier2bold'),	$
				UVALUE = 'changetext',			$
				VALUE = 'Change Text Style' )
	doneBt		= WIDGET_BUTTON(				$
				spoBase,				$
				FONT = GetCorrectFont('courier2bold'),	$
				UVALUE = 'done',			$
				VALUE = 'Done' )
	WIDGET_CONTROL, spoBase, /REALIZE
	WIDGET_CONTROL, spoBase, SET_UVALUE = misrDataMenuObj
	WIDGET_CONTROL, spoBase, DEFAULT_BUTTON = doneBt
	XMANAGER, 'SetPlaneOptions', spoBase, EVENT_HANDLER = 'spo_ev'
END
; SetPlaneOptions

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ ConvertFloatingMem2Str @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION ConvertFloatingMem2Str, floatMem
;---------------------------------------------
; Utility function which takes a floating-point
; number and converts it into a string which
; represents the floating-point number to
; the nearest tenth accuracy (e.g., 2.45
; would be represented as "2.5").  Basically,
; all that occurs is that the floating-point
; number is converted to a string and broken
; up into two sub-strings: the number to the
; left of the decimal point, and the number to
; the right of the decimal point.  The latter
; strings' second character is then checked to
; see if it is greater than or less than 5.
; If it is greater than 5, the "tenths"
; character is incremented by one.
;---------------------------------------------
	memCurrStrArr	= STR_SEP( STRTRIM( STRING( floatMem ), 2 ), '.' )
	IF STRMID( memCurrStrArr[ 1 ], 1, 1 ) GE 5 THEN			$
		decimalPortion	= STRTRIM(				$
		STRING( FIX( STRMID( memCurrStrArr[1], 0, 1 ) ) + 1 ),	$
		2 )							$
	ELSE								$
		decimalPortion	= STRTRIM(				$
		STRING( FIX( STRMID( memCurrStrArr[1], 0, 1 ) ) ),	$
		2 )
	memCurrStr	= memCurrStrArr[0] + '.' + decimalPortion + ' MB'
	RETURN, memCurrStr
END
; ConvertFloatingMem2Str

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ CalculateCurrentMemory @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION CalculateCurrentMemory, altRes, actRes
;---------------------------------------------
; Dummy function which is to be replaced by
; a "real" memory calculator based upon the
; current data request from the user.
;---------------------------------------------
	memRequired	= ( FLOAT( altRes EQ 275.0 ) * 2.1 ) +		$
			  ( FLOAT( altRes NE 275.0 ) * 4.2 ) +		$
			  ( FLOAT( actRes EQ 275.0 ) * 2.1 ) +		$
			  ( FLOAT( actRes NE 275.0 ) * 4.2 )
	RETURN, memRequired
END
; CalculateCurrentMemory

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ GetLowerBaseContents_ev @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GetLowerBaseContents_ev, event
;---------------------------------------------
; Event handler for GetLowerBaseContents; all
; events from all widgets set up in
; the GetLowerBaseContents procedure are
; handled in this procedure; hence it is
; fairly long.
;---------------------------------------------
	TRUE		= 1
	FALSE		= 0

	;---------------------------------------------
	; Retrieve pointer to structure containing
	; information that is to be used by the
	; event handler; for a listing of the structure's
	; contents, look at the end of GetLowerBaseContents
	; below.
	;---------------------------------------------
	WIDGET_CONTROL, event.handler, GET_UVALUE = infoPtr

	;---------------------------------------------
	; Retrieve the name of the event structure
	; and check that name in a case statement
	;---------------------------------------------
	widgetType	= TAG_NAMES( event, /STRUCTURE_NAME )
;print,'widgetType = ',widgetType
	CASE STRUPCASE( widgetType ) OF

	'MISR_DATA_MENU_OBJ': BEGIN
		;---------------------------------------------
		; Event structure has been returned by a
		; MISR_DATA_MENU_OBJ object.  Check for
		; either event.type equal to 0 (mouse down)
		; or 1 (mouse up)
		;---------------------------------------------
		CASE event.type OF

		;---------------------------------------------
		; mouse down: flag this object as the next
		; "active" object once a "mouse up" event
		; occurs
		;---------------------------------------------
		0: (*infoPtr).nextActivePlaneObj = event.obj

		;---------------------------------------------
		; mouse up: call MISR_DATA_MENU_OBJ method
		; Deactivate to "turn off" the previous
		; "active" object, and call method
		; Activate to "turn on" the object that
		; recorded the last "mouse down" event
		; Also set the current object to be the
		; "activated" object
		;---------------------------------------------
		1: BEGIN
			IF OBJ_VALID( (*infoPtr).currentPlaneObj ) THEN $
				(*infoPtr).currentPlaneObj->Deactivate
			IF OBJ_VALID( (*infoPtr).nextActivePlaneObj ) THEN $
				(*infoPtr).nextActivePlaneObj->Activate
			(*infoPtr).currentPlaneObj =			$
				(*infoPtr).nextActivePlaneObj
			END
		ELSE:
		ENDCASE

		END
	'WIDGET_BUTTON': BEGIN
		;---------------------------------------------
		; Event structure returned by widget_button;
		; retrieve uvalue of the button pressed
		; and compare it in another case statement
		;---------------------------------------------
		WIDGET_CONTROL, event.id, GET_UVALUE = uVal
		CASE uVal OF

		;---------------------------------------------
		; Data pulldown menu pressed
		;---------------------------------------------
		'data': BEGIN
            		WIDGET_CONTROL, WIDGET_INFO( event.top, /CHILD ), $
            		   GET_UVALUE = upperBaseStatePtr
		        WIDGET_CONTROL, (*upperBaseStatePtr).orbitText,   $
		           GET_VALUE = orbit
		        WIDGET_CONTROL, (*upperBaseStatePtr).pathText,    $
		           GET_VALUE = path
			WIDGET_CONTROL, (*infoPtr).stashBase, GET_UVALUE = dataMenuBase

			IF NOT WIDGET_INFO( dataMenuBase, /VALID_ID ) THEN BEGIN
			   fill_data_button, (*upperBaseStatePtr).lowerBase,   $
;;;jan99, ckt			                     (*upperBaseStatePtr).catContents,	$
			                     *((*upperBaseStatePtr).catContentsPtr),	$
			                     LONG(orbit[0]), FIX(path[0])
			                     
			   WIDGET_CONTROL, (*infoPtr).stashBase, GET_UVALUE = dataMenuBase
			ENDIF

			WIDGET_CONTROL, dataMenuBase, MAP = 1
			WIDGET_CONTROL, event.top, SENSITIVE = 0

			END
		;---------------------------------------------
		; "Set Plane" button pressed
		;---------------------------------------------
		'setplane': BEGIN
			;---------------------------------------------
			; Grab the current data string, ACT res. value,
			; and ALT res. value
			;---------------------------------------------
			WIDGET_CONTROL, event.top, GET_UVALUE = topLevelBasePtr
;print,'TAG_NAMES( *topLevelBasePtr ) = ',TAG_NAMES( *topLevelBasePtr )
			WIDGET_CONTROL, (*topLevelBasePtr).createID, SENSITIVE = 1

            		WIDGET_CONTROL, WIDGET_INFO( event.top, /CHILD ), $
            		   GET_UVALUE = upperBaseStatePtr
            		   
currentOrbit = ''
IF NOT (*upperBaseStatePtr).airMisrFlag THEN BEGIN
			WIDGET_CONTROL, (*upperBaseStatePtr).orbitText, GET_VALUE = orbitStrArr
			currentOrbit = 'ORBIT ' + STRTRIM(orbitStrArr[0],2)

ENDIF
			gdIdx = WHERE( (*infoPtr).planeObjArr EQ (*infoPtr).currentPlaneObj )

			WIDGET_CONTROL,					$
				(*infoPtr).dataLabel[(*infoPtr).mappedDataLabelBaseIdx],	$
				GET_VALUE = tmpStr
				
				
				
IF NOT (*upperBaseStatePtr).airMisrFlag THEN BEGIN
			WIDGET_CONTROL, (*infoPtr).altResMenu,		$
				GET_UVALUE = altResLabel
			WIDGET_CONTROL, altResLabel,			$
				GET_VALUE = altResStr
			WIDGET_CONTROL, (*infoPtr).actResMenu,		$
				GET_UVALUE = actResLabel
			WIDGET_CONTROL, actResLabel,			$
				GET_VALUE = actResStr
ENDIF




WIDGET_CONTROL, (*infoPtr).dataLabel[0], GET_UVALUE = dataInfoStruct
tmpTagN = TAG_NAMES(dataInfoStruct)
(*infoPtr).planeObjStructArray[gdIdx].fname    = dataInfoStruct.file_name
(*infoPtr).planeObjStructArray[gdIdx].grid     = dataInfoStruct.grid_name
(*infoPtr).planeObjStructArray[gdIdx].field    = dataInfoStruct.field_name
(*infoPtr).planeObjStructArray[gdIdx].num_type = dataInfoStruct.num_type
dimIdx = WHERE(STRUPCASE(tmpTagN) EQ 'DIMIDX', dimCnt)
;print,'tmpTagN = ',tmpTagN
IF dimCnt GT 0 THEN BEGIN
   dims = dataInfoStruct.dimIdx
;print,''
;print,'GetLowerBaseContents_color_menus -- dims = ',dims
;print,''
   (*infoPtr).planeObjStructArray[gdIdx].extraDims = PTR_NEW(dims)
ENDIF


resStr	= ''
IF NOT (*upperBaseStatePtr).airMisrFlag THEN BEGIN
            (*topLevelBasePtr).gridInfo[gdIdx,0] = tmpStr ;camera, grid and field info
            tmpc = STRTRIM(actResStr,2)
            (*topLevelBasePtr).gridInfo[gdIdx,1] = STRMID(tmpc,0,STRLEN(tmpc)-1) ;ACT Res.
            tmpc = STRTRIM(altResStr,2)
            (*topLevelBasePtr).gridInfo[gdIdx,2] = STRMID(tmpc,0,STRLEN(tmpc)-1) ;ALT Res.


			;---------------------------------------------
			; Create a string describing ALT and ACT res.
			; specifications
			;---------------------------------------------
			resStr	= STRTRIM(actResStr,2) +		$
				  ' (cross-track) x ' + 		$
				  STRTRIM(altResStr,2) + 		$
				  ' (along-track)'
ENDIF

;;;ckt,aug2000			;---------------------------------------------
;;;ckt,aug2000			; Retrieve current memory requirements of
;;;ckt,aug2000			; currently-selected data
;;;ckt,aug2000			;---------------------------------------------
;;;ckt,aug2000			WIDGET_CONTROL, (*infoPtr).memCurrLabel,	$
;;;ckt,aug2000				GET_VALUE = memCurrStr
;;;ckt,aug2000			memNum	= STRMID( STRTRIM( memCurrStr, 2 ), 0,	$
;;;ckt,aug2000				  STRLEN( STRTRIM( memCurrStr, 2 ) ) - 2 ) + ' MB'


memNum=''
tmpsep=str_sep(string(tmpStr[0]),'___')
tmpStr1=tmpsep[0]
tmpStr2=tmpsep[1]
for a=2,n_elements(tmpsep)-1 do tmpStr2=tmpStr2+'_'+tmpsep[a]


			;---------------------------------------------
			; Create a string array to be placed into
			; the current MISR_DATA_MENU_OBJ object which
			; contains orbit number, selected data (tmpStr),
			; selected resolution (resStr), and memory
			; usage for the current selections (memNum)
			;---------------------------------------------
;;;ckt,aug2000			dataStr	= [					$
;;;ckt,aug2000				currentOrbit,				$
;;;ckt,aug2000				tmpStr,					$
;;;ckt,aug2000				resStr,					$
;;;ckt,aug2000				memNum ]
			dataStr	= [						$
				currentOrbit,					$
				tmpStr1,					$
				tmpStr2,					$
				resStr ]

			;---------------------------------------------
			; Call MISR_DATA_MENU_OBJ methods SetText and
			; SetMemoryValue to place dataStr into the
			; current MISR_DATA_MENU_OBJ and set its
			; current memory specs, respectively
			;---------------------------------------------
			(*infoPtr).currentPlaneObj->SetText, dataStr

;;;ckt,aug2000			(*infoPtr).currentPlaneObj->SetMemoryValue,	$
;;;ckt,aug2000				(*infoPtr).memCurr

			;---------------------------------------------
			; Increment the cumulative memory and set the
			; "cumulative memory label"; reduce the
			; "available memory" and set the "available
			; memory label" likewise
			;---------------------------------------------
;;;ckt,aug2000			(*infoPtr).memCum	=			$
;;;ckt,aug2000				(*infoPtr).memCum + (*infoPtr).memCurr
;;;ckt,aug2000			memCumStr		=			$
;;;ckt,aug2000				ConvertFloatingMem2Str( (*infoPtr).memCum )
;;;ckt,aug2000			WIDGET_CONTROL, (*infoPtr).memCumLabel,		$
;;;ckt,aug2000				SET_VALUE = memCumStr
;;;ckt,aug2000			(*infoPtr).memAvail	=			$
;;;ckt,aug2000				(*infoPtr).memAvail - (*infoPtr).memCurr
;;;ckt,aug2000			memAvailStr		=			$
;;;ckt,aug2000				ConvertFloatingMem2Str( (*infoPtr).memAvail )
;;;ckt,aug2000			WIDGET_CONTROL, (*infoPtr).memAvailLabel,	$
;;;ckt,aug2000				SET_VALUE = memAvailStr

			END
		;---------------------------------------------
		; "Clear Plane" button pressed
		;---------------------------------------------
		'clearplane': BEGIN
			;---------------------------------------------
			; Set up an "empty" string array to be displayed
			; within the current MISR_DATA_MENU_OBJ object,
			; then call SetText method
			;---------------------------------------------
			dataStr	= [					$
				'',					$
				'NOT SET',				$
				'',					$
				'' ]
			(*infoPtr).currentPlaneObj->SetText, dataStr

			;---------------------------------------------
			; Use the method GetMemoryValue to retrieve
			; the memory associated with the current
			; object before clearing; subtract this
			; value from the cumulative memory figure
			; and add it to the available memory figure.
			; update the memory labels appropriately,
			; and set the memory associated with the
			; current object to 0.
			;---------------------------------------------
;;;ckt,aug2000			currMem			=			$
;;;ckt,aug2000				(*infoPtr).currentPlaneObj->GetMemoryValue()
;;;ckt,aug2000			(*infoPtr).memCum	=			$
;;;ckt,aug2000				(*infoPtr).memCum - currMem

			WIDGET_CONTROL, event.top, GET_UVALUE = topLevelBasePtr
            		WIDGET_CONTROL, WIDGET_INFO( event.top, /CHILD ), $
            		   GET_UVALUE = upperBaseStatePtr

			any_plane_with_data_loaded	= 0
			k				= 0
			WHILE k LE 5 AND NOT any_plane_with_data_loaded DO BEGIN
				dataStr	= ((*infoPtr).planeObjArr[k])->GetText()
				idx	= WHERE( STRTRIM(STRUPCASE(dataStr),2) EQ 'NOT SET', cnt )
				IF cnt LE 0 THEN any_plane_with_data_loaded = 1
				k	= k + 1
			ENDWHILE
			
;;;ckt,aug2000			IF (*infoPtr).memCum LE 0.0 THEN BEGIN
;;;ckt,aug2000               		   WIDGET_CONTROL, event.top, GET_UVALUE = topLevelBasePtr
;;;ckt,aug2000			   WIDGET_CONTROL, (*topLevelBasePtr).createID, SENSITIVE = 0
;;;ckt,aug2000			ENDIF
			
			IF NOT any_plane_with_data_loaded THEN BEGIN
               		   WIDGET_CONTROL, event.top, GET_UVALUE = topLevelBasePtr
			   WIDGET_CONTROL, (*topLevelBasePtr).createID, SENSITIVE = 0
			ENDIF

			gdIdx = WHERE( (*infoPtr).planeObjArr EQ (*infoPtr).currentPlaneObj )

(*infoPtr).planeObjStructArray[gdIdx].fname    = ''
(*infoPtr).planeObjStructArray[gdIdx].grid     = ''
(*infoPtr).planeObjStructArray[gdIdx].field    = ''
(*infoPtr).planeObjStructArray[gdIdx].num_type = ''
ptr = (*infoPtr).planeObjStructArray[gdIdx].extraDims
IF PTR_VALID(ptr) THEN PTR_FREE, ptr

            		(*topLevelBasePtr).gridInfo[gdIdx,0] = ''
            		(*topLevelBasePtr).gridInfo[gdIdx,1] = ''
            		(*topLevelBasePtr).gridInfo[gdIdx,2] = ''

;;;ckt,aug2000				memCumStr		=			$
;;;ckt,aug2000					ConvertFloatingMem2Str( (*infoPtr).memCum )
;;;ckt,aug2000				WIDGET_CONTROL, (*infoPtr).memCumLabel,		$
;;;ckt,aug2000					SET_VALUE = memCumStr
;;;ckt,aug2000				(*infoPtr).memAvail	=			$
;;;ckt,aug2000					(*infoPtr).memAvail + currMem
;;;ckt,aug2000				memAvailStr		=			$
;;;ckt,aug2000					ConvertFloatingMem2Str( (*infoPtr).memAvail )
;;;ckt,aug2000				WIDGET_CONTROL, (*infoPtr).memAvailLabel,	$
;;;ckt,aug2000					SET_VALUE = memAvailStr
;;;ckt,aug2000				(*infoPtr).currentPlaneObj->SetMemoryValue, 0.0
			END
		;---------------------------------------------
		; "ACT Res." or "ALT Res." buttons pressed
		;---------------------------------------------
		'res': BEGIN
			;---------------------------------------------
			; Retrieve resolution value from current button
			;---------------------------------------------
			WIDGET_CONTROL, event.id,			$
				GET_VALUE = resValueStr
			;---------------------------------------------
			; Retrieve current button's parents' ID
			;---------------------------------------------
			parentButton	= WIDGET_INFO( event.id,	$
				/PARENT )
			;---------------------------------------------
			; Grab the uvalue from parentButton; this is
			; the ID of the label we want to set
			;---------------------------------------------
			WIDGET_CONTROL, parentButton,			$
				GET_UVALUE = resLabel
			;---------------------------------------------
			; Set the label's value
			;---------------------------------------------
			WIDGET_CONTROL, resLabel,			$
				SET_VALUE = resValueStr
			;---------------------------------------------
			; Now, go through and get the res. value for
			; ACT and ALT
			;---------------------------------------------
			WIDGET_CONTROL, (*infoPtr).altResMenu,		$
				GET_UVALUE = altResLabel
			WIDGET_CONTROL, altResLabel,			$
				GET_VALUE = altResStr
			WIDGET_CONTROL, (*infoPtr).actResMenu,		$
				GET_UVALUE = actResLabel
			WIDGET_CONTROL, actResLabel,			$
				GET_VALUE = actResStr
				
;;;ckt,aug2000			;---------------------------------------------
;;;ckt,aug2000				; If the memory labels have been "enabled"
;;;ckt,aug2000				; (meaning data HAS been selected), use the
;;;ckt,aug2000				; values of ACT Res. and ALT Res. to
;;;ckt,aug2000				; calculate memory requirements for the
;;;ckt,aug2000				; current selections, and update the current
;;;ckt,aug2000				; memory label
;;;ckt,aug2000				;---------------------------------------------
;;;ckt,aug2000				IF (*infoPtr).enableMem THEN BEGIN
;;;ckt,aug2000					altNumFlt	=			$
;;;ckt,aug2000						FLOAT( STRMID(			$
;;;ckt,aug2000						STRTRIM( altResStr, 2 ), 0,	$
;;;ckt,aug2000						STRLEN(				$
;;;ckt,aug2000						STRTRIM( altResStr, 2 ) ) - 1 ) )
;;;ckt,aug2000					actNumFlt	= 			$
;;;ckt,aug2000						FLOAT( STRMID( 			$
;;;ckt,aug2000						STRTRIM( actResStr, 2 ), 0, 	$
;;;ckt,aug2000						STRLEN( 			$
;;;ckt,aug2000						STRTRIM( actResStr, 2 ) ) - 1 ) )
;;;ckt,aug2000					calcCurrMem		= 		$
;;;ckt,aug2000						CalculateCurrentMemory(		$
;;;ckt,aug2000						altNumFlt, actNumFlt )
;;;ckt,aug2000					(*infoPtr).memCurr	= calcCurrMem
;;;ckt,aug2000					memCurrStr		= 		$
;;;ckt,aug2000						ConvertFloatingMem2Str( calcCurrMem )
;;;ckt,aug2000					WIDGET_CONTROL,				$
;;;ckt,aug2000						(*infoPtr).memCurrLabel,	$
;;;ckt,aug2000						SET_VALUE = memCurrStr
;;;ckt,aug2000				ENDIF
;;;ckt,aug2000				
;;;ckt,aug2000				
;;;ckt,aug2000					
;;;ckt,aug2000				;---------------------------------------------
;;;ckt,aug2000				; Retrieve current memory requirements of
;;;ckt,aug2000				; currently-selected data
;;;ckt,aug2000				;---------------------------------------------
;;;ckt,aug2000				WIDGET_CONTROL, (*infoPtr).memCurrLabel,	$
;;;ckt,aug2000					GET_VALUE = memCurrStr
;;;ckt,aug2000					
;;;ckt,aug2000				memNum	= STRMID( STRTRIM( memCurrStr, 2 ), 0,	$
;;;ckt,aug2000					  STRLEN( STRTRIM( memCurrStr, 2 ) ) - 2 ) + ' MB'
					  
				resStr	= STRTRIM(actResStr,2) +		$
				  ' (cross-track) x ' + 		$
				  STRTRIM(altResStr,2) + 		$
				  ' (along-track)'
				  
			FOR k = 0, 5 DO BEGIN
				dataStr	= ((*infoPtr).planeObjArr[k])->GetText()
				idx	= WHERE( STRTRIM(STRUPCASE(dataStr),2) EQ 'NOT SET', cnt )
				IF cnt LE 0 THEN BEGIN
					n		= N_ELEMENTS(dataStr)
;;;ckt,aug2000					dataStr[n-2]	= resStr
					dataStr[n-1]	= resStr
;;;ckt,aug2000					dataStr[n-1]	= memNum
					((*infoPtr).planeObjArr[k])->SetText, dataStr[1:n-1]

;;;ckt,aug2000					((*infoPtr).planeObjArr[k])->SetMemoryValue,	$
;;;ckt,aug2000						(*infoPtr).memCurr
				ENDIF
			ENDFOR
			
			END
		;---------------------------------------------
		; "Set Plane Options..." button pressed; call
		; the SetPlaneOptions procedure listed and
		; described above
		;---------------------------------------------
		'options': SetPlaneOptions, event.top, (*infoPtr).currentPlaneObj
		ELSE:
		ENDCASE
		END
	ELSE:
	ENDCASE
	
	RETURN, event
END
; GetLowerBaseContents_ev

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ GetLowerBaseContents_kill @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GetLowerBaseContents_kill, topBase
;---------------------------------------------
; Procedure that "cleans up" when the widgets
; set up in GetLowerBaseContents are destroyed
;---------------------------------------------
	WIDGET_CONTROL, topBase, GET_UVALUE = infoPtr
	OBJ_DESTROY, (*infoPtr).planeObjArr
	OBJ_DESTROY, (*infoPtr).nextActivePlaneObj
	IF WIDGET_INFO((*infoPtr).stashBase, /VALID_ID) THEN $
	   WIDGET_CONTROL, (*infoPtr).stashBase, /DESTROY
        PTR_FREE, ((*infoPtr).planeObjStructArray[*]).extraDims
	PTR_FREE, infoPtr
END
; GetLowerBaseContents_ev

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GetLowerBaseContents @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GetLowerBaseContents, topBase, EIGHT_BIT_DISPLAY = eightBitDisplay, $
				   USE_PALETTE = usePalette,		$
                                   RESERVED_IDX_START = reservedIdxStart, $
                                   RGB_VEC_PTR_ARR = rgbPtrArr
;---------------------------------------------
; Procedure which sets up the lower half of the
; MISRTOOL data selection interface.  topBase
; is the base ID of the base which is going to
; contain/receive all of the widgets set up below.
;---------------------------------------------

	;----------------------------------------------------------------
	; begin building interface
	;----------------------------------------------------------------
	dataBase	= WIDGET_BASE(					$
				topBase,         			$
				/ALIGN_CENTER,				$
				/ROW )
	;----------------------------------------------------------------
	; glbc stands for Get Lower Base Contents
	;----------------------------------------------------------------
	dataSubBase1	= WIDGET_BASE(					$
				dataBase,				$
				RESOURCE_NAME = 'glbc',			$
				/ROW,					$
				/FRAME )
	dataMenu	= WIDGET_BUTTON(				$
				dataSubBase1,				$
				VALUE = 'Data',				$
				UVALUE = 'data',                        $
				RESOURCE_NAME = 'menu_level_0',		$
				FONT = GetCorrectFont('courier2bold') )

	;----------------------------------------------------------------
	; left delimiter for the data label field
	;----------------------------------------------------------------
	dataLeftCaret	= WIDGET_LABEL(					$
				dataSubBase1,				$
				VALUE = ' <',				$
				FONT = GetCorrectFont('courier2bold') )

	;----------------------------------------------------------------
	; Be tricky with the data label: since we do NOT want to give the
	; data label as much room as it might want, create two types of
	; data labels, one a widget_label with a fixed width, and the
	; other a widget_text that will allow very long data names to
	; be placed without truncation.  Put each one of these types of
	; data labels in their own respective bases, and initially map
	; only the base with the widget_label.  If there is ever a data
	; name encountered that is greater than the allotted label
	; space, un-map the base containing the widget_label and map the
	; other base containing the widget_text.
	;----------------------------------------------------------------
	dataLabelBase	= WIDGET_BASE(					$
				dataSubBase1,				$
				/BASE_ALIGN_CENTER )
	dataLabelBaseA	= WIDGET_BASE(					$
				dataLabelBase,				$
				/BASE_ALIGN_CENTER,			$
				MAP = 1 )
	noneText	= '        None Selected         '
	dataLabel	= WIDGET_LABEL(					$
				dataLabelBaseA,				$
				VALUE = noneText,			$
				/ALIGN_CENTER,				$
				FONT = GetCorrectFont('courier2bold') )
	dataLabelBaseB	= WIDGET_BASE(					$
				dataLabelBase,				$
				/BASE_ALIGN_CENTER,			$
				MAP = 0 )
	dataText	= WIDGET_TEXT(					$
				dataLabelBaseB,				$
				XSIZE = STRLEN( noneText ),		$
				/ALIGN_CENTER,				$
				FONT = GetCorrectFont('courier2bold') )

	dataRightCaret	= WIDGET_LABEL(					$
				dataSubBase1,				$
				VALUE = '> ',				$
				FONT = GetCorrectFont('courier2bold') )
	dataSubBase2	= WIDGET_BASE(					$
				dataBase,				$
				RESOURCE_NAME = 'glbc',			$
				/ROW,					$
				/FRAME )
	actResMenu	= WIDGET_BUTTON(				$
				dataSubBase2,				$
				VALUE = 'Cross-track',			$
				RESOURCE_NAME = 'menu_level_0',		$
				/MENU,					$
				FONT = GetCorrectFont('courier2bold') )
	actResSub1	= WIDGET_BUTTON(				$
				actResMenu,				$
				VALUE = '275m',				$
				UVALUE = 'res',				$
				RESOURCE_NAME = 'menu_level_1',		$
				FONT = GetCorrectFont('courier2bold') )

	actResSub2	= WIDGET_BUTTON(				$
				actResMenu,				$
				VALUE = '550m',				$
				UVALUE = 'res',				$
				RESOURCE_NAME = 'menu_level_1',		$
				FONT = GetCorrectFont('courier2bold') )

	actResSub3	= WIDGET_BUTTON(				$
				actResMenu,				$
				VALUE = '1100m',			$
				RESOURCE_NAME = 'menu_level_1',		$
				UVALUE = 'res',				$
				FONT = GetCorrectFont('courier2bold') )
				
	actResSub4	= WIDGET_BUTTON(				$
				actResMenu,				$
				VALUE = '2200m',			$
				RESOURCE_NAME = 'menu_level_1',		$
				UVALUE = 'res',				$
				FONT = GetCorrectFont('courier2bold') )
				
	actResSub5	= WIDGET_BUTTON(				$
				actResMenu,				$
				VALUE = '4400m',			$
				RESOURCE_NAME = 'menu_level_1',		$
				UVALUE = 'res',				$
				FONT = GetCorrectFont('courier2bold') )
				
	actResSub6	= WIDGET_BUTTON(				$
				actResMenu,				$
				VALUE = '8800m',			$
				RESOURCE_NAME = 'menu_level_1',		$
				UVALUE = 'res',				$
				FONT = GetCorrectFont('courier2bold') )
				
	actResSub7	= WIDGET_BUTTON(				$
				actResMenu,				$
				VALUE = '17600m',			$
				RESOURCE_NAME = 'menu_level_1',		$
				UVALUE = 'res',				$
				FONT = GetCorrectFont('courier2bold') )
				
;	actResLeftCaret	= WIDGET_LABEL(					$
;				dataSubBase2,				$
;				VALUE = ' <',				$
;				FONT = GetCorrectFont('courier2bold') )
	actResLabel	= WIDGET_LABEL(					$
				dataSubBase2,				$
				VALUE = ' 1100m ',			$
				FONT = GetCorrectFont('courier2bold') )
;	actResRightCaret= WIDGET_LABEL(					$
;				dataSubBase2,				$
;				VALUE = '> ',				$
;				FONT = GetCorrectFont('courier2bold') )
	dataSubBase3	= WIDGET_BASE(					$
				dataBase,				$
				RESOURCE_NAME = 'glbc',			$
				/ROW,					$
				/FRAME )
	altResMenu	= WIDGET_BUTTON(				$
				dataSubBase3,				$
				VALUE = 'Along-track',			$
				RESOURCE_NAME = 'menu_level_0',		$
				/MENU,					$
				FONT = GetCorrectFont('courier2bold') )
	altResSub1	= WIDGET_BUTTON(				$
				altResMenu,				$
				UVALUE = 'res',				$
				VALUE = '275m',				$
				RESOURCE_NAME = 'menu_level_1',		$
				FONT = GetCorrectFont('courier2bold') )

	altResSub2	= WIDGET_BUTTON(				$
				altResMenu,				$
				UVALUE = 'res',				$
				VALUE = '550m',				$
				RESOURCE_NAME = 'menu_level_1',		$
				FONT = GetCorrectFont('courier2bold') )

	altResSub3	= WIDGET_BUTTON(				$
				altResMenu,				$
				VALUE = '1100m',			$
				RESOURCE_NAME = 'menu_level_1',		$
				UVALUE = 'res',				$
				FONT = GetCorrectFont('courier2bold') )

	altResSub4	= WIDGET_BUTTON(				$
				altResMenu,				$
				VALUE = '2200m',			$
				RESOURCE_NAME = 'menu_level_1',		$
				UVALUE = 'res',				$
				FONT = GetCorrectFont('courier2bold') )

	altResSub5	= WIDGET_BUTTON(				$
				altResMenu,				$
				VALUE = '4400m',			$
				RESOURCE_NAME = 'menu_level_1',		$
				UVALUE = 'res',				$
				FONT = GetCorrectFont('courier2bold') )

	altResSub6	= WIDGET_BUTTON(				$
				altResMenu,				$
				VALUE = '8800m',			$
				RESOURCE_NAME = 'menu_level_1',		$
				UVALUE = 'res',				$
				FONT = GetCorrectFont('courier2bold') )

	altResSub7	= WIDGET_BUTTON(				$
				altResMenu,				$
				VALUE = '17600m',			$
				RESOURCE_NAME = 'menu_level_1',		$
				UVALUE = 'res',				$
				FONT = GetCorrectFont('courier2bold') )
				
;	altResLeftCaret	= WIDGET_LABEL(					$
;				dataSubBase3,				$
;				VALUE = ' <',				$
;				FONT = GetCorrectFont('courier2bold') )
	altResLabel	= WIDGET_LABEL(					$
				dataSubBase3,				$
				VALUE = ' 1100m ',			$
				FONT = GetCorrectFont('courier2bold') )
;	altResRightCaret= WIDGET_LABEL(					$
;				dataSubBase3,				$
;				VALUE = '> ',				$
;				FONT = GetCorrectFont('courier2bold') )
	;----------------------------------------------------------------
	; End of data base creation
	;----------------------------------------------------------------

;;;ckt,aug2000	;----------------------------------------------------------------
;;;ckt,aug2000		; Begin creating next base for the display of memory information
;;;ckt,aug2000		;----------------------------------------------------------------
;;;ckt,aug2000		memBase		= WIDGET_BASE(					$
;;;ckt,aug2000					topBase,				$
;;;ckt,aug2000					/ROW,					$
;;;ckt,aug2000					/FRAME,					$
;;;ckt,aug2000					/ALIGN_CENTER,				$
;;;ckt,aug2000					/BASE_ALIGN_CENTER )
;;;ckt,aug2000		memLabel1	= WIDGET_LABEL(					$
;;;ckt,aug2000					memBase,				$
;;;ckt,aug2000					VALUE = 'MEMORY USAGE:',		$
;;;ckt,aug2000					/ALIGN_CENTER,				$
;;;ckt,aug2000					FONT = GetCorrectFont('courier2bold') )
;;;ckt,aug2000		memLabel2	= WIDGET_LABEL(					$
;;;ckt,aug2000					memBase,				$
;;;ckt,aug2000					VALUE = '   Current Selection:',	$
;;;ckt,aug2000					/ALIGN_CENTER,				$
;;;ckt,aug2000					FONT = GetCorrectFont('courier2bold') )
;;;ckt,aug2000		memLabel3	= WIDGET_LABEL(					$
;;;ckt,aug2000					memBase,				$
;;;ckt,aug2000					VALUE = ' 0.0 MB',			$
;;;ckt,aug2000					/ALIGN_CENTER,				$
;;;ckt,aug2000					FONT = GetCorrectFont('courier2bold') )
;;;ckt,aug2000		memLabel4	= WIDGET_LABEL(					$
;;;ckt,aug2000					memBase,				$
;;;ckt,aug2000					VALUE = '   Previous:',			$
;;;ckt,aug2000					/ALIGN_CENTER,				$
;;;ckt,aug2000					FONT = GetCorrectFont('courier2bold') )
;;;ckt,aug2000		memLabel5	= WIDGET_LABEL(					$
;;;ckt,aug2000					memBase,				$
;;;ckt,aug2000					VALUE = ' 0.0 MB',			$
;;;ckt,aug2000					/ALIGN_CENTER,				$
;;;ckt,aug2000					FONT = GetCorrectFont('courier2bold') )
;;;ckt,aug2000		memLabel6	= WIDGET_LABEL(					$
;;;ckt,aug2000					memBase,				$
;;;ckt,aug2000					VALUE = '   Available:',		$
;;;ckt,aug2000					/ALIGN_CENTER,				$
;;;ckt,aug2000					FONT = GetCorrectFont('courier2bold') )
;;;ckt,aug2000		memLabel7	= WIDGET_LABEL(					$
;;;ckt,aug2000					memBase,				$
;;;ckt,aug2000					VALUE = '128.0 MB',			$
;;;ckt,aug2000					/ALIGN_CENTER,				$
;;;ckt,aug2000					FONT = GetCorrectFont('courier2bold') )
	;----------------------------------------------------------------
	; Begin creating next base for the display of viewer fields
	;----------------------------------------------------------------
	drawXsize	= 285
	drawYsize	= 100

	viewBase	= WIDGET_BASE(					$
				topBase,				$
				/COLUMN,				$
				/ALIGN_CENTER )
	buttonBase	= WIDGET_BASE(					$
				viewBase,				$
				RESOURCE_NAME = 'glbc',			$
				/ALIGN_CENTER,				$
				/ROW )
	setButton	= WIDGET_BUTTON(				$
				buttonBase,				$
				UVALUE = 'setplane',			$
				RESOURCE_NAME = 'menu_level_0',		$
				VALUE = '    Set Active Plane    ',	$
				FONT = GetCorrectFont('courier2bold') )

	clearButton	= WIDGET_BUTTON(				$
				buttonBase,				$
				UVALUE = 'clearplane',			$
				RESOURCE_NAME = 'menu_level_0',		$
				VALUE = '   Clear Active Plane   ',	$
				FONT = GetCorrectFont('courier2bold') )

	optionButton	= WIDGET_BUTTON(				$
				buttonBase,				$
				UVALUE = 'options',			$
				RESOURCE_NAME = 'menu_level_0',		$
				VALUE = 'Plane Display Options...',	$
				FONT = GetCorrectFont('courier2bold') )

	viewBaseSub1	= WIDGET_BASE(					$
				viewBase,				$
				/ROW,					$
				/ALIGN_CENTER )

	emptyText	= [						$
				'',					$
				'NOT SET',				$
				'',					$
				'' ]
;ckt,mar99	font2Use	= 'Courier*Bold'
	font2Use	= 'Helvetica*Bold'
	;----------------------------------------------------------------
	; Set up 6 MISR_DATA_MENU_OBJ objects; see the source code for
	; MISR_DATA_MENU_OBJ for a description of the keywords
	;----------------------------------------------------------------
	nPlanes		   = 6
	planeObjArr	   = OBJARR(nPlanes)
	planeObjInfoStruct = { fname:'', grid:'', field:'', num_type:'', extraDims: PTR_NEW() }
	planeObjStructArray= REPLICATE( planeObjInfoStruct, nPlanes )

        activeTextColor = [ 255, 0, 0 ]
        inactiveTextColor = [ 255, 255, 255 ]
;   IF KEYWORD_SET(usePalette) THEN print, 'setting extra in get lower base contents' $
;   ELSE print,'use palette NOT set in get lower base contents'
        IF KEYWORD_SET(usePalette) THEN e = {USE_PALETTE:usePalette}
        
	planeObjArr[0]	= OBJ_NEW( 'MISR_DATA_MENU_OBJECT',		$
				viewBaseSub1,				$
				DRAW_XSIZE	= drawXsize,		$
				DRAW_YSIZE	= drawYsize,		$
				FONT_STRING = font2Use,			$
				ACTIVE_TEXT_COLOR = activeTextColor,	$
				INACTIVE_TEXT_COLOR = inactiveTextColor,	$
				TITLE = 'RED PLANE',			$
				IS_ACTIVE = 1,				$
				CHAR_DIMS = [0,0],			$
				TEXT_STRING = emptyText, _EXTRA = e )
				
        activeTextColor = [ 0, 255, 0 ]

	planeObjArr[1]	= OBJ_NEW( 'MISR_DATA_MENU_OBJECT',		$
				viewBaseSub1,				$
				DRAW_XSIZE	= drawXsize,		$
				DRAW_YSIZE	= drawYsize,		$
				FONT_STRING = font2Use,			$
				ACTIVE_TEXT_COLOR = activeTextColor,	$
				INACTIVE_TEXT_COLOR = inactiveTextColor,	$
				TITLE = 'GREEN PLANE',			$
				CHAR_DIMS = [0,0],			$
				TEXT_STRING = emptyText, _EXTRA = e )
				
        activeTextColor = [ 0, 0, 255 ]
        
	planeObjArr[2]	= OBJ_NEW( 'MISR_DATA_MENU_OBJECT',		$
				viewBaseSub1,				$
				DRAW_XSIZE	= drawXsize,		$
				DRAW_YSIZE	= drawYsize,		$
				ACTIVE_TEXT_COLOR = activeTextColor,	$
				INACTIVE_TEXT_COLOR = inactiveTextColor,	$
				FONT_STRING = font2Use,			$
				TITLE = 'BLUE PLANE',			$
				CHAR_DIMS = [0,0],			$
				TEXT_STRING = emptyText, _EXTRA = e )

	viewBaseSub2	= WIDGET_BASE(					$
				viewBase,				$
				/ROW,					$
				/ALIGN_CENTER )

        activeTextColor = [ 255, 0, 255 ]
        
	planeObjArr[3]	= OBJ_NEW( 'MISR_DATA_MENU_OBJECT',		$
				viewBaseSub2,				$
				DRAW_XSIZE	= drawXsize,		$
				DRAW_YSIZE	= drawYsize,		$
				FONT_STRING = font2Use,			$
				ACTIVE_TEXT_COLOR = activeTextColor,	$
				INACTIVE_TEXT_COLOR = inactiveTextColor,	$
				TITLE = 'ANCILLARY 1 PLANE',		$
				CHAR_DIMS = [0,0],			$
				TEXT_STRING = emptyText, _EXTRA = e )

        activeTextColor = [ 255, 0, 255 ]
        
	planeObjArr[4]	= OBJ_NEW( 'MISR_DATA_MENU_OBJECT',		$
				viewBaseSub2,				$
				DRAW_XSIZE	= drawXsize,		$
				DRAW_YSIZE	= drawYsize,		$
				FONT_STRING = font2Use,			$
				ACTIVE_TEXT_COLOR = activeTextColor,	$
				INACTIVE_TEXT_COLOR = inactiveTextColor,	$
				TITLE = 'ANCILLARY 2 PLANE',		$
				CHAR_DIMS = [0,0],			$
				TEXT_STRING = emptyText, _EXTRA = e )

        activeTextColor = [ 255, 0, 255 ]
        
	planeObjArr[5]	= OBJ_NEW( 'MISR_DATA_MENU_OBJECT',		$
				viewBaseSub2,				$
				DRAW_XSIZE	= drawXsize,		$
				DRAW_YSIZE	= drawYsize,		$
				FONT_STRING = font2Use,			$
				ACTIVE_TEXT_COLOR = activeTextColor,	$
				INACTIVE_TEXT_COLOR = inactiveTextColor,	$
				TITLE = 'ANCILLARY 3 PLANE',		$
				CHAR_DIMS = [0,0],			$
				TEXT_STRING = emptyText, _EXTRA = e )

	;----------------------------------------------------------------
	; Set up a structure containing all information to be accessed
	; by event handlers, and return
	;----------------------------------------------------------------

	dlb		= [dataLabelBaseA,dataLabelBaseB]

	stashBase       = WIDGET_BASE(UVALUE=(-1L))

	info		= {						$
			nPlanes			:nPlanes,		$
			planeObjArr		:planeObjArr,		$
			currentPlaneObj		:planeObjArr[0],	$
			nextActivePlaneObj	:OBJ_NEW(),		$
			dataLabelLenThresh	:STRLEN( noneText ),	$
			dataLabelBase		:dlb,			$
			dataLabel		:[dataLabel,dataText],	$
			resBase1		:dataSubBase2,		$
			resBase2		:dataSubBase3,		$
			altResMenu		:altResMenu,		$
			actResMenu		:actResMenu,		$
			altResLabel		:altResLabel,		$
			actResLabel		:actResLabel,		$
			setButton		:setButton,		$
;;;ckt,aug2000			enableMem		:0,			$
;;;ckt,aug2000			memCurr			:0.0,			$
;;;ckt,aug2000			memCum			:0.0,			$
;;;ckt,aug2000			memAvail		:128.0,			$
;;;ckt,aug2000			memCurrLabel		:memLabel3,		$
;;;ckt,aug2000			memCumLabel		:memLabel5,		$
;;;ckt,aug2000			memAvailLabel		:memLabel7,		$
			mappedDataLabelBaseIdx	:0,			$
			dataMenu                :dataMenu,              $
			stashBase               :stashBase,             $
			planeObjStructArray     :planeObjStructArray    $
			 }

	infoPtr		= PTR_NEW( info, /NO_COPY )

	WIDGET_CONTROL, topBase, SET_UVALUE = infoPtr

	WIDGET_CONTROL, topBase,					$
			EVENT_FUNC = 'GetLowerBaseContents_ev',		$
			KILL_NOTIFY = 'GetLowerBaseContents_kill'
;print,'@@@@@@@@@@@@@@@@ returning from GetLowerBaseContents @@@@@@@@@@@@@@@@@@'
END
; GetLowerBaseContents
