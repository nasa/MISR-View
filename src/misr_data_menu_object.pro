;12345678901234567890123456789012345678901234567890123456789012345678901234
;+
;==========================================================================
;
;Module Name:	MISR_DATA_MENU_OBJECT
;
;Call Protocol:	x = OBJ_NEW(						$
;			'MISR_DATA_MENU_OBJECT',			$
;			parentBase,					$
;			DRAW_XSIZE = drawXsize,				$
;			DRAW_YSIZE = drawYsize,				$
;			FONT_STRING = fontString,			$
;			ACTIVE_TEXT_COLOR = activeTextColor,		$
;			INACTIVE_TEXT_COLOR = inActiveTextColor,	$
;			CHAR_DIMS = charDims,				$
;			FONT_SIZE = fontSize,				$
;			TEXT_STRING = textString,			$
;			TEXT_LOCATION = textLocation,			$
;			IS_ACTIVE = isActive,				$
;			TITLE = title
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
;	The MISR_DATA_MENU_OBJECT is a special-purpose object for the MISRTOOL
;	IDL program being developed at JPL by Charles Thompson and Jeff Hall.
;	The object is essentially a widget draw realized as object-graphics
;	compatible, with some rendered text drawn within.
;
;Input Parameters (INIT method):
;
;	Type	Name		Units		Purpose
;	-------------------------------------------------------------------
;	    	parentBase		the base ID to which the object
;					belongs
;	INTEGER	DRAW_XSIZE		horizontal dimension of widget_draw
;	INTEGER	DRAW_XSIZE		vertical dimension of widget_draw
;	STRING	FONT_STRING		font type to use for rendered text
;	BYTARR	ACTIVE_TEXT_COLOR	color triplet to use for "active"
;	BYTARR	INACTIVE_TEXT_COLOR	color triplet to use for "inactive"
;					text
;	INTARR	CHAR_DIMS		array of 2 specifying character
;					spacing; generally speaking, this
;					keyword should NOT be set
;	INTEGER	FONT_SIZE		point size of characters
;	STRING	TEXT_STRING		the text to be rendered in the
;					widget_draw
;	INTARR	TEXT_LOCATION		x, y, and z locations of the text
;					within the widget_draw; generally
;					speaking, this should NOT be set
;	INTEGER	IS_ACTIVE		toggle specifying whether the text
;					should be rendered in the "active"
;					color or the "inactive" color
;	STRING	TITLE			the title of the widget_draw; this
;					string is rendered "above" the other
;					text in the widget_draw
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
;Return Values (INIT):
;
;	Type	Name	Units	Purpose
;	-------------------------------------------------------------------
;	INTEGER	success	none	specifies successful instantiation of
;				MISR_DATA_MENU_OBJECT
;	INTEGER	failure	none	specifies unsuccessful instantiation of
;				MISR_DATA_MENU_OBJECT
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

;@misr_data_menu_draw_object.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::SetFont @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_DATA_MENU_OBJECT::SetFont, font2Use
;-------------------------------------------
; Utility method which sets the font type of
; the text in the widget_draw; font2Use is
; a string such as "helvetica*bold"
;-------------------------------------------
	SELF.fontObj->SetProperty, NAME = font2Use
	SELF->Redraw
END
; MISR_DATA_MENU_OBJECT::SetFont

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::SetSize @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_DATA_MENU_OBJECT::SetSize, size2Use
;-------------------------------------------
; Utility method which sets the point size
; of the text in the widget_draw
;-------------------------------------------
	SELF.fontObj->SetProperty, SIZE = size2Use
	SELF->Redraw
END
; MISR_DATA_MENU_OBJECT::SetSize

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::GetCurrentTextSize @@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION MISR_DATA_MENU_OBJECT::GetCurrentTextSize
;-------------------------------------------
; Utility method which returns the point
; size of the text in the widget_draw
;-------------------------------------------
	SELF.fontObj->GetProperty, SIZE = textSize
	RETURN, STRTRIM( STRING( FIX( textSize ) ), 2 )
END
; MISR_DATA_MENU_OBJECT::GetCurrentTextSize

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::GetCurrentFont @@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION MISR_DATA_MENU_OBJECT::GetCurrentFont
;-------------------------------------------
; Utility method which returns the font type
; of the text in the widget_draw; currentFont
; is a string such as "helvetica*bold"
;-------------------------------------------
	SELF.fontObj->GetProperty, NAME = fontName
	validFontTypes	= [						$
				'helvetica',				$
				'courier' ]
	fontName	= STRLOWCASE( fontName )
	FOR i = 0, N_ELEMENTS( validFontTypes ) - 1 DO			$
		IF STRPOS( fontName, validFontTypes[ i ] ) GE 0 THEN	$
			currentFont = validFontTypes[ i ]
	RETURN, currentFont
END
;

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::ZoomIn @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_DATA_MENU_OBJECT::ZoomIn
;-------------------------------------------
; Method which changes the z-location of the
; text to make it appear that the text is
; closer to the eye; if the location exceeds
; 3.0 (the closest the text can be to the
; eye), a message appears notifying the user
;-------------------------------------------
	IF ( SELF.currentTextZinc + SELF.textZinc GT 3.0 ) THEN BEGIN
		res	= DIALOG_MESSAGE(				$
			'Currently at maximum zoom level',		$
			/INFORMATION )
		RETURN
	ENDIF
	SELF.currentTextZinc	= SELF.currentTextZinc + SELF.textZinc
	nLines	= (SIZE(*(SELF.textLocationPtr)))[2]
	FOR i = 0, nLines - 1 DO					$
		(*(SELF.textLocationPtr))[ 2, i ] =			$
			SELF.currentTextZinc
	SELF.textObj->SetProperty, LOCATIONS = *(SELF.textLocationPtr)
	SELF->Redraw
END
; MISR_DATA_MENU_OBJECT::ZoomIn

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::ZoomOut @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_DATA_MENU_OBJECT::ZoomOut
;-------------------------------------------
; Method which changes the z-location of the
; text to make it appear that the text is
; farther from the eye; if the location falls
; below -3.0 (the farthest the text can be 
; from the eye), a message appears notifying
; the user
;-------------------------------------------
	IF ( SELF.currentTextZinc - SELF.textZinc LT -3.0 ) THEN BEGIN
		res	= DIALOG_MESSAGE(				$
			'Currently at minimum zoom level',		$
			/INFORMATION )
		RETURN
	ENDIF
	SELF.currentTextZinc	= SELF.currentTextZinc - SELF.textZinc
	nLines	= (SIZE(*(SELF.textLocationPtr)))[2]
	FOR i = 0, nLines - 1 DO					$
		(*(SELF.textLocationPtr))[2,i] =			$
			SELF.currentTextZinc
	SELF.textObj->SetProperty, LOCATIONS = *(SELF.textLocationPtr)
	SELF->Redraw
END
; MISR_DATA_MENU_OBJECT::ZoomOut

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::GetDrawID @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION MISR_DATA_MENU_OBJECT::GetDrawID
;-------------------------------------------
; Method which returns the title as a string
;-------------------------------------------
	RETURN, SELF.drawID
END
; MISR_DATA_MENU_OBJECT::GetDrawID

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::GetTitle @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION MISR_DATA_MENU_OBJECT::GetTitle
;-------------------------------------------
; Method which returns the title as a string
;-------------------------------------------
	RETURN, SELF.title
END
; MISR_DATA_MENU_OBJECT::GetTitle

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::SetMemoryValue @@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_DATA_MENU_OBJECT::SetMemoryValue, memVal
;-------------------------------------------
; Method which sets the memory attribute of
; this object
;-------------------------------------------
	SELF.memoryValue	= memVal
END
; MISR_DATA_MENU_OBJECT::SetMemoryValue

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::GetMemoryValue @@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION MISR_DATA_MENU_OBJECT::GetMemoryValue
;-------------------------------------------
; Method which returns the memory attribute of
; this object
;-------------------------------------------
	RETURN, SELF.memoryValue
END
; MISR_DATA_MENU_OBJECT::GetMemoryValue

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::Redraw @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_DATA_MENU_OBJECT::Redraw
;-------------------------------------------
; Method which redraws the view object inside
; the widget_draw
;-------------------------------------------
;print,'calling redraw'
	SELF.drawObj->Draw, SELF.viewObj
END
; MISR_DATA_MENU_OBJECT::Redraw

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::SetText @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_DATA_MENU_OBJECT::SetText, str
;-------------------------------------------
; Method which sets the text inside
; the widget_draw
;-------------------------------------------
	PTR_FREE, SELF.textStringPtr
	SELF.textStringPtr	= PTR_NEW( str )
	outStr	= [									$
				SELF.title,						$
				str ]
	SELF.textObj->SetProperty, STRINGS = outStr
	SELF.drawObj->Draw, SELF.viewObj
END
; MISR_DATA_MENU_OBJECT::SetText

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::GetText @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION MISR_DATA_MENU_OBJECT::GetText
;-------------------------------------------
; Method which gets the text inside
; the widget_draw
;-------------------------------------------
	SELF.textObj->GetProperty, STRINGS = outStr
	RETURN, [outStr]
END
; MISR_DATA_MENU_OBJECT::GetText

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::SetDrawObj @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_DATA_MENU_OBJECT::SetDrawObj, drawObj
;-------------------------------------------
; Method which sets the drawObj member to
; the widget_draw's object graphics
;-------------------------------------------
	SELF.drawObj	= drawObj
	IF OBJ_VALID(SELF.paletteObj) THEN SELF.drawObj->SetProperty, PALETTE=SELF.paletteObj
	SELF.drawObj->Draw, SELF.viewObj
END
; MISR_DATA_MENU_OBJECT::SetDrawObj

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::Activate @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_DATA_MENU_OBJECT::Activate
;-------------------------------------------
; Method which sets the color of the text to
; its "active" color
;-------------------------------------------
	IF NOT SELF.isActive THEN BEGIN
		SELF.textObj->SetProperty, COLOR = *(SELF.activeTextColor)
		SELF.drawObj->Draw, SELF.viewObj
		SELF.isActive = 1
	ENDIF
END
; MISR_DATA_MENU_OBJECT::Activate

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::Deactivate @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_DATA_MENU_OBJECT::Deactivate
;-------------------------------------------
; Method which sets the color of the text to
; its "inactive" color
;-------------------------------------------
	IF SELF.isActive THEN BEGIN
		SELF.textObj->SetProperty, COLOR = *(SELF.inActiveTextColor)
		SELF.drawObj->Draw, SELF.viewObj
		SELF.isActive = 0
	ENDIF
END
; MISR_DATA_MENU_OBJECT::Deactivate

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ misr_data_menu_obj_realize @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO misr_data_menu_obj_realize, drawID
;-------------------------------------------
; Routine called only when the object is 
; realized; the draw object is retrieved
; from the widget_draw and the method
; misrDataMenuObj->SetDrawObj is invoked
;-------------------------------------------
	WIDGET_CONTROL, drawID, GET_UVALUE = misrDataMenuObj
	WIDGET_CONTROL, drawID, GET_VALUE = drawObj
	misrDataMenuObj->SetDrawObj, drawObj
	
drawObj->GetProperty,RETAIN=retain
;print,'retain value for misr_data_menu_object = ',retain
;print,'object_class = ',obj_class(drawObj)

;drawObj->GetProperty, COLOR_MODEL = colorModel
;drawObj->GetProperty, DIMENSIONS  = dims
;drawObj->GetProperty, GRAPHICS_TREE  = g_tree
;drawObj->GetProperty, LOCATION  = loc
;drawObj->GetProperty, PALETTE  = paletteObj
;drawObj->GetProperty, QUALITY  = qual
;drawObj->GetProperty, UNITS  = units
;drawObj->GetProperty, UVALUE  = uval

;obj_destroy, drawObj

;newDrawObj = OBJ_NEW('MISR_DATA_MENU_DRAW_OBJECT', $
;   COLOR_MODEL = colorModel, $
;   DIMENSIONS  = dims, $
;   GRAPHICS_TREE  = g_tree, $
;   LOCATION  = loc, $
;   PALETTE  = paletteObj, $
;   QUALITY  = qual, $
;  UNITS  = units, $
;   UVALUE  = uval)
;misrDataMenuObj->SetDrawObj, newDrawObj

END
; misr_data_menu_obj_realize

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ misr_data_menu_obj_ev @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION misr_data_menu_obj_ev, event
;-------------------------------------------
; Event handler for the object; the event
; structure is simply passed on after
; being modified to include the object's
; reference 
;-------------------------------------------
	WIDGET_CONTROL, event.id, GET_UVALUE = misrDataMenuObj
;help,event
;help,event.id
;help,event.top
;help,event.handler
;help,misrDataMenuObj
;help,event.type
;help,event.x
;help,event.y
;help,event.press
;help,event.release
;help,event.clicks
	evStruct	= {						$
					MISR_DATA_MENU_OBJ,		$
					ID	:event.id,		$
					TOP	:event.top,		$
					HANDLER	:event.handler,		$
					OBJ	:misrDataMenuObj,	$
					TYPE	:event.type,		$
					X	:event.x,		$
					Y	:event.y,		$
					PRESS	:event.press,		$
					RELEASE	:event.release,		$
					CLICKS	:event.clicks }
;print,'event.type inside MISR_DATA_MENU_OBJ = ',event.type
	IF event.type eq 4 THEN misrDataMenuObj->Redraw
	RETURN, evStruct
END

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::INIT @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION MISR_DATA_MENU_OBJECT::INIT,					$
			parentBase,					$
			DRAW_XSIZE = drawXsize,				$
			DRAW_YSIZE = drawYsize,				$
			FONT_STRING = fontString,			$
			ACTIVE_TEXT_COLOR = activeTextColor,		$
			INACTIVE_TEXT_COLOR = inActiveTextColor,	$
			CHAR_DIMS = charDims,				$
			FONT_SIZE = fontSize,				$
			TEXT_STRING = textString,			$
			TEXT_LOCATION = textLocation,			$
			IS_ACTIVE = isActive,				$
			COLOR_MODEL = colorModel,			$
			TITLE = title,					$
			USE_PALETTE = usePalette
;-------------------------------------------
; INIT method called when the object is
; instantiated
;-------------------------------------------
	success	= 1
	failure	= 0

	SELF.parentBase	= parentBase

	;-------------------------------------------
	; TITLE keyword (default = '')
	;-------------------------------------------
	IF KEYWORD_SET( title ) THEN					$
		SELF.title = title					$
	ELSE								$
		SELF.title = ''
	;-------------------------------------------
	; DRAW_XSIZE keyword (default = 100)
	;-------------------------------------------
	IF KEYWORD_SET( drawXsize ) THEN				$
		SELF.drawXsize = drawXsize				$
	ELSE								$
		SELF.drawXsize = 100
	;-------------------------------------------
	; DRAW_YSIZE keyword (default = 100)
	;-------------------------------------------
	IF KEYWORD_SET( drawYsize ) THEN				$
		SELF.drawYsize = drawYsize				$
	ELSE								$
		SELF.drawYsize = 100
	;-------------------------------------------
	; FONT_STRING keyword (default = 'Courier')
	;-------------------------------------------
	IF KEYWORD_SET( fontString ) THEN				$
		SELF.fontString = fontString				$
	ELSE								$
;ckt,mar99		SELF.fontString = 'Courier'
		SELF.fontString = 'Helvetica'
	;-------------------------------------------
	; ACTIVE_TEXT_COLOR keyword (default = [0,0,0])
	;-------------------------------------------
	IF KEYWORD_SET( activeTextColor ) THEN					$
		SELF.activeTextColor = PTR_NEW(activeTextColor,/NO_COPY)	$
	ELSE									$
		SELF.activeTextColor = PTR_NEW([0,0,0],/NO_COPY)
	;-------------------------------------------
	; INACTIVE_TEXT_COLOR keyword (default = [255,255,255])
	;-------------------------------------------
	IF KEYWORD_SET( inActiveTextColor ) THEN				$
		SELF.inActiveTextColor = PTR_NEW(inActiveTextColor,/NO_COPY)	$
	ELSE									$
		SELF.inActiveTextColor = PTR_NEW([255,255,255],/NO_COPY)
		
;print,'*(SELF.activeTextColor) = ',*(SELF.activeTextColor)
;tvlct,rr,gg,bb,/get
;print,'rr[*(SELF.activeTextColor)],gg[*(SELF.activeTextColor)],bb[*(SELF.activeTextColor)]=', $
;   rr[*(SELF.activeTextColor)],gg[*(SELF.activeTextColor)],bb[*(SELF.activeTextColor)]
	;-------------------------------------------
	; CHAR_DIMS keyword (default = [ 0.050, 0.25 ])
	;-------------------------------------------
	IF NOT KEYWORD_SET( charDims ) THEN BEGIN
		CASE !version.os OF
			'sunos'	:charDims=[ 0.050, 0.25 ]
			'IRIX'	:charDims=[ 0.050, 0.25 ]
			'Win32'	:charDims=[ 0.100, 0.25 ]
			ELSE	:charDims=[ 0.100, 0.25 ]
		ENDCASE
	ENDIF
	SELF.charDims = charDims

	;-------------------------------------------
	; FONT_SIZE keyword (default = 12)
	;-------------------------------------------
	IF KEYWORD_SET( fontSize ) THEN					$
		SELF.fontSize = fontSize				$
	ELSE								$
;ckt,mar99		SELF.fontSize = 12
		SELF.fontSize = 10

	;-------------------------------------------
	; TEXT_STRING keyword (default = '')
	;-------------------------------------------
	IF NOT KEYWORD_SET( textString ) THEN textString = ''
	SELF.textStringPtr	= PTR_NEW( textString )
	
	;-------------------------------------------
	; TEXT_LOCATION keyword (default = depends
	; on the number of text lines... see loop
	; below)
	;-------------------------------------------
	IF NOT KEYWORD_SET( textLocation ) THEN BEGIN
		;-----------------------------------
		; add 1 to include title
		;-----------------------------------
		nStrings	= N_ELEMENTS( *(SELF.textStringPtr) )+1
		textLocation= FLTARR( 3, nStrings )
		yInc	= 0.40
		currY	= 1.15
		currZ	= 0.0
		FOR i = 0, nStrings - 1 DO BEGIN
			currY	= currY - yInc
			textLocation[ *, i ] = [ 0.0, currY, currZ ]
		ENDFOR
	ENDIF
	SELF.textLocationPtr	= PTR_NEW( textLocation, /NO_COPY )
	
	;-------------------------------------------
	; IS_ACTIVE keyword (default = 0, or FALSE)
	;-------------------------------------------
	IF KEYWORD_SET( isActive ) THEN					$
		SELF.isActive = isActive				$
	ELSE								$
		SELF.isActive = 0

	;-------------------------------------------
	; base containing draw widget
	;-------------------------------------------
	SELF.topBase	= WIDGET_BASE(					$
				SELF.parentBase,			$
				/FRAME,					$
				/ROW,					$
				/BASE_ALIGN_CENTER,			$
				EVENT_FUNC = 'misr_data_menu_obj_ev' )
				
colorModel = 0
IF KEYWORD_SET(usePalette) THEN BEGIN
;print,'setting palette object and color model'
   colorModel = 1
   SELF.paletteObj = OBJ_NEW('IDLgrPalette',usePalette.r, usePalette.g, usePalette.b)
ENDIF
	;-------------------------------------------
	; draw widget
	;-------------------------------------------
	SELF.drawID	= WIDGET_DRAW(					$
				SELF.topBase,				$
				XSIZE = SELF.drawXsize,			$
				YSIZE = SELF.drawYsize,			$
				/EXPOSE_EVENTS,				$
				/BUTTON_EVENTS,				$
				UVALUE = SELF,				$
				GRAPHICS_LEVEL = 2,			$
				COLOR_MODEL = colorModel,		$
				RETAIN = 0,				$
				NOTIFY_REALIZE = 'misr_data_menu_obj_realize' )
				
	;-------------------------------------------
	; font object
	;-------------------------------------------
	SELF.fontObj	= OBJ_NEW(					$
				'IDLgrFont',				$
				SELF.fontString,			$
				SIZE = SELF.fontSize )
				
	;-------------------------------------------
	; view object
	;-------------------------------------------
	SELF.viewObj	= OBJ_NEW(					$
				'IDLgrView',				$
				PROJECTION=2,				$
				ZCLIP = [ 3, -3 ],			$
				COLOR = [ 0, 0, 0 ] )

	;-------------------------------------------
	; model object
	;-------------------------------------------
	SELF.modelObj	= OBJ_NEW(					$
				'IDLgrModel' )

	IF SELF.isActive THEN						$
		color2Use = *(SELF.activeTextColor)			$
	ELSE								$
		color2Use = *(SELF.inActiveTextColor)
		
	text2Use	= [						$
				SELF.title,				$
				*(SELF.textStringPtr) ]

	;-------------------------------------------
	; text object
	;-------------------------------------------
	SELF.textObj	= OBJ_NEW(					$
				'IDLgrText',				$
				text2Use,				$
				COLOR = color2Use,			$
				ALIGNMENT = 0.5,			$
				FONT = SELF.fontObj,			$
				CHAR_DIMENSIONS = SELF.charDims,	$
				LOCATIONS = *(SELF.textLocationPtr) )

	;-------------------------------------------
	; add the text object to the model object
	; and the model object to the view object
	;-------------------------------------------
	SELF.modelObj->Add, SELF.textObj
	SELF.viewObj->Add, SELF.modelObj

	;-------------------------------------------
	; initialize memory, current text z-value,
	; and z-increment value
	;-------------------------------------------
	SELF.memoryValue	= 0.0
	SELF.currentTextZinc	= 0.0
	SELF.textZinc		= 0.5

	;-------------------------------------------
	; return success flag (1)
	;-------------------------------------------
	RETURN, success
END
; MISR_DATA_MENU_OBJECT::INIT

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT::CLEANUP @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_DATA_MENU_OBJECT::CLEANUP
;-------------------------------------------
; CLEANUP method called when the object is
; destroyed
;-------------------------------------------
	OBJ_DESTROY, SELF.drawObj
	OBJ_DESTROY, SELF.textObj
	OBJ_DESTROY, SELF.viewObj
	OBJ_DESTROY, SELF.modelObj
	OBJ_DESTROY, SELF.fontObj
	OBJ_DESTROY, SELF.paletteObj
	PTR_FREE, SELF.textLocationPtr
	PTR_FREE, SELF.textStringPtr
	PTR_FREE, SELF.activeTextColor
	PTR_FREE, SELF.inActiveTextColor
END
; MISR_DATA_MENU_OBJECT::CLEANUP

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ MISR_DATA_MENU_OBJECT__DEFINE @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_DATA_MENU_OBJECT__DEFINE
;-------------------------------------------
; Def'n of MISR_DATA_MENU_OBJECT__DEFINE
; (see header for a description of keywords)
;-------------------------------------------
	MISR_DATA_MENU_OBJECT		=	{			$
		MISR_DATA_MENU_OBJECT,					$
			drawObj			:OBJ_NEW(),		$
			textObj			:OBJ_NEW(),		$
			viewObj			:OBJ_NEW(),		$
			modelObj		:OBJ_NEW(),		$
			fontObj			:OBJ_NEW(),		$
			paletteObj		:OBJ_NEW(),		$
			activeTextColor		:PTR_NEW(),		$
			inActiveTextColor	:PTR_NEW(),		$
			fontString		:'Courier',		$
			fontSize		:12,			$
			charDims		:FLTARR(2),		$
			textStringPtr		:PTR_NEW(),		$
			title			:'',			$
			textLocationPtr		:PTR_NEW(),		$
			topBase			:0L,			$
			drawID			:0L,			$
			drawXsize		:100,			$
			drawYsize		:100,			$
			isActive		:0,			$
			memoryValue		:0.0,			$
			currentTextZinc		:0.0,			$
			textZinc		:0.5,			$
			parentBase		:0L }
END
; MISR_DATA_MENU_OBJECT__DEFINE
