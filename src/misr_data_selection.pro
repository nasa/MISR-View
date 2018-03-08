@fill_data_button
@blockchooser_before_realize
@blockchooser_after_realize

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@ misr_data_selection_upperBase_kill @@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO misr_data_selection_upperBase_kill, upperBase
	WIDGET_CONTROL, upperBase, GET_UVALUE = ptr
	IF PTR_VALID(ptr) THEN PTR_FREE, ptr
END
; misr_data_selection_upperBase_kill

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ misr_data_selection_kill @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO misr_data_selection_kill, tlb
	WIDGET_CONTROL, tlb, GET_UVALUE = ptr
	IF PTR_VALID(ptr) THEN BEGIN
		WIDGET_CONTROL, (*ptr).tlb, GET_UVALUE = tlbPtr
		IF PTR_VALID(tlbPtr) THEN				$
			WIDGET_CONTROL, (*tlbPtr).dataSelectToggleID,	$
					SET_VALUE = 'Show Data Selection Interface'
		PTR_FREE, ptr
	ENDIF
END
; misr_data_selection_kill

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ misr_data_selection_ev @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO misr_data_selection_ev, event
	returnStruc = {							$
		   MISR_DATA_SELECTION,					$
   		   id		:event.id,				$
		   top		:event.top,				$
		   handler	:event.handler,				$
		   widgetType	:TAG_NAMES( event, /STRUCTURE_NAME ) }
	misr_view_eh, returnStruc
END
; misr_data_selection_ev

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ misr_data_selection @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;;jan99, ckt FUNCTION misr_data_selection, parent, catContents, _Extra = e
FUNCTION misr_data_selection, parent, catContentsPtr, elevationMapPtr, agpPtr, headerSize, _Extra = e

DEVICE, GET_SCREEN_SIZE = ss
ss_x = ss[0]
ss_y = ss[1]
IF ss_x LT 1280 OR ss_y LT 1024 THEN BEGIN
	tlb		= WIDGET_BASE(					$
			       /COLUMN,					$
			       TITLE = 'MISR Data Selection Interface',	$
			       RESOURCE_NAME = 'glbc',			$
			       GROUP_LEADER = parent,			$
			       EVENT_PRO = 'misr_data_selection_ev',	$
			       TLB_FRAME_ATTR = 9,			$
			       KILL_NOTIFY = 'misr_data_selection_kill',$
				/SCROLL,				$
				X_SCROLL_SIZE = ss_x - 100,		$
				Y_SCROLL_SIZE = ss_y - 100 )
ENDIF ELSE BEGIN
	tlb		= WIDGET_BASE(					$
			       /COLUMN,					$
			       TITLE = 'MISR Data Selection Interface',	$
			       RESOURCE_NAME = 'glbc',			$
			       GROUP_LEADER = parent,			$
			       EVENT_PRO = 'misr_data_selection_ev',	$
			       TLB_FRAME_ATTR = 9,			$
			       KILL_NOTIFY = 'misr_data_selection_kill' )
ENDELSE			    
			    

	upperBase	= WIDGET_BASE( tlb,				$
				       /ROW,				$
				       KILL_NOTIFY = 'misr_data_selection_upperBase_kill' )

	;---------------------------------
	; Jeff's interface setup goes here
	;---------------------------------
;;;jan99, ckt	blockchooser_before_realize, upperBase, catContents, _EXTRA = e
	blockchooser_before_realize, upperBase, catContentsPtr, elevationMapPtr, agpPtr, headerSize, _EXTRA = e

	lowerBase	= WIDGET_BASE( tlb,				$
				       FRAME = 3,			$
				       /COLUMN,				$
				       EVENT_PRO = 'misr_data_selection_ev' )

	;---------------------------------
	; retrieve Charles' interface
	;---------------------------------
	IF !D.N_COLORS LE 256 THEN BEGIN
		WIDGET_CONTROL, upperBase, GET_UVALUE = statePtr
		EightBit_RestoreColorMap, statePtr
		TVLCT, r, g, b, /GET
		use_palette	= {r:r,g:g,b:b}
		GetLowerBaseContents, lowerBase, USE_PALETTE = use_palette
	ENDIF ELSE BEGIN
		GetLowerBaseContents, lowerBase
	ENDELSE

	btBase		= WIDGET_BASE( tlb, /ALIGN_CENTER, /ROW )

	;---------------------------------
	; UVALUE of storeBase is used to 
	; store user-selected directory 
	; location STORE and RECALL files.
	; The same stored directory is 
	; used for both.
	;---------------------------------
	storeBase		= WIDGET_BASE( btBase, /ROW, /FRAME )
	DataSelectionLabel	= WIDGET_LABEL( storeBase, VALUE = 'Data Selection Parameters:' )
	storeDataSelectionBt	= WIDGET_BUTTON( storeBase,			$
					 RESOURCE_NAME = 'menu_level_0',	$
					 VALUE = 'Store' )
	recallDataSelectionBt	= WIDGET_BUTTON( storeBase,			$
					 RESOURCE_NAME = 'menu_level_0',	$
					 VALUE = 'Recall' )

	rotateBase	= WIDGET_BASE( btBase, /ROW, /FRAME )
	rotateLa	= WIDGET_LABEL( rotateBase, VALUE = 'Rotate' )
	rotateTx	= WIDGET_TEXT( rotateBase, VALUE = '0.0', XSIZE = 6, /EDITABLE )
	degreeLa	= WIDGET_LABEL( rotateBase, VALUE = 'Deg.' )

	createBase	= WIDGET_BASE( btBase, /ROW, /FRAME )
	createBt	= WIDGET_BUTTON( createBase,				$
					 UVALUE = 'create',			$
					 RESOURCE_NAME = 'menu_level_0',	$
					 VALUE = 'Create Viewer' )

	WIDGET_CONTROL, lowerBase, SENSITIVE = 0
	WIDGET_CONTROL, upperBase, SENSITIVE = 0
	WIDGET_CONTROL, storeBase, SENSITIVE = 0
	WIDGET_CONTROL, rotateBase, SENSITIVE = 0
	WIDGET_CONTROL, createBt, SENSITIVE = 0

	WIDGET_CONTROL, tlb, /REALIZE

	;------------------------------------------------
	; get widget draw window ID from Jeff's interface
	;------------------------------------------------
	blockchooser_after_realize, upperBase

	;------------------------------------------------
	; set some buttons on Charles' interface
	;------------------------------------------------
	WIDGET_CONTROL, lowerBase, GET_UVALUE = lowerInfoPtr
	WIDGET_CONTROL, (*lowerInfoPtr).setButton, SENSITIVE = 0
	WIDGET_CONTROL, (*lowerInfoPtr).altResMenu, SET_UVALUE = (*lowerInfoPtr).altResLabel
	WIDGET_CONTROL, (*lowerInfoPtr).actResMenu, SET_UVALUE = (*lowerInfoPtr).actResLabel
      
	WIDGET_CONTROL, createBt, SENSITIVE = 0
	nplanes		= (*lowerInfoPtr).nplanes
	gridInfo	= STRARR( nplanes, 3 )

	infoPtr   = PTR_NEW( {						$
				lowerInfoPtr	:lowerInfoPtr,		$
				rotateTx	:rotateTx,		$
				createID	:createBt,		$
				gridInfo	:gridInfo,		$
				tlb		:parent } )	; don't do a /NO_COPY!

	WIDGET_CONTROL, tlb, SET_UVALUE = infoPtr

	WIDGET_CONTROL, upperBase, GET_UVALUE = upperBaseStatePtr
	WIDGET_CONTROL, (*upperBaseStatePtr).pathText, GET_VALUE = pathStrArr
	WIDGET_CONTROL, (*upperBaseStatePtr).orbitText, GET_VALUE = orbitStrArr

	unset_planes, upperBaseStatePtr, LONG(orbitStrArr[0]),FIX(pathStrArr[0])

	WIDGET_CONTROL, lowerBase, SENSITIVE = 1
	WIDGET_CONTROL, upperBase, SENSITIVE = 1
	WIDGET_CONTROL, storeBase, SENSITIVE = 1
	WIDGET_CONTROL, rotateBase, SENSITIVE = 1

	RETURN, tlb
END
; misr_data_selection
