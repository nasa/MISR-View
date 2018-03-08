PRO fileList_event, event
	TRUE	= 1
	WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
	
	IF ( event.clicks GE 2 ) THEN BEGIN
	   (*infoPtr).selectedIdx = event.index
	   WIDGET_CONTROL, event.top, /DESTROY
	   RETURN
	ENDIF
	
	(*infoPtr).selectedIdx = event.index
END
; fileList_event

PRO okButton_event, event
	WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
	(*infoPtr).okPressed = 1
	WIDGET_CONTROL, event.top, /DESTROY
END
; okButton_event

PRO cancelButton_event, event
	WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
	(*infoPtr).cancelPressed = 1
	WIDGET_CONTROL, event.top, /DESTROY
END
; cancelButton_event

PRO ReturnListSelectionIdx_killnotify, tlb
	WIDGET_CONTROL, tlb, GET_UVALUE = infoPtr
	IF NOT (*infoPtr).okPressed AND NOT (*infoPtr).cancelPressed THEN $
		(*infoPtr).selectedIdx = (-1)
END

FUNCTION ReturnListSelectionIdx,						$
				WINDOW_TITLE = windowTitle,			$
				LIST_TITLE = listTitle,				$
				LIST_CONTENTS = listContents,			$
				OK_BUTTON_TITLE = okButtonTitle,		$
				CANCEL_BUTTON_TITLE = cancelButtonTitle,	$
				GROUP_LEADER = groupLeader,			$
				NO_CANCEL = noCancel
			
	IF ( NOT KEYWORD_SET( windowTitle ) ) THEN windowTitle = 'Please Make A Selection'
	IF ( NOT KEYWORD_SET( listTitle ) ) THEN listTitle = 'Current List'
	listSubTitle = '(double-click on list item to select)'
	IF ( NOT KEYWORD_SET( listContents ) ) THEN BEGIN
		listContents	= '<none>'
		nItems		= 0
	ENDIF ELSE BEGIN
		sz		= SIZE( listContents )
		IF ( sz( 0 ) EQ 0 )	THEN nItems = 1 $
					ELSE nItems = sz( 1 )
	ENDELSE
	IF ( NOT KEYWORD_SET( okButtonTitle ) ) THEN okButtonTitle = 'OK'
	IF ( NOT KEYWORD_SET( cancelButtonTitle ) ) THEN cancelButtonTitle = 'Cancel'
	IF KEYWORD_SET(groupLeader) THEN					$
		pickAddBase	= WIDGET_BASE(	TITLE = windowTitle,		$
						GROUP_LEADER = groupLeader,	$
						/MODAL,                         $
						/COLUMN,			$
						/BASE_ALIGN_CENTER,		$
						KILL_NOTIFY = 'ReturnListSelectionIdx_killnotify' )	$
	ELSE									$
		pickAddBase	= WIDGET_BASE(	TITLE = windowTitle,		$
						/COLUMN,			$
						/BASE_ALIGN_CENTER,		$
						KILL_NOTIFY = 'ReturnListSelectionIdx_killnotify' )
						
	listBase	= WIDGET_BASE(	pickAddBase,				$
					/COLUMN,				$
					FRAME = 5,				$
					/BASE_ALIGN_CENTER )
	fileLabel1	= WIDGET_LABEL(	listBase,				$
					VALUE = listTitle,			$
					/ALIGN_CENTER )
	fileLabel2	= WIDGET_LABEL(	listBase,				$
					VALUE = listSubTitle,			$
					/ALIGN_CENTER )
	fileList	= WIDGET_LIST(	listBase,				$
					VALUE = listContents,			$
					SCR_YSIZE = 100,			$
					SCR_XSIZE = 800,			$
					EVENT_PRO = 'fileList_event' )
	okButton	= WIDGET_BUTTON(pickAddBase,				$
					VALUE = okButtonTitle,			$
					EVENT_PRO = 'okButton_event' )
	IF NOT KEYWORD_SET(noCancel) THEN					$
		cancelButton	= WIDGET_BUTTON(pickAddBase,			$
						VALUE = cancelButtonTitle,	$
						EVENT_PRO = 'cancelButton_event' )
					
	WIDGET_CONTROL, pickAddBase, /REALIZE
	
	WIDGET_CONTROL, fileList, SET_LIST_SELECT = 0
	
	infoStruct	= {	n_items		:nItems,			$
				list_id		:fileList,			$
				selectedIdx	:0,				$
				cancelPressed	:0,				$
				okPressed	:0 }
				
	infoPtr		= PTR_NEW( infoStruct, /NO_COPY )
	
	WIDGET_CONTROL, pickAddBase, SET_UVALUE = infoPtr

	XMANAGER, 'ReturnListSelectionIdx', pickAddBase
	
	IF ( (*infoPtr).selectedIdx LT 0 OR (*infoPtr).cancelPressed ) THEN	$
	   returnedIdx = (-1)								$
	ELSE										$
	   returnedIdx = (*infoPtr).selectedIdx
		
	RETURN, returnedIdx
END
; ReturnListSelectionIdx
