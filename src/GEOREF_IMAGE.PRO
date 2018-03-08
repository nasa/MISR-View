@do_text.pro
@save_image_gui.pro
@display_save_image_help.pro

;01234567890123456789012345678901234567890123456789012345678901234567891
;+
;==========================================================================
;
;Module Name:	GEOREF_IMAGE
;
;Call Protocol:	obj = OBJ_NEW( 'GEOREF_IMAGE',				$
;			parentBase,					$
;			GROUP_LEADER = groupLeader,			$
;			LON_IMAGE = lonImg,				$
;			LAT_IMAGE = latImg,				$
;			DATA_PTR = dataPtr,				$
;			N_CHANNELS = nChannels,				$
;			XSIZE = xSize,					$
;			YSIZE = ySize,					$
;			MIN_XSIZE = minXsize,				$
;			MIN_YSIZE = minYsize,				$
;			DATA_IDX = dataIdx,				$
;			MENU_MODES = menuModes,                         $
;                       BUTTON_MODES = buttonModes,                     $
;                       NO_SELECT = noSelect )
;
;==========================================================================
;
;		Jet Propulsion Laboratory (JPL)
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
;		Jeffrey R Hall		jeffrey.r.hall@jpl.nasa.gov
;
;===========================================================================
;
;Description:
;
;	This is an IDL object which displays georeferenced images.
;
;Input Parameters:
;
;	Type	Name		Units		Purpose
;	-------------------------------------------------------------------
;	LONG	parentBase		ID of parent base that the
;					georef_image belongs to
; GROUP_LEADER	: group leader, if any
; LON_IMAGE	: image to use for longitude values
; LAT_IMAGE	: image to use for latitude values
; DATA_PTR	: PTRARR(), with each pointer referencing a different image
; N_CHANNELS	: either 1 or 3, analagous to 8-bit or 24-bit display
; XSIZE		: inital horizontal dimension of georef_image
; YSIZE		: inital vertical dimension of georef_image
; DATA_IDX	: index of image in the above PTRARR() whose dimensions and
;		  aspect ratio are to be used in displaying images in the
;		  georef_image
; MENU_MODES	: add'l menu items to be added; should be a STRARR() with
;		  dimensions nAddedMenus x 2, with (*,0) being the name
;		  of the menu (its displayed value), and (*,1) being its
;		  UVALUE (menu items are parsed based upon their UVALUE)
; BUTTON_MODES  : similar to MENU_MODES, but for adding buttons instead of menus
; NO_SELECT     : specified if the "Select..." mode is not desired as an option
;-----------------------------------------------------------------------------
;Keywords:
;
;	LONG	GROUP_LEADER		ID of group leader
;	varies	LON_IMAGE		image to use for longitude values
;	varies	LAT_IMAGE		image to use for latitude values
;	PTRARR	DATA_PTR		PTRARR(), with each pointer referencing
;					a different image
;	INT	N_CHANNELS		either 1 or 3, analagous to 8-bit or
;					24-bit display
;	INT	XSIZE			inital horizontal dimension of georef_image
;	INT	YSIZE			inital vertical dimension of georef_image
;	INT	DATA_IDX		index of image in the above PTRARR() whose
;					dimensions and aspect ratio are to be used
;					in displaying images in the georef_image
;	STRARR	MENU_MODES		add'l menu items to be added; should be a
;					STRARR() with dimensions nAddedMenus x 2,
;					with (*,0) being the name of the menu (its
;					displayed value), and (*,1) being its UVALUE
;					(menu items are parsed based upon their UVALUE)
;
;Output Parameters:
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
;	INT	success		Returned if instatiation of object is
;				successful
;	INT	failure		Returned if instatiation of object is
;				unsuccessful
;
;Known Bugs and Limitations:
;
;Parent(s):
;
;	<parents, if any>
;
;Routines Called (all within this file):
;
;	<METHODS>
;		GEOREF_IMAGE::ZoomDisplay
;		GEOREF_IMAGE::stretchDnPlotHistogram
;		GEOREF_IMAGE::DisplayData
;		GEOREF_IMAGE::GetLonLat
;		GEOREF_IMAGE::DisplayValsForXY
;		GEOREF_IMAGE::UnLinkObj
;		GEOREF_IMAGE::ClearLinkObj
;		GEOREF_IMAGE::AddLinkObj
;		GEOREF_IMAGE::SetWindowLink
;		GEOREF_IMAGE::MoveCursor
;		GEOREF_IMAGE::GetLinkedObj
;		GEOREF_IMAGE::DisplayButtons
;		GEOREF_IMAGE::HideButtons
;		GEOREF_IMAGE::HideLabels
;		GEOREF_IMAGE::ReplaceImageUnderCursor
;		GEOREF_IMAGE::ButtonsDisplayed
;		GEOREF_IMAGE::IsLinked
;		GEOREF_IMAGE::Resize
;		GEOREF_IMAGE::SetUpAnchorPoint
;		GEOREF_IMAGE::DrawBox
;		GEOREF_IMAGE::UpdateBox
;		GEOREF_IMAGE::SetMouseDown
;		GEOREF_IMAGE::GetMouseDown
;		GEOREF_IMAGE::SetMode
;		GEOREF_IMAGE::GetMode
;		GEOREF_IMAGE::SetWindowID
;		GEOREF_IMAGE::INIT
;		GEOREF_IMAGE::CLEANUP
;		GEOREF_IMAGE__DEFINE
;
;	<OTHER ROUTINES>
;		map_unlink_realize
;		map_draw_realize
;		georef_image_ev
;		ShowLinkMessage
;		showLinkMsg_ev
;		get_stretchDn_base
;
;Files Accessed:
;
;	<files accessed, if any>
;
;Revision History:
;
;	MAR-98 (CKT) - initial creation of object
;	APR-98 (CKT) - added rubber-banding feature
;	APR-98 (CKT) - added MENU_MODE keyword
;	APR-98 (CKT) - added pan mode
;       MAY-98 (CKT) - added NO_SELECT keyword
;       MAY-98 (CKT) - added BUTTON_MODE feature
;       MAY-98 (CKT) - allowed input data PTRARR()'s
;	MAY-98 (JRH) - added color stretch interface
;                      to have imbedded null pointer elements
;	FEB-99 (JRH) - added where_not to ignore fill values
;	MAR-99 (JRH) - debugged/reworked StretchDN Interface code
;	OCT-99 (CKT) - changed acceptable input data to IMAGE_DATA object
;	NOV-99 (CKT) - completely rewrote ZoomDisplay to handle arbitrary zoom values
;	MAR-01 (JRH) - added colorbar
;	NOV-02 (CKT) - enabled unscaled data to be saved out to HDF/NetCDF/binary
;
;Notes:
;
;
;=============================================================================
;-
;
@IMAGE_DATA_R2.pro
@GEOREF_IMAGE_TABLE.PRO
;;;ckt,sep2004 @where_not.pro
;;;ckt,sep2004 @where_is.pro
@GetDirectoryDivider.pro
@blankFileName.pro
@band_slider.pro
@interactive_perspective_viewer
@reprojection_tool
@gamma_colorbar
@depointer
@do_velocity_vectors

@vicgetpar.pro
@vicsetpar.pro
@gethost.pro
@write_vicar.pro
@read_vicar.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::DisplayTransformInterface @@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::DisplayTransformInterface, idx

	change_flag	= (*((*(SELF.dataPtr))[idx]))->DisplayTransformInfo(		$
							GROUP_LEADER = SELF.parentBase,	$
							DATA_DESCRIPTION = (*(SELF.data_description_ptr))[idx])

	IF NOT change_flag THEN RETURN, change_flag

		PTR_FREE, (*(SELF.stretchDnBottomPtr))[idx]
		(*(SELF.stretchDnBottomPtr))[idx]	=						$
			PTR_NEW( ((*((*(SELF.dataPtr))[idx])))->GetMinVal(		$
				0,((*((*(SELF.dataPtr))[idx])))->GetImageWidth() - 1L,	$
				0,((*((*(SELF.dataPtr))[idx])))->GetImageHeight() - 1L ) )	;+ (maxVal-minVal)*0.01 )
;print,'>>>>> NEW stretchDnBottomPtr = ',*((*(SELF.stretchDnBottomPtr))[idx])

		PTR_FREE, (*(SELF.stretchDnTopPtr))[idx]
		(*(SELF.stretchDnTopPtr))[idx]	=						$
			PTR_NEW( ((*((*(SELF.dataPtr))[idx])))->GetMaxVal(		$
				0,((*((*(SELF.dataPtr))[idx])))->GetImageWidth() - 1L,	$
				0,((*((*(SELF.dataPtr))[idx])))->GetImageHeight() - 1L ) )	;+ (maxVal-minVal)*0.01 )
;print,'>>>>> NEW stretchDnTopPtr = ',*((*(SELF.stretchDnTopPtr))[idx])

		PTR_FREE, (*(SELF.min_dataPtr))[idx]
		(*(SELF.min_dataPtr))[idx]	=						$
			PTR_NEW( ((*((*(SELF.dataPtr))[idx])))->GetMinVal(		$
				0,((*((*(SELF.dataPtr))[idx])))->GetImageWidth() - 1L,	$
				0,((*((*(SELF.dataPtr))[idx])))->GetImageHeight() - 1L ) )	;+ (maxVal-minVal)*0.01 )

;print,'>>>>> NEW min_dataPtr = ',*((*(SELF.min_dataPtr))[idx])
		PTR_FREE, (*(SELF.max_dataPtr))[idx]
		(*(SELF.max_dataPtr))[idx]	=						$
			PTR_NEW( ((*((*(SELF.dataPtr))[idx])))->GetMaxVal(		$
				0,((*((*(SELF.dataPtr))[idx])))->GetImageWidth() - 1L,	$
				0,((*((*(SELF.dataPtr))[idx])))->GetImageHeight() - 1L ) )	;+ (maxVal-minVal)*0.01 )
;print,'>>>>> NEW max_dataPtr = ',*((*(SELF.max_dataPtr))[idx])


		;------------------------------------------------------------------------
		; Turn off any existing histogram and colorbar widgets.
		;------------------------------------------------------------------------
		IF WIDGET_INFO(SELF.stretchDnBase, /VALID_ID) THEN			$
			newRealize	= SELF->ToggleStretchDnInterface( 0 )
		IF WIDGET_INFO( SELF.colorbar_draw, /VALID_ID ) THEN			$
			WIDGET_CONTROL, WIDGET_INFO( SELF.colorbar_draw, /PARENT ), /DESTROY

	RETURN, change_flag
END
; GEOREF_IMAGE::DisplayTransformInterface


;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Transform_Data_Kill @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO Transform_Data_Kill, tlb
END
; Transform_Data_Kill

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Transform_Data_eh @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO Transform_Data_eh, event
	WIDGET_CONTROL, event.id, GET_UVALUE = uval
	WIDGET_CONTROL, event.top, GET_UVALUE = info_ptr


;print,'Transform_Data_eh   ---   event.id uval = ',uval

	CASE 1 OF
		STRPOS(STRLOWCASE(uval),'transform') GE 0: BEGIN
			obj			= (*info_ptr).obj
			idx			= FIX((STR_SEP(uval,'_'))[1])
			(*info_ptr).update	= obj->DisplayTransformInterface(idx)
			IF (*info_ptr).update THEN $
				WIDGET_CONTROL, (*info_ptr).dismiss_button, SET_VALUE = 'Apply'
			END
		STRPOS(STRLOWCASE(uval),'dismiss') GE 0: BEGIN
			WIDGET_CONTROL, event.top, /DESTROY
			END
		ELSE:
	ENDCASE
END
; Transform_Data_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::Transform_Data @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::Transform_Data
	tlb	= WIDGET_BASE(							$
				GROUP_LEADER = SELF.parentBase,			$
				/MODAL,						$
				/COLUMN,					$
				/BASE_ALIGN_CENTER, 				$
				KILL_NOTIFY = 'Transform_Data_Kill',		$
				TITLE = 'Transform Data Interface' )

	n_datasets	= N_ELEMENTS(*(SELF.data_description_ptr))

	lbl		= WIDGET_LABEL(							$
					tlb,						$
					VALUE = 'Dataset Descriptions',			$
					/ALIGN_CENTER, 					$
					FONT = GetCorrectFont('courier2bold') )

	FOR i = 0, n_datasets - 1 DO BEGIN
		sub_tlb		= WIDGET_BASE(							$
						tlb,						$
						/ROW,						$
						/BASE_ALIGN_LEFT )
		lbl		= WIDGET_LABEL(							$
						sub_tlb,					$
						VALUE = (*(SELF.data_description_ptr))[i],	$
						/DYNAMIC_RESIZE,				$
						FONT = GetCorrectFont('courier2bold') )
		btn		= WIDGET_BUTTON(						$
						sub_tlb,					$
						VALUE = 'View/Edit Transform',			$
						UVALUE = 'transform_' + STRTRIM(STRING(i),2),	$
						FONT = GetCorrectFont('courier2bold') )

	ENDFOR

	dismiss_button	= WIDGET_BUTTON( tlb, VALUE = 'Close', UVALUE = 'dismiss', FONT = GetCorrectFont('courier2bold') )

	WIDGET_CONTROL, tlb, DEFAULT_BUTTON = dismiss
	WIDGET_CONTROL, tlb, /REALIZE

	info_ptr	= PTR_NEW( {						$
					update		: 0,			$
					obj		: SELF,			$
					dismiss_button	: dismiss_button	$
					}, /NO_COPY )

	WIDGET_CONTROL, tlb, SET_UVALUE = info_ptr

	XMANAGER, 'Transform Data Base', tlb, EVENT_HANDLER = 'Transform_Data_eh'
	update_hist	= (*info_ptr).update
	PTR_FREE, info_ptr

;print,''
;print,'GEOREF_IMAGE::Transform_Data update_hist = ',update_hist
;print,''
	IF update_hist THEN							$
		IF SELF->UpdateStretchDnInterface( /UPDATE_LIMITS ) THEN	$
			SELF->DisplayData

END
; GEOREF_IMAGE::Transform_Data

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Perspective_Kill @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO Perspective_Kill, tlb
END
; Perspective_Kill

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Perspective_Setup_eh @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO Perspective_Setup_eh, event
	WIDGET_CONTROL, event.id, GET_UVALUE = uval
	WIDGET_CONTROL, event.top, GET_UVALUE = info_ptr

	CASE STRLOWCASE(uval) OF
		'wd_elevation': (*info_ptr).wd_idx	= event.index
		'useentiredata': (*info_ptr).display_entire	= event.select
		'usedisplayeddata': (*info_ptr).display_entire	= event.select LE 0
		'ok': WIDGET_CONTROL, event.top, /DESTROY
		'cancel': BEGIN
			(*info_ptr).cancel_pressed	= 1
			WIDGET_CONTROL, event.top, /DESTROY
			END
		ELSE:
	ENDCASE
END
; Perspective_Setup_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::Display_Perspective @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::Display_Perspective

;==================================================================================
;==================================================================================
;==================================================================================
;==================================================================================
; FOLLOWING CODE ADDED UNTIL ADD'L WORK ON PERSPECTIVE VIEWER IS FINISHED, ckt,aug2000
;==================================================================================
;==================================================================================
;==================================================================================
;==================================================================================
; FOLLOWING CODE ADDED UNTIL ADD'L WORK ON PERSPECTIVE VIEWER IS FINISHED, ckt,aug2000
if strupcase(strtrim(!version.os_family,2)) eq 'UNIX' then begin
	spawn,'hostname',host_name
	spawn,'printenv | grep DISPLAY',display_name
	tmp=str_sep(display_name[0],'=')
	tmp2=str_sep(tmp[n_elements(tmp)-1],':')

	if (strtrim(tmp2[0],2) ne '') or ((strtrim(host_name[0],2) ne strtrim(tmp2[0],2)) and ( strtrim(tmp2[0],2) ne '' ) ) then begin
		msg	= [							$
				'It appears that the program is being run',	$
				'across a network, as the display device',	$
				'does not belong to the host from which',	$
				'the program was launched.  Attempting to',	$
				'run the perspective viewer while operating',	$
				'the program in this mode may cause SEVERE',	$
				'delays in the execution of the software.',	$
				'Do you want to continue?' ]
		res	= dialog_message(msg,/question)
		if strupcase(strtrim(res,2)) eq 'NO' then return
	endif
endif

msg	= [									$
		'***********************************************************',	$
		'************************* WARNING *************************',	$
		'***********************************************************',	$
		'',								$
		'Please be aware that the perspective viewer currently',	$
		'imposes the following size restrictions on datasets:',		$
		'          DATA TO BE USED AS ELEVATION:    512 x  512',	$
		'          DATA TO BE USED AS TEXTURE MAP: 1024 x 1024',	$
		'If the incoming datasets do not conform to these ',		$
		'limitations, they will be decimated as necessary.  Also,',	$
		'since the perspective viewer operates with square-sized',	$
		'datasets, incoming data will be padded as necessary to',	$
		'achieve this requirement.',					$
		'',								$
		'Do you wish to continue?' ]
ret	= DIALOG_MESSAGE( msg, /question )
if strupcase(strtrim(ret,2)) eq 'NO' then return
;==================================================================================
;==================================================================================
;==================================================================================
;==================================================================================
;==================================================================================
;==================================================================================
;==================================================================================
;==================================================================================



	tlb	= WIDGET_BASE( GROUP_LEADER = SELF.parentBase, /MODAL,		$
				/COLUMN, /BASE_ALIGN_CENTER, 			$
				KILL_NOTIFY = 'Perspective_Kill',		$
				TITLE = 'Display Perspective Setup' )
	msg	= 'NOTE: Texture Map Will Always Be All Displayed RGB Data Planes'
	lbl	= WIDGET_LABEL( tlb, VALUE = msg, /ALIGN_CENTER )
	wd_list	= ['none', *(SELF.data_description_ptr)]
	wd		= WIDGET_DROPLIST( tlb, 				$
				UVALUE = 'wd_elevation',			$
				VALUE = wd_list,				$
				TITLE = 'Select Data To Be Used As Elevation' )

	sub_base	= WIDGET_BASE( tlb, /ROW, /BASE_ALIGN_CENTER, /FRAME,		$
				/EXCLUSIVE )
	disp_entire_btn	= WIDGET_BUTTON( sub_base, VALUE = 'Use Entire Data Extent',	$
				UVALUE = 'useentiredata' )
	WIDGET_CONTROL, disp_entire_btn, SET_BUTTON = 0
	disp_only_btn	= WIDGET_BUTTON( sub_base, VALUE = 'Use Displayed Data Extent',	$
				UVALUE = 'usedisplayeddata' )
	WIDGET_CONTROL, disp_only_btn, SET_BUTTON = 1

	sub_base	= WIDGET_BASE( tlb, /ROW, /BASE_ALIGN_CENTER, /FRAME )
	ok		= WIDGET_BUTTON( sub_base, VALUE = 'OK', UVALUE = 'ok' )
	cancel		= WIDGET_BUTTON( sub_base, VALUE = 'Cancel', UVALUE = 'cancel' )

	WIDGET_CONTROL, tlb, DEFAULT_BUTTON = ok
	WIDGET_CONTROL, tlb, /REALIZE

	info_ptr	= PTR_NEW( {							$
					wd_idx		:0,				$
					display_entire	:0,				$
					cancel_pressed	:0				$
					}, /NO_COPY )

	WIDGET_CONTROL, tlb, SET_UVALUE = info_ptr

	XMANAGER, 'Perspective Setup Base', tlb, EVENT_HANDLER = 'Perspective_Setup_eh'

	IF (*info_ptr).cancel_pressed THEN BEGIN
		PTR_FREE, info_ptr
		RETURN
	ENDIF


	plane_display_status	= [ ( SELF.redBandIdx GE 0 AND SELF.display_rgb[0] ),	$
				    ( SELF.grnBandIdx GE 0 AND SELF.display_rgb[1] ),	$
				    ( SELF.bluBandIdx GE 0 AND SELF.display_rgb[2] ) ]

	;------------------------------------------------------------------------
	; First determine how many planes contain DISPLAYED DATA
	; DISPLAYED DATA requires that data is loaded into a
	; particular band AND that band is currently being displayed
	;------------------------------------------------------------------------
	n_displayed_data		= TOTAL(plane_display_status)
	active_plane_idx		= WHERE(plane_display_status GT 0)
	img_idx				= [ SELF.redBandIdx, SELF.grnBandIdx, SELF.bluBandIdx ]

	CASE n_displayed_data OF
		1: BEGIN
			texture_map_img	= SELF->GetBand( img_idx[active_plane_idx[0]],		$
				active_plane_idx[0],						$
				RETURN_DISPLAY_ONLY = ((*info_ptr).display_entire LT 1 ) )
			x_sz		= (SIZE(texture_map_img))[1]
			y_sz		= (SIZE(texture_map_img))[2]
			END
		ELSE: BEGIN
			FOR i = 0, N_ELEMENTS(active_plane_idx) - 1 DO BEGIN
				img		=						$
					SELF->GetBand( img_idx[active_plane_idx[i]],		$
						active_plane_idx[i],				$
						RETURN_DISPLAY_ONLY = ((*info_ptr).display_entire LT 1 ) )
				;------------------------------------------------------------------------------------
				; if images comprising RGB texture map are different sizes, use first one encountered
				; for determining image size used in perspective viewer
				;------------------------------------------------------------------------------------
				IF i EQ 0 THEN BEGIN
					x_sz		= (SIZE(img))[1]
					y_sz		= (SIZE(img))[2]
					texture_map_img	= BYTARR( 3, x_sz, y_sz )
				ENDIF

				texture_map_img[active_plane_idx[i],*,*]	= CONGRID(img,x_sz,y_sz)
			ENDFOR

			END
	ENDCASE

	elev_img	= BYTARR( x_sz, y_sz )

	IF (*info_ptr).wd_idx GT 0 THEN BEGIN
		elev_idx	= (*info_ptr).wd_idx - 1
		elev_img	= SELF->GetBand( elev_idx, 0, /NO_CHECK_DISPLAY_STATUS,	$
					RETURN_DISPLAY_ONLY = ((*info_ptr).display_entire LT 1 ), OUT_XSIZE = x_sz, OUT_YSIZE = y_sz )
	ENDIF

	PTR_FREE, info_ptr


MAX_TEXTURE_MAP_SIZE_PER_BAND	= 1024L*1024L
MAX_TEXTURE_SIZE_PER_DIM	= 1024L
sz1=size(texture_map_img)

xsize1 = sz1[1]
ysize1 = sz1[2]
n_bands		= 2
x_y_band_idx	= [0,1]

;help,xsize1
if sz1[0] ge 3 then begin
	case 1 of
		(sz1[0] ge 3) and (sz1[1] eq 3): begin
			xsize1 		= sz1[2]
			ysize1 		= sz1[3]
			n_bands		= 3
			x_y_band_idx	= [1,2,0]
			end
		(sz1[0] ge 3) and (sz1[2] eq 3): begin
			xsize1 = sz1[1]
			ysize1 = sz1[3]
			n_bands		= 3
			x_y_band_idx	= [0,2,1]
			end
		(sz1[0] ge 3) and (sz1[3] eq 3): begin
			xsize1 = sz1[1]
			ysize1 = sz1[2]
			n_bands		= 3
			x_y_band_idx	= [0,1,2]
			end
		else:
	endcase
endif
;help,xsize1
if (xsize1*ysize1 gt MAX_TEXTURE_MAP_SIZE_PER_BAND) or (xsize1 gt MAX_TEXTURE_SIZE_PER_DIM) or (ysize1 gt MAX_TEXTURE_SIZE_PER_DIM)	$
then begin
	out_xsize1	= MAX_TEXTURE_SIZE_PER_DIM
	out_ysize1	= MAX_TEXTURE_SIZE_PER_DIM

	if xsize1 gt ysize1 then out_ysize1 = long(float(ysize1)*(float(MAX_TEXTURE_SIZE_PER_DIM)/float(xsize1)))
	if ysize1 gt xsize1 then out_xsize1 = long(float(xsize1)*(float(MAX_TEXTURE_SIZE_PER_DIM)/float(ysize1)))

	if n_bands eq 3 then begin
		new_size			= LONARR(3)
		new_size[x_y_band_idx[0]]	= out_xsize1
		new_size[x_y_band_idx[1]]	= out_ysize1
		new_size[x_y_band_idx[2]]	= 3
		texture_map_img			= congrid( texture_map_img, new_size[0], new_size[1], new_size[2] )
		xsize1				= (SIZE(texture_map_img))[x_y_band_idx[0]+1]
		ysize1				= (SIZE(texture_map_img))[x_y_band_idx[1]+1]
	endif else begin
		texture_map_img			= congrid( texture_map_img, out_xsize1, out_ysize1 )
		xsize1				= (SIZE(texture_map_img))[1]
		ysize1				= (SIZE(texture_map_img))[2]
	endelse
endif

new_size			= LONARR(3)
old_size			= new_size
if (xsize1 ne ysize1) then begin
	if xsize1 gt ysize1 then begin
		if n_bands eq 3 then begin
			new_size[x_y_band_idx[0]]	= xsize1
			new_size[x_y_band_idx[1]]	= xsize1
			new_size[x_y_band_idx[2]]	= 3

			old_size[x_y_band_idx[0]]	= xsize1
			old_size[x_y_band_idx[1]]	= ysize1
			old_size[x_y_band_idx[2]]	= 3

			out_img				= BYTARR(new_size[0],new_size[1],new_size[2])
			out_img[0:old_size[0]-1,0:old_size[1]-1,0:old_size[2]-1]				$
							= texture_map_img
		endif else begin
			out_img				= BYTARR(xsize1,xsize1)
			out_img[0:xsize1-1,0:ysize1-1]	= texture_map_img
		endelse
	endif else begin
		if n_bands eq 3 then begin
			new_size[x_y_band_idx[0]]	= ysize1
			new_size[x_y_band_idx[1]]	= ysize1
			new_size[x_y_band_idx[2]]	= 3

			old_size[x_y_band_idx[0]]	= xsize1
			old_size[x_y_band_idx[1]]	= ysize1
			old_size[x_y_band_idx[2]]	= 3
;help,ysize1
;help,xsize1
;help,texture_map_img

			out_img				= BYTARR(new_size[0],new_size[1],new_size[2])
			out_img[0:old_size[0]-1,0:old_size[1]-1,0:old_size[2]-1]				$
							= texture_map_img
		endif else begin
;help,ysize1
;help,xsize1
;help,texture_map_img
			out_img				= BYTARR(ysize1,ysize1)
			out_img[0:xsize1-1,0:ysize1-1]	= texture_map_img
		endelse
	endelse
	texture_map_img	= out_img
endif



MAX_ELEV_SIZE_PER_BAND	= 512L*512L
MAX_ELEV_SIZE_PER_DIM	= 512L
sz1=size(elev_img)

xsize1 = sz1[1]
ysize1 = sz1[2]
n_bands		= 2
x_y_band_idx	= [0,1]

if sz1[0] ge 3 then begin
	case 1 of
		(sz1[0] ge 3) and (sz1[1] eq 3): begin
			xsize1 		= sz1[2]
			ysize1 		= sz1[3]
			n_bands		= 3
			x_y_band_idx	= [1,2,0]
			end
		(sz1[0] ge 3) and (sz1[2] eq 3): begin
			xsize1 = sz1[1]
			ysize1 = sz1[3]
			n_bands		= 3
			x_y_band_idx	= [0,2,1]
			end
		(sz1[0] ge 3) and (sz1[3] eq 3): begin
			xsize1 = sz1[1]
			ysize1 = sz1[2]
			n_bands		= 3
			x_y_band_idx	= [0,1,2]
			end
		else:
	endcase
endif

if (xsize1*ysize1 gt MAX_ELEV_SIZE_PER_BAND) or (xsize1 gt MAX_ELEV_SIZE_PER_DIM) or (ysize1 gt MAX_ELEV_SIZE_PER_DIM) then begin
	out_xsize1	= MAX_ELEV_SIZE_PER_DIM
	out_ysize1	= MAX_ELEV_SIZE_PER_DIM

	if xsize1 gt ysize1 then out_ysize1 = long(float(ysize1)*(float(MAX_ELEV_SIZE_PER_DIM)/float(xsize1)))
	if ysize1 gt xsize1 then out_xsize1 = long(float(xsize1)*(float(MAX_ELEV_SIZE_PER_DIM)/float(ysize1)))

	if n_bands eq 3 then begin
		new_size			= LONARR(3)
		new_size[x_y_band_idx[0]]	= out_xsize1
		new_size[x_y_band_idx[1]]	= out_ysize1
		new_size[x_y_band_idx[2]]	= 3
		elev_img			= congrid( elev_img, new_size[0], new_size[1], new_size[2] )
		xsize1				= (SIZE(texture_map_img))[x_y_band_idx[0]]
		ysize1				= (SIZE(texture_map_img))[x_y_band_idx[1]]
	endif else begin
		elev_img			= congrid( elev_img, out_xsize1, out_ysize1 )
		xsize1				= (SIZE(elev_img))[1]
		ysize1				= (SIZE(elev_img))[2]
	endelse
endif

new_size			= LONARR(3)
old_size			= new_size

if (xsize1 ne ysize1) then begin
	if xsize1 gt ysize1 then begin
		if n_bands eq 3 then begin
			new_size[x_y_band_idx[0]]	= xsize1
			new_size[x_y_band_idx[1]]	= xsize1
			new_size[x_y_band_idx[2]]	= 3

			old_size[x_y_band_idx[0]]	= xsize1
			old_size[x_y_band_idx[1]]	= ysize1
			old_size[x_y_band_idx[2]]	= 3

			out_img				= BYTARR(new_size[0],new_size[1],new_size[2])
			out_img[0:old_size[0]-1,0:old_size[1]-1,0:old_size[2]-1]				$
							= elev_img
		endif else begin
			out_img				= BYTARR(xsize1,xsize1)
;help,out_img
;xsize1
;print,ysize1
;help,elev_img
			out_img[0:xsize1-1,0:ysize1-1]	= elev_img
		endelse
	endif else begin
		if n_bands eq 3 then begin
			new_size[x_y_band_idx[0]]	= ysize1
			new_size[x_y_band_idx[1]]	= ysize1
			new_size[x_y_band_idx[2]]	= 3

			old_size[x_y_band_idx[0]]	= xsize1
			old_size[x_y_band_idx[1]]	= ysize1
			old_size[x_y_band_idx[2]]	= 3

			out_img				= BYTARR(new_size[0],new_size[1],new_size[2])
			out_img[0:old_size[0]-1,0:old_size[1]-1,0:old_size[2]-1]				$
							= elev_img
		endif else begin
			out_img				= BYTARR(ysize1,ysize1)
			out_img[0:xsize1-1,0:ysize1-1]	= elev_img
		endelse
	endelse
	elev_img	= out_img
endif





	interactive_perspective_viewer, SURFACE = elev_img, IMAGE = texture_map_img
END
; GEOREF_IMAGE::Display_Perspective

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::GetBand @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::GetBand, img_idx,					$
				rgb_idx,					$
				RETURN_DISPLAY_ONLY = return_display_only,	$
				OUT_XSIZE = out_xsize,				$
				OUT_YSIZE = out_ysize,				$
				NO_CHECK_DISPLAY_STATUS = no_check_display_status
;--------------------------------------------------------------------------------
; img_idx: index of the image to consider
;
; rgb_idx: 0 (red), 1 (green), or 2 (blue)
;
; RETURN_DISPLAY_ONLY: set this keyword if only the portion of the image within
; the GEOREF_IMAGE viewport is desired; returned data will have the same
; dimensions as the GEOREF_IMAGE viewport, REGARDLESS of whether OUT_XSIZE or
; OUT_YSIZE has been set
;
; OUT_XSIZE, OUT_YSIZE: output dimensions for returned data; ONLY USED if the
; entire dataset corresponding to the img_idx is requested; if these keywords are
; not specified and a request is made for the entire dataset corresponding
; to the img_idx, the input dimensions of the data are used.
;--------------------------------------------------------------------------------


	consider_display_only	= 0
	IF KEYWORD_SET( return_display_only ) THEN consider_display_only = 1

	must_be_displayed	= 1
	IF KEYWORD_SET( no_check_display_status ) THEN must_be_displayed = 0

	WIDGET_CONTROL, /HOURGLASS

	plane_display_status	= [ ( SELF.redBandIdx GE 0 AND SELF.display_rgb[0] ),	$
				    ( SELF.grnBandIdx GE 0 AND SELF.display_rgb[1] ),	$
				    ( SELF.bluBandIdx GE 0 AND SELF.display_rgb[2] ) ]

	;------------------------------------------------------------------------
	; First determine how many planes contain DISPLAYED DATA
	; DISPLAYED DATA requires that data is loaded into a
	; particular band AND that band is currently being displayed
	;------------------------------------------------------------------------
	n_displayed_data		= TOTAL(plane_display_status)
	active_plane_idx		= WHERE(plane_display_status GT 0)

	;------------------------------------------------------------------------
	; Next, determine if the currently-requested band contains data AND is
	; being displayed.
	;------------------------------------------------------------------------
	is_current_band_displayed	= ( img_idx GE 0 AND SELF.display_rgb[rgb_idx] )

	;------------------------------------------------------------------------
	; If there is only one band being displayed, and it is NOT the current band,
	; return (-1), as opposed to returning an array of zeroes in the case
	; of more than one band being displayed.  THIS CHECK IS OVERRIDDEN if
	; the NO_CHECK_DISPLAY_STATUS keyword is set
	;------------------------------------------------------------------------
	IF ( n_displayed_data LE 1 AND NOT is_current_band_displayed AND must_be_displayed ) THEN BEGIN
		RETURN, (-1)
	ENDIF

	IF consider_display_only THEN BEGIN
		;----------------------------------------------------------------
		; If only the displayed portion of the image in question is to
		; be returned, we have to be concerned with other bands that
		; may be of a different resolution than the current image.
		; Therefore, we go through a series of checks and calculations
		; to make sure that the portion of the current data we are
		; grabbing corresponds to any other bands that are displayed.
		;----------------------------------------------------------------

;;;ckt,sep2000		;----------------------------------------------------------------
;;;ckt,sep2000		; Retrieve the current x- and y- zoom values for the
;;;ckt,sep2000		; data in question.  Since the zoom values are stored after
;;;ckt,sep2000		; being multiplied by 100.0, restore them to their actual value
;;;ckt,sep2000		; by dividing by 100.0
;;;ckt,sep2000		;----------------------------------------------------------------
;;;ckt,sep2000		zoom_x			= (*(SELF.zoomPctX))[img_idx] / 100.0
;;;ckt,sep2000		zoom_y			= (*(SELF.zoomPctY))[img_idx] / 100.0

		;----------------------------------------------------------------
		; Now, grab the x- and y- zoom values for the data at the highest
		; resolution.  Also grab the number of x- and y- pixels that
		; are used in the display for the highest resolution data
		; after applying the zoom value for the highest resolution data.
		;----------------------------------------------------------------
		x_zoom_val_for_highest_res_data						$
			= (*(SELF.zoomPctX))[SELF.highest_res_data_idx] / 100.0
		zoomed_nx_for_highest_res_data						$
			=								$
			ROUND( (*(SELF.dataNX))[SELF.highest_res_data_idx] *		$
			x_zoom_val_for_highest_res_data )
		y_zoom_val_for_highest_res_data						$
			= (*(SELF.zoomPctY))[SELF.highest_res_data_idx] / 100.0
		zoomed_ny_for_highest_res_data						$
			=								$
			ROUND( (*(SELF.dataNY))[SELF.highest_res_data_idx] *		$
			y_zoom_val_for_highest_res_data )

		;----------------------------------------------------------------
		; Set x_size and y_size to the nx and ny values calculated above.
		;----------------------------------------------------------------
		x_size	= zoomed_nx_for_highest_res_data
		y_size	= zoomed_ny_for_highest_res_data

		IF is_current_band_displayed OR NOT must_be_displayed THEN BEGIN
			;----------------------------------------------------------------
			; Retrieve the current x- and y- zoom values for the
			; data in question.  Since the zoom values are stored after
			; being multiplied by 100.0, restore them to their actual value
			; by dividing by 100.0
			;----------------------------------------------------------------
			zoom_x			= (*(SELF.zoomPctX))[img_idx] / 100.0
			zoom_y			= (*(SELF.zoomPctY))[img_idx] / 100.0
;print,'zoom_x,zoom_y=',zoom_x,zoom_y
			;--------------------------------------------------------
			; If the current dataset is displayed, grab its
			; its information regarding pixel start and end indices
			; to display in the horizontal and vertical directions.
			; These values can be fractional if only a portion of
			; a pixel should be displayed, such as in the case of
			; being zoomed in considerably.
			;--------------------------------------------------------
			floating_global_image_coord_xs	= (*(SELF.dataXS))[img_idx]
			floating_global_image_coord_xe	= floating_global_image_coord_xs + (*(SELF.dataNX))[img_idx] - 1.0
			floating_global_image_coord_ys	= (*(SELF.dataYS))[img_idx]
			floating_global_image_coord_ye	= floating_global_image_coord_ys + (*(SELF.dataNY))[img_idx] - 1.0
;print,'floating_global_image_coord_xs=',floating_global_image_coord_xs
;print,'floating_global_image_coord_xe=',floating_global_image_coord_xe
;print,'floating_global_image_coord_ys=',floating_global_image_coord_ys
;print,'floating_global_image_coord_ye=',floating_global_image_coord_ye
			;--------------------------------------------------------
			; Convert the floating values above to integer.  For xs
			; and ys, truncate the floating values; for xe and ye,
			; if the integer version of these values are not equal to
			; their floating counterparts, add 1 to the integer
			; versions.
			;--------------------------------------------------------
			global_image_coord_xs		= FIX(floating_global_image_coord_xs)

			IF FLOAT(FIX(floating_global_image_coord_xe)) NE floating_global_image_coord_xe THEN	$
				global_image_coord_xe	= FIX( floating_global_image_coord_xe + 1.0 )		$
			ELSE											$
				global_image_coord_xe	= FIX( floating_global_image_coord_xe )

			global_image_coord_ys		= FIX(floating_global_image_coord_ys)

			IF FLOAT(FIX(floating_global_image_coord_ye)) NE floating_global_image_coord_ye THEN	$
				global_image_coord_ye	= FIX( floating_global_image_coord_ye + 1.0 )		$
			ELSE											$
				global_image_coord_ye	= FIX( floating_global_image_coord_ye )

			;--------------------------------------------------------
			; Calculate zoom_factor and horizontal and vertical
			; extents of displayable portion of data.  This is used
			; as input to the IMAGE_DATA method Return_Display_Struct,
			; which provides information about the ACTUAL horizontal
			; and vertical extents of the ZOOMED data.
			;--------------------------------------------------------
			zoom_factor		= (*(SELF.zoomPctX))[img_idx] / 100.0
			view_width		= FLOAT( global_image_coord_xe - global_image_coord_xs + 1L ) * zoom_factor
			view_height		= FLOAT( global_image_coord_ye - global_image_coord_ys + 1L ) * zoom_factor
			in_h			= global_image_coord_xe - global_image_coord_xs + 1L
			in_v			= global_image_coord_ye - global_image_coord_ys + 1L

			disp_info		= (*((*(SELF.dataPtr))[img_idx]))->Return_Display_Struct(	$
							global_image_coord_xs,					$
							global_image_coord_ys,					$
							global_image_coord_xe,					$
							global_image_coord_ye,					$
							in_h,							$
							in_v, 							$
							zoom_factor )

			;--------------------------------------------------------
			; Calculate the data xs and ys values of the ZOOMED data;
			; also calculate the nx and ny values of the zoomed data.
			; If the starting values plus the extent values of the
			; horizontal and vertical directions EXCEED the dimensions
			; of the image, constrain nx and ny to values that
			; include (but don't exceed) the end of the image.  Use
			; these values below in the method call to
			; ReturnByteScaledImage.
			;--------------------------------------------------------
			zoomed_x_start		=								$
				ROUND( ( floating_global_image_coord_xs - FLOAT( global_image_coord_xs ) ) * zoom_x )


			zoomed_nx		= MAX( [ zoomed_nx_for_highest_res_data, 1 ] )
			IF zoomed_x_start + zoomed_nx GT disp_info.out_h_sz THEN zoomed_nx = disp_info.out_h_sz - zoomed_x_start

			zoomed_y_start		=								$
				ROUND( ( floating_global_image_coord_ys - FLOAT( global_image_coord_ys ) ) * zoom_y )
			zoomed_ny		= MAX( [ zoomed_ny_for_highest_res_data, 1 ] )
			IF zoomed_y_start + zoomed_ny GT disp_info.out_v_sz THEN zoomed_ny = disp_info.out_v_sz - zoomed_y_start

		ENDIF
	ENDIF ELSE BEGIN
		IF KEYWORD_SET(out_xsize) THEN BEGIN
			x_size = out_xsize
		ENDIF ELSE BEGIN
			x_size	= (-1L)
			rgb_indices	= [ SELF.redBandIdx, SELF.grnBandIdx, SELF.bluBandIdx ]
			FOR i = 0, N_ELEMENTS(active_plane_idx) - 1 DO BEGIN
				ref_idx	= rgb_indices[ active_plane_idx[i] ]
				x_size	= MAX( [ x_size, (*((*(SELF.dataPtr))[ref_idx]))->GetImageWidth() ] )
			ENDFOR
		ENDELSE

		IF KEYWORD_SET(out_ysize) THEN BEGIN
			y_size = out_ysize
		ENDIF ELSE BEGIN
			y_size	= (-1L)
			rgb_indices	= [ SELF.redBandIdx, SELF.grnBandIdx, SELF.bluBandIdx ]
			FOR i = 0, N_ELEMENTS(active_plane_idx) - 1 DO BEGIN
				ref_idx	= rgb_indices[ active_plane_idx[i] ]
				y_size	= MAX( [ y_size, (*((*(SELF.dataPtr))[ref_idx]))->GetImageHeight() ] )
			ENDFOR
		ENDELSE
		IF is_current_band_displayed OR NOT must_be_displayed THEN BEGIN
			global_image_coord_xs	= 0
			global_image_coord_xe	= global_image_coord_xs + (*((*(SELF.dataPtr))[img_idx]))->GetImageWidth() - 1
			global_image_coord_ys	= 0
			global_image_coord_ye	= global_image_coord_ys + (*((*(SELF.dataPtr))[img_idx]))->GetImageHeight() - 1
			view_width		= x_size
			view_height		= y_size
		ENDIF
	ENDELSE

	IF is_current_band_displayed OR NOT must_be_displayed THEN BEGIN
;print,'GEOREF_IMAGE::GetBand -- calling SetDisplayMinVal2Byte and SetDisplayMaxVal2Byte'
;print,'GEOREF_IMAGE::GetBand -- (*((*(SELF.stretchDnBottomPtr))[img_idx])) = ',(*((*(SELF.stretchDnBottomPtr))[img_idx]))
;print,'GEOREF_IMAGE::GetBand -- (*((*(SELF.stretchDnTopPtr))[img_idx])) = ',(*((*(SELF.stretchDnTopPtr))[img_idx]))
		(*((*(SELF.dataPtr))[img_idx]))->SetDisplayGamma, (*((*(SELF.stretchDnGammaPtr))[img_idx]))
		(*((*(SELF.dataPtr))[img_idx]))->SetDisplayMinVal2Byte, (*((*(SELF.stretchDnBottomPtr))[img_idx]))
		(*((*(SELF.dataPtr))[img_idx]))->SetDisplayMaxVal2Byte, (*((*(SELF.stretchDnTopPtr))[img_idx]))

		IF consider_display_only THEN							$
			;--------------------------------------------------------
			; "Carve out" the portion of the returned image that
			; corresponds to the display by using the zoom paramters
			; calculated above.
			;--------------------------------------------------------
			RETURN, ((*((*(SELF.dataPtr))[img_idx]))->ReturnByteScaledImage(	$
					global_image_coord_xs,					$
					global_image_coord_xe,					$
					global_image_coord_ys,					$
					global_image_coord_ye,					$
					view_width,						$
					view_height ))						$
					[zoomed_x_start:zoomed_x_start+zoomed_nx-1,		$
					zoomed_y_start:zoomed_y_start+zoomed_ny-1]		$
		ELSE										$
			RETURN, (*((*(SELF.dataPtr))[img_idx]))->ReturnByteScaledImage(		$
					global_image_coord_xs,					$
					global_image_coord_xe,					$
					global_image_coord_ys,					$
					global_image_coord_ye,					$
					view_width,						$
					view_height )
	ENDIF ELSE BEGIN
		RETURN, BYTARR( x_size, y_size )
	ENDELSE
END
; GEOREF_IMAGE::GetBand

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::GetPlaneDescription @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::GetPlaneDescription, img_idx
;print,'in GEOREF_IMAGE::GetPlaneDescription, img_idx = ',img_idx
;print,'PTR_VALID( (*(SELF.dataPtr))[img_idx] )=',PTR_VALID( (*(SELF.dataPtr))[img_idx] )
	desc	= ''
	IF img_idx GE 0 THEN BEGIN
		IF PTR_VALID( (*(SELF.dataPtr))[img_idx] ) THEN			$
			desc	= (*((*(SELF.dataPtr))[img_idx]))->Return_Data_Description()
	ENDIF
	RETURN, desc
END
; GEOREF_IMAGE::GetPlaneDescription

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::GetPlaneUnits @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::GetPlaneUnits, img_idx
	units	= ''
	IF img_idx GE 0 THEN BEGIN
		IF PTR_VALID( (*(SELF.dataPtr))[img_idx] ) THEN			$
			units	= (*((*(SELF.dataPtr))[img_idx]))->Return_Data_Units()
	ENDIF
	RETURN, units
END
; GEOREF_IMAGE::GetPlaneUnits

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::GetPlaneDisplayStatus @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::GetPlaneDisplayStatus

	plane_display_status = [( SELF.redBandIdx GE 0 AND SELF.display_rgb[0] ),	$
				( SELF.grnBandIdx GE 0 AND SELF.display_rgb[1] ),	$
				( SELF.bluBandIdx GE 0 AND SELF.display_rgb[2] )]
	RETURN, plane_display_status
END
; GEOREF_IMAGE::GetPlaneDisplayStatus










;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::SaveHDF @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::SaveHDF, out_fname
;	hdf_types	= [ 'UNDEFINED',					$
;			    '/BYTE',						$
;			    '/INT',						$
;			    '/LONG',						$
;			    '/FLOAT',						$
;			    '/DOUBLE',						$
;			    'UNDEFINED',					$
;			    '/STRING',						$
;			    'UNDEFINED',					$
;			    'UNDEFINED',					$
;			    'UNDEFINED',					$
;			    'UNDEFINED',					$
;			    '/DFNT_UINT16',					$
;			    '/DFNT_UINT32',					$
;			    '/DOUBLE',						$ ;long64
;			    '/DOUBLE' ]						  ;ulong64
	hdf_types	= [ 'UNDEFINED',					$ ;0
			    'BYTARR',						$ ;1
			    'INTARR',						$ ;2
			    'LONARR',						$ ;3
			    'FLTARR',						$ ;4
			    'DBLARR',						$ ;5
			    'UNDEFINED',					$ ;6
			    'STRARR',						$ ;7
			    'UNDEFINED',					$ ;8
			    'UNDEFINED',					$ ;9
			    'UNDEFINED',					$ ;10
			    'UNDEFINED',					$ ;11
			    'UINTARR',						$ ;12
			    'ULONARR',						$ ;13
			    'LON64ARR',						$ ;14
			    'ULON64ARR' ]					  ;15

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== GEOREF_IMAGE::SaveHDF =========='
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

	n_datasets	= N_ELEMENTS(*(SELF.data_source_filename_ptr))
	src_files	= *(SELF.data_source_filename_ptr)
	data_ptrs	= *(SELF.dataPtr)
	data_descs	= *(SELF.data_description_ptr)

	IF  PTR_VALID((SELF.lonImgPtr)[1]) AND				$
	    PTR_VALID((SELF.latImgPtr)[1]) THEN BEGIN
		n_datasets	= n_datasets + 2
		src_files	= [ SELF.lon_source_filename,		$
				    SELF.lat_source_filename,		$
				    src_files ]
		data_ptrs	= [ (SELF.lonImgPtr)[0],		$
				    (SELF.latImgPtr)[0],		$
				    data_ptrs ]
		data_descs	= [ 'Longitude Image',			$
				    'Latitude Image',			$
				    data_descs ]
	ENDIF

	sd_id		= HDF_SD_START( out_fname, /CREATE )

	FOR i = 0, n_datasets - 1 DO BEGIN
			src_file	= src_files[i]
			data_type_idx	= (*(data_ptrs[i]))->GetNumberType()
			data_ptr	= data_ptrs[i]
			data_desc	= data_descs[i]
			xy_dims		= [ (*data_ptr)->GetImageWidth(), (*data_ptr)->GetImageHeight() ]




tmpdata	= CALL_FUNCTION(hdf_types[data_type_idx],2)
tmptype	= HDF_IDL2HDFTYPE(SIZE(tmpdata, /TYPE))
;sds_id	= CALL_FUNCTION('HDF_SD_CREATE',sd_id,data_desc,xy_dims,hdf_types[data_type_idx])
sds_id	= CALL_FUNCTION('HDF_SD_CREATE',sd_id,data_desc,xy_dims,HDF_TYPE=tmptype)
;;;ckt,aug2004			str2exec	=					$
;;;ckt,aug2004				'sds_id=' +					$
;;;ckt,aug2004				'HDF_SD_CREATE(sd_id,data_desc,xy_dims,' +	$
;;;ckt,aug2004				hdf_types[data_type_idx] +			$
;;;ckt,aug2004				')'
;;;ckt,aug2004			success		= EXECUTE( str2exec )




			m_name		= 'Size_x'
			HDF_SD_ATTRSET, sds_id, m_name, xy_dims[0], /LONG

			m_name		= 'Size_y'
			HDF_SD_ATTRSET, sds_id, m_name, xy_dims[1], /LONG

			HDF_SD_ADDDATA, sds_id, REVERSE(				$
							(*data_ptr)->ReturnUnbytescaledImage(	$
							0,					$
							(*data_ptr)->GetImageWidth()-1L,	$
							0,					$
							(*data_ptr)->GetImageHeight()-1L,	$
							(*data_ptr)->GetImageWidth(),		$
							(*data_ptr)->GetImageHeight(),		$
							DATA_TYPE = (*data_ptr)->GetNumberType(), $
							/NATIVE_RES ), 2 )

			HDF_SD_ENDACCESS, sds_id
	ENDFOR

	HDF_SD_END, sd_id

;;;ckt,nov2002	msg	= [ 'Sorry, saving scientific data out to HDF',		$
;;;ckt,nov2002		    'format is not currently implemented' ]
;;;ckt,nov2002
;;;ckt,nov2002	res	= DIALOG_MESSAGE( msg, /INFORMATION )
END
; GEOREF_IMAGE::SaveHDF

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::SaveNetCDF @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::SaveNetCDF, out_fname
;	ncdf_types	= [ 'UNDEFINED',					$
;			    '/BYTE',						$
;			    '/SHORT',						$
;			    '/LONG',						$
;			    '/FLOAT',						$
;			    '/DOUBLE',						$
;			    'UNDEFINED',					$
;			    '/CHAR',						$
;			    'UNDEFINED',					$
;			    'UNDEFINED',					$
;			    'UNDEFINED',					$
;			    'UNDEFINED',					$
;			    '/FLOAT',						$ ;uint16
;			    '/FLOAT',						$ ;uint32
;			    '/FLOAT',						$ ;long64
;			    '/FLOAT' ]						  ;ulong64
	ncdf_types	= [ 'UNDEFINED',					$
			    'BYTE',						$
			    'SHORT',						$
			    'LONG',						$
			    'FLOAT',						$
			    'DOUBLE',						$
			    'UNDEFINED',					$
			    'CHAR',						$
			    'UNDEFINED',					$
			    'UNDEFINED',					$
			    'UNDEFINED',					$
			    'UNDEFINED',					$
			    'FLOAT',						$ ;uint16
			    'FLOAT',						$ ;uint32
			    'FLOAT',						$ ;long64
			    'FLOAT' ]						  ;ulong64

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== GEOREF_IMAGE::SaveNetCDF =========='
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

	n_datasets	= N_ELEMENTS(*(SELF.data_source_filename_ptr))
	src_files	= *(SELF.data_source_filename_ptr)
	data_ptrs	= *(SELF.dataPtr)
	data_descs	= *(SELF.data_description_ptr)

	IF  PTR_VALID((SELF.lonImgPtr)[1]) AND				$
	    PTR_VALID((SELF.latImgPtr)[1]) THEN BEGIN
		n_datasets	= n_datasets + 2
		src_files	= [ SELF.lon_source_filename,		$
				    SELF.lat_source_filename,		$
				    src_files ]
		data_ptrs	= [ (SELF.lonImgPtr)[0],		$
				    (SELF.latImgPtr)[0],		$
				    data_ptrs ]
		data_descs	= [ 'Longitude Image',			$
				    'Latitude Image',			$
				    data_descs ]
	ENDIF

	ncdf_id		= NCDF_CREATE( out_fname, /CLOBBER )
;print,'out_fname=',out_fname
;print,'ncdf_id = ',ncdf_id
	FOR i = 0, n_datasets - 1 DO BEGIN
			src_file	= src_files[i]
			data_type_idx	= (*(data_ptrs[i]))->GetNumberType()
			;
			; IDL's encapsulation of NetCDF does not support Unsigned Integers (IDL 5.4).
			; Convert all unsigned integers to FLOATs.
			;
			ret	= 'YES'
			IF data_type_idx GE 12 THEN BEGIN
				msg = [ 'The native format of the current dataset,',			$
					data_descs[i],							$
					'is Unsigned Integer, which is not supported by the IDL 5.4',	$
					'encapsulation of the NetCDF library...',			$
					'',								$
					'This data can be converted to FLOAT and saved as REAL.',	$
					'This will not alter the data values.',				$
					' ',								$
					'Continue saving current dataset as FLOAT?' ]

				ret = DIALOG_MESSAGE( msg, /QUESTION )
			ENDIF

			IF STRUPCASE( STRTRIM(ret,2) ) EQ 'YES' THEN BEGIN

				data_ptr	= data_ptrs[i]
				data_desc	= data_descs[i]
				xy_dims		= [ (*data_ptr)->GetImageWidth(), (*data_ptr)->GetImageHeight() ]
;print,'(*data_ptr)->GetImageWidth()=',(*data_ptr)->GetImageWidth()
;help,(*data_ptr)->GetImageWidth()
;print,'data_desc+:x=',data_desc+':x'
;help,data_desc+':x'
;print,'ncdf_id=',ncdf_id
;help,ncdf_id
datadesc	= STRCOMPRESS(STRTRIM(data_desc,2))
len		= STRLEN(datadesc)
tmpstr		= STRARR(len)
FOR g = 0, len-1 DO tmpstr[g] = STRMID(datadesc,g,1)
idx		= WHERE(					$
			tmpstr EQ ' ' OR 			$
			tmpstr EQ ':' OR 			$
			tmpstr EQ '/' OR 			$
			tmpstr EQ '!' OR			$
			tmpstr EQ '@' OR			$
			tmpstr EQ '#' OR			$
			tmpstr EQ '$' OR			$
			tmpstr EQ '%' OR			$
			tmpstr EQ '^' OR			$
			tmpstr EQ '&' OR			$
			tmpstr EQ '*' OR			$
			tmpstr EQ '(' OR			$
			tmpstr EQ ')' OR			$
			tmpstr EQ '-' OR			$
			tmpstr EQ '+' OR			$
			tmpstr EQ '=' OR			$
			tmpstr EQ '?' OR			$
			tmpstr EQ '<' OR			$
			tmpstr EQ ',' OR			$
			tmpstr EQ ';' OR			$
			tmpstr EQ ':' OR			$
			tmpstr EQ '"' OR			$
			tmpstr EQ "'" OR			$
			tmpstr EQ '[' OR			$
			tmpstr EQ ']' OR			$
			tmpstr EQ '{' OR			$
			tmpstr EQ '}' OR			$
			tmpstr EQ '|' OR			$
			tmpstr EQ '\', cnt)
IF cnt GT 0 THEN tmpstr[idx] = '_'
datadesc	= ''
FOR g = 0, len-1 DO datadesc = datadesc+tmpstr[g]
xdatadesc	= STRCOMPRESS(STRTRIM(data_desc+'_x',2))
len		= STRLEN(xdatadesc)
tmpstr		= STRARR(len)
FOR g = 0, len-1 DO tmpstr[g] = STRMID(xdatadesc,g,1)
idx		= WHERE(					$
			tmpstr EQ ' ' OR 			$
			tmpstr EQ ':' OR 			$
			tmpstr EQ '/' OR 			$
			tmpstr EQ '!' OR			$
			tmpstr EQ '@' OR			$
			tmpstr EQ '#' OR			$
			tmpstr EQ '$' OR			$
			tmpstr EQ '%' OR			$
			tmpstr EQ '^' OR			$
			tmpstr EQ '&' OR			$
			tmpstr EQ '*' OR			$
			tmpstr EQ '(' OR			$
			tmpstr EQ ')' OR			$
			tmpstr EQ '-' OR			$
			tmpstr EQ '+' OR			$
			tmpstr EQ '=' OR			$
			tmpstr EQ '?' OR			$
			tmpstr EQ '<' OR			$
			tmpstr EQ ',' OR			$
			tmpstr EQ ';' OR			$
			tmpstr EQ ':' OR			$
			tmpstr EQ '"' OR			$
			tmpstr EQ "'" OR			$
			tmpstr EQ '[' OR			$
			tmpstr EQ ']' OR			$
			tmpstr EQ '{' OR			$
			tmpstr EQ '}' OR			$
			tmpstr EQ '|' OR			$
			tmpstr EQ '\', cnt)
IF cnt GT 0 THEN tmpstr[idx] = '_'
xdatadesc	= ''
FOR g = 0, len-1 DO xdatadesc = xdatadesc+tmpstr[g]
ydatadesc	= STRCOMPRESS(STRTRIM(data_desc+'_y',2))
len		= STRLEN(ydatadesc)
tmpstr		= STRARR(len)
FOR g = 0, len-1 DO tmpstr[g] = STRMID(ydatadesc,g,1)
idx		= WHERE(					$
			tmpstr EQ ' ' OR 			$
			tmpstr EQ ':' OR 			$
			tmpstr EQ '/' OR 			$
			tmpstr EQ '!' OR			$
			tmpstr EQ '@' OR			$
			tmpstr EQ '#' OR			$
			tmpstr EQ '$' OR			$
			tmpstr EQ '%' OR			$
			tmpstr EQ '^' OR			$
			tmpstr EQ '&' OR			$
			tmpstr EQ '*' OR			$
			tmpstr EQ '(' OR			$
			tmpstr EQ ')' OR			$
			tmpstr EQ '-' OR			$
			tmpstr EQ '+' OR			$
			tmpstr EQ '=' OR			$
			tmpstr EQ '?' OR			$
			tmpstr EQ '<' OR			$
			tmpstr EQ ',' OR			$
			tmpstr EQ ';' OR			$
			tmpstr EQ ':' OR			$
			tmpstr EQ '"' OR			$
			tmpstr EQ "'" OR			$
			tmpstr EQ '[' OR			$
			tmpstr EQ ']' OR			$
			tmpstr EQ '{' OR			$
			tmpstr EQ '}' OR			$
			tmpstr EQ '|' OR			$
			tmpstr EQ '\', cnt)
IF cnt GT 0 THEN tmpstr[idx] = '_'
ydatadesc	= ''
FOR g = 0, len-1 DO ydatadesc = ydatadesc+tmpstr[g]
;print,'xdatadesc=',xdatadesc
;print,'ydatadesc=',ydatadesc
;print,'datadesc=',datadesc

				xid		= NCDF_DIMDEF( ncdf_id, xdatadesc, (*data_ptr)->GetImageWidth() )
				yid		= NCDF_DIMDEF( ncdf_id, ydatadesc, (*data_ptr)->GetImageHeight() )
;help,xid,yid
eee	= CREATE_STRUCT(ncdf_types[data_type_idx],1)
var_id	= CALL_FUNCTION('NCDF_VARDEF',ncdf_id,datadesc,[xid,yid],_Extra=eee)

;help,var_id
;help,xid
;help,yid

;var_id	= CALL_FUNCTION('NCDF_VARDEF',ncdf_id,data_desc,[xid,yid],ncdf_types[data_type_idx])
;;;ckt,aug2004				str2exec	=					$
;;;ckt,aug2004					'var_id=' +					$
;;;ckt,aug2004					'NCDF_VARDEF(ncdf_id,data_desc,[xid,yid],'+ncdf_types[data_type_idx]+')'
;;;ckt,aug2004				success		= EXECUTE( str2exec )

				NCDF_CONTROL, ncdf_id, /ENDEF

				NCDF_VARPUT, ncdf_id, var_id,					$
						REVERSE(					$
							(*data_ptr)->ReturnUnbytescaledImage(	$
							0,					$
							(*data_ptr)->GetImageWidth()-1L,	$
							0,					$
							(*data_ptr)->GetImageHeight()-1L,	$
							(*data_ptr)->GetImageWidth(),		$
							(*data_ptr)->GetImageHeight(),		$
							DATA_TYPE = (*data_ptr)->GetNumberType(), $
							/NATIVE_RES ), 2 )
				NCDF_CONTROL, ncdf_id, /REDEF
			ENDIF
	ENDFOR

	NCDF_CLOSE, ncdf_id

;;;	msg	= [ 'Sorry, saving scientific data out to NetCDF',	$
;;;		    'format is not currently implemented' ]
;;;
;;;	res	= DIALOG_MESSAGE( msg, /INFORMATION )
END
; GEOREF_IMAGE::SaveNetCDF

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::SaveBinary @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::SaveBinary, out_fname
	num_types	= [ 'UNDEFINED',					$
			    'BYTE',						$
			    'INT16',						$
			    'INT32',						$
			    'FLOAT32',						$
			    'DOUBLE',						$
			    'UNDEFINED',					$
			    'STRING',						$
			    'UNDEFINED',					$
			    'UNDEFINED',					$
			    'UNDEFINED',					$
			    'UNDEFINED',					$
			    'UINT16',						$
			    'UINT32',						$
			    'LONG64',						$ ;long64
			    'ULONG64' ]						  ;ulong64

	num_arr		= [ 'UNDEFINED',					$
			    'BYTARR',						$
			    'INTARR',						$
			    'LONARR',						$
			    'FLTARR',						$
			    'DBLARR',						$
			    'UNDEFINED',					$
			    'STRARR',						$
			    'UNDEFINED',					$
			    'UNDEFINED',					$
			    'UNDEFINED',					$
			    'UNDEFINED',					$
			    'UINTARR',						$
			    'ULONARR',						$
			    'LON64ARR',						$ ;long64
			    'ULON64ARR' ]					  ;ulong64


	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== GEOREF_IMAGE::SaveBinary =========='
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

	n_datasets		= 1
	plane_display_status	= SELF->GetPlaneDisplayStatus()
	idx			= (WHERE( plane_display_status EQ 1 ))[0]
	;
	; The above statement returns the index of the one displayed band, but
	; it DOES NOT tell the program the index of the dataset displayed within the
	; band of interest, hence the CASE statement below:
	;
	CASE idx[0] OF
		0: idx = SELF.redBandIdx
		1: idx = SELF.grnBandIdx
		2: idx = SELF.bluBandIdx
		ELSE:
	ENDCASE
	data_ptrs		= [(*(SELF.dataPtr))[idx]]
	src_files		= ['']
	data_descs		= ['']
	IF N_ELEMENTS(*(SELF.data_source_filename_ptr))-1 GE idx THEN src_files = [(*(SELF.data_source_filename_ptr))[idx]]
	IF N_ELEMENTS(*(SELF.data_description_ptr))-1 GE idx THEN data_descs = [(*(SELF.data_description_ptr))[idx]]

	out_fnames		= [out_fname]
	out_finfonames		= [out_fname+'_info']

	IF  PTR_VALID((SELF.lonImgPtr)[1]) AND				$
	    PTR_VALID((SELF.latImgPtr)[1]) THEN BEGIN
		n_datasets	= n_datasets + 2
		src_files	= [ SELF.lon_source_filename,		$
				    SELF.lat_source_filename,		$
				    src_files ]
		data_ptrs	= [ (SELF.lonImgPtr)[0],		$
				    (SELF.latImgPtr)[0],		$
				    data_ptrs ]
		data_descs	= [ 'Longitude Image',			$
				    'Latitude Image',			$
				    data_descs ]

		tmp		= STR_SEP( out_fname, GetDirectoryDivider() )
		filename_only	= tmp[N_ELEMENTS(tmp)-1]
		qualified_dir	= tmp[0]
		FOR g = 1, N_ELEMENTS(tmp)-2 DO qualified_dir = qualified_dir + GetDirectoryDivider() + tmp[g]
		IF STRLEN(qualified_dir) GT 0 THEN qualified_dir = qualified_dir + GetDirectoryDivider()

		tmp		= STR_SEP( filename_only, '.' )
		n_parts		= N_ELEMENTS(tmp)
		IF n_parts GT 1 THEN BEGIN
			fname_start = tmp[0]

			FOR g = 1, N_ELEMENTS(tmp)-2 DO fname_start = fname_start + '.' + tmp[g]

			suffix	= tmp[N_ELEMENTS(tmp)-1]

			out_fnames	= [								$
						qualified_dir + fname_start + '_longitude.' + suffix,	$
						qualified_dir + fname_start + '_latitude.' + suffix,	$
						out_fname ]
			out_finfonames	= [									$
						qualified_dir + fname_start + '_longitude.' + suffix + '_info',	$
						qualified_dir + fname_start + '_latitude.' + suffix + '_info',	$
						out_fname + '_info' ]
		ENDIF ELSE BEGIN
			out_fnames	= [								$
						out_fname + '_longitude',				$
						out_fname + '_latitude',				$
						out_fname ]
			out_finfonames	= 	[							$
						out_fname + '_longitude_info',				$
						out_fname + '_latitude_info',				$
						out_fname + '_info' ]
		ENDELSE
	ENDIF

	FOR i = 0, n_datasets - 1 DO BEGIN
		src_file	= src_files[i]
		data_type_idx	= (*(data_ptrs[i]))->GetNumberType()
		data_ptr	= data_ptrs[i]
		data_desc	= data_descs[i]
		xy_dims		= [ (*data_ptr)->GetImageWidth(), (*data_ptr)->GetImageHeight() ]

		OPENW, lun, out_fnames[i], /GET_LUN

		WRITEU,								$
			lun, 							$
			REVERSE(						$
				(*data_ptr)->ReturnUnbytescaledImage(		$
				0,						$
				(*data_ptr)->GetImageWidth()-1L,		$
				0,						$
				(*data_ptr)->GetImageHeight()-1L,		$
				(*data_ptr)->GetImageWidth(),			$
				(*data_ptr)->GetImageHeight(),			$
				DATA_TYPE = (*data_ptr)->GetNumberType(),	$
				/NATIVE_RES ), 2 )
		FREE_LUN, lun

		OPENW, lun, out_finfonames[i], /GET
		PRINTF, lun, 'Information for binary file: ' + out_fnames[i]
		PRINTF, lun, ''
		PRINTF, lun, 'Width of Image: ' + STRTRIM((*data_ptr)->GetImageWidth(),2)
		PRINTF, lun, 'Height of Image: ' + STRTRIM((*data_ptr)->GetImageHeight(),2)
		PRINTF, lun, 'Number Type: ' + num_types[ data_type_idx ]
		PRINTF, lun, ''
		PRINTF, lun, 'Sample IDL Code to read in data from file:'
		PRINTF, lun, ''
		PRINTF, lun, 'data='+num_arr[ data_type_idx ]+'('+STRTRIM((*data_ptr)->GetImageWidth(),2)+','+STRTRIM((*data_ptr)->GetImageHeight(),2)+')'
		PRINTF, lun, 'OPENR,lun,"' + out_fnames[i] + '",/GET_LUN'
		PRINTF, lun, 'READU, lun, data'
		PRINTF, lun, 'FREE_LUN, lun'
		PRINTF, lun, ''
		PRINTF, lun, 'THE REFERENCED BINARY FILE HAS BEEN CREATED ON THE FOLLOWING MACHINE TYPE:'
		PRINTF, lun, ''
		PRINTF, lun, '     OS: ' + !VERSION.OS
		PRINTF, lun, '     OS FAMILY: ' + !VERSION.OS_FAMILY
		PRINTF, lun, '     ARCHITECTURE: ' + !VERSION.ARCH
		PRINTF, lun, ''
		PRINTF, lun, 'READING THIS DATA ON MACHINE TYPES WITH DIFFERING BYTE ORDERING MAY'
		PRINTF, lun, 'REQUIRE BYTE-SWAPPING BY THE USER.  WITHIN IDL, THIS CAN BE PERFORMED'
		PRINTF, lun, 'WITH THE FUNCTION SWAP_ENDIAN (e.g., data=SWAP_ENDIAN(data)) OR THE'
		PRINTF, lun, 'PROCEDURE BYTEORDER (e.g., BYTEORDER, data, /LSWAP).  PLEASE SEE THE'
		PRINTF, lun, 'IDL DOCUMENTATION FOR MORE INFORMATION ON HOW TO USE THESE ROUTINES.'

		FREE_LUN, lun

	ENDFOR

END
; GEOREF_IMAGE::SaveBinary

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::SaveVICAR @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::SaveVICAR, out_fname

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== GEOREF_IMAGE::SaveVICAR =========='
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

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; This is for saving data in it's native bit depth.
	;; Please see Save_Image for saving RGB image data in VICAR format.
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	plane_display_status	= SELF->GetPlaneDisplayStatus()
	;
	; The above statement returns the index of the one displayed band, but
	; it DOES NOT tell the program the index of the dataset displayed within the
	; band of interest, hence the CASE statement below:
	;
	idx	= WHERE( plane_display_status EQ 1 )
	CASE idx[0] OF
		0: idx = SELF.redBandIdx
		1: idx = SELF.grnBandIdx
		2: idx = SELF.bluBandIdx
		ELSE:
	ENDCASE

	data_ptr = (*(SELF.dataPtr))[idx]
	data_ptr = data_ptr[0]
	data_type = (*data_ptr)->GetNumberType()

	; VICAR does not support Unsigned Integer.  Convert to FULLWORD.
	IF data_type EQ 12 THEN BEGIN
		msg = [ 'The native format of your data is Unsigned Integer,',		$
			'which is not supported by VICAR at this time.',		$
			' ',								$
			'Your data will be converted to FLOAT and saved as REAL.',	$
			'This will not alter your data values.',			$
			' ',								$
			'Continue?' ]

		ret = DIALOG_MESSAGE( msg, /QUESTION )

		IF STRUPCASE( ret ) EQ 'NO' THEN RETURN

		write_vicar, out_fname,							$
			FLOAT(								$
			ROTATE( (*data_ptr)->ReturnUnbytescaledImage(			$
						0,						$
						(*data_ptr)->GetImageWidth()-1L,		$
						0,						$
						(*data_ptr)->GetImageHeight()-1L,		$
						(*data_ptr)->GetImageWidth(),			$
						(*data_ptr)->GetImageHeight(),			$
						DATA_TYPE = data_type				$
						),						$
					7 ) $ ;flip vertically (about the horizontal azis) for VICAR format.
					)
	ENDIF ELSE BEGIN
		write_vicar, out_fname,							$
			ROTATE( (*data_ptr)->ReturnUnbytescaledImage(			$
						0,						$
						(*data_ptr)->GetImageWidth()-1L,		$
						0,						$
						(*data_ptr)->GetImageHeight()-1L,		$
						(*data_ptr)->GetImageWidth(),			$
						(*data_ptr)->GetImageHeight(),			$
						DATA_TYPE = data_type				$
						),						$
					7 ) ;flip vertically (about the horizontal azis) for VICAR format.
	ENDELSE
END
; GEOREF_IMAGE::SaveVICAR

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ save_data_eh @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO save_data_eh, event

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== save_data_eh =========='
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


	WIDGET_CONTROL, event.id, GET_UVALUE = what_widget
	WIDGET_CONTROL, event.top, GET_UVALUE = info, /NO_COPY
	kill_tlb	= 0

	CASE what_widget OF
		'ok': BEGIN

			plane_display_status	= info.obj->GetPlaneDisplayStatus()
			n_images		= TOTAL( plane_display_status )

			CASE 1 OF
				n_images LE 0 : BEGIN
					msg = [	'All viewplanes seem to be turned off.  Please use the',	$
						'Manage Data to turn on a single plane such that the',		$
						'data data you would like to save is viewed in black and',	$
						'white.',							$
						'',								$
						'Please try Save Data again, thank you.']
					ret = DIALOG_MESSAGE( msg, /INFORMATION )

					WIDGET_CONTROL, event.top, SET_UVALUE = info, /NO_COPY
					WIDGET_CONTROL, event.top, /DESTROY
					RETURN
				END

				n_images GT 1 AND info.only_one_plane_allowed[info.select_idx] : BEGIN
					msg = [	'You have more than one R, G or B image plane loaded and',	$
						'displayed, which is not allowed when saving ' + STRTRIM(info.out_fmt[info.select_idx],2) + ' files.', $
						'Please use the Manage Data to turn off unwanted planes',	$
						'such that the data you would like to save is viewed in',	$
						'black and white.',						$
						'Then, data can then be saved to a single '+STRTRIM(info.out_fmt[info.select_idx],2)+' file',		$
						'as a mosaicked array at the native bit depth.',		$
						'',								$
						'Please try Save Data again, thank you.',			$
						'',								$
						'P.S.  If you are trying to save a color image, then please',	$
						'use Save Image instead of Save Data.' ]
					ret = DIALOG_MESSAGE( msg, /INFORMATION )

					WIDGET_CONTROL, event.top, SET_UVALUE = info, /NO_COPY
					WIDGET_CONTROL, event.top, /DESTROY
					RETURN
				END
				ELSE:
			ENDCASE








			WIDGET_CONTROL, /HOURGLASS

			cancel_this	= 0
			;-----------------------------------------------------
			; The user-selected directory is NOT stored because
			; the button that calls this has a UVALUE that is used
			; for something else.  An alternate storage location
			; would need to be utilized to implement such storage.
			; This particular feature maybe won't be used often
			; enough to require such directory storage and recall.
			; If it does, then implement it already!
			;-----------------------------------------------------
;;;ckt,apr2001			out_fname	= DIALOG_PICKFILE( TITLE = 'Enter name of output file', GET_PATH = pathname )

			default_filename_prefix	= 'Untitled-'
			default_filename_suffix	= info.out_suffix[info.select_idx]
			found			= 1
			file_number		= 1
			WHILE found DO BEGIN
				default_filename	= default_filename_prefix + STRTRIM(file_number,2) + default_filename_suffix
				found_file		= FINDFILE( default_filename, COUNT = cnt )
				IF cnt LE 0 THEN found = 0 ELSE file_number = file_number + 1
			ENDWHILE

			out_fname	= dialog_pickfile_wrapper(				$
				TITLE = 'Enter name of output ' +				$
				STRTRIM(info.out_fmt[info.select_idx],2) +			$
				' file', GET_PATH = pathname,					$
				FILE = default_filename )
			IF STRTRIM(pathname,2) EQ STRTRIM(out_fname,2) THEN out_fname = ''
			tmp		= STR_SEP( out_fname, GetDirectoryDivider() )
			fname		= tmp[ N_ELEMENTS(tmp) - 1 ]
			WHILE STRTRIM(out_fname,2) EQ '' AND NOT cancel_this DO BEGIN
				msg	= [ 'No file name was entered...',	$
					    'Do you want to cancel the save operation?' ]
				res	= DIALOG_MESSAGE( msg, /QUESTION )
				IF STRUPCASE(res) EQ 'YES' THEN BEGIN
					cancel_this = 1
				ENDIF ELSE BEGIN
;;;ckt,apr2001					out_fname	= DIALOG_PICKFILE( TITLE = 'Enter name of output file', GET_PATH = pathname )
;;;ckt,nov2002					out_fname	= dialog_pickfile_wrapper( TITLE = 'Enter name of output file', GET_PATH = pathname )
					out_fname	= dialog_pickfile_wrapper(				$
						TITLE = 'Enter name of output ' +				$
						STRTRIM(info.out_fmt[info.select_idx],2) +			$
						' file', GET_PATH = pathname,					$
						FILE = default_filename )
					IF STRTRIM(pathname,2) EQ STRTRIM(out_fname,2) THEN out_fname = ''
					tmp		= STR_SEP( out_fname, GetDirectoryDivider() )
					fname		= tmp[ N_ELEMENTS(tmp) - 1 ]
				ENDELSE
			ENDWHILE
;help,info
;help,info.obj
;print,'obj_valid(info.obj)? = ',obj_valid(info.obj)
			IF NOT cancel_this THEN BEGIN
				CASE info.out_fmt[info.select_idx] OF
					'HDF'		: info.obj->SaveHDF, out_fname
					'NetCDF'	: info.obj->SaveNetCDF, out_fname
					'Binary'	: info.obj->SaveBinary, out_fname
					'VICAR'		: info.obj->SaveVICAR, out_fname
					ELSE:
				ENDCASE
			ENDIF

			kill_tlb	= 1
			END
		'cancel': BEGIN
			info.cancel_pressed = 1
			kill_tlb	= 1
			END
		'cwb': BEGIN
			IF event.select THEN info.select_idx = event.value
			FOR j = 0, N_ELEMENTS(info.lbl_ids)-1 DO		$
				WIDGET_CONTROL, info.lbl_ids[j],		$
					SET_VALUE = info.out_lbls[j,info.select_idx]
			END
		ELSE:
	ENDCASE
	WIDGET_CONTROL, event.top, SET_UVALUE = info, /NO_COPY

	IF kill_tlb THEN WIDGET_CONTROL, event.top, /DESTROY
END
; save_data_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Return_Save_Data_Base @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION Return_Save_Data_Base, group_leader, obj, MAP_STATUS = map_status
	select_idx	= 0
	map		= 0
	IF KEYWORD_SET(map_status) THEN map = 1

	tlb		= WIDGET_BASE(						$
				GROUP_LEADER = group_leader,			$
				/COLUMN,					$
				TITLE = 'Save Scientific Data',			$
				MAP = map,					$
				/BASE_ALIGN_CENTER )

	lbl_ids	= LONARR(10)

;;;ckt,nov2002	lbl1	= WIDGET_LABEL( tlb, VALUE = 'Select a scientific output format below.' )
;;;ckt,nov2002	lbl2	= WIDGET_LABEL( tlb, VALUE = 'Each band/plane of data within the viewer' )
;;;ckt,nov2002	lbl3	= WIDGET_LABEL( tlb, VALUE = 'will be stored as a separate scientific dataset' )
;;;ckt,nov2002	lbl4	= WIDGET_LABEL( tlb, VALUE = 'in a single file and will represent the' )
;;;ckt,nov2002	lbl5	= WIDGET_LABEL( tlb, VALUE = 'identical data values and number types as' )
;;;ckt,nov2002	lbl6	= WIDGET_LABEL( tlb, VALUE = 'found in the source file(s).' )
	HDF_LIB_INFO, VERSION = v
	out_lbls= [								$
			[							$
				'Each band/plane of data within the viewer',	$
				'will be stored as a separate scientific',	$ ;HDF
				'dataset (SDS) in a single file and will',	$
				'represent the identical data values and',	$
				'number types as found in the source',		$
				'file(s).',					$
				'',						$
				'HDF Version Information:',			$
				'',						$
				v ],						$
			[							$
				'Each band/plane of data within the viewer',	$
				'will be stored as a separate variable',	$ ;NetCDF
				'in a single file and will represent the',	$
				'identical data values and number types',	$
				'as found in the source file(s) unless the',	$
				'native number type is some sort of unsigned',	$
				'integer.  In this case, the data can be',	$
				'converted to FLOAT while retaining the',	$
				'data values.',					$
				'' ],						$
			[							$
				'Data will be stored in flat binary format.',	$
				'Additionally, a text file will be generated',	$
				'which provides useful information on the data',$
				'and how to access it.',			$
				'ONLY ONE BAND/PLANE CAN BE ACTIVE PER SAVE.',	$
				'',						$
				'',						$
				'',						$
				'',						$
				''],						$
			[							$
				'Data will be stored in VICAR format.',		$ ;VICAR
				'ONLY ONE BAND/PLANE CAN BE ACTIVE PER SAVE.',	$
				'',						$
				'',						$
				'',						$
				'',						$
				'',						$
				'',						$
				'',						$
				''  ]	]

	FOR j = 0, N_ELEMENTS(lbl_ids)-1 DO					$
		lbl_ids[j]	= WIDGET_LABEL(					$
					tlb,					$
					/DYNAMIC_RESIZE,			$
					VALUE = out_lbls[j] )

	out_fmt		= [							$
			'HDF',							$
			'NetCDF',						$
			'Binary',						$
			'VICAR' ]
	out_suffix	= [							$
			'.hdf',							$
			'.ncdf',						$
			'_'+STRTRIM(!VERSION.OS_FAMILY)+'.bin',			$
			'.vic' ]

	only_one_plane_allowed							$
		= [								$
			0,							$
			0,							$
			1,							$
			1 ]

	cwb	= CW_BGROUP(							$
			tlb,							$
			out_fmt,						$
			/EXCLUSIVE,						$
			/RETURN_INDEX,						$
			UVALUE = 'cwb',						$
			IDS = cwb_ids,						$
			SET_VALUE = 0 )

	sub	= WIDGET_BASE( tlb, /ROW, /BASE_ALIGN_CENTER )
	ok	= WIDGET_BUTTON( sub, VALUE = 'OK', UVALUE = 'ok',		$
			/ALIGN_CENTER )
	cancel	= WIDGET_BUTTON( sub, VALUE = 'Cancel', UVALUE = 'cancel',	$
			/ALIGN_CENTER )

	WIDGET_CONTROL, tlb, /REALIZE

	info	= {								$
		cwb_ids			: cwb_ids,				$
		select_idx		: select_idx,				$
		cancel_pressed		: 0,					$
		obj			: obj,					$
		out_fmt			: out_fmt,				$
		only_one_plane_allowed	: only_one_plane_allowed,		$
		out_suffix		: out_suffix,				$
		out_lbls		: out_lbls,				$
		lbl_ids			: lbl_ids }

	WIDGET_CONTROL, tlb, SET_UVALUE = info, /NO_COPY

	XMANAGER, 'Save_Data', tlb, EVENT_HANDLER = 'save_data_eh'

	RETURN, tlb

END
; Return_Save_Data_Base

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::Save_Data @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::Save_Data
	IF NOT WIDGET_INFO( SELF.save_data_tlb, /VALID_ID ) THEN		$
		SELF.save_data_tlb = Return_Save_Data_Base( SELF.parentBase, SELF )

	WIDGET_CONTROL, SELF.save_data_tlb, MAP = 1
END
; GEOREF_IMAGE::Save_Data

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Save_Image_Kill @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO Save_Image_Kill, tlb
END
; Save_Image_Kill

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Save_Image_eh @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO Save_Image_eh, event

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== Save_Image_eh (GEOREF_IMAGE) =========='
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

	COMMON GEOREFIMAGE, linkObj, nullObj, linkMsg, nObj, idArr, commonblock_pseudo_color_table_ptrarr, commonblock_self

	WIDGET_CONTROL, event.id, GET_VALUE = val
	WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr

;print,'Save_Image_eh   ---   event.id val = ',val

	IF STRUPCASE(val) EQ 'HELP' THEN BEGIN
		display_save_image_help, event.top
		RETURN
	ENDIF

	IF STRUPCASE(val) EQ 'CANCEL' THEN BEGIN
		WIDGET_CONTROL, event.top, /DESTROY
		RETURN
	ENDIF

	IF STRUPCASE(val) EQ 'SELECT...' THEN BEGIN
		;-----------------------------------------------------
		; User-selected directory is stored in the button's
		; uvalue.
		;-----------------------------------------------------
		WIDGET_CONTROL, (*infoPtr).save_button, GET_UVALUE = dir
		fileCheck	= ''
		WHILE STRLEN(fileCheck) LE 0 DO BEGIN
			fileName= dialog_pickfile_wrapper( TITLE = 'enter ' + STRTRIM(STRUPCASE((*infoPtr).saveType),2) + ' file name', $
						PATH = dir, GET_PATH = dir )
			IF dir NE '' THEN	fileCheck	= STRTRIM( (STR_SEP(fileName,dir))[1], 2 )	$
			ELSE			fileCheck	= STRTRIM( fileName, 2 )
			IF fileCheck EQ '' THEN BEGIN
				msg	= 'No file name was entered... Retry?'
				res	= DIALOG_MESSAGE( msg, /QUESTION )
				IF STRTRIM(STRUPCASE(res),2) EQ 'NO' THEN RETURN
			ENDIF ELSE BEGIN
				CD, CURRENT = currdir
				IF STRTRIM(dir,2) NE '' THEN CD, dir
				file_exists	= FILE_SEARCH(fileName,COUNT=filecnt)
				CD, currdir
				IF filecnt GT 0 THEN BEGIN
					msg	= ['The file ' + fileName + ' exists...',	$
						   'OVERWRITE THIS FILE?' ]
					ans	= DIALOG_MESSAGE(msg,/QUESTION)
					IF STRTRIM(STRUPCASE(ans),2) EQ 'NO' THEN fileCheck = ''
				ENDIF
			ENDELSE

		ENDWHILE
		;-----------------------------------------------------
		; Store the newly user-selected directory in the
		; button's uvalue.
		;-----------------------------------------------------
		WIDGET_CONTROL, (*infoPtr).save_button, SET_UVALUE = dir
		IF dir NE '' THEN BEGIN
			dir	= STRTRIM(dir,2)
			IF STRMID(dir,STRLEN(dir)-1,1) NE PATH_SEP() THEN dir = dir + PATH_SEP()
		ENDIF
		WIDGET_CONTROL, (*infoPtr).file_label, SET_VALUE = dir+fileCheck
		RETURN
	ENDIF


	WIDGET_CONTROL, (*infoPtr).file_label, GET_VALUE = fileName



	fileName	= fileName[0]



	IF STRLOWCASE(STRTRIM(fileName,2)) EQ 'none' THEN BEGIN

		msg	= ['Enter a file name before attempting to proceed!']

		res	= DIALOG_MESSAGE(msg,/ERROR)

		RETURN

	ENDIF


	;-----------------------------------------------------
	; User-selected directory is stored in the button's
	; uvalue.
	;-----------------------------------------------------
	WIDGET_CONTROL, event.id, GET_UVALUE = dir


	;----------------------------------------------------------------------
	; First determine whether keyword RETURN_DISPLAY_ONLY should be active
	; or not; this is accomplished by setting display_type to either one (1)
	; or zero (0)
	;----------------------------------------------------------------------
	display_type	= STRUPCASE((*infoPtr).areaType) NE 'ALL'

	;----------------------------------------------------------------------
	; call object method GetBand to retrieve fully-processed data; Get Band
	; will return one of three things:
	;	if a band is active and has data loaded into it, GetBand returns
	;		the fully-processed data
	;	if a band is not active and it is determined by GetBand that
	;		there are at least 2 active bands, GetBand returns
	;		a zeroed-out byte array for the current band
	;	if a band is not active and it is determined by GetBand that
	;		there is only one active band, GetBand returns a
	;		scalar value of (-1) for the current band.
	;----------------------------------------------------------------------
	red_band	= ((*infoPtr).obj)->GetBand( (*infoPtr).redBandIdx, 0, RETURN_DISPLAY_ONLY = display_type )
	grn_band	= ((*infoPtr).obj)->GetBand( (*infoPtr).grnBandIdx, 1, RETURN_DISPLAY_ONLY = display_type )
	blu_band	= ((*infoPtr).obj)->GetBand( (*infoPtr).bluBandIdx, 2, RETURN_DISPLAY_ONLY = display_type )



	colorbar_present= 0
	data_desc_rrr	= ''
	data_units	= ''
	data_desc_ggg	= ''
	data_desc_bbb	= ''

	;----------------------------------------------------------------------
	; Check to see if only a single band is displayed in which case the
	; image saved to file will be black and white.  Also check to see if
	; Data Manager has all colors turned off in which case just RETURN.
	;----------------------------------------------------------------------
;;;;	IF N_ELEMENTS(red_band) NE N_ELEMENTS(grn_band) OR N_ELEMENTS(red_band) NE N_ELEMENTS(blu_band) THEN BEGIN
	plane_display_status	= [ ( (*infoPtr).redBandIdx GE 0 AND (*infoPtr).display_rgb[0] ),	$
				    ( (*infoPtr).grnBandIdx GE 0 AND (*infoPtr).display_rgb[1] ),	$
				    ( (*infoPtr).bluBandIdx GE 0 AND (*infoPtr).display_rgb[2] ) ]
	n_displayed_data	= TOTAL( plane_display_status )
	IF n_displayed_data EQ 0 THEN BEGIN
		result = DIALOG_MESSAGE( 'No data is currently displayed.  Image cannot be saved to file.' )
		RETURN
	ENDIF
;help,n_displayed_data
;help,(*infoPtr).obj -> get_colorbar_drawwidgetid()
;help,WIDGET_INFO( (*infoPtr).obj -> get_colorbar_drawwidgetid(), /VALID_ID )
	IF n_displayed_data EQ 1 THEN BEGIN

		n_elem_arr	= [ N_ELEMENTS(red_band), N_ELEMENTS(grn_band), N_ELEMENTS(blu_band) ]
		max_elem	= MAX(n_elem_arr)
		idx		= (WHERE( n_elem_arr EQ max_elem ))[0]

		WIDGET_CONTROL, /HOURGLASS
		IF NOT WIDGET_INFO( (*infoPtr).obj -> get_colorbar_drawwidgetid(), /VALID_ID ) THEN BEGIN
;print,'b/w'
			CASE idx OF
				0: BEGIN
;print,'calling ((*infoPtr).obj)->GetPlaneDescription((*infoPtr).redBandIdx)'
;print,'(*infoPtr).redBandIdx=',(*infoPtr).redBandIdx
;print,'data_desc_rrr=',data_desc_rrr
					data_desc_rrr	= ((*infoPtr).obj)->GetPlaneDescription((*infoPtr).redBandIdx)
					data_units	= ((*infoPtr).obj)->GetPlaneUnits((*infoPtr).redBandIdx)
					out_img = red_band
					END
				1: BEGIN
					data_desc_ggg	= ((*infoPtr).obj)->GetPlaneDescription((*infoPtr).grnBandIdx)
					data_units	= ((*infoPtr).obj)->GetPlaneUnits((*infoPtr).grnBandIdx)
					out_img = grn_band
					END
				2: BEGIN
					data_desc_bbb	= ((*infoPtr).obj)->GetPlaneDescription((*infoPtr).bluBandIdx)
					data_units	= ((*infoPtr).obj)->GetPlaneUnits((*infoPtr).bluBandIdx)
					out_img = blu_band
					END
				ELSE:
			ENDCASE
		ENDIF ELSE BEGIN
			colorbar_present		= 1
			;----------------------------------------------------------------------
			; Auto-re-load the selected pseudo color table, just in case.
			;----------------------------------------------------------------------
			TVLCT,								$
				*(commonblock_pseudo_color_table_ptrarr[0]),		$
				*(commonblock_pseudo_color_table_ptrarr[1]),		$
				*(commonblock_pseudo_color_table_ptrarr[2])
			TVLCT, red_tab, grn_tab, blu_tab, /GET
;print,'pseudo color'
;help,idx
;print,red_tab
;print,grn_tab
;print,blu_tab
			CASE idx OF
				0: BEGIN
					;------------------------------------------------------
					; Red band contains pseudo colored data.
					;------------------------------------------------------
					nx		= (SIZE(red_band))[1]
					ny		= (SIZE(red_band))[2]
					out_img		= BYTARR( 3, nx, ny )
					out_img[0,*,*]	= red_tab( red_band )
					out_img[1,*,*]	= grn_tab( red_band )
					out_img[2,*,*]	= blu_tab( red_band )
					data_desc_rrr	= ((*infoPtr).obj)->GetPlaneDescription((*infoPtr).redBandIdx)
					data_units	= ((*infoPtr).obj)->GetPlaneUnits((*infoPtr).redBandIdx)
					END
				1: BEGIN
					;------------------------------------------------------
					; Green band contains pseudo colored data.
					;------------------------------------------------------
					nx		= (SIZE(grn_band))[1]
					ny		= (SIZE(grn_band))[2]
					out_img		= BYTARR( 3, nx, ny )
					out_img[0,*,*]	= red_tab( grn_band )
					out_img[1,*,*]	= grn_tab( grn_band )
					out_img[2,*,*]	= blu_tab( grn_band )
					data_desc_ggg	= ((*infoPtr).obj)->GetPlaneDescription((*infoPtr).grnBandIdx)
					data_units	= ((*infoPtr).obj)->GetPlaneUnits((*infoPtr).grnBandIdx)
					END
				2: BEGIN
					;------------------------------------------------------
					; Blue band contains pseudo colored data.
					;------------------------------------------------------
					nx		= (SIZE(blu_band))[1]
					ny		= (SIZE(blu_band))[2]
					out_img		= BYTARR( 3, nx, ny )
					out_img[0,*,*]	= red_tab( blu_band )
					out_img[1,*,*]	= grn_tab( blu_band )
					out_img[2,*,*]	= blu_tab( blu_band )
					data_desc_bbb	= ((*infoPtr).obj)->GetPlaneDescription((*infoPtr).bluBandIdx)
					data_units	= ((*infoPtr).obj)->GetPlaneUnits((*infoPtr).bluBandIdx)
					END
				ELSE:
			ENDCASE
		ENDELSE

	ENDIF ELSE BEGIN
;print,'true color'
		IF plane_display_status[0] THEN BEGIN
			data_desc_rrr	= ((*infoPtr).obj)->GetPlaneDescription((*infoPtr).redBandIdx)
		ENDIF
		IF plane_display_status[1] THEN BEGIN
			data_desc_ggg	= ((*infoPtr).obj)->GetPlaneDescription((*infoPtr).grnBandIdx)
		ENDIF
		IF plane_display_status[2] THEN BEGIN
			data_desc_bbb	= ((*infoPtr).obj)->GetPlaneDescription((*infoPtr).bluBandIdx)
		ENDIF

		;----------------------------------------------------------------------
		; If there is more than one active band, simply create a 3D byte array
		; with the band index being the first dimension
		;----------------------------------------------------------------------
		nx		= (SIZE(red_band))[1]
		ny		= (SIZE(red_band))[2]
		out_img		= BYTARR( 3, nx, ny )
		out_img[0,*,*]	= REFORM(red_band,1,nx,ny)
		out_img[1,*,*]	= REFORM(grn_band,1,nx,ny)
		out_img[2,*,*]	= REFORM(blu_band,1,nx,ny)
	ENDELSE

	cb_rgb		= BYTARR(3)

	IF n_displayed_data EQ 1 AND					$
		WIDGET_INFO( (*infoPtr).obj -> get_colorbar_drawwidgetid(), /VALID_ID ) AND $
		colorbar_present THEN BEGIN

		WIDGET_CONTROL, /HOURGLASS
		curwin = !D.WINDOW
		WSET, (*infoPtr).obj -> get_colorbar_window()

		;----------------------------------------------------------------------
		; Auto-re-load the selected pseudo color table, just in case.
		;----------------------------------------------------------------------
		TVLCT,								$
			*(commonblock_pseudo_color_table_ptrarr[0]),		$
			*(commonblock_pseudo_color_table_ptrarr[1]),		$
			*(commonblock_pseudo_color_table_ptrarr[2])
		TVLCT, red_tab, grn_tab, blu_tab, /GET
;tmpp=bindgen(256,256)
;window,/free
;tv,tmpp
;tv,grn_tab[tmpp],ch=2
;tv,blu_tab[tmpp],ch=3
		; need to load grayscale palette in order to properly
		; obtain grayscale image in some cases (e.g., running on PC with device,decompose=0)
		LOADCT, 0
		colorbar_redraw, TLB = ((*infoPtr).obj) -> getDrawBaseID()
		WSET, (*infoPtr).obj -> get_colorbar_window()
;print,'getting greyscale_image'
		greyscale_image = TVRD()

		TVLCT, red_tab, grn_tab, blu_tab
		colorbar_redraw, TLB = ((*infoPtr).obj) -> getDrawBaseID()

;save,greyscale_image,filename='greyscale.sav'
;window,/free
;		cb_red		= red_tab[ REVERSE( greyscale_image, 2) ]
; 		cb_grn		= grn_tab[ REVERSE( greyscale_image, 2) ]
;		cb_blu		= blu_tab[ REVERSE( greyscale_image, 2) ]
		cb_red		= red_tab[ greyscale_image ]
 		cb_grn		= grn_tab[ greyscale_image ]
		cb_blu		= blu_tab[ greyscale_image ]
;tv,greyscale_image
;tv,cb_grn,ch=2
;tv,cb_blu,ch=3
		cb_rgb		= TRANSPOSE([[[cb_red]],[[cb_grn]],[[cb_blu]]],[2,0,1])
		WSET, curwin
  	ENDIF

 	saveType	= STRUPCASE((*infoPtr).saveType)

	WIDGET_CONTROL, event.top, /DESTROY

 	parent_base	= ((*infoPtr).obj)->GetParentBase()


	save_image_gui,								$
				parent_base,					$
				fileName,					$
				colorbar_present,				$
				[data_desc_rrr, data_desc_ggg, data_desc_bbb],	$
				data_units,					$
				out_img,					$
				cb_rgb,						$
				saveType


END
; Save_Image_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ Save_Image_radioButton_eh @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO Save_Image_radioButton_eh, event

	WIDGET_CONTROL, event.id, GET_VALUE = val
	WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr

	IF STRUPCASE(val) EQ 'TIFF' THEN (*infoPtr).saveType = 'TIFF'
	IF STRUPCASE(val) EQ 'JPEG' THEN (*infoPtr).saveType = 'JPEG'
	IF STRUPCASE(val) EQ 'VICAR' THEN (*infoPtr).saveType = 'VICAR'

	IF STRUPCASE(val) EQ 'ALL LOADED BLOCKS' THEN (*infoPtr).areaType = 'ALL'
	IF STRUPCASE(val) EQ 'VIEWED AREA ONLY' THEN (*infoPtr).areaType = 'VIEW'

END
; Save_Image_radioButton_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::Save_Image @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::Save_Image


	tlb			= WIDGET_BASE( GROUP_LEADER = SELF.parentBase, /MODAL,	$
					/COLUMN, 					$
					KILL_NOTIFY = 'Save_Image_Kill',		$
					TITLE = 'Save Image' )
	lbl_01			= WIDGET_LABEL( tlb, VALUE = 'Save viewed image as one of these formats:' )

	radioButtonBase1	= WIDGET_BASE( tlb, /EXCLUSIVE, EVENT_PRO = 'Save_Image_radioButton_eh' )
	saveType1		= WIDGET_BUTTON( radioButtonBase1, VALUE = 'TIFF' )
	saveType2		= WIDGET_BUTTON( radioButtonBase1, VALUE = 'JPEG' )
	saveType3		= WIDGET_BUTTON( radioButtonBase1, VALUE = 'VICAR' )

	radioButtonBase2	= WIDGET_BASE( tlb, /EXCLUSIVE, EVENT_PRO = 'Save_Image_radioButton_eh', /FRAME )
	saveAllButton		= WIDGET_BUTTON( radioButtonBase2, VALUE = 'ALL LOADED BLOCKS' )
	saveViewButton		= WIDGET_BUTTON( radioButtonBase2, VALUE = 'VIEWED AREA ONLY' )

	filename_base		= WIDGET_BASE(				$
					tlb,				$
					/ROW )
	filename_lbl1		= WIDGET_LABEL(				$
					filename_base,			$
					VALUE = 'FILE TO BE SAVED: ' )
	filename_lbl2		= WIDGET_LABEL(				$
					filename_base,			$
					/DYNAMIC_RESIZE,		$
					VALUE = '               NONE               ' )
	filename_btn		= WIDGET_BUTTON(			$
					filename_base,			$
					VALUE = 'Select...',			$
					UVALUE = 'select' )

	saveBase		= WIDGET_BASE( tlb, /ROW )
	saveButton		= WIDGET_BUTTON( saveBase, VALUE = 'SAVE' )	; UVALUE of this button stores user-selected path.
	cancelButton		= WIDGET_BUTTON( saveBase, VALUE = 'CANCEL' )

	helpButton		= WIDGET_BUTTON( saveBase, VALUE = 'HELP' )

	lbl_02			= WIDGET_LABEL( tlb, VALUE = 'Image will be saved as 8-bits per channel.' )
	lbl_03			= WIDGET_LABEL( tlb, VALUE = 'The current stretch will be applied.' )

	WIDGET_CONTROL, tlb, DEFAULT_BUTTON = saveButton
	WIDGET_CONTROL, tlb, /REALIZE

	infoPtr = PTR_NEW( {					$
		saveType		: 'TIFF'		,$
		areaType		: 'ALL'			,$
		obj			: SELF			,$
		dataPtr			: SELF.dataPtr		,$
		redBandIdx		: SELF.redBandIdx	,$
		grnBandIdx		: SELF.grnBandIdx	,$
		bluBandIdx		: SELF.bluBandIdx	,$
		display_rgb		: SELF.display_rgb	,$
		file_label		: filename_lbl2		,$
		save_button		: saveButton		 $
		} )

	WIDGET_CONTROL, tlb, SET_UVALUE = infoPtr

	WIDGET_CONTROL, saveType1, SET_BUTTON = 1
;	WIDGET_CONTROL, saveType3, SENSITIVE = 0
	WIDGET_CONTROL, saveAllButton, SET_BUTTON = 1

	XMANAGER, 'Save Image Base', tlb, EVENT_HANDLER = 'Save_Image_eh'


END
; GEOREF_IMAGE::Save_Image

PRO GEOREF_IMAGE::SetDisplayRGB, rgb
	SELF.display_rgb	= rgb
END

PRO GEOREF_IMAGE::SetIdxRGB, rgb
	SELF.redBandIdx		= rgb[0]
	SELF.grnBandIdx		= rgb[1]
	SELF.bluBandIdx		= rgb[2]
END

PRO GEOREF_IMAGE::SetIdxAncillary, anc
	FOR i = 0, N_ELEMENTS(anc)-1 DO (*(SELF.ancillaryBandIdxPtr))[i] = anc[i]
END

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Data_Manager_Kill @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO Data_Manager_Kill, tlb
END
; Data_Manager_Kill

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Data_Manager_eh @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO Data_Manager_eh, event


	WIDGET_CONTROL, event.id, GET_UVALUE = uval
	WIDGET_CONTROL, event.top, GET_UVALUE = info_ptr


;print,'Data_Manager_eh   ---   event.id uval = ',uval

	CASE 1 OF
		STRPOS(STRLOWCASE(uval),'wd') GE 0: BEGIN
			idx			= FIX((STR_SEP(uval,'_'))[1])
			(*info_ptr).wd_idx[idx]	= event.index
			END
		STRPOS(STRLOWCASE(uval),'displayredplane') GE 0: BEGIN
			(*info_ptr).display_red		= event.select
			END
		STRPOS(STRLOWCASE(uval),'displaygreenplane') GE 0: BEGIN
			(*info_ptr).display_green	= event.select
			END
		STRPOS(STRLOWCASE(uval),'displayblueplane') GE 0: BEGIN
			(*info_ptr).display_blue	= event.select
			END
		STRPOS(STRLOWCASE(uval),'applydismiss') GE 0: BEGIN
			IF TOTAL((*info_ptr).wd_idx[0:2]) LE 0 OR			$
			( (*info_ptr).display_red LE 0 AND				$
			(*info_ptr).display_green LE 0 AND				$
			(*info_ptr).display_blue LE 0) THEN BEGIN
				msg	= 'At least one RGB plane must be contain ' +	$
					  'data and be displayed!'
				res	= DIALOG_MESSAGE( msg, /ERROR )
				RETURN
			ENDIF
			WIDGET_CONTROL, event.top, /DESTROY
			END
		STRPOS(STRLOWCASE(uval),'apply') GE 0: BEGIN
			obj	= (*info_ptr).obj
			obj->SetDisplayRGB,						$
				[	(*info_ptr).display_red,			$
					(*info_ptr).display_green,			$
					(*info_ptr).display_blue ]
			obj->SetIdxRGB,							$
				[	(*info_ptr).wd_idx[0] - 1,			$
					(*info_ptr).wd_idx[1] - 1,			$
					(*info_ptr).wd_idx[2] - 1 ]
			IF N_ELEMENTS((*info_ptr).wd_idx) GT 3 THEN			$
					obj->SetIdxAncillary,				$
					(*info_ptr).wd_idx[3:3+N_ELEMENTS((*info_ptr).wd_idx) - 4] - 1

			IF obj->UpdateStretchDnInterface() THEN obj->DisplayData

			END
		STRPOS(STRLOWCASE(uval),'cancel') GE 0: BEGIN
			(*info_ptr).cancel_pressed	= 1
			WIDGET_CONTROL, event.top, /DESTROY
			END
		ELSE:
	ENDCASE


END
; Data_Manager_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::Data_Manager_Base @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::Data_Manager_Base


	tlb	= WIDGET_BASE( GROUP_LEADER = SELF.parentBase, /MODAL,			$
				/COLUMN, /BASE_ALIGN_CENTER, 				$
				KILL_NOTIFY = 'Data_Manager_Kill',			$
				TITLE = 'Data Manager Interface' )

	wd_list	= ['none', *(SELF.data_description_ptr)]
	wd_idx	= INTARR(SELF.max_datasets)

	FOR i = 0, SELF.max_datasets - 1 DO BEGIN
		CASE 1 OF
			i EQ 0: BEGIN
				wd_title	= 'Red Plane'
				wd_idx[i]	= SELF.redBandIdx + 1
				END
			i EQ 1: BEGIN
				wd_title	= 'Green Plane'
				wd_idx[i]	= SELF.grnBandIdx + 1
				END
			i EQ 2: BEGIN
				wd_title	= 'Blue Plane'
				wd_idx[i]	= SELF.bluBandIdx + 1
				END
			ELSE: BEGIN
				wd_title	= 'Ancillary Plane #'+STRTRIM(STRING(i-2),2)
				wd_idx[i]	= (*(SELF.ancillaryBandIdxPtr))[i-3] + 1
				END
		ENDCASE

		sub_tlb		= WIDGET_BASE(						$
						tlb,					$
						/ROW,					$
						/BASE_ALIGN_LEFT )

		wd		= WIDGET_DROPLIST( sub_tlb, 				$
					UVALUE = 'wd_' + STRTRIM(STRING(i),2),		$
					VALUE = wd_list,				$
					TITLE = wd_title,				$
					FONT = GetCorrectFont('courier2bold') )

		WIDGET_CONTROL, wd, SET_DROPLIST_SELECT = wd_idx[i]
	ENDFOR

	sub_base	= WIDGET_BASE( tlb, /ROW, /BASE_ALIGN_CENTER, /FRAME,		$
				/NONEXCLUSIVE )
	disp_r_btn	= WIDGET_BUTTON( sub_base, VALUE = 'Display Red Plane',		$
				UVALUE = 'displayredplane', FONT = GetCorrectFont('courier2bold') )
	WIDGET_CONTROL, disp_r_btn, SET_BUTTON = SELF.display_rgb[0]
	disp_g_btn	= WIDGET_BUTTON( sub_base, VALUE = 'Display Green Plane',	$
				UVALUE = 'displaygreenplane', FONT = GetCorrectFont('courier2bold') )
	WIDGET_CONTROL, disp_g_btn, SET_BUTTON = SELF.display_rgb[1]
	disp_b_btn	= WIDGET_BUTTON( sub_base, VALUE = 'Display Blue Plane',	$
				UVALUE = 'displayblueplane', FONT = GetCorrectFont('courier2bold') )
	WIDGET_CONTROL, disp_b_btn, SET_BUTTON = SELF.display_rgb[2]

	sub_base	= WIDGET_BASE( tlb, /ROW, /BASE_ALIGN_CENTER, /FRAME )
	ok		= WIDGET_BUTTON( sub_base, VALUE = 'Apply & Dismiss', UVALUE = 'applydismiss',	$
				FONT = GetCorrectFont('courier2bold') )
	apply		= WIDGET_BUTTON( sub_base, VALUE = 'Apply', UVALUE = 'apply', FONT = GetCorrectFont('courier2bold') )
	cancel		= WIDGET_BUTTON( sub_base, VALUE = 'Cancel', UVALUE = 'cancel', FONT = GetCorrectFont('courier2bold') )

	WIDGET_CONTROL, tlb, DEFAULT_BUTTON = ok
	WIDGET_CONTROL, tlb, /REALIZE

	info_ptr	= PTR_NEW( {							$
					display_red	:SELF.display_rgb[0],		$
					display_green	:SELF.display_rgb[1],		$
					display_blue	:SELF.display_rgb[2],		$
					wd_idx		:wd_idx,			$
					cancel_pressed	:0,				$
					obj		:SELF				$
					}, /NO_COPY )

	WIDGET_CONTROL, tlb, SET_UVALUE = info_ptr

	XMANAGER, 'Data Manager Base', tlb, EVENT_HANDLER = 'Data_Manager_eh'

	IF (*info_ptr).cancel_pressed THEN BEGIN
		PTR_FREE, info_ptr
		RETURN
	ENDIF

	SELF.display_rgb	= [	(*info_ptr).display_red,			$
					(*info_ptr).display_green,			$
					(*info_ptr).display_blue ]

	SELF.redBandIdx		= (*info_ptr).wd_idx[0] - 1
	SELF.grnBandIdx		= (*info_ptr).wd_idx[1] - 1
	SELF.bluBandIdx		= (*info_ptr).wd_idx[2] - 1

	FOR i = 0, N_ELEMENTS((*info_ptr).wd_idx) - 4 DO $
		(*(SELF.ancillaryBandIdxPtr))[i] = (*info_ptr).wd_idx[i+3] - 1

	PTR_FREE, info_ptr

	IF SELF->UpdateStretchDnInterface() THEN $
		SELF->DisplayData


END
; GEOREF_IMAGE::Data_Manager_Base

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::SetQueryValues @@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::SetQueryValues, float_base, value_strarr, LABEL_STRINGS = labels_strarr, UNIT_STRINGS = units_strarr
   WIDGET_CONTROL, float_base, GET_UVALUE = in_struct

   FOR i = 0, N_ELEMENTS(in_struct.value_labels)-1 DO BEGIN
      IF KEYWORD_SET(labels_strarr) THEN $
         WIDGET_CONTROL, in_struct.label_id[i], SET_VALUE = labels_strarr[i] $
      ELSE $
         WIDGET_CONTROL, in_struct.label_id[i], SET_VALUE = in_struct.labels_strarr[i]

      IF KEYWORD_SET(units_strarr) THEN $
         WIDGET_CONTROL, in_struct.unit_labels[i], SET_VALUE = units_strarr[i] $
      ELSE $
         WIDGET_CONTROL, in_struct.unit_labels[i], SET_VALUE = in_struct.units_strarr[i]

   ENDFOR

   FOR i = 0, N_ELEMENTS(in_struct.value_labels)-1 DO $
      WIDGET_CONTROL, in_struct.value_labels[i], SET_VALUE = value_strarr[i]
END
; GEOREF_IMAGE::SetQueryValues

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::RealizeQueryBase @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::RealizeQueryBase
   WIDGET_CONTROL, SELF.query_base, /REALIZE
END
; GEOREF_IMAGE::RealizeQueryBase

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::Set_ShowHideComWinFlag @@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::Set_ShowHideComWinFlag, flagValue
	; Don't do this if GEOREF_OBJ is being killed.
	IF WIDGET_INFO( SELF.showHideComWinBut, /VALID_ID ) THEN BEGIN
		SELF.showHideComWinFlag = flagValue
		IF flagValue EQ 0 THEN BEGIN
			WIDGET_CONTROL, SELF.showHideComWinBut, SET_VALUE = 'Show Companion Window'
		ENDIF ELSE BEGIN
			WIDGET_CONTROL, SELF.showHideComWinBut, SET_VALUE = 'Hide Companion Window'
		ENDELSE
	ENDIF
END
;GEOREF_IMAGE::Set_ShowHideComWinFlag

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ FloatingQueryBaseKillNotify @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO FloatingQueryBaseKillNotify, tlb
	WIDGET_CONTROL, tlb, GET_UVALUE = struct
	struct.georef_image_obj -> Set_ShowHideComWinFlag, 0
END
; FloatingQueryBaseKillNotify

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::ReturnFloatingQueryBase @@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::ReturnFloatingQueryBase, group_leader, n_datasets, $
   LABELS_STRARR = labels_strarr, $
   UNITS_STRARR = units_strarr, WINDOW_TITLE = window_title, $
   ROW = row, COLUMN = column, MAP = map

   IF NOT KEYWORD_SET(map) THEN map = 0

   IF NOT KEYWORD_SET(row) THEN column = 1
   IF NOT KEYWORD_SET(window_title) THEN window_title = 'query window'

   IF NOT KEYWORD_SET(labels_strarr) THEN BEGIN
      labels_strarr = STRARR(n_datasets)
      FOR i = 0, n_datasets-1 DO labels_strarr[i] = 'Dataset #' + STRTRIM(STRING(i+1),2)
   ENDIF

   IF NOT KEYWORD_SET(units_strarr) THEN units_strarr = STRARR(n_datasets)

;   float_base = WIDGET_BASE( GROUP_LEADER = group_leader, /BASE_ALIGN_LEFT, ROW = row, COLUMN = column, $
;      TITLE = window_title, /FLOATING, MAP=map )
   float_base = WIDGET_BASE( GROUP_LEADER = group_leader, /BASE_ALIGN_LEFT, ROW = row, COLUMN = column, $
      TITLE = window_title, MAP=map, KILL_NOTIFY = 'FloatingQueryBaseKillNotify' )

   label_id     = LONARR(n_datasets)
   value_labels = label_id
   unit_labels  = value_labels
   FOR i = 0, n_datasets-1 DO BEGIN
      label_base      = widget_base(float_base,/row)
      label_id[i]     = widget_label(label_base,value=labels_strarr[i], /dynamic_resize)
      value_labels[i] = widget_label(label_base,value = '', /dynamic_resize)
      unit_labels[i]  = widget_label(label_base,value = units_strarr[i], /dynamic_resize)
   ENDFOR

;   widget_control, float_base, /realize

   widget_control, float_base, set_uvalue = {			$
				label_id	:label_id,	$
				value_labels	:value_labels,	$
				unit_labels	:unit_labels,	$
				labels_strarr	:labels_strarr,	$
				units_strarr	:units_strarr,	$
				georef_image_obj:SELF	}

   return, float_base

END
; GEOREF_IMAGE::ReturnFloatingQueryBase

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::EightBit_GetColorVectorPtr @@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::EightBit_GetColorVectorPtr, RED = r, GRN = g, BLU = b

   CASE 1 OF
      KEYWORD_SET(r) AND PTR_VALID(SELF.red_vec_ptr): ret_vec_ptr = SELF.red_vec_ptr
      KEYWORD_SET(g) AND PTR_VALID(SELF.grn_vec_ptr): ret_vec_ptr = SELF.grn_vec_ptr
      KEYWORD_SET(b) AND PTR_VALID(SELF.blu_vec_ptr): ret_vec_ptr = SELF.blu_vec_ptr
      ELSE: ret_vec_ptr = PTR_NEW()
   ENDCASE

;print,'fetching ret_vec_ptr:  ',*ret_vec_ptr
   RETURN, ret_vec_ptr
END
; GEOREF_IMAGE::EightBit_GetColorVectorPtr

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::EightBit_SetColorVectorPtr @@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::EightBit_SetColorVectorPtr, RED = r, GRN = g, BLU = b
   CASE 1 OF
      KEYWORD_SET(r): BEGIN
         IF PTR_VALID(SELF.red_vec_ptr) THEN PTR_FREE, SELF.red_vec_ptr
;print,'GEOREF_IMAGE::EightBit_SetColorVectorPtr -- setting SELF.red_vec_ptr = ',r
         SELF.red_vec_ptr = PTR_NEW(r, /NO_COPY)
         END
       KEYWORD_SET(g): BEGIN
         IF PTR_VALID(SELF.grn_vec_ptr) THEN PTR_FREE, SELF.grn_vec_ptr
;print,'GEOREF_IMAGE::EightBit_SetColorVectorPtr -- setting SELF.grn_vec_ptr = ',g
         SELF.grn_vec_ptr = PTR_NEW(g, /NO_COPY)
         END
      KEYWORD_SET(b): BEGIN
         IF PTR_VALID(SELF.blu_vec_ptr) THEN PTR_FREE, SELF.blu_vec_ptr
;print,'GEOREF_IMAGE::EightBit_SetColorVectorPtr -- setting SELF.blu_vec_ptr = ',b
         SELF.blu_vec_ptr = PTR_NEW(b, /NO_COPY)
         END
     ELSE:
   ENDCASE
END
; GEOREF_IMAGE::EightBit_SetColorVectorPtr

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::EightBit_SetTranslationPtr @@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::EightBit_SetTranslationPtr, transVec
   IF PTR_VALID(SELF.trans_vec_ptr) THEN PTR_FREE, SELF.trans_vec_ptr
   SELF.trans_vec_ptr = PTR_NEW(transVec, /NO_COPY)
END
; GEOREF_IMAGE::EightBit_SetTranslationPtr

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::EightBit_GetTranslationPtr @@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::EightBit_GetTranslationPtr
      RETURN, SELF.trans_vec_ptr
END
; GEOREF_IMAGE::EightBit_GetTranslationPtr

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::GetParentBase @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::GetParentBase
      RETURN, SELF.parentBase
END
; GEOREF_IMAGE::GetParentBase

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::Set_Cursor_XY @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::Set_Cursor_XY, x, y
	SELF.cursor_x = x
	SELF.cursor_y = y
END
; GEOREF_IMAGE::Set_Cursor_XY

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::Set_histogramWidgetDrawIds @@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::Set_histogramWidgetDrawIds, histogramWidgetDrawIds
	SELF.histogramWidgetDrawIds = histogramWidgetDrawIds
END
; GEOREF_IMAGE::Set_histogramWidgetDrawIdsf

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::Set_histogramPixmapId @@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::Set_histogramPixmapId, histogramPixmapId
	SELF.histogramPixmapId = histogramPixmapId
END
; GEOREF_IMAGE::Set_histogramPixmapId

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::Calculate_histBinSize @@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::Calculate_histBinSize, bandIdx, FULL_HIST = full_hist



	binsize = 1

	IF KEYWORD_SET(full_hist) THEN BEGIN
		max_val	= DOUBLE( *((*(SELF.max_dataPtr))[bandIdx]) )
		min_val	= DOUBLE( *((*(SELF.min_dataPtr))[bandIdx]) )
	ENDIF ELSE BEGIN
		max_val	= DOUBLE( *((*(SELF.dataMaxValPtr))[bandIdx]) )
		min_val	= DOUBLE( *((*(SELF.dataMinValPtr))[bandIdx]) )
	ENDELSE

	spread	= max_val - min_val
	typ	= SIZE( *((*(SELF.dataMaxValPtr))[bandIdx]), /TYPE )

	IF typ EQ 2 OR typ EQ 3 THEN binsize = 1
	; Histograms for floating point data can be improved by optimizing the binsize.
	; In this case, the binsize is restrained to powers of 10.  Alternatively, the
	; binsize could be set to, say for example, 1/1000 of the spread, but then the
	; binsize would be a potentially confusing arbitrary value.  Thus, powers of 10
	; are here used as a more straightforward approach; the disadvantage of this is
	; that the NUMBER of bins becomes arbitrary.  In any case, an excess number of
	; bins are needed for when the sliders positioned close together thereby effectively
	; "zooming" into the histogram plot.
	IF typ EQ 4 OR typ EQ 5 THEN BEGIN
		IF spread LT 10000000000.0 THEN binsize = 1000000.0
		IF spread LT 1000000000.0 THEN binsize = 100000.0
		IF spread LT 100000000.0 THEN binsize = 10000.0
		IF spread LT 10000000.0 THEN binsize = 1000.0
		IF spread LT 1000000.0 THEN binsize = 100.0
		IF spread LT 100000.0 THEN binsize = 10.0
		IF spread LT 10000.0 THEN binsize = 1.0
		IF spread LT 1000.0 THEN binsize = 0.1
		IF spread LT 100.0 THEN binsize = 0.01
		IF spread LT 10.0 THEN binsize = 0.001
		IF spread LT 1.0 THEN binsize = 0.0001
		IF spread LT 0.1 THEN binsize = 0.00001
		IF spread LT 0.01 THEN binsize = 0.000001
		IF spread LT 0.001 THEN binsize = 0.0000001
		IF spread LT 0.0001 THEN binsize = 0.00000001
		IF spread LT 0.00001 THEN binsize = 0.000000001
		IF spread LT 0.000001 THEN binsize = 0.0000000001
		IF spread LT 0.0000001 THEN binsize = 0.00000000001
		IF spread LT 0.00000001 THEN binsize = 0.000000000001
		IF spread LT 0.000000001 THEN binsize = 0.0000000000001
		IF spread LT 0.0000000001 THEN binsize = 0.00000000000001
	ENDIF

;;;ckt,feb2000
;;;added code to handle bin sizes of less than 1 and data types being integer
;;;without this code, a bin size of less than 1 is converted to 0 ID the data
;;; type is an integer value, thus throwing an "illegal binsize" error.
;;;Although data type was originally checked ABOVE in this routine, perhaps
;;the original data type is lost somewhere, so we will check the data type of the original
;;; data below.

IF binsize LT 1.0 THEN BEGIN
	IDL_FLOATING_TYPE_CODES = [ 4, 5, 6, 9 ] ; includes complex float and complex double type codes.
	data_type_code		= (*((*(SELF.dataPtr))[bandIdx]))->GetNumberType()
	type_idx		= WHERE( IDL_FLOATING_TYPE_CODES EQ data_type_code, type_cnt )
	IF type_cnt LE 0 THEN binsize = 1.0
ENDIF

	PTR_FREE, (*(SELF.histBinSize))[bandIdx]
	(*(SELF.histBinSize))[bandIdx] = PTR_NEW(binsize)


	RETURN, *((*(SELF.histBinSize))[bandIdx])
END
; GEOREF_IMAGE::Calculate_histBinSize

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::Get_x_tickv_values_ptr @@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::Get_x_tickv_values_ptr, axis, index, value

	RETURN, SELF.x_tickv_values_ptr

END
; GEOREF_IMAGE::Get_x_tickv_values_ptr

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ x_tickv_callback @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION x_tickv_callback, axis, index, value

	COMMON GEOREFIMAGE, linkObj, nullObj, linkMsg, nObj, idArr, commonblock_pseudo_color_table_ptrarr, commonblock_self

	x_tickv_values_ptr	= commonblock_self -> Get_x_tickv_values_ptr()
	string			= STRTRIM( (*x_tickv_values_ptr)[index], 2 )

	; Zero stripping code cut'n'pasted from AIRS Vendaval routine number2string.pro.
	; Strip trailing decimal zeroes from end of a string that:
	; A)  contains exactly one period (.) indicating that the string represents a decimal and
	; B)  does not contain an "E" as the fourth character from the end indicating that it would be in scientific notation.
	; C)  The string is assumed to represent a valid number by the time it gets to this part of the program.
	IF STRPOS( string, '.' ) GE 0 THEN								$	; Check for a period.
		IF STRPOS( string, '.', STRPOS( string, '.' ) + 1 ) EQ (-1) THEN			$	; Check for a second period
			IF STRPOS( string, 'E' ) EQ (-1) THEN						$	; Check for scientific notation.
				FOR i = 0, STRLEN( (STR_SEP( string, '.' ))[1] ) - 1 DO			$	; Loop through all characters following the period.
					IF STRMID( string, STRLEN( string ) - 1, 1 ) EQ '0' THEN 	$	; Check for a trailing zero.
						string = STRMID( string, 0, STRLEN( string ) - 1 )		; Remove the trailing zero.

	; If the final character is a period, then remove it.
	IF STRPOS( string, '.' ) EQ STRLEN( string ) - 1 THEN	$
		string = STRMID( string, 0, STRLEN( string ) - 1 )

	RETURN, string
END
; x_tickv_callback

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::stretchDnPlotHistogram @@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::stretchDnPlotHistogram, whichPlot, NEWHIST = newhist ;keyword obsolete?  probably.

	COMMON GEOREFIMAGE, linkObj, nullObj, linkMsg, nObj, idArr, commonblock_pseudo_color_table_ptrarr, commonblock_self

	WIDGET_CONTROL, SELF.histogramWidgetDrawIds[whichPlot], GET_VALUE = plotWindowID

	curWin			= !D.WINDOW

	;---------------------------------------------------------------------------------------------------------
	; Do the RGB histogram windows.  All three (or two) histograms are drawn, but only one histogram/viewplane
	; combination is active depending on which button was pressed.
	;---------------------------------------------------------------------------------------------------------

	;----------------------------------------------------------------
	; These start/end x/y position values are for the display value
	; histogram plot where the actual histograms are plotted.  There
	; are other position values for the individual R,G,B y axes.  The
	; x axis values are the same in both cases.
	;----------------------------------------------------------------
	hist_x_start_pos	= 0.2
	hist_x_end_pos		= 0.95
	hist_y_start_pos	= 0.4
	hist_y_end_pos		= 0.9
	IF whichPlot LE 2 THEN hist_y_start_pos= 0.2	; If individual R, G, or B plot.

	;---------------------------------------------------------------------------------------------------------
	; To minimize lines of code needed to accomplish having the active histogram draw last (on top), a loop is
	; used to call the various plotting commands.  The "order" variable controls the order in which the
	; histograms are drawn, and associates all the necessary variables with the corresponding histograms.  The
	; histogram corresponding to the last number in the "order" array is plotted last (on top).  "whichPlot"
	; determines the active viewplane, whereas "order" determines which histogram is active in that viewplane.
	; "Active" means the sliders are associated with that viewplane and histogram.  The other histograms(s)
	; are passively drawn underneath for viewing reference.
	;---------------------------------------------------------------------------------------------------------
	;---------------------------------------------------------------------
	; setupHist determines the y-axis for each individual plot, and for
	; the plot of the R,G,B combined plot (active viewplane).
	; Also, determine the background color (black means whichPlot is not
	; one of the 6 possible choices indicating something is wrong).
	;---------------------------------------------------------------------

	rgb			= [0L,0L,0L]

	colormap_idx_start = 0

;print,''
;help,SELF.rgbHistogramPtrarr
;help,SELF.rgbHistogramPtrarr[0]
;help,*(SELF.rgbHistogramPtrarr[0])
	CASE 1 OF
		whichPlot EQ 0 OR whichPlot EQ 3 : BEGIN
			setupHist	= CONGRID( *(SELF.rgbHistogramPtrarr[0]), 256 )
			order		= [1,2,0]	; RED viewplane histogram is active.
			IF SELF.nChannels EQ 1 THEN	$
				colormap_idx_start = 0
		END
		whichPlot EQ 1 OR whichPlot EQ 4 : BEGIN
			setupHist	= CONGRID( *(SELF.rgbHistogramPtrarr[1]), 256 )
			order		= [2,0,1]	; GRN viewplane histogram is active.
			IF SELF.nChannels EQ 1 THEN	$
				colormap_idx_start = 4
		END
		whichPlot EQ 2 OR whichPlot EQ 5 : BEGIN
			setupHist	= CONGRID( *(SELF.rgbHistogramPtrarr[2]), 256 )
			order		= [0,1,2]	; BLU viewplane histogram is active.
			IF SELF.nChannels EQ 1 THEN	$
				colormap_idx_start = 8
		END
	ENDCASE

	bandIdx = [ SELF.redBandIdx, SELF.grnBandIdx, SELF.bluBandIdx ]	; Mapping of bands to viewplanes.
	histValid = [	PTR_VALID(SELF.rgbHistogramPtrarr[0]), $	; Which histograms exist.
			PTR_VALID(SELF.rgbHistogramPtrarr[1]), $
			PTR_VALID(SELF.rgbHistogramPtrarr[2]) ]
	y_start_pos		= [0.3,0.2,0.1]				; R, G, B x axis vertical position.
	y_end_pos		= [0.8,0.7,0.6]				; R, G, B x axis vertical position.
	IF whichPlot LE 2 THEN BEGIN					; If individual R, G, or B plot.
		y_start_pos	= [0.1,0.1,0.1]				; R, G, B x axis vertical position.
		y_end_pos	= [0.6,0.6,0.6]				; R, G, B x axis vertical position.
	ENDIF
	color		= ['RED','GREEN','BLUE']			; Color names for x axis title.
	rgbPrimary		= [255L,191L,191L]			; Active histogram color (default = RED).
	rgbSecondary		= [127L,63L,63L]			; Passive histograms color (default = RED).
	rgbBackColor		= [63L,0L,0L]
	rgbDrawColor		= [255L,223L,223L]

	;---------------------------------------------------------------------
	; Set the color index for background and draw for the setup plot axes.
	;---------------------------------------------------------------------
	backColor		= rgbBackColor[order[2]]+256*(rgbBackColor[order[1]]+256*rgbBackColor[order[0]])
	drawColor		= rgbDrawColor[order[2]]+256*(rgbDrawColor[order[1]]+256*rgbDrawColor[order[0]])

	IF SELF.nChannels EQ 1 THEN BEGIN
		;R primary, secondary, back, draw (indices 0-3)
		;G primary, secondary, back, draw (indices 4-7)
		;B primary, secondary, back, draw (indices 8-11)

		backColor	= colormap_idx_start+2
		drawColor	= colormap_idx_start+3
	ENDIF

	;---------------------------------------------------------------------
	; Set character size for all plots.
	;---------------------------------------------------------------------
	cSize			= 0.7		; SGI
	IF !version.os eq 'sunos' THEN $	; Sun
		cSize		= 1.0

	;---------------------------------------------------------------------
	; Do the plot setup, this draw the x/y axes for
	;	x axis = viewplane dn mapping,
	;	y axis = pixel count for data in active viewplane (R, G, or B)
	; but don't plot the data, just draw the axes in white.
	;---------------------------------------------------------------------
	WSET, plotWindowID
	PLOT, setupHist,/NODATA,XSTYLE=1,YSTYLE=1, $
		POSITION=[ hist_x_start_pos, hist_y_start_pos, hist_x_end_pos, hist_y_end_pos ], $
		XTITLE='Display Value',YTITLE=color[order[2]]+' Viewplane Data Pixel Count',TITLE='HISTOGRAM',CHARSIZE=cSize, $
		BACKGROUND=backColor, COLOR=drawColor

	starthere		= 0				; Combined R, G, and B plots.
	IF whichPlot LE 2 THEN $
		starthere	= 2				; Individual R, G, or B plot.

	FOR i = starthere,2 DO BEGIN

		IF bandIdx[order[i]] GE 0 AND histValid[order[i]] THEN BEGIN

			;-----------------------------------------------------
			; Download gamma and convert to exponent (inverse).
			;-----------------------------------------------------
			exponent	= 1.0 / *((*(SELF.stretchDnGammaPtr))[bandIdx[order[i]]])

			;-----------------------------------------------------
			; Download Minimum Value and Maximum Value values.
			;-----------------------------------------------------
			minVal2Byte	= *((*(SELF.stretchDnBottomPtr))[bandIdx[order[i]]])
			maxVal2Byte	= *((*(SELF.stretchDnTopPtr))[bandIdx[order[i]]])

			;-----------------------------------------------------
			; Select the secondary (passive) histogram color, red,
			; green or blue.
			;-----------------------------------------------------
			rgb = [	rgbSecondary[order[i]], $
				rgbSecondary[order[(i+2) mod 3]], $
				rgbSecondary[order[(i+1) mod 3]]]

			;-----------------------------------------------------
			; Select the primary (active) histogram color.
			;-----------------------------------------------------
			IF i EQ 2 THEN $
				rgb = [	rgbPrimary[order[i]], $
					rgbPrimary[order[(i+2) mod 3]], $
					rgbPrimary[order[(i+1) mod 3]]]
			drawColor = rgb[0]+256*(rgb[1]+256*rgb[2])

			IF SELF.nChannels EQ 1 THEN BEGIN

				IF PTR_VALID(SELF.red_vec_ptr) $
				AND PTR_VALID(SELF.grn_vec_ptr) $
				AND PTR_VALID(SELF.blu_vec_ptr) THEN BEGIN
					tmpidx = WHERE(*(SELF.red_vec_ptr) EQ rgb[0] AND $
						*(SELF.grn_vec_ptr) EQ rgb[1] AND $
						*(SELF.blu_vec_ptr) EQ rgb[2], rgb_cnt)
					IF rgb_cnt GT 0 THEN drawColor = tmpidx[0]
				ENDIF ELSE BEGIN
				        rrr = $
					                                ;R primary, secondary, back, draw (indices 0-3)
 				                                       [    255,      127,     63,   255,     $
 				                                            191,       63,      0,   223,     $
				                                             191,       63,      0,   223 ]

				        ggg = $
 				                                       [    191,       63,      0,   223,     $
					                                ;G primary, secondary, back, draw (indices 4-7)
 				                                            255,      127,     63,   255,     $
 				                                            191,       63,      0,   223 ]

 				        bbb = $
 				                                       [    191,       63,      0,   223,     $
				                                             191,       63,      0,   223,     $
					                                ;B primary, secondary, back, draw (indices 8-11)
				                                             255,      127,     63,   255 ]
					tmpidx = WHERE(rrr EQ rgb[0] AND $
						ggg EQ rgb[1] AND $
						bbb EQ rgb[2], rgb_cnt)
					IF rgb_cnt GT 0 THEN drawColor = tmpidx[0]
				ENDELSE
			ENDIF

			;-----------------------------------------------------
			; Calculate the x axis (nonlinear when gamma NE 1.0).
			; x_ticks = Number of INTERVALS between ticks.
			; x_tickv = Labels for each of the tick marks.
			;-----------------------------------------------------
			spread		= DOUBLE( *((*(SELF.stretchDnTopPtr))[bandIdx[order[i]]]) )		$
						-  DOUBLE( *((*(SELF.stretchDnBottomPtr))[bandIdx[order[i]]]) )
;print
;print,'----------- help, *((*(SELF.histBinSize))[bandIdx[order[i]]])'
;help, *((*(SELF.histBinSize))[bandIdx[order[i]]])
;print,'spread = ',spread
;print,'----------- help, *((*(SELF.stretchDnTopPtr))[bandIdx[order[i]]])'
;help, *((*(SELF.stretchDnTopPtr))[bandIdx[order[i]]])
;print,'----------- help, *((*(SELF.stretchDnBottomPtr))[bandIdx[order[i]]])'
;help, *((*(SELF.stretchDnBottomPtr))[bandIdx[order[i]]])
;print
;help,spread
;help,*((*(SELF.histBinSize))[bandIdx[order[i]]])
;help,(*(SELF.histBinSize))[bandIdx[order[i]]]
;help,*(SELF.histBinSize)
;print,'*((*(SELF.histBinSize))[bandIdx[order[i]]]) = ',*((*(SELF.histBinSize))[bandIdx[order[i]]])
;print,'(*(SELF.histBinSize))[bandIdx[order[i]]] = ',(*(SELF.histBinSize))[bandIdx[order[i]]]
;print,'*(SELF.histBinSize) = ',*(SELF.histBinSize)

			x_axis_len	= MAX( [ ROUND( spread / *((*(SELF.histBinSize))[bandIdx[order[i]]]) ), 1 ] )
;help,spread
;help,*((*(SELF.histBinSize))[bandIdx[order[i]]])
;print,'*((*(SELF.histBinSize))[bandIdx[order[i]]]) = ',*((*(SELF.histBinSize))[bandIdx[order[i]]])
;help,x_axis_len

			x_axis		= DINDGEN( x_axis_len )						$
						* *((*(SELF.histBinSize))[bandIdx[order[i]]])		$
						+ *((*(SELF.stretchDnBottomPtr))[bandIdx[order[i]]])

			x_axis_min_idx	= (WHERE( x_axis GE minVal2Byte, cnt ))[0]

			IF x_axis[x_axis_min_idx] NE minVal2Byte AND x_axis_min_idx NE 0 THEN $
				x_axis_min_idx = x_axis_min_idx - 1

			x_axis_max_idx	= WHERE( x_axis LE maxVal2Byte, count )

;print
;print,'*((*(SELF.stretchDnTopPtr))[bandIdx[order[i]]]) = ',*((*(SELF.stretchDnTopPtr))[bandIdx[order[i]]])
;print,'*((*(SELF.stretchDnBottomPtr))[bandIdx[order[i]]]) = ',*((*(SELF.stretchDnBottomPtr))[bandIdx[order[i]]])
;print,'x_axis = ',x_axis
;print,'maxVal2Byte = ',maxval2byte
;print,'count = ',count
;print,'x_axis LE maxVal2Byte = ',x_axis LE maxVal2Byte
;help,x_axis,maxval2byte,count
			IF count EQ 0 THEN BEGIN
				result = DIALOG_MESSAGE( [								$
					'',										$
					'NON-FATAL ERROR, try re-adjusting the histogram sliders and/or limits.',	$
					'',										$
					'',										$
					'PLEASE NOTIFY THE PROGRAMMER(S)',						$
					'',										$
					'note to programmers:  this shouldve been caught in UpdateStretchDnInterface',	$
					'',										$
					'',										$
					'Diagnostics:',									$
					'',										$
					'Data values [ MIN, MAX ] = ['							$
					+ STRTRIM( DOUBLE( *((*(SELF.dataMinValPtr))[bandIdx[order[i]]]) ), 2 ) + ','	$
					+ STRTRIM( DOUBLE( *((*(SELF.dataMaxValPtr))[bandIdx[order[i]]]) ), 2 ) + ']',	$
					'',										$
					'Recall values [ minVal2Byte, maxVal2Byte ] = ['				$
					+ STRTRIM( DOUBLE( minVal2Byte ), 2 ) + ','					$
					+ STRTRIM( DOUBLE( maxVal2Byte ), 2 ) + ']',					$
					'',										$
					'RETURNing' ],									$
					/INFORMATION )

;PRINT,'_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_'
;PRINT,'whichPlot = ',whichPlot
;PRINT,'i = ',i
;PRINT,'order[i] = ',order[i]
;PRINT,'order[*] = ',order[*]
;PRINT,'bandIdx[order[i]] = ',bandIdx[order[i]]
;PRINT,'*((*(SELF.dataMinValPtr))[bandIdx[order[i]]]) = ',*((*(SELF.dataMinValPtr))[bandIdx[order[i]]])
;PRINT,'*((*(SELF.dataMaxValPtr))[bandIdx[order[i]]]) = ',*((*(SELF.dataMaxValPtr))[bandIdx[order[i]]])
;HELP,*((*(SELF.dataMinValPtr))[bandIdx[order[i]]])
;HELP,*((*(SELF.dataMaxValPtr))[bandIdx[order[i]]])
;PRINT,'*((*(SELF.stretchDnBottomPtr))[bandIdx[order[i]]]) = ',*((*(SELF.stretchDnBottomPtr))[bandIdx[order[i]]])
;PRINT,'*((*(SELF.stretchDnTopPtr))[bandIdx[order[i]]]) = ',*((*(SELF.stretchDnTopPtr))[bandIdx[order[i]]])
;HELP,*((*(SELF.stretchDnBottomPtr))[bandIdx[order[i]]])
;HELP,*((*(SELF.stretchDnTopPtr))[bandIdx[order[i]]])
;HELP,bandIdx
;FOR i = 0, N_ELEMENTS( bandIdx ) - 1 DO BEGIN
; PRINT,'>>>>> FORLOOP:  i=',i
; PRINT,'*((*(SELF.stretchDnBottomPtr))[bandIdx[order[i]]]) = ',*((*(SELF.stretchDnBottomPtr))[bandIdx[order[i]]])
; PRINT,'*((*(SELF.stretchDnTopPtr))[bandIdx[order[i]]]) = ',*((*(SELF.stretchDnTopPtr))[bandIdx[order[i]]])
;ENDFOR
;PRINT,'minVal2Byte = ',minVal2Byte
;PRINT,'maxVal2Byte = ',maxVal2Byte
;HELP,minVal2Byte
;HELP,maxVal2Byte
;IF SELF.REDBandIdx GE 0 THEN BEGIN
;	IF PTR_VALID( ((*(SELF.stretchDnBottomPtr))[SELF.redBandIdx]) ) THEN BEGIN
;		PRINT,'*((*(SELF.stretchDnBottomPtr))[SELF.redBandIdx]) = ',*((*(SELF.stretchDnBottomPtr))[SELF.redBandIdx])
;		PRINT,'*((*(SELF.stretchDnTopPtr))[SELF.redBandIdx]) = ',*((*(SELF.stretchDnTopPtr))[SELF.redBandIdx])
;		HELP,*((*(SELF.stretchDnBottomPtr))[SELF.redBandIdx])
;		HELP,*((*(SELF.stretchDnTopPtr))[SELF.redBandIdx])
;	ENDIF
;ENDIF
;IF SELF.grnBandIdx GE 0 THEN BEGIN
;	IF PTR_VALID( ((*(SELF.stretchDnBottomPtr))[SELF.grnBandIdx]) ) THEN BEGIN
;		PRINT,'*((*(SELF.stretchDnBottomPtr))[SELF.grnBandIdx]) = ',*((*(SELF.stretchDnBottomPtr))[SELF.grnBandIdx])
;		PRINT,'*((*(SELF.stretchDnTopPtr))[SELF.grnBandIdx]) = ',*((*(SELF.stretchDnTopPtr))[SELF.grnBandIdx])
;		HELP,*((*(SELF.stretchDnBottomPtr))[SELF.grnBandIdx])
;		HELP,*((*(SELF.stretchDnTopPtr))[SELF.grnBandIdx])
;	ENDIF
;ENDIF
;IF SELF.bluBandIdx GE 0 THEN BEGIN
;	IF PTR_VALID( ((*(SELF.stretchDnBottomPtr))[SELF.bluBandIdx]) ) THEN BEGIN
;		PRINT,'*((*(SELF.stretchDnBottomPtr))[SELF.bluBandIdx]) = ',*((*(SELF.stretchDnBottomPtr))[SELF.bluBandIdx])
;		PRINT,'*((*(SELF.stretchDnTopPtr))[SELF.bluBandIdx]) = ',*((*(SELF.stretchDnTopPtr))[SELF.bluBandIdx])
;		HELP,*((*(SELF.stretchDnBottomPtr))[SELF.bluBandIdx])
;		HELP,*((*(SELF.stretchDnTopPtr))[SELF.bluBandIdx])
;	ENDIF
;ENDIF
;PRINT,'_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_'

				RETURN
			ENDIF

			x_axis_max_idx				= x_axis_max_idx[count-1]

			;------------------------------------------------------------------
			; x_ticks = INTERVALS = number of tick marks - 1.
			; Save the unstretched values for x_ticks_callback routine.
			;------------------------------------------------------------------
			x_ticks			= 5
			SELF.x_tickv_values_ptr	= PTR_NEW( CONGRID( x_axis, x_ticks + 1 ) )

			IF x_axis[x_axis_max_idx] NE maxVal2Byte AND x_axis_max_idx NE N_ELEMENTS(x_axis)-1 THEN $
				x_axis_max_idx				= x_axis_max_idx + 1

			IF x_axis[x_axis_max_idx] - x_axis[x_axis_min_idx] EQ 0 THEN BEGIN
				x_axis					= x_axis - minVal2Byte
				x_axis[x_axis_min_idx:x_axis_max_idx]	= x_axis[x_axis_min_idx:x_axis_max_idx] ^ exponent
				x_axis					= x_axis + minVal2Byte
			ENDIF ELSE BEGIN
				x_axis					= x_axis - minVal2Byte
				spread					= x_axis[x_axis_max_idx] - x_axis[x_axis_min_idx]
				x_axis					= x_axis / spread
				x_axis[x_axis_min_idx]			= MAX([x_axis[x_axis_min_idx],0])
				x_axis[x_axis_min_idx:x_axis_max_idx]	= x_axis[x_axis_min_idx:x_axis_max_idx] ^ exponent * spread
				x_axis					= x_axis + minVal2Byte
			ENDELSE

			;------------------------------------------------------------------
			; Save the stretched values for x axis tick spacing.
			;------------------------------------------------------------------
			x_tickv			= CONGRID( x_axis, x_ticks + 1 )

			;------------------------------------------------------------------
			; calculate intersection of histogram vs. currently displayed data.
			;------------------------------------------------------------------
			IF *(SELF.calc_hist_for_displayed_data_ptr) THEN BEGIN
				hist_min = DOUBLE( *((*(SELF.dataMinValPtr))[bandIdx[order[i]]]) )
				hist_max = DOUBLE( *((*(SELF.dataMaxValPtr))[bandIdx[order[i]]]) )
			ENDIF ELSE BEGIN
				hist_min = DOUBLE( *((*(SELF.min_dataPtr))[bandIdx[order[i]]]) )
				hist_max = DOUBLE( *((*(SELF.max_dataPtr))[bandIdx[order[i]]]) )
			ENDELSE

			axis_min = DOUBLE( x_axis[x_axis_min_idx] )
			axis_max = DOUBLE( x_axis[x_axis_max_idx] )
			binsize = *((*(SELF.histBinSize))[bandIdx[order[i]]])
;;;print,'|||||||||| 	binsize	=',binsize
			hist = *(SELF.rgbHistogramPtrarr[order[i]])

			CASE 1 OF
				axis_min LT ( hist_min - binsize ) : BEGIN
					; Pad the hist array with zeroes to push the plot out along the xaxis.
					low_pad = LON64ARR( ROUND( ( hist_min - axis_min ) / binsize ) )
					hist = [ low_pad, hist ]
				END
				axis_min GE ( hist_min + binsize ) : BEGIN
					; Index into hist to truncate the lower end.
					low_idx = ROUND( ( axis_min - hist_min ) / binsize )
					low_idx = MIN( [ MAX( [ 0, low_idx ] ), N_ELEMENTS( hist ) - 1 ] )	; Constrain index.
					hist = hist[ low_idx : N_ELEMENTS( hist ) - 1 ]

					; Special case when all data falls below bottomSlider.
					IF axis_min GT ( hist_max + binsize ) THEN $
						hist = hist * 0
				END
				ELSE : BEGIN
				END
			ENDCASE
			CASE 1 OF
				axis_max GT ( hist_max + binsize ) : BEGIN
					; Pad the hist array with zeroes.
					high_pad = LONARR( ROUND( ( axis_max - hist_max ) / binsize ) )
					hist = [ hist, high_pad ]
				END
				axis_max LE ( hist_max - binsize ) : BEGIN
					; Index into hist to truncate the high end.
					high_idx = ( N_ELEMENTS( hist ) - 1 ) - ROUND( ( hist_max - axis_max ) / binsize )
					high_idx = MIN( [ MAX( [ 0, high_idx ] ), N_ELEMENTS( hist ) - 1 ] )	; Constrain index.
					hist = hist[ 0 : high_idx ]
				END
				ELSE : BEGIN
				END
			ENDCASE
			IF N_ELEMENTS( hist ) GT ( x_axis_max_idx - x_axis_min_idx + 1 ) THEN $
				hist = hist[ 0 : x_axis_max_idx - x_axis_min_idx ]


			;-----------------------------------------------------
			; Do setup of plot axis and location, but draw to
			; offscreen pixmap.
			;-----------------------------------------------------
			WSET, SELF.histogramPixmapId
			PLOT, x_axis[x_axis_min_idx:x_axis_max_idx], hist, $
				POSITION=[ hist_x_start_pos, y_start_pos[order[i]], hist_x_end_pos, y_end_pos[order[i]] ], $
				/NODATA, /NOERASE, XSTYLE=1, YSTYLE=4

			;-----------------------------------------------------
			; Using initialized plot axis and location setup, draw
			; the individual data value x axis to the plot window.
			;-----------------------------------------------------
			WSET, plotWindowID
			commonblock_self = SELF
			AXIS, 0, 0, XAX=0, /DATA, COLOR=drawColor, CHARSIZE=cSize, XSTYLE=1, $
				XTITLE=color[order[i]]+' Viewplane Data Value', XTICKV=x_tickv, XTICKS=x_ticks, XTICKFORMAT = 'x_tickv_callback'

			;-----------------------------------------------------
			; Do plot setup but draw to off-screen pixmap.
			;-----------------------------------------------------
			WSET, SELF.histogramPixmapId
			PLOT, x_axis[x_axis_min_idx:x_axis_max_idx], hist, $
				POSITION=[ hist_x_start_pos, hist_y_start_pos, hist_x_end_pos, hist_y_end_pos ], $
				/NODATA, /NOERASE, XSTYLE=1, YSTYLE=4

			;-----------------------------------------------------
			; Draw histogram to plot window, the active histogram
			; is drawn last (on top).
			;-----------------------------------------------------
			WSET, plotWindowID
;print,'n_elements(x_axis) = ',n_elements(x_axis)
;print,'n_elements(hist) = ',n_elements(hist)
;print,'beginning OPLOT'
			OPLOT, x_axis[x_axis_min_idx:x_axis_max_idx], hist, $
				COLOR=drawColor, PSYM=10
;print,'finished OPLOT'

		ENDIF
	ENDFOR

	; This is only utilized for B/W data that is to be pseudocolored.
	; Therefore, the last x_axis coming out of the FOR loop is valid
	; (since for B/W data there will only be one.)
	x_axis_used	= x_axis[x_axis_min_idx:x_axis_max_idx]
;help, x_axis_used
	PTR_FREE, SELF.x_axis_for_colorbar_ptr
	SELF.x_axis_for_colorbar_ptr = PTR_NEW( x_axis_used )

	WSET, curWin

END
; GEOREF_IMAGE::stretchDnPlotHistogram

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ bottomSlidersEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO bottomSlidersEvent, event
	WIDGET_CONTROL, event.top, GET_UVALUE = statePtr

	whichColor = (WHERE(event.id eq (*statePtr).bottomSliders))[0]

	;---------------------------------------------------------------------
	; Sometimes this event handler is called manually and the 'value' tag
	; is not set.
	; Check for presence, use it if found, otherwise get from widget.
	;---------------------------------------------------------------------
	IF (WHERE(STRLOWCASE(TAG_NAMES(event)) EQ 'value'))[0] GE 0 THEN BEGIN
		bottomSliderValue = event.value
	ENDIF ELSE BEGIN
		WIDGET_CONTROL, (*statePtr).bottomSliders[whichColor], GET_VALUE = bottomSliderValue
	ENDELSE

	WIDGET_CONTROL, (*statePtr).topSliders[whichColor], GET_VALUE = topSliderValue

	IF bottomSliderValue GT topSliderValue THEN BEGIN
		bottomSliderValue = topSliderValue
		WIDGET_CONTROL, (*statePtr).bottomSliders[whichColor], SET_VALUE = bottomSliderValue
	ENDIF

	CASE 1 OF
		whichColor EQ 0 OR whichColor EQ 3 : BEGIN
			tmpIdx		= (*statePtr).redBandIdx
			redMin		= DOUBLE( *((*((*statePtr).bottomLimitPtr))[tmpIdx]) )
			redMax		= DOUBLE( *((*((*statePtr).topLimitPtr))[tmpIdx]) )
			newBottomValue	= bottomSliderValue / 1000.0D * ( redMax - redMin ) + redMin
		END
		whichColor EQ 1 OR whichColor EQ 4 : BEGIN
			tmpIdx		= (*statePtr).grnBandIdx
			grnMin		= DOUBLE( *((*((*statePtr).bottomLimitPtr))[tmpIdx]) )
			grnMax		= DOUBLE( *((*((*statePtr).topLimitPtr))[tmpIdx]) )
			newBottomValue	= bottomSliderValue / 1000.0D * ( grnMax - grnMin ) + grnMin
		END
		whichColor EQ 2 OR whichColor EQ 5 : BEGIN
			tmpIdx		= (*statePtr).bluBandIdx
			bluMin		= DOUBLE( *((*((*statePtr).bottomLimitPtr))[tmpIdx]) )
			bluMax		= DOUBLE( *((*((*statePtr).topLimitPtr))[tmpIdx]) )
			newBottomValue	= bottomSliderValue / 1000.0D * ( bluMax - bluMin ) + bluMin
		END
	ENDCASE

	;---------------------------------------------------------------------
	; If any integer type, then convert from double to long64.
	;---------------------------------------------------------------------
	rgbBandIdx		= (*statePtr).obj -> get_rgbBandIdx()
	display_rgb		= (*statePtr).obj -> get_display_rgb()
	plane_display_status	= [ ( rgbBandIdx[0] GE 0 AND display_rgb[0] ),	$
				    ( rgbBandIdx[1] GE 0 AND display_rgb[1] ),	$
				    ( rgbBandIdx[2] GE 0 AND display_rgb[2] ) ]
	active_plane_idx	= (WHERE( plane_display_status GT 0 ))[0]
	active_image_obj	= (*statePtr).obj -> ReturnImageDataObj( DATA_INDEX = rgbBandIdx[ active_plane_idx ] )
	datatype		= active_image_obj -> GetNumberType()
	ints			= [1,2,3,12,13,14,15]
	IF (WHERE( datatype EQ ints ))[0] NE -1 THEN $
		newBottomValue = LONG64( newBottomValue )

	*((*((*statePtr).stretchDnBottomPtr))[tmpIdx]) = newBottomValue

	whichPlot = whichColor
	(*statePtr).obj->stretchDnPlotHistogram, whichPlot

	;---------------------------------------------------------------------
	; Set the current label to reflect the new value.
	;---------------------------------------------------------------------
	WIDGET_CONTROL, (*statePtr).bottomLabels[whichColor], SET_VALUE = STRTRIM(newBottomValue,2)

	;---------------------------------------------------------------------
	; Set the OTHER label and slider that corresponds to the current viewplane color.
	;---------------------------------------------------------------------
	IF whichColor LT 3 THEN		$
		OTHER = whichColor + 3	$
	ELSE				$
		OTHER = whichColor - 3
	WIDGET_CONTROL, (*statePtr).bottomLabels[OTHER], SET_VALUE = STRTRIM(newBottomValue,2)
	WIDGET_CONTROL, (*statePtr).bottomSliders[OTHER], SET_VALUE = bottomSliderValue
END
; bottomSlidersEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ topSlidersEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO topSlidersEvent, event
	WIDGET_CONTROL, event.top, GET_UVALUE = statePtr

	whichColor = (WHERE(event.id eq (*statePtr).topSliders))[0]

	;---------------------------------------------------------------------
	; Sometimes this event handler is called manually and the 'value' tag
	; is not set.
	; Check for presence, use it if found, otherwise get from widget.
	;---------------------------------------------------------------------
	IF (WHERE(STRLOWCASE(TAG_NAMES(event)) EQ 'value'))[0] GE 0 THEN BEGIN
		topSliderValue = event.value
	ENDIF ELSE BEGIN
		WIDGET_CONTROL, (*statePtr).topSliders[whichColor], GET_VALUE = topSliderValue
	ENDELSE

	WIDGET_CONTROL, (*statePtr).bottomSliders[whichColor], GET_VALUE=bottomSliderValue

	IF topSliderValue LT bottomSliderValue THEN BEGIN
		topSliderValue = bottomSliderValue
		WIDGET_CONTROL, (*statePtr).topSliders[whichColor], SET_VALUE=topSliderValue
	ENDIF

	CASE 1 OF
		whichColor EQ 0 OR whichColor EQ 3 : BEGIN
			tmpIdx		= (*statePtr).redBandIdx
			redMin		= DOUBLE( *((*((*statePtr).bottomLimitPtr))[tmpIdx]) )
			redMax		= DOUBLE( *((*((*statePtr).topLimitPtr))[tmpIdx]) )
			newTopValue	= topSliderValue / 1000.0 * ( redMax - redMin ) + redMin
		END
		whichColor EQ 1 OR whichColor EQ 4 : BEGIN
			tmpIdx		= (*statePtr).grnBandIdx
			grnMin		= DOUBLE( *((*((*statePtr).bottomLimitPtr))[tmpIdx]) )
			grnMax		= DOUBLE( *((*((*statePtr).topLimitPtr))[tmpIdx]) )
			newTopValue	= topSliderValue / 1000.0 * ( grnMax - grnMin ) + grnMin
		END
		whichColor EQ 2 OR whichColor EQ 5 : BEGIN
			tmpIdx		= (*statePtr).bluBandIdx
			bluMin		= DOUBLE( *((*((*statePtr).bottomLimitPtr))[tmpIdx]) )
			bluMax		= DOUBLE( *((*((*statePtr).topLimitPtr))[tmpIdx]) )
			newTopValue	= topSliderValue / 1000.0 * ( bluMax - bluMin ) + bluMin
		END
	ENDCASE

	;---------------------------------------------------------------------
	; If any integer type, then convert from double to long64.
	;---------------------------------------------------------------------
	rgbBandIdx		= (*statePtr).obj -> get_rgbBandIdx()
	display_rgb		= (*statePtr).obj -> get_display_rgb()
	plane_display_status	= [ ( rgbBandIdx[0] GE 0 AND display_rgb[0] ),	$
				    ( rgbBandIdx[1] GE 0 AND display_rgb[1] ),	$
				    ( rgbBandIdx[2] GE 0 AND display_rgb[2] ) ]
	active_plane_idx	= (WHERE( plane_display_status GT 0 ))[0]
	active_image_obj	= (*statePtr).obj -> ReturnImageDataObj( DATA_INDEX = rgbBandIdx[ active_plane_idx ] )
	datatype		= active_image_obj -> GetNumberType()
	ints			= [1,2,3,12,13,14,15]
	IF (WHERE( datatype EQ ints ))[0] NE -1 THEN $
		newTopValue = LONG64( newTopValue )

	IF ( newTopValue LT *((*((*statePtr).stretchDnBottomPtr))[tmpIdx]) ) THEN $
		newTopValue = *((*((*statePtr).stretchDnBottomPtr))[tmpIdx])

	*((*((*statePtr).stretchDnTopPtr))[tmpIdx]) = newTopValue

	whichPlot = whichColor
	(*statePtr).obj->stretchDnPlotHistogram, whichPlot

	;---------------------------------------------------------------------
	; Set the current label to reflect the new value.
	;---------------------------------------------------------------------
	WIDGET_CONTROL, (*statePtr).topLabels[whichColor], SET_VALUE = STRTRIM(newTopValue,2)

	;---------------------------------------------------------------------
	; Set the OTHER label and slider that corresponds to the current viewplane color.
	;---------------------------------------------------------------------
	IF whichColor LT 3 THEN		$
		OTHER = whichColor + 3	$
	ELSE				$
		OTHER = whichColor - 3
	WIDGET_CONTROL, (*statePtr).topLabels[OTHER], SET_VALUE = STRTRIM(newTopValue,2)
	WIDGET_CONTROL, (*statePtr).topSliders[OTHER], SET_VALUE = topSliderValue
END
; topSlidersEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ gammaSlidersEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO gammaSlidersEvent, event
	WIDGET_CONTROL, event.top, GET_UVALUE = statePtr

	whichColor = (WHERE(event.id EQ (*statePtr).gammaSliders))[0]

	; Set the current label to reflect the new value.
	WIDGET_CONTROL, (*statePtr).gammaLabels[whichColor], SET_VALUE = STRTRIM(event.value/100.0,2)

	; Set the OTHER label and slider that corresponds to the current viewplane color.
	IF whichColor LT 3 THEN		$
		OTHER = whichColor + 3	$
	ELSE				$
		OTHER = whichColor - 3
	WIDGET_CONTROL, (*statePtr).gammaLabels[OTHER], SET_VALUE = STRTRIM(event.value/100.0,2)
	WIDGET_CONTROL, (*statePtr).gammaSliders[OTHER], SET_VALUE = event.value

	CASE 1 OF
		whichColor EQ 0 OR whichColor EQ 3 : idx = (*statePtr).redBandIdx
		whichColor EQ 1 OR whichColor EQ 4 : idx = (*statePtr).grnBandIdx
		whichColor EQ 2 OR whichColor EQ 5 : idx = (*statePtr).bluBandIdx
	ENDCASE

	*((*((*statePtr).stretchDnGammaPtr))[idx]) = event.value/100.0

	whichPlot = whichColor
	(*statePtr).obj->stretchDnPlotHistogram, whichPlot
END
; gammaSlidersEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ bottomLimitTextEvent @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO bottomLimitTextEvent, event

	WIDGET_CONTROL, event.top, GET_UVALUE = statePtr

	whichColor = (WHERE(event.id eq (*statePtr).bottomLimitText))[0]

	WIDGET_CONTROL, event.id, GET_VALUE = bottomLimitTextValue
	bottomLimitTextValue = bottomLimitTextValue[0]
	CASE 1 OF
		whichColor EQ 0 OR whichColor EQ 3 : BEGIN
			idx = (*statePtr).redBandIdx
			IF whichColor EQ 0 THEN otherBillboard = 3 ELSE otherBillboard = 0
		END
		whichColor EQ 1 OR whichColor EQ 4 : BEGIN
			idx = (*statePtr).grnBandIdx
			IF whichColor EQ 1 THEN otherBillboard = 4 ELSE otherBillboard = 1
		END
		whichColor EQ 2 OR whichColor EQ 5 : BEGIN
			idx = (*statePtr).bluBandIdx
			IF whichColor EQ 2 THEN otherBillboard = 5 ELSE otherBillboard = 2
		END
	ENDCASE

	*((*((*statePtr).bottomLimitPtr))[idx]) = bottomLimitTextValue

	; Set the corresponding "other" billboard (if R then set Rgb, if G then set rGb, etc.)
	WIDGET_CONTROL, (*statePtr).bottomLimitText[otherBillboard], SET_VALUE = bottomLimitTextValue

	;---------------------------------------------------------------------
	; If the limit is being changed to a value inbetween the bottom and
	; top slider positions, then reset the stored value of the slider to
	; the end value.  When the slider event handler is called the slider
	; will then be moved to this position.  (Without this step, the slider
	; would remain in is position and a new DN value would be calculated.)
	;---------------------------------------------------------------------
	IF bottomlimittextvalue GT *((*((*statePtr).stretchDnBottomPtr))[idx]) THEN BEGIN
		WIDGET_CONTROL,(*statePtr).bottomSliders[whichColor],SET_VALUE=0
		WIDGET_CONTROL,(*statePtr).bottomSliders[otherBillboard],SET_VALUE=0
	ENDIF

	;---------------------------------------------------------------------
	; If text limit is being set to a value within the current slider
	; limits then the slider widgets must be "touched" to get them to
	; set them selves correctly.
	;---------------------------------------------------------------------
	IF bottomLimitTextValue GT *((*((*statePtr).stretchDnBottomPtr))[idx]) THEN BEGIN
		bottomSlidersEvent,						$
			{							$
			TOP	: event.top,					$
			ID	: (*statePtr).bottomSliders[whichColor],	$
			HANDLER	: 'bottomSlidersEvent'				$
			}
		bottomSlidersEvent,						$
			{							$
			TOP	: event.top,					$
			ID	: (*statePtr).bottomSliders[otherBillboard],	$
			HANDLER	: 'bottomSlidersEvent'				$
			}
	ENDIF

	dataMinLim		= DOUBLE( *((*((*statePtr).bottomLimitPtr))[idx]) )
	dataMaxLim		= DOUBLE( *((*((*statePtr).topLimitPtr))[idx]) )
	dataTop			= DOUBLE( *((*((*statePtr).stretchDnTopPtr))[idx]) )
	newTopSliderValue 	= ROUND( 1000.0d * ABS( ( dataTop - dataMinLim ) / ( dataMaxLim - dataMinLim ) ) )
	WIDGET_CONTROL, (*statePtr).topSliders[whichColor], SET_VALUE = newTopSliderValue
	WIDGET_CONTROL, (*statePtr).topSliders[otherBillboard], SET_VALUE = newTopSliderValue

	dataMinLim		= DOUBLE( *((*((*statePtr).bottomLimitPtr))[idx]) )
	dataMaxLim		= DOUBLE( *((*((*statePtr).topLimitPtr))[idx]) )
	dataBottom		= DOUBLE( *((*((*statePtr).stretchDnBottomPtr))[idx]) )
	newBottomSliderValue 	= ROUND( 1000.0d * ABS( ( dataBottom - dataMinLim ) / ( dataMaxLim - dataMinLim ) ) )
	WIDGET_CONTROL, (*statePtr).bottomSliders[whichColor], SET_VALUE = newBottomSliderValue
	WIDGET_CONTROL, (*statePtr).bottomSliders[otherBillboard], SET_VALUE = newBottomSliderValue

END
; bottomLimitTextEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ topLimitTextEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO topLimitTextEvent, event

	WIDGET_CONTROL, event.top, GET_UVALUE = statePtr

	whichColor = (WHERE(event.id eq (*statePtr).topLimitText))[0]

	WIDGET_CONTROL, event.id, GET_VALUE = topLimitTextValue
	topLimitTextValue = topLimitTextValue[0]
	CASE 1 OF
		whichColor EQ 0 OR whichColor EQ 3 : BEGIN
			idx = (*statePtr).redBandIdx
			IF whichColor EQ 0 THEN otherBillboard = 3 ELSE otherBillboard = 0
		END
		whichColor EQ 1 OR whichColor EQ 4 : BEGIN
			idx = (*statePtr).grnBandIdx
			IF whichColor EQ 1 THEN otherBillboard = 4 ELSE otherBillboard = 1
		END
		whichColor EQ 2 OR whichColor EQ 5 : BEGIN
			idx = (*statePtr).bluBandIdx
			IF whichColor EQ 2 THEN otherBillboard = 5 ELSE otherBillboard = 2
		END
	ENDCASE

	*((*((*statePtr).topLimitPtr))[idx]) = topLimitTextValue

	; Set the corresponding "other" billboard (if R then set Rgb, if G then set rGb, etc.)
	WIDGET_CONTROL, (*statePtr).topLimitText[otherBillboard], SET_VALUE = topLimitTextValue

	;---------------------------------------------------------------------
	; If the limit is being changed to a value inbetween the bottom and
	; top slider positions, then reset the stored value of the slider to
	; the end value.  When the slider event handler is called the slider
	; will then be moved to this position.  (Without this step, the slider
	; would remain in is position and a new DN value would be calculated.)
	;---------------------------------------------------------------------
	IF toplimittextvalue LT *((*((*statePtr).stretchDnTopPtr))[idx]) THEN BEGIN
		WIDGET_CONTROL,(*statePtr).topSliders[whichColor],SET_VALUE=1000
		WIDGET_CONTROL,(*statePtr).topSliders[otherBillboard],SET_VALUE=1000
	ENDIF

	;---------------------------------------------------------------------
	; If text limit is being set to a value within the current slider
	; limits then the slider widgets must be "touched" to get them to
	; set them selves correctly.
	;---------------------------------------------------------------------
	IF topLimitTextValue[0] LT *((*((*statePtr).stretchDnTopPtr))[idx]) THEN BEGIN
		topSlidersEvent,						$
			{								$
			TOP	: event.top,						$
			ID	: (*statePtr).topSliders[whichColor],	$
			HANDLER	: 'topSlidersEvent'				$
			}
		topSlidersEvent,						$
			{								$
			TOP	: event.top,						$
			ID	: (*statePtr).topSliders[otherBillboard],	$
			HANDLER	: 'topSlidersEvent'				$
			}
	ENDIF

	dataMinLim		= DOUBLE( *((*((*statePtr).bottomLimitPtr))[idx]) )
	dataMaxLim		= DOUBLE( *((*((*statePtr).topLimitPtr))[idx]) )
	dataTop			= DOUBLE( *((*((*statePtr).stretchDnTopPtr))[idx]) )
	newTopSliderValue 	= ROUND( 1000.0d * ABS( ( dataTop - dataMinLim ) / ( dataMaxLim - dataMinLim ) ) )
	WIDGET_CONTROL, (*statePtr).topSliders[whichColor], SET_VALUE = newTopSliderValue
	WIDGET_CONTROL, (*statePtr).topSliders[otherBillboard], SET_VALUE = newTopSliderValue

	dataMinLim		= DOUBLE( *((*((*statePtr).bottomLimitPtr))[idx]) )
	dataMaxLim		= DOUBLE( *((*((*statePtr).topLimitPtr))[idx]) )
	dataBottom		= DOUBLE( *((*((*statePtr).stretchDnBottomPtr))[idx]) )
	newBottomSliderValue 	= ROUND( 1000.0d * ABS( ( dataBottom - dataMinLim ) / ( dataMaxLim - dataMinLim ) ) )
	WIDGET_CONTROL, (*statePtr).bottomSliders[whichColor], SET_VALUE = newBottomSliderValue
	WIDGET_CONTROL, (*statePtr).bottomSliders[otherBillboard], SET_VALUE = newBottomSliderValue

END
; topLimitTextEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ bottomLimitResetButtonEvent @@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO bottomLimitResetButtonEvent, event
	WIDGET_CONTROL, event.top, GET_UVALUE = statePtr

	whichColor = (WHERE(event.id eq (*statePtr).bottomLimitResetButton))[0]

	CASE 1 OF
		whichColor EQ 0 OR whichColor EQ 3 : idx = (*statePtr).redBandIdx
		whichColor EQ 1 OR whichColor EQ 4 : idx = (*statePtr).grnBandIdx
		whichColor EQ 2 OR whichColor EQ 5 : idx = (*statePtr).bluBandIdx
	ENDCASE
	IF whichColor LT 3 THEN			$
		otherBillboard = whichColor + 3	$
	ELSE	otherBillBoard = whichColor - 3

	newLimitValue = DOUBLE( *((*((*statePtr).min_dataPtr))[idx]) )

	;---------------------------------------------------------------------
	; Check against top limit value for incorrect overlap.  Reset top
	; limit value if necessary.
	;---------------------------------------------------------------------
	WIDGET_CONTROL, (*statePtr).topLimitText[whichColor], GET_VALUE = topLimitValue
	IF newLimitValue GE topLimitValue[0] THEN BEGIN
		topLimitResetButtonEvent,						$
			{								$
			TOP	: event.top,						$
			ID	: (*statePtr).topLimitResetButton[whichColor],		$
			HANDLER	: 'topLimitResetButtonEvent'				$
			}
		topLimitResetButtonEvent,						$
			{								$
			TOP	: event.top,						$
			ID	: (*statePtr).topLimitResetButton[otherBillboard],	$
			HANDLER	: 'topLimitResetButtonEvent'				$
			}
	ENDIF

	;---------------------------------------------------------------------
	; Reset MIN value.
	;---------------------------------------------------------------------
	*((*((*statePtr).bottomLimitPtr))[idx]) = newLimitValue
	WIDGET_CONTROL, (*statePtr).bottomLimitText[whichColor], SET_VALUE = STRTRIM( newLimitValue, 2 )

	;---------------------------------------------------------------------
	; Reset the sliders.
	;---------------------------------------------------------------------
	dataMinLim		= DOUBLE( *((*((*statePtr).bottomLimitPtr))[idx]) )
	dataMaxLim		= DOUBLE( *((*((*statePtr).topLimitPtr))[idx]) )

	WIDGET_CONTROL, (*statePtr).topLabels[whichColor], GET_VALUE = topVal
	IF topVal LT dataMinLim THEN BEGIN
		topVal = dataMinLim

		;---------------------------------------------------------------------
		; If any integer type, then convert from double to long64.
		;---------------------------------------------------------------------
		rgbBandIdx		= (*statePtr).obj -> get_rgbBandIdx()
		display_rgb		= (*statePtr).obj -> get_display_rgb()
		plane_display_status	= [ ( rgbBandIdx[0] GE 0 AND display_rgb[0] ),	$
					    ( rgbBandIdx[1] GE 0 AND display_rgb[1] ),	$
					    ( rgbBandIdx[2] GE 0 AND display_rgb[2] ) ]
		active_plane_idx	= (WHERE( plane_display_status GT 0 ))[0]
		active_image_obj	= (*statePtr).obj -> ReturnImageDataObj( DATA_INDEX = rgbBandIdx[ active_plane_idx ] )
		datatype		= active_image_obj -> GetNumberType()
		ints			= [1,2,3,12,13,14,15]
		IF (WHERE( datatype EQ ints ))[0] NE -1 THEN $
			dataMinLim = LONG64( dataMinLim )

		WIDGET_CONTROL, (*statePtr).topLabels[whichColor], SET_VALUE = STRTRIM(dataMinLim,2)
	ENDIF
	newTopSliderValue 	= ROUND( 1000.0d * ABS( ( topVal - dataMinLim ) / ( dataMaxLim - dataMinLim ) ) )
	WIDGET_CONTROL, (*statePtr).topSliders[whichColor], SET_VALUE = newTopSliderValue
	WIDGET_CONTROL, (*statePtr).topSliders[otherBillboard], SET_VALUE = newTopSliderValue

	WIDGET_CONTROL, (*statePtr).bottomLabels[whichColor], GET_VALUE = bottomVal
	IF bottomVal LT dataMinLim THEN BEGIN
		bottomVal = dataMinLim

		;---------------------------------------------------------------------
		; If any integer type, then convert from double to long64.
		;---------------------------------------------------------------------
		rgbBandIdx		= (*statePtr).obj -> get_rgbBandIdx()
		display_rgb		= (*statePtr).obj -> get_display_rgb()
		plane_display_status	= [ ( rgbBandIdx[0] GE 0 AND display_rgb[0] ),	$
					    ( rgbBandIdx[1] GE 0 AND display_rgb[1] ),	$
					    ( rgbBandIdx[2] GE 0 AND display_rgb[2] ) ]
		active_plane_idx	= (WHERE( plane_display_status GT 0 ))[0]
		active_image_obj	= (*statePtr).obj -> ReturnImageDataObj( DATA_INDEX = rgbBandIdx[ active_plane_idx ] )
		datatype		= active_image_obj -> GetNumberType()
		ints			= [1,2,3,12,13,14,15]
		IF (WHERE( datatype EQ ints ))[0] NE -1 THEN $
			dataMinLim = LONG64( dataMinLim )

		WIDGET_CONTROL, (*statePtr).bottomLabels[whichColor], SET_VALUE = STRTRIM(dataMinLim,2)
	ENDIF
	newBottomSliderValue 	= ROUND( 1000.0d * ABS( ( bottomVal - dataMinLim ) / ( dataMaxLim - dataMinLim ) ) )
	WIDGET_CONTROL, (*statePtr).bottomSliders[whichColor], SET_VALUE = newBottomSliderValue
	WIDGET_CONTROL, (*statePtr).bottomSliders[otherBillboard], SET_VALUE = newBottomSliderValue

	;---------------------------------------------------------------------
	; Reset the sliders.
	;---------------------------------------------------------------------
	bottomSlidersEvent,	{							$
				TOP	: event.top,					$
				ID	: (*statePtr).bottomSliders[whichColor],	$
				HANDLER	: 'bottomSlidersEvent'				$
				}

END
; bottomLimitResetButtonEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ topLimitResetButtonEvent @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO topLimitResetButtonEvent, event
	WIDGET_CONTROL, event.top, GET_UVALUE = statePtr

	whichColor = (WHERE(event.id eq (*statePtr).topLimitResetButton))[0]

	CASE 1 OF
		whichColor EQ 0 OR whichColor EQ 3 : idx = (*statePtr).redBandIdx
		whichColor EQ 1 OR whichColor EQ 4 : idx = (*statePtr).grnBandIdx
		whichColor EQ 2 OR whichColor EQ 5 : idx = (*statePtr).bluBandIdx
	ENDCASE
	IF whichColor LT 3 THEN			$
		otherBillboard = whichColor + 3	$
	ELSE	otherBillBoard = whichColor - 3

	newLimitValue = DOUBLE( *((*((*statePtr).max_dataPtr))[idx]) )

	;---------------------------------------------------------------------
	; Check against bottom limit value for incorrect overlap.  Reset bottom
	; limit value if necessary.
	;---------------------------------------------------------------------
	WIDGET_CONTROL, (*statePtr).bottomLimitText[whichColor], GET_VALUE = bottomLimitValue
	IF newLimitValue LE bottomLimitValue[0] THEN BEGIN
		bottomLimitResetButtonEvent,						$
			{								$
			TOP	: event.top,						$
			ID	: (*statePtr).bottomLimitResetButton[whichColor],	$
			HANDLER	: 'bottomLimitResetButtonEvent'				$
			}
		bottomLimitResetButtonEvent,						$
			{								$
			TOP	: event.top,						$
			ID	: (*statePtr).bottomLimitResetButton[otherBillboard],	$
			HANDLER	: 'bottomLimitResetButtonEvent'				$
			}
	ENDIF

	;---------------------------------------------------------------------
	; Reset MAX value.
	;---------------------------------------------------------------------
	*((*((*statePtr).topLimitPtr))[idx]) = newLimitValue
	WIDGET_CONTROL, (*statePtr).topLimitText[whichColor], SET_VALUE = STRTRIM( newLimitValue, 2 )

	;---------------------------------------------------------------------
	; Reset the sliders.
	;---------------------------------------------------------------------
	dataMinLim		= DOUBLE( *((*((*statePtr).bottomLimitPtr))[idx]) )
	dataMaxLim		= DOUBLE( *((*((*statePtr).topLimitPtr))[idx]) )

	WIDGET_CONTROL, (*statePtr).topLabels[whichColor], GET_VALUE = topVal
	IF topVal GT dataMaxLim THEN BEGIN
		topVal = dataMaxLim

		;---------------------------------------------------------------------
		; If any integer type, then convert from double to long64.
		;---------------------------------------------------------------------
		rgbBandIdx		= (*statePtr).obj -> get_rgbBandIdx()
		display_rgb		= (*statePtr).obj -> get_display_rgb()
		plane_display_status	= [ ( rgbBandIdx[0] GE 0 AND display_rgb[0] ),	$
					    ( rgbBandIdx[1] GE 0 AND display_rgb[1] ),	$
					    ( rgbBandIdx[2] GE 0 AND display_rgb[2] ) ]
		active_plane_idx	= (WHERE( plane_display_status GT 0 ))[0]
		active_image_obj	= (*statePtr).obj -> ReturnImageDataObj( DATA_INDEX = rgbBandIdx[ active_plane_idx ] )
		datatype		= active_image_obj -> GetNumberType()
		ints			= [1,2,3,12,13,14,15]
		IF (WHERE( datatype EQ ints ))[0] NE -1 THEN $
			dataMaxLim = LONG64( dataMaxLim )

		WIDGET_CONTROL, (*statePtr).topLabels[whichColor], SET_VALUE = STRTRIM(dataMaxLim,2)
	ENDIF
	newTopSliderValue 	= ROUND( 1000.0d * ABS( ( topVal - dataMinLim ) / ( dataMaxLim - dataMinLim ) ) )
	WIDGET_CONTROL, (*statePtr).topSliders[whichColor], SET_VALUE = newTopSliderValue
	WIDGET_CONTROL, (*statePtr).topSliders[otherBillboard], SET_VALUE = newTopSliderValue

	WIDGET_CONTROL, (*statePtr).bottomLabels[whichColor], GET_VALUE = bottomVal
	IF bottomVal GT dataMaxLim THEN BEGIN
		bottomVal = dataMaxLim

		;---------------------------------------------------------------------
		; If any integer type, then convert from double to long64.
		;---------------------------------------------------------------------
		rgbBandIdx		= (*statePtr).obj -> get_rgbBandIdx()
		display_rgb		= (*statePtr).obj -> get_display_rgb()
		plane_display_status	= [ ( rgbBandIdx[0] GE 0 AND display_rgb[0] ),	$
					    ( rgbBandIdx[1] GE 0 AND display_rgb[1] ),	$
					    ( rgbBandIdx[2] GE 0 AND display_rgb[2] ) ]
		active_plane_idx	= (WHERE( plane_display_status GT 0 ))[0]
		active_image_obj	= (*statePtr).obj -> ReturnImageDataObj( DATA_INDEX = rgbBandIdx[ active_plane_idx ] )
		datatype		= active_image_obj -> GetNumberType()
		ints			= [1,2,3,12,13,14,15]
		IF (WHERE( datatype EQ ints ))[0] NE -1 THEN $
			dataMaxLim = LONG64( dataMaxLim )

		WIDGET_CONTROL, (*statePtr).bottomLabels[whichColor], SET_VALUE = STRTRIM(dataMaxLim,2)
	ENDIF
	newBottomSliderValue 	= ROUND( 1000.0d * ABS( ( bottomVal - dataMinLim ) / ( dataMaxLim - dataMinLim ) ) )
	WIDGET_CONTROL, (*statePtr).bottomSliders[whichColor], SET_VALUE = newBottomSliderValue
	WIDGET_CONTROL, (*statePtr).bottomSliders[otherBillboard], SET_VALUE = newBottomSliderValue

	;---------------------------------------------------------------------
	; Reset the sliders.
	;---------------------------------------------------------------------
	topSlidersEvent,	{							$
				TOP	: event.top,					$
				ID	: (*statePtr).topSliders[whichColor],		$
				HANDLER	: 'topSlidersEvent'				$
				}

END
; topLimitResetButtonEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ applyButtonEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;PRO applyButtonEvent, event
FUNCTION applyButtonEvent, event

	IF !D.N_COLORS GT 256 THEN BEGIN
		WIDGET_CONTROL, event.top, GET_UVALUE = statePtr
;;		((*statePtr).obj)->DisplayData
		IF ((*statePtr).obj)->UpdateStretchDnInterface() THEN $
			((*statePtr).obj)->DisplayData

	ENDIF

	RETURN, { GEOREFIMAGEHIST, id:event.id, top:event.top, handler:event.handler, button_pressed:'apply' }
END
; applyButtonEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ storeStretchButtonEvent @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO storeStretchButtonEvent, event

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== storeStretchButtonEvent =========='
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

	;-----------------------------------------------------
	; Note that stretches are stored by viewing color, not
	; by data or data type.  To recall a stretch for any
	; given image data, that data must be viewed in the
	; color corresponding to the same color specified in
	; the ASCII stretch file.  The ASCII stretch file can
	; be edited manually prior to recall, thus allowing
	; the dataset to be used in a different color plane.
	;-----------------------------------------------------

;;;	IF !D.N_COLORS GT 256 THEN BEGIN
		WIDGET_CONTROL, event.top, GET_UVALUE = statePtr

		start_over = 1
		WHILE start_over DO BEGIN
			start_over	= 0
			;-----------------------------------------------------
			; User-selected directory is stored in the button's
			; uvalue.
			;-----------------------------------------------------
			WIDGET_CONTROL, event.id, GET_UVALUE = path
;;;ckt,apr2001			filename	= DIALOG_PICKFILE( TITLE = 'Enter file name:', PATH = path, GET_PATH = path )
			filename	= dialog_pickfile_wrapper( TITLE = 'Enter file name:', PATH = path, GET_PATH = path )
			;-----------------------------------------------------
			; Account for the "Cancel" button in DIALOG_PICKFILE.
			;-----------------------------------------------------
			IF filename EQ '' AND path EQ '' THEN BEGIN
				RETURN
			ENDIF
			;-----------------------------------------------------
			; Store the newly user-selected directory in the
			; button's uvalue.
			;-----------------------------------------------------
			WIDGET_CONTROL, event.id, SET_UVALUE = path
			;-----------------------------------------------------
			; Check for presence of a filename.
			;-----------------------------------------------------
			IF blankFileName( filename ) THEN BEGIN
				message		= [ 'No file name was entered.', '', 'Try again?' ]
				IF STRUPCASE( DIALOG_MESSAGE( message, /QUESTION ) ) EQ 'YES' THEN BEGIN
					filename	= ''
					start_over	= 1
				ENDIF ELSE BEGIN
					RETURN
				ENDELSE
			ENDIF ELSE BEGIN
				;-----------------------------------------------------
				; Check to see if file already exists because it will
				; be overwritten.
				;-----------------------------------------------------
				IF (FINDFILE( filename ))[0] NE '' THEN BEGIN
					msg = [ 'File exists and will be overwritten.', filename, '', 'Is this OK?' ]
					IF STRUPCASE( DIALOG_MESSAGE( msg, /QUESTION ) ) EQ 'NO' THEN BEGIN
						start_over = 1
					ENDIF
				ENDIF
			ENDELSE
		ENDWHILE

		;-----------------------------------------------------
		; Stretch parameters are stored as ASCII so they can
		; be modified with a text editor.
		;-----------------------------------------------------
		OPENW, stretchfile_lun, filename, /GET_LUN
		header = 'stretch parameter file'
		PRINTF, stretchfile_lun, header + '   Date:  ' + systime()

		;-----------------------------------------------------
		; Store red stretch parameters.
		;-----------------------------------------------------
		IF (*statePtr).redBandIdx GE 0 THEN BEGIN
			idx = (*statePtr).redBandIdx
			PRINTF, stretchfile_lun, 'red_bot=' + STRTRIM( *((*((*statePtr).stretchDnBottomPtr))[idx]), 2 )
			PRINTF, stretchfile_lun, 'red_top=' + STRTRIM( *((*((*statePtr).stretchDnTopPtr))[idx]), 2 )
			PRINTF, stretchfile_lun, 'red_gam=' + STRTRIM( *((*((*statePtr).stretchDnGammaPtr))[idx]), 2 )
			PRINTF, stretchfile_lun, 'red_min_limit=' + STRTRIM( *((*((*statePtr).bottomLimitPtr))[idx]), 2 )
			PRINTF, stretchfile_lun, 'red_max_limit=' + STRTRIM( *((*((*statePtr).topLimitPtr))[idx]), 2 )
		ENDIF

		;-----------------------------------------------------
		; Store grn stretch parameters.
		;-----------------------------------------------------
		IF (*statePtr).grnBandIdx GE 0 THEN BEGIN
			idx = (*statePtr).grnBandIdx
			PRINTF, stretchfile_lun, 'grn_bot=' + STRTRIM( *((*((*statePtr).stretchDnBottomPtr))[idx]), 2 )
			PRINTF, stretchfile_lun, 'grn_top=' + STRTRIM( *((*((*statePtr).stretchDnTopPtr))[idx]), 2 )
			PRINTF, stretchfile_lun, 'grn_gam=' + STRTRIM( *((*((*statePtr).stretchDnGammaPtr))[idx]), 2 )
			PRINTF, stretchfile_lun, 'grn_min_limit=' + STRTRIM( *((*((*statePtr).bottomLimitPtr))[idx]), 2 )
			PRINTF, stretchfile_lun, 'grn_max_limit=' + STRTRIM( *((*((*statePtr).topLimitPtr))[idx]), 2 )
		ENDIF

		;-----------------------------------------------------
		; Store blu stretch parameters.
		;-----------------------------------------------------
		IF (*statePtr).bluBandIdx GE 0 THEN BEGIN
			idx = (*statePtr).bluBandIdx
			PRINTF, stretchfile_lun, 'blu_bot=' + STRTRIM( *((*((*statePtr).stretchDnBottomPtr))[idx]), 2 )
			PRINTF, stretchfile_lun, 'blu_top=' + STRTRIM( *((*((*statePtr).stretchDnTopPtr))[idx]), 2 )
			PRINTF, stretchfile_lun, 'blu_gam=' + STRTRIM( *((*((*statePtr).stretchDnGammaPtr))[idx]), 2 )
			PRINTF, stretchfile_lun, 'blu_min_limit=' + STRTRIM( *((*((*statePtr).bottomLimitPtr))[idx]), 2 )
			PRINTF, stretchfile_lun, 'blu_max_limit=' + STRTRIM( *((*((*statePtr).topLimitPtr))[idx]), 2 )
		ENDIF

		;-----------------------------------------------------
		; Close and free (in one step) the file's logical unit
		; number.
		;-----------------------------------------------------
		FREE_LUN, stretchfile_lun
;;;	ENDIF
END
; storeStretchButtonEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ recallStretchButtonEvent @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO recallStretchButtonEvent, event

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== recallStretchButtonEvent =========='
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

	;-----------------------------------------------------
	; Note that stretches are stored by viewing color, not
	; by data or data type.  To recall a stretch for any
	; given image data, that data must be viewed in the
	; color corresponding to the same color specified in
	; the ASCII stretch file.  The ASCII stretch file can
	; be edited manually prior to recall, thus allowing
	; the dataset to be used in a different color plane.
	;-----------------------------------------------------


;;;	IF !D.N_COLORS GT 256 THEN BEGIN
		WIDGET_CONTROL, event.top, GET_UVALUE = statePtr

		start_over = 1
		WHILE start_over DO BEGIN
			start_over	= 0
			;-----------------------------------------------------
			; User-selected directory is stored in the button's
			; uvalue.
			;-----------------------------------------------------
			WIDGET_CONTROL, event.id, GET_UVALUE = path
;;;ckt,apr2001			filename	= DIALOG_PICKFILE( TITLE = 'Enter file name:', PATH = path, GET_PATH = path )
			filename	= dialog_pickfile_wrapper( TITLE = 'Enter file name:', PATH = path, GET_PATH = path, /MUST_EXIST )
;help,filename
;help,path
;help,filename[0]
;help,path[0]
;help,n_elements(filename)
;help,n_elements(findfile(filename[0]))

			;-----------------------------------------------------
			; Account for:
			;	1. "Cancel" button in DIALOG_PICKFILE.
			; 	2. <CR> w/o any file specified, in which case
			;		DIALOG_PICKFILE returns array list of
			;		all files in specified directory.
			;	3. Existance of any specified filename.
			;-----------------------------------------------------
			IF ( filename[0] EQ '' AND path[0] EQ '' )					$
			OR N_ELEMENTS( filename ) GT 1 OR N_ELEMENTS( FINDFILE( filename[0] ) ) GT 1 	$
			OR (FINDFILE( filename[0] ))[0] EQ '' THEN BEGIN
				result = DIALOG_MESSAGE( 'File not found.  Please retry.', /INFORMATION )
				RETURN
			ENDIF

			;-----------------------------------------------------
			; Store the newly user-selected directory in the
			; button's uvalue.
			;-----------------------------------------------------
			WIDGET_CONTROL, event.id, SET_UVALUE = path
			;-----------------------------------------------------
			; Check for presence of a filename.
			;-----------------------------------------------------
			IF blankFileName( filename ) THEN BEGIN
				message		= [ 'No file name was entered.', '', 'Try again?' ]
				IF STRUPCASE( DIALOG_MESSAGE( message, /QUESTION ) ) EQ 'YES' THEN BEGIN
					filename	= ''
					start_over	= 1
				ENDIF ELSE BEGIN
					RETURN
				ENDELSE
			ENDIF ELSE BEGIN
				;-----------------------------------------------------
				; Check to see if it's a valid stretch parameter file.
				;-----------------------------------------------------
				OPENR, stretchfile_lun, filename, /GET_LUN
				header = ''
				READF, stretchfile_lun, header
				FREE_LUN, stretchfile_lun
				IF STRLOWCASE(STRMID( header, 0, 22 )) NE 'stretch parameter file' THEN BEGIN
					msg = [ 'Invalid stretch parameter file.'		,$
						filename					,$
						''						,$
						'Do you want to select another file?'		]
					IF STRUPCASE( DIALOG_MESSAGE( msg, /QUESTION ) ) EQ 'YES' THEN BEGIN
						start_over = 1
					ENDIF ELSE BEGIN
						RETURN
					ENDELSE
				ENDIF
			ENDELSE
		ENDWHILE

		;-----------------------------------------------------
		; Stretch parameters are stored as ASCII so they can
		; be modified with a text editor.  Use the READ_ASCII
		; routine just to help count the number of parameters
		; present (not all params need to be present; only
		; those present will be utilized; the missing ones
		; will remain unchanged.)  Then use OPENR and READF to
		; get the information from the file.
		;-----------------------------------------------------
		result = READ_ASCII( filename, DATA_START = 1, DELIMITER = '=' )

		; Work-around for bug that turned up in IDL 5.4.
		tagnames = TAG_NAMES( result )
		correct_tag_index = (WHERE( tagnames EQ 'FIELD1' OR tagnames EQ 'FIELD01' ))[0]

		num_str_params_in_file = N_ELEMENTS( (result.(correct_tag_index))[0,*] )
		OPENR, stretchfile_lun, filename, /GET_LUN
		header = ''
		READF, stretchfile_lun, header
		strparms = strarr( 2, 15 )
		asciistring = ''
		FOR i = 0, num_str_params_in_file - 1 DO BEGIN
			READF, stretchfile_lun, asciistring
			strparms[*,i] = STR_SEP( asciistring, '=' )
		ENDFOR
		FREE_LUN, stretchfile_lun

		;-----------------------------------------------------
		; Error check any RED values to be restored.

;***** ERROR CHECKING PROBABLY NEEDS MORE WORK !! *****;

		;-----------------------------------------------------
		IF (*statePtr).redBandIdx GE 0 THEN BEGIN

			IF (WHERE(strparms EQ 'red_min_limit'))[0] NE -1 THEN BEGIN
				redMinLimit = DOUBLE( (strparms[WHERE(strparms EQ 'red_min_limit')+1])[0] )
			ENDIF ELSE BEGIN
				WIDGET_CONTROL, (*statePtr).bottomLimitText[0], GET_VALUE = minLimit
				redMinLimit = DOUBLE( minLimit[0] )
			ENDELSE
			IF (WHERE(strparms EQ 'red_max_limit'))[0] NE -1 THEN BEGIN
				redMaxLimit = DOUBLE( (strparms[WHERE(strparms EQ 'red_max_limit')+1])[0] )
			ENDIF ELSE BEGIN
				WIDGET_CONTROL, (*statePtr).topLimitText[0], GET_VALUE = maxLimit
				redMaxLimit = DOUBLE( maxLimit[0] )
			ENDELSE
			IF redMinLimit GT redMaxLimit THEN BEGIN
				result = DIALOG_MESSAGE( $
					[ 'The RED LIMIT values in the recalled stretch file are possibly reversed.', $
					'To use this stretch file, you must manually edit the LIMIT values', $
					'in the stretch file and try again to recall it.' ], /INFORMATION )
				RETURN
			ENDIF

			IF (WHERE(strparms EQ 'red_bot'))[0] NE -1 THEN BEGIN
				redBot = FLOAT((strparms[WHERE(strparms EQ 'red_bot')+1])[0])
				IF redBot GT redMaxLimit THEN BEGIN
					result = DIALOG_MESSAGE( $
						[ 'ERROR:  The RED BOTTOM value in the recalled stretch file is', $
						'larger than the red_max_limit.  This is invalid.  To use this', $
						'stretch file, you must manually edit the values and try again', $
						'to recall it.' ], /ERROR )
					RETURN
				ENDIF
;				IF redBot GT redMinLimit THEN BEGIN
;					redBot = redMinLimit
;					result = DIALOG_MESSAGE( $
;						[ 'FYI:  The RED BOTTOM value in the recalled stretch file is inbetween', $
;						'the red min and max slider values.  RED BOTTOM is being reset to', $
;						'the red min value.' ], /INFORMATION )
;				ENDIF
			ENDIF

			IF (WHERE(strparms EQ 'red_top'))[0] NE -1 THEN BEGIN
				redTop = FLOAT((strparms[WHERE(strparms EQ 'red_top')+1])[0])
				IF redTop LT redMinLimit THEN BEGIN
					result = DIALOG_MESSAGE( $
						[ 'ERROR:  The RED TOP value in the recalled stretch file is', $
						'less than the red_min_limit.  This is invalid.  To use this', $
						'stretch file, you must manually edit the values and try again', $
						'to recall it.' ], /ERROR )
					RETURN
				ENDIF
;				IF redTop LT redMaxLimit THEN BEGIN
;					redTop = redMaxLimit
;					result = DIALOG_MESSAGE( $
;						[ 'FYI:  The RED TOP value in the recalled stretch file is inbetween', $
;						'the red min and max slider values.  RED TOP is being reset to', $
;						'the red max value.' ], /INFORMATION )
;				ENDIF
			ENDIF

			IF (WHERE(strparms EQ 'red_gam'))[0] NE -1 THEN BEGIN
				redGam = FLOAT((strparms[WHERE(strparms EQ 'red_gam')+1])[0])
				IF redGam LT 0.25 THEN BEGIN
					result = DIALOG_MESSAGE( $
						[ 'ERROR:  The RED GAMMA value in the recalled stretch file is', $
						'less than the allowed minimum value of 0.25 (this allowed', $
						'minimum value could be changed if necessary, please contact', $
						'the programmers.)  To use this stretch file, you must', $
						'manually edit the gamma value and try again to recall it.' ], /ERROR )
					RETURN
				ENDIF
				IF redGam GT 4.0 THEN BEGIN
					result = DIALOG_MESSAGE( $
						[ 'ERROR:  The RED GAMMA value in the recalled stretch file is', $
						'greater than the allowed maximum value of 4.0 (this allowed', $
						'maximum value could be changed if necessary, please contact', $
						'the programmers.)  To use this stretch file, you must', $
						'manually edit the gamma value and try again to recall it.' ], /ERROR )
					RETURN
				ENDIF
			ENDIF
		ENDIF
		;-----------------------------------------------------
		; Error check any GRN values to be restored.
		;-----------------------------------------------------
		IF (*statePtr).grnBandIdx GE 0 THEN BEGIN

			IF (WHERE(strparms EQ 'grn_min_limit'))[0] NE -1 THEN BEGIN
				grnMinLimit = DOUBLE( (strparms[WHERE(strparms EQ 'grn_min_limit')+1])[0] )
			ENDIF ELSE BEGIN
				WIDGET_CONTROL, (*statePtr).bottomLimitText[0], GET_VALUE = minLimit
				grnMinLimit = DOUBLE( minLimit[0] )
			ENDELSE
			IF (WHERE(strparms EQ 'grn_max_limit'))[0] NE -1 THEN BEGIN
				grnMaxLimit = DOUBLE( (strparms[WHERE(strparms EQ 'grn_max_limit')+1])[0] )
			ENDIF ELSE BEGIN
				WIDGET_CONTROL, (*statePtr).topLimitText[0], GET_VALUE = maxLimit
				grnMaxLimit = DOUBLE( maxLimit[0] )
			ENDELSE
			IF grnMinLimit GT grnMaxLimit THEN BEGIN
				result = DIALOG_MESSAGE( $
					[ 'The GRN LIMIT values in the recalled stretch file are possibly reversed.', $
					'To use this stretch file, you must manually edit the LIMIT values', $
					'in the stretch file and try again to recall it.' ], /INFORMATION )
				RETURN
			ENDIF

			IF (WHERE(strparms EQ 'grn_bot'))[0] NE -1 THEN BEGIN
				grnBot = FLOAT((strparms[WHERE(strparms EQ 'grn_bot')+1])[0])
				IF grnBot GT grnMaxLimit THEN BEGIN
					result = DIALOG_MESSAGE( $
						[ 'ERROR:  The GRN BOTTOM value in the recalled stretch file is', $
						'larger than the grn_max_limit.  This is invalid.  To use this', $
						'stretch file, you must manually edit the values and try again', $
						'to recall it.' ], /ERROR )
					RETURN
				ENDIF
;				IF grnBot GT grnMinLimit THEN BEGIN
;					grnBot = grnMinLimit
;					result = DIALOG_MESSAGE( $
;						[ 'FYI:  The GRN BOTTOM value in the recalled stretch file is inbetween', $
;						'the grn min and max slider values.  GRN BOTTOM is being reset to', $
;						'the grn min value.' ], /INFORMATION )
;				ENDIF
			ENDIF

			IF (WHERE(strparms EQ 'grn_top'))[0] NE -1 THEN BEGIN
				grnTop = FLOAT((strparms[WHERE(strparms EQ 'grn_top')+1])[0])
				IF grnTop LT grnMinLimit THEN BEGIN
					result = DIALOG_MESSAGE( $
						[ 'ERROR:  The GRN TOP value in the recalled stretch file is', $
						'less than the grn_min_limit.  This is invalid.  To use this', $
						'stretch file, you must manually edit the values and try again', $
						'to recall it.' ], /ERROR )
					RETURN
				ENDIF
;				IF grnTop LT grnMaxLimit THEN BEGIN
;					grnTop = grnMaxLimit
;					result = DIALOG_MESSAGE( $
;						[ 'FYI:  The GRN TOP value in the recalled stretch file is inbetween', $
;						'the grn min and max slider values.  GRN TOP is being reset to', $
;						'the grn max value.' ], /INFORMATION )
;				ENDIF
			ENDIF

			IF (WHERE(strparms EQ 'grn_gam'))[0] NE -1 THEN BEGIN
				grnGam = FLOAT((strparms[WHERE(strparms EQ 'grn_gam')+1])[0])
				IF grnGam LT 0.25 THEN BEGIN
					result = DIALOG_MESSAGE( $
						[ 'ERROR:  The GRN GAMMA value in the recalled stretch file is', $
						'less than the allowed minimum value of 0.25 (this allowed', $
						'minimum value could be changed if necessary, please contact', $
						'the programmers.)  To use this stretch file, you must', $
						'manually edit the gamma value and try again to recall it.' ], /ERROR )
					RETURN
				ENDIF
				IF grnGam GT 4.0 THEN BEGIN
					result = DIALOG_MESSAGE( $
						[ 'ERROR:  The GRN GAMMA value in the recalled stretch file is', $
						'greater than the allowed maximum value of 4.0 (this allowed', $
						'maximum value could be changed if necessary, please contact', $
						'the programmers.)  To use this stretch file, you must', $
						'manually edit the gamma value and try again to recall it.' ], /ERROR )
					RETURN
				ENDIF
			ENDIF
		ENDIF
		;-----------------------------------------------------
		; Error check any BLU values to be restored.
		;-----------------------------------------------------
		IF (*statePtr).bluBandIdx GE 0 THEN BEGIN

			IF (WHERE(strparms EQ 'blu_min_limit'))[0] NE -1 THEN BEGIN
				bluMinLimit = DOUBLE( (strparms[WHERE(strparms EQ 'blu_min_limit')+1])[0] )
			ENDIF ELSE BEGIN
				WIDGET_CONTROL, (*statePtr).bottomLimitText[0], GET_VALUE = minLimit
				bluMinLimit = DOUBLE( minLimit[0] )
			ENDELSE
			IF (WHERE(strparms EQ 'blu_max_limit'))[0] NE -1 THEN BEGIN
				bluMaxLimit = DOUBLE( (strparms[WHERE(strparms EQ 'blu_max_limit')+1])[0] )
			ENDIF ELSE BEGIN
				WIDGET_CONTROL, (*statePtr).topLimitText[0], GET_VALUE = maxLimit
				bluMaxLimit = DOUBLE( maxLimit[0] )
			ENDELSE
			IF bluMinLimit GT bluMaxLimit THEN BEGIN
				result = DIALOG_MESSAGE( $
					[ 'The BLU LIMIT values in the recalled stretch file are possibly reversed.', $
					'To use this stretch file, you must manually edit the LIMIT values', $
					'in the stretch file and try again to recall it.' ], /INFORMATION )
				RETURN
			ENDIF

			IF (WHERE(strparms EQ 'blu_bot'))[0] NE -1 THEN BEGIN
				bluBot = FLOAT((strparms[WHERE(strparms EQ 'blu_bot')+1])[0])
				IF bluBot GT bluMaxLimit THEN BEGIN
					result = DIALOG_MESSAGE( $
						[ 'ERROR:  The BLU BOTTOM value in the recalled stretch file is', $
						'larger than the blu_max_limit.  This is invalid.  To use this', $
						'stretch file, you must manually edit the values and try again', $
						'to recall it.' ], /ERROR )
					RETURN
				ENDIF
;				IF bluBot GT bluMinLimit THEN BEGIN
;					bluBot = bluMinLimit
;					result = DIALOG_MESSAGE( $
;						[ 'FYI:  The BLU BOTTOM value in the recalled stretch file is inbetween', $
;						'the blu min and max slider values.  BLU BOTTOM is being reset to', $
;						'the blu min value.' ], /INFORMATION )
;				ENDIF
			ENDIF

			IF (WHERE(strparms EQ 'blu_top'))[0] NE -1 THEN BEGIN
				bluTop = FLOAT((strparms[WHERE(strparms EQ 'blu_top')+1])[0])
				IF bluTop LT bluMinLimit THEN BEGIN
					result = DIALOG_MESSAGE( $
						[ 'ERROR:  The BLU TOP value in the recalled stretch file is', $
						'less than the blu_min_limit.  This is invalid.  To use this', $
						'stretch file, you must manually edit the values and try again', $
						'to recall it.' ], /ERROR )
					RETURN
				ENDIF
;				IF bluTop LT bluMaxLimit THEN BEGIN
;					bluTop = bluMaxLimit
;					result = DIALOG_MESSAGE( $
;						[ 'FYI:  The BLU TOP value in the recalled stretch file is inbetween', $
;						'the blu min and max slider values.  BLU TOP is being reset to', $
;						'the blu max value.' ], /INFORMATION )
;				ENDIF
			ENDIF

			IF (WHERE(strparms EQ 'blu_gam'))[0] NE -1 THEN BEGIN
				bluGam = FLOAT((strparms[WHERE(strparms EQ 'blu_gam')+1])[0])
				IF bluGam LT 0.25 THEN BEGIN
					result = DIALOG_MESSAGE( $
						[ 'ERROR:  The BLU GAMMA value in the recalled stretch file is', $
						'less than the allowed minimum value of 0.25 (this allowed', $
						'minimum value could be changed if necessary, please contact', $
						'the programmers.)  To use this stretch file, you must', $
						'manually edit the gamma value and try again to recall it.' ], /ERROR )
					RETURN
				ENDIF
				IF bluGam GT 4.0 THEN BEGIN
					result = DIALOG_MESSAGE( $
						[ 'ERROR:  The BLU GAMMA value in the recalled stretch file is', $
						'greater than the allowed maximum value of 4.0 (this allowed', $
						'maximum value could be changed if necessary, please contact', $
						'the programmers.)  To use this stretch file, you must', $
						'manually edit the gamma value and try again to recall it.' ], /ERROR )
					RETURN
				ENDIF
			ENDIF
		ENDIF


		;-----------------------------------------------------
		; Restore red stretch parameters.
		;-----------------------------------------------------
		IF (*statePtr).redBandIdx GE 0 THEN BEGIN
			;-----------------------------------------------------
			; Check that the band is active, and for the presence
			; of each stretch parameter for the band.  (It's not
			; necessary to have all 3 of the three parameters
			; present; only the present parms will be used.  This
			; can be handy when the user edits or constructs the
			; ASCII file with a text editor.)
			; The "strparms" variable is searched for matching
			; strings and the resulting index value is wrapping
			; rather than based on x and y.
			;-----------------------------------------------------
			IF (WHERE(strparms EQ 'red_min_limit'))[0] NE -1 THEN BEGIN
				red_min_limit = (strparms[WHERE(strparms EQ 'red_min_limit')+1])[0]
				*((*((*statePtr).bottomLimitPtr))[(*statePtr).redBandIdx]) = red_min_limit
				WIDGET_CONTROL, (*statePtr).bottomLimitText[0], SET_VALUE = red_min_limit
				WIDGET_CONTROL, (*statePtr).bottomLimitText[3], SET_VALUE = red_min_limit
				;---------------------------------------------------------------------
				; Reset the sliders.
				;---------------------------------------------------------------------
				bottomSlidersEvent,	{							$
							TOP	: event.top,					$
							ID	: (*statePtr).bottomSliders[0],			$
							HANDLER	: 'bottomSlidersEvent'				$
							}
			ENDIF
			IF (WHERE(strparms EQ 'red_max_limit'))[0] NE -1 THEN BEGIN
				red_max_limit = (strparms[WHERE(strparms EQ 'red_max_limit')+1])[0]
				*((*((*statePtr).topLimitPtr))[(*statePtr).redBandIdx]) = red_max_limit
				WIDGET_CONTROL, (*statePtr).topLimitText[0], SET_VALUE = red_max_limit
				WIDGET_CONTROL, (*statePtr).topLimitText[3], SET_VALUE = red_max_limit
				;---------------------------------------------------------------------
				; Reset the sliders.
				;---------------------------------------------------------------------
				topSlidersEvent,	{						$
							TOP	: event.top,				$
							ID	: (*statePtr).topSliders[0],		$
							HANDLER	: 'topSlidersEvent'			$
							}
			ENDIF

			IF (WHERE(strparms EQ 'red_bot'))[0] NE -1 THEN BEGIN
;PRINT,'BEFORE|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
;PRINT,'*((*((*statePtr).stretchDnBottomPtr))[(*statePtr).redBandIdx]) = ',*((*((*statePtr).stretchDnBottomPtr))[(*statePtr).redBandIdx])
;PRINT,'      |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
				*((*((*statePtr).stretchDnBottomPtr))[(*statePtr).redBandIdx]) = $
					FLOAT((strparms[WHERE(strparms EQ 'red_bot')+1])[0])
;PRINT,'AFTER |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
;PRINT,'*((*((*statePtr).stretchDnBottomPtr))[(*statePtr).redBandIdx]) = ',*((*((*statePtr).stretchDnBottomPtr))[(*statePtr).redBandIdx])
;PRINT,'      |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
			ENDIF
			IF (WHERE(strparms EQ 'red_top'))[0] NE -1 THEN BEGIN
;PRINT,'BEFORE|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
;PRINT,'*((*((*statePtr).stretchDnTopPtr   ))[(*statePtr).redBandIdx]) = ',*((*((*statePtr).stretchDnTopPtr   ))[(*statePtr).redBandIdx])
;PRINT,'      |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
				*((*((*statePtr).stretchDnTopPtr   ))[(*statePtr).redBandIdx]) = $
					FLOAT((strparms[WHERE(strparms EQ 'red_top')+1])[0])
;PRINT,'AFTER |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
;PRINT,'*((*((*statePtr).stretchDnTopPtr   ))[(*statePtr).redBandIdx]) = ',*((*((*statePtr).stretchDnTopPtr   ))[(*statePtr).redBandIdx])
;PRINT,'      |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
			ENDIF
			IF (WHERE(strparms EQ 'red_gam'))[0] NE -1 THEN $
				*((*((*statePtr).stretchDnGammaPtr ))[(*statePtr).redBandIdx]) = $
					FLOAT((strparms[WHERE(strparms EQ 'red_gam')+1])[0])

		ENDIF

		;-----------------------------------------------------
		; Restore grn stretch parameters.
		;-----------------------------------------------------
		IF (*statePtr).grnBandIdx GE 0 THEN BEGIN
			IF (WHERE(strparms EQ 'grn_min_limit'))[0] NE -1 THEN BEGIN
				grn_min_limit = (strparms[WHERE(strparms EQ 'grn_min_limit')+1])[0]
				*((*((*statePtr).bottomLimitPtr))[(*statePtr).grnBandIdx]) = grn_min_limit
				WIDGET_CONTROL, (*statePtr).bottomLimitText[1], SET_VALUE = grn_min_limit
				WIDGET_CONTROL, (*statePtr).bottomLimitText[4], SET_VALUE = grn_min_limit
				;---------------------------------------------------------------------
				; Reset the sliders.
				;---------------------------------------------------------------------
				bottomSlidersEvent,	{							$
							TOP	: event.top,					$
							ID	: (*statePtr).bottomSliders[1],			$
							HANDLER	: 'bottomSlidersEvent'				$
							}
			ENDIF
			IF (WHERE(strparms EQ 'grn_max_limit'))[0] NE -1 THEN BEGIN
				grn_max_limit = (strparms[WHERE(strparms EQ 'grn_max_limit')+1])[0]
				*((*((*statePtr).topLimitPtr))[(*statePtr).grnBandIdx]) = grn_max_limit
				WIDGET_CONTROL, (*statePtr).topLimitText[1], SET_VALUE = grn_max_limit
				WIDGET_CONTROL, (*statePtr).topLimitText[4], SET_VALUE = grn_max_limit
				;---------------------------------------------------------------------
				; Reset the sliders.
				;---------------------------------------------------------------------
				topSlidersEvent,	{						$
							TOP	: event.top,				$
							ID	: (*statePtr).topSliders[1],		$
							HANDLER	: 'topSlidersEvent'			$
							}
			ENDIF

			IF (WHERE(strparms EQ 'grn_bot'))[0] NE -1 THEN BEGIN
;PRINT,'BEFORE|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
;PRINT,'*((*((*statePtr).stretchDnBottomPtr))[(*statePtr).grnBandIdx]) = ',*((*((*statePtr).stretchDnBottomPtr))[(*statePtr).grnBandIdx])
;PRINT,'      |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
				*((*((*statePtr).stretchDnBottomPtr))[(*statePtr).grnBandIdx]) = $
					FLOAT((strparms[WHERE(strparms EQ 'grn_bot')+1])[0])
;PRINT,'AFTER |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
;PRINT,'*((*((*statePtr).stretchDnBottomPtr))[(*statePtr).grnBandIdx]) = ',*((*((*statePtr).stretchDnBottomPtr))[(*statePtr).grnBandIdx])
;PRINT,'      |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
			ENDIF
			IF (WHERE(strparms EQ 'grn_top'))[0] NE -1 THEN BEGIN
;PRINT,'BEFORE|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
;PRINT,'*((*((*statePtr).stretchDnTopPtr))[(*statePtr).grnBandIdx]) = ',*((*((*statePtr).stretchDnTopPtr))[(*statePtr).grnBandIdx])
;PRINT,'      |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
				*((*((*statePtr).stretchDnTopPtr   ))[(*statePtr).grnBandIdx]) = $
					FLOAT((strparms[WHERE(strparms EQ 'grn_top')+1])[0])
;PRINT,'AFTER |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
;PRINT,'*((*((*statePtr).stretchDnTopPtr))[(*statePtr).grnBandIdx]) = ',*((*((*statePtr).stretchDnTopPtr))[(*statePtr).grnBandIdx])
;PRINT,'      |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
			ENDIF
			IF (WHERE(strparms EQ 'grn_gam'))[0] NE -1 THEN $
				*((*((*statePtr).stretchDnGammaPtr ))[(*statePtr).grnBandIdx]) = $
					FLOAT((strparms[WHERE(strparms EQ 'grn_gam')+1])[0])

		ENDIF

		;-----------------------------------------------------
		; Restore blu stretch parameters.
		;-----------------------------------------------------
		IF (*statePtr).bluBandIdx GE 0 THEN BEGIN
			IF (WHERE(strparms EQ 'blu_min_limit'))[0] NE -1 THEN BEGIN
				blu_min_limit = (strparms[WHERE(strparms EQ 'blu_min_limit')+1])[0]
				*((*((*statePtr).bottomLimitPtr))[(*statePtr).bluBandIdx]) = blu_min_limit
				WIDGET_CONTROL, (*statePtr).bottomLimitText[2], SET_VALUE = blu_min_limit
				WIDGET_CONTROL, (*statePtr).bottomLimitText[5], SET_VALUE = blu_min_limit
				;---------------------------------------------------------------------
				; Reset the sliders.
				;---------------------------------------------------------------------
				bottomSlidersEvent,	{							$
							TOP	: event.top,					$
							ID	: (*statePtr).bottomSliders[2],			$
							HANDLER	: 'bottomSlidersEvent'				$
							}
			ENDIF
			IF (WHERE(strparms EQ 'blu_max_limit'))[0] NE -1 THEN BEGIN
				blu_max_limit = (strparms[WHERE(strparms EQ 'blu_max_limit')+1])[0]
				*((*((*statePtr).topLimitPtr))[(*statePtr).bluBandIdx]) = blu_max_limit
				WIDGET_CONTROL, (*statePtr).topLimitText[2], SET_VALUE = blu_max_limit
				WIDGET_CONTROL, (*statePtr).topLimitText[5], SET_VALUE = blu_max_limit
				;---------------------------------------------------------------------
				; Reset the sliders.
				;---------------------------------------------------------------------
				topSlidersEvent,	{						$
							TOP	: event.top,				$
							ID	: (*statePtr).topSliders[2],		$
							HANDLER	: 'topSlidersEvent'			$
							}
			ENDIF

			IF (WHERE(strparms EQ 'blu_bot'))[0] NE -1 THEN BEGIN
;PRINT,'BEFORE|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
;PRINT,'*((*((*statePtr).stretchDnBottomPtr))[(*statePtr).bluBandIdx]) = ',*((*((*statePtr).stretchDnBottomPtr))[(*statePtr).bluBandIdx])
;PRINT,'      |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
				*((*((*statePtr).stretchDnBottomPtr))[(*statePtr).bluBandIdx]) = $
					FLOAT((strparms[WHERE(strparms EQ 'blu_bot')+1])[0])
;PRINT,'AFTER |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
;PRINT,'*((*((*statePtr).stretchDnBottomPtr))[(*statePtr).bluBandIdx]) = ',*((*((*statePtr).stretchDnBottomPtr))[(*statePtr).bluBandIdx])
;PRINT,'      |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
			ENDIF
			IF (WHERE(strparms EQ 'blu_top'))[0] NE -1 THEN BEGIN
;PRINT,'BEFORE|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
;PRINT,'*((*((*statePtr).stretchDnTopPtr))[(*statePtr).bluBandIdx]) = ',*((*((*statePtr).stretchDnTopPtr))[(*statePtr).bluBandIdx])
;PRINT,'      |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
				*((*((*statePtr).stretchDnTopPtr   ))[(*statePtr).bluBandIdx]) = $
					FLOAT((strparms[WHERE(strparms EQ 'blu_top')+1])[0])
;PRINT,'AFTER |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
;PRINT,'*((*((*statePtr).stretchDnTopPtr))[(*statePtr).bluBandIdx]) = ',*((*((*statePtr).stretchDnTopPtr))[(*statePtr).bluBandIdx])
;PRINT,'      |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
			ENDIF
			IF (WHERE(strparms EQ 'blu_gam'))[0] NE -1 THEN $
				*((*((*statePtr).stretchDnGammaPtr ))[(*statePtr).bluBandIdx]) = $
					FLOAT((strparms[WHERE(strparms EQ 'blu_gam')+1])[0])

		ENDIF

		;-----------------------------------------------------
		; Re-display the data using the restored stretch.
		;-----------------------------------------------------
		waitbase = WIDGET_BASE( TITLE = 'Please wait...' )
		waitlbl1 = WIDGET_LABEL( waitbase, VALUE = '' )
		waitlbl2 = WIDGET_LABEL( waitbase, VALUE = 'Redisplaying data...' )
		waitlbl3 = WIDGET_LABEL( waitbase, VALUE = '' )
		WIDGET_CONTROL, waitbase, /REALIZE
;print,''
;print,''
;print,'recallStretchButtonEvent:  calling ((*statePtr).obj)->UpdateStretchDnInterface'
		;-----------------------------------------------------
		; Turn on the stretch recall keyword so that
		; UpdateStretchDnInterface will run.
		;-----------------------------------------------------
		IF ((*statePtr).obj)->UpdateStretchDnInterface( /STRETCH_RECALL ) THEN $
			((*statePtr).obj)->DisplayData

		;-----------------------------------------------------
		; Make sure the message window still exists (the user
		; may have x-killed it.)
		;-----------------------------------------------------
		IF WIDGET_INFO( waitbase, /VALID_ID ) THEN $
			WIDGET_CONTROL, waitbase, /DESTROY
;;;	ENDIF


END
; recallStretchButtonEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ excludeZeroesEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO excludeZeroesEvent, event
	WIDGET_CONTROL, event.top, GET_UVALUE = statePtr
	WIDGET_CONTROL, (*statePtr).excludeZeroesButton, $
		GET_VALUE = excludeZeroesButtonValue
	IF STRUPCASE( excludeZeroesButtonValue[0] ) EQ 'EXCLUDE ZEROES' THEN BEGIN
		WIDGET_CONTROL, (*statePtr).excludeZeroesButton, $
			SET_VALUE = 'Include Zeroes'
		(*statePtr).excludeZeroesFlag = 1	; Flag=1 means to EXclude zeroes.
	ENDIF ELSE BEGIN
		WIDGET_CONTROL, (*statePtr).excludeZeroesButton, $
			SET_VALUE = 'Exclude Zeroes'
		(*statePtr).excludeZeroesFlag = 0	; Flag=0 means to INclude zeroes.
	ENDELSE
END
; excludeZeroesEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ stretchDnBaseKillNotify @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO stretchDnBaseKillNotify, base
	WIDGET_CONTROL, base, GET_UVALUE = statePtr
	;---------------------------------------------------------------------
	; In this case, "newRealize" will not be utilized.  It is intended to
	; act as a flag indicating that a new WIDGET_DRAW was realized and may
	; have messed up any MAP_SET that may be active in the application
	; that instantiated this instance of GEOREF_IMAGE.
	;---------------------------------------------------------------------

;;;ckt,aug99	Had commented out this line of code.
;;;jrh,feb01	Reinstated it.
	IF OBJ_VALID( (*statePtr).obj ) THEN $
		newRealize = (*statePtr).obj->ToggleStretchDnInterface( 0 )

	PTR_FREE, statePtr
END
; stretchDnBaseKillNotify

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ stretchDnBaseEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;PRO stretchDnBaseEvent, event
FUNCTION stretchDnBaseEvent, event


;print,'inside stretchDnBaseEvent, event type = ',tag_names(event,/structure_name)
	geom = WIDGET_INFO( event.id, /GEOMETRY )

	IF STRUPCASE(TAG_NAMES( event, /STRUCTURE_NAME )) EQ 'GEOREFIMAGEHIST' THEN BEGIN
		e	= event
	ENDIF ELSE BEGIN
		;
		; This portion of the code will never be invoked because
		; only the APPLY button event handler is a function; therefore,
		; that event handler will be the only one passing events to this event handler.
		;
		e	= { GEOREFIMAGEHISTNOAPPLY, id:event.id, top:event.top, handler:event.handler, button_pressed:'unknown' }
	ENDELSE

	WIDGET_CONTROL, event.top, GET_UVALUE = statePtr
	WIDGET_CONTROL, (*statePtr).obj->GetDrawBaseID(), SEND_EVENT = e


END
; stretchDnBaseEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ rgbSelectEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO rgbSelectEvent, event
	WIDGET_CONTROL, event.id, GET_VALUE = whichColor
	WIDGET_CONTROL, event.top, GET_UVALUE = statePtr


	CASE 1 OF
		whichColor EQ 'R' : BEGIN
			;-----------------------------------------------------
			; For the case of each individual color, turn on that
			; color's controls and turn off the other 2 colors.
			; (This is only possible if the color is available in
			; the first place, otherwise the color selection
			; button would be disabled for any missing colors.)
			;-----------------------------------------------------
			WIDGET_CONTROL, (*statePtr).controlsBase[0], MAP = 1
			WIDGET_CONTROL, (*statePtr).controlsBase[3], MAP = 0
			IF (*statePtr).bluBandIdx GE 0 THEN $
				WIDGET_CONTROL, (*statePtr).controlsBase[2], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[5], MAP = 0
			IF (*statePtr).grnBandIdx GE 0 THEN $
				WIDGET_CONTROL, (*statePtr).controlsBase[1], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[4], MAP = 0

			(*statePtr).obj->stretchDnPlotHistogram, 0
		END
		whichColor EQ 'G' : BEGIN
			;-----------------------------------------------------
			; Ditto as per red.
			;-----------------------------------------------------
			WIDGET_CONTROL, (*statePtr).controlsBase[1], MAP = 1
			WIDGET_CONTROL, (*statePtr).controlsBase[4], MAP = 0
			IF (*statePtr).redBandIdx GE 0 THEN BEGIN
				WIDGET_CONTROL, (*statePtr).controlsBase[0], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[3], MAP = 0
			ENDIF
			IF (*statePtr).bluBandIdx GE 0 THEN BEGIN
				WIDGET_CONTROL, (*statePtr).controlsBase[2], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[5], MAP = 0
			ENDIF

			(*statePtr).obj->stretchDnPlotHistogram, 1
		END
		whichColor EQ 'B' : BEGIN
			;-----------------------------------------------------
			; Ditto as per red.
			;-----------------------------------------------------
			WIDGET_CONTROL, (*statePtr).controlsBase[2], MAP = 1
			WIDGET_CONTROL, (*statePtr).controlsBase[5], MAP = 0
			IF (*statePtr).grnBandIdx GE 0 THEN BEGIN
				WIDGET_CONTROL, (*statePtr).controlsBase[1], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[4], MAP = 0
			ENDIF
			IF (*statePtr).redBandIdx GE 0 THEN BEGIN
				WIDGET_CONTROL, (*statePtr).controlsBase[0], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[3], MAP = 0
			ENDIF

			(*statePtr).obj->stretchDnPlotHistogram, 2
		END
		whichColor EQ 'Rgb' : BEGIN
			;-----------------------------------------------------
			; For the case of each individual color, turn on that
			; color's controls and turn off the other 2 colors.
			; (This is only possible if the color is available in
			; the first place, otherwise the color selection
			; button would be disabled for any missing colors.)
			;-----------------------------------------------------
			WIDGET_CONTROL, (*statePtr).controlsBase[0], MAP = 0
			WIDGET_CONTROL, (*statePtr).controlsBase[3], MAP = 1
			IF (*statePtr).bluBandIdx GE 0 THEN BEGIN
				WIDGET_CONTROL, (*statePtr).controlsBase[2], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[5], MAP = 0
			ENDIF
			IF (*statePtr).grnBandIdx GE 0 THEN BEGIN
				WIDGET_CONTROL, (*statePtr).controlsBase[1], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[4], MAP = 0
			ENDIF

			(*statePtr).obj->stretchDnPlotHistogram, 3
		END
		whichColor EQ 'rGb' : BEGIN
			;-----------------------------------------------------
			; Ditto as per red.
			;-----------------------------------------------------
			WIDGET_CONTROL, (*statePtr).controlsBase[1], MAP = 0
			WIDGET_CONTROL, (*statePtr).controlsBase[4], MAP = 1
			IF (*statePtr).redBandIdx GE 0 THEN BEGIN
				WIDGET_CONTROL, (*statePtr).controlsBase[0], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[3], MAP = 0
			ENDIF
			IF (*statePtr).bluBandIdx GE 0 THEN BEGIN
				WIDGET_CONTROL, (*statePtr).controlsBase[2], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[5], MAP = 0
			ENDIF

			(*statePtr).obj->stretchDnPlotHistogram, 4
		END
		whichColor EQ 'rgB' : BEGIN
			;-----------------------------------------------------
			; Ditto as per red.
			;-----------------------------------------------------
			WIDGET_CONTROL, (*statePtr).controlsBase[2], MAP = 0
			WIDGET_CONTROL, (*statePtr).controlsBase[5], MAP = 1
			IF (*statePtr).grnBandIdx GE 0 THEN BEGIN
				WIDGET_CONTROL, (*statePtr).controlsBase[1], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[4], MAP = 0
			ENDIF
			IF (*statePtr).redBandIdx GE 0 THEN BEGIN
				WIDGET_CONTROL, (*statePtr).controlsBase[0], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[3], MAP = 0
			ENDIF

			(*statePtr).obj->stretchDnPlotHistogram, 5
		END
	ENDCASE


END
; rgbSelectEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::CalculateHistogram @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::CalculateHistogram, img_idx, rgb_plane_idx

	IF img_idx LT 0 THEN RETURN

	;=======================================================================
	; rgb_plane_idx is 0, 1, 2, depending on which plane we're dealing with
	; img_idx is the index of the image that is to be placed in the current
	; plane
	;=======================================================================
;print,'*(SELF.calc_hist_for_displayed_data_ptr) = ',*(SELF.calc_hist_for_displayed_data_ptr)
	IF *(SELF.calc_hist_for_displayed_data_ptr) THEN BEGIN
		xs		= (*(SELF.dataXS))[img_idx]
		xe		= xs + (*(SELF.dataNX))[img_idx] - 1
		ys		= (*(SELF.dataYS))[img_idx]
		ye		= ys + (*(SELF.dataNY))[img_idx] - 1
		min_data_val	= *((*(SELF.dataMinValPtr))[img_idx])
		max_data_val	= *((*(SELF.dataMaxValPtr))[img_idx])
		bin_sz		= SELF->Calculate_histBinSize(img_idx)
	ENDIF ELSE BEGIN
		xs		= 0L
		xe		= (*((*(SELF.dataPtr))[img_idx]))->GetImageWidth() - 1L
		ys		= 0L
		ye		= (*((*(SELF.dataPtr))[img_idx]))->GetImageHeight() - 1L
		min_data_val	= *((*(SELF.min_dataPtr))[img_idx])
		max_data_val	= *((*(SELF.max_dataPtr))[img_idx])
		bin_sz		= SELF->Calculate_histBinSize(img_idx, /FULL_HIST)
	ENDELSE
;print,'xs,xe,ys,ye = ',xs,xe,ys,ye
;print,'PTR_VALID((*(SELF.dataPtr))[img_idx])? = ',PTR_VALID((*(SELF.dataPtr))[img_idx])
;print,'bin_sz,min_data_val,max_data_val=',bin_sz,min_data_val,max_data_val
	hst	= (*((*(SELF.dataPtr))[img_idx]))->CalculateHistogram(		$
				xs, xe, ys, ye, 				$
				BINSIZE = bin_sz,				$
				MIN = min_data_val, MAX = max_data_val )

	IF hst[0] EQ (-1) THEN BEGIN
		;------------------------------------------------------------------------
		; No valid data.
		; Turn off histogram, if it currently exists.
		;------------------------------------------------------------------------
		IF WIDGET_INFO(SELF.stretchDnBase, /VALID_ID) THEN		$
			newRealize	= SELF->ToggleStretchDnInterface( 0 )

		RETURN
	ENDIF

	PTR_FREE, SELF.rgbHistogramPtrarr[rgb_plane_idx]
	SELF.rgbHistogramPtrarr[rgb_plane_idx] = PTR_NEW( hst )

	SELF->stretchDnPlotHistogram, rgb_plane_idx, /NEWHIST
	SELF->stretchDnPlotHistogram, rgb_plane_idx + 3, /NEWHIST

END
; GEOREF_IMAGE::CalculateHistogram

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::UpdateStretchDnInterface @@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::UpdateStretchDnInterface, STRETCH_RECALL = stretch_recall, UPDATE_LIMITS = update_limits

	IF KEYWORD_SET( stretch_recall ) THEN	$
		stretchRecall = 1		$
	ELSE					$
		stretchRecall = 0

	IF KEYWORD_SET( update_limits ) THEN	$
		updateLimits = 1		$
	ELSE					$
		updateLimits = 0

	IF NOT WIDGET_INFO( SELF.stretchDnBase, /VALID_ID ) THEN BEGIN
		RETURN, 1
	ENDIF

	WIDGET_CONTROL, SELF.stretchDnBase, GET_UVALUE = statePtr

	IF updateLimits THEN $
		SELF -> InitializeDataLimits, statePtr

;print,''
;print,'GEOREF_IMAGE::UpdateStretchDnInterface:  InitializeDataMinMax'
	SELF -> InitializeDataMinMax

;print,''
;print,'GEOREF_IMAGE::UpdateStretchDnInterface   **CHECKING RED if-statement**'
;print,'SELF.redBandIdx = ',SELF.redBandIdx
;print,'(*statePtr).redBandIdx = ',(*statePtr).redBandIdx
	IF ( SELF.redBandIdx NE (*statePtr).redBandIdx ) OR stretchRecall OR updateLimits THEN BEGIN
;print,''
;print,'GEOREF_IMAGE::UpdateStretchDnInterface   **SELF.redBandIdx NE (*statePtr).redBandIdx**'
		(*statePtr).redBandIdx	= SELF.redBandIdx
		IF SELF.redBandIdx LT 0 THEN BEGIN
;print,''
;print,'GEOREF_IMAGE::UpdateStretchDnInterface   **SELF.redBandIdx LT 0**'
			WIDGET_CONTROL, (*statePtr).controlsBase[0], SENSITIVE = 0
			WIDGET_CONTROL, (*statePtr).controlsBase[3], SENSITIVE = 0
			WIDGET_CONTROL, (*statePtr).rgb_Rgb_rGb_rgB_buttons[0], SENSITIVE = 0
			WIDGET_CONTROL, (*statePtr).rgb_Rgb_rGb_rgB_buttons[3], SENSITIVE = 0
		ENDIF ELSE BEGIN
;print,''
;print,'GEOREF_IMAGE::UpdateStretchDnInterface   **SELF.redBandIdx LT 0 <ELSE PORTION>**'
			redMin			= DOUBLE( *((*(SELF.bottomLimitPtr))[SELF.redBandIdx]) )
			redMax			= DOUBLE( *((*(SELF.topLimitPtr))[SELF.redBandIdx]) )
			redBottom		= *((*(SELF.stretchDnBottomPtr))[SELF.redBandIdx])
			redTop			= *((*(SELF.stretchDnTopPtr))[SELF.redBandIdx])
;print,'|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\'
;print,'GEOREF_IMAGE::UpdateStretchDnInterface'
;print,'redMin = ',redMin
;print,'redMax = ',redMax
;print,'redBottom = ',redBottom
;print,'redTop = ',redTop
;print,'|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\'

			IF redTop LT redMin OR redBottom GT redMax THEN BEGIN
				result = DIALOG_MESSAGE( [						$
					'You are (probably) trying to recall a stored stretch',		$
					'that contains BOTTOM and TOP values that are entirely ',	$
					'outside of the range of the data in one of the viewplanes.',	$
					'',								$
					'Diagnostics:',							$
					'',								$
					'red',								$
					'',								$
					'Data values [ redMin, redMax ] = ['				$
					+ STRTRIM( redMin, 2 ) + ','					$
					+ STRTRIM( redMax, 2 ) + ']',					$
					'',								$
					'Recall values [ redBottom, redTop ] = ['			$
					+ STRTRIM( DOUBLE( redBottom ), 2 ) + ','			$
					+ STRTRIM( DOUBLE( redTop ), 2 ) + ']',				$
					'',								$
					'RETURNing' ],							$
					/INFORMATION )

				RETURN, 0
			ENDIF

			;
			; SliderValue is in slider coordinates, integers between 0 and 1000 in this case.
			;
			redBottomSliderValue 	= LONG( 0.5 + 1000.0 * ABS( FLOAT( redBottom - redMin ) / FLOAT( redMax - redMin ) ) )
			redTopSliderValue 	= LONG( 0.5 + 1000.0 * ABS( FLOAT( redTop - redMin ) / FLOAT( redMax - redMin ) ) )
			redGammaSliderValue	= *((*(SELF.stretchDnGammaPtr))[SELF.redBandIdx])

			newTopValue = redTopSliderValue / 1000.0 * ( FLOAT( redMax ) - FLOAT( redMin ) ) + FLOAT( redMin )

			PTR_FREE, (*((*statePtr).stretchDnTopPtr))[(*statePtr).redBandIdx]
			(*((*statePtr).stretchDnTopPtr))[(*statePtr).redBandIdx]	= PTR_NEW(newTopValue)

			newBottomValue	= redBottomSliderValue / 1000.0 * ( FLOAT(redMax ) - FLOAT( redMin ) ) + FLOAT( redMin )

			PTR_FREE, (*((*statePtr).stretchDnBottomPtr))[(*statePtr).redBandIdx]
			(*((*statePtr).stretchDnBottomPtr))[(*statePtr).redBandIdx]	= PTR_NEW(newBottomValue)

			FOR i = 0, 3, 3 DO BEGIN
				WIDGET_CONTROL, (*statePtr).bottomLimitText[i], SET_VALUE = STRTRIM( redMin, 2 )
				WIDGET_CONTROL, (*statePtr).bottomLabels[i],		$
					SET_VALUE = STRTRIM( *((*(SELF.stretchDnBottomPtr))[SELF.redBandIdx]), 2 )
				WIDGET_CONTROL, (*statePtr).bottomSliders[i], SET_VALUE = redBottomSliderValue
				WIDGET_CONTROL, (*statePtr).topLimitText[i], SET_VALUE = STRTRIM( redMax, 2 )
				WIDGET_CONTROL, (*statePtr).topLabels[i],		$
					SET_VALUE = STRTRIM( *((*(SELF.stretchDnTopPtr))[SELF.redBandIdx]), 2 )
				WIDGET_CONTROL, (*statePtr).topSliders[i], SET_VALUE = redTopSliderValue
				WIDGET_CONTROL, (*statePtr).gammaLabels[i], SET_VALUE = STRTRIM(redGammaSliderValue,2)
				WIDGET_CONTROL, (*statePtr).gammaSliders[i], SET_VALUE = FIX(redGammaSliderValue*100.0)
;print,'|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-'
;print,'GEOREF_IMAGE::UpdateStretchDnInterface   **FOR LOOP**   red   i = ',i
;print,'GEOREF_IMAGE::UpdateStretchDnInterface:  diagnostics...'
;print,'SELF.redBandIdx = ',SELF.redBandIdx
;print,'bottomLabel:   STRTRIM( *((*(SELF.stretchDnBottomPtr))[SELF.redBandIdx]), 2 ) = ',	$
;	STRTRIM( *((*(SELF.stretchDnBottomPtr))[SELF.redBandIdx]), 2 )
;print,'bottomSlider:  redBottomSliderValue = ',redBottomSliderValue
;print,'topLabel:      STRTRIM( *((*(SELF.stretchDnTopPtr))[SELF.redBandIdx]), 2 ) = ',		$
;	STRTRIM( *((*(SELF.stretchDnTopPtr))[SELF.redBandIdx]), 2 )
;print,'topSlider:     redTopSliderValue = ',redTopSliderValue
;print,'gammaLabel:    STRTRIM(redGammaSliderValue,2) = ',STRTRIM(redGammaSliderValue,2)
;print,'gammaSlider:   FIX(redGammaSliderValue*100.0) = ',FIX(redGammaSliderValue*100.0)
;print,'SELF.redBandIdx = ',SELF.redBandIdx
;print,'|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-'
			ENDFOR

			WIDGET_CONTROL, (*statePtr).controlsBase[0], SENSITIVE = 1
			WIDGET_CONTROL, (*statePtr).controlsBase[3], SENSITIVE = 1

			WIDGET_CONTROL, (*statePtr).rgb_Rgb_rGb_rgB_buttons[0], SENSITIVE = 1
			WIDGET_CONTROL, (*statePtr).rgb_Rgb_rGb_rgB_buttons[3], SENSITIVE = 1

			SELF->CalculateHistogram, SELF.redBandIdx, 0

		ENDELSE
	ENDIF

	IF SELF.grnBandIdx NE (*statePtr).grnBandIdx OR stretchRecall or updateLimits THEN BEGIN
		(*statePtr).grnBandIdx	= SELF.grnBandIdx
		IF SELF.grnBandIdx LT 0 THEN BEGIN
			WIDGET_CONTROL, (*statePtr).controlsBase[1], SENSITIVE = 0
			WIDGET_CONTROL, (*statePtr).controlsBase[4], SENSITIVE = 0
			WIDGET_CONTROL, (*statePtr).rgb_Rgb_rGb_rgB_buttons[1], SENSITIVE = 0
			WIDGET_CONTROL, (*statePtr).rgb_Rgb_rGb_rgB_buttons[4], SENSITIVE = 0
		ENDIF ELSE BEGIN
			grnMin			= DOUBLE( *((*(SELF.bottomLimitPtr))[SELF.grnBandIdx]) )
			grnMax			= DOUBLE( *((*(SELF.topLimitPtr))[SELF.grnBandIdx]) )
			grnBottom		= *((*(SELF.stretchDnBottomPtr))[SELF.grnBandIdx])
			grnTop			= *((*(SELF.stretchDnTopPtr))[SELF.grnBandIdx])
;print,'|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\'
;print,'GEOREF_IMAGE::UpdateStretchDnInterface'
;print,'grnMin = ',grnMin
;print,'grnMax = ',grnMax
;print,'grnBottom = ',grnBottom
;print,'grnTop = ',grnTop
;print,'|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\'

			IF grnTop LT grnMin OR grnBottom GT grnMax THEN BEGIN
				result = DIALOG_MESSAGE( [							$
					'You are (probably) trying to recall a stored stretch',			$
					'that contains BOTTOM and TOP values that are entirely ',		$
					'outside of the range of the data in one of the viewplanes.',		$
					'',									$
					'Diagnostics:',								$
					'',									$
					'grn',									$
					'',									$
					'Data values [ redMin, redMax ] = ['					$
					+ STRTRIM( grnMin, 2 ) + ','						$
					+ STRTRIM( grnMax, 2 ) + ']',						$
					'',									$
					'Recall values [ grnBottom, grnTop ] = ['				$
					+ STRTRIM( DOUBLE( grnBottom ), 2 ) + ','				$
					+ STRTRIM( DOUBLE( grnTop ), 2 ) + ']',					$
					'',									$
					'RETURNing' ],								$
					/INFORMATION )

				RETURN, 0
			ENDIF

			;
			; SliderValue is in slider coordinates, integers between 0 and 1000 in this case.
			;
			grnBottomSliderValue 	= LONG( 0.5 + 1000.0 * ABS( FLOAT( grnBottom - grnMin ) / FLOAT( grnMax - grnMin ) ) )
			grnTopSliderValue 	= LONG( 0.5 + 1000.0 * ABS( FLOAT( grnTop - grnMin ) / FLOAT( grnMax - grnMin ) ) )
			grnGammaSliderValue	= *((*(SELF.stretchDnGammaPtr))[SELF.grnBandIdx])

			newTopValue = grnTopSliderValue / 1000.0 * ( FLOAT( grnMax ) - FLOAT( grnMin ) ) + FLOAT( grnMin )
			PTR_FREE, (*((*statePtr).stretchDnTopPtr))[(*statePtr).grnBandIdx]
			(*((*statePtr).stretchDnTopPtr))[(*statePtr).grnBandIdx]	= PTR_NEW(newTopValue)

			newBottomValue	= grnBottomSliderValue / 1000.0 * ( FLOAT(grnMax ) - FLOAT( grnMin ) ) + FLOAT( grnMin )
			PTR_FREE, (*((*statePtr).stretchDnBottomPtr))[(*statePtr).grnBandIdx]
			(*((*statePtr).stretchDnBottomPtr))[(*statePtr).grnBandIdx]	= PTR_NEW(newBottomValue)

			FOR i = 1, 4, 3 DO BEGIN
				WIDGET_CONTROL, (*statePtr).bottomLimitText[i], SET_VALUE = STRTRIM( grnMin, 2 )
				WIDGET_CONTROL, (*statePtr).bottomLabels[i],		$
					SET_VALUE = STRTRIM( *((*(SELF.stretchDnBottomPtr))[SELF.grnBandIdx]),2 )
				WIDGET_CONTROL, (*statePtr).bottomSliders[i], SET_VALUE = grnBottomSliderValue
				WIDGET_CONTROL, (*statePtr).topLimitText[i], SET_VALUE = STRTRIM( grnMax, 2 )
				WIDGET_CONTROL, (*statePtr).topLabels[i],		$
					SET_VALUE = STRTRIM( *((*(SELF.stretchDnTopPtr))[SELF.grnBandIdx]),2 )
				WIDGET_CONTROL, (*statePtr).topSliders[i], SET_VALUE = grnTopSliderValue
				WIDGET_CONTROL, (*statePtr).gammaLabels[i], SET_VALUE = STRTRIM(grnGammaSliderValue,2)
				WIDGET_CONTROL, (*statePtr).gammaSliders[i], SET_VALUE = FIX(grnGammaSliderValue*100.0)
;print,'|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-'
;print,'GEOREF_IMAGE::UpdateStretchDnInterface   **FOR LOOP**   grn   i = ',i
;print,'GEOREF_IMAGE::UpdateStretchDnInterface:  diagnostics...'
;print,'SELF.grnBandIdx = ',SELF.grnBandIdx
;print,'bottomLabel:   STRTRIM( *((*(SELF.stretchDnBottomPtr))[SELF.grnBandIdx]),2 ) = ',	$
;	STRTRIM( *((*(SELF.stretchDnBottomPtr))[SELF.grnBandIdx]),2 )
;print,'bottomSlider:  grnBottomSliderValue = ',grnBottomSliderValue
;print,'topLabel:      STRTRIM( *((*(SELF.stretchDnTopPtr))[SELF.grnBandIdx]),2 ) = ',		$
;	STRTRIM( *((*(SELF.stretchDnTopPtr))[SELF.grnBandIdx]),2 )
;print,'topSlider:     grnTopSliderValue = ',grnTopSliderValue
;print,'gammaLabel:    STRTRIM(grnGammaSliderValue,2) = ',STRTRIM(grnGammaSliderValue,2)
;print,'gammaSlider:   FIX(grnGammaSliderValue*100.0) = ',FIX(grnGammaSliderValue*100.0)
;print,'SELF.grnBandIdx = ',SELF.grnBandIdx
;print,'|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-'
			ENDFOR

			WIDGET_CONTROL, (*statePtr).controlsBase[1], SENSITIVE = 1
			WIDGET_CONTROL, (*statePtr).controlsBase[4], SENSITIVE = 1

			WIDGET_CONTROL, (*statePtr).rgb_Rgb_rGb_rgB_buttons[1], SENSITIVE = 1
			WIDGET_CONTROL, (*statePtr).rgb_Rgb_rGb_rgB_buttons[4], SENSITIVE = 1

			SELF->CalculateHistogram, SELF.grnBandIdx, 1
		ENDELSE
	ENDIF

	IF SELF.bluBandIdx NE (*statePtr).bluBandIdx OR stretchRecall or updateLimits THEN BEGIN
		(*statePtr).bluBandIdx	= SELF.bluBandIdx
		IF SELF.bluBandIdx LT 0 THEN BEGIN
			WIDGET_CONTROL, (*statePtr).controlsBase[2], SENSITIVE = 0
			WIDGET_CONTROL, (*statePtr).controlsBase[5], SENSITIVE = 0
			WIDGET_CONTROL, (*statePtr).rgb_Rgb_rGb_rgB_buttons[2], SENSITIVE = 0
			WIDGET_CONTROL, (*statePtr).rgb_Rgb_rGb_rgB_buttons[5], SENSITIVE = 0
		ENDIF ELSE BEGIN
			bluMin			= DOUBLE( *((*(SELF.bottomLimitPtr))[SELF.bluBandIdx]) )
			bluMax			= DOUBLE( *((*(SELF.topLimitPtr))[SELF.bluBandIdx]) )
			bluBottom		= *((*(SELF.stretchDnBottomPtr))[SELF.bluBandIdx])
			bluTop			= *((*(SELF.stretchDnTopPtr))[SELF.bluBandIdx])
;print,'|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\'
;print,'GEOREF_IMAGE::UpdateStretchDnInterface'
;print,'bluMin = ',bluMin
;print,'bluMax = ',bluMax
;print,'bluBottom = ',bluBottom
;print,'bluTop = ',bluTop
;print,'|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\'

			IF bluTop LT bluMin OR bluBottom GT bluMax THEN BEGIN
				result = DIALOG_MESSAGE( [							$
					'You are (probably) trying to recall a stored stretch',			$
					'that contains BOTTOM and TOP values that are entirely ',		$
					'outside of the range of the data in one of the viewplanes.',		$
					'',									$
					'Diagnostics:',								$
					'',									$
					'blu',									$
					'',									$
					'Data values [ bluMin, bluMax ] = ['					$
					+ STRTRIM( bluMin, 2 ) + ','						$
					+ STRTRIM( bluMax, 2 ) + ']',						$
					'',									$
					'Recall values [ bluBottom, bluTop ] = ['				$
					+ STRTRIM( DOUBLE( bluBottom ), 2 ) + ','				$
					+ STRTRIM( DOUBLE( bluTop ), 2 ) + ']',					$
					'',									$
					'RETURNing' ],								$
					/INFORMATION )

				RETURN, 0
			ENDIF

			;
			; SliderValue is in slider coordinates, integers between 0 and 1000 in this case.
			;
			bluBottomSliderValue 	= LONG( 0.5 + 1000.0 * ABS( FLOAT( bluBottom - bluMin ) / FLOAT( bluMax - bluMin ) ) )
			bluTopSliderValue 	= LONG( 0.5 + 1000.0 * ABS( FLOAT( bluTop - bluMin ) / FLOAT( bluMax - bluMin ) ) )
			bluGammaSliderValue	= *((*(SELF.stretchDnGammaPtr))[SELF.bluBandIdx])

			newTopValue = bluTopSliderValue / 1000.0 * ( FLOAT( bluMax ) - FLOAT( bluMin ) ) + FLOAT( bluMin )
			PTR_FREE, (*((*statePtr).stretchDnTopPtr))[(*statePtr).bluBandIdx]
			(*((*statePtr).stretchDnTopPtr))[(*statePtr).bluBandIdx]	= PTR_NEW(newTopValue)

			newBottomValue	= bluBottomSliderValue / 1000.0 * ( FLOAT( bluMax ) - FLOAT( bluMin ) ) + FLOAT( bluMin )
			PTR_FREE, (*((*statePtr).stretchDnBottomPtr))[(*statePtr).bluBandIdx]
			(*((*statePtr).stretchDnBottomPtr))[(*statePtr).bluBandIdx]	= PTR_NEW(newBottomValue)

			FOR i = 2, 5, 3 DO BEGIN
				WIDGET_CONTROL, (*statePtr).bottomLimitText[i], SET_VALUE = STRTRIM( bluMin, 2 )
				WIDGET_CONTROL, (*statePtr).bottomLabels[i],		$
					SET_VALUE = STRTRIM( *((*(SELF.stretchDnBottomPtr))[SELF.bluBandIdx]),2 )
				WIDGET_CONTROL, (*statePtr).bottomSliders[i], SET_VALUE = bluBottomSliderValue
				WIDGET_CONTROL, (*statePtr).topLimitText[i], SET_VALUE = STRTRIM( bluMax, 2 )
				WIDGET_CONTROL, (*statePtr).topLabels[i],		$
					SET_VALUE = STRTRIM( *((*(SELF.stretchDnTopPtr))[SELF.bluBandIdx]),2 )
				WIDGET_CONTROL, (*statePtr).topSliders[i], SET_VALUE = bluTopSliderValue
				WIDGET_CONTROL, (*statePtr).gammaLabels[i], SET_VALUE = STRTRIM(bluGammaSliderValue,2)
				WIDGET_CONTROL, (*statePtr).gammaSliders[i], SET_VALUE = FIX(bluGammaSliderValue*100.0)
;print,'|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-'
;print,'GEOREF_IMAGE::UpdateStretchDnInterface   **FOR LOOP**   blu   i = ',i
;print,'GEOREF_IMAGE::UpdateStretchDnInterface:  diagnostics...'
;print,'SELF.bluBandIdx = ',SELF.bluBandIdx
;print,'bottomLabel:   STRTRIM( *((*(SELF.stretchDnBottomPtr))[SELF.bluBandIdx]),2 ) = ',	$
;	STRTRIM( *((*(SELF.stretchDnBottomPtr))[SELF.bluBandIdx]),2 )
;print,'bottomSlider:  bluBottomSliderValue = ',bluBottomSliderValue
;print,'topLabel:      STRTRIM( *((*(SELF.stretchDnTopPtr))[SELF.bluBandIdx]),2 ) = ',		$
;	STRTRIM( *((*(SELF.stretchDnTopPtr))[SELF.bluBandIdx]),2 )
;print,'topSlider:     bluTopSliderValue = ',bluTopSliderValue
;print,'gammaLabel:    STRTRIM(bluGammaSliderValue,2) = ',STRTRIM(bluGammaSliderValue,2)
;print,'gammaSlider:   FIX(bluGammaSliderValue*100.0) = ',FIX(bluGammaSliderValue*100.0)
;print,'SELF.bluBandIdx = ',SELF.bluBandIdx
;print,'|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-'
			ENDFOR

			WIDGET_CONTROL, (*statePtr).controlsBase[2], SENSITIVE = 1
			WIDGET_CONTROL, (*statePtr).controlsBase[5], SENSITIVE = 1

			WIDGET_CONTROL, (*statePtr).rgb_Rgb_rGb_rgB_buttons[2], SENSITIVE = 1
			WIDGET_CONTROL, (*statePtr).rgb_Rgb_rGb_rgB_buttons[5], SENSITIVE = 1

			SELF->CalculateHistogram, SELF.bluBandIdx, 2

		ENDELSE
	ENDIF

	IF NOT stretchRecall THEN BEGIN
		CASE 1 OF
			(*statePtr).redBandIdx GE 0: BEGIN
				WIDGET_CONTROL, (*statePtr).controlsBase[0], MAP = 1
				WIDGET_CONTROL, (*statePtr).controlsBase[1], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[2], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[3], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[4], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[5], MAP = 0
				END
			(*statePtr).grnBandIdx GE 0: BEGIN
				WIDGET_CONTROL, (*statePtr).controlsBase[0], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[1], MAP = 1
				WIDGET_CONTROL, (*statePtr).controlsBase[2], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[3], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[4], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[5], MAP = 0
				END
			(*statePtr).bluBandIdx GE 0: BEGIN
				WIDGET_CONTROL, (*statePtr).controlsBase[0], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[1], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[2], MAP = 1
				WIDGET_CONTROL, (*statePtr).controlsBase[3], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[4], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[5], MAP = 0
				END
			ELSE: BEGIN
				WIDGET_CONTROL, (*statePtr).controlsBase[0], MAP = 1
				WIDGET_CONTROL, (*statePtr).controlsBase[1], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[2], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[3], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[4], MAP = 0
				WIDGET_CONTROL, (*statePtr).controlsBase[5], MAP = 0
				END
		ENDCASE
	ENDIF

	;---------------------------------------------------------------------
	; Update axis values on colorbar, if colorbar exists.
	;---------------------------------------------------------------------
	IF WIDGET_INFO( SELF.colorbar_tlb, /VALID_ID ) THEN $
		colorbar_redraw, TLB = SELF.drawBase

	RETURN, 1

END
; GEOREF_IMAGE::UpdateStretchDnInterface

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ dismissButtonEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO dismissButtonEvent, event
	WIDGET_CONTROL, event.top, GET_UVALUE = statePtr
	newRealize	= (*statePtr).obj->ToggleStretchDnInterface( 0 )
END
; dismissButtonEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ histTypeButtonEvent @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO histTypeButtonEvent, event
	WIDGET_CONTROL, event.top, GET_UVALUE = statePtr
	*((*statePtr).calc_hist_for_displayed_data_ptr)	= event.select

	((*statePtr).obj)->CalculateHistogram,(*statePtr).redBandIdx,0
	((*statePtr).obj)->CalculateHistogram,(*statePtr).grnBandIdx,1
	((*statePtr).obj)->CalculateHistogram,(*statePtr).bluBandIdx,2
END
; histTypeButtonEvent

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ get_stretchDn_base @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION get_stretchDn_base, obj, min_dataPtr, max_dataPtr, stretchDnBottomPtr,		$
				stretchDnTopPtr, stretchDnGammaPtr,			$
				bottomLimitPtr, topLimitPtr,				$
				redBandIdx, grnBandIdx, bluBandIdx,			$
				calc_hist_for_displayed_data_ptr,			$
				GROUP_LEADER = GROUP_LEADER,				$
				XSIZE = XSIZE,						$
				TITLE_PREFIX = title_prefix,				$
				_EXTRA = extra

	;---------------------------------------------------------------------
	; Set up top-level histogram base
	;---------------------------------------------------------------------
	IF KEYWORD_SET( GROUP_LEADER ) THEN BEGIN
		tlb	= WIDGET_BASE( /COLUMN, /FLOATING,				$
					MAP = 0,					$
					GROUP_LEADER = GROUP_LEADER,			$
					KILL_NOTIFY = 'stretchDnBaseKillNotify',	$
					/TLB_SIZE_EVENTS,				$
					EVENT_FUNC = 'stretchDnBaseEvent',		$
					TITLE = title_prefix,				$
					_EXTRA = extra )
	ENDIF ELSE BEGIN
		tlb	= WIDGET_BASE( /COLUMN,						$
					MAP = 0,					$
					KILL_NOTIFY = 'stretchDnBaseKillNotify',	$
					/TLB_SIZE_EVENTS,				$
					EVENT_FUNC = 'stretchDnBaseEvent',		$
					TITLE = title_prefix,				$
					_EXTRA = extra )
	ENDELSE

	;---------------------------------------------------------------------
	; Set up offscreen drawing
	;---------------------------------------------------------------------
	IF NOT KEYWORD_SET( XSIZE ) THEN XSIZE = 512
	YSIZE			= XSIZE * 1.0
	curWin			= !D.WINDOW
	WINDOW, /FREE, XSIZE=XSIZE, YSIZE=YSIZE, /PIXMAP
	histogramPixmapId	= !D.WINDOW
	WSET, curWin

	stretchControlsBase	= WIDGET_BASE( tlb )

	;---------------------------------------------------------------------
	; Set up arrays to hold controls on the histogram interface.
	;
	; It is coincidencidental that there are 6 elements in these arrays,
	; and there having been also exactly 6 display planes in misr_view
	; versions 2.0 - 4.0.
	; The 6 elements of these arrays are for the 6 types of histograms:
	;	1. R only
	;	2. G only
	;	3. B only
	;	4. R active & GB passive
	;	5. G active & RB passive
	;	6. B active & RG passive
	; These are fixed and not subjected to data manager index swapping.
	;---------------------------------------------------------------------
	controlsBase		= LONARR( 6 )
	histWidgetDrawIds	= LONARR( 6 )
	bottomBase		= LONARR( 6 )
	bottomLimitText		= LONARR( 6 )
	bottomLimitResetButton	= LONARR( 6 )
	bottomLabelBase		= LONARR( 6 )
	bottomLabelLabel	= LONARR( 6 )
	bottomLabels		= LONARR( 6 )
	bottomSliders		= LONARR( 6 )
	topBase			= LONARR( 6 )
	topLimitText		= LONARR( 6 )
	topLimitResetButton	= LONARR( 6 )
	topLabelBase		= LONARR( 6 )
	topLabelLabel		= LONARR( 6 )
	topLabels		= LONARR( 6 )
	topSliders		= LONARR( 6 )
	gammaBase		= LONARR( 6 )
	gammaLabelBase		= LONARR( 6 )
	gammaLabelLabel		= LONARR( 6 )
	gammaLabels		= LONARR( 6 )
	gammaSliders		= LONARR( 6 )

	;------------------------------------------------------------------
	; Set up all controls, regardless of whether they are active or not
	;------------------------------------------------------------------
	FOR i = 0,5 DO BEGIN
		CASE 1 OF
			(i EQ 0 OR i EQ 3) AND redBandIdx GE 0: BEGIN	; active red interface
				data_bottomLimit	= *((*bottomLimitPtr)[redBandIdx])
				data_topLimit		= *((*topLimitPtr)[redBandIdx])
				data_min		= *((*min_dataPtr)[redBandIdx])
				data_max		= *((*max_dataPtr)[redBandIdx])
				data_bottom		= *((*stretchDnBottomPtr)[redBandIdx])
				data_top		= *((*stretchDnTopPtr)[redBandIdx])
				gamma_slider_val	= *((*stretchDnGammaPtr)[redBandIdx])
				END
			(i EQ 1 OR i EQ 4) AND grnBandIdx GE 0: BEGIN	; active green interface
				data_bottomLimit	= *((*bottomLimitPtr)[grnBandIdx])
				data_topLimit		= *((*topLimitPtr)[grnBandIdx])
				data_min		= *((*min_dataPtr)[grnBandIdx])
				data_max		= *((*max_dataPtr)[grnBandIdx])
				data_bottom		= *((*stretchDnBottomPtr)[grnBandIdx])
				data_top		= *((*stretchDnTopPtr)[grnBandIdx])
				gamma_slider_val	= *((*stretchDnGammaPtr)[grnBandIdx])
				END
			(i EQ 2 OR i EQ 5) AND bluBandIdx GE 0: BEGIN	; active blue interface
				data_bottomLimit	= *((*bottomLimitPtr)[bluBandIdx])
				data_topLimit		= *((*topLimitPtr)[bluBandIdx])
				data_min		= *((*min_dataPtr)[bluBandIdx])
				data_max		= *((*max_dataPtr)[bluBandIdx])
				data_bottom		= *((*stretchDnBottomPtr)[bluBandIdx])
				data_top		= *((*stretchDnTopPtr)[bluBandIdx])
				gamma_slider_val	= *((*stretchDnGammaPtr)[bluBandIdx])
				END
			ELSE: BEGIN					; inactive interface
				data_bottomLimit	= 0
				data_topLimit		= 1
				data_min		= 0
				data_max		= 1
				data_bottom		= 0
				data_top		= 1
				gamma_slider_val	= 1.0
				END
		ENDCASE

		;---------------------------------------------------------------------
		; Adjust "min" and "max" for calculations.
		; This accounts for the new "limits" feature allowing
		; the user to set the slider limits beyond the data range.
		; This adjustment will only take effect if the user destroys
		; the stretch DN histogram interface window (eg. X-kill) and
		; then calls it up again.
		;---------------------------------------------------------------------
		IF data_max LT data_top THEN data_max = data_top
		IF data_min GT data_bottom THEN data_min = data_bottom

;print,'data_top,data_bottom, data_min, data_max = ',data_top, data_bottom, data_min, data_max

		IF data_max EQ data_min THEN denom = 1 ELSE denom = data_max - data_min
		bottom_slider_val	= FIX( 0.5 + 1000.0 * ABS( FLOAT( data_bottom - data_min )/ FLOAT( denom ) ) )
;print,'bottom_slider_val = ',bottom_slider_val

		top_slider_val		= FIX(0.5+1000.0*ABS(FLOAT(data_top-data_min)/FLOAT(denom)))
;print,'top_slider_val = ',top_slider_val

		controlsBase[i]		= WIDGET_BASE( stretchControlsBase, /COLUMN )


		histWidgetDrawIds[i]	= WIDGET_DRAW( controlsBase[i],				$
						XSIZE = XSIZE, YSIZE = YSIZE, RETAIN = 2 )

		bottomBase[i]		= WIDGET_BASE( controlsBase[i], /COLUMN, /FRAME )
		bottomLimitBase		= WIDGET_BASE( bottomBase[i], /ROW )
		bottomLimitLabel	= WIDGET_LABEL( bottomLimitBase, VALUE = 'Lower Limit:  ' )
		bottomLimitText[i]	= WIDGET_TEXT( bottomLimitBase, XSIZE = 16,		$
						VALUE = STRTRIM(data_bottomLimit,2),	$
						EVENT_PRO = 'bottomLimitTextEvent',		$
						/EDITABLE )
		bottomLimitResetButton[i]= WIDGET_BUTTON( bottomLimitBase,			$
						VALUE = 'Reset Limit',				$
						EVENT_PRO = 'bottomLimitResetButtonEvent' )
		bottomLabelBase[i]	= WIDGET_BASE( bottomBase[i], /ROW )
		bottomLabelLabel[i]	= WIDGET_LABEL( bottomLabelBase[i], VALUE = 'Minimum Value:  ' )
		IF SIZE(data_bottom,/TYPE) EQ 1 THEN data_bottom = FIX(data_bottom)
		bottomLabels[i]		= WIDGET_LABEL( bottomLabelBase[i],			$
						VALUE = STRTRIM(data_bottom,2), /DYNAMIC_RESIZE )
		bottomSliders[i]	= WIDGET_SLIDER( bottomBase[i],				$
						xsize = XSIZE, VALUE = bottom_slider_val,	$
						MINIMUM = 0, MAXIMUM = 1000,			$		; DO NOT CHANGE THE MAXIMUM VALUE.  THIS VALUE IS USED ELSEWHERE.  PROBABLY, IT SHOULD BE A VARIABLE BUT AT THIS POINT IT IS NOT.
						/DRAG, /SUPPRESS_VALUE, SCROLL = 1,		$
						EVENT_PRO = 'bottomSlidersEvent' )

		topBase[i]		= WIDGET_BASE( controlsBase[i], /COLUMN, /FRAME )
		topLimitBase		= WIDGET_BASE( topBase[i], /ROW )
		topLimitLabel		= WIDGET_LABEL( topLimitBase, VALUE = 'Upper Limit:  ' )
		topLimitText[i]		= WIDGET_TEXT( topLimitBase, XSIZE = 16,		$
						VALUE = STRTRIM(data_topLimit,2),	$
						EVENT_PRO = 'topLimitTextEvent',		$
						/EDITABLE )
		topLimitResetButton[i]	= WIDGET_BUTTON( topLimitBase,				$
						VALUE = 'Reset Limit',				$
						EVENT_PRO = 'topLimitResetButtonEvent' )
		topLabelBase[i]		= WIDGET_BASE( topBase[i], /ROW )
		topLabelLabel[i]	= WIDGET_LABEL( topLabelBase[i], VALUE = 'Maximum Value:  ' )
		IF SIZE(data_top,/TYPE) EQ 1 THEN data_top = FIX(data_top)
		topLabels[i]		= WIDGET_LABEL( topLabelBase[i],			$
						VALUE = STRTRIM(data_top,2), /DYNAMIC_RESIZE )
		topSliders[i]		= WIDGET_SLIDER( topBase[i],				$
						XSIZE = xsize, VALUE = top_slider_val,		$
						MINIMUM = 0, MAXIMUM = 1000,			$		; DO NOT CHANGE THE MAXIMUM VALUE.  THIS VALUE IS USED ELSEWHERE.  PROBABLY, IT SHOULD BE A VARIABLE BUT AT THIS POINT IT IS NOT.
						/DRAG, /SUPPRESS_VALUE, SCROLL = 1,		$
						event_pro = 'topSlidersEvent' )

		gammaBase[i]		= WIDGET_BASE( controlsBase[i], /COLUMN, /FRAME )
		gammaLabelBase[i]	= WIDGET_BASE( gammaBase[i], /ROW )
		gammaLabelLabel[i]	= WIDGET_LABEL( gammaLabelBase[i],			$
						VALUE = 'Gamma:  ' )
		gammaLabels[i]		= WIDGET_LABEL( gammaLabelBase[i],			$
						VALUE = STRTRIM(gamma_slider_val,2), /DYNAMIC_RESIZE )
		gammaSliders[i]		= WIDGET_SLIDER( gammaBase[i], xsize = XSIZE,		$
						event_pro = 'gammaSlidersEvent',		$
						/SUPPRESS_VALUE, /DRAG, SCROLL = 1,		$
						MINIMUM = 25, MAXIMUM = 400,			$
						VALUE = FIX(gamma_slider_val*100.0) )
	ENDFOR

	;---------------------------------------------------------------------
	; Set the RGB histogram interface to be active initially.
	;---------------------------------------------------------------------
	CASE 1 OF
		redBandIdx GE 0 : BEGIN
			WIDGET_CONTROL, controlsBase[0], MAP = 1
			WIDGET_CONTROL, controlsBase[1], MAP = 0
			WIDGET_CONTROL, controlsBase[2], MAP = 0
			WIDGET_CONTROL, controlsBase[3], MAP = 0
			WIDGET_CONTROL, controlsBase[4], MAP = 0
			WIDGET_CONTROL, controlsBase[5], MAP = 0
		END
		grnBandIdx GE 0 AND redBandIdx LT 0 : BEGIN
			WIDGET_CONTROL, controlsBase[0], MAP = 0
			WIDGET_CONTROL, controlsBase[1], MAP = 1
			WIDGET_CONTROL, controlsBase[2], MAP = 0
			WIDGET_CONTROL, controlsBase[3], MAP = 0
			WIDGET_CONTROL, controlsBase[4], MAP = 0
			WIDGET_CONTROL, controlsBase[5], MAP = 0
		END
		bluBandIdx GE 0 AND grnBandIdx LT 0 AND redBandIdx LT 0 : BEGIN
			WIDGET_CONTROL, controlsBase[0], MAP = 0
			WIDGET_CONTROL, controlsBase[1], MAP = 0
			WIDGET_CONTROL, controlsBase[2], MAP = 1
			WIDGET_CONTROL, controlsBase[3], MAP = 0
			WIDGET_CONTROL, controlsBase[4], MAP = 0
			WIDGET_CONTROL, controlsBase[5], MAP = 0
		END
	ENDCASE

	;---------------------------------------------------------------------
	; Desensitize histogram interfaces of absent colors.
	;---------------------------------------------------------------------
	IF redBandIdx LT 0 THEN BEGIN
		WIDGET_CONTROL, controlsBase[0], SENSITIVE = 0
		WIDGET_CONTROL, controlsBase[3], SENSITIVE = 0
	ENDIF
	IF grnBandIdx LT 0 THEN BEGIN
		WIDGET_CONTROL, controlsBase[1], SENSITIVE = 0
		WIDGET_CONTROL, controlsBase[4], SENSITIVE = 0
	ENDIF
	IF bluBandIdx LT 0 THEN BEGIN
		WIDGET_CONTROL, controlsBase[2], SENSITIVE = 0
		WIDGET_CONTROL, controlsBase[5], SENSITIVE = 0
	ENDIF

	obj->Set_histogramWidgetDrawIds, histWidgetDrawIds
	obj->Set_histogramPixmapId, histogramPixmapId

	stretchControlButtonBase= WIDGET_BASE( tlb, /ROW )

	rgbSelectBase		= WIDGET_BASE( stretchControlButtonBase, /ROW, /FRAME )
	redSelectButton		= WIDGET_BUTTON( rgbSelectBase, VALUE = 'R', $
					EVENT_PRO = 'rgbSelectEvent' )
	grnSelectButton		= widget_BUTTON( rgbSelectBase, VALUE = 'G', $
					EVENT_PRO = 'rgbSelectEvent' )
	bluSelectButton		= widget_BUTTON( rgbSelectBase, VALUE = 'B', $
					EVENT_PRO = 'rgbSelectEvent' )
	rrrSelectButton		= WIDGET_BUTTON( rgbSelectBase, VALUE = 'Rgb', $
					EVENT_PRO = 'rgbSelectEvent' )
	gggSelectButton		= widget_BUTTON( rgbSelectBase, VALUE = 'rGb', $
					EVENT_PRO = 'rgbSelectEvent' )
	bbbSelectButton		= widget_BUTTON( rgbSelectBase, VALUE = 'rgB', $
					EVENT_PRO = 'rgbSelectEvent' )

	IF redBandIdx LT 0 THEN BEGIN
		WIDGET_CONTROL, redSelectButton, SENSITIVE = 0
		WIDGET_CONTROL, rrrSelectButton, SENSITIVE = 0
	ENDIF
	IF grnBandIdx LT 0 THEN BEGIN
		WIDGET_CONTROL, grnSelectButton, SENSITIVE = 0
		WIDGET_CONTROL, gggSelectButton, SENSITIVE = 0
	ENDIF
	IF bluBandIdx LT 0 THEN BEGIN
		WIDGET_CONTROL, bluSelectButton, SENSITIVE = 0
		WIDGET_CONTROL, bbbSelectButton, SENSITIVE = 0
	ENDIF

	rgb_Rgb_rGb_rgB_buttons	= [	redSelectButton,	$
					grnSelectButton,	$
					bluSelectButton,	$
					rrrSelectButton,	$
					gggSelectButton,	$
					bbbSelectButton ]

	applyBase		= WIDGET_BASE( stretchControlButtonBase, /ROW, /FRAME )
	applyButton		= WIDGET_BUTTON( applyBase, VALUE = 'Apply', $
					EVENT_FUNC = 'applyButtonEvent' )
	storeStretchButton	= WIDGET_BUTTON( applyBase, VALUE = 'Store', $
					EVENT_PRO = 'storeStretchButtonEvent' )	; UVALUE of this button stores user-selected path.
	recallStretchButton	= WIDGET_BUTTON( applyBase, VALUE = 'Recall', $
					EVENT_PRO = 'recallStretchButtonEvent' ); UVALUE of this button stores user-selected path.

dismissBase	= WIDGET_BASE( stretchControlButtonBase, /ROW, /FRAME )
dismissButton	= WIDGET_BUTTON( dismissBase, VALUE = 'Dismiss', EVENT_PRO = 'dismissButtonEvent' )
hist_type_base	= WIDGET_BASE( tlb, /ROW, /NONEXCLUSIVE )
hist_type_btn	= WIDGET_BUTTON( hist_type_base, VALUE = 'Calculate Histogram Only For Displayed Data',	$
				EVENT_PRO = 'histTypeButtonEvent' )
WIDGET_CONTROL, hist_type_btn, SET_BUTTON = *calc_hist_for_displayed_data_ptr

	statePtr = PTR_NEW({	obj 					: obj					,$
				redBandIdx				: redBandIdx				,$
				grnBandIdx				: grnBandIdx				,$
				bluBandIdx				: bluBandIdx				,$
				min_dataPtr				: min_dataPtr				,$
				max_dataPtr				: max_dataPtr				,$
				controlsBase				: controlsBase				,$
				histogramWidgetDrawIds			: histWidgetDrawIds			,$
				bottomLimitPtr				: bottomLimitPtr			,$
				bottomLimitText				: bottomLimitText			,$
				bottomLimitResetButton			: bottomLimitResetButton		,$
				bottomLabels 				: bottomLabels				,$
				bottomSliders 				: bottomSliders				,$
				topLimitPtr				: topLimitPtr				,$
				topLimitText				: topLimitText				,$
				topLimitResetButton			: topLimitResetButton			,$
				topLabels 				: topLabels				,$
				topSliders 				: topSliders				,$
				gammaLabels				: gammaLabels				,$
				gammaSliders 				: gammaSliders				,$
				stretchDnBottomPtr			: stretchDnBottomPtr			,$
				stretchDnTopPtr				: stretchDnTopPtr			,$
				stretchDnGammaPtr			: stretchDnGammaPtr			,$
				rgb_Rgb_rGb_rgB_buttons			: rgb_Rgb_rGb_rgB_buttons		,$
				calc_hist_for_displayed_data_ptr	: calc_hist_for_displayed_data_ptr	,$
				excludeZeroesFlag			: 0					})

	WIDGET_CONTROL, tlb, SET_UVALUE = statePtr

	WIDGET_CONTROL, tlb, /REALIZE


	RETURN, tlb
END
;get_stretchDn_base


;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::Set_stretchDnBaseToggledOnOrOff @@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::Set_stretchDnBaseToggledOnOrOff, toggleValue

	SELF.stretchDnBaseToggledOnOrOff = toggleValue

END
; GEOREF_IMAGE::Set_stretchDnBaseToggledOnOrOff

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::ToggleStretchDnInterface @@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::ToggleStretchDnInterface, mappedOnOff

;help,SELF.min_dataPtr
;help,*SELF.min_dataPtr
;print,'*SELF.min_dataPtr = ',*SELF.min_dataPtr
;print,'*SELF.max_dataPtr = ',*SELF.max_dataPtr

	newRealize = 0

	XSIZE = 512

	IF NOT WIDGET_INFO( SELF.stretchDnBase, /VALID_ID ) THEN BEGIN

		SELF.stretchDnBase = get_stretchDn_base(		$
			SELF,						$
			SELF.min_dataPtr,				$
			SELF.max_dataPtr,				$
			SELF.stretchDnBottomPtr,			$
			SELF.stretchDnTopPtr,				$
			SELF.stretchDnGammaPtr,				$
			SELF.bottomLimitPtr,				$
			SELF.topLimitPtr,				$
			SELF.redBandIdx,				$
			SELF.grnBandIdx,				$
			SELF.bluBandIdx,				$
			SELF.calc_hist_for_displayed_data_ptr,		$
			GROUP_LEADER = SELF.drawBase, 			$
			XSIZE = XSIZE,					$
			TITLE_PREFIX = SELF.title_prefix )

		;========================================================================
		; Add scroll bars, if necessary.
		;========================================================================
		tlb_geom = WIDGET_INFO(SELF.stretchDnBase,/GEOMETRY)
		DEVICE, GET_SCREEN_SIZE = screen_size
		tlb_xsize = tlb_geom.SCR_XSIZE + (2 * tlb_geom.MARGIN)
		tlb_ysize = tlb_geom.SCR_YSIZE + (2 * tlb_geom.MARGIN)
		scr_xsize = tlb_xsize < ( screen_size[0] - 100 )
		scr_ysize = tlb_ysize < ( screen_size[1] - 100 )
;print,tlb_geom.SCR_YSIZE,tlb_geom.YSIZE,tlb_ysize,screen_size[1],scr_ysize
		IF (tlb_ysize GT screen_size[1]) THEN BEGIN
			WIDGET_CONTROL,SELF.stretchDnBase,/DESTROY
			extra = {	SCROLL:1,			$
					X_SCROLL_SIZE:scr_xsize,	$
					Y_SCROLL_SIZE:scr_ysize,	$
					XSIZE:tlb_geom.SCR_XSIZE,	$
					YSIZE:tlb_geom.SCR_YSIZE	}
			SELF.stretchDnBase = get_stretchDn_base(		$
				SELF,						$
				SELF.min_dataPtr,				$
				SELF.max_dataPtr,				$
				SELF.stretchDnBottomPtr,			$
				SELF.stretchDnTopPtr,				$
				SELF.stretchDnGammaPtr,				$
				SELF.bottomLimitPtr,				$
				SELF.topLimitPtr,				$
				SELF.redBandIdx,				$
				SELF.grnBandIdx,				$
				SELF.bluBandIdx,				$
				SELF.calc_hist_for_displayed_data_ptr,		$
				GROUP_LEADER = SELF.drawBase, 			$
				XSIZE = XSIZE,					$
				TITLE_PREFIX = SELF.title_prefix,		$
				_EXTRA = extra )
		ENDIF

		newRealize = 1
;help, SELF.redBandIdx
;print,'+++++++++ GEOREF_IMAGE::ToggleStretchDnInterface     SELF.redBandIdx = ', SELF.redBandIdx
		IF SELF.redBandIdx GE 0 AND						$
			( *(SELF.calc_hist_for_displayed_data_ptr) OR			$
				NOT PTR_VALID(SELF.rgbHistogramPtrarr[0]) ) THEN	$
					SELF->CalculateHistogram, SELF.redBandIdx, 0
		IF SELF.grnBandIdx GE 0 AND						$
			( *(SELF.calc_hist_for_displayed_data_ptr) OR			$
				NOT PTR_VALID(SELF.rgbHistogramPtrarr[1]) ) THEN	$
					SELF->CalculateHistogram, SELF.grnBandIdx, 1
		IF SELF.bluBandIdx GE 0 AND						$
			( *(SELF.calc_hist_for_displayed_data_ptr) OR			$
				NOT PTR_VALID(SELF.rgbHistogramPtrarr[2]) ) THEN	$
					SELF->CalculateHistogram, SELF.bluBandIdx, 2
	ENDIF

	WIDGET_CONTROL, SELF.stretchDnBase, map = mappedOnOff

	;========================================================================
	; This line added so that program does not crash if an image window is
	; killed while the stretchDn interface is visible
	;========================================================================
	IF NOT WIDGET_INFO( SELF.stretchDnButton, /VALID_ID ) THEN BEGIN
		RETURN, newRealize
	ENDIF

	IF mappedOnOff EQ 0 THEN BEGIN
		WIDGET_CONTROL, SELF.stretchDnButton, SET_VALUE = 'Show Stretch DN Interface'
		SELF->Set_stretchDnBaseToggledOnOrOff, 0
	ENDIF ELSE BEGIN
		WIDGET_CONTROL, SELF.stretchDnButton, SET_VALUE = 'Hide Stretch DN Interface'
		SELF->Set_stretchDnBaseToggledOnOrOff, 1
	ENDELSE

	IF SELF.nChannels LE 1 $
	AND PTR_VALID(SELF.red_vec_ptr) $
	AND PTR_VALID(SELF.grn_vec_ptr) $
	AND PTR_VALID(SELF.blu_vec_ptr) THEN BEGIN
;print,'GEOREF_IMAGE::ToggleStretchDnInterface -- TVLCT, *(SELF.red_vec_ptr), *(SELF.grn_vec_ptr), *(SELF.blu_vec_ptr)'
		TVLCT, *(SELF.red_vec_ptr), *(SELF.grn_vec_ptr), *(SELF.blu_vec_ptr)
	ENDIF

	RETURN, newRealize
END
; GEOREF_IMAGE::ToggleStretchDnInterface

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::set_colorbar_xy_current @@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::set_colorbar_xy_current, x, y
	SELF.colorbar_xsize_current = x
	SELF.colorbar_ysize_current = y
END
; GEOREF_IMAGE::set_colorbar_xy_current

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::get_colorbar_xy_default @@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::get_colorbar_xy_default, x, y
	RETURN, [ SELF.colorbar_xsize_default, SELF.colorbar_ysize_default ]
END
; GEOREF_IMAGE::get_colorbar_xy_default

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::get_colorbar_xy_current @@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::get_colorbar_xy_current, x, y
	RETURN, [ SELF.colorbar_xsize_current, SELF.colorbar_ysize_current ]
END
; GEOREF_IMAGE::get_colorbar_xy_current

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::get_colorbar_drawwidgetid @@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::get_colorbar_drawwidgetid
	RETURN, SELF.colorbar_draw
END
; GEOREF_IMAGE::get_colorbar_drawwidgetid

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::get_colorbar_window @@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::get_colorbar_window
	RETURN, SELF.colorbar_window
END
; GEOREF_IMAGE::get_colorbar_window

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::get_colorbar_tlb @@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::get_colorbar_tlb
	RETURN, SELF.colorbar_tlb
END
; GEOREF_IMAGE::get_colorbar_tlb

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::get_colorbar_button_base @@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::get_colorbar_button_base
	RETURN, SELF.colorbar_button_base
END
; GEOREF_IMAGE::get_colorbar_button_base

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::get_colorbar_sizebutton @@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::get_colorbar_sizebutton
	RETURN, SELF.colorbar_sizebutton
END
; GEOREF_IMAGE::get_colorbar_sizebutton

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::get_NUMBER_HISTOGRAM_COLORS @@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::get_NUMBER_HISTOGRAM_COLORS
	RETURN, SELF.NUMBER_HISTOGRAM_COLORS
END
; GEOREF_IMAGE::get_NUMBER_HISTOGRAM_COLORS

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::get_stretchDnBottomPtr @@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::get_stretchDnBottomPtr
	RETURN, SELF.stretchDnBottomPtr
END
; GEOREF_IMAGE::get_stretchDnBottomPtr

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::get_stretchDnTopPtr @@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::get_stretchDnTopPtr
	RETURN, SELF.stretchDnTopPtr
END
; GEOREF_IMAGE::get_stretchDnTopPtr

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::get_stretchDnGammaPtr @@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::get_stretchDnGammaPtr
	RETURN, SELF.stretchDnGammaPtr
END
; GEOREF_IMAGE::get_stretchDnGammaPtr

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::get_rgbBandIdx @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::get_rgbBandIdx
	RETURN, [ SELF.redBandIdx, SELF.grnBandIdx, SELF.bluBandIdx ]
END
; GEOREF_IMAGE::get_rgbBandIdx

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::get_display_rgb @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::get_display_rgb
	RETURN, SELF.display_rgb
END
; GEOREF_IMAGE::get_display_rgb

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::get_x_axis_for_colorbar_ptr @@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::get_x_axis_for_colorbar_ptr
	RETURN, SELF.x_axis_for_colorbar_ptr
END
; GEOREF_IMAGE::get_x_axis_for_colorbar_ptr

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::get_colorbar_divisions @@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::get_colorbar_divisions
	RETURN, SELF.colorbar_divisions
END
; GEOREF_IMAGE::get_colorbar_divisions

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::set_colorbar_divisions @@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::set_colorbar_divisions, divisions
	SELF.colorbar_divisions = divisions
END
; GEOREF_IMAGE::set_colorbar_divisions

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::get_colorbar_charsize @@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::get_colorbar_charsize
	RETURN, SELF.colorbar_charsize
END
; GEOREF_IMAGE::get_colorbar_charsize

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::set_colorbar_charsize @@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::set_colorbar_charsize, charsize
	SELF.colorbar_charsize = charsize
END
; GEOREF_IMAGE::set_colorbar_charsize

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::get_colorbar_charsizes_allowed_ptr
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::get_colorbar_charsizes_allowed_ptr
	RETURN, SELF.colorbar_charsizes_allowed_ptr
END
; GEOREF_IMAGE::get_colorbar_charsizes_allowed_ptr

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::set_colorbar_widgetids_to_neg_one @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::set_colorbar_widgetids_to_neg_one
	SELF.colorbar_tlb		= -1
	SELF.colorbar_draw		= -1
	SELF.colorbar_window		= -1
END
; GEOREF_IMAGE::set_colorbar_widgetids_to_neg_one

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ colorBar_kill_notify @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO colorBar_kill_notify, colorbar_tlb

	COMMON GEOREFIMAGE, linkObj, nullObj, linkMsg, nObj, idArr, commonblock_pseudo_color_table_ptrarr, commonblock_self

	WIDGET_CONTROL, colorbar_tlb, GET_UVALUE = georef_obj
	IF OBJ_VALID( georef_obj ) THEN BEGIN

		;-------------------------------------------------------------
		; Set the GEOREF_IMAGE members that store the colorbar widget
		; ID's to (-1) since they're being destroyed.
		;-------------------------------------------------------------
		georef_obj -> set_colorbar_widgetids_to_neg_one

		;-------------------------------------------------------------
		; Reset colorbar divisions to 1, so they match the number
		; displayed by the droplist next time it gets created.
		;-------------------------------------------------------------
		georef_obj -> set_colorbar_divisions, 1
		georef_obj -> set_colorbar_charsize, 1.0

		;-------------------------------------------------------------
		; Reset the common block pseudo color table to grey scale, so
		; that colors won't auto-re-load after the colorbar is killed.
		;-------------------------------------------------------------
;print,'colorBar_kill_notify -- LOADCT, 0'
		LOADCT, 0
		TVLCT, red_table, grn_table, blu_table, /GET
		IF PTR_VALID( commonblock_pseudo_color_table_ptrarr[0] ) THEN	$
			PTR_FREE, commonblock_pseudo_color_table_ptrarr[0]
		IF PTR_VALID( commonblock_pseudo_color_table_ptrarr[1] ) THEN	$
			PTR_FREE, commonblock_pseudo_color_table_ptrarr[1]
		IF PTR_VALID( commonblock_pseudo_color_table_ptrarr[2] ) THEN	$
			PTR_FREE, commonblock_pseudo_color_table_ptrarr[2]
;print,'colorbar_kill_notify -- commonblock_pseudo_color_table_ptrarr    is being set to B/W table'
;print,'colorbar_kill_notify -- red_table = ',red_table
;print,'colorbar_kill_notify -- grn_table = ',grn_table
;print,'colorbar_kill_notify -- blu_table = ',blu_table
		commonblock_pseudo_color_table_ptrarr[0] = PTR_NEW( red_table )
		commonblock_pseudo_color_table_ptrarr[1] = PTR_NEW( grn_table )
		commonblock_pseudo_color_table_ptrarr[2] = PTR_NEW( blu_table )

	ENDIF
END
; colorBar_kill_notify

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ colorBar_event @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO colorBar_event, event

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== colorBar_event (GEOREF_IMAGE) =========='
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


	COMMON GEOREFIMAGE, linkObj, nullObj, linkMsg, nObj, idArr, commonblock_pseudo_color_table_ptrarr, commonblock_self

	WIDGET_CONTROL, event.top, GET_UVALUE = georef_obj
	widget_type = TAG_NAMES( event, /STRUCTURE_NAME )

;print,'widget_type = ',widget_type

	CASE 1 OF

		widget_type EQ 'WIDGET_BASE' : BEGIN

			;-------------------------------------------------------
			; Resize by user.
			;-------------------------------------------------------
			buttonBaseGeom	= WIDGET_INFO( georef_obj -> get_colorbar_button_base(), /GEOMETRY )
			tlbGeom		= WIDGET_INFO( georef_obj -> get_colorbar_tlb(), /GEOMETRY )
;print,''
;print,'(georef_obj -> get_colorbar_xy_current())[0] = ',(georef_obj -> get_colorbar_xy_current())[0]
;print,'(georef_obj -> get_colorbar_xy_current())[0] = ',(georef_obj -> get_colorbar_xy_current())[1]
;print,'tlbGeom.xpad = ',tlbGeom.xpad
;print,'tlbGeom.ypad = ',tlbGeom.ypad
			georef_obj -> set_colorbar_xy_current,				$
				event.x - tlbGeom.xpad * 2,				$
				event.y - buttonBaseGeom.ysize - tlbGeom.ypad * 3	; one extra ypad for the space between
											; the widget draw and button base.
;print,'event.x = ',event.x
;print,'event.y = ',event.y
;print,'buttonBaseGeom.ysize = ',buttonBaseGeom.ysize
;print,'buttonBaseGeom.margin = ',buttonBaseGeom.margin
;print,'(georef_obj -> get_colorbar_xy_current())[0] = ',(georef_obj -> get_colorbar_xy_current())[0]
;print,'(georef_obj -> get_colorbar_xy_current())[0] = ',(georef_obj -> get_colorbar_xy_current())[1]
			WIDGET_CONTROL, georef_obj -> get_colorbar_drawwidgetid(),	$
				XSIZE = (georef_obj -> get_colorbar_xy_current())[0],	$
				YSIZE = (georef_obj -> get_colorbar_xy_current())[1]

			;-------------------------------------------------------
			; Update the colorbar.
			;-------------------------------------------------------
			colorbar_redraw, TLB = georef_obj -> getDrawBaseID()

			END

		widget_type EQ 'WIDGET_TRACKING' : BEGIN

			IF event.enter EQ 1 THEN BEGIN

				;----------------------------------------------------------------------
				; Mouse is entering the draw widget.
				; Auto-re-load the selected pseudo color table, just in case.
				;----------------------------------------------------------------------
				TVLCT,								$
					*(commonblock_pseudo_color_table_ptrarr[0]),		$
					*(commonblock_pseudo_color_table_ptrarr[1]),		$
					*(commonblock_pseudo_color_table_ptrarr[2])

			ENDIF

			END

		widget_type EQ 'WIDGET_DROPLIST' : BEGIN

			WIDGET_CONTROL, event.id, GET_UVALUE = uval

			CASE 1 OF
				uval EQ 'divisions' : BEGIN

					;-------------------------------------------------------
					; Possible divisions values are assumed to begin at 1, and
					; increment by 1.  For example, the declaration of the
					; WIDGET_DROPLIST might have this:
					; VALUES = STRTRIM( [1,2,3,4,5,6,7,8,9,10], 2 )
					;
					; If the start value is changed, then the line below
					; (n_divisions = event.index + 1) must be changed, as well,
					; to reflect the new start value as it gets added to
					; event.index.
					;-------------------------------------------------------
					n_divisions	= event.index + 1
					georef_obj -> set_colorbar_divisions, n_divisions

					;-------------------------------------------------------
					; Update the colorbar.
					;-------------------------------------------------------
					colorbar_redraw, TLB = georef_obj -> getDrawBaseID()
				END
				uval EQ 'charsize' : BEGIN
					charsize	= (*(georef_obj -> get_colorbar_charsizes_allowed_ptr()))[event.index]
					georef_obj -> set_colorbar_charsize, charsize
					colorbar_redraw, TLB = georef_obj -> getDrawBaseID()
				END
				ELSE : BEGIN
				END
			ENDCASE
			END

		widget_type EQ 'WIDGET_BUTTON' : BEGIN

			WIDGET_CONTROL, event.id, GET_VALUE = button_name

			CASE 1 OF

				STRUPCASE( button_name ) EQ 'DEFAULT SIZE' : BEGIN

					;-------------------------------------------------------
					; Resize colorbar to default size.
					;-------------------------------------------------------
					georef_obj -> set_colorbar_xy_current,				$
						(georef_obj -> get_colorbar_xy_default())[0],		$
						(georef_obj -> get_colorbar_xy_default())[1]
					WIDGET_CONTROL, georef_obj -> get_colorbar_drawwidgetid(),	$
						XSIZE = (georef_obj -> get_colorbar_xy_current())[0],	$
						YSIZE = (georef_obj -> get_colorbar_xy_current())[1]

					;-------------------------------------------------------
					; Update the colorbar.
					;-------------------------------------------------------
					colorbar_redraw, TLB = georef_obj -> getDrawBaseID()

					END

				STRUPCASE( button_name ) EQ 'SAVE TIFF' : BEGIN

					;-------------------------------------------------------
					; Save TIFF.
					;-------------------------------------------------------
;;;ckt,apr2001		  			filename = DIALOG_PICKFILE( TITLE = 'Enter filename for colorbar TIFF' )
		  			filename = dialog_pickfile_wrapper( TITLE = 'Enter filename for colorbar TIFF' )
					IF STRTRIM( filename, 2 ) EQ '' THEN BEGIN
		  				res	= DIALOG_MESSAGE( [				$
								'No filename specified.',		$
								'Ignoring save command.' ],		$
								/INFORMATION )
						RETURN
					ENDIF
					IF (FINDFILE( filename ))[0] THEN BEGIN
						ret	= DIALOG_MESSAGE( [				$
								filename,				$
								'This file exists.',			$
								'Please choose another filename.' ],	$
								/INFORMATION )
						RETURN
					ENDIF

			  		WIDGET_CONTROL, /HOURGLASS
					curwin = !D.WINDOW
					WINDOW, 							$
						/FREE, 							$
						/PIXMAP,						$
						XSIZE = (georef_obj -> get_colorbar_xy_current())[0],	$
						YSIZE = (georef_obj -> get_colorbar_xy_current())[1]
					colorbar_redraw, TLB = georef_obj -> getDrawBaseID(), 		$
								/NO_DECOMPOSE, WINDOW_ID = !D.WINDOW
					greyscale_image = TVRD()
					WDELETE, !D.WINDOW
					WSET, curwin


					WSET, georef_obj -> get_colorbar_window()



;greyscale_image_tmp = TVRD()

					;----------------------------------------------------------------------
					; Auto-re-load the selected pseudo color table, just in case.
					;----------------------------------------------------------------------
					TVLCT,								$
						*(commonblock_pseudo_color_table_ptrarr[0]),		$
						*(commonblock_pseudo_color_table_ptrarr[1]),		$
						*(commonblock_pseudo_color_table_ptrarr[2])
		  			TVLCT, red_tab, grn_tab, blu_tab, /GET
;loadct,0
;					greyscale_image = TVRD()

;save,greyscale_image,greyscale_image_tmp,red_tab,grn_tab,blu_tab,filename='ct_vars.sav'
 					WRITE_TIFF,							$
  						filename,						$
  						RED   = red_tab( REVERSE( greyscale_image, 2) ),	$
  						GREEN = grn_tab( REVERSE( greyscale_image, 2) ),	$
  						BLUE  = blu_tab( REVERSE( greyscale_image, 2) ),	$
  						1,							$
  						PLANARCONFIG = 2
;					WRITE_TIFF,							$
;						filename,						$
;						RED   = red_tab( REVERSE( TVRD( CHANNEL = 1 ), 2) ),	$
;						GREEN = grn_tab( REVERSE( TVRD( CHANNEL = 2 ), 2) ),	$
;						BLUE  = blu_tab( REVERSE( TVRD( CHANNEL = 3 ), 2) ),	$
;						1,							$
; 						PLANARCONFIG = 2
;  		 			WRITE_TIFF, filename, REVERSE(tvrd(),2), 1, RED = r, GREEN = g, BLUE = b

	  				WSET, curwin

					END

				STRUPCASE( button_name ) EQ 'DISMISS' : BEGIN

					WIDGET_CONTROL, event.top, /DESTROY

					END

				ELSE :

			ENDCASE

			END

		ELSE :

	ENDCASE

END
; colorBar_event

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ colorBar_redraw @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO colorBar_redraw, TLB = tlb, NO_DECOMPOSE = nodecom, WINDOW_ID = wid

	WIDGET_CONTROL, tlb, GET_UVALUE = georef_obj
	WIDGET_CONTROL, /HOURGLASS

	curwin	= !D.WINDOW
	IF KEYWORD_SET(wid) THEN WSET,wid ELSE WSET, georef_obj -> get_colorbar_window()
	ERASE

	rgbBandIdx		= georef_obj -> get_rgbBandIdx()
	display_rgb		= georef_obj -> get_display_rgb()
	plane_display_status	= [ ( rgbBandIdx[0] GE 0 AND display_rgb[0] ),	$
				    ( rgbBandIdx[1] GE 0 AND display_rgb[1] ),	$
				    ( rgbBandIdx[2] GE 0 AND display_rgb[2] ) ]
	active_plane_idx	= (WHERE( plane_display_status GT 0 ))[0]
;print,'rgbBandIdx = ',rgbBandIdx
;print,'display_rgb = ',display_rgb
;print,'plane_display_status = ',plane_display_status
;print,'active_plane_idx = ',active_plane_idx
;print,'rgbBandIdx[ active_plane_idx ] = ',rgbBandIdx[ active_plane_idx ]
	active_image_obj	= georef_obj -> ReturnImageDataObj( DATA_INDEX = rgbBandIdx[ active_plane_idx ] )
;help,active_image_obj
	datatype		= active_image_obj -> GetNumberType()
;help,datatype
	position		= [0.10, 0.30, 0.90, 0.95]
	bottom			= georef_obj -> get_NUMBER_HISTOGRAM_COLORS()
	ncolors			= !D.N_COLORS
	rangeMin		= *((*( georef_obj -> get_stretchDnBottomPtr() ))[rgbBandIdx[ active_plane_idx ]])
	rangeMax		= *((*( georef_obj -> get_stretchDnTopPtr() ))[rgbBandIdx[ active_plane_idx ]])
	range			= [ rangeMin, rangeMax ]
;help,range
	divisions		= georef_obj -> get_colorbar_divisions()
	charsize		= georef_obj -> get_colorbar_charsize()
	gamma			= *((*( georef_obj -> get_stretchDnGammaPtr() ))[rgbBandIdx[ active_plane_idx ]])
;help,gamma
;print,'rangeMin = ',rangeMin
;print,'rangeMax = ',rangeMax
	rangeDif		= rangeMax - rangeMin
	range_limiter		= rangeMin * 0.0000001
;print,'rangeDif = ',rangeDif
;print,'range_limiter = ',range_limiter
	IF rangeDif LT range_limiter THEN BEGIN

		result	= DIALOG_MESSAGE(					$
				[						$
				'Warning:',					$
				'',						$
				'Insufficient data range for colorbar.',	$
				'Display range is ' + STRTRIM( rangeDif, 2 ),	$
				'',						$
				'Please use the histogram utility',		$
				'to increase the displayed range to at least:',	$
				STRTRIM( range_limiter, 2 )			$
				],						$
				/ERROR )

		WIDGET_CONTROL, WIDGET_INFO( georef_obj -> get_colorbar_drawwidgetid(), /PARENT ), /DESTROY
		RETURN
	ENDIF



	cmap_applies	= colormap_applicable(redraw_required)
;help,cmap_applies

	decompose_equals_zero = 1
	IF KEYWORD_SET(nodecom) THEN decompose_equals_zero = 0
;print,'++++++++++++++++++++++++ DECOMPOSE VALUE ++++++++++++++++++++++++'
;print,'decompose_equals_zero=',decompose_equals_zero
	IF NOT cmap_applies AND decompose_equals_zero THEN BEGIN

		DEVICE, GET_DECOMPOSED = orig_decomposed_value

		DEVICE, DECOMPOSE = 0

	ENDIF

	;-------------------------------------------------------
	; gamma_colorbar.pro provides the colorbar.
	;-------------------------------------------------------
	gamma_colorbar,				$
		DATATYPE	= datatype,	$
		POSITION	= position,	$
		BOTTOM		= bottom,	$
		NCOLORS		= ncolors,	$
		RANGE		= range,	$
		DIVISIONS	= divisions,	$
		CHARSIZE	= charsize,	$
		GAMMA		= gamma

;print,'cmap_applies=',cmap_applies
	IF NOT cmap_applies AND decompose_equals_zero THEN	$

		DEVICE, DECOMPOSE = orig_decomposed_value

	WSET, curwin

	;-------------------------------------------------------
	; Check to make sure the colorbar was created.  If the
	; data range contains only one value, the colorbar will
	; not be created.
	;-------------------------------------------------------
	IF WIDGET_INFO( georef_obj -> get_colorbar_drawwidgetid(), /VALID_ID ) THEN BEGIN
		;-------------------------------------------------------
		; Grey-out the "Default Size" button if
		; current size equals default size.
		;-------------------------------------------------------
		IF  (georef_obj -> get_colorbar_xy_default())[0] EQ (georef_obj -> get_colorbar_xy_current())[0]	$
		AND (georef_obj -> get_colorbar_xy_default())[1] EQ (georef_obj -> get_colorbar_xy_current())[1] THEN BEGIN
			WIDGET_CONTROL, georef_obj -> get_colorbar_sizebutton(), SENSITIVE = 0
		ENDIF ELSE BEGIN
			WIDGET_CONTROL, georef_obj -> get_colorbar_sizebutton(), SENSITIVE = 1
		ENDELSE
	ENDIF ELSE BEGIN
		WIDGET_CONTROL, WIDGET_INFO( georef_obj -> get_colorbar_drawwidgetid(), /PARENT ), /DESTROY
	ENDELSE
END
; colorBar_redraw

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::deal_with_colorbar @@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::deal_with_colorbar

	rgbBandIdx		= [ SELF.redBandIdx, SELF.grnBandIdx, SELF.bluBandIdx ]
	plane_display_status	= [ ( rgbBandIdx[0] GE 0 AND SELF.display_rgb[0] ),	$
				    ( rgbBandIdx[1] GE 0 AND SELF.display_rgb[1] ),	$
				    ( rgbBandIdx[2] GE 0 AND SELF.display_rgb[2] ) ]
	active_plane_idx	= (WHERE( plane_display_status GT 0 ))[0]
	active_image_obj	= SELF -> ReturnImageDataObj( DATA_INDEX = rgbBandIdx[ active_plane_idx ] )
	rangeMin		= *((*( SELF.stretchDnBottomPtr ))[rgbBandIdx[ active_plane_idx ]])
	rangeMax		= *((*( SELF.stretchDnTopPtr ))[rgbBandIdx[ active_plane_idx ]])

	IF rangeMin EQ rangeMax THEN BEGIN

		result	= DIALOG_MESSAGE(					$
				[						$
				'Warning:',					$
				'',						$
				'Data range contains only one value.',		$
				'Colorbar cannot be used unless data', 		$
				'range has two or more values.',		$
				'',						$
				'Use the histogram utility to increase',	$
				'the range.'					$
				],						$
				/ERROR )

		RETURN, 0

	ENDIF ELSE BEGIN

		IF SELF.colorbar_window EQ -1 THEN BEGIN

			curwin	= !D.WINDOW

			SELF.colorbar_tlb		= WIDGET_BASE(								$
								GROUP_LEADER = SELF.parentBase,					$
								KILL_NOTIFY = 'colorbar_kill_notify',				$
								UVALUE = SELF,							$
								TITLE = SELF.title_prefix + '  Resizeable Color Bar',		$
								/TLB_SIZE_EVENTS,						$
								/COLUMN )
			SELF.colorbar_draw		= WIDGET_DRAW( SELF.colorbar_tlb,					$
								XSIZE = SELF.colorbar_xsize_current,				$
								YSIZE = SELF.colorbar_ysize_current,				$
								RETAIN = 2,							$
								/TRACKING_EVENTS )
			SELF.colorbar_button_base	= WIDGET_BASE( SELF.colorbar_tlb, /COLUMN )
			colorbar_button_base1		= WIDGET_BASE( SELF.colorbar_button_base, /ROW )
			colorbar_divisions_droplist	= WIDGET_DROPLIST( colorbar_button_base1,				$
								TITLE = 'Divisions:',						$
								VALUE = STRTRIM( [1,2,3,4,5,6,7,8,9,10], 2 ),			$
								UVALUE = 'divisions' )
			colorbar_charsize_droplist	= WIDGET_DROPLIST( colorbar_button_base1,				$
								TITLE = 'Char Size:',						$
								VALUE = STRTRIM(*(SELF.colorbar_charsizes_allowed_ptr),2),	$
								UVALUE = 'charsize' )
			colorbar_button_base2		= WIDGET_BASE( SELF.colorbar_button_base, /ROW )
			colorbar_tiffbutton		= WIDGET_BUTTON( colorbar_button_base2,					$
								VALUE = 'Save TIFF' )
			SELF.colorbar_sizebutton	= WIDGET_BUTTON( colorbar_button_base2,					$
								VALUE = 'Default Size',						$
								SENSITIVE = 0 )
			colorbar_dismissbutton		= WIDGET_BUTTON( colorbar_button_base2,					$
								VALUE = 'Dismiss' )

			WIDGET_CONTROL, SELF.colorbar_tlb, /REALIZE
			XMANAGER, 'deal_with_colorbar', SELF.colorbar_tlb, EVENT_HANDLER = 'colorBar_event'
			WIDGET_CONTROL, SELF.colorbar_draw, GET_VALUE = colorbar_win
			SELF.colorbar_window = colorbar_win

			WSET, curwin

		ENDIF

		;-------------------------------------------------------
		; Draw the colorbar.
		;-------------------------------------------------------
		colorbar_redraw, TLB = SELF.drawBase

	ENDELSE

	RETURN, 1
END
; GEOREF_IMAGE::deal_with_colorbar

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@ GEOREF_IMAGE::update_pseudocolor_display @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::update_pseudocolor_display
;	SELF->ZoomDisplay, 		$
;		SELF.viewportNX/2, 	$
;		SELF.viewportNY/2, 1.0, 1.0
	SELF->DisplayData
	IF WIDGET_INFO( SELF.colorbar_tlb, /VALID_ID ) THEN $
		colorbar_redraw, TLB = SELF.drawBase
END
; GEOREF_IMAGE::update_pseudocolor_display

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ xloadct_callback @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO xloadct_callback, DATA=data

	COMMON GEOREFIMAGE, linkObj, nullObj, linkMsg, nObj, idArr, commonblock_pseudo_color_table_ptrarr, commonblock_self

	TVLCT, red_table, grn_table, blu_table, /GET
	IF PTR_VALID( commonblock_pseudo_color_table_ptrarr[0] ) THEN	$
		PTR_FREE, commonblock_pseudo_color_table_ptrarr[0]
	IF PTR_VALID( commonblock_pseudo_color_table_ptrarr[1] ) THEN	$
		PTR_FREE, commonblock_pseudo_color_table_ptrarr[1]
	IF PTR_VALID( commonblock_pseudo_color_table_ptrarr[2] ) THEN	$
		PTR_FREE, commonblock_pseudo_color_table_ptrarr[2]
;print,'xloadct_callback -- commonblock_pseudo_color_table_ptrarr     is begin reset to user-selected XLOADCT table.'
;print,'xloadct_callback -- red_table = ',red_table
;print,'xloadct_callback -- grn_table = ',grn_table
;print,'xloadct_callback -- blu_table = ',blu_table
	commonblock_pseudo_color_table_ptrarr[0]	= PTR_NEW( red_table )
	commonblock_pseudo_color_table_ptrarr[1]	= PTR_NEW( grn_table )
	commonblock_pseudo_color_table_ptrarr[2]	= PTR_NEW( blu_table )
	commonblock_self->EightBit_SetColorVectorPtr, RED = red_table
	commonblock_self->EightBit_SetColorVectorPtr, GRN = grn_table
	commonblock_self->EightBit_SetColorVectorPtr, BLU = blu_table

	;Added for pseudocolor mode on Macs and PCs... requires device,decomposed=0 command
	;prior to starting program.   ckt,jul2004
;print,'calling commonblock_self->DisplayData_24Bit'
	IF !D.N_COLORS GT 256 AND KEYWORD_SET(data)THEN 				$
		data->update_pseudocolor_display
END
;xloadct_callback

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::LoadColorTables @@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::LoadColorTables

        COMMON GEOREFIMAGE, linkObj, nullObj, linkMsg, nObj, idArr, commonblock_pseudo_color_table_ptrarr, commonblock_self

	IF PTR_VALID( SELF -> EightBit_GetTranslationPtr() ) THEN BEGIN
		result = DIALOG_MESSAGE( [ 'Warning:',				$
				'',						$
				'Color tables and color bar are not allowed',	$
				'when using Common Colormap in 8-bit mode.',	$
				'',						$
				'Please select Private Colormap.' ], /INFORMATION )
		IF WIDGET_INFO( SELF -> get_colorbar_drawwidgetid(), /VALID_ID ) THEN	$
			WIDGET_CONTROL, WIDGET_INFO( SELF -> get_colorbar_drawwidgetid(), /PARENT ), /DESTROY

		RETURN
	ENDIF

	n_displayed_data	= TOTAL( [							$
					( SELF.redBandIdx GE 0 AND SELF.display_rgb[0] ),	$
					( SELF.grnBandIdx GE 0 AND SELF.display_rgb[1] ),	$
					( SELF.bluBandIdx GE 0 AND SELF.display_rgb[2] ) ] )

	IF n_displayed_data EQ 1 THEN BEGIN

		success		= SELF -> deal_with_colorbar()

		IF NOT success THEN	$
			RETURN

		IF !D.N_COLORS LE 256 THEN BEGIN
			ncolors		= SELF.n_8bit_colors
		ENDIF ELSE BEGIN
			ncolors		= !D.N_COLORS
		ENDELSE

;print,'!D.N_COLORS = ',!D.N_COLORS
;print,'SELF.n_8bit_colors = ',SELF.n_8bit_colors
;print,'ncolors = ',ncolors
;print,'SELF.NUMBER_HISTOGRAM_COLORS = ',SELF.NUMBER_HISTOGRAM_COLORS
;print,'ncolors - SELF.NUMBER_HISTOGRAM_COLORS = ',ncolors - SELF.NUMBER_HISTOGRAM_COLORS
		commonblock_self = SELF
		XLOADCT,									$
			BOTTOM		= SELF.NUMBER_HISTOGRAM_COLORS,				$
			GROUP		= SELF -> get_colorbar_drawwidgetid(),			$
			NCOLORS		= ncolors - SELF.NUMBER_HISTOGRAM_COLORS,		$
			UPDATECALLBACK	= 'xloadct_callback',					$
			UPDATECBDATA = SELF

	ENDIF ELSE IF n_displayed_data EQ 0 THEN BEGIN

		result	= DIALOG_MESSAGE( [ 'No data is currently displayed.',			$
			'Please use the Manage Data utility to display a single band.' ] )
		RETURN

	ENDIF ELSE BEGIN

		result	= DIALOG_MESSAGE( [							$
			'More than one band is currently displayed.',				$
			'Color tables require that a single band be displayed.',		$
			'',									$
			'Please use the Manage Data utility to display a single band.'	] )
		RETURN

	ENDELSE
END
; GEOREF_IMAGE::LoadColorTables

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::ZoomDisplay @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::ZoomDisplay, display_x, display_y, new_data_zoom_x, new_data_zoom_y

	IF SELF.min_zoom_pct GT 0.0 AND SELF.max_zoom_pct GT 0.0 THEN BEGIN
		nDatasets	= (SIZE(*(SELF.dataPtr)))[1]
		done		= 0
		i		= 0
		WHILE NOT done AND i LT nDatasets DO BEGIN
			data_x_zoom_val2check	= (*(SELF.zoomPctX))[i] * new_data_zoom_x
			data_y_zoom_val2check	= (*(SELF.zoomPctY))[i] * new_data_zoom_y
			IF	(									$
					(data_x_zoom_val2check LT SELF.min_zoom_pct ) OR		$
					(data_y_zoom_val2check LT SELF.min_zoom_pct ) OR		$
					(data_x_zoom_val2check GT SELF.max_zoom_pct ) OR		$
					(data_y_zoom_val2check GT SELF.max_zoom_pct )  ) THEN BEGIN
				IF (data_x_zoom_val2check LT SELF.min_zoom_pct ) THEN wrd = 'Minimum' ELSE wrd = 'Maximum'
				msg	= [								$
						wrd + ' zoom encountered... no zooming performed' ]
				res	= DIALOG_MESSAGE(msg, /INFORMATION)
				done	= 1
			ENDIF
			i	= i + 1
		ENDWHILE
		IF done THEN RETURN
	ENDIF

	;=====================================================================
	; Check to see if there is valid lon/lat images; if not, only process
	; other images
	;=====================================================================
	IF  PTR_VALID((SELF.lonImgPtr)[1]) AND PTR_VALID((SELF.latImgPtr)[1]) THEN BEGIN
		;-------------------------------------------------------------
		; Determine display parameters for lon/lat in x-direction
		;-------------------------------------------------------------
		data_x_start	= SELF.lonXS
		data_x_zoom_val	= (SELF.lonLatZoomPctX / 100.0)
		displayX2dataX	= ( FLOAT( display_x ) / data_x_zoom_val ) + data_x_start

		data_y_start	= SELF.lonYS
		data_y_zoom_val	= (SELF.lonLatZoomPctY / 100.0)
		displayY2dataY	= ( FLOAT( display_y ) / data_y_zoom_val ) + data_y_start

		IF	(												$
				(( data_x_zoom_val * new_data_zoom_x * 100.0 ) LT SELF.min_zoom_pct ) OR		$
				(( data_y_zoom_val * new_data_zoom_y * 100.0 ) LT SELF.min_zoom_pct ) OR		$
				(( data_x_zoom_val * new_data_zoom_x * 100.0 ) GT SELF.max_zoom_pct ) OR		$
				(( data_y_zoom_val * new_data_zoom_y * 100.0 ) GT SELF.max_zoom_pct )  ) AND		$
				SELF.min_zoom_pct GT 0.0 AND								$
				SELF.max_zoom_pct GT 0.0 THEN BEGIN
			IF (( data_x_zoom_val * new_data_zoom_x * 100.0 ) LT SELF.min_zoom_pct ) THEN wrd = 'Minimum' ELSE wrd = 'Maximum'
			msg	= [								$
					wrd + ' zoom encountered... no zooming performed' ]
			res	= DIALOG_MESSAGE(msg, /INFORMATION)
			RETURN
		ENDIF

		IF displayX2dataX GE (*((SELF.lonImgPtr)[1]))->GetImageWidth() OR		$
			displayY2dataY GE (*((SELF.lonImgPtr)[1]))->GetImageHeight() THEN BEGIN
			msg	= [								$
					'Zoom center needs to be specified over',		$
					'a pixel within the image...  no zooming performed' ]
			res	= DIALOG_MESSAGE(msg)
			RETURN
		ENDIF

		display_width		= SELF.drawXsize
		data_width		= (*((SELF.lonImgPtr)[1]))->GetImageWidth()
		rev_new_data_zoom_x	= new_data_zoom_x * data_x_zoom_val
		dataX2displayX		= displayX2dataX * rev_new_data_zoom_x
		new_data_x_start	= MAX( [ 0, ( dataX2displayX - ( display_width / 2.0 ) ) / rev_new_data_zoom_x ] )
		new_data_nx		= MIN( [ FLOAT( display_width ) / rev_new_data_zoom_x, data_width - new_data_x_start ] )
		new_data_zoom_val_x	= rev_new_data_zoom_x * 100.0

		display_height		= SELF.drawYsize
		data_height		= (*((SELF.lonImgPtr)[1]))->GetImageHeight()
		rev_new_data_zoom_y	= new_data_zoom_y * data_y_zoom_val
		dataY2displayY		= displayY2dataY * rev_new_data_zoom_y
		new_data_y_start	= MAX( [ 0, ( dataY2displayY - ( display_height / 2.0 ) ) / rev_new_data_zoom_y ] )
		new_data_ny		= MIN( [ FLOAT( display_height ) / rev_new_data_zoom_y, data_height - new_data_y_start ] )
		new_data_zoom_val_y	= rev_new_data_zoom_y * 100.0

		SELF.lonXS		= new_data_x_start
		SELF.lonNX		= new_data_nx

		SELF.latXS		= SELF.lonXS
		SELF.latNX		= SELF.lonNX

		SELF.lonLatZoomPctX	= new_data_zoom_val_x

		SELF.lonYS		= new_data_y_start
		SELF.lonNY		= new_data_ny

		SELF.latYS		= SELF.lonYS
		SELF.latNY		= SELF.lonNY

		SELF.lonLatZoomPctY	= new_data_zoom_val_y
	ENDIF



	;=====================================================================
	; Now process all other images, if any
	;=====================================================================
        nDatasets	= (SIZE(*(SELF.dataPtr)))[1]
        done		= 0
	i		= 0

	WHILE NOT done AND i LT nDatasets DO BEGIN

		data_x_start	= (*(SELF.dataXS))[i]
		data_x_zoom_val	= (*(SELF.zoomPctX))[i] / 100.0
		displayX2dataX	= ( FLOAT( display_x ) / data_x_zoom_val ) + data_x_start

		data_y_start	= (*(SELF.dataYS))[i]
		data_y_zoom_val	= (*(SELF.zoomPctY))[i] / 100.0
		displayY2dataY	= ( FLOAT( display_y ) / data_y_zoom_val ) + data_y_start

	   IF displayX2dataX GE (*((*(SELF.dataPtr))[i]))->GetImageWidth() OR		$
	      displayY2dataY GE (*((*(SELF.dataPtr))[i]))->GetImageHeight() THEN BEGIN
		msg	= [ 'Zoom center needs to be specified over',		$
		            'a pixel within the image...  no zooming performed' ]
		res	= DIALOG_MESSAGE(msg)
		done	= 1
	   ENDIF
	   i	= i + 1
	ENDWHILE

	IF done THEN BEGIN
		RETURN
	ENDIF

	FOR i = 0, nDatasets - 1 DO BEGIN
		data_x_start		= (*(SELF.dataXS))[i]
		display_width		= SELF.drawXsize
		data_width		= (*((*(SELF.dataPtr))[i]))->GetImageWidth()
		data_x_zoom_val		= (*(SELF.zoomPctX))[i] / 100.0
		displayX2dataX		= ( FLOAT( display_x ) / data_x_zoom_val ) + data_x_start
		rev_new_data_zoom_x	= new_data_zoom_x * data_x_zoom_val
		dataX2displayX		= displayX2dataX * rev_new_data_zoom_x
		new_data_x_start	= MAX( [ 0, ( dataX2displayX - ( display_width / 2.0 ) ) / rev_new_data_zoom_x ] )
		new_data_nx		= MIN( [ FLOAT( display_width ) / rev_new_data_zoom_x, data_width - new_data_x_start ] )
		new_data_zoom_val_x	= rev_new_data_zoom_x * 100.0

		data_y_start		= (*(SELF.dataYS))[i]
		display_height		= SELF.drawYsize
		data_height		= (*((*(SELF.dataPtr))[i]))->GetImageHeight()
		data_y_zoom_val		= (*(SELF.zoomPctY))[i] / 100.0
		displayY2dataY		= ( FLOAT( display_y ) / data_y_zoom_val ) + data_y_start
		rev_new_data_zoom_y	= new_data_zoom_y * data_y_zoom_val
		dataY2displayY		= displayY2dataY * rev_new_data_zoom_y
		new_data_y_start	= MAX( [ 0, ( dataY2displayY - ( display_height / 2.0 ) ) / rev_new_data_zoom_y ] )
		new_data_ny		= MIN( [ FLOAT( display_height ) / rev_new_data_zoom_y, data_height - new_data_y_start ] )
		new_data_zoom_val_y	= rev_new_data_zoom_y * 100.0

		(*(SELF.dataXS))[i]	= new_data_x_start
		(*(SELF.dataNX))[i]	= new_data_nx
;print,'IN ZoomDisplay, i,(*(SELF.dataXS))[i],(*(SELF.dataNX))[i] = ',i,(*(SELF.dataXS))[i],(*(SELF.dataNX))[i]
		(*(SELF.zoomPctX))[i]	= new_data_zoom_val_x
		(*(SELF.dataYS))[i]	= new_data_y_start
		(*(SELF.dataNY))[i]	= new_data_ny
		(*(SELF.zoomPctY))[i]	= new_data_zoom_val_y
        ENDFOR

	;=====================================================================
	; Set viewport NX and NY appropriately
	;=====================================================================
	SELF.viewportNX	= ROUND( FLOAT( new_data_nx ) * rev_new_data_zoom_x )
	SELF.viewportNY	= ROUND( FLOAT( new_data_ny ) * rev_new_data_zoom_y )


END
; GEOREF_IMAGE::ZoomDisplay

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::ToggleClosestPoint @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::ToggleClosestPoint, x, y, nClicks

   IF NOT PTR_VALID(SELF.tabularDataPtr) THEN RETURN

   lonLat		= SELF->GetLonLat(x,y)

   IF lonLat[0] LT -180.0 THEN RETURN
   tmpWin	= !D.WINDOW
   WSET, SELF.windowID
   nSets		= N_ELEMENTS(*(SELF.tabularDataPtr))

   globalMinDiff	= 999.0
   setIdx		= (-1)
   ptIdx		= (-1)

   FOR i = 0, nSets - 1 DO BEGIN
      lonDiffImg = ABS((*((*(SELF.tabularDataPtr))[i]))[0,*] - lonLat[0])
      latDiffImg = ABS((*((*(SELF.tabularDataPtr))[i]))[1,*] - lonLat[1])
      currMinDiff= MIN( [ MIN(lonDiffImg+latDiffImg), globalMinDiff ] )

      IF currMinDiff NE globalMinDiff THEN BEGIN
         minIdx     = WHERE(lonDiffImg+latDiffImg EQ currMinDiff)
         lon        = (*((*(SELF.tabularDataPtr))[i]))[0,minIdx[0]]
         lat        = (*((*(SELF.tabularDataPtr))[i]))[1,minIdx[0]]

         lonXYlatXY	= (*((SELF.lonImgPtr)[1]))->ReturnClosestXYMatchForDualValues(	$
	                   lon, (*((SELF.latImgPtr)[1])), lat )

         xx=lonXYlatXY[0]
         yy=lonXYlatXY[1]

         IF lonXYlatXY[0,0] GE 0 THEN BEGIN
            viewX = ROUND( FLOAT(xx-SELF.lonXS) * (FLOAT(SELF.viewportNX)/FLOAT(SELF.lonNX)) )
            viewY = ROUND( FLOAT(yy-SELF.lonYS) * (FLOAT(SELF.viewportNY)/FLOAT(SELF.lonNY)) )
            IF (viewX GE 0 AND viewX LT SELF.viewportNX) AND		$
               (viewY GE 0 AND viewY LT SELF.viewportNY) THEN BEGIN
               dist = SQRT(((ABS(viewX-x))^2)+((ABS(viewY-y))^2))
               IF dist LE SELF.distTolerance THEN BEGIN
                  globalMinDiff = currMinDiff
                  setIdx        = i
                  ptIdx         = minIdx[0]
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDFOR

   IF setIdx GE 0 THEN BEGIN
      CASE 1 OF
         nClicks GE 2: (*((*(SELF.selectedTabularDataPtr))[setIdx]))[ptIdx] = 1
         ELSE: (*((*(SELF.selectedTabularDataPtr))[setIdx]))[ptIdx] = 	$
            (*((*(SELF.selectedTabularDataPtr))[setIdx]))[ptIdx] EQ 0
      ENDCASE
      PLOTS, viewX,							$
      	     viewY,							$
      	     PSYM = setIdx + 1,						$
      	     COLOR =							$
      	     SELF.tabularDataColors[(*((*(SELF.selectedTabularDataPtr))[setIdx]))[ptIdx],setIdx], $
      	     /DEVICE
   ENDIF

   WSET, tmpWin
END
; GEOREF_IMAGE::ToggleClosestPoint

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@ GGEOREF_IMAGE::ClearSelectedTabularData @@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::ClearSelectedTabularData
  IF NOT PTR_VALID(SELF.tabularDataPtr) THEN RETURN

  FOR i = 0, N_ELEMENTS(*(SELF.tabularDataPtr)) - 1 DO		$
      *((*(SELF.selectedTabularDataPtr))[i]) = *((*(SELF.selectedTabularDataPtr))[i]) * 0
END
; GEOREF_IMAGE::ClearSelectedTabularData

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::DisplayTabularData @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::DisplayTabularData
   IF NOT PTR_VALID(SELF.tabularDataPtr) THEN RETURN

   tmpWin	= !D.WINDOW
   WSET, SELF.windowID

   nSets         = N_ELEMENTS(*(SELF.tabularDataPtr))

   i = 0
   found = 0
   WHILE i LT nSets AND NOT found DO BEGIN
      check = WHERE( *((*(SELF.selectedTabularDataPtr))[i]) GE 1, cnt )
      IF cnt GT 0 THEN found = 1
      i = i + 1
   ENDWHILE

   viewXY        = LONARR(2)
   currentSymbol = 0

   IF NOT found THEN BEGIN
      minX	= MIN( [ SELF.selectAnchorX, SELF.currentSelectX ], MAX = maxX )
      minY	= MIN( [ SELF.selectAnchorY, SELF.currentSelectY ], MAX = maxY )
   ENDIF

   FOR i = 0, nSets - 1 DO BEGIN
      currentSymbol	= currentSymbol + 1
      d_sz  = SIZE(*((*(SELF.tabularDataPtr))[i]))
      IF d_sz[0] EQ 1 THEN nPts = 1 ELSE nPts = d_sz[d_sz[0]]

;      nPts	= (SIZE(*((*(SELF.tabularDataPtr))[i])))[2]

      FOR j = 0, nPts - 1 DO BEGIN
         lon		= (*((*(SELF.tabularDataPtr))[i]))[0,j]
         lat		= (*((*(SELF.tabularDataPtr))[i]))[1,j]

         lonXYlatXY	= (*((SELF.lonImgPtr)[1]))->ReturnClosestXYMatchForDualValues(	$
         			lon, (*((SELF.latImgPtr)[1])), lat )

         IF lonXYlatXY[0,0] LT 0 THEN offMap = 1 ELSE offMap = 0
         ;
         ;for now, set x and y to lonXYlatXY[0,0] and [0,1], respectively
         ;
         x=lonXYlatXY[0]
         y=lonXYlatXY[1]
         ;-------------------------------------------------------------
         ; Convert the data from x/y coordinates to viewport x/y coordinates.
         ;-------------------------------------------------------------
         viewXY[0] = ROUND( FLOAT(x-SELF.lonXS) * (FLOAT(SELF.viewportNX)/FLOAT(SELF.lonNX)) )
         viewXY[1] = ROUND( FLOAT(y-SELF.lonYS) * (FLOAT(SELF.viewportNY)/FLOAT(SELF.lonNY)) )
         IF (viewXY[0] LT 0 OR viewXY[0] GE SELF.viewportNX) OR		$
            (viewXY[1] LT 0 OR viewXY[1] GE SELF.viewportNY) THEN offMap = 1

	 IF NOT offMap THEN BEGIN
	    CASE found OF
	       0: BEGIN
	          IF viewXY[0] GE minX AND viewXY[0] LE maxX AND		$
		     viewXY[1] GE minY AND viewXY[1] LE maxY AND		$
		      minX NE -999 AND maxX NE -999 AND				$
		      minY NE -999 AND maxY NE -999 THEN BEGIN
		      color2Use = SELF.tabularDataColors[1,i]
	              (*((*(SELF.selectedTabularDataPtr))[i]))[j] = 1
		  ENDIF ELSE BEGIN
		      color2Use = SELF.tabularDataColors[0,i]
	              (*((*(SELF.selectedTabularDataPtr))[i]))[j] = 0
		  ENDELSE
 	          END
	       1: BEGIN
	             IF (*((*(SELF.selectedTabularDataPtr))[i]))[j] THEN	$
		      color2Use = SELF.tabularDataColors[1,i]			$
		  ELSE								$
		      color2Use = SELF.tabularDataColors[0,i]
	          END
	       ELSE:
	    ENDCASE
            PLOTS, viewXY[0], viewXY[1], PSYM = currentSymbol, COLOR = color2Use, /DEVICE
	 ENDIF

      ENDFOR
   ENDFOR

   WSET, tmpWin
END
; GEOREF_IMAGE::DisplayTabularData

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::InitializeDataMinMax @@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::InitializeDataMinMax
	;=======================================================================
	; Get and store min and max of each displayed channel.  Initialize
	; bottom and top stretchDn values with a 1% clip (at each end of the
	; range) to provide adequate default viewing.
	;=======================================================================
	rgb_idx	= [ SELF.redBandIdx, SELF.grnBandIdx, SELF.bluBandIdx ]

	FOR i = 0, 2 DO BEGIN
	IF rgb_idx[ i ] GE 0 THEN BEGIN
		xs	= (*(SELF.dataXS))[rgb_idx[i]]
		xe	= xs + (*(SELF.dataNX))[rgb_idx[i]] - 1
		ys	= (*(SELF.dataYS))[rgb_idx[i]]
		ye	= ys + (*(SELF.dataNY))[rgb_idx[i]] - 1

		minVal	= (*((*(SELF.dataPtr))[rgb_idx[i]]))->GetMinVal(xs,xe,ys,ye,xe-xs+1L,ye-ys+1L)
		maxVal	= (*((*(SELF.dataPtr))[rgb_idx[i]]))->GetMaxVal(xs,xe,ys,ye,xe-xs+1L,ye-ys+1L)

		PTR_FREE, (*(SELF.dataMinValPtr))[rgb_idx[i]]
		PTR_FREE, (*(SELF.dataMaxValPtr))[rgb_idx[i]]

		;===============================================================
		; min/max of data currently displayed.
		;===============================================================
		(*(SELF.dataMinValPtr))[rgb_idx[i]]	= PTR_NEW( minVal )
		(*(SELF.dataMaxValPtr))[rgb_idx[i]]	= PTR_NEW( maxVal )
	ENDIF
	ENDFOR
END
; GEOREF_IMAGE::InitializeDataMinMax

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::InitializeDataLimits @@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::InitializeDataLimits, statePtr
	;=======================================================================
	; Get and store min and max of each displayed channel.  Initialize
	; bottom and top stretchDn values with a 1% clip (at each end of the
	; range) to provide adequate default viewing.
	;=======================================================================
	rgb_idx	= [ SELF.redBandIdx, SELF.grnBandIdx, SELF.bluBandIdx ]

	FOR i = 0, 2 DO BEGIN
		IF rgb_idx[ i ] GE 0 THEN BEGIN
			xs	= 0
			xe	= (*((*(SELF.dataPtr))[rgb_idx[i]]))->GetImageWidth() - 1L
			ys	= 0
			ye	= (*((*(SELF.dataPtr))[rgb_idx[i]]))->GetImageHeight() - 1L

			minVal	= (*((*(SELF.dataPtr))[rgb_idx[i]]))->GetMinVal(xs,xe,ys,ye,xe-xs+1L,ye-ys+1L)
			maxVal	= (*((*(SELF.dataPtr))[rgb_idx[i]]))->GetMaxVal(xs,xe,ys,ye,xe-xs+1L,ye-ys+1L)
;print,'GEOREF_IMAGE::InitializeDataLimits minVal,maxVal = ',minVal,maxVal

			PTR_FREE, (*(SELF.bottomLimitPtr))[rgb_idx[i]]
			PTR_FREE, (*(SELF.topLimitPtr))[rgb_idx[i]]

			;===============================================================
;			; min/max of data currently displayed.
			;===============================================================
			(*(SELF.bottomLimitPtr))[rgb_idx[i]]	= PTR_NEW( minVal )
			(*(SELF.topLimitPtr))[rgb_idx[i]]	= PTR_NEW( maxVal )
;print,'InitializeDataLimits [minVal,maxVal] = ',minVal,maxVal
			IF WIDGET_INFO( SELF.stretchDnBase, /VALID_ID ) THEN BEGIN
				WIDGET_CONTROL, (*statePtr).bottomLimitText[i],		SET_VALUE = STRTRIM( minVal, 2 )
				WIDGET_CONTROL, (*statePtr).bottomLimitText[i+3],	SET_VALUE = STRTRIM( minVal, 2 )
				WIDGET_CONTROL, (*statePtr).topLimitText[i],		SET_VALUE = STRTRIM( maxVal, 2 )
				WIDGET_CONTROL, (*statePtr).topLimitText[i+3],		SET_VALUE = STRTRIM( maxVal, 2 )

;				idx			= rgb_idx[i]
;				dataMinLim		= DOUBLE( *((*((*statePtr).bottomLimitPtr))[idx]) )
;				dataMaxLim		= DOUBLE( *((*((*statePtr).topLimitPtr))[idx]) )
;				dataTop			= DOUBLE( *((*((*statePtr).stretchDnTopPtr))[idx]) )
;				newTopSliderValue 	= ROUND( 1000.0d * ABS( ( dataTop - dataMinLim ) / ( dataMaxLim - dataMinLim ) ) )
;print,'newTopSliderValue = ',newTopSliderValue
;				WIDGET_CONTROL, (*statePtr).topSliders[whichColor], SET_VALUE = newTopSliderValue
;				WIDGET_CONTROL, (*statePtr).topSliders[otherBillboard], SET_VALUE = newTopSliderValue
;
;				dataMinLim		= DOUBLE( *((*((*statePtr).bottomLimitPtr))[idx]) )
;				dataMaxLim		= DOUBLE( *((*((*statePtr).topLimitPtr))[idx]) )
;				dataBottom		= DOUBLE( *((*((*statePtr).stretchDnBottomPtr))[idx]) )
;				newBottomSliderValue 	= ROUND( 1000.0d * ABS( ( dataBottom - dataMinLim ) / ( dataMaxLim - dataMinLim ) ) )
;print,'newBottomSliderValue = ',newBottomSliderValue
;				WIDGET_CONTROL, (*statePtr).bottomSliders[whichColor], SET_VALUE = newBottomSliderValue
;				WIDGET_CONTROL, (*statePtr).bottomSliders[otherBillboard], SET_VALUE = newBottomSliderValue
			ENDIF
		ENDIF
	ENDFOR
END
; GEOREF_IMAGE::InitializeDataLimits

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::GetZoomFactor @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::GetZoomFactor
	;---------------------------------------------------------------------
	; Get the starting and ending X values that represent the image
	; area to be displayed.
	;---------------------------------------------------------------------
;;;ckt,oct1999	xs = (*(SELF.dataXS))[SELF.dataIdx]
;;;ckt,oct1999	xe = xs + (*(SELF.dataNX))[SELF.dataIdx] - 1
;;;ckt,oct1999print,'SELF.dataIdx = ',SELF.dataIdx
;;;ckt,oct1999print,'xs,xe=',xs,xe
	;---------------------------------------------------------------------
	; The returned zoom factor is calculated in the X dimension only.  The
	; Y dimension should be the same.
	;---------------------------------------------------------------------
	RETURN, (*(SELF.zoomPctX))[SELF.dataIdx] / 100.0
;;;ckt,oct1999	RETURN, FLOAT(SELF.viewportNX)/FLOAT(xe-xs+1)
END
; GEOREF_IMAGE::GetZoomFactor

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::DisplayData_24Bit @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::DisplayData_24Bit

	COMMON GEOREFIMAGE, linkObj, nullObj, linkMsg, nObj, idArr, commonblock_pseudo_color_table_ptrarr, commonblock_self

	plane_display_status	= [ ( SELF.redBandIdx GE 0 AND SELF.display_rgb[0] ),	$
				    ( SELF.grnBandIdx GE 0 AND SELF.display_rgb[1] ),	$
				    ( SELF.bluBandIdx GE 0 AND SELF.display_rgb[2] ) ]

	;------------------------------------------------------------------------
	; First determine how many planes contain DISPLAYED DATA
	; DISPLAYED DATA requires that data is loaded into a
	; particular band AND that band is currently being displayed
	;------------------------------------------------------------------------
	n_displayed_data		= TOTAL(plane_display_status)
	active_plane_idx		= WHERE(plane_display_status GT 0)
	img_idx				= [ SELF.redBandIdx, SELF.grnBandIdx, SELF.bluBandIdx ]

	CASE n_displayed_data OF
		1: BEGIN
;print,'ckt->ready to display data'


			IF WIDGET_INFO( SELF -> get_colorbar_drawwidgetid(), /VALID_ID ) THEN BEGIN
				TVLCT,								$
					*(commonblock_pseudo_color_table_ptrarr[0]),		$
					*(commonblock_pseudo_color_table_ptrarr[1]),		$
					*(commonblock_pseudo_color_table_ptrarr[2])
			ENDIF

			cmap_applies	= colormap_applicable( redraw_required )

			IF NOT cmap_applies THEN BEGIN
				DEVICE, GET_DECOMPOSED = orig_decomposed_value
				DEVICE, DECOMPOSED = 0
			ENDIF


			data_2_display = SELF->GetBand( img_idx[active_plane_idx[0]],		$
				active_plane_idx[0],						$
				/RETURN_DISPLAY_ONLY )
			TV, data_2_display, CHANNEL = 0

			IF NOT cmap_applies THEN						$
				DEVICE, DECOMPOSED = orig_decomposed_value

		END
		ELSE: BEGIN
			IF WIDGET_INFO( SELF.colorbar_draw, /VALID_ID ) THEN BEGIN
				WIDGET_CONTROL, WIDGET_INFO( SELF.colorbar_draw, /PARENT ), /DESTROY
;print,'GEOREF_IMAGE::DisplayData_24Bit -- LOADCT, 0'
				LOADCT, 0
			ENDIF

			FOR i = 0, N_ELEMENTS(active_plane_idx) - 1 DO BEGIN
				data_2_display = SELF->GetBand( img_idx[active_plane_idx[i]],	$
					active_plane_idx[i],				$
					/RETURN_DISPLAY_ONLY )
				TV, data_2_display, CHANNEL = active_plane_idx[i] + 1
				IF *(SELF.calc_hist_for_displayed_data_ptr) THEN	$
					SELF->CalculateHistogram,			$
						img_idx[active_plane_idx[i]],		$
						active_plane_idx[i]
			ENDFOR
			END
	ENDCASE

END
; GEOREF_IMAGE::DisplayData_24Bit

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::DisplayData_8Bit @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::DisplayData_8Bit, SET_TRANSLATION = setTranslation, $
                                    USE_TRANSLATION = useStruct,      $
                                    SHARED_MAP = sharedMap

	COMMON GEOREFIMAGE, linkObj, nullObj, linkMsg, nObj, idArr, commonblock_pseudo_color_table_ptrarr, commonblock_self

	TRUE                            = 1
	FALSE                           = 0
	bw_img                          = (-1)
	shared_map                      = FALSE

	IF KEYWORD_SET(sharedMap) THEN shared_map = TRUE

	CASE TRUE OF
		KEYWORD_SET(useStruct): BEGIN
			IF PTR_VALID(SELF.red_vec_ptr) THEN PTR_FREE, SELF.red_vec_ptr
			IF PTR_VALID(SELF.grn_vec_ptr) THEN PTR_FREE, SELF.grn_vec_ptr
			IF PTR_VALID(SELF.blu_vec_ptr) THEN PTR_FREE, SELF.blu_vec_ptr
			IF PTR_VALID(SELF.trans_vec_ptr) THEN PTR_FREE, SELF.trans_vec_ptr

;print,'GEOREF_IMAGE::DisplayData_8Bit -- setting SELF.red_vec_ptr to: ', useStruct.r_vec
;print,'GEOREF_IMAGE::DisplayData_8Bit -- setting SELF.grn_vec_ptr to: ', useStruct.g_vec
;print,'GEOREF_IMAGE::DisplayData_8Bit -- setting SELF.blu_vec_ptr to: ', useStruct.b_vec
			SELF.red_vec_ptr	= PTR_NEW(useStruct.r_vec, /NO_COPY)
			SELF.grn_vec_ptr	= PTR_NEW(useStruct.g_vec, /NO_COPY)
			SELF.blu_vec_ptr	= PTR_NEW(useStruct.b_vec, /NO_COPY)
			SELF.trans_vec_ptr	= PTR_NEW(useStruct.trans, /NO_COPY)
			shared_map		= TRUE
			END
		KEYWORD_SET(setTranslation): BEGIN
;print,'GEOREF_IMAGE::DisplayData_8Bit -- PTR_FREE, SELF.red_vec_ptr, grn and blu also.'
			IF PTR_VALID(SELF.red_vec_ptr) THEN PTR_FREE, SELF.red_vec_ptr
			IF PTR_VALID(SELF.grn_vec_ptr) THEN PTR_FREE, SELF.grn_vec_ptr
			IF PTR_VALID(SELF.blu_vec_ptr) THEN PTR_FREE, SELF.blu_vec_ptr
			IF PTR_VALID(SELF.trans_vec_ptr) THEN PTR_FREE, SELF.trans_vec_ptr
			END
		ELSE: BEGIN
			END
	ENDCASE

	plane_display_status	= [ ( SELF.redBandIdx GE 0 AND SELF.display_rgb[0] ),	$
				    ( SELF.grnBandIdx GE 0 AND SELF.display_rgb[1] ),	$
				    ( SELF.bluBandIdx GE 0 AND SELF.display_rgb[2] ) ]

	;------------------------------------------------------------------------
	; First determine how many planes contain DISPLAYED DATA
	; DISPLAYED DATA requires that data is loaded into a
	; particular band AND that band is currently being displayed
	;------------------------------------------------------------------------
	n_displayed_data		= TOTAL(plane_display_status)
	active_plane_idx		= WHERE(plane_display_status GT 0)
	img_idx				= [ SELF.redBandIdx, SELF.grnBandIdx, SELF.bluBandIdx ]

	CASE n_displayed_data OF
		1: bw_img = SELF->GetBand( img_idx[active_plane_idx[0]],		$
				active_plane_idx[0],					$
				/RETURN_DISPLAY_ONLY )
		ELSE: BEGIN
			red_img	= SELF->GetBand( SELF.redBandIdx, 0, /RETURN_DISPLAY_ONLY )
			grn_img	= SELF->GetBand( SELF.grnBandIdx, 1, /RETURN_DISPLAY_ONLY )
			blu_img	= SELF->GetBand( SELF.bluBandIdx, 2, /RETURN_DISPLAY_ONLY )
			FOR i = 0, N_ELEMENTS(active_plane_idx) - 1 DO BEGIN
				IF *(SELF.calc_hist_for_displayed_data_ptr) THEN	$
					SELF->CalculateHistogram,			$
						img_idx[active_plane_idx[i]],		$
						active_plane_idx[i]
			ENDFOR
			END
	ENDCASE

	bw_img_exists = (SIZE(bw_img))[0] GT 0

	IF bw_img_exists AND (PTR_VALID(SELF.trans_vec_ptr) OR shared_map) THEN BEGIN
		red_img	= bw_img
		grn_img = bw_img
		blu_img = bw_img
	ENDIF

;print,''
;print,''
;print,''
;print,'bw_img_exists = ',bw_img_exists,'    should be yes.'
;print,'PTR_VALID(SELF.trans_vec_ptr) = ',PTR_VALID(SELF.trans_vec_ptr),'    should be no.'
;print,'shared_map = ',shared_map,'   should be no.'
	CASE TRUE OF
		;----------------------------------------------------------------------------------
		; case 1: only one band loaded, translation pointer not set, not a shared color map
		;----------------------------------------------------------------------------------
		bw_img_exists AND NOT PTR_VALID(SELF.trans_vec_ptr) AND NOT shared_map: BEGIN
			r_bw				= BYTARR(SELF.n_8bit_colors)
			r_bw[ SELF.NUMBER_HISTOGRAM_COLORS :SELF.n_8bit_colors-1] = $
				CONGRID(BINDGEN(256),SELF.n_8bit_colors - SELF.NUMBER_HISTOGRAM_COLORS )
			g_bw				= r_bw
			b_bw				= r_bw

			;----------------------------------------------------------------------------------
			; rgbPrimary		= [255L,191L,191L]			; Active histogram color (default = RED).
			; rgbSecondary		= [127L,63L,63L]			; Passive histograms color (default = RED).
			; rgbBackColor		= [63L,0L,0L]
			; rgbDrawColor		= [255L,223L,223L]
			;R primary, secondary, back, draw (indices 0-3)
			;----------------------------------------------------------------------------------

			r_bw[0: SELF.NUMBER_HISTOGRAM_COLORS-1 ]	=		$
			;----------------------------------------------------------------------------------
			;R primary, secondary, back, draw (indices 0-3)
			;----------------------------------------------------------------------------------
				[    255,      127,     63,   255,			$
				     191,       63,      0,   223,			$
				     191,       63,      0,   223 ]

			g_bw[0: SELF.NUMBER_HISTOGRAM_COLORS-1 ]	=		$
				[    191,       63,      0,   223,			$
			;----------------------------------------------------------------------------------
			;G primary, secondary, back, draw (indices 4-7)
			;----------------------------------------------------------------------------------
				     255,      127,     63,   255,			$
				     191,       63,      0,   223 ]

			 b_bw[0: SELF.NUMBER_HISTOGRAM_COLORS-1 ]	=		$
				[    191,       63,      0,   223,			$
				     191,       63,      0,   223,			$
			;----------------------------------------------------------------------------------
			;B primary, secondary, back, draw (indices 8-11)
			;----------------------------------------------------------------------------------
				     255,      127,     63,   255 ]

;print,'GEOREF_IMAGE::DisplayData_8Bit -- PTR_FREE, SELF.red_vec_ptr, grn and blu also.'
			PTR_FREE, SELF.red_vec_ptr
			PTR_FREE, SELF.grn_vec_ptr
			PTR_FREE, SELF.blu_vec_ptr
;print,'WIDGET_INFO( SELF -> get_colorbar_drawwidgetid(), /VALID_ID ) = ',WIDGET_INFO( SELF -> get_colorbar_drawwidgetid(), /VALID_ID )
			IF WIDGET_INFO( SELF -> get_colorbar_drawwidgetid(), /VALID_ID ) THEN BEGIN
;print,'GEOREF_IMAGE::DisplayData_8Bit -- set to commonblock colors'
				r_vec			= *(commonblock_pseudo_color_table_ptrarr[0])
				g_vec			= *(commonblock_pseudo_color_table_ptrarr[1])
				b_vec			= *(commonblock_pseudo_color_table_ptrarr[2])
;print,'GEOREF_IMAGE::DisplayData_8Bit -- r_vec = ',r_vec
;print,'GEOREF_IMAGE::DisplayData_8Bit -- g_vec = ',g_vec
;print,'GEOREF_IMAGE::DisplayData_8Bit -- b_vec = ',b_vec
				SELF.red_vec_ptr	= PTR_NEW( r_vec )
				SELF.grn_vec_ptr	= PTR_NEW( g_vec )
				SELF.blu_vec_ptr	= PTR_NEW( b_vec )
			ENDIF ELSE BEGIN
;print,'GEOREF_IMAGE::DisplayData_8Bit -- set to bw colors'
;print,'GEOREF_IMAGE::DisplayData_8Bit -- r_bw = ',r_bw
;print,'GEOREF_IMAGE::DisplayData_8Bit -- g_bw = ',g_bw
;print,'GEOREF_IMAGE::DisplayData_8Bit -- b_bw = ',b_bw
				SELF.red_vec_ptr	= PTR_NEW( r_bw, /NO_COPY )
				SELF.grn_vec_ptr	= PTR_NEW( g_bw, /NO_COPY )
				SELF.blu_vec_ptr	= PTR_NEW( b_bw, /NO_COPY )
			ENDELSE
;print,'GEOREF_IMAGE::DisplayData_8Bit -- TVLCT, *(SELF.red_vec_ptr), *(SELF.grn_vec_ptr), *(SELF.blu_vec_ptr)'
;print,'GEOREF_IMAGE::DisplayData_8Bit -- *(SELF.red_vec_ptr) = ',*(SELF.red_vec_ptr)
;print,'GEOREF_IMAGE::DisplayData_8Bit -- *(SELF.grn_vec_ptr) = ',*(SELF.grn_vec_ptr)
;print,'GEOREF_IMAGE::DisplayData_8Bit -- *(SELF.blu_vec_ptr) = ',*(SELF.blu_vec_ptr)
			TVLCT, *(SELF.red_vec_ptr), *(SELF.grn_vec_ptr), *(SELF.blu_vec_ptr)
			TV, BYTSCL(bw_img,MIN=0,MAX=255,TOP=SELF.n_8bit_colors - SELF.NUMBER_HISTOGRAM_COLORS ) + SELF.NUMBER_HISTOGRAM_COLORS

			END
		;----------------------------------------------------------------------------------
		; case 2: translation pointer set, implying more than one band loaded or shared
		; color map
		;----------------------------------------------------------------------------------
		PTR_VALID(SELF.trans_vec_ptr): BEGIN
			IF WIDGET_INFO( SELF -> get_colorbar_drawwidgetid(), /VALID_ID ) THEN $
				WIDGET_CONTROL, WIDGET_INFO( SELF -> get_colorbar_drawwidgetid(), /PARENT ), /DESTROY
			TV, COLOR_QUAN( red_img, grn_img, blu_img, rTab, gTab, bTab,			$
				COLORS = SELF.n_8bit_colors - SELF.NUMBER_HISTOGRAM_COLORS , TRANSLATION = *(SELF.trans_vec_ptr) )
			END
		;----------------------------------------------------------------------------------
		; case 3: translation pointer not set, implying only that translation vector needs
		; to be created
		;----------------------------------------------------------------------------------
		ELSE: BEGIN
			IF WIDGET_INFO( SELF -> get_colorbar_drawwidgetid(), /VALID_ID ) THEN $
				WIDGET_CONTROL, WIDGET_INFO( SELF -> get_colorbar_drawwidgetid(), /PARENT ), /DESTROY
			IF shared_map THEN BEGIN
				TV, COLOR_QUAN( red_img, grn_img, blu_img, rTab, gTab, bTab,		$
					COLORS = SELF.n_8bit_colors - SELF.NUMBER_HISTOGRAM_COLORS,	$
					GET_TRANSLATION = tmp_trans, /MAP_ALL ) + SELF.NUMBER_HISTOGRAM_COLORS
			ENDIF ELSE BEGIN
				TV, COLOR_QUAN( red_img, grn_img, blu_img, rTab, gTab, bTab,		$
					COLORS = SELF.n_8bit_colors - SELF.NUMBER_HISTOGRAM_COLORS,	$
					GET_TRANSLATION = tmp_trans ) + SELF.NUMBER_HISTOGRAM_COLORS
			ENDELSE
			rr							= BYTARR(SELF.n_8bit_colors)
			rr[ SELF.NUMBER_HISTOGRAM_COLORS :SELF.n_8bit_colors-1]	= rTab
			gg							= rr
			gg[ SELF.NUMBER_HISTOGRAM_COLORS :SELF.n_8bit_colors-1]	= gTab
			bb 							= rr
			bb[ SELF.NUMBER_HISTOGRAM_COLORS :SELF.n_8bit_colors-1]	= bTab
			tt 							= rr
			tmp_trans						= tmp_trans + SELF.NUMBER_HISTOGRAM_COLORS

			;---------------------------------------------------------------------------
			;	rgbPrimary		= [255L,191L,191L]		; Active histogram color (default = RED).
			;	rgbSecondary		= [127L,63L,63L]		; Passive histograms color (default = RED).
			;	rgbBackColor		= [63L,0L,0L]
			;	rgbDrawColor		= [255L,223L,223L]
			;---------------------------------------------------------------------------
			rr[0: SELF.NUMBER_HISTOGRAM_COLORS - 1 ] =		$
			;---------------------------------------------------------------------------
			;R primary, secondary, back, draw (indices 0-3)
			;---------------------------------------------------------------------------
				[    255,      127,     63,   255,		$
				     191,       63,      0,   223,		$
				     191,       63,      0,   223 ]
			gg[0: SELF.NUMBER_HISTOGRAM_COLORS - 1 ] =		$
				[    191,       63,      0,   223,		$
			;---------------------------------------------------------------------------
			;G primary, secondary, back, draw (indices 4-7)
			;---------------------------------------------------------------------------
				     255,      127,     63,   255,		$
				     191,       63,      0,   223 ]
			bb[0: SELF.NUMBER_HISTOGRAM_COLORS - 1 ] =		$
				[    191,       63,      0,   223,		$
				     191,       63,      0,   223,		$
			;---------------------------------------------------------------------------
			;B primary, secondary, back, draw (indices 8-11)
			;---------------------------------------------------------------------------
				     255,      127,     63,   255 ]
			PTR_FREE, SELF.trans_vec_ptr
			SELF.trans_vec_ptr	= PTR_NEW(tmp_trans,/NO_COPY)
;print,'GEOREF_IMAGE::DisplayData_8Bit -- rr = ',rr
;print,'GEOREF_IMAGE::DisplayData_8Bit -- gg = ',gg
;print,'GEOREF_IMAGE::DisplayData_8Bit -- bb = ',bb
			SELF.red_vec_ptr	= PTR_NEW(rr, /NO_COPY)
			SELF.grn_vec_ptr	= PTR_NEW(gg, /NO_COPY)
			SELF.blu_vec_ptr	= PTR_NEW(bb, /NO_COPY)
			END
	ENDCASE

;print,'GEOREF_IMAGE::DisplayData_8Bit -- TVLCT, *(SELF.red_vec_ptr), *(SELF.grn_vec_ptr), *(SELF.blu_vec_ptr)'
	TVLCT, *(SELF.red_vec_ptr), *(SELF.grn_vec_ptr), *(SELF.blu_vec_ptr)

	IF SELF.redBandIdx GE 0 AND *(SELF.calc_hist_for_displayed_data_ptr) THEN	$
		SELF->CalculateHistogram, SELF.redBandIdx, 0
	IF SELF.grnBandIdx GE 0 AND *(SELF.calc_hist_for_displayed_data_ptr) THEN	$
		SELF->CalculateHistogram, SELF.grnBandIdx, 1
	IF SELF.bluBandIdx GE 0 AND *(SELF.calc_hist_for_displayed_data_ptr) THEN	$
		SELF->CalculateHistogram, SELF.bluBandIdx, 2

;print,''
;print,''
;print,''
END
; GEOREF_IMAGE::DisplayData_8Bit

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::DisplayData @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::DisplayData, _EXTRA = e


;===============================================================================
; This method displays the data specified for the georef_images channels
;===============================================================================
	SRC_COPY	= 3

	;=======================================================================
	; No data?  Just return
	;=======================================================================
	IF NOT PTR_VALID(SELF.dataPtr) THEN RETURN

	SELF->InitializeDataMinMax
	;=======================================================================
	; In this case, "newRealize" will not be utilized.  It is intended to
	; act as a flag indicating that a new WIDGET_DRAW was realized and may
	; have messed up any MAP_SET that may be active in the application
	; that instantiated this instance of GEOREF_IMAGE.
	;=======================================================================
	newRealize = SELF->ToggleStretchDnInterface( SELF.stretchDnBaseToggledOnOrOff )

	;=======================================================================
	; Save current window ID and set current georef_image
	;=======================================================================
	tmpWin	= !D.WINDOW

;;;ckt,9-2-1999	WSET, SELF.windowID
	SELF->SetUpPanPixmap

	WSET, SELF.panPixmap
	ERASE

	IF !D.N_COLORS GT 256 THEN						$
		SELF->DisplayData_24Bit						$
	ELSE									$
		SELF->DisplayData_8Bit, _EXTRA = e

	WSET, SELF.windowID
	WSHOW, ICONIC = 0

	DEVICE, SET_GRAPHICS_FUNCTION = SRC_COPY

	DEVICE, COPY = [ 0, 0, SELF.drawXsize, SELF.drawYsize, 0, 0, SELF.panPixmap ]

	SELF->UpdateBox
	SELF->DisplayTabularData

	WSET, tmpWin


END
; GEOREF_IMAGE::DisplayData

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::GetLonLat @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::GetLonLat, x, y
;-----------------------------------------------------------------------------
; This method returns the latitude and longitude of an x-y location specified
; in VIEWPORT coordinates
;-----------------------------------------------------------------------------

	;---------------------------------------------------------------------
	; Set up return array with "bad" values
	;---------------------------------------------------------------------
	lonLat = [ -999.0, -999.0 ]

	;---------------------------------------------------------------------
	; Check to see if a longitude image and a latitude image exist; if
	; they do, convert x-y VIEWPRT coordinates to X-Y data coordinates,
	; and check to see if the returned coordinates fall within the
	; dimensions of each image.  If so, set the return array to the
	; appropriate longitude and latitude values from each image,
	; respectively.  Otherwise, return the "bad" flag of [-999.0,-999.0]
	;---------------------------------------------------------------------
	IF  PTR_VALID((SELF.lonImgPtr)[1]) AND PTR_VALID((SELF.latImgPtr)[1]) THEN BEGIN


		inverse_zoom_val_x	= 1.0 / ( SELF.lonLatZoomPctX / 100.0 )
		inverse_zoom_val_y	= 1.0 / ( SELF.lonLatZoomPctY / 100.0 )
		xIdxLon			= FIX( inverse_zoom_val_x * FLOAT( x ) + SELF.lonXS )
		yIdxLon			= FIX( inverse_zoom_val_y * FLOAT( y ) + SELF.lonYS )
		xIdxLat			= FIX( inverse_zoom_val_x * FLOAT( x ) + SELF.latXS )
		yIdxLat			= FIX( inverse_zoom_val_y * FLOAT( y ) + SELF.latYS )

;;;ckt,nov1999		xIdxLon = ROUND( (FLOAT(SELF.lonNX)/FLOAT(SELF.viewportNX)) * FLOAT(x) ) + SELF.lonXS
;;;ckt,nov1999		yIdxLon = ROUND( (FLOAT(SELF.lonNY)/FLOAT(SELF.viewportNY)) * FLOAT(y) ) + SELF.lonYS
;;;ckt,nov1999		xIdxLat = ROUND( (FLOAT(SELF.latNX)/FLOAT(SELF.viewportNX)) * FLOAT(x) ) + SELF.latXS
;;;ckt,nov1999		yIdxLat = ROUND( (FLOAT(SELF.latNY)/FLOAT(SELF.viewportNY)) * FLOAT(y) ) + SELF.latYS

		IF xIdxLon GE 0 AND xIdxLon LT (*((SELF.lonImgPtr)[1]))->GetImageWidth() AND	$
		   yIdxLon GE 0 AND yIdxLon LT (*((SELF.lonImgPtr)[1]))->GetImageHeight() AND	$
		   xIdxLat GE 0 AND xIdxLat LT (*((SELF.latImgPtr)[1]))->GetImageWidth() AND	$
		   yIdxLat GE 0 AND yIdxLat LT (*((SELF.latImgPtr)[1]))->GetImageHeight() THEN BEGIN
		      lon_val = (*((SELF.lonImgPtr)[1]))->ReturnImageValueAtXY( xIdxLon, yIdxLon )
		      lat_val = (*((SELF.latImgPtr)[1]))->ReturnImageValueAtXY( xIdxLon, yIdxLon )
		      IF NOT lon_val.bad_value AND NOT lat_val.bad_value THEN lonLat = [ lon_val.value, lat_val.value ]
;ckt,oct1999		      lonLat[0] = (*((SELF.lonImgPtr)[1]))->ReturnImageValueAtXY( xIdxLon, yIdxLon )
;ckt,oct1999		      lonLat[1] = (*((SELF.latImgPtr)[1]))->ReturnImageValueAtXY( xIdxLon, yIdxLon )
		ENDIF
	ENDIF
	RETURN, lonLat
END
; GEOREF_IMAGE::GetLonLat

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::GetImageXY @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::GetImageXY, x, y, zoom_pct_x, zoom_pct_y, xs, ys, nx, ny, img_width, img_height
;-----------------------------------------------------------------------------
; This method returns the x-y absolute location of an x-y specified
; in VIEWPORT coordinates
;-----------------------------------------------------------------------------

	;---------------------------------------------------------------------
	; Set up return array with "bad" values
	;---------------------------------------------------------------------
	xy = [ -999, -999 ]

	;---------------------------------------------------------------------
	; Check to see if an image exists; if
	; it does, convert x-y VIEWPORT coordinates to X-Y data coordinates,
	; and check to see if the returned coordinates fall within the
	; dimensions of each image.  If so, set the return array to the
	; appropriate x-y coordinates,
	; respectively.  Otherwise, return the "bad" flag of [-999.0,-999.0]
	;---------------------------------------------------------------------
	IF  PTR_VALID(SELF.dataPtr) THEN BEGIN
		inverse_zoom_val_x	= 1.0 / ( zoom_pct_x / 100.0 )
		inverse_zoom_val_y	= 1.0 / ( zoom_pct_y / 100.0 )
		xIdx			= FIX( inverse_zoom_val_x * FLOAT( x ) + xs )
		yIdx			= FIX( inverse_zoom_val_y * FLOAT( y ) + ys )

;;;ckt,nov1999		xIdx = ROUND( (FLOAT((*(SELF.dataNX))[0])/FLOAT(SELF.viewportNX)) * FLOAT(x) ) + (*(SELF.dataXS))[0]
;;;ckt,nov1999		yIdx = ROUND( (FLOAT((*(SELF.dataNY))[0])/FLOAT(SELF.viewportNY)) * FLOAT(y) ) + (*(SELF.dataYS))[0]

		IF xIdx GE 0 AND xIdx LT img_width AND yIdx GE 0 AND yIdx LT img_height THEN BEGIN
		      xy[0] = xIdx
		      xy[1] = yIdx
		ENDIF
	ENDIF
	RETURN, xy
END
; GEOREF_IMAGE::GetImageXY

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::GetLabelInfoString @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::GetLabelInfoString
	IF SELF.report_global_coordinates THEN					$
		RETURN, '(x, y, data value)'					$
	ELSE									$
		RETURN, '(tile id, tile-x, tile-y, data value)'
END
; GEOREF_IMAGE::GetLabelInfoString

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::GetLabelDataString @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::GetLabelDataString, in_x, in_y, data_ptr_idx

	inverse_zoom_val_x	= 1.0 / ( (*(SELF.zoomPctX))[data_ptr_idx] / 100.0 )
	inverse_zoom_val_y	= 1.0 / ( (*(SELF.zoomPctY))[data_ptr_idx] / 100.0 )

	xIdxData		= FIX( inverse_zoom_val_x * FLOAT( in_x ) + (*(SELF.dataXS))[data_ptr_idx] )
	yIdxData		= FIX( inverse_zoom_val_y * FLOAT( in_y ) + (*(SELF.dataYS))[data_ptr_idx] )

	xsz			= (*((*(SELF.dataPtr))[data_ptr_idx]))->GetImageWidth()-1L
	ysz			= (*((*(SELF.dataPtr))[data_ptr_idx]))->GetImageHeight()-1L

	IF xIdxData LT 0 THEN xIdxData = 0
	IF xIdxData GT xsz THEN xIdxData = xsz
	IF yIdxData LT 0 THEN yIdxData = 0
	IF yIdxData GT ysz THEN yIdxData = ysz

	inVal		= (*((*(SELF.dataPtr))[data_ptr_idx]))->ReturnImageValueAtXY( xIdxData, yIdxData )
	inLoc_xy	= [												$
				FIX( xIdxData/(*((*(SELF.dataPtr))[data_ptr_idx]))->ReturnOriginalSizeFactor() )+1,	$
				FIX( yIdxData/(*((*(SELF.dataPtr))[data_ptr_idx]))->ReturnOriginalSizeFactor() )+1 ]

	IF NOT SELF.report_global_coordinates AND NOT inVal.bad_value THEN						$
		inLoc_xy	= (*((*(SELF.dataPtr))[data_ptr_idx]))->ReturnImageTileLocationValueAtXY( xIdxData, yIdxData ) + 1
	in_tile_num	= ''
	IF NOT SELF.report_global_coordinates AND NOT inVal.bad_value THEN				$
		in_tile_num	= STRTRIM( (*((*(SELF.dataPtr))[data_ptr_idx]))->ReturnImageTileValueAtXY( xIdxData, yIdxData ), 2 ) + ','

	out_val_str	= 'Off Map'

	IF NOT inVal.bad_value THEN BEGIN
		;=============================================================
		; check to see if data is of type BYTE (type = 1);
		; if so, coerce to LONG
		;=============================================================
		type		= SIZE(inVal.value,/TYPE)
		IF type EQ 1 THEN inVal = LONG(inVal.value) ELSE inVal = inVal.value

		dataStr		= STRTRIM( STRING( inVal ), 2 )

		out_val_str	= STRTRIM(in_tile_num,2) + ' ' +STRTRIM(inLoc_xy[0],2) + ', ' + STRTRIM(inLoc_xy[1],2) + ', ' + dataStr

;;;ckt,oct2001		IF SELF.use_upper_left_origin THEN yIdxData = (*((*(SELF.dataPtr))[data_ptr_idx]))->GetImageHeight() - yIdxData - 1
;;;ckt,oct2001		out_val_str	 = STRTRIM( STRING( FIX( xIdxData/(*((*(SELF.dataPtr))[data_ptr_idx]))->ReturnOriginalSizeFactor() )+1 ),2 ) +		$
;;;ckt,oct2001           					', ' + STRTRIM( STRING( FIX( yIdxData/(*((*(SELF.dataPtr))[data_ptr_idx]))->ReturnOriginalSizeFactor() )+1 ),2 ) + ', ' + dataStr
	ENDIF

	RETURN, out_val_str

END
; GEOREF_IMAGE::GetLabelDataString

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::DisplayValsForXY @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::DisplayValsForXY, x, y, OFF_MAP_FLAG = offMapFlag
;-----------------------------------------------------------------------------
; This method displays the current values of longitude, latitude, and data
; given the viewport x-y coordinates of the cursor.  eventType is either
; 0 (mouseDown) or 2 (mouseMove).  If OFF_MAP_FLAG is set, the appropriate
; message is displayed.
;
; NOTE:
; Need to set up a check for mouseDown so that this method is only called when
; the mouse is actually down.
;-----------------------------------------------------------------------------

;	SELF->HideButtons

	keyword_set_offMapFlag	= KEYWORD_SET(offMapFlag)

	;---------------------------------------------------------------------
	; IF statement which checks to see whether offMapFlag should be set
	; IF it has not already been set upon entry of this method.
	;---------------------------------------------------------------------
	   ;------------------------------------------------------------------
	   ; If longitude and latitude images exist and OFF_MAP_FLAG is not
	   ; set, convert x-y coordinates in VIEWPORT space to x-y coordinates
	   ; in DATA space, then check to see if converted coordinates are
	   ; within the dimensions of each image.  If not, set offMapFlag to
	   ; TRUE (1); otherwise, set it to FALSE (0).
	   ;------------------------------------------------------------------
	IF PTR_VALID((SELF.lonImgPtr)[1]) AND PTR_VALID((SELF.latImgPtr)[1]) AND			$
		NOT keyword_set_offMapFlag THEN BEGIN
;;;ckt,nov1999          xIdxLon = ROUND( (FLOAT(SELF.lonNX)/FLOAT(SELF.viewportNX)) * FLOAT(x) ) + SELF.lonXS
;;;ckt,nov1999          yIdxLon = ROUND( (FLOAT(SELF.lonNY)/FLOAT(SELF.viewportNY)) * FLOAT(y) ) + SELF.lonYS
;;;ckt,nov1999          xIdxLat = ROUND( (FLOAT(SELF.latNX)/FLOAT(SELF.viewportNX)) * FLOAT(x) ) + SELF.latXS
;;;ckt,nov1999         	yIdxLat = ROUND( (FLOAT(SELF.latNY)/FLOAT(SELF.viewportNY)) * FLOAT(y) ) + SELF.latYS

		inverse_zoom_val_x	= 1.0 / ( SELF.lonLatZoomPctX / 100.0 )
		inverse_zoom_val_y	= 1.0 / ( SELF.lonLatZoomPctY / 100.0 )
		xIdxLon			= FIX( inverse_zoom_val_x * FLOAT( x ) + SELF.lonXS )
		yIdxLon			= FIX( inverse_zoom_val_y * FLOAT( y ) + SELF.lonYS )
		xIdxLat			= FIX( inverse_zoom_val_x * FLOAT( x ) + SELF.latXS )
		yIdxLat			= FIX( inverse_zoom_val_y * FLOAT( y ) + SELF.latYS )

         	 IF xIdxLon LT 0 OR xIdxLon GE (*((SELF.lonImgPtr)[1]))->GetImageWidth() OR		$
             		yIdxLon LT 0 OR yIdxLon GE (*((SELF.lonImgPtr)[1]))->GetImageHeight() OR	$
             		xIdxLat LT 0 OR xIdxLat GE (*((SELF.latImgPtr)[1]))->GetImageWidth() OR		$
             		yIdxLat LT 0 OR yIdxLat GE (*((SELF.latImgPtr)[1]))->GetImageHeight() THEN	$
            		offMapFlag = 1								$
          	 ELSE										$
			offMapFlag = 0
	ENDIF ELSE BEGIN
	   ;------------------------------------------------------------------
	   ; If longitude and latitude images DO NOT exist and OFF_MAP_FLAG
	   ; is not set, then check the input x-y viewport coordinates against
	   ; the dimensions of the georef_image; if the x-y coordinates fall
	   ; within the dimensions of the georef_image, set offMapFlag to FALSE
	   ; (0); otherwise, set it to TRUE (1).
	   ;------------------------------------------------------------------
	   IF NOT keyword_set_offMapFlag THEN BEGIN
;;;ckt,nov1999           	 xIdxData = FIX( (FLOAT((*(SELF.dataNX))[0])/FLOAT(SELF.viewportNX)) * FLOAT(x) ) + (*(SELF.dataXS))[0]
;;;ckt,nov1999          	 yIdxData = FIX( (FLOAT((*(SELF.dataNY))[0])/FLOAT(SELF.viewportNY)) * FLOAT(y) ) + (*(SELF.dataYS))[0]

		inverse_zoom_val_x	= 1.0 / ( (*(SELF.zoomPctX))[0] / 100.0 )
		inverse_zoom_val_y	= 1.0 / ( (*(SELF.zoomPctY))[0] / 100.0 )
		xIdxData		= FIX( inverse_zoom_val_x * FLOAT( x ) + (*(SELF.dataXS))[0] )
		yIdxData		= FIX( inverse_zoom_val_y * FLOAT( y ) + (*(SELF.dataYS))[0] )

          	 IF xIdxData LT 0 OR xIdxData GT (*((*(SELF.dataPtr))[0]))->GetImageWidth() - 1 OR				$
          	    yIdxData LT 0 OR yIdxData GT (*((*(SELF.dataPtr))[0]))->GetImageHeight() - 1 THEN	$
			offMapFlag = 1							$
		 ELSE									$
			offMapFlag = 0
	   ENDIF
	ENDELSE

;;;ckt, may1999	;---------------------------------------------------------------------
;;;ckt, may1999	; Check to see if the georef_image's labels are currently visible; if not,
;;;ckt, may1999	; check to see what type of event is currently being processed.  A mouse
;;;ckt, may1999	; down event (eventType = 0) means that the labels should be mapped IF
;;;ckt, may1999	; not already visible.  A mouse move does NOT control the "mapping" of
;;;ckt, may1999	; the labels.
;;;ckt, may1999	;---------------------------------------------------------------------
;;;ckt, may1999    IF NOT WIDGET_INFO(SELF.query_base,/VALID_ID) THEN BEGIN
;;;ckt, may1999       nImgs = 0
;;;ckt, may1999       IF PTR_VALID(SELF.dataPtr) THEN nImgs = N_ELEMENTS(*(SELF.dataPtr))
;;;ckt, may1999       SELF.query_base = ReturnFloatingQueryBase(SELF.parentBase,SELF.max_datasets+2,MAP=1,WINDOW_TITLE = SELF.title_prefix)
;;;ckt, may1999       SELF->RealizeQueryBase
;;;ckt, may1999    ENDIF

;;;ckt, may1999    WIDGET_CONTROL, SELF.query_base, MAP=1, /SHOW

	;---------------------------------------------------------------------
	; If offMapFlag has been set, then set up the longitude and latitude
	; strings to be displayed as 'Off' and 'Map', repectively.  Otherwise,
	; set these strings to 'Not' and 'Available'.  'Not Available' will
	; only be displayed if longitude and latitude images do NOT exist AND
	; the viewport x-y coordinates are within the bounds of the georef_image.
	;---------------------------------------------------------------------
	IF offMapFlag THEN BEGIN
		invalidLonStr = 'Off'
		invalidLatStr = ' Map'
	ENDIF ELSE BEGIN
		invalidLonStr = 'Not'
		invalidLatStr = ' Available'
	ENDELSE

	;---------------------------------------------------------------------
	; If longitude and latitude images exist and offMapFlag has not been
	; set, then grab the appropriate longitude and latitude values from
	; each image and stringify.  Otherwise, set the ouput labels to the
	; appropriate "invalid string" value set above.
	;---------------------------------------------------------------------

    value_strarr = STRARR(SELF.max_datasets+2)
    labels_strarr= value_strarr
    units_strarr = value_strarr



	IF PTR_VALID((SELF.lonImgPtr)[1]) AND PTR_VALID((SELF.latImgPtr)[1]) AND NOT offMapFlag THEN BEGIN
		lon_val	= (*((SELF.lonImgPtr)[1]))->ReturnImageValueAtXY( xIdxLon, yIdxLon )
		lat_val	= (*((SELF.latImgPtr)[1]))->ReturnImageValueAtXY( xIdxLon, yIdxLon )

		IF lon_val.bad_value OR lat_val.bad_value THEN BEGIN
        		labels_strarr[0] 	= 'Longitude: Off Map'
        		labels_strarr[1] 	= 'Latitude: Off Map'
		ENDIF ELSE BEGIN
			value_strarr[0]		= STRTRIM( STRING( lon_val.value ), 2 )
			value_strarr[1]		= STRTRIM( STRING( lat_val.value ), 2 )
			units_strarr[0]		= 'degrees'
        		units_strarr[1]		= 'degrees'

        		labels_strarr[0]	= 'Longitude:'
        		labels_strarr[1]	= 'Latitude:'
		ENDELSE

;ckt,oct1999		lonStr	= STRTRIM( STRING( (*((SELF.lonImgPtr)[1]))->ReturnImageValueAtXY( xIdxLon, yIdxLon ) ), 2 )
;ckt,oct1999		latStr	= STRTRIM( STRING( (*((SELF.latImgPtr)[1]))->ReturnImageValueAtXY( xIdxLon, yIdxLon ) ), 2 )

;ckt,oct1999        	value_strarr[0] = lonStr
;ckt,oct1999        	value_strarr[1] = latStr

        	data_start_ctr	 = 2
	ENDIF ELSE BEGIN
		IF PTR_VALID((SELF.lonImgPtr)[1]) AND PTR_VALID((SELF.latImgPtr)[1]) AND offMapFlag THEN BEGIN
        		labels_strarr[0] = 'Longitude: Off Map'
        		labels_strarr[1] = 'Latitude: Off Map'
        		data_start_ctr	 = 2
		ENDIF ELSE BEGIN
        		data_start_ctr	 = 0
		ENDELSE

	ENDELSE

        ancillary_idx = 0

	label_info_str	= SELF->GetLabelInfoString()

	FOR i = 0, N_ELEMENTS(*(SELF.dataPtr)) - 1 DO BEGIN
		out_val_str	= 'Off Map'
		IF NOT offMapFlag THEN out_val_str = SELF->GetLabelDataString( x, y, i )

		IF i EQ SELF.redBandIdx THEN BEGIN
			labels_strarr[data_start_ctr]	= 'RED PLANE ' + label_info_str + ':'
			value_strarr[data_start_ctr]	= out_val_str
		ENDIF

		IF i EQ SELF.grnBandIdx THEN BEGIN
			labels_strarr[data_start_ctr+1]	= 'GREEN PLANE ' + label_info_str + ':'
			value_strarr[data_start_ctr+1]	= out_val_str
		ENDIF

		IF i EQ SELF.bluBandIdx THEN BEGIN
			labels_strarr[data_start_ctr+2]	= 'BLUE PLANE ' + label_info_str + ':'
			value_strarr[data_start_ctr+2]	= out_val_str
		ENDIF

		IF SELF.max_datasets GT 3 THEN BEGIN
			idx	= WHERE( (*(SELF.ancillaryBandIdxPtr)) EQ i, cnt)
			IF cnt GE 0 THEN BEGIN
				FOR j = 0, cnt - 1 DO BEGIN
					labels_strarr[idx[j]+data_start_ctr+3]	=		$
						'ANCILLARY PLANE #' +				$
						STRTRIM(STRING(idx[j]+1),2) +			$
						' ' + label_info_str + ':'
					value_strarr[idx[j]+data_start_ctr+3]	= out_val_str
				ENDFOR
			ENDIF
		ENDIF
	ENDFOR

	;---------------------------------------------------------------------
	; Check to see if the georef_image's labels are currently visible; if not,
	; check to see what type of event is currently being processed.  A mouse
	; down event (eventType = 0) means that the labels should be mapped IF
	; not already visible.  A mouse move does NOT control the "mapping" of
	; the labels.
	;---------------------------------------------------------------------
	IF NOT WIDGET_INFO(SELF.query_base,/VALID_ID) THEN BEGIN
		nImgs = 0
		IF PTR_VALID((SELF.lonImgPtr)[1]) AND PTR_VALID((SELF.latImgPtr)[1]) THEN	$
			n_sets = SELF.max_datasets+2						$
		ELSE	n_sets = SELF.max_datasets
		IF PTR_VALID(SELF.dataPtr) THEN nImgs = N_ELEMENTS(*(SELF.dataPtr))
		SELF.query_base = SELF->ReturnFloatingQueryBase(				$
			SELF.parentBase,							$
			n_sets,									$
			MAP = SELF.showHideComWinFlag,						$
			LABELS_STRARR = labels_strarr,						$
			UNITS_STRARR = units_strarr,						$
			WINDOW_TITLE = SELF.title_prefix )
		SELF->RealizeQueryBase
	ENDIF

	SELF->SetQueryValues,			$
		SELF.query_base,		$
		value_strarr,			$
		LABEL_STRINGS = labels_strarr,	$
		UNIT_STRINGS = units_strarr

	WIDGET_CONTROL, SELF.query_base, MAP=SELF.showHideComWinFlag, /SHOW

	RETURN, offMapFlag

END
; GEOREF_IMAGE::DisplayValsForXY

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::ShowHide_FloatingQueryBase @@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::ShowHide_FloatingQueryBase, showhide
	CASE 1 OF
		showhide EQ 'show'	: BEGIN
			SELF.showHideComWinFlag = 1
			WIDGET_CONTROL, SELF.showHideComWinBut, SET_VALUE = 'Hide Companion Window'
			END
		showhide EQ 'hide'	: BEGIN
			SELF.showHideComWinFlag = 0
			WIDGET_CONTROL, SELF.showHideComWinBut, SET_VALUE = 'Show Companion Window'
			END
		ELSE:
	ENDCASE
	IF WIDGET_INFO( SELF.query_base, /VALID_ID ) THEN BEGIN
		WIDGET_CONTROL, SELF.query_base, MAP = SELF.showHideComWinFlag, SHOW = SELF.showHideComWinFlag
	ENDIF ELSE BEGIN
		ret_flag = SELF->DisplayValsForXY( SELF.cursor_x, SELF.cursor_y )
	ENDELSE
END
; GEOREF_IMAGE::ShowHide_FloatingQueryBase

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::UnLinkObj @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::UnLinkObj, obj2Unlink
;-----------------------------------------------------------------------------
; This method "un-links" an object from the current object
;-----------------------------------------------------------------------------

	COMMON GEOREFIMAGE, linkObj, nullObj, linkMsg, nObj, idArr, commonblock_pseudo_color_table_ptrarr, commonblock_self

	;---------------------------------------------------------------------
	; First, figure out the index of obj2Unlink within the current objects'
	; OBJARR().  ASSUMING that an index is found, set that element of the
	; OBJARR() to the NULL object.
	;---------------------------------------------------------------------
	idx	= WHERE(SELF.linkObj EQ obj2Unlink,cnt)
	IF cnt GT 0 THEN SELF.linkObj[idx[0]] = nullObj

	;---------------------------------------------------------------------
	; Then, check to see if the current object is linked to any other objects;
	; if not, de-sensitize the "Un-link..." button
	;---------------------------------------------------------------------
	idx	= WHERE(OBJ_VALID(SELF.linkObj),cnt)
	IF cnt EQ 0 THEN WIDGET_CONTROL, SELF.unlinkButton, SENSITIVE = 0

	;---------------------------------------------------------------------
	; Since we just un-linked an object, we know that there is space to
	; link another object, so make sure that the "Link..." button is
	; sensitized.
	;---------------------------------------------------------------------
	WIDGET_CONTROL, SELF.linkButton, SENSITIVE = 1
END
; GEOREF_IMAGE::UnLinkObj

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::ClearLinkObj @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::ClearLinkObj
;-----------------------------------------------------------------------------
; This method "un-links" the current object from ALL objects it may be
; linked to
;-----------------------------------------------------------------------------

	COMMON GEOREFIMAGE, linkObj, nullObj, linkMsg, nObj, idArr, commonblock_pseudo_color_table_ptrarr, commonblock_self

	;---------------------------------------------------------------------
	; First, determine the indices (if any) of the current object's
	; OBJARR() which contain valid objects.  If there are none, just return.
	;---------------------------------------------------------------------
	idx	= WHERE(OBJ_VALID(SELF.linkObj),cnt)
	IF cnt EQ 0 THEN RETURN

	;---------------------------------------------------------------------
	; Otherwise, sett all indices of the OBJARR() containing valid objects
	; to the NULL object
	;---------------------------------------------------------------------
	FOR i = 0, cnt - 1 DO SELF.linkObj[idx[i]] = nullObj

	;---------------------------------------------------------------------
	; Since we just un-linked all objects from the current object, we know
	; that there is space to link another object, so make sure that the
	; "Link..." button is sensitized.
	;---------------------------------------------------------------------
	WIDGET_CONTROL, SELF.linkButton, SENSITIVE = 1
END
; ClearLinkObj

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::AddLinkObj @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::AddLinkObj, oArr
;-----------------------------------------------------------------------------
; This method "adds" an array of objects to the current objects' array of
; linked objects.
;
; NOTE: No checking is done to see if there is enough space to accomodate the
; objects to be added because that check is done in the method SetWindowLink
;-----------------------------------------------------------------------------

	;---------------------------------------------------------------------
	; First, determine where there are spaces within the current objects'
	; OBJARR() to add the objects (as mentioned above, by the time we
	; get to this point, we are already assured that there DEFINITELY is
	; enough space within the current objects' OBJARR()).
	;---------------------------------------------------------------------
	idx	= WHERE( NOT OBJ_VALID(SELF.linkObj), nSpacesLeft )

	;---------------------------------------------------------------------
	; Counter which is incremented ONLY after an object passes all the
	; tests below
	;---------------------------------------------------------------------
	actualObjectsAdded   = 0

	;---------------------------------------------------------------------
	; Go through the entire list of "input" objects
	;---------------------------------------------------------------------
	FOR i = 0, N_ELEMENTS(oArr)-1 DO BEGIN
	   ;------------------------------------------------------------------
	   ; Set currentObj to the appropriate input object and, BEFORE
	   ; adding currentObj to the current objects' OBJARR(), check to make
	   ; sure that the object isn't already linked to this object AND
	   ; make sure that we are not trying to link an object to itself.
	   ; If those checks pass, THEN add currentObj and increment
	   ; actualObjectsAdded by 1.
	   ;------------------------------------------------------------------
	   currentObj = oArr[i]
	   check = WHERE(SELF.linkObj EQ currentObj, cnt)
	   IF (cnt EQ 0) AND (currentObj NE SELF) THEN BEGIN
	      SELF.linkObj[idx[i]] = currentObj
	      actualObjectsAdded   = actualObjectsAdded + 1
	   ENDIF
	ENDFOR

	;---------------------------------------------------------------------
	; If actualObjectsAdded is greater than 0, then sensitize the "Un-Link..."
	; button.
	;---------------------------------------------------------------------
	IF (actualObjectsAdded GT 0) THEN WIDGET_CONTROL, SELF.unlinkButton,	$
	   SENSITIVE = 1

	;---------------------------------------------------------------------
	; If actualObjectsAdded is greater than 0, and if all the spaces in
	; the current objects' OBJARR() are filled, de-sensitize the "Link..."
	; button.
	;---------------------------------------------------------------------
	IF (actualObjectsAdded GT 0 AND actualObjectsAdded EQ nSpacesLeft) THEN	$
	   WIDGET_CONTROL, SELF.linkButton, SENSITIVE = 0
END
; GEOREF_IMAGE::AddLinkObj

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::SetWindowLink @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::SetWindowLink, obj2link
;-----------------------------------------------------------------------------
; This method sets up a link between the current object (and all of its
; previously-linked windows) and obj2link (and all of its previously-linked
; windows)
;-----------------------------------------------------------------------------

	;---------------------------------------------------------------------
	; If attempting to link an object to itself, display error message
	; and return
	;---------------------------------------------------------------------
	IF SELF EQ obj2link THEN BEGIN
		msg	= [ 'Cannot link a window to itself...' ]
		res	= DIALOG_MESSAGE( msg )
		RETURN
	ENDIF

	;---------------------------------------------------------------------
	; Get linked object arrays of obj2link and the current object
	;---------------------------------------------------------------------
	obj2link_objarr	= obj2link->GetLinkedObj()
	selflink_objarr	= SELF.linkObj

	;---------------------------------------------------------------------
	; Determine objects currently linked to obj2link and the
	; current object, and store indices of these links, respectively.
	; Also, store the number of objects linked to obj2link and the
	; current object.
	;---------------------------------------------------------------------
	obj2link_vobj	= WHERE( OBJ_VALID(obj2link_objarr), cnt )
	IF cnt EQ 0 THEN obj2link_nobj = 0				$
	ELSE             obj2link_nobj = (SIZE(obj2link_vobj))[1]

	selflink_vobj	= WHERE( OBJ_VALID(selflink_objarr), cnt )
	IF cnt EQ 0 THEN selflink_nobj = 0 				$
	ELSE             selflink_nobj = (SIZE(selflink_vobj))[1]

	;---------------------------------------------------------------------
	; Case statment which sets up a temporary OBJARR() of objects that
	; are to be linked to both obj2link and the current object.
	;---------------------------------------------------------------------
	CASE 1 OF
	   ;------------------------------------------------------------------
	   ; Case A: Both obj2link and the current object are linked to
	   ; other objects; set up the temporary array to include obj2link,
	   ; the current object, and objects that are linked to obj2link
	   ; and the current object.
	   ;------------------------------------------------------------------
   	   obj2link_nobj GT 0 AND selflink_nobj GT 0:			$
              totalObj = 						$
                 [ SELF, obj2link, obj2link_objarr[obj2link_vobj],	$
                   selflink_objarr[selflink_vobj] ]
	   ;------------------------------------------------------------------
	   ; Case B: obj2link is not linked to any other objects, but the
	   ; current object is; set up the temporary array to include obj2link,
	   ; the current object, and the objects that are linked to the current
	   ; object.
	   ;------------------------------------------------------------------
  	   obj2link_nobj LE 0 AND selflink_nobj GT 0: $
              totalObj =						$
                 [ SELF, obj2link, selflink_objarr[selflink_vobj] ]
	   ;------------------------------------------------------------------
	   ; Case C: The current object is not linked to any other objects, but
	   ; obj2link is; set up the temporary array to include obj2link,
	   ; the current object, and the objects that are linked to obj2link.
	   ;------------------------------------------------------------------
   	   obj2link_nobj GT 0 AND selflink_nobj LE 0:			$
              totalObj =						$
                 [ SELF, obj2link, obj2link_objarr[obj2link_vobj] ]
	   ;------------------------------------------------------------------
	   ; Case D: Neither obj2link nor the current object are linked to
	   ; anything; set up the temporary array to inlcude just obj2link
	   ; and the current object.
	   ;------------------------------------------------------------------
   	   ELSE: totalObj = [ SELF, obj2link ]
	ENDCASE

	;---------------------------------------------------------------------
	; Remove any duplicate object references in the temporary array;
	; not sure if this would ever occur.
	;---------------------------------------------------------------------
	uniqObj = totalObj[UNIQ( totalObj, SORT(totalObj) )]

	;---------------------------------------------------------------------
	; Check to see if the number of objects in uniqObj exceed the number
	; of available spaces.  If so, display error message and return
	;---------------------------------------------------------------------
	IF N_ELEMENTS(uniqObj)-1 GE N_ELEMENTS(obj2link_objarr) THEN BEGIN
  	   msg = [ 'Cannot link the two specified windows together...',	$
	      'Each window can have at a maximum ' +			$
	      STRTRIM( STRING( N_ELEMENTS(obj2link_objarr) ), 2 ) +	$
	      ' other windows linked to it' ]
   	   res = DIALOG_MESSAGE( msg, /ERROR )
   	   RETURN
	ENDIF

	;---------------------------------------------------------------------
	; Otherwise, go through uniqObj and invoke the method AddLinkObj for
	; each element, passing the ENTIRE uniqObj array into the method.
	; This will set up links from all objects in uniqObj (which includes
	; obj2link and the current object) to all other objects in uniqObj,
	; and vice-versa.
	;---------------------------------------------------------------------
	FOR i = 0, N_ELEMENTS(uniqObj)-1 DO $
	   (uniqObj[i])->AddLinkObj, uniqObj
END
; SetWindowLink

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::MoveCursor @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::MoveCursor, x, y, LON_LAT = lonLat,		$
					 EVENT_TYPE = eType,		$
					 OFF_MAP = offMap
;-----------------------------------------------------------------------------
; This method "moves" a simulated cursor across a georef_image which is linked
; to another georef_image, where the "real" cursor is in the other georef_image.
; Event type is either 0 (mouseDown) or 2 (mouseMove).  If OFF_MAP is set,
; only replace the image where the cursor previously was located (using
; DEVICE, COPY). The LON_LAT keyword, if set, means that the incoming x-y
; values are in longitude/latitude, as opposed to viewport x-y, coordinates).
;-----------------------------------------------------------------------------
	not_keyword_set_offmap = 1
	IF KEYWORD_SET(offMap) THEN not_keyword_set_offmap = 0
	keyword_set_lonlat = 0
	IF KEYWORD_SET(lonLat) THEN keyword_set_lonlat = 1
	;---------------------------------------------------------------------
	; minVal is the threshold value used for determining the nearest locations
	; within the longitude and latitude images (if they exist) of the current
	; x-y (lon/lat) input values.  Setting minVal to 0 means that we expect
	; the input lon/lat pair to match EXACTLY with a lon/lat pair within
	; the lon/lat images.  However, minVal is set to potentially something other
	; than 0 (see below).
	;
	; NOTE: This may cause a problem with lon/lat images of different
	; dimensions.
	;---------------------------------------------------------------------
	minVal			= 0.0

	;---------------------------------------------------------------------
	; If event type not explicitly set, force it to be a mouseDown event.
	;---------------------------------------------------------------------
	IF NOT KEYWORD_SET(eType) THEN eType = 0

	;---------------------------------------------------------------------
	; Case statement which determines what x-y values should be sent off
	; to the method DisplayValsForXY
	;---------------------------------------------------------------------
	CASE 1 OF
	   ;------------------------------------------------------------------
	   ; Case A: The LON_LAT keyword has been specified, but the OFF_MAP
	   ; keyword has not
	   ;------------------------------------------------------------------
	   keyword_set_lonlat AND not_keyword_set_offmap: BEGIN
	      lonXYlatXY	= (*((SELF.lonImgPtr)[1]))->ReturnClosestXYMatchForDualValues(	$
	      					x, (*((SELF.latImgPtr)[1])), y )

	      IF lonXYlatXY[0,0] LT 0 THEN offMap = 1 ELSE offMap = 0
	      ;
	      ;for now, set x and y to lonXYlatXY[0,0] and [0,1], respectively
	      ;
	      x=lonXYlatXY[0]
	      y=lonXYlatXY[1]
	        ;-------------------------------------------------------------
	        ; Call method ConvertDataIdx2ViewportIdx to convert the data
	        ; x/y coordinates to viewport x/y coordinates.
	        ;-------------------------------------------------------------
	        viewXY=LONARR(2)

;;;ckt,nov1999	        viewXY[0] = ROUND( FLOAT(x-SELF.lonXS) * (FLOAT(SELF.viewportNX)/FLOAT(SELF.lonNX)) )
;;;ckt,nov1999	        viewXY[1] = ROUND( FLOAT(y-SELF.lonYS) * (FLOAT(SELF.viewportNY)/FLOAT(SELF.lonNY)) )

		zoom_val_x	= SELF.lonLatZoomPctX / 100.0
		zoom_val_y	= SELF.lonLatZoomPctY / 100.0

	        viewXY[0] = ROUND( ( FLOAT( x ) - SELF.lonXS ) * zoom_val_x )
	        viewXY[1] = ROUND( ( FLOAT( y ) - SELF.lonYS ) * zoom_val_y )

	        IF (viewXY[0] LT 0 OR viewXY[0] GE SELF.viewportNX) OR		$
	           (viewXY[1] LT 0 OR viewXY[1] GE SELF.viewportNY) THEN offMap = 1
	        ;-------------------------------------------------------------
	        ; Call the method DisplayValsForXY to update the georef_image
	        ; labels as appropriate
	        ;-------------------------------------------------------------
;	        IF SELF->GetMouseDown(/LEFT) THEN					$
;		   SELF->DisplayValsForXY, viewXY[0], viewXY[1], OFF_MAP_FLAG = offMap

	        ret_flag = SELF->DisplayValsForXY( viewXY[0], viewXY[1], OFF_MAP_FLAG = offMap )

	        ;-------------------------------------------------------------
	        ; Set x and y to the returned viewport coordinates for
	        ; processing below.
	        ;-------------------------------------------------------------
		x	= viewXY[0]
		y	= viewXY[1]

	      END
	   not_keyword_set_offmap: BEGIN
	   ;------------------------------------------------------------------
	   ; Case B: offMap keyword not set and LON_LAT keyword not set
	   ; call DisplayValsForXY, passing the input x/y values as viewport
	   ; coordinates
	   ;------------------------------------------------------------------
;	         IF SELF->GetMouseDown(/LEFT) THEN					$
;	           SELF->DisplayValsForXY, x, y

		ret_flag = SELF->DisplayValsForXY( x, y )

	         offMap = 0
	      END
	   ;------------------------------------------------------------------
	   ; Case C: Keyword OFF_MAP has been set, call DisplayValsForXY with
	   ; the OFF_MAP_FLAG keyword
	   ;------------------------------------------------------------------
;	   ELSE: IF SELF->GetMouseDown(/LEFT) THEN SELF->DisplayValsForXY, x, y, OFF_MAP_FLAG = 1

	   ELSE: ret_flag = SELF->DisplayValsForXY( x, y, OFF_MAP_FLAG = 1 )

	ENDCASE


	;---------------------------------------------------------------------
	; Prepare for simulated cursor manipulation
	; First, store the ID of the current active window
	;---------------------------------------------------------------------
	tmpWin	= !D.WINDOW

	;---------------------------------------------------------------------
	; Then, if the simulated cursor is actually being displayed, first
	; perform a DEVICE, COPY to restore the portion of the georef_image
	; previously occluded by the simulated cursor
	;---------------------------------------------------------------------
	IF SELF.simulatedCursorOn THEN BEGIN
		WSET, SELF.windowID
		IF SELF.cnx GT 0 AND SELF.cny GT 0 THEN			$
		DEVICE, COPY=[						$
				SELF.cxs+17,				$
				SELF.cys,				$
				SELF.cnx,				$
				SELF.cny,				$
				SELF.cxd,				$
				SELF.cyd,				$
				SELF.cursorPixmap ]
	ENDIF

	;---------------------------------------------------------------------
	; Half a bogus check; if offMap is TRUE (1) or if an EXACT lon/lat
	; match was NOT found, don't perform any more simulated cursor
	; manipulations, and return immediately
	;
	; NOTE: The minVal portion of this check needs to be removed or
	; modified.
	;---------------------------------------------------------------------
	IF offMap EQ 1 OR minVal NE 0.0 THEN RETURN

	;---------------------------------------------------------------------
	; Set the current window to the cursorPixmap, figure out the location
	; of the current simulated cursor within the georef_image, and store
	; the portion of the georef_image that is about to be occluded by
	; the simulated cursor in the appropriate portion of the cursorPixmap
	;---------------------------------------------------------------------
	WSET, SELF.cursorPixmap
	xs		= MAX([0,MIN([x,SELF.drawXsize])-8])
	ys		= MAX([0,MIN([y,SELF.drawYsize])-8])
	nx		= MIN([17,9+x,SELF.drawXsize-xs])
	ny		= MIN([17,9+y,SELF.drawYsize-ys])
	xd		= MIN([17,SELF.drawXsize-xs]) - nx
	yd		= MIN([17,SELF.drawYsize-ys]) - ny
	SELF.cxs	= xd
	SELF.cys	= yd
	SELF.cnx	= nx
	SELF.cny	= ny
	SELF.cxd	= xs
	SELF.cyd	= ys

	IF nx GT 0 AND ny GT 0 THEN				$
	DEVICE, COPY=[						$
			xs,					$
			ys,					$
			nx,					$
			ny,					$
			xd+17,					$
			yd,					$
			SELF.windowID ]

	;
	; display cursor
	;
	;---------------------------------------------------------------------
	; Set the current window to the georef_image, and perform a DEVICE, COPY
	; to display the simulated cursor on the georef_image.  Use the copy
	; mode OR (SET_GRAPHICS = 7) for this procedure.
	;---------------------------------------------------------------------
	WSET, SELF.windowID
	IF SELF.cnx GT 0 AND SELF.cny GT 0 THEN BEGIN
	DEVICE, COPY=[						$
			SELF.cxs,				$
			SELF.cys,				$
			SELF.cnx,				$
			SELF.cny,				$
			SELF.cxd,				$
			SELF.cyd,				$
			SELF.cursorPixmap ], SET_GRAPHICS = 7, GET_GRAPHICS = oldG
	DEVICE, SET_GRAPHICS=oldG
	ENDIF

	;---------------------------------------------------------------------
	; Make sure the simulated cursor flag is set to TRUE(1) within the current
	; object, restore the current window to its previous value, and return
	;---------------------------------------------------------------------
	SELF.simulatedCursorOn	= 1
	WSET, tmpWin
END
; GEOREF_IMAGE::MoveCursor

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::GetLinkedObj @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::GetLinkedObj
;-----------------------------------------------------------------------------
; Method which returns the current objects' OBJARR() of linked objects
;-----------------------------------------------------------------------------
	RETURN, SELF.linkObj
END
; GEOREF_IMAGE::GetLinkedObj

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::DisplayButtons @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::DisplayButtons
;-----------------------------------------------------------------------------
; Method which displays the georef_image's buttons, if not already displayed
;-----------------------------------------------------------------------------
	IF NOT SELF.buttonsDisplayed THEN				$
		WIDGET_CONTROL, SELF.buttonBase, MAP = 1

	SELF.buttonsDisplayed	= 1
END
; GEOREF_IMAGE::DisplayButtons

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::HideButtons @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::HideButtons
;-----------------------------------------------------------------------------
; Method which hides the georef_image's buttons, if not already hidden
;-----------------------------------------------------------------------------
	TRUE	= 1
	FALSE	= 0

	IF SELF.buttonsDisplayed EQ 1 THEN				$
		WIDGET_CONTROL, SELF.buttonBase, MAP = 0

	SELF.buttonsDisplayed	= FALSE

END
; GEOREF_IMAGE::HideButtons

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::HideLabels @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::HideLabels
;-----------------------------------------------------------------------------
; Method which hides the georef_image's labels, if not already hidden
;-----------------------------------------------------------------------------
	TRUE	= 1
	FALSE	= 0

        ;---------------------------------------------------------------------
        ; If the simulated cursor has been on, restore the georef_image are
        ; which was occluded by the simulated cursor.
        ;
        ; NOTE: THIS MAY HAVE TO BE UN-COMMENTED LATER!!!!!
        ;---------------------------------------------------------------------
;	IF SELF.simulatedCursorOn THEN BEGIN
;		tmpWin	= !D.WINDOW
;		;
;		; restore image under previous position of cursor
;		;
;		WSET, SELF.windowID
;		IF SELF.cnx GT 0 AND SELF.cny GT 0 THEN			$
;		DEVICE, COPY=[						$
;				SELF.cxs+17,				$
;				SELF.cys,				$
;				SELF.cnx,				$
;				SELF.cny,				$
;				SELF.cxd,				$
;				SELF.cyd,				$
;				SELF.cursorPixmap ]
;		SELF.simulatedCursorOn	= FALSE
;		WSET, tmpWin
;	ENDIF
END
; GEOREF_IMAGE::HideLabels

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::ReplaceImageUnderCursor @@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::ReplaceImageUnderCursor
	FALSE = 0
	IF SELF.simulatedCursorOn THEN BEGIN
		tmpWin	= !D.WINDOW
		;
		; restore image under previous position of cursor
		;
		WSET, SELF.windowID
		IF SELF.cnx GT 0 AND SELF.cny GT 0 THEN			$
		DEVICE, COPY=[						$
				SELF.cxs+17,				$
				SELF.cys,				$
				SELF.cnx,				$
				SELF.cny,				$
				SELF.cxd,				$
				SELF.cyd,				$
				SELF.cursorPixmap ]
		SELF.simulatedCursorOn	= FALSE
		WSET, tmpWin
	ENDIF
END
; GEOREF_IMAGE::ReplaceImageUnderCursor

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::ButtonsDisplayed @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::ButtonsDisplayed
;-----------------------------------------------------------------------------
; Method which returns TRUE or FALSE
;-----------------------------------------------------------------------------
	TRUE	= 1
	FALSE	= 0

	CASE SELF.buttonsDisplayed OF
	   (-1): RETURN, FALSE
	   0:    RETURN, FALSE
	   1:    RETURN, TRUE
	   ELSE: RETURN, TRUE
	ENDCASE
END
; GEOREF_IMAGE::ButtonsDisplayed

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ showLinkMsg_ev @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO showLinkMsg_ev, event
;-----------------------------------------------------------------------------
; Event-handler for "Select window to link..." interface
; Simply sets the linkObj in the COMMON block to the nullObj and
; un-maps the interface.
;-----------------------------------------------------------------------------

	COMMON GEOREFIMAGE, linkObj, nullObj, linkMsg, nObj, idArr, commonblock_pseudo_color_table_ptrarr, commonblock_self

	linkObj	= nullObj

	WIDGET_CONTROL, event.top, MAP = 0
END
; showLinkMsg_ev

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ShowLinkMessage @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO ShowLinkMessage
;-----------------------------------------------------------------------------
; This routine does one of two things: if the link message interface has
; not yet been created, the routine creates and displays it.  If the
; the interface has already been created, this routine simply "maps"
; it onto the screen.
;-----------------------------------------------------------------------------

	COMMON GEOREFIMAGE, linkObj, nullObj, linkMsg, nObj, idArr, commonblock_pseudo_color_table_ptrarr, commonblock_self

	IF linkMsg LT 0 THEN BEGIN
		linkMsg	= WIDGET_BASE( TITLE = 'Link Message', /COLUMN )
		msg	= 'Use left mouse button and ' +		$
		          'click within image area of ' +		$
		          'map viewer to link'
		linkLab1= WIDGET_LABEL(					$
				linkMsg,				$
				VALUE = msg )
        	;-------------------------------------------------------------
        	; The "Cancel" button is the only part of the interface
        	; that generates an event
        	;-------------------------------------------------------------
		linkBut1= WIDGET_BUTTON(				$
				linkMsg,				$
				VALUE = 'Cancel' )
		WIDGET_CONTROL, linkMsg, /REALIZE
		XMANAGER, 'Link Message', linkMsg, /NO_BLOCK,		$
			EVENT_HANDLER = 'showLinkMsg_ev'
	ENDIF ELSE BEGIN
		WIDGET_CONTROL, linkMsg, MAP = 1
	ENDELSE
END
; ShowLinkMessage

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::IsLinked @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION  GEOREF_IMAGE::IsLinked
;-----------------------------------------------------------------------------
; This method returns TRUE or FALSE, depending on whether the current object's
; OBJARR() contains a valid object
;-----------------------------------------------------------------------------
	idx	= WHERE( OBJ_VALID(SELF.linkObj), cnt )
	IF cnt EQ 0 THEN RETURN, 0 ELSE RETURN, 1
END
; GEOREF_IMAGE::IsLinked

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::Resize @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::Resize, x, y


	SELF->UpdateBox
;-----------------------------------------------------------------------------
; This method is called when a "resize event" occurs on the georef_image.
;-----------------------------------------------------------------------------
	tmpX	= MAX([x,SELF.minXsize])
	tmpY	= MAX([y,SELF.minYsize])

	;---------------------------------------------------------------------
	; Locate the drawBase, then resize the topBase, drawBase, and draw
	; widget itself.
	;---------------------------------------------------------------------
;print,'+++++++++++++++++++++++++++++++++++++++'
;print,'x, SELF.minXsize = ',x, SELF.minXsize
;print,'y, SELF.minYsize = ',y, SELF.minYsize
;print,'new base x, y =',tmpX,tmpY
;print,'+++++++++++++++++++++++++++++++++++++++'
	WIDGET_CONTROL, SELF.drawBase, XSIZE = tmpX, YSIZE = tmpY
	WIDGET_CONTROL, SELF.drawID, XSIZE = tmpX, YSIZE = tmpY
	drawInfo	= WIDGET_INFO( SELF.drawID, /GEOMETRY )
	SELF.drawXsize	= drawInfo.xsize
	SELF.drawYsize	= drawInfo.ysize

WIDGET_CONTROL, SELF.drawID, SENSITIVE = 0
	SELF->ZoomDisplay, SELF.viewportNX/2, SELF.viewportNY/2, 1.0, 1.0
	SELF->DisplayData
WIDGET_CONTROL, SELF.drawID, SENSITIVE = 1
	SELF.notFirstResize = 1B


END
; GEOREF_IMAGE::Resize

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::SetMouseDown @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::SetMouseDown, buttonFlag
;-----------------------------------------------------------------------------
; Method which sets the drawMouseDown buttonFlag to 0, 1, 2, or 4, cooresponding
; to no button down, left button down, middle button down, or right button down
;-----------------------------------------------------------------------------
	SELF.drawMouseDown	= buttonFlag
END
; GEOREF_IMAGE::SetMouseDown

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::SetMode @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::SetMode, mode, modeID
	WIDGET_CONTROL, SELF.modeID, GET_VALUE = itemLabel
	WIDGET_CONTROL, SELF.modeID, GET_UVALUE = itemUvalue

	itemLabel	= STRTRIM(itemLabel,2)
	itemLabel	= STRMID(itemLabel,1,STRLEN(itemLabel)-1)
	WIDGET_CONTROL, SELF.modeID, SET_VALUE = itemLabel

	WIDGET_CONTROL, modeID, GET_VALUE = itemLabel
	itemLabel	= '>' + STRTRIM(itemLabel,2)
	WIDGET_CONTROL, modeID, SET_VALUE = itemLabel

	SELF.mode	= mode
	SELF.modeID	= modeID
END
; GEOREF_IMAGE::SetMode

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::GetMode @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::GetMode
	RETURN, SELF.mode
END
; GEOREF_IMAGE::GetMode

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::GetMouseDown @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::GetMouseDown, LEFT = left, RIGHT = right, MIDDLE = middle
   CASE 1 OF
      KEYWORD_SET(left): button2Check = 1
      KEYWORD_SET(right): button2Check = 4
      KEYWORD_SET(middle): button2Check = 2
      ELSE: button2Check = 1
   ENDCASE
;-----------------------------------------------------------------------------
; Method which returns the current setting of the drawMouseDown flag
;-----------------------------------------------------------------------------
	IF SELF.drawMouseDown EQ button2Check THEN RETURN, 1 ELSE RETURN, 0
END
; GEOREF_IMAGE::GetMouseDown

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::DisplayPannedImage @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::DisplayPannedImage
;print,'GEOREF_IMAGE::DisplayPannedImage::: SELF.viewportNX, SELF.viewportNY = ',SELF.viewportNX, SELF.viewportNY
;print,'GEOREF_IMAGE::DisplayPannedImage::: SELF.panDiffX, SELF.panDiffY = ',SELF.panDiffX, SELF.panDiffY
   adjustedX = (SELF.viewportNX / 2) + SELF.panDiffX
   adjustedY = (SELF.viewportNY / 2) + SELF.panDiffY
;print,'GEOREF_IMAGE::DisplayPannedImage::: adjustedX, adjustedY = ',adjustedX, adjustedY

WIDGET_CONTROL, SELF.drawID, SENSITIVE = 0
   SELF->ZoomDisplay, adjustedX, adjustedY, (1.0/1.0), (1.0/1.0)
   SELF->DisplayData
WIDGET_CONTROL, SELF.drawID, SENSITIVE = 1
;   SELF->UpdateBox
END
; GEOREF_IMAGE::DisplayPannedImage

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::SetUpPanPixmap @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::SetUpPanPixmap
	current_window	= !D.WINDOW

	IF SELF.panPixmap GE 0L AND								$
		( SELF.panPixmapX NE SELF.drawXsize OR SELF.panPixmapY NE SELF.drawYsize ) THEN BEGIN
		WDELETE, SELF.panPixmap
		SELF.panPixmap	= (-1L)
	ENDIF

	IF SELF.panPixmap LT 0L THEN BEGIN
		WINDOW, /FREE, /PIXMAP, XSIZE = SELF.drawXsize, YSIZE = SELF.drawYsize
		SELF.panPixmap	= !D.WINDOW
		SELF.panPixmapX	= SELF.drawXsize
		SELF.panPixmapY	= SELF.drawYsize
	ENDIF
	WSET, current_window

END
; GEOREF_IMAGE::SetUpPanPixmap

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::SetUpPanAnchorPt @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::SetUpPanAnchorPt, x, y
	SELF.panAnchorPt[0]	= x
	SELF.panAnchorPt[1]	= y

	;=======================================================================
	; Either use longitude image (if it exists) or one of the data images
	; to extract the following:
	;
	; x_dim		: horizontal dimension of image
	; y_dim		: vertical dimension of image
	; nx		: number of horizontal pixels displayed within viewport
	; ny		: number of vertical pixels displayed within viewport
	; xs		: starting horizontal pixel
	; ys		: starting vertical pixel
	;=======================================================================
	IF  PTR_VALID((SELF.lonImgPtr)[1]) AND PTR_VALID((SELF.latImgPtr)[1]) THEN BEGIN
		x_dim		= (*((SELF.lonImgPtr)[1]))->GetImageWidth()
		y_dim		= (*((SELF.lonImgPtr)[1]))->GetImageHeight()
		nx		= SELF.lonNX
		ny		= SELF.lonNY
		xs		= SELF.lonXS
		ys		= SELF.lonYS
	ENDIF ELSE BEGIN
		x_dim		= (*((*(SELF.dataPtr))[0]))->GetImageWidth()
		y_dim		= (*((*(SELF.dataPtr))[0]))->GetImageHeight()
		nx		= (*(SELF.dataNX))[0]
		ny		= (*(SELF.dataNY))[0]
		xs		= (*(SELF.dataXS))[0]
		ys		= (*(SELF.dataYS))[0]
	ENDELSE

	;=======================================================================
	; viewportNX represents the number of horizontal pixels required in the
	; display window for any image.  viewportNY represents the number of
	; vertical pixels required in the display window for any image.
	; drawXsize represents the horizontal size of the display window
	; in pixels.  drawYsize represents the horizontal size of the
	; display window in pixels.  Therefore, the viewport variables will never
	; be greater than the drawXsize and drawYsize variables.
	;
	; tmpViewNX	: minimum value of viewportNX, drawXsize
	; tmpViewNY	: minimum value of viewportNY, drawYsize
	;=======================================================================
	SELF.view_nx	= MIN([SELF.viewportNX,SELF.drawXsize])
	SELF.view_ny	= MIN([SELF.viewportNY,SELF.drawYsize])

	;=======================================================================
	; maxDiffXLeft	: maximum "draggable" distance to the left, in viewport
	;			pixels
	; maxDiffXRight	: maximum "draggable" distance to the right, in viewport
	;			pixels
	; maxDiffYLower	: maximum "draggable" distance to the bottom, in viewport
	;			pixels
	; maxDiffYUpper	: maximum "draggable" distance to the top, in viewport
	;			pixels
	;=======================================================================
	pct_x		= FLOAT(SELF.viewportNX) / FLOAT(nx)
	pct_y		= FLOAT(SELF.viewportNY) / FLOAT(ny)

	SELF.max_diff_x_left	= ROUND( FLOAT(xs) * pct_x * (-1.0) )
	SELF.max_diff_x_right	= ROUND( FLOAT(x_dim - (xs + nx)) * pct_x )
	SELF.max_diff_y_lower	= ROUND( FLOAT(ys) * pct_y * (-1.0) )
	SELF.max_diff_y_upper	= ROUND( FLOAT(y_dim - (ys + ny)) * pct_y )
;print,'	GEOREF_IMAGE::SetUpPanAnchorPt::: nx, ny = ', nx, ny
;print,'	GEOREF_IMAGE::SetUpPanAnchorPt::: SELF.viewportNX, SELF.viewportNY = ', SELF.viewportNX, SELF.viewportNY
;print,'	GEOREF_IMAGE::SetUpPanAnchorPt::: x, y = ', x, y
;print,'	GEOREF_IMAGE::SetUpPanAnchorPt::: pct_x, pct_y =',pct_x, pct_y
;print,'	GEOREF_IMAGE::SetUpPanAnchorPt::: SELF.max_diff_x_left = ',SELF.max_diff_x_left
;print,'	GEOREF_IMAGE::SetUpPanAnchorPt::: SELF.max_diff_x_right = ',SELF.max_diff_x_right
;print,'	GEOREF_IMAGE::SetUpPanAnchorPt::: SELF.max_diff_y_lower = ',SELF.max_diff_y_lower
;print,'	GEOREF_IMAGE::SetUpPanAnchorPt::: SELF.max_diff_y_upper = ',SELF.max_diff_y_upper
	SELF.src_x	= 0L
	SELF.src_y	= 0L
	SELF.nx		= SELF.drawXsize - SELF.src_x
	SELF.ny		= SELF.drawYsize - SELF.src_y
	SELF.dest_x	= 0L
	SELF.dest_y	= 0L
	SELF.panDiffX	= 0L
	SELF.panDiffY	= 0L
END
; GEOREF_IMAGE::SetUpPanAnchorPt

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::PanImage @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::PanImage, x, y
	SRC_COPY		= 3
	SRC_XOR_DEST_COPY	= 6

	current_window		= !D.WINDOW

	;=======================================================================
	; x		: incoming horizontal coordinate in viewport space
	; y		: incoming vertical coordinate in viewport space
	; SELF.panDiffX	: difference between current horizontal position of last
	;			mouseDown event and x, constrained to be within
	;			the horizontal viewport dimension
	; SELF.panDiffY	: difference between current vertical position of last
	;			mouseDown event and y, constrained to be within
	;			the vertical viewport dimension
	; Constrain diffX and diffY within the maximum limits calculated in
	; the method GEOREF_IMAGE::SetUpPanAnchorPt
	;=======================================================================
	SELF.panDiffX	= MIN([ SELF.max_diff_x_right,				$
				MAX([SELF.max_diff_x_left,SELF.panAnchorPt[0]-	$
					MAX([0,MIN([x,SELF.drawXsize-1])])])])
	SELF.panDiffY	= MIN([SELF.max_diff_y_upper,				$
				MAX([SELF.max_diff_y_lower,SELF.panAnchorPt[1]-	$
					MAX([0,MIN([y,SELF.drawYsize-1])])])])
;print,'GEOREF_IMAGE::PanImage::: SELF.panDiffX = ',SELF.panDiffX
;print,'GEOREF_IMAGE::PanImage::: SELF.panDiffY = ',SELF.panDiffY
	WSET, SELF.windowID
	DEVICE, SET_GRAPHICS_FUNCTION = SRC_XOR_DEST_COPY
	IF SELF.nx LE 0 OR SELF.ny LE 0 THEN RETURN
	DEVICE, COPY = [							$
			SELF.src_x,						$
			SELF.src_y,						$
			SELF.nx,						$
			SELF.ny,						$
			SELF.dest_x,						$
			SELF.dest_y,						$
			SELF.panPixmap ]

	SELF.src_x	= MAX( [ SELF.panDiffX, 0L ] )
	SELF.src_y	= MAX( [ SELF.panDiffY, 0L ] )
	SELF.nx	= SELF.drawXsize - SELF.src_x
	SELF.ny	= SELF.drawYsize - SELF.src_y
	SELF.dest_x	= ABS( MIN( [ SELF.panDiffX, 0L ] ) )
	SELF.dest_y	= ABS( MIN( [ SELF.panDiffY, 0L ] ) )

	DEVICE, SET_GRAPHICS_FUNCTION = SRC_COPY
	DEVICE, COPY = [							$
			SELF.src_x,						$
			SELF.src_y,						$
			SELF.nx,						$
			SELF.ny,						$
			SELF.dest_x,						$
			SELF.dest_y,						$
			SELF.panPixmap ]

	WSET, current_window

END
; GEOREF_IMAGE::PanImage

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::SetUpAnchorPoint @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::SetUpAnchorPoint, x, y, NO_REDRAW = noRedraw
	tmpWin	= !D.WINDOW
	WSET, SELF.windowID

	;=======================================================================
	; First off, restore image area where previous box was drawn, if any
	;=======================================================================
	IF SELF.selectPixmapXY[0] GE 0L THEN BEGIN
		IF NOT KEYWORD_SET(noRedraw) THEN BEGIN

			IF SELF.selectXS1[0] NE -999 AND SELF.selectNX GT 0 THEN	$
				DEVICE, COPY = [0,					$ ;source starting horizontal
						1,					$ ;source starting vertical
						SELF.selectNX,				$ ;source number of vertical
						1,					$ ;source number of horizontal
						SELF.selectXS1[0],			$ ;destination starting horizontal
						SELF.selectXS1[1],			$ ;destination starting vertical
						SELF.selectPixmapXY[0] ]		  ;source window ID

			IF SELF.selectXS2[0] NE -999 AND SELF.selectNX GT 0 THEN	$
				DEVICE, COPY = [0,					$
						2,					$
						SELF.selectNX,				$
						1,					$
						SELF.selectXS2[0],			$
						SELF.selectXS2[1],			$
						SELF.selectPixmapXY[0] ]

			IF SELF.selectYS1[0] NE -999 AND SELF.selectNY GT 0 THEN	$
				DEVICE, COPY = [1,					$
				 	 0,						$
				 	 1,						$
				 	 SELF.selectNY,					$
				 	 SELF.selectYS1[0],				$
				 	 SELF.selectYS1[1],				$
				 	 SELF.selectPixmapXY[1] ]

			IF SELF.selectYS2[0] NE -999 AND SELF.selectNY GT 0 THEN	$
				DEVICE, COPY = [2,					$
						0,					$
						1,					$
						SELF.selectNY,				$
						SELF.selectYS2[0],			$
						SELF.selectYS2[1],			$
						SELF.selectPixmapXY[1] ]
		ENDIF

		WDELETE, SELF.selectPixmapXY[0]
		WDELETE, SELF.selectPixmapXY[1]
	ENDIF

	horiz_line	= BYTARR( SELF.drawXsize, 1 ) + 255B
	vert_line	= BYTARR( 1, SELF.drawYsize ) + 255B

	WINDOW, /FREE, /PIXMAP, XSIZE = SELF.drawXsize, YSIZE = 3
	SELF.selectPixmapXY[0]	= !D.WINDOW
	TV, horiz_line

	WINDOW, /FREE, /PIXMAP, XSIZE = 3, YSIZE = SELF.drawYsize
	SELF.selectPixmapXY[1]	= !D.WINDOW
	TV, vert_line
	WSET, tmpWin

	SELF.selectAnchorX	= x
	SELF.selectAnchorY	= y

	SELF.currentSelectX	= (-999)
	SELF.currentSelectY	= (-999)
	SELF.selectXS1		= [-999, -999 ]
	SELF.selectXS2		= [-999, -999 ]
	SELF.selectYS1		= [-999, -999 ]
	SELF.selectYS2		= [-999, -999 ]

	;=======================================================================
	; Retrieve anchor point longitude and latitude for linking purposes
	; If longitude/latitude information is not available, GetImageXY should be
	; called and SHOULD return the global x-y coordinates of the image, NOT
	; the display x-y coordinates!
	;=======================================================================
	IF  PTR_VALID((SELF.lonImgPtr)[1]) AND PTR_VALID((SELF.latImgPtr)[1]) THEN BEGIN
		SELF.anchorLonLat	= SELF->GetLonLat(				$
								SELF.selectAnchorX,	$
								SELF.selectAnchorY )
	ENDIF ELSE BEGIN
;;;ckt,nov1999		SELF.anchorLonLat	= SELF->GetImageXY(				$
;;;ckt,nov1999								SELF.selectAnchorX,	$
;;;ckt,nov1999								SELF.selectAnchorY )
		SELF.anchorLonLat	= SELF->GetImageXY(				$
								SELF.selectAnchorX,	$
								SELF.selectAnchorY,	$
								SELF.lonLatZoomPctX,	$
								SELF.lonLatZoomPctY,	$
								SELF.lonXS,		$
								SELF.lonYS,		$
								SELF.lonNX,		$
								SELF.lonNY,		$
								(*((SELF.lonImgPtr)[1]))->GetImageWidth(),	$
								(*((SELF.lonImgPtr)[1]))->GetImageHeight() )
	ENDELSE
END
; GEOREF_IMAGE::SetUpAnchorPoint

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::DrawBox @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::DrawBox, x, y

	IF  PTR_VALID((SELF.lonImgPtr)[1]) AND PTR_VALID((SELF.latImgPtr)[1]) THEN BEGIN
		data_x_start		= SELF.lonXS
		data_nx			= SELF.lonNX
		data_y_start		= SELF.lonYS
		data_ny			= SELF.lonNY
;;;ckt,nov1999		view_nx			= SELF.viewportNX
;;;ckt,nov1999		view_ny			= SELF.viewportNY
		view_data_ratio_x	= SELF.lonLatZoomPctX / 100.0
		view_data_ratio_y	= SELF.lonLatZoomPctY / 100.0
		img_width		= FLOAT( (*((SELF.lonImgPtr)[1]))->GetImageWidth() - 1 )
		img_height		= FLOAT( (*((SELF.lonImgPtr)[1]))->GetImageHeight() - 1 )
	ENDIF ELSE BEGIN
		data_x_start		= (*(SELF.dataXS))[0]
		data_nx			= (*(SELF.dataNX))[0]
		data_y_start		= (*(SELF.dataYS))[0]
		data_ny			= (*(SELF.dataNY))[0]
;;;ckt,nov1999		view_nx			= SELF.viewportNX
;;;ckt,nov1999		view_ny			= SELF.viewportNY
		view_data_ratio_x	= (*(SELF.zoomPctX))[0] / 100.0
		view_data_ratio_y	= (*(SELF.zoomPctY))[0] / 100.0
		img_width		= FLOAT( (*((*(SELF.dataPtr))[0]))->GetImageWidth() - 1 )
		img_height		= FLOAT( (*((*(SELF.dataPtr))[0]))->GetImageHeight() - 1 )
	ENDELSE

;;;ckt,nov1999	view_data_ratio_x	= view_nx / data_nx
;;;ckt,nov1999	view_data_ratio_y	= view_ny / data_ny

	;=======================================================================
	; Compute min/max extents of DISPLAY coordinates, given the extent of the
	; image displayed in the viewer.  In the case of an entire image NOT
	; being displayed in the viewer, the calculated min and max values may
	; be less than 0 or greater than the size of the display, respectively.
	; These values are eventually constrained below.
	;=======================================================================
	maxViewX	= ROUND( ( img_width - data_x_start ) * view_data_ratio_x )
	maxViewY	= ROUND( ( img_height - data_y_start ) * view_data_ratio_y )
	minViewX	= ROUND( ( (-1.0) * data_x_start ) * view_data_ratio_x )
	minViewY	= ROUND( ( (-1.0) * data_y_start ) * view_data_ratio_y )

;print,'x,y = ',x,y
;print,'maxViewX,maxViewY,minViewX,minViewY=',maxViewX,maxViewY,minViewX,minViewY

	;=======================================================================
	; Constrain currentSelectX and currentSelectY to extent of current
	; display dimensions.  SELF.currentSelectX and SELF.currentSelectY are
	; viewport coordinates
	;=======================================================================
	SELF.currentSelectX	= MAX( [ minViewX, MIN( [ maxViewX, x ] ) ] )
	SELF.currentSelectY	= MAX( [ minViewY, MIN( [ maxViewY, y ] ) ] )

	;=======================================================================
	; Get lon/lat values of currentSelectX and currentSelectY or, if lon/lat
	; images do not exist, get global image coordinates.
	;=======================================================================
	IF  PTR_VALID((SELF.lonImgPtr)[1]) AND PTR_VALID((SELF.latImgPtr)[1]) THEN BEGIN
		SELF.currentLonLat	= SELF->GetLonLat(			$
						SELF.currentSelectX,		$
						SELF.currentSelectY)
	ENDIF ELSE BEGIN
		SELF.anchorLonLat	= SELF->GetImageXY(				$
								SELF.currentSelectX,	$
								SELF.currentSelectY,	$
								SELF.lonLatZoomPctX,	$
								SELF.lonLatZoomPctY,	$
								SELF.lonXS,		$
								SELF.lonYS,		$
								SELF.lonNX,		$
								SELF.lonNY,		$
								(*((SELF.lonImgPtr)[1]))->GetImageWidth(),	$
								(*((SELF.lonImgPtr)[1]))->GetImageHeight() )
;;;ckt,nov1999		SELF.currentLonLat	= SELF->GetImageXY(			$
;;;ckt,nov1999						SELF.currentSelectX,		$
;;;ckt,nov1999						SELF.currentSelectY)
	ENDELSE

;print,'SELF.currentLonLat = ',SELF.currentLonLat
;print,'SELF.selectAnchorX,SELF.selectAnchorY=',SELF.selectAnchorX,SELF.selectAnchorY
;print,'SELF.currentSelectX,SELF.currentSelectY = ',SELF.currentSelectX,SELF.currentSelectY

	;=======================================================================
	; Compare selectAnchorX and selectAnchorY with currentSelectX and
	; currentSelectY, respectively, and set the values minX, maxX, minY,
	; and maxY accordingly.
	;=======================================================================
	minX	= MIN( [ SELF.selectAnchorX, SELF.currentSelectX ], MAX = maxX )
	minY	= MIN( [ SELF.selectAnchorY, SELF.currentSelectY ], MAX = maxY )

	tmpWin	= !D.WINDOW

	WSET, SELF.windowID

	;=======================================================================
	; Replace the previously-drawn box, if any
	;=======================================================================
	IF SELF.selectXS1[0] NE -999 AND SELF.selectNX GT 0 THEN	$
		DEVICE, COPY = [ 0,					$
				 1,					$
				 SELF.selectNX,				$
				 1,					$
				 SELF.selectXS1[0],			$
				 SELF.selectXS1[1],			$
				 SELF.selectPixmapXY[0] ]

	IF SELF.selectXS2[0] NE -999 AND SELF.selectNX GT 0 THEN	$
		DEVICE, COPY = [ 0,					$
				 2,					$
				 SELF.selectNX,				$
				 1,					$
				 SELF.selectXS2[0],			$
				 SELF.selectXS2[1],			$
				 SELF.selectPixmapXY[0] ]

	IF SELF.selectYS1[0] NE -999 AND SELF.selectNY GT 0 THEN	$
		DEVICE, COPY = [ 1,					$
				 0,					$
				 1,					$
				 SELF.selectNY,				$
				 SELF.selectYS1[0],			$
				 SELF.selectYS1[1],			$
				 SELF.selectPixmapXY[1] ]

	IF SELF.selectYS2[0] NE -999 AND SELF.selectNY GT 0 THEN	$
		DEVICE, COPY = [ 2,					$
				 0,					$
				 1,					$
				 SELF.selectNY,				$
				 SELF.selectYS2[0],			$
				 SELF.selectYS2[1],			$
				 SELF.selectPixmapXY[1] ]

	nx	= MIN( [ SELF.drawXsize-1, maxX ] ) - MAX( [ 0, minX ] ) + 1
	ny	= MIN( [ SELF.drawYsize-1, maxY ] ) - MAX( [ 0, minY ] ) + 1
	xs	= MAX( [ minX, 0 ] )
	ys	= MAX( [ minY, 0 ] )
;print,'nx,ny,xs,ys,SELF.drawXsize,SELF.drawYsize,maxX,maxY,minX,minY=',nx,ny,xs,ys,SELF.drawXsize,SELF.drawYsize,maxX,maxY,minX,minY
;;;ckt, may1999	drawHoriz1 = (minY GE 0) AND (nx GT 0)
;;;ckt, may1999	drawHoriz2 = (maxY LE SELF.drawYsize-1) AND (nx GT 0)
;;;ckt, may1999	drawVert1  = (minX GE 0) AND (ny GT 0)
;;;ckt, may1999	drawVert2  = (maxX LE SELF.drawXsize-1) AND (ny GT 0)

	drawHoriz1 = (minY GE 0 AND minY LT SELF.drawYsize) AND (nx GT 0 AND nx LE SELF.drawXsize)
	drawHoriz2 = (maxY LT SELF.drawYsize AND maxY GE 0) AND (nx GT 0 AND nx LE SELF.drawXsize)
	drawVert1  = (minX GE 0 AND minX LT SELF.drawXsize) AND (ny GT 0 AND ny LE SELF.drawYsize)
	drawVert2  = (maxX LT SELF.drawXsize AND maxX GE 0) AND (ny GT 0 AND ny LE SELF.drawYsize)

	WSET, SELF.selectPixmapXY[0]
	IF drawHoriz1 THEN		$
		DEVICE, COPY = [ xs, minY, nx, 1, 0, 1, SELF.windowID ]
	IF drawHoriz2 THEN		$
		DEVICE, COPY = [ xs, maxY, nx, 1, 0, 2, SELF.windowID ]

	WSET, SELF.selectPixmapXY[1]
	IF drawVert1 THEN		$
		DEVICE, COPY = [ minX, ys, 1, ny, 1, 0, SELF.windowID ]
	IF drawVert2 THEN		$
		DEVICE, COPY = [ maxX, ys, 1, ny, 2, 0, SELF.windowID ]

	WSET, SELF.windowID
	IF drawHoriz1 THEN		$
		DEVICE, COPY = [ 0, 0, nx, 1, xs, minY, SELF.selectPixmapXY[0] ]
	IF drawHoriz2 THEN		$
		DEVICE, COPY = [ 0, 0, nx, 1, xs, maxY, SELF.selectPixmapXY[0] ]
	IF drawVert1 THEN		$
		DEVICE, COPY = [ 0, 0, 1, ny, minX, ys, SELF.selectPixmapXY[1] ]
	IF drawVert2 THEN		$
		DEVICE, COPY = [ 0, 0, 1, ny, maxX, ys, SELF.selectPixmapXY[1] ]

	IF drawHoriz1 THEN			 $
		SELF.selectXS1	= [ xs, minY ]	 $
	ELSE					 $
		SELF.selectXS1	= [ -999, -999 ]

	IF drawHoriz2 THEN			 $
		SELF.selectXS2	= [ xs, maxY ]   $
	ELSE					 $
		SELF.selectXS2	= [ -999, -999 ]

	IF drawVert1 THEN			 $
		SELF.selectYS1	= [ minX, ys ]   $
	ELSE					 $
		SELF.selectYS1	= [ -999, -999 ]

	IF drawVert2 THEN			 $
		SELF.selectYS2	= [ maxX, ys ]   $
	ELSE					 $
		SELF.selectYS2	= [ -999, -999 ]

	SELF.selectNX	= nx
	SELF.selectNY	= ny

	WSET, tmpWin

END
; GEOREF_IMAGE::DrawBox

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::UpdateBox @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::UpdateBox
	IF SELF.selectAnchorX NE -999 AND 				$
	   SELF.selectAnchorY NE -999 AND				$
	   SELF.currentSelectX NE -999 AND				$
	   SELF.currentSelectY NE -999 THEN BEGIN

	   IF  PTR_VALID((SELF.lonImgPtr)[1]) AND			$
		PTR_VALID((SELF.latImgPtr)[1]) THEN BEGIN
		anchorLonLatXY	=					$
			(*(SELF.lonImgPtr[1]))->ReturnClosestXYMatchForDualValues(	$
				SELF.anchorLonLat[0], (*(SELF.latImgPtr[1])),		$
				SELF.anchorLonLat[1] )
		currentLonLatXY	= (*(SELF.lonImgPtr[1]))->ReturnClosestXYMatchForDualValues(	$
				SELF.currentLonLat[0], (*(SELF.latImgPtr[1])),			$
				SELF.currentLonLat[1] )
		anchorViewX	=					$
			ROUND(FLOAT(anchorLonLatXY[0]-SELF.lonXS) /	$
			(FLOAT(SELF.lonNX)/FLOAT(SELF.viewportNX)))
		anchorViewY	=					$
			ROUND(FLOAT(anchorLonLatXY[1]-SELF.lonYS) /	$
			(FLOAT(SELF.lonNY)/FLOAT(SELF.viewportNY)))
		currentViewX	=					$
			ROUND(FLOAT(currentLonLatXY[0]-SELF.lonXS) /	$
			(FLOAT(SELF.lonNX)/FLOAT(SELF.viewportNX)))
		currentViewY	=					$
			ROUND(FLOAT(currentLonLatXY[1]-SELF.lonYS) /	$
			(FLOAT(SELF.lonNY)/FLOAT(SELF.viewportNY)))
	   ENDIF ELSE BEGIN
		anchorLonLatXY	= [ SELF.anchorLonLat[0], SELF.anchorLonLat[1] ]
		currentLonLatXY	= [ SELF.currentLonLat[0], SELF.currentLonLat[1] ]
		anchorViewX	=					$
			ROUND(FLOAT(anchorLonLatXY[0]-(*(SELF.dataXS))[0]) /	$
			(FLOAT((*(SELF.dataNX))[0])/FLOAT(SELF.viewportNX)))
		anchorViewY	=					$
			ROUND(FLOAT(anchorLonLatXY[1]-(*(SELF.dataYS))[0]) /	$
			(FLOAT((*(SELF.dataNY))[0])/FLOAT(SELF.viewportNY)))
		currentViewX	=					$
			ROUND(FLOAT(currentLonLatXY[0]-(*(SELF.dataXS))[0]) /	$
			(FLOAT((*(SELF.dataNX))[0])/FLOAT(SELF.viewportNX)))
		currentViewY	=					$
			ROUND(FLOAT(currentLonLatXY[1]-(*(SELF.dataYS))[0]) /	$
			(FLOAT((*(SELF.dataNY))[0])/FLOAT(SELF.viewportNY)))
	   ENDELSE


	   SELF->SetUpAnchorPoint, anchorViewX, anchorViewY, /NO_REDRAW
	   SELF->DrawBox, currentViewX, currentViewY
	ENDIF
END
; GEOREF_IMAGE::UpdateBox

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::GetPtr2LonImg @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::GetPtr2LonImg
	RETURN, (SELF.lonImgPtr)[0]
END
; GEOREF_IMAGE::GetPtr2LonImg

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::GetPtr2LatImg @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::GetPtr2LatImg
	RETURN, (SELF.latImgPtr)[0]
END
; GEOREF_IMAGE::GetPtr2LatImg

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::Spawn_Band_Slider @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::Spawn_Band_Slider
	plane_display_status	= [ ( SELF.redBandIdx GE 0 AND SELF.display_rgb[0] ),	$
				    ( SELF.grnBandIdx GE 0 AND SELF.display_rgb[1] ),	$
				    ( SELF.bluBandIdx GE 0 AND SELF.display_rgb[2] ) ]

	;------------------------------------------------------------------------
	; First determine how many planes contain DISPLAYED DATA
	; DISPLAYED DATA requires that data is loaded into a
	; particular band AND that band is currently being displayed
	;------------------------------------------------------------------------
	n_displayed_data		= TOTAL(plane_display_status)

	IF n_displayed_data LE 1 THEN BEGIN
		msg	= [							$
				'Band slider tool requires at least TWO',	$
				'active and displayed data sets!' ]
		res	= DIALOG_MESSAGE( msg, /ERROR )
		RETURN

	ENDIF

	IF plane_display_status[0] THEN red_band = SELF->GetBand( SELF.redBandIdx, 0, /RETURN_DISPLAY_ONLY )
	IF plane_display_status[1] THEN grn_band = SELF->GetBand( SELF.grnBandIdx, 1, /RETURN_DISPLAY_ONLY )
	IF plane_display_status[2] THEN blu_band = SELF->GetBand( SELF.bluBandIdx, 2, /RETURN_DISPLAY_ONLY )

	IF n_displayed_data LT 3 THEN BEGIN
		idx	= (WHERE(plane_display_status GT 0))[0]
		CASE idx OF
			0: sz	= SIZE(red_band)
			1: sz	= SIZE(grn_band)
			2: sz	= SIZE(blu_band)
			ELSE:
		ENDCASE

		idx	= (WHERE(plane_display_status LE 0))[0]
		CASE idx OF
			0: red_band	= BYTARR(sz[1],sz[2])
			1: grn_band	= BYTARR(sz[1],sz[2])
			2: blu_band	= BYTARR(sz[1],sz[2])
			ELSE:
		ENDCASE
	ENDIF

	band_slider, red_band, grn_band, blu_band, GROUP_LEADER = SELF.drawBase
END
; GEOREF_IMAGE::Spawn_Band_Slider

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::Spawn_Scroll_Tool @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::Spawn_Scroll_Tool

END
; GEOREF_IMAGE::Spawn_Scroll_Tool

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::Add_Personal_Tool @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::Add_Personal_Tool

END
; GEOREF_IMAGE::Add_Personal_Tool

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::SetAcceptFlag @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::SetAcceptFlag, flagValue
	SELF.acceptFlag = flagValue
END
; GEOREF_IMAGE::SetAcceptFlag

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::GetAcceptFlag @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::GetAcceptFlag
	RETURN, SELF.acceptFlag
END
; GEOREF_IMAGE::GetAcceptFlag

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::SetMinXDrawSize @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::SetMinXDrawSize, new_min_x_size
	SELF.minXsize	= new_min_x_size
END
; GEOREF_IMAGE::SetMinXDrawSize

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::SetMinYDrawSize @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::SetMinYDrawSize, new_min_y_size
	SELF.minYsize	= new_min_y_size
END
; GEOREF_IMAGE::SetMinYDrawSize

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::ReturnMaxDatasets @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::ReturnMaxDatasets
	RETURN, SELF.max_datasets
END
; GEOREF_IMAGE::ReturnMaxDatasets

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::GetDrawBaseID @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::GetDrawBaseID
	RETURN, SELF.drawBase
END
; GEOREF_IMAGE::GetDrawBaseID

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::ReturnImageDataObj @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::ReturnImageDataObj, RGB = rgb, DATA_INDEX = index

	return_obj	= OBJ_NEW()

	;=====================================================================
	; keyword INDEX overrides keyword RGB in ALL cases.
	;=====================================================================
	IF SIZE(index,/TYPE) NE 0 THEN BEGIN
		idx	= FIX(index)
		IF idx LT 0 OR idx GT N_ELEMENTS(*(SELF.dataPtr)) - 1 THEN BEGIN
			msg	= [ 'GEOREF_IMAGE::ReturnImageDataObj: Unknown index: ' + STRTRIM(idx,2) ]
			res	= DIALOG_MESSAGE( msg, /ERROR )
			RETURN, return_obj
		ENDIF
;print,'idx = ',idx
;help,(*((*(SELF.dataPtr))[idx]))
		RETURN, (*((*(SELF.dataPtr))[idx]))
	ENDIF

	IF NOT KEYWORD_SET( rgb ) THEN band = 'RED' ELSE band = STRTRIM(STRUPCASE(rgb),2)

	IF band NE 'RED' AND band NE 'GREEN' AND band NE 'BLUE' AND STRPOS(band,'ANCILLARY') LT 0 THEN BEGIN
		msg	= [ 'GEOREF_IMAGE::ReturnImageDataObj: Unknown band: ' + band ]
		res	= DIALOG_MESSAGE( msg, /ERROR )
		RETURN, return_obj
	ENDIF

	CASE 1 OF
		SELF.redBandIdx GE 0 AND SELF.display_rgb[0] AND band EQ 'RED': BEGIN
			return_obj	= (*((*(SELF.dataPtr))[SELF.redBandIdx]))
			END
		SELF.grnBandIdx GE 0 AND SELF.display_rgb[1] AND band EQ 'GREEN': BEGIN
			return_obj	= (*((*(SELF.dataPtr))[SELF.grnBandIdx]))
			END
		SELF.bluBandIdx GE 0 AND SELF.display_rgb[2] AND band EQ 'BLUE': BEGIN
			return_obj	= (*((*(SELF.dataPtr))[SELF.bluBandIdx]))
			END
		PTR_VALID(SELF.ancillaryBandIdxPtr) AND STRPOS(band,'ANCILLARY') GE 0: BEGIN
			i_val	= (STR_SEP( band, '_' ))[1]
			n_ancillary	= N_ELEMENTS(*(SELF.ancillaryBandIdxPtr))
			IF n_ancillary GE i_val THEN BEGIN
				IF (*(SELF.ancillaryBandIdxPtr))[i_val-1] GE 0 THEN		$
					return_obj = (*((*(SELF.dataPtr))[(*(SELF.ancillaryBandIdxPtr))[i_val-1]]))
			ENDIF
			END
		ELSE:
	ENDCASE

	RETURN, return_obj
END
; GEOREF_IMAGE::ReturnImageDataObj

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::Get_Data_Description_Ptr @@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::Get_Data_Description_Ptr
	RETURN, SELF.data_description_ptr
END
; GEOREF_IMAGE::Get_Data_Description_Ptr

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ georef_image_ev @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION georef_image_ev, event


;===============================================================================
; Main event handler for the georef_image
;===============================================================================

	COMMON GEOREFIMAGE, linkObj, nullObj, linkMsg, nObj, idArr, commonblock_pseudo_color_table_ptrarr, commonblock_self

	modeChangeFlag = 0
	newRealize = 0

	;=======================================================================
	; Obtain the structure name for the current event structure
	;=======================================================================
	widgetType	= TAG_NAMES( event, /STRUCTURE_NAME )

	id		= event.id
	top		= event.top
	handler		= event.handler
	select		= (-1)
	x		= (-1)
	y		= (-1)
	type		= (-1)
	press		= 0B
	release		= 0B
	clicks		= (-1)
	enter		= (-1)
	value		= (-1L)
	drag		= (-1)
	projection_ptr	= PTR_NEW()

	;=======================================================================
	; Process the event based upon the structure name
	;=======================================================================
	CASE widgetType OF

		;===============================================================
		; WIDGET_TIMER
		; WIDGET_TIMER
		; WIDGET_TIMER
		; WIDGET_TIMER
		; WIDGET_TIMER
		;===============================================================
		'WIDGET_TIMER' : BEGIN
			WIDGET_CONTROL, event.id, GET_UVALUE = thisObj
			IF event.id EQ thisObj->GetDrawBaseID() THEN BEGIN
				;===============================================
				; Resume accepting (processing) events.
				;===============================================
				WIDGET_CONTROL, event.id, GET_UVALUE = thisObj
				thisObj->SetAcceptFlag, 1
				widgetType = 'GEOREFIMAGE_TIMER'
			ENDIF
			END

		;===============================================================
		; WIDGET_DRAW
		; WIDGET_DRAW
		; WIDGET_DRAW
		; WIDGET_DRAW
		; WIDGET_DRAW
		;===============================================================
		'WIDGET_DRAW' : BEGIN
			;=======================================================
			; Get reference to current object (stored in UVALUE of
			; draw widget)
			;=======================================================
			WIDGET_CONTROL, event.id, GET_UVALUE = thisObj

			thisObj->Set_Cursor_XY, event.x, event.y

			type		= event.type
			x		= event.x
			y		= event.y
			press		= event.press
			release		= event.release
			clicks		= event.clicks

			;=======================================================
			; Special section for mouse motion events; check
			; to see if mouse move events are currently
			; being processed or discarded.
			;=======================================================
			IF event.type EQ 2 THEN BEGIN	; mouse motion
				;===============================================
				; Determe if "mouse move event" is to be
				; processed or discarded.
				;===============================================
				IF thisObj->GetAcceptFlag() THEN BEGIN
					;=======================================
					; Accept this event, but set
					; flag and timer to reject
					; further events until timer
					; goes off.
					;=======================================
					thisObj->SetAcceptFlag, 0
					;=======================================
					; use SELF.drawBase to
					; differentiate time event from
					; the timer event set off in
					; misr_view.pro
					;=======================================
					WIDGET_CONTROL, thisObj->GetDrawBaseID(),	$
							TIMER = 0.1
					;=======================================
					; ...process the event by
					; continuing in this event
					; handler...
					;=======================================
				ENDIF ELSE BEGIN
					;=======================================
					; Reject events by just
					; returning the incoming event
					; structure unchanged.
					;=======================================
					RETURN, { GEOREFIMAGE, 				$
							id		:id,		$
							top		:top,		$
							handler		:handler,	$
							widgetType	:widgetType,	$
							select		:select,	$
							x		:x,		$
							y		:y,		$
							type		:type,		$
							press		:press,		$
							release		:release,	$
							clicks		:clicks,	$
							enter		:enter,		$
							value		:value,		$
							drag		:drag,		$
							modeChangeFlag	:modeChangeFlag,$
							newRealize	:newRealize,	$
							projection_ptr	:projection_ptr,$
							mode		:thisObj->GetMode() }
				ENDELSE
			ENDIF

			;=======================================================
			; use mouse x-y location to display coordinate
			; information
			;=======================================================
			ret_flag = thisObj->DisplayValsForXY( event.x, event.y )

			;=======================================================
			; If current object is linked to other objects,
			; call method MoveCursor to manipulate simulated
			; cursor in other object's display area.
			;=======================================================
			IF thisObj->IsLinked() THEN BEGIN
				oArr	= thisObj->GetLinkedObj()
				idx	= WHERE( OBJ_VALID(oArr) )
				lonLat = thisObj->GetLonLat(event.x,event.y)
				FOR i = 0, N_ELEMENTS(idx) - 1 DO BEGIN
					IF lonLat[0] LT -180.0 THEN			$
						(oArr[idx[i]])->MoveCursor,		$
							event.x,			$
							event.y, 			$
							EVENT_TYPE = type,		$
							OFF_MAP = 1			$
					ELSE						$
						(oArr[idx[i]])->MoveCursor,		$
							lonLat[0],			$
							lonLat[1],			$
							/LON_LAT,			$
							EVENT_TYPE = type
				ENDFOR
			ENDIF

			;=======================================================
			; Another case statement, this time based upon
			; event.type
			;=======================================================
			CASE event.type OF

			;=======================================================
			; WIDGET_DRAW: MOUSE DOWN
			; WIDGET_DRAW: MOUSE DOWN
			; WIDGET_DRAW: MOUSE DOWN
			; WIDGET_DRAW: MOUSE DOWN
			; WIDGET_DRAW: MOUSE DOWN
			;=======================================================
			0: BEGIN
				;===============================================
				; Set mouseDown flag in current object
				; and check for linked objects in which
				; to do the same.
				;===============================================
				thisObj->SetMouseDown, event.press
				IF thisObj->IsLinked() THEN BEGIN
					oArr	= thisObj->GetLinkedObj()
					idx	= WHERE( OBJ_VALID(oArr) )
					FOR i = 0, N_ELEMENTS(idx) - 1 DO		$
						(oArr[idx[i]])->SetMouseDown,		$
							event.press
				ENDIF

				;===============================================
				; Yet another case statement, this time
				; based upon event.press
				;===============================================
				CASE event.press OF
					;=======================================
					; WIDGET_DRAW: MOUSE DOWN: LEFT BUTTON
					; WIDGET_DRAW: MOUSE DOWN: LEFT BUTTON
					; WIDGET_DRAW: MOUSE DOWN: LEFT BUTTON
					; WIDGET_DRAW: MOUSE DOWN: LEFT BUTTON
					; WIDGET_DRAW: MOUSE DOWN: LEFT BUTTON
					;=======================================
					1: BEGIN
						;===============================
						; A left button press
						; can mean one of two
						; things: either this is
						; a "link event", or it
						; simply is an event
						; which displays the
						; lon/lat/data values
						; for the current
						; position of the
						; cursor.  First, check
						; to see if linkObj is
						; valid.
						;===============================
						CASE 1 OF
							OBJ_VALID( linkObj ): BEGIN ; link event
								IF PTR_VALID( thisObj->GetPtr2LonImg() ) AND	$
									PTR_VALID( thisObj->GetPtr2LatImg() ) THEN BEGIN
									;=======
									; Link
									; event,
									; so call
									; the
									; method
									; SetWindowLink
									; to set
									; up all
									; the
									; links,
									; then
									; reset
									; linkObj
									; to the
									; nullObj
									;=======
									linkObj->SetWindowLink, thisObj
								ENDIF ELSE BEGIN
									msg	= [ 'Only georeferenced displays can be linked!' ]
									res	= DIALOG_MESSAGE( msg, /ERROR )
								ENDELSE

								linkObj->HideButtons
								linkObj	= nullObj
								WIDGET_CONTROL, linkMsg, MAP = 0
								END
							thisObj->GetMode() EQ 'zoom': BEGIN
								WIDGET_CONTROL, event.id, SENSITIVE = 0
								thisObj->ZoomDisplay, event.x, event.y, (1.0/2.0), (1.0/2.0)
								thisObj->DisplayData
								WIDGET_CONTROL, event.id, SENSITIVE = 1
								END
							thisObj->GetMode() EQ 'boxselect': BEGIN
								thisObj->ClearSelectedTabularData
								thisObj->SetUpAnchorPoint, event.x, event.y
								END
							thisObj->GetMode() EQ 'pan': BEGIN
								thisObj->SetUpPanAnchorPt, event.x, event.y
								END
							ELSE:
						ENDCASE
						END
					;=======================================
					; WIDGET_DRAW: MOUSE DOWN: MIDDLE BUTTON
					; WIDGET_DRAW: MOUSE DOWN: MIDDLE BUTTON
					; WIDGET_DRAW: MOUSE DOWN: MIDDLE BUTTON
					; WIDGET_DRAW: MOUSE DOWN: MIDDLE BUTTON
					; WIDGET_DRAW: MOUSE DOWN: MIDDLE BUTTON
					;=======================================
					2: BEGIN
						CASE 1 OF
							thisObj->GetMode() EQ 'zoom': BEGIN
								WIDGET_CONTROL, event.id, SENSITIVE = 0
								thisObj->ZoomDisplay, event.x, event.y, (2.0/1.0), (2.0/1.0)
								thisObj->DisplayData
								WIDGET_CONTROL, event.id, SENSITIVE = 1
								END
							thisObj->GetMode() EQ 'boxselect': BEGIN
								;===============
								; if there is a rubber-band box set up, erase it
								; and set the anchor and select variables to -999
								; save all the selected points in some buffer
								;===============

								thisObj->SetUpAnchorPoint, -999, -999
								thisObj->ToggleClosestPoint, event.x, event.y, event.clicks

								;===============
								; find the closest point to where the mouseDown occurred
								; within a certain tolerance (expressed in viewport coordinates)
								; if there is one point meeting the criteria and it's
								; not already highlighted, highlight it and add it to the
								; list of selected points; if there are more than one
								; point meeting the criteria, take the "first" one
								; encountered.  event.clicks can be used in conjunction with
								; event.press by the parent program
								; to do something neat if the user double-clicks on
								; a point.
								;===============
								END
							ELSE:
						ENDCASE
						END
					;=======================================
					; WIDGET_DRAW: MOUSE DOWN: RIGHT BUTTON
					; WIDGET_DRAW: MOUSE DOWN: RIGHT BUTTON
					; WIDGET_DRAW: MOUSE DOWN: RIGHT BUTTON
					; WIDGET_DRAW: MOUSE DOWN: RIGHT BUTTON
					; WIDGET_DRAW: MOUSE DOWN: RIGHT BUTTON
					;=======================================
					4: BEGIN
						;===============================
						; So far, this is only a toggle to display or hide
						; the georef_image's buttons
						;===============================
						IF NOT thisObj->ButtonsDisplayed() THEN	$
							thisObj->DisplayButtons		$
						ELSE					$
							thisObj->HideButtons
						END
					ELSE:
				ENDCASE
				END

			;=======================================================
			; WIDGET_DRAW: MOUSE UP
			; WIDGET_DRAW: MOUSE UP
			; WIDGET_DRAW: MOUSE UP
			; WIDGET_DRAW: MOUSE UP
			; WIDGET_DRAW: MOUSE UP
			;=======================================================
			1: BEGIN ; mouse up
				CASE 1 OF
					thisObj->GetMode() EQ 'pan': BEGIN
						IF thisObj->GetMouseDown(/LEFT) THEN BEGIN
							thisObj->PanImage, event.x, event.y
							thisObj->DisplayPannedImage
						ENDIF
						END
					thisObj->GetMode() EQ 'boxselect': BEGIN
					;=======================================
					; The image table, due to its potential instability, has been
					; disabled until further notice.  CKT, May 1999
;						IF thisObj->GetMouseDown(/LEFT) THEN thisObj->DisplayImageTable
					;=======================================
						END
					ELSE:
				ENDCASE

				thisObj->DisplayTabularData
				;===============================================
				; Call the method SetMouseDown to "un-set" the mouseDown flag
				;===============================================
				thisObj->SetMouseDown, 0

				;===============================================
				; Hide the labels of any linked objects
				;===============================================
				IF thisObj->IsLinked() THEN BEGIN
					oArr	= thisObj->GetLinkedObj()
					idx	= WHERE( OBJ_VALID(oArr) )
					FOR i = 0, N_ELEMENTS(idx) - 1 DO BEGIN
						(oArr[idx[i]])->HideLabels
						(oArr[idx[i]])->SetMouseDown, 0
					ENDFOR
				ENDIF

				END

			;=======================================================
			; WIDGET_DRAW: MOUSE MOVE
			; WIDGET_DRAW: MOUSE MOVE
			; WIDGET_DRAW: MOUSE MOVE
			; WIDGET_DRAW: MOUSE MOVE
			; WIDGET_DRAW: MOUSE MOVE
			;=======================================================
			2: BEGIN ; mouse move
				;===============================================
				; Similar mechanism to mouseDown event, except that
				; event type is specified as 2 in calls to
				; DisplayValsForXY and MoveCursor
				;===============================================
				CASE 1 OF
					thisObj->GetMode() EQ 'pan': BEGIN
						IF thisObj->GetMouseDown(/LEFT) THEN	$
							thisObj->PanImage, event.x, event.y
						END
					thisObj->GetMode() EQ 'boxselect': BEGIN
						IF thisObj->GetMouseDown(/LEFT) THEN 	$
							thisObj->DrawBox, event.x, event.y
						END
					ELSE:
				ENDCASE
				END

			;=======================================================
			; WIDGET_DRAW: VIEWPORT MOVE
			; WIDGET_DRAW: VIEWPORT MOVE
			; WIDGET_DRAW: VIEWPORT MOVE
			; WIDGET_DRAW: VIEWPORT MOVE
			; WIDGET_DRAW: VIEWPORT MOVE
			;=======================================================
			3: BEGIN ; viewport move
				END
			ELSE:
			ENDCASE
			END


		;===============================================================
		; WIDGET_BUTTON
		; WIDGET_BUTTON
		; WIDGET_BUTTON
		; WIDGET_BUTTON
		; WIDGET_BUTTON
		;===============================================================
		'WIDGET_BUTTON': BEGIN

			select = event.select

			;=======================================
			; Get reference to current object (stored in UVALUE of draw
			; widget)
			;=======================================
			WIDGET_CONTROL, event.id, GET_UVALUE = uVal
			parentBase	= WIDGET_INFO( event.id, /PARENT )
			WIDGET_CONTROL, parentBase, GET_UVALUE = thisObj

			;=======================================================
			; One of the georef_image's widget_buttons has generated an
			; event; determine which one
			;=======================================================
			CASE uVal OF
				;===============================================
				; 'Link...' button pressed; set linkObj to the current
				; object and display link message
				;===============================================
				'link': BEGIN
					ShowLinkMessage
					linkObj	= thisObj
					END
				;===============================================
				; 'Un-link...' button pressed; grab all valid object
				; references from the current object's linked object array
				; and call the method UnlinkObj to remove this object
				; from the OBJARR()'s of the linked objects.  Then, clear the
				; OBJARR() of the current object
				;===============================================
				'unlink': BEGIN
					valid_idx		= WHERE(OBJ_VALID(thisObj->GetLinkedObj()),cnt)
					IF cnt GT 0 THEN				$
						validLinkedObjList	= (thisObj->GetLinkedObj())[valid_idx]
					FOR i = 0, cnt-1 DO (validLinkedObjList[i])->UnLinkObj, thisObj
					thisObj->ClearLinkObj
					END
				'stretchDn': BEGIN
					WIDGET_CONTROL, event.id, GET_VALUE = val
					IF STRPOS( val, 'Show' ) GE 0 THEN BEGIN
						newRealize	= thisObj->ToggleStretchDnInterface( 1 )
					ENDIF ELSE BEGIN
						newRealize	= thisObj->ToggleStretchDnInterface( 0 )
					ENDELSE
					END
				'colorTables': BEGIN
					thisObj -> LoadColorTables
					END
				'showhidecomwin': BEGIN
					WIDGET_CONTROL, event.id, GET_VALUE = val
;print,''
;print,'val'
;print,STRLOWCASE( STRMID( val, 0, 4 ) )
					thisObj->ShowHide_FloatingQueryBase, STRLOWCASE( STRMID( val, 0, 4 ) )
					END

				'manageData':	thisObj->Data_Manager_Base

				'bandSlider':	thisObj->Spawn_Band_Slider

				'scrollTool':	thisObj->Spawn_Scroll_Tool

				'perspective':	thisObj->Display_Perspective

				'saveImage':	thisObj->Save_Image

				'saveData':	thisObj->Save_Data

				'transformData':thisObj->Transform_Data

				'vectorOverlay':do_velocity_vectors, thisObj

				'reprojection':	BEGIN
;
;
;
; REMEMBER TO ALSO CHANGE THE METHODS ReturnImageDataObj AND ReturnMaxDatasets
;
;
;
					max_datasets		= thisObj->ReturnMaxDatasets()
					n			= 1
					IF thisObj->IsLinked() THEN BEGIN
						oArr	= thisObj->GetLinkedObj()
						idx	= WHERE( OBJ_VALID(oArr), cnt )
						IF cnt GT 0 THEN BEGIN
							FOR c = 0, cnt - 1 DO						$
								max_datasets	= MAX( [ max_datasets, (oArr[idx[c]])->	$
											ReturnMaxDatasets() ] )
						ENDIF
						n	= n + cnt
					ENDIF

					rgba_objarr	= OBJARR( max_datasets, n )

					FOR georef_img_ctr = 0, n-1 DO BEGIN
						IF georef_img_ctr EQ 0 THEN BEGIN
							lon_img_data_obj_arr	= [ *(thisObj->GetPtr2LonImg()) ]
							lat_img_data_obj_arr	= [ *(thisObj->GetPtr2LatImg()) ]
							olist	= [thisObj]
						ENDIF ELSE BEGIN
							lon_img_data_obj_arr	= [ lon_img_data_obj_arr,	$
											*(oArr[idx[georef_img_ctr-1]]	->	$
												GetPtr2LonImg()) ]
							lat_img_data_obj_arr	= [ lat_img_data_obj_arr,	$
											*(oArr[idx[georef_img_ctr-1]]	->	$
												GetPtr2LatImg()) ]
							olist	= [olist,oArr[idx[georef_img_ctr-1]]]
						ENDELSE

						FOR band_ctr = 0, max_datasets-1 DO BEGIN
							IF georef_img_ctr EQ 0 THEN BEGIN
								CASE band_ctr OF
									0:    rgba_objarr[0,0]		=			$
										thisObj->ReturnImageDataObj(RGB = 'RED')
									1:    rgba_objarr[1,0]		=			$
										thisObj->ReturnImageDataObj(RGB = 'GREEN')
									2:    rgba_objarr[2,0]		=			$
										thisObj->ReturnImageDataObj(RGB = 'BLUE')
									ELSE: rgba_objarr[band_ctr,0]	=			$
										thisObj->ReturnImageDataObj(RGB =		$
											'ANCILLARY_'+STRTRIM(band_ctr-2,2))
								ENDCASE
							ENDIF ELSE BEGIN
								CASE band_ctr OF
									0:    rgba_objarr[0,georef_img_ctr]		=		$
											oArr[idx[georef_img_ctr-1]]	->		$
												ReturnImageDataObj(RGB = 'RED')
									1:    rgba_objarr[1,georef_img_ctr]		=		$
											oArr[idx[georef_img_ctr-1]]	->		$
												ReturnImageDataObj(RGB = 'GREEN')
									2:    rgba_objarr[2,georef_img_ctr]		=		$
											oArr[idx[georef_img_ctr-1]]	->		$
												ReturnImageDataObj(RGB = 'BLUE')
									ELSE: rgba_objarr[band_ctr,georef_img_ctr]	=		$
											oArr[idx[georef_img_ctr-1]]	->		$
												ReturnImageDataObj(RGB = 'ANCILLARY_' +	$
													STRTRIM(band_ctr-2,2))
								ENDCASE
							ENDELSE
						ENDFOR
					ENDFOR

;print,'n_elements(lon_img_data_obj_arr) =',n_elements(lon_img_data_obj_arr)
;print,'n_elements(r_img_data_obj_arr) =',n_elements(r_img_data_obj_arr)

					out_struct_ptr	= reprojection_tool( lon_img_data_obj_arr, lat_img_data_obj_arr, rgba_objarr,	$
								GROUP_LEADER = thisObj->GetParentBase() )
					IF PTR_VALID(out_struct_ptr) THEN BEGIN
						s	= *out_struct_ptr
						PTR_FREE, out_struct_ptr
						projection_ptr	= PTR_NEW( CREATE_STRUCT( s, 'georef_img_objarr', olist ) )
					ENDIF ELSE BEGIN
						projection_ptr	= out_struct_ptr
					ENDELSE

					END

				'addPersonalTool':thisObj->Add_Personal_Tool

				;===============================================
				; MODES!!!!!
				;===============================================
				'zoom':BEGIN
					currentMode	= thisObj->GetMode()
					IF STRUPCASE(currentMode) NE 'ZOOM' THEN	$
						modeChangeFlag = 1
					thisObj->SetMode, uVal, event.id
					END
				'boxselect':BEGIN
					currentMode	= thisObj->GetMode()
					IF STRUPCASE(currentMode) NE 'BOXSELECT' THEN	$
						modeChangeFlag = 1
					thisObj->SetMode, uVal, event.id
					END
				'pan':BEGIN
					currentMode	= thisObj->GetMode()
					IF STRUPCASE(currentMode) NE 'PAN' THEN		$
						modeChangeFlag = 1
					thisObj->SetMode, uVal, event.id
					END
				ELSE:
			ENDCASE
			END

		;===============================================================
		; WIDGET_SLIDER
		; WIDGET_SLIDER
		; WIDGET_SLIDER
		; WIDGET_SLIDER
		; WIDGET_SLIDER
		;===============================================================
		'WIDGET_SLIDER': BEGIN
			value	= event.value
			drag	= event.drag
			END

		;===============================================================
		; WIDGET_TRACKING
		; WIDGET_TRACKING
		; WIDGET_TRACKING
		; WIDGET_TRACKING
		; WIDGET_TRACKING
		;===============================================================
		'WIDGET_TRACKING': BEGIN
			enter	= event.enter
			;=======================================================
			; This portion of the code is invoked whenever the mouse enters
			; or leaves a georef_image
			;=======================================================
			WIDGET_CONTROL, event.id, GET_UVALUE = thisObj

			;=======================================================
			; If the mouse is exiting a georef_image, hide the current object's
			; labels and the labels of all linked objects
			;=======================================================
			IF NOT event.enter THEN BEGIN
				IF thisObj->IsLinked() THEN BEGIN
					oArr	= thisObj->GetLinkedObj()
					idx	= WHERE( OBJ_VALID(oArr) )
					FOR i = 0, N_ELEMENTS(idx) - 1 DO BEGIN
						(oArr[idx[i]])->HideLabels
						(oArr[idx[i]])->ReplaceImageUnderCursor
					ENDFOR
				ENDIF
			ENDIF

			IF event.enter AND WIDGET_INFO( thisObj->get_colorbar_drawwidgetid(), /VALID_ID ) THEN BEGIN
				;----------------------------------------------------------------------
				; Mouse is entering the GEOREF_IMAGE draw widget
				; *AND* the colorbar is present.
				; Auto-re-load the selected pseudo color table, just in case it has
				; been unloaded by another widget such as the blockchooser map.
				;----------------------------------------------------------------------
				r_vec	= *(commonblock_pseudo_color_table_ptrarr[0])
				g_vec	= *(commonblock_pseudo_color_table_ptrarr[1])
				b_vec	= *(commonblock_pseudo_color_table_ptrarr[2])
;print,'WIDGET_TRACKING -- TVLCT, r_vec, g_vec, b_vec'
;print,'WIDGET_TRACKING -- r_vec = ',r_vec
;print,'WIDGET_TRACKING -- g_vec = ',g_vec
;print,'WIDGET_TRACKING -- b_vec = ',b_vec
				TVLCT, r_vec, g_vec, b_vec
				thisObj->EightBit_SetColorVectorPtr, RED = r_vec
				thisObj->EightBit_SetColorVectorPtr, GRN = g_vec
				thisObj->EightBit_SetColorVectorPtr, BLU = b_vec
			ENDIF

			END

		;===============================================================
		; WIDGET_BASE
		; WIDGET_BASE
		; WIDGET_BASE
		; WIDGET_BASE
		; WIDGET_BASE
		;===============================================================
		'WIDGET_BASE': BEGIN
			x	= event.x
			y	= event.y
			;=======================================================
			; Implies a resize event; use event.x and event.y to resize
			; georef_image
			;=======================================================
			tmp		= WIDGET_INFO( event.id, /CHILD )
			drawBase	= WIDGET_INFO( tmp, /CHILD )
			WIDGET_CONTROL, drawBase, GET_UVALUE = thisObj
			thisObj->Resize, event.x, event.y
			thisObj->DisplayData
;			thisObj->DisplayTabularData
			END
		ELSE: BEGIN
			END
	ENDCASE

	IF NOT OBJ_VALID(thisObj) THEN							$
		WIDGET_CONTROL, event.id, GET_UVALUE = thisObj


	RETURN, { GEOREFIMAGE, 						$
			id		:id,				$
			top		:top,				$
			handler		:handler,			$
			widgetType	:widgetType,			$
			select		:select,			$
			x		:x,				$
			y		:y,				$
			type		:type,				$
			press		:press,				$
			release		:release,			$
			clicks		:clicks,			$
			enter		:enter,				$
			value		:value,				$
			drag		:drag,				$
			modeChangeFlag	:modeChangeFlag,		$
			newRealize	:newRealize,			$
			projection_ptr	:projection_ptr,		$
			mode		:thisObj->GetMode() }

END
; georef_image_ev

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::SetWindowID @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::SetWindowID, drawWinID
;-----------------------------------------------------------------------------
; Method which sets the window ID of the current georef_image
;-----------------------------------------------------------------------------
	SELF.windowID	= drawWinID
	drawInfo	= WIDGET_INFO( SELF.drawID, /GEOMETRY )
	SELF.drawXsize	= drawInfo.xsize
	SELF.drawYsize	= drawInfo.ysize
END
; GEOREF_IMAGE::SetWindowID

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ map_draw_realize @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO map_draw_realize, drawID


	TRUE = 1
;-----------------------------------------------------------------------------
; Routine called only when georef_image is REALIZED; the draw_widget ID is
; obtained and SetWindowID is invoked, then the routine DisplayData is
; called to throw a picture onto the screen (if data exists)
;-----------------------------------------------------------------------------
	drawBase	= WIDGET_INFO(drawID, /PARENT)
	topBase		= WIDGET_INFO(drawID, /PARENT)

	WIDGET_CONTROL, drawID, GET_VALUE = drawWinID
	WIDGET_CONTROL, drawID, GET_UVALUE = thisObj
	thisObj->SetWindowID, drawWinID

	r_vec_ptr = thisObj->EightBit_GetColorVectorPtr(/RED)
	g_vec_ptr = thisObj->EightBit_GetColorVectorPtr(/GRN)
	b_vec_ptr = thisObj->EightBit_GetColorVectorPtr(/BLU)
	t_vec_ptr = thisObj->EightBit_GetTranslationPtr()

	CASE TRUE OF
	   NOT PTR_VALID(r_vec_ptr) AND NOT PTR_VALID(g_vec_ptr) AND $
	   NOT PTR_VALID(b_vec_ptr) AND NOT PTR_VALID(t_vec_ptr): BEGIN
	      thisObj->DisplayData, /SET_TRANSLATION
	      END
	   PTR_VALID(r_vec_ptr) AND PTR_VALID(g_vec_ptr) AND $
	   PTR_VALID(b_vec_ptr) AND PTR_VALID(t_vec_ptr): BEGIN
	      IF (SIZE(*t_vec_ptr))[0] LE 0 THEN                       $
	         thisObj->DisplayData, /SET_TRANSLATION, /SHARED_MAP   $
	      ELSE                                                     $
	         thisObj->DisplayData
	      END
	   ELSE: thisObj->DisplayData, /SET_TRANSLATION
	ENDCASE

    thisObj->RealizeQueryBase


;	thisObj->DisplayData
;	thisObj->DisplayTabularData
END
; map_draw_realize

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ map_unlink_realize @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO map_unlink_realize, unlinkButton
;-----------------------------------------------------------------------------
; Routine which de-sensitizes the "Un-link..." button upon realization,
; since there is initially nothing to un-link
;-----------------------------------------------------------------------------
	WIDGET_CONTROL, unlinkButton, SENSITIVE = 0
END
; map_unlink_realize

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::INIT @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE::INIT, parentBase,				$
			GROUP_LEADER = groupLeader,			$
			LON_IMAGE = lonImg,				$
			LAT_IMAGE = latImg,				$
			DATA_PTR = dataPtr,				$
			MISSING_DATA_PTR = missingDataPtr,		$
			TABULAR_DATA_PTR = tabularDataPtr,		$
			N_CHANNELS = nChannels,				$
			XSIZE = xSize,					$
			YSIZE = ySize,					$
			VIEW_SIZE_X = view_disp_x,			$
			VIEW_SIZE_Y = view_disp_y,			$
			MIN_XSIZE = minXsize,				$
			MIN_YSIZE = minYsize,				$
			DATA_IDX = dataIdx,				$
			MENU_MODES = menuModes,                         $
			BUTTON_MODES = buttonModes,                     $
			NO_SELECT = noSelect,                           $
			USE_TRANSLATION = transStruct,                  $
			N_AVAILABLE_COLORS_8BIT = nColors8Bit,          $
			TITLE_PREFIX = title_prefix,			$
			DATA_DESCRIPTION_STRING = dataDescriptionStr,	$
			DATA_SOURCE_FILENAME = dataSourceFilename,	$
			LON_SOURCE_FILENAME = lonSourceFilename,	$
			LAT_SOURCE_FILENAME = latSourceFilename,	$
			USE_UPPER_LEFT_ORIGIN = use_upper_left_origin,	$
			MIN_ZOOM_PCT = min_zoom_pct,			$
			MAX_ZOOM_PCT = max_zoom_pct,			$
			REPORT_GLOBAL_COORDINATES = report_global_coordinates
;-----------------------------------------------------------------------------
; Map window instantiation routine
; Keywords are as follows:
;
; parentBase		: ID of parent base that the georef_image belongs to
; GROUP_LEADER		: group leader, if any
; LON_IMAGE		: image to use for longitude values
; LAT_IMAGE		: image to use for latitude values
; DATA_PTR		: PTRARR() or PTR_NEW(), with each pointer referencing a different image
; MISSING_DATA_PTR	: PTRARR() or PTR_NEW(), with each pointer referencing an array of values
;			  that are missing data flags for each different image.  These values are
;			  excluded from bytscl and histogram calculations.
; TABULAR_DATA_PTR	: PTRARR() or PTR_NEW(), with each pointer referencing an array of
;		          lon/lat values (2 x n arrays)
; N_CHANNELS		: either 1 or 3, analagous to 8-bit or 24-bit display
; XSIZE			: inital horizontal dimension of georef_image
; YSIZE			: inital vertical dimension of georef_image
; DATA_IDX		: index of image in the above PTRARR() whose dimensions and
;			  aspect ratio are to be used in displaying images in the
;			  georef_image
; MENU_MODES		: add'l menu items to be added; should be a STRARR() with
;		 	  dimensions nAddedMenus x 2, with (*,0) being the name
;			  of the menu item (its displayed value), and (*,1) being its
;			  UVALUE (menu items are parsed based upon their UVALUE)
;			  If a UVALUE is not set for any add'l menu item, that
;			  particular item is considered a MENU, with all succeeding
;			  menu items accessible through that menu until the end
;			  of the STRARR() is encountered or another MENU entry
;		 	  is encountered.
;-----------------------------------------------------------------------------


	COMMON GEOREFIMAGE, linkObj, nullObj, linkMsg, nObj, idArr, commonblock_pseudo_color_table_ptrarr, commonblock_self

	commonblock_self = SELF

	SELF.report_global_coordinates	= KEYWORD_SET( report_global_coordinates )

	IF KEYWORD_SET( min_zoom_pct ) THEN SELF.min_zoom_pct = min_zoom_pct ELSE SELF.min_zoom_pct = (-1.0)
	IF KEYWORD_SET( max_zoom_pct ) THEN SELF.max_zoom_pct = max_zoom_pct ELSE SELF.max_zoom_pct = (-1.0)

	IF !D.N_COLORS GT 256 THEN BEGIN
		SELF.NUMBER_HISTOGRAM_COLORS    = 0
	ENDIF ELSE BEGIN
		SELF.NUMBER_HISTOGRAM_COLORS    = 12
	ENDELSE

	;---------------------------------------------------------------------
	; commonblock_pseudo_color_table_ptrarr is for auto-re-loading of the
	; pseudo color table, which is desireable if pseudo color (XLOADCT and
	; the colorbar) is being used.  Initialize to table #0 (b/w), even
	; though it's not necessarily going to be used.
	;---------------------------------------------------------------------
	commonblock_pseudo_color_table_ptrarr		= PTRARR( 3 )
;print,'GEOREF_IMAGE::INIT -- LOADCT, 0'
	LOADCT, 0
	TVLCT, red_table, grn_table, blu_table, /GET
;print,'GEOREF_IMAGE::INIT -- commonblock_pseudo_color_table_ptrarr    is being set to B/W table'
;print,'GEOREF_IMAGE::INIT -- red_table = ',red_table
;print,'GEOREF_IMAGE::INIT -- grn_table = ',grn_table
;print,'GEOREF_IMAGE::INIT -- blu_table = ',blu_table
	commonblock_pseudo_color_table_ptrarr[0]	= PTR_NEW( red_table )
	commonblock_pseudo_color_table_ptrarr[1]	= PTR_NEW( grn_table )
	commonblock_pseudo_color_table_ptrarr[2]	= PTR_NEW( blu_table )

	IDL_OBJECT_ID	= 11
	success		= 1
	failure		= 0
	MAX_IMAGES	= 2
	MAX_SETS	= 6
	VAL_LABEL_WIDTH1= 120
	VAL_LABEL_WIDTH2= 65
	DEFAULT_DRAW_X	= 500
	DEFAULT_DRAW_Y	= 500

	IF KEYWORD_SET( use_upper_left_origin ) THEN SELF.use_upper_left_origin = 1B ELSE SELF.use_upper_left_origin = 0B

    IF NOT KEYWORD_SET(title_prefix) THEN SELF.title_prefix = '' ELSE SELF.title_prefix = title_prefix

        ;---------------------------------------------------------------------
        ; If N_CHANNELS keyword not set, default nChannels to 3 (24-bit)
        ;---------------------------------------------------------------------
	IF NOT KEYWORD_SET(nChannels) THEN nChannels = 3

        ;---------------------------------------------------------------------
        ; Set the drawXsize member of the object to either xsize or
        ; DEFAULT_DRAW_X, depending on whether the XSIZE keyword is set
        ;---------------------------------------------------------------------
	IF NOT KEYWORD_SET(xSize) THEN SELF.drawXsize = DEFAULT_DRAW_X	$
	ELSE SELF.drawXsize = xSize

        ;---------------------------------------------------------------------
        ; Set the drawYsize member of the object to either ysize or
        ; DEFAULT_DRAW_Y, depending on whether the YSIZE keyword is set
        ;---------------------------------------------------------------------
	IF NOT KEYWORD_SET(ySize) THEN SELF.drawYsize = DEFAULT_DRAW_Y	$
	ELSE SELF.drawYsize = ySize

	IF NOT KEYWORD_SET(view_disp_x) THEN init_disp_x = SELF.drawXsize ELSE init_disp_x = view_disp_x
	IF NOT KEYWORD_SET(view_disp_y) THEN init_disp_y = SELF.drawYsize ELSE init_disp_y = view_disp_y

	IF NOT KEYWORD_SET(minXsize) THEN				$
		SELF.minXsize = MIN([DEFAULT_DRAW_X,SELF.drawXsize])	$
	ELSE								$
		SELF.minXsize = minXsize

 	IF NOT KEYWORD_SET(minYsize) THEN				$
		SELF.minYsize = MIN([DEFAULT_DRAW_Y,SELF.drawYsize])	$
	ELSE								$
		SELF.minYsize = minYsize
       ;---------------------------------------------------------------------
        ; Check to see if lon/lat images have been passed in.  Either BOTH
        ; images have to be passed in OR neither can be specified.  If, for
        ; example, only the lon image has been specified, an error message
        ; will be displayed, and the instantiation will fail!
        ;---------------------------------------------------------------------
	IF ( KEYWORD_SET(lonImg) AND NOT KEYWORD_SET(latImg) ) OR	$
	   ( NOT KEYWORD_SET(lonImg) AND KEYWORD_SET(latImg) ) THEN BEGIN
	   msg	= [ 'Either both LAT_IMAGE and LON_IMAGE keywords',	$
	            'or NEITHER need to be specified... cannot',	$
	            'specify either LON_IMAGE or LAT_IMAGE!' ]
	   res	= DIALOG_MESSAGE( msg, /ERROR )
	   RETURN, failure
	ENDIF

	IF NOT KEYWORD_SET(lonImg) AND					$
	   NOT KEYWORD_SET(latImg) AND					$
	   NOT KEYWORD_SET(dataPtr) THEN BEGIN
	   msg	= [ 'Either both LAT_IMAGE and LON_IMAGE keywords',	$
	            'or the DATA_PTR keyword need to be specified',	$
	            'cannot instantiate an empty object!' ]
	   res	= DIALOG_MESSAGE( msg, /ERROR )
	   RETURN, failure
	ENDIF
        ;---------------------------------------------------------------------
        ; If lon/lat images exist, determine their dimensions and the
        ; maximum x and y values.  Then, if either image does not have
        ; dimensions equal to the maximum values, use CONGRID to match
        ; the maximum dimensions and store this "new" image in the second
        ; element of the lon and lat PTRARR()'s.  If the dimensions ARE
        ; equal to the maximum values, however, just set the second element
        ; of each PTRARR() to its first element.
        ;---------------------------------------------------------------------
;;;ckt,nov1999 (not needed, I think these variables are initialized to 0)        SELF.lonXS	= 0L
;;;ckt,nov1999 (not needed, I think these variables are initialized to 0)        SELF.lonNX	= 0L
;;;ckt,nov1999 (not needed, I think these variables are initialized to 0)        SELF.lonYS	= 0L
;;;ckt,nov1999 (not needed, I think these variables are initialized to 0)        SELF.lonNY	= 0L
;;;ckt,nov1999 (not needed, I think these variables are initialized to 0)        SELF.latXS	= 0L
;;;ckt,nov1999 (not needed, I think these variables are initialized to 0)        SELF.latNX	= 0L
;;;ckt,nov1999 (not needed, I think these variables are initialized to 0)        SELF.latYS	= 0L
;;;ckt,nov1999 (not needed, I think these variables are initialized to 0)        SELF.latNY	= 0L
;;;ckt,nov1999 (not needed, I think these variables are initialized to 0)	SELF.lonLatZoomPctX	= 0.0
;;;ckt,nov1999 (not needed, I think these variables are initialized to 0)	SELF.lonLatZoomPctY	= 0.0
	IF KEYWORD_SET(lonImg) AND KEYWORD_SET(latImg) THEN BEGIN

;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change
lonImgX			= lonImg->GetImageWidth()
latImgX			= latImg->GetImageWidth()
lonImgY			= lonImg->GetImageHeight()
latImgY			= latImg->GetImageHeight()

;		lonImgX			= (SIZE(lonImg))[1]
;		latImgX			= (SIZE(latImg))[1]
;		lonImgY			= (SIZE(lonImg))[2]
;		latImgY			= (SIZE(latImg))[2]
;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change

		maxDimX			= MAX([lonImgx, latImgX])
		maxDimY			= MAX([lonImgY, latImgY])

		SELF.lonImgPtr[0]	= PTR_NEW(lonImg, /NO_COPY)
		SELF.latImgPtr[0]	= PTR_NEW(latImg, /NO_COPY)

;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change
		IF lonImgX NE maxDimX OR lonImgY NE maxDimY THEN	$
SELF.lonImgPtr[1] = PTR_NEW( *(SELF.lonImgPtr[0])->CloneObject( XSIZE = maxDimX, YSIZE = maxDimY ), /NO_COPY ) $
;			SELF.lonImgPtr[1] = PTR_NEW( CONGRID(*(SELF.lonImgPtr[0]),maxDimX,maxDimY), /NO_COPY ) $
		ELSE							$
			SELF.lonImgPtr[1] = SELF.lonImgPtr[0]

		IF latImgX NE maxDimX OR latImgY NE maxDimY THEN	$
SELF.latImgPtr[1] = PTR_NEW( *(SELF.latImgPtr[0])->CloneObject( XSIZE = maxDimX, YSIZE = maxDimY ), /NO_COPY ) $
;			SELF.latImgPtr[1] = PTR_NEW( CONGRID(*(SELF.latImgPtr[0]),maxDimX,maxDimY), /NO_COPY ) $
		ELSE							$
			SELF.latImgPtr[1] = SELF.latImgPtr[0]
;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change

		SELF.lonNX	= maxDimX
		SELF.lonNY	= maxDimY

		SELF.latNX	= maxDimX
		SELF.latNY	= maxDimY

	ENDIF
        ;---------------------------------------------------------------------
        ; If data has been specified, determine the maximum x and y dimensions
        ; for the data (including the lon/lat imagery)
        ;---------------------------------------------------------------------
	nImgs			= 0
	SELF.viewportNX		= 0L
	SELF.viewportNY 	= 0L
	IF KEYWORD_SET(dataPtr) THEN BEGIN
           ;------------------------------------------------------------------
           ; dPtr is created, and dataPtr is set to it JUST in case dataPtr is
           ; not a PTRARR(), but instead simply a PTR_NEW()
           ;------------------------------------------------------------------
	   nImgs	= N_ELEMENTS(dataPtr)
	   dPtr		= PTRARR(nImgs)
	   dPtr[*]	= dataPtr

	   imgIdx       = WHERE(PTR_VALID(dPtr))
	   nImgs        = N_ELEMENTS(imgIdx)
	   SELF.dataPtr = PTR_NEW(dPtr[imgIdx], /NO_COPY)

	   SELF.max_datasets	= 3
	   IF imgIdx[N_ELEMENTS(imgIdx)-1] GT 2 THEN		$
	   	SELF.max_datasets = SELF.max_datasets + (imgIdx[N_ELEMENTS(imgIdx)-1] - 2)


	   tmpR         = WHERE(imgIdx EQ 0, redCnt)
	   tmpG         = WHERE(imgIdx EQ 1, grnCnt)
	   tmpB         = WHERE(imgIdx EQ 2, bluCnt)

	   tmpAncillaryIdx = WHERE(imgIdx GE 3, ancillaryCnt)

	   non_RGB_imgs = nImgs - redCnt - grnCnt - bluCnt
           ;------------------------------------------------------------------
           ; Set up the red, green, and blue indices depending on number of
           ; channels available; initially, the data image residing at dataPtr[0]
           ; is set to be the red band, the data image residing at dataPtr[1] the
           ; green, and the data image residing at dataPtr[2] the blue.
           ;------------------------------------------------------------------
	   IF nImgs GE 1 AND nChannels GE 1 AND redCnt GT 0 THEN   $
	      SELF.redBandIdx = tmpR[0]                            $
	   ELSE                                                    $
	      SELF.redBandIdx = (-1)

	   IF nImgs GE 1 AND nChannels GE 1 AND grnCnt GT 0 THEN   $
	      SELF.grnBandIdx = tmpG[0]                            $
	   ELSE                                                    $
	      SELF.grnBandIdx = (-1)

	   IF nImgs GE 1 AND nChannels GE 1 AND bluCnt GT 0 THEN   $
	      SELF.bluBandIdx = tmpB[0]                            $
	   ELSE                                                    $
	      SELF.bluBandIdx = (-1)

	   IF nImgs GE 1 AND nChannels GE 1 AND ancillaryCnt GT 0 THEN BEGIN
	   	SELF.ancillaryBandIdxPtr	= PTR_NEW( LONARR( SELF.max_datasets - 3 ) - 1L, /NO_COPY )
	   	(*(SELF.ancillaryBandIdxPtr))[imgIdx[tmpAncillaryIdx]-3]	= tmpAncillaryIdx
	   ENDIF

	   SELF.display_rgb	= 1


	   IF NOT KEYWORD_SET( dataIdx ) THEN dataIdx = 0

;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change

aspect	=								$
	FLOAT( (*((*(SELF.dataPtr))[dataIdx]))->GetImageWidth() ) /	$
	FLOAT( (*((*(SELF.dataPtr))[dataIdx]))->GetImageHeight() )

;	   aspect	= FLOAT((SIZE(*((*(SELF.dataPtr))[dataIdx])))[1]) / $
;	                  FLOAT((SIZE(*((*(SELF.dataPtr))[dataIdx])))[2])
;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change

	   IF aspect GE 1.0 THEN BEGIN
	      SELF.viewportNX	= SELF.drawXsize
	      SELF.viewportNY	= ROUND( FLOAT(SELF.viewportNX) * (1.0/aspect) )
	   ENDIF ELSE BEGIN
	      SELF.viewportNY	= SELF.drawYsize
	      SELF.viewportNX	= ROUND( FLOAT(SELF.viewportNY) * aspect )
	   ENDELSE

	   SELF.zoomPctX	= PTR_NEW( FLTARR(nImgs), /NO_COPY )
	   SELF.zoomPctY	= PTR_NEW( FLTARR(nImgs), /NO_COPY )
	   SELF.dataXS		= PTR_NEW( FLTARR(nImgs), /NO_COPY )
	   SELF.dataNX		= PTR_NEW( FLTARR(nImgs), /NO_COPY )
	   SELF.dataYS		= PTR_NEW( FLTARR(nImgs), /NO_COPY )
	   SELF.dataNY		= PTR_NEW( FLTARR(nImgs), /NO_COPY )

;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change
SELF.highest_res_data_idx	= 0L
	   FOR i = 0, nImgs - 1 DO BEGIN
;;;ckt,nov1999 (not needed, I think these variables are initialized to 0	   	(*(SELF.dataXS))[i]	= 0L
(*(SELF.dataNX))[i]	= (*((*(SELF.dataPtr))[i]))->GetImageWidth()


;
; might need to include checking lon/lat image resolutions as well!!!!
; might need to include checking lon/lat image resolutions as well!!!!
; might need to include checking lon/lat image resolutions as well!!!!
; might need to include checking lon/lat image resolutions as well!!!!
IF (*(SELF.dataNX))[i] GT (*(SELF.dataNX))[ SELF.highest_res_data_idx ] THEN SELF.highest_res_data_idx = i

;	   	(*(SELF.dataNX))[i]	= (SIZE(*((*(SELF.dataPtr))[i])))[1]
;;;ckt,nov1999 (not needed, I think these variables are initialized to 0	   	(*(SELF.dataYS))[i]	= 0L
(*(SELF.dataNY))[i]	= (*((*(SELF.dataPtr))[i]))->GetImageHeight()
;	   	(*(SELF.dataNY))[i]	= (SIZE(*((*(SELF.dataPtr))[i])))[2]

(*(SELF.zoomPctX))[i]	=						$
	( FLOAT(SELF.viewportNX) / FLOAT((*((*(SELF.dataPtr))[i]))->GetImageWidth()) ) * 100.0

;	   	(*(SELF.zoomPctX))[i]	= ( FLOAT(SELF.viewportNX) /		$
;	   	   FLOAT((SIZE(*((*(SELF.dataPtr))[i])))[1]) ) * 100.0

(*(SELF.zoomPctY))[i]	=						$
	( FLOAT(SELF.viewportNY) / FLOAT((*((*(SELF.dataPtr))[i]))->GetImageHeight()) ) * 100.0

;	   	(*(SELF.zoomPctY))[i]	= ( FLOAT(SELF.viewportNY) /		$
;	   	   FLOAT((SIZE(*((*(SELF.dataPtr))[i])))[2]) ) * 100.0

	   ENDFOR

	   IF PTR_VALID(SELF.lonImgPtr[0]) AND PTR_VALID(SELF.latImgPtr[0]) THEN BEGIN
SELF.lonLatZoomPctX	= ( FLOAT(SELF.viewportNX) / FLOAT( (*((SELF.lonImgPtr)[1]))->GetImageWidth()) ) * 100.0
SELF.lonLatZoomPctY	= ( FLOAT(SELF.viewportNY) / FLOAT( (*((SELF.lonImgPtr)[1]))->GetImageHeight()) ) * 100.0
;print,'in init, SELF.viewportNX, (*((SELF.lonImgPtr)[1]))->GetImageWidth(), SELF.lonLatZoomPctX = ',SELF.viewportNX,	$
;	(*((SELF.lonImgPtr)[1]))->GetImageWidth(), SELF.lonLatZoomPctX
;	      SELF.lonLatZoomPctX	= ( FLOAT(SELF.viewportNX) / FLOAT((SIZE(*((SELF.lonImgPtr)[1])))[1]) ) * 100.0
;	      SELF.lonLatZoomPctY	= ( FLOAT(SELF.viewportNY) / FLOAT((SIZE(*((SELF.lonImgPtr)[1])))[2]) ) * 100.0
	   ENDIF
	ENDIF
;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change

	SELF.dataIdx	= dataIdx

	tabularDataPtr_keywordSet = KEYWORD_SET(tabularDataPtr)
	IF tabularDataPtr_keywordSet AND					$
	( NOT KEYWORD_SET(lonImg) OR NOT KEYWORD_SET(latImg) ) THEN BEGIN
	   msg	= [ 'Both LAT_IMAGE and LON_IMAGE keywords',	$
	            'need to be specified when using the',	$
	            'TABULAR_DATA_PTR keyword!' ]
	   res	= DIALOG_MESSAGE( msg, /ERROR )
	   RETURN, failure
	ENDIF

	IF tabularDataPtr_keywordSet THEN BEGIN
	   nSets		= N_ELEMENTS(tabularDataPtr)
	   sPtr			= PTRARR(nSets)
	   sPtr[*]		= tabularDataPtr
	   SELF.tabularDataPtr	= PTR_NEW(sPtr, /NO_COPY)
	   tmpSelectPtr		= PTRARR(nSets)
	   maxElem = 0
	   FOR i = 0, nSets - 1 DO BEGIN
        d_sz  = SIZE(*((*(SELF.tabularDataPtr))[i]))
        IF d_sz[0] EQ 1 THEN nPts = 1 ELSE nPts = d_sz[d_sz[0]]
	   	maxElem = MAX([maxElem,nPts])
	   	tmpSelectPtr[i] = PTR_NEW(INTARR(maxElem)*0,/NO_COPY)
	   ENDFOR
	   SELF.selectedTabularDataPtr = PTR_NEW(tmpSelectPtr,/NO_COPY)
	ENDIF


;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change
;	IF KEYWORD_SET(missingDataPtr) THEN BEGIN
;		SELF.missingDataPtr = PTR_NEW(missingDataPtr[imgIdx], /NO_COPY)
;	ENDIF
;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change


	SELF.nChannels	= nChannels

        ;---------------------------------------------------------------------
	; Set up pointers for data min and max, and for the bottom and top
	; stretch values for the various stretchDN routines.
        ;---------------------------------------------------------------------
	SELF.dataMinValPtr	= PTR_NEW( PTRARR( nImgs ), /NO_COPY )
	SELF.dataMaxValPtr	= PTR_NEW( PTRARR( nImgs ), /NO_COPY )
	SELF.histBinSize	= PTR_NEW( PTRARR( nImgs ), /NO_COPY )
	SELF.stretchDnBottomPtr	= PTR_NEW( PTRARR( nImgs ), /NO_COPY )
	SELF.stretchDnTopPtr	= PTR_NEW( PTRARR( nImgs ), /NO_COPY )
	SELF.stretchDnGammaPtr	= PTR_NEW( PTRARR( nImgs ), /NO_COPY )
	SELF.bottomLimitPtr	= PTR_NEW( PTRARR( nImgs ), /NO_COPY )
	SELF.topLimitPtr	= PTR_NEW( PTRARR( nImgs ), /NO_COPY )

	SELF.min_dataPtr	= PTR_NEW( PTRARR( nImgs ), /NO_COPY )
	SELF.max_dataPtr	= PTR_NEW( PTRARR( nImgs ), /NO_COPY )

	FOR j = 0, nImgs - 1 DO BEGIN
;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change
		;---------------------------------------------------------------------
		; set up dataIdx to initially include all indices from current dataset
		;---------------------------------------------------------------------
;		dataIdx	= WHERE( (*((*(SELF.dataPtr))[j])) EQ (*((*(SELF.dataPtr))[j])) )
;
;		IF PTR_VALID( SELF.missingDataPtr ) THEN BEGIN
;			IF PTR_VALID( (*(SELF.missingDataPtr))[j] ) THEN BEGIN
;				missing_vals	= *((*(SELF.missingDataPtr))[j])
;				dataIdx		= where_not( (*((*(SELF.dataPtr))[j])), missing_vals )
;			ENDIF
;			IF N_ELEMENTS(dataIdx) EQ 1 AND dataIdx[0] LT 0 THEN BEGIN
;	  			msg	= [ 'Data selected contains no valid values...',	$
;	  		    	  	    'image window not opened' ]
;	   			res	= DIALOG_MESSAGE( msg, /ERROR )
;	   			RETURN, failure
;	   		ENDIF
;		ENDIF
;
;		minVal	= MIN( (*((*(SELF.dataPtr))[j]))[dataIdx], MAX = maxVal )

		(*(SELF.stretchDnBottomPtr))[j]	=					$
			PTR_NEW( ((*((*(SELF.dataPtr))[j])))->GetMinVal(		$
				0,((*((*(SELF.dataPtr))[j])))->GetImageWidth() - 1L,	$
				0,((*((*(SELF.dataPtr))[j])))->GetImageHeight() - 1L ) )	;+ (maxVal-minVal)*0.01 )
		(*(SELF.stretchDnTopPtr))[j]	=					$
			PTR_NEW( ((*((*(SELF.dataPtr))[j])))->GetMaxVal(		$
				0,((*((*(SELF.dataPtr))[j])))->GetImageWidth() - 1L,	$
				0,((*((*(SELF.dataPtr))[j])))->GetImageHeight() - 1L ) )	;+ (maxVal-minVal)*0.01 )

		(*(SELF.min_dataPtr))[j]	=					$
			PTR_NEW( ((*((*(SELF.dataPtr))[j])))->GetMinVal(		$
				0,((*((*(SELF.dataPtr))[j])))->GetImageWidth() - 1L,	$
				0,((*((*(SELF.dataPtr))[j])))->GetImageHeight() - 1L ) )	;+ (maxVal-minVal)*0.01 )

		(*(SELF.max_dataPtr))[j]	=					$
			PTR_NEW( ((*((*(SELF.dataPtr))[j])))->GetMaxVal(		$
				0,((*((*(SELF.dataPtr))[j])))->GetImageWidth() - 1L,	$
				0,((*((*(SELF.dataPtr))[j])))->GetImageHeight() - 1L ) )	;+ (maxVal-minVal)*0.01 )
;print,'GEOREF_IMAGE::INIT, (*(SELF.max_dataPtr))[j] = ',*((*(SELF.max_dataPtr))[j])
;print,'GEOREF_IMAGE::INIT, (*(SELF.min_dataPtr))[j] = ',*((*(SELF.min_dataPtr))[j])

;		(*(SELF.stretchDnBottomPtr))[j]	= PTR_NEW( minVal )	;+ (maxVal-minVal)*0.01 )
;		(*(SELF.stretchDnTopPtr))[j]	= PTR_NEW( maxVal )	;+ (maxVal-minVal)*0.01 )
;
;		(*(SELF.min_dataPtr))[j]	= PTR_NEW( minVal )
;		(*(SELF.max_dataPtr))[j]	= PTR_NEW( maxVal )
;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change
	ENDFOR

	FOR i=0,nImgs-1 DO $
		(*(SELF.histBinSize))[i]	= PTR_NEW( 1.0 )
	FOR i=0,nImgs-1 DO $
		(*(SELF.stretchDnGammaPtr))[i]	= PTR_NEW( 1.0 )
	FOR i=0,nImgs-1 DO $
		(*(SELF.bottomLimitPtr))[i]	= PTR_NEW( *((*(SELF.min_dataPtr))[i]) )
	FOR i=0,nImgs-1 DO $
		(*(SELF.topLimitPtr))[i]	= PTR_NEW( *((*(SELF.max_dataPtr))[i]) )

	SELF.calc_hist_for_displayed_data_ptr	= PTR_NEW(1)

	IF KEYWORD_SET(dataDescriptionStr) THEN BEGIN
	   imgIdx2      = WHERE(STRTRIM(dataDescriptionStr,2) NE '',desc_cnt)

	   check	= WHERE( imgIdx2 EQ imgIdx, cnt )

	   IF N_ELEMENTS(dataDescriptionStr[imgIdx2]) NE nImgs OR cnt NE N_ELEMENTS(imgIdx) THEN BEGIN
	      msg	= [ 'Data description array must be equal to data array in ',	$
	                    'size and location of valid data!' ]
	      res	= DIALOG_MESSAGE( msg, /ERROR )
	      RETURN, failure
	   ENDIF

	   SELF.data_description_ptr	= PTR_NEW(dataDescriptionStr[imgIdx2],/NO_COPY)
	ENDIF ELSE BEGIN
		s = 'Dataset #' + SINDGEN(N_ELEMENTS(imgIdx))
	   	SELF.data_description_ptr	= PTR_NEW(s,/NO_COPY)
	ENDELSE

	IF KEYWORD_SET(dataSourceFilename) THEN BEGIN
	   imgIdx2      = WHERE(STRTRIM(dataSourceFilename,2) NE '',desc_cnt)

	   check	= WHERE( imgIdx2 EQ imgIdx, cnt )

	   IF N_ELEMENTS(dataSourceFilename[imgIdx2]) NE nImgs OR cnt NE N_ELEMENTS(imgIdx) THEN BEGIN
	      msg	= [ 'Data source filename array must be equal to data array in ',	$
	                    'size and location of valid data!' ]
	      res	= DIALOG_MESSAGE( msg, /ERROR )
	      RETURN, failure
	   ENDIF

	   SELF.data_source_filename_ptr	= PTR_NEW(dataSourceFilename[imgIdx2],/NO_COPY)
	ENDIF ELSE BEGIN
		s = STRARR(N_ELEMENTS(imgIdx)) + 'UNKNOWN SOURCE FILE'
	   	SELF.data_source_filename_ptr	= PTR_NEW(s,/NO_COPY)
	ENDELSE

	IF KEYWORD_SET( lonSourceFilename ) THEN SELF.lon_source_filename = lonSourceFilename
	IF KEYWORD_SET( latSourceFilename ) THEN SELF.lat_source_filename = latSourceFilename

        ;---------------------------------------------------------------------
        ; If the COMMON block has not yet been set up (indicating that this
        ; is the first georef_image instantiated), set up all members of the
        ; COMMON block to their initial states.
        ;---------------------------------------------------------------------
	IF (SIZE(nullObj))[1] NE IDL_OBJECT_ID THEN BEGIN
		nullObj			= OBJ_NEW()
		linkObj			= nullObj
		linkMsg			= (-1L)
		nObj			= 0
		idArr			= BYTARR(50) * 0B
	ENDIF

        ;---------------------------------------------------------------------
        ; Currently, 50 georef_images are allowed on the screen at the same time;
        ; if this georef_image will exceed that amount, display an error message
        ; and return a failure value
        ;---------------------------------------------------------------------
	wID	= WHERE(idArr EQ 0B, cnt)
	idArr[wID[0]] = wID[0] + 1B
	SELF.id = idArr[wID[0]]
	IF cnt LE 0 THEN BEGIN
		msg = [ 'Cannot open any more georef_images...',	$
		        'To open more windows, existing windows',	$
		        'need to be closed first' ]
		res = DIALOG_MESSAGE( msg, /ERROR )
		RETURN, failure
	ENDIF

	SELF.parentBase		= parentBase

        ;---------------------------------------------------------------------
        ; Base for WIDGET_DRAW
        ;---------------------------------------------------------------------
	IF NOT KEYWORD_SET( groupLeader ) THEN				$
	   SELF.drawBase	= WIDGET_BASE( SELF.parentBase,		$
	   				UVALUE = SELF,			$
					EVENT_FUNC='georef_image_ev' )	$
	ELSE								$
	   SELF.drawBase	= WIDGET_BASE( SELF.parentBase,		$
	   				UVALUE = SELF,			$
	      			   GROUP_LEADER = groupLeader,		$
				       EVENT_FUNC='georef_image_ev' )

        ;---------------------------------------------------------------------
        ; Base for widget_buttons
        ;---------------------------------------------------------------------
	SELF.buttonBase	= WIDGET_BASE(					$
	      			SELF.drawBase,				$
	      			/ROW,					$
	      			MAP = 0,				$
	      			UVALUE = SELF )
        ;---------------------------------------------------------------------
        ; widget_buttons
        ;---------------------------------------------------------------------
	utilButton	= WIDGET_BUTTON(				$
				SELF.buttonBase,			$
				UVALUE = SELF,				$
				/MENU,					$
				VALUE = 'Utilities' )

	IF PTR_VALID(SELF.lonImgPtr[0]) AND PTR_VALID(SELF.latImgPtr[0]) THEN BEGIN

		SELF.linkButton	= WIDGET_BUTTON(				$
					utilButton,				$
					UVALUE = 'link',			$
					VALUE = 'Link...' )
		SELF.unlinkButton = WIDGET_BUTTON(				$
					utilButton,				$
					NOTIFY_REALIZE = 'map_unlink_realize',	$
					UVALUE = 'unlink',			$
					VALUE = 'Un-Link...' )
	ENDIF

	SELF.colorTablesButton=	WIDGET_BUTTON(				$
				utilButton,				$
				UVALUE = 'colorTables',			$
				VALUE = 'Color Tables and Color Bar' )

	SELF.stretchDnButton=	WIDGET_BUTTON(				$
				utilButton,				$
				UVALUE = 'stretchDn',			$
				VALUE = 'Show Stretch DN Interface' )

	SELF.showHideComWinBut= WIDGET_BUTTON(				$
				utilButton,				$
				UVALUE = 'showhidecomwin',		$
				VALUE = 'Hide Companion Window' )

;------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------
	SELF.transformDataButton = WIDGET_BUTTON(			$
				utilButton,				$
				UVALUE = 'transformData',		$
				VALUE = 'Transform Data...' )
	SELF.manageDataButton = WIDGET_BUTTON(				$
				utilButton,				$
				UVALUE = 'manageData',			$
				VALUE = 'Manage Data...' )
;;;ckt,feb2001	SELF.perspectiveButton = WIDGET_BUTTON(			$
;;;ckt,feb2001				utilButton,			$
;;;ckt,feb2001				UVALUE = 'perspective',		$
;;;ckt,feb2001				VALUE = 'Perspective...' )
;;;ckt,feb2001	IF !D.N_COLORS GT 256 THEN				$
;;;ckt,feb2001		SELF.bandSliderButton = WIDGET_BUTTON(		$
;;;ckt,feb2001				utilButton,			$
;;;ckt,feb2001				UVALUE = 'bandSlider',		$
;;;ckt,feb2001				VALUE = 'Spawn Band Slider' )
;;;ckt,feb2001	SELF.scrollToolButton = WIDGET_BUTTON(			$
;;;ckt,feb2001				utilButton,			$
;;;ckt,feb2001				UVALUE = 'scrollTool',		$
;;;ckt,feb2001				VALUE = 'Spawn Scroll Tool', SENSITIVE = 0 )
	SELF.saveDataButton = WIDGET_BUTTON(				$
				utilButton,				$
				UVALUE = 'saveData',			$
				VALUE = 'Save Data...' )
	SELF.saveImageButton = WIDGET_BUTTON(				$
				utilButton,				$
				UVALUE = 'saveImage',			$
				VALUE = 'Save Image...' )


;------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------

toolsButton		= WIDGET_BUTTON(				$
			SELF.buttonBase,				$
			UVALUE = SELF,					$
			/MENU,						$
			VALUE = 'Tools' )

SELF.perspectiveButton	= WIDGET_BUTTON(				$
			toolsButton,					$
			UVALUE = 'perspective',				$
			VALUE = 'Perspective Tool...' )
IF !D.N_COLORS GT 256 THEN						$
	SELF.bandSliderButton = WIDGET_BUTTON(				$
			toolsButton,					$
			UVALUE = 'bandSlider',				$
			VALUE = 'Band Slider Tool' )
SELF.scrollToolButton	= WIDGET_BUTTON(				$
			toolsButton,					$
			UVALUE = 'scrollTool',				$
			VALUE = 'Scroll Tool', SENSITIVE = 0 )
SELF.vectorOverlayTool	= WIDGET_BUTTON(				$
			toolsButton,					$
			UVALUE = 'vectorOverlay',			$
			VALUE = 'Vector Overlay Tool...' )
IF PTR_VALID(SELF.lonImgPtr[0]) AND PTR_VALID(SELF.latImgPtr[0]) THEN BEGIN
	SELF.reprojectionTool	= WIDGET_BUTTON(				$
				toolsButton,					$
				UVALUE = 'reprojection',			$
				VALUE = 'Reprojection Tool...',			$
				SENSITIVE = PTR_VALID(SELF.lonImgPtr[0]) AND PTR_VALID(SELF.latImgPtr[0]) )
ENDIF

;===================================================================================
;SELF.personalToolsMenu	= WIDGET_BUTTON(				$
;			toolsButton,					$
;			UVALUE = SELF,					$
;			/MENU,						$
;			VALUE = 'Personal Tools' )
;SELF.addPersonalTool	= WIDGET_BUTTON(				$
;			SELF.personalToolsMenu,				$
;			UVALUE = 'addPersonalTool',			$
;			VALUE = 'Add...' )
;===================================================================================



	modeButton	= WIDGET_BUTTON(				$
				SELF.buttonBase,			$
				UVALUE = SELF,				$
				/MENU,					$
				VALUE = 'Modes' )

;	queryMenuItem	= WIDGET_BUTTON(				$
;				modeButton,				$
;				UVALUE = 'query',			$
;				VALUE = '>Query' )
;	zoomMenuItem	= WIDGET_BUTTON(				$
;				modeButton,				$
;				UVALUE = 'zoom',			$
;				VALUE = 'Zoom' )
	zoomMenuItem	= WIDGET_BUTTON(				$
				modeButton,				$
				UVALUE = 'zoom',			$
				VALUE = '>Zoom' )

	panMenuItem	= WIDGET_BUTTON(				$
				modeButton,				$
				UVALUE = 'pan',				$
				VALUE = 'Pan' )
        IF NOT KEYWORD_SET(noSelect) THEN                               $
	   selectMenuItem	=                                       $
	                  WIDGET_BUTTON(				$
				modeButton,				$
				UVALUE = 'boxselect',			$
				VALUE = 'Box Select     ' )

;	closeButton	= WIDGET_BUTTON(				$
;				SELF.buttonBase,			$
;				UVALUE = 'close',			$
;				VALUE = 'Close' )

	IF KEYWORD_SET(menuModes) THEN BEGIN
		nAddedMenuIdx	= WHERE(menuModes[*,1] EQ '', nAddedMenus)
		nAddedItems	= N_ELEMENTS(menuModes[*,0])
	ENDIF ELSE BEGIN
		nAddedMenus	= 0
	ENDELSE

	FOR i = 0, nAddedMenus - 1 DO BEGIN
		itemStart 	= nAddedMenuIdx[i] + 1

		IF i LT nAddedMenus - 1 THEN				$
			itemEnd		= nAddedMenuIdx[i+1] - 1	$
		ELSE							$
			itemEnd		= nAddedItems - 1

		addedMenu	= WIDGET_BUTTON(			$
					SELF.buttonBase,		$
					UVALUE = SELF,			$
					/MENU,				$
					VALUE = menuModes[i,0] )
		FOR j = itemStart, itemEnd DO				$
			addeditem	= WIDGET_BUTTON(		$
						addedMenu,		$
						UVALUE = menuModes[j,1],$
						VALUE = menuModes[j,0] )
	ENDFOR

	IF KEYWORD_SET(buttonModes) THEN BEGIN
	   nAddedButtons = N_ELEMENTS(buttonModes[*,0])
	   FOR i = 0, nAddedButtons-1 DO				$
              addeditem = WIDGET_BUTTON( SELF.buttonBase,		$
                                         UVALUE = buttonModes[i,1],	$
                                         VALUE = buttonModes[i,0] )
	ENDIF

	SELF.mode	= 'zoom'
	SELF.modeID	= zoomMenuItem

        ;---------------------------------------------------------------------
        ; Base for labels
        ;---------------------------------------------------------------------
;	SELF.labelBase	= WIDGET_BASE(					$
;	      			SELF.drawBase,				$
;	      			/ROW,					$
;	      			MAP = 0,				$
;	      			UVALUE = SELF )
        ;---------------------------------------------------------------------
        ; Labels
        ;---------------------------------------------------------------------
;	SELF.lonLabel	= WIDGET_LABEL(					$
;	      			SELF.labelBase,				$
;	      			/DYNAMIC_RESIZE,			$
;				VALUE = '' )
;	SELF.latLabel	= WIDGET_LABEL(					$
;	      			SELF.labelBase,				$
;	      			/DYNAMIC_RESIZE,			$
;				VALUE = '' )


        ;---------------------------------------------------------------------
        ; Map window itself
        ;---------------------------------------------------------------------
	SELF.drawID	= WIDGET_DRAW(					$
				SELF.drawBase,				$
				XSIZE = init_disp_x,			$
				YSIZE = init_disp_y,			$
				UVALUE = SELF,				$
				NOTIFY_REALIZE = 'map_draw_realize',	$
				RETAIN = 2,				$
;				EVENT_FUNC='georef_image_ev', $
;				RETAIN = 0,				$
;				/EXPOSE_EVENTS,				$
				/BUTTON_EVENTS,				$
				/MOTION_EVENTS,				$
				/TRACKING_EVENTS )
        ;---------------------------------------------------------------------
        ; Store ID of current window
        ;---------------------------------------------------------------------
	tmpWin	= !D.WINDOW

        ;---------------------------------------------------------------------
        ; Create cursorPixmap twice as large as it has to be to accomodate
        ; the portion of the georef_image it occludes
        ;---------------------------------------------------------------------
	WINDOW, /FREE, /PIXMAP, XSIZE = 34, YSIZE = 17
	SELF.cursorPixmap	= !D.WINDOW
	cursorArray		= BYTARR( 17, 17 ) * 0B
	cursorArray[ 8, 0:5 ]	= 255B
	cursorArray[ 8, 11:16 ]	= 255B
	cursorArray[ 0:5, 8 ]	= 255B
	cursorArray[ 11:16, 8 ]	= 255B
	TV, cursorArray

        ;---------------------------------------------------------------------
        ; Reset current window
        ;---------------------------------------------------------------------
	WSET, tmpWin

        ;---------------------------------------------------------------------
        ; Set up GUI for saving scientific data
        ;---------------------------------------------------------------------
	SELF.save_data_tlb	= Return_Save_Data_Base( SELF.parentBase, SELF )

        ;---------------------------------------------------------------------
        ; Initializations
        ;---------------------------------------------------------------------
	SELF.panPixmap		= (-1L)
	SELF.panPixmapX		= 0L
	SELF.panPixmapY		= 0L
	SELF.dataPixmap		= (-1L)
	SELF.buttonsDisplayed	= 0
	SELF.simulatedCursorOn	= 0
	SELF.windowID		= 0L
	SELF.cxs		= 0
	SELF.cys		= 0
	SELF.cnx		= 0
	SELF.cny		= 0
	SELF.cxd		= 0
	SELF.cyd		= 0
	SELF.drawMouseDown	= 0
	SELF.selectAnchorX	= (-999)
	SELF.selectAnchorY	= (-999)
	SELF.currentSelectX	= (-999)
	SELF.currentSelectY	= (-999)
	SELF.selectPixmapXY	= [-1L,-1L]
	SELF.selectXS1		= [-999, -999 ]
	SELF.selectXS2		= [-999, -999 ]
	SELF.selectYS1		= [-999, -999 ]
	SELF.selectYS2		= [-999, -999 ]
	SELF.notFirstResize	= 0B
	UnselectedColors	= [	128L,		$
					32768L,		$
					8838608L,	$
					32896L, 	$
					8838786L,	$
					8421376L ]
	SelectedColors		= [	255L,		$
					65280L,		$
					16711680L,	$
					65535L, 	$
					16711935L,	$
					16776960L ]
	SELF.tabularDataColors[0,*]	= UnselectedColors[0:MAX_SETS-1]
	SELF.tabularDataColors[1,*]	= SelectedColors[0:MAX_SETS-1]
	SELF.distTolerance		= 3 ;device pixels

	IF KEYWORD_SET(transStruct) THEN BEGIN
;
; IMPORTANT NOTE:
; transStruct can be one of the following:
;  - r_vec, g_vec, b_vec, and trans_vec are ALL filled out
;    (this signifies a shared color map mode in 8-bit displays)
;  - r_vec, g_vec, b_vec, and trans_vec are ALL NULL
;    (this implies only that the object, when realized, should
;    set all 4 members in 8-bit mode, except in the special case
;    of only a SINGLE band specified in the object.  In this case,
;    COLOR_QUAN IS NOT USED (simply a TV and a BYTSCL to produce
;    a GRAY-SCALE IMAGE!!!!).  In this special mode, the r_vec, g_vec,
;    and b_vec members are set (for window updates), but the trans_vec
;    IS NOT SET!!!
;  - r_vec, g_vec, b_vec, and trans_vec are set to SCALAR VALUES.
;    this implies that the map_realize procedure should call DisplayData
;    with the /SHARED_MAP keyword so that, in the event of only a single
;    band specified for the object, the grayscale is produced by using
;    COLOR_QUAN with the specified band used for all three (R, G, and B).
;    Check out map_draw_realize to see what it does.
;
;print,'GEOREF_IMAGE::INIT -- transStruct.r_vec = ',transStruct.r_vec
;print,'GEOREF_IMAGE::INIT -- transStruct.g_vec = ',transStruct.g_vec
;print,'GEOREF_IMAGE::INIT -- transStruct.b_vec = ',transStruct.b_vec
	   SELF.red_vec_ptr = PTR_NEW(transStruct.r_vec,/NO_COPY)
	   SELF.grn_vec_ptr = PTR_NEW(transStruct.g_vec,/NO_COPY)
	   SELF.blu_vec_ptr = PTR_NEW(transStruct.b_vec,/NO_COPY)
	   SELF.trans_vec_ptr = PTR_NEW(transStruct.trans,/NO_COPY)
	ENDIF ELSE BEGIN
;print,'GEOREF_IMAGE::INIT -- red_table = ',red_table
;print,'GEOREF_IMAGE::INIT -- grn_table = ',grn_table
;print,'GEOREF_IMAGE::INIT -- blu_table = ',blu_table
	   SELF.red_vec_ptr = PTR_NEW(red_table)
	   SELF.grn_vec_ptr = PTR_NEW(grn_table)
	   SELF.blu_vec_ptr = PTR_NEW(blu_table)
	ENDELSE

	IF !D.N_COLORS LE 256 THEN BEGIN
		IF KEYWORD_SET(nColors8Bit) THEN BEGIN
			SELF.n_8bit_colors = FIX( nColors8Bit )
		ENDIF ELSE BEGIN
			SELF.n_8bit_colors = FIX( !D.N_COLORS )
		ENDELSE
	ENDIF

    ;---------------------------------------------------------------------
    ; set up floating base for query mode with nImgs + 2 being passed in
    ; to include lat/lon imgs
    ;---------------------------------------------------------------------

; tmpAncillaryIdx = WHERE(imgIdx GE 3, ancillaryCnt)

    labels_strarr    = STRARR(nImgs+2)
    units_strarr     = STRARR(nImgs+2)
    ;
    ; If the ability to add more datasets to GEOREF_IMAGE is eventually implemented,
    ; the query_base will have to be destroyed and re-instantiated so that the
    ; new number of datasets is accurately reflected in the number of available
    ; labels
    ;

	SELF.showHideComWinFlag = 0

	IF PTR_VALID(SELF.lonImgPtr[0]) AND PTR_VALID(SELF.latImgPtr[0]) THEN	$
		n_sets = SELF.max_datasets+2					$
	ELSE	n_sets = SELF.max_datasets
	SELF.query_base = SELF->ReturnFloatingQueryBase(			$
				SELF.parentBase,				$
				n_sets,						$
				MAP=SELF.showHideComWinFlag,			$
				WINDOW_TITLE = SELF.title_prefix )

	SELF.showHideComWinFlag = 1

        ;---------------------------------------------------------------------
        ; Increment nObj (from COMMON block)
        ;---------------------------------------------------------------------
	nObj	= nObj + 1

	SELF.acceptFlag = 1

        ;---------------------------------------------------------------------
	; Colorbar window stuff.
        ;---------------------------------------------------------------------
	SELF.colorbar_tlb			= -1L
	SELF.colorbar_draw			= -1L
	SELF.colorbar_window			= -1L
	SELF.colorbar_button_base		= -1L
	SELF.colorbar_sizebutton		= -1L
	SELF.colorbar_xsize_default		= 512
	SELF.colorbar_ysize_default		= 128
	SELF.colorbar_xsize_current		= SELF.colorbar_xsize_default
	SELF.colorbar_ysize_current		= SELF.colorbar_ysize_default
	SELF.colorbar_divisions			= '1'
	SELF.colorbar_charsize			= '1.0'
	SELF.colorbar_charsizes_allowed_ptr	= PTR_NEW( [1.0,0.5,0.6,0.7,0.8,0.9,1.2,1.4,1.6,1.8,2.0] )

        ;---------------------------------------------------------------------
        ; Return success flag
        ;---------------------------------------------------------------------
	RETURN, success
END
; GEOREF_IMAGE::INIT

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE::CLEANUP @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE::CLEANUP
;-----------------------------------------------------------------------------
; Map window destructor routine
;-----------------------------------------------------------------------------


	COMMON GEOREFIMAGE, linkObj, nullObj, linkMsg, nObj, idArr, commonblock_pseudo_color_table_ptrarr, commonblock_self

	IF WIDGET_INFO(SELF.query_base, /VALID_ID) THEN 			$
		WIDGET_CONTROL, SELF.query_base, /DESTROY

	IF WIDGET_INFO(SELF.save_data_tlb, /VALID_ID) THEN 			$
			WIDGET_CONTROL, SELF.save_data_tlb, /DESTROY

	IF WIDGET_INFO(SELF.stretchDnBase, /VALID_ID) THEN			$
		newRealize	= SELF->ToggleStretchDnInterface( 0 )

        ;---------------------------------------------------------------------
        ; Use Self.id as an index into idArr, and set this element
        ; of idArr to 0, indicating that it is available for use once
        ; again as a window identifier (in the window title string)
        ;---------------------------------------------------------------------
	idArr[SELF.id-1] = 0B

        ;---------------------------------------------------------------------
        ; Free pointers to lon/lat images, if valid
        ;---------------------------------------------------------------------
;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change
IF PTR_VALID((SELF.lonImgPtr)[0]) THEN BEGIN
	IF OBJ_VALID(*( (SELF.lonImgPtr)[0] )) THEN OBJ_DESTROY, *( (SELF.lonImgPtr)[0] )
	PTR_FREE, (SELF.lonImgPtr)[0]
ENDIF
IF PTR_VALID((SELF.latImgPtr)[0]) THEN BEGIN
	IF OBJ_VALID(*( (SELF.latImgPtr)[0] )) THEN OBJ_DESTROY, *( (SELF.latImgPtr)[0] )
	PTR_FREE, (SELF.latImgPtr)[0]
ENDIF
IF PTR_VALID((SELF.lonImgPtr)[1]) THEN BEGIN
	IF OBJ_VALID(*( (SELF.lonImgPtr)[1] )) THEN OBJ_DESTROY, *( (SELF.lonImgPtr)[1] )
	PTR_FREE, (SELF.lonImgPtr)[1]
ENDIF
IF PTR_VALID((SELF.latImgPtr)[0]) THEN BEGIN
	IF OBJ_VALID(*( (SELF.latImgPtr)[1] )) THEN OBJ_DESTROY, *( (SELF.latImgPtr)[1] )
	PTR_FREE, (SELF.latImgPtr)[1]
ENDIF

;	IF PTR_VALID((SELF.lonImgPtr)[0]) THEN PTR_FREE, (SELF.lonImgPtr)[0]
;	IF PTR_VALID((SELF.latImgPtr)[0]) THEN PTR_FREE, (SELF.latImgPtr)[0]
;	IF PTR_VALID((SELF.lonImgPtr)[1]) THEN PTR_FREE, (SELF.lonImgPtr)[1]
;	IF PTR_VALID((SELF.latImgPtr)[1]) THEN PTR_FREE, (SELF.latImgPtr)[1]
;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change

        ;---------------------------------------------------------------------
        ; Free dataPtr, if valid.  Since dataPtr is itself a pointer to
        ; a PTRARR(), first free all valid members of the pointer array,
        ; then free dataPtr.
        ;---------------------------------------------------------------------
	IF PTR_VALID(SELF.dataPtr) THEN BEGIN
;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change
FOR i = 0, N_ELEMENTS(*(SELF.dataPtr)) - 1 DO BEGIN
	IF OBJ_VALID( *((*(SELF.dataPtr))[i]) ) THEN OBJ_DESTROY, *((*(SELF.dataPtr))[i])
	PTR_FREE, (*(SELF.dataPtr))[i]
ENDFOR
;		FOR i = 0, N_ELEMENTS(*(SELF.dataPtr)) - 1 DO $
;			PTR_FREE, (*(SELF.dataPtr))[i]
;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change
		PTR_FREE, SELF.dataPtr
		PTR_FREE, SELF.dataXS
		PTR_FREE, SELF.dataYS
		PTR_FREE, SELF.dataNX
		PTR_FREE, SELF.dataNY
		PTR_FREE, SELF.zoomPctX
		PTR_FREE, SELF.zoomPctY
	ENDIF

;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change
;	IF PTR_VALID(SELF.missingDataPtr) THEN BEGIN
;		PTR_FREE, *(SELF.missingDataPtr)
;		PTR_FREE, SELF.missingDataPtr
;	ENDIF
;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change
	result = depointer( SELF.dataMinValPtr )
	result = depointer( SELF.dataMaxValPtr )
	result = depointer( SELF.histBinSize )
	result = depointer( SELF.stretchDnBottomPtr )
	result = depointer( SELF.stretchDnTopPtr )
	result = depointer( SELF.stretchDnGammaPtr )
	result = depointer( SELF.bottomLimitPtr )
	result = depointer( SELF.topLimitPtr )

	result = depointer( SELF.calc_hist_for_displayed_data_ptr )
	result = depointer( SELF.rgbHistogramPtrarr )

	result = depointer( SELF.min_dataPtr )
	result = depointer( SELF.max_dataPtr )

	result = depointer( SELF.tabularDataPtr )
	result = depointer( SELF.selectedTabularDataPtr )

	IF PTR_VALID(SELF.modeButtonsPtr) THEN PTR_FREE, SELF.modeButtonsPtr

	IF PTR_VALID(SELF.red_vec_ptr) THEN PTR_FREE, SELF.red_vec_ptr
	IF PTR_VALID(SELF.grn_vec_ptr) THEN PTR_FREE, SELF.grn_vec_ptr
	IF PTR_VALID(SELF.blu_vec_ptr) THEN PTR_FREE, SELF.blu_vec_ptr
	IF PTR_VALID(SELF.trans_vec_ptr) THEN PTR_FREE, SELF.trans_vec_ptr


	IF PTR_VALID(SELF.data_description_ptr) THEN PTR_FREE, SELF.data_description_ptr
	IF PTR_VALID(SELF.data_source_filename_ptr) THEN PTR_FREE, SELF.data_source_filename_ptr
	IF PTR_VALID(SELF.ancillaryBandIdxPtr) THEN PTR_FREE, SELF.ancillaryBandIdxPtr
        ;---------------------------------------------------------------------
        ; If this object is linked to others, query the user as to whether
        ; or not he wants to destory all linked windows as well.
        ;---------------------------------------------------------------------
	idx	= WHERE(OBJ_VALID(SELF.linkObj),cnt)
	IF cnt GT 0 THEN BEGIN
;		msg	= [ 'This window is linked...',		$
;			    'close linked windows as well?' ]
;		res	= DIALOG_MESSAGE( msg, /QUESTION )
                ;-------------------------------------------------------------
                ; Set linkedObjList to all valid objects in the OBJARR()
                ;-------------------------------------------------------------
		linkedObjList	= SELF.linkObj[idx]

                ;-------------------------------------------------------------
                ; If all linked objects are to be destroyed as well, first
                ; clear their linked object lists, then call the destructor
                ;-------------------------------------------------------------
;		IF res EQ 'Yes' THEN BEGIN
;			FOR i = 0, N_ELEMENTS(linkedObjList)-1 DO BEGIN
;			   (linkedObjList[i])->ClearLinkObj
;			   OBJ_DESTROY, linkedObjList[i]
;			ENDFOR
                ;-------------------------------------------------------------
                ; Otherwise, call the method UnLinkObj to remove the current
                ; object from the linked object lists of the other objects
                ;-------------------------------------------------------------
;		ENDIF ELSE BEGIN
			FOR i = 0, N_ELEMENTS(linkedObjList)-1 DO $
			   (linkedObjList[i])->UnLinkObj, SELF
;		ENDELSE
	ENDIF

        ;---------------------------------------------------------------------
        ; Decrement nObj (from COMMON block)
        ;---------------------------------------------------------------------
	nObj	= nObj - 1

        ;---------------------------------------------------------------------
        ; If nObj is 0 and the link message interface is still around,
        ; destroy it!
        ;---------------------------------------------------------------------
	IF nObj LE 0 AND linkMsg GE 0 THEN BEGIN
		WIDGET_CONTROL, linkMsg, /DESTROY
		linkMsg			= (-1L)
	ENDIF

	IF PTR_VALID( SELF.x_axis_for_colorbar_ptr ) THEN		$
		PTR_FREE, SELF.x_axis_for_colorbar_ptr
	IF PTR_VALID( SELF.colorbar_charsizes_allowed_ptr ) THEN	$
		PTR_FREE, SELF.colorbar_charsizes_allowed_ptr

END
; GEOREF_IMAGE::CLEANUP

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE__DEFINE @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE__DEFINE
;-----------------------------------------------------------------------------
; Map window object definition
;-----------------------------------------------------------------------------

        ;---------------------------------------------------------------------
        ; Maximum number of linked windows per object
        ;---------------------------------------------------------------------
	MAX_LINK_OBJ	= 5
	MAX_SETS	= 6

	GEOREF_IMAGE		=	{				$
		GEOREF_IMAGE,						$
			drawBase		:0L,			$
			modeButtonsPtr		:PTR_NEW(),		$
			id			:0B,			$
			drawMouseDown		:0,			$
			dataPtr			:PTR_NEW(),		$
;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change
;			missingDataPtr		:PTR_NEW(),		$
;IMAGE_DATA change
;IMAGE_DATA change
;IMAGE_DATA change
			tabularDataPtr			:PTR_NEW(),		$
			selectedTabularDataPtr		:PTR_NEW(),		$
			distTolerance			:0,			$
			tabularDataColors		:LONARR(2,MAX_SETS),	$
			lonImgPtr			:PTRARR(2),		$
			latImgPtr			:PTRARR(2),		$
			viewportNX			:0L,			$
			viewportNY			:0L,			$
			minXsize			:0L,			$
			minYsize			:0L,			$
			dataIdx				:0,			$
			lonXS				:0.0,			$
			lonYS				:0.0,			$
			lonNX				:0.0,			$
			lonNY				:0.0,			$
			latXS				:0.0,			$
			latYS				:0.0,			$
			latNX				:0.0,			$
			latNY				:0.0,			$
			lonLatZoomPctX			:0.0,			$
			lonLatZoomPctY			:0.0,			$
			dataXS				:PTR_NEW(),		$
			dataNX				:PTR_NEW(),		$
			dataYS				:PTR_NEW(),		$
			dataNY				:PTR_NEW(),		$
			zoomPctX			:PTR_NEW(),		$
			zoomPctY			:PTR_NEW(),		$
			zoomIncX			:0.0,			$
			zoomIncY			:0.0,			$
			mode				:'',			$
			modeID				:0L,			$
			drawID				:0L,			$
			windowID			:0L,			$
			nChannels			:0,			$
			drawXsize			:0,			$
			drawYsize			:0,			$
			redBandIdx			:(-1),			$
			grnBandIdx			:(-1),			$
			bluBandIdx			:(-1),			$
			linkButton			:0L,			$
			unlinkButton			:0L,			$
			panPixmapX			:0L,			$
			panPixmapY			:0L,			$
			panDiffX			:0L,			$
			panDiffY			:0L,			$
			panAnchorPt			:LONARR(2),		$
			view_nx				:0L,			$
			view_ny				:0L,			$
			max_diff_x_left			:0L,			$
			max_diff_x_right		:0L,			$
			max_diff_y_lower		:0L,			$
			max_diff_y_upper		:0L,			$
			src_x				:0L,			$
			src_y				:0L,			$
			nx				:0L,			$
			ny				:0L,			$
			dest_x				:0L,			$
			dest_y				:0L,			$
			panPixmap			:0L,			$
			linkObj				:OBJARR(MAX_LINK_OBJ),	$
			buttonsDisplayed		:0,			$
			parentBase			:(-1L),			$
			buttonBase			:0L,			$
			simulatedCursorOn		:0,			$
			dataPixmap			:0L,			$
			dataPixmapXsize			:0L,			$
			dataPixmapYsize			:0L,			$
			cursorPixmap			:0L,			$
			cxs				:0,			$
			cys				:0,			$
			cnx				:0,			$
			cny				:0,			$
			cxd				:0,			$
			cyd				:0,			$
			selectAnchorX			:0,			$
			selectAnchorY			:0,			$
			currentSelectX			:0,			$
			currentSelectY			:0,			$
			currentLonLat			:FLTARR(2),		$
			anchorLonLat			:FLTARR(2),		$
			selectPixmapXY			:LONARR(2),		$
			selectXS1			:INTARR(2),		$
			selectXS2			:INTARR(2),		$
			selectYS1			:INTARR(2),		$
			selectYS2			:INTARR(2),		$
			selectNX			:0,			$
			selectNY			:0,			$
			notFirstResize			:0B,			$
			stretchDnBaseToggledOnOrOff	:0,			$
			stretchDnButton			:0L,			$
			colorTablesButton		:0L,			$
			stretchDnBase			:0L,			$
			histogramWidgetDrawIds		:LONARR( 6 ),		$
			histogramPixmapId		:0L,			$
			histBinSize			:PTR_NEW(),		$
			dataMinValPtr			:PTR_NEW(),		$
			dataMaxValPtr			:PTR_NEW(),		$
			stretchDnBottomPtr		:PTR_NEW(),		$
			stretchDnTopPtr			:PTR_NEW(),		$
			stretchDnGammaPtr		:PTR_NEW(),		$
			bottomLimitPtr			:PTR_NEW(),		$
			topLimitPtr			:PTR_NEW(),		$
			rgbHistogramPtrarr		:PTRARR(3),		$
			x_tickv_values_ptr		:PTR_NEW(),		$
			red_vec_ptr             	:PTR_NEW(),		$
			grn_vec_ptr             	:PTR_NEW(),		$
			blu_vec_ptr             	:PTR_NEW(),		$
			trans_vec_ptr           	:PTR_NEW(),		$
			NUMBER_HISTOGRAM_COLORS		:0,			$
			n_8bit_colors           	:0,			$
			title_prefix            	:'',        		$
			table_obj_base          	:0L,        		$
			data_description_ptr		:PTR_NEW(),		$
			data_source_filename_ptr	:PTR_NEW(),		$
			lon_source_filename		:'',			$
			lat_source_filename		:'',			$
			max_datasets			:0L,			$
			ancillaryBandIdxPtr		:PTR_NEW(),		$
			display_rgb			:INTARR(3),		$
			manageDataButton		:0L,			$
			perspectiveButton		:0L,			$
			bandSliderButton		:0L,			$
			scrollToolButton		:0L,			$
			saveImageButton			:0L,			$
			saveDataButton			:0L,			$
			transformDataButton		:0L,			$
			vectorOverlayTool		:0L,			$
			reprojectionTool		:0L,			$
			personalToolsMenu		:0L,			$
			addPersonalTool			:0L,			$
			min_dataPtr			:PTR_NEW(),		$
			max_dataPtr			:PTR_NEW(),		$
			calc_hist_for_displayed_data_ptr:PTR_NEW(),	$
			save_data_tlb           	:0L,			$
			query_base              	:0L,			$
			acceptFlag			:0L,			$
			use_upper_left_origin		:0B,			$
			highest_res_data_idx		:0L,			$
			showHideComWinBut		:0L,			$
			showHideComWinFlag		:0L,			$
			cursor_x			:0L,			$
			cursor_y			:0L,			$
			x_axis_for_colorbar_ptr		:PTR_NEW(),		$
			colorbar_tlb			:(-1L),			$
			colorbar_draw			:(-1L),			$
			colorbar_window			:(-1L),			$
			colorbar_button_base		:(-1L),			$
			colorbar_sizebutton		:(-1L),			$
			colorbar_xsize_default		:0L,			$
			colorbar_ysize_default		:0L,			$
			colorbar_xsize_current		:0L,			$
			colorbar_ysize_current		:0L,			$
			colorbar_divisions		:'',			$
			colorbar_charsize		:'',			$
			colorbar_charsizes_allowed_ptr	:PTR_NEW(),		$
			min_zoom_pct			: (-1.0),		$
			max_zoom_pct			: (-1.0),		$
			report_global_coordinates	: 0			$
			 }
END
; GEOREF_IMAGE__DEFINE
