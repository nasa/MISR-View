;12345678901234567890123456789012345678901234567890123456789012345678901234
;+
;==========================================================================
;
;Module Name:	misr_view
;
;Call Protocol:	misr_view
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
;		Jeffrey R Hall		Jeffrey.R.Hall@jpl.nasa.gov
;
;===========================================================================
;
;Description:
;
; misr_view.pro is the main module of the misr_view software.  It contains the following
; routines:
;
;	PROCEDURES
;	==========
;	misr_kill_window
;	misr_kill_location_window
;	askUserForInterpType_eh
;	create_proj_info_window_eh
;	create_proj_info_window
;	misr_view_eh
;	misr_view_kill
;	create_splash_screen
;	misr_view
;
;	FUNCTIONS
;	=========
;	EightBit_ApplyColorMap
;	Get_Please_Wait_Dlg
;	get_location_window
;	GetAGPFileName
;	GetGMPFileName
;	askUserForInterpType
;	rotator_90inc
;	rotate_data
;	get_new_projection_window
;	get_new_projection_window_8bit
;	get_new_window
;	get_new_window_8bit
;
;Input Parameters:
;
;	Type	Name		Units		Purpose
;	-------------------------------------------------------------------
;	    	fontRequestString	string describing the kind of font
;					desired
;
;Output Parameters:
;
;	Type	Name		Units	Purpose
;	-------------------------------------------------------------------
;		fontString	 	platform-dependent string representing
;					the desired font which can be used in
;					widget creation routines
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
;	misr_view 3.1		Mar 2000
;	misr_view 4.0		Mar 2001
;
;Notes:
;
;
;=============================================================================
;-
;

@misr_readHDFEOSgrid.pro
@parseCatalog.pro
@misr_data_selection.pro
@MISR_GEOREF_IMAGE.PRO
@MISR_IMAGE_DATA.pro
@ReturnListSelectionIdx.pro
@is_valid_number.pro
@GetDirectoryDivider.pro
@data_selections_write.pro
@data_selections_read.pro
@depointer.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ misr_kill_window @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO misr_kill_window, windowBase
	WIDGET_CONTROL, windowBase, GET_UVALUE = wPtr

	IF PTR_VALID(wPtr) THEN BEGIN

		;--------------------------------------------------------------
		; Re-run MAP_SET to fix the map projection problem in the
		; block chooser interface map.  This problem is caused by
		; killing a GEOREF_IMAGE window, thus calling MAP_SET in the
		; KILL_NOTIFY routine.
		;--------------------------------------------------------------
		IF WIDGET_INFO( (*((*wPtr).tlbPtr)).dataSelectionBase, /VALID_ID ) THEN BEGIN
			WIDGET_CONTROL, WIDGET_INFO( (*((*wPtr).tlbPtr)).dataSelectionBase, /CHILD ), GET_UVALUE = upperBaseStatePtr
			curWin = !D.WINDOW
		 	WSET,(*upperBaseStatePtr).mapDrawWindowID
			MAP_SET,									$
				(*upperBaseStatePtr).mapCenterLatLon[0],				$
				(*upperBaseStatePtr).mapCenterLatLon[1],				$
				0.0,									$
				/NOERASE,								$
				_EXTRA = *((*upperBaseStatePtr).mapSetStruct)
			WSET, curWin
		ENDIF

		;--------------------------------------------------------------
		; Destroy the GEOREF_IMAGE object.
		;--------------------------------------------------------------
		IF OBJ_VALID((*wPtr).obj) THEN OBJ_DESTROY, (*wPtr).obj
		PTR_FREE, wPtr

	ENDIF

END
; misr_kill_window

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ misr_kill_location_window @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO misr_kill_location_window, windowBase
   WIDGET_CONTROL, windowBase, GET_UVALUE = wPtr
   IF PTR_VALID(wPtr) THEN BEGIN
      IF OBJ_VALID((*wPtr).obj) THEN BEGIN
         OBJ_DESTROY, (*wPtr).obj
         (*(*wPtr).tlbPtr).locationMapBase = -1
      ENDIF
      WIDGET_CONTROL, (*wPtr).toggleID, SET_VALUE = 'Show Location Map'
      PTR_FREE, wPtr
   ENDIF
END
; misr_kill_location_window

;8.  when the locator window is killed via X-window-quit the variable
;	(*(dws.infoPtr)).locationMapBase is not reset to -1.  This can
;	sometimes cause misr_view to crash when opening another image window
;	(in 8-bit mode anyway, not confirmed in 24-bit mode).  See
;	misr_view.pro lines 400-417, crash occurs when code reaches line 417:
;		trans = *((*(dws.infoPtr)).translationPtr)


;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ Get_Please_Wait_Dlg @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION Get_Please_Wait_Dlg, GROUP_LEADER = groupLeader

	IF KEYWORD_SET(groupLeader) THEN                     $
		b = WIDGET_BASE(MAP=0,GROUP_LEADER = groupLeader,TITLE="Please Wait...") $
	ELSE                                                 $
		b = WIDGET_BASE(MAP=0,TITLE="Please Wait...")

	l = WIDGET_LABEL(b, VALUE = 'Creating Image Window...' )
	WIDGET_CONTROL, b, /REALIZE

	RETURN, b
END
; Get_Please_Wait_Dlg

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ get_location_window @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION get_location_window, toggleID, tlbPtr, _Extra = extra

   IF NOT WIDGET_INFO( (*tlbPtr).please_wait_dlg, /VALID_ID ) THEN $
      (*tlbPtr).please_wait_dlg = Get_Please_Wait_Dlg()
   WIDGET_CONTROL, (*tlbPtr).please_wait_dlg, MAP = 1
   WIDGET_CONTROL, /HOURGLASS

   wTitle = 'misr_view location window'
   b      = WIDGET_BASE( /COLUMN, /TLB_SIZE_EVENTS, TITLE = wTitle, KILL_NOTIFY = 'misr_kill_location_window', $
                         EVENT_PRO = 'misr_view_eh' )

;;;ckt,feb2000   locImg = READ_TIFF( 'backImg.tiff', r1, g1, b1 )
;;;ckt,feb2000   sz     = SIZE(locImg)

   sz     = SIZE((*tlbPtr).locImg)

   rr_img     = BYTARR( sz[sz[0]-1], sz[sz[0]] )
   gg_img     = rr_img
   bb_img     = gg_img
   rr_img[*,*]= (*tlbPtr).locImg[0,*,*]
   rr_img     = REVERSE( TEMPORARY(rr_img), 2 )
   gg_img[*,*]= (*tlbPtr).locImg[1,*,*]
   gg_img     = REVERSE( TEMPORARY(gg_img), 2 )
   bb_img[*,*]= (*tlbPtr).locImg[2,*,*]
   bb_img     = REVERSE( TEMPORARY(bb_img), 2 )

   rr	= OBJ_NEW( 'IMAGE_DATA', rr_img, [0], [0] )
   gg	= OBJ_NEW( 'IMAGE_DATA', gg_img, [0], [0] )
   bb	= OBJ_NEW( 'IMAGE_DATA', bb_img, [0], [0] )

   xy_dims	= SIZE( (*tlbPtr).locImg, /DIMENSIONS )
   xdim		= xy_dims[1]
   ydim		= xy_dims[2]
;;;print,'xy_dims=',xy_dims
   lon_img	= (FINDGEN(xdim,ydim) MOD xdim)*(360.0/(xdim-1))-180.0
;;; print,'lon_img[*,0]=',lon_img[*,0]
   lat_img	= (LINDGEN(xdim,ydim)  /  xdim)*(180.0/(ydim-1))- 90.0
;;;ckt,oct30,2002   lon_img    = ( ( FINDGEN(sz[sz[0]-1],sz[sz[0]]) MOD sz[sz[0]-1] )*( float( sz[sz[0]-1] ) / float( sz[sz[0]-1]-1 ) ) / 2.0 ) - 180.0
;;;ckt,oct30,2002   lat_img   = (FLOAT((LINDGEN(sz[sz[0]-1],sz[sz[0]])/sz[sz[0]-1])*(float(sz[sz[0]])/float(sz[sz[0]]-1)))/2.0) - 90.0

   lon	= OBJ_NEW( 'IMAGE_DATA', lon_img, [0], [0] )
   lat	= OBJ_NEW( 'IMAGE_DATA', lat_img, [0], [0] )

   minx   = 500; 360
   miny   = 250; 180

   IF (*tlbPtr).n_available_colors LE 256 THEN n_chan = 1 ELSE n_chan = 3

   mw     = OBJ_NEW( 'GEOREF_IMAGE',                                      $
               b,                                                         $
               LON_IMAGE = lon,                                           $
               LAT_IMAGE = lat,                                           $
               DATA_PTR = [ PTR_NEW(rr), PTR_NEW(gg), PTR_NEW(bb) ],      $
               XSIZE = minx,                                              $
               YSIZE = miny,                                              $
               /NO_SELECT,                                                $
               N_CHANNELS = n_chan,                                       $
 	       /USE_UPPER_LEFT_ORIGIN,			  		  $
		;===========================================================
		; min/max zooms are based upon zooming 9 levels in
		; both directions.  If the baseline zoom is 1.0 (100%),
		; then the next level of zooming is either 50% or 200%,
		; depending on whether the user is zooming in or out.  The
		; next eight levels for zoom in/zoom out are:
		;
		;			25%		or 400%
		;			12.5%		or 800%
		; 			6.25%		or 1600%
		;			3.125%		or 3200%
		;			1.5625% 	or 6400%
		;			0.78125%	or 12800%
 		;			0.390625%	or 25600%
 		;			0.195312%	or 51200%
		;===========================================================
 	       MIN_ZOOM_PCT = 0.195312,					  $
 	       MAX_ZOOM_PCT = 51200.0,					  $
 	       /REPORT_GLOBAL_COORDINATES,				  $
              _EXTRA = extra )

   WIDGET_CONTROL, b, /REALIZE
   WIDGET_CONTROL, b, TLB_GET_SIZE = xydims
   baseX2objX = FLOAT(xydims[0])/FLOAT(minx)
   baseY2objY = FLOAT(xydims[1])/FLOAT(miny)

  (*tlbPtr).lastActiveObj    = mw

   infoPtr    = PTR_NEW( { minx         :minx,        $
                           miny         :miny,        $
                           minBaseXYdims:xydims,      $
                           objxydims    :[minx,miny], $
                           obj          :mw,          $
                           baseX2objX   :baseX2objX,  $
                           baseY2objY   :baseY2objY,  $
                           toggleID     :toggleID,    $
                           tlbPtr       :tlbPtr,      $
                           winTitle     :wTitle },    $
                           /NO_COPY)
 WIDGET_CONTROL, b, SET_UVALUE = infoPtr

 IF NOT WIDGET_INFO( (*tlbPtr).please_wait_dlg, /VALID_ID ) THEN $
  (*tlbPtr).please_wait_dlg = Get_Please_Wait_Dlg()
 WIDGET_CONTROL, (*tlbPtr).please_wait_dlg, MAP = 0

 RETURN,b
END
; get_location_window

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GetAGPFileName @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GetAGPFileName, pathStr, catContents
	fn = ''
	pathStr = STRMID('00'+pathStr,STRLEN(pathStr)-1,3)
	agpIdx = WHERE( catContents[*,0] EQ 'AGP' AND catContents[*,1] EQ pathStr, cnt )
	IF cnt GT 0 THEN BEGIN
		idx2Use = agpIdx[0]
		fn = STRTRIM(catContents[idx2Use,4],2) + STRTRIM(catContents[idx2Use,5],2)
	ENDIF
	RETURN, fn
END
; GetAGPFileName

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GetGMPFileName @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GetGMPFileName, pathStr, orbitStr, catContents, planeFileNames
	fn = ''
;print,'>>>>>>>>>>>>>> planeFileNames=',planeFileNames
	; Check if GMP required, if camera(s) is(are) being displayed.
	files = STRTRIM(catContents[*,4],2) + STRTRIM(catContents[*,5],2)
	cameras = STRARR( 6 )
	FOR i=0,5 DO BEGIN
		idx = WHERE( files EQ planeFileNames(i), count )
		IF count GT 0 THEN BEGIN
			; Parse filename, determine if camera or other.
			cameras[i] = catContents[idx[0],3]
		ENDIF
	ENDFOR
;print,'cameras = ', cameras
	idx = WHERE( STRLEN( cameras ) GT 0, count )
;print,'count = ',count
;help,idx
	GMP_required = count GT 0

	pathStr = STRMID('00'+pathStr,STRLEN(pathStr)-1,3)
	orbitStr = STRMID('00000'+orbitStr,STRLEN(orbitStr)-1,6)
	gmpIdx = WHERE( catContents[*,0] EQ 'GP_GMP' AND			$
		catContents[*,1] EQ pathStr AND				$
		catContents[*,2] EQ orbitStr, cnt )
	IF cnt GT 0 AND GMP_required THEN BEGIN
		idx2Use = gmpIdx[0]
		fn = STRTRIM(catContents[idx2Use,4],2) + STRTRIM(catContents[idx2Use,5],2)
	ENDIF
	RETURN, { fn : fn, cameras : cameras, camerasIdx : idx }
END
; GetGMPFileName

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ askUserForInterpType_eh @@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO askUserForInterpType_eh, event

	WIDGET_CONTROL, event.top, GET_UVALUE = rotationInterpTypePtr
	WIDGET_CONTROL, event.id, GET_VALUE = button_name

	CASE 1 OF
		STRUPCASE(button_name) EQ 'NEAREST NEIGHBOR'		OR $
		STRUPCASE(button_name) EQ 'BILINEAR'			OR $
		STRUPCASE(button_name) EQ 'CUBIC'			: BEGIN
			WIDGET_CONTROL, event.id, GET_VALUE = bVal
			*rotationInterpTypePtr = STRTRIM(bVal,2)
		END
		STRUPCASE(button_name) EQ 'OK'				: BEGIN
			WIDGET_CONTROL, event.top, /DESTROY
		END
		STRUPCASE(button_name) EQ 'CANCEL'			: BEGIN
			*rotationInterpTypePtr = 'CANCEL'
			WIDGET_CONTROL, event.id, GET_UVALUE = groupLeader
			WIDGET_CONTROL, groupLeader, SENSITIVE = 1
			WIDGET_CONTROL, event.top, /DESTROY
		END
		ELSE:
	ENDCASE

END
; askUserForInterpType_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ askUserForInterpType @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION askUserForInterpType, rotationAngle, groupLeader

	interpBase = WIDGET_BASE( /COLUMN, /MODAL, GROUP_LEADER = groupLeader )
	interpLabel = WIDGET_TEXT( interpBase, YSIZE = 5, VALUE = [ $
		'rotationAngle = ' + STRTRIM(STRING(rotationAngle),2) + ' degrees clockwise.', $
		'Rotation angles that are not evenly', $
		'divisible by 90.0 require resampling.  ', $
		'You may choose from the following', $
		'resampling methods:' ] )
	interpRadioBase = WIDGET_BASE( interpBase, /EXCLUSIVE, /COLUMN )
	interpButton1 = WIDGET_BUTTON( interpRadioBase, UVALUE = 'interp_type',	$
					VALUE = 'Nearest Neighbor' )
	interpButton2 = WIDGET_BUTTON( interpRadioBase, UVALUE = 'interp_type',	$
					VALUE = 'Bilinear' )
	interpButton3 = WIDGET_BUTTON( interpRadioBase, UVALUE = 'interp_type',	$
					VALUE = 'Cubic' )
	interpButtonOK = WIDGET_BUTTON( interpBase, UVALUE = 'ok',		$
					VALUE = 'OK' )
	interpButtonCANCEL = WIDGET_BUTTON( interpBase, UVALUE = groupLeader,	$
					VALUE = 'Cancel' )
	WIDGET_CONTROL, interpButton1, /SET_BUTTON
	WIDGET_CONTROL, interpBase, DEFAULT_BUTTON = interpButtonOK

	WIDGET_CONTROL, interpBase, /REALIZE
	rotationInterpTypePtr = PTR_NEW( 'Nearest Neighbor' )
	WIDGET_CONTROL, interpBase, SET_UVALUE = rotationInterpTypePtr
	XMANAGER, 'interpBase', interpBase, EVENT_HANDLER = 'askUserForInterpType_eh'
	returnVal	= *rotationInterpTypePtr
	PTR_FREE, rotationInterpTypePtr
	RETURN, returnVal
END
; askUserForInterpType

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ rotator_90inc @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION rotator_90inc, a, rot_val
	trans_idx	= [1,0]
	IF (SIZE(a))[0] eq 3 THEN trans_idx=[1,0,2]

	CASE rot_val OF
		90.0:	RETURN, TRANSPOSE(TEMPORARY(REVERSE(TEMPORARY(a),2)),trans_idx)
		-90.0:	RETURN, TRANSPOSE(TEMPORARY(REVERSE(TEMPORARY(a))),trans_idx)
		180.0:	RETURN, REVERSE(TEMPORARY(REVERSE(TEMPORARY(a),1)),2)
		ELSE:	RETURN, a
	ENDCASE

;;;ckt,mar2000	trans_idx	= [1,0]
;;;ckt,mar2000	IF (SIZE(a))[0] eq 3 THEN trans_idx=[1,0,2]

;;;ckt,mar2000	CASE rot_val OF
;;;ckt,mar2000		90.0:	RETURN, TRANSPOSE(REVERSE(a,2),trans_idx)
;;;ckt,mar2000		-90.0:	RETURN, TRANSPOSE(REVERSE(a),trans_idx)
;;;ckt,mar2000		180.0:	RETURN, REVERSE(REVERSE(a,1),2)
;;;ckt,mar2000		ELSE:	RETURN, a
;;;ckt,mar2000	ENDCASE
END
; rotator_90inc

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ rotate_data @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION rotate_data, imgStruct, rotationAngle, interpType

	;--------------------------------------------------------------------
	; Insure the rotationAngle value to be in the range:  0.0 <= rotationAngle < 360.0
	;--------------------------------------------------------------------
	rotationAngle = FLOAT( rotationAngle )
	IF rotationAngle GE 360.0 THEN WHILE rotationAngle GE 360.0 DO rotationAngle = rotationAngle - 360.0
	IF rotationAngle LT   0.0 THEN WHILE rotationAngle LT   0.0 DO rotationAngle = rotationAngle + 360.0


;----------------------------------------------------------------
; Block of code to deal with rotations of 90 degrees
;----------------------------------------------------------------
IF rotationAngle EQ 90.0 OR rotationAngle EQ 180.0 OR rotationAngle EQ 270.0 THEN BEGIN

	block_width	= (SIZE(REFORM( imgStruct.blocks[*,*,0] )))[1]
	block_height	= (SIZE(REFORM( imgStruct.blocks[*,*,0] )))[2]
	mosaic_height	= MAX(imgStruct.offsetY) + block_height
	mosaic_width	= MAX(imgStruct.offsetX) + block_width
;print,'	rotationAngle = ',rotationAngle
	CASE 1 OF
		rotationAngle EQ 90.0: BEGIN
;print,'ready to call rotator_90_inc'
;help,imgStruct.blocks
			tmpArr		= rotator_90inc( imgStruct.blocks, (-90.0) )
;help,tmpArr
			new_x_offset	= imgStruct.offsetY
			new_y_offset	= mosaic_width - imgStruct.offsetX - block_width
			block_numbers	= imgStruct.block_numbers
			END
		rotationAngle EQ 180.0: BEGIN
			tmpArr		= rotator_90inc( imgStruct.blocks, rotationAngle )
			new_x_offset	= mosaic_width - imgStruct.offsetX - block_width
			new_y_offset	= mosaic_height - imgStruct.offsetY - block_height
			block_numbers	= REVERSE(imgStruct.block_numbers)
			END
		rotationAngle EQ 270.0: BEGIN
			tmpArr		= rotator_90inc( imgStruct.blocks, 90.0 )
			new_x_offset	= mosaic_height - imgStruct.offsetY - block_height
			new_y_offset	= imgStruct.offsetX
			block_numbers	= REVERSE(imgStruct.block_numbers)
			END
		ELSE:
	ENDCASE

	RETURN, {	block_numbers	: block_numbers,		$
			blocks		: tmpArr,			$
			offsetX		: new_x_offset,			$
			offsetY		: new_y_offset,			$
			resize_data_factor	: imgStruct.resize_data_factor }
ENDIF



	XOldSize	= N_ELEMENTS( imgStruct.blocks[*,0,0] )	;Horizontal.
	YOldSize	= N_ELEMENTS( imgStruct.blocks[0,*,0] )	;Vertical.
	n_blocks	= N_ELEMENTS( imgStruct.blocks[0,0,*] )
	type		= SIZE( imgStruct.blocks, /TYPE )
	rdata_ptrarr	= PTRARR( n_blocks )
	convert2double	= 0

	;--------------------------------------------------------------------
	; Do only if rotationAngle is non-zero.  Rotation is primarily for red-blue
	; stereo viewing but could be employed for other unforeseen purposes.
	;--------------------------------------------------------------------
	IF rotationAngle NE 0.0 THEN BEGIN
		CASE 1 OF
			;--------------------------------------------------------------
			; ROTATE is used for 90 degree increments of rotation.
			; ROT is used for arbitrary rotation angles.
			;--------------------------------------------------------------
			rotationAngle EQ  90.0 OR rotationAngle EQ 180.0 OR rotationAngle EQ 270.0 : BEGIN
				IF rotationAngle EQ  90.0 THEN direction = 3	; 270 counter-clockwise
				IF rotationAngle EQ 180.0 THEN direction = 2	; 180 degree rotation
				IF rotationAngle EQ 270.0 THEN direction = 1	; 90 counter-clockwise
				FOR i = 0, n_blocks - 1 DO BEGIN
					; ROTATE cannot operate on a three dimensional array.
					temp		= imgStruct.blocks[*,*,i]
					rtemp		= ROTATE( temp, direction )
					rdata_ptrarr[i] = PTR_NEW( rtemp )
				ENDFOR
			END
			ELSE : BEGIN

				IF STRUPCASE(interpType) EQ 'BILINEAR' THEN $
					extra = { INTERP : 1 }
				IF STRUPCASE(interpType) EQ 'CUBIC' THEN $
					extra = { CUBIC : -0.5 }

				;-----------------------------------------------------------
				; ROT does not recalulate the output size, the output is
				; always the same type and dimensions as the input to ROT.
				; Determine the correct type and sizes of the input, output,
				; and temporary holding array.  Put the input data into the
				; center of the temporary holding array and rotate it, then
				; copy out the rotated data into the final output array.
				; The temporary holding array is required to avoid any
				; clipping of the input data.
				;-----------------------------------------------------------
				sine		= SIN( rotationAngle * !DTOR )
				cosine		= COS( rotationAngle * !DTOR )
				XNewSize	= ROUND( XOldSize * ABS( cosine ) + YOldSize * ABS( sine ) )
				YNewSize	= ROUND( YOldSize * ABS( cosine ) + XOldSize * ABS( sine ) )
				XTmpSize	= MAX( [XOldSize,XNewSize] )
				YTmpSize	= MAX( [YOldSize,YNewSize] )
;print,'type = ', type
;help,imgStruct.blocks
				CASE type OF
					1:	tmpArr = BYTARR( XTmpSize, YTmpSize )
					2:	tmpArr = INTARR( XTmpSize, YTmpSize )
					3:	tmpArr = LONARR( XTmpSize, YTmpSize )
					4:	tmpArr = FLTARR( XTmpSize, YTmpSize )
					5:	tmpArr = DBLARR( XTmpSize, YTmpSize )
					6:	tmpArr = COMPLEXARR( XTmpSize, YTmpSize )
					9:	tmpArr = DCOMPLEXARR( XTmpSize, YTmpSize )
					12:	tmpArr = UINTARR( XTmpSize, YTmpSize )
					13:	tmpArr = ULONARR( XTmpSize, YTmpSize )
					14:	tmpArr = LON64ARR( XTmpSize, YTmpSize )
					15:	tmpArr = ULON64ARR( XTmpSize, YTmpSize )
					ELSE: BEGIN
						result = DIALOG_MESSAGE( 'Unrecognized data type! Converting to DOUBLE', /INFORMATION )
						tmpArr = DBLARR( XTmpSize, YTmpSize )
						convert2double = 1
					END
				ENDCASE
				IF convert2double THEN BEGIN
						FOR i = 0, n_blocks - 1 DO $
							rdata_ptrarr[i]	= PTR_NEW( DOUBLE( imgStruct.blocks[*,*,i] ) )
				ENDIF ELSE BEGIN
						FOR i = 0, n_blocks - 1 DO $
							rdata_ptrarr[i]	= PTR_NEW( imgStruct.blocks[*,*,i] )
				ENDELSE
;help,tmpArr

				xsOld	= (XTmpSize - XOldSize) / 2
				xeOld	= xsOld + XOldSize - 1
				ysOld	= (YTmpSize - YOldSize) / 2
				yeOld	= ysOld + YOldSize - 1
				xsNew	= (XTmpSize - XNewSize) / 2
				xeNew	= xsNew + XNewSize - 1
				ysNew	= (YTmpSize - YNewSize) / 2
				yeNew	= ysNew + YNewSize - 1
;print,'XTmpSize,YTmpSize = ',XTmpSize,YTmpSize
;print,'xsOld,xeOld,ysOld,yeOld = ',xsOld,xeOld,ysOld,yeOld
;print,'xsNew,xeNew,ysNew,yeNew = ',xsNew,xeNew,ysNew,yeNew
				FOR i = 0, n_blocks - 1 DO BEGIN
					; ROT cannot operate on a three dimensional array.
					tmpArr				= tmpArr * 0b
					tmpArr[xsOld:xeOld,ysOld:yeOld]	= (*(rdata_ptrarr[i]))[*,*]
					tmpArr				= ROT( tmpArr, rotationAngle, MISSING = 0, _EXTRA = extra )
					temp				= tmpArr[xsNew:xeNew,ysNew:yeNew]
					rdata_ptrarr[i]			= PTR_NEW( temp )
				ENDFOR
			END
		ENDCASE
	ENDIF ELSE BEGIN
		;----------------------------------------------------------------
		; rotationAngle was 0.0 so nothing happened.  Just return.
		;----------------------------------------------------------------
		RETURN, imgStruct
	ENDELSE

	;----------------------------------------------------------------
	; Prep some variables for rotation of the offsets:
	;	1. rotate the image array,
	;	2. rotation angle in radians,
	;	3. x and y dimensions of rotated data,
	;	4. difference in size between rotated and unrotated data,
	;	5. distance from [0,0] to offset point.
	;----------------------------------------------------------------
	rad_angle	= rotationAngle * !DTOR
	new_x_size	= N_ELEMENTS( (*(rdata_ptrarr[0]))[*,0] )
	new_y_size	= N_ELEMENTS( (*(rdata_ptrarr[0]))[0,*] )
	dif_x_size	= new_x_size - XOldSize
	dif_y_size	= new_y_size - YOldSize
	dist		= SQRT( imgStruct.offsetX ^ 2 + imgStruct.offsetY ^ 2 )
	n_offsets	= N_ELEMENTS( imgStruct.offsetX )
	new_x_offset	= LONARR( n_offsets )
	new_y_offset	= LONARR( n_offsets )

;	;----------------------------------------------------------------
;	; Rotate the offsets.  Account for places where X is zero and
;	; avoid dividing by zero.
;	;----------------------------------------------------------------
;	zeroX		= WHERE( imgStruct.offsetX EQ 0 )
;	nonZeroX	= WHERE( imgStruct.offsetX NE 0 )
;	IF zeroX[0] EQ -1 THEN BEGIN
;		new_x_offset	= dist * COS( ATAN( imgStruct.offsetY / imgStruct.offsetX ) - rad_angle )
;		new_y_offset	= dist * SIN( ATAN( imgStruct.offsetY / imgStruct.offsetX ) - rad_angle )
;	ENDIF ELSE BEGIN
;		new_x_offset	= imgStruct.offsetX
;		new_y_offset	= imgStruct.offsetY
;		IF nonZeroX[0] EQ -1 THEN BEGIN
;			IF imgStruct.offsetY[zeroX] NE 0 THEN BEGIN
;				maxATAN = !PI / 2.0
;				new_x_offset[zeroX] = dist[zeroX] * COS( maxATAN - rad_angle )
;				new_y_offset[zeroX] = dist[zeroX] * SIN( maxATAN - rad_angle )
;			ENDIF
;		ENDIF ELSE BEGIN
;			new_x_offset[nonZeroX]	= dist[nonZeroX] * $
;						COS( ATAN( imgStruct.offsetY[nonZeroX] / imgStruct.offsetX[nonZeroX] ) - rad_angle )
;			new_y_offset[nonZeroX]	= dist[nonZeroX] * $
;						SIN( ATAN( imgStruct.offsetY[nonZeroX] / imgStruct.offsetX[nonZeroX] ) - rad_angle )
;		ENDELSE
;	ENDELSE

	;----------------------------------------------------------------
	; Rotate the offsets, avoiding division by zero.
	;----------------------------------------------------------------
	FOR i = 0, n_offsets - 1 DO BEGIN
		IF imgStruct.offsetX[i] EQ 0 THEN				$
			atangent = !PI / 2.0					$
		ELSE								$
			atangent = ATAN( FLOAT( imgStruct.offsetY[i] ) / FLOAT( imgStruct.offsetX[i] ) )
		new_x_offset[i]	= dist[i] * COS( atangent - rad_angle )
		new_y_offset[i]	= dist[i] * SIN( atangent - rad_angle )
	ENDFOR

	IF MIN( new_x_offset ) LT 0 THEN $
		new_x_offset = new_x_offset + ABS( MIN( new_x_offset ) )
	IF MIN( new_y_offset ) LT 0 THEN $
		new_y_offset = new_y_offset + ABS( MIN( new_y_offset ) )

	IF MIN( new_x_offset ) LT 0 THEN $
		new_x_offset = new_x_offset + ABS( MIN( new_x_offset ) )
	IF MIN( new_y_offset ) LT 0 THEN $
		new_y_offset = new_y_offset + ABS( MIN( new_y_offset ) )

;	PRINT, ''
;	PRINT, '_.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._'
;	PRINT, '_.-^-._.-^-._.-^-._.-^-._.-^-.Diagnostics for rotation of offsets._.-^-._.-^-._.-^-._.-^-._'
;	PRINT, '_.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._'
;	HELP, imgStruct.blocks
;	HELP, *(rdata_ptrarr[0])
;	PRINT, 'rotationAngle						= ', rotationAngle
;	PRINT, 'dif_x_size, dif_y_size					= ', dif_x_size, ',', dif_y_size
;	PRINT, 'imgStruct.offsetX, imgStruct.offsetY			= ', imgStruct.offsetX, ',', imgStruct.offsetY
;	PRINT, 'SQRT( imgStruct.offsetX ^ 2 + imgStruct.offsetY ^ 2 )	= ', SQRT( imgStruct.offsetX ^ 2 + imgStruct.offsetY ^ 2 )
;	PRINT, 'ATAN( new_y_size / new_x_size )				= ', ATAN( new_y_size / new_x_size )
;	PRINT, 'rad_angle						= ', rad_angle
;	PRINT, 'rad_angle - ATAN( new_y_size / new_x_size )		= ', rad_angle - ATAN( new_y_size / new_x_size )
;	PRINT, 'COS( rad_angle - ATAN( new_y_size / new_x_size ) )	= ', COS( rad_angle - ATAN( new_y_size / new_x_size ) )
;	PRINT, 'SIN( rad_angle - ATAN( new_y_size / new_x_size ) )	= ', SIN( rad_angle - ATAN( new_y_size / new_x_size ) )
;	PRINT, 'imgStruct.offsetX, imgStruct.offsetY			= ', imgStruct.offsetX, ',', imgStruct.offsetY
;	PRINT, 'new_x_offset, new_y_offset				= ', new_x_offset, ',', new_y_offset
;	PRINT, 'ROUND( new_x_offset ), ROUND( new_y_offset )		= ', ROUND( new_x_offset), ',', ROUND( new_y_offset )
;	PRINT, '_.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._'
;	PRINT, '_.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._'
;	PRINT, '_.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._.-^-._'
;	PRINT, ''

	XTmpSize = N_ELEMENTS( (*(rdata_ptrarr[0]))[*,0] )
	YTmpSize = N_ELEMENTS( (*(rdata_ptrarr[0]))[0,*] )
	CASE type OF
		1:	tmpArr = BYTARR( XTmpSize, YTmpSize, n_blocks )
		2:	tmpArr = INTARR( XTmpSize, YTmpSize, n_blocks )
		3:	tmpArr = LONARR( XTmpSize, YTmpSize, n_blocks )
		4:	tmpArr = FLTARR( XTmpSize, YTmpSize, n_blocks )
		5:	tmpArr = DBLARR( XTmpSize, YTmpSize, n_blocks )
		6:	tmpArr = COMPLEXARR( XTmpSize, YTmpSize, n_blocks )
		9:	tmpArr = DCOMPLEXARR( XTmpSize, YTmpSize, n_blocks )
		12:	tmpArr = UINTARR( XTmpSize, YTmpSize, n_blocks )
		13:	tmpArr = ULONARR( XTmpSize, YTmpSize, n_blocks )
		14:	tmpArr = LON64ARR( XTmpSize, YTmpSize, n_blocks )
		15:	tmpArr = ULON64ARR( XTmpSize, YTmpSize, n_blocks )
	ENDCASE
	IF convert2double THEN $
		tmpArr		= DBLARR( XTmpSize, YTmpSize, n_blocks )

;help,tmpArr
;help,(*(rdata_ptrarr[0]))[*,*]

	FOR i = 0, n_blocks - 1 DO $
		tmpArr[*,*,i]	= (*(rdata_ptrarr[i]))[*,*]

	RETURN, {	block_numbers	: imgStruct.block_numbers,	$
			blocks		: tmpArr,			$
			offsetX		: ROUND( new_x_offset ),	$
			offsetY		: ROUND( new_y_offset ),	$
			resize_data_factor	: imgStruct.resize_data_factor		}
END
; rotate_data

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ create_proj_info_window_eh @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO create_proj_info_window_eh, event
	WIDGET_CONTROL, event.id, GET_UVALUE = widget_name

	CASE STRTRIM(STRUPCASE(widget_name),2) OF
		'DISMISS': BEGIN
			WIDGET_CONTROL, event.top, /DESTROY
			END
		ELSE:
	ENDCASE
END
; create_proj_info_window_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ create_proj_info_window @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO create_proj_info_window, win_base, infoPtr

	b	= WIDGET_BASE(								$
				GROUP_LEADER = win_base,				$
				/COLUMN,						$
				/BASE_ALIGN_CENTER,					$
				TITLE = 'projection information window' )
	b1	= WIDGET_BASE(								$
				b,							$
				/COLUMN,						$
				/BASE_ALIGN_CENTER,					$
				/FRAME )
	l1	= WIDGET_LABEL(								$
				b1,							$
				VALUE = 'Projection Information For Window ID:',	$
				/ALIGN_CENTER )
	l2	= WIDGET_LABEL(								$
				b1,							$
				VALUE = (*infoPtr).winTitle,				$
				/ALIGN_CENTER )
	l3	= WIDGET_LABEL(								$
				b,							$
				VALUE = 'Source Window IDs For Projected Data:',	$
				/ALIGN_CENTER )
	lst1	= WIDGET_LIST(								$
				b,							$
				/ALIGN_CENTER,						$
				VALUE = (*infoPtr).proj_info_strarr,			$
				SCR_XSIZE = 400,					$
				SCR_YSIZE = 200,					$
				UVALUE = 'list' )
	b2	= WIDGET_BASE(								$
				b,							$
				/ROW,							$
				/BASE_ALIGN_CENTER )
	btn1	= WIDGET_BUTTON(							$
				b2,							$
				VALUE = 'Hide',						$
				UVALUE = 'dismiss' )

	WIDGET_CONTROL, b, /REALIZE

	XMANAGER, 'proj_info_window', b, EVENT_HANDLER = 'create_proj_info_window_eh'


END
;create_proj_info_window

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@ get_new_projection_window @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION get_new_projection_window,					$
					lon,				$
					lat,				$
					imgPtrArr,			$
					wTitle,				$
					proj_info_strarr,		$
					tlbPtr,				$
					_Extra = extra

	prev_last_active_obj	= (*tlbPtr).lastActiveObj
	idx2use			= WHERE(PTR_VALID(imgPtrArr))
	whichChannels		= WHERE( idx2use GE 0 and idx2use LE 2, channelCount )
	IF channelCount EQ 0 THEN RETURN, -1L

	b			= WIDGET_BASE(					$
						/COLUMN,			$
						/TLB_SIZE_EVENTS,		$
						TITLE = wTitle,			$
						EVENT_PRO = 'misr_view_eh',	$
						KILL_NOTIFY = 'misr_kill_window' )

	initXsize		= 0
	initYsize		= 0

	FOR i = 0, N_ELEMENTS(idx2use) - 1 DO BEGIN
		IF (*(imgPtrArr[idx2use[i]]))->GetImageWidth() GT initXsize THEN BEGIN
			data_idx	= i
			initXsize	= (*(imgPtrArr[idx2use[i]]))->GetImageWidth()
			initYsize	= (*(imgPtrArr[idx2use[i]]))->GetImageHeight()
		ENDIF
	ENDFOR

	DEVICE, GET_SCREEN_SIZE = screenSize
	max_disp_x	= screenSize[0] - 100L ;account for any borders, etc.
	max_disp_y	= screenSize[1] - 100L ;account for any borders, etc.

	WHILE initXsize GT max_disp_x OR initYsize GT max_disp_y DO BEGIN
		initXsize	= initXsize / 2
		initYsize	= initYsize / 2
	ENDWHILE

	view_disp_x	= MAX( [ 384L, initXsize ] )
	view_disp_y	= MAX( [ 384L, initYsize ] )

	bmodes		= STRARR(2,2)
	bmodes[0,0]	= 'Projection Info'
	bmodes[0,1]	= 'projinfo'
	bmodes[1,0]	= 'Kill'
	bmodes[1,1]	= 'kill'

	IF (*tlbPtr).n_available_colors LE 256 THEN nchannels = 1 ELSE nchannels = 3

	length		= STRPOS(wTitle,' ')
	IF length EQ -1 THEN length = STRLEN( wTitle )
	title_prefix	= STRMID(wTitle,0,length)

	;===========================================================
	; min/max zooms are based upon zooming 9 levels in
	; both directions.  If the baseline zoom is 1.0 (100%),
	; then the next level of zooming is either 50% or 200%,
	; depending on whether the user is zooming in or out.  The
	; next eight levels for zoom in/zoom out are:
	;
	;			25%		or 400%
	;			12.5%		or 800%
	; 			6.25%		or 1600%
	;			3.125%		or 3200%
	;			1.5625% 	or 6400%
	;			0.78125%	or 12800%
 	;			0.390625%	or 25600%
	;			0.195312%	or 51200%
	;===========================================================
	mw = OBJ_NEW(								$
			'GEOREF_IMAGE',						$
				b,						$
				LON_IMAGE = lon,				$
				LAT_IMAGE = lat,				$
				DATA_PTR = imgPtrArr,				$
				XSIZE = initXsize,				$
				YSIZE = initYsize,				$
				VIEW_SIZE_X = view_disp_x,			$
				VIEW_SIZE_Y = view_disp_y,			$
				MIN_XSIZE = 500,				$
				MIN_YSIZE = 500,				$
				BUTTON_MODES = bmodes,				$
				N_CHANNELS = nchannels,				$
				TITLE_PREFIX = title_prefix,			$
				DATA_IDX = data_idx,				$
				/NO_SELECT,					$
				/USE_UPPER_LEFT_ORIGIN,				$
				MIN_ZOOM_PCT = 0.195312,			$
				MAX_ZOOM_PCT = 51200.0,				$
				/REPORT_GLOBAL_COORDINATES,			$
				_EXTRA = extra )

	IF NOT OBJ_VALID(mw) THEN RETURN, (-1)

	WIDGET_CONTROL, b, /REALIZE
	WIDGET_CONTROL, b, TLB_GET_SIZE = xydims

	(*tlbPtr).lastActiveObj    = mw

	mw->SetMinXDrawSize, xydims[0]-(xydims[0]-view_disp_x)
	mw->SetMinYDrawSize, xydims[1]-(xydims[1]-view_disp_y)

	infoPtr = PTR_NEW(									$
				{								$
					minx		: view_disp_x,				$
					miny		: view_disp_y,				$
					minBaseXYdims	: xydims,				$
					objxydims	: [initXsize,initYsize],		$
					obj		: mw,					$
					baseX2objX	: FLOAT(xydims[0])/FLOAT(initXsize),	$
					baseY2objY	: FLOAT(xydims[1])/FLOAT(initYsize),	$
					tlbPtr		: tlbPtr,				$
					proj_info_strarr: proj_info_strarr,			$
					winTitle 	: wTitle }, /NO_COPY )
	WIDGET_CONTROL, b, SET_UVALUE = infoPtr
	RETURN, b
END
; get_new_projection_window

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@ get_new_projection_window_8bit @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION get_new_projection_window_8bit, DATA_WINDOW_STRUCT = dws

   ;---------------------------------------------------------
   ; *********** Display loop for data window(s) ************
   ;---------------------------------------------------------
   ;------------------------------------------------------
   ; Determine number of currently open windows (if any)
   ;------------------------------------------------------
   IF PTR_VALID((*(dws.infoPtr)).mwPtr) THEN BEGIN
      n_open_img_win         = 0
      tmpPtrArr              = *((*(dws.infoPtr)).mwPtr)
      FOR i = 0, N_ELEMENTS(tmpPtrArr) - 1 DO     $
         IF WIDGET_INFO(*(tmpPtrArr[i]),/VALID_ID) THEN n_open_img_win = n_open_img_win + 1
   ENDIF ELSE BEGIN
      n_open_img_win         = 0
   ENDELSE

   ;------------------------------------------------------
   ; Grab display mode
   ;------------------------------------------------------
   display_mode           = (*(dws.infoPtr)).current_display_mode

   ;------------------------------------------------------
   ; Check for existence of location window
   ;------------------------------------------------------
   location_window_exists = 0

   IF (*(dws.infoPtr)).locationMapBase GE 0L OR                            $
      WIDGET_INFO((*(dws.infoPtr)).locationMapBase, /VALID_ID) THEN        $
         location_window_exists = 1

   ;------------------------------------------------------
   ; at least one image window or location window open,
   ; shared colormap
   ;------------------------------------------------------
   IF ( n_open_img_win GT 0 OR location_window_exists ) AND display_mode EQ 'commoncolormap' THEN BEGIN
      trans = *((*(dws.infoPtr)).translationPtr)
      r_vec = *((*(dws.infoPtr)).rvec_ptr)
      g_vec = *((*(dws.infoPtr)).gvec_ptr)
      b_vec = *((*(dws.infoPtr)).bvec_ptr)
      win   = get_new_projection_window(			$
      				dws.lon,          		$
				dws.lat,          		$
				dws.imgPtrArr,    		$
				dws.winTitle,     		$
				dws.win_title_arr,     		$
				dws.infoPtr,      		$
				USE_TRANSLATION = { trans:trans, r_vec:r_vec, g_vec:g_vec, b_vec:b_vec } )
   ENDIF ELSE BEGIN

      IF display_mode EQ 'commoncolormap' THEN			$
         win   = get_new_projection_window(			$
         			dws.lon,			$
				dws.lat,			$
				dws.imgPtrArr,			$
				dws.winTitle,			$
				dws.win_title_arr,     		$
				dws.infoPtr,			$
				USE_TRANSLATION = { trans:0, r_vec:0, g_vec:0, b_vec:0 } ) $
      ELSE							$
         win   = get_new_projection_window(			$
         			dws.lon,			$
				dws.lat,			$
				dws.imgPtrArr,			$
				dws.winTitle,			$
				dws.win_title_arr,     		$
				dws.infoPtr,			$
				/SET_TRANSLATION )

      IF display_mode EQ 'commoncolormap' AND n_open_img_win LE 0 and win GE 0 THEN BEGIN
         WIDGET_CONTROL, win, GET_UVALUE = winPtr

	 IF PTR_VALID((*(dws.infoPtr)).translationPtr) THEN PTR_FREE, (*(dws.infoPtr)).translationPtr

	 tmp_trans = *((*winPtr).obj->EightBit_GetTranslationPtr())
	 (*(dws.infoPtr)).translationPtr = PTR_NEW(tmp_trans,/NO_COPY)

	 IF PTR_VALID((*(dws.infoPtr)).rvec_ptr) THEN PTR_FREE, (*(dws.infoPtr)).rvec_ptr
	 tmp_rvec = *((*winPtr).obj->EightBit_GetColorVectorPtr( /RED ))
	 (*(dws.infoPtr)).rvec_ptr = PTR_NEW(tmp_rvec,/NO_COPY)

	 IF PTR_VALID((*(dws.infoPtr)).gvec_ptr) THEN PTR_FREE, (*(dws.infoPtr)).gvec_ptr
	 tmp_gvec = *((*winPtr).obj->EightBit_GetColorVectorPtr( /GRN ))
	 (*(dws.infoPtr)).gvec_ptr = PTR_NEW(tmp_gvec,/NO_COPY)

	 IF PTR_VALID((*(dws.infoPtr)).bvec_ptr) THEN PTR_FREE, (*(dws.infoPtr)).bvec_ptr
	 tmp_bvec = *((*winPtr).obj->EightBit_GetColorVectorPtr( /BLU ))
	 (*(dws.infoPtr)).bvec_ptr = PTR_NEW(tmp_bvec,/NO_COPY)
      ENDIF
   ENDELSE

   RETURN, win
END
; get_new_projection_window_8bit

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ get_new_window @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION get_new_window, lon, lat, imgPtrArr, wTitle, tlbPtr,			$
		data_id_str, data_source_fname,		$
		lon_lat_source_file, start_block, n_blocks, in_resolution,	$
		anglesPtr, _Extra = extra

;for i = 0, 5 do begin
;print,'plane ', i
;if ptr_valid(imgPtrArr[i]) then help, *(imgPtrArr[i])
;endfor
	prev_last_active_obj	= (*tlbPtr).lastActiveObj
	;----------------------------
	; basic error catch mechanism
	;----------------------------
	routineName	= '----- get_new_window -----'
	returnMsg	= 'Returning...'
	CATCH, errorStatus
	IF errorStatus NE 0 THEN BEGIN
		eIndex	= 'Error Index:' +				$
			STRTRIM( STRING(errorStatus), 2 )
		eMsg	= 'Error Message:' + !ERR_STRING
		res	= DIALOG_MESSAGE(				$
				[ routineName,				$
				  eIndex,				$
				  eMsg,					$
				  returnMsg ],				$
				/ERROR )
		(*tlbPtr).lastActiveObj	= prev_last_active_obj
		IF WIDGET_INFO( b, /VALID_ID ) THEN WIDGET_CONTROL, b, /DESTROY
		IF OBJ_VALID( mw ) THEN OBJ_DESTROY, mw
		RETURN, (-1)
	ENDIF

   idx2use       = WHERE(PTR_VALID(imgPtrArr))
   whichChannels = WHERE( idx2use GE 0 and idx2use LE 2, channelCount )
   IF channelCount EQ 0 THEN RETURN, -1L

   b = WIDGET_BASE( /COLUMN, /TLB_SIZE_EVENTS, TITLE = wTitle, $
	            EVENT_PRO = 'misr_view_eh',                $
	            KILL_NOTIFY = 'misr_kill_window' )

   initXsize = 0
   initYsize = 0
   FOR i = 0, N_ELEMENTS(idx2use) - 1 DO BEGIN
      IF (*(imgPtrArr[idx2use[i]]))->GetImageWidth() GT initXsize THEN BEGIN
;;;ckt,oct1999      IF (SIZE(*(imgPtrArr[idx2use[i]])))[1] GT initXsize THEN BEGIN
         data_idx  = i
         initXsize = (*(imgPtrArr[idx2use[i]]))->GetImageWidth()
         initYsize = (*(imgPtrArr[idx2use[i]]))->GetImageHeight()
      ENDIF
   ENDFOR
;print,'initXsize,initYsize = ',   initXsize,initYsize

    DEVICE, GET_SCREEN_SIZE = screenSize
    max_disp_x	= screenSize[0] - 100L ;account for any borders, etc.
    max_disp_y	= screenSize[1] - 100L ;account for any borders, etc.

    WHILE initXsize GT max_disp_x OR initYsize GT max_disp_y DO BEGIN
    	initXsize	= initXsize / 2
    	initYsize	= initYsize / 2
    ENDWHILE

    view_disp_x	= MAX( [ 384L, initXsize ] )
    view_disp_y	= MAX( [ 384L, initYsize ] )

;   WHILE initXsize LT 256L OR initYsize LT 256L DO BEGIN
;      initXsize = initXsize * 2
;      initYsize = initYsize * 2
;   ENDWHILE

;   WHILE initXsize GT 800L OR initYsize GT 800L DO BEGIN
;      initXsize = MAX( [ 384L, initXsize / 2 ] )
;      initYsize = MAX( [ 384L, initYsize / 2 ] )
;   ENDWHILE

   bmodes = STRARR(1,2)
   bmodes[0,0] = 'Kill'
   bmodes[0,1] = 'kill'

;;;;;;commented out 9/3/98, ckt   IF channelCount GT 1 THEN nchannels = 3 ELSE nchannels = 1

   ;--------------------------------------------------------------------
   ; In order to get the correct value for !D.N_COLORS it is necessary
   ; to either open a window on screen (or pixmap) or to run HELP,/DEVICE.
   ; Since opening a window or pixmap is undesireable in this case, the
   ; HELP,/DEVICE is used.  Since HELP will do screen paging on Sun
   ; systems when the results are lengthly, it is necessary to send the
   ; output results to a variable.  Since HELP,/DEVICE is only being
   ; used to have X set the correct values for !D.N_COLORS, the output
   ; results are not needed and therefore not printed.
   ;--------------------------------------------------------------------
   ; revised, ckt, 8/18/98
   ;
   IF (*tlbPtr).n_available_colors LE 256 THEN nchannels = 1 ELSE nchannels = 3

   length = STRPOS(wTitle,' ')
   IF length EQ -1 THEN length = STRLEN( wTitle )
   title_prefix = STRMID(wTitle,0,length)

   IF NOT OBJ_VALID(lon) OR NOT OBJ_VALID(lat) THEN BEGIN
      mw = OBJ_NEW( 'MISR_GEOREF_IMAGE',                           $
                    b,                                             $
                    DATA_PTR = imgPtrArr,                          $
		    DATA_DESCRIPTION_STRING = data_id_str,	   $
		    DATA_SOURCE_FILENAME = data_source_fname,	   $
		    LON_SOURCE_FILENAME = lon_lat_source_file,	   $
		    LAT_SOURCE_FILENAME = lon_lat_source_file,	   $
		    START_BLOCK = start_block,			   $
		    N_BLOCKS = n_blocks,			   $
                    XSIZE = initXsize,                             $
                    YSIZE = initYsize,                             $
                    VIEW_SIZE_X = view_disp_x,			   $
                    VIEW_SIZE_Y = view_disp_y,			   $
		    MIN_XSIZE = 500,				   $
		    MIN_YSIZE = 500,				   $
                    BUTTON_MODES = bmodes,                         $
                    N_CHANNELS = nchannels,                        $
                    TITLE_PREFIX = title_prefix,                   $
                    DATA_IDX = data_idx,			   $
		    ANGLESPTR = anglesPtr,			   $
		    IN_RESOLUTION = in_resolution,		   $
                    /NO_SELECT,                                    $
		    /USE_UPPER_LEFT_ORIGIN,			   $
 		    MIN_ZOOM_PCT = 0.195312,			   $
		    MAX_ZOOM_PCT = 51200.0,			   $
                   _EXTRA = extra )
   ENDIF ELSE BEGIN
      mw = OBJ_NEW( 'MISR_GEOREF_IMAGE',                           $
                    b,                                             $
                    LON_IMAGE = lon,                               $
                    LAT_IMAGE = lat,                               $
                    DATA_PTR = imgPtrArr,                          $
		    DATA_DESCRIPTION_STRING = data_id_str,	   $
		    DATA_SOURCE_FILENAME = data_source_fname,	   $
		    LON_SOURCE_FILENAME = lon_lat_source_file,	   $
		    LAT_SOURCE_FILENAME = lon_lat_source_file,	   $
		    START_BLOCK = start_block,			   $
		    N_BLOCKS = n_blocks,			   $
                    XSIZE = initXsize,                             $
                    YSIZE = initYsize,                             $
                    VIEW_SIZE_X = view_disp_x,			   $
                    VIEW_SIZE_Y = view_disp_y,			   $
		    MIN_XSIZE = 500,				   $
		    MIN_YSIZE = 500,				   $
                    BUTTON_MODES = bmodes,                         $
                    N_CHANNELS = nchannels,                        $
                    TITLE_PREFIX = title_prefix,                   $
                    DATA_IDX = data_idx,			   $
		    ANGLESPTR = anglesPtr,			   $
		    IN_RESOLUTION = in_resolution,		   $
                    /NO_SELECT,                                    $
		    /USE_UPPER_LEFT_ORIGIN,			   $
 		    MIN_ZOOM_PCT = 0.195312,			   $
		    MAX_ZOOM_PCT = 51200.0,			   $
                    _EXTRA = extra )
   ENDELSE

   IF NOT OBJ_VALID(mw) THEN RETURN, (-1)

   WIDGET_CONTROL, b, /REALIZE
   WIDGET_CONTROL, b, TLB_GET_SIZE = xydims

  (*tlbPtr).lastActiveObj    = mw

   mw->SetMinXDrawSize, xydims[0]-(xydims[0]-view_disp_x)
   mw->SetMinYDrawSize, xydims[1]-(xydims[1]-view_disp_y)

  infoPtr = PTR_NEW(                                                       $
                     { minx          :view_disp_x,                         $
                       miny          :view_disp_y,                         $
                       minBaseXYdims :xydims,                              $
                       objxydims     :[initXsize,initYsize],               $
                       obj           :mw,                                  $
                       baseX2objX    :float(xydims[0])/FLOAT(initXsize),   $
                       baseY2objY    :float(xydims[1])/FLOAT(initYsize),   $
                       tlbPtr        :tlbPtr,                              $
                       winTitle      :wTitle },                            $
                       /NO_COPY )
 WIDGET_CONTROL, b, SET_UVALUE = infoPtr
 RETURN, b
END
; get_new_window

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ get_new_window_8bit @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION get_new_window_8bit, LOCATION_WINDOW_STRUCT = lws, $
                              DATA_WINDOW_STRUCT = dws

   ;---------------------------------------------------------
   ; *********** Display loop for location window ***********
   ;---------------------------------------------------------
   IF KEYWORD_SET(lws) THEN BEGIN
      ;------------------------------------------------------
      ; Determine number of currently open windows (if any)
      ;------------------------------------------------------
      IF PTR_VALID((*(lws.infoPtr)).mwPtr) THEN BEGIN
         n_open_img_win         = 0
         tmpPtrArr              = *((*(lws.infoPtr)).mwPtr)
         FOR i = 0, N_ELEMENTS(tmpPtrArr) - 1 DO     $
            IF WIDGET_INFO(*(tmpPtrArr[i]),/VALID_ID) THEN n_open_img_win = n_open_img_win + 1
      ENDIF ELSE BEGIN
         n_open_img_win         = 0
      ENDELSE
      ;------------------------------------------------------
      ; Grab display mode
      ;------------------------------------------------------
      display_mode   = (*(lws.infoPtr)).current_display_mode
      ;------------------------------------------------------
      ; CASE 1: Common color map mode, open windows exist;
      ; this implies that there is already an established
      ; color table and translation table
      ;------------------------------------------------------
      IF display_mode EQ 'commoncolormap' AND n_open_img_win GT 0 THEN BEGIN
         ;---------------------------------------------------
         ; Grab the current translation table
         ;---------------------------------------------------
         trans = *((*(lws.infoPtr)).translationPtr)
         r_vec = *((*(lws.infoPtr)).rvec_ptr)
         g_vec = *((*(lws.infoPtr)).gvec_ptr)
         b_vec = *((*(lws.infoPtr)).bvec_ptr)
         ;---------------------------------------------------
         ; Make a call to get_location_window with the
         ; USE_TRANSLATION keyword
         ;---------------------------------------------------
         win   = get_location_window( lws.id,                   $
                                      lws.infoPtr,              $
                                      USE_TRANSLATION = { trans:trans, r_vec:r_vec, g_vec:g_vec, b_vec:b_vec } )
      ;------------------------------------------------------
      ; CASE 2: Common color map mode, no open windows exist
      ; OR private color map mode; in either case, use the
      ; SET_TRANSLATION keyword which establishes the current
      ; color map and translation table to use
      ;------------------------------------------------------
      ENDIF ELSE BEGIN
      IF display_mode EQ 'commoncolormap' THEN                                                   $
         win   = get_location_window( lws.id,                                                    $
                                      lws.infoPtr,                                               $
                                      USE_TRANSLATION = { trans:0, r_vec:0, g_vec:0, b_vec:0 } ) $
      ELSE                                                                                       $
         win   = get_location_window( lws.id,                                                    $
                                      lws.infoPtr,                                               $
                                      /SET_TRANSLATION )

         ;---------------------------------------------------
         ; CASE 2a: Common color map mode, no open windows
         ; exist; save the translation table in a variable
         ; available to all objects
         ;---------------------------------------------------
         IF display_mode EQ 'commoncolormap' AND n_open_img_win LE 0 THEN BEGIN
	    WIDGET_CONTROL, win, GET_UVALUE = winPtr

	    IF PTR_VALID((*(lws.infoPtr)).translationPtr) THEN PTR_FREE, (*(lws.infoPtr)).translationPtr
	    tmp_trans = *((*winPtr).obj->EightBit_GetTranslationPtr())
	    (*(lws.infoPtr)).translationPtr = PTR_NEW(tmp_trans,/NO_COPY)

	    IF PTR_VALID((*(lws.infoPtr)).rvec_ptr) THEN PTR_FREE, (*(lws.infoPtr)).rvec_ptr
	    tmp_rvec = *((*winPtr).obj->EightBit_GetColorVectorPtr( /RED ))
	    (*(lws.infoPtr)).rvec_ptr = PTR_NEW(tmp_rvec,/NO_COPY)

	    IF PTR_VALID((*(lws.infoPtr)).gvec_ptr) THEN PTR_FREE, (*(lws.infoPtr)).gvec_ptr
	    tmp_gvec = *((*winPtr).obj->EightBit_GetColorVectorPtr( /GRN ))
	    (*(lws.infoPtr)).gvec_ptr = PTR_NEW(tmp_gvec,/NO_COPY)

	    IF PTR_VALID((*(lws.infoPtr)).bvec_ptr) THEN PTR_FREE, (*(lws.infoPtr)).bvec_ptr
	    tmp_bvec = *((*winPtr).obj->EightBit_GetColorVectorPtr( /BLU ))
	    (*(lws.infoPtr)).bvec_ptr = PTR_NEW(tmp_bvec,/NO_COPY)
         ENDIF
      ENDELSE
      RETURN, win
   ENDIF

   ;---------------------------------------------------------
   ; *********** Display loop for data window(s) ************
   ;---------------------------------------------------------
   ;------------------------------------------------------
   ; Determine number of currently open windows (if any)
   ;------------------------------------------------------
   IF PTR_VALID((*(dws.infoPtr)).mwPtr) THEN BEGIN
      n_open_img_win         = 0
      tmpPtrArr              = *((*(dws.infoPtr)).mwPtr)
      FOR i = 0, N_ELEMENTS(tmpPtrArr) - 1 DO     $
         IF WIDGET_INFO(*(tmpPtrArr[i]),/VALID_ID) THEN n_open_img_win = n_open_img_win + 1
   ENDIF ELSE BEGIN
      n_open_img_win         = 0
   ENDELSE

   ;------------------------------------------------------
   ; Grab display mode
   ;------------------------------------------------------
   display_mode           = (*(dws.infoPtr)).current_display_mode

   ;------------------------------------------------------
   ; Check for existence of location window
   ;------------------------------------------------------
   location_window_exists = 0

   IF (*(dws.infoPtr)).locationMapBase GE 0L OR                            $
      WIDGET_INFO((*(dws.infoPtr)).locationMapBase, /VALID_ID) THEN        $
         location_window_exists = 1

   ;------------------------------------------------------
   ; at least one image window or location window open,
   ; shared colormap
   ;------------------------------------------------------
   IF ( n_open_img_win GT 0 OR location_window_exists ) AND display_mode EQ 'commoncolormap' THEN BEGIN
      trans = *((*(dws.infoPtr)).translationPtr)
      r_vec = *((*(dws.infoPtr)).rvec_ptr)
      g_vec = *((*(dws.infoPtr)).gvec_ptr)
      b_vec = *((*(dws.infoPtr)).bvec_ptr)
      win   = get_new_window(	dws.lon,          		$
				dws.lat,          		$
				dws.imgPtrArr,    		$
				dws.winTitle,     		$
				dws.infoPtr,      		$
				dws.data_id_str,		$
				dws.data_source_fname,		$
				dws.lon_lat_source_file,	$
				dws.start_block,		$
				dws.n_blocks,			$
				dws.in_resolution,		$
				dws.anglesPtr,			$
				USE_TRANSLATION = { trans:trans, r_vec:r_vec, g_vec:g_vec, b_vec:b_vec } )
   ENDIF ELSE BEGIN

      IF display_mode EQ 'commoncolormap' THEN			$
         win   = get_new_window(dws.lon,			$
				dws.lat,			$
				dws.imgPtrArr,			$
				dws.winTitle,			$
				dws.infoPtr,			$
				dws.data_id_str,		$
				dws.data_source_fname,		$
				dws.lon_lat_source_file,	$
				dws.start_block,		$
				dws.n_blocks,			$
				dws.in_resolution,		$
				dws.anglesPtr,			$
				USE_TRANSLATION = { trans:0, r_vec:0, g_vec:0, b_vec:0 } ) $
      ELSE							$
         win   = get_new_window(dws.lon,			$
				dws.lat,			$
				dws.imgPtrArr,			$
				dws.winTitle,			$
				dws.infoPtr,			$
				dws.data_id_str,		$
				dws.data_source_fname,		$
				dws.lon_lat_source_file,	$
				dws.start_block,		$
				dws.n_blocks,			$
				dws.in_resolution,		$
				dws.anglesPtr,			$
				/SET_TRANSLATION )

      IF display_mode EQ 'commoncolormap' AND n_open_img_win LE 0 and win GE 0 THEN BEGIN
         WIDGET_CONTROL, win, GET_UVALUE = winPtr

	 IF PTR_VALID((*(dws.infoPtr)).translationPtr) THEN PTR_FREE, (*(dws.infoPtr)).translationPtr

	 tmp_trans = *((*winPtr).obj->EightBit_GetTranslationPtr())
	 (*(dws.infoPtr)).translationPtr = PTR_NEW(tmp_trans,/NO_COPY)

	 IF PTR_VALID((*(dws.infoPtr)).rvec_ptr) THEN PTR_FREE, (*(dws.infoPtr)).rvec_ptr
	 tmp_rvec = *((*winPtr).obj->EightBit_GetColorVectorPtr( /RED ))
	 (*(dws.infoPtr)).rvec_ptr = PTR_NEW(tmp_rvec,/NO_COPY)

	 IF PTR_VALID((*(dws.infoPtr)).gvec_ptr) THEN PTR_FREE, (*(dws.infoPtr)).gvec_ptr
	 tmp_gvec = *((*winPtr).obj->EightBit_GetColorVectorPtr( /GRN ))
	 (*(dws.infoPtr)).gvec_ptr = PTR_NEW(tmp_gvec,/NO_COPY)

	 IF PTR_VALID((*(dws.infoPtr)).bvec_ptr) THEN PTR_FREE, (*(dws.infoPtr)).bvec_ptr
	 tmp_bvec = *((*winPtr).obj->EightBit_GetColorVectorPtr( /BLU ))
	 (*(dws.infoPtr)).bvec_ptr = PTR_NEW(tmp_bvec,/NO_COPY)
      ENDIF
   ENDELSE

   RETURN, win
END
; get_new_window_8bit

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ EightBit_ApplyColorMap @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION EightBit_ApplyColorMap, infoPtr, PRIVATE_CMAP = private_cmap
   valid_obj_arr        = OBJ_VALID()
   IF TOTAL(OBJ_VALID(valid_obj_arr)) LE 0 THEN BEGIN
	result8bit	= 0
	RETURN, result8bit
   ENDIF

   georef_cnt = 0
   georef_idx = LONARR(N_ELEMENTS(valid_obj_arr))
   FOR i = 0, N_ELEMENTS(valid_obj_arr) - 1 DO BEGIN
      IF STRPOS( STRUPCASE(OBJ_CLASS(valid_obj_arr[i])), 'GEOREF_IMAGE' ) GE 0 THEN BEGIN
         georef_idx[georef_cnt] = i
         georef_cnt             = georef_cnt + 1
      ENDIF
   ENDFOR

   ;-----------------------------------------------------------
   ; If there are no GEOREF_IMAGE objects on the screen,
   ; there is no need to do anything
   ;-----------------------------------------------------------
   IF georef_cnt LE 0 THEN BEGIN
	result8bit	= 0
	RETURN, result8bit
   ENDIF

   georef_idx     = georef_idx[0:georef_cnt - 1]

;   georef_idx     = WHERE(OBJ_CLASS(valid_obj_arr) EQ 'GEOREF_IMAGE', georef_cnt)
   georef_obj_arr = valid_obj_arr[georef_idx]

   IF KEYWORD_SET(private_cmap) THEN BEGIN
;print,''
;print,'KEYWORD_SET(private_cmap) = true'
;print,''
	FOR i = 0, N_ELEMENTS(georef_obj_arr)-1 DO $
		(georef_obj_arr[i])->DisplayData, /SET_TRANSLATION
		result8bit	= 1
	RETURN, result8bit
   ENDIF

   obj_titles = STRARR(georef_cnt)
   FOR i = 0, N_ELEMENTS(georef_obj_arr)-1 DO BEGIN
      obj_parent_base = (georef_obj_arr[i])->GetParentBase()
      WIDGET_CONTROL, obj_parent_base, GET_UVALUE = basePtr
      obj_titles[i]   = (*basePtr).winTitle
   ENDFOR

   selected_idx   = (-1)

   t1             = 'Shared Colormap Source Selection'
   t2             = 'Select One of The Image Windows For Source Colormap'

   selected_idx   = ReturnListSelectionIdx( WINDOW_TITLE = t1,            $
                                            LIST_TITLE = t2,              $
                                            LIST_CONTENTS = obj_titles,   $
                                            GROUP_LEADER = WIDGET_BASE(), $
                                            /NO_CANCEL )
;print,'selected_idx = ',selected_idx
   IF selected_idx LT 0 THEN BEGIN
	result		= DIALOG_MESSAGE( 'No change made.', /INFORMATION )
	result8bit	= 0
	RETURN, result8bit
   ENDIF
   selectedObj    = georef_obj_arr[selected_idx]

   selectedObj->DisplayData, /SET_TRANSLATION, /SHARED_MAP

   r_vec_ptr      = selectedObj->EightBit_GetColorVectorPtr(/RED)
   g_vec_ptr      = selectedObj->EightBit_GetColorVectorPtr(/GRN)
   b_vec_ptr      = selectedObj->EightBit_GetColorVectorPtr(/BLU)
   trans_vec_ptr  = selectedObj->EightBit_GetTranslationPtr()

   idx = WHERE(georef_obj_arr NE selectedObj, count)
   IF count GT 0 THEN BEGIN
	georef_obj_arr = georef_obj_arr[idx]

	FOR i = 0, N_ELEMENTS(georef_obj_arr)-1 DO						$
		(georef_obj_arr[i])->DisplayData, USE_TRANSLATION = { trans:*(trans_vec_ptr),	$
                                                            r_vec:*(r_vec_ptr),			$
                                                            g_vec:*(g_vec_ptr),			$
                                                            b_vec:*(b_vec_ptr) }
   ENDIF

   ;-----------------------------------------------------------
   ; Store the translation pointer for future use
   ;----------------------------------------------------------
   IF PTR_VALID((*infoPtr).translationPtr) THEN PTR_FREE, (*infoPtr).translationPtr
   tmp_trans = *trans_vec_ptr
   (*infoPtr).translationPtr = PTR_NEW(tmp_trans,/NO_COPY)

   IF PTR_VALID((*infoPtr).rvec_ptr) THEN PTR_FREE, (*infoPtr).rvec_ptr
   tmp_rvec = *r_vec_ptr
   (*infoPtr).rvec_ptr = PTR_NEW(tmp_rvec,/NO_COPY)

   IF PTR_VALID((*infoPtr).gvec_ptr) THEN PTR_FREE, (*infoPtr).gvec_ptr
   tmp_gvec = *g_vec_ptr
   (*infoPtr).gvec_ptr = PTR_NEW(tmp_gvec,/NO_COPY)

   IF PTR_VALID((*infoPtr).bvec_ptr) THEN PTR_FREE, (*infoPtr).bvec_ptr
   tmp_bvec = *b_vec_ptr
   (*infoPtr).bvec_ptr = PTR_NEW(tmp_bvec,/NO_COPY)

	result8bit	= 1
	RETURN, result8bit

END
; EightBit_ApplyColorMap


;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@ show_misr_view_preferences_eh @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO show_misr_view_preferences_eh, event
	COMMON MISRVIEWDATA, set_font, default_transform_directory

	WIDGET_CONTROL, event.id, GET_UVALUE = widget_name
	widget_name	= STRUPCASE(STRTRIM(widget_name,2))
	WIDGET_CONTROL, event.top, GET_UVALUE = info_struct

	CASE widget_name OF
		'DISMISS': WIDGET_CONTROL, event.top, /DESTROY
		'CHANGETRANSFORMDIRECTORY': BEGIN
			title	= 'Please specify a new directory for the file misr_view_DEFAULT_TRANSFORMS'
;ckt,apr2001			dummy_filename	= DIALOG_PICKFILE(		$
;ckt,apr2001							TITLE = title,	$
;ckt,apr2001							GET_PATH = p )
			dummy_filename	= dialog_pickfile_wrapper(		$
							/DIR_ONLY,		$
							TITLE = title,		$
							GET_PATH = p )
			p	= STRTRIM(p,2)
			IF p NE '' AND STRMID(p,STRLEN(p)-1,1) NE GetDirectoryDivider() THEN	$
					p	= p + GetDirectoryDivider()
			default_transform_directory	= p
			END
		ELSE:
	ENDCASE
	IF WIDGET_INFO( event.top, /VALID_ID ) THEN WIDGET_CONTROL, event.top, SET_UVALUE = info_struct
END
; show_misr_view_preferences_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@ show_misr_view_preferences @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO show_misr_view_preferences, group_leader

	COMMON MISRVIEWDATA, set_font, default_transform_directory

	tlb		= WIDGET_BASE(					$
				/COLUMN,				$
				GROUP_LEADER = group_leader,		$
				/BASE_ALIGN_CENTER,			$
				TITLE = 'misr_view Preferences' )
	sub_base1	= WIDGET_BASE(					$
				tlb,					$
				/COLUMN,				$
				/FRAME,					$
				/ALIGN_CENTER,				$
				/BASE_ALIGN_CENTER )
	label1_text	= 'Location of misr_view_DEFAULT_TRANSFORMS'
	label1		= WIDGET_LABEL(					$
				sub_base1,				$
				FONT = GetCorrectFont('courier2bold'),	$
				VALUE = label1_text )
	label2		= WIDGET_LABEL(					$
				sub_base1,				$
				FONT = GetCorrectFont('courier2bold'),	$
				/DYNAMIC_RESIZE,			$
				VALUE = default_transform_directory )
	btn1		= WIDGET_BUTTON(				$
				sub_base1,				$
				VALUE = 'Change...',			$
				FONT = GetCorrectFont('courier2bold'),	$
				UVALUE = 'changetransformdirectory' )
	dismiss_btn	= WIDGET_BUTTON(				$
				tlb,					$
				VALUE = 'Dismiss',			$
				FONT = GetCorrectFont('courier2bold'),	$
				UVALUE = 'dismiss' )

	WIDGET_CONTROL, tlb, /REALIZE

	WIDGET_CONTROL, tlb, SET_UVALUE = {				$
				default_transform_label	: label2 }

	XMANAGER, 'Show misr_view Preferences', tlb, EVENT_HANDLER = 'show_misr_view_preferences_eh'

END
; show_misr_view_preferences

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ misr_view_eh @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO misr_view_eh, event

	;error code goes here.  "recall_problem" is temprary name for error code.
	recall_problem = 0
	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
;	routine_name	= '========== misr_view_eh =========='
;	CATCH, error_status
;	IF error_status NE 0 THEN BEGIN
;		e_msg	= [											$
;				routine_name,									$
;				'Error Index: ' + STRTRIM( error_status, 2 ),					$
;				'Error Message: ' + !ERR_STRING,						$
;				'',										$
;				'Suggestion: If attempting to read or write a file, then check permissions.',	$
;				'',										$
;				'Returning...' ]
;		result	= DIALOG_MESSAGE( e_msg, /ERROR )
;		IF recall_problem THEN BEGIN
;			WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
;			WIDGET_CONTROL, event.top, SENSITIVE=1
;			WIDGET_CONTROL, (*infoPtr).tlb, SENSITIVE=1
;		ENDIF
;		RETURN
;	ENDIF


	whatWidget	= TAG_NAMES(event, /STRUCTURE_NAME)

	CASE whatWidget OF
		;===============================================================
		; WIDGET_BUTTON
		; WIDGET_BUTTON
		; WIDGET_BUTTON
		; WIDGET_BUTTON
		; WIDGET_BUTTON
		;===============================================================
		'WIDGET_BUTTON': BEGIN
			WIDGET_CONTROL, event.id, GET_UVALUE = whatButton
			CASE whatButton OF
				;===============================================
				; WIDGET_BUTTON: quit
				; WIDGET_BUTTON: quit
				; WIDGET_BUTTON: quit
				; WIDGET_BUTTON: quit
				; WIDGET_BUTTON: quit
				;===============================================
				'quit': BEGIN

					res	= DIALOG_MESSAGE( 'Exit misr_view?', /QUESTION )
					IF STRUPCASE(STRTRIM(res,2)) EQ 'NO' THEN RETURN

					WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
					IF PTR_VALID((*infoPtr).mwPtr) THEN BEGIN
						nw	= N_ELEMENTS(*((*infoPtr).mwPtr))
						FOR i = 0, nw - 1 DO BEGIN
							IF PTR_VALID((*((*infoPtr).mwPtr))[i]) THEN BEGIN
								IF WIDGET_INFO(*((*((*infoPtr).mwPtr))[i]), /VALID_ID) THEN BEGIN
									WIDGET_CONTROL, *((*((*infoPtr).mwPtr))[i]), GET_UVALUE = wPtr
									IF PTR_VALID(wPtr) THEN BEGIN
										IF OBJ_VALID((*wPtr).obj) THEN OBJ_DESTROY, (*wPtr).obj
										PTR_FREE, wPtr
									ENDIF
									WIDGET_CONTROL, *((*((*infoPtr).mwPtr))[i]), /DESTROY
								ENDIF
								PTR_FREE, (*((*infoPtr).mwPtr))[i]
							ENDIF
						ENDFOR
					ENDIF

					IF (*infoPtr).locationMapBase GE 0L AND WIDGET_INFO((*infoPtr).locationMapBase, /VALID_ID) $
					THEN BEGIN
						WIDGET_CONTROL, (*infoPtr).locationMapBase, GET_UVALUE = wPtr
						OBJ_DESTROY, (*wPtr).obj
						WIDGET_CONTROL, (*infoPtr).locationMapBase, /DESTROY
						PTR_FREE, wPtr
					ENDIF

					IF WIDGET_INFO((*infoPtr).dataSelectionBase,/VALID_ID) THEN BEGIN
						WIDGET_CONTROL,(*infoPtr).dataSelectionBase,MAP=0
						upperBase	= WIDGET_INFO((*infoPtr).dataSelectionBase, /CHILD)
						WIDGET_CONTROL, upperBase, GET_UVALUE = statePtr
						PTR_FREE, statePtr
						lowerBase	= WIDGET_INFO(upperBase, /SIBLING)
						WIDGET_CONTROL, lowerBase, /DESTROY
						WIDGET_CONTROL, (*infoPtr).dataSelectionBase, GET_UVALUE = ptr
						IF PTR_VALID(ptr) THEN PTR_FREE, ptr
					ENDIF

					PTR_FREE, infoPtr
					;
					; need to free pointers and destroy objects in data selection interface
					;
					WIDGET_CONTROL, event.top, /DESTROY
					END
				;===============================================
				; WIDGET_BUTTON: help
				; WIDGET_BUTTON: help
				; WIDGET_BUTTON: help
				; WIDGET_BUTTON: help
				; WIDGET_BUTTON: help
				;===============================================
				'help': BEGIN
					WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
					WIDGET_CONTROL, event.id, SENSITIVE = 0
;;;ckt,dec1999					create_splash_screen, (*infoPtr).misr_version, (*infoPtr).idl_version
					display_misr_view_logo,			$
						(*infoPtr).misr_version,	$
						(*infoPtr).versionImgPtr,	$
						(*infoPtr).reqImgPtr,		$
						(*infoPtr).logoImgPtr,		$
						SECONDS = 5,			$
						IDL_SAVE_VERSION = (*infoPtr).idl_save_version
					WIDGET_CONTROL, event.id, SENSITIVE = 1
					END
				;===============================================
				; WIDGET_BUTTON: openCatalogFile
				; WIDGET_BUTTON: openCatalogFile
				; WIDGET_BUTTON: openCatalogFile
				; WIDGET_BUTTON: openCatalogFile
				; WIDGET_BUTTON: openCatalogFile
				;===============================================
				'openCatalogFile': BEGIN

					;===============================================
					; Desensitize data selection interface while
					; catalog is being parsed.
					;===============================================
					WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
					IF WIDGET_INFO((*infoPtr).dataSelectionBase,/VALID_ID) THEN $
						WIDGET_CONTROL, (*infoPtr).dataSelectionBase, SENSITIVE=0

					done		= 0
					msg		= 'Select either a misr_view catalog or MISR HDF-EOS GRID file'

					WIDGET_CONTROL, WIDGET_INFO( event.id, /PARENT ), GET_UVALUE = initPath
					IF SIZE( initPath, /TYPE ) EQ 0 THEN BEGIN
						initPath	= GETENV('MISR_VIEW_CATALOG_DIR')
						WIDGET_CONTROL, WIDGET_INFO( event.id, /PARENT ), SET_UVALUE = initPath
					ENDIF

					IF SIZE( initPath, /TYPE ) GT 0 THEN BEGIN
						initPath	= initPath[0]
;;;ckt,apr2001						catFile		= DIALOG_PICKFILE( TITLE = msg, PATH=initPath, GET_PATH = initPath )
						catFile		= dialog_pickfile_wrapper( TITLE = msg, PATH=initPath, GET_PATH = initPath, /MUST_EXIST )
					ENDIF ELSE BEGIN
;;;ckt,apr2001						catFile		= DIALOG_PICKFILE( TITLE = msg, GET_PATH = initPath )
						catFile		= dialog_pickfile_wrapper( TITLE = msg, GET_PATH = initPath, /MUST_EXIST )
					ENDELSE

					IF initPath NE '' THEN $
						WIDGET_CONTROL, WIDGET_INFO( event.id, /PARENT ), SET_UVALUE = initPath

					msg1		= [	'No valid selection was made for a catalog/file...',	$
								'Do you wish to wish to re-select a catalog/file?' ]
					msg		= msg1

					WHILE NOT done DO BEGIN
						WHILE STRLEN(STRTRIM(catFile,2)) LE 0 DO BEGIN
							ret	= DIALOG_MESSAGE( msg, /QUESTION )
							IF ret EQ 'No' THEN BEGIN
								;===============================================
								; Resensitize data selection interface before
								; RETURN'ing.
								;===============================================
								IF WIDGET_INFO((*infoPtr).dataSelectionBase,/VALID_ID) THEN $
									WIDGET_CONTROL, (*infoPtr).dataSelectionBase, SENSITIVE=1
								RETURN
							ENDIF
							msg	= 'Select either a misr_view catalog or MISR HDF-EOS GRID file'
							WIDGET_CONTROL, WIDGET_INFO( event.id, /PARENT ), GET_UVALUE = initPath
							initPath = initPath[0]
;print,'UVALUE initPath (NOT done) = ',initPath
;help,initPath
;;;ckt,apr2001							catFile	= DIALOG_PICKFILE( TITLE = msg, PATH = initPath, GET_PATH = initPath )
							catFile	= dialog_pickfile_wrapper( TITLE = msg, PATH = initPath, GET_PATH = initPath, /MUST_EXIST )
;print,'user initPath (NOT done) = ',initPath
;help,initPath
							WIDGET_CONTROL, WIDGET_INFO( event.id, /PARENT ), SET_UVALUE = initPath
							msg	= msg1
						ENDWHILE

						ret		= FINDFILE( catFile, COUNT = cnt )

						IF cnt LE 0 THEN BEGIN
							msg2	= [							$
									'The catalog/file',				$
									catFile,					$
									'could not be located...',			$
									'Do you wish to wish to re-select a catalog/file?' ]
							msg	= msg2
							catFile = ''
						ENDIF ELSE BEGIN
							done	= 1
						ENDELSE
					ENDWHILE

					IF WIDGET_INFO((*infoPtr).dataSelectionBase,/VALID_ID) THEN BEGIN
						upperBase	= WIDGET_INFO((*infoPtr).dataSelectionBase, /CHILD)
						WIDGET_CONTROL, upperBase, GET_UVALUE = statePtr
					ENDIF ELSE BEGIN
						res	= DIALOG_MESSAGE(						$
							[ 'Data selection interface ID not valid...',			$
							  'Please make sure that this interface was not',		$
							  'killed.  If this this is the case, it first needs to be',	$
							  'restarted by clicking on the Show Data Selection Interface',	$
							  'menu item'], /ERROR )
						RETURN
					ENDELSE

					WIDGET_CONTROL, /HOURGLASS

					file_only		= HDF_ISHDF( catFile )

;print,'file_only = ',file_only
					IF file_only THEN				$
						msg	= 'Parsing File...'		$
					ELSE	msg	= 'Parsing Catalog...'
					pleasewaitbase = WIDGET_BASE(TITLE="Please Wait...")
					pleasewaitlabel = WIDGET_LABEL(pleasewaitbase, VALUE = msg )
					WIDGET_CONTROL, pleasewaitbase, /REALIZE

					catContents		= parseCatalog( catFile, FILE_ONLY = file_only, statePtr )

					IF WIDGET_INFO( pleasewaitbase, /VALID_ID ) THEN $
						WIDGET_CONTROL, pleasewaitbase, /DESTROY

					(*infoPtr).file_only	= file_only
					(*infoPtr).catFilePtr	= PTR_NEW( catFile )
;help,catContents
;print,'print,catContents[*,0]'
;print,catContents[*,0]
;print,'print,catContents[*,1]'
;print,catContents[*,1]
;print,'print,catContents[*,2]'
;print,catContents[*,2]
;print,'print,catContents[*,3]'
;print,catContents[*,3]
;print,'print,catContents[*,4]'
;print,catContents[*,4]
;print,'print,catContents[*,5]'
;print,catContents[*,5]
					IF TOTAL(STRLEN(catContents)) LE 0 THEN BEGIN
						;===============================================
						; Resensitize data selection interface before
						; RETURN'ing.
						;===============================================
						IF WIDGET_INFO((*infoPtr).dataSelectionBase,/VALID_ID) THEN $
							WIDGET_CONTROL, (*infoPtr).dataSelectionBase, SENSITIVE=1
						RETURN
					ENDIF
					WIDGET_CONTROL, event.top, GET_UVALUE = statePtr
					*((*statePtr).catContentsPtr)	= catContents
					tmp_idx				= WHERE(LONG(catContents[*,2]) GT 0, tmp_cnt)
					newOrbitNumber			= ''
					newPathNumber			= (FIX(catContents[*,1]))[0]
					IF tmp_cnt GT 0 THEN BEGIN
						newPathNumber		= (FIX(catContents[*,1]))[tmp_idx[0]]
						newOrbitNumber		= catContents[tmp_idx[0],2]
					ENDIF

					upper_data_selection_base	=						$
						WIDGET_INFO( (*statePtr).dataSelectionBase, /CHILD )

					WIDGET_CONTROL, upper_data_selection_base, GET_UVALUE = upperBaseStatePtr

					reset_upperBase_buttons, upperBaseStatePtr, LONG(newPathNumber), LONG(newOrbitNumber)

					;===============================================
					; Resensitize data selection interface.
					;===============================================
					IF WIDGET_INFO((*infoPtr).dataSelectionBase,/VALID_ID) THEN $
						WIDGET_CONTROL, (*infoPtr).dataSelectionBase, SENSITIVE=1

					END
				;===============================================
				; WIDGET_BUTTON: reinitialize o-p-d
				; WIDGET_BUTTON: reinitialize o-p-d
				; WIDGET_BUTTON: reinitialize o-p-d
				; WIDGET_BUTTON: reinitialize o-p-d
				; WIDGET_BUTTON: reinitialize o-p-d
				;===============================================
				'reinitialize o-p-d': BEGIN
					WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
					IF WIDGET_INFO((*infoPtr).dataSelectionBase,/VALID_ID) THEN			$
						WIDGET_CONTROL, WIDGET_INFO( (*infoPtr).dataSelectionBase, /CHILD ),	$
							GET_UVALUE = statePtr

					orbit_path_date_struct	= misr_get_initial_orbit_path_date_info( $
									INITIAL_VALUES_STRUCT_PTR = $
										(*infoPtr).initial_values_struct_ptr )
					(*infoPtr).initial_values_struct_ptr $
								= orbit_path_date_struct.initial_values_struct_ptr

					(*statePtr).Nref	= LONG( orbit_path_date_struct.init_orbit_val )
					(*statePtr).PNref	= LONG( orbit_path_date_struct.init_path_val )
					(*statePtr).JNref	= julianday( orbit_path_date_struct.init_date_val )

					initialPath		= (*statePtr).PNref

					IF SIZE( *((*statePtr).catContentsPtr), /TYPE) LE 0 THEN BEGIN
						initialOrbit = path2most_recent_orbit( $
							(*statePtr).PNref, (*statePtr).Nref, (*statePtr).JNref )
					ENDIF ELSE BEGIN
						initialOrbit = long( max( (*((*statePtr).catContentsPtr))[*,2] ) )
					ENDELSE

					IF PTR_VALID( (*infoPtr).catFilePtr ) THEN BEGIN
						;===============================================
						; Desensitize data selection interface while catalog is being parsed.
						;===============================================
						WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
						IF WIDGET_INFO((*infoPtr).dataSelectionBase,/VALID_ID) THEN $
							WIDGET_CONTROL, (*infoPtr).dataSelectionBase, SENSITIVE=0

						pleasewaitbase = WIDGET_BASE(TITLE="Please Wait...")
;print,'(*infoPtr).file_only = ',(*infoPtr).file_only
						IF (*infoPtr).file_only THEN			$
							msg	= 'Parsing File...'		$
						ELSE	msg	= 'Parsing Catalog...'
						pleasewaitlabel = WIDGET_LABEL(pleasewaitbase, VALUE = msg )
						WIDGET_CONTROL, pleasewaitbase, /REALIZE

						catContents			= parseCatalog( *(*infoPtr).catFilePtr,		$
											FILE_ONLY = (*infoPtr).file_only,	$
											statePtr )

						IF WIDGET_INFO( pleasewaitbase, /VALID_ID ) THEN $
							WIDGET_CONTROL, pleasewaitbase, /DESTROY

						initialPath			= (FIX(catContents[*,1]))[0]

						IF TOTAL(STRLEN(catContents)) LE 0 THEN BEGIN
							;===============================================
							; Resensitize data selection interface before RETURN'ing.
							;===============================================
							IF WIDGET_INFO((*infoPtr).dataSelectionBase,/VALID_ID) THEN $
								WIDGET_CONTROL, (*infoPtr).dataSelectionBase, SENSITIVE=1
							RETURN
						ENDIF

						*((*statePtr).catContentsPtr)	= catContents
						tmp_idx				= WHERE(FIX(catContents[*,2]) GT 0, tmp_cnt)
						initialOrbit			= ''
						IF tmp_cnt GT 0 THEN								$
							initialOrbit		= catContents[tmp_idx[0],2]

						;===============================================
						; Resensitize data selection interface.
						;===============================================
						IF WIDGET_INFO((*infoPtr).dataSelectionBase,/VALID_ID) THEN $
							WIDGET_CONTROL, (*infoPtr).dataSelectionBase, SENSITIVE=1

					ENDIF

					reset_upperBase_buttons, statePtr, initialPath, initialOrbit

					END
				;===============================================
				; WIDGET_BUTTON: toggleDataSelectionInterface
				; WIDGET_BUTTON: toggleDataSelectionInterface
				; WIDGET_BUTTON: toggleDataSelectionInterface
				; WIDGET_BUTTON: toggleDataSelectionInterface
				; WIDGET_BUTTON: toggleDataSelectionInterface
				;===============================================
				'toggleDataSelectionInterface': BEGIN
					WIDGET_CONTROL, event.id, GET_VALUE = btName
					showGUI	= STRMID(STRTRIM(btName,2),0,4) EQ 'Show'
					WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
					IF showGUI THEN BEGIN
						IF WIDGET_INFO((*infoPtr).dataSelectionBase,/VALID_ID) THEN BEGIN
							WIDGET_CONTROL, (*infoPtr).dataSelectionBase, MAP = 1
						ENDIF ELSE BEGIN
							IF (*infoPtr).eight_bit_display THEN BEGIN
								(*infoPtr).dataSelectionBase =				$
									misr_data_selection(event.top,			$
									(*infoPtr).catContentsPtr,			$
									(*infoPtr).elevationMapPtr,			$
									(*infoPtr).agpPtr,				$
									(*infoPtr).headersize,				$
									/EIGHT_BIT_DISPLAY)
							ENDIF ELSE BEGIN
								(*infoPtr).dataSelectionBase	=			$
									misr_data_selection(event.top,			$
									(*infoPtr).catContentsPtr,			$
									(*infoPtr).elevationMapPtr,			$
									(*infoPtr).agpPtr,				$
									(*infoPtr).headersize				$
									)
							ENDELSE
						ENDELSE
						WIDGET_CONTROL, event.id, SET_VALUE = 'Hide Data Selection Interface'
					ENDIF ELSE BEGIN
						WIDGET_CONTROL, event.id, SET_VALUE = 'Show Data Selection Interface'
						WIDGET_CONTROL, (*infoPtr).dataSelectionBase, MAP = 0
					ENDELSE
					WIDGET_CONTROL, event.top, /SHOW
					END
				;===============================================
				; WIDGET_BUTTON: toggleLocationMap
				; WIDGET_BUTTON: toggleLocationMap
				; WIDGET_BUTTON: toggleLocationMap
				; WIDGET_BUTTON: toggleLocationMap
				; WIDGET_BUTTON: toggleLocationMap
				;===============================================
				'toggleLocationMap': BEGIN

			WIDGET_CONTROL, event.top, GET_UVALUE = tlbPtr


					WIDGET_CONTROL, event.id, GET_VALUE = btName
					showGUI	= STRMID(STRTRIM(btName,2),0,4) EQ 'Show'
					WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
					IF showGUI THEN BEGIN
						IF (*infoPtr).locationMapBase LT 0L OR					$
							NOT WIDGET_INFO((*infoPtr).locationMapBase, /VALID_ID) THEN BEGIN
							IF (*infoPtr).eight_bit_display THEN BEGIN
								lws                        = { id:event.id, infoPtr:infoPtr }
								(*infoPtr).locationMapBase =				$
									get_new_window_8bit( LOCATION_WINDOW_STRUCT = lws )
							ENDIF ELSE BEGIN
								(*infoPtr).locationMapBase = get_location_window(event.id,infoPtr)
							ENDELSE





					win	= (*infoPtr).locationMapBase
					IF win GE 0 THEN BEGIN
						first_child			= WIDGET_INFO( win, /CHILD )
						first_child_of_first_child	= WIDGET_INFO( first_child, /CHILD )
						WIDGET_CONTROL, first_child_of_first_child, GET_UVALUE = currentObj
						obj_parent_base			= currentObj->GetParentBase()
						WIDGET_CONTROL, win, GET_UVALUE = basePtr
						; constuct new title.
						newTitle			=					$
								(*basePtr).winTitle +					$
								'  z=' +						$
								STRTRIM(STRING(currentObj->GetZoomFactor()),2) +	$
								'  m=' +						$
								currentObj->GetMode()
						; change title.
						WIDGET_CONTROL, win, TLB_SET_TITLE=newTitle

						IF NOT PTR_VALID((*tlbPtr).mwPtr) THEN BEGIN
							(*tlbPtr).mwPtr		= PTR_NEW(PTRARR(1))
							(*((*tlbPtr).mwPtr))[0]	= PTR_NEW(win)
						ENDIF ELSE BEGIN
							tmpPtrArr		= *((*tlbPtr).mwPtr)
							noExtraPtr		= 1
							ii			= 0
							ptrIdx			= (-1L)
							WHILE noExtraPtr AND ii LT N_ELEMENTS(tmpPtrArr) DO BEGIN
								IF NOT WIDGET_INFO(*(tmpPtrArr[ii]),/VALID_ID) THEN BEGIN
									PTR_FREE, tmpPtrArr[ii]
									noExtraPtr	= 0
									ptrIdx		= ii
								ENDIF
								ii = ii + 1
							ENDWHILE

							IF noExtraPtr THEN BEGIN
								PTR_FREE, (*tlbPtr).mwPtr
								(*tlbPtr).mwPtr					=	$
									PTR_NEW(PTRARR(N_ELEMENTS(tmpPtrArr)+1))
								(*((*tlbPtr).mwPtr))[0:N_ELEMENTS(tmpPtrArr)-1] =	$
									tmpPtrArr
								(*((*tlbPtr).mwPtr))[N_ELEMENTS(tmpPtrArr)]	=	$
									PTR_NEW(win)
							ENDIF ELSE BEGIN
								(*((*tlbPtr).mwPtr))[ptrIdx] = PTR_NEW(win)
							ENDELSE
						ENDELSE
					ENDIF





						ENDIF ELSE BEGIN
							IF (*infoPtr).eight_bit_display THEN BEGIN
								WIDGET_CONTROL, (*infoPtr).locationMapBase, GET_UVALUE = locBasePtr
								r_vec_ptr	= ((*(locBasePtr)).obj)->EightBit_GetColorVectorPtr(/RED)
								g_vec_ptr	= ((*(locBasePtr)).obj)->EightBit_GetColorVectorPtr(/GRN)
								b_vec_ptr	= ((*(locBasePtr)).obj)->EightBit_GetColorVectorPtr(/BLU)
;print,'WIDGET_BUTTON: toggleLocationMap -- TVLCT, *(r_vec_ptr), *(g_vec_ptr), *(b_vec_ptr)'
;print,'WIDGET_BUTTON: toggleLocationMap -- *(r_vec_ptr) = ',*(r_vec_ptr)
;print,'WIDGET_BUTTON: toggleLocationMap -- *(g_vec_ptr) = ',*(g_vec_ptr)
;print,'WIDGET_BUTTON: toggleLocationMap -- *(b_vec_ptr) = ',*(b_vec_ptr)
								TVLCT, *(r_vec_ptr), *(g_vec_ptr), *(b_vec_ptr)
							ENDIF
							WIDGET_CONTROL, (*infoPtr).locationMapBase, MAP = 1
						ENDELSE

						WIDGET_CONTROL, event.id, SET_VALUE = 'Hide Location Map'
					ENDIF ELSE BEGIN
						WIDGET_CONTROL, event.id, SET_VALUE = 'Show Location Map'
						WIDGET_CONTROL, (*infoPtr).locationMapBase, MAP = 0
					ENDELSE
					WIDGET_CONTROL, event.top, /SHOW
					END
				;===============================================
				; WIDGET_BUTTON: commoncolormap
				; WIDGET_BUTTON: commoncolormap
				; WIDGET_BUTTON: commoncolormap
				; WIDGET_BUTTON: commoncolormap
				; WIDGET_BUTTON: commoncolormap
				;===============================================
				'commoncolormap': BEGIN
					WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
					IF (*infoPtr).current_display_mode NE 'commoncolormap' THEN BEGIN
						result8bit = EightBit_ApplyColorMap( infoPtr )
						IF result8bit GT 0 THEN BEGIN
							WIDGET_CONTROL, (*infoPtr).privateID, SET_VALUE = 'Private Color Maps'
							WIDGET_CONTROL, (*infoPtr).commonID, SET_VALUE = '> Common Color Map'
							(*infoPtr).current_display_mode	= 'commoncolormap'
						ENDIF
					ENDIF ELSE BEGIN
						msg	= [								$
							'A common color map is already being used by misr_view...',	$
							'Do you wish to redefine the shared color map by selecting',	$
							'a different source image?' ]
						res	= DIALOG_MESSAGE( msg, /QUESTION )
						IF res EQ 'Yes' THEN	$
							result8bit = EightBit_ApplyColorMap( infoPtr )
					ENDELSE
					END
				;===============================================
				; WIDGET_BUTTON: privatecolormap
				; WIDGET_BUTTON: privatecolormap
				; WIDGET_BUTTON: privatecolormap
				; WIDGET_BUTTON: privatecolormap
				; WIDGET_BUTTON: privatecolormap
				;===============================================
				'privatecolormap': BEGIN
					WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
;print,'(*infoPtr).current_display_mode = ',(*infoPtr).current_display_mode,'     should be privatecolormap.'
					IF (*infoPtr).current_display_mode NE 'privatecolormap' THEN BEGIN
						WIDGET_CONTROL, (*infoPtr).privateID, SET_VALUE = '> Private Color Maps'
						WIDGET_CONTROL, (*infoPtr).commonID, SET_VALUE = 'Common Color Map'
						(*infoPtr).current_display_mode	= 'privatecolormap'
						result8bit = EightBit_ApplyColorMap( infoPtr, /PRIVATE_CMAP )
					ENDIF
					END
				;===============================================
				; WIDGET_BUTTON: preferences
				; WIDGET_BUTTON: preferences
				; WIDGET_BUTTON: preferences
				; WIDGET_BUTTON: preferences
				; WIDGET_BUTTON: preferences
				;===============================================
				'preferences': BEGIN
					show_misr_view_preferences, event.top
					END
				ELSE:
			ENDCASE
			END


		;===============================================================
		; MISR_DATA_SELECTION
		; MISR_DATA_SELECTION
		; MISR_DATA_SELECTION
		; MISR_DATA_SELECTION
		; MISR_DATA_SELECTION
		;===============================================================
		'MISR_DATA_SELECTION': BEGIN
			skip_this_event	= 1
			WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
			WIDGET_CONTROL, (*infoPtr).tlb, GET_UVALUE = tlbPtr

			IF event.widgetType EQ 'WIDGET_BUTTON' THEN BEGIN
				WIDGET_CONTROL, event.id, GET_VALUE = button_name
				IF button_name EQ 'Create Viewer'	THEN skip_this_event = 0
				IF button_name EQ 'Store'		THEN skip_this_event = 0
				IF button_name EQ 'Recall'		THEN skip_this_event = 0

			ENDIF

			IF skip_this_event THEN BEGIN
				(*tlbPtr).lastActiveObj = OBJ_NEW()
				RETURN
			ENDIF

			CASE button_name OF	; We're inside the 'MISR_DATA_SELECTION' portion of a CASE statement.

				;===============================================
				; MISR_DATA_SELECTION: Store Data Selections
				; MISR_DATA_SELECTION: Store Data Selections
				; MISR_DATA_SELECTION: Store Data Selections
				; MISR_DATA_SELECTION: Store Data Selections
				; MISR_DATA_SELECTION: Store Data Selections
				;===============================================
				'Store': BEGIN

					WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
					WIDGET_CONTROL, (*infoPtr).tlb, GET_UVALUE = tlbInfoPtr
					WIDGET_CONTROL, WIDGET_INFO(event.top,/CHILD), GET_UVALUE = upperBaseStatePtr

					WIDGET_CONTROL, event.top, SENSITIVE=0
					WIDGET_CONTROL, (*infoPtr).tlb, SENSITIVE=0
					WIDGET_CONTROL, /HOURGLASS

					;--------------------------------------------------
					; Get all sources of needed data.
					;--------------------------------------------------
					WIDGET_CONTROL, (*upperBaseStatePtr).orbitText, GET_VALUE = orbitStr
					WIDGET_CONTROL, (*upperBaseStatePtr).pathText, GET_VALUE = pathStr
					WIDGET_CONTROL, (*((*infoPtr).lowerInfoPtr)).actResLabel, GET_VALUE = actResStr
					WIDGET_CONTROL, (*((*infoPtr).lowerInfoPtr)).altResLabel, GET_VALUE = altResStr
					WIDGET_CONTROL, (*infoPtr).rotateTx, GET_VALUE = rotationAngle
					planeObjStructArray	= (*((*infoPtr).lowerInfoPtr)).planeObjStructArray

					;--------------------------------------------------
					; Create arrays for all image planes.
					;--------------------------------------------------
					nPlanes			= (*((*infoPtr).lowerInfoPtr)).nPlanes
					product			= STRARR( nPlanes )			;string
					grid			= STRARR( nPlanes )			;string
					field			= STRARR( nPlanes )			;string
					num_type		= STRARR( nPlanes )			;string
					extraDims		= PTRARR( nPlanes )			;pointer
;help,(*upperBaseStatePtr).airMisrFilename

					;--------------------------------------------------
					; Collect up all the pieces of data to Store.
					;--------------------------------------------------
;help,planeObjStructArray
					FOR plane=0,nPlanes-1 DO BEGIN
						product[plane]		= planeObjStructArray[plane].fname
						grid[plane]		= planeObjStructArray[plane].grid
						field[plane]		= planeObjStructArray[plane].field
						num_type[plane]		= planeObjStructArray[plane].num_type
;help,plane
;help,product[plane]
;print, 'product[', plane, '] = ', product[plane]
;print,'grid[', plane, '] = ',grid[plane]
;print,'field[', plane, '] = ',field[plane]
;print,'num_type[', plane, '] = ',num_type[plane]
						IF PTR_VALID( planeObjStructArray[plane].extraDims ) THEN	$
							extraDims[plane]	= PTR_NEW(*(planeObjStructArray[plane].extraDims))
					ENDFOR

					orbit			= orbitStr[0]
					path			= pathStr[0]
					blockStart		= (*upperBaseStatePtr).whichBlocks[0]
					blockEnd		= (*upperBaseStatePtr).whichBlocks[1]
					actRes			= actResStr[0]
					altRes			= altResStr[0]
					rotationAngle		= rotationAngle[0]

					IF (WHERE( STRLOWCASE( TAG_NAMES( *tlbInfoPtr ) ) EQ 'catfileptr' ))[0] GE 0 THEN BEGIN
						IF PTR_VALID( (*tlbInfoPtr).catFilePtr ) THEN	$
							catFile = *(*tlbInfoPtr).catFilePtr
					ENDIF ELSE	catFile = ''
					IF SIZE( catFile, /TYPE ) EQ 0 THEN $
							catFile	= ''
;print,'catFile = ',catFile
;print,'(*tlbInfoPtr).file_only = ',(*tlbInfoPtr).file_only
					IF (*tlbInfoPtr).file_only THEN $
							catFile = ''

					;--------------------------------------------------
					; Put all into a structure to send off to the ASCII
					; write routine.
					;--------------------------------------------------
					data_selections_struct	= 	{					$
									nPlanes		: nPlanes,		$	;integer
									product		: product,		$	;string array
									grid		: grid,			$	;string array
									field		: field,		$	;string array
									num_type	: num_type,		$	;string array
									extraDims	: extraDims,		$	;pointer array
									orbit		: orbit,		$	;string
									path		: path,			$	;string
									blockStart	: blockStart,		$	;string
									blockEnd	: blockEnd,		$	;string
									actRes		: actRes,		$	;string
									altRes		: altRes,		$	;string
									rotationAngle	: rotationAngle,	$	;string
									catalogFile	: catFile,		$	;string
									airMisrFile	: (*upperBaseStatePtr).airMisrFilename }
;print,'data_selections_struct.product = ',data_selections_struct.product
;print,'data_selections_struct.grid = ',data_selections_struct.grid
;print,'data_selections_struct.field = ',data_selections_struct.field
;print,'data_selections_struct.airMisrFile = ',data_selections_struct.airMisrFile

					WIDGET_CONTROL, WIDGET_INFO( event.id, /PARENT ), GET_UVALUE = directory
;help,directory
					success		= data_selections_write( data_selections_struct, directory )
;help,directory
					IF SIZE( directory, /TYPE ) NE 0 THEN $
						WIDGET_CONTROL, WIDGET_INFO( event.id, /PARENT ), SET_UVALUE = directory

					WIDGET_CONTROL, event.top, SENSITIVE=1
					WIDGET_CONTROL, (*infoPtr).tlb, SENSITIVE=1

					END

				;===============================================
				; MISR_DATA_SELECTION: Recall Data Selections
				; MISR_DATA_SELECTION: Recall Data Selections
				; MISR_DATA_SELECTION: Recall Data Selections
				; MISR_DATA_SELECTION: Recall Data Selections
				; MISR_DATA_SELECTION: Recall Data Selections
				;===============================================
				'Recall': BEGIN

					recall_problem = 1

					WIDGET_CONTROL, /HOURGLASS
					WIDGET_CONTROL, event.top, SENSITIVE=0
					WIDGET_CONTROL, (*infoPtr).tlb, SENSITIVE=0

					WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
					WIDGET_CONTROL, (*infoPtr).tlb, GET_UVALUE = tlbInfoPtr
					WIDGET_CONTROL, WIDGET_INFO(event.top,/CHILD), GET_UVALUE = upperBaseStatePtr
					WIDGET_CONTROL, (*upperBaseStatePtr).lowerBase, GET_UVALUE = lowerBaseStatePtr

					;---------------------------------------------------------------------------
					; data_selections_read *CREATES* data_selections_struct.
					;---------------------------------------------------------------------------
					WIDGET_CONTROL, WIDGET_INFO( event.id, /PARENT ), GET_UVALUE = directory
					success		= data_selections_read( data_selections_struct, directory )
					IF success EQ 0 THEN BEGIN
						;-------------------------------------
						; Error handling is done within the
						; data_selections_read.pro routine.
						;-------------------------------------
						WIDGET_CONTROL, event.top, SENSITIVE=1
						WIDGET_CONTROL, (*infoPtr).tlb, SENSITIVE=1
						RETURN
					ENDIF
					IF SIZE( directory, /TYPE ) NE 0 THEN $
						WIDGET_CONTROL, WIDGET_INFO( event.id, /PARENT ), SET_UVALUE = directory

					pleasewaitbase_RDSP = WIDGET_BASE(TITLE="Please Wait...")
					pleasewaitlabel = WIDGET_LABEL(pleasewaitbase_RDSP,	$
								VALUE = 'Recalling Data Selection Parameters...' )
					WIDGET_CONTROL, pleasewaitbase_RDSP, /REALIZE

					nPlanes		= data_selections_struct.nPlanes
					orbit		= data_selections_struct.orbit
					path		= data_selections_struct.path
					blockStart	= data_selections_struct.blockStart
					blockEnd	= data_selections_struct.blockEnd
					actRes		= data_selections_struct.actRes
					altRes		= data_selections_struct.altRes
					rotationAngle	= data_selections_struct.rotationAngle
					catalogFile	= data_selections_struct.catalogFile
					product		= data_selections_struct.product
					grid		= data_selections_struct.grid
					field		= data_selections_struct.field
					num_type	= data_selections_struct.num_type
					tagnames	= TAG_NAMES( data_selections_struct )
					IF (WHERE( STRLOWCASE(TAG_NAMES( data_selections_struct )) EQ 'extradims' ))[0] GE 0 THEN	$
						extraDims	= data_selections_struct.extraDims					$
					ELSE	extraDims	= ''
					(*upperBaseStatePtr).airMisrFilename					$
								= data_selections_struct.airMisrFile

					(*upperBaseStatePtr).airMisrFlag = 0
					IF (*upperBaseStatePtr).airMisrFilename NE '' THEN			$
						(*upperBaseStatePtr).airMisrFlag = 1

					IF (*upperBaseStatePtr).airMisrFlag THEN BEGIN

						WIDGET_CONTROL, (*upperBaseStatePtr).airMisrChooseButton, /SET_BUTTON

						misrChooseButtonEvent, {					$
							WIDGET_BUTTON,						$
							ID	: (*upperBaseStatePtr).airMisrChooseButton,	$
							TOP	: event.top,					$
							HANDLER	: WIDGET_INFO(					$
							(*upperBaseStatePtr).airMisrChooseButton, /PARENT ),	$
							SELECT	: 1 }

						found = FINDFILE( (*upperBaseStatePtr).airMisrFilename, COUNT = count )
						IF count EQ 0 THEN BEGIN
							IF WIDGET_INFO( pleasewaitbase_RDSP, /VALID_ID ) THEN $
								WIDGET_CONTROL, pleasewaitbase_RDSP, /DESTROY

							RETURN
						ENDIF

						valid = valid_airmisr_file( (*upperBaseStatePtr).airMisrFilename )
						IF NOT valid THEN BEGIN
							IF WIDGET_INFO( pleasewaitbase_RDSP, /VALID_ID ) THEN $
								WIDGET_CONTROL, pleasewaitbase_RDSP, /DESTROY

							RETURN
						ENDIF

						;-------------------------------------------
						; Put the AirMISR filename into the text
						; widget, which has scroll bars to allow
						; seeing the entire filename.
						;-------------------------------------------
						WIDGET_CONTROL, (*upperBaseStatePtr).airMisrFileText,		$
							SET_VALUE = STRTRIM( (*upperBaseStatePtr).airMisrFilename, 2 )

						;-------------------------------------------
						; Make sure that the selection of viewplanes
						; is unset.
						;-------------------------------------------
						unset_planes, upperBaseStatePtr, -1, -1,			$
							AIRMISR_FILE = (*upperBaseStatePtr).airMisrFilename

						;-------------------------------------------
						; Temporarily put the AirMISR coordinates in
						; the mapCenterLatLon variable to be used by
						; MAP_SET in the drawMap routine.  Then set
						; mapCenterLatLon back to MISR coordinates.
						;-------------------------------------------
						savedMapCenterLatLon			= (*upperBaseStatePtr).mapCenterLatLon
						airmisr_latlon_info			= retrieve_airmisr_latlon_info(	$
												(*upperBaseStatePtr).airMisrFilename )
						(*upperBaseStatePtr).mapCenterLatLon	= [ 0, airmisr_latlon_info.center_latlon[1] ]
						drawMap, upperBaseStatePtr
						(*upperBaseStatePtr).mapCenterLatLon	= savedMapCenterLatLon

					ENDIF ELSE BEGIN

						WIDGET_CONTROL, (*upperBaseStatePtr).misrChooseButton, /SET_BUTTON
						misrChooseButtonEvent, {					$
							WIDGET_BUTTON,						$
							ID	: (*upperBaseStatePtr).misrChooseButton,	$
							TOP	: event.top,					$
							HANDLER	: WIDGET_INFO(					$
							(*upperBaseStatePtr).misrChooseButton, /PARENT ),	$
							SELECT	: 1 }

						;-------------------------------------------
						; Take care of orbit, path, date & time.
						;-------------------------------------------
						WIDGET_CONTROL, (*upperBaseStatePtr).orbitText, SET_VALUE = STRTRIM(orbit,2)
						WIDGET_CONTROL, (*upperBaseStatePtr).pathText, SET_VALUE = STRTRIM(path,2)
						WIDGET_CONTROL, (*upperBaseStatePtr).orbitListButton, $
							SET_VALUE = 'Orbit List, Path ' + STRTRIM(path[0],2)
						setDate, upperBaseStatePtr, orbit
						setTime, upperBaseStatePtr, orbit
						orbitTextEvent, {						$
							WIDGET_TEXT,						$
							ID	: (*upperBaseStatePtr).misrChooseButton,	$
							TOP	: event.top,					$
							HANDLER	: WIDGET_INFO(					$
							(*upperBaseStatePtr).misrChooseButton, /PARENT )	}

						;-------------------------------------------
						; Take care of blocks and put data selection
						; interface into "Choose Blocks" mode.
						;-------------------------------------------
						WIDGET_CONTROL, (*upperBaseStatePtr).whichBlocksText[0],		$
							SET_VALUE = STRTRIM(blockStart,2)
						WIDGET_CONTROL, (*upperBaseStatePtr).whichBlocksText[1],		$
							SET_VALUE = STRTRIM(blockEnd,2)
						(*upperBaseStatePtr).oldLineValue[0] = $
							(*upperBaseStatePtr).blockScreenLineNumbers[1,blockStart-1]
						(*upperBaseStatePtr).oldLineValue[1] = $
							(*upperBaseStatePtr).blockScreenLineNumbers[1,blockEnd-1]
						WIDGET_CONTROL, (*upperBaseStatePtr).pathAndOrbitChooseBase, sensitive = 0
						WIDGET_CONTROL, (*upperBaseStatePtr).blocksChooseBase, sensitive = 1
						WIDGET_CONTROL, (*upperBaseStatePtr).mapModeChooseBlocksButton, set_button = 1
						(*upperBaseStatePtr).chooseOrbitOrBlocksFlag = 'blocks'
						errorCheck_whichBlocks, upperBaseStatePtr
						WIDGET_CONTROL, (*upperBaseStatePtr).zoomOutButton, /SET_BUTTON
						mapModeChooseEvent, {							$
							WIDGET_BUTTON,							$
							ID	: (*upperBaseStatePtr).mapModeChooseBlocksButton,	$
							TOP	: (*upperBaseStatePtr).parent,				$
							HANDLER	: WIDGET_INFO(						$
							(*upperBaseStatePtr).mapModeChooseBlocksButton, /PARENT ),	$
							SELECT	: 1 }
						zoomButtonEvent, {							$
							ID	: (*upperBaseStatePtr).zoomOutButton,			$
							TOP	: event.top,						$
							HANDLER	: WIDGET_INFO(						$
							(*upperBaseStatePtr).zoomOutButton, /PARENT ),			$
							SELECT	: 1 }

						;-------------------------------------------
						; Fill out more text widgets.
						;-------------------------------------------
						WIDGET_CONTROL, (*((*infoPtr).lowerInfoPtr)).actResLabel, SET_VALUE = STRTRIM(actRes[0],2)
						WIDGET_CONTROL, (*((*infoPtr).lowerInfoPtr)).altResLabel, SET_VALUE = STRTRIM(altRes[0],2)
						WIDGET_CONTROL, (*infoPtr).rotateTx, SET_VALUE = STRTRIM(rotationAngle[0],2)

					ENDELSE

					;-------------------------------------------
					; Loop over nPlanes, empty planes will be
					; skipped when no product is found.
					;-------------------------------------------
					catalog_is_present_and_parsed	= 0
					planes_need_to_be_unset		= 1
help,nPlanes
					FOR plane=0,nPlanes[0]-1 DO BEGIN

						;-------------------------------------------
						; Take care of catalog file if there is one.
						; If no catalog then parse each filename
						; contained in product[plane].
						;-------------------------------------------
;help,catalog_is_present_and_parsed
;help,(*upperBaseStatePtr).airMisrFilename
						IF catalogFile NE ''				$
						AND NOT catalog_is_present_and_parsed		$
						AND NOT (*upperBaseStatePtr).airMisrFlag	$
						THEN BEGIN

							(*tlbInfoPtr).file_only		= HDF_ISHDF( catalogFile )
							(*tlbInfoPtr).catFilePtr	= PTR_NEW( catalogFile )

							pleasewaitbase = WIDGET_BASE(TITLE="Please Wait...")
;print,'(*tlbInfoPtr).file_only = ',(*tlbInfoPtr).file_only
							;----------------------------------------------------------------------
							; To be consistent, do the check even though FILE_ONLY should be FALSE.
							;----------------------------------------------------------------------
							IF (*tlbInfoPtr).file_only THEN			$
								msg	= 'Parsing File...'		$
							ELSE	msg	= 'Parsing Catalog...'
							pleasewaitlabel = WIDGET_LABEL(pleasewaitbase, VALUE = msg )
							WIDGET_CONTROL, pleasewaitbase, /REALIZE

							catContents			= parseCatalog( catalogFile,			$
												FILE_ONLY = (*tlbInfoPtr).file_only,	$
												upperBaseStatePtr )

							IF WIDGET_INFO( pleasewaitbase, /VALID_ID ) THEN $
								WIDGET_CONTROL, pleasewaitbase, /DESTROY

							IF TOTAL(STRLEN(catContents)) LE 0 THEN BEGIN
								;===============================================
								; Resensitize data selection interface before
								; RETURN'ing.
								;===============================================
								WIDGET_CONTROL, event.top, SENSITIVE=1
								WIDGET_CONTROL, (*infoPtr).tlb, SENSITIVE=1

								IF WIDGET_INFO( pleasewaitbase_RDSP, /VALID_ID ) THEN $
									WIDGET_CONTROL, pleasewaitbase_RDSP, /DESTROY

								RETURN
							ENDIF

							*((*tlbInfoPtr).catContentsPtr)	= catContents
							tmp_idx				= WHERE(FIX(catContents[*,2]) GT 0, tmp_cnt)
							initialOrbit			= ''
							IF tmp_cnt GT 0 THEN initialOrbit	= catContents[tmp_idx[0],2]

							catalog_is_present_and_parsed	= 1

							unset_planes, upperBaseStatePtr,	$
								LONG(orbit), FIX(path)

						ENDIF ELSE IF product[plane] NE ''		$
						AND NOT catalog_is_present_and_parsed		$
						AND NOT (*upperBaseStatePtr).airMisrFlag	$
						THEN BEGIN

							(*tlbInfoPtr).file_only		= HDF_ISHDF( product[plane] )
							(*tlbInfoPtr).catFilePtr	= PTR_NEW( product[plane] )
;help,(*upperBaseStatePtr).airMisrFilename
;print,'parsing ',product[plane]
							pleasewaitbase = WIDGET_BASE(TITLE="Please Wait...")
;print,'(*tlbInfoPtr).file_only = ',(*tlbInfoPtr).file_only
							;----------------------------------------------------------------------
							; To be consistent, do the check even though FILE_ONLY should be TRUE.
							;----------------------------------------------------------------------
							IF (*tlbInfoPtr).file_only THEN			$
								msg	= 'Parsing File...'		$
							ELSE	msg	= 'Parsing Catalog...'
							pleasewaitlabel = WIDGET_LABEL(pleasewaitbase, VALUE = msg )
							WIDGET_CONTROL, pleasewaitbase, /REALIZE

							catContents			= parseCatalog( product[plane],			$
												FILE_ONLY = (*tlbInfoPtr).file_only,	$
												upperBaseStatePtr )

							IF WIDGET_INFO( pleasewaitbase, /VALID_ID ) THEN $
								WIDGET_CONTROL, pleasewaitbase, /DESTROY

							IF TOTAL(STRLEN(catContents)) LE 0 THEN BEGIN
								;===============================================
								; Resensitize data selection interface before
								; RETURN'ing.
								;===============================================
								WIDGET_CONTROL, event.top, SENSITIVE=1
								WIDGET_CONTROL, (*infoPtr).tlb, SENSITIVE=1

								IF WIDGET_INFO( pleasewaitbase_RDSP, /VALID_ID ) THEN $
									WIDGET_CONTROL, pleasewaitbase_RDSP, /DESTROY

								RETURN
							ENDIF

							*((*tlbInfoPtr).catContentsPtr)	= catContents
							tmp_idx				= WHERE(FIX(catContents[*,2]) GT 0, tmp_cnt)
							initialOrbit			= ''
							IF tmp_cnt GT 0 THEN						$
								initialOrbit		= catContents[tmp_idx[0],2]

							IF planes_need_to_be_unset THEN BEGIN
								unset_planes, upperBaseStatePtr,			$
									LONG(orbit), FIX(path)
								planes_need_to_be_unset	= 0
							ENDIF ELSE BEGIN
								fill_data_button, (*upperBaseStatePtr).lowerBase,	$
									*((*upperBaseStatePtr).catContentsPtr),		$
									LONG(orbit), FIX(path)
							ENDELSE

						ENDIF

						WIDGET_CONTROL, (*lowerBaseStatePtr).stashBase, GET_UVALUE = dataMenuBase

						IF NOT (*upperBaseStatePtr).airMisrFlag THEN BEGIN
							IF (WHERE( STRLOWCASE( TAG_NAMES( *tlbInfoPtr ) ) EQ 'catfileptr' ))[0]	$
							GE 0 THEN BEGIN
								IF PTR_VALID( (*tlbInfoPtr).catFilePtr ) THEN BEGIN
									products_in_cat	= STRARR( N_ELEMENTS(				$
											(*((*upperBaseStatePtr).catContentsPtr))[*,0] ) )
									FOR i=0,N_ELEMENTS( products_in_cat )-1 DO BEGIN
;print,(*((*upperBaseStatePtr).catContentsPtr))[i,*]
										dir			=				$
											(*((*upperBaseStatePtr).catContentsPtr))[i,4]
										filename		=				$
											(*((*upperBaseStatePtr).catContentsPtr))[i,5]
										products_in_cat[i]	=				$
											STRTRIM( dir, 2 ) + STRTRIM( filename, 2 )
									ENDFOR
								ENDIF
							ENDIF
;print,''
;print,'products_in_cat = '
;FOR i=0,N_ELEMENTS( products_in_cat )-1 DO print,products_in_cat[i]
;print,''
;print,'product[0] = ',product[0]
;print,''
						ENDIF

						;-------------------------------------------
						; No product info in recall file, stop here.
						;-------------------------------------------
						IF dataMenuBase EQ -1 THEN BEGIN
;print,'WIDGET_INFO( dataMenuBase, /VALID_ID ) = ',WIDGET_INFO( dataMenuBase, /VALID_ID )
;print,'dataMenuBase = ',dataMenuBase
							WIDGET_CONTROL, event.top, SENSITIVE=1
							WIDGET_CONTROL, (*infoPtr).tlb, SENSITIVE=1

							IF WIDGET_INFO( pleasewaitbase_RDSP, /VALID_ID ) THEN $
								WIDGET_CONTROL, pleasewaitbase_RDSP, /DESTROY

							RETURN
						ENDIF
						WIDGET_CONTROL, dataMenuBase, GET_UVALUE = dataMenuBaseInfoPtr

						IF NOT (*upperBaseStatePtr).airMisrFlag THEN BEGIN
							;-------------------------------------------
							; Select only those products (files) in the
							; catalog that match the current orbit.
							;-------------------------------------------
;print,'orbit = ',orbit
;print,'(*((*upperBaseStatePtr).catContentsPtr)) = ',(*((*upperBaseStatePtr).catContentsPtr))
							orbit_idx	= WHERE(							$
										LONG( (*((*upperBaseStatePtr).catContentsPtr))[*,2])	$
										EQ LONG( orbit )					$
										OR							$
										(							$
										 LONG( (*((*upperBaseStatePtr).catContentsPtr))[*,1] )	$
										 EQ orbit2path( orbit,					$
										 (*upperBaseStatePtr).Nref,				$
										 (*upperBaseStatePtr).PNref )				$
										 AND							$
										 (							$
										  (*((*upperBaseStatePtr).catContentsPtr))[*,2]		$
										  EQ ''							$
										 )							$
										)							$
										)
;print,'orbit_idx = ',orbit_idx
							products_in_cat_by_orbit	= products_in_cat[orbit_idx]
;print,'------------------------------------------------------------------------------------------------------------------------'
;print,'plane = ',plane
;print,'product[plane] = ',product[plane]
;print,'products_in_cat_by_orbit = '
;FOR i=0,N_ELEMENTS( products_in_cat_by_orbit )-1 DO print,products_in_cat_by_orbit[i]
;print,''
							;-------------------------------------------
							; Get the index of the product for the
							; current plane, if there is one.
							;-------------------------------------------
							p_idx	= (WHERE( products_in_cat_by_orbit EQ product[plane] ))[0]
						ENDIF ELSE BEGIN
							p_idx	= 0
						ENDELSE
;print,'||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
;print,'p_idx = ',p_idx
;print,'||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
						;-------------------------------------------
						; Skip any planes that have no product
						; associated with them.
						;-------------------------------------------
						IF p_idx GE 0 THEN BEGIN

							;-------------------------------------------
							; Make all the selections in the data menu.
							; This includes Product, Grid, Field, and
							; any ExtraDims.
							;-------------------------------------------
							p_wd_ev, { WIDGET_LIST,				$
								ID	: (*dataMenuBaseInfoPtr).p_wd,	$
								TOP	: dataMenuBase,			$
								HANDLER	: (*dataMenuBaseInfoPtr).p_wd,	$
								INDEX	: p_idx,			$
								CLICKS	: 1 }
							g_idx	= (WHERE( *(*dataMenuBaseInfoPtr).gridsListPtr EQ grid[plane] ))[0]
							IF (*upperBaseStatePtr).airMisrFlag THEN		$
								g_idx	= 0
							IF g_idx GE 0 THEN					$
								g_wd_ev, { WIDGET_LIST,				$
									ID	: (*dataMenuBaseInfoPtr).g_wd,	$
									TOP	: dataMenuBase,			$
									HANDLER	: (*dataMenuBaseInfoPtr).g_wd,	$
									INDEX	: g_idx,			$
									CLICKS	: 1 }
							f_idx	= (WHERE( *(*dataMenuBaseInfoPtr).fieldsListPtr EQ field[plane] ))[0]
;print,''
;print,*(*dataMenuBaseInfoPtr).fieldsListPtr
;print,''
;print,'field[plane] = ',field[plane]
							IF f_idx GE 0 THEN					$
								f_wd_ev, { WIDGET_LIST,				$
									ID	: (*dataMenuBaseInfoPtr).f_wd,	$
									TOP	: dataMenuBase,			$
									HANDLER	: (*dataMenuBaseInfoPtr).f_wd,	$
									INDEX	: f_idx,			$
									CLICKS	: 1 }
;print,'extraDims[',plane,'] = ',extraDims[plane]
							IF extraDims[plane] NE '' THEN BEGIN
								exDims	= STR_SEP( STRTRIM( extraDims[plane] , 2 ), ' ' )
								FOR dim=0,N_ELEMENTS( exDims )-1 DO BEGIN
									d_idx	= exDims[dim]
;print,'d_idx = ',d_idx
									IF d_idx GE 0 THEN						$
										d_wd_ev, { WIDGET_LIST,					$
											ID	: (*dataMenuBaseInfoPtr).d_wd[dim],	$
											TOP	: dataMenuBase,				$
											HANDLER	: (*dataMenuBaseInfoPtr).d_wd[dim],	$
											INDEX	: d_idx,				$
											CLICKS	: 1 }
								ENDFOR
							ENDIF
;print,'p_idx = ',p_idx
;print,'g_idx = ',g_idx
;print,'f_idx = ',f_idx

							IF f_idx GE 0 THEN BEGIN
								;-------------------------------------------
								; Simulate "click" on the "OK" button in the
								; data menu.
								;-------------------------------------------
								build_data_menu_ev, { WIDGET_BUTTON,				$
									ID	: WIDGET_BUTTON( WIDGET_BASE(), VALUE = 'OK' ),	$
									TOP	: dataMenuBase,					$
									HANDLER	: dataMenuBase,					$
									SELECT	: 1 }

;print,'||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
;print,'(*dataMenuBaseInfoPtr).p_idx = ',(*dataMenuBaseInfoPtr).p_idx
;print,'(*dataMenuBaseInfoPtr).g_idx = ',(*dataMenuBaseInfoPtr).g_idx
;print,'(*dataMenuBaseInfoPtr).f_idx = ',(*dataMenuBaseInfoPtr).f_idx
;IF extraDims[plane] NE '' THEN print,'(*dataMenuBaseInfoPtr).d_idx = ',(*dataMenuBaseInfoPtr).d_idx
;print,'||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'

;help,(*lowerBaseStatePtr).planeObjArr[plane]
								;-------------------------------------------
								; Simulate mouse down (first half of "click")
								; on the next plane object.
								; TYPE:0
								;-------------------------------------------
								drawID	= (*lowerBaseStatePtr).planeObjArr[plane] -> GetDrawID()
								event	= GetLowerBaseContents_ev( {					$
									MISR_DATA_MENU_OBJ,						$
									ID		: drawID,					$
									TOP 		: (*tlbInfoPtr).dataSelectionBase,		$
									HANDLER		: (*upperBaseStatePtr).lowerBase,		$
									OBJ		: (*lowerBaseStatePtr).planeObjArr[plane],	$
									TYPE		: 0,						$
									X		: 0,						$
									Y		: 0,						$
									PRESS		: 0,						$
									RELEASE		: 0,						$
									CLICKS		: 0 } )
								;-------------------------------------------
								; Simulate mouse up (second half of "click")
								; on the next plane object.
								; TYPE:1
								;-------------------------------------------
								event	= GetLowerBaseContents_ev( {					$
									MISR_DATA_MENU_OBJ,						$
									ID		: drawID,					$
									TOP 		: (*tlbInfoPtr).dataSelectionBase,		$
									HANDLER		: (*upperBaseStatePtr).lowerBase,		$
									OBJ		: (*lowerBaseStatePtr).planeObjArr[plane],	$
									TYPE		: 1,						$
									X		: 0,						$
									Y		: 0,						$
									PRESS		: 0,						$
									RELEASE		: 0,						$
									CLICKS		: 0 } )
								;-------------------------------------------
								; Simulate "click" on the "Set Plane" button.
								;-------------------------------------------
								event	= GetLowerBaseContents_ev( {					$
									WIDGET_BUTTON,							$
									ID		: (*((*infoPtr).lowerInfoPtr)).setButton,	$
									TOP 		: (*tlbInfoPtr).dataSelectionBase,		$
									HANDLER		: (*upperBaseStatePtr).lowerBase,		$
									SELECT		: 1						$
									} )
							ENDIF
						ENDIF
					ENDFOR

					WIDGET_CONTROL, event.top, SENSITIVE=1
					WIDGET_CONTROL, (*infoPtr).tlb, SENSITIVE=1

					IF WIDGET_INFO( pleasewaitbase_RDSP, /VALID_ID ) THEN $
						WIDGET_CONTROL, pleasewaitbase_RDSP, /DESTROY

					END

				;===============================================
				; MISR_DATA_SELECTION: Create Viewer
				; MISR_DATA_SELECTION: Create Viewer
				; MISR_DATA_SELECTION: Create Viewer
				; MISR_DATA_SELECTION: Create Viewer
				; MISR_DATA_SELECTION: Create Viewer
				;===============================================
				'Create Viewer': BEGIN
					WIDGET_CONTROL, event.top, SENSITIVE = 0
					;--------------------------------------------------------------
					; Fetch the rotationAngle value from the rotate degrees text widget.
					;--------------------------------------------------------------
					WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
					WIDGET_CONTROL, (*infoPtr).rotateTx, GET_VALUE = rotationAngle
					rotationAngle	= rotationAngle[0]

					kill_objarr = OBJARR( 100 )

;;; BETTER ERROR CHECKING SHOULD BE HERE.

					IF rotationAngle EQ '' THEN rotationAngle = 0.0


					IF NOT is_valid_number( rotationAngle ) THEN BEGIN
						result = DIALOG_MESSAGE( 'Invalid rotation angle value, please retry.', /INFORMATION )
						WIDGET_CONTROL, (*infoPtr).rotateTx, SET_VALUE = '0.0'
						WIDGET_CONTROL, event.top, SENSITIVE = 1
						RETURN
					ENDIF

					;--------------------------------------------------------------------
					; ROTATE THE DATA ONLY IF rotationAngle IS NON-ZERO.  Rotation is mainly for
					; red-blue stereo viewing but could be employed for other purposes.
					;--------------------------------------------------------------------
					IF KEYWORD_SET( rotationAngle ) THEN BEGIN
						;--------------------------------------------------------------------
						; Insure the rotationAngle value to be in the range:  0.0 <= rotationAngle < 360.0
						;--------------------------------------------------------------------
						IF rotationAngle GE 360.0 THEN $
							WHILE rotationAngle GE 360.0 DO rotationAngle = rotationAngle - 360.0
						IF rotationAngle LT   0.0 THEN $
							WHILE rotationAngle LT   0.0 DO rotationAngle = rotationAngle + 360.0
						;--------------------------------------------------------------------
						; If rotationAngle is not evenly divisible by 90.0, then ask user what kind
						; of interpolation is desired for resampling the grid.
						;--------------------------------------------------------------------
						IF rotationAngle NE 0.0 THEN BEGIN
							;--------------------------------------------------------------------
							; Default to Nearest Neighbor.
							;--------------------------------------------------------------------
							interpType = ''
							IF (rotationAngle NE 90.0) $
							AND (rotationAngle NE 180.0) $
							AND (rotationAngle NE 270.0) THEN BEGIN
								interpType = askUserForInterpType( rotationAngle, $
									(*tlbPtr).dataSelectionBase )
								IF interpType EQ 'CANCEL' THEN RETURN
							ENDIF

						ENDIF
					ENDIF

					IF NOT WIDGET_INFO( (*tlbPtr).please_wait_dlg, /VALID_ID ) THEN	$
						(*tlbPtr).please_wait_dlg = Get_Please_Wait_Dlg()
					WIDGET_CONTROL, (*tlbPtr).please_wait_dlg, MAP = 1
					WIDGET_CONTROL, /HOURGLASS
					WIDGET_CONTROL, WIDGET_INFO(event.top,/CHILD), GET_UVALUE = upperBaseStatePtr

					lowerBasePtr	= (*infoPtr).lowerInfoPtr
					WIDGET_CONTROL, (*lowerBasePtr).altResMenu, GET_UVALUE = altResLabel
					WIDGET_CONTROL, altResLabel, GET_VALUE = altResStr
					WIDGET_CONTROL, (*lowerBasePtr).actResMenu, GET_UVALUE = actResLabel
					WIDGET_CONTROL, actResLabel, GET_VALUE = actResStr

					tmp_act_res_str	= STRMID( STRTRIM(actResStr[0],2), 0, STRLEN(STRTRIM(actResStr[0],2))-1 )
					tmp_alt_res_str	= STRMID( STRTRIM(altResStr[0],2), 0, STRLEN(STRTRIM(altResStr[0],2))-1 )
;print,'tmp_act_res_str=',tmp_act_res_str
;print,'tmp_alt_res_str=',tmp_alt_res_str
					resolution_act	= -1.0
					resolution_alt	= -1.0

					IF is_valid_number(tmp_alt_res_str) AND is_valid_number(tmp_act_res_str) THEN BEGIN
						resolution_act	= FLOAT(STRMID( STRTRIM(actResStr[0],2), 0, $
							STRLEN(STRTRIM(actResStr[0],2))-1 ))
						resolution_alt	= FLOAT(STRMID( STRTRIM(altResStr[0],2), 0, $
							STRLEN(STRTRIM(altResStr[0],2))-1 ))
					ENDIF

					e		= {AIRMISR_DATA:0}

					anglesPtr	= PTR_NEW()

;print,'misr_view.pro:  (*upperBaseStatePtr).airMisrFlag = ',(*upperBaseStatePtr).airMisrFlag
					IF (*upperBaseStatePtr).airMisrFlag THEN BEGIN
						lat_lon_struct	=							$
							retrieve_airmisr_lonlat_grids(					$
									(*upperBaseStatePtr).airMisrFilename )
						lon		= lat_lon_struct.longitude
						lat		= lat_lon_struct.latitude
;help,lon,lat

						lon_lat_source_file							$
								= (*upperBaseStatePtr).airMisrFilename

						lonStruct	= { block_numbers:0, blocks:lon, offsetX:[0L], offsetY:[0L] }
						latStruct	= { block_numbers:0, blocks:lat, offsetX:[0L], offsetY:[0L] }

						IF rotationAngle NE 0.0 THEN				$
							lonStruct = rotate_data(			$
										lonStruct,		$
										rotationAngle,		$
										interpType )

						lon		= OBJ_NEW( 'IMAGE_DATA',		$
									lonStruct.blocks,		$
									lonStruct.offsetX,		$
									lonStruct.offsetY,		$
									SOURCE_FILE = lon_lat_source_file, $
									STACK_DIMENSION_IDX = 2 )

						IF NOT OBJ_VALID(lon) THEN BEGIN
							WIDGET_CONTROL, (*tlbPtr).please_wait_dlg, MAP = 0
							WIDGET_CONTROL, event.top, SENSITIVE = 1
							RETURN
						ENDIF

;print,''
;print,'misr_view.pro  ---  lonStruct.offsetX = ',lonStruct.offsetX
;print,'misr_view.pro  ---  lonStruct.offsetY = ',lonStruct.offsetY
;print,''
						IF rotationAngle NE 0.0 THEN				$
								latStruct = rotate_data(		$
											latStruct,	$
											rotationAngle,	$
											interpType )

						lat		= OBJ_NEW( 'IMAGE_DATA',		$
									latStruct.blocks,		$
									latStruct.offsetX,		$
									latStruct.offsetY,		$
									SOURCE_FILE = lon_lat_source_file, $
									STACK_DIMENSION_IDX = 2 )
;print,''
;print,'misr_view.pro  ---  lonStruct.offsetX = ',lonStruct.offsetX
;print,'misr_view.pro  ---  lonStruct.offsetY = ',lonStruct.offsetY
;print,''

						IF NOT OBJ_VALID(lat) THEN BEGIN
							WIDGET_CONTROL, (*tlbPtr).please_wait_dlg, MAP = 0
							WIDGET_CONTROL, event.top, SENSITIVE = 1
							RETURN
						ENDIF

						e		= {AIRMISR_DATA:1}

						blockStart	= (-1)
						nBlocks		= (-1)
					ENDIF ELSE BEGIN
						WIDGET_CONTROL, (*upperBaseStatePtr).orbitText, GET_VALUE = orbitStrArr
						WIDGET_CONTROL, (*upperBaseStatePtr).pathText, GET_VALUE = pathStrArr

						orbitStr	= STRTRIM(orbitStrArr[0],2)
						pathStr		= STRTRIM(pathStrArr[0],2)
						blockStart	= FIX((*upperBaseStatePtr).whichBlocks[0])
						nBlocks		= FIX((*upperBaseStatePtr).whichBlocks[1])-blockStart + 1

						agpFileName	= GetAGPFileName(					$
									pathStr,					$
									*((*upperBaseStatePtr).catContentsPtr) )

						gmpStruct	= GetGMPFileName(					$
									pathStr,					$
									orbitStr,					$
									*((*upperBaseStatePtr).catContentsPtr), 	$
									(*lowerBasePtr).planeObjStructArray[*].fname )
						gmpFileName	= gmpStruct.fn
gmpFileName = ''
;===============================================
;
; This code has been added as a result of an error
; which occurred November 28, 2001 (ckt).  The error
; actually occurred in MISR_GEOREF_IMAGE.PRO
; at line 446, as follows:
;
;% XMANAGER: Caught unexpected error from client application. Message follows...
;% Unable to dereference NULL pointer: AZ_PTR.
;% Execution halted at:  MISR_GEOREF_IMAGE::ZOOMDISPLAY  446 /data/vis/vesa/misr_grid_viewer/beta_release/V4.1_beta_release/MISR_GEOREF_IMAGE.PRO
;%                       GEOREF_IMAGE_EV  8692 /data/vis/vesa/misr_grid_viewer/beta_release/V4.1_beta_release/GEOREF_IMAGE.PRO
;%                       XMANAGER_EVLOOP_STANDARD  478 /usr/local/rsi/idl_5.4/lib/xmanager.pro
;%                       XMANAGER          708 /usr/local/rsi/idl_5.4/lib/xmanager.pro
;%                       MISR_VIEW        4301 /data/vis/vesa/misr_grid_viewer/beta_release/V4.1_beta_release/misr_view.pro
;%                       $MAIN$
;
; The error occurred with an image window loaded with radiance data in the red, green, and blue planes, DF camera azimuth and zenith
; information loaded into the first two ancillary planes, and solar azimuth data loaded into the last ancillary plane.
;
; The error was thrown due to the anglesPtr structure not FULLY filling out its contents.  Although there are "slots" for all six
; planes, only the first three slots contained any information (since they had a camera associated with them).  The ancillary planes
; contained data that came from the GMP (geometric parameters) file, which is not directly associated with any one camera.
;
; I think that what needs to happen is for MISR_GEOREF_IMAGE to check the pointers it retrieves from the anglesPtr structure before using them.
;
;===============================================

						;========================================================================
						; THIS IS WHERE DATA FROM THE GMP FILE SHOULD BE RETRIEVED!!!!!!!!
						;========================================================================

						IF STRLEN(gmpFileName) GT 0 AND gmpStruct.camerasIdx[0] GE 0 THEN BEGIN
							;================================================================
							; Pull angle data corresponding to camera.
							;================================================================
							solarAzimuthStruct	=					$
								misr_readHDFEOSgrid(					$
										gmpFileName,				$
										'GeometricParameters',			$
										'SolarAzimuth',				$
										BLOCK_START = blockStart,		$
										N_BLOCKS = nBlocks,			$
										DATA_RES_ACT_METERS = resolution_act,	$
										DATA_RES_ALT_METERS = resolution_alt )

							IF (SIZE( solarAzimuthStruct ))[0] EQ 0 THEN BEGIN
								IF solarAzimuthStruct EQ -1 THEN BEGIN
									WIDGET_CONTROL, (*tlbPtr).please_wait_dlg, MAP = 0
									result = DIALOG_MESSAGE(			$
										[ 'HDF read failed on Solar Azimuth.',	$
										'Possibly some blocks are missing.' ],	$
										/INFORMATION )
									WIDGET_CONTROL, event.top, SENSITIVE = 1
									RETURN
								ENDIF
							ENDIF

							IF rotationAngle NE 0.0 THEN BEGIN
								solarAzimuthStruct	= rotate_data(			$
												solarAzimuthStruct,	$
												rotationAngle,		$
												interpType )
							ENDIF

							solarAzimuth	= OBJ_NEW( 'MISR_IMAGE_DATA',			$
											solarAzimuthStruct.blocks,	$
											solarAzimuthStruct.offsetX,	$
											solarAzimuthStruct.offsetY,	$
											GRID_NAME = 'GeometricParameters', $
											FIELD_NAME = 'SolarAzimuth',	$
											SOURCE_FILE = gmpFileName,	$
											STACK_DIMENSION_IDX = 2,	$
											TILE_ID = solarAzimuthStruct.block_numbers )

							IF OBJ_VALID( solarAzimuth ) THEN BEGIN
								good_idx = WHERE( OBJ_VALID( kill_objarr ), count )
								kill_objarr[count] = solarAzimuth
							ENDIF


							solarZenithStruct	=					$
								misr_readHDFEOSgrid(					$
									gmpFileName,					$
									'GeometricParameters',				$
									'SolarZenith',					$
									BLOCK_START = blockStart,			$
									N_BLOCKS = nBlocks,				$
									DATA_RES_ACT_METERS = resolution_act,		$
									DATA_RES_ALT_METERS = resolution_alt )

							IF (SIZE( solarZenithStruct ))[0] EQ 0 THEN BEGIN
								IF solarZenithStruct EQ -1 THEN BEGIN
									WIDGET_CONTROL, (*tlbPtr).please_wait_dlg, MAP = 0
									OBJ_DESTROY, kill_objarr
									result = DIALOG_MESSAGE(			$
										[ 'HDF read failed on Solar Zenith.',	$
										'Possibly some blocks are missing.' ],	$
										/INFORMATION )
									WIDGET_CONTROL, event.top, SENSITIVE = 1
									RETURN
								ENDIF
							ENDIF

							IF rotationAngle NE 0.0 THEN BEGIN
								solarZenithStruct	= rotate_data(			$
												solarZenithStruct,	$
												rotationAngle,		$
												interpType )
							ENDIF

							solarZenith	= OBJ_NEW( 'MISR_IMAGE_DATA',			$
											solarZenithStruct.blocks,	$
											solarZenithStruct.offsetX,	$
											solarZenithStruct.offsetY,	$
											GRID_NAME = 'GeometricParameters', $
											FIELD_NAME = 'SolarZenith',	$
											SOURCE_FILE = gmpFileName,	$
											STACK_DIMENSION_IDX = 2,	$
											TILE_ID = solarZenithStruct.block_numbers )

							IF OBJ_VALID( solarZenith ) THEN BEGIN
								good_idx = WHERE( OBJ_VALID( kill_objarr ), count )
								kill_objarr[count] = solarZenith
							ENDIF

							azimuth		= { slot : PTRARR(6) }
							zenith		= { slot : PTRARR(6) }
							az_field	= { slot : STRARR(6) }
							zen_field	= { slot : STRARR(6) }

							FOR i = 0, N_ELEMENTS(gmpStruct.camerasIdx)-1 DO BEGIN
								cam		=					$
									gmpStruct.cameras[gmpStruct.camerasIdx[i]]
								cam		= STRUPCASE(STRMID(cam,0,1)) +		$
										STRLOWCASE(STRMID(cam,1,1))
								camA		= cam + 'Azimuth'
								camZ		= cam + 'Zenith'
								cameraAzimuthStruct	= misr_readHDFEOSgrid(		$
											gmpFileName,			$
											'GeometricParameters',		$
											camA,				$
											BLOCK_START = blockStart,	$
											N_BLOCKS = nBlocks,		$
											DATA_RES_ACT_METERS =		$
												resolution_act,		$
											DATA_RES_ALT_METERS =		$
												resolution_alt )

								IF (SIZE( cameraAzimuthStruct ))[0] EQ 0 THEN BEGIN
									IF cameraAzimuthStruct EQ -1 THEN BEGIN
										WIDGET_CONTROL, (*tlbPtr).please_wait_dlg, MAP = 0
										OBJ_DESTROY, kill_objarr
									result = DIALOG_MESSAGE(			$
										[ 'HDF read failed on Camera Azimuth.',	$
										'Possibly some blocks are missing.' ],	$
										/INFORMATION )
										WIDGET_CONTROL, event.top, SENSITIVE = 1
										RETURN
									ENDIF
								ENDIF

								IF rotationAngle NE 0.0 THEN BEGIN
									cameraAzimuthStruct	= rotate_data(			$
													cameraAzimuthStruct,	$
													rotationAngle,		$
													interpType )
								ENDIF

								cameraAzimuth	= OBJ_NEW( 'MISR_IMAGE_DATA',		$
											cameraAzimuthStruct.blocks,	$
											cameraAzimuthStruct.offsetX,	$
											cameraAzimuthStruct.offsetY,	$
											GRID_NAME = 'GeometricParameters', $
											FIELD_NAME = camA,		$
											SOURCE_FILE = gmpFileName,	$
											STACK_DIMENSION_IDX = 2,	$
											TILE_ID = cameraAzimuthStruct.block_numbers )

								IF OBJ_VALID( cameraAzimuth ) THEN BEGIN
									good_idx = WHERE( OBJ_VALID( kill_objarr ), count )
									kill_objarr[count] = cameraAzimuth
								ENDIF

								cameraZenithStruct	= misr_readHDFEOSgrid(		$
											gmpFileName,			$
											'GeometricParameters',		$
											camZ,				$
											BLOCK_START = blockStart,	$
											N_BLOCKS = nBlocks,		$
											DATA_RES_ACT_METERS =		$
												resolution_act,		$
											DATA_RES_ALT_METERS =		$
												resolution_alt )

								IF (SIZE( cameraZenithStruct ))[0] EQ 0 THEN BEGIN
									IF cameraZenithStruct EQ -1 THEN BEGIN
										WIDGET_CONTROL, (*tlbPtr).please_wait_dlg, MAP = 0
										OBJ_DESTROY, kill_objarr
										result = DIALOG_MESSAGE(			$
											[ 'HDF read failed on Camera Zenith.',	$
											'Possibly some blocks are missing.' ],	$
											/INFORMATION )
										WIDGET_CONTROL, event.top, SENSITIVE = 1
										RETURN
									ENDIF
								ENDIF

								IF rotationAngle NE 0.0 THEN BEGIN
									cameraZenithStruct	= rotate_data(			$
													cameraZenithStruct,	$
													rotationAngle,		$
													interpType )
								ENDIF

								cameraZenith	= OBJ_NEW( 'MISR_IMAGE_DATA',		$
											cameraZenithStruct.blocks,	$
											cameraZenithStruct.offsetX,	$
											cameraZenithStruct.offsetY,	$
											GRID_NAME = 'GeometricParameters', $
											FIELD_NAME = camZ,		$
											SOURCE_FILE = gmpFileName,	$
											STACK_DIMENSION_IDX = 2,	$
											TILE_ID = cameraZenithStruct.block_numbers )

								IF OBJ_VALID( cameraZenith ) THEN BEGIN
									good_idx = WHERE( OBJ_VALID( kill_objarr ), count )
									kill_objarr[count] = cameraZenith
								ENDIF

								azimuth.slot[gmpStruct.camerasIdx[i]]	=		$
									PTR_NEW( cameraAzimuth, /NO_COPY )
								zenith.slot[gmpStruct.camerasIdx[i]]	=		$
									PTR_NEW( cameraZenith, /NO_COPY )
								az_field.slot[gmpStruct.camerasIdx[i]]	= camA
								zen_field.slot[gmpStruct.camerasIdx[i]]	= camZ
							ENDFOR

							anglesPtr = PTR_NEW( {						$
									source_file	: gmpFileName,			$
									cameraAzimuth	: azimuth,			$
									cameraZenith	: zenith,			$
									cameraAzField	: az_field,			$
									cameraZenField	: zen_field,			$
									solarAzimuth	: PTR_NEW(			$
													solarAzimuth,	$
													/NO_COPY ),	$
									solarZenith	: PTR_NEW(			$
													solarZenith,	$
													/NO_COPY ) },	$
									/NO_COPY )

						ENDIF
;res=dialog_message(['made it to here 1'],/information)
						lon_lat_source_file	= ''

						;========================================================================
						; If AGP file exists, then grab lon/lat images, otherwise set lon and
						; lat variables to (-999)
						;========================================================================
						IF STRLEN(agpFileName) GT 0 THEN BEGIN
;res=dialog_message(['made it to here 1b'],/information)
							lonStruct	= misr_readHDFEOSgrid(				$
										agpFileName,				$
										'Standard',				$
										'GeoLongitude',				$
										BLOCK_START = blockStart,		$
										N_BLOCKS = nBlocks,			$
										DATA_RES_ACT_METERS = resolution_act,	$
										DATA_RES_ALT_METERS = resolution_alt )

							IF (SIZE( lonStruct ))[0] EQ 0 THEN BEGIN
								IF lonStruct EQ -1 THEN BEGIN
									WIDGET_CONTROL, (*tlbPtr).please_wait_dlg, MAP = 0
									OBJ_DESTROY, kill_objarr
									result = DIALOG_MESSAGE(			$
										[ 'HDF read failed on Longitude.',	$
										'Possibly some blocks are missing.' ],	$
										/INFORMATION )
									WIDGET_CONTROL, event.top, SENSITIVE = 1
									RETURN
								ENDIF
							ENDIF

							IF rotationAngle NE 0.0 THEN				$
								lonStruct = rotate_data(			$
											lonStruct,		$
											rotationAngle,		$
											interpType )

;res=dialog_message(['made it to here 1c'],/information)
							lon		= OBJ_NEW( 'MISR_IMAGE_DATA',		$
										lonStruct.blocks,		$
										lonStruct.offsetX,		$
										lonStruct.offsetY,		$
										GRID_NAME = 'Standard',		$
										FIELD_NAME = 'GeoLongitude',	$
										SOURCE_FILE = agpFileName,	$
										STACK_DIMENSION_IDX = 2,	$
										TILE_ID = lonStruct.block_numbers )
;res=dialog_message(['made it to here 1d'],/information)


							IF OBJ_VALID( lon ) THEN BEGIN
								good_idx = WHERE( OBJ_VALID( kill_objarr ), count )
								kill_objarr[count] = lon
							ENDIF

;;							IF NOT OBJ_VALID(lon) THEN BEGIN
;;								WIDGET_CONTROL, (*tlbPtr).please_wait_dlg, MAP = 0
;;								WIDGET_CONTROL, event.top, SENSITIVE = 1
;;								RETURN
;;							ENDIF

							latStruct	= misr_readHDFEOSgrid(				$
										agpFileName,				$
										'Standard',				$
										'GeoLatitude',				$
										BLOCK_START = blockStart,		$
										N_BLOCKS = nBlocks,			$
										DATA_RES_ACT_METERS = resolution_act,	$
										DATA_RES_ALT_METERS = resolution_alt )

							IF (SIZE( latStruct ))[0] EQ 0 THEN BEGIN
								IF latStruct EQ -1 THEN BEGIN
									WIDGET_CONTROL, (*tlbPtr).please_wait_dlg, MAP = 0
									OBJ_DESTROY, kill_objarr
									result = DIALOG_MESSAGE(			$
										[ 'HDF read failed on Latitude.',	$
										'Possibly some blocks are missing.' ],	$
										/INFORMATION )
									WIDGET_CONTROL, event.top, SENSITIVE = 1
									RETURN
								ENDIF
							ENDIF

							IF rotationAngle NE 0.0 THEN				$
									latStruct = rotate_data(		$
												latStruct,	$
												rotationAngle,	$
												interpType )

							lat		= OBJ_NEW( 'MISR_IMAGE_DATA',		$
										latStruct.blocks,		$
										latStruct.offsetX,		$
										latStruct.offsetY,		$
										GRID_NAME = 'Standard',		$
										FIELD_NAME = 'GeoLatitude',	$
										SOURCE_FILE = agpFileName,	$
										STACK_DIMENSION_IDX = 2,	$
										TILE_ID = latStruct.block_numbers )

							IF OBJ_VALID( lat ) THEN BEGIN
								good_idx = WHERE( OBJ_VALID( kill_objarr ), count )
								kill_objarr[count] = lat
							ENDIF

;;							IF NOT OBJ_VALID(lat) THEN BEGIN
;;								WIDGET_CONTROL, (*tlbPtr).please_wait_dlg, MAP = 0
;;								WIDGET_CONTROL, event.top, SENSITIVE = 1
;;								RETURN
;;							ENDIF

							lon_lat_source_file	= agpFileName

						ENDIF ELSE BEGIN
							lon	= OBJ_NEW()
							lat	= OBJ_NEW()
						ENDELSE

					ENDELSE

					imgPtrArr		= PTRARR(6)
					data_id_str		= STRARR(6)
					data_source_fname	= STRARR(6)
					in_resolution		= { act:resolution_act, alt:resolution_alt }

;res=dialog_message(['made it to here 2'],/information)
					FOR planeIdx = 0, 5 DO BEGIN
						IF (*lowerBasePtr).planeObjStructArray[planeIdx].fname NE '' THEN BEGIN
							p	= (*lowerBasePtr).planeObjStructArray[planeIdx].fname
							g	= (*lowerBasePtr).planeObjStructArray[planeIdx].grid
							f	= (*lowerBasePtr).planeObjStructArray[planeIdx].field
							dPtr	= (*lowerBasePtr).planeObjStructArray[planeIdx].extraDims

							data_source_fname[planeIdx]					$
								= p
							pp	= STR_SEP(p,GetDirectoryDivider())
							data_id_str[planeIdx]						$
								= pp[N_ELEMENTS(pp)-1] + ' :: ' + g + ' :: ' + f

							;---------------------------------------------------------------
							; deal with extra dimensions other than SOMBlock, XDim, and YDim
							;---------------------------------------------------------------
							IF PTR_VALID(dPtr) THEN BEGIN
								extraDims	= (*dPtr)
								imgStruct	= misr_readHDFEOSgrid(			$
										p,					$
										g,					$
										f,					$
										BLOCK_START = blockStart,		$
										N_BLOCKS = nBlocks,			$
										DATA_RES_ACT_METERS = resolution_act,	$
										DATA_RES_ALT_METERS = resolution_alt,	$
										EXTRA_DIMS=[extraDims],			$
										MISSING_DATA_VALUES = missing_data,	$
										_EXTRA = e )

								IF (SIZE( imgStruct ))[0] EQ 0 THEN BEGIN
									IF imgStruct EQ -1 THEN BEGIN
										WIDGET_CONTROL, (*tlbPtr).please_wait_dlg, MAP = 0
										OBJ_DESTROY, kill_objarr
										result = DIALOG_MESSAGE(			$
											[ 'HDF read failed on MISR Image Data.',$
											'Possibly some blocks are missing.' ],	$
											/INFORMATION )
										WIDGET_CONTROL, event.top, SENSITIVE = 1
										RETURN
									ENDIF
								ENDIF

							ENDIF ELSE BEGIN
								imgStruct	= misr_readHDFEOSgrid(			$
										p,					$
										g,					$
										f,					$
										BLOCK_START = blockStart,		$
										N_BLOCKS = nBlocks,			$
										DATA_RES_ACT_METERS = resolution_act,	$
										DATA_RES_ALT_METERS = resolution_alt,	$
										MISSING_DATA_VALUES = missing_data,	$
										_EXTRA = e )

								IF (SIZE( imgStruct ))[0] EQ 0 THEN BEGIN
									IF imgStruct EQ -1 THEN BEGIN
										WIDGET_CONTROL, (*tlbPtr).please_wait_dlg, MAP = 0
										OBJ_DESTROY, kill_objarr
										result = DIALOG_MESSAGE(			$
											[ 'HDF read failed on MISR Image Data.',$
											'Possibly some blocks are missing.' ],	$
											/INFORMATION )
										WIDGET_CONTROL, event.top, SENSITIVE = 1
										RETURN
									ENDIF
								ENDIF

							ENDELSE
;;;ckt,jan2000 print,tag_names(imgStruct)
;;;ckt,jan2000 help,imgStruct.blocks
;;;ckt,jan2000 help,imgStruct.offsetX
;;;ckt,jan2000 help,imgStruct.offsetY
;;;ckt,jan2000 print,''
;;;ckt,jan2000 print,'misr_view.pro  ---  lonStruct.offsetX = ',lonStruct.offsetX
;;;ckt,jan2000 print,'misr_view.pro  ---  lonStruct.offsetY = ',lonStruct.offsetY
;;;ckt,jan2000 print,''
							;--------------------------------------------------------------
							; Rotate the data and x,y offsets.
							;--------------------------------------------------------------
							IF rotationAngle NE 0.0 THEN $
								imgStruct	= rotate_data(		$
											imgStruct,	$
											rotationAngle,	$
											interpType )
;;;ckt,jan2000 print,''
;;;ckt,jan2000 print,'misr_view.pro  ---  lonStruct.offsetX = ',lonStruct.offsetX
;;;ckt,jan2000 print,'misr_view.pro  ---  lonStruct.offsetY = ',lonStruct.offsetY
;;;ckt,jan2000 print,''

							;--------------------------------------------------------------
							; Put data into IMAGE_DATA object.
							;--------------------------------------------------------------
							IF SIZE( missing_data, /TYPE ) EQ 0 THEN $
								missing_data = -9999
;print,'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'
;print,'in misr_view.pro: missing_data = ',missing_data
;print,'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'

;transform_struct	= misr_get_transform_info(p,g,f)
;res=dialog_message(['made it to here 2b'],/information)

							IF (SIZE(missing_data))[0] GT 0 THEN					$
								img		= OBJ_NEW( 'MISR_IMAGE_DATA',			$
											imgStruct.blocks,			$
											imgStruct.offsetX,			$
											imgStruct.offsetY,			$
											GRID_NAME = g,				$
											FIELD_NAME = f,				$
											MISSING_DATA_VALUES = missing_data,	$
											STACK_DIMENSION_IDX = 2,		$
											ORIG_DATA_RESIZE_FACTOR = imgStruct.resize_data_factor, $
											SOURCE_FILE = p,			$
											TILE_ID = imgStruct.block_numbers )	$
							ELSE									$
								img		= OBJ_NEW( 'MISR_IMAGE_DATA',			$
											imgStruct.blocks,			$
											imgStruct.offsetX,			$
											imgStruct.offsetY,			$
											GRID_NAME = g,				$
											FIELD_NAME = f,				$
											STACK_DIMENSION_IDX = 2,		$
											ORIG_DATA_RESIZE_FACTOR = imgStruct.resize_data_factor, $
											SOURCE_FILE = p,			$
											TILE_ID = imgStruct.block_numbers )
;res=dialog_message(['made it to here 2c'],/information)
;;;ckt,jan2000 print,'misr_view.pro  ---  OBJ_VALID(img)=',OBJ_VALID(img)
							IF NOT OBJ_VALID(img) THEN BEGIN
								WIDGET_CONTROL, (*tlbPtr).please_wait_dlg, MAP = 0
								WIDGET_CONTROL, event.top, SENSITIVE = 1
								RETURN
							ENDIF

							imgPtrArr[planeIdx]	= PTR_NEW(img,/NO_COPY)

						ENDIF
					ENDFOR
;res=dialog_message(['made it to here 3'],/information)

					IF (*upperBaseStatePtr).airMisrFlag THEN BEGIN
						(*tlbPtr).uniqueWindowId	= (*tlbPtr).uniqueWindowId + 1L
						;--------------------------------------------------------------
						; GEOREF_IMAGE viewer window title is as follows:
						; w=UNIQUE WINDOW NUMBER
						;--------------------------------------------------------------
						winTitle			=					$
							'w=' +								$
							STRTRIM(STRING((*tlbPtr).uniqueWindowId),2)
					ENDIF ELSE BEGIN
						(*tlbPtr).uniqueWindowId	= (*tlbPtr).uniqueWindowId + 1L
						;--------------------------------------------------------------
						; GEOREF_IMAGE viewer window title is as follows:
						; w=UNIQUE WINDOW NUMBER, p=PATH NUMBER, o=ORBIT NUMBER, b=START BLOCK:END BLOCK
						;--------------------------------------------------------------
						winTitle			=					$
							'w=' +								$
							STRTRIM(STRING((*tlbPtr).uniqueWindowId),2) +			$
							'  p=' +							$
							STRTRIM(STRING(FIX(pathStr)),2) +				$
							'  o=' +							$
							STRTRIM(STRING(LONG(orbitStr)),2) +				$
							'  b=' +							$
							STRTRIM(STRING(blockStart),2) +					$
							':' +								$
							STRTRIM(STRING( blockStart + nBlocks - 1 ),2)
					ENDELSE

					;--------------------------------------------------------------
					; 8-BIT DISPLAY
					;--------------------------------------------------------------
					 IF (*tlbPtr).eight_bit_display THEN BEGIN
						dws	= {								$
								lon			:lon,				$
								lat			:lat,				$
								imgPtrArr		:imgPtrArr,			$
								winTitle		:winTitle,			$
								infoPtr			:tlbPtr,			$
								data_id_str		:data_id_str,			$
								data_source_fname	:data_source_fname,		$
								lon_lat_source_file	:lon_lat_source_file,		$
								start_block		:blockStart,			$
								n_blocks		:nBlocks,			$
								in_resolution		:in_resolution,			$
								anglesPtr		:anglesPtr }
						win	=  get_new_window_8bit( DATA_WINDOW_STRUCT = dws )
					ENDIF ELSE BEGIN
					;--------------------------------------------------------------
					; 24-BIT DISPLAY
					;--------------------------------------------------------------
						win = get_new_window(							$
									lon,						$
									lat,						$
									imgPtrArr,					$
									winTitle,					$
									tlbPtr,						$
									data_id_str,					$
									data_source_fname,				$
									lon_lat_source_file,				$
									blockStart,					$
									nBlocks,					$
									in_resolution,					$
									anglesPtr )
					ENDELSE

					;--------------------------------------------------------------
					; Phantom MAP_SET to fix the mystery problem that seemed to be
					; caused by instantiating GEOREF_IMAGE, probably as a result of
					; REALIZE'ing a new WIDGET_DRAW in the object.
					;--------------------------------------------------------------
					curWin		= !D.WINDOW
					WSET,(*upperBaseStatePtr).mapDrawWindowID
					MAP_SET,									$
						(*upperBaseStatePtr).mapCenterLatLon[0],				$
						(*upperBaseStatePtr).mapCenterLatLon[1],				$
						0.0,									$
						/NOERASE,								$
						_EXTRA = *((*upperBaseStatePtr).mapSetStruct)
					WSET, curWin

					IF win GE 0 THEN BEGIN
						first_child			= WIDGET_INFO( win, /CHILD )
						first_child_of_first_child	= WIDGET_INFO( first_child, /CHILD )
						WIDGET_CONTROL, first_child_of_first_child, GET_UVALUE = currentObj
						obj_parent_base			= currentObj->GetParentBase()
						WIDGET_CONTROL, win, GET_UVALUE = basePtr
						; constuct new title.
						newTitle			=					$
								(*basePtr).winTitle +					$
								'  z=' +						$
								STRTRIM(STRING(currentObj->GetZoomFactor()),2) +	$
								'  m=' +						$
								currentObj->GetMode()
						; change title.
						WIDGET_CONTROL, win, TLB_SET_TITLE=newTitle

						IF NOT PTR_VALID((*tlbPtr).mwPtr) THEN BEGIN
							(*tlbPtr).mwPtr		= PTR_NEW(PTRARR(1))
							(*((*tlbPtr).mwPtr))[0]	= PTR_NEW(win)
						ENDIF ELSE BEGIN
							tmpPtrArr		= *((*tlbPtr).mwPtr)
							noExtraPtr		= 1
							ii			= 0
							ptrIdx			= (-1L)
							WHILE noExtraPtr AND ii LT N_ELEMENTS(tmpPtrArr) DO BEGIN
								IF NOT WIDGET_INFO(*(tmpPtrArr[ii]),/VALID_ID) THEN BEGIN
									PTR_FREE, tmpPtrArr[ii]
									noExtraPtr	= 0
									ptrIdx		= ii
								ENDIF
								ii = ii + 1
							ENDWHILE

							IF noExtraPtr THEN BEGIN
								PTR_FREE, (*tlbPtr).mwPtr
								(*tlbPtr).mwPtr					=	$
									PTR_NEW(PTRARR(N_ELEMENTS(tmpPtrArr)+1))
								(*((*tlbPtr).mwPtr))[0:N_ELEMENTS(tmpPtrArr)-1] =	$
									tmpPtrArr
								(*((*tlbPtr).mwPtr))[N_ELEMENTS(tmpPtrArr)]	=	$
									PTR_NEW(win)
							ENDIF ELSE BEGIN
								(*((*tlbPtr).mwPtr))[ptrIdx] = PTR_NEW(win)
							ENDELSE
						ENDELSE
					ENDIF
					IF NOT WIDGET_INFO( (*tlbPtr).please_wait_dlg, /VALID_ID ) THEN			$
						(*tlbPtr).please_wait_dlg = Get_Please_Wait_Dlg()
					WIDGET_CONTROL, (*tlbPtr).please_wait_dlg, MAP = 0
					WIDGET_CONTROL, event.top, /SENSITIVE


					IF win GE 0 THEN WIDGET_CONTROL, win, /SHOW

					END

			ENDCASE
			END	; Leaving the 'MISR_DATA_SELECTION' portion of a CASE statement.

		;===============================================================
		; GEOREFIMAGE
		; GEOREFIMAGE
		; GEOREFIMAGE
		; GEOREFIMAGE
		; GEOREFIMAGE
		;===============================================================
		'GEOREFIMAGE': BEGIN
			first_child			= WIDGET_INFO( event.top, /CHILD )
			first_child_of_first_child	= WIDGET_INFO( first_child, /CHILD )
			WIDGET_CONTROL, first_child_of_first_child, GET_UVALUE = currentObj

			IF event.modeChangeFlag									$
				OR ( STRUPCASE(event.mode) EQ 'ZOOM' AND					$
				STRUPCASE(event.widgetType) EQ 'WIDGET_DRAW' AND event.type EQ 0 ) THEN BEGIN
				obj_parent_base	= currentObj->GetParentBase()
				WIDGET_CONTROL, obj_parent_base, GET_UVALUE = basePtr
				; constuct new title.
				newTitle	= (*basePtr).winTitle						$
					+ '  z=' + STRTRIM(STRING(currentObj->GetZoomFactor()),2)		$
					+ '  m=' + event.mode
				; change title.
				WIDGET_CONTROL, obj_parent_base, TLB_SET_TITLE=newTitle
			ENDIF

			obj_base			= (currentObj)->GetParentBase()
			WIDGET_CONTROL, obj_base, GET_UVALUE = infoPtr
			tlbPtr				= (*infoPtr).tlbPtr
			eight_bit_display		= (*tlbPtr).eight_bit_display

			;--------------------------------------------------------------
			; Check to see if the GEOREFIMAGE event needs further processing
			;--------------------------------------------------------------
			CASE event.widgetType OF
				;===============================================================
				; GEOREFIMAGE: GEOREFIMAGEHIST
				; GEOREFIMAGE: GEOREFIMAGEHIST
				; GEOREFIMAGE: GEOREFIMAGEHIST
				; GEOREFIMAGE: GEOREFIMAGEHIST
				; GEOREFIMAGE: GEOREFIMAGEHIST
				;===============================================================
				'GEOREFIMAGEHIST': BEGIN ; implies that the GEOREF_IMAGE StretchDn
					; interface's APPLY button has been pressed.
					;============================================================
					; If the current display is 24-bit, just return
					;============================================================
					IF !D.N_COLORS GT 256 THEN RETURN

					WIDGET_CONTROL, event.top, GET_UVALUE = statePtr
					WIDGET_CONTROL, (*statePtr).obj->GetParentBase(), GET_UVALUE = infoPtr

					IF (*((*(infoPtr)).tlbPtr)).current_display_mode EQ 'privatecolormap' THEN BEGIN
						(*statePtr).obj->DisplayData, /SET_TRANSLATION
						RETURN
					ENDIF ELSE BEGIN
						(*statePtr).obj->DisplayData,						$
							USE_TRANSLATION = {						$
								trans:*((*((*(infoPtr)).tlbPtr)).translationPtr),	$
								r_vec:*((*((*(infoPtr)).tlbPtr)).rvec_ptr),		$
								g_vec:*((*((*(infoPtr)).tlbPtr)).gvec_ptr),		$
								b_vec:*((*((*(infoPtr)).tlbPtr)).bvec_ptr) }
						RETURN
					ENDELSE
					END
				;===============================================================
				; GEOREFIMAGE: WIDGET_TIMER
				; GEOREFIMAGE: WIDGET_TIMER
				; GEOREFIMAGE: WIDGET_TIMER
				; GEOREFIMAGE: WIDGET_TIMER
				; GEOREFIMAGE: WIDGET_TIMER
				;===============================================================
				'WIDGET_TIMER': BEGIN
					IF (*tlbPtr).lastActiveObj EQ currentObj THEN BEGIN
;print,'misr_view WIDGET_TIMER event -- getting color vectors from currentObj'
						r_vec_ptr	= currentObj->EightBit_GetColorVectorPtr(/RED)
						g_vec_ptr	= currentObj->EightBit_GetColorVectorPtr(/GRN)
						b_vec_ptr	= currentObj->EightBit_GetColorVectorPtr(/BLU)
;print,'misr_view WIDGET_TIMER event -- TVLCT, *(r_vec_ptr), *(g_vec_ptr), *(b_vec_ptr)'
;print,'misr_view WIDGET_TIMER event -- *(r_vec_ptr) = ',*(r_vec_ptr)
;print,'misr_view WIDGET_TIMER event -- *(g_vec_ptr) = ',*(g_vec_ptr)
;print,'misr_view WIDGET_TIMER event -- *(b_vec_ptr) = ',*(b_vec_ptr)
						TVLCT, *(r_vec_ptr), *(g_vec_ptr), *(b_vec_ptr)
					ENDIF
					END
				;===============================================================
				; GEOREFIMAGE: WIDGET_DRAW
				; GEOREFIMAGE: WIDGET_DRAW
				; GEOREFIMAGE: WIDGET_DRAW
				; GEOREFIMAGE: WIDGET_DRAW
				; GEOREFIMAGE: WIDGET_DRAW
				;===============================================================
				'WIDGET_DRAW': BEGIN
					;-----------------------------------------------------------
					; If the current display is 8-bit and the current event is a
					; MOTION_EVENT within the current GEOREFIMAGE's WIDGET_DRAW,
					; check the current object reference (the object associated
					; with the WIDGET_DRAW that generated this event) with the
					; lastActiveObj reference.  If these are NOT equal, set off
					; a WIDGET_TIMER event that will go off in a few seconds.  This
					; part of the IF statement will continue to be processed as
					; long as the user moves the mouse around the screen through
					; various GEOREFIMAGE objects.  However, as soon as the user
					; concentrates mouse movement within a SINGLE GEOREFIMAGE
					; object for more than a few seconds, the  IF
					; statement above will be processed (in other words, currentObj
					; will be equal to lastActiveObj).  This will cause the
					; current object's assocaited color table to be loaded.
					;-----------------------------------------------------------
					IF eight_bit_display AND event.type EQ 2 AND (*tlbPtr).lastActiveObj NE currentObj THEN BEGIN
						WIDGET_CONTROL, event.id, TIMER = 1
					ENDIF

					(*tlbPtr).lastActiveObj	= currentObj
					END
				;===============================================================
				; GEOREFIMAGE: WIDGET_BUTTON
				; GEOREFIMAGE: WIDGET_BUTTON
				; GEOREFIMAGE: WIDGET_BUTTON
				; GEOREFIMAGE: WIDGET_BUTTON
				; GEOREFIMAGE: WIDGET_BUTTON
				;===============================================================
				'WIDGET_BUTTON': BEGIN
					WIDGET_CONTROL, event.id, GET_UVALUE = uVal1
					CASE uVal1 OF
						;===============================================
						; GEOREFIMAGE: WIDGET_BUTTON: kill
						; GEOREFIMAGE: WIDGET_BUTTON: kill
						; GEOREFIMAGE: WIDGET_BUTTON: kill
						; GEOREFIMAGE: WIDGET_BUTTON: kill
						; GEOREFIMAGE: WIDGET_BUTTON: kill
						;===============================================
						'kill': WIDGET_CONTROL, event.top, /DESTROY

						'projinfo': BEGIN
							WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
							create_proj_info_window, event.top, infoPtr
							END

						;===============================================
						; GEOREFIMAGE: WIDGET_BUTTON: reprojection
						; GEOREFIMAGE: WIDGET_BUTTON: reprojection
						; GEOREFIMAGE: WIDGET_BUTTON: reprojection
						; GEOREFIMAGE: WIDGET_BUTTON: reprojection
						; GEOREFIMAGE: WIDGET_BUTTON: reprojection
						;===============================================
						'reprojection': BEGIN
							IF NOT PTR_VALID(event.projection_ptr) THEN RETURN
							obj_struct	= *(event.projection_ptr)
							PTR_FREE, event.projection_ptr
							IF obj_struct.cancel_pressed THEN RETURN
;print,'tag_names(obj_struct)=',tag_names(obj_struct)
							;
							; structure is of the form:
							;
							;
							; { rgba_image_data_objarr: lon_image_data_obj: lat_image_data_obj: projname: georef_img_objarr: }
							imgPtrArr	= PTRARR(N_ELEMENTS(obj_struct.(0)))
							FOR i = 0, N_ELEMENTS(obj_struct.(0))-1 DO BEGIN
								IF OBJ_VALID((obj_struct.(0))[i]) THEN BEGIN
									imgPtrArr[i] = PTR_NEW((obj_struct.(0))[i], /NO_COPY)
								ENDIF
							ENDFOR

							(*tlbPtr).uniqueWindowId	= (*tlbPtr).uniqueWindowId + 1L
							;--------------------------------------------------------------
							; GEOREF_IMAGE viewer window title is as follows:
							; w=UNIQUE WINDOW NUMBER
							;--------------------------------------------------------------
							wTitle	= 'w=' + STRTRIM(STRING((*tlbPtr).uniqueWindowId),2) +	$
									' proj=' + STRLOWCASE(STRTRIM(obj_struct.projname,2))
;print,'}}}}}}}}}}}}}}}}}}}}}}}}}} PTR_VALID((*tlbPtr).mwPtr? = ',PTR_VALID((*tlbPtr).mwPtr)
							IF NOT PTR_VALID((*tlbPtr).mwPtr) THEN BEGIN
								;
								; going down this portion of the IF statement should NEVER happen
								; since, to get here, one must have selected the "Reprojection"
								; button from an existing object.
								;
								wTitle	= 'PROJECTED DATA'
							ENDIF ELSE BEGIN
								tmpPtrArr	= *((*tlbPtr).mwPtr)
								win_title_arr	= [ 'TITLES OF ALL SOURCE WINDOWS LISTED BELOW:' ]
;print,'}}}}}}}}}}}}}}}}}}}}}}}}}} N_ELEMENTS(tmpPtrArr) = ',N_ELEMENTS(tmpPtrArr)
								FOR i = 0, N_ELEMENTS(tmpPtrArr) - 1 DO BEGIN
;print,'}}}}}}}}}}}}}}}}}}}}}}}}}} i = ',i
									IF WIDGET_INFO(*(tmpPtrArr[i]),/VALID_ID) THEN BEGIN
										WIDGET_CONTROL, *(tmpPtrArr[i]), GET_UVALUE = basePtr
										obj2check	= (*basePtr).obj
										obj_idx		= WHERE( obj_struct.georef_img_objarr EQ obj2check, obj_cnt )
;print,'}}}}}}}}}}}}}}}}}}}}}}}}}} (*basePtr).winTitle = ',(*basePtr).winTitle
										IF obj_cnt GT 0 THEN win_title_arr = [win_title_arr, (*basePtr).winTitle]
									ENDIF
								ENDFOR
;help,win_title_arr
								win_title_arr = [ win_title_arr, '     ', 'MAP PROJECTION PARAMETERS LISTED BELOW:' ]
								FOR jj = 0, N_ELEMENTS(obj_struct.parameter_listing[0,*]) - 1 DO		$
									win_title_arr = [ win_title_arr, obj_struct.parameter_listing[0,jj] + obj_struct.parameter_listing[1,jj] ]
							ENDELSE

;help,win_title_arr
							win_title_arr = [ win_title_arr, '     ', 'IDL MAP_SET COMMAND USED FOR REPROJECTION:', obj_struct.map_set_string ]

							;--------------------------------------------------------------
							; 8-BIT DISPLAY
							;--------------------------------------------------------------
							 IF (*tlbPtr).eight_bit_display THEN BEGIN
								dws	= {								$
										lon			:obj_struct.lon_image_data_obj,	$
										lat			:obj_struct.lat_image_data_obj,	$
										imgPtrArr		:imgPtrArr,			$
										winTitle		:wTitle,			$
										win_title_arr		:win_title_arr,			$
										infoPtr			:tlbPtr }
								win	=  get_new_projection_window_8bit( DATA_WINDOW_STRUCT = dws )
							ENDIF ELSE BEGIN
							;--------------------------------------------------------------
							; 24-BIT DISPLAY
							;--------------------------------------------------------------
									win	= get_new_projection_window(					$
														obj_struct.lon_image_data_obj,	$
														obj_struct.lat_image_data_obj,	$
														imgPtrArr,			$
														wTitle,				$
														win_title_arr,			$
														tlbPtr )
							ENDELSE

							;--------------------------------------------------------------
							; Phantom MAP_SET to fix the mystery problem that seemed to be
							; caused by instantiating GEOREF_IMAGE, probably as a result of
							; REALIZE'ing a new WIDGET_DRAW in the object.
							;--------------------------------------------------------------
							upper_data_selection_base	= WIDGET_INFO( (*tlbPtr).dataSelectionBase, /CHILD )

							WIDGET_CONTROL, upper_data_selection_base, GET_UVALUE = upperBaseStatePtr

							curWin		= !D.WINDOW
							WSET,(*upperBaseStatePtr).mapDrawWindowID
							MAP_SET,									$
								(*upperBaseStatePtr).mapCenterLatLon[0],				$
								(*upperBaseStatePtr).mapCenterLatLon[1],				$
								0.0,									$
								/NOERASE,								$
								_EXTRA = *((*upperBaseStatePtr).mapSetStruct)
							WSET, curWin

							IF win GE 0 THEN BEGIN
								first_child			= WIDGET_INFO( win, /CHILD )
								first_child_of_first_child	= WIDGET_INFO( first_child, /CHILD )
								WIDGET_CONTROL, first_child_of_first_child, GET_UVALUE = currentObj
								obj_parent_base			= currentObj->GetParentBase()
								WIDGET_CONTROL, win, GET_UVALUE = basePtr
								; constuct new title.
								newTitle			=					$
										(*basePtr).winTitle +					$
										'  z=' +						$
										STRTRIM(STRING(currentObj->GetZoomFactor()),2) +	$
										'  m=' +						$
										currentObj->GetMode()
								; change title.
								WIDGET_CONTROL, win, TLB_SET_TITLE=newTitle

								IF NOT PTR_VALID((*tlbPtr).mwPtr) THEN BEGIN
									(*tlbPtr).mwPtr		= PTR_NEW(PTRARR(1))
									(*((*tlbPtr).mwPtr))[0]	= PTR_NEW(win)
								ENDIF ELSE BEGIN
									tmpPtrArr		= *((*tlbPtr).mwPtr)
									noExtraPtr		= 1
									ii			= 0
									ptrIdx			= (-1L)
									WHILE noExtraPtr AND ii LT N_ELEMENTS(tmpPtrArr) DO BEGIN
										IF NOT WIDGET_INFO(*(tmpPtrArr[ii]),/VALID_ID) THEN BEGIN
											PTR_FREE, tmpPtrArr[ii]
											noExtraPtr	= 0
											ptrIdx		= ii
										ENDIF
										ii = ii + 1
									ENDWHILE

									IF noExtraPtr THEN BEGIN
										PTR_FREE, (*tlbPtr).mwPtr
										(*tlbPtr).mwPtr					=	$
											PTR_NEW(PTRARR(N_ELEMENTS(tmpPtrArr)+1))
										(*((*tlbPtr).mwPtr))[0:N_ELEMENTS(tmpPtrArr)-1] =	$
											tmpPtrArr
										(*((*tlbPtr).mwPtr))[N_ELEMENTS(tmpPtrArr)]	=	$
											PTR_NEW(win)
									ENDIF ELSE BEGIN
										(*((*tlbPtr).mwPtr))[ptrIdx] = PTR_NEW(win)
									ENDELSE
								ENDELSE
							ENDIF
							END
						ELSE:
					ENDCASE
					END
				;===============================================================
				; GEOREFIMAGE: stretchDN
				; GEOREFIMAGE: stretchDN
				; GEOREFIMAGE: stretchDN
				; GEOREFIMAGE: stretchDN
				; GEOREFIMAGE: stretchDN
				;===============================================================
				'stretchDN': BEGIN
					IF event.newRealize THEN BEGIN
						;--------------------------------------------------------------
						; Phantom MAP_SET to fix the mystery problem that seemed to be
						; caused by instantiating GEOREF_IMAGE, probably as a result of
						; REALIZE'ing a new WIDGET_DRAW in the object.
						;--------------------------------------------------------------
						curWin	= !d.window
						wset,(*upperBaseStatePtr).mapDrawWindowID
						map_set, (*upperBaseStatePtr).mapCenterLatLon[0],		$
							(*upperBaseStatePtr).mapCenterLatLon[1], 0.0, /noerase,	$
							_extra=*((*upperBaseStatePtr).mapSetStruct)
						wset,curWin
					ENDIF
					END
				;===============================================================
				; GEOREFIMAGE: STRETCHDNINTERFACEAPPLYBUTTON
				; GEOREFIMAGE: STRETCHDNINTERFACEAPPLYBUTTON
				; GEOREFIMAGE: STRETCHDNINTERFACEAPPLYBUTTON
				; GEOREFIMAGE: STRETCHDNINTERFACEAPPLYBUTTON
				; GEOREFIMAGE: STRETCHDNINTERFACEAPPLYBUTTON
				;===============================================================
				'STRETCHDNINTERFACEAPPLYBUTTON': BEGIN
					current_display_mode	= (*tlbPtr).current_display_mode
					IF NOT eight_bit_display OR current_display_mode NE 'commoncolormap' THEN RETURN

					;-----------------------------------------------------------
					; Get list of ALL references to valid objects
					;-----------------------------------------------------------
					valid_obj_arr	= OBJ_VALID()
					georef_cnt	= 0
					georef_idx	= LONARR(N_ELEMENTS(valid_obj_arr))
					FOR i = 0, N_ELEMENTS(valid_obj_arr) - 1 DO BEGIN
						IF OBJ_CLASS(valid_obj_arr[i]) EQ 'GEOREF_IMAGE' THEN BEGIN
							georef_idx[georef_cnt] = i
							georef_cnt             = georef_cnt + 1
						ENDIF
					ENDFOR

					;-----------------------------------------------------------
					; If there are less than TWO GEOREF_IMAGE objects on the screen,
					; there is no need to do anything
					;-----------------------------------------------------------
					IF georef_cnt LE 1 THEN RETURN

					r_vec_ptr	= currentObj->EightBit_GetColorVectorPtr(/RED)
					g_vec_ptr	= currentObj->EightBit_GetColorVectorPtr(/GRN)
					b_vec_ptr	= currentObj->EightBit_GetColorVectorPtr(/BLU)
					trans_vec_ptr	= currentObj->EightBit_GetTranslationPtr()

					;-----------------------------------------------------------
					; Store the translation pointer for future use
					;-----------------------------------------------------------
					IF PTR_VALID((*infoPtr).translationPtr) THEN PTR_FREE, (*infoPtr).translationPtr
					tmp_trans			= *trans_vec_ptr
					(*infoPtr).translationPtr	= PTR_NEW(tmp_trans,/NO_COPY)

					IF PTR_VALID((*infoPtr).rvec_ptr) THEN PTR_FREE, (*infoPtr).rvec_ptr
					tmp_rvec			= *r_vec_ptr
					(*infoPtr).rvec_ptr		= PTR_NEW(tmp_rvec,/NO_COPY)

					IF PTR_VALID((*infoPtr).gvec_ptr) THEN PTR_FREE, (*infoPtr).gvec_ptr
					tmp_gvec			= *g_vec_ptr
					(*infoPtr).gvec_ptr		= PTR_NEW(tmp_gvec,/NO_COPY)

					IF PTR_VALID((*infoPtr).bvec_ptr) THEN PTR_FREE, (*infoPtr).bvec_ptr
					tmp_bvec			= *b_vec_ptr
					(*infoPtr).bvec_ptr		= PTR_NEW(tmp_bvec,/NO_COPY)

					georef_obj_arr			= valid_obj_arr[georef_idx]
					georef_obj_arr			= georef_obj_arr[WHERE(georef_obj_arr NE currentObj)]

					FOR i = 0, N_ELEMENTS(georef_obj_arr)-1 DO						$
						(georef_obj_arr[i])->DisplayData, USE_TRANSLATION = { trans:*(trans_vec_ptr),	$
							r_vec:*(r_vec_ptr),							$
							g_vec:*(g_vec_ptr),							$
							b_vec:*(b_vec_ptr) }
					END
				ELSE:
			ENDCASE
			END
		;===============================================================
		; WIDGET_BASE
		; WIDGET_BASE
		; WIDGET_BASE
		; WIDGET_BASE
		; WIDGET_BASE
		;===============================================================
		'WIDGET_BASE': BEGIN
			WIDGET_CONTROL, event.id, GET_UVALUE = infoPtr
			newBaseX	= MAX([event.x,(*infoPtr).minBaseXYdims[0]])
			newBaseY	= MAX([event.y,(*infoPtr).minBaseXYdims[1]])
			newObjX		= newBaseX-((*infoPtr).minBaseXYdims[0]-(*infoPtr).minx)
			newObjY		= newBaseY-((*infoPtr).minBaseXYdims[1]-(*infoPtr).miny)

			WIDGET_CONTROL,event.id, XSIZE=newBaseX,YSIZE=newBaseY

			((*infoPtr).obj)->Resize,newBaseX,newBaseY
;
; un-comment this line if resizing the image window becomes problematic
;;;ckt,oct1999			((*infoPtr).obj)->Resize,newObjX,newObjY
			END
		ELSE:
	ENDCASE
END
; misr_view_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ misr_view_kill @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO misr_view_kill, tlb
   WIDGET_CONTROL, tlb, GET_UVALUE = infoPtr

   IF PTR_VALID(infoPtr) THEN BEGIN
      IF PTR_VALID((*infoPtr).mwPtr) THEN BEGIN
         nw = N_ELEMENTS(*((*infoPtr).mwPtr))
         FOR i = 0, nw - 1 DO BEGIN
            IF PTR_VALID((*((*infoPtr).mwPtr))[i]) THEN BEGIN
               IF WIDGET_INFO(*((*((*infoPtr).mwPtr))[i]), /VALID_ID) THEN BEGIN
                  WIDGET_CONTROL, *((*((*infoPtr).mwPtr))[i]), GET_UVALUE = wPtr
                  IF PTR_VALID(wPtr) THEN BEGIN
                     IF OBJ_VALID((*wPtr).obj) THEN OBJ_DESTROY, (*wPtr).obj
                     PTR_FREE, wPtr
                  ENDIF
                  WIDGET_CONTROL, *((*((*infoPtr).mwPtr))[i]), /DESTROY
               ENDIF
               PTR_FREE, (*((*infoPtr).mwPtr))[i]
            ENDIF
         ENDFOR
      ENDIF

      IF (*infoPtr).locationMapBase GE 0L AND WIDGET_INFO((*infoPtr).locationMapBase, /VALID_ID) THEN BEGIN
         WIDGET_CONTROL, (*infoPtr).locationMapBase, GET_UVALUE = wPtr
         OBJ_DESTROY, (*wPtr).obj
         WIDGET_CONTROL, (*infoPtr).locationMapBase, /DESTROY
         PTR_FREE, wPtr
      ENDIF

      IF WIDGET_INFO((*infoPtr).dataSelectionBase,/VALID_ID) THEN BEGIN
         WIDGET_CONTROL,(*infoPtr).dataSelectionBase,MAP=0
         upperBase = WIDGET_INFO((*infoPtr).dataSelectionBase, /CHILD)
         WIDGET_CONTROL, upperBase, GET_UVALUE = statePtr
         result = depointer( statePtr )
         lowerBase = WIDGET_INFO(upperBase, /SIBLING)
         WIDGET_CONTROL, lowerBase, /DESTROY
         WIDGET_CONTROL, (*infoPtr).dataSelectionBase, GET_UVALUE = ptr
         IF PTR_VALID(ptr) THEN PTR_FREE, ptr
      ENDIF

      IF PTR_VALID((*infoPtr).catContentsPtr) THEN PTR_FREE, (*infoPtr).catContentsPtr

      IF PTR_VALID((*infoPtr).translationPtr) THEN PTR_FREE, (*infoPtr).translationPtr
      IF PTR_VALID((*infoPtr).rvec_ptr) THEN PTR_FREE, (*infoPtr).rvec_ptr
      IF PTR_VALID((*infoPtr).gvec_ptr) THEN PTR_FREE, (*infoPtr).gvec_ptr
      IF PTR_VALID((*infoPtr).bvec_ptr) THEN PTR_FREE, (*infoPtr).bvec_ptr
      IF OBJ_VALID((*infoPtr).lastActiveObj) THEN OBJ_DESTROY, (*infoPtr).lastActiveObj
      IF WIDGET_INFO((*infoPtr).please_wait_dlg, /VALID_ID) THEN $
         WIDGET_CONTROL, (*infoPtr).please_wait_dlg, /DESTROY

      PTR_FREE, infoPtr
   ENDIF
END
; misr_view_kill

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ create_splash_screen @@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO create_splash_screen, misr_version, idl_version
	splash_screen = WIDGET_BASE( /COLUMN, TLB_FRAME_ATTR = 1+2+4+8 )
	splash_screen_text = WIDGET_TEXT( splash_screen,		$
		VALUE = [ '','','', '',					$
		'               ' + misr_version + '               ',	$
		'', '',							$
		'        ' + idl_version + '        ',			$
		'', '', '' ],						$
		YSIZE = 11 )
	WIDGET_CONTROL, splash_screen, /REALIZE, /SHOW
	WAIT, 3
	WIDGET_CONTROL, splash_screen, /DESTROY
END
; create_splash_screen

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ vm_used @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

FUNCTION vm_used
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
;		result = DIALOG_MESSAGE( !ERR_STRING )
		CATCH, /CANCEL
		RETURN,1
	ENDIF
	success	= EXECUTE('a=1')
	RETURN, 0

END

; vm_used

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ misr_view @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO misr_view, FONT = font

	full_license	= NOT vm_used()

	; http://www.rsinc.com
	; Tech Tip Article ID: 3257
	IF !VERSION.RELEASE EQ '5.5' AND !VERSION.OS EQ 'linux' THEN BEGIN
		WINDOW, /PIXMAP
		WDELETE
		DEVICE, BYPASS_TRANSLATION = 0
	ENDIF

	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		result = DIALOG_MESSAGE( !ERR_STRING )
		RETURN
	ENDIF

	COMMON MISRVIEWDATA, set_font, default_transform_directory


	IF KEYWORD_SET(font) THEN set_font = 1 ELSE set_font = 0

	CD, CURRENT = default_transform_directory
	default_transform_directory	= STRTRIM( default_transform_directory, 2 )
	IF STRMID(default_transform_directory,STRLEN(default_transform_directory)-1,1) NE GetDirectoryDivider() THEN	$
		default_transform_directory = default_transform_directory + GetDirectoryDivider()

;;;ckt,sep2004	save_version_file	= FINDFILE( 'idl_save_version.sav' )
;print,'save_version_file = ',save_version_file

	idl_save_version	= ''

;help,save_version_file[0]
;;;ckt,sep2004	IF save_version_file[0] NE '' THEN RESTORE, save_version_file[0]

	ancillary_data_file	= FINDFILE( 'ancillary_variables.sav' )
	IF ancillary_data_file[0] EQ '' THEN BEGIN
		msg	= [							$
				'File <ancillary_variables.sav> not found...',	$
				'This file is required to run misr_view.',	$
				'Make sure that this file is located within',	$
				'same directory as the rest of the misr_view',	$
				'files.  If running misr_view on a PC, make',	$
				'sure that you have set your current',		$
				'directory as detailed in the README file.' ]

		res	= DIALOG_MESSAGE( msg, /ERROR )
		RETURN
	ENDIF

	RESTORE, ancillary_data_file[0]

	matching_save_files	= 0
	IF SIZE(time_tag,/TYPE) NE 0 THEN matching_save_files = verify_save_files(time_tag)

	IF NOT matching_save_files THEN BEGIN
		res	= DIALOG_MESSAGE( [					$
			'The 2 save files that are being used for running',	$
			'misr_view were not created for use together.',		$
			'misr_view cannot run using mismatched save files.' ], /ERROR )
		RETURN
	ENDIF

	versionImgPtr	= PTR_NEW(version,/NO_COPY)
	reqImgPtr	= PTR_NEW(req_img,/NO_COPY)
	logoImgPtr	= PTR_NEW(logo,/NO_COPY)

	display_misr_view_logo, misr_version, versionImgPtr, reqImgPtr, logoImgPtr, SECONDS = 5, IDL_SAVE_VERSION = idl_save_version

	elevationMapPtr	= PTR_NEW(elevationMap,/NO_COPY)
	agpPtr		= PTR_NEW(asciiCornerFile,/NO_COPY)

	TRUE			= 1
	FALSE			= 0
	N_RESERVED_COLORS	= 5
	eight_bit_display	= FALSE
	done			= FALSE

	HELP, /DEVICE, OUTPUT = helpInfo

	n_available_colors	= !D.N_COLORS

	IF n_available_colors LE 256 THEN BEGIN
		msg	= [							$
		'misr_view has detected that the current display device',	$
		'currently can only support 8-bit imagery.  This may be',	$
		'due to the following:',					$
		'',								$
		'   (a) the current display device is a true 8-bit display',	$
		'   (b) the current display device is currently configured',	$
		'     to emulate an 8-bit display',				$
		'',								$
		'If (b) is true and the user wishes to reconfigure the',	$
		'display for 24-bit capabilities, misr_view (and IDL) should',	$
		'be exited before the reconfiguration takes place.',		$
		'',								$
		'Otherwise, misr_view will attempt to optimize colors',		$
		'through the use of shared or private color maps; these two',	$
		'modes will be options in the misr_view main menu under the',	$
		'"Controls" menu item (consult the misr_view User"s Guide',	$
		'for more information).  The default option is to share a',	$
		'single color map in order to minimize flashing.  For ',	$
		'higher-quality color maps, use the private color map option.'	$
		]
		ret			= DIALOG_MESSAGE( msg, /INFORMATION )
		eight_bit_display	= TRUE
		n_available_colors	= n_available_colors - N_RESERVED_COLORS
		TVLCT, r_tmp, g_tmp, b_tmp, /GET
;print,'misr_view -- TVLCT,r_tmp,g_tmp,b_tmp'
;print,'misr_view -- r_tmp = ',r_tmp
;print,'misr_view -- g_tmp = ',g_tmp
;print,'misr_view -- b_tmp = ',b_tmp
		TVLCT,r_tmp,g_tmp,b_tmp
	ENDIF

	tlb	= WIDGET_BASE( TITLE = 'MISR_VIEW ' + misr_version, /ROW, KILL_NOTIFY = 'misr_view_kill' )
	;----------------------------------------------------------------
	; The UVALUE of "bt1" is utilized for storing user input for the
	; directory path for opening the catalog or file.
	;----------------------------------------------------------------
	bt1	= WIDGET_BUTTON( tlb, VALUE = 'Controls', /MENU )
	it1	= WIDGET_BUTTON( bt1, VALUE = 'Open MISR Catalog/File...',			$
			UVALUE = 'openCatalogFile' )
	it2	= WIDGET_BUTTON( bt1, VALUE = 'Hide Data Selection Interface',			$
			UVALUE = 'toggleDataSelectionInterface' )
	it3	= WIDGET_BUTTON( bt1, VALUE = 'Show Location Map',				$
			UVALUE = 'toggleLocationMap' )
	it4a	= (-1L)
	it4b	= (-1L)
	it5	= WIDGET_BUTTON( bt1, VALUE = 'Redefine Orbit-Path-Date Initialization',	$
			UVALUE = 'reinitialize o-p-d' )
	current_display_mode									$
		= ''
	IF eight_bit_display THEN BEGIN
		it4  = WIDGET_BUTTON( bt1, VALUE = '8-Bit Color Options', /MENU )
		it4a = WIDGET_BUTTON( it4, VALUE = '> Common Color Map', UVALUE = 'commoncolormap' )
		it4b = WIDGET_BUTTON( it4, VALUE = 'Private Color Maps', UVALUE = 'privatecolormap' )
		current_display_mode = 'commoncolormap'
	ENDIF

	it6	= WIDGET_BUTTON( bt1, VALUE = 'Preferences...', UVALUE = 'preferences' )

	quitButton	= WIDGET_BUTTON( tlb, VALUE = 'Quit', UVALUE = 'quit' )
	helpButton	= WIDGET_BUTTON( tlb, VALUE = 'Help', UVALUE = 'help' )

	WIDGET_CONTROL, tlb, /REALIZE, SENSITIVE = 0

	catContentsPtr	= PTR_NEW(/ALLOCATE_HEAP)

	IF eight_bit_display THEN BEGIN
		tmpArr			= [PTR_NEW(r_tmp),PTR_NEW(g_tmp),PTR_NEW(b_tmp)]
		dataSelectionBase	=					$
;;;jan99, ckt			misr_data_selection( tlb, catContents, /EIGHT_BIT_DISPLAY )
			misr_data_selection( tlb, catContentsPtr, elevationMapPtr, agpPtr, headerSize, /EIGHT_BIT_DISPLAY )
		PTR_FREE, tmpArr
	ENDIF ELSE BEGIN
;;;jan99, ckt		dataSelectionBase	= misr_data_selection( tlb, catContents )
		dataSelectionBase	= misr_data_selection( tlb, catContentsPtr, elevationMapPtr, agpPtr, headerSize )
	ENDELSE

	WIDGET_CONTROL, tlb, /SHOW, SENSITIVE = 1

	WIDGET_CONTROL, tlb, SET_UVALUE = PTR_NEW(					$
			{ dataSelectionBase	:dataSelectionBase,		$
				misr_version		:misr_version,			$
;;;jan99, ckt				  catContents		:catContents,		$
				  catContentsPtr	:catContentsPtr,		$
				  catFilePtr		:PTR_NEW(),			$
				  file_only		:0,				$
				  locationMapBase	:(-1L),				$
				  dataSelectToggleID	:it2,				$
				  locationToggleID	:it3,				$
				  eight_bit_display	:eight_bit_display,		$
				  commonID		:it4a,				$
				  privateID		:it4b,				$
				  n_available_colors	:n_available_colors,		$
				  current_display_mode	:current_display_mode,		$
				  translationPtr	:PTR_NEW(),			$
				  rvec_ptr 		:PTR_NEW(),			$
				  gvec_ptr		:PTR_NEW(),			$
				  bvec_ptr		:PTR_NEW(),			$
				  n_reserved_colors	:N_RESERVED_COLORS,		$
				  please_wait_dlg	:Get_Please_Wait_Dlg(),		$
				  lastActiveObj		:OBJ_NEW(),			$
				  idl_save_version	:idl_save_version,		$
				  locImg		:locImg,			$
				  agpPtr		:agpPtr,			$
				  headerSize		:headerSize,			$
				  elevationMapPtr	:elevationMapPtr,		$
				  versionImgPtr		:versionImgPtr,			$
				  reqImgPtr		:reqImgPtr,			$
				  logoImgPtr		:logoImgPtr,			$
				  uniqueWindowId	:0L,				$
				  mwPtr			:PTR_NEW(),			$
				INITIAL_VALUES_STRUCT_PTR : PTR_NEW()			$
				 }, /NO_COPY )

	XMANAGER, 'MISR_VIEW', tlb, EVENT_HANDLER = 'misr_view_eh'
END
; misr_view
