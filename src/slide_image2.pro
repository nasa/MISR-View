; $Id: slide_image2.pro,v 1.13 2000/01/21 00:29:33 scottm Exp $
;
; Copyright (c) 1991-2000, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	SLIDE_IMAGE2	; SLIDE_IMAGE"2" -- modified for misr_view 4.0 2001 (IDL 5.4).
;			; Added save as tiff button, dialog_pickfile_wrapper, and basic error catch.
;
; PURPOSE:
;	Create a scrolling graphics window for examining large images.
;	By default, 2 draw widgets are used.  The left draw widget shows
;	a reduced version of the complete image, while the draw widget on
;	the right displays the actual image with scrollbars that allow sliding
;	the visible window.
;
; CALLING SEQUENCE:
;	SLIDE_IMAGE2 [, Image]
;
; INPUTS:
;	Image:	The 2-dimensional image array to be displayed.  If this 
;		argument is not specified, no image is displayed. The 
;		FULL_WINDOW and SCROLL_WINDOW keywords can be used to obtain 
;		the window numbers of the 2 draw widgets so they can be drawn
;		into at a later time.
;
; KEYWORDS:
;      CONGRID:	Normally, the image is processed with the CONGRID
;		procedure before it is written to the fully visible
;		window on the left. Specifying CONGIRD=0 will force
;		the image to be drawn as is.
;
;  FULL_WINDOW:	A named variable in which to store the IDL window number of \
;		the non-sliding window.  This window number can be used with 
;		the WSET procedure to draw to the scrolling window at a later
;		point.
;
;	GROUP:	The widget ID of the widget that calls SLIDE_IMAGE2.  If this
;		keyword is specified, the death of the caller results in the
;		death of SLIDE_IMAGE2.
;
;	BLOCK:  Set this keyword to have XMANAGER block when this
;		application is registered.  By default the Xmanager
;               keyword NO_BLOCK is set to 1 to provide access to the
;               command line if active command 	line processing is available.
;               Note that setting BLOCK for this application will cause
;		all widget applications to block, not only this
;		application.  For more information see the NO_BLOCK keyword
;		to XMANAGER.
;
;	ORDER:	This keyword is passed directly to the TV procedure
;		to control the order in which the images are drawn. Usually,
;		images are drawn from the bottom up.  Set this keyword to a
;		non-zero value to draw images from the top down.
;
;     REGISTER:	Set this keyword to create a "Done" button for SLIDE_IMAGE2
;		and register the widgets with the XMANAGER procedure.
;
;		The basic widgets used in this procedure do not generate
;		widget events, so it is not necessary to process events
;		in an event loop.  The default is therefore to simply create
;		the widgets and return.  Hence, when register is not set, 
;		SLIDE_IMAGE2 can be displayed and the user can still type 
;		commands at the "IDL>" prompt that use the widgets.
;
;	RETAIN:	This keyword is passed directly to the WIDGET_DRAW
;		function, and controls the type of backing store
;		used for the draw windows.  If not present, a value of
;		2 is used to make IDL handle backing store.
;
; SLIDE_WINDOW:	A named variable in which to store the IDL window number of 
;		the sliding window.  This window number can be used with the 
;		WSET procedure to draw to the scrolling window at a later 
;		time.
;
;	TITLE:	The title to be used for the SLIDE_IMAGE2 widget.  If this
;		keyword is not specified, "Slide Image" is used.
;
;	TOP_ID:	A named variable in which to store the top widget ID of the 
;		SLIDE_IMAGE2 hierarchy.  This ID can be used to kill the 
;		hierarchy as shown below:
;
;			SLIDE_IMAGE2, TOP_ID=base, ...
;			.
;			.
;			.
;			WIDGET_CONTROL, /DESTROY, base
;
;	XSIZE:	The maximum width of the image that can be displayed by
;		the scrolling window.  This keyword should not be confused 
;		with the visible size of the image, controlled by the XVISIBLE
;		keyword.  If XSIZE is not specified, the width of Image is 
;		used.  If Image is not specified, 256 is used.
;
;     XVISIBLE:	The width of the viewport on the scrolling window.  If this 
;		keyword is not specified, 256 is used.
;
;	YSIZE:	The maximum height of the image that can be displayed by
;		the scrolling window.  This keyword should not be confused 
;		with the visible size of the image, controlled by the YVISIBLE
;		keyword.  If YSIZE is not present the height of Image is used.
;		If Image is not specified, 256 is used.
;
;     YVISIBLE:	The height of the viewport on the scrolling window. If
;		this keyword is not present, 256 is used.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	Widgets for displaying a very large image are created.
;	The user typically uses the window manager to destroy
;	the window, although the TOP_ID keyword can also be used to
;	obtain the widget ID to use in destroying it via WIDGET_CONTROL.
;
; RESTRICTIONS:
;	Scrolling windows don't work correctly if backing store is not 
;	provided.  They work best with window-system-provided backing store
;	(RETAIN=1), but are also usable with IDL provided backing store 
;	(RETAIN=2).
;
;	Various machines place different restrictions on the size of the
;	actual image that can be handled.
;
; MODIFICATION HISTORY:
;	7 August, 1991, Written by AB, RSI.
;	10 March, 1993, ACY, Change default RETAIN=2
;	23 Sept., 1994  KDB, Fixed Typo in comments. Fixed error in
;			Congrid call. xvisible was used instead of yvisible.
;-

pro SLIDE_IMG2_EVENT, ev
COMPILE_OPT hidden



;	SLIDE_IMAGE2	; SLIDE_IMAGE"2" -- modified for misr_view 4.0 2001 (IDL 5.4).
;			; Added save as tiff button, dialog_pickfile_wrapper, and basic error catch.





	;=======================================================================
	; basic error catch mechanism, generally for file read or write when 
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== SLIDE_IMG2_EVENT =========='
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

  
  widget_control, ev.id, get_uvalue = widget_name
 print,'widget_name = ',widget_name
  case strupcase(strtrim(widget_name,2)) OF
  	'DISMISS': widget_control, ev.top, /DESTROY
  	'SAVEASTIFF': begin
;;;ckt,apr2001  		filename=dialog_pickfile(title='Enter the name of the output file')
  		filename=dialog_pickfile_wrapper(title='Enter the name of the output file')
  		IF STRTRIM(filename,2) EQ '' THEN BEGIN
  			res	= DIALOG_MESSAGE( ['No filename specified... ignoring save command...'], /INFORMATION)
  			RETURN
  		ENDIF
  		WIDGET_CONTROL, /HOURGLASS
  		widget_control,ev.top,get_uvalue=s
  		wset, s.slide_window_id
  		if s.nbands le 1 then begin
  			IF !D.N_COLORS GT 256 THEN BEGIN
  				WRITE_TIFF,									$
  						filename,							$
  						RED   = REVERSE( TVRD( CHANNEL = 1 ), 2),			$
  						GREEN = REVERSE( TVRD( CHANNEL = 2 ), 2),			$
  						BLUE  = REVERSE( TVRD( CHANNEL = 3 ), 2),			$
  						1,								$
  						PLANARCONFIG = 2
  			ENDIF ELSE BEGIN
  				TVLCT,r,g,b,/GET
  				IF s.vec_index LE 0 THEN BEGIN
  					r[1:N_ELEMENTS(r)-1]	= r[0:N_ELEMENTS(r)-2]
   					g[1:N_ELEMENTS(r)-1]	= g[0:N_ELEMENTS(r)-2]
  					b[1:N_ELEMENTS(r)-1]	= b[0:N_ELEMENTS(r)-2]
  					r[0]			= s.rgb_color[0]
  					g[0]			= s.rgb_color[1]
  					b[0]			= s.rgb_color[2]
 				ENDIF ELSE BEGIN
 					res	= DIALOG_MESSAGE(['slide_image2: unable to handle vec_index equal to anything greater than 0'],/INFORMATION)
 					RETURN
  				ENDELSE
 				WRITE_TIFF, filename, REVERSE(tvrd(),2), 1, RED = r, GREEN = g, BLUE = b
 			ENDELSE
 		endif else begin
  			WRITE_TIFF,									$
  					filename,							$
  					RED   = REVERSE( TVRD( CHANNEL = 1 ), 2),			$
  					GREEN = REVERSE( TVRD( CHANNEL = 2 ), 2),			$
  					BLUE  = REVERSE( TVRD( CHANNEL = 3 ), 2),			$
  					1,								$
  					PLANARCONFIG = 2
  		endelse
  		end
  	'DRAWTHING':
  	ELSE:
  endcase

;  WIDGET_CONTROL, ev.top, /DESTROY
end







pro slide_image2, image, CONGRID=USE_CONGRID, ORDER=ORDER, REGISTER=REGISTER, $
	RETAIN=RETAIN, SHOW_FULL=SHOW_FULL, SLIDE_WINDOW=SLIDE_WINDOW, $
	XSIZE=XSIZE, XVISIBLE=XVISIBLE, YSIZE=YSIZE, YVISIBLE=YVISIBLE, $
	TITLE=TITLE, TOP_ID=BASE, FULL_WINDOW=FULL_WINDOW, GROUP = GROUP, $
	BLOCK=block,N_BANDS = nb,VEC_COLOR=vec_color,VEC_INDEX = vec_idx

  SWIN = !D.WINDOW

  if (n_params() ne 0) then begin
    image_size = SIZE(image)
    if (image_size[0] ne 2) then message,'Image must be a 2-D array'
    if (n_elements(XSIZE) eq 0) then XSIZE = image_size[1]
    if (n_elements(YSIZE) eq 0) then YSIZE = image_size[2]
  endif else begin
    image_size=bytarr(1)
    if (n_elements(XSIZE) eq 0) then XSIZE = 256
    if (n_elements(YSIZE) eq 0) then YSIZE = 256
  endelse
  if (n_elements(xvisible) eq 0) then XVISIBLE=256
  if (n_elements(Yvisible) eq 0) then YVISIBLE=256
  if(n_elements(SHOW_FULL) eq 0) THEN SHOW_FULL = 1
  if(not KEYWORD_SET(ORDER)) THEN ORDER = 0
  if(not KEYWORD_SET(USE_CONGRID)) THEN USE_CONGRID = 1
  if(n_elements(RETAIN) eq 0) THEN RETAIN = 2
  if(n_elements(TITLE) eq 0) THEN TITLE='Slide Image'
  if(not KEYWORD_SET(REGISTER)) THEN REGISTER = 0
  IF N_ELEMENTS(block) EQ 0 THEN block=0
  
  IF KEYWORD_SET(nb) THEN nbands = nb ELSE nbands = 3
  IF KEYWORD_SET(vec_color) THEN BEGIN
	rgb_color = BYTE([ vec_color and 255L, ishft( vec_color, -8 ) and 255L, ishft( vec_color, -16 ) and 255L ])
  ENDIF ELSE BEGIN
	rgb_color = [ 255b, 255b, 255b ]
  ENDELSE
  IF KEYWORD_SET(vec_idx) THEN vec_index = vec_idx ELSE vec_index = 0

  if (REGISTER OR BLOCK) then begin
    base = WIDGET_BASE(title=title, GROUP = GROUP, /COLUMN, TLB_FRAME_ATTR = 9 )
    bbase = WIDGET_BASE(base,/ROW, /ALIGN_CENTER, /BASE_ALIGN_CENTER )
    dismiss = WIDGET_BUTTON(bbase,VALUE = 'Dismiss', UVALUE = 'dismiss' )
    saveastiff = WIDGET_BUTTON(bbase,VALUE = 'Save As TIFF...', UVALUE = 'saveastiff' )
    ibase = WIDGET_BASE(base, /ROW)
  endif else begin
    base = WIDGET_BASE(title=title, GROUP = GROUP, /column, TLB_FRAME_ATTR = 9)
    bbase = WIDGET_BASE(base,/ROW, /ALIGN_CENTER, /BASE_ALIGN_CENTER )
    dismiss = WIDGET_BUTTON(bbase,VALUE = 'Dismiss', UVALUE = 'dismiss' )
    saveastiff = WIDGET_BUTTON(bbase,VALUE = 'Save As TIFF...', UVALUE = 'saveastiff' )
   ibase = base
  endelse
  ; Setting the managed attribute indicates our intention to put this app
  ; under the control of XMANAGER, and prevents our draw widgets from
  ; becoming candidates for becoming the default window on WSET, -1. XMANAGER
  ; sets this, but doing it here prevents our own WSETs at startup from
  ; having that problem.
  WIDGET_CONTROL, /MANAGED, base

  if (SHOW_FULL) then begin
      fbase = WIDGET_BASE(ibase, /COLUMN, /FRAME)
        junk = WIDGET_LABEL(fbase, value='Full Image')
        all = widget_draw(fbase,retain=retain,xsize=xvisible,ysize=yvisible)
      sbase = WIDGET_BASE(ibase, /COLUMN, /FRAME)
        junk = WIDGET_LABEL(sbase, value='Full Resolution')
        scroll = widget_draw(sbase, retain=retain,xsize=xsize,ysize=ysize, $
		/scroll, x_scroll_size=xvisible, y_scroll_size=yvisible)
    WIDGET_CONTROL, /REAL, base
    WIDGET_CONTROL, get_value=FULL_WINDOW, all
  endif else begin
    scroll = widget_draw(ibase, retain=retain, xsize=xsize, ysize=ysize, $
	/frame, /scroll, x_scroll_size=xvisible, y_scroll_size=yvisible,uvalue='drawthing')
    WIDGET_CONTROL, /REAL, base
    FULL_WINDOW=-1
  endelse

  WIDGET_CONTROL, get_value=SLIDE_WINDOW, scroll
  
  widget_control,base,set_uvalue={nbands:nbands,slide_window_id:SLIDE_WINDOW, rgb_color:rgb_color,vec_index:vec_index}

  ; Show the image(s) if one is present
  if (image_size[0] ne 0) then begin
    if (SHOW_FULL) then begin
      WSET, FULL_WINDOW
      if (use_congrid) then begin
	TV, congrid(image, xvisible, yvisible), ORDER=ORDER
      endif else begin
	TV, image, ORDER=ORDER
      endelse
    endif
    WSET, SLIDE_WINDOW
    TV, image, ORDER=ORDER
  endif
  if (n_elements(group) eq 0) then group=base
  WSET, SWIN

  if (REGISTER OR BLOCK) then $
    XMANAGER, 'SLIDE_IMAGE2', base, event='SLIDE_IMG2_EVENT', $
	NO_BLOCK=(NOT(FLOAT(block)))

end
