;+ ------------------------------------------------
; gamma_colorbar.pro
; March 2001 Jeffrey.R.Hall@jpl.nasa.gov
; Jet Propulsion Laboratory, California Insitute of Technology
; Thanks for the help: David Fanning.
;- ------------------------------------------------
PRO gamma_colorbar,	DATATYPE = datatype,		$
			POSITION = position,		$
			BOTTOM = bottom,		$
			NCOLORS = ncolors,		$
			RANGE = range,			$
			DIVISIONS = divisions,		$
			CHARSIZE = charsize,		$
			GAMMA = gamma

;;for testing.
;;window,/free,ysize=128

;print,''
;print,'***** entering gamma_colorbar *****'
	IF N_ELEMENTS(datatype)		EQ 0 THEN datatype	= 1
	IF N_ELEMENTS(position)		EQ 0 THEN position	= [0.10, 0.30, 0.90, 0.95]
	IF N_ELEMENTS(bottom)		EQ 0 THEN bottom	= 0
	IF N_ELEMENTS(ncolors)		EQ 0 THEN ncolors	= !D.N_COLORS
	IF N_ELEMENTS(range)		EQ 0 THEN range		= [0,255]
	IF N_ELEMENTS(divisions)	EQ 0 THEN divisions	= 6
	IF N_ELEMENTS(gamma)		EQ 0 THEN gamma		= 1.0

	ncolors		= MIN( [ ncolors, 256, !D.N_COLORS ] )

	axis		= ( DINDGEN( divisions + 1 ) / divisions ) * ( range[1] - range[0] ) + range[0]
;print,'range = ',range
;print,'divisions = ',divisions
;help,axis
;print,'min(axis),max(axis) = ',min(axis),max(axis)

	wedge_width	= (position[2]-position[0]) * !D.X_SIZE + 1
	wedge_height	= (position[3]-position[1]) * !D.Y_SIZE + 1
	wedge		= ( DINDGEN( wedge_width ) / ( wedge_width - 1 ) ) # REPLICATE( 1, wedge_height )	; size
	wedge		= BYTSCL( wedge ^ ( 1.0 / gamma ), TOP = ncolors - bottom - 1 ) + BYTE( bottom )	; range
;print,'gamma = ',gamma
;print,'ncolors = ',ncolors
;print,'bottom = ',bottom
;print,'min(wedge),max(wedge) = ',min(wedge),max(wedge)
;save,wedge,filename='wedge.sav'
;print,'position[0], position[1]=',position[0], position[1]
	ERASE, bottom
	TV, wedge, position[0], position[1], /NORMAL
;cb1=tvrd()
;save,cb1,filename='cb1.sav'
;curwin=!d.window
;window,/free,xsize=512,ysize=128,title='cbtest'
;TV, wedge, position[0], position[1], /NORMAL
;wset,curwin

	ints		= [1,2,3,12,13,14,15]
	IF (WHERE( datatype EQ ints ))[0] NE -1 THEN BEGIN
;print,''
;print,'integer plot'
;print,axis
;help,axis

		;--------------------------------------------------
		; Integer axis annotation.
		;--------------------------------------------------
;		dummydata	= HISTOGRAM( axis )
		dummydata	= HISTOGRAM( RANDOMU( 1, 1000 ), BINSIZE = .0001 )
;help,dummydata

		;--------------------------------------------------
		; Modify dummydata because PLOT will crash otherwise.
		;--------------------------------------------------
		IF TOTAL( dummydata ) MOD N_ELEMENTS( dummydata ) EQ 0 THEN					$
			dummydata[0] = dummydata[0] + 1

		PLOT, axis, dummydata,										$
			/NODATA, /NOERASE, POSITION = position, CHARSIZE = charsize, COLOR = !P.COLOR,		$
			XTICKFORMAT = '(I5)', XSTYLE = 1, XTICKS = divisions, XMINOR = 2, XTICKLEN = 0.2,	$
			YTICKFORMAT = '(A1)', YSTYLE = 1, YTICKS = 1

	ENDIF ELSE BEGIN
;print,''
;print,'float plot'
;print,'axis = ',axis
;help,axis

		;--------------------------------------------------
		; Floating point axis annotation.  Charsize is set
		; to make the characters effectively invisible by
		; being so tiny as to not show up even as 1 pixel,
		; or probably that pixel is plotted on top of the
		; axis line, making the character invisible.
		; The AXIS command will draw the characters.
		;--------------------------------------------------
		dummydata	= HISTOGRAM( RANDOMU( 1, 1000 ), BINSIZE = .0001 )
;help,dummydata
		dummycharsize	= 0.001
		PLOT, axis, dummydata,										$
			/NODATA, /NOERASE, POSITION = position, CHARSIZE = dummycharsize, COLOR = !P.COLOR,	$
			XTICKFORMAT = '(I5)', XSTYLE = 1, XTICKS = divisions, XMINOR = 2, XTICKLEN = 0.2,	$
			YTICKFORMAT = '(A1)', YSTYLE = 1, YTICKS = 1

		;--------------------------------------------------
		; Set up dummy POSITION such that AXIS can draw the
		; characters slightly lower (than the default) so
		; that the superscripts of scientific notation don't
		; overlap into the wedge area.
		;--------------------------------------------------
		curwin		= !D.WINDOW
		WINDOW, XSIZE = !D.X_SIZE, YSIZE = !D.Y_SIZE, /PIXMAP
		dummywindow	= !D.WINDOW
		dummyposition	= position
		dummyposition[1]= position[1] - 0.08
		PLOT, axis, dummydata, POSITION = dummyposition, XSTYLE = 1
		WDELETE, dummywindow
		WSET, curwin

		AXIS, 0, 0, XAX=0, /DATA, XTICKV = axis, XTICKS = divisions, CHARSIZE = charsize, COLOR = !P.COLOR, XTICKLAYOUT = 1

	ENDELSE
;cb2=tvrd()
;save,cb2,filename='cb2.sav'
;print,'***** leaving gamma_colorbar *****'
;print,''
END
