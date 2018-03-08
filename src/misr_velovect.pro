;==================================================
;=========== misr_velovect_subroutine =============
;==================================================
PRO misr_velovect_subroutine,		$
	u		= u,		$
	v		= v,		$
	lon_idx		= lon_idx,	$
	lat_idx		= lat_idx,	$
	_EXTRA		= extra

	;--------------------------------------------------
	; VELOVECT trims the longer window dimension to 
	; equal the shorter dimension.  Account for this in 
	; the MAP_SET LIMIT.
	;--------------------------------------------------
	IF !d.x_size LT !d.y_size THEN BEGIN
		xlim		= 1.0
		ylim		= MIN( [FLOAT(!d.y_size)/!d.x_size,FLOAT(!d.x_size)/!d.y_size] )
	ENDIF ELSE BEGIN
		xlim		= MIN( [FLOAT(!d.y_size)/!d.x_size,FLOAT(!d.x_size)/!d.y_size] )
		ylim		= 1.0
	ENDELSE

	;--------------------------------------------------
	; Set up the plotting space for VELOVECT.
	;--------------------------------------------------
	MAP_SET,0.0,0.0,0.0,			$
		LIMIT=[0.0,0.0,xlim,ylim ],	$
		/CYLINDRICAL,			$
		/ISOTROPIC,			$
		/NOBORDER,			$
		XMARGIN=0.0,YMARGIN=0.0,	$
		/NOERASE

	;--------------------------------------------------
	; Draw the arrows.
	;--------------------------------------------------
	VELOVECT, u, v, lon_idx, lat_idx, /OVERPLOT, _EXTRA = extra
end

;==================================================
;=========== misr_velovect ========================
;==================================================
PRO misr_velovect

;--------------------------------------------------
; INCOMING DATA.
;--------------------------------------------------
nblocks		= 2
blocks		= BYTARR(512,128,nblocks)+128
xoffset		= [0,63]
yoffset		= [0,127]
u		= [ [2,2,2,2,2,2,2,2], [2,2,2,2,2,2,2,2] ]
v		= u
lon_idx		= INDGEN(8)*64+32
lat_idx		= [32,96]

;--------------------------------------------------
; Create a window the exact size needed.
;--------------------------------------------------
WINDOW,											$
	XSIZE = TOTAL(xoffset[WHERE(xoffset NE 0)]+1)+N_ELEMENTS(blocks[*,0,0]),	$
	YSIZE = TOTAL(yoffset[WHERE(yoffset NE 0)]+1)+N_ELEMENTS(blocks[0,*,0]),	$
	/FREE

;--------------------------------------------------
; TV all the blocks first so that the arrows may 
; protrude beyond the block boundry without being 
; partially erased by the next block being TV'ed.
; Then draw all the arrows in a second FOR loop.
;--------------------------------------------------
FOR i=0,nblocks-1 DO TV, blocks[*,*,i], xoffset[i], yoffset[i]
FOR i=0,nblocks-1 DO	$
	misr_velovect_subroutine,									$
		u		= u,									$
		v		= v,									$
		lon_idx		= ( lon_idx + xoffset[i] ) / FLOAT( MAX( [!d.x_size,!d.y_size] ) ),	$
		lat_idx		= ( lat_idx + yoffset[i] ) / FLOAT( MAX( [!d.x_size,!d.y_size] ) ),	$
		_extra		= {COLOR:0,DOTS:0,LENGTH:.3,MISSING:9999}

END
