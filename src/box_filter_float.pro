FUNCTION box_filter_float, img, boxWidth, lowGoodValue, highGoodValue, pct_of_good_pixels_in_box

	TRUE		= 1
	FALSE		= 0
	type_code	= SIZE(img,/TYPE)
	IDL_DATA_TYPE_ARRAY	= [								$
					'UNDEFINED',						$
					'BYTE',							$
					'FIX',							$
					'LONG',							$
					'FLOAT',						$
					'DOUBLE',						$
					'NOT SUPPORTED',					$
					'STRING',						$
					'NOT SUPPORTED',					$
					'NOT SUPPORTED',					$
					'NOT SUPPORTED',					$
					'NOT SUPPORTED',					$
					'UINT',							$
					'ULONG',						$
					'LONG64',						$
					'ULONG64' ]
					
	IF N_PARAMS() LE 4 THEN pct_of_good_pixels_in_box = 0.50

	sz		= SIZE( img )
	width		= sz( 1 )
	height		= sz( 2 )
	
	tmpImg		= DBLARR( width + ( 2 * ( boxWidth / 2 ) ), height + ( 2 * ( boxWidth / 2 ) ) )
	tmpImg( ( boxWidth / 2 ):( boxWidth / 2 ) + width - 1, ( boxWidth / 2 ):( boxWidth / 2 ) + height - 1 ) = $
		img

	outImg		= DBLARR( width, height )
	
	xIndex		= ROTATE( L64INDGEN( height, width ) / height, 3 ) + ( boxWidth / 2 )
	yIndex		= L64INDGEN( width, height ) / width + ( boxWidth / 2 )

	values2Average	= DBLARR( width, height )
	cntImg		= LON64ARR( width, height )

	startBoxVal	= ( ( boxWidth ) / 2 ) * ( -1 )
	endBoxVal	= startBoxVal * ( -1 )
	
	FOR x = startBoxVal, endBoxVal DO BEGIN
		FOR y = startBoxVal, endBoxVal DO BEGIN

			goodIndices1 = WHERE( ( ( xIndex + x ) GE ( boxWidth / 2 ) ) AND									$
					      ( ( xIndex + x ) LE ( width + ( boxWidth / 2 ) - 1 ) ) AND							$
					      ( ( yIndex + y ) GE ( boxWidth / 2 ) ) AND									$
					      ( ( yIndex + y ) LE ( height + ( boxWidth / 2 ) - 1) ) AND							$
					      ( tmpImg( xIndex + x, yIndex + y ) GE lowGoodValue AND tmpImg( xIndex + x, yIndex + y ) LE highGoodValue ) AND	$
					      ( tmpImg( xIndex, yIndex ) LT lowGoodValue OR tmpImg( xIndex, yIndex ) GT  highGoodValue ) )

			sz	= SIZE( goodIndices1 )
			IF ( sz( 0 ) EQ 0 ) THEN GOTO, SKIP
				
			xInx		= goodIndices1 MOD width
			yInx		= goodIndices1 / width

			values2Average( xInx, yInx )	= values2Average( xInx, yInx ) + img( xInx + x, yInx + y )
			cntImg( xInx, yInx )		= cntImg( xInx, yInx ) + 1L
SKIP:

		ENDFOR
	ENDFOR

;	inx1		= WHERE( cntImg NE 0L )
;	sz		= SIZE( inx1 )
;	IF ( sz( 0 ) EQ 0 ) THEN RETURN, img
	
n_box_pixels			= boxWidth*boxWidth-1
n_required_good_box_pixels	= ROUND(pct_of_good_pixels_in_box * FLOAT(n_box_pixels))
inx1				= WHERE( cntImg GT n_required_good_box_pixels, cnt1 )
IF cnt1 LE 0 THEN BEGIN

;=========== replace the EXECUTE ================
;ckt,aug2004	str2exec	= 'tmp='+IDL_DATA_TYPE_ARRAY[type_code]+'(img)'
;ckt,aug2004	success		= EXECUTE(str2exec)
;================================================
	
;=========== replacement code ===================
	tmp		= CALL_FUNCTION(IDL_DATA_TYPE_ARRAY[type_code],img)
;================================================
	
	RETURN, tmp
ENDIF

	xInx1		= inx1 MOD width
	yInx1		= inx1 / width
	
	NOTinx		= WHERE( cntImg LE n_required_good_box_pixels, cnt )
	IF cnt LE 0 THEN BEGIN
	
;=========== replace the EXECUTE ================
;ckt,aug2004		str2exec	= 'tmp='+IDL_DATA_TYPE_ARRAY[type_code]+'(values2Average( xInx1, yInx1 ) / FLOAT( cntImg( xInx1, yInx1 ) ))'
;ckt,aug2004		success		= EXECUTE(str2exec)
;================================================

;=========== replacement code ===================
		tmp	= CALL_FUNCTION(IDL_DATA_TYPE_ARRAY[type_code],values2Average[xInx1,yInx1]/FLOAT(cntImg[xInx1,yInx1]))
;================================================

		RETURN, tmp
	ENDIF
		
	NOTxInx		= NOTinx MOD width
	NOTyInx		= NOTinx / width
	
	outImg( xInx1, yInx1 )		= values2Average( xInx1, yInx1 ) / DOUBLE( cntImg( xInx1, yInx1 ) )
	outImg( NOTxInx, NOTyInx )	= img( NOTxInx, NOTyInx )

;=========== replace the EXECUTE ================
;ckt,aug2004	str2exec	= 'tmp='+IDL_DATA_TYPE_ARRAY[type_code]+'(outImg)'
;ckt,aug2004	success		= EXECUTE(str2exec)
;================================================

;=========== replacement code ===================
	tmp	= CALL_FUNCTION(IDL_DATA_TYPE_ARRAY[type_code],outImg)
;================================================
	
	RETURN, tmp
	
END
; box_filter_float
