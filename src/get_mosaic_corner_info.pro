;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ eimp_GDBlockSOMCoordCompute  @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION eimp_GDBlockSOMCoordCompute,					$
					first_block,			$
					last_block,			$
					ulc,				$
					lrc,				$
					blocksize,			$
					offsets
					
	TOTAL_NUMBER_BLOCKS	= N_ELEMENTS(offsets.offset) + 1
					
	blocksize_meters	= {					$
					x	:lrc[0]-ulc[0],		$
					y	:ulc[1]-lrc[1]}
					
	pixelsize		= {					$
					x	:blocksize_meters.x /	$
							blocksize.line,	$
					y	:blocksize_meters.y /	$
							blocksize.sample}
							
	coord			= REPLICATE(				$
						{			$
						ulc	:{x:0.0,y:0.0},	$
						lrc	:{x:0.0,y:0.0},	$
						dim	:{x:0.0,y:0.0}	$
						}, TOTAL_NUMBER_BLOCKS )
						
	;=======================================================================
	; Note: Is the Ulc_first_block and Lrc_first_block arrays correct
	; This is probably problem in the HDF-EOS grid file and a PFR
	; should be filed.  We are looking into this.
	;=======================================================================
	coord[0].ulc.x		= ulc[0]
	coord[0].ulc.y		= lrc[1]	; Questionable ?
	
	;=======================================================================
	; coord[0].ulc.y = ulc[1] Apparently wrong.
	;=======================================================================
	coord[0].lrc.x		= lrc[0]
	coord[0].lrc.y		= ulc[1]	; Questionable ?
	;=======================================================================
	; coord[0].lrc.y = lrc[1]; Apparently wrong.
	;=======================================================================

	;=======================================================================
	; The Ulc_first_block and Lrc_first_block are in terms of pixel
	; corners, not centers, as required by Erdas Imagine.
	; The following shifts the ulc and lrc coordinates by half of
	; the pixel size to center the coordinates.
	;=======================================================================

	coord[0].ulc.x		= coord[0].ulc.x + pixelsize.x / 2.0
	coord[0].ulc.y		= coord[0].ulc.y + pixelsize.y / 2.0

	coord[0].lrc.x		= coord[0].lrc.x + pixelsize.x / 2.0
	coord[0].lrc.y		= coord[0].lrc.y + pixelsize.y / 2.0

	coord[0].dim.x		= coord[0].lrc.x - coord[0].ulc.x
	coord[0].dim.y		= coord[0].lrc.y - coord[0].ulc.y

	left_most_block_index	= 0
	right_most_block_index	= 0
						
	FOR i = 1, TOTAL_NUMBER_BLOCKS-1 DO BEGIN

		;===============================================================
		; Determine upper left corner coordinates in meters.
		;===============================================================
		coord[i].ulc.x	= coord[i-1].ulc.x + blocksize_meters.x
		coord[i].ulc.y	= coord[i-1].ulc.y + (pixelsize.y * offsets.offset[i-1])

		;===============================================================
		; Determine lower right corner coordinates in meters.
		;===============================================================
		coord[i].lrc.x	= coord[i-1].lrc.x + blocksize_meters.x
		coord[i].lrc.y	= coord[i-1].lrc.y + (pixelsize.y * offsets.offset[i-1])

		;===============================================================
		; Determine size of block in meters.
		;===============================================================
		coord[i].dim.x	= coord[i].lrc.x - coord[i].ulc.x
		coord[i].dim.y	= coord[i].lrc.y - coord[i].ulc.y

		;===============================================================
		; Determine the left most block with respect to entire swath.
		;===============================================================
		IF (coord[i].ulc.y LT					$
			coord[left_most_block_index].ulc.y) THEN	$
			left_most_block_index	= i

		;===============================================================
		; Determine the right most block with respect to entire swath.
		;===============================================================

		IF (coord[i].lrc.y GT					$
			coord[right_most_block_index].lrc.y) THEN	$
			right_most_block_index	= i
	ENDFOR
								
	RETURN, {							$
			first_block	:first_block,			$
			last_block	:last_block,			$
			number_block	:last_block-first_block+1,	$
			pixelsize	:pixelsize,			$
			coord		:coord,				$
			left_most_block_index				$
					:left_most_block_index,		$
			right_most_block_index				$
					:right_most_block_index }
			
			
END
; eimp_GDBlockSOMCoordCompute

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ eimp_GDSwathSOMCoordCompute @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION eimp_GDSwathSOMCoordCompute,					$
					first_block,			$
					last_block,			$
					ulc,				$
					lrc,				$
					blocksize,			$
					offsets

	block_som			=				$
			eimp_GDBlockSOMCoordCompute(			$
							first_block,	$
							last_block,	$
							ulc,		$
							lrc,		$
							blocksize,	$
							offsets )
							
	coord			= {				$
					ulc	:{x:0.0,y:0.0},	$
					lrc	:{x:0.0,y:0.0},	$
					dim	:{x:0.0,y:0.0} }
						
	first_block			= block_som.first_block
	last_block			= block_som.last_block
	number_block			= block_som.number_block

	left_most_block_index		= first_block - 1
	right_most_block_index		= first_block - 1

	pixelsize			= block_som.pixelsize

	FOR i = first_block - 1, last_block - 2 DO BEGIN
		;===============================================================
		; Determine the left most block.
		;===============================================================
		IF (							$
			block_som.coord[ i ].ulc.y LT			$
			block_som.coord[left_most_block_index].ulc.y )	$
			THEN left_most_block_index	= i

		;===============================================================
		; Determine the right most block.
		;===============================================================
		IF (							$
			block_som.coord[ i ].lrc.y LT			$
			block_som.coord[right_most_block_index].lrc.y )	$
			THEN right_most_block_index	= i
	ENDFOR

	;=======================================================================
	; Convert first and last block to block index for clarity.
	;=======================================================================
	first_block_index	= first_block - 1
	last_block_index	= last_block - 1

	;=======================================================================
	; Determine coordinates of the blocks bounded by first_block and
	; last_block. 
	;=======================================================================
	coord.ulc.x	= block_som.coord[first_block_index].ulc.x
	coord.ulc.y	= block_som.coord[left_most_block_index].ulc.y
	coord.lrc.x	= block_som.coord[last_block_index].lrc.x
	coord.lrc.y	= block_som.coord[right_most_block_index].lrc.y

	coord.dim.x	= coord.lrc.x - coord.ulc.x
	coord.dim.y	= coord.lrc.y - coord.ulc.y

	RETURN, {							$
			first_block	:first_block,			$
			last_block	:last_block,			$
			number_block	:block_som.number_block,	$
			pixelsize	:pixelsize,			$
			coord		:coord,				$
			left_most_block_index				$
					:left_most_block_index,		$
			right_most_block_index				$
					:right_most_block_index }

END
; eimp_GDSwathSOMCoordCompute

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ get_mosaic_corner_info @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION get_mosaic_corner_info,					$
	fn,								$
	grid_name,							$
	field_name,							$
	block_start,							$
	block_end,							$
	act_res
	
	file_id		= EOS_GD_OPEN( fn, /READ )
	grid_id		= EOS_GD_ATTACH( file_id, grid_name )
	iStat		= EOS_GD_GRIDINFO(				$
				grid_id,				$
				line,					$
				sample,					$
				ulc,					$
				lrc )
	
	iStat		= EOS_GD_DETACH( grid_id )
	iStat		= EOS_GD_CLOSE( file_id )
	
	
	ptr		= misr_get_meta_info(				$
				fn,					$
				METADATA_NAME =				$
					'PerBlockMetadataCommon',	$
				METADATA_FIELDS =			$
					'Block_coor_lrc_som_meter.y' )
					
	;==================================================================
	; y refers to cross-track direction
	;==================================================================
	act_offsets		= (*(ptr[0]))
	act_offsets_pix		= ROUND( (				$
		act_offsets[1:N_ELEMENTS(act_offsets)-1] -		$
		act_offsets[0:N_ELEMENTS(act_offsets)-2] ) / act_res )
	
	PTR_FREE, ptr
	
	;==================================================================
	;Compute swath coordinates in SOM.
	;==================================================================
	blocksize	= { line:line, sample:sample }
	offsets		= { offset:act_offsets_pix }
	swath_som	= eimp_GDSwathSOMCoordCompute(			$
							block_start,	$
							block_end,	$
							ulc,		$
							lrc,		$
							blocksize,	$
							offsets )
	RETURN, swath_som
END
; get_mosaic_corner_info
