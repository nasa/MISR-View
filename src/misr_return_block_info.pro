@misr_get_meta_info.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ misr_return_block_info @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION misr_return_block_info, filename
	e_flag	= 0
	e_msg	= ['']
	;=======================================================================
	; NOTE (ADDED 12/01/2000 by ckt
	;
	; This area of the code has been revised multiple times in order to
	; accomodate MISR products with and without "fill blocks".  The
	; latest revision to the code determines the actual number of 
	; blocks with "real" data as follows:
	;
	; The metadata "Start_block" is retrieved; if found, the variables
	; "first_good_block" and "blockStart" are set to the value of "Start_block".
	; "Start_block" is defined as the value of the first block which contains
	; "real data" in the path/orbit.
	;
	; The metadata "Block_number" is retrieved; "Block_number" is defined
	; as an array with each element representing a block number to which
	; it corresponds, EVEN IF THE CORRESPONDING BLOCK IS A FILL BLOCK.
	; HOWEVER, IT IS ALSO TRUE THAT "Block_number" MAY BE SET TO 0 FOR
	; CORRESPONDING BLOCKS THAT ARE FILL BLOCKS.
	;
	; The three examples below represent the current scenarios for
	; the values of "Start_block", "End block", and "Block_number":
	;
	; EXAMPLE 1: File containing fill blocks for blocks 1-4 and "real data"
	; for blocks 5-9
	;
	; Start_block	= 5
	; End block	= 9
	; Block_number	= [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
	;
	; EXAMPLE 2: Same file as Example 1, but with NO FILL BLOCKS!
	;
	; Start_block	= 5
	; End block	= 9
	; Block_number	= [ 5, 6, 7, 8, 9 ]
	;
	; EXAMPLE 3: Same file as Example 1, but with "Block_number"
	; set to 0 for fill blocks
	;
	; Start_block	= 5
	; End block	= 9
	; Block_number	= [ 0, 0, 0, 0, 5, 6, 7, 8, 9 ]
	;
	; misr_view will determine the location of the "real data" blocks
	; in the file as follows (this should work with all of the scenarios above):
	;
	; (1) Obtain the value for "Start_block" (scalar)
	; (2) Obtain the value for "Block_number" (array)
	; (3) Subtract one from the value of "Start_block" so that it can
	;     be used as an index into "Block_number"
	; (4) 
	;=======================================================================
	

	;=======================================================================
	; Grab Start_block
	;=======================================================================
	ptr		= misr_get_meta_info( filename,	METADATA_NAME = 'Start_block' )
	
	IF LONG(TOTAL(PTR_VALID(ptr))) NE N_ELEMENTS(ptr) THEN BEGIN
		PTR_FREE, ptr
		e_flag	= 1
		e_msg	= [ 'Could not find metadata START_BLOCK' ]
		RETURN, { error_encountered:e_flag, error_msg:e_msg }
	ENDIF
	
	misr_start_block	= *(ptr[0])
	PTR_FREE, ptr

	;=======================================================================
	; Grab End block (should be End_block, so check for both with and without
	; underscore
	;=======================================================================
	ptr		= misr_get_meta_info( filename, METADATA_NAME = 'End block' )
						      
	IF LONG(TOTAL(PTR_VALID(ptr))) NE N_ELEMENTS(ptr) THEN			$
		ptr	= misr_get_meta_info( filename, METADATA_NAME = 'End_block' )
						      
	IF LONG(TOTAL(PTR_VALID(ptr))) NE N_ELEMENTS(ptr) THEN BEGIN
		PTR_FREE, ptr
		e_flag	= 1
		e_msg	= [ 'Could not find metadata END_BLOCK' ]
		RETURN, { error_encountered:e_flag, error_msg:e_msg }
	ENDIF
						      
	misr_end_block		= *(ptr[0])
	PTR_FREE, ptr
	
	;=======================================================================
	; Grab Block_number
	;=======================================================================
	ptr		= misr_get_meta_info( filename,				$
				METADATA_NAME = 'PerBlockMetadataCommon',	$
				METADATA_FIELDS = 'Block_number' )
						      
	IF LONG(TOTAL(PTR_VALID(ptr))) NE N_ELEMENTS(ptr) THEN BEGIN
		PTR_FREE, ptr
		e_flag	= 1
		e_msg	= [							$
				'Could not find field BLOCK_NUMBER',		$
				'in metadata PERBLOCKMETADATACOMMON' ]
		RETURN, { error_encountered:e_flag, error_msg:e_msg }
	ENDIF
	
	misr_block_number	= *(ptr[0])
	PTR_FREE, ptr
	
	start_block_idx		= misr_start_block - 1
	max_block_number_idx	= N_ELEMENTS(misr_block_number) - 1
	
	CASE 1 OF
		start_block_idx GT max_block_number_idx: BEGIN
			misr_fill_blocks	= 0
			END
		misr_block_number[start_block_idx] NE misr_start_block: BEGIN
			misr_fill_blocks	= 0
			END
		misr_block_number[start_block_idx] EQ misr_start_block: BEGIN
			misr_fill_blocks	= start_block_idx
			END
		ELSE: BEGIN
			e_flag	= 1
			e_msg	= [							$
					'misr_view has encountered metadata values',	$
					'for Start_block, End_block, and Block_number',	$
					'for which the actual location of blocks of',	$
					'real data cannot be determined.' ]
			RETURN, { error_encountered:e_flag, error_msg:e_msg }
			END
	ENDCASE
	
	RETURN, {								$
			error_encountered	: e_flag,			$
			error_msg		: e_msg,			$
			start_block		: misr_start_block,		$
			end_block		: misr_end_block,		$
			fill_blocks		: misr_fill_blocks }
	
END
; misr_return_block_info
