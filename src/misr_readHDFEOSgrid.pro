;01234567890123456789012345678901234567890123456789012345678901234567891
;+
;==========================================================================
;
;Module Name:	misr_readHDFEOSgrid
;
;Call Protocol:	grid = misr_readHDFEOSgrid(				$
;				fileName,				$
;				gridName,				$
;				fieldName,				$
;				BLOCK_START = blockStart,		$
;				N_BLOCKS = nBlocks,			$
;				DATA_RES_ACT_METERS = actResM,		$
;				DATA_RES_ALT_METERS = altResM,		$
;				DATA_TYPE = dataType,			$
;				REVERSE_BLOCK_ORDER = reverseOrder,	$
;				RETURN_MOSAIC_ONLY = returnMosaic )
;
;==========================================================================
;
;		Jet Propulsion Laboratory (JPL)
;
;		Multi-angle Imaging Spectro-Radiometer (MISR)
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
;		Catherine Moroney	catherine@pampero.atmo.arizona.edu
;
;===========================================================================
;
;Description:
;
;	This module is designed to read MISR data stored in the HDF-EOS GRID
;	format.  For more information on MISR HDF-EOS specifics, please
;	consult http://www-misr.jpl.nasa.gov
;
;Input Parameters:
;
;	Type	Name		Units		Purpose
;	-------------------------------------------------------------------
;	STRING	fileName		fully-qualified HDF-EOS file name
;	STRING	gridName		name of HDF-EOS grid
;					(e.g., 'RedBand')
;	STRING	fieldName		name of field within GRID to
;					retrieve (e.g., 'Radiance')
;
;Keywords:
;
;	INTEGER	BLOCK_START		starting block to read grid data
;	INTEGER	N_BLOCKS		# of contiguous blocks of grid
;					data to read
;	FLOAT	DATA_RES_ACT_METERS	output across-track res. of grid
;					data, in meters
;	FLOAT	DATA_RES_ALT_METERS	output along-track res. of grid
;					data, in meters
;	STRING	DATA_TYPE		grid data type (e.g., 'INT16',
;					'INT32', 'FLOAT32', 'FLOAT64' )
;	INTEGER	REVERSE_BLOCK_ORDER	if specified, output grid data will
;					be 'flipped' along the x-axis
;	INTEGER	RETURN_MOSAIC_ONLY	if specified, returned grid, if
;					composed of multiple blocks, will
;					be mosaicked.  See OUTPUT below.
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
;	If the keyword RETURN_MOSAIC_ONLY is NOT set, the following
;	structure is returned:
;	STRUCTURE	{ blocks:, offsetX:, offsetY: }
;		where:
;			blocks	= stack of "n" blocks of grid number type,
;				  with dimensions ( n, nSamples, nLines )
;			offsetX	= array of x-offset values for each returned
;				  block
;			offsetY = array of y-offset values for each returned
;				  block
;
;	If the keyword RETURN_MOSAIC_ONLY IS set, ONLY a mosiacked image is
;	returned, the same number type as the grid number type
;
;Known Bugs and Limitations:
;
;Parent(s):
;
;	<parents, if any>
;
;Routines Called:
;
;	hdf_gd_interface.pro (separate package)
;	misr_readOffset (included in this module)
;	openGridData (included in this module)
;	misr_readHDFEOSgrid (included in this module)
;
;Files Accessed:
;
;	<files accessed, if any>
;
;Revision History:
;
;	JAN-98 (CKT) - fixed bug with block offsets in x-direction
;	FEB-98 (CKT) - added resolution keywords
;	JAN-99 (CKT) - incorporated EOS_GD routines
;	JAN-99 (CKT) - fixed "looping error catching" mechanism
;	JAN-99 (CKT) - changed header references to appropriate MISR
;			documentation
;	JAN-99 (CKT) - completely reworked the block fetching mechanism
;	JAN-99 (CKT) - removed requirement to explicitly declare data type
;		       (DATA_TYPE keyword removed)
;	JAN-99 (CKT) - removed INVERSE_DATA keyword
;	JAN-99 (CKT) - added new IDL data types (unsigned short, long, 64 integer)
;	JAN-99 (CKT) - enabled DATA_RES_ACT_METERS and DATA_RES_ALT_METERS
;	JAN-99 (CKT) - removed routine that returned total number of blocks in file
;	MAR-99 (CKT) - fixed bug which flipped SOMBlockDim dimension for a single block
;	MAR-99 (CKT) - switched to using STRIDE and EDGE instead of CONGRID to change x-y dimensions
;
;Notes:
;
;
;=============================================================================
;-
;
@misr_return_block_info.pro
@misr_return_fill_values.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ get_misr_fill_values @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION get_misr_fill_values, file_name, grid_name, field_name, NUM_TYPE = num_type
	;
	; The keyword NUM_TYPE is only being used until the fill values
	; are implemented within the MISR product files
	;
	IF KEYWORD_SET(num_type) THEN BEGIN
;
; numbers can be added in any order and can be duplicated (sort below resolves this)
;
all_missing_data_values	= DOUBLE([ 2E14-1D, 2E14-2D, 2E14-3D, 2E14-4D, 2E14-5D, 2E14-6D, $
				65535, 65523, 65515, 65511, 16380, 16379, 16378, 16377, 255, 253, 99, 0, $
				-21, -99, -111, -222, -333, -444, -555, -999, -9999, -32768 ] )
;print,'num_type = ',num_type

all_missing_data_values = all_missing_data_values[UNIQ(all_missing_data_values,SORT(all_missing_data_values))]
missing_values2exclude	= [ 255, 99, -99, -111 ]
snum_type=['none','byte','fix','long','float','double','n/a','n/a','n/a','n/a','n/a','n/a','uint','ulong','long64','ulong64']
		CASE num_type OF
			;
			;
			;-----------------------------
			; BYTE
			;-----------------------------
			1: BEGIN & min = 0B & max = 255B & END
			;
			;
			;-----------------------------
			; SHORT INTEGER
			;-----------------------------
			2: BEGIN & min = 2^15 & max = 2^15-1 & END
			;
			;
			;-----------------------------
			; LONG INTEGER
			;-----------------------------
			3: BEGIN & min = 2l^31l & max =2l^31l-1L  & END
			;
			;
			;-----------------------------
			; FLOAT
			;-----------------------------
			4: BEGIN & min = 2.0^31.0 & max = 2.0^31.0-1.0 & END
			;
			;
			;-----------------------------
			; UNSIGNED SHORT INTEGER
			;-----------------------------
			12: BEGIN & min = 0u & max = 2u^16u-1u & END
			;
			;
			;-----------------------------
			; UNSIGNED LONG INTEGER
			;-----------------------------
			13: BEGIN & min = 0ul & max = 2ul^32ul-1ul & END
			;
			;
			;-----------------------------
			; 64-BIT INTEGER
			;-----------------------------
			14: BEGIN & min = 0 & max = 0 & END
			;
			;
			;-----------------------------
			; UNSIGNED 64-BIT INTEGER
			;-----------------------------
			15: BEGIN & min = 0 & max = 0 & END
			;
			;
			;----------------------------------------------------------
			; OTHER DATA TYPE, RETURN DOUBLE-PRECISION FLOAT VALUES
			;----------------------------------------------------------
			ELSE: BEGIN & min = 0 & max = 0 & END
		ENDCASE


		;
		; Although the data values below are tagged as missing above, do not exclude them,
		; as it causes problems for GEOREF_IMAGE's display/histogram routines.  However, these
		; values have been used as flag values in some versions of MISR data products
		;
		IF min EQ max THEN BEGIN
			idx	=  LINDGEN(N_ELEMENTS(all_missing_data_values))
		ENDIF ELSE BEGIN
			idx	= WHERE( all_missing_data_values GE min AND	$
					 all_missing_data_values LE max, cnt )
		ENDELSE


returnVal	= CALL_FUNCTION(STRTRIM(snum_type[num_type],2),all_missing_data_values[idx])

;;;ckt,aug2004		str2exec	= 'returnVal='+STRTRIM(snum_type[num_type],2)+'(all_missing_data_values[idx])'
;;;ckt,aug2004		success		= EXECUTE(str2exec)





		RETURN, returnVal[ where2( returnVal, missing_values2exclude, /INVERSE ) ]
	ENDIF

	;---------------------------------------------------------------
	;
	;
	;
	;	GEORECTIFIED RADIANCE PRODUCT (GRP)
	;
	;
	;
	;---------------------------------------------------------------

	IF STRPOS( STRUPCASE(file_name), 'GRP_ELLIPSOID' ) GE 0 THEN BEGIN
		CASE 1 OF
			STRPOS( STRUPCASE(field_name), 'RADIANCE' ) GE 0:	$
				fill_values	= [ 16378, 16380 ]
			ELSE:
		ENDCASE
	ENDIF

	IF STRPOS( STRUPCASE(file_name), 'GRP_TERRAIN' ) GE 0 THEN BEGIN
		CASE 1 OF
			STRPOS( STRUPCASE(field_name), 'RADIANCE' ) GE 0:	$
				fill_values	= [ 16377, 16378, 16379, 16380 ]
			ELSE:
		ENDCASE
	ENDIF

	IF STRPOS( STRUPCASE(file_name), 'GP_GMP' ) GE 0 THEN BEGIN
		fill_values	= [ -9999.0 ]
	ENDIF

	IF STRPOS( STRUPCASE(file_name), 'GRP_RCCM' ) GE 0 THEN BEGIN
	ENDIF


	;---------------------------------------------------------------
	;
	;
	;
	;	TOP-OF-ATMOSPHERE (TOA)/CLOUD PRODUCT
	;
	;
	;
	;---------------------------------------------------------------

	IF STRPOS( STRUPCASE(file_name), 'TC' ) GE 0 THEN BEGIN
		fill_values	= [ -9999.0, 9999.0 ]
	ENDIF


	;---------------------------------------------------------------
	;
	;
	;
	;	LEVEL 2 AEROSOL/SURFACE PRODUCT
	;
	;
	;
	;---------------------------------------------------------------

	IF STRPOS( STRUPCASE(file_name), 'AS_AEROSOL' ) GE 0 THEN BEGIN
	ENDIF

	IF STRPOS( STRUPCASE(file_name), 'AS_LANDSFC1' ) GE 0 THEN BEGIN
	ENDIF

	IF STRPOS( STRUPCASE(file_name), 'AS_LANDSFC2' ) GE 0 THEN BEGIN
	ENDIF

	IF STRPOS( STRUPCASE(file_name), 'AS_LANDSFC3' ) GE 0 THEN BEGIN
	ENDIF

	IF STRPOS( STRUPCASE(file_name), 'AS_OCEANSFC' ) GE 0 THEN BEGIN
	ENDIF

	;---------------------------------------------------------------
	;
	;
	;
	;	Ancillary Geographic Product (AGP)
	;
	;
	;
	;---------------------------------------------------------------

	IF STRPOS( STRUPCASE(file_name), 'AGP' ) GE 0 THEN BEGIN
		fill_values	= [ -9999.0 ]
	ENDIF

	RETURN, fill_values

END
; get_misr_fill_values

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ misr_readOffset @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION misr_readOffset, fileName, startBlock, endBlock, first_good_block, last_good_block, fill_blocks

;-----------------------------------------------------------------------
; function which returns the x-offset values for each block to be read,
; for the purpose of mosaicking together the blocks properly
;
;	INPUT PARAMETERS:
;		fileName	- fully-qualified name of HDF-EOS file
;		startBlock	- starting block within file to consider
;		endBlock	- ending block in file to consider
;
;	RETURNED VALUES:
;		{bad_read:FALSE, x_position:, x_offset:, x_pad:, n_blocks }
;			where:
;				x_position 	= array containing global
;						  x-position of blocks
;				x_offset	= array containing relative
;						  x-position of blocks
;				x_pad		= difference between min and
;						  max x-position values
;						  for the current blocks
;				n_blocks	= number of blocks read in
;
;		{bad_read:TRUE} - on error
;-----------------------------------------------------------------------

	routineName	= '----- misr_readOffset -----'
	returnMsg	= 'Returning...'
	TRUE		= 1
	FALSE		= 0

	;----------------------------
	; basic error catch mechanism
	;----------------------------
	CATCH, errorStatus
	IF errorStatus NE 0 THEN BEGIN
		eIndex	= 'Error Index:' + 				$
				STRTRIM( STRING(errorStatus), 2 )
		eMsg	= 'Error Message:' + !ERR_STRING
		res	= DIALOG_MESSAGE(				$
				[ routineName,				$
				  eIndex,				$
				  eMsg,					$
				  returnMsg ],				$
				  /ERROR )
		RETURN, { bad_read:TRUE }
	ENDIF

	ptr		= misr_get_meta_info( fileName,					$
					      METADATA_NAME = 'PerBlockMetadataCommon',	$
					      METADATA_FIELDS = 'Block_coor_lrc_som_meter.y' )

	nBlocks		= endBlock - startBlock + 1

;help,*(ptr[0])
;print,'startBlock,nBlocks = ',startBlock,nBlocks
;;;ckt,feb2000	xPosition	= (*(ptr[0]))[startBlock-1:startBlock+nBlocks-2]
;;;ckt,mar2000	xPosition	= (*(ptr[0]))[startBlock-first_good_block:startBlock-first_good_block+nBlocks-1]
;;;ckt,may2000	xPosition	= (*(ptr[0]))[startBlock-(first_good_block-fill_blocks):startBlock-(first_good_block-fill_blocks)+nBlocks-1]
;;;ckt,jan2001	xPosition	= (*(ptr[0]))[startBlock-1:startBlock+nBlocks-2]
	xPosition	= (*(ptr[0]))[startBlock-(first_good_block-fill_blocks):startBlock-(first_good_block-fill_blocks)+nBlocks-1]
	PTR_FREE, ptr

	;-------------------------------------------
	; locate the min and max x-offset values
	; and set xOffset to be a relative position
	; array of values
	;-------------------------------------------
	xMin	= MIN( xPosition, MAX = xMax )
	xPad	= ABS( xMax - xMin )
	xOffset	= xPosition - xMax
;print,''
;print,''
;print,''
;print,'==============================================='
;print,'xOffset = ', xOffset
;print,'xPosition = ', xPosition
;print,'xPad = ',xPad
;print,'nBlocks = ',nBlocks
;print,'==============================================='
;print,''
;print,''
;print,''
	RETURN, { 	bad_read	:FALSE,				$
			x_position	:xPosition,			$
			x_offset	:xOffset,			$
			x_pad		:xPad,				$
			n_blocks	:nBlocks }

END	;misr_readOffset

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ openGridData @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION openGridData, fileName,					$
		       gridName,					$
		       fieldName,					$
		       GRID_ID = gridID,				$
		       DIMS = dims,					$
		       DIM_NAMES = dimNames,                            $
		       HDFEOSFILE_ID = hdfeosID

;-----------------------------------------------------------------------
; function which returns the x-offset values for each block to be read,
; for the purpose of mosaicking together the blocks properly
;
;	INPUT PARAMETERS:
;		fileName	- fully-qualified name of HDF-EOS file
;		gridName	- name of grid to read from
;		fieldName	- name of field to retrieve
;	KEYWORDS:
;		GRID_ID		- long value of grid ID
;		DIMS		- dimensions of the block data ( nBlocks,
;				  nSamples, nLines)
;               DIM_NAMES       - string names of each dimension
;		SD_ID		- HDF ID for scientific data
;
;	RETURNED VALUES:
;		success		- successful completion
;		failure		- on error
;-----------------------------------------------------------------------
	FIELD_SEPARATOR	= ','
	routineName	= '----- openGridData -----'
	returnMsg	= 'Returning...'

	MAX_DIM		= 100
	success		= 1
	failure		= 0

	;----------------------------
	; basic error catch mechanism
	;----------------------------
	CATCH, errorStatus
	IF errorStatus NE 0 THEN BEGIN
		eIndex	= 'Error Index:' + STRTRIM( STRING(errorStatus), 2 )
		eMsg	= 'Error Message:' + !ERR_STRING
		res	= DIALOG_MESSAGE(				$
				[ routineName,				$
				  eIndex,				$
				  eMsg,					$
				  returnMsg ],				$
				/ERROR )

		RETURN, failure
	ENDIF

	fieldList	= STRARR( MAX_DIM )
	rank		= LONARR( MAX_DIM )
	numberType	= LONARR( MAX_DIM )

	;-----------------------------
	; open file up as HDF-EOS file
	;-----------------------------

	hdfeosID	= EOS_GD_open( fileName )

	IF hdfeosID LT 0 THEN BEGIN

        	eMsg	= 'Problem with EOS_GD_open'

		res	= DIALOG_MESSAGE(				$
				[ routineName,				$
				  eMsg,					$
				  fileName,				$
				  returnMsg ],				$
				/ERROR )
		RETURN, failure
	ENDIF

	gList		= ''
	strBufSz	= 0L

	;--------------------------------------------------------
	; inquire as to the number of grids contained within file
	;--------------------------------------------------------

	nGrid		= EOS_GD_inqgrid( fileName, gList, LENGTH = strBufSz )
	IF nGrid LT 0 THEN BEGIN

		eMsg	= 'Problem with EOS_GD_inqgrid'

		res	= DIALOG_MESSAGE(				$
				[ routineName,				$
				  eMsg,					$
				  fileName,				$
				  returnMsg ],				$
				/ERROR )
		RETURN, failure
	ENDIF

	;--------------------------------------------------------
	; attach to the grid of interest within file
	;--------------------------------------------------------
	gridID		= EOS_GD_attach( hdfeosID, gridName )

	;--------------------------------------------------------
	; inquire as to the number of fields within the current
	; grid
	;--------------------------------------------------------

	nField		= EOS_GD_INQFIELDS(gridID, tmp_fList, rank, numberType )
	fieldList	= STR_SEP( tmp_fList, FIELD_SEPARATOR )
	IF nField LT 0 THEN BEGIN

		eMsg	= 'Problem with EOS_GD_inqfields'

		res	= DIALOG_MESSAGE(				$
				[ routineName,				$
				  eMsg,					$
				  fileName,				$
				  returnMsg ],				$
				/ERROR )
		RETURN, failure
	ENDIF

	;--------------------------------------------------------
	; inquire as to the rank, dimensions, and data type of
	; current field
	;--------------------------------------------------------
	iStat		= EOS_GD_FIELDINFO( 			$
					    gridID,		$
					    fieldName,		$
					    rank,		$
					    dims,		$
					    nType,		$
					    dimNames )

	dimNames	= STR_SEP( dimNames, FIELD_SEPARATOR )
	dims		= dims[0:N_ELEMENTS(dimNames)-1]

	IF iStat LT 0 THEN BEGIN
		eMsg	= 'Problem with EOS_GD_fieldinfo'

		res	= DIALOG_MESSAGE(				$
				[ routineName,				$
				  eMsg,					$
				  fileName,				$
				  returnMsg ],				$
				/ERROR )
		RETURN, failure
	ENDIF

	;--------------------------------------------------------
	; return success flag if all inquiries pan out
	;--------------------------------------------------------
	RETURN, success
END ;openGridData

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ misr_readHDFEOSgrid @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION misr_readHDFEOSgrid,						$
			fileName,					$
			gridName,					$
			fieldName,					$
			BLOCK_START = blockStart,			$
			N_BLOCKS = nBlocks,				$
			DATA_RES_ACT_METERS = actRes,			$
			DATA_RES_ALT_METERS = altRes,			$
			RETURN_MOSAIC_ONLY = returnMosaicImageOnly,     $
			MISSING_DATA_VALUES = missing_data,		$
			EXTRA_DIMS = extraDims,				$
			AIRMISR_DATA = airMisrData

;------------------------------------------------------------------
; see header of this file for a description of this function
;------------------------------------------------------------------

	routineName	= '----- misr_readHDFEOSgrid -----'
	returnMsg	= 'Returning...'

	TRUE			= 1
	FALSE			= 0
	BASE_SAMPLES_PER_275M	= 2048
	BASE_LINES_PER_275M	= 512

	fill_block_offset	= 0L

	;----------------------------
	; basic error catch mechanism
	;----------------------------
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

		RETURN, (-1)
	ENDIF

	;-------------------------------------------
	; must be at least three parameters specified
	;-------------------------------------------
	IF N_PARAMS() LT 3 THEN BEGIN
		eMsg	= 'Need to specify file name, ' +		$
				'grid name, field name'
		res	= DIALOG_MESSAGE(				$
				[ routineName,				$
				  eMsg,					$
				  returnMsg ],				$
				/ERROR )
		RETURN, (-1L)
	ENDIF

	;-----------------------------------------------------------------------
	;-----------------------------------------------------------------------
	;-----------------------------------------------------------------------
	;-----------------------------------------------------------------------
	;-----------------------------------------------------------------------
	; MISR Grid Data Processing
	;-----------------------------------------------------------------------
	;-----------------------------------------------------------------------
	;-----------------------------------------------------------------------
	;-----------------------------------------------------------------------
	;-----------------------------------------------------------------------
	IF NOT KEYWORD_SET(airMisrData) THEN BEGIN

		block_info_struct	= misr_return_block_info( filename )
		IF block_info_struct.error_encountered THEN BEGIN
			res	= DIALOG_MESSAGE( block_info_struct.error_msg, /ERROR )
			RETURN, (-1)
		ENDIF

		first_good_block	= block_info_struct.start_block
		last_good_block		= block_info_struct.end_block
		fill_blocks		= block_info_struct.fill_blocks
		totalNBlocks		= last_good_block - first_good_block + 1

		IF NOT KEYWORD_SET( blockStart ) THEN  blockStart = first_good_block
		IF NOT KEYWORD_SET( nBlocks ) THEN nBlocks = last_good_block - first_good_block + 1

		bad_block_info	= 0

		IF blockStart GT last_good_block THEN BEGIN
			eMsg		= 'Block start value > # blocks in file:'
			bad_block_info	= 1
		ENDIF

		IF (blockStart+nBlocks-1) GT last_good_block THEN BEGIN
			eMsg	= 'Block start + # blocks > total blocks in file:'
			bad_block_info	= 1
		ENDIF

		IF (blockStart LE 0) OR (nBlocks LE 0) THEN BEGIN
			eMsg	= 'Bad value(s) for block start, n_blocks'
			bad_block_info	= 1
		ENDIF

		IF blockStart LT first_good_block THEN BEGIN
			eMsg	= 'Bad value for block start...'
			bad_block_info	= 1
		ENDIF

		IF bad_block_info THEN BEGIN
			b_range		=					$
					'   BLOCK NUMBER RANGE: ' +		$
					STRTRIM(STRING(first_good_block),2) +	$
					' to ' +				$
					STRTRIM(STRING(last_good_block),2)
			max_blocks	=					$
					'   MAXIMUM NUMBER OF BLOCKS: ' +	$
					STRTRIM(STRING(last_good_block-first_good_block+1),2)

			e_msg	= [						$
					'Valid values for block selection are',	$
					'as follows:',				$
					'',					$
					b_range,				$
					max_blocks ]

			res	= DIALOG_MESSAGE( [				$
							eMsg,			$
							'',			$
							e_msg ], /ERROR )
			RETURN, (-1)
		ENDIF

		;-------------------------------------------
		; get x-offset information for the blocks to
		; be read in
		;-------------------------------------------
;print,'=============== blockStart,blockStart +	 nBlocks - 1,last_good_block = ',blockStart,blockStart + nBlocks - 1,last_good_block
		retStruct	= misr_readOffset( fileName,		$
						   blockStart,		$
						   blockStart +	 nBlocks - 1,	$
						   first_good_block,		$
						   last_good_block, fill_blocks )

		IF retStruct.bad_read THEN RETURN, (-1L)

		nRead	= retStruct.n_blocks

IF fill_blocks LE 0 THEN  fill_block_offset	= first_good_block-1

	ENDIF
	;-----------------------------------------------------------------------
	;-----------------------------------------------------------------------
	;-----------------------------------------------------------------------
	;-----------------------------------------------------------------------
	;-----------------------------------------------------------------------
	; End of MISR Grid Data Processing
	;-----------------------------------------------------------------------
	;-----------------------------------------------------------------------
	;-----------------------------------------------------------------------
	;-----------------------------------------------------------------------
	;-----------------------------------------------------------------------




;-----------------------------------------------------------------------
; XDim corresponds to along-track dimension, YDim is across-track; this is
; seemingly switched from the usual notion of x-dimension being cross-track
;-----------------------------------------------------------------------
;
;
; NOTE:  need to check the following code to see if it works with AirMISR data;
; if not, it needs to be put inside a MISR-only loop
;
xdim_meta_name	= 'XDim:'+STRTRIM(gridName)
;print,'%%%%%%%%%%%%%% xdim_meta_name=',xdim_meta_name
ptr	= misr_get_meta_info( fileName, METADATA_NAME = xdim_meta_name )
IF LONG(TOTAL(PTR_VALID(ptr))) NE N_ELEMENTS(ptr) THEN BEGIN
	eMsg	= 'Could not find metadata ' + xdim_meta_name
	res	= DIALOG_MESSAGE(				$
			[ routineName,				$
			  eMsg,					$
			  returnMsg ],				$
			/ERROR )
	PTR_FREE, ptr
	RETURN, (-1)
ENDIF
in_lines	= *(ptr[0])


ydim_meta_name	= 'YDim:'+STRTRIM(gridName)
ptr	= misr_get_meta_info( fileName, METADATA_NAME = ydim_meta_name )
IF LONG(TOTAL(PTR_VALID(ptr))) NE N_ELEMENTS(ptr) THEN BEGIN
	eMsg	= 'Could not find metadata ' + ydim_meta_name
	res	= DIALOG_MESSAGE(				$
			[ routineName,				$
			  eMsg,					$
			  returnMsg ],				$
			/ERROR )
	PTR_FREE, ptr
	RETURN, (-1)
ENDIF
in_samps	= *(ptr[0])
;print,'======================================================================'
;print,'xdim_meta_name,ydim_meta_name,in_samps,in_lines = ', xdim_meta_name,ydim_meta_name,in_samps,in_lines
;print,'======================================================================'
;---------------------------------------------------------------------------
; input resolution
;---------------------------------------------------------------------------
in_res_samps	= ( FLOAT(BASE_SAMPLES_PER_275M) / FLOAT(in_samps) ) * 275.0
in_res_lines	= ( FLOAT(BASE_LINES_PER_275M) / FLOAT(in_lines) ) * 275.0

;---------------------------------------------------------------------------
; determine stride for cross-track dimension
;---------------------------------------------------------------------------
IF NOT KEYWORD_SET( actRes ) THEN actRes = in_res_samps
samp_stride	= MAX( [ ROUND(actRes / in_res_samps), 1 ] )
samp_edge	= in_samps / samp_stride

;---------------------------------------------------------------------------
; determine stride for along-track dimension
;---------------------------------------------------------------------------
IF NOT KEYWORD_SET( altRes ) THEN altRes = in_res_lines
line_stride	= MAX( [ ROUND(altRes / in_res_lines), 1 ] )
line_edge	= in_lines / line_stride

;print,'in_res_lines,in_res_samps = ',in_res_lines,in_res_samps
;print,'altRes = ',altRes

	;-------------------------------------------
	; open up the file as an HDF-EOS file and
	; set up the grid and field for reading
	;-------------------------------------------
	success	= openGridData(	fileName,				$
				gridName,				$
				fieldName,				$
				GRID_ID = gridID,			$
				DIMS = dims,				$
				DIM_NAMES = dimNames,                   $
      			        HDFEOSFILE_ID = hdfeosID )

	IF NOT success THEN RETURN, (-1L)


	num_dims        = N_ELEMENTS(dims)
	start           = LONARR(num_dims)
	stride          = start
	edge            = start
	extraDimCtr     = 0

	FOR dim_ctr = 0, num_dims - 1 DO BEGIN
		CASE STRUPCASE(dimNames[dim_ctr]) OF
			'XDIM': BEGIN
				start[dim_ctr]	= 0L
edge[dim_ctr]	= LONG(line_edge)
				alt_dim		= dims[dim_ctr]
stride[dim_ctr] = LONG(line_stride)
				END
			'YDIM': BEGIN
				start[dim_ctr]	= 0L
edge[dim_ctr]	= LONG(samp_edge)
				act_dim		= dims[dim_ctr]
stride[dim_ctr] = LONG(samp_stride)
	         		END
			'SOMBLOCKDIM': BEGIN
;;;ckt,feb2000				start[dim_ctr]	= blockStart-1
				start[dim_ctr]	= blockStart-fill_block_offset-1
				edge[dim_ctr]	= nBlocks
				stride[dim_ctr] = 1L
				END
			ELSE: BEGIN
				start[dim_ctr]	= extraDims[extraDimCtr]
				edge[dim_ctr]	= 1
				extraDimCtr	= extraDimCtr + 1
				stride[dim_ctr] = 1L
				END
		ENDCASE
	ENDFOR
;print,'start = ',start
;print,'stride = ',stride
;print,'edge = ',edge
;print,'gridID = ', gridID
;print,'fieldName = ', fieldName
;help,block_stack

;=================================================================
; NNN    NN   OOOO   TTTTTTTT EEEEEEEE   !!!
; NNNN   NN  OO  OO     TT    EE         !!!
; NN NN  NN OO    OO    TT    EEEEEEEE   !!!
; NN  NN NN 00    OO    TT    EE         !!!
; NN   NNNN  OO  OO     TT    EE
; NN    NNN   OOOO      TT    EEEEEEEE   !!!
;
;IDL version 5.3 and 5.4 require that the START, STRIDE, and EDGE keywords
;used in the call EOS_GD_READFIELD be REVERSED in order to extract
;the desired data properly.
;
;=================================================================

IF								$
	STRMID(STRTRIM(!VERSION.RELEASE,2),0,3) EQ '5.3' OR	$
	STRMID(STRTRIM(!VERSION.RELEASE,2),0,3) EQ '5.4' THEN BEGIN

	start	= REVERSE(start)
	stride	= REVERSE(stride)
	edge	= REVERSE(edge)

ENDIF

	status	= EOS_GD_readfield( 				$
				gridID,				$
				fieldName,			$
				block_stack,   			$
				START=start,			$
				STRIDE=stride,			$
				EDGE=edge )

	IF status LT 0 THEN BEGIN

		eMsg1	= 'Problem with EOS_GD_readfield'

		res	= DIALOG_MESSAGE(			$
				[ routineName,			$
				  eMsg1,			$
				  returnMsg ],			$
				/ERROR )
		RETURN, (-1)
	ENDIF

	;---------------------------------------------------------------
	; close the data hose
	;---------------------------------------------------------------

	iStat	= EOS_GD_detach( gridID )

	IF iStat LT 0 THEN BEGIN

		eMsg	= 'Problem with EOS_GD_detach'

		res	= DIALOG_MESSAGE(				$
				[ routineName,				$
				  eMsg,					$
				  returnMsg ],				$
				/ERROR )
			RETURN, (-1)
	ENDIF

	;---------------------------------------------------------------
	; close the file
	;---------------------------------------------------------------

	iStat	= EOS_GD_close( hdfeosID )

	IF iStat LT 0 THEN BEGIN

		eMsg	= 'Problem with EOS_GD_close'

		res	= DIALOG_MESSAGE(				$
				[ routineName,				$
				  eMsg,					$
				  returnMsg ],				$
				/ERROR )
		RETURN, (-1)
	ENDIF

;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
; NOTE:  BEFORE CALLING EOS_GD_READFIELD, THE RETURNED DIMENSIONAL
; INFORMATION FROM openGridData INDICATED THAT THE ORDER OF THE
; DIMENSIONS WERE SOMBLOCKDIM, XDIM, AND YDIM.  HOWEVER, AFTER
; EOS_GD_READFIELD, THE DIMENSIONS OF THE RETURNED DATA INDICATE THAT
; THE FIRST DIMENSION IS YDIM, THEN XDIM, THEN SOMBLOCKDIM.  REMEMBER,
; YDIM IS THE ACROSS-TRACK DIMENSION; IT IS CONFUSING BECAUSE, IN IMAGE
; SPACE, THIS IS USUALLY CONSIDERED THE X (OR SAMPLES) DIMENSION.
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------

	;----------------------------------------------------
	; remove last dimension if it is 1 (SOMBlockDim)
	;----------------------------------------------------
	block_stack		= REFORM(TEMPORARY(block_stack))

	n_block_stack_dims	= (SIZE(block_stack))[0]

	;----------------------------------------------------
	; flip returned data in the vertical direction (Xdim)
	;----------------------------------------------------
	block_stack		= REVERSE(TEMPORARY(block_stack),2)

	;----------------------------------------------------
	; if more than 1 block is involved, flip in the block dimension as well (SOMBlockDim)
	;----------------------------------------------------
	IF n_block_stack_dims GT 2 THEN					$
		block_stack	= REVERSE(TEMPORARY(block_stack),3)

	;----------------------------------------
	; set up rad array depending on data type
	;----------------------------------------
	CASE SIZE( block_stack, /TYPE ) OF
		1:  tmpStr = 'BYTARR('
		2:  tmpStr = 'INTARR('
		3:  tmpStr = 'LONARR('
		4:  tmpStr = 'FLTARR('
		5:  tmpStr = 'DBLARR('
		6:  tmpStr = 'COMPLEXARR('
		7:  tmpStr = 'STRARR('
		9:  tmpStr = 'DCOMPLEXARR('
		12: tmpStr = 'UINTARR('
		13: tmpStr = 'ULONARR('
		14: tmpStr = 'LON64ARR('
		15: tmpStr = 'ULON64ARR('
		ELSE: BEGIN
			eMsg	= 'Unsupported data type: ' + dataType
			res	= DIALOG_MESSAGE(			$
						[ routineName,		$
						  eMsg,			$
						  returnMsg ],		$
						/ERROR )
			RETURN, (-1)
			END
	ENDCASE

	;-------------------------------------------
	; default values take precedence if resolution
	; parameters are not set
	; NOTE: No idea why dims[1] is for the Y-resolution
	; and dims[2] is for the X-resolution; seems
	; like they should be switched (CKT)
	;
	; Update: XDim corresponds to along-track,
	; YDim to across-track, thereby explaining the
	; confusion.
	;-------------------------------------------
	IF NOT KEYWORD_SET(airMisrData) THEN BEGIN
;ckt,mar-1999		IF NOT KEYWORD_SET( actRes ) THEN actRes =			$
;ckt,mar-1999			( BASE_SAMPLES_PER_275M / FLOAT( act_dim ) ) * 275.0
;ckt,mar-1999		IF NOT KEYWORD_SET( altRes ) THEN altRes =			$
;ckt,mar-1999			( BASE_LINES_PER_275M / FLOAT( alt_dim ) ) * 275.0

IF in_res_samps GT actRes THEN div_act = in_res_samps ELSE div_act = actRes
IF in_res_lines GT altRes THEN div_alt = in_res_lines ELSE div_alt = altRes

		nSamp	= ROUND( BASE_SAMPLES_PER_275M * ( 275.0 / div_act ) )
		nLine	= ROUND( BASE_LINES_PER_275M * ( 275.0 / div_alt ) )

		iOffset	= ROUND( retStruct.x_offset/FLOAT(div_act) )
		iPad	= ROUND( retStruct.x_pad/FLOAT(div_act) )

		;==============================================================================
		; October 3, 2001: Added the following code to deal with offsets of less than 1
		; This occurs with the MISR lowest-resolution data.
		;==============================================================================
		float_nsamp		= BASE_SAMPLES_PER_275M * ( 275.0 / div_act )
		float_nline		= BASE_LINES_PER_275M * ( 275.0 / div_alt )
		float_offset		= retStruct.x_offset/FLOAT(div_act)
;print,'float_offset=',float_offset
		float_pad		= retStruct.x_pad/FLOAT(div_act)
		fractional_offset_idx	= WHERE( ROUND(float_offset) NE float_offset, fractional_offset_cnt )

		resize_data_factor	= 1.0
		;
		; Find the minimum of the absolute value of all fractional offsets
		; We take the absolute value because the minimum value is used to resize arrays below
		;
		IF fractional_offset_cnt GT 0 THEN					$
			resize_data_factor	= 1.0 / MIN( ABS( float_offset[fractional_offset_idx] ) )
;print,'resize_data_factor=',resize_data_factor
		iOffset			= ROUND( float_offset * resize_data_factor )
		iPad			= ROUND( float_pad * resize_data_factor )
		nSamp			= ROUND( float_nsamp * resize_data_factor )
		nLine			= ROUND( float_nline * resize_data_factor )
		tmpsz			= SIZE( block_stack )
;print,'tmpsz=',tmpsz
		ydim_len		= tmpsz[1]
		xdim_len		= tmpsz[2]
		new_ydim_len		= ROUND( ydim_len * resize_data_factor )
		new_xdim_len		= ROUND( xdim_len * resize_data_factor )
		IF n_block_stack_dims GT 2 THEN BEGIN
			block_stack	= REBIN(					$
							TEMPORARY(block_stack),		$
							new_ydim_len,			$
							new_xdim_len,			$
							tmpsz[3],			$
							/SAMPLE )
		ENDIF ELSE BEGIN
			block_stack	= REBIN(					$
							TEMPORARY(block_stack),		$
							new_ydim_len,			$
							new_xdim_len,			$
							/SAMPLE )
		ENDELSE

;print,'retStruct.x_offset=',retStruct.x_offset
;print,'div_act=',div_act
;print,'iOffset (floating) = ', retStruct.x_offset/FLOAT(div_act)

;print,'iPad (floating) =',retStruct.x_pad/FLOAT(div_act)
;print,'iOffset = ',iOffset
;print,'iPad = ',iPad
	ENDIF ELSE BEGIN
		nSamp	= act_dim
		nLine	= alt_dim
	ENDELSE

	IF NOT KEYWORD_SET(airMisrData) THEN BEGIN
		;
		; Check for missing data values, utilizing metadata (not implemented;
		; currently, missing data values are returned based upon product, grid, and
		; field... eventually, this needs to be changed, as soon as metadata values are
		; available)
		;
;		missing_data	= get_misr_fill_values( fileName, gridName, fieldName, NUM_TYPE = SIZE(block_stack,/TYPE) )

		missing_data_struct	= misr_return_fill_values( fileName, gridName, fieldName, SIZE( block_stack, /TYPE ) )

		IF missing_data_struct.error THEN BEGIN
			res	= DIALOG_MESSAGE( missing_data_struct.error_msg, /ERROR )
			RETURN, (-1)
		ENDIF

		IF missing_data_struct.fill_values_exist THEN BEGIN
			missing_data	= missing_data_struct.fill_values
		ENDIF

		IF KEYWORD_SET( returnMosaicImageOnly ) THEN BEGIN



mosaicData	= CALL_FUNCTION(STRMID(tmpStr,0,STRLEN(tmpStr)-1),nSamp+iPad,nLine*nRead)

;;;ckt,aug2004			mosaicAllocStr	= 'mosaicData = ' +			$
;;;ckt,aug2004				tmpStr + 'nSamp + iPad, nLine * nRead )'
;;;ckt,aug2004			success			= EXECUTE( mosaicAllocStr )








;help,mosaicData
			IF (SIZE(missing_data))[0] GT 0 THEN BEGIN
				CASE SIZE( block_stack, /TYPE ) OF
					1:  mosaicData = mosaicData + BYTE(missing_data[0])
					2:  mosaicData = mosaicData + FIX(missing_data[0])
					3:  mosaicData = mosaicData + LONG(missing_data[0])
					4:  mosaicData = mosaicData + FLOAT(missing_data[0])
					5:  mosaicData = mosaicData + DOUBLE(missing_data[0])
					6:  mosaicData = mosaicData + COMPLEX(missing_data[0])
					7:  mosaicData = mosaicData + STRING(missing_data[0])
					9:  mosaicData = mosaicData + DCOMPLEX(missing_data[0])
					12: mosaicData = mosaicData + UINT(missing_data[0])
					13: mosaicData = mosaicData + ULONG(missing_data[0])
					14: mosaicData = mosaicData + LONG64(missing_data[0])
					15: mosaicData = mosaicData + ULONG64(missing_data[0])
					ELSE: BEGIN
						eMsg	= 'Unsupported data type: ' + dataType
						res	= DIALOG_MESSAGE(			$
									[ routineName,		$
									  eMsg,			$
									  returnMsg ],		$
									/ERROR )
						RETURN, (-1)
						END
				ENDCASE
			ENDIF
;help,mosaicData
;print,'in misr_readHDFEOSgrid, missing_data = ',missing_data
		ENDIF ELSE BEGIN

mosaicData	= CALL_FUNCTION(STRMID(tmpStr,0,STRLEN(tmpStr)-1),nRead,nSamp,nLine)
;;;ckt,aug2004			mosaicAllocStr	= 'mosaicData = ' + tmpStr +		$
;;;ckt,aug2004				'nRead, nSamp, nLine )'
			xOffsetData 	= LONARR( nRead )
			yOffsetData	= LONARR( nRead )
;;;ckt,aug2004			success		= EXECUTE( mosaicAllocStr )
		ENDELSE
	ENDIF

	IF KEYWORD_SET(airMisrData) THEN BEGIN
		missing_data_struct	= misr_return_fill_values( fileName, gridName, fieldName, SIZE( block_stack, /TYPE ), /AIRMISR )

		IF missing_data_struct.error THEN BEGIN
			res	= DIALOG_MESSAGE( missing_data_struct.error_msg, /ERROR )
			RETURN, (-1)
		ENDIF

		IF missing_data_struct.fill_values_exist THEN BEGIN
			missing_data	= missing_data_struct.fill_values
		ENDIF

;		missing_data	= get_misr_fill_values( '', '', '', NUM_TYPE = SIZE(block_stack,/TYPE) )

		;
		; ASSUMPTION: There is no fractional offset that might need to be accounted for with AirMISR data, so set
		; resize_data_factor to 1.0 (ckt,dec2001)
		;
		resize_data_factor	= 1.0
		RETURN, { block_numbers:0, blocks:block_stack, offsetX:0, offsetY:0, resize_data_factor	: resize_data_factor }
	ENDIF

;ckt, mar-1999	IF nSamp NE (SIZE(block_stack))[1] OR nLine NE (SIZE(block_stack))[2] THEN BEGIN
;ckt, mar-1999		CASE SIZE( block_stack, /TYPE ) OF
;ckt, mar-1999			1 : blk_stk = BYTARR( nSamp, nLine, nBlocks )
;ckt, mar-1999			2 : blk_stk = INTARR( nSamp, nLine, nBlocks )
;ckt, mar-1999			3 : blk_stk = LONARR( nSamp, nLine, nBlocks )
;ckt, mar-1999			4 : blk_stk = FLTARR( nSamp, nLine, nBlocks )
;ckt, mar-1999			5 : blk_stk = DBLARR( nSamp, nLine, nBlocks )
;ckt, mar-1999			6 : blk_stk = COMPLEXARR( nSamp, nLine, nBlocks )
;ckt, mar-1999			7 : blk_stk = STRARR( nSamp, nLine, nBlocks )
;ckt, mar-1999			8 : blk_stk = DCOMPLEXARR( nSamp, nLine, nBlocks )
;ckt, mar-1999			12 : blk_stk = UINTARR( nSamp, nLine, nBlocks )
;ckt, mar-1999			13 : blk_stk = ULONARR( nSamp, nLine, nBlocks )
;ckt, mar-1999			14 : blk_stk = LON64ARR( nSamp, nLine, nBlocks )
;ckt, mar-1999			15 : blk_stk = ULON64ARR( nSamp, nLine, nBlocks )
;ckt, mar-1999		ENDCASE
;ckt, mar-1999
;ckt, mar-1999		FOR blk = 0, nBlocks - 1 DO						$
;ckt, mar-1999			blk_stk[*,*,blk] = CONGRID( REFORM( block_stack[*,*,blk] ),	$
;ckt, mar-1999						nSamp,					$
;ckt, mar-1999						nLine )
;ckt, mar-1999
;ckt, mar-1999		block_stack	= blk_stk
;ckt, mar-1999	ENDIF
;;		block_stack	= CONGRID( TEMPORARY(block_stack),		$
;;					nSamp,					$
;;					nLine,					$
;;					nBlocks )

	iOffset		= REVERSE(TEMPORARY(iOffset),1)

	FOR i = blockStart, blockStart + nBlocks - 1 DO BEGIN
		imb	= i - blockStart
		;-------------------------------------------------------
		; if a mosaic is to be returned, figure out where the
		; current block fits into the "big" picture, then store
		; the block in the appropriate area of the output mosaic
		; array
		;-------------------------------------------------------
		IF KEYWORD_SET( returnMosaicImageOnly ) THEN BEGIN
			xStart	= iPad+iOffset[imb]
			xEnd	= nSamp+iPad+iOffset[imb]-1
			yStart	= imb*nLine
			yEnd	= (imb+1)*nLine-1
			IF nBlocks LE 1 THEN BEGIN
				mosaicData[xStart:xEnd,yStart:yEnd] = block_stack[*,*]
			ENDIF ELSE BEGIN
;help,mosaicData
;help,block_stack
				mosaicData[xStart:xEnd,yStart:yEnd] = block_stack[*,*,imb]
			ENDELSE
		ENDIF ELSE BEGIN
			;-----------------------------------------------
			; store the current block in the
			; output "stack array's" current image position.
			; In either case, also store the current image's
			; x- and y- relative offsets
			;-----------------------------------------------
;;ckt,jul99			IF nBlocks LE 1 THEN BEGIN
;;ckt,jul99				mosaicData[imb,*,*]	= block_stack[*,*]
;;ckt,jul99			ENDIF ELSE BEGIN
;;ckt,jul99				mosaicData[imb,*,*]	= block_stack[*,*,imb]
;;ckt,jul99			ENDELSE

			;---------------------------------------
			; may need to change to iOffset[i]
			;---------------------------------------
			xOffsetData[imb]	= iOffset[imb]+iPad
;print,'xOffsetData[',imb,'] = ',xOffsetData[imb]
			yOffsetData[imb]	= nLine*(imb)
;print,'yOffsetData[imb]=',yOffsetData[imb]
		ENDELSE
	ENDFOR

	IF KEYWORD_SET( returnMosaicImageOnly ) THEN BEGIN
		RETURN, mosaicData
	ENDIF ELSE BEGIN

;		mosaicData		= REVERSE(TEMPORARY(block_stack),2)
		mosaicData		= block_stack

		IF n_block_stack_dims GT 2 THEN BEGIN
;			mosaicData	= REVERSE(TEMPORARY(mosaicData),3)
;			xOffsetData	= REVERSE(TEMPORARY(xOffsetData))
;			yOffsetData	= REVERSE(TEMPORARY(yOffsetData))
		ENDIF

		RETURN, { block_numbers		: REVERSE(LINDGEN(nBlocks)+blockStart),	$
			  blocks		: mosaicData,				$
			  offsetX		: xOffsetData,				$
			  offsetY		: yOffsetData,				$
			  resize_data_factor	: resize_data_factor }
	ENDELSE


END
;misr_readHDFEOSgrid
