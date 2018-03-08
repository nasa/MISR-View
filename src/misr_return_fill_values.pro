@convert_number.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ misr_return_fill_values @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION misr_return_fill_values, file_name, grid_name, field_name, data_type_code, AIRMISR = airmisr
	
	TRUE		= 1
	FALSE		= 0
	routine_name	= '========== misr_return_fill_values =========='
	
	;=======================================================================
	; basic error catch mechanism for any HDF errors
	;=======================================================================
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		e_msg	= [							$
				routine_name,					$
				'Error Index:' + STRTRIM( error_status, 2 ),	$
				'Error Message:' + !ERR_STRING,			$
				'Returning...' ]
		RETURN,								$
			{							$
				error			: TRUE,			$
				error_msg		: e_msg,		$
				fill_values_exist	: FALSE,		$
				fill_values		: [0B] }
	ENDIF
	
	;=======================================================================
	; Check number type of incoming data.  If no fill values exist for
	; number type, set fill_values_exist to FALSE in return structure and
	; immediately exit
	;=======================================================================
	CASE data_type_code OF
		;===============================================================
		; TYPE CODE 1: Byte
		;===============================================================
		1:	minmax_fill_values	= [ 0, 0, 0 ]
		
		;===============================================================
		; TYPE CODE 2: Signed 16-bit integer
		;===============================================================
		2:	minmax_fill_values	= [ 0, 0 ]
		
		;===============================================================
		; TYPE CODE 3: Signed 32-bit integer
		;===============================================================
		3:	minmax_fill_values	= [ 0L, 0L ]
		
		;===============================================================
		; TYPE CODE 4: 32-bit floating point
		;===============================================================
		4:	minmax_fill_values	= [ -9999.0, -8888.0, 1111.0 ]
		
		;===============================================================
		; TYPE CODE 5: 64-bit floating point
		;===============================================================
		5:	minmax_fill_values	= [ -9999.0D, -8888.0D, 1111.0D ]
		
		;===============================================================
		; TYPE CODE 12: Unsigned 16-bit integer
		;===============================================================
		12:	minmax_fill_values	= [ 0U, 0U ]
		
		;===============================================================
		; TYPE CODE 13: Unsigned 32-bit integer
		;===============================================================
		13:	minmax_fill_values	= [ 0UL, 0UL ]
		
		;===============================================================
		; TYPE CODE 14: Signed 64-bit integer
		;===============================================================
		14:	minmax_fill_values	= [ 0LL, 0LL ]
		
		;===============================================================
		; TYPE CODE 15: Unsigned 64-bit integer
		;===============================================================
		15:	minmax_fill_values	= [ 0Ull, 0ULL ]
		
		;===============================================================
		; UNKNOWN TYPE CODE: Error code
		;===============================================================
		ELSE: BEGIN
			e_msg	= [						$
					routine_name,				$
					'Unknown data type code (' +		$
					STRTRIM( data_type_code, 2 ) + ')',	$
					'Returning...' ]
			RETURN,							$
				{						$
					error			: TRUE,		$
					error_msg		: e_msg,	$
					fill_values_exist	: FALSE,	$
					fill_values		: [0B] }
			END
	ENDCASE
	
	found_file	= FINDFILE( file_name )
	
	IF found_file[ 0 ] EQ '' THEN BEGIN
		e_msg	= [							$
				routine_name,					$
				'Cannot locate file:',				$
				file_name,					$
				'Returning...' ]
		RETURN, {							$
				error			: TRUE,			$
				error_msg		: e_msg,		$
				fill_values_exist	: FALSE,		$
				fill_values		: [0B] }
	ENDIF
	
	IF KEYWORD_SET( airmisr) THEN airmisr_data = 1 ELSE airmisr_data = 0
	
	;=======================================================================
	; Obtain the LOCALGRANULEID metadata; if it does not exist, return
	; immediately with error set to TRUE
	;=======================================================================
	IF NOT airmisr_data THEN BEGIN
		local_granule_id = get_local_granule_id( file_name )
	ENDIF ELSE BEGIN
		local_granule_id = 'AIRMISRL1B_AIRMISRL1B_AIRMISRL1B_AIRMISRL1B'
	ENDELSE
	
	IF local_granule_id EQ '' THEN BEGIN
		e_msg	= [							$
				routine_name,					$
				'Cannot find LOCALGRANULEID metadata in file:',	$
				file_name,					$
				'Returning...' ]
		RETURN, {							$
				error			: TRUE,			$
				error_msg		: e_msg,		$
				fill_values_exist	: FALSE,		$
				fill_values		: [0B] }
	ENDIF
	
	;=======================================================================
	; Separate the LOCALGRANULEID by underscore ('_')
	;=======================================================================
	f_sep = STR_SEP( local_granule_id, '_' )

	;=======================================================================
	; If STR_SEP does not produce at least a 3-element STRARR, return
	; immediately with error set to TRUE
	;=======================================================================
	IF N_ELEMENTS( f_sep ) LT 3 THEN BEGIN
		e_msg	= [							$
				routine_name,					$
				'Problem with LOCALGRANULEID metadata in file:',$
				file_name,					$
				'Returning...' ]
		RETURN, {							$
				error			: TRUE,			$
				error_msg		: e_msg,		$
				fill_values_exist	: FALSE,		$
				fill_values		: [0B] }
	ENDIF
	
	grid_file_id		= EOS_GD_OPEN( file_name, /READ )
	IF grid_file_id LT 0 THEN BEGIN
		e_msg	= [							$
				routine_name,					$
				'Problem with EOS_GD_OPEN for file:',		$
				file_name,					$
				'Returning...' ]
		RETURN, {							$
				error			: TRUE,			$
				error_msg		: e_msg,		$
				fill_values_exist	: FALSE,		$
				fill_values		: [0B] }
	ENDIF
	
	grid_id			= EOS_GD_ATTACH( grid_file_id, grid_name )
	IF grid_id LT 0 THEN BEGIN
		e_msg	= [							$
				routine_name,					$
				'Problem with EOS_GD_ATTACH for file:',		$
				file_name,					$
				'Returning...' ]
		success	= EOS_GD_CLOSE( grid_file_id )
		RETURN, {							$
				error			: TRUE,			$
				error_msg		: e_msg,		$
				fill_values_exist	: FALSE,		$
				fill_values		: [0B] }
	ENDIF
	
	field_inq_success	= EOS_GD_INQFIELDS(				$
							grid_id,		$
							field_list,		$
							rank,			$
							number_type )
	IF field_inq_success LT 0 THEN BEGIN
		e_msg	= [							$
				routine_name,					$
				'Problem with EOS_GD_INQFIELDS for file:',	$
				file_name,					$
				'Returning...' ]
		success	= EOS_GD_DETACH( grid_id )
		success	= EOS_GD_CLOSE( grid_file_id )
		RETURN, {							$
				error			: TRUE,			$
				error_msg		: e_msg,		$
				fill_values_exist	: FALSE,		$
				fill_values		: [0B] }
	ENDIF
	
	detach_grid_success	= EOS_GD_DETACH( grid_id )
	IF detach_grid_success LT 0 THEN BEGIN
		e_msg	= [							$
				routine_name,					$
				'Problem with EOS_GD_DETACH for file:',		$
				file_name,					$
				'Returning...' ]
		success	= EOS_GD_CLOSE( grid_file_id )
		RETURN, {							$
				error			: TRUE,			$
				error_msg		: e_msg,		$
				fill_values_exist	: FALSE,		$
				fill_values		: [0B] }
	ENDIF
	
	file_close_success	= EOS_GD_CLOSE( grid_file_id )
	IF file_close_success LT 0 THEN BEGIN
		e_msg	= [							$
				routine_name,					$
				'Problem with EOS_GD_CLOSE for file:',		$
				file_name,					$
				'Returning...' ]
		RETURN, {							$
				error			: TRUE,			$
				error_msg		: e_msg,		$
				fill_values_exist	: FALSE,		$
				fill_values		: [0B] }
	ENDIF

	field_array	= STRTRIM( STR_SEP( field_list, ',' ), 2 )
	field_name	= STRTRIM( field_name, 2 )
	idx		= WHERE( field_array EQ field_name, cnt )
	IF cnt LE 0 THEN BEGIN
		e_msg	= [							$
				routine_name,					$
				'Could not locate field ' + field_name + ' in ',$
				'grid ' + grid_name + ' within file:',		$
				file_name,					$
				'Returning...' ]
		RETURN, {							$
				error			: TRUE,			$
				error_msg		: e_msg,		$
				fill_values_exist	: FALSE,		$
				fill_values		: [0B] }
	ENDIF
	
	hdf_field_number_type	= number_type[ idx[ 0 ] ]
	
	;=======================================================================
	; Obtain product name (3rd element in STRARR)
	;=======================================================================
	product_name	= f_sep[ 2 ]

	CASE product_name OF
		;===============================================================
		;
		; AirMISR Level 1B PRODUCT FILE
		;
		;===============================================================
		'AIRMISRL1B': BEGIN
			IF							$
				STRPOS( field_name, 'Terrain' ) GE 0 AND	$
 				STRPOS( field_name, 'DQI' ) LT 0 THEN		$
					minmax_fill_values	=		$
						convert_number(			$
								[65535,65535],	$
								data_type_code )
 				
			IF							$
				STRPOS( field_name, 'Terrain' ) GE 0 AND	$
 				STRPOS( field_name, 'DQI' ) GE 0 THEN		$
					minmax_fill_values	=		$
						convert_number(			$
								[ 255, 255 ],	$
								data_type_code )
								
			IF							$
				STRPOS( field_name, 'Ellipsoid' ) GE 0 AND	$
 				STRPOS( field_name, 'DQI' ) LT 0 THEN		$
					minmax_fill_values	=		$
						convert_number(			$
								[65535,65535],	$
								data_type_code )
 				
			IF							$
				STRPOS( field_name, 'Ellipsoid' ) GE 0 AND	$
 				STRPOS( field_name, 'DQI' ) GE 0 THEN		$
					minmax_fill_values	=		$
						convert_number(			$
								[ 255, 255 ],	$
								data_type_code )
			END
			
		;===============================================================
		;
		; ANCILLARY GEOGRAPHIC PRODUCT FILE
		;
		;     product_name
		;          |||
		;          |||
		;         \   /
		;          \ /
		; MISR_AM1_AGP_Pmmm_vv.hdf
		;
		; where:
		;	mmm		= path number
		;	vv		= version number
		;===============================================================
		'AGP': BEGIN
			IF hdf_field_number_type GE 20 THEN			$
				minmax_fill_values	=			$
					convert_number(				$
							[ -9999, -9999 ],	$
							data_type_code )
			END
			
		;===============================================================
		;
		; AEROSOL/SURFACE PRODUCT FILE
		;
		;     product_name
		;          ||
		;          ||
		;         \  /
		;          \/
		; MISR_AM1_AS_AEROSOL_Pmmm_Onnnnnn_vv.hdf
		; MISR_AM1_AS_LAND_Pmmm_Onnnnnn_vv.hdf
		; MISR_AM1_AS_OCEAN_Pmmm_Onnnnnn_vv.hdf
		;
		; where:
		;	mmm		= path number
		;	nnnnnn		= absolute orbit number
		;	vv		= version number
		;===============================================================
		'AS': BEGIN
				IF f_sep[ 3 ] EQ 'AEROSOL' THEN BEGIN
				
					IF STRPOS( field_name, 'NumEofUsed' ) GE 0 THEN	$
						minmax_fill_values	=		$
							convert_number(			$
									[ 253, 255 ],	$
									data_type_code )
									
					IF STRPOS( field_name, 'NumAcceptHetOptDepth' ) GE 0 THEN	$
						minmax_fill_values	=		$
							convert_number(			$
									[ 253, 255 ],	$
									data_type_code )
									
					IF STRPOS( field_name, 'BestFitModel' ) GE 0 THEN	$
						minmax_fill_values	=		$
							convert_number(			$
									[ 253, 255 ],	$
									data_type_code )
									
					IF STRPOS( field_name, 'ExtNDVI' ) GE 0 THEN	$
						minmax_fill_values	=		$
							convert_number(			$
									[ 253, 255 ],	$
									data_type_code )
 				
					IF STRPOS( field_name, 'RetrAppMask' ) GE 0 THEN		$
						minmax_fill_values	=				$
							convert_number(					$
									[ 253, 255 ],			$
									data_type_code )
 				
					IF STRPOS( field_name, 'AerCompModId' ) GE 0 THEN		$
						minmax_fill_values	=				$
							convert_number(					$
									[ 253, 255 ],			$
									data_type_code )
					IF STRPOS( field_name, 'OptDepthUpBdCam' ) GE 0 THEN		$
						minmax_fill_values	=				$
							convert_number(					$
									[ 253, 255 ],			$
									data_type_code )
 				
					IF STRPOS( field_name, 'OptDepthUpBdBand' ) GE 0 THEN		$
						minmax_fill_values	=				$
							convert_number(					$
									[ 253, 255 ],			$
									data_type_code )
 				
					IF STRPOS( field_name, 'AlgTypeFlag' ) GE 0 THEN		$
						minmax_fill_values	=				$
							convert_number(					$
									[ 253, 255 ],			$
									data_type_code )
					IF STRPOS( field_name, 'RegClassInd' ) GE 0 THEN		$
						minmax_fill_values	=				$
							convert_number(					$
									[ 253, 255 ],			$
									data_type_code )
 				
					IF STRPOS( field_name, 'Rainbow' ) GE 0 THEN			$
						minmax_fill_values	=				$
							convert_number(					$
									[ 65533, 65535 ],		$
									data_type_code )
 				
					IF STRPOS( field_name, 'NumAcceptSubr' ) GE 0 THEN		$
						minmax_fill_values	=				$
							convert_number(					$
									[ 65533, 65535 ],		$
									data_type_code )
					IF STRPOS( field_name, 'AerRetrSuccFlag' ) GE 0 THEN		$
						minmax_fill_values	=				$
							convert_number(					$
									[ 253, 255 ],			$
									data_type_code )
 				
					IF STRPOS( field_name, 'NumSuccAerModel' ) GE 0 THEN		$
						minmax_fill_values	=				$
							convert_number(					$
									[ 253, 255 ],			$
									data_type_code )
					IF STRPOS( field_name, 'Src' ) GE 0 THEN			$
						minmax_fill_values	=				$
							convert_number(					$
									[ 253, 255 ],			$
									data_type_code )
 				
					IF STRPOS( field_name, 'StratAerFlag' ) GE 0 THEN		$
						minmax_fill_values	=				$
							convert_number(					$
									[ 65533, 65535 ],		$
									data_type_code )
 				
					IF STRPOS( field_name, 'CirrFlag' ) GE 0 THEN			$
						minmax_fill_values	=				$
							convert_number(					$
									[ 65533, 65535 ],		$
									data_type_code )
				ENDIF

				IF f_sep[ 3 ] EQ 'LAND' THEN BEGIN
				
					IF STRPOS( field_name, 'RDQI' ) GE 0 THEN	$
						minmax_fill_values	=		$
							convert_number(			$
									[ 253, 255 ],	$
									data_type_code )
 				
					IF STRPOS( field_name, 'LandHDRF' ) GE 0 THEN	$
						minmax_fill_values	=		$
							convert_number(			$
									[65533,65535],	$
									data_type_code )
									
					IF STRPOS( field_name,'LandBHR'  ) GE 0 THEN	$
						minmax_fill_values	=		$
							convert_number(			$
									[ 253, 255 ],	$
									data_type_code )
				
					IF STRPOS( field_name, 'LandBRF' ) GE 0 THEN	$
						minmax_fill_values	=		$
							convert_number(			$
									[65533,65535],	$
									data_type_code )
 				
					IF STRPOS( field_name, 'LandDHR' ) GE 0 THEN	$
						minmax_fill_values	=		$
							convert_number(			$
									[ 253, 255 ],	$
									data_type_code )
									
					IF STRPOS( field_name, 'BRFModParam1' ) GE 0 THEN	$
						minmax_fill_values	=			$
							convert_number(				$
									[65533,65535],		$
									data_type_code )

					IF STRPOS( field_name, 'BRFModParam' ) GE 0 THEN	$
						minmax_fill_values	=			$
							convert_number(				$
									[ 253, 255 ],		$
									data_type_code )
 				
					IF STRPOS( field_name, 'BRFModFit' ) GE 0 THEN		$
						minmax_fill_values	=			$
							convert_number(				$
									[65533,65535],		$
									data_type_code )
									
					IF STRPOS( field_name, 'NDVI' ) GE 0 THEN		$
						minmax_fill_values	=			$
							convert_number(				$
									[ 253, 255 ],		$
									data_type_code )

					IF STRPOS( field_name, 'LAI' ) GE 0 THEN		$
						minmax_fill_values	=			$
							convert_number(				$
									[ 253, 255 ],		$
									data_type_code )

					IF STRPOS( field_name, 'SubrVar' ) GE 0 THEN		$
						minmax_fill_values	=			$
							convert_number(				$
									[ 253, 255 ],		$
									data_type_code )
 				
					IF STRPOS( field_name, 'NumSubr' ) GE 0 THEN		$
						minmax_fill_values	=			$
							convert_number(				$
									[65533,65535],		$
									data_type_code )
									
					IF STRPOS( field_name, 'FreqOccur' ) GE 0 THEN		$
						minmax_fill_values	=			$
							convert_number(				$
									[65533,65535],		$
									data_type_code )
				ENDIF
			END
			
		;===============================================================
		;
		; LEVEL 2 TOA/CLOUD PRODUCT FILE
		;
		;     product_name
		;          ||
		;          ||
		;         \  /
		;          \/
		; MISR_AM1_TC_?????_Pmmm_Onnnnnn_vv.hdf
		;
		; where:
		;	mmm		= path number
		;	nnnnnn		= absolute orbit number
		;	vv		= version number
		;===============================================================
		'TC': BEGIN
			IF N_ELEMENTS( f_sep ) GE 4 THEN BEGIN
				IF f_sep[ 3 ] EQ 'STEREO' THEN BEGIN
				
					IF STRPOS( field_name, 'Disparity' ) GE 0 THEN	$
						minmax_fill_values	=		$
							convert_number(			$
									[-9999,-9999],	$
									data_type_code )
 				
					IF STRPOS( field_name, 'NBinsCloudMotionSpeed' ) GE 0 THEN	$
						minmax_fill_values	=				$
							convert_number(					$
									[-9999,-9999],			$
									data_type_code )
 				
					IF STRPOS( field_name, 'CloudMotionSource' ) GE 0 THEN		$
						minmax_fill_values	=				$
							convert_number(					$
									[ 253, 255 ],			$
									data_type_code )
				ENDIF
				IF f_sep[ 3 ] EQ 'ALBEDO' THEN BEGIN
					IF STRPOS( field_name, 'NumUnobscuredTop' ) GE 0 THEN	$
						minmax_fill_values	=			$
							convert_number(				$
									[-99,-89],		$
									data_type_code )
				
					IF STRPOS( field_name, 'NumUnobscuredSide' ) GE 0 THEN	$
						minmax_fill_values	=			$
							convert_number(				$
									[-9999,-9998],		$
									data_type_code )
				
					IF STRPOS( field_name, 'NumSubRestr' ) GE 0 THEN	$
						minmax_fill_values	=			$
							convert_number(				$
									[65533,65535],		$
									data_type_code )
				ENDIF
			ENDIF
			END
			
		;===============================================================
		;
		; GEORECTIFIED RADIANCE PRODUCT FILE (PGE-2)
		;
		;     product_name
		;          |||
		;          |||
		;         \   /
		;          \ /
		; MISR_AM1_GRP_TERRAIN_GM_Pmmm_Onnnnnn_cc_vv.hdf
		; MISR_AM1_GRP_ELLIPSOID_GM_Pmmm_Onnnnnn_cc_vv.hdf
		; MISR_AM1_GRP_RCCM_GM_Pmmm_Onnnnnn_cc_vv.hdf
		;
		; where:
		;	mmm		= path number
		;	nnnnnn		= absolute orbit number
		;	cc		= camera identifier
		;	vv		= version number
		;===============================================================
		'GRP': BEGIN
			IF STRPOS( field_name, 'Radiance/RDQI' ) GE 0 THEN	$
				minmax_fill_values	=			$
					convert_number(				$
							[ 65508, 65535 ],	$
							data_type_code )
							
			IF N_ELEMENTS( f_sep ) GE 4 THEN BEGIN
				IF f_sep[ 3 ] EQ 'RCCM' THEN			$
				minmax_fill_values	=			$
					convert_number(				$
							[ 255, 255 ],		$
							data_type_code )
			ENDIF
			END
			
		;===============================================================
		;
		; GEORECTIFIED RADIANCE PRODUCT FILE (PGE-1)
		;
		;     product_name
		;          |||
		;          |||
		;         \   /
		;          \ /
		; MISR_AM1_PGRP_TERRAIN_GM_Pmmm_Onnnnnn_cc_vv.hdf
		; MISR_AM1_PGRP_ELLIPSOID_GM_Pmmm_Onnnnnn_cc_vv.hdf
		; MISR_AM1_PGRP_RCCM_GM_Pmmm_Onnnnnn_cc_vv.hdf
		;
		; where:
		;	mmm		= path number
		;	nnnnnn		= absolute orbit number
		;	cc		= camera identifier
		;	vv		= version number
		;===============================================================
		'PGRP': BEGIN
			IF STRPOS( field_name, 'Radiance/RDQI' ) GE 0 THEN	$
				minmax_fill_values	=			$
					convert_number(				$
							[ 65508, 65535 ],	$
							data_type_code )
							
			IF N_ELEMENTS( f_sep ) GE 4 THEN BEGIN
				IF f_sep[ 3 ] EQ 'RCCM' THEN			$
				minmax_fill_values	=			$
					convert_number(				$
							[ 255, 255 ],		$
							data_type_code )
			ENDIF
			END
			
		;===============================================================
		;
		; GEOMETRIC PARAMETER FILE
		;
		;     product_name
		;          ||
		;          ||
		;         \  /
		;          \/
		; MISR_AM1_GP_GMP_Pmmm_Onnnnnn_vv.hdf
		;
		; where:
		;	mmm		= path number
		;	nnnnnn		= absolute orbit number
		;	vv		= version number
		;===============================================================
		'GP': BEGIN
			minmax_fill_values	= [ -999.0, -111.0, 111.0 ]
			END
		ELSE: BEGIN
			e_msg	= [							$
					routine_name,					$
					'Unknown product name in file:',		$
					file_name,					$
					'Returning...' ]
			RETURN, {							$
					error			: TRUE,			$
					error_msg		: e_msg,		$
					fill_values_exist	: FALSE,		$
					fill_values		: [0B] }
			END
	ENDCASE
	
	;=======================================================================
	; Special case for byte data that is not called out explicitly above.
	; If this is the case, set fill_values_exist to FALSE.  THis is checked
	; by looking at the default value of minmax_fill_values, which sets the
	; increment value (the third position in the array) to 0.
	;=======================================================================
	IF data_type_code EQ 1 AND N_ELEMENTS( minmax_fill_values ) EQ 3 THEN BEGIN
		IF minmax_fill_values[2] EQ 0 THEN				$
		RETURN,							$
				{						$
					error			: FALSE,	$
					error_msg		: '',		$
					fill_values_exist	: FALSE,	$
					fill_values		: [0B] }
	ENDIF
	
	;=======================================================================
	; Use the min/max values to create a continuous list of missing values
	; Use the third element of minmax_fill_values (IF IT EXISTS) as an
	; incremental value.
	;=======================================================================
	inc	= 1
	IF N_ELEMENTS(minmax_fill_values) GT 2 THEN inc = minmax_fill_values[ 2 ]
	
	array_length	= ( minmax_fill_values[ 1 ] - minmax_fill_values[ 0 ] + inc ) / inc
	fill_values	= ( DINDGEN( array_length ) * inc ) + minmax_fill_values[ 0 ]
	fill_values	= convert_number( fill_values, data_type_code )
	
	;
	;
	;
	; KLUGE FOR fill_values that DO NOT follow a constant increment (found in TC_ALBEDO)
	;
	;
	IF minmax_fill_values[ 0 ] EQ -9999.0 AND minmax_fill_values[ 1 ] EQ -8888.0 THEN	$
		fill_values	= [ fill_values[0], convert_number( -9998, data_type_code ), fill_values[1:N_ELEMENTS(fill_values)-1] ]

	RETURN,									$
		{								$
			error			: FALSE,			$
			error_msg		: '',				$
			fill_values_exist	: TRUE,				$
			fill_values		: fill_values }
END
; misr_return_fill_values
