@GetDirectoryDivider.pro
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ misr_get_transform_info @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION misr_get_transform_info, p, g, f
	product_filename	= ''
	grid			= ''
	field			= ''
	
	CASE N_PARAMS() OF
		0:  BEGIN
			END
		1: BEGIN
			product_filename	= p
			END
		2: BEGIN
			product_filename	= p
			grid			= g
			END
		3: BEGIN
			product_filename	= p
			grid			= g
			field			= f
			END
		ELSE: BEGIN
			product_filename	= p
			grid			= g
			field			= f
			END
	ENDCASE
	
	product_filename= STRTRIM( product_filename, 2 )
	grid		= STRTRIM( grid, 2 )
	field		= STRTRIM( field, 2 )
	
	sepStr		= STR_SEP( product_filename, GetDirectoryDivider() )
	fName		= sepStr[ N_ELEMENTS( sepStr ) - 1 ]
	qualifiedDir	= STRMID( product_filename, 0, STRLEN( product_filename ) - STRLEN( fName ) )
	fSep		= STR_SEP( fName, '_' )
	IF N_ELEMENTS(fSep) GE 3 THEN productName = fSep[ 2 ] ELSE productName = 'UNKNOWN'

	found_file	= FINDFILE(product_filename)
	IF found_file[0] EQ '' THEN productName = 'FILENOTFOUND'
	
	transform_formula	= [ '' ]
	transform_notes		= [ '' ]
	
	CASE productName OF
		
		;===============================================================
		; MISR_AM1_GP_GMP_Pmmm_Onnnnnn_vv.hdf
		;===============================================================
		'GP': BEGIN	; GEOMETRIC PARAMETER FILES
			transform_formula	= [ '' ]
			transform_notes		= [ '' ]
			END
		'GRP': BEGIN	; GEORECTIFIED RADIANCE PRODUCT FILES
		;===============================================================
		; MISR_AM1_GRP_TERRAIN_GM_Pmmm_Onnnnnn_cc_vv.hdf
		; MISR_AM1_GRP_ELLIPSOID_GM_Pmmm_Onnnnnn_cc_vv.hdf
		; MISR_AM1_GRP_RCCM_GM_Pmmm_Onnnnnn_cc_vv.hdf
		;
		; mmm		= path number
		; nnnnnn	= absolute orbit number
		; cc		= camera identifier
		; vv		= version number
		;===============================================================
			grp_type	= fSep[3]
			CASE grp_type OF
				'TERRAIN': BEGIN
					mname			= 'Scale factor'
					ret			= misr_get_meta_info( product_filename, METADATA_NAME=mname )
					IF PTR_VALID( ret[0] ) THEN scale_factor = *(ret[0]) ELSE scale_factor = 1.0
					PTR_FREE, ret
					transform_formula	= [												$
							'transformed_data=FLOAT(ISHFT(source_data,-2))',							$
							'idx=WHERE(transformed_data NE 16378u AND transformed_data NE 16380u,cnt)',				$
							'IF cnt GT 0 THEN transformed_data[idx]=transformed_data[idx]*'+STRTRIM(STRING(scale_factor),2) ]
					transform_notes		= [												$
							'Transform has been applied to display radiance values.  Scale value retrieved',			$
							'from the metadata named "' + mname + '".  Transform will need to be modified',				$
							'to display RDQI.' ]
					END
				'ELLIPSOID': BEGIN
					mname			= 'Scale factor'
					ret			= misr_get_meta_info( product_filename, METADATA_NAME=mname )
					IF PTR_VALID( ret[0] ) THEN scale_factor = *(ret[0]) ELSE scale_factor = 1.0
					PTR_FREE, ret
					transform_formula	= [												$
							'transformed_data=FLOAT(ISHFT(source_data,-2))',							$
							'idx=WHERE(transformed_data NE 16378u AND transformed_data NE 16380u,cnt)',				$
							'IF cnt GT 0 THEN transformed_data[idx]=transformed_data[idx]*'+STRTRIM(STRING(scale_factor),2) ]
					transform_notes		= [												$
							'Transform has been applied to display radiance values.  Scale value retrieved',			$
							'from the metadata named "' + mname + '".  Transform will need to be modified',				$
							'to display RDQI.' ]
					END
				'RCCM': BEGIN
					transform_formula	= [ '' ]
					transform_notes		= [ '' ]
					END
				ELSE: BEGIN
					transform_formula	= [ '' ]
					transform_notes		= [ '' ]
					END
			ENDCASE
			
			END
		'AGP': BEGIN	; ANCILLARY GEOGRAPHIC PRODUCT FILES
		;===============================================================
		; MISR_AM1_AGP_Pmmm_vv.hdf
		;
		; mmm		= path number
		; vv		= version number
		;===============================================================
			transform_formula	= [ '' ]
			transform_notes	= [ '' ]
			END
		'AS': BEGIN	; AEROSOL/SURFACE
		;===============================================================
		; MISR_AM1_AS_?????_Pmmm_Onnnnnn_vv.hdf
		; MISR_AM1_AS_LANDSFC1_?????_Pmmm_Onnnnnn_vv.hdf
		; MISR_AM1_AS_LANDSFC2_?????_Pmmm_Onnnnnn_vv.hdf
		; MISR_AM1_AS_LANDSFC3_?????_Pmmm_Onnnnnn_vv.hdf
		; MISR_AM1_AS_OCEANSFC_?????_Pmmm_Onnnnnn_vv.hdf
		;
		; mmm		= path number
		; nnnnnn	= absolute orbit number
		; vv		= version number
		;===============================================================
			scale_mname		= 'Scale ' + field
			offset_mname		= 'Offset ' + field
			min_mname		= 'Min ' + field
			max_mname		= 'Max ' + field
			min_factor_not_found	= 0
			max_factor_not_found	= 0
			min_factor		= 0
			max_factor		= 0
			ret		= misr_get_meta_info( product_filename, METADATA_NAME=scale_mname )
			IF PTR_VALID( ret[0] ) THEN scale_factor = *(ret[0]) ELSE scale_factor = 1.0
			PTR_FREE, ret
			ret		= misr_get_meta_info( product_filename, METADATA_NAME=offset_mname )
			IF PTR_VALID( ret[0] ) THEN offset_factor = *(ret[0]) ELSE offset_factor = 0.0
			PTR_FREE, ret
			ret		= misr_get_meta_info( product_filename, METADATA_NAME=min_mname )
			IF PTR_VALID( ret[0] ) THEN min_factor = *(ret[0]) ELSE min_factor_not_found = 1
			PTR_FREE, ret
			ret		= misr_get_meta_info( product_filename, METADATA_NAME=max_mname )
			IF PTR_VALID( ret[0] ) THEN max_factor = *(ret[0]) ELSE max_factor_not_found = 1
			PTR_FREE, ret
			
			IF scale_factor EQ 1.0 AND offset_factor EQ 0.0 AND min_factor_not_found AND max_factor_not_found THEN BEGIN
				transform_formula	= [ '' ]
				transform_notes		= [ '' ]
			ENDIF ELSE BEGIN
				transform_formula	= [															$
					'transformed_data = source_data',													$
					'IF ' + STRTRIM(STRING(min_factor_not_found),2) + ' THEN min_val = MIN(source_data) ELSE min_val = ' + STRTRIM(STRING(min_factor),2),	$
					'IF ' + STRTRIM(STRING(max_factor_not_found),2) + ' THEN max_val = MAX(source_data) ELSE max_val = ' + STRTRIM(STRING(max_factor),2),	$
					'idx = WHERE( source_data GE min_val AND source_data LE max_val, cnt )',								$
					'IF cnt GT 0 THEN transformed_data[idx]=transformed_data[idx]*'+STRTRIM(STRING(scale_factor),2)+'+'+STRTRIM(STRING(offset_factor),2) ]
				transform_notes		= [											$
					'The following metadata has been searched for:',							$
					scale_mname+', '+offset_mname+', '+min_mname+', '+max_mname,						$
					'If any of these metadata values are not found, the following default values are used:',		$
					scale_mname+': 1.0',											$
					offset_mname+': 0.0',											$
					min_mname+': global data minimum value',								$
					max_mname+': global data maximum value',								$
					'The transformation formula is as follows:',								$
					'transformed_data_value = ' + scale_mname + '*data_value + ' + offset_mname ]
			ENDELSE
			END
		'TC': BEGIN	; LEVEL 2 TOA/CLOUD PRODUCT FILES
		;===============================================================
		; MISR_AM1_TC_?????_Pmmm_Onnnnnn_vv.hdf
		;
		; mmm		= path number
		; nnnnnn	= absolute orbit number
		; vv		= version number
		;===============================================================
			END
		ELSE: BEGIN
			transform_formula	= [ '' ]
			transform_notes		= [ '' ]
			END
	ENDCASE
	
	RETURN, { transform_formula:transform_formula, transform_notes:transform_notes }

END
; misr_get_transform_info
