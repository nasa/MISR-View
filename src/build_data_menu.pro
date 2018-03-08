@GetLowerBaseContents_color_menus
@add_dim_info
@GetDirectoryDivider.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ misr_get_field_dims @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION misr_get_field_dims, sdID, grid_id, field_name
	rank     = 0L
	dims     = LONARR(10)
	numtype  = 0L
	dim_list = ''
	num_idx  = [3,4,5,6,20,21,22,23,24,25]
	cnumtype = [			$
		'DFNT_UCHAR8',		$	; nType = 3
		'DFNT_CHAR8',		$	; nType = 4
		'DFNT_FLOAT32',		$	; nType = 5
		'DFNT_FLOAT64',		$	; nType = 6
		'DFNT_INT8',		$	; nType = 20
		'DFNT_UINT8',		$	; nType = 21
		'DFNT_INT16',		$	; nType = 22
		'DFNT_UINT16',		$		; nType = 23
		'DFNT_INT32',		$	; nType = 24
		'DFNT_UINT32'		]	; nType = 25

;;;;;	iStat	= HDF_GD_FIELDINFO(	$
;;;;;		sdID,			$
;;;;;		grid_id,		$
;;;;;		field_name,		$
;;;;;		rank,			$
;;;;;		dims,			$
;;;;;		numtype,		$
;;;;;		dim_list		)
	iStat	= EOS_GD_fieldinfo(	$
		grid_id,		$
		field_name,		$
		rank,			$
		dims,			$
		numtype,		$
		dim_list		)
	dims	= dims[0:rank-1]
	num_idx	= WHERE(num_idx EQ numtype )
	ctype	= cnumtype[num_idx[0]]
	RETURN, {			$
		rank     :rank,		$
		dims     :dims,		$
		numtype  :numtype,	$
		cnumtype :ctype,	$
		dim_list :STR_SEP(dim_list,',') }
END
;

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ misr_get_field_names @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;;;;FUNCTION misr_get_field_names, sdID, grid_id
FUNCTION misr_get_field_names, grid_id
	;--------------------------------------------------------------------
	; MAX_DIM is completely arbitrary
	;--------------------------------------------------------------------
	MAX_DIM      = 100

	field_list   = STRARR( MAX_DIM )
	rank_list    = LONARR( MAX_DIM )
	numtype_list = LONARR( MAX_DIM )

;;;;;	n_fields     = HDF_GD_INQFLDS( sdID, grid_id, field_list, rank_list, numtype_list )
	n_fields     = EOS_GD_inqfields( grid_id, field_list, rank_list, numtype_list )
;print,'n_fields = ',n_fields
;help,field_list
	field_list = STR_SEP(field_list,',')
	RETURN, field_list[0:n_fields-1]
END
; misr_get_field_names

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ misr_get_file_type @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION misr_get_file_type, n_grid, grid_list, filename

	productName	= 'NOTHING'
	
	ptr	= misr_get_meta_info( filename, METADATA_NAME = 'StructMetadata.0' )
	am	= STRUPCASE( STRING(*(ptr[0])) )
	PTR_FREE, ptr
	IF STRPOS( am, 'AIRMISR' ) GE 0 THEN BEGIN
		productName = 'OTHER'
	ENDIF ELSE BEGIN
		granule_id	= get_local_granule_id( filename )
		IF granule_id NE '' THEN BEGIN
			fSep		= STR_SEP( granule_id, '_' )
			productName	= fSep[ 2 ]
		ENDIF
	ENDELSE
	
;;;ckt,dec2000	productName	= 'NOTHING'
;;;ckt,dec2000	filename	= STRTRIM( filename, 2 )
;;;ckt,dec2000	sepStr		= STR_SEP( filename, GetDirectoryDivider() )
;;;ckt,dec2000	fName		= sepStr[ N_ELEMENTS( sepStr ) - 1 ]
	
;;;ckt,dec2000	fSep		= STR_SEP( fName, '_' )
	
;
;
;
; THIS IS ONLY A TEMPORARY FIX UNTIL THE FILE TYPE IS EXTRACTED FROM WITHIN THE FILE
;
;;;ckt,apr2000	IF N_ELEMENTS(fSep) LT 3 THEN BEGIN
;;;ckt,apr2000		ptr	= misr_get_meta_info( filename, METADATA_NAME = 'StructMetadata.0' )
;;;ckt,apr2000		am	= STRUPCASE( STRING(*(ptr[0])) )
;;;ckt,apr2000		PTR_FREE, ptr
;;;ckt,apr2000		IF STRPOS( am, 'AIRMISR' ) GE 0 THEN productName = 'OTHER'
;;;ckt,apr2000	ENDIF ELSE BEGIN
;;;ckt,apr2000		productName	= fSep[ 2 ]
;;;ckt,apr2000	ENDELSE

;;;ckt,dec2000	ptr	= misr_get_meta_info( filename, METADATA_NAME = 'StructMetadata.0' )
;;;ckt,dec2000	am	= STRUPCASE( STRING(*(ptr[0])) )
;;;ckt,dec2000	PTR_FREE, ptr
;;;ckt,dec2000	IF STRPOS( am, 'AIRMISR' ) GE 0 THEN productName = 'OTHER' ELSE productName = fSep[ 2 ]
	
	CASE productName OF
		'GP': BEGIN	; GEOMETRIC PARAMETER FILES
		;---------------------------------------------------------------
		; MISR_AM1_GP_GMP_Pmmm_Onnnnnn_vv.hdf
		;
		; mmm		= path number
		; nnnnnn	= absolute orbit number
		; cc		= camera identifier
		; vv		= version number
		;---------------------------------------------------------------
			file_type	=						$
				productName +	 ', ' +	fSep[3] + ', ' + fSep[4] + ', ' + fSep[5]		; product
			END
			
		'GRP': BEGIN	; GEORECTIFIED RADIANCE PRODUCT FILES (PGE-2)
		;---------------------------------------------------------------
		; MISR_AM1_GRP_TERRAIN_GM_Pmmm_Onnnnnn_cc_vv.hdf
		; MISR_AM1_GRP_ELLIPSOID_GM_Pmmm_Onnnnnn_cc_vv.hdf
		; MISR_AM1_GRP_RCCM_GM_Pmmm_Onnnnnn_cc_vv.hdf
		;
		; mmm		= path number
		; nnnnnn	= absolute orbit number
		; cc		= camera identifier
		; vv		= version number
		;---------------------------------------------------------------
			file_type	=						$
				productName + ', ' + fSep[3] + ', ' + fSep[7]	+ ', ' + fSep[5] + ', ' + fSep[6]				  ; camera
								
			END

		'PGRP': BEGIN	; GEORECTIFIED RADIANCE PRODUCT FILES (PGE-1)
		;---------------------------------------------------------------
		; MISR_AM1_PGRP_TERRAIN_GM_Pmmm_Onnnnnn_cc_vv.hdf
		; MISR_AM1_PGRP_ELLIPSOID_GM_Pmmm_Onnnnnn_cc_vv.hdf
		; MISR_AM1_PGRP_RCCM_GM_Pmmm_Onnnnnn_cc_vv.hdf
		;
		; mmm		= path number
		; nnnnnn	= absolute orbit number
		; cc		= camera identifier
		; vv		= version number
		;---------------------------------------------------------------
			file_type	=						$
				productName + ', ' + fSep[3] + ', ' + fSep[7]	+ ', ' + fSep[5] + ', ' + fSep[6]				  ; camera
								
			END

		'AGP':	BEGIN	; ANCILLARY GEOGRAPHIC PRODUCT FILES
		;---------------------------------------------------------------
		; MISR_AM1_AGP_Pmmm_vv.hdf
		;
		; mmm		= path number
		; vv		= version number
		;---------------------------------------------------------------
			file_type	= productName + ', ' + fSep[3]
			END
			
		'AS': BEGIN	; AEROSOL/SURFACE
		;---------------------------------------------------------------
		; MISR_AM1_AS_AEROSOL_Pmmm_Onnnnnn_vv.hdf
		; MISR_AM1_AS_LANDSFC1_Pmmm_Onnnnnn_vv.hdf
		; MISR_AM1_AS_LANDSFC2_Pmmm_Onnnnnn_vv.hdf
		; MISR_AM1_AS_LANDSFC3_Pmmm_Onnnnnn_vv.hdf
		; MISR_AM1_AS_OCEANSFC_Pmmm_Onnnnnn_vv.hdf
		;
		; mmm		= path number
		; nnnnnn	= absolute orbit number
		; vv		= version number
		;---------------------------------------------------------------
			file_type	=					$
				productName				+	$
				', '					+	$
				fSep[3]					+	$
				', '					+	$
				fSep[4]					+	$
				', '					+	$
				fSep[5]					; product
			END
			
		'TC': BEGIN	; LEVEL 2 TOA/CLOUD PRODUCT FILES
		;---------------------------------------------------------------
		; MISR_AM1_TC_?????_Pmmm_Onnnnnn_vv.hdf
		;
		; mmm		= path number
		; nnnnnn	= absolute orbit number
		; vv		= version number
		;---------------------------------------------------------------
			file_type	=					$
				productName				+	$
				', '					+	$
				fSep[3]					+	$
				', '					+	$
				fSep[4]					+	$
				', '					+	$
				fSep[5]					; product
			END
		'OTHER': file_type	= 'AirMISR'
		ELSE:	file_type	= 'UNRECOGNIZED PRODUCT'
	ENDCASE

	RETURN, file_type
END
; misr_get_file_type

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ parse_misr_grid_file @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION parse_misr_grid_file, filename

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when 
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== parse_misr_grid_file =========='
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
		RETURN, { success:0 }
	ENDIF


	max_rank=0

	;----------------------------------------------------
	; HDF_GD_OPEN
	;----------------------------------------------------
;print,'in parse_misr_grid_file just before eos_gd_open, filename = ',filename
;;;;;	file_id = HDF_GD_OPEN( filename, sdID )
	file_id = EOS_GD_open( filename )
;print,'in parse_misr_grid_file just after eos_gd_open, file_id =',  file_id
	IF file_id LT 0 THEN BEGIN
;;;;;		msg = 'Problem with HDF_GD_OPEN... Exiting...'
		msg = [ 'Problem with EOS_GD_OPEN on file',	$
			filename,				$
			'Check to make sure file exists...',	$
			'For now, skipping this file...'	]
		ret = DIALOG_MESSAGE( msg, /ERROR )
		RETURN, { success:0 }
	ENDIF

	grid_list = ''
	strBufSz  = 0L
	;----------------------------------------------------
	; HDF_GD_INQGRID
	;----------------------------------------------------
;;;;;	n_grid      = HDF_GD_INQGRID( filename, grid_list, strBufSz )
	n_grid      = EOS_GD_inqgrid( filename, grid_list, LENGTH = strBufSz )
	grid_names  = STR_SEP( grid_list, ',' )

	grid_ptrarr = PTRARR(2)
	grid_info   = PTRARR(n_grid)
	;----------------------------------------------------
	; make call to misr_get_file_type to determine
	; product... eventually, this routine will make
	; HDF-EOS calls to figure this out, but for now,
	; determine file type by checking number of grids
	; in file...
	;----------------------------------------------------
	file_type  = misr_get_file_type( n_grid, grid_list, filename )

;	field_struct = {				$
;		name			:'',            $
;		field_rank		:0L,            $
;		field_numtype		:0,             $
;		field_cnumtype		:'',            $
;		field_dims_ptr		:PTR_NEW(),     $
;		field_dim_names_ptr	:PTR_NEW(),     $
;		ptr			:PTR_NEW() }

	field_ptrarr = PTRARR(2)

	FOR i = 0, n_grid - 1 DO BEGIN
;		grid_info[i].name = grid_names[i]
		grid_info[i] = PTR_NEW(PTRARR(2))
		(*(grid_info[i]))[0] = PTR_NEW(grid_names[i])

;;;;;		grid_id     = HDF_GD_ATTACH( file_id, sdID, grid_names[i] )
		grid_id     = EOS_GD_attach( file_id, grid_names[i] )
;;;;;		field_names = misr_get_field_names( sdID, grid_id )
		field_names = misr_get_field_names( grid_id )

;		field_info = REPLICATE( field_struct, N_ELEMENTS( field_names ) )
		field_info=PTRARR(N_ELEMENTS( field_names ))
		FOR j = 0, N_ELEMENTS(field_names) - 1 DO BEGIN
;			field_info[j].name     = field_names[j]

;			field_info[j] = PTR_NEW(PTRARR(2))

			field_info[j] = PTR_NEW(PTRARR(3))

			(*(field_info[j]))[0] = PTR_NEW(field_names[j])

			ret_struct =			$
				misr_get_field_dims(	$
					sdID,		$
					grid_id,	$
					field_names[j]	)

			(*(field_info[j]))[2] = PTR_NEW(ret_struct.cnumtype)

			if ret_struct.rank gt max_rank then $
				max_rank = ret_struct.rank
			;-------------------------------------------------------------
			; Check dimensions of field; if greater than 3, that means that
			; there are add'l dimensions to xDim, yDim, and SOMBlockDim
			; that need to be "fixed"
			;-------------------------------------------------------------
			IF ret_struct.rank GT 3 THEN BEGIN

				n_extra_dims = ret_struct.rank - 3

				FOR d = 0, n_extra_dims - 1 DO BEGIN
					dim_name		= ret_struct.dim_list[d]	;;+3]	;jrh Mar2001 evidently RSI changed EOS_GD_fieldinfo in IDL 5.4.
					dim_size		= ret_struct.dims[d]		;;+3]	;jrh Mar2001 evidently RSI changed EOS_GD_fieldinfo in IDL 5.4.
;tmpPtr		= field_info[j].ptr
					tmpPtr			= (*(field_info[j]))[1]
					tmpPtr			= add_dim_info(dim_name, dim_size, tmpPtr)
;field_info[j].ptr = tmpPtr
					(*(field_info[j]))[1]	= tmpPtr

				ENDFOR
			ENDIF

;         field_info[j].field_rank          = ret_struct.rank
;         field_info[j].field_numtype       = ret_struct.numtype
;         field_info[j].field_cnumtype      = ret_struct.cnumtype
;         field_info[j].field_dims_ptr      = PTR_NEW(ret_struct.dims, /NO_COPY)
;         field_info[j].field_dim_names_ptr = PTR_NEW(ret_struct.dim_list, /NO_COPY)

		ENDFOR

;		grid_info[i].ptr	= PTR_NEW( field_info, /NO_COPY )
		(*(grid_info[i]))[1]	= PTR_NEW( field_info, /NO_COPY )

;;;;;		iStat      		= HDF_GD_DETACH( sdID, grid_id )
		iStat      		= EOS_GD_detach( grid_id )
	ENDFOR

;;;;;   iStat = HDF_GD_CLOSE( sdID, grid_id )
	iStat = EOS_GD_close(file_id)

	RETURN, { grid_info:grid_info, max_rank:max_rank, file_type:file_type, success:1 }

END
; parse_misr_grid_file

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ update_data_droplists @@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO update_data_droplists, topBase
	WIDGET_CONTROL, topBase, GET_UVALUE = infoPtr
	;--------------------------------------------------------------------
	; products
	;--------------------------------------------------------------------
	prod = STRARR(N_ELEMENTS((*infoPtr).ptr_tree))
	FOR i = 0, N_ELEMENTS((*infoPtr).ptr_tree)-1 DO $
		prod[i] = *((*(( (*infoPtr).ptr_tree )[i]))[0])
	id      = ((*infoPtr).p_wd)
	sel     = ((*infoPtr).p_idx)
	WIDGET_CONTROL, id, SET_VALUE = prod, SET_LIST_SELECT = sel

	;--------------------------------------------------------------------
	; grids
	;--------------------------------------------------------------------
	gridPtr = *((*(( (*infoPtr).ptr_tree )[(*infoPtr).p_idx]))[1])
	grids   = STRARR(N_ELEMENTS(gridPtr))
	FOR i = 0, N_ELEMENTS(gridPtr)-1 DO $
		grids[i] = *((*(gridPtr[i]))[0])
	id      = ((*infoPtr).g_wd)
	sel     = ((*infoPtr).g_idx)
	WIDGET_CONTROL, id, SET_VALUE = grids, SET_LIST_SELECT = sel

	IF PTR_VALID( (*infoPtr).gridsListPtr ) THEN PTR_FREE, (*infoPtr).gridsListPtr
	(*infoPtr).gridsListPtr	= PTR_NEW( grids )

	;--------------------------------------------------------------------
	; fields
	;--------------------------------------------------------------------
	fieldPtr = *((*(gridPtr[(*infoPtr).g_idx]))[1])
	fields   = STRARR(N_ELEMENTS(fieldPtr))
	FOR i = 0, N_ELEMENTS(fieldPtr)-1 DO $
		fields[i] = *((*(fieldPtr[i]))[0])
	id	= ((*infoPtr).f_wd)
	sel	= ((*infoPtr).f_idx)
;print,'((*infoPtr).f_idx) = ',((*infoPtr).f_idx)
	WIDGET_CONTROL, id, SET_VALUE = fields, SET_LIST_SELECT = sel

	IF PTR_VALID( (*infoPtr).fieldsListPtr ) THEN PTR_FREE, (*infoPtr).fieldsListPtr
	(*infoPtr).fieldsListPtr	= PTR_NEW( fields )

	;--------------------------------------------------------------------
	; dimensions other than SOMBlock, XDim, and YDim (if any)
	;--------------------------------------------------------------------
	IF PTR_VALID( (*infoPtr).dimsListPtr ) THEN	$
		result = DEPOINTER( (*infoPtr).dimsListPtr )
	IF (*infoPtr).n_dims EQ 0 THEN BEGIN
		(*infoPtr).dimsListPtr	= PTR_NEW()
	ENDIF ELSE BEGIN
		(*infoPtr).dimsListPtr	= PTR_NEW( PTRARR( (*infoPtr).n_dims ) )
	ENDELSE

	startPtr = (*(fieldPtr[(*infoPtr).f_idx]))[1]
	FOR i = 0, (*infoPtr).n_dims-1 DO BEGIN
		IF PTR_VALID(startPtr) THEN BEGIN
			dimPtr = *startPtr
			dims = STRARR(N_ELEMENTS(dimPtr))
			FOR j = 0, N_ELEMENTS(dimPtr)-1 DO $
				dims[j] = *((*(dimPtr[j]))[0]) + ' ' + STRTRIM(STRING(j+1),2)
			WIDGET_CONTROL, (*infoPtr).d_lab[i], SET_VALUE = *((*(dimPtr[0]))[0])
			WIDGET_CONTROL, (*infoPtr).d_base[i], SENSITIVE = 1
			id  = (*infoPtr).d_wd[i]
			sel = (*infoPtr).d_idx[i]
			WIDGET_CONTROL, id, SET_VALUE = dims, SET_LIST_SELECT = sel
			startPtr = (*(dimPtr[(*infoPtr).d_idx[i]]))[1]

			(*((*infoPtr).dimsListPtr))[i]	= PTR_NEW( dims )
		ENDIF ELSE BEGIN
			WIDGET_CONTROL, (*infoPtr).d_wd[i], SET_VALUE = ''
			WIDGET_CONTROL, (*infoPtr).d_lab[i], SET_VALUE = ''
			WIDGET_CONTROL, (*infoPtr).d_base[i], SENSITIVE = 0
		ENDELSE
	ENDFOR
END
; update_data_droplists

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ p_wd_ev @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO p_wd_ev, event
	WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
	(*infoPtr).p_idx = event.index
	(*infoPtr).g_idx = 0
	(*infoPtr).f_idx = 0
	IF (*infoPtr).n_dims GT 0 THEN $
		(*infoPtr).d_idx = (*infoPtr).d_idx*0L
	update_data_droplists, event.top
END
; p_wd_ev

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ g_wd_ev @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO g_wd_ev, event
	WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
	(*infoPtr).g_idx = event.index
	(*infoPtr).f_idx = 0
	IF (*infoPtr).n_dims GT 0 THEN $
		(*infoPtr).d_idx = (*infoPtr).d_idx*0L
	update_data_droplists, event.top
END
; g_wd_ev

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ f_wd_ev @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO f_wd_ev, event
;print,'event.index = ',event.index
	WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
	(*infoPtr).f_idx = event.index
;print,'(*infoPtr).f_idx = ',(*infoPtr).f_idx
;print,'(*infoPtr).n_dims = ',(*infoPtr).n_dims
	IF (*infoPtr).n_dims GT 0 THEN $
		(*infoPtr).d_idx = (*infoPtr).d_idx*0L
	update_data_droplists, event.top
END
; f_wd_ev

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ d_wd_ev @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO d_wd_ev, event
	WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
	WIDGET_CONTROL, event.id, GET_UVALUE = didx
	(*infoPtr).d_idx[didx] = event.index
	IF didx LT (*infoPtr).n_dims-1 THEN $
		(*infoPtr).d_idx[didx+1:(*infoPtr).n_dims-1] = 0L
	update_data_droplists, event.top
END
; d_wd_ev

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ build_data_menu_ev @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO build_data_menu_ev, event
	widgetType = STRUPCASE( TAG_NAMES( event, /STRUCTURE_NAME ) )
;print, 'in build_data_menu_ev, widgetType = ',widgetType
	CASE widgetType OF
		'WIDGET_BUTTON': BEGIN

			WIDGET_CONTROL, event.id, GET_VALUE = buttonName
			WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr

			IF buttonName EQ 'Cancel' THEN BEGIN
				WIDGET_CONTROL, event.top, MAP = 0
				IF (*infoPtr).base2desensitize GE 0 THEN			$
					WIDGET_CONTROL, (*infoPtr).base2desensitize, /SENSITIVE
				RETURN
			ENDIF

			product_id = *((*(( (*infoPtr).ptr_tree )[(*infoPtr).p_idx]))[0])
			gridPtr    = *((*(( (*infoPtr).ptr_tree )[(*infoPtr).p_idx]))[1])
			grid_id    = *((*(gridPtr[(*infoPtr).g_idx]))[0])
			fieldPtr   = *((*(gridPtr[(*infoPtr).g_idx]))[1])
			field_id   = *((*(fieldPtr[(*infoPtr).f_idx]))[0])

			num_type = *((*(fieldPtr[(*infoPtr).f_idx]))[2])
			dimCtr   = 0

			IF (*infoPtr).n_dims GT 0 THEN BEGIN
				dim_id   = LONARR((*infoPtr).n_dims)
				startPtr = (*(fieldPtr[(*infoPtr).f_idx]))[1]
;;ckt,7/28/98            dimCtr   = 0
				FOR i = 0, (*infoPtr).n_dims-1 DO BEGIN
					IF PTR_VALID(startPtr) THEN BEGIN
						dim_id[i] = (*infoPtr).d_idx[i]
						dimPtr    = *startPtr
						startPtr  = (*(dimPtr[(*infoPtr).d_idx[i]]))[1]
						dimCtr    = dimCtr + 1
					ENDIF ELSE BEGIN
						dim_id[i] = (-1L)
					ENDELSE
				ENDFOR
				finalDataStr = product_id + '___' + grid_id    + '___' + field_id
				FOR dd = 0, dimCtr - 1 DO $
					finalDataStr = finalDataStr + '_' + STRTRIM(dim_id[dd]+1,2)

			ENDIF ELSE BEGIN
				finalDataStr = product_id + '___' + grid_id    + '___' + field_id
			ENDELSE

			;---------------------------------------------
			; Enable the "Set Plane" button (previously
			; disabled because no data was selected to
			; set any of the planes with)
			;---------------------------------------------
			topPtr = (*infoPtr).topInfoPtr

			nm = (*infoPtr).fnames[(*infoPtr).p_idx]
			IF (*infoPtr).n_dims GT 0 AND dimCtr GT 0 THEN		$
				extractInfo = { file_name  : nm,		$
					grid_name  : grid_id,			$
					field_name : field_id,			$
					num_type   : num_type,			$
					dimIdx     : dim_id[0:dimCtr-1] }	$
			ELSE							$
				extractInfo = { file_name  : nm,		$
					grid_name  : grid_id,			$
					field_name : field_id,			$
					num_type   : num_type }

			WIDGET_CONTROL, (*topPtr).dataLabel[0], SET_UVALUE = extractInfo

			;---------------------------------------------
			; Now, go through yet another case statement
			; to determine whether the string created
			; above exceeds the maximum length of the
			; widget_label that it is to be placed into;
			; if it is, then un-map the widget_label (if
			; has not already been unmapped), and map the
			; widget_text instead (if it has not already
			; been mapped). Finally, set whatever is mapped
			; to the string created above.
			;---------------------------------------------
			WIDGET_CONTROL, (*topPtr).setButton, SENSITIVE = 1
			CASE 1 OF
				( STRLEN( finalDataStr ) GT			$
				(*topPtr).dataLabelLenThresh ) AND		$
				(*topPtr).mappedDataLabelBaseIdx EQ 0: BEGIN

					WIDGET_CONTROL, (*topPtr).dataLabelBase[0], MAP = 0
					WIDGET_CONTROL, (*topPtr).dataLabelBase[1], MAP = 1
					(*topPtr).mappedDataLabelBaseIdx = 1

					END
				( STRLEN( finalDataStr ) LE			$
				(*topPtr).dataLabelLenThresh ) AND		$
				(*topPtr).mappedDataLabelBaseIdx EQ 1: BEGIN

					WIDGET_CONTROL, (*topPtr).dataLabelBase[0], MAP = 1
					WIDGET_CONTROL, (*topPtr).dataLabelBase[1], MAP = 0
					(*topPtr).mappedDataLabelBaseIdx = 0

					END
				ELSE:
			ENDCASE
;print,'@@@@@@@@@@@@@@@@@@@@@@@ (*topPtr).mappedDataLabelBaseIdx = ',(*topPtr).mappedDataLabelBaseIdx
;print,'@@@@@@@@@@@@@@@@@@@@@@@ finalDataStr = ',finalDataStr
			WIDGET_CONTROL, (*topPtr).dataLabel[(*topPtr).mappedDataLabelBaseIdx], $
				SET_VALUE = finalDataStr

;;;ckt,aug2000         ;---------------------------------------------
;;;ckt,aug2000         ; Check to see if the memory labels have been
;;;ckt,aug2000         ; enabled or not; if they have NOT, enable
;;;ckt,aug2000         ; them and grab the values of the ACT and ALT
;;;ckt,aug2000         ; resolution labels in order to calculate
;;;ckt,aug2000         ; the current memory requirements for the
;;;ckt,aug2000         ; data selected (use the functions
;;;ckt,aug2000         ; CalculateCurrentMemory and
;;;ckt,aug2000         ; ConvertFloatingMem2Str to determine and
;;;ckt,aug2000         ; "stringify" the memory value, respectively.
;;;ckt,aug2000         ; Set this value in the appropriate memory
;;;ckt,aug2000         ; label.
;;;ckt,aug2000         ;---------------------------------------------
;;;ckt,aug2000         IF NOT (*topPtr).enableMem THEN BEGIN
;;;ckt,aug2000            WIDGET_CONTROL, (*topPtr).altResMenu, GET_UVALUE = altResLabel
;;;ckt,aug2000            WIDGET_CONTROL, altResLabel, GET_VALUE = altResStr
;;;ckt,aug2000            WIDGET_CONTROL, (*topPtr).actResMenu, GET_UVALUE = actResLabel
;;;ckt,aug2000            WIDGET_CONTROL, actResLabel, GET_VALUE = actResStr
;;;ckt,aug2000            (*topPtr).enableMem = 1



;;;ckt,aug2000;----------------------------------------------------------------------
;;;ckt,aug2000;
;;;ckt,aug2000; THIS NEEDS TO BE MODIFIED SO THAT altNumFlt AND actNumFlt CAN BE USED
;;;ckt,aug2000; TO ACCURATELY DETERMINE MEMORY REQUIREMENTS FOR THE CURRENT DATASET
;;;ckt,aug2000;
;;;ckt,aug2000;----------------------------------------------------------------------
;;;ckt,aug2000IF NOT (*infoPtr).airMisrFlag THEN BEGIN
;;;ckt,aug2000            altNumFlt           =                                       $
;;;ckt,aug2000               FLOAT(STRMID(STRTRIM(altResStr,2),0,STRLEN(STRTRIM(altResStr,2))-1))
;;;ckt,aug2000            actNumFlt           =                                       $
;;;ckt,aug2000               FLOAT(STRMID(STRTRIM(actResStr,2),0,STRLEN(STRTRIM(actResStr,2))-1))
;;;ckt,aug2000ENDIF ELSE BEGIN
;;;ckt,aug2000            altNumFlt           = 275.0
;;;ckt,aug2000            actNumFlt           = 275.0
;;;ckt,aug2000ENDELSE
;;;ckt,aug2000;----------------------------------------------------------------------




;;;ckt,aug2000            calcCurrMem         = CalculateCurrentMemory(altNumFlt,actNumFlt)
;;;ckt,aug2000            (*topPtr).memCurr   = calcCurrMem
;;;ckt,aug2000            memCurrStr          = ConvertFloatingMem2Str(calcCurrMem)
;;;ckt,aug2000            WIDGET_CONTROL, (*topPtr).memCurrLabel, SET_VALUE = memCurrStr
;;;ckt,aug2000         ENDIF

			WIDGET_CONTROL, event.top, MAP = 0
			IF (*infoPtr).base2desensitize GE 0 THEN			$
				WIDGET_CONTROL, (*infoPtr).base2desensitize, /SENSITIVE
			RETURN
			END

		'WIDGET_BASE': BEGIN

			WIDGET_CONTROL, event.top, GET_UVALUE = infoPtr
			p_base          = WIDGET_INFO(event.top, /CHILD)
			WIDGET_CONTROL, p_base, GET_UVALUE = dimStruct
			max_rank        = (*infoPtr).n_dims + 3
			ptr_tree        = (*infoPtr).ptr_tree
			p_idx           = (*infoPtr).p_idx
			g_idx           = (*infoPtr).g_idx
			f_idx           = (*infoPtr).f_idx
			topPtr          = (*infoPtr).topInfoPtr
			heapVar2Destroy = (*infoPtr).heapVar2Destroy
			fnames          = (*infoPtr).fnames
			n_dims          = max_rank - 3
			IF (*infoPtr).n_dims GT 0 THEN d_idx = (*infoPtr).d_idx
			baseXsize       = MAX([dimStruct.min_base_x,event.x])
			baseYsize       = MAX([dimStruct.min_base_y,event.y])

			disable_base	= (*infoPtr).base2desensitize
			airMisr		= (*infoPtr).airMisrFlag
               
;         listXsize       = ROUND(dimStruct.list2base_ratio_x * FLOAT(baseXsize))
;         listYsize       = ROUND(dimStruct.list2base_ratio_y * FLOAT(baseYsize))

			listXsize       = baseXsize
			listYsize       = ((baseYsize - dimStruct.buttonBaseSizeY) / max_rank) - dimStruct.listBaseExtraYSize
;print,'baseYsize,listYsize=',baseYsize,listYsize
			min_base_x      = dimStruct.min_base_x
			min_base_y      = dimStruct.min_base_y
			(*infoPtr).kill_command = 'soft'
			WIDGET_CONTROL, event.top, /DESTROY
			IF n_dims GT 0 THEN BEGIN
				setup_data_menu, fnames, ptr_tree, max_rank, topPtr, heapVar2Destroy,	$
					LIST_XSIZE = listXsize, LIST_YSIZE = listYsize,			$
					MIN_BASE_X = min_base_x, MIN_BASE_Y = min_base_y,		$
					PROD_IDX = p_idx, GRID_IDX = g_idx, FIELD_IDX = f_idx,		$
					DIM_IDX = d_idx, /SHOW, AIRMISR=airMisr, DISABLE_BASE=disable_base
			ENDIF ELSE BEGIN
				setup_data_menu, fnames, ptr_tree, max_rank, topPtr, heapVar2Destroy,	$
					LIST_XSIZE = listXsize, LIST_YSIZE = listYsize,			$
					MIN_BASE_X = min_base_x, MIN_BASE_Y = min_base_y,		$
					PROD_IDX = p_idx, GRID_IDX = g_idx, FIELD_IDX = f_idx, /SHOW,	$
					AIRMISR=airMisr, DISABLE_BASE=disable_base
			ENDELSE
			END
		ELSE:
	ENDCASE
END
; build_data_menu_ev

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ kill_data_menu @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO kill_data_menu, tlb
	WIDGET_CONTROL, tlb, GET_UVALUE = infoPtr

	; Re-sensitize data selection interface.
	WIDGET_CONTROL, (*infoPtr).base2desensitize, SENSITIVE = 1

	IF (*infoPtr).kill_command NE 'soft' THEN $
		PTR_FREE, (*infoPtr).heapVar2Destroy
	PTR_FREE, infoPtr
END
; kill_data_menu

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ setup_data_menu @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO setup_data_menu, fn, prod_ptr_tree, max_rank, topInfoPtr, heapVar2Destroy, $
                     LIST_XSIZE = x_dim, LIST_YSIZE = y_dim,                   $
                     MIN_BASE_X = min_base_x, MIN_BASE_Y = min_base_y,         $
                     PROD_IDX = p_idx, GRID_IDX = g_idx,                       $
                     FIELD_IDX = f_idx, DIM_IDX = d_idx, SHOW = show,		$
                     AIRMISR=airMisr, DISABLE_BASE=disable_base

   IF NOT KEYWORD_SET(show) THEN show = 0

   parent = WIDGET_INFO((*topInfoPtr).dataMenu, /PARENT)
   parent = WIDGET_INFO(parent, /PARENT)
   parent = WIDGET_INFO(parent, /PARENT)
   b = WIDGET_BASE( /COLUMN,                        $
                    TITLE = 'MISR Data Menu',       $
                    KILL_NOTIFY = 'kill_data_menu', $
                    /BASE_ALIGN_CENTER,             $
                    /TLB_SIZE_EVENTS,               $
                    MAP = show,                     $
                    GROUP_LEADER = parent,          $
                    EVENT_PRO = 'build_data_menu_ev' )

   IF NOT KEYWORD_SET(p_idx) THEN p_idx=0
   IF NOT KEYWORD_SET(g_idx) THEN g_idx=0
   IF NOT KEYWORD_SET(f_idx) THEN f_idx=0

   IF NOT KEYWORD_SET(x_dim) THEN x_dim=200
   IF NOT KEYWORD_SET(y_dim) THEN y_dim=75

   p_base = WIDGET_BASE( b,       $
                         /FRAME,  $
                         /COLUMN, $
                         /BASE_ALIGN_CENTER )
   p_label= WIDGET_LABEL( p_base,            $
                          VALUE = 'PRODUCT', $
                          FONT = GetCorrectFont('courier2bold') )
   p_wd   = WIDGET_LIST( p_base,                                $
                         VALUE = '',                            $
                         SCR_XSIZE = x_dim,                     $
                         SCR_YSIZE = y_dim,                     $
                         FONT = GetCorrectFont('courier2bold'), $
                         EVENT_PRO = 'p_wd_ev')
   g_base = WIDGET_BASE( b,                                     $
                         /FRAME,                                $
                         /COLUMN,                               $
                         /BASE_ALIGN_CENTER )
   g_label= WIDGET_LABEL( g_base,                               $
                          VALUE = 'GRID',                       $
                          FONT = GetCorrectFont('courier2bold') )
   g_wd   = WIDGET_LIST( g_base,                                $
                         VALUE = '',                            $
                         SCR_XSIZE = x_dim,                     $
                         SCR_YSIZE = y_dim,                     $
                         FONT = GetCorrectFont('courier2bold'), $
                         EVENT_PRO = 'g_wd_ev')
   f_base = WIDGET_BASE( b,                                     $
                        /FRAME,                                 $
                        /COLUMN,                                $
                        /BASE_ALIGN_CENTER )
   f_label= WIDGET_LABEL( f_base,                               $
                          VALUE = 'FIELD',                      $
                          FONT = GetCorrectFont('courier2bold') )
   f_wd   = WIDGET_LIST( f_base,                                $
                         VALUE = '',                            $
                         SCR_XSIZE = x_dim,                     $
                         SCR_YSIZE = y_dim,                     $
                         FONT = GetCorrectFont('courier2bold'), $
                         EVENT_PRO = 'f_wd_ev')
                         
;------------------------------------------------------------------------
; These lines were added to desensitize the data selection base while the
; data menu base was on screen
;------------------------------------------------------------------------
base2desensitize	= (-1)
IF KEYWORD_SET(disable_base) THEN base2desensitize = disable_base

   IF max_rank GT 3 THEN BEGIN
      IF NOT KEYWORD_SET(d_idx) THEN d_idx = LONARR(max_rank-3)
      d_wd   = d_idx*0L
      d_lab  = d_idx*0L
      d_base = d_idx*0L
      FOR j = 0, max_rank-4 DO BEGIN
         d_base[j] = WIDGET_BASE( b, /FRAME, /COLUMN, /BASE_ALIGN_CENTER )
         d_base1   = WIDGET_BASE( d_base[j], /ROW, /BASE_ALIGN_CENTER )
         d_label1  = WIDGET_LABEL( d_base1,                  $
                                   VALUE = 'DIMENSION: ',    $
                                   FONT = GetCorrectFont('courier2bold') )
         d_lab[j]  = WIDGET_LABEL( d_base1,                  $
                                   VALUE = '',               $
                                   /DYNAMIC_RESIZE,          $
                                   FONT = GetCorrectFont('courier2bold') )
         d_wd[j]   = WIDGET_LIST( d_base[j],                             $
                                  VALUE = '',                            $
                                  SCR_XSIZE = x_dim,                     $
                                  SCR_YSIZE = y_dim,                     $
                                  FONT = GetCorrectFont('courier2bold'), $
                                  UVALUE = j,                            $
                                  EVENT_PRO = 'd_wd_ev')
      ENDFOR

      info = {                                    $
               fnames          : fn,              $
               base2desensitize:base2desensitize, $
               kill_command    : '',              $
               topInfoPtr      : topInfoPtr,      $
               heapVar2Destroy : heapVar2Destroy, $
               ptr_tree        : prod_ptr_tree,   $
               p_idx           : p_idx,           $
               g_idx           : g_idx,           $
               f_idx           : f_idx,           $
               d_idx           : d_idx,           $
               p_wd            : p_wd,            $
               g_wd            : g_wd,            $
               f_wd            : f_wd,            $
               d_wd            : d_wd,            $
               d_lab           : d_lab,           $
               d_base          : d_base,          $
               n_dims          : max_rank-3,      $
		gridsListPtr	: PTR_NEW(),		$
		fieldsListPtr	: PTR_NEW(),		$
		dimsListPtr	: PTR_NEW(),		$
               airMisrFlag     :KEYWORD_SET(airMisr) }
   ENDIF ELSE BEGIN
      info = {                                    $
               fnames          : fn,              $
               base2desensitize:base2desensitize, $
               kill_command    : '',              $
               topInfoPtr      : topInfoPtr,      $
               heapVar2Destroy : heapVar2Destroy, $
               ptr_tree        : prod_ptr_tree,   $
               p_idx           : p_idx,           $
               g_idx           : g_idx,           $
               f_idx           : f_idx,           $
               p_wd            : p_wd,            $
               g_wd            : g_wd,            $
               f_wd            : f_wd,            $
               n_dims          : 0,               $
		gridsListPtr	: PTR_NEW(),		$
		fieldsListPtr	: PTR_NEW(),		$
		dimsListPtr	: PTR_NEW(),		$
               airMisrFlag     :KEYWORD_SET(airMisr) }
   ENDELSE

   bBase = WIDGET_BASE( b,                                       $
                        /ROW,                                    $
                        /BASE_ALIGN_CENTER,                      $
                        EVENT_PRO = 'build_data_menu_ev' )
   q     = WIDGET_BUTTON( bBase,                                 $
                          VALUE = 'OK',                          $
                          FONT = GetCorrectFont('courier2bold'), $
                          EVENT_PRO = 'build_data_menu_ev' )
   q     = WIDGET_BUTTON( bBase,                                 $
                          VALUE = 'Cancel',                      $
                          FONT = GetCorrectFont('courier2bold'), $
                          EVENT_PRO = 'build_data_menu_ev' )
   WIDGET_CONTROL, b, /REALIZE
   infoPtr = PTR_NEW( info, /NO_COPY )
   WIDGET_CONTROL, b, SET_UVALUE = infoPtr
   update_data_droplists, b

   geom = WIDGET_INFO( b, /GEOMETRY )

   base_xsize  = geom.scr_xsize + (2* geom.margin)
   base_ysize  = geom.scr_ysize + (2* geom.margin)

   IF NOT KEYWORD_SET(min_base_x) THEN min_base_x  = base_xsize
   IF NOT KEYWORD_SET(min_base_y) THEN min_base_y  = base_ysize

;   geom = WIDGET_INFO( p_wd, /GEOMETRY )
;   list_xsize = geom.scr_xsize + (2* geom.margin)
;   list_ysize = geom.scr_ysize + (2* geom.margin)

;   list2base_ratio_x = FLOAT(list_xsize) / FLOAT(base_xsize)
;   list2base_ratio_y = FLOAT(list_ysize) / FLOAT(base_ysize)

   geom = WIDGET_INFO( bBase, /GEOMETRY )
   buttonBaseSizeY = geom.scr_ysize + (2* geom.margin)

   geom        = WIDGET_INFO( f_base, /GEOMETRY )
   baseY       = geom.scr_ysize + (2* geom.margin)
   geom        = WIDGET_INFO( f_wd, /GEOMETRY )
   listBaseExtraYSize = baseY -  (geom.scr_ysize + (2* geom.margin))
; print,'y size of list = ',(geom.scr_ysize + (2* geom.margin))
;print,'base_ysize, buttonBaseSizeY = ',base_ysize, buttonBaseSizeY
;   WIDGET_CONTROL, p_base, set_uvalue = { list2base_ratio_x : list2base_ratio_x, $
;                                          list2base_ratio_y : list2base_ratio_y, $
;                                          min_base_x        : min_base_x,        $
;                                          min_base_y        : min_base_y,        $
;                                          buttonBaseSizeY   : buttonBaseSizeY }
   WIDGET_CONTROL, p_base, set_uvalue = { min_base_x        : min_base_x,        $
                                          min_base_y        : min_base_y,        $
                                          buttonBaseSizeY   : buttonBaseSizeY,   $
                                          listBaseExtraYSize: listBaseExtraYSize }

   stash_base = (*topInfoPtr).stashBase
   WIDGET_CONTROL, stash_base, SET_UVALUE = b

END
; setup_data_menu

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ build_data_menu @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO build_data_menu, fn, infoPtr, _Extra = e
;print,'in build_data_menu, tag_names(e) = '
;	if (size(e))[0] ne 0 then print,tag_names(e)

	msg = [ 'Creating data menu for selected orbit/path;', $
		'please be patient, as this may take a few seconds...' ]
	parent = WIDGET_INFO((*infoPtr).dataMenu, /PARENT)
	parent = WIDGET_INFO(parent, /PARENT)
	parent = WIDGET_INFO(parent, /PARENT)
	tmpb = WIDGET_BASE( /COLUMN, GROUP_LEADER = parent, /BASE_ALIGN_CENTER )
	tmpl = WIDGET_LABEL( tmpb, VALUE = msg[0], /ALIGN_CENTER )
	tmpl = WIDGET_LABEL( tmpb, VALUE = msg[1], /ALIGN_CENTER )
	WIDGET_CONTROL, tmpb, /REALIZE

	WIDGET_CONTROL, (*infoPtr).stashBase, GET_UVALUE = dataMenuBase
;print,'WIDGET_INFO(dataMenuBase,/VALID_ID)=',WIDGET_INFO(dataMenuBase,/VALID_ID)
	IF WIDGET_INFO(dataMenuBase,/VALID_ID) THEN  $
		WIDGET_CONTROL, dataMenuBase, /DESTROY

	heapList1 = PTR_VALID()

	n_files       = N_ELEMENTS(fn)
	prod_ptr_tree = PTRARR(n_files)
	max_rank      = 0
	i_success	 = 0
	FOR i = 0, n_files - 1 DO BEGIN
;print,'parsing file = ',fn[i]
		ret = parse_misr_grid_file(fn[i])

		IF ret.success THEN BEGIN
			prod_ptr_tree[i_success] = PTR_NEW(PTRARR(2))
;print,'++++++++++++++++++++++++++++'
;print,'ret.file_type = ',ret.file_type
;print,'++++++++++++++++++++++++++++'
			(*(prod_ptr_tree[i_success]))[0] = PTR_NEW(ret.file_type)
			(*(prod_ptr_tree[i_success]))[1] = PTR_NEW(ret.grid_info)
			IF ret.max_rank GT max_rank THEN max_rank = ret.max_rank
			i_success	= i_success + 1
		ENDIF ELSE BEGIN
;print,'**************** problem with ' + fn[i]
		ENDELSE

	ENDFOR
   
	heapList2       = PTR_VALID()
   
   
	IF PTR_VALID(heapList2[0]) AND ( N_ELEMENTS(heapList2) GT N_ELEMENTS(heapList1) ) THEN BEGIN
		heapVar2Destroy	= PTRARR(N_ELEMENTS(heapList2)-N_ELEMENTS(heapList1))
		heapCtr		= 0LL
		FOR i = 0LL, N_ELEMENTS(heapList2)-1LL DO BEGIN
			ptr      = heapList2[i]
			idx2free = WHERE(heapList1 EQ ptr,cnt)
			IF cnt LE 0 THEN BEGIN
				heapVar2Destroy[heapCtr] = ptr
				heapCtr = heapCtr + 1
			ENDIF
		ENDFOR
	ENDIF ELSE BEGIN
			heapVar2Destroy	= PTRARR(1)
	ENDELSE

	WIDGET_CONTROL, tmpb, /DESTROY
   
	IF i_success LE 0 THEN BEGIN
		msg	= [								$
			'No valid files encountered... data menu not created!',	$
			'Please check initial orbit/path values used to',	$
			'initiate this session of misr_view.  Also, make sure',	$
			'that the data files to be accessed exist and are on-line.' ]
		res	= DIALOG_MESSAGE( msg, /ERROR )
		PTR_FREE, prod_ptr_tree
		RETURN
	ENDIF
   
	prod_ptr_tree	= prod_ptr_tree[0:i_success-1]
   
	setup_data_menu, fn, prod_ptr_tree, max_rank, infoPtr, heapVar2Destroy, _Extra = e

END
; build_data_menu
