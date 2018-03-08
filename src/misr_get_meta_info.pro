@depointer.pro
@misr_display_meta.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ misr_get_meta_info @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION misr_get_meta_info, fileName,					$
			       MENU = menu,				$
			       METADATA_NAME = metadata_name,		$
			       METADATA_FIELDS = metadata_fields
;-----------------------------------------------------------------------
; function which returns the total number of blocks stored in the
; current HDF file
;
;	INPUT PARAMETERS:
;		fileName	- fully-qualified name of HDF-EOS file
;	INPUT KEYWORDS:
;		MENU		- displays list of all metadata and allows
;				  user to interactively select
;		METADATA_NAME	- a string containing the name of the metadata to consider
;		METADATA_FIELDS - a string array containing the names of the fields
;				  to retrieve; if this is not set, all fields will be
;				  returned.
;
;	RETURNED VALUES:
;		metadata_values	- pointer array, with each entry pointing to
;				  the value of requested metadatum
;		NOTE: a returned pointer array entry which has a NULL returned
;		value indicates that particular metadatum was NOT found!
;		PTRARR(1)	- on error
;-----------------------------------------------------------------------

	routineName	= '----- misr_get_meta_info -----'
	returnMsg	= 'Returning...'
	;-----------------------------------------------
	; basic error catch mechanism for any HDF errors
	;-----------------------------------------------
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
		RETURN, PTRARR(1)
	ENDIF
	IF KEYWORD_SET(menu) AND KEYWORD_SET(metadata_name) THEN BEGIN
		eMsg	= 'Cannot simultaneously set keywords!'
		res	= DIALOG_MESSAGE(				$
				[ routineName,				$
				  eMsg,					$
				  fileName,				$
				  returnMsg ],				$
				  /ERROR )
		RETURN, PTRARR(1)
	ENDIF
	
	IF NOT KEYWORD_SET(menu) AND NOT KEYWORD_SET(metadata_name) THEN BEGIN
		eMsg	= 'At least one keyword must be set!'
		res	= DIALOG_MESSAGE(				$
				[ routineName,				$
				  eMsg,					$
				  fileName,				$
				  returnMsg ],				$
				  /ERROR )
		RETURN, PTRARR(1)
	ENDIF

	;--------------------------------
	; set up tag value to search upon
	;--------------------------------
	VDATA_TAG	= 1962
	TRUE		= 1
	FALSE		= 0
	FIELD_SEPARATOR	= ','
	
	build_list	= FALSE
	
	IF KEYWORD_SET(menu) THEN build_list = TRUE

	;---------------------------------
	; open up file as vanilla HDF file
	;---------------------------------
	fileID		= HDF_OPEN( fileName, /READ )

	IF fileID LT 0 THEN BEGIN
		eMsg	= 'Routine HDF_OPEN failed on'
		res	= DIALOG_MESSAGE(				$
				[ routineName,				$
				  eMsg,					$
				  fileName,				$
				  returnMsg ],				$
				  /ERROR )
		RETURN, PTRARR(1)
	ENDIF

	;--------------------------------
	; obtain number of VDATA sets
	;--------------------------------
	nVdatas	= HDF_NUMBER( fileID, TAG = VDATA_TAG )
	
	IF nVdatas LE 0 THEN BEGIN
		eMsg	= 'No metadata (VDATA) found in file'
		res	= DIALOG_MESSAGE(				$
				[ routineName,				$
				  eMsg,					$
				  fileName,				$
				  returnMsg ],				$
				  /ERROR )
		RETURN, PTRARR(1)
	ENDIF
	
	IF build_list THEN BEGIN
		list_ptrarr	= PTRARR(nVdatas)
		list_idx	= 0L
	ENDIF
		
	vRef			= (-1)

	vdata_ctr		= 0L
	done			= FALSE
	nBlocks			= 0L
	metadata_values		= PTRARR(1)

	;--------------------------------
	; go through WHILE loop until the
	; 'PerBlockMetadataCommon' VDATA
	; is located
	;--------------------------------
	WHILE NOT done AND vdata_ctr LT nVdatas DO BEGIN
		vRef	= HDF_VD_GETID( fileID, vRef )
		IF vRef LT 0 THEN BEGIN
			eMsg	= 'Routine HDF_VD_GETID failed on ' +	$
				  'VDATA #' + STRTRIM( STRING(i+1), 2 )
			res	= DIALOG_MESSAGE(			$
					[ routineName,			$
					  eMsg,				$
					  fileName,			$
					  returnMsg ],			$
					  /ERROR )
			RETURN, PTRARR(1)
		ENDIF
		vdataID	= HDF_VD_ATTACH( fileID, vRef, /READ )
		IF vdataID LT 0 THEN BEGIN
			eMsg	= 'Routine HDF_VD_GETID failed on ' +	$
				  'VDATA #' + STRTRIM( STRING(i+1), 2 )
			res	= DIALOG_MESSAGE( [ routineName,	$
						    eMsg, 		$
						    fileName,		$
						    returnMsg ],	$
						    /ERROR )
			RETURN, PTRARR(1)
		ENDIF
		HDF_VD_GET, vdataID,					$
			COUNT		= nRec,				$
			INTERLACE	= iLace,			$
			FIELDS		= cField,			$
			SIZE		= sz,				$
			NFIELDS		= n_fields,			$
			NAME		= nm
		IF build_list THEN BEGIN
			name_field_ptrarr	= PTRARR(2)
			name_field_ptrarr[0]	= PTR_NEW(nm)
			name_field_ptrarr[1]	= PTR_NEW(STR_SEP(cField,FIELD_SEPARATOR))
			list_ptrarr[list_idx]	= PTR_NEW(name_field_ptrarr,/NO_COPY)
			list_idx		= list_idx + 1L
		ENDIF ELSE BEGIN
			IF STRTRIM(STRUPCASE(metadata_name),2) EQ STRTRIM(STRUPCASE(nm),2) THEN BEGIN
				field_list		= STRTRIM(STR_SEP(cField,FIELD_SEPARATOR),2)
				
				IF KEYWORD_SET(metadata_fields) THEN BEGIN
					metadata_values	= PTRARR(N_ELEMENTS(metadata_fields))
				ENDIF ELSE BEGIN
					metadata_values	= PTRARR(N_ELEMENTS(field_list))
					metadata_fields	= field_list
				ENDELSE
			
				FOR i = 0, N_ELEMENTS(metadata_values) - 1 DO BEGIN
					idx	= WHERE(STRUPCASE(field_list) EQ STRUPCASE(metadata_fields[i]), cnt)
					IF cnt GT 0 THEN BEGIN
						field_name	= field_list[idx[0]]
						res		= HDF_VD_READ( vdataID,		$
									       val,		$
									       FIELDS = field_name )
						metadata_values[i]				$
								= PTR_NEW(val, /NO_COPY)
					ENDIF
				ENDFOR
				done	= TRUE
			ENDIF
		ENDELSE
		vdata_ctr	= vdata_ctr + 1
		HDF_VD_DETACH, vdataID
		
	ENDWHILE

	;--------------------------------
	; close HDF file
	;--------------------------------
	HDF_CLOSE, fileID
	
	IF NOT build_list THEN RETURN, metadata_values
	
	res	= misr_display_meta(list_ptrarr)
	IF N_ELEMENTS(res) LT 2 THEN BEGIN
		PTR_FREE, res
		RETURN, PTRARR(1)
	ENDIF ELSE BEGIN
		nm	= *(res[0])
		flds	= *(res[1])
		PTR_FREE, res
		RETURN, misr_get_meta_info( fileName,		$
			       METADATA_NAME = nm,		$
			       METADATA_FIELDS = flds )
	ENDELSE
	
END
;misr_get_meta_info

