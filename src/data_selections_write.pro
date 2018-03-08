FUNCTION data_selections_write, data_selections_struct, directory, FILENAME = filename

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== data_selections_write =========='
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
		RETURN, 0
	ENDIF

IF NOT KEYWORD_SET( filename ) THEN filename = dialog_pickfile_wrapper( TITLE = 'Please Enter A New Filename.', PATH = directory )
IF filename EQ '' THEN RETURN, 0
IF (FINDFILE( filename ))[0] THEN BEGIN
	message		= [ filename, 'WARNING:  This file exists.', 'Please choose another filename.' ]
	ret		= DIALOG_MESSAGE( message )
	RETURN, 0
ENDIF
;--------------------------------------------------
; Set the directory variable which the calling
; routine (misr_view.pro) will store as the default
; path next time this is called.
;--------------------------------------------------
FILEBREAK, filename, DIRECTORY = directory
;help,filename
;help,directory

OPENW, lun, filename, /GET_LUN

PRINTF, lun, 'orbit=',		STRTRIM( data_selections_struct.orbit, 2 )			;string
PRINTF, lun, 'path=',		STRTRIM( data_selections_struct.path, 2 )			;string
PRINTF, lun, 'blockStart=',	STRTRIM( data_selections_struct.blockStart, 2 )			;string
PRINTF, lun, 'blockEnd=',	STRTRIM( data_selections_struct.blockEnd, 2 )			;string
PRINTF, lun, 'crossTrackRes=',	STRTRIM( data_selections_struct.actRes, 2 )			;string
PRINTF, lun, 'alongTrackRes=',	STRTRIM( data_selections_struct.altRes, 2 )			;string
PRINTF, lun, 'rotationAngle=',	STRTRIM( data_selections_struct.rotationAngle, 2 )		;string
PRINTF, lun, 'nPlanes=',	STRTRIM( data_selections_struct.nPlanes, 2 )			;integer
FILEBREAK, data_selections_struct.catalogFile, DIRECTORY=catalog_directory, FILE=catalog_filename
PRINTF, lun, 'catalogDir=',	STRTRIM( catalog_directory, 2 )					;string
PRINTF, lun, 'catalogFile=',	STRTRIM( catalog_filename, 2 )					;string
FILEBREAK, data_selections_struct.airMisrFile, DIRECTORY=airMisr_directory, FILE=airMisr_filename
PRINTF, lun, 'airMisrDir=',	STRTRIM( airMisr_directory, 2 )					;string
PRINTF, lun, 'airMisrFile=',	STRTRIM( airMisr_filename, 2 )					;string

;--------------------------------------------------
; Any subset of planes may be active or inactive.
; Inactive planes simply have no data, for example:
; plane1:product=''
;--------------------------------------------------
FOR plane = 0, data_selections_struct.nPlanes-1 DO BEGIN
	CASE STRTRIM(STRUPCASE(!VERSION.OS),2) OF
		'WIN32':os_type='DOS'
		ELSE: os_type='UNIX'
	ENDCASE
	FILEBREAK, data_selections_struct.product[plane], DIRECTORY=product_dir, FILE=product,OS=os_type
	grid		= data_selections_struct.grid[plane]
	field		= data_selections_struct.field[plane]
	numberType	= data_selections_struct.num_type[plane]
	PRINTF, lun, 'plane', STRTRIM( plane, 2 ), ':', 'directory=',	STRTRIM( product_dir, 2 )
	PRINTF, lun, 'plane', STRTRIM( plane, 2 ), ':', 'product=',	STRTRIM( product, 2 )
	PRINTF, lun, 'plane', STRTRIM( plane, 2 ), ':', 'grid=',	STRTRIM( grid, 2 )
	PRINTF, lun, 'plane', STRTRIM( plane, 2 ), ':', 'field=',	STRTRIM( field, 2 )
	PRINTF, lun, 'plane', STRTRIM( plane, 2 ), ':', 'numberType=',	STRTRIM( numberType, 2 )
	IF PTR_VALID( data_selections_struct.extraDims[plane] ) THEN BEGIN
		extraDims	= *(data_selections_struct.extraDims[plane])	;pointer array
		PRINTF, lun, 'plane', STRTRIM( plane, 2 ), ':', 'extraDims=',	STRTRIM( extraDims, 2 )
	ENDIF
ENDFOR

FREE_LUN, lun

RETURN, 1

END
