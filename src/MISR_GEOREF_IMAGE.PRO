;01234567890123456789012345678901234567890123456789012345678901234567891
;+
;==========================================================================
;
;Module Name:	MISR_GEOREF_IMAGE
;
;Call Protocol:	obj = OBJ_NEW( 'MISR_GEOREF_IMAGE' )
;
;==========================================================================
;
;		Jet Propulsion Laboratory (JPL)
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
;		Jeffrey R Hall		jeffrey.r.hall@jpl.nasa.gov
;
;===========================================================================
;
;Description:
;
;	This is an IDL object which displays georeferenced images.
;
;Input Parameters:
;
;	Type	Name		Units		Purpose
;	-------------------------------------------------------------------

;-----------------------------------------------------------------------------
;Keywords:
;
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
;	INT	success		Returned if instatiation of object is
;				successful
;	INT	failure		Returned if instatiation of object is
;				unsuccessful
;
;Known Bugs and Limitations:
;
;Parent(s):
;
;	<parents, if any>
;
;Routines Called (all within this file):
;
;	<METHODS>
;
;	<OTHER ROUTINES>
;
;Files Accessed:
;
;	<files accessed, if any>
;
;Revision History:
;
;	MAR-99 (CKT) - Initial creation.
;	MAY-99 (CKT) - Added encapsulated routines for angle display.
;
;Notes:
;
;
;=============================================================================
;-
;
@GEOREF_IMAGE.PRO
@scroll_tool.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ MISR_GEOREF_IMAGE::SaveBinary @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_GEOREF_IMAGE::SaveBinary, out_fname
	num_types	= [ 'UNDEFINED',					$
			    'BYTE',						$
			    'INT16',						$
			    'INT32',						$
			    'FLOAT32',						$
			    'DOUBLE',						$
			    'UNDEFINED',					$
			    'STRING',						$
			    'UNDEFINED',					$
			    'UNDEFINED',					$
			    'UNDEFINED',					$
			    'UNDEFINED',					$
			    'UINT16',						$
			    'UINT32',						$
			    'LONG64',						$ ;long64
			    'ULONG64' ]						  ;ulong64

	num_arr		= [ 'UNDEFINED',					$
			    'BYTARR',						$
			    'INTARR',						$
			    'LONARR',						$
			    'FLTARR',						$
			    'DBLARR',						$
			    'UNDEFINED',					$
			    'STRARR',						$
			    'UNDEFINED',					$
			    'UNDEFINED',					$
			    'UNDEFINED',					$
			    'UNDEFINED',					$
			    'UINTARR',						$
			    'ULONARR',						$
			    'LON64ARR',						$ ;long64
			    'ULON64ARR' ]					  ;ulong64


	;=======================================================================
	; basic error catch mechanism, generally for file read or write when
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== GEOREF_IMAGE::SaveBinary =========='
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
		RETURN
	ENDIF

	first_fname	= STRTRIM( (*(SELF.data_source_filename_ptr))[0], 2 )

	airmisr_data	= first_fname NE 'UNKNOWN SOURCE FILE' AND		$
			  SELF.misr_start_block LT 0 AND			$
			  SELF.misr_n_blocks LT 0

	misr_data	= first_fname NE 'UNKNOWN SOURCE FILE' AND		$
			  SELF.misr_start_block GT 0 AND			$
			  SELF.misr_n_blocks GT 0

	IF airmisr_data THEN BEGIN
		SELF->GEOREF_IMAGE::SaveBinary, out_fname
		RETURN
	ENDIF


	n_datasets		= 1
	plane_display_status	= SELF->GetPlaneDisplayStatus()
	idx			= (WHERE( plane_display_status EQ 1 ))[0]
	;
	; The above statement returns the index of the one displayed band, but
	; it DOES NOT tell the program the index of the dataset displayed within the
	; band of interest, hence the CASE statement below:
	;
	CASE idx[0] OF
		0: idx = SELF.redBandIdx
		1: idx = SELF.grnBandIdx
		2: idx = SELF.bluBandIdx
		ELSE:
	ENDCASE
	data_ptrs		= [(*(SELF.dataPtr))[idx]]
	src_files		= ['']
	data_descs		= ['']
	IF N_ELEMENTS(*(SELF.data_source_filename_ptr))-1 GE idx THEN src_files = [(*(SELF.data_source_filename_ptr))[idx]]
	IF N_ELEMENTS(*(SELF.data_description_ptr))-1 GE idx THEN data_descs = [(*(SELF.data_description_ptr))[idx]]

	out_fnames		= [out_fname]
	out_finfonames		= [out_fname+'_info']

	IF  PTR_VALID((SELF.lonImgPtr)[1]) AND				$
	    PTR_VALID((SELF.latImgPtr)[1]) THEN BEGIN
		n_datasets	= n_datasets + 2
		src_files	= [ SELF.lon_source_filename,		$
				    SELF.lat_source_filename,		$
				    src_files ]
		data_ptrs	= [ (SELF.lonImgPtr)[0],		$
				    (SELF.latImgPtr)[0],		$
				    data_ptrs ]
		data_descs	= [ 'GRID:Standard, FIELD:GeoLongitude',	$
				    'GRID:Standard, FIELD:GeoLatitude',		$
				    data_descs ]

		tmp		= STR_SEP( out_fname, GetDirectoryDivider() )
		filename_only	= tmp[N_ELEMENTS(tmp)-1]
		qualified_dir	= tmp[0]
		FOR g = 1, N_ELEMENTS(tmp)-2 DO qualified_dir = qualified_dir + GetDirectoryDivider() + tmp[g]
		IF STRLEN(qualified_dir) GT 0 THEN qualified_dir = qualified_dir + GetDirectoryDivider()

		tmp		= STR_SEP( filename_only, '.' )
		n_parts		= N_ELEMENTS(tmp)
		IF n_parts GT 1 THEN BEGIN
			fname_start = tmp[0]

			FOR g = 1, N_ELEMENTS(tmp)-2 DO fname_start = fname_start + '.' + tmp[g]

			suffix	= tmp[N_ELEMENTS(tmp)-1]

			out_fnames	= [								$
						qualified_dir + fname_start + '_longitude.' + suffix,	$
						qualified_dir + fname_start + '_latitude.' + suffix,	$
						out_fnames ]
			out_finfonames	= [									$
						qualified_dir + fname_start + '_longitude.' + suffix + '_info',	$
						qualified_dir + fname_start + '_latitude.' + suffix + '_info',	$
						out_finfonames ]
		ENDIF ELSE BEGIN
			out_fnames	= [								$
						out_fnames + '_longitude',				$
						out_fnames + '_latitude',				$
						out_fnames ]
			out_finfonames	= 	[							$
						out_fnames + '_longitude_info',				$
						out_fnames + '_latitude_info',				$
						out_finfonames ]
		ENDELSE
	ENDIF

	IF PTR_VALID( SELF.anglesPtr ) THEN BEGIN
		n_datasets	= n_datasets + 2
		src_files	= [ (*(SELF.anglesPtr)).source_file,	$
				    (*(SELF.anglesPtr)).source_file,	$
				    src_files ]
		data_ptrs	= [ (*(SELF.anglesPtr)).solarAzimuth,	$
				    (*(SELF.anglesPtr)).solarZenith,	$
				    data_ptrs ]
		data_descs	= [ 'GRID:GeometricParameters, FIELD:SolarAzimuth',	$
				    'GRID:GeometricParameters, FIELD:SolarZenith',	$
				    data_descs ]

		tmp		= STR_SEP( out_fname, GetDirectoryDivider() )
		filename_only	= tmp[N_ELEMENTS(tmp)-1]
		qualified_dir	= tmp[0]
		FOR g = 1, N_ELEMENTS(tmp)-2 DO qualified_dir = qualified_dir + GetDirectoryDivider() + tmp[g]
		IF STRLEN(qualified_dir) GT 0 THEN qualified_dir = qualified_dir + GetDirectoryDivider()

		tmp		= STR_SEP( filename_only, '.' )
		n_parts		= N_ELEMENTS(tmp)
		IF n_parts GT 1 THEN BEGIN
			fname_start = tmp[0]

			FOR g = 1, N_ELEMENTS(tmp)-2 DO fname_start = fname_start + '.' + tmp[g]

			suffix	= tmp[N_ELEMENTS(tmp)-1]

			out_fnames	= [								$
						qualified_dir + fname_start + '_solar_az.' + suffix,	$
						qualified_dir + fname_start + '_solar_zen.' + suffix,	$
						out_fnames ]
			out_finfonames	= [									$
						qualified_dir + fname_start + '_solar_az.' + suffix + '_info',	$
						qualified_dir + fname_start + '_solar_zen.' + suffix + '_info',	$
						out_finfonames ]
		ENDIF ELSE BEGIN
			out_fnames	= [								$
						out_fname + '_solar_az',				$
						out_fname + '_solar_zen',				$
						out_fnames ]
			out_finfonames	= 	[							$
						out_fname + '_solar_az_info',				$
						out_fname + '_solar_zen_info',				$
						out_finfonames ]
		ENDELSE

		n_angles	= N_ELEMENTS(*(SELF.valid_img_idx_ptr))


		FOR i = 0, n_angles - 1 DO BEGIN
			az_field	= (*(SELF.anglesPtr)).cameraAzField.slot[(*(SELF.valid_img_idx_ptr))[i]]
			zen_field	= (*(SELF.anglesPtr)).cameraZenField.slot[(*(SELF.valid_img_idx_ptr))[i]]

			new_az_desc	= 'GRID:GeometricParameters, FIELD:' + STRTRIM(az_field, 2 )
			new_zen_desc	= 'GRID:GeometricParameters, FIELD:' + STRTRIM(zen_field, 2 )

			idx		= WHERE( data_descs EQ new_az_desc, cnt )

			IF cnt LE 0 THEN BEGIN

				az_ptr		= (*(SELF.anglesPtr)).cameraAzimuth.slot[(*(SELF.valid_img_idx_ptr))[i]]
				zen_ptr		= (*(SELF.anglesPtr)).cameraZenith.slot[(*(SELF.valid_img_idx_ptr))[i]]
				n_datasets	= n_datasets + 2
				src_files	= [ (*(SELF.anglesPtr)).source_file,			$
						    (*(SELF.anglesPtr)).source_file,			$
						    src_files ]
				data_ptrs	= [ az_ptr,						$
						    zen_ptr,						$
						    data_ptrs ]
				data_descs	= [ new_az_desc,					$
						    new_zen_desc,					$
						    data_descs ]




				az_field2use	= STRCOMPRESS(az_field,/REMOVE_ALL)
				zen_field2use	= STRCOMPRESS(zen_field,/REMOVE_ALL)

				tmp		= STR_SEP( out_fname, GetDirectoryDivider() )
				filename_only	= tmp[N_ELEMENTS(tmp)-1]
				qualified_dir	= tmp[0]
				FOR g = 1, N_ELEMENTS(tmp)-2 DO qualified_dir = qualified_dir + GetDirectoryDivider() + tmp[g]
				IF STRLEN(qualified_dir) GT 0 THEN qualified_dir = qualified_dir + GetDirectoryDivider()

				tmp		= STR_SEP( filename_only, '.' )
				n_parts		= N_ELEMENTS(tmp)
				IF n_parts GT 1 THEN BEGIN
					fname_start = tmp[0]

					FOR g = 1, N_ELEMENTS(tmp)-2 DO fname_start = fname_start + '.' + tmp[g]

					suffix	= tmp[N_ELEMENTS(tmp)-1]

					out_fnames	= [								$
								qualified_dir + fname_start + '_'+az_field2use+'.' + suffix,	$
								qualified_dir + fname_start + '_'+zen_field2use+'.' + suffix,	$
								out_fnames ]
					out_finfonames	= [									$
								qualified_dir + fname_start + '_'+az_field2use+'.' + suffix + '_info',	$
								qualified_dir + fname_start + '_'+zen_field2use+'.' + suffix + '_info',	$
								out_finfonames ]
				ENDIF ELSE BEGIN
					out_fnames	= [								$
								out_fname + '_'+az_field2use,				$
								out_fname + '_'+zen_field2use,				$
								out_fnames ]
					out_finfonames	= 	[							$
								out_fname + '_'+az_field2use+'_info',			$
								out_fname + '_'+zen_field2use+'_info',			$
								out_finfonames ]
				ENDELSE










			ENDIF
		ENDFOR

	ENDIF

	FOR i = 0, n_datasets - 1 DO BEGIN
		src_file	= src_files[i]
		data_type_idx	= (*(data_ptrs[i]))->GetNumberType()
		data_ptr	= data_ptrs[i]
		data_desc	= data_descs[i]
		xy_dims		= [ (*data_ptr)->GetImageWidth(), (*data_ptr)->GetImageHeight() ]

		OPENW, lun, out_fnames[i], /GET_LUN

		WRITEU,								$
			lun, 							$
			REVERSE(						$
				(*data_ptr)->ReturnUnbytescaledImage(		$
				0,						$
				(*data_ptr)->GetImageWidth()-1L,		$
				0,						$
				(*data_ptr)->GetImageHeight()-1L,		$
				(*data_ptr)->GetImageWidth(),			$
				(*data_ptr)->GetImageHeight(),			$
				DATA_TYPE = (*data_ptr)->GetNumberType(),	$
				/NATIVE_RES ), 2 )
		FREE_LUN, lun

		OPENW, lun, out_finfonames[i], /GET
		PRINTF, lun, 'Information for binary file: ' + out_fnames[i]
		PRINTF, lun, ''
		PRINTF, lun, 'Width of Image: ' + STRTRIM((*data_ptr)->GetImageWidth(),2)
		PRINTF, lun, 'Height of Image: ' + STRTRIM((*data_ptr)->GetImageHeight(),2)
		PRINTF, lun, 'Number Type: ' + num_types[ data_type_idx ]
		PRINTF, lun, ''
		PRINTF, lun, 'MISR metadata corresponding to data:'
		PRINTF, lun, ''

			m_name		= 'Path_number'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
		PRINTF, lun, m_name+': ', STRTRIM(*(ptr[0]),2)
		PTR_FREE, ptr

			m_name		= 'AGP_version_id'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
		PRINTF, lun, m_name+': ', STRTRIM(*(ptr[0]),2)
		PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_ellipsoid.a'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
		PRINTF, lun, m_name+': ', STRTRIM(*(ptr[0]),2)
		PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_ellipsoid.e2'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
		PRINTF, lun, m_name+': ', STRTRIM(*(ptr[0]),2)
		PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_orbit.aprime'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
		PRINTF, lun, m_name+': ', STRTRIM(*(ptr[0]),2)
		PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_orbit.eprime'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
		PRINTF, lun, m_name+': ', STRTRIM(*(ptr[0]),2)
		PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_orbit.gama'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
		PRINTF, lun, m_name+': ', STRTRIM(*(ptr[0]),2)
		PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_orbit.nrev'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
		PRINTF, lun, m_name+': ', STRTRIM(*(ptr[0]),2)
		PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_orbit.ro'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
		PRINTF, lun, m_name+': ', STRTRIM(*(ptr[0]),2)
		PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_orbit.i'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
		PRINTF, lun, m_name+': ', STRTRIM(*(ptr[0]),2)
		PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_orbit.P2P1'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
		PRINTF, lun, m_name+': ', STRTRIM(*(ptr[0]),2)
		PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_orbit.lambda0'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
		PRINTF, lun, m_name+': ', STRTRIM(*(ptr[0]),2)
		PTR_FREE, ptr

			m_name		= 'coremetadata'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
			str		= STRING(*(ptr[0]))
			PTR_FREE, ptr
			p		= STRPOS( str, 'LOCALGRANULEID' )
			p1		= STRPOS( str, '"', p )
			p2		= STRPOS( str, '"', p1 + 1 )
			val		= STRMID( str, p1 + 1, ( p2 - 1 ) - p1 )
			m_name		= 'LocalGranuleID'

		PRINTF, lun, m_name+': ', STRTRIM(val,2)
		PRINTF, lun, ''

		PRINTF, lun, 'Sample IDL Code to read in data from file:'
		PRINTF, lun, ''
		PRINTF, lun, 'data='+num_arr[ data_type_idx ]+'('+STRTRIM((*data_ptr)->GetImageWidth(),2)+','+STRTRIM((*data_ptr)->GetImageHeight(),2)+')'
		PRINTF, lun, 'OPENR,lun,"' + out_fnames[i] + '",/GET_LUN'
		PRINTF, lun, 'READU, lun, data'
		PRINTF, lun, 'FREE_LUN, lun'
		PRINTF, lun, ''
		PRINTF, lun, 'THE REFERENCED BINARY FILE HAS BEEN CREATED ON THE FOLLOWING MACHINE TYPE:'
		PRINTF, lun, ''
		PRINTF, lun, '     OS: ' + !VERSION.OS
		PRINTF, lun, '     OS FAMILY: ' + !VERSION.OS_FAMILY
		PRINTF, lun, '     ARCHITECTURE: ' + !VERSION.ARCH
		PRINTF, lun, ''
		PRINTF, lun, 'READING THIS DATA ON MACHINE TYPES WITH DIFFERING BYTE ORDERING MAY'
		PRINTF, lun, 'REQUIRE BYTE-SWAPPING BY THE USER.  WITHIN IDL, THIS CAN BE PERFORMED'
		PRINTF, lun, 'WITH THE FUNCTION SWAP_ENDIAN (e.g., data=SWAP_ENDIAN(data)) OR THE'
		PRINTF, lun, 'PROCEDURE BYTEORDER (e.g., BYTEORDER, data, /LSWAP).  PLEASE SEE THE'
		PRINTF, lun, 'IDL DOCUMENTATION FOR MORE INFORMATION ON HOW TO USE THESE ROUTINES.'

		FREE_LUN, lun

	ENDFOR

END
; MISR_GEOREF_IMAGE::SaveBinary

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ MISR_GEOREF_IMAGE::SaveHDF @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_GEOREF_IMAGE::SaveHDF, out_fname

;	hdf_types	= [ 'UNDEFINED',					$
;			    '/BYTE',						$
;			    '/INT',						$
;			    '/LONG',						$
;			    '/FLOAT',						$
;			    '/DOUBLE',						$
;			    'UNDEFINED',					$
;			    '/STRING',						$
;			    'UNDEFINED',					$
;			    'UNDEFINED',					$
;			    'UNDEFINED',					$
;			    'UNDEFINED',					$
;			    '/DFNT_UINT16',					$
;			    '/DFNT_UINT32',					$
;			    '/DOUBLE',						$ ;long64
;			    '/DOUBLE' ]						  ;ulong64

	hdf_types	= [ 'UNDEFINED',					$ ;0
			    'BYTARR',						$ ;1
			    'INTARR',						$ ;2
			    'LONARR',						$ ;3
			    'FLTARR',						$ ;4
			    'DBLARR',						$ ;5
			    'UNDEFINED',					$ ;6
			    'STRARR',						$ ;7
			    'UNDEFINED',					$ ;8
			    'UNDEFINED',					$ ;9
			    'UNDEFINED',					$ ;10
			    'UNDEFINED',					$ ;11
			    'UINTARR',						$ ;12
			    'ULONARR',						$ ;13
			    'LON64ARR',						$ ;14
			    'ULON64ARR' ]					  ;15

	first_fname	= STRTRIM( (*(SELF.data_source_filename_ptr))[0], 2 )

	airmisr_data	= first_fname NE 'UNKNOWN SOURCE FILE' AND		$
			  SELF.misr_start_block LT 0 AND			$
			  SELF.misr_n_blocks LT 0

	misr_data	= first_fname NE 'UNKNOWN SOURCE FILE' AND		$
			  SELF.misr_start_block GT 0 AND			$
			  SELF.misr_n_blocks GT 0

	IF misr_data THEN BEGIN
;print,'out_fname = ', out_fname
		sd_id		= HDF_SD_START( out_fname, /CREATE )

		HDF_SD_ATTRSET, sd_id, 'Start_block', SELF.misr_start_block, /INT
		HDF_SD_ATTRSET, sd_id, 'End_block',				$
			SELF.misr_start_block + SELF.misr_n_blocks - 1, /INT

		HDF_SD_ATTRSET, sd_id, 'Resolution_act', SELF.in_resolution_act, /LONG
		HDF_SD_ATTRSET, sd_id, 'Resolution_alt', SELF.in_resolution_alt, /LONG

		n_datasets	= N_ELEMENTS(*(SELF.data_source_filename_ptr))
		src_files	= *(SELF.data_source_filename_ptr)
		data_ptrs	= *(SELF.dataPtr)
		data_descs	= *(SELF.data_description_ptr)

		FOR i = 0, N_ELEMENTS(data_descs) - 1 DO BEGIN
			s		= STR_SEP( data_descs[i], '::' )
			data_descs[i]	= 'GRID:' +					$
				  	   STRTRIM( s[ N_ELEMENTS(s) - 2 ], 2 ) +	$
				 	   ', FIELD:' +					$
					   STRTRIM( s[ N_ELEMENTS(s) - 1 ], 2 )
		ENDFOR
;print,'	SELF.lat_source_filename = ',	SELF.lat_source_filename
		IF  PTR_VALID((SELF.lonImgPtr)[1]) AND				$
		    PTR_VALID((SELF.latImgPtr)[1]) THEN BEGIN
			n_datasets	= n_datasets + 2
			src_files	= [ SELF.lon_source_filename,		$
					    SELF.lat_source_filename,		$
					    src_files ]
			data_ptrs	= [ (SELF.lonImgPtr)[0],		$
					    (SELF.latImgPtr)[0],		$
					    data_ptrs ]
			data_descs	= [ 'GRID:Standard, FIELD:GeoLongitude',	$
					    'GRID:Standard, FIELD:GeoLatitude',		$
					    data_descs ]
		ENDIF

		IF PTR_VALID( SELF.anglesPtr ) THEN BEGIN
			n_datasets	= n_datasets + 2
			src_files	= [ (*(SELF.anglesPtr)).source_file,	$
					    (*(SELF.anglesPtr)).source_file,	$
					    src_files ]
			data_ptrs	= [ (*(SELF.anglesPtr)).solarAzimuth,	$
					    (*(SELF.anglesPtr)).solarZenith,	$
					    data_ptrs ]
			data_descs	= [ 'GRID:GeometricParameters, FIELD:SolarAzimuth',	$
					    'GRID:GeometricParameters, FIELD:SolarZenith',	$
					    data_descs ]

			n_angles	= N_ELEMENTS(*(SELF.valid_img_idx_ptr))

;;;ckt.oct1999 angle_idx	= WHERE( PTR_VALID( (*(SELF.anglesPtr)).cameraAzimuth.slot ), n_angles )

			FOR i = 0, n_angles - 1 DO BEGIN
;;;ckt.oct1999		FOR j = 0, n_angles - 1 DO BEGIN
;;;ckt.oct1999i		= angle_idx[j]
				az_field	= (*(SELF.anglesPtr)).cameraAzField.slot[(*(SELF.valid_img_idx_ptr))[i]]
				zen_field	= (*(SELF.anglesPtr)).cameraZenField.slot[(*(SELF.valid_img_idx_ptr))[i]]

				new_az_desc	= 'GRID:GeometricParameters, FIELD:' + STRTRIM(az_field, 2 )
				new_zen_desc	= 'GRID:GeometricParameters, FIELD:' + STRTRIM(zen_field, 2 )

				idx		= WHERE( data_descs EQ new_az_desc, cnt )

				IF cnt LE 0 THEN BEGIN

					az_ptr		= (*(SELF.anglesPtr)).cameraAzimuth.slot[(*(SELF.valid_img_idx_ptr))[i]]
					zen_ptr		= (*(SELF.anglesPtr)).cameraZenith.slot[(*(SELF.valid_img_idx_ptr))[i]]
					n_datasets	= n_datasets + 2
					src_files	= [ (*(SELF.anglesPtr)).source_file,			$
							    (*(SELF.anglesPtr)).source_file,			$
							    src_files ]
					data_ptrs	= [ az_ptr,						$
							    zen_ptr,						$
							    data_ptrs ]
					data_descs	= [ new_az_desc,					$
							    new_zen_desc,					$
							    data_descs ]
				ENDIF
			ENDFOR

		ENDIF

		FOR i = 0, n_datasets - 1 DO BEGIN
			src_file	= src_files[i]
;;;ckt,nov2002print,'SOURCE FILE = ', src_file
			data_type_idx	= (*(data_ptrs[i]))->GetNumberType()
			data_ptr	= data_ptrs[i]
;;;ckt,nov2002print,'------------- SAVING HDF DATA'
;help,*data_ptr
			data_desc	= data_descs[i]
;;;ckt,nov2002print,'Data Description = ', data_desc
			xy_dims		= [ (*data_ptr)->GetImageWidth(), (*data_ptr)->GetImageHeight() ]


help,sd_id
help,data_desc
help,xy_dims
print,'hdf_types[data_type_idx]=',hdf_types[data_type_idx]

tmpdata	= CALL_FUNCTION(hdf_types[data_type_idx],2)
tmptype	= HDF_IDL2HDFTYPE(SIZE(tmpdata, /TYPE))
;sds_id	= CALL_FUNCTION('HDF_SD_CREATE',sd_id,data_desc,xy_dims,hdf_types[data_type_idx])
sds_id	= CALL_FUNCTION('HDF_SD_CREATE',sd_id,data_desc,xy_dims,HDF_TYPE=tmptype)
;;;ckt,aug2004			str2exec	=					$
;;;ckt,aug2004				'sds_id=' +					$
;;;ckt,aug2004				'HDF_SD_CREATE(sd_id,data_desc,xy_dims,' +	$
;;;ckt,aug2004				hdf_types[data_type_idx] +			$
;;;ckt,aug2004				')'
;;;ckt,nov2002;print,'str2exec = ', str2exec
;;;ckt,nov2002print,'xy_dims=',xy_dims
;;;ckt,aug2004			success		= EXECUTE( str2exec )
;;;ckt,nov2002print,'success = ',success
			m_name		= 'Size_x'
			HDF_SD_ATTRSET, sds_id, m_name, xy_dims[0], /LONG

			m_name		= 'Size_y'
			HDF_SD_ATTRSET, sds_id, m_name, xy_dims[1], /LONG

			m_name		= 'Path_number'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
			HDF_SD_ATTRSET, sds_id, m_name, *(ptr[0]), /LONG
			PTR_FREE, ptr

			m_name		= 'AGP_version_id'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
			HDF_SD_ATTRSET, sds_id, m_name, *(ptr[0]), /LONG
			PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_ellipsoid.a'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
			HDF_SD_ATTRSET, sds_id, m_name, *(ptr[0]), /DOUBLE
			PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_ellipsoid.e2'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
			HDF_SD_ATTRSET, sds_id, m_name, *(ptr[0]), /DOUBLE
			PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_orbit.aprime'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
			HDF_SD_ATTRSET, sds_id, m_name, *(ptr[0]), /DOUBLE
			PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_orbit.eprime'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
			HDF_SD_ATTRSET, sds_id, m_name, *(ptr[0]), /DOUBLE
			PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_orbit.gama'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
			HDF_SD_ATTRSET, sds_id, m_name, *(ptr[0]), /DOUBLE
			PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_orbit.nrev'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
			HDF_SD_ATTRSET, sds_id, m_name, *(ptr[0]), /LONG
			PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_orbit.ro'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
			HDF_SD_ATTRSET, sds_id, m_name, *(ptr[0]), /DOUBLE
			PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_orbit.i'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
			HDF_SD_ATTRSET, sds_id, m_name, *(ptr[0]), /DOUBLE
			PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_orbit.P2P1'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
			HDF_SD_ATTRSET, sds_id, m_name, *(ptr[0]), /DOUBLE
			PTR_FREE, ptr

			m_name		= 'SOM_parameters.som_orbit.lambda0'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
			HDF_SD_ATTRSET, sds_id, m_name, *(ptr[0]), /DOUBLE
			PTR_FREE, ptr

			m_name		= 'coremetadata'
			ptr		= misr_get_meta_info( src_file, METADATA_NAME = m_name )
			str		= STRING(*(ptr[0]))
			PTR_FREE, ptr
			p		= STRPOS( str, 'LOCALGRANULEID' )
			p1		= STRPOS( str, '"', p )
			p2		= STRPOS( str, '"', p1 + 1 )
			val		= STRMID( str, p1 + 1, ( p2 - 1 ) - p1 )
			m_name		= 'LocalGranuleID'
			HDF_SD_ATTRSET, sds_id, m_name, val, STRLEN(val) + 1

;;;ckt,nov2002 help,(*data_ptr)->ReturnUnbytescaledImage(	$
;;;ckt,nov2002						0,					$
;;;ckt,nov2002						(*data_ptr)->GetImageWidth()-1L,	$
;;;ckt,nov2002						0,					$
;;;ckt,nov2002						(*data_ptr)->GetImageHeight()-1L,	$
;;;ckt,nov2002						(*data_ptr)->GetImageWidth(),		$
;;;ckt,nov2002						(*data_ptr)->GetImageHeight(),		$
;;;ckt,nov2002						DATA_TYPE = (*data_ptr)->GetNumberType(), $
;;;ckt,nov2002						/NATIVE_RES )

			HDF_SD_ADDDATA, sds_id, REVERSE(				$
							(*data_ptr)->ReturnUnbytescaledImage(	$
							0,					$
							(*data_ptr)->GetImageWidth()-1L,	$
							0,					$
							(*data_ptr)->GetImageHeight()-1L,	$
							(*data_ptr)->GetImageWidth(),		$
							(*data_ptr)->GetImageHeight(),		$
							DATA_TYPE = (*data_ptr)->GetNumberType(), $
							/NATIVE_RES ), 2 )

			HDF_SD_ENDACCESS, sds_id
		ENDFOR

		HDF_SD_END, sd_id
	ENDIF

	IF airmisr_data THEN SELF->GEOREF_IMAGE::SaveHDF, out_fname

END
; MISR_GEOREF_IMAGE::SaveHDF


;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ MISR_GEOREF_IMAGE::ZoomDisplay @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_GEOREF_IMAGE::ZoomDisplay, display_x, display_y, new_data_zoom_x, new_data_zoom_y

	viewport_nx	= SELF.viewportNX
	viewport_ny	= SELF.viewportNY

	SELF->GEOREF_IMAGE::ZoomDisplay, display_x, display_y, new_data_zoom_x, new_data_zoom_y

;print,' MISR_GEOREF_IMAGE::ZoomDisplay   PTR_VALID(SELF.anglesPtr) = ',PTR_VALID(SELF.anglesPtr)
	IF PTR_VALID(SELF.anglesPtr) THEN BEGIN
		;=============================================================
		; Solar azimuth
		;=============================================================
		data_x_start	= SELF.solarAzimuthZoomXS
		data_x_zoom_val	= (SELF.solarAzimuthZoomPctX / 100.0)
		displayX2dataX	= ( FLOAT( display_x ) / data_x_zoom_val ) + data_x_start

		data_y_start	= SELF.solarAzimuthZoomYS
		data_y_zoom_val	= (SELF.solarAzimuthZoomPctY / 100.0)
		displayY2dataY	= ( FLOAT( display_y ) / data_y_zoom_val ) + data_y_start

		sol_az_ptr	= (*(SELF.anglesPtr)).solarAzimuth

		IF displayX2dataX GE (*sol_az_ptr)->GetImageWidth() OR		$
		   displayY2dataY GE (*sol_az_ptr)->GetImageHeight() THEN RETURN

		display_width		= SELF.drawXsize
		data_width		= (*sol_az_ptr)->GetImageWidth()
		rev_new_data_zoom_x	= new_data_zoom_x * data_x_zoom_val
		dataX2displayX		= displayX2dataX * rev_new_data_zoom_x
		new_data_x_start	= MAX( [ 0, ( dataX2displayX - ( display_width / 2.0 ) ) / rev_new_data_zoom_x ] )
		new_data_nx		= MIN( [ FLOAT( display_width ) / rev_new_data_zoom_x, data_width - new_data_x_start ] )
		new_data_zoom_val_x	= rev_new_data_zoom_x * 100.0

		display_height		= SELF.drawYsize
		data_height		= (*sol_az_ptr)->GetImageHeight()
		rev_new_data_zoom_y	= new_data_zoom_y * data_y_zoom_val
		dataY2displayY		= displayY2dataY * rev_new_data_zoom_y
		new_data_y_start	= MAX( [ 0, ( dataY2displayY - ( display_height / 2.0 ) ) / rev_new_data_zoom_y ] )
		new_data_ny		= MIN( [ FLOAT( display_height ) / rev_new_data_zoom_y, data_height - new_data_y_start ] )
		new_data_zoom_val_y	= rev_new_data_zoom_y * 100.0

		SELF.solarAzimuthZoomXS		= new_data_x_start
		SELF.solarAzimuthZoomNX		= new_data_nx
		SELF.solarAzimuthZoomPctX	= new_data_zoom_val_x
		SELF.solarAzimuthZoomYS		= new_data_y_start
		SELF.solarAzimuthZoomNY		= new_data_ny
		SELF.solarAzimuthZoomPctY	= new_data_zoom_val_y

		;=============================================================
		; Solar zenith
		;=============================================================
		data_x_start	= SELF.solarZenithZoomXS
		data_x_zoom_val	= (SELF.solarZenithZoomPctX / 100.0)
		displayX2dataX	= ( FLOAT( display_x ) / data_x_zoom_val ) + data_x_start

		data_y_start	= SELF.solarZenithZoomYS
		data_y_zoom_val	= (SELF.solarZenithZoomPctY / 100.0)
		displayY2dataY	= ( FLOAT( display_y ) / data_y_zoom_val ) + data_y_start

		sol_zen_ptr	= (*(SELF.anglesPtr)).solarZenith

		IF displayX2dataX GE (*sol_zen_ptr)->GetImageWidth() OR		$
		   displayY2dataY GE (*sol_zen_ptr)->GetImageHeight() THEN RETURN

		display_width		= SELF.drawXsize
		data_width		= (*sol_zen_ptr)->GetImageWidth()
		rev_new_data_zoom_x	= new_data_zoom_x * data_x_zoom_val
		dataX2displayX		= displayX2dataX * rev_new_data_zoom_x
		new_data_x_start	= MAX( [ 0, ( dataX2displayX - ( display_width / 2.0 ) ) / rev_new_data_zoom_x ] )
		new_data_nx		= MIN( [ FLOAT( display_width ) / rev_new_data_zoom_x, data_width - new_data_x_start ] )
		new_data_zoom_val_x	= rev_new_data_zoom_x * 100.0

		display_height		= SELF.drawYsize
		data_height		= (*sol_zen_ptr)->GetImageHeight()
		rev_new_data_zoom_y	= new_data_zoom_y * data_y_zoom_val
		dataY2displayY		= displayY2dataY * rev_new_data_zoom_y
		new_data_y_start	= MAX( [ 0, ( dataY2displayY - ( display_height / 2.0 ) ) / rev_new_data_zoom_y ] )
		new_data_ny		= MIN( [ FLOAT( display_height ) / rev_new_data_zoom_y, data_height - new_data_y_start ] )
		new_data_zoom_val_y	= rev_new_data_zoom_y * 100.0

		SELF.solarZenithZoomXS		= new_data_x_start
		SELF.solarZenithZoomNX		= new_data_nx
		SELF.solarZenithZoomPctX	= new_data_zoom_val_x
		SELF.solarZenithZoomYS		= new_data_y_start
		SELF.solarZenithZoomNY		= new_data_ny
		SELF.solarZenithZoomPctY	= new_data_zoom_val_y

		;=============================================================
		; camera solar and zenith
		;=============================================================
		n_angles	= N_ELEMENTS(*(SELF.valid_img_idx_ptr))

;;;ckt,oct1999 angle_idx	= WHERE( PTR_VALID( (*(SELF.anglesPtr)).cameraAzimuth.slot ), n_angles )

		FOR i = 0, n_angles - 1 DO BEGIN
;;;ckt,oct1999		FOR j = 0, n_angles - 1 DO BEGIN
;;;ckt,oct1999i		= angle_idx[j]
			;=====================================================
			; Azimuth
			;=====================================================
;print,'i = ',i
;print,'(*(SELF.valid_img_idx_ptr))[i] = ',(*(SELF.valid_img_idx_ptr))[i]
;print,'viewport_nx=',viewport_nx
;print,'viewport_ny=',viewport_ny
			az_ptr	= (*(SELF.anglesPtr)).cameraAzimuth.slot[(*(SELF.valid_img_idx_ptr))[i]]
;;;ckt,nov1999			az_nx	= (*(SELF.cameraAzimuthZoomNXPtr))[i]
;;;ckt,nov1999			az_ny	= (*(SELF.cameraAzimuthZoomNYPtr))[i]
			az_xs	= (*(SELF.cameraAzimuthZoomXSPtr))[i]
			az_ys	= (*(SELF.cameraAzimuthZoomYSPtr))[i]
			az_pctx	= (*(SELF.cameraAzimuthZoomPctXPtr))[i]
			az_pcty	= (*(SELF.cameraAzimuthZoomPctYPtr))[i]

			data_x_start		= az_xs
			data_x_zoom_val		= az_pctx / 100.0
			displayX2dataX		= ( FLOAT( display_x ) / data_x_zoom_val ) + data_x_start

			data_y_start		= az_ys
			data_y_zoom_val		= (az_pcty / 100.0)
			displayY2dataY		= ( FLOAT( display_y ) / data_y_zoom_val ) + data_y_start

			IF displayX2dataX GE (*az_ptr)->GetImageWidth() OR		$
			   displayY2dataY GE (*az_ptr)->GetImageHeight() THEN RETURN

			display_width		= SELF.drawXsize
			data_width		= (*az_ptr)->GetImageWidth()
			rev_new_data_zoom_x	= new_data_zoom_x * data_x_zoom_val
			dataX2displayX		= displayX2dataX * rev_new_data_zoom_x
			new_data_x_start	= MAX( [ 0, ( dataX2displayX - ( display_width / 2.0 ) ) / rev_new_data_zoom_x ] )
			new_data_nx		= MIN( [ FLOAT( display_width ) / rev_new_data_zoom_x, data_width - new_data_x_start ] )
			new_data_zoom_val_x	= rev_new_data_zoom_x * 100.0

			display_height		= SELF.drawYsize
			data_height		= (*az_ptr)->GetImageHeight()
			rev_new_data_zoom_y	= new_data_zoom_y * data_y_zoom_val
			dataY2displayY		= displayY2dataY * rev_new_data_zoom_y
			new_data_y_start	= MAX( [ 0, ( dataY2displayY - ( display_height / 2.0 ) ) / rev_new_data_zoom_y ] )
			new_data_ny		= MIN( [ FLOAT( display_height ) / rev_new_data_zoom_y, data_height - new_data_y_start ] )
			new_data_zoom_val_y	= rev_new_data_zoom_y * 100.0

			(*(SELF.cameraAzimuthZoomXSPtr))[i]	= data_x_start
			(*(SELF.cameraAzimuthZoomNXPtr))[i]	= new_data_nx
			(*(SELF.cameraAzimuthZoomPctXPtr))[i]	= new_data_zoom_val_x
			(*(SELF.cameraAzimuthZoomYSPtr))[i]	= data_y_start
			(*(SELF.cameraAzimuthZoomNYPtr))[i]	= new_data_ny
			(*(SELF.cameraAzimuthZoomPctYPtr))[i]	= new_data_zoom_val_y

			;=====================================================
			; Zenith
			;=====================================================
			zen_ptr	= (*(SELF.anglesPtr)).cameraZenith.slot[(*(SELF.valid_img_idx_ptr))[i]]
;;;ckt,nov1999			zen_nx	= (*(SELF.cameraZenithZoomNXPtr))[i]
;;;ckt,nov1999			zen_ny	= (*(SELF.cameraZenithZoomNYPtr))[i]
			zen_xs	= (*(SELF.cameraZenithZoomXSPtr))[i]
			zen_ys	= (*(SELF.cameraZenithZoomYSPtr))[i]
			zen_pctx= (*(SELF.cameraZenithZoomPctXPtr))[i]
			zen_pcty= (*(SELF.cameraZenithZoomPctYPtr))[i]

			data_x_start		= zen_xs
			data_x_zoom_val		= zen_pctx / 100.0
			displayX2dataX		= ( FLOAT( display_x ) / data_x_zoom_val ) + data_x_start

			data_y_start		= zen_ys
			data_y_zoom_val		= (zen_pcty / 100.0)
			displayY2dataY		= ( FLOAT( display_y ) / data_y_zoom_val ) + data_y_start

			IF displayX2dataX GE (*zen_ptr)->GetImageWidth() OR		$
			   displayY2dataY GE (*zen_ptr)->GetImageHeight() THEN RETURN

			display_width		= SELF.drawXsize
			data_width		= (*zen_ptr)->GetImageWidth()
			rev_new_data_zoom_x	= new_data_zoom_x * data_x_zoom_val
			dataX2displayX		= displayX2dataX * rev_new_data_zoom_x
			new_data_x_start	= MAX( [ 0, ( dataX2displayX - ( display_width / 2.0 ) ) / rev_new_data_zoom_x ] )
			new_data_nx		= MIN( [ FLOAT( display_width ) / rev_new_data_zoom_x, data_width - new_data_x_start ] )
			new_data_zoom_val_x	= rev_new_data_zoom_x * 100.0

			display_height		= SELF.drawYsize
			data_height		= (*zen_ptr)->GetImageHeight()
			rev_new_data_zoom_y	= new_data_zoom_y * data_y_zoom_val
			dataY2displayY		= displayY2dataY * rev_new_data_zoom_y
			new_data_y_start	= MAX( [ 0, ( dataY2displayY - ( display_height / 2.0 ) ) / rev_new_data_zoom_y ] )
			new_data_ny		= MIN( [ FLOAT( display_height ) / rev_new_data_zoom_y, data_height - new_data_y_start ] )
			new_data_zoom_val_y	= rev_new_data_zoom_y * 100.0

			(*(SELF.cameraZenithZoomXSPtr))[i]	= data_x_start
			(*(SELF.cameraZenithZoomNXPtr))[i]	= new_data_nx
			(*(SELF.cameraZenithZoomPctXPtr))[i]	= new_data_zoom_val_x
			(*(SELF.cameraZenithZoomYSPtr))[i]	= data_y_start
			(*(SELF.cameraZenithZoomNYPtr))[i]	= new_data_ny
			(*(SELF.cameraZenithZoomPctYPtr))[i]	= new_data_zoom_val_y
		ENDFOR
	ENDIF
END
; MISR_GEOREF_IMAGE::ZoomDisplay

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@ MISR_GEOREF_IMAGE::GetLabelInfoString @@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION MISR_GEOREF_IMAGE::GetLabelInfoString
	IF SELF.report_global_coordinates THEN					$
		RETURN, '(x, y, data value)'					$
	ELSE									$
		RETURN, '(block #, block-y, block-x, data value)'
END
; MISR_GEOREF_IMAGE::GetLabelInfoString

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@ MISR_GEOREF_IMAGE::DisplayValsForXY @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION MISR_GEOREF_IMAGE::DisplayValsForXY, x, y, OFF_MAP_FLAG = offMapFlag
	;=====================================================================
	; Set flag2send to either 0 or 1, for used in setting keyword OFF_MAP_FLAG
	;=====================================================================
	flag2send	= KEYWORD_SET(offMapFlag)

	;=====================================================================
	; Call method GEOREF_IMAGE::DisplayValsForXY; return value is offMapFlag,
	; which is set by the method or passed in.
	;=====================================================================
	offMapFlag		= SELF->GEOREF_IMAGE::DisplayValsForXY(x,y,OFF_MAP_FLAG = flag2send)
	IF NOT PTR_VALID( SELF.anglesPtr ) THEN RETURN, offMapFlag

	sol_az_val		= -9999.0
	sol_zen_val		= -9999.0
	r_az_zen		= [-9999.0,-9999.0]
	g_az_zen		= [-9999.0,-9999.0]
	b_az_zen		= [-9999.0,-9999.0]

	r_az_zen_minmax		= [-9999.0,-9999.0,-9999.0,-9999.0]
	g_az_zen_minmax		= [-9999.0,-9999.0,-9999.0,-9999.0]
	b_az_zen_minmax		= [-9999.0,-9999.0,-9999.0,-9999.0]
	sol_az_zen_minmax	= [-9999.0,-9999.0,-9999.0,-9999.0]

;	IF NOT offMapFlag AND						$
;		WIDGET_INFO( SELF.anglesFloatingBase, /VALID_ID ) THEN BEGIN
	IF NOT offMapFlag THEN BEGIN

		;=============================================================
		; Get solar azimuth and zenith values
		;=============================================================
;;;ckt,nov1999		xIdxData	= FIX( (FLOAT(SELF.solarAzimuthZoomNX)/FLOAT(SELF.viewportNX))	$
;;;ckt,nov1999					* FLOAT(x) ) + SELF.solarAzimuthZoomXS
;;;ckt,nov1999		yIdxData	= FIX( (FLOAT(SELF.solarAzimuthZoomNY)/FLOAT(SELF.viewportNY))	$
;;;ckt,nov1999					* FLOAT(y) ) + SELF.solarAzimuthZoomYS

		inverse_zoom_val_x	= 1.0 / ( SELF.solarAzimuthZoomPctX / 100.0 )
		inverse_zoom_val_y	= 1.0 / ( SELF.solarAzimuthZoomPctY / 100.0 )
		xIdxData		= FIX( inverse_zoom_val_x * FLOAT( x ) + SELF.solarAzimuthZoomXS )
		yIdxData		= FIX( inverse_zoom_val_y * FLOAT( y ) + SELF.solarAzimuthZoomYS )

;print,'MISR_GEOREF_IMAGE::DisplayValsForXY   help,*(SELF.anglesPtr)'
;help,*(SELF.anglesPtr)
		sol_az_val	= (*((*(SELF.anglesPtr)).solarAzimuth))->ReturnImageValueAtXY( xIdxData, yIdxData )

		IF sol_az_val.bad_value THEN RETURN, 1

		sol_az_val	= sol_az_val.value

;;;ckt,nov1999		xIdxData	= FIX( (FLOAT(SELF.solarZenithZoomNX)/FLOAT(SELF.viewportNX))	$
;;;ckt,nov1999					* FLOAT(x) ) + SELF.solarZenithZoomXS
;;;ckt,nov1999		yIdxData	= FIX( (FLOAT(SELF.solarZenithZoomNY)/FLOAT(SELF.viewportNY))	$
;;;ckt,nov1999					* FLOAT(y) ) + SELF.solarZenithZoomYS

		inverse_zoom_val_x	= 1.0 / ( SELF.solarZenithZoomPctX / 100.0 )
		inverse_zoom_val_y	= 1.0 / ( SELF.solarZenithZoomPctY / 100.0 )
		xIdxData		= FIX( inverse_zoom_val_x * FLOAT( x ) + SELF.solarZenithZoomXS )
		yIdxData		= FIX( inverse_zoom_val_y * FLOAT( y ) + SELF.solarZenithZoomYS )

		sol_zen_val	= (*((*(SELF.anglesPtr)).solarZenith))->ReturnImageValueAtXY( xIdxData, yIdxData )

		IF sol_zen_val.bad_value THEN RETURN, 1

		sol_zen_val	= sol_zen_val.value
		sol_az_zen_minmax = [ *(SELF.solarAzMinMaxPtr), *(SELF.solarZenMinMaxPtr) ]

		r_az_ptr	= PTR_NEW()
		red_idx		= -1
		IF SELF.redBandIdx GE 0 THEN BEGIN
			red_idx		= (*(SELF.valid_img_idx_ptr))[SELF.redBandIdx]
			r_az_ptr	= (*(SELF.anglesPtr)).cameraAzimuth.slot[ red_idx ]
			r_zen_ptr	= (*(SELF.anglesPtr)).cameraZenith.slot[ red_idx ]
		ENDIF

		IF PTR_VALID(r_az_ptr) AND red_idx GE 0 THEN BEGIN
			az_nx		= (*(SELF.cameraAzimuthZoomNXPtr))[SELF.redBandIdx]
			az_xs		= (*(SELF.cameraAzimuthZoomXSPtr))[SELF.redBandIdx]
			az_ny		= (*(SELF.cameraAzimuthZoomNYPtr))[SELF.redBandIdx]
			az_ys		= (*(SELF.cameraAzimuthZoomYSPtr))[SELF.redBandIdx]
			az_zoom_pct_x	= (*(SELF.cameraAzimuthZoomPctXPtr))[SELF.redBandIdx]
			az_zoom_pct_y	= (*(SELF.cameraAzimuthZoomPctYPtr))[SELF.redBandIdx]
;;;ckt,nov1999			xIdxData	= FIX( (FLOAT( az_nx )/FLOAT(SELF.viewportNX))	$
;;;ckt,nov1999						* FLOAT(x) ) + az_xs
;;;ckt,nov1999			yIdxData	= FIX( (FLOAT( az_ny )/FLOAT(SELF.viewportNY))	$
;;;ckt,nov1999						* FLOAT(y) ) + az_ys

			inverse_zoom_val_x	= 1.0 / ( az_zoom_pct_x / 100.0 )
			inverse_zoom_val_y	= 1.0 / ( az_zoom_pct_y / 100.0 )
			xIdxData		= FIX( inverse_zoom_val_x * FLOAT( x ) + az_xs )
			yIdxData		= FIX( inverse_zoom_val_y * FLOAT( y ) + az_ys )

			r_az		= (*r_az_ptr)->ReturnImageValueAtXY( xIdxData, yIdxData )

			IF r_az.bad_value THEN RETURN, 1

			r_az		= r_az.value

			zen_nx		= (*(SELF.cameraZenithZoomNXPtr))[SELF.redBandIdx]
			zen_xs		= (*(SELF.cameraZenithZoomXSPtr))[SELF.redBandIdx]
			zen_ny		= (*(SELF.cameraZenithZoomNYPtr))[SELF.redBandIdx]
			zen_ys		= (*(SELF.cameraZenithZoomYSPtr))[SELF.redBandIdx]
			zen_zoom_pct_x	= (*(SELF.cameraZenithZoomPctXPtr))[SELF.redBandIdx]
			zen_zoom_pct_y	= (*(SELF.cameraZenithZoomPctYPtr))[SELF.redBandIdx]

;;;ckt,nov1999			xIdxData	= FIX( (FLOAT( zen_nx )/FLOAT(SELF.viewportNX))	$
;;;ckt,nov1999						* FLOAT(x) ) + zen_xs
;;;ckt,nov1999			yIdxData	= FIX( (FLOAT( zen_ny )/FLOAT(SELF.viewportNY))	$
;;;ckt,nov1999						* FLOAT(y) ) + zen_ys

			inverse_zoom_val_x	= 1.0 / ( zen_zoom_pct_x / 100.0 )
			inverse_zoom_val_y	= 1.0 / ( zen_zoom_pct_y / 100.0 )
			xIdxData		= FIX( inverse_zoom_val_x * FLOAT( x ) + zen_xs )
			yIdxData		= FIX( inverse_zoom_val_y * FLOAT( y ) + zen_ys )

			r_zen		= (*r_zen_ptr)->ReturnImageValueAtXY( xIdxData, yIdxData )

			IF r_zen.bad_value THEN RETURN, 1

			r_zen		= r_zen.value

			r_az_zen	= [ r_az, r_zen ]

			r_az_zen_minmax	= [ $
						(*(SELF.cameraAzMinMaxPtr))[SELF.redBandIdx,0], $
						(*(SELF.cameraAzMinMaxPtr))[SELF.redBandIdx,1], $
						(*(SELF.cameraZenMinMaxPtr))[SELF.redBandIdx,0], $
						(*(SELF.cameraZenMinMaxPtr))[SELF.redBandIdx,1] ]
		ENDIF

		g_az_ptr	= PTR_NEW()
		grn_idx		= -1
		IF SELF.grnBandIdx GE 0 THEN BEGIN
			grn_idx		= (*(SELF.valid_img_idx_ptr))[SELF.grnBandIdx]
			g_az_ptr	= (*(SELF.anglesPtr)).cameraAzimuth.slot[ grn_idx ]
			g_zen_ptr	= (*(SELF.anglesPtr)).cameraZenith.slot[ grn_idx ]
		ENDIF

		IF PTR_VALID(g_az_ptr) AND grn_idx GE 0 THEN BEGIN
			az_nx		= (*(SELF.cameraAzimuthZoomNXPtr))[SELF.grnBandIdx]
			az_xs		= (*(SELF.cameraAzimuthZoomXSPtr))[SELF.grnBandIdx]
			az_ny		= (*(SELF.cameraAzimuthZoomNYPtr))[SELF.grnBandIdx]
			az_ys		= (*(SELF.cameraAzimuthZoomYSPtr))[SELF.grnBandIdx]
			az_zoom_pct_x	= (*(SELF.cameraAzimuthZoomPctXPtr))[SELF.grnBandIdx]
			az_zoom_pct_y	= (*(SELF.cameraAzimuthZoomPctYPtr))[SELF.grnBandIdx]

;;;ckt,nov1999			xIdxData	= FIX( (FLOAT( az_nx )/FLOAT(SELF.viewportNX))	$
;;;ckt,nov1999						* FLOAT(x) ) + az_xs
;;;ckt,nov1999			yIdxData	= FIX( (FLOAT( az_ny )/FLOAT(SELF.viewportNY))	$
;;;ckt,nov1999						* FLOAT(y) ) + az_ys
			inverse_zoom_val_x	= 1.0 / ( az_zoom_pct_x / 100.0 )
			inverse_zoom_val_y	= 1.0 / ( az_zoom_pct_y / 100.0 )
			xIdxData		= FIX( inverse_zoom_val_x * FLOAT( x ) + az_xs )
			yIdxData		= FIX( inverse_zoom_val_y * FLOAT( y ) + az_ys )

			g_az		= (*g_az_ptr)->ReturnImageValueAtXY( xIdxData, yIdxData )
			IF g_az.bad_value THEN RETURN, 1

			g_az		= g_az.value

			zen_nx		= (*(SELF.cameraZenithZoomNXPtr))[SELF.grnBandIdx]
			zen_xs		= (*(SELF.cameraZenithZoomXSPtr))[SELF.grnBandIdx]
			zen_ny		= (*(SELF.cameraZenithZoomNYPtr))[SELF.grnBandIdx]
			zen_ys		= (*(SELF.cameraZenithZoomYSPtr))[SELF.grnBandIdx]
			zen_zoom_pct_x	= (*(SELF.cameraZenithZoomPctXPtr))[SELF.grnBandIdx]
			zen_zoom_pct_y	= (*(SELF.cameraZenithZoomPctYPtr))[SELF.grnBandIdx]

;;;ckt,nov1999			xIdxData	= FIX( (FLOAT( zen_nx )/FLOAT(SELF.viewportNX))	$
;;;ckt,nov1999						* FLOAT(x) ) + zen_xs
;;;ckt,nov1999			yIdxData	= FIX( (FLOAT( zen_ny )/FLOAT(SELF.viewportNY))	$
;;;ckt,nov1999						* FLOAT(y) ) + zen_ys

			inverse_zoom_val_x	= 1.0 / ( zen_zoom_pct_x / 100.0 )
			inverse_zoom_val_y	= 1.0 / ( zen_zoom_pct_y / 100.0 )
			xIdxData		= FIX( inverse_zoom_val_x * FLOAT( x ) + zen_xs )
			yIdxData		= FIX( inverse_zoom_val_y * FLOAT( y ) + zen_ys )

			g_zen		= (*g_zen_ptr)->ReturnImageValueAtXY( xIdxData, yIdxData )

			IF g_zen.bad_value THEN RETURN, 1

			g_zen		= g_zen.value
			g_az_zen	= [ g_az, g_zen ]

			g_az_zen_minmax	= [ $
						(*(SELF.cameraAzMinMaxPtr))[SELF.grnBandIdx,0], $
						(*(SELF.cameraAzMinMaxPtr))[SELF.grnBandIdx,1], $
						(*(SELF.cameraZenMinMaxPtr))[SELF.grnBandIdx,0], $
						(*(SELF.cameraZenMinMaxPtr))[SELF.grnBandIdx,1] ]
		ENDIF

		b_az_ptr	= PTR_NEW()
		blu_idx		= -1
		IF SELF.bluBandIdx GE 0 THEN BEGIN
			blu_idx		= (*(SELF.valid_img_idx_ptr))[SELF.bluBandIdx]
			b_az_ptr	= (*(SELF.anglesPtr)).cameraAzimuth.slot[ blu_idx ]
			b_zen_ptr	= (*(SELF.anglesPtr)).cameraZenith.slot[ blu_idx ]
		ENDIF

		IF PTR_VALID(b_az_ptr) AND blu_idx GE 0 THEN BEGIN
			az_nx		= (*(SELF.cameraAzimuthZoomNXPtr))[SELF.bluBandIdx]
			az_xs		= (*(SELF.cameraAzimuthZoomXSPtr))[SELF.bluBandIdx]
			az_ny		= (*(SELF.cameraAzimuthZoomNYPtr))[SELF.bluBandIdx]
			az_ys		= (*(SELF.cameraAzimuthZoomYSPtr))[SELF.bluBandIdx]
			az_zoom_pct_x	= (*(SELF.cameraAzimuthZoomPctXPtr))[SELF.bluBandIdx]
			az_zoom_pct_y	= (*(SELF.cameraAzimuthZoomPctYPtr))[SELF.bluBandIdx]

;;;ckt,nov1999			xIdxData	= FIX( (FLOAT( az_nx )/FLOAT(SELF.viewportNX))	$
;;;ckt,nov1999						* FLOAT(x) ) + az_xs
;;;ckt,nov1999			yIdxData	= FIX( (FLOAT( az_ny )/FLOAT(SELF.viewportNY))	$
;;;ckt,nov1999						* FLOAT(y) ) + az_ys

			inverse_zoom_val_x	= 1.0 / ( az_zoom_pct_x / 100.0 )
			inverse_zoom_val_y	= 1.0 / ( az_zoom_pct_y / 100.0 )
			xIdxData		= FIX( inverse_zoom_val_x * FLOAT( x ) + az_xs )
			yIdxData		= FIX( inverse_zoom_val_y * FLOAT( y ) + az_ys )

			b_az		= (*b_az_ptr)->ReturnImageValueAtXY( xIdxData, yIdxData )

			IF b_az.bad_value THEN RETURN, 1

			b_az		= b_az.value
			zen_nx		= (*(SELF.cameraZenithZoomNXPtr))[SELF.bluBandIdx]
			zen_xs		= (*(SELF.cameraZenithZoomXSPtr))[SELF.bluBandIdx]
			zen_ny		= (*(SELF.cameraZenithZoomNYPtr))[SELF.bluBandIdx]
			zen_ys		= (*(SELF.cameraZenithZoomYSPtr))[SELF.bluBandIdx]
			zen_zoom_pct_x	= (*(SELF.cameraZenithZoomPctXPtr))[SELF.bluBandIdx]
			zen_zoom_pct_y	= (*(SELF.cameraZenithZoomPctYPtr))[SELF.bluBandIdx]

;;;ckt,nov1999			xIdxData	= FIX( (FLOAT( zen_nx )/FLOAT(SELF.viewportNX))	$
;;;ckt,nov1999						* FLOAT(x) ) + zen_xs
;;;ckt,nov1999			yIdxData	= FIX( (FLOAT( zen_ny )/FLOAT(SELF.viewportNY))	$
;;;ckt,nov1999						* FLOAT(y) ) + zen_ys

			inverse_zoom_val_x	= 1.0 / ( zen_zoom_pct_x / 100.0 )
			inverse_zoom_val_y	= 1.0 / ( zen_zoom_pct_y / 100.0 )
			xIdxData		= FIX( inverse_zoom_val_x * FLOAT( x ) + zen_xs )
			yIdxData		= FIX( inverse_zoom_val_y * FLOAT( y ) + zen_ys )

			b_zen		= (*b_zen_ptr)->ReturnImageValueAtXY( xIdxData, yIdxData )

			IF b_zen.bad_value THEN RETURN, 1

			b_zen		= b_zen.value
			b_az_zen	= [ b_az, b_zen ]

			b_az_zen_minmax	= [ $
						(*(SELF.cameraAzMinMaxPtr))[SELF.bluBandIdx,0], $
						(*(SELF.cameraAzMinMaxPtr))[SELF.bluBandIdx,1], $
						(*(SELF.cameraZenMinMaxPtr))[SELF.bluBandIdx,0], $
						(*(SELF.cameraZenMinMaxPtr))[SELF.bluBandIdx,1] ]
		ENDIF
	ENDIF

;;;DISABLE FOR NOW...(July 20, 1999)
;;	SELF->SetAngleDisplayValues, sol_az_val, sol_zen_val, r_az_zen, g_az_zen, b_az_zen, $
;;		{ $
;;		redAzZenMinMax : r_az_zen_minmax, $
;;		grnAzZenMinMax : g_az_zen_minmax, $
;;		bluAzZenMinMax : b_az_zen_minmax, $
;;		solarAzZenMinMax : sol_az_zen_minmax }

	RETURN, offMapFlag
END
; MISR_GEOREF_IMAGE::DisplayValsForXY

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@ drawCircleAngle @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
pro drawCircleAngle, circleWindowID, circleState, all_missing_data_values_ptr
;print,'drawCircleAngle'

	currentID = !d.window
	wset,circleWindowID

	; Shift angles so they get drawn in lower half of circle.
	Angle	= circleState.Angle	- 90.0
	x	= circleState.Max	- 90.0
	y	= circleState.Min	- 90.0

	; Draw circle for Azimuth indicator.
	plot, cos(findgen(361)*!dtor), sin(findgen(361)*!dtor), $
		xmargin=[0,0], ymargin=[0,0], $
		xstyle=4, ystyle=4, $
; DO NOT DRAW CIRCLE, RUN PLOT WITH /NODATA TO SET UP POSITION COORDINATES.
		position=[0.0,0.0,1.0,1.0], /nodata

	for i=0, 3 do begin		; Draw indicator for RED GRN BLU angles.
;print,'i = ',i

		; Set up color index.
		if i eq 0 then rgb = [255L,127L,127L]	; medium Red
		if i eq 1 then rgb = [127L,255L,127L]	; medium Green
		if i eq 2 then rgb = [191L,191L,255L]	; bright Blue
		if i eq 3 then rgb = [255L,255L,127L]	; Solar Yellow
		colorIndex=rgb[0]+256*(rgb[1]+256*rgb[2])

		; Shift solar angle so it gets drawn in upper half of the circle.
		if i eq 3 then begin
			Angle	= circleState.Angle	+ 90.0
			x	= circleState.Max	+ 90.0
			y	= circleState.Min	+ 90.0
		endif

;print,'(where_is( *all_missing_data_values_ptr, circleState.Angle[i] ))[0] = ',(where_is( *all_missing_data_values_ptr, circleState.Angle[i] ))[0]
;;;ckt,sep2004		IF (where_is( *all_missing_data_values_ptr, circleState.Angle[i] ))[0] EQ -1 THEN BEGIN
		IF (where2( *all_missing_data_values_ptr, circleState.Angle[i] ))[0] EQ -1 THEN BEGIN

			AA = Angle[i]
			XA = x[i]
			YA = y[i]
			; Draw centerline (draw solar centerline slightly shorter).
			if i lt 3 then 	linelength = 2.0 $
			else		linelength = 0.85
			plots, color=colorIndex, [0,cos(AA*!dtor)*linelength],   [0,sin(AA*!dtor)*linelength]
			; Draw first spread line.
			plots, [0,cos(XA*!dtor)*linelength], [0,sin(XA*!dtor)*linelength], color=colorIndex, linestyle=4
			; Draw the other spread line.
			plots, [0,cos(YA*!dtor)*linelength], [0,sin(YA*!dtor)*linelength], color=colorIndex, linestyle=4
; DO NOT USE CONNECTING LINE.
;;;			; Draw connecting line between spread ends.
;;;			plots, [cos(XA*!dtor),cos(YA*!dtor)], [sin(XA*!dtor),sin(YA*!dtor)], color=colorIndex

			; Draw circle to represent sun.
			if i eq 3 then begin
				; Describe circle with 18 points (plus 1 for complete circle).
				; Multiply point values by 20 to get 360 degrees.
				; Divide radians by 20 to make the circle small.
				; Add the centerline endpoint to locate the circle.
				plots, color=colorIndex, $
					cos(findgen(18+1)*20*!dtor)/20+cos(AA*!dtor), $
					sin(findgen(18+1)*20*!dtor)/20+sin(AA*!dtor)
				; Draw 12 rays.
				for ii=0,360,30 do begin
					J = cos(ii*!dtor)
					K = sin(ii*!dtor)
					plots, color=colorIndex, $
						[ J+J/2, J ] / 15 + cos(AA*!dtor), $
						[ K+K/2, K ] / 15 + sin(AA*!dtor)
				endfor
			endif
		ENDIF
	endfor

; DO NOT PUT INFORMATIONAL TEXT INSIDE CIRCLE.
;;;	; Create strings for ANGLE text indicators.
;;;	RedArray = str_sep(strtrim(string(Angle[0]+90),2),'.')
;;;	GrnArray = str_sep(strtrim(string(Angle[1]+90),2),'.')
;;;	BluArray = str_sep(strtrim(string(Angle[2]+90),2),'.')
;;;	; Create strings for SPREAD text indicators.
;;;	RedArray2 = str_sep(strtrim(string(circleState.Spread[0]),2),'.')
;;;	GrnArray2 = str_sep(strtrim(string(circleState.Spread[1]),2),'.')
;;;	BluArray2 = str_sep(strtrim(string(circleState.Spread[2]),2),'.')
;;;	RedAngleString = "R " + RedArray[0] + '.' + strmid(RedArray[1],0,1) + $
;;;		"/" + RedArray2[0] + '.' + strmid(RedArray2[1],0,1)
;;;	GrnAngleString = "G " + GrnArray[0] + '.' + strmid(GrnArray[1],0,1) + $
;;;		"/" + GrnArray2[0] + '.' + strmid(GrnArray2[1],0,1)
;;;	BluAngleString = "B " + BluArray[0] + '.' + strmid(BluArray[1],0,1) + $
;;;		"/" + BluArray2[0] + '.' + strmid(BluArray2[1],0,1)
;;;	; Write string indicators to window.
;;;	rgb=[255L,127L,127L]
;;;	colorIndex=rgb[0]+256*(rgb[1]+256*rgb[2])
;;;	xyouts,0.5,0.50,RedAngleString,/normal,alignment=0.5,color=colorIndex
;;;	rgb=[127L,255L,127L]
;;;	colorIndex=rgb[0]+256*(rgb[1]+256*rgb[2])
;;;	xyouts,0.5,0.60,GrnAngleString,/normal,alignment=0.5,color=colorIndex
;;;	rgb=[127L,127L,255L]
;;;	colorIndex=rgb[0]+256*(rgb[1]+256*rgb[2])
;;;	xyouts,0.5,0.70,BluAngleString,/normal,alignment=0.5,color=colorIndex

	; Draw title in indicator circle.
	xyouts,0.5,0.5,circleState.title,/normal,alignment=0.5

	wset,currentID
end
; drawCircleAngle

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@ MISR_GEOREF_IMAGE::SetAngleDisplayValues @@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_GEOREF_IMAGE::SetAngleDisplayValues, solar_az, solar_zen,		$
		red_az_zen, grn_az_zen, blu_az_zen, angleStruct
;print,'MISR_GEOREF_IMAGE::SetAngleDisplayValues'
;print,'solar_az=',solar_az
;print,'solar_zen=',solar_zen
;print,'red_az_zen=',red_az_zen
;print,'grn_az_zen=',grn_az_zen
;print,'blu_az_zen=',blu_az_zen

	IF NOT WIDGET_INFO(SELF.anglesFloatingBase,/VALID_ID) THEN BEGIN
		SELF.anglesFloatingBase = SELF->ReturnAnglesFloatingQueryBase(SELF.parentBase,MAP=1, COLUMN=1, $
			WINDOW_TITLE = 'Angle Display')
	ENDIF
	WIDGET_CONTROL, SELF.anglesFloatingBase, MAP = 1

	window1 = WIDGET_INFO( SELF.anglesFloatingBase, /CHILD )
	window2 = WIDGET_INFO( window1, /SIBLING )
	WIDGET_CONTROL, window1, GET_VALUE = window1ID
	WIDGET_CONTROL, window2, GET_VALUE = window2ID

	; circleState must be a structure containing:
	;  { 	title : string,		$
	;	size : bytarr(2),	$
	;	Angle : fltarr(4),	$
	;	Min : fltarr(4) }
	;	Max : fltarr(4) }

	; Azimuth (in top half or left half, depending on column or row orientation.)
	circleState = { title : 'Azimuth',				$
			size : SELF.anglesDisplaySize,			$
			Angle : [ red_az_zen[0],			$
				grn_az_zen[0],				$
				blu_az_zen[0],				$
				solar_az ],				$
			Min : [ angleStruct.redAzZenMinMax[0],		$
				angleStruct.grnAzZenMinMax[0],		$
				angleStruct.bluAzZenMinMax[0],		$
				angleStruct.solarAzZenMinMax[0] ],	$
			Max : [ angleStruct.redAzZenMinMax[1],		$
				angleStruct.grnAzZenMinMax[1],		$
				angleStruct.bluAzZenMinMax[1],		$
				angleStruct.solarAzZenMinMax[1] ]	}
	drawCircleAngle, window1ID, circleState, SELF.all_missing_data_values_ptr

	; Zenith (in bottom half or right half, depending on column or row orientation.)
	circleState = { title : 'Zenith',				$
			size : SELF.anglesDisplaySize,			$
			Angle : [ red_az_zen[1],			$
				grn_az_zen[1],				$
				blu_az_zen[1],				$
				solar_zen ],				$
			Min : [ angleStruct.redAzZenMinMax[2],		$
				angleStruct.grnAzZenMinMax[2],		$
				angleStruct.bluAzZenMinMax[2],		$
				angleStruct.solarAzZenMinMax[2] ],	$
			Max : [ angleStruct.redAzZenMinMax[3],		$
				angleStruct.grnAzZenMinMax[3],		$
				angleStruct.bluAzZenMinMax[3],		$
				angleStruct.solarAzZenMinMax[3] ]	}
	drawCircleAngle, window2ID, circleState, SELF.all_missing_data_values_ptr

END
; MISR_GEOREF_IMAGE::SetAngleDisplayValues

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@ MISR_GEOREF_IMAGE::ReturnAnglesFloatingQueryBase @@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION MISR_GEOREF_IMAGE::ReturnAnglesFloatingQueryBase, group_leader, $
	WINDOW_TITLE = window_title, ROW = row, COLUMN = column, MAP = map

	anglesFloatingBase = -1L

	IF PTR_VALID(SELF.anglesPtr) THEN BEGIN
		IF NOT KEYWORD_SET( window_title ) THEN window_title = 'Angle Display'
		mapKeywordSet = KEYWORD_SET( map )
		anglesColSet = KEYWORD_SET( column )
		anglesRowSet = KEYWORD_SET( row )
		IF ( NOT anglesColSet AND NOT anglesRowSet ) OR ( anglesColSet and anglesRowSet ) THEN BEGIN
			anglesColSet = 1
			anglesRowSet = 0
		ENDIF
		SELF.anglesRowSet = anglesRowSet
		anglesFloatingBase	= WIDGET_BASE( COLUMN = anglesColSet, ROW = anglesRowSet, $
						TITLE = window_title, MAP = mapKeywordSet, $
						GROUP_LEADER = group_leader, TLB_FRAME_ATTR = 1 )
		anglesDraw1		= WIDGET_DRAW( anglesFloatingBase, $
						XSIZE = SELF.anglesDisplaySize, $
						YSIZE = SELF.anglesDisplaySize )
		anglesDraw2		= WIDGET_DRAW( anglesFloatingBase, $
						XSIZE = SELF.anglesDisplaySize, $
						YSIZE = SELF.anglesDisplaySize )
		WIDGET_CONTROL, anglesFloatingBase, /REALIZE
	ENDIF

	RETURN, anglesFloatingBase
END
; MISR_GEOREF_IMAGE::ReturnAnglesFloatingQueryBase

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@ resolve_resolution_differences_eh @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO resolve_resolution_differences_eh, event
	WIDGET_CONTROL, event.id, GET_VALUE = val
	IF STRUPCASE( val ) EQ 'OK' THEN BEGIN
		WIDGET_CONTROL, event.top, /DESTROY
	ENDIF ELSE BEGIN
		WIDGET_CONTROL, event.top, GET_UVALUE = resolutionPtr
		WIDGET_CONTROL, event.id, GET_UVALUE = resolution
		*resolutionPtr = resolution
	ENDELSE
END
; resolve_resolution_differences_eh

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@ resolve_resolution_differences @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION resolve_resolution_differences,	$
		groupLeader,			$
		REDSIZE = redTileSize,		$
		GRNSIZE = grnTileSize,		$
		BLUSIZE = bluTileSize

	;---------------------------------------------
	; Download the value of KEYWORD_SET because it
	; gets used more than once.
	;---------------------------------------------
	IF KEYWORD_SET( redTileSize ) THEN redKey = 1 ELSE redKey = 0
	IF KEYWORD_SET( grnTileSize ) THEN grnKey = 1 ELSE grnKey = 0
	IF KEYWORD_SET( bluTileSize ) THEN bluKey = 1 ELSE bluKey = 0

	;---------------------------------------------
	; Check in reverse order (bgr) to default
	; resolution to red, or to green if no red is
	; present.  This routine never gets called in
	; the single channel b/w case, so either red
	; or green will always be present.
	;---------------------------------------------
	IF bluKey THEN BEGIN
		bluLabelValue	= 'blu resolution = ' + STRING( bluTileSize[0] ) + STRING( bluTileSize[1] )
		bluSensitive	= 1
		resolution	= bluTileSize
	ENDIF ELSE BEGIN
		bluLabelValue	= 'blu plane not loaded'
		bluSensitive	= 0
		bluTileSize	= [0,0]
	ENDELSE
	IF grnKey THEN BEGIN
		grnLabelValue	= 'grn resolution = ' + STRING( grnTileSize[0] ) + STRING( grnTileSize[1] )
		grnSensitive	= 1
		grnSetButton	= 1		; Set grn to default if present.
		resolution	= grnTileSize
	ENDIF ELSE BEGIN
		grnLabelValue	= 'grn plane not loaded'
		grnSensitive	= 0
		grnTileSize	= [0,0]
	ENDELSE
	IF redKey THEN BEGIN
		redLabelValue	= 'red resolution = ' + STRING( redTileSize[0] ) + STRING( redTileSize[1] )
		redSensitive	= 1
		grnSetButton	= 0		; Unset green as default when red is present.
		redSetButton	= 1		; Set red as default when present.
		resolution	= redTileSize
	ENDIF ELSE BEGIN
		redLabelValue	= 'red plane not loaded'
		redSensitive	= 0
		redTileSize	= [0,0]
	ENDELSE

	tlb		= WIDGET_BASE( /COLUMN, /MODAL, GROUP_LEADER = groupLeader, TITLE = 'Select one' )
	explanation	= WIDGET_LABEL( tlb, VALUE = 'Select block size for scroll tool.' )
	resBase		= WIDGET_BASE( tlb, /EXCLUSIVE, /COLUMN )
	redResButton	= WIDGET_BUTTON( resBase, VALUE = redLabelValue, UVALUE = redTileSize, SENSITIVE = redSensitive )
	grnResButton	= WIDGET_BUTTON( resBase, VALUE = grnLabelValue, UVALUE = grnTileSize, SENSITIVE = grnSensitive )
	bluResButton	= WIDGET_BUTTON( resBase, VALUE = bluLabelValue, UVALUE = bluTileSize, SENSITIVE = bluSensitive )
	buttonBase	= WIDGET_BASE( tlb, /ROW )
	okButton	= WIDGET_BUTTON( buttonBase, VALUE = 'OK' )
	resolutionPtr	= PTR_NEW( resolution )
	WIDGET_CONTROL, tlb, SET_UVALUE = resolutionPtr
	WIDGET_CONTROL, tlb, DEFAULT_BUTTON = okButton

	;---------------------------------------------
	; Check in reverse order (bgr) to default
	; chosen button to red, or to grn if no red.
	;---------------------------------------------
	IF NOT bluKey THEN BEGIN
		WIDGET_CONTROL, bluResButton, SENSITIVE = 0
		WIDGET_CONTROL, redResButton, /SET_BUTTON
	ENDIF
	IF NOT grnKey THEN BEGIN
		WIDGET_CONTROL, grnResButton, SENSITIVE = 0
		WIDGET_CONTROL, redResButton, /SET_BUTTON
	ENDIF
	IF NOT redKey THEN BEGIN
		WIDGET_CONTROL, redResButton, SENSITIVE = 0
		WIDGET_CONTROL, grnResButton, /SET_BUTTON
	ENDIF

	WIDGET_CONTROL, tlb, /REALIZE
	XMANAGER, 'resolution resolver', tlb, EVENT_HANDLER = 'resolve_resolution_differences_eh'

	resolution = *resolutionPtr
	PTR_FREE, resolutionPtr

;print,''
;print,'resolve_resolution_differences   ---   resolution = ',resolution
;print,''
	RETURN, resolution
END
; resolve_resolution_differences

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@ MISR_GEOREF_IMAGE::Spawn_Scroll_Tool @@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_GEOREF_IMAGE::Spawn_Scroll_Tool

	WIDGET_CONTROL, /HOURGLASS

	data_obj_array = OBJARR( 3 )

	;-------------------------------------------------
	;Get block sizes for each dislayed channel.
	;-------------------------------------------------
	IF ( SELF.redBandIdx GE 0 ) AND SELF.display_rgb[0] THEN BEGIN
		data_obj_array[0]	= *((*(SELF.dataPtr))[SELF.redBandIdx])
		redTileSize		= data_obj_array[0] -> GetTileSize()
	ENDIF ELSE redTileSize		= [0,0]
	IF ( SELF.grnBandIdx GE 0 ) AND SELF.display_rgb[1] THEN BEGIN
		data_obj_array[1]	= *((*(SELF.dataPtr))[SELF.grnBandIdx])
		grnTileSize		= data_obj_array[1] -> GetTileSize()
	ENDIF ELSE grnTileSize		= [0,0]
	IF ( SELF.bluBandIdx GE 0 ) AND SELF.display_rgb[2] THEN BEGIN
		data_obj_array[2]	= *((*(SELF.dataPtr))[SELF.bluBandIdx])
		bluTileSize		= data_obj_array[2] -> GetTileSize()
	ENDIF ELSE bluTileSize		= [0,0]
;print,'redTileSize = ',redTileSize
;print,'grnTileSize = ',grnTileSize
;print,'bluTileSize = ',bluTileSize

	groupLeader = SELF.drawBase

	;---------------------------------------------
	; Check resolutions and if two or more
	; channels have different resolutions then ask
	; user to specify which resolution the data is
	; to be scrolled at.
	;---------------------------------------------
	CASE 1 OF

		;---------------------------------------------
		; Three channel case.
		;---------------------------------------------
		(						$
		redTileSize[0] NE 0 AND				$
		redTileSize[1] NE 0 AND				$
		grnTileSize[0] NE 0 AND				$
		grnTileSize[1] NE 0 AND				$
		bluTileSize[0] NE 0 AND				$
		bluTileSize[1] NE 0				$
		) AND (						$
		( redTileSize[0] NE grnTileSize[0] ) OR		$
		( redTileSize[0] NE bluTileSize[0] ) OR		$
		( grnTileSize[0] NE bluTileSize[0] ) OR		$
		( redTileSize[1] NE grnTileSize[1] ) OR		$
		( redTileSize[1] NE bluTileSize[1] ) OR		$
		( grnTileSize[1] NE bluTileSize[1] )		$
		) :						$
		resolution = resolve_resolution_differences(	$
				groupLeader,			$
				REDSIZE = redTileSize,		$
				GRNSIZE = grnTileSize,		$
				BLUSIZE = bluTileSize		)

		;---------------------------------------------
		; Two channel case:  RED against GRN
		;---------------------------------------------
		(						$
		redTileSize[0] NE 0 AND				$
		redTileSize[1] NE 0 AND				$
		grnTileSize[0] NE 0 AND				$
		grnTileSize[1] NE 0				$
		) AND (						$
		( redTileSize[0] NE grnTileSize[0] ) OR		$
		( redTileSize[1] NE grnTileSize[1] )		$
		) :						$
		resolution = resolve_resolution_differences(	$
				groupLeader,			$
				REDSIZE = redTileSize,		$
				GRNSIZE = grnTileSize		)

		;---------------------------------------------
		; Two channel case:  RED against BLU
		;---------------------------------------------
		(						$
		redTileSize[0] NE 0 AND				$
		redTileSize[1] NE 0 AND				$
		bluTileSize[0] NE 0 AND				$
		bluTileSize[1] NE 0				$
		) AND (						$
		( redTileSize[0] NE bluTileSize[0] ) OR		$
		( redTileSize[1] NE bluTileSize[1] )		$
		) :						$
		resolution = resolve_resolution_differences(	$
				groupLeader,			$
				REDSIZE = redTileSize,		$
				BLUSIZE = bluTileSize		)

		;---------------------------------------------
		; Two channel case:  GRN against BLU
		;---------------------------------------------
		(						$
		grnTileSize[0] NE 0 AND				$
		grnTileSize[1] NE 0 AND				$
		bluTileSize[0] NE 0 AND				$
		bluTileSize[1] NE 0				$
		) AND (						$
		( grnTileSize[0] NE bluTileSize[0] ) OR		$
		( grnTileSize[1] NE bluTileSize[1] )		$
		) :						$
		resolution = resolve_resolution_differences(	$
				groupLeader,			$
				GRNSIZE = grnTileSize,		$
				BLUSIZE = bluTileSize		)

		;---------------------------------------------
		; One channel case (black and white display)
		;---------------------------------------------
		ELSE : BEGIN

;help,resolution
;help,redTileSize
			CASE 1 OF
				redTileSize[0] NE 0 AND redTileSize[1] NE 0 : resolution = redTileSize
				grnTileSize[0] NE 0 AND grnTileSize[1] NE 0 : resolution = grnTileSize
				bluTileSize[0] NE 0 AND bluTileSize[1] NE 0 : resolution = bluTileSize
			ENDCASE
		END

	ENDCASE
;print,''
;print,'MISR_GEOREF_IMAGE::Spawn_Scroll_Tool   ---   resolution = ',resolution
;print,''

	;---------------------------------------------
	; Put data into objarr and call scroll tool.
	;---------------------------------------------
	scroll_tool, data_obj_array, RESOLUTION = resolution, GROUP_LEADER = groupLeader

END
; MISR_GEOREF_IMAGE::Spawn_Scroll_Tool

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ MISR_GEOREF_IMAGE::INIT @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION MISR_GEOREF_IMAGE::INIT, parentBase, DATA_PTR = dataPtr,			$
					ANGLESPTR=anglesPtr,				$
					ANGLES_DISPLAY_SIZE = anglesDisplaySize,	$
					START_BLOCK = start_block,			$
					N_BLOCKS = n_blocks,				$
					IN_RESOLUTION = in_resolution,			$
					_EXTRA = e

	SELF.anglesFloatingBase = -1
	data_exists		= 0

	IF KEYWORD_SET(dataPtr) THEN BEGIN
		data_exists	= 1
		success	= SELF->GEOREF_IMAGE::INIT( parentBase, DATA_PTR = dataPtr, _EXTRA = e )
	ENDIF ELSE BEGIN
		success	= SELF->GEOREF_IMAGE::INIT( parentBase, _EXTRA = e )
	ENDELSE

	IF WIDGET_INFO( SELF.scrollToolButton, /VALID_ID ) THEN $
		WIDGET_CONTROL, SELF.scrollToolButton, SENSITIVE = 1

	IF KEYWORD_SET(in_resolution) THEN BEGIN
		SELF.in_resolution_act	= in_resolution.act
		SELF.in_resolution_alt	= in_resolution.alt
	ENDIF

	SELF.misr_start_block = (-1)
	IF KEYWORD_SET(start_block) THEN SELF.misr_start_block = start_block

	SELF.misr_n_blocks = (-1)
	IF KEYWORD_SET(n_blocks) THEN SELF.misr_n_blocks = n_blocks

	IF KEYWORD_SET(anglesPtr) AND data_exists THEN BEGIN

		SELF.all_missing_data_values_ptr = PTR_NEW( DOUBLE( [ $
			2E14-1D, 2E14-2D, 2E14-3D, 2E14-4D, 2E14-5D, 2E14-6D, $
			65523, 65515, 65511, 16380, 16379, 16378, 16377, 255, 99, 0, $
			-21, -99, -111, -222, -333, -444, -555, -999, -9999 ] ), /NO_COPY )
;print,'MISR_GEOREF_IMAGE::INIT   *(SELF.all_missing_data_values_ptr) = '
;print,*(SELF.all_missing_data_values_ptr)

;print,'PTR_VALID(anglesPtr) = ',PTR_VALID(anglesPtr)
		IF PTR_VALID(anglesPtr) THEN BEGIN

			SELF.valid_img_idx_ptr	=				$
				PTR_NEW( WHERE( PTR_VALID(dataPtr) ), /NO_COPY )


			n_imgs			= N_ELEMENTS( *(SELF.valid_img_idx_ptr) )

			SELF.cameraAzimuthZoomPctXPtr		= PTR_NEW(FLTARR(n_imgs),/NO_COPY)
			SELF.cameraAzimuthZoomPctYPtr		= PTR_NEW(FLTARR(n_imgs),/NO_COPY)

;;;ckt,nov1999			SELF.cameraAzimuthZoomXSPtr		= PTR_NEW(LONARR(n_imgs),/NO_COPY)
;;;ckt,nov1999			SELF.cameraAzimuthZoomYSPtr		= PTR_NEW(LONARR(n_imgs),/NO_COPY)
;;;ckt,nov1999			SELF.cameraAzimuthZoomNXPtr		= PTR_NEW(LONARR(n_imgs),/NO_COPY)
;;;ckt,nov1999			SELF.cameraAzimuthZoomNYPtr		= PTR_NEW(LONARR(n_imgs),/NO_COPY)

			SELF.cameraAzimuthZoomXSPtr		= PTR_NEW(FLTARR(n_imgs),/NO_COPY)
			SELF.cameraAzimuthZoomYSPtr		= PTR_NEW(FLTARR(n_imgs),/NO_COPY)
			SELF.cameraAzimuthZoomNXPtr		= PTR_NEW(FLTARR(n_imgs),/NO_COPY)
			SELF.cameraAzimuthZoomNYPtr		= PTR_NEW(FLTARR(n_imgs),/NO_COPY)

			SELF.cameraZenithZoomPctXPtr		= PTR_NEW(FLTARR(n_imgs),/NO_COPY)
			SELF.cameraZenithZoomPctYPtr		= PTR_NEW(FLTARR(n_imgs),/NO_COPY)

;;;ckt,nov1999			SELF.cameraZenithZoomXSPtr		= PTR_NEW(LONARR(n_imgs),/NO_COPY)
;;;ckt,nov1999			SELF.cameraZenithZoomYSPtr		= PTR_NEW(LONARR(n_imgs),/NO_COPY)
;;;ckt,nov1999			SELF.cameraZenithZoomNXPtr		= PTR_NEW(LONARR(n_imgs),/NO_COPY)
;;;ckt,nov1999			SELF.cameraZenithZoomNYPtr		= PTR_NEW(LONARR(n_imgs),/NO_COPY)

			SELF.cameraZenithZoomXSPtr		= PTR_NEW(FLTARR(n_imgs),/NO_COPY)
			SELF.cameraZenithZoomYSPtr		= PTR_NEW(FLTARR(n_imgs),/NO_COPY)
			SELF.cameraZenithZoomNXPtr		= PTR_NEW(FLTARR(n_imgs),/NO_COPY)
			SELF.cameraZenithZoomNYPtr		= PTR_NEW(FLTARR(n_imgs),/NO_COPY)

			SELF.cameraAzMinMaxPtr			= PTR_NEW(FLTARR(n_imgs,2),/NO_COPY)
			SELF.cameraZenMinMaxPtr			= PTR_NEW(FLTARR(n_imgs,2),/NO_COPY)
			SELF.solarAzMinMaxPtr			= PTR_NEW(FLTARR(2),/NO_COPY)
			SELF.solarZenMinMaxPtr			= PTR_NEW(FLTARR(2),/NO_COPY)

			current_solaz_ptr			= (*anglesPtr).solarAzimuth
			current_solzen_ptr			= (*anglesPtr).solarZenith

;;;ckt,nov1999			SELF.solarAzimuthZoomXS			= 0L
			SELF.solarAzimuthZoomNX			= (*current_solaz_ptr)->GetImageWidth()
;;;ckt,nov1999			SELF.solarAzimuthZoomYS			= 0L
			SELF.solarAzimuthZoomNY			= (*current_solaz_ptr)->GetImageHeight()

			SELF.solarAzimuthZoomPctX		= ( FLOAT(SELF.viewportNX) /	$
				FLOAT( (*current_solaz_ptr)->GetImageWidth() )) * 100.0

			SELF.solarAzimuthZoomPctY		= ( FLOAT(SELF.viewportNY) /	$
				FLOAT((*current_solaz_ptr)->GetImageHeight() )) * 100.0

;;;ckt,nov1999			SELF.solarZenithZoomXS			= 0L
			SELF.solarZenithZoomNX			= (*current_solzen_ptr)->GetImageWidth()
;;;ckt,nov1999			SELF.solarZenithZoomYS			= 0L
			SELF.solarZenithZoomNY			= (*current_solzen_ptr)->GetImageHeight()
			SELF.solarZenithZoomPctX		= ( FLOAT(SELF.viewportNX) /	$
				FLOAT((*current_solzen_ptr)->GetImageWidth() )) * 100.0
			SELF.solarZenithZoomPctY		= ( FLOAT(SELF.viewportNY) /	$
				FLOAT((*current_solzen_ptr)->GetImageHeight() )) * 100.0

			SELF.solarAzMinMaxPtr			= PTR_NEW( [(*current_solaz_ptr)->GetMinVal(	$
				0,(*current_solaz_ptr)->GetImageWidth()-1L,0,(*current_solaz_ptr)->GetImageHeight()-1L,	$
				(*current_solaz_ptr)->GetImageWidth(),(*current_solaz_ptr)->GetImageHeight()),	$
										(*current_solaz_ptr)->GetMaxVal(	$
				0,(*current_solaz_ptr)->GetImageWidth()-1L,0,(*current_solaz_ptr)->GetImageHeight()-1L,	$
				(*current_solaz_ptr)->GetImageWidth(),(*current_solaz_ptr)->GetImageHeight())], /NO_COPY )
			SELF.solarZenMinMaxPtr			= PTR_NEW( [(*current_solzen_ptr)->GetMinVal(	$
				0,(*current_solzen_ptr)->GetImageWidth()-1L,0,(*current_solzen_ptr)->GetImageHeight()-1L,	$
				(*current_solzen_ptr)->GetImageWidth(),(*current_solzen_ptr)->GetImageHeight()),	$
									(*current_solzen_ptr)->GetMaxVal(	$
				0,(*current_solzen_ptr)->GetImageWidth()-1L,0,(*current_solzen_ptr)->GetImageHeight()-1L,	$
				(*current_solzen_ptr)->GetImageWidth(),(*current_solzen_ptr)->GetImageHeight())], /NO_COPY )

			FOR i = 0, n_imgs - 1 DO BEGIN
;print,'i = ',i

				;===========================================================================
				; assumption: of current_az_ptr is valid, the associated current_zen_ptr
				; is also valid.
				;===========================================================================
				current_az_ptr	= (*anglesPtr).cameraAzimuth.slot[ (*(SELF.valid_img_idx_ptr))[i] ]
				current_zen_ptr	= (*anglesPtr).cameraZenith.slot[ (*(SELF.valid_img_idx_ptr))[i] ]
;print,'PTR_VALID( current_az_ptr ) = ',PTR_VALID( current_az_ptr )
				IF PTR_VALID( current_az_ptr ) THEN BEGIN
;;;ckt,nov1999					(*(SELF.cameraAzimuthZoomXSPtr))[i]	= 0L
					(*(SELF.cameraAzimuthZoomNXPtr))[i]	= (*current_az_ptr)->GetImageWidth()
;;;ckt,nov1999					(*(SELF.cameraAzimuthZoomYSPtr))[i]	= 0L
					(*(SELF.cameraAzimuthZoomNYPtr))[i]	= (*current_az_ptr)->GetImageHeight()

					(*(SELF.cameraAzimuthZoomPctXPtr))[i]	= ( FLOAT(SELF.viewportNX) /	$
						FLOAT((*current_az_ptr)->GetImageWidth() )) * 100.0
					(*(SELF.cameraAzimuthZoomPctYPtr))[i]	= ( FLOAT(SELF.viewportNY) /	$
						FLOAT((*current_az_ptr)->GetImageHeight() )) * 100.0

;;;ckt,nov1999					(*(SELF.cameraZenithZoomXSPtr))[i]	= 0L
					(*(SELF.cameraZenithZoomNXPtr))[i]	= (*current_zen_ptr)->GetImageWidth()
;;;ckt,nov1999					(*(SELF.cameraZenithZoomYSPtr))[i]	= 0L
					(*(SELF.cameraZenithZoomNYPtr))[i]	= (*current_zen_ptr)->GetImageHeight()

					(*(SELF.cameraZenithZoomPctXPtr))[i]	= ( FLOAT(SELF.viewportNX) /	$
						FLOAT((*current_zen_ptr)->GetImageWidth() )) * 100.0
					(*(SELF.cameraZenithZoomPctYPtr))[i]	= ( FLOAT(SELF.viewportNY) /	$
						FLOAT((*current_zen_ptr)->GetImageHeight() )) * 100.0

					*(SELF.all_missing_data_values_ptr)	= DOUBLE([ $
						2E14-1D, 2E14-2D, 2E14-3D, 2E14-4D, 2E14-5D, 2E14-6D, $
						65523, 65515, 65511, 16380, 16379, 16378, 16377, 255, 99, 0, $
						-21, -99, -111, -222, -333, -444, -555, -999, -9999 ] )
					(*(SELF.cameraAzMinMaxPtr))[i,*] = [ (*current_az_ptr)->GetMinVal(	$
					0, (*current_az_ptr)->GetImageWidth()-1L,0,(*current_az_ptr)->GetImageHeight()-1L,	$
					(*current_az_ptr)->GetImageWidth(), (*current_az_ptr)->GetImageHeight()),	$
					(*current_az_ptr)->GetMaxVal(		$
					0, (*current_az_ptr)->GetImageWidth()-1L,0,(*current_az_ptr)->GetImageHeight()-1L,	$
					(*current_az_ptr)->GetImageWidth(), (*current_az_ptr)->GetImageHeight()) ]
					(*(SELF.cameraZenMinMaxPtr))[i,*] = [ (*current_zen_ptr)->GetMinVal(	$
					0, (*current_zen_ptr)->GetImageWidth()-1L,0,(*current_zen_ptr)->GetImageHeight()-1L,	$
					(*current_zen_ptr)->GetImageWidth(), (*current_zen_ptr)->GetImageHeight()),	$
					(*current_zen_ptr)->GetMaxVal(	$
					0, (*current_zen_ptr)->GetImageWidth()-1L,0,(*current_zen_ptr)->GetImageHeight()-1L,	$
					(*current_zen_ptr)->GetImageWidth(), (*current_zen_ptr)->GetImageHeight()) ]
				ENDIF
			ENDFOR

			IF NOT KEYWORD_SET( anglesDisplaySize ) $
			THEN SELF.anglesDisplaySize = 200 $
			ELSE SELF.anglesDisplaySize = anglesDisplaySize

			SELF.anglesPtr = anglesPtr
;;;DISABLE FOR NOW...(July 20, 1999)
;;			SELF.anglesFloatingBase = SELF->ReturnAnglesFloatingQueryBase(SELF.parentBase,MAP=0, COLUMN=1, $
;;				WINDOW_TITLE = 'Angle Display')
		ENDIF
	ENDIF

;print,'exiting MISR_GEOREF_IMAGE::INIT'

	RETURN, success
END
; MISR_GEOREF_IMAGE::INIT

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ MISR_GEOREF_IMAGE::CLEANUP @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_GEOREF_IMAGE::CLEANUP
	IF PTR_VALID(SELF.anglesPtr) THEN success = depointer(SELF.anglesPtr)

	PTR_FREE, SELF.valid_img_idx_ptr
	PTR_FREE, SELF.cameraAzimuthZoomPctXPtr
	PTR_FREE, SELF.cameraAzimuthZoomPctYPtr
	PTR_FREE, SELF.cameraAzimuthZoomXSPtr
	PTR_FREE, SELF.cameraAzimuthZoomYSPtr
	PTR_FREE, SELF.cameraAzimuthZoomNXPtr
	PTR_FREE, SELF.cameraAzimuthZoomNYPtr
	PTR_FREE, SELF.cameraZenithZoomPctXPtr
	PTR_FREE, SELF.cameraZenithZoomPctYPtr
	PTR_FREE, SELF.cameraZenithZoomXSPtr
	PTR_FREE, SELF.cameraZenithZoomYSPtr
	PTR_FREE, SELF.cameraZenithZoomNXPtr
	PTR_FREE, SELF.cameraZenithZoomNYPtr

	PTR_FREE, SELF.cameraAzMinMaxPtr
	PTR_FREE, SELF.cameraZenMinMaxPtr
	PTR_FREE, SELF.solarAzMinMaxPtr
	PTR_FREE, SELF.solarZenMinMaxPtr

	IF WIDGET_INFO(SELF.anglesFloatingBase,/VALID_ID) THEN WIDGET_CONTROL, SELF.anglesFloatingBase, /DESTROY
	SELF->GEOREF_IMAGE::CLEANUP
END
; MISR_GEOREF_IMAGE::CLEANUP

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ MISR_GEOREF_IMAGE__DEFINE @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_GEOREF_IMAGE__DEFINE
	MISR_GEOREF_IMAGE		=	{			$
		MISR_GEOREF_IMAGE,					$
			INHERITS GEOREF_IMAGE,				$

;azimuth	= { slot : PTRARR(6) }
;zenith	= { slot : PTRARR(6) }
;anglesPtr = PTR_NEW( { $
;	cameraAzimuth	: azimuth, $
;	cameraZenith	: zenith, $
;	solarAzimuth	: PTR_NEW(solarAzimuth, /NO_COPY), $
;	solarZenith	: PTR_NEW(solarZenith, /NO_COPY) }, /NO_COPY )


				anglesPtr			: PTR_NEW(),	$
				valid_img_idx_ptr		: PTR_NEW(),	$

;;;ckt,nov1999				solarAzimuthZoomPctX		: 0L,		$
;;;ckt,nov1999				solarAzimuthZoomPctY		: 0L,		$
;;;ckt,nov1999				solarAzimuthZoomXS		: 0L,		$
;;;ckt,nov1999				solarAzimuthZoomYS		: 0L,		$
;;;ckt,nov1999				solarAzimuthZoomNX		: 0L,		$
;;;ckt,nov1999				solarAzimuthZoomNY		: 0L,		$

				solarAzimuthZoomPctX		: 0.0,		$
				solarAzimuthZoomPctY		: 0.0,		$
				solarAzimuthZoomXS		: 0.0,		$
				solarAzimuthZoomYS		: 0.0,		$
				solarAzimuthZoomNX		: 0.0,		$
				solarAzimuthZoomNY		: 0.0,		$

				solarZenithZoomPctX		: 0.0,		$
				solarZenithZoomPctY		: 0.0,		$
				solarZenithZoomXS		: 0.0,		$
				solarZenithZoomYS		: 0.0,		$
				solarZenithZoomNX		: 0.0,		$
				solarZenithZoomNY		: 0.0,		$

;;;ckt,nov1999				solarZenithZoomPctX		: 0L,		$
;;;ckt,nov1999				solarZenithZoomPctY		: 0L,		$
;;;ckt,nov1999				solarZenithZoomXS		: 0L,		$
;;;ckt,nov1999				solarZenithZoomYS		: 0L,		$
;;;ckt,nov1999				solarZenithZoomNX		: 0L,		$
;;;ckt,nov1999				solarZenithZoomNY		: 0L,		$

				cameraAzimuthZoomPctXPtr	: PTR_NEW(),	$
				cameraAzimuthZoomPctYPtr	: PTR_NEW(),	$
				cameraAzimuthZoomXSPtr		: PTR_NEW(),	$
				cameraAzimuthZoomYSPtr		: PTR_NEW(),	$
				cameraAzimuthZoomNXPtr		: PTR_NEW(),	$
				cameraAzimuthZoomNYPtr		: PTR_NEW(),	$

				cameraZenithZoomPctXPtr		: PTR_NEW(),	$
				cameraZenithZoomPctYPtr		: PTR_NEW(),	$
				cameraZenithZoomXSPtr		: PTR_NEW(),	$
				cameraZenithZoomYSPtr		: PTR_NEW(),	$
				cameraZenithZoomNXPtr		: PTR_NEW(),	$
				cameraZenithZoomNYPtr		: PTR_NEW(),	$

				anglesFloatingBase		: 0L,		$
				anglesDisplaySize		: 0,		$
				anglesRowSet			: 0,		$
				cameraAzMinMaxPtr		: PTR_NEW(),	$
				cameraZenMinMaxPtr		: PTR_NEW(),	$
				solarAzMinMaxPtr		: PTR_NEW(),	$
				solarZenMinMaxPtr		: PTR_NEW(),	$

				misr_start_block		:0,		$
				misr_n_blocks			:0,		$
				in_resolution_act		:0,		$
				in_resolution_alt		:0,		$
				all_missing_data_values_ptr	: PTR_NEW()	$

			 }
END
; MISR_GEOREF_IMAGE__DEFINE
