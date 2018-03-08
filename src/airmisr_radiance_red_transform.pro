FUNCTION airmisr_radiance_red_transform, image_data_obj, source_data
; 
;  This transform file attempts to search a MISR HDF-EOS grid
;  file for scale ("Scale factor") metadata that is to be applied to
;  the MISR radiance parameter.
; 
;  The GetGridAttribute method has the following call sequence:
; 
;  metadata_value = SELF->GetGridAttribute( metadata_name, default_value, /SILENT, /MENU )
; 
;  where:
; 	metadata_name	= full or partial name of metadata to be located.  GetGridAttribute
; 			  extracts all of the names of the metadata associated with the
; 			  current field.  It then matches the names of the extracted metadata
; 			  with metadata_name.  If metadata_name matches a sub-string or full-string
; 			  name of any of the extracted metadata, the value of the matching
; 			  metadata name is returned.  If there is more than one match, only the
; 			  value of the first matchn is returned.  This allows this transform file
; 			  to be used with several fields which have similar metadata (such as
; 			  scale and offset), but whose associated metadata names are slightly
; 			  different (such as 'Scale_ExtNDVI' or 'Scale_LandHDRF')
; 	default_value	= the value to return if the specified metadata name does not exist for the
; 			  current field
; 	SILENT		= keyword used to suppress error messages popping up
; 	MENU		= keyword to use to bring up interface which lists all metadata names
; 			  associated with the current field and which allows user to make a
; 			  selection interactively.  The metadata_name positional parameter must still
; 			  be specified, but it can be a dummy string.
; 
;  For this transform, the metadata value is applied to the source data as follows:
; 
; 	transformed_data_value	= scale * data_value
; 
;  The default value of scale is 1.0 so that, if "Scale Factor"
;  is not found, the default value does not alter the original value of the data.
; 
scale_factor	= image_data_obj->GetGridAttribute( 'Rad_scale_factor', 1.0, /SILENT )
transformed_data=FLOAT(source_data)
idx	= WHERE( transformed_data NE 16378u AND transformed_data NE 16380u, cnt )
IF cnt GT 0 THEN transformed_data[idx] = transformed_data[idx] * scale_factor[2]
RETURN, transformed_data
END
