FUNCTION scale_offset_min_max_transform, image_data_obj, source_data
; The following metadata has been searched for:
; Scale... (e.g., Scale_ExtNDVI)
; Offset... (e.g., Offset_NDVI)
; Min... (e.g., Min_ExtNDVI)
; Max... (e.g., Max_ExtNDVI)
; If any of these metadata values are not found, the following default values are used:
; Scale...: 1.0
; Offset...: 0.0
; Min...: global data minimum value
; Max...: global data maximum value
; The transformation formula is as follows:
; transformed_data_value = scale*data_value+offset
scale_factor	= image_data_obj->GetGridAttribute( 'Scale', 1.0, /SILENT )
offset_factor	= image_data_obj->GetGridAttribute( 'Offset', 1.0, /SILENT )
min_val		= image_data_obj->GetGridAttribute( 'Min', MIN(source_data), /SILENT )
max_val		= image_data_obj->GetGridAttribute( 'Max', MAX(source_data), /SILENT )
transformed_data = source_data
idx = WHERE( source_data GE min_val AND source_data LE max_val, cnt )
IF cnt GT 0 THEN transformed_data[idx]=transformed_data[idx]*scale_factor[0]+offset_factor[0]
RETURN, transformed_data
END
