@IMAGE_DATA_R2.pro
@get_local_granule_id.pro
@ReturnListSelectionIdx.pro
@misr_get_default_transform_file.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ MISR_IMAGE_DATA::GetGridAttribute @@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION MISR_IMAGE_DATA::GetGridAttribute,					$
						attributename,			$
						defaultvalue,			$
						MENU = menu,			$
						SILENT = silent

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when 
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== MISR_IMAGE_DATA::GetGridAttribute =========='
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		e_msg	= [											$
				routine_name,									$
				'Error Index: ' + STRTRIM( error_status, 2 ),					$
				'Error Message: ' + !ERR_STRING,						$
				'',										$
				'Suggestion: If attempting to read or write a file, then check permissions.',	$
				'',										$
				'Returning defaultvalue...' ]
		result	= DIALOG_MESSAGE( e_msg, /ERROR )
		RETURN, defaultvalue
	ENDIF


	display_message	= 1
	IF KEYWORD_SET(silent) THEN display_message = 0
	
;print,'SELF.source_file=',SELF.source_file	
;print,'SELF.grid_name=',SELF.grid_name

	IF SELF.source_file EQ '' OR SELF.grid_name EQ '' THEN BEGIN
;print,'SELF.source_file=',SELF.source_file
;print,'SELF.grid_name=',SELF.grid_name
		msg	= [							$
				'MISR_IMAGE_DATA::GetGridAttribute:',		$
				'',						$
				'	no defined filename or grid name' ]
		IF display_message THEN res = DIALOG_MESSAGE( msg, /ERROR )
		res = EOS_GD_CLOSE( fid )
		RETURN, defaultvalue
	ENDIF
	
	fid			= EOS_GD_OPEN( SELF.source_file, /READ )
	IF fid LT 0 THEN BEGIN
;print,'fid=',fid
		msg	= [							$
				'MISR_IMAGE_DATA::GetGridAttribute:',		$
				'',						$
				'	EOS_GD_OPEN error with the file:',	$
				'	' + STRTRIM( SELF.source_file, 2 ) ]
		IF display_message THEN res = DIALOG_MESSAGE( msg, /ERROR )
		res = EOS_GD_CLOSE( fid )
		RETURN, defaultvalue
	ENDIF
	
	n_grid			= EOS_GD_INQGRID( SELF.source_file, grid_list )
	IF n_grid LT 0 THEN BEGIN
;print,'n_grid=',n_grid
		msg	= [							$
				'MISR_IMAGE_DATA::GetGridAttribute:',		$
				'',						$
				'	EOS_GD_INQGRID error with the file:',	$
				'	' + STRTRIM( SELF.source_file, 2 ) ]
		IF display_message THEN res	= DIALOG_MESSAGE( msg, /ERROR )
		res	= EOS_GD_CLOSE( fid )
		RETURN, defaultvalue
	ENDIF
	
	split_grid_list	= STRTRIM( STR_SEP( grid_list, ',' ), 2 )
	idx		= WHERE( split_grid_list EQ SELF.grid_name, cnt )
	IF cnt LE 0 THEN BEGIN
;print,'split_grid_list=',split_grid_list
;print,'SELF.grid_name,=',SELF.grid_name
;print,'cnt=',cnt
		msg	= [							$
				'MISR_IMAGE_DATA::GetGridAttribute:',		$
				'',						$
				'	grid named ' + SELF.grid_name + ' not in:',	$
				'	' + STRTRIM( SELF.source_file, 2 ) ]
		IF display_message THEN res	= DIALOG_MESSAGE( msg, /ERROR )
		res	= EOS_GD_CLOSE( fid )
		RETURN, defaultvalue
	ENDIF
	
	grid_id		= EOS_GD_ATTACH( fid, SELF.grid_name )
	IF grid_id LT 0 THEN BEGIN
;print,'SELF.grid_name=',SELF.grid_name
		msg	= [							$
				'MISR_IMAGE_DATA::GetGridAttribute:',		$
				'',						$
				'	EOS_GD_ATTACH error with the file:',	$
				'	' + STRTRIM( SELF.source_file, 2 ),	$
				'	and grid ' + SELF.grid_name ]
		IF display_message THEN res	= DIALOG_MESSAGE( msg, /ERROR )
		res	= EOS_GD_CLOSE( fid )
		RETURN, defaultvalue
	ENDIF
	
	n_attrs		= EOS_GD_INQATTRS( grid_id, attr_list )
	IF n_attrs LT 0 THEN BEGIN
;print,'n_attrs=',n_attrs
		msg	= [							$
				'MISR_IMAGE_DATA::GetGridAttribute:',		$
				'',						$
				'	EOS_GD_INQATTRS error with the file:',	$
				'	' + STRTRIM( SELF.source_file, 2 ),	$
				'	and grid ' + SELF.grid_name ]
		IF display_message THEN res	= DIALOG_MESSAGE( msg, /ERROR )
		success	= EOS_GD_DETACH( grid_id )
		res	= EOS_GD_CLOSE( fid )
		RETURN, defaultvalue
	ENDIF
	
	split_attr_list	= STRTRIM( STR_SEP( attr_list, ',' ), 2 )
	
	IF KEYWORD_SET(menu) THEN BEGIN
		done	= 0
		cancel	= 0
		tmpb	= WIDGET_BASE()
		WHILE NOT done DO BEGIN
			attr_idx	= ReturnListSelectionIdx(											$
						WINDOW_TITLE = 'Attributes For Grid: '+SELF.grid_name+', Field: '+SELF.field_name,			$
						LIST_TITLE = 'Select An Attribute Corresponding To ' + attributename+' From The List Below:',		$
						GROUP_LEADER = tmpb,											$
						LIST_CONTENTS = split_attr_list )
			IF attr_idx LT 0 THEN BEGIN
				res	= DIALOG_MESSAGE( [ 'No attribute selected...',					$
							    'do you want another chance to select an attribute?' ],	$
							    /QUESTION )
				IF STRTRIM(STRUPCASE(res),2) EQ 'NO' THEN BEGIN
					done	= 1
					cancel	= 1
				ENDIF
			ENDIF ELSE BEGIN
				done		= 1
				attributename	= split_attr_list[attr_idx]
			ENDELSE
		ENDWHILE
		WIDGET_CONTROL, tmpb, /DESTROY
	ENDIF
	
	idx		= WHERE( split_attr_list EQ attributename, cnt )
;print,'SELF.source_file=',SELF.source_file
;print,'attributename=',attributename
;print,'split_attr_list='
;print,split_attr_list
	IF cnt LE 0 THEN BEGIN
		;
		; Look for a partial match within all of the attributes; if exactly one is
		; found, then return it, otherwise, return a null pointer.
		;
		FOR i = 0, N_ELEMENTS(split_attr_list)-1 DO BEGIN
			IF STRPOS( split_attr_list[i], attributename, 0 ) GE 0 THEN BEGIN
				attributename	= split_attr_list[i]
				cnt		= cnt + 1
			ENDIF
		ENDFOR
		IF cnt GT 1 THEN cnt = 0
	ENDIF
	
	IF cnt LE 0 THEN BEGIN
		msg	= [								$
				'MISR_IMAGE_DATA::GetGridAttribute:',			$
				'',							$
				'	attribute named ' + attributename + ' not in:',	$
				'	file ' + STRTRIM( SELF.source_file, 2 ),	$
				'	nor in grid ' + SELF.grid_name ]
		IF display_message THEN res	= DIALOG_MESSAGE( msg, /ERROR )
		success	= EOS_GD_DETACH( grid_id )
		res	= EOS_GD_CLOSE( fid )
		RETURN, defaultvalue
	ENDIF
	
print,attributename
help,attributename
	success		= EOS_GD_READATTR( grid_id, attributename, attribute_value )
print,attribute_value
help,attribute_value
	
	IF success LT 0 THEN BEGIN
		msg	= [									$
				'MISR_IMAGE_DATA::GetGridAttribute:',				$
				'',								$
				'	error reading attribute named ' + attributename,	$
				'	from file ' + STRTRIM( SELF.source_file, 2 ),		$
				'	and grid ' + SELF.grid_name ]
		IF display_message THEN res	= DIALOG_MESSAGE( msg, /ERROR )
		success	= EOS_GD_DETACH( grid_id )
		res	= EOS_GD_CLOSE( fid )
		RETURN, defaultvalue
	ENDIF
	
	success		= EOS_GD_DETACH( grid_id )
	
	res	= EOS_GD_CLOSE( fid )
	RETURN, attribute_value
	
END
; MISR_IMAGE_DATA::GetGridAttribute

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@ MISR_IMAGE_DATA::Set_Default_Transform @@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_IMAGE_DATA::Set_Default_Transform
	SELF->SetTransform, misr_get_default_transform_file( SELF.granule_id_string ), SUCCESS = transform_successful
	IF transform_successful THEN BEGIN
		SELF.default_transform_file	= misr_get_default_transform_file( SELF.granule_id_string )
		SELF.current_transform_file	= SELF.default_transform_file
	ENDIF
END
; MISR_IMAGE_DATA::Set_Default_Transform



;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@ MISR_IMAGE_DATA::ReturnImageTileLocationValueAtXY @@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION MISR_IMAGE_DATA::ReturnImageTileLocationValueAtXY, 			$
					global_x_image_coord,		$
					global_y_image_coord
					
	ret		= SELF->Return_Display_Struct(			$
					global_x_image_coord,		$
					global_y_image_coord,		$
					global_x_image_coord,		$
					global_y_image_coord,		$
					1,				$
					1,				$
					1.0 )
					
	
	slice_size_xy	= SIZE( REFORM( ((*(SELF.img_stack_ptr))[*,*,(ret.slice_idx)[0]]) ) )
	h_size		= slice_size_xy[1] - 1
	v_size		= slice_size_xy[2] - 1
	
	
	RETURN,									$
		[ FIX( ret.slice_h_start / SELF.original_data_size_factor ),	$
		  FIX( ( v_size - ret.slice_v_start ) / SELF.original_data_size_factor ) ]
END
; MISR_IMAGE_DATA::ReturnImageTileLocationValueAtXY





;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ MISR_IMAGE_DATA::INIT @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION MISR_IMAGE_DATA::INIT,							$
				img_stack,					$
				global_h_off_stack,				$
				global_v_off_stack,				$
				GRID_NAME = grid_name,				$
				FIELD_NAME = field_name,			$
				_Extra = e
				
	IF KEYWORD_SET(grid_name) THEN SELF.grid_name = grid_name ELSE grid_name = ''
	IF KEYWORD_SET(field_name) THEN SELF.field_name = field_name ELSE field_name = ''
print,'SELF.grid_name=',SELF.grid_name
print,'SELF.field_name=',SELF.field_name
	success			= SELF->IMAGE_DATA::INIT(			$
						img_stack,			$
						global_h_off_stack,		$
						global_v_off_stack,		$
						_Extra = e )
;print,'MISR_IMAGE_DATA::INIT, success #1 =',success
;print,'just before call to get_local_granule_id from MISR_IMAGE_DATA::INIT'
	IF STRUPCASE(STRTRIM(SELF.grid_name,2)) NE 'AIRMISR' THEN SELF.granule_id_string	= get_local_granule_id( SELF.source_file )
	
	SELF->Set_Default_Transform
;print,'MISR_IMAGE_DATA::INIT, success =',success

	CASE 1 OF
	;For now, do not use the grid name in the data description
;;;ckt,jul2004		grid_name NE '' AND field_name NE '': desc = grid_name+'::'+field_name
		grid_name NE '' AND field_name NE '': desc = field_name
;;;ckt,jul2004		grid_name NE '' AND field_name EQ '': desc = grid_name
		grid_name EQ '' AND field_name NE '': desc = field_name
		ELSE: desc = ''
	ENDCASE
	
	SELF.data_description	= desc
print,'SELF.data_description=',SELF.data_description
	RETURN, success
END
; MISR_IMAGE_DATA::INIT

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ MISR_IMAGE_DATA::CLEANUP @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_IMAGE_DATA::CLEANUP
	SELF->IMAGE_DATA::CLEANUP
END
; MISR_IMAGE_DATA::CLEANUP

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@ MISR_IMAGE_DATA__DEFINE @@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO MISR_IMAGE_DATA__DEFINE

	MISR_IMAGE_DATA = {							$
		MISR_IMAGE_DATA,						$
			INHERITS IMAGE_DATA,					$
			granule_id_string		: '',			$
			grid_name			: '',			$
			field_name			: '' }
END
; MISR_IMAGE_DATA__DEFINE
