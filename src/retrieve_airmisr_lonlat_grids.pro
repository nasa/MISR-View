FUNCTION retrieve_airmisr_lonlat_grids, fn

	file_id		= EOS_GD_OPEN( fn, /READ )
	grid_id		= EOS_GD_ATTACH( file_id, 'AirMisr' )
	iStat		= EOS_GD_PROJINFO(				$
				grid_id,				$
				proj_code,				$
				zone_code,				$
				sphere_code,				$
				proj_parm )
	iStat		= EOS_GD_GRIDINFO(				$
				grid_id,				$
				x_size,					$
				y_size,					$
				ul_easting_northing,			$
				lr_easting_northing )
	iStat		= EOS_GD_DETACH( grid_id )
	iStat		= EOS_GD_CLOSE( file_id )
	
	out_grid_size_x	= x_size / 4
	out_grid_size_y	= y_size / 4
	
	easting_array	= CONGRID(					$
				[ [ ul_easting_northing[0], lr_easting_northing[0] ],	$
				  [ ul_easting_northing[0], lr_easting_northing[0] ] ],	$
				out_grid_size_x, out_grid_size_y, /INTERP, /MINUS_ONE )
				
	northing_array	= CONGRID(					$
				[ [ lr_easting_northing[1], lr_easting_northing[1] ],	$
				  [ ul_easting_northing[1], ul_easting_northing[1] ] ],	$
				out_grid_size_x, out_grid_size_y, /INTERP, /MINUS_ONE )
				
	str_zone	= 'T' + STRTRIM(STRING(ABS(zone_code)),2)
	IF zone_code LT 0 THEN str_zone = 'G' + STRTRIM(STRING(ABS(zone_code)),2)	
	
	RETURN, utm2ll( 23, easting_array, northing_array, str_zone )
END
; retrieve_airmisr_lonlat_grids
