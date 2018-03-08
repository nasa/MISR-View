@utm2ll.pro
FUNCTION retrieve_airmisr_latlon_info, fn
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
	
	str_zone	= 'T' + STRTRIM(STRING(ABS(zone_code)),2)
	IF zone_code LT 0 THEN str_zone = 'G' + STRTRIM(STRING(ABS(zone_code)),2)
		
	ul_lonlat		= utm2ll( 23, ul_easting_northing[0], ul_easting_northing[1], str_zone )
	upper_left_latlon	= [ ul_lonlat.latitude, ul_lonlat.longitude ]
	
	ur_lonlat		= utm2ll( 23, lr_easting_northing[0], ul_easting_northing[1], str_zone )
	upper_right_latlon	= [ ur_lonlat.latitude, ur_lonlat.longitude ]
	
	ll_lonlat		= utm2ll( 23, ul_easting_northing[0], lr_easting_northing[1], str_zone )
	lower_left_latlon	= [ ll_lonlat.latitude, ll_lonlat.longitude ]
	
	lr_lonlat		= utm2ll( 23, lr_easting_northing[0], lr_easting_northing[1], str_zone )
	lower_right_latlon	= [ lr_lonlat.latitude, lr_lonlat.longitude ]
	
	center_easting		= (lr_easting_northing[0] + ul_easting_northing[0]) / 2.0
	center_northing		= (lr_easting_northing[1] + ul_easting_northing[1]) / 2.0

	center_lonlat		= utm2ll( 23, center_easting, center_northing, str_zone )
	center_latlon		= [ center_lonlat.latitude, center_lonlat.longitude ]

	RETURN, {								$
			upper_left_latlon	:upper_left_latlon,		$
			upper_right_latlon	:upper_right_latlon,		$
			lower_right_latlon	:lower_right_latlon,		$
			lower_left_latlon	:lower_left_latlon,		$
			center_latlon		:center_latlon }
END
; retrieve_airmisr_latlon_info
