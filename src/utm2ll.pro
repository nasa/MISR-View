FUNCTION utm2ll,						$
		ref_ellipsoid,					$
		utm_easting,					$
		utm_northing,					$
		utm_zone	; UTM zone designators followed by
				; zone number (e.g., L24, M60...)
				; letters go from C through X, 
				; numbers go from 1 through 60
		
	ellipsoid_info	= {					$
			id: [  -1,				$
			        1,				$
			        2,				$
			        3,				$
			        4,				$
			        5,				$
			        6,				$
			        7,				$
			        8,				$
			        9,				$
			       10,				$
			       11,				$
			       12,				$
			       13,				$
			       14,				$
			       15,				$
			       16,				$
			       17,				$
			       18,				$
			       19,				$
			       20,				$
			       21,				$
			       22,				$
			       23 ],				$
			name: [ 'placeholder',			$
			        'Airy',				$
			        'Australian National',		$
			        'Bessel 1841',			$
			        'Bessel 1841 (Nambia)',		$
			        'Clarke 1866',			$
			        'Clarke 1880',			$
			        'Everest',			$
			        'Fischer 1960 (Mercury)',	$
			        'Fischer 1968',			$
			        'GRS 1967',			$
			        'GRS 1980',			$
			        'Helmert 1906',			$
			        'Hough',			$
			        'International',		$
			        'Krassovsky',			$
			        'Modified Airy',		$
			        'Modified Everest',		$
			        'Modified Fischer 1960',	$
			        'South American 1969',		$
			        'WGS 60',			$
			        'WGS 66',			$
			        'WGS-72',			$
			        'WGS-84' ],			$
			radius: [       0L,			$
				  6377563L,			$
				  6378160L,			$
				  6377397L,			$
				  6377484L,			$
				  6378206L,			$
				  6378249L,			$
				  6377276L,			$
				  6378166L,			$
				  6378150L,			$
				  6378160L,			$
				  6378137L,			$
				  6378200L,			$
				  6378270L,			$
				  6378388L,			$
				  6378245L,			$
				  6377340L,			$
				  6377304L,			$
				  6378155L,			$
				  6378160L,			$
				  6378165L,			$
				  6378145L,			$
				  6378135L,			$
				  6378137L ],			$
			square_of_eccentricity :		$
				 [ 0.00000000D,			$
				   0.00667054D,			$
				   0.006694542D,		$
				   0.006674372D,		$
				   0.006674372D,		$
				   0.006768658D,		$
				   0.006803511D,		$
				   0.006637847D,		$
				   0.006693422D,		$
				   0.006693422D,		$
				   0.006694605D,		$
				   0.00669438D,			$
				   0.006693422D,		$
				   0.00672267D,			$
				   0.00672267D,			$
				   0.006693422D,		$
				   0.00667054D,			$
				   0.006637847D,		$
				   0.006693422D,		$
				   0.006694542D,		$
				   0.006693422D,		$
				   0.006694542D,		$
				   0.006694318D,		$
				   0.00669438D ] }
				   
				   
				   
	PI		= 3.14159265D
	FOURTHPI	= PI / 4.0D
	deg2rad		= PI / 180.0D
	rad2deg		= 180.0D / PI

	k0		= 0.9996D
	
	a		= ellipsoid_info.radius[ ref_ellipsoid ]
	eccSquared	= ellipsoid_info.square_of_eccentricity[ ref_ellipsoid ]

	e1		= ( 1.0D - SQRT( 1.0D - eccSquared ) ) /	$
			  ( 1.0D + SQRT( 1.0D - eccSquared ) )

	x		= utm_easting - 500000.0 ;remove 500,000 meter offset for longitude
	y		= utm_northing

	ZoneNumber	= FIX( STRMID( STRTRIM(utm_zone,2), 1, 2 ) )
	ZoneDesignator	= (BYTE(STRUPCASE(STRMID( STRTRIM(utm_zone,2), 0, 1 ))))[0]
	Zone_N		= (BYTE( 'N' ))[0]
	
	IF LONG(ZoneDesignator) - LONG(Zone_N) GE 0 THEN BEGIN
		NorthernHemisphere	= 1			;point is in northern hemisphere
	ENDIF ELSE BEGIN
		NorthernHemisphere	= 0			;point is in southern hemisphere
		y			= y - 10000000.0	;remove 10,000,000 meter offset used for southern hemisphere
	ENDELSE
print,'NorthernHemisphere=',NorthernHemisphere

	LongOrigin	= (ZoneNumber - 1)*6 - 180 + 3  ;+3 puts origin in middle of zone

	eccPrimeSquared	= ( eccSquared ) / ( 1.0D - eccSquared )
	M		= y / k0
	mu		= M/(a*((1.0D)-eccSquared/(4.0D)-(3.0D)*eccSquared*eccSquared/(64.0D)-(5.0D)*eccSquared*eccSquared*eccSquared/(256.0D)))


	phi1Rad		= mu    + ((3.0D)*e1/(2.0D)-(27.0D)*e1*e1*e1/(32.0D))*SIN((2.0D)*mu)		$
				+ ((21.0D)*e1*e1/(16.0D)-(55.0D)*e1*e1*e1*e1/(32.0D))*SIN((4.0D)*mu)	$
				+((151.0D)*e1*e1*e1/(96.0D))*SIN((6.0D)*mu)
	phi1		= phi1Rad*rad2deg

	N1		= a/SQRT(1-eccSquared*SIN(phi1Rad)*SIN(phi1Rad))
	T1		= TAN(phi1Rad)*TAN(phi1Rad)
	C1		= eccPrimeSquared*COS(phi1Rad)*COS(phi1Rad)
	
	R1		= a*((1.0D)-eccSquared)/(((1.0D)-eccSquared*SIN(phi1Rad)*SIN(phi1Rad))^(1.5D))
	
	D		= x/(N1*k0)
	Lat		= phi1Rad -											$
				(N1*TAN(phi1Rad)/R1)*(D*D/(2.0D)-((5.0D)+(3.0D)*T1+(10.0D)*C1-(4.0D)*C1*C1-(9.0D)*eccPrimeSquared)*D*D*D*D/(24.0D)		$
					+((61.0D)+(90.0D)*T1+(298.0D)*C1+(45.0D)*T1*T1-(252.0D)*eccPrimeSquared-(3.0D)*C1*C1)*D*D*D*D*D*D/(720.0D))
	Lat		= Lat * rad2deg

	Long		= (D-((1.0D)+(2.0D)*T1+C1)*D*D*D/(6.0D)+((5.0D)-(2.0D)*C1+(28.0D)*T1-(3.0D)*C1*C1+(8.0D)*eccPrimeSquared+(24.0D)*T1*T1)			$
					*D*D*D*D*D/(120.0D))/COS(phi1Rad)
	Long		= LongOrigin + Long * rad2deg
	
	RETURN, { latitude:Lat, longitude:Long }
END
; utm2ll
