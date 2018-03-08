pro etop05_to_25

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when 
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== etop05_to_25 =========='
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


	WorldElevation = intarr(4320,2160)
	openr, lun, '/data/vis/vesa/tasc/ETOP05/etop05.raw', /get_lun
	readu, lun, WorldElevation
	close, lun
	free_lun, lun
	; Convert to kilometers and change to real*4.
	elev = bytscl( congrid( WorldElevation , 864, 432 ), $
		min=-10000, max=20000 )
	ele = bytarr( 864, 432 )
	; Switch E-W hemispheres of elevation data to match DAO data.
	ele[0:431,*] = elev[432:863,*]
	ele[432:863,*] = elev[0:431,*]
	print,'elemin = ',min(ele)
	print,'elemax = ',max(ele)

	openw,lun,'etop25.raw',/get_lun
	writeu,lun,ele
	close,lun
	free_lun,lun

;help,ele,output=helpInfo
;print,helpInfo

end
