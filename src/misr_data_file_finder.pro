@unix_locatefile.pro

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ misr_orbit2julday @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION misr_orbit2julday, current_orbit, orbit_ref, jul_day_ref
	RETURN, FLOAT(jul_day_ref) +					$
	   ((16.0/233.0)*FLOAT(current_orbit-orbit_ref))
END
; misr_orbit2julday

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ misr_orbit2path @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;;
;;;
;;; DUPLICATE FUNCTION.  
;;; PLEASE USE CENTRALIZED FUNCTION IN FILE:  orbit2path.pro
;;;
;;; (7-21-99  NOTE:  EQUATION HAS CHANGED, THIS COPY NO LONGER CORRECT.)
;;;
;;;
FUNCTION misr_orbit2path, current_orbit,orbit_ref,path_orbit_ref
MESSAGE=DIALOG_MESSAGE('YOU ARE ACCESSING AN OUT-OF-DATE ORBIT-TO-PATH FUNCTION.  SEE PROGRAMMER.')
	m	= (16L*(current_orbit-orbit_ref))+path_orbit_ref
	curPath	= m - (233L*FIX((m-1L)/233L))
	RETURN, curPath
END
; misr_orbit2path

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ misr_data_file_finder @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO misr_data_file_finder
	TRUE		= 1
	FALSE		= 0

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when 
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== misr_data_file_finder =========='
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


	orbit_ref	= 1L ;reference orbit number
	path_orbit_ref	= 1L ;path number corresponding to orbit_ref
	
;-------------------------------------------------------------------------------
;temporary until real number is known
;-------------------------------------------------------------------------------
jul_day_ref = FLOAT(JULDAY(4,29,1998))
;-------------------------------------------------------------------------------
;	jul_day_ref	= 0L ;Julian day for reference orbit
;-------------------------------------------------------------------------------

	current_orbit	= orbit_ref

	months		= [ 'Jan',					$
			    'Feb',					$
			    'Mar',					$
			    'Apr',					$
			    'May',					$
			    'Jun',					$
			    'Jul',					$
			    'Aug',					$
			    'Sep',					$
			    'Oct',					$
			    'Nov',					$
			    'Dec' ]
			    
	cameras		= ['DF','CF','BF','AF','AN','AA','BA','CA','DA']
	
	n_cameras	= 9

	;-----------------------------------------------------------------------
	; if the AGP file is not found, the orbit/path is tagged as 
	; "unavailable"
	;-----------------------------------------------------------------------

	;-----------------------------------------------------------------------
	; for now, the only array elements that are to be varied are
	; product name, path, orbit, and camera.
	;-----------------------------------------------------------------------

	;-----------------------------------------------------------------------
	; obtain current date/time
	;-----------------------------------------------------------------------
	tmpTime		= SYSTIME()
	timeSep		= STR_SEP(STRCOMPRESS(tmpTime),' ')
	monthNum	= LONG(TOTAL(WHERE(timeSep[1] EQ months)))+1L
	dayNum		= LONG(timeSep[2])
	yrNum		= LONG(timeSep[4])
	daySep		= STR_SEP(timeSep[3],':')
	secNum		= FLOAT((LONG(daySep[0])*3600L)+		$
				(LONG(daySep[1])*60L)+			$
				(LONG(daySep[2])))/(3600.0*24.0)

	now_date_time	= FLOAT(JULDAY(monthNum, dayNum, yrNum)) + secNum
	current_orbit_date_time =					$
		misr_orbit2julday(current_orbit,orbit_ref,jul_day_ref)

	msg	= 'Select name and location of output file'
	outfile	= DIALOG_PICKFILE(TITLE = msg)
	OPENW, outlun, outfile, /GET_LUN

	msg	=							$
	   'Select/enter fully-qualified top directory to begin search'
	baseDir	= DIALOG_PICKFILE(TITLE=msg,GET_PATH=path)
	baseDir	= path
	
	;-----------------------------------------------------------------------
	;loop through all paths
	;-----------------------------------------------------------------------
	WHILE current_orbit_date_time LE now_date_time DO BEGIN
		PRINT,' > current_orbit = ',current_orbit
		;---------------------------------------------------------------
		;get path associated with current orbit
		;---------------------------------------------------------------
		current_path	=					$
		   orbit2path(current_orbit,orbit_ref,path_orbit_ref)
;;;JRH 7-21-99	   misr_orbit2path(current_orbit,orbit_ref,path_orbit_ref)
		path2str	= STRTRIM(STRING(current_path),2)
		path_str2use	=					$
		   'P'+STRMID('00'+path2str,STRLEN(path2str)-1,3)
		orbit2str	= STRTRIM(STRING(current_orbit),2)
		orbit_str2use	= 'O'+					$
		   STRMID('00000'+orbit2str,STRLEN(orbit2str)-1,6)
		PRINT,' > current_path = ',current_path

		;---------------------------------------------------------------
		;create filename string to look for
		;---------------------------------------------------------------
		agpFound	= FALSE
		agp2check = 'MISR_AM1_' +				$
			    'AGP_' +					$
			    'GM_' +					$
			    path_str2use + '_' + 			$
		;---------------------------------------------------------------
		; don't think this is part of agp file name
		; orbit should not matter to agp file
		;---------------------------------------------------------------
;					orbit_str2use + '_' + 		$
;					STRTRIM(cameras[j],2) + '_' +	$
		;---------------------------------------------------------------
			    '01' +					$
			    '.hdf'
		
		returnVal	= unix_locatefile(baseDir,agp2check,cnt)
		
		IF cnt GT 0 AND STRLEN(returnVal) GT 0 THEN BEGIN
		   PRINT,'     <agp file found>'
		   PRINTF, outlun, STRTRIM(returnVal,2)
		ENDIF
		
		endLoopVar	= (cnt GT 0)*n_cameras-1
		
		FOR j = 0L, endLoopVar DO BEGIN
		   ;-----------------------------------------------------
		   ; GRP-TERRAIN
		   ;-----------------------------------------------------
		   terrain_proj2check	= 'MISR_AM1_' +			$
					  'GRP_TERRAIN_' +		$
					  'GM_' +			$
					   path_str2use + '_' + 	$
					   orbit_str2use + '_' + 	$
					   STRTRIM(cameras[j],2) + '_'+	$
					   '01' +			$
					   '.hdf'
		   returnVal	=					$
		      unix_locatefile(baseDir,terrain_proj2check,cnt)
		   IF cnt GT 0 AND STRLEN(returnVal) GT 0 THEN BEGIN
		      PRINT,'     <terrain file found>'
		      PRINTF, outlun, STRTRIM(returnVal,2)
		   ENDIF

		   ;-----------------------------------------------------
		   ; GRP-ELLIPSOID
		   ;-----------------------------------------------------
		   ellipsoid_proj2check= 'MISR_AM1_' +			$
					 'GRP_ELLIPSOID_' +		$
					 'GM_' +			$
					 path_str2use + '_' + 		$
					 orbit_str2use + '_' + 		$
					 STRTRIM(cameras[j],2)+'_' +	$
					 '01' +				$
					 '.hdf'
					 
		   returnVal	=					$
		      unix_locatefile(baseDir,ellipsoid_proj2check,cnt)
		   IF cnt GT 0 AND STRLEN(returnVal) GT 0 THEN BEGIN
		      PRINT,'     <ellipsoid file found>'
		      PRINTF, outlun, STRTRIM(returnVal,2)
		   ENDIF
		   
		   ;-----------------------------------------------------
		   ; AEROSOL
		   ;-----------------------------------------------------
		   aerosol2check = 'MISR_AM1_' +			$
					 'AS_AEROSOL_' +		$
					 path_str2use + '_' + 		$
					 orbit_str2use + '_' + 		$
					 STRTRIM(cameras[j],2)+'_' +	$
					 '01' +				$
					 '.hdf'
					 
		   returnVal	=					$
		      unix_locatefile(baseDir,aerosol2check,cnt)
		   IF cnt GT 0 AND STRLEN(returnVal) GT 0 THEN BEGIN
		      PRINT,'     <aerosol file found>'
		      PRINTF, outlun, STRTRIM(returnVal,2)
		   ENDIF
		   
		   ;-----------------------------------------------------
		   ; CLOUDS
		   ;-----------------------------------------------------
		   aerosol2check = 'MISR_AM1_' +			$
					 'TC_ALBEDO_' +		$
					 path_str2use + '_' + 		$
					 orbit_str2use + '_' + 		$
					 STRTRIM(cameras[j],2)+'_' +	$
					 '01' +				$
					 '.hdf'
					 
		   returnVal	=					$
		      unix_locatefile(baseDir,aerosol2check,cnt)
		   IF cnt GT 0 AND STRLEN(returnVal) GT 0 THEN BEGIN
		      PRINT,'     <cloud file found>'
		      PRINTF, outlun, STRTRIM(returnVal,2)
		   ENDIF
		ENDFOR

		;---------------------------------------------------------------
		;get next orbit's starting GMT date/time
		;---------------------------------------------------------------
		current_orbit		= current_orbit + 1L
		current_orbit_date_time =				$
		   misr_orbit2julday(current_orbit,orbit_ref,jul_day_ref)
	ENDWHILE

	CLOSE, outlun
END
; misr_data_file_finder
