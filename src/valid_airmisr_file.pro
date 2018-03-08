FUNCTION valid_airmisr_file, fn
   
	;=======================================================================
	; basic error catch mechanism, generally for file read or write when 
	; permissions do not allow it.
	;=======================================================================
	routine_name	= '========== valid_airmisr_file =========='
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


   hdfeosID   = EOS_GD_OPEN( fn, /READ )
print,'valid_airmisr_file:  hdfeosID = ***',hdfeosID,'***'
   IF hdfeosID LT 0 THEN RETURN, 0
   
   gList      = ''
   strBufSz   = 0L
   
   nGrid      = EOS_GD_INQGRID( fn, gList )
print,'valid_airmisr_file:  nGrid = ***',nGrid,'***'
   IF nGrid LT 0 THEN RETURN, 0
   
   gList	= STR_SEP(TEMPORARY(gList),',')
   
   idx        = WHERE( gList EQ 'AirMisr', cnt )
print,'valid_airmisr_file:  cnt = ***',cnt,'***'
   IF cnt LT 1 THEN RETURN, 0
   
   gridID     = EOS_GD_ATTACH( hdfeosID, 'AirMisr')
   
   nField     = EOS_GD_INQFIELDS( gridID, fieldList, rank,numberType )
print,'valid_airmisr_file:  nField = ***',nField,'***'
   IF nField LT 0 THEN RETURN, 0

   iStat      = EOS_GD_DETACH( gridID )
print,'valid_airmisr_file:  iStat = ***',iStat,'***'
   IF iStat LT 0 THEN RETURN, 0
   
   iStat      = EOS_GD_CLOSE( hdfeosID )
print,'valid_airmisr_file:  iStat = ***',iStat,'***'
   IF iStat LT 0 THEN RETURN, 0

   RETURN, 1
END
