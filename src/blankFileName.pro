;+
;
; FUCTION blankFileName, filename
;
; For use following call to DIALOG_PICKFILE().
;
; Return 1 if filename is blank.
; Return 0 if filename is not blank.
; This accounts for the filename having (or not having) a full 
; 	directory path attached to it.
; This does not account for the validity of the filename.
;
; written by Jeffrey.R.Hall@jpl.nasa.gov 9/99 for MISR.
;
;-
@GetDirectoryDivider.pro
FUNCTION blankFileName, filename
	IF STRLEN( STRTRIM( filename, 2 ) ) EQ 0 THEN BEGIN
		RETURN, 1
	ENDIF ELSE BEGIN
		IF STRMID( filename, STRLEN( filename ) - 1, 1) EQ GetDirectoryDivider() THEN BEGIN
			RETURN, 1
		ENDIF ELSE BEGIN
			RETURN, 0
		ENDELSE
	ENDELSE
END
; blankFileName
