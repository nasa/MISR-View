;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ reverse_if_necessary @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION reverse_if_necessary, in_array
	IF STRMID(STRTRIM(!version.release,2),0,3) EQ '5.3'	$
	OR STRMID(STRTRIM(!version.release,2),0,3) EQ '5.4'	$
	THEN							$
		RETURN, REVERSE(in_array) ELSE RETURN, in_array
END
; reverse_if_necessary
