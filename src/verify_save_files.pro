FUNCTION verify_save_files, time_tag2check
	time_tag	= 2454078
	IF time_tag NE time_tag2check THEN RETURN, 0
	RETURN, 1
END
; verify_save_files
