;//////////////////////////////////////////////////////////////////////////
; GetDirectoryDivider
;//////////////////////////////////////////////////////////////////////////
FUNCTION GetDirectoryDivider
	CASE !VERSION.OS OF
		'sunos': divider = '/'
		'IRIX': divider = '/'
		'Win32': divider = '\'
		ELSE: divider = '/'
	ENDCASE
	
	RETURN, divider
END
; GetDirectoryDivider
