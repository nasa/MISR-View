;
;
;
; Modified Dec 26, 2001 (ckt)
; added functional check for scientific notation
;
FUNCTION is_valid_number, num_str
   valid_flag                 = 1

	routine_name	= '========== is_valid_number =========='
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		valid_flag	= 0
		RETURN, valid_flag
	ENDIF

   valid_flag                 = 1
   valid_strarr               = STRTRIM([ SINDGEN(10), '-', '.' ],2)
   num_str                    = STRTRIM(num_str,2)
   n_decimal_pts              = 0
   len                        = STRLEN(num_str)
   i                          = 0
   check_exp                  = 0
   
   exp_sep		      = STR_SEP( STRUPCASE(num_str), 'E' )
   IF N_ELEMENTS(exp_sep) GT 2 THEN valid_flag = 0
   
   IF N_ELEMENTS(exp_sep) EQ 2 THEN BEGIN
   	check_exp	= 1
   	num_str		= exp_sep[0]
   	num_str2	= exp_sep[1]
   ENDIF
   
   WHILE valid_flag AND i LT len DO BEGIN
      current_char = STRMID(num_str,i,1)
      idx          = WHERE(current_char EQ valid_strarr, cnt)
      CASE 1 OF
         cnt LE 0: valid_flag = 0
         ELSE: BEGIN
         
            IF current_char EQ '.' THEN n_decimal_pts = n_decimal_pts + 1
            
            IF n_decimal_pts GT 1 THEN valid_flag = 0
            
            IF current_char EQ '-' AND i GT 0 THEN valid_flag = 0
            
            END
      ENDCASE
      i = i + 1
   ENDWHILE
   
   IF valid_flag AND check_exp THEN BEGIN
   	pos_neg		= STRMID(num_str2, 0, 1)
   	tmp_num_str	= STRMID(num_str2, 1)
   	IF (pos_neg EQ '+' OR pos_neg EQ '-') AND tmp_num_str NE '' THEN BEGIN
   		valid_flag	= is_valid_number( tmp_num_str )
   	ENDIF ELSE BEGIN
   		valid_flag	= 0
   	ENDELSE
   ENDIF
   
   RETURN, valid_flag
END
