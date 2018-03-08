;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ convert_number @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION convert_number, in_val, to_data_type
	
	CASE to_data_type OF
		1: one_type = BYTE(in_val)
		2: one_type = FIX(in_val)
		3: one_type = LONG(in_val)
		4: one_type = FLOAT(in_val)
		5: one_type = DOUBLE(in_val)
		12: one_type = UINT(in_val)
		13: one_type = ULONG(in_val)
		14: one_type = LONG64(in_val)
		15: one_type = ULONG64(in_val)
		ELSE: one_type = BYTE(in_val)
	ENDCASE
	
	RETURN, one_type
END
; convert_number
