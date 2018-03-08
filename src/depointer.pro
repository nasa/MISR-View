;+
; depointer.pro
;
; This fuction evaluates the reference given, and 
; frees ALL pointers and destroys ALL objects thoughout 
; any nested arrangement of structures and pointers 
; that are referred to by the reference.
;
; (Objects are responsible for cleaning themselves up, 
; they may call this routine in their own cleanup routine.)
;
; Returns:
;	0 if zero valid pointers or objects were found.
;	1 if any pointers were freed or objects destroyed.
;
; Written for the AIRS project, Jan 1999 by:
;	Jeffrey.R.Hall@jpl.nasa.gov
;	Charles.K.Thompson@jpl.nasa.gov
;-

function depointer, reference, NO_OBJ = no_obj

;; size(reference) returns:
;; Type Code	Data Type	
;; 0		Undefined	
;; 1		Byte	
;; 2		Integer	
;; 3		Longword integer	
;; 4		Floating point	
;; 5		Double-precision floating	
;; 6		Complex floating	
;; 7		String	
;; 8		Structure	
;; 9		Double-precision complex	
;; 10		Pointer	
;; 11		Object reference

	kill_objects = 1
;print,'DEPOINTER:  SIZE( no_obj, /TYPE ) = ',SIZE( no_obj, /TYPE )
	IF SIZE( no_obj, /TYPE ) GT 0 THEN BEGIN
;print,'DEPOINTER:  no_obj = ',no_obj
		IF no_obj THEN BEGIN
			kill_objects = 0
		ENDIF
	ENDIF
;print,'DEPOINTER:  kill_objects = ',kill_objects

success = 0

;print,'size( reference, /type ) = ',size( reference, /type )

case size( reference, /type ) of

	;STRUCTURE
	8 : begin
		ntags = n_tags( reference )
		tnames = tag_names( reference )
;print, 'DEPOINTER:  Recursing on a STRUCTURE with ntags = ', ntags
		for tag = 0, ntags - 1 do begin
;print, 'DEPOINTER:     Tag Name = ', tnames[tag]
			success = depointer( reference.( tag ), NO_OBJ = kill_objects )
;print, 'DEPOINTER:     (STRUCTURE)   Result = ', success
		endfor
	end

	;POINTER
	10 : begin

		; Account for pointer arrays.
		valid_ptrarr_indicies = where( ptr_valid( reference[*] ) )
;print,'DEPOINTER:  n_elements( valid_ptrarr_indicies ) = ', n_elements( valid_ptrarr_indicies )
;print,'DEPOINTER:  list of valid_ptrarr_indicies = ', valid_ptrarr_indicies
		if valid_ptrarr_indicies[0] ne -1 then begin
			for i = 0, n_elements( valid_ptrarr_indicies ) - 1 do begin
				if (where( size( *(reference[valid_ptrarr_indicies[i]]), /type ) eq [8,10,11] ))[0] ne -1 then begin
;print, 'DEPOINTER:  Recursing on a POINTER'
					success = depointer( *(reference[valid_ptrarr_indicies[i]]), NO_OBJ = kill_objects )
;print, 'DEPOINTER:     (POINTER)   Result = ', success
				endif
				ptr_free, reference[valid_ptrarr_indicies[i]]
				success = 1
;print, 'DEPOINTER:  A POINTER WAS FREED modified copy of depointer'
			endfor
		endif
	end

	;OBJECT
	11 : begin

		IF kill_objects THEN BEGIN
			; Account for object arrays.
			valid_objarr_indicies = where( obj_valid( reference[*] ) )
;print,'DEPOINTER:  n_elements( valid_objarr_indicies ) = ', n_elements( valid_objarr_indicies )
;print,'DEPOINTER:  list of valid_objarr_indicies = ', valid_objarr_indicies
			if valid_objarr_indicies[0] ne -1 then begin
				for i = 0, n_elements( valid_objarr_indicies ) - 1 do begin
					obj_destroy, reference[valid_objarr_indicies[i]]
;print, 'DEPOINTER:  AN OBJECT WAS DESTROYED modified copy of depointer'
				endfor
			endif
		ENDIF
	end

	;OTHER
	else : begin
	end

endcase

return, success

end
