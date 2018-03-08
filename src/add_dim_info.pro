;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ add_dim_info @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION add_dim_info,dim_name, dim_size, in_ptr
;   extra_dim_struct = { name         :'',       $
;                        ptr     :PTR_NEW(/ALLOCATE_HEAP) }

   IF NOT PTR_VALID(in_ptr) THEN BEGIN
;      struct_arr = REPLICATE( { name : '', ptr :PTR_NEW() }, dim_size )
struct_ptrarr = PTRARR(dim_size)
      FOR j = 0, dim_size - 1 DO BEGIN
;         struct_arr[j].name = STRTRIM(dim_name,2) + ' ' + STRTRIM(STRING(j+1),2)
struct_ptrarr[j] = PTR_NEW(PTRARR(2)) 
(*(struct_ptrarr[j]))[0] = PTR_NEW(dim_name)
      ENDFOR
;      in_ptr = PTR_NEW(struct_arr,/NO_COPY)
      in_ptr = PTR_NEW(struct_ptrarr)
  ENDIF ELSE BEGIN
      FOR i = 0, N_ELEMENTS(*in_ptr) - 1 DO BEGIN
;         tmpPtr = ((*in_ptr)[i]).ptr
tmpPtr = (*((*in_ptr)[i]))[1]

         tmpPtr = add_dim_info(dim_name, dim_size, tmpPtr)
;print,'>>>>>>>>>>>>>>> ptr_valid(tmpPtr)',ptr_valid(tmpPtr)
(*((*in_ptr)[i]))[1] = tmpPtr
;         (((*in_ptr)[i]).ptr) = tmpPtr
;print,'>>>>>>>>>>>>>>> ptr_valid((((*in_ptr)[i]).ptr))',ptr_valid((((*in_ptr)[i]).ptr))
;print,'>>>>>>>>>>>>>>> ptr_valid((*((*in_ptr)[i]))[1])',ptr_valid((*((*in_ptr)[i]))[1])
      ENDFOR
   ENDELSE

   RETURN, in_ptr
END
; add_dim_info

