;=============================================================================
;+
; vicsetpar
;
; PURPOSE
;`
;  Add, delete, or change a keyword/value pair in a VICAR label.
;
;'
; CALLING SEQUENCE :
;
;       vicsetpar, label, keyword, value
;
;
; ARGUMENTS
;  INPUT : label - String giving the VICAR label.
;
;          keyword - String giving the keyword whose value is to be set.
;                    If the keyword is not found, it will be added.
;
;          value - Value for the given keyword.
;
;  OUTPUT : NONE
;
;
;
; KEYWORDS 
;  INPUT : delete - Delete the keyword/value pair from the label, if found.
;
;  OUTPUT : pos - Position at which the keyword was placed in the label.
;
;           status - If no errors occur, status will be zero, otherwise
;                    it will be a string giving an error message.
;
;
;
; RETURN : NONE
;
;
;
; KNOWN BUGS : NONE
;
;
;
; ORIGINAL AUTHOR : J. Spitale ; 5/96
;
; UPDATE HISTORY : 
;
;-
;=============================================================================

;===========================================================================
; vicsetpar
;
;===========================================================================
pro vicsetpar, label, keyword, value, status=status, pos=pos, delete=delete

 status=0


;-----------------build keyval string-----------------------

 if(keyword_set(delete)) then keyval='' $
 else $
  begin
   s=size(value)
   typecode=s(s(0)+1)

   if(typecode EQ 7) then value_s="'" + value + "'" $

   else if((typecode GT 0) AND (typecode LT 6)) then $
                                                   value_s=strtrim(value, 2) $

   else $
    begin
     status='Unsupported type.'
     return
    end


   keyval=keyword + "=" + value_s
  end


;---------------check for keyword already in label-----------------

 current_value_s=vicgetpar(label, keyword, pos=pos, status=stat, /string)


;-----------if it is not found, then it will be appended------------

 if(keyword_set(stat)) then $
  begin
   pos=strlen(label)
   if(pos NE 0) then head=label+' ' else head=''
   tail=''
  end $


;-------------otherwise it will replace the current value--------------

 else $
  begin
   len=strlen(keyword)+strlen(current_value_s)+2
   head=strmid(label, 0, pos)
   tail=strmid(label, pos+len, strlen(label)-pos-len)
  end



 label=head+keyval+' '+tail
end
;===========================================================================



