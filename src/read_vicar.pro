;=============================================================================
;+
; read_vicar
;
; PURPOSE
;`
;  Reads a vicar data file.
;
;'
; CALLING SEQUENCE :
;
;       data=read_vicar(filename, label)
;
;
; ARGUMENTS
;  INPUT : filename - String giving the name of the file to be read.
;
;  OUTPUT : label - Named variable in which the vicar label will be returned.
;
;
;
; KEYWORDS 
;  INPUT : n_l - If set, this will override the number of lines given by the 
;                'NL' field of the label.
;
;          n_s - If set, this will override the number of samples given by the 
;                'NS' field of the label.
;
;          n_b - If set, this will override the number of bands given by the 
;                'NB' field of the label.
;
;          nlb - If set, this will override the number of binary header records
;                given by the 'NLB' field of the label.
;
;          nbb - If set, this will override the number of binary prefix bytes
;                given by the 'NBB' field of the label.
;
;          silent - If set, no messages are printed.
;
;          default_format - Data format to use if not given in the label.
;                           choices are 'BYTE', 'HALF', 'FULL", 'REAL', and
;                           'DOUB'.  default is 'BYTE'.
;
;          swap - If set, the data array will be byte-swapped.
;
;          flip - If set, the data array will be subjected to a rotate(data, 7),
;                 i.e., if its an image, it will be flipped vertically.
;
;  OUTPUT : status - If no errors occur, status will be zero, otherwise
;                    it will be a string giving an error message.
;
;
;
; RETURN : The data array read from the file.
;
;
; RESTRICTIONS : This program only works with band-sequential data and
;                does not recognize EOF labels.
;
;
;
; KNOWN BUGS : NONE
;
;
;
; ORIGINAL AUTHOR : J. Spitale ; 10/95
;
; UPDATE HISTORY : 
;
;-
;=============================================================================

;===========================================================================
; rvc_pl_suffix
;
;===========================================================================
function rvc_pl_suffix, n
 if(n NE 1) then return, 's'
 return, ''
end
;===========================================================================



;===========================================================================
; rvc_strip_quotes
;
;===========================================================================
function rvc_strip_quotes, s

 t=strtrim(s,2)
 
 if(strmid(t, 0, 1) EQ "'") then t=strmid(t, 1, strlen(t)-1)
 if(strmid(t, strlen(t)-1, 1) EQ "'") then t=strmid(t, 0, strlen(t)-1)

 return, t
end
;===========================================================================



;===========================================================================
; read_vicar
;
;===========================================================================
function read_vicar, filename, label, status=status, $
   silent=silent, default_format=default_format, swap=swap, $
   n_l=n_l, n_s=n_s, n_b=n_b, nlb=nlb, nbb=nbb, show=show, flip=flip

 if(n_elements(n_l) NE 0) then nl=n_l
 if(n_elements(n_s) NE 0) then ns=n_s
 if(n_elements(n_b) NE 0) then nb=n_b

 if(NOT keyword_set(default_format)) then default_format='BYTE'

 status=0

;----------------open file------------------

 openr, unit, filename, /get_lun, error=error
 if(error NE 0) then $
  begin
   status=!err_string
   if(NOT keyword_set(silent)) then message, status
   return, 0
  end


;---------------read label size----------------

 records=assoc(unit, bytarr(30, /nozero))
 record=records(0)
 str=string(record)

 label_nbytes=vicgetpar(str, 'LBLSIZE', status=status)
 if(keyword_set(status)) then $
  begin
   if(NOT keyword_set(silent)) then message, status
   return, 0
  end


;-----------------get label-------------------

 label_records=assoc(unit, bytarr(label_nbytes, /nozero))
 label=string(label_records(0))


;------------get image dimensions-------------

 nl_note=''
 if(n_elements(nl) EQ 0) then $
  begin
   nl=fix(vicgetpar(label, 'NL', status=status))
   if(keyword_set(status)) then $
    begin
     if(NOT keyword_set(silent)) then message, status
     return, 0
    end
  end else nl_note=' (forced)'

 ns_note=''
 if(n_elements(ns) EQ 0) then $
  begin
   ns=fix(vicgetpar(label, 'NS', status=status))
   if(keyword_set(status)) then $
    begin
     if(NOT keyword_set(silent)) then message, status
     return, 0
    end
  end else ns_note=' (forced)'

 nb_note=''
 if(n_elements(nb) EQ 0) then $
  begin
   nb=fix(vicgetpar(label, 'NB', status=status))
   if(keyword_set(status)) then $
    begin
     nb=1
     nb_note=' (by default)'
    end
  end else nb_note=' (forced)'

 nlb_note=''
 if(n_elements(nlb) EQ 0) then $
  begin
   nlb=fix(vicgetpar(label, 'NLB', status=status))
   if(keyword_set(status)) then nlb_note=' (by default)'
  end else nlb_note=' (forced)'

 nbb_note=''
 if(n_elements(nbb) EQ 0) then $
  begin
   nbb=fix(vicgetpar(label, 'NBB', status=status))
   if(keyword_set(status)) then $
    begin
     nbb=200
     nbb_note=' (by default)'
    end
  end else nbb_note=' (forced)'


 if(NOT keyword_set(silent)) then $
  begin
   print,'NL = '+strtrim(nl,2)+' line'+rvc_pl_suffix(nl)+nl_note
   print,'NS = '+strtrim(ns,2)+' sample'+rvc_pl_suffix(ns)+ns_note
   print,'NB = '+strtrim(nb,2)+' band'+rvc_pl_suffix(nb)+nb_note
   print,'NLB= '+strtrim(nlb,2)+$
                           ' binary header record'+rvc_pl_suffix(nlb)+nlb_note
   print,'NBB= '+strtrim(nbb,2)+$
                             ' binary prefix byte'+rvc_pl_suffix(nbb)+nbb_note
  end


;-----------------get data format------------------

 format_note=''
 format=vicgetpar(label, 'FORMAT', status=status)
 if(keyword_set(status)) then $
  begin
   format_note=' (by default)'
   if(keyword_set(default_format)) then format=default_format $
   else $
    begin
     if(NOT keyword_set(silent)) then message, status
     return, 0
    end
  end


;------set up arrays for the appropriate data format-------

 format=rvc_strip_quotes(strupcase(format))

 case format of
  'BYTE' : begin
            line=bytarr(ns, /nozero)
            image=bytarr(ns, nl, nb, /nozero)
            elm_size=1
           end

  'HALF' :  begin
            line=intarr(ns, /nozero)
            image=intarr(ns, nl, nb, /nozero)
            elm_size=2
           end

  'FULL' :  begin
            line=lonarr(ns, /nozero)
            image=lonarr(ns, nl, nb, /nozero)
            elm_size=4
           end

  'REAL' :  begin
            line=fltarr(ns, /nozero)
            image=fltarr(ns, nl, nb, /nozero)
            elm_size=4
           end

  'DOUB' :  begin
            line=dblarr(ns, /nozero)
            image=dblarr(ns, nl, nb, /nozero)
            elm_size=8
           end

  else : $
    begin
     status='Unrecognized data format : '+format+'.'
     if(NOT keyword_set(silent)) then message, status
     return, 0
    end
 endcase

 if(NOT keyword_set(silent)) then $
                               print, 'Data format is '+format+'.'+format_note


;-----------------read the image---------------------

 if(nbb NE 0) then binary_prefix=bytarr(nbb)

 if(nlb NE 0) then $
  begin
   binary_header=bytarr(((ns*elm_size)+nbb)*nlb)
   readu, unit, binary_header 
  end

 for j=0, nb-1 do $
  for i=0, nl-1 do $
   begin
    if(nbb NE 0) then readu, unit, binary_prefix
    readu, unit, line
    image(*,i,j)=line
   end


;----------take care of any necessary byte-swapping-----------

 if(keyword_set(swap)) then $
  begin
   byteorder, image
   if(NOT keyword_set(silent)) then $
                                    print, 'Byte swapping has been performed.'
  end


;----------------flip if necessary---------------

 if(keyword_set(flip)) then image=rotate(image, 7)


;------------------clean up-------------------

 close, unit
 free_lun, unit


 return, image
end
;===========================================================================
