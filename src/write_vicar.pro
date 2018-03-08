;=============================================================================
;+
; write_vicar
;
; PURPOSE
;`
;  Writes a vicar data file.
;
;'
; CALLING SEQUENCE :
;
;       write_vicar, filename, data, label
;
;
; ARGUMENTS
;  INPUT : filename - String giving the name of the file to be written.
;
;          data - Data to be written.
;
;          label - String giving the vicar label.  System items
;                  will be added or changed as appropriate.
;
;  OUTPUT : NONE
;
;
;
; KEYWORDS 
;  INPUT : silent - If set, no messages are printed.
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
; RETURN : NONE
;
;
; RESTRICTIONS : This program only writes band-sequential data with
;                no binary header or prefixes.
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
; write_vicar
;
;===========================================================================
pro write_vicar, filename, data, label, status=status, $
   silent=silent, swap=swap, show=show, flip=flip

 status=0
 file_data=data

 if(NOT keyword_set(label)) then label=''


;----------------------determine image dimensions------------------------

 s=size(data)
 n_dim=s(0)
 n_samples=s(1)
 if(n_dim GT 1) then n_lines=s(2) else n_lines=1
 if(n_dim EQ 3) then n_bands=s(3) else n_bands=1

 if(n_dim GT 3) then $
  begin
   status='Number of dimensions must be 3 or fewer.'
   if(NOT keyword_set(silent)) then message, status
   return
  end


;----------------------determine format------------------------

 typecode=s(n_dim+1)

 case typecode of
  1: begin
      format='BYTE'
      elm_size=1
     end
  2: begin
      format='HALF'
      elm_size=2
     end
  3: begin
      format='FULL'
      elm_size=4
     end
  4: begin
      format='REAL'
      elm_size=4
     end
  5: begin
      format='DOUB'
      elm_size=8
     end

  else : $
	begin
	 status='Unsupported format.'
	 if(NOT keyword_set(silent)) then message, status
	 return
	end
 endcase

 recsize=n_samples*elm_size


;----------------------determine host------------------------

 host=gethost(status=status)
 if(keyword_set(status)) then $
  begin
   if(NOT keyword_set(silent)) then message, status
   return
  end


;--------------determine integer and real formats-------------

 if((host EQ 'VAX-VMS')		$
 or (host EQ 'DECSTATN')	$
 or (host EQ 'X86-LINUX')	$
 or (host EQ 'X86-NT'))		$
 then intfmt='LOW'		$
 else intfmt='HIGH'

 if(host EQ 'VAX-VMS') then realfmt='VAX' $
 else if(host EQ 'DECSTATN') or (host EQ 'X86-LINUX') or (host EQ 'X86-NT') then realfmt='RIEEE' $
 else realfmt='IEEE'


;--------------set the system label items---------------

 vicsetpar, label, 'LBLSIZE', 100000		; Make sure that LBLSIZE is the
						; first keyword if this is a new
						; label.
 vicsetpar, label, 'FORMAT', format
 vicsetpar, label, 'TYPE', '', /delete
 vicsetpar, label, 'BUFSIZ', recsize
 vicsetpar, label, 'DIM', 3
 vicsetpar, label, 'EOL', '', /delete
 vicsetpar, label, 'RECSIZE', recsize
 vicsetpar, label, 'ORG', 'BSQ'
 vicsetpar, label, 'NL', n_lines
 vicsetpar, label, 'NS', n_samples
 vicsetpar, label, 'NB', n_bands
 vicsetpar, label, 'N1', n_samples
 vicsetpar, label, 'N2', n_lines
 vicsetpar, label, 'N3', n_bands
 vicsetpar, label, 'N4', 0
 vicsetpar, label, 'NBB', 0
 vicsetpar, label, 'NLB', 0
 vicsetpar, label, 'HOST', host
 vicsetpar, label, 'INTFMT', intfmt
 vicsetpar, label, 'REALFMT', realfmt
 vicsetpar, label, 'BHOST', host
 vicsetpar, label, 'BINTFMT', intfmt
 vicsetpar, label, 'BREALFMT', realfmt
 vicsetpar, label, 'BLTYPE', ''

 label_nbytes= $				; Align label with record
       (fix((strlen(label)+10)/recsize)+1) $	; boundary.  The '+10' is to
                                      * recsize	; ensure that this size will
						; remain appropriate after
						; LBLSIZE is modified.
 vicsetpar, label, 'LBLSIZE', label_nbytes

 for i=strlen(label), label_nbytes-2 do $	; Pad the label out to the
                              label=label + ' '	; record boundary.


;----------------open file------------------

 openw, unit, filename, /get_lun, error=error
 if(error NE 0) then $
  begin
   status=!err_string
   if(NOT keyword_set(silent)) then message, status
   return
  end


;-------------------write label----------------------

 printf, unit, label


;----------take care of any necessary byte-swapping-----------

 if(keyword_set(swap)) then $
  begin
   byteorder, file_data
   if(NOT keyword_set(silent)) then print, 'Byte swapping has been performed.'
  end


;----------------flip if necessary---------------

 if(keyword_set(flip)) then file_data=rotate(file_data, 7)


;------------------write data---------------

 writeu, unit, data


;------------------clean up-------------------

 close, unit
 free_lun, unit


end
;===========================================================================
