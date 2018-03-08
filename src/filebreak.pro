;-------------------------------------------------------------
;+
; NAME:
;       FILEBREAK
; PURPOSE:
;       Breaks file name(s) into components.
; CATEGORY:
; CALLING SEQUENCE:
;       filebreak, input
; INPUTS:
;       input = input file name (may be an array).  in
; KEYWORD PARAMETERS:
;       Keywords:
;         DIRECTORY=d  returned directory name.
;         FILE=f  returned complete file name (without directory).
;         NVFILE=fnv  returned complete file name without version.
;         NAME=n  returned file name without ext or vers.
;         EXTENSION=e  returned file extension.
;         VERSION=v  returned file version number.
;         OS=os  Input operating system name if not the
;           one for the current IDL session.  Values may be:
;           vms = Vax VMS, dos = PC Dos, anything else is unix.
;         The following diagram show relations between components:
;       
;        . . . A A A / N N N N N N . E E E E ; V V V
;       
;       |             |           | |       | |     |
;       |    DIR      |   NAME    | |  EXT  | | VER |
;       +-------------+-----------+ +-------+ +-----+
;       |             |                     |       |
;       |             |        NVFILE       |       |
;       |             +---------------------+       |
;       |             |                             |
;       |             |              FILE           |
;       |             +-----------------------------+
;       |                                           |
;       |                  INPUT                    |
;       +-------------------------------------------+
;       
; OUTPUTS:
; COMMON BLOCKS:
; NOTES:
;       Notes: The argument to this routine has the form:
;         dir+file where dir is operating system dependent
;         but something like d0:[aaa.bbb] for vms, or
;         /aaa.bbb/ for unix, or d:\aaa.bbb\ for DOS.
;         The file will be of the form: name.ext;vers.
;         Only VMS has version numbers.
;         Requested values that do not occur are returned as
;         null strings
; MODIFICATION HISTORY:
;       Ray Sterner,  16 APR, 1985.
;       Johns Hopkins University Applied Physics Laboratory.
;       RES, Added DIR 29 May, 1985.
;       R. Sterner, made op. sys. independent --- 11 Sep, 1991.
;
; Copyright (C) 1985, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
	PRO FILEBREAK, pathfile, directory=dir, file=file, nvfile=nvfile, $
	  name=name, extension=ext, version=vers, os=os, help=hlp
 
	IF (N_PARAMS(0) EQ 0) or keyword_set(hlp) THEN BEGIN
	  PRINT,' Breaks file name(s) into components.
	  PRINT,' filebreak, input'
	  print,'   input = input file name (may be an array).  in'
	  print,' Keywords:'
	  print,'   DIRECTORY=d  returned directory name.'
	  print,'   FILE=f  returned complete file name (without directory).'
	  print,'   NVFILE=fnv  returned complete file name without version.'
	  print,'   NAME=n  returned file name without ext or vers.'
	  print,'   EXTENSION=e  returned file extension.'
	  print,'   VERSION=v  returned file version number.'
	  print,'   OS=os  Input operating system name if not the'
	  print,'     one for the current IDL session.  Values may be:'
	  print,'     vms = Vax VMS, dos = PC Dos, anything else is unix.'
	  print,'   The following diagram show relations between components:'
	  print,' '
          print,'  . . . A A A / N N N N N N . E E E E ; V V V
	  print,' '
          print,' |             |           | |       | |     |
          print,' |    DIR      |   NAME    | |  EXT  | | VER |
          print,' +-------------+-----------+ +-------+ +-----+
          print,' |             |                     |       |
          print,' |             |        NVFILE       |       |
          print,' |             +---------------------+       |
          print,' |             |                             |
          print,' |             |              FILE           |
          print,' |             +-----------------------------+
          print,' |                                           |
          print,' |                  INPUT                    |
          print,' +-------------------------------------------+
	  print,' '
	  print,' Notes: The argument to this routine has the form:'
	  print,'   dir+file where dir is operating system dependent'
	  print,'   but something like d0:[aaa.bbb] for vms, or'
	  print,"   /aaa.bbb/ for unix, or d:\aaa.bbb\ for DOS."
	  print,'   The file will be of the form: name.ext;vers.'
	  print,'   Only VMS has version numbers.'
	  print,'   Requested values that do not occur are returned as'
	  print,'   null strings'
	  RETURN
	ENDIF 
 
	;---  Allow input to be an array  ------
	n = n_elements(pathfile) 
 
	;------  By default all parts of the path+file are null strings ---
	dir = strarr(n)
	file = strarr(n)
	nvfile = strarr(n)
	name = strarr(n)
	ext = strarr(n)
	vers = strarr(n)
 
	;---  Loop through each input element  ---
	for i = 0, n-1 do begin
 
	  ;-------  The path+file was null  -----
	  if pathfile(i) ne '' then begin
 
	    ;-------  Find where to split directory and file name  ------
	    if n_elements(os) eq 0 then os = !version.os
	    os = strupcase(os)
 
	    case os of
'VMS':	    begin
	      file(i) = pathfile(i)
	      loc = strpos(pathfile(i),']')
	      if loc eq -1 then loc = strpos(pathfile(i),':')
	    end
'DOS':	    begin
	      file(i) = pathfile(i)
	      loc = strpos(strtrim(reverse(byte(pathfile(i))),2),'\')
	      if loc ge 0 then loc = strlen(pathfile(i))-loc-1
	    end
else:	    begin	; Assume UNIX.
	      file(i) = pathfile(i)
	      loc = strpos(strtrim(reverse(byte(pathfile(i))),2),'/')
	      if loc ge 0 then loc = strlen(pathfile(i))-loc-1
	    end
	    endcase
 
	    ;---  If directory exists split directory and rest of file name ---
	    if loc ge 0 then begin
	      dir(i) = strmid(pathfile(i),0,loc+1)
	      file(i) = strmid(pathfile(i),loc+1,999)
	    endif
 
	    ;---  If version exists split version and rest of file name  ---
	    nvfile(i) = file(i)
	    loc = strpos(file(i),';')
	    if loc ge 0 then begin
	      nvfile(i) = strmid(file(i),0,loc)
	      vers(i) = strmid(file(i),loc+1,999)
	    endif
 
	    ;---  If extension exists split extension and file name  ---
	    name(i) = nvfile(i)
	    loc = strpos(nvfile(i),'.')
	    if loc ge 0 then begin
;	      name(i) = strmid(nvfile(i),0,loc)
;	      ext(i) = strmid(nvfile(i),loc+1,999)
	      name(i) = getwrd(nvfile(i),-99,-1,/last,del='.')
	      ext(i) = getwrd(nvfile(i),/last,del='.')
	    endif
 
	  endif  ; Endif name not null.
 
	endfor  ; End of loop through input elements.
 
	;---  Check if scalars  -----
 	if n_elements(dir) eq 1 then dir = dir(0)
 	if n_elements(file) eq 1 then file = file(0)
 	if n_elements(nvfile) eq 1 then nvfile = nvfile(0)
 	if n_elements(name) eq 1 then name = name(0)
 	if n_elements(ext) eq 1 then ext = ext(0)
 	if n_elements(vers) eq 1 then vers = vers(0)
 
	return
	end
