@filebreak.pro
@getwrd.pro

FUNCTION data_selections_read, data_selections_struct, directory, FILENAME = filename

	;=======================================================================
	; basic error catch mechanism, generally for file read or write when 
	; permissions do not allow it.  Also, in this case, for any ASCII read 
	; errors.
	;=======================================================================
	routine_name	= '========== data_selections_read =========='
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		e_msg	= [											$
				routine_name,									$
				'Error Index: ' + STRTRIM( error_status, 2 ),					$
				'Error Message: ' + !ERR_STRING,						$
				'',										$
				'Suggestion 1: Possible problem reading data selections recall file.',		$
				'              Please check formatting of ASCII file.',				$
				'              Try STORE to generate a template for comparison.',		$
				'',										$
				'Suggestion 2: If attempting to read or write a file, then check permissions.',	$
				'',										$
				'Returning...' ]
		result	= DIALOG_MESSAGE( e_msg, /ERROR )
		RETURN, 0
	ENDIF

;;;ckt,apr2001IF NOT KEYWORD_SET(filename) THEN filename = DIALOG_PICKFILE( TITLE = 'Select ASCII Store File', PATH = directory )
IF NOT KEYWORD_SET(filename) THEN filename = dialog_pickfile_wrapper( TITLE = 'Select ASCII Store File', PATH = directory, /MUST_EXIST )
IF filename EQ '' THEN RETURN, 0
IF (FINDFILE(filename))[0] EQ '' THEN BEGIN
	message		= [ filename, 'WARNING:  File not found.', 'Please try again.' ]
	ret		= DIALOG_MESSAGE(message)
	RETURN, 0
ENDIF
;--------------------------------------------------
; Set the directory variable which the calling 
; routine (misr_view.pro) will store as the default 
; path next time this is called.
;--------------------------------------------------
FILEBREAK, filename, DIRECTORY = directory

;--------------------------------------------------
; Determine number of records in file and read 
; contents into array.  Then, separate keywords and 
; values.
;--------------------------------------------------
n		= N_ELEMENTS((READ_ASCII( filename, DELIMITER = '' )).field1)
s		= STRARR(n)
OPENR, lun, filename, /GET_LUN
READF, lun, s
FREE_LUN, lun
ss		= STRARR(2,n)
FOR i=0,n-1 DO		$
	ss[*,i]		= STR_SEP(s[i],'=')

;--------------------------------------------------
; Extract the information.
; *************************************************
; ****     ERROR CHECK WHERE RESULT FOR -1     ****
; *************************************************
;--------------------------------------------------
nPlanes		= ss[ 1, WHERE(STRLOWCASE(STRMID(ss[0,*],0, 7)) eq 'nplanes') ]
orbit		= ss[ 1, WHERE(STRLOWCASE(STRMID(ss[0,*],0, 5)) eq 'orbit') ]
path		= ss[ 1, WHERE(STRLOWCASE(STRMID(ss[0,*],0, 4)) eq 'path') ]
blockStart	= ss[ 1, WHERE(STRLOWCASE(STRMID(ss[0,*],0,10)) eq 'blockstart') ]
blockEnd	= ss[ 1, WHERE(STRLOWCASE(STRMID(ss[0,*],0, 8)) eq 'blockend') ]
crossTrackRes	= ss[ 1, WHERE(STRLOWCASE(STRMID(ss[0,*],0,13)) eq 'crosstrackres') ]
alongTrackRes	= ss[ 1, WHERE(STRLOWCASE(STRMID(ss[0,*],0,13)) eq 'alongtrackres') ]
rotationAngle	= ss[ 1, WHERE(STRLOWCASE(STRMID(ss[0,*],0,13)) eq 'rotationangle') ]
catalogDir	= ss[ 1, WHERE(STRLOWCASE(STRMID(ss[0,*],0,13)) eq 'catalogdir') ]
catalogFile	= catalogDir + ss[ 1, WHERE(STRLOWCASE(STRMID(ss[0,*],0,13)) eq 'catalogfile') ]
airMisrDir	= ss[ 1, WHERE(STRLOWCASE(STRMID(ss[0,*],0,13)) eq 'airmisrdir') ]
airMisrFile	= airMisrDir + ss[ 1, WHERE(STRLOWCASE(STRMID(ss[0,*],0,13)) eq 'airmisrfile') ]

;--------------------------------------------------
; NOTE0:  When extracting information that goes 
; into the arrays for multi-plane info, allow for 
; the ASCII file to have planes listed in any order 
; and for whole planes to be missing.  HOWEVER, 
; each piece of info for a given plane must be 
; adjacent in the ASCII file.
;
; NOTE1:  While the information is stored in the 
; ASCII file grouped into plane groups, the info is 
; stored in the structure by item- that is, the 
; "product" information for all planes is stored in 
; a "product" array.  CONFUSED?  This note was 
; supposed to straighten that out!
;
; NOTE2:  "extraDims" is optional.
;--------------------------------------------------
; *************************************************
; ****     ERROR CHECK WHERE RESULT FOR -1     ****
; *************************************************
allplanesidx	= WHERE(STRLOWCASE(STRMID(ss[0,*],0, 5)) eq 'plane')
ss2		= REFORM(ss[0,*])
FOR i=0,n-1 DO								$
	ss2[i]	= (STR_SEP(ss2[i],':'))[0]
FOR i=allplanesidx[0],allplanesidx[0]+N_ELEMENTS(allplanesidx)-1 DO	$
	ss2[i]	= (STR_SEP(ss2[i],'plane'))[1]
planeNumbers	= ss2[UNIQ(ss2[allplanesidx])+MIN(allplanesidx)]
product		= STRARR(nPlanes[0])
grid		= STRARR(nPlanes[0])
field		= STRARR(nPlanes[0])
numbertype	= STRARR(nPlanes[0])
extradims	= STRARR(nPlanes[0])
FOR i=0,N_ELEMENTS(planeNumbers)-1 DO BEGIN
; *************************************************
; ****     ERROR CHECK WHERE RESULT FOR -1     ****
; *************************************************
	info		= ss[*,WHERE(ss2 eq planenumbers[i])]
	FOR j=0,N_ELEMENTS(WHERE(ss2 EQ STRTRIM(i,2)))-1 DO						$
		IF STRLOWCASE((STR_SEP((STR_SEP(info[0,j],'plane'))[1],':'))[1]) EQ 'directory' THEN	$
			d_		= info[1,j]
	FOR j=0,N_ELEMENTS(WHERE(ss2 EQ STRTRIM(i,2)))-1 DO						$
		IF STRLOWCASE((STR_SEP((STR_SEP(info[0,j],'plane'))[1],':'))[1]) EQ 'product' THEN	$
			p_		= info[1,j]
	FOR j=0,N_ELEMENTS(WHERE(ss2 EQ STRTRIM(i,2)))-1 DO						$
		IF STRLOWCASE((STR_SEP((STR_SEP(info[0,j],'plane'))[1],':'))[1]) EQ 'grid' THEN		$
			g_		= info[1,j]
	FOR j=0,N_ELEMENTS(WHERE(ss2 EQ STRTRIM(i,2)))-1 DO						$
		IF STRLOWCASE((STR_SEP((STR_SEP(info[0,j],'plane'))[1],':'))[1]) EQ 'field' THEN	$
			f_		= info[1,j]
	FOR j=0,N_ELEMENTS(WHERE(ss2 EQ STRTRIM(i,2)))-1 DO						$
		IF STRLOWCASE((STR_SEP((STR_SEP(info[0,j],'plane'))[1],':'))[1]) EQ 'numbertype' THEN	$
			n_		= info[1,j]							$
		ELSE	n_		= ''
	FOR j=0,N_ELEMENTS(WHERE(ss2 EQ STRTRIM(i,2)))-1 DO						$
		IF STRLOWCASE((STR_SEP((STR_SEP(info[0,j],'plane'))[1],':'))[1]) EQ 'extradims' THEN	$
			e_		= info[1,j]							$
		ELSE	e_		= ''
	product[	planeNumbers[i] ]		= d_ + p_
	grid[		planeNumbers[i] ]		= g_
	field[		planeNumbers[i] ]		= f_
	numbertype[	planeNumbers[i] ]		= n_
	extradims[	planeNumbers[i] ]		= e_
ENDFOR

data_selections_struct = {			$
	nPlanes		: nplanes,		$
	product		: product,		$
	grid		: grid,			$
	field		: field,		$
	num_type	: numbertype,		$
	extraDims	: extradims,		$
	orbit		: orbit,		$
	path		: path,			$
	blockStart	: blockstart,		$
	blockEnd	: blockend,		$
	actRes		: crosstrackres,	$
	altRes		: alongtrackres,	$
	rotationAngle	: rotationangle,	$
	catalogFile	: catalogFile,		$
	airMisrFile	: airMisrFile		}

RETURN, 1

END
