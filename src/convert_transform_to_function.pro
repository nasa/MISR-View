PRO convert_transform_to_function, transform_file
;1234567890123456789012345678901234567890123456789012345678901234567890123456789
	ON_IOERROR, BAD

	trans_info_strarr	= ['']
	trans_strarr		= ['']
	trans_setup_strarr	= ['']

	dot_pos			= STRPOS(transform_file,'.',/REVERSE_SEARCH)
	sep			= [STRMID(transform_file,0,dot_pos),STRMID(transform_file,dot_pos+1)]
;	sep			= STR_SEP(transform_file,'.')
	outfile			= sep[0]+'_'+sep[1]+'.pro'
	sep			= STR_SEP(transform_file,PATH_SEP())
	pro_name		= sep[N_ELEMENTS(sep)-1]
	sep			= STR_SEP(pro_name,'.')
	pro_name		= sep[0]+'_'+sep[1]

	program_listing		= ['FUNCTION '+ pro_name+', image_data_obj, source_data']


	OPENR, lun, transform_file, /GET_LUN
	str		= ''
	bad_file	= 0
	READF, lun, str
	str			= STRTRIM(str,2)
	IF STRLEN(str) GE STRLEN(';IMAGE_DATA_TRANSFORM_FILE') THEN BEGIN
		end_str	= STRMID(						$
				str,						$
				STRLEN(str)-					$
					STRLEN('IMAGE_DATA_TRANSFORM_FILE'),	$
				STRLEN('IMAGE_DATA_TRANSFORM_FILE'))
		IF end_str NE 'IMAGE_DATA_TRANSFORM_FILE' THEN BEGIN
			msg	= [						$
				'The file ' + transform_file,			$
				'does not appear to be a valid transform file.',$
				'Please consult the misr_view user guide for',	$
				'details on creating transform files.' ]
			res	= DIALOG_MESSAGE( msg, /ERROR )
			bad_file	= 1
		ENDIF
	ENDIF ELSE BEGIN
		msg	= [							$
			'The file ' + transform_file,				$
			'does not appear to be a valid transform file.',	$
			'Please consult the misr_view user guide for',		$
			'details on creating transform files.' ]
		res	= DIALOG_MESSAGE( msg, /ERROR )
		bad_file= 1
	ENDELSE

	IF bad_file THEN BEGIN
		FREE_LUN, lun
		RETURN
	ENDIF

	trans_info_strarr	= STRARR(100)
	trans_strarr		= STRARR(100)
	trans_setup_strarr	= STRARR(100)
	trans_info_ctr		= 0
	trans_ctr		= 0
	trans_setup_ctr		= 0

	transform_comment_open	= 0
	transform_setup_open	= 0
	transform_eqn_open	= 0
	error_encountered	= 0
	setup_encountered	= 0
	msg			= [ 'Error encountered reading transform file' ]

	WHILE NOT EOF(lun) AND NOT error_encountered DO BEGIN
		READF, lun, str
		str	= STRTRIM( str, 2 )
		CASE 1 OF
			str EQ ';START_IMAGE_DATA_TRANSFORM_COMMENTS': BEGIN
				IF NOT error_encountered THEN BEGIN
					IF					$
						NOT transform_comment_open AND	$
						NOT transform_setup_open AND	$
						NOT transform_eqn_open THEN BEGIN
						transform_comment_open	= 1
					ENDIF ELSE BEGIN
						error_encountered	= 1
						msg			= [ 'start/end keywords out of order' ]
					ENDELSE
				ENDIF
				END
			str EQ ';END_IMAGE_DATA_TRANSFORM_COMMENTS':BEGIN
				IF NOT error_encountered THEN BEGIN
					IF					$
						transform_comment_open AND	$
						NOT transform_setup_open AND	$
						NOT transform_eqn_open THEN BEGIN
						transform_comment_open	= 0
					ENDIF ELSE BEGIN
						error_encountered	= 1
						msg			= [ 'start/end keywords out of order' ]
					ENDELSE
				ENDIF
				END
			str EQ ';START_IMAGE_DATA_TRANSFORM_SETUP': BEGIN
				IF NOT error_encountered THEN BEGIN
					IF					$
						NOT transform_comment_open AND	$
						NOT transform_setup_open AND	$
						NOT transform_eqn_open THEN BEGIN
						transform_setup_open	= 1
					ENDIF ELSE BEGIN
						error_encountered	= 1
						msg			= [ 'start/end keywords out of order' ]
					ENDELSE
				ENDIF
				END
			str EQ ';END_IMAGE_DATA_TRANSFORM_SETUP': BEGIN
				IF NOT error_encountered THEN BEGIN
					IF					$
						NOT transform_comment_open AND	$
						transform_setup_open AND	$
						NOT transform_eqn_open THEN BEGIN
						transform_setup_open	= 0
					ENDIF ELSE BEGIN
						error_encountered	= 1
						msg			= [ 'start/end keywords out of order' ]
					ENDELSE
				ENDIF
				END
			str EQ ';START_IMAGE_DATA_TRANSFORM_EQUATION': BEGIN
				IF NOT error_encountered THEN BEGIN
					IF					$
						NOT transform_comment_open AND	$
						NOT transform_setup_open AND	$
						NOT transform_eqn_open THEN BEGIN
						transform_eqn_open	= 1
					ENDIF ELSE BEGIN
						error_encountered	= 1
						msg			= [ 'start/end keywords out of order' ]
					ENDELSE
				ENDIF
				END
			str EQ ';END_IMAGE_DATA_TRANSFORM_EQUATION': BEGIN
				IF NOT error_encountered THEN BEGIN
					IF					$
						NOT transform_comment_open AND	$
						NOT transform_setup_open AND	$
						transform_eqn_open THEN BEGIN
						transform_eqn_open	= 0
					ENDIF ELSE BEGIN
						error_encountered	= 1
						msg			= [ 'start/end keywords out of order' ]
					ENDELSE
				ENDIF
				END
			STRMID(str,0,1) EQ ';': BEGIN
				IF transform_comment_open AND NOT error_encountered THEN BEGIN
					trans_info_strarr[trans_info_ctr]	= STRMID(str,1)
					trans_info_ctr				= trans_info_ctr + 1
				ENDIF
				END
			ELSE: BEGIN
				CASE 1 OF
					STRLEN(str) EQ 0: BEGIN
						END
					transform_comment_open: BEGIN
						error_encountered	= 1
						msg			= [ 'non-comment line encountered',	$
									    'in comment section' ]
						END
					transform_eqn_open: BEGIN
						trans_strarr[trans_ctr]	= str
						trans_ctr		= trans_ctr + 1
						END
					transform_setup_open: BEGIN
						trans_setup_strarr[trans_setup_ctr]	= str
						trans_setup_ctr				= trans_setup_ctr + 1
						setup_encountered			= 1
						END
					ELSE: BEGIN
						error_encountered	= 1
						msg			= [ 'non-comment line encountered outside',	$
									    'of setup or equation sections' ]
						END
				ENDCASE
				END
		ENDCASE
	ENDWHILE

	GOTO, DONE

BAD:	msg			= [ 'Error encountered while attempting to read',			$
				     transform_file,							$
				     !ERR_STRING ]
	error_encountered	= 1

DONE:	FREE_LUN, lun

	IF trans_ctr LE 0 AND NOT error_encountered THEN BEGIN
		error_encountered	= 1
		msg			= [ 'no transform specified' ]
	ENDIF

	IF NOT error_encountered THEN BEGIN
		IF					$
			transform_comment_open OR	$
			transform_setup_open OR		$
			transform_eqn_open THEN BEGIN
			error_encountered	= 1
			msg			= [ 'missing start/end keyword' ]
		ENDIF
	ENDIF

	IF error_encountered THEN BEGIN
		ret	= DIALOG_MESSAGE( msg, /ERROR )
		RETURN
	ENDIF

	IF trans_info_ctr GT 0 THEN program_listing = [program_listing,'; '+trans_info_strarr[0:trans_info_ctr-1]]

	IF setup_encountered THEN BEGIN
		i			= 0
		WHILE i LT trans_setup_ctr AND NOT error_encountered DO BEGIN
			tmpstr		= trans_setup_strarr[i]
			sep_str		= STR_SEP( tmpstr, '=' )
			IF N_ELEMENTS(sep_str) NE 2 THEN BEGIN
				error_encountered	= 1
				msg			= [ 'bad line in setup portion of transform file:',	$
							    tmpstr ]
			ENDIF ELSE BEGIN

				start_pos		= 0
				done			= 0

				WHILE NOT done DO BEGIN
					pos			= STRPOS(STRUPCASE(STRCOMPRESS(tmpstr,/REMOVE_ALL)),'SELF->',start_pos)
					IF pos GE 0 THEN BEGIN
						pos2		= STRPOS(STRUPCASE(tmpstr),'SELF')
						tmpstr		= STRMID(tmpstr,0,pos2)+'image_data_obj'+STRMID(tmpstr,pos2+4)
					ENDIF ELSE BEGIN
						done		= 1
					ENDELSE
					start_pos		= start_pos + pos + 6
				ENDWHILE

				program_listing			= [program_listing, tmpstr]

			ENDELSE
			i		= i + 1
		ENDWHILE

		IF error_encountered THEN BEGIN
			ret	= DIALOG_MESSAGE( msg, /ERROR )
			RETURN
		ENDIF

		i	= 0

		WHILE i LT trans_ctr AND NOT error_encountered DO BEGIN
			t		= trans_strarr[i]
			done		= 0
			start_pos	= 0
			WHILE NOT done AND NOT error_encountered DO BEGIN
				pos1	= STRPOS( t, '<<', start_pos )
				IF pos1 GE 0 THEN BEGIN
					pos2	= STRPOS( t, '>>', pos1 )
					IF pos2 GE pos1+2 AND NOT (pos1 EQ 0 AND pos2 EQ STRLEN(t)-2) THEN BEGIN
						current_varname		= STRCOMPRESS( STRTRIM( STRMID( t, pos1+2,pos2-pos1-2 ), 2 ), /REMOVE_ALL )
						IF STRLEN( current_varname ) GT 0 THEN BEGIN

							str2chars	= STRARR(STRLEN(current_varname))
							FOR char_ctr = 0, N_ELEMENTS(str2chars)-1 DO str2chars[char_ctr] = STRMID(current_varname,char_ctr,1)

							bracket_idx1			= WHERE( str2chars EQ '[', bracket_cnt1 )
							bracket_idx2			= WHERE( str2chars EQ ']', bracket_cnt2 )
							bad_brackets_encountered	= 0
							bracket_value_str		= ''
							IF 								$
								(bracket_cnt1 GT 0 AND bracket_cnt2 EQ 0) OR		$ ;unmatched forward brackets
								(bracket_cnt1 EQ 0 AND bracket_cnt2 GT 0) OR		$ ;unmatched backward brackets
								bracket_cnt1 GT 1 OR					$ ;too many forward brackets
								bracket_cnt2 GT 1 OR					$ ;too many backward brackets
								((bracket_cnt1 EQ 1 AND bracket_cnt2 EQ 1) AND 		$
								  bracket_idx1[0] GE bracket_idx2[0]) OR		$ ;out-of-order brackets
								bracket_idx1[0] EQ 0 OR					$ ;forward bracket is before variable name
								((bracket_cnt1 EQ 1 AND bracket_cnt2 EQ 1) AND 		$
								  bracket_idx1[0]+1 GE bracket_idx2[0])			$ ;nothing between brackets (e.g., '[]')
							THEN bad_brackets_encountered = 1

							IF NOT bad_brackets_encountered AND bracket_cnt1 EQ 1 AND bracket_cnt2 EQ 1 THEN BEGIN
								bracket_value_str	= STRMID( current_varname, bracket_idx1[0], bracket_idx2[0]-bracket_idx1[0]+1 )
								current_varname		= STRMID( current_varname, 0, bracket_idx1[0] )
							ENDIF

							IF bad_brackets_encountered LE 0 AND error_encountered LT 1 THEN BEGIN
								sep	= STR_SEP(t,'<<')
								out_t	= ''
								FOR n = 0, N_ELEMENTS(sep) - 1 DO out_t = out_t + sep[n]
								sep	= STR_SEP(out_t,'>>')
								out_t	= ''
								FOR n = 0, N_ELEMENTS(sep) - 1 DO out_t = out_t + sep[n]
								t	= out_t
							ENDIF ELSE BEGIN
								error_encountered	= 1
								msg			= [ 'unknown variable in equation',	$
											    'portion of transform file:',	$
											    current_varname ]
							ENDELSE
							done			= 1
						ENDIF ELSE BEGIN
							error_encountered	= 1
							msg			= [ 'bad line in equation portion of transform file:',	$
										    t ]
							done			= 1
						ENDELSE
					ENDIF ELSE BEGIN
						error_encountered	= 1
						msg			= [ 'bad line in equation portion of transform file:',	$
									    t ]
						done			= 1
					ENDELSE
				ENDIF ELSE BEGIN
					done	= 1
				ENDELSE
				program_listing	= [program_listing, t]
			ENDWHILE
			i	= i + 1
		ENDWHILE
	ENDIF

	IF error_encountered THEN BEGIN
		ret	= DIALOG_MESSAGE( msg, /ERROR )
		RETURN
	ENDIF
;print,'outfile = ',outfile
	OPENW, lun, outfile, /GET_LUN
	FOR n = 0, N_ELEMENTS(program_listing) - 1 DO PRINTF, lun, program_listing[n]
	PRINTF, lun, 'RETURN, transformed_data'
	PRINTF, lun, 'END'
	FREE_LUN, lun
END
; convert_transform_to_function
