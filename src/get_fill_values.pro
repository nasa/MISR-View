function get_fill_values, product

;print, 'get_fill_values, product = ', product

CASE STRUPCASE( product ) OF
	'GP_GMP' : BEGIN
		 fill_values = [ $
			-111, $		; level 1 fill above data
			-222, $		; level 1 fill below data
			-333, $		; level 1 fill IPI invalid
			-444, $		; level 1 fill to side of data
			-555, $		; level 1 fill not processed
			-999 $		; level 1 fill IPI error
			]
	END
	'FM_SCI' : BEGIN
		fill_values = [ $
			16377 $		; gap fill
			]
	END
	'RP_GM' : BEGIN
		fill_values = [ $
			16380, $	; negative radiance
			16379, $	; radiance out of range
			16378, $	; negative discriminant
			16377 $		; gap fill
			]
	END
	'GRP_TERRAIN' : BEGIN
		fill_values = [ $
			65515, $	; Added Aug 3, 1999, JRH
			16380, $	; fill unusable
			16379, $	; fill ocean
			16378, $	; fill outside
			16377 $	; fill obscured
			]
	END
	'GRP_ELLIPSOID' : BEGIN
		fill_values = [ $
; Oct 18, 1999 JRH - assumed to be same as for 'GRP_TERRAIN'
			65515, $	; Added Aug 3, 1999, JRH
			16380, $	; fill unusable
			16379, $	; fill ocean
			16378, $	; fill outside
			16377 $	; fill obscured
			]
	END
	'GRP_RCCM_GM' : BEGIN
		fill_values = [ $
			255 $		; fill for cloud, glitter or quality arrays
			]
	END
	ELSE : BEGIN
		fill_values = [ $
			2E14-1D, $
			2E14-2D, $
			2E14-3D, $
			2E14-4D, $
			2E14-5D, $
			2E14-6D, $
			65515, $	; Added Aug 3, 1999, JRH
			65511, $
			16380, $
			16379, $
			16378, $
			16377, $
		;;	255, $
			0, $		; Radience fill
			-21, $		; Radience fill (left and right of block?)
		;;	-99, $
		;;	-111, $
			-222, $
			-333, $
			-444, $
			-555, $
			-999, $
			-9999 $
			]
	END
ENDCASE

RETURN, fill_values

END
