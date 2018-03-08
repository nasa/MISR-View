;===============================================================================
; display_save_image_help
;===============================================================================
PRO display_save_image_help, group_leader
	text	= [								$
		'Notes On Saving Annotated Images With misr_view',		$
		'===============================================',		$
		'',								$
		'misr_view provides the user with several options for saving',	$
		'annotated images.  The process to save annotated images is ',	$
		'fairly straightforward, but there are some points to ',	$
		'consider:',							$
		'',								$
		'(1) An exact replica of the displayed color bar is attached',	$
		'to the output image when the user requests concatenation of ',$
		'the color bar to the image.  This means that any prior ',	$
		'changes in size, tick marks, or text size to the color bar ',	$
		'will be reflected in the annotated output image.',		$
		'',								$
		'(2) It is not necessary to supply primary, secondary and',	$
		'tertiary titles to an image; these options are simply ',	$
		'provided to the user.  If a single-line title is desired, ',	$
		'the droplist controls for the unnecessary titles can be',	$
		'set to "None".',						$
		'',								$
		'(3) When saving an annotated color image, the preview of ',	$
		'the output image may appear to look incoorect, in terms of ',	$
		'color.  Move the cursor around on the screen (preferably ',	$
		'over the XLOADCT interface) to see if the proper colors ',	$
		'reset themselves.  If they do not, use another application',	$
		'to check the output image.  Color images, even if they are',	$
		'single-band pseudocolored images, are saved as 24-bit true',	$
		'color, so there should not be any issues other than ',	$
		'potential display problems.' ]

	xdisplayfile,								$
;		GROUP = group_leader,						$
		TITLE = 'Save Image Notes',					$
		TEXT = text,							$
		/MODAL
END
; display_save_image_help
