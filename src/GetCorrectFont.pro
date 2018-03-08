;12345678901234567890123456789012345678901234567890123456789012345678901234
;+
;==========================================================================
;
;Module Name:	GetCorrectFont
;
;Call Protocol:	fontString = GetCorrectFont( fontRequestString )
;
;==========================================================================
;
;		Jet Propulsion Laboratory (JPL)
;
;		Multi-angle Imaging SpectroRadiometer  (MISR)
;
;		Instrument(s) :	<instrument>
;
;		Subsystem(s) :	<subsystem(s)>
;
;	Copyright (c) 2001 California Institute of Technology
;	U.S. Government Sponsorship under NASA Contract NAS7-1270
;	is acknowledged.
;
;		Cognizant Programmer(s) :
;
;		Charles Thompson	Charles.K.Thompson@jpl.nasa.gov
;
;===========================================================================
;
;Description:
;
; GetCorrectFont is a utility function designed to aid the cross-platform
; look and feel of IDL interfaces.  The user can call this function with 
; one of the strings described below in order to set the font for widgets
; such as buttons and labels.  The result is a standardized "look" of
; the widget regardless of whether it is shown on a PC ("Win32"), SGI
; ("IRIX"), or Sun ("sunos").
;
;Input Parameters:
;
;	Type	Name		Units		Purpose
;	-------------------------------------------------------------------
;	    	fontRequestString	string describing the kind of font
;					desired
;
;Output Parameters:
;
;	Type	Name		Units	Purpose
;	-------------------------------------------------------------------
;		fontString	 	platform-dependent string representing
;					the desired font which can be used in
;					widget creation routines
;
;Globals:
;
;	Type	Name	Units	Purpose
;	-------------------------------------------------------------------
;	<type>	<name>	<units>	<purpose>
;
;Return Values (INIT):
;
;	Type	Name	Units	Purpose
;	-------------------------------------------------------------------
;
;Known Bugs and Limitations:
;
;	<bugs & limitations>
;
;Parent(s):
;
;	<parents, if any>
;
;Routines Called:
;
;	<routines called, if any>
;
;Files Accessed:
;
;	<files accessed, if any>
;
;Revision History:
;
;Notes:
;
;
;=============================================================================
;-
;
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GetCorrectFont @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GetCorrectFont, usageCode

	COMMON MISRVIEWDATA, set_font, default_transform_directory
	
	IF SIZE(set_font,/TYPE) EQ 0 THEN set_font = 1
	
	IF NOT set_font THEN RETURN, ''

	sys	= !VERSION.OS
;-----------------------------------------------
; Codes are the font type plus a number that 
; represents a relative size, beginning at 1 (smallest)
; 
;-----------------------------------------------
	CASE sys OF
	   'sunos': BEGIN
	      CASE usageCode OF
	      ;--------------------------------------------------------------
	      ; ************** Sun OS - HELVETICA **************
	      ;--------------------------------------------------------------
	      	;-----------------------------
	      	; medium
	      	;-----------------------------
		 'helvetica1'		:fontType = '-*-helvetica-medium-r-normal-*-*-120-*-*-*-*-*-*'
		 'helvetica2'		:fontType = '-*-helvetica-medium-r-normal-*-*-130-*-*-*-*-*-*'
		 'helvetica3'		:fontType = '-*-helvetica-medium-r-normal-*-*-150-*-*-*-*-*-*'
		 'helvetica4'		:fontType = '-*-helvetica-medium-r-normal-*-*-180-*-*-*-*-*-*'
		 'helvetica5'		:fontType = '-*-helvetica-medium-r-normal-*-*-200-*-*-*-*-*-*'
		 'helvetica6'		:fontType = '-*-helvetica-medium-r-normal-*-*-230-*-*-*-*-*-*'
	      	;-----------------------------
	      	; bold
	      	;-----------------------------
		 'helvetica1bold'	:fontType = '-*-helvetica-bold-r-normal-*-*-120-*-*-*-*-*-*'
		 'helvetica2bold'	:fontType = '-*-helvetica-bold-r-normal-*-*-130-*-*-*-*-*-*'
		 'helvetica3bold'	:fontType = '-*-helvetica-bold-r-normal-*-*-150-*-*-*-*-*-*'
		 'helvetica4bold'	:fontType = '-*-helvetica-bold-r-normal-*-*-180-*-*-*-*-*-*'
		 'helvetica5bold'	:fontType = '-*-helvetica-bold-r-normal-*-*-200-*-*-*-*-*-*'
		 'helvetica6bold'	:fontType = '-*-helvetica-bold-r-normal-*-*-230-*-*-*-*-*-*'
		 
	      ;--------------------------------------------------------------
	      ; ************** Sun OS - COURIER ************** 
	      ;--------------------------------------------------------------
	      	;-----------------------------
	      	; bold
	      	;-----------------------------
		 'courier2bold'		:fontType = '-*-courier-bold-r-normal-*-*-160-*-*-*-*-*-*'		
		 'courier3bold'		:fontType = '-*-courier-bold-*-*-*-*-130-*-*-*-*-*-*'
	      	;-----------------------------
	      	; bold italic
	      	;-----------------------------
		 'courier2bolditalic'	:fontType = '-*-courier-bold-*-*-*-*-160-*-*-*-*-*-*'
		 
	      ;--------------------------------------------------------------
	      ; ************** Sun OS - HERSHEY VECTOR ************** 
	      ;--------------------------------------------------------------
		 'vector1'		:fontType = 1.5
		 'vector2'		:fontType = 2.0
		 'vector3'		:fontType = 2.5
	      ENDCASE
	      END
	   'IRIX': BEGIN
	   	fontType=''
	      CASE usageCode OF
	      ;--------------------------------------------------------------
	      ; ************** SGI Irix OS - HELVETICA **************
	      ;--------------------------------------------------------------
	      	;-----------------------------
	      	; medium
	      	;-----------------------------
		 'helvetica1'		:fontType = '-*-helvetica-medium-r-normal-*-*-70-*-*-*-*-*-*'
		 'helvetica2'		:fontType = '-*-helvetica-medium-r-normal-*-*-80-*-*-*-*-*-*'
		 'helvetica3'		:fontType = '-*-helvetica-medium-r-normal-*-*-130-*-*-*-*-*-*'
		 'helvetica4'		:fontType = '-*-helvetica-medium-r-normal-*-*-150-*-*-*-*-*-*'
		 'helvetica5'		:fontType = '-*-helvetica-medium-r-normal-*-*-180-*-*-*-*-*-*'
		 'helvetica6'		:fontType = '-*-helvetica-medium-r-normal-*-*-200-*-*-*-*-*-*'
	      	;-----------------------------
	      	; bold
	      	;-----------------------------
		 'helvetica1bold'	:fontType = '-*-helvetica-bold-r-normal-*-*-70-*-*-*-*-*-*'
		 'helvetica2bold'	:fontType = '-*-helvetica-bold-r-normal-*-*-100-*-*-*-*-*-*'
		 'helvetica3bold'	:fontType = '-*-helvetica-bold-r-normal-*-*-130-*-*-*-*-*-*'
		 'helvetica4bold'	:fontType = '-*-helvetica-bold-r-normal-*-*-150-*-*-*-*-*-*'
	         'helvetica5bold'	:fontType = '-*-helvetica-bold-r-normal-*-*-180-*-*-*-*-*-*'
		 'helvetica6bold'	:fontType = '-*-helvetica-bold-r-normal-*-*-200-*-*-*-*-*-*'

	      ;--------------------------------------------------------------
	      ; ************** SGI Irix OS - COURIER **************
	      ;--------------------------------------------------------------
	      	;-----------------------------
	      	; bold
	      	;-----------------------------
		 'courier2bold'		:fontType = '-*-courier-bold-r-normal-*-*-130-*-*-*-*-*-*'
		 'courier3bold'		:fontType = '-*-courier-bold-*-*-*-*-100-*-*-*-*-*-*'
		 
	      	;-----------------------------
	      	; bold italic
	      	;-----------------------------
		 'courier2bolditalic'	:fontType = '-*-courier-bold-*-*-*-*-130-*-*-*-*-*-*'
		 
	      ;--------------------------------------------------------------
	      ; ************** SGI Irix OS - HERSHEY VECTOR ************** 
	      ;--------------------------------------------------------------
		 'vector1'		:fontType = 0.5
		 'vector2'		:fontType = 1.1
		 'vector3'		:fontType = 1.25
	      ENDCASE
	      END
	   'MacOS':
	      ;--------------------------------------------------------------
	      ; ************** Mac OS - HELVETICA **************
	      ;--------------------------------------------------------------
	      ;--------------------------------------------------------------
	      ; ************** Mac OS - COURIER **************
	      ;--------------------------------------------------------------
	      ;--------------------------------------------------------------
	      ; ************** Mac OS - HERSHEY VECTOR ************** 
	      ;--------------------------------------------------------------
	   'Win32': BEGIN
	      CASE usageCode OF
	         'mainA': fontType = 'HELVETICA*BOLD*23'
		 'mainB': fontType = 'HELVETICA*BOLD*20'
		 'courier2': fontType = 'COURIER*BOLD*15'
		 'mainB3': fontType = 'COURIER*12'
		 'mainC': fontType = 'HELVETICA*BOLD*13'
		 'widgetA': fontType = 'HELVETICA*23'
		 'widgetB': fontType = 'HELVETICA*20'
		 'widgetC': fontType = 'HELVETICA*18'
		 'widgetD': fontType = 'HELVETICA*13'
		 'widgetE': fontType = 'HELVETICA*12'
	      ;--------------------------------------------------------------
	      ; ************** Windows OS - HELVETICA **************
	      ;--------------------------------------------------------------
	      	;-----------------------------
	      	; medium
	      	;-----------------------------
		 'helvetica1'		:fontType = 'HELVETICA*12'
		 'helvetica2'		:fontType = 'HELVETICA*13'
		 'helvetica3'		:fontType = 'HELVETICA*15'
		 'helvetica4'		:fontType = 'HELVETICA*18'
		 'helvetica5'		:fontType = 'HELVETICA*20'
		 'helvetica6'		:fontType = 'HELVETICA*23'
	      	;-----------------------------
	      	; bold
	      	;-----------------------------
		 'helvetica1bold'	:fontType = 'HELVETICA*BOLD*12'
		 'helvetica2bold'	:fontType = 'HELVETICA*BOLD*13'
		 'helvetica3bold'	:fontType = 'HELVETICA*BOLD*15'
		 'helvetica4bold'	:fontType = 'HELVETICA*BOLD*18'
	         'helvetica5bold'	:fontType = 'HELVETICA*BOLD*20'
		 'helvetica6bold'	:fontType = 'HELVETICA*BOLD*23'

	      ;--------------------------------------------------------------
	      ; ************** Windows OS - COURIER **************
	      ;--------------------------------------------------------------
	      	;-----------------------------
	      	; bold
	      	;-----------------------------
		 'courier2bold'		:fontType = 'COURIER*BOLD*13'
		 'courier3bold'		:fontType = 'COURIER*BOLD*15'
		 
	      	;-----------------------------
	      	; bold italic
	      	;-----------------------------
		 'courier2bolditalic'	:fontType = 'COURIER*BOLD*ITALIC*13'
		 
	      ;--------------------------------------------------------------
	      ; ************** Windows OS - HERSHEY VECTOR ************** 
	      ;--------------------------------------------------------------
		 'vectorA': fontType = 2.5
		 'vectorB': fontType = 2.0
		 'vectorC': fontType = 1.5
		 ENDCASE
		 END
	   ELSE:
	   ENDCASE

	RETURN, fontType
END
; GetCorrectFont
