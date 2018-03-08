FUNCTION do_text, textString, size, dims, locs, align

;IF N_PARAMS() EQ 1 THEN BEGIN
;	size = 45
;	dims = [1200,120]
;	locs = [0.0,0.0]
;	align = 0.5
;ENDIF

CASE N_PARAMS() OF
	1: BEGIN
		size = 45
		dims = [1200,120]
		locs = [0.0,0.0]
		align = 0.5
		END
	2: BEGIN
		dims = [1200,120]
		locs = [0.0,0.0]
		align = 0.5
		END
	3: BEGIN
		locs = [0.0,0.0]
		align = 0.5
		END
	4: BEGIN
		align = 0.5
		END
ENDCASE

curWin = !D.WINDOW

fontObject = OBJ_NEW( 'IDLgrFont', SIZE = size )
textObject = OBJ_NEW( 'IDLgrText', FONT = fontObject )
textObject -> SetProperty, COLOR = [255,255,255]
textObject -> SetProperty, LOCATIONS = locs
textObject -> SetProperty, STRINGS = [textString]
textObject -> SetProperty, ALIGNMENT = align

modelObject = OBJ_NEW( 'IDLgrModel' )
modelObject -> Add, textObject

viewObject = OBJ_NEW( 'IDLgrView', COLOR = [0,0,0] )
windowObject = OBJ_NEW( 'IDLgrBuffer', DIMENSIONS = dims, $
	GRAPHICS_TREE = viewObject )
viewObject -> Add, modelObject
windowObject -> Draw, viewObject

imageObject = windowObject->READ()
imageObject -> GetProperty, DATA = imageData

OBJ_DESTROY, imageObject
OBJ_DESTROY, fontObject
OBJ_DESTROY, textObject
OBJ_DESTROY, modelObject
OBJ_DESTROY, viewObject
OBJ_DESTROY, windowObject

WSET, curWin

RETURN, imageData

END
