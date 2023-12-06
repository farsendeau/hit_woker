;
; Pour metaSpritesActionXX
;     00 : Frame avant changement de state (genre perso qui cligne des yeux)
;     01-xx : une action peu avoir plusieurs metaSprite, c'est l'id du metaSprite (dataMetaSpriteXX)
metaSpritesActionTable:
	.dw metaSpritesActionStanding
	.dw metaSpritesActionFiringWithStanding
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionMovingSlowing
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionMovingRun
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionLadder

metaSpritesActionStanding:
	.db $00, $00

metaSpritesActionFiringWithStanding:
metaSpritesActionDISPO:

metaSpritesActionMovingSlowing:
	.db $22, $01, $01, $01

metaSpritesActionMovingRun:
	.db $16, $02, $03	

metaSpritesActionLadder:
	.db $16, $04, $05

;
; Pour dataSpriteXX
;     00 : Nb sprite
;     01 : Id offsetTable
;     02 : tile Id
;     03 : tile Attr 
;     xx : boucle [01 -> 04] * 00
; 
dataMetaSpriteTable:
	.dw dataMetaSpriteStanding
	.dw dataMetaSpriteMovingSlowly
	.dw dataMetaSpriteMovingRun1
	.dw dataMetaSpriteMovingRun2
	.dw dataMetaSpriteLadderClimb1
	.dw dataMetaSpriteLadderClimb2

dataMetaSpriteStanding:
	.db $09 ; 9 sprite
	.db $00 ;offsetLanding
	.db $01, $00
	.db $02, $00
	.db $10, $00 
	.db $11, $00 
	.db $12, $00 
	.db $20, $00 
	.db $21, $00 
	.db $30, $00 
	.db $31, $00


dataMetaSpriteMovingSlowly:
	.db $09 ; 9 sprite
	.db $01 ;offsetTable offsetLanding
	.db $01, $00
	.db $02, $00
	.db $13, $00 
	.db $12, $00 
	.db $22, $00 
	.db $23, $00 
	.db $32, $00 
	.db $33, $00 
	.db $34, $00

dataMetaSpriteMovingRun1:
	.db $08 ; 8 sprits
	.db $01 ;offsetTable todo offsetMoving1
	.db $03, $00
	.db $04, $00
	.db $15, $00 
	.db $16, $00 
	.db $24, $00 
	.db $25, $00
	.db $00, $00 
	.db $35, $00

dataMetaSpriteMovingRun2:
	.db $0c ; 12 sprites
	.db $02 ;offsetTable todo offsetMoving2
	.db $05, $00
	.db $06, $00
	.db $07, $00 
	.db $17, $00 
	.db $18, $00 
	.db $19, $00 
	.db $26, $00 
	.db $27, $00 
	.db $28, $00
	.db $36, $00
	.db $37, $00
	.db $38, $00

dataMetaSpriteLadderClimb1:
	.db $07 ; 8 sprite
	.db $03 ;offsetLadder
	.db $08, $00
	.db $09, $00
	.db $1a, $00 
	.db $1b, $00 
	.db $29, $00 
	.db $2a, $00 
	.db $3a, $00

dataMetaSpriteLadderClimb2:
	.db $07 ; 8 sprite
	.db $04 ;offsetLadder2
	.db $08, $40
	.db $09, $40
	.db $1a, $40 
	.db $1b, $40 
	.db $29, $40 
	.db $2a, $40 
	.db $3a, $40

offsetTable:
	.dw offsetStanding
	.dw offsetMovingSlowly ; et dataMetaSpriteMovingRun1
	.dw offsetMovingRun2
	.dw offsetLadder
	.dw offsetLadder2
	
;
; Contient les offsetId de chaque sprite composant le metasprite
;
offsetStanding:
	.db $00, $01, $02, $03, $04, $05, $06, $07, $08
offsetMovingSlowly
	.db $09, $00, $02, $03, $0a, $05, $0b, $07, $08
offsetMovingRun2:
	.db $09, $00, $01, $02, $03, $04, $0a, $05, $06, $0b, $07, $08
offsetLadder:
    .db $0c, $0d, $0e, $0f, $10, $11, $13 
offsetLadder2:
    .db $0d, $0c, $0f, $0e, $11, $10, $12 

	;    00   01   02   03   04   05   06   07   08   09   0a   0b  0c   0d   0e   0f   10   11   12   13
offsetRightX:
   ;.db $08, $00, $10, $08, $00, $08, $00, $08, $00, $10, $10, $10, $08, $00, $08, $00, $08, $00, $08, $00
	.db $fc, $f4, $04, $fc, $f4, $fc, $f4, $fc, $f4, $04, $04, $04, $00, $f8, $00, $f8, $00, $f8, $00, $f8
offsetLeftX:
	;.db $08, $10, $00, $08, $10, $08, $10, $08, $10, $00, $00, $00, $00, $08, $00, $08, $00, $08, $00, $08
	.db $fc, $04, $f4, $fc, $04, $fc, $04, $fc, $04, $f4, $f4, $f4, $f8, $00, $f8, $00, $f8, $00, $f8, $00
offsetY:
	.db $00, $00, $08, $08, $08, $10, $10, $18, $18, $00, $10, $18, $00, $00, $08, $08, $10, $10, $18, $18