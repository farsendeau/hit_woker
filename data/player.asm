;
; Permet d'obtenir l'ID du tableau XSpeedANdFraction 
;   pour l'état de l'action en cours
;   
xSpeedAndFractionStateTable:
    .byte $00,$03,$06,$0C,$0F,$12,$13,$14,$6E

xSpeedAndFractionIdTable:
    .byte $00,$01,$08,$03,$00,$09,$01,$00,$00

;
; Table de valeurs pour objectXSpeed et objectXSpeedFraction
; MSB XSpeedFraction
; LSB XSpeed
; 
xSpeedAndFraction1:
    .db $00,$20,$21,$80,$01,$04,$15,$51,$61,$90 

playerXWidthTable
    .db $08, $08, $08, $08 ;00
	.db $08, $08, $08, $08 ;
	.db $08, $08, $08, $08 ;08
	.db $08, $08, $08, $08 ;
	.db $08, $08, $08, $08 ;10

;TableObjectYHeightTable2
playerYHeightTable:
    .db $14, $14, $14, $14 ;00
    .db $14, $14, $14, $14 ;
    .db $14, $0c, $14, $14 ;08
    .db $14, $14, $14, $14 ;
    .db $14, $14, $14, $14 ;10
    .db $14, $14, $14, $14
	.db $10, $00, $14, $14 ;18

;
; Pour metaSpritesActionXX
;     00 : Frame avant changement de state (genre perso qui cligne des yeux)
;     01-xx : une action peu avoir plusieurs metaSprite, c'est l'id du metaSprite (dataMetaSpriteXX)
metaSpritesActionTable:
	.dw metaSpritesActionStanding
	.dw metaSpritesActionStandingFiring
	.dw metaSpritesShoot4
	.dw metaSpritesActionMovingSlowing
	.dw metaSpritesActionStandingFiring
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionMovingRun
	.dw metaSpritesActionMovingRunFiring
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionJump
	.dw metaSpritesActionMovingRunFiring
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionHitGround
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionDISPO
	.dw metaSpritesActionLadder
	.dw metaSpritesActionLadderFiring
	.dw metaSpritesActionLadderTop
	.dw metaSpritesActionDeath

metaSpritesActionStanding:
	.db $a8, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

metaSpritesActionStandingFiring:
	.db $a8, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09

metaSpritesActionDISPO:

metaSpritesActionMovingSlowing:
	.db $22, $01, $01, $01

metaSpritesShoot4:
	.db $00, $0a

metaSpritesActionMovingRun:
	.db $36, $02, $03, $02, $03	

metaSpritesActionMovingRunFiring:
	.db $36, $0b, $0c, $0b, $0c	

metaSpritesActionLadder:
	.db $16, $04, $05

metaSpritesActionLadderFiring:
	.db $18, $0d, $0d

metaSpritesActionLadderTop:
	.db $00, $06, $06

; sprite du run1
metaSpritesActionJump:
	.db $00, $02

; sprite du run2
metaSpritesActionHitGround:
	.db $00, $07, $07 

metaSpritesActionDeath:
	.db $00, $08

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
	.dw dataMetaSpriteLadderTop
	.dw dataMetaSpriteHitGround
	.dw dataMetaSpriteDeath1
	.dw dataMetaSpriteStandingFire
	.dw dataMetaSpritesShoot1
	.dw dataMetaSpriteMovingRunFire1
	.dw dataMetaSpriteMovingRunFire2
	.dw dataMetaSpriteLadderFire


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
	.db $07 ; 7 sprites
	.db $03 ;offsetLadder
	.db $08, $00
	.db $09, $00
	.db $1a, $00 
	.db $1b, $00 
	.db $29, $00 
	.db $2a, $00 
	.db $3a, $00

dataMetaSpriteLadderClimb2:
	.db $07 ; 7 sprites
	.db $04 ;offsetLadder2
	.db $08, $40
	.db $09, $40
	.db $1a, $40 
	.db $1b, $40 
	.db $29, $40 
	.db $2a, $40 
	.db $3a, $40

dataMetaSpriteLadderTop:
	.db $05
	.db $03 ; Même que offsetLadder
	.db $0a, $00
	.db $0b, $00
	.db $2b, $00
	.db $2c, $00
	.db $3a, $00

dataMetaSpriteHitGround:
	.db $0c ; 12 sprites
	.db $05 ; offsetHitGround
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

dataMetaSpriteDeath1:
	.db $05; 5 sprites
	.db $06 ; offsetDeath
	.db $1c, $00
	.db $1d, $00
	.db $1e, $00
	.db $1f, $00
	.db $0d, $00

dataMetaSpriteStandingFire:
	.db $09 ; 9 sprite
	.db $00 ;offsetLanding
	.db $01, $00
	.db $02, $00
	.db $2d, $00 
	.db $2e, $00 
	.db $2f, $00 
	.db $20, $00 
	.db $21, $00 
	.db $30, $00 
	.db $31, $00

dataMetaSpritesShoot1:
	.db $01 ; 1 sprite
	.db $00 ;offsetLanding
	.db $0c, $00

dataMetaSpriteMovingRunFire1:
	.db $08 ; 8 sprits
	.db $01 ;offsetTable todo offsetMoving1
	.db $03, $00
	.db $04, $00
	.db $2d, $00 
	.db $16, $00 
	.db $24, $00 
	.db $25, $00
	.db $00, $00 
	.db $35, $00

dataMetaSpriteMovingRunFire2:
	.db $0c ; 12 sprites
	.db $02 ;offsetTable todo offsetMoving2
	.db $05, $00
	.db $06, $00
	.db $07, $00 
	.db $2d, $00 
	.db $2e, $00 
	.db $19, $00 
	.db $26, $00 
	.db $27, $00 
	.db $28, $00
	.db $36, $00
	.db $37, $00
	.db $38, $00

dataMetaSpriteLadderFire:
	.db $08 ; 8 sprites
	.db $08 ;offsetLadderFire
	.db $08, $40
	.db $09, $40
	.db $1a, $40 
	.db $3b, $40 
	.db $3c, $40  
	.db $29, $40
	.db $2a, $40 
	.db $3a, $40

offsetTable:
	.dw offsetStanding
	.dw offsetMovingSlowly ; et dataMetaSpriteMovingRun1
	.dw offsetMovingRun2
	.dw offsetLadder
	.dw offsetLadder2
	.dw offsetHitGround
	.dw offsetDeath
	.dw offsetLadder
	.dw offsetLadderFire

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
offsetLadderFire:
	.db $21, $22, $23, $24, $25, $26, $27, $28 
offsetLadder2:
    .db $0d, $0c, $0f, $0e, $11, $10, $12
offsetHitGround: 
	.db $17, $18, $19, $09, $00, $01, $02, $03, $04, $1a, $05, $1b
offsetDeath:
	.db $1c, $1d, $1e, $1f, $20

;
; Décalage de pixel
; 00 = f4
; 08 = fc
; 10 = 04
; 18 = 0c
;
;        00   01   02   03   04   05   06   07   08   09   0a   0b   0c   0d   0e   0f   10   11   12   13   14   15   16  17    18    19  1a  1b   1c    1d   1e   1f  20  21   22   23   24   25    26  27   28
offsetRightX:
   ;    $08, $00, $10, $08, $00, $08, $00, $08, $00, $10, $10, $10, $08, $00, $08, $00, $08, $00, $08, $00, $10, $08, $00, $00, 
	.db $fc, $f4, $04, $fc, $f4, $fc, $f4, $fc, $f4, $04, $04, $04, $00, $f8, $00, $f8, $00, $f8, $00, $f8, $04, $fc, $f4, $04, $fc, $f4, $04, $f4, $0c, $04, $fc, $f4, $04, $f8, $00, $f8, $00, $08, $f8, $00, $00
offsetLeftX:
	;   $08, $10, $00, $08, $10, $08, $10, $08, $10, $00, $00, $00, $00, $08, $00, $08, $00, $08, $00, $08, $00, $08, $10, $10,
	.db $fc, $04, $f4, $fc, $04, $fc, $04, $fc, $04, $f4, $f4, $f4, $f8, $00, $f8, $00, $f8, $00, $f8, $00, $f4, $fc, $04, $f4, $fc, $04, $f4, $04, $f4, $fc, $04, $0c, $fc, $00, $f8, $00, $f8, $f0, $00, $f8, $f8
offsetY:
	;   $00, $00, $08, $08, $08, $10, $10, $18, $18, $00, $10, $18, $f4, $f4, $fc, $fc, $04, $04, $0c, $0c, $00, $18, $18, xx
	.db $f4, $f4, $fc, $fc, $fc, $04, $04, $0c, $0c, $f4, $04, $0c, $f4, $f4, $fc, $fc, $04, $04, $0c, $0c, $f4, $0c, $0c, $ec, $ec, $ec, $04, $04, $0c, $0c, $0c, $0c, $04, $f4, $f4, $fc, $fc, $fc, $04, $04, $0c