;
; $00,    msb nombre d'action (commencant par 00)
;         lsb nombre frame avant changement
; $01-$xx id dataMetaSpriteEnemyTable     
metaEnemySpritesTable:
    .dw metaSpritesEnemyCrackBoy; 00
    .dw metaSpritesEnemyDispo ;01
    .dw metaSpritesEnemyDispo ;02
    .dw metaSpritesEnemyDispo ;03
    .dw metaSpritesEnemyDispo ;04
    .dw metaSpritesEnemyDispo ;05
    .dw metaSpritesEnemyDispo ;06
    .dw metaSpritesEnemyDispo ;07
    .dw metaSpritesEnemyDispo ;08
    .dw metaSpritesEnemyDispo ;09
    .dw metaSpritesEnemyDispo ;0a
    .dw metaSpritesEnemyDispo ;0b
    .dw metaSpritesEnemyDispo ;0c
    .dw metaSpritesEnemyDispo ;0d
    .dw metaSpritesEnemyDispo ;0e
    .dw metaSpritesEnemyDispo ;0f
    .dw metaSpritesEnemyDispo ;10
    .dw metaSpritesEnemyDispo ;11
    .dw metaSpritesEnemyDispo ;12
    .dw metaSpritesEnemyDispo ;13
    .dw metaSpritesEnemyDispo ;14
    .dw metaSpritesEnemyDispo ;15
    .dw metaSpritesEnemyDispo ;16
    .dw metaSpritesEnemyDispo ;17
    .dw metaSpritesEnemyDispo ;18
    .dw metaSpritesEnemyDispo ;19
    .dw metaSpritesEnemyDispo ;1a
    .dw metaSpritesEnemyKilling;1b
	.dw metaSpritesEnemyDispo ;1c
	.dw metaSpritesEnemyDispo ;1d
	.dw metaSpritesEnemyDispo ;1e
	.dw metaSpritesEnemyDispo ;1f
	.dw metaSpritesEnemyDispo ;20
	.dw metaSpritesEnemyDispo ;21
	.dw metaSpritesEnemyDispo ;22
	.dw metaSpritesEnemyDispo ;23
	.dw metaSpritesEnemyDispo ;24
	.dw metaSpritesEnemyDispo ;25
	.dw metaSpritesEnemyDispo ;26
	.dw metaSpritesEnemyDispo ;27
	.dw metaSpritesEnemyDispo ;28
	.dw metaSpritesEnemyDispo ;29
	.dw metaSpritesEnemyDispo ;2a
	.dw metaSpritesEnemyDispo ;2b
	.dw metaSpritesEnemyDispo ;2c
	.dw metaSpritesEnemyDispo ;2d
	.dw metaSpritesEnemyDispo ;2e
	.dw metaSpritesEnemyDispo ;2f
	.dw metaSpritesEnemyDispo ;30
	.dw metaSpritesEnemyDispo ;31
	.dw metaSpritesEnemyDispo ;32
	.dw metaSpritesEnemyDispo ;33
	.dw metaSpritesEnemyDispo ;34
	.dw metaSpritesEnemyDispo ;35
	.dw metaSpritesEnemyDispo ;36
	.dw metaSpritesEnemyDispo ;37
	.dw metaSpritesEnemyDispo ;38
	.dw metaSpritesEnemyDispo ;39
	.dw metaSpritesEnemyDispo ;3a
	.dw metaSpritesEnemyDispo ;3b
	.dw metaSpritesEnemyBonusB ;3c
	.dw metaSpritesEnemyBonusC ;3d
	.dw metaSpritesEnemyBonus ;3e	
	.dw metaSpritesEnemyBonus ;3F	
	.dw metaSpritesEnemyBonus ;40	
	.dw metaSpritesEnemyBonus ;41	

metaSpritesEnemyDispo:

metaSpritesEnemyBonus:
metaSpritesEnemyBonusB:
	.db $00, $05

metaSpritesEnemyBonusC:
	.db $00, $06

metaSpritesEnemyCrackBoy:
    .db $16, $00, $01

metaSpritesEnemyKilling:
	.db $52, $02, $02, $03, $03, $04, $04

dataMetaSpriteEnemyTable:
    .dw dataMetaSpriteEnemyCrackBoy1 
	.dw dataMetaSpriteEnemyCrackBoy2
	.dw dataMetaSpriteEnemyKilling1
	.dw dataMetaSpriteEnemyKilling2
	.dw dataMetaSpriteEnemyKilling3
	.dw dataMetaSpriteEnemyBonnusB
	.dw dataMetaSpriteEnemyBonnusC

;-------------------
; Récap attr:
;-------------------
; 76543210
; ||||||-- palette
; |||----- rien
; ||------ bg prio, 0=devant bg; 1= derrière bg
; |------- Flip horiontal
; -------- Flip vertical 

;---------------------------
; dataMetaSpriteEnemyXX INFO
; --------------------------
; bit   | info
;-----------
; 00    | Nombre de sprite
; 01    | Id offset
; 02/03 | Id du sprite / attr (voir ci-dessus)
; xx/xx | boucle sur 02/03 

dataMetaSpriteEnemyCrackBoy1:
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

dataMetaSpriteEnemyCrackBoy2:
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

dataMetaSpriteEnemyKilling1:
	.db $04 ; nb sprite
	.db $0b ; offset ID
	.db $40, $00
	.db $40, $40
	.db $40, $80
	.db $40, $c0

dataMetaSpriteEnemyKilling2:
	.db $04
	.db $0b
	.db $42, $00
	.db $42, $40
	.db $42, $80
	.db $42, $c0

dataMetaSpriteEnemyKilling3:
	.db $04
	.db $0b
	.db $43, $00
	.db $43, $40
	.db $43, $80
	.db $43, $c0

dataMetaSpriteEnemyBonnusB
	.db $03
	.db $0a
	.db $41, $40
	.db $f7, $00
	.db $41, $00

dataMetaSpriteEnemyBonnusC
	.db $03
	.db $0a
	.db $41, $40
	.db $f8, $00
	.db $41, $00