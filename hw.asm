; info
; https://bisqwit.iki.fi/jutut/megamansource/maincode.txt
;
; variable varFoo
; méthode MethodFoo

; HEADER 
  .inesprg 8 ; 8x16KB prg code
  .ineschr 0 ; chr ram
  .inesmap 2 ; mapper UNROM
  .inesmir 1 ; Nametable sera en mirroir en vertical
  
;-----------------------------------------------------  
; VARIABLE ET CONSTANTES
  .rsset $0000
echo .rs 1
pointer .rs 19

PPU2000value .rs 1
PPU2001Value .rs 1

tmpPosScreen .rs 1     
tmpPosXFraction .rs 1
tmpPosX .rs 1         
tmpPosY .rs 1     

stageId .rs 1
currentBank .rs 1
saveBank .rs 1
stallTimer .rs 1 ; Timer quand il n'y a rien faire
joyPad    .rs 2 ; Boutons encore pressés 
joyPadOld .rs 2 
joyD      .rs 2 ; Boutons qui viennent d'être pressés
frameCounter .rs 1
miscCounter .rs 1
nmiGfxUpdateDone .rs 1 ; update de la nmi ok
paletteUpdateDelay .rs 1
paletteParam .rs 1
palettePtr .rs 4
ppuTransferRawSize .rs 1
tsaPPUtransferSize  .rs 1
lastRestartPointType .rs 1 ; 0=level beginning; #$xx (todo nombre de niveau) = point A 

currentBeginScreen .rs 1 ; Id écran du début de scrolling
currentEndScreen .rs 1 ; Id écran de fin de scrolling
currentOrderNum .rs 1 ; l'ordre d'affichage de la room
currentRoomPointer .rs 4
currentStripeEndType .rs 1 ;0=right 1=up 2=left 3=down  

tileLadderState .rs 1 ; state pour l'échelle  (CurrentTileState)
currentTuilesState .rs 3 ; state des tuiles du dessus et en dessous
ladderPosX .rs 1

scrollPosX      .rs 1 ; nb pixel de scrollin #$00 -> #$FF
scrollPosScreen .rs 1 ; id de l'écran en cours
scrollPosY      .rs 1
screenMovedFlag .rs 1
;Bit 0: (&01) = Nouvel ennemi peut être chargé (le screen a bougé)
;Bit 6: (&40) = Mouvement 4=avant 0=arrière


forcedInputFlag .rs 1
forcedInputData .rs 1

previousEnemyIndex .rs 1
currentEnemyIndex .rs 1

bossCurrentStrategy .rs 1
bossFightingId .rs 1 ; FightingBossNum

totalObjects .rs 1
objectId .rs 1 ;(RefObjectNumber)
objectHitType .rs 1 ; ObjectReceivedHitType 

weaponMeter .rs 6
weaponSelect .rs 1
weaponFiring .rs 1
meters .rs 5 ; Life
drawMetersFlag .rs 1

playerWalkTimer .rs 1
playerStandingTimer .rs 1
playerBlinkState .rs 1 ; Timer: 0 = no blinking, #$6f = max
bossBlinkState .rs 1

activesLowerIndex .rs 1

enemySpawned .rs 16

var33 .rs 1 ; pour l'instant utilisé uniquement dans DrawBlocksScroll
varBF .rs 1 ; todo je ne sais pas à quoi ça sert


  .rsset $0100
stack .rs 256 ; stack

  .rsset $0200
sprites .rs 256  ;sprite ram

  .rsset $0300
tsaPPUTransferNTaddress .rs 2
tsaPPUTransferNTdata    .rs 16
tsaPPUTransferAttrAddress .rs 2
tsaPPUTransferAttrData  .rs 1
tsaPPUTransferRestData .rs 21 ; reste de la data de transfer

bGPalettes     .rs 16
spritePalettes .rs 16
unknownPalettes .rs 16

  .rsset $0400
; 0x20 = 32d
objectSpriteNum     .rs 32 ;400
objectFlags         .rs 32 ;420
objectActionStateCounter .rs 32 ;440  compter changement d'état d'une action; ObjectUnknown440
objectPosScreen      .rs 32 ; 460 ID écran ou se trouve l'object comment à 0
objectCurrentScreen .rs 32 ; 480
objectPosX          .rs 32 ; 4a0 Pos x de l'objet en px
objectPosXFraction  .rs 32 ; 4c0
objectXSpeed        .rs 32 ; 4e0 
objectXSpeedFraction .rs 32 ;500
objectPosY          .rs 32  ;520
objectPosYFraction  .rs 32 ; 540
objectFireDelay     .rs 32 ; 560
objectYSpeedFraction .rs 32 ; 580
objectYSpeed        .rs 32 ; 5a0
objectLifeCycleCounter .rs 32 ;5c0
objectLifeMeter     .rs 32 ; 5e0
objectType          .rs 32 ; enemi ID ; 600

ppuTransferRawAddr .rs 2
ppuTransferRawBuf .rs 126 

	.rsset $720
roomActiveTable .rs 32  ; 6 octets par bloc 


; PPU const
PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
PPUSCROLL = $2005
PPUADDR   = $2006
PPUDATA   = $2007

; DMA
OAMADDR = $2003 ; Object Attribute Memory
OAMDATA = $2004
OAMDMA  = $4014

; Stage Data
ROOM_BLOCK_DATA      = $b000
ROOM_BLOCK_PALETTE   = $bbe0
ROOM_ORDER           = $bC00
ROOM_POINTER_TABLE   = $bC30
ROOM_LAYOUT_TABLE    = $bc70 ; size = #$48
ROOM_SPRITE_PALETTE1 = $bca0
; ...
ROOM_ACTIVES1        = $bE00
ROOM_SHUTTER_BLOCK_DATA = $bf40
ROOM_SHUTTER_BLOCK_PALS = $bF70
ROOM_SHUTTER_INFO    = $bF80

; Sprite
SPRITE_TABLE = $0200 
CURRENT_SPRITE_DATA = $0204 ;(200-203 sprite 0)
PLAYER_NUMBER_STATE_MOVING = 03 ; Nombre de phase de l'action de déplacement

;-----------
; bank 01
  .include "data/level/level_1.asm"
;-----------
  .bank 4		;02
  .org $8000

  .bank 5		;02
  .org $A000

;-----------

  .bank 6		;03
  .org $8000

  .bank 7		;03
  .org $A000
		
;-----------

  .bank 8		;04
  .org $8000

  .bank 9		;04
  .org $A000

;-----------BANK 5------------------------------------------------------------------------

  .bank 10		;05
  .org $8000

; HEADER roomTileTable
;--------+----------------
;byte #  | what it tells us
;--------+----------------
;00      | Nombre de bank
;01      | Nombre de 256 bytes à copier (1 ligne de 16 tiles = 256)
;02      | Adresse début de la bank src (offset)
;xx      | Info pour les banks suivantes (comme byte 1 et 2)
;/!\ offset $04 = une ligne; $40 début bank bg

roomTileTable:
	.dw roomTileTitleScreen
	.dw roomTileStage1

roomTileTitleScreen:
	.db $04      ; Nombre de bank
	.db $10, $00 ; bank 1. 16 x 256 (16 lignes) adresse 80+00 = 8000 (début bank sprite)
	.db $03, $40 ; bank 2. 3 x 256 (3 lignes) adresse 80+40 = 9000 (début bank bg)
	.db $04, $4c ; bank 3. 4 x 256 (4 lignes) adresse 80+4c
	.db $09, $5c ; bank 4. 9 x 256 (9 lignes) adresse 80+5c
	; FIN RAM $9e2f

roomTileStage1:
	.db $01
	;.db $20, $81 ; bank1 32 x256 addresse 80+80 = A0000 (on charge tous les sprites + bg)
	.db $10, $81 ; bank1 32 x256 addresse 80+80 = A0000 (on charge tous les sprites + bg)


stikTileTable:
	.dw $0000
	.dw stikTileStage1

stikTileStage1:
	.db $01
	.db $20, $01

dataSkitTable:
	.dw 0000
	.dw dataSkit1TablePointer

dataSkit1TablePointer:
	.dw dataSkit11
	.dw dataSkit12
	.dw dataSkit13

; OPCODE:
;   $fe,$xx,$yy = $yy sera répété $xx fois
;   $fd,$xx,$yy = $yy sera incrementé $xx fois
dataSkit11:	
	.db $20,$89, $fe,$05,$45, $fd,$05,$40, $fe,$04,$45, $ff
	.db $20,$a9, $45,$45,$45, $fd,$07,$45, $fe,$04,$45, $ff
	.db $20,$c9, $fe,$04,$45, $fd,$07,$4c, $45,$45,$45, $ff
	.db $20,$e9, $45,$45,$45, $fd,$08,$53, $45,$45,$45, $ff
	.db $21,$09, $fe,$04,$45, $fd,$07,$5b, $45,$45,$45, $ff
	.db $21,$29, $fe,$05,$45, $fd,$06,$62, $45,$45,$45, $ff
	.db $21,$49, $fe,$06,$45, $fd,$07,$69, $45, $ff
	.db $21,$69, $fe,$07,$45, $fd,$06,$70, $45, $ff
	.db $21,$89, $fe,$07,$45, $fd,$07,$77, $ff
	.db $21,$a9, $fe,$07,$45, $fd,$07,$7e, $ff

dataSkit12:
	.db $24,$89, $fe,$0e,$90, $ff
	.db $24,$a9, $fe,$0e,$90, $ff
	.db $24,$c9, $fe,$0e,$90, $ff
	.db $24,$e9, $fe,$08,$90, $91, $fe,$05,$90, $ff
	.db $25,$09, $fe,$06,$90, $92,$93,$94, $fe,$05,$90, $ff
	.db $25,$29, $fe,$06,$90, $95,$96, $fe,$06,$90, $ff
	.db $25,$49, $fe,$04,$90, $fd,$05,$97, $fe,$05,$90, $ff
	.db $25,$69, $fe,$04,$90, $fd,$06,$9c, $fe,$04,$90, $ff;
	.db $25,$89, $90,$90,$90, $fd,$04,$a2, $fd,$05,$a5, $90,$90, $ff
	.db $25,$a9, $90,$90,$90, $aa,$ab,$ac,$9e, $fd,$05,$ad, $90,$90, $ff

dataSkit13:
	.db $20,$89, $fe,$0e,$c0, $ff
	.db $20,$a9, $fe,$05,$c0, $fd,$05,$c1, $fe,$04,$c0, $ff
	.db $20,$c9, $fe,$05,$c0, $fd,$05,$c6, $fe,$04,$c0, $ff
	.db $20,$e9, $fe,$05,$c0, $fd,$05,$cb, $fe,$04,$c0, $ff
	.db $21,$09, $fe,$05,$c0, $fd,$05,$d0, $fe,$04,$c0, $ff
	.db $21,$29, $fe,$05,$c0, $fd,$05,$d5, $fe,$04,$c0, $ff
	.db $21,$49, $fe,$06,$c0, $fd,$04,$da, $fe,$04,$c0, $ff
	.db $21,$69, $fe,$05,$c0, $fd,$05,$de, $fe,$04,$c0, $ff
	.db $21,$89, $fe,$0e,$c0, $ff
	.db $21,$a9, $fe,$0e,$c0, $ff

skitTextTable:
	.dw $0000
	.dw skitText1TablePointer

skitText1TablePointer:
	.dw textSkit11
	.dw textSkit12
	.dw textSkit13

; OPCODE:
;   $ff fin de linge
;   $fe  fin de text   
;   /!\ max 256 par texte                             
textSkit11:
	.db $05,$04,$15,$14,$05,$04,$00,$15,$14,$00,$10,$05,$12,$13,$10,$09,$03,$09,$01,$14,$09,$13,$09,$13, $ff
	.db $15,$0e,$04,$05,$00,$0f,$0d,$09,$13,$00,$09,$13,$14,$05,$00,$0e,$01,$14,$15,$00,$00,$00,$00,$00, $ff
	.db $15,$0e,$04,$05,$00,$0f,$0d,$09,$13,$00,$09,$13,$14,$05,$00,$0e,$01,$14,$0e,$01,$14,$15,$00,$00, $ff
	.db $15,$0e,$04,$05,$00,$0f,$0d,$09,$13,$00,$09,$13,$14,$05,$00,$0e,$01,$14,$00,$00,$00,$00,$00,$00, $ff
	.db $15,$0e,$04,$05,$00,$0f,$0d,$09,$13,$00,$09,$13,$14,$05,$00,$0e,$01,$00,$00,$00,$14,$15,$10,$00, $ff
	.db $15,$0e,$04,$05,$00,$0f,$0d,$09,$13,$00,$09,$13,$14,$05,$00,$0e,$01,$14,$00,$00,$00,$00,$00,$00, $ff
	.db $15,$0e,$04,$05,$00,$0f,$0d,$09,$13,$00,$09,$13,$14,$05,$00,$0e,$01,$14,$04,$04,$06,$0b,$00,$00, $ff
	.db $15,$0e,$04,$05,$00,$0f,$0d,$09,$13,$00,$09,$13,$14,$05,$00,$0e,$01,$14,$00,$00,$00,$00,$00,$00, $ff
	.db $15,$0e,$04,$05,$00,$0f,$0d,$09,$13,$00,$09,$13,$14,$05,$00,$0e,$01,$14,$06,$00,$00,$14,$14,$06, $ff
	.db $15,$0e,$04,$05,$00,$0f,$0d,$09,$13,$00,$09,$13,$14,$05,$00,$0e,$01,$14,$00,$00,$00,$00,$00,$00, $ff
	.db $15,$0e,$04,$05,$00, $fe

textSkit12:
	.db $01,$02,$03,$04,$00,$00,$01,$02,$03,$04,$00,$00,$01,$02,$03,$04,$00,$00,$01,$02,$01,$02,$03,$04, $ff
	.db $17,$18,$00,$17,$18,$00,$17,$18,$00,$17,$18,$00,$17,$18,$00,$17,$18,$00,$17,$18,$00,$00,$17,$18, $ff
	.db $19,$1a,$1b,$00,$19,$1a,$1b,$00,$19,$1a,$1b,$00,$19,$1a,$1b,$00,$19,$1a,$1b,$00,$19,$1a,$1b, $ff
	.db $26,$27,$00,$26,$27,$00,$00,$26,$27,$00,$26,$27,$00,$26,$27,$00,$26,$27,$00,$26,$27,$00,$26,$27, $ff
	.db $0a,$0b,$0c,$00,$00,$0a,$0b,$0c,$00,$00,$0a,$0b,$0c,$00,$0a,$0b,$0c,$0a,$0b,$0c,$00,$0a,$0b,$0c, $ff
	.db $15,$16,$17,$00,$15,$16,$17,$00,$15,$16,$17, $ff
	.db $1b,$1c,$1d,$1e,$1f, $ff
	.db $06,$07,$08,$09,$00,$00,$06,$07,$08,$09,$00,$00,$06,$07,$08,$09,$00,$06,$07,$08,$09, $fe

textSkit13:
	.db $10,$09,$05,$12,$12,$05,$00,$10,$01,$0c,$0d,$01,$04,$05,$00,$10,$09,$05,$12,$12,$05,$00,$10,$01, $ff
	.db $0c,$0d,$01,$04,$00,$10,$09,$05,$12,$12,$05,$00,$10,$01,$0c,$0d,$01,$04,$05,$00,$10,$09,$05,$12, $ff
	.db $10,$09,$05,$12,$12,$05,$00,$10,$01,$0c,$0d,$01,$04,$05,$00,$00,$10,$09,$05,$12,$12,$05,$ff
	.db $10,$09,$05,$12,$12,$05,$00,$10,$01,$0c,$0d,$01,$04,$05,$00,$00,$10,$09,$05,$12,$12,$05,$ff
	.db $10,$09,$05,$12,$12,$05,$00,$10,$01,$0c,$0d,$01,$04,$05,$00,$00,$00,$00,$10,$09,$05,$12,$12,$05, $ff
	.db $10,$09,$05,$12,$12,$05,$00,$10,$01,$0c,$0d,$01,$04,$05,$00,$00,$00,$10,$09,$05,$12,$12,$05,$ff
	.db $10,$09,$05,$12,$12,$05,$00,$10,$01,$0c,$0d,$01,$04,$05,$00,$00,$00,$00,$10,$09,$05,$12,$12,$05, $fe


	
  .bank 11		;05
  .org $A000

; $00 id screen
; $01 x pos
; $02 y pos
; $03 id type
enemyDataTable:
	.dw enemyDataLvl00
	.dw enemyDataLvl01

enemyDataLvl00:
enemyDataLvl01:
	.db $01, $10, $4c, $00

; voir info.txt
defaultEnemyFlags:
	.db $06

defaultEnemySpeedTable:
	.db $00

;DefaultObjectFireDelay
defaultEnemyFireDelay:
	.db $00

;-----------BANK 6------------------------------------------------------------------

  .bank 12		;06
  .org $8000

Reset2:
	.wait1:       ; On attend 1 frame
		BIT PPUSTATUS
		BPL .wait1
		
		.clrmem:
			LDA #$00
			STA $0000, x
			STA $0100, x
			STA $0200, x
			STA $0300, x
			STA $0400, x
			STA $0500, x
			STA $0600, x
			STA $0600, x
			STA $0700, x
			INX
			BNE .clrmem
			
	.wait2:      ; Encore 1 frame
		BIT PPUSTATUS
		BPL .wait2
		
	lda #12
	sta saveBank
	
	lda #$10      
	sta PPU2000value
	lda #$06         ;Pas de clipping (affiche sprite et bg sur 8 pixels à gauche)
	sta PPU2001Value 
	
	; On met les chr en RAM
	lda #$00
	sta stageId
	sta pointer+2
	lda #$0
	sta pointer+3
	jsr WriteChr
	
	; Init stage palette
	jsr InitStagePaletteAndActive

	; $06 = nombre de ligne
	lda #$09
	sta $06
	ldx #$ff
	
	.newLine:
	dec $06
	bmi .endLoopDataTitle
	inx 
	lda dataTitle, x
	sta PPUADDR
	inx 
	lda dataTitle, x
	sta PPUADDR
	inx
	.loopDataTitle:
		lda dataTitle, x
		cmp #$ff
		beq .newLine
		sta PPUDATA
		inx
		bne .loopDataTitle
	.endLoopDataTitle:		
	
	; Attribute
	; juste pour le Press Start, le reset sera sur la palette 0
	lda #$23
	sta PPUADDR
	lda #$ea
	sta PPUADDR
	ldx #04
	.loop6:
		lda #$ff
		sta PPUDATA
		dex
		bne .loop6
	
	lda #$90
	sta PPUCTRL
	
	lda #$1E ; enable sprites, enable background
	sta PPUMASK
	
	.waitSelect:
		lda PPU2000value
		ora #$80
		sta PPU2000value
		sta PPUCTRL
		
		.wait:
			jsr NextFrame
			; Si start pressé
			lda joyPad
			cmp #08
			bne .wait
		
		jsr PaletteSetupForBGwith3F0
		lda #$78 ; (120 / 60 frames = 2s)
		sta paletteUpdateDelay
		lda #$80
		sta miscCounter
		.startFlickering:	
			jsr NextFrame
			dec miscCounter
			bne .startFlickering

	; On supprime le press start
	lda #$2a
	sta PPUADDR
	lda #$ca
	sta PPUADDR
	ldx #$c
	.loopEraseStart:
		lda #0
		sta PPUDATA
		dex
		bpl .loopEraseStart

	; init stage 1
	lda #1
	sta stageId

; Scénette de début de stage
SkitStage:
	; Turn off PPU
	jsr DisableNMIPPU

	; Charge tiles pour les skits en RAM
	;   stageId est déjà setté
	lda #$1 ;pour skit
	sta pointer+2
	lda #$0
	sta pointer+3
	jsr WriteChr

	; Charge 2 Nametables en PPU (2 skits)
	;    cadre pour les deux Nametables
	lda #$20
	sta pointer+2
	lda #$21
	sta pointer+3
	jsr SkitStageDrawFrame

	lda #$24
	sta pointer+2
	lda #$25
	sta pointer+3
	jsr SkitStageDrawFrame

	; palette Skit data todo
	; palette Skit text todo
	jsr SkitStagePalette

	lda #12
	sta saveBank
	ldy stageId

	; Charge title dans les deux Nametables	
	lda #0
	sta $03 ; init save Y
	sta $06 ; init position courante ligne
	sta $08 ; init diver (loop i repeatData ou inc data incData)
	lda #01
	sta $0c ; charge skit 1 nametable 24xx
	lda #$0a
	sta $07

	.loopDataTile:
		jsr WriteSkitChr
		lda ppuTransferRawSize
		beq .dontTransfer
		jsr PPUTransferRaw
		.dontTransfer:
		lda $08
		cmp #$ff
		bne .loopDataTile
		lda #0
		sta $08
		dec $07
		bne .loopDataTile
		lda #$0a
		sta $07 ; on remet 10 ligne au compteur
		lda #0
		sta $03; reset Y
		dec $0c ; charge skit 0 namateble 20xx
		bpl .loopDataTile
	
	; init skitText
	lda #0 ; id skitTable (ex: textSkit11 = 0)
	sta $03

	; Turn on PPU
	lda PPU2000value
	ora #$80
	sta PPU2000value
	sta PPUCTRL

	;jsr SkitText
	
	; Turn off PPU
	jsr DisableNMIPPU

	; Charge tiles pour le stage
	;   stageId est déjà setté
	lda #$0 ;pour skit
	sta pointer+2
	lda #$1
	sta pointer+3
	jsr WriteChr

StageBegin:
	; to do palette
	jsr InitStagePaletteAndActive

; Chargement des deux NT
StageBeginFromDeath:
	; On commence par la NT 24
	clc
	lda lastRestartPointType ; 0 = début stage ; #$0xx (todo nombre de niveau) point A
	adc stageId
	pha
	tax
	lda firstScreenScreenTable, x
	sta currentBeginScreen
	pha
	lda lastRestartPointType
	beq .beginningStage
	; todo point de respawn
	.beginningStage:
	; update Background tiles
	lda lastRestartPointType
	clc
	adc #$01
	sta scrollPosScreen
	jsr ScrollProcess
	pla
	sta objectCurrentScreen
	jsr ScrollProcess
	; update ennemi
	pla
	tax ; x = stageId
	lda firstScreenEnemyPointer, x
	sta previousEnemyIndex
	clc
	adc #$01
	sta currentEnemyIndex
	; update room
	lda firstScreenScreenTable, x
	sta currentOrderNum
	tay
	jsr RoomLayoutLoadRoomNum
	and #$1F
	clc
	adc currentBeginScreen
	sta currentEndScreen
	lda objectCurrentScreen
	sta scrollPosScreen
	lda #$00
	sta scrollPosX
	sta weaponSelect
	sta bossCurrentStrategy
	sta objectLifeMeter + 1

	lda lastRestartPointType
	beq .next
	; todo truc à faire pour le checkpoint 

	.next:

	ldy #$00
	ldx #$40
	jsr HideSprites
	lda #$20 ; max 20 objets
	sta totalObjects
	jsr ForgetRoomObjects

	lda #$01
	sta screenMovedFlag

	; Todo play sound stage

	lda stageId
	bne .noEndStage
	.noEndStage:

	; INIT player
	; 	metter life
	lda #$1c
	sta meters
	; Sprite
	lda #$00
	sta playerBlinkState
	sta objectSpriteNum ; player stand
	sta objectFireDelay ; init fire 0
	lda #$40 ; Face droite
	sta objectFlags
	; Position
	lda #$80 ; milieu de l'écrant "0x80 = 128d (32x8)/2"
	sta objectPosX
	;sta $22 ; posX Current du player ?
	lda #$AB ;
	sta objectPosY
	lda #$ff
	sta objectXSpeed
	sta objectYSpeed

	jsr UpdateGraphics

	; Turn on PPU
	lda PPU2000value
	ora #$80
	sta PPU2000value
	sta PPUCTRL
	sta nmiGfxUpdateDone
	jsr NextFrame

MainLoop:
	jsr RecalculateActivesLowerIndex
	jsr PlayerIA
	lda playerWalkTimer
	; todo gestion du run

	jsr RunBossIA
	jsr RunCollisionChecks ; /!\ jusqu'a LoadEnemies $00 n'est plus settable
	;jsr RunWeaponIA
	jsr RunObjectIA
	jsr LoadEnemies ; ok pour set $00 
	jsr UpdateGraphics

	; scrolling vertical
	lda currentStripeEndType
	bne CheckStripeEnding

	MainLoopEndCurrentFrame:
		jsr NextFrame
		JMP MainLoop

RunCollisionChecks:
	sec
	lda objectPosX
	sbc scrollPosX
	sec
	sbc #$07
	sta $00

	rts

; A = currentStripeEndType (0=right 1=up 2=left 3=down)  
CheckStripeEnding:
	cmp #$01
	bne .process
	; todo teleport
	;lda TeleportEnteredFlag

	lda objectFlags
	and #$10 ; si sur une échelle
	beq .next 
	.onLadder:
		lda currentStripeEndType
	.process:	
		ldx scrollPosX
		bne .next
		ldx scrollPosScreen ; id écran
		cpx currentBeginScreen  ; si écran du début
		bne .goUp
	;.goDown:
		ldy currentOrderNum ; le masque de la room précédente
		dey
		jsr RoomLayoutLoadRoomNum
		ldy currentStripeEndType ;0=right 1=up 2=left 3=down 
		and shutter2Table, y
		bne ScrollPreviousRoom
	.goUp:
		cpx currentEndScreen ; si écran de fin
		bne .next ; non
	    ldy currentOrderNum ; id écran courrant layout
		jsr RoomLayoutLoadRoomNum; retourne A = id ROOM_LAYOUT_TABLE
		ldy currentStripeEndType ; ;0=right 1=up 2=left 3=down  
		and shutterTable, y
		bne ScrollNextRoom
	.scroll:
		ldx scrollPosX
	.next:
		; pas sur une échelle
		lda currentStripeEndType
		cmp #$03
		bne CheckStripeEndingEnd
		lda #$00
		sta currentStripeEndType
		lda #$f8
		sta objectPosY
		jmp KillPlayer		
		
	CheckStripeEndingEnd:
		lda #$00
		sta currentStripeEndType
		jmp MainLoopEndCurrentFrame

; Scrolling écran précédent 
ScrollPreviousRoom:
	dex 
	stx currentEndScreen
	dec currentOrderNum
	ldy currentOrderNum
	jsr RoomLayoutLoadRoomNum
	and #$1f
	sta currentBeginScreen
	lda currentEndScreen
	SEC
	sbc currentBeginScreen
	sta currentBeginScreen
	; jsr ForgetRoomObjects
	lda currentEndScreen
	jsr DrawOneScreen
	dec objectPosScreen
	; todo enemy graphics
	lda currentStripeEndType
	cmp #$04
	php 
	bne .doScroll
		dec scrollPosScreen
	.doScroll:
		jsr DoScrolling
	PLP
	beq .nextFrame
		dec scrollPosScreen
	.nextFrame:
		jsr NextFrame
	lda currentEndScreen
	SEC
	sbc #$01
	jsr DrawOneScreen
	lda #$01
	sta screenMovedFlag
	lda #$00
	sta currentStripeEndType
	jmp MainLoopEndCurrentFrame

; Scrolling écran suivant
ScrollNextRoom:
	; todo supprimer object de la room courrante
	;jsr  ForgetRoomObjects
	ldx currentEndScreen ; ID écran 
	inx ; up de la currentEndScreen
	txa
	pha
	jsr DrawOneScreen

	; Check permier porte	
	ldx stageId
	lda firstDoorLocations, x
	cmp objectPosScreen;
	bne .bossDoor
	; .firstDood:
		; todo update palette
		jsr OpenFirstDoor
	.bossDoor:
		; la fermeture de la boss door se fait RunBossIA
		; 	avec le lancement de la music 
		ldx stageId
		stx bossFightingId
		lda #$70
		sta miscCounter
		lda objectPosScreen
		cmp bossDoorLocations, x
		bne .runScrolling
		lda #$01
		sta bossCurrentStrategy
	.runScrolling:
	inc objectPosScreen
	; todo gestion ennemi

	lda currentStripeEndType
	cmp #$04 ; max 5 rooms de suite
	php
	bne .doScrolling
		inc scrollPosScreen
	.doScrolling:
		jsr DoScrolling
	plp
	beq .nextFrame
	inc scrollPosScreen
	.nextFrame:
		jsr NextFrame
	lda currentEndScreen
	clc
	adc #$02 ; on draw la room suivante pour préparer le scroll
	jsr DrawOneScreen 
	inc currentOrderNum
	ldy currentOrderNum
	jsr RoomLayoutLoadRoomNum
	pha
	and #$e0
	bne .move 
		; todo gestion room boss
	.move:
	pla 
	and #$1f
	sta currentBeginScreen
	pla
	TAX
	CLC
	adc currentBeginScreen
	sta currentEndScreen
	stx currentBeginScreen
	lda #$00
	sta joyPad
	sta joyPadOld
	lda #$41
	sta screenMovedFlag
	;lda teleportEnteredFlage

	jmp CheckStripeEndingEnd

;bank5_938B_table
shutter2Table: ;right, up, left, down. down=up, up=down
    .db $00, $40, $00, $80
;bank5_938F_table
shutterTable:
    ; shutter=right, up=up, shutter=left, down=down
    .db $20, $80, $20, $40, $00



PlayerIA:
	ldx #$00
	stx objectId

	; Stop le perso
	lda objectFlags
	and #$7f
	sta objectFlags

	and #$0f
	beq .checkClimbing
	; todo perso est entrain de: 
	;     quitter une echelle
	;     lancer quelque chose en standing
	;     se faire toucher 
	
	.checkClimbing:
		; Entrain de monter une échelle ?
		lda objectFireDelay
		and #$f0
		cmp #$20
		beq PlayerIACheckMoving
		lda objectFlags
		and #$10 ; début d'action
		bne .ladderHandler
		lda tileLadderState
		and #$7f ; check si échelle est dans le coin
		beq PlayerIACheckMoving
		; si echelle on regarde le pad directionnelle
		lda joyPad
		and #$30 ; up/down
		beq PlayerIACheckMoving
		ora tileLadderState
		cmp #$11 ; up + tuile état 1 = 0001 0001
		beq PlayerIACheckMoving
		cmp #$2e ; down + tuile état e = 0010 1110
		beq PlayerIACheckMoving
		jmp LadderInit
	.ladderHandler:
		jmp LadderHandler
	
	PlayerIACheckMoving:
	; Deplacement
	lda playerWalkTimer
	beq .dontWalking
	; todo walking
	rts

	.dontWalking:
		lda #$00
		sta playerStandingTimer
		sta playerWalkTimer
		
	
	; Si gauche/droite est appuyé pendant cette frame
	;  
	lda joyPad ; Touche pressée
	and #$c0
	beq .handleSpeed ; 
	lda joyPadOld ; Si Ancienne touche pressée
	and #$c0      ;     On est déjà en mouvement  
	bne .handleFacing
	; Nouveau déplacement init de l'action
	;   player move doucement
	lda #$03 ; action état mouvement lent
	sta objectSpriteNum
	LDA #$00
	sta objectActionStateCounter
	sta objectFireDelay
	
	.handleFacing:
		jsr SetPlayerFacing
		ora #$80   ; Met le perso en mouvement
		sta objectFlags

	.handleSpeed:
		jsr HandleSpeed

	; Update mouvement
	lda $01 ; tmp facing
	and #$40 ; droite
	beq .facingLeft
	; facingRight
	jsr ObjectUpdateMovementRight
	jmp .autoCenterScreen

	.facingLeft:
		jsr ObjectUpdateMovementLeft

	.autoCenterScreen:
		jsr AutoCenterScreen
		jsr UpdateCurrentTileState
	
	.pressB: ; shoot 
		lda joyD
		and #$02             ; si press b
		beq .checkSpeed      ; non
		jsr PlayerWeaponFire ; oui

	.checkSpeed:
		lda objectYSpeed
		bmi .speedMi     ; si FF
		jsr ObjectDoCollisionChecksAndAvoidWalls
		bcs .goEnd
		lda joyPad
		and #$01 ; continue press A jump
		beq	.fallingJump
		jmp .continueJump

	.goEnd:
		jmp .end

	.fallingJump:
		lda objectYSpeed
		bmi .goJump
		cmp #$01
		bcc .goJump
		beq .goJump
		lda #$01
		sta objectYSpeed
		lda #$00
		sta objectYSpeedFraction

	.goJump:	
		jmp .continueJump

	.speedMi:
		; voir le retour de TestReding
		lda #$00
		pha
		jsr ObjectDoCollisionChecksAndAvoidWalls
		bcs .jumpInit
		pla
		jmp .continueJump
		; collision

	.jumpInit: ; todo mal nommé genre checkJump
		pla 
		lda objectSpriteNum
		cmp #$09 ; saut/chute
		beq .jumpSound  ; TODO jumping/falling
		cmp #$6f ; shoot en jump
		bne .jumpCheckPosY
		lda #$09
		sta objectSpriteNum

	.jumpSound:
		; lda sound id
		; jsr playsound

	.jumpCheckPosY:
		lda tmpPosY
		cmp objectPosY
		bcs .setTmpPosY
		;check shake player

	.setTmpPosY:	
		lda objectPosY
		sta tmpPosY
	
	; Mouvement si A n'est pas pressé
	lda joyD
	and #$01
	bne .makeJump
	
	;si player n'est pas en mouvement
	lda objectFlags
	and #$80
	bne .inMove 

	; et si touche droite/gauche est pressée à la frame précédente
	lda joyPadOld
	and #$c0
	bne .running ; true

	; si left/right n'est pas pressé dans la frame d'avant
	lda objectSpriteNum
	cmp #$09 ; jump / falling
	bne .goEnd1
	; si en l'air
	lda #$00
	lda #$0f
	bne .setSpriteNum

	.running: 
		lda objectSpriteNum
		cmp #$06 ; running
		bne .goEnd2    
		; on fait ralentir le player
		lda #$0C ; slow down

	.setSpriteNum:
		sta objectSpriteNum
		lda #$01
		sta objectActionStateCounter

	.goEnd1:	
		; from 95D5: was not in a jumping state
		lda objectActionStateCounter
		bne .end ; END

	.goEnd2:
		lda #$00 ; standing
		sta objectSpriteNum
		sta objectActionStateCounter
		beq .end
	
	.inMove: 
		lda objectSpriteNum
		cmp #$09 ; saut/chute
		beq .makeRun ; true
		cmp #$03 ; mouvement lent
		bne .end ; false
		lda objectActionStateCounter
		cmp #$22
		bne .end ; false

	.makeRun:
		lda #$06
		sta objectSpriteNum

	.makeRunningEnd:
		lda #$00 ; standing
		sta objectActionStateCounter
		rts ; END

	.makeJump:
		lda #$04
		sta objectYSpeed
		lda #$df
		sta objectYSpeedFraction
		rts

	.continueJump:
		lda #$09
		sta objectSpriteNum
		lda #$00
		sta objectActionStateCounter
		
	.end:
		rts

RunBossIA
	lda bossCurrentStrategy
	cmp #$01
	bcs .run
	lda #$00
	sta objectLifeMeter+1 ; vie du boss à zéro
		rts 
	.begin: 
		jmp BossIABegin
	.run:
		bne .begin ; si c'est >= 01  

	lda stageId
	cmp bossFightingId ; FightingBossNum
	jsr CloseBossDoor
	inc bossCurrentStrategy
	rts

BossIABegin:
	
	rts

; /!\ doit être suivi de ObjectShouldBeDestroyed ne pas déplacer /!\
RunObjectIA:
	ldx #$02     ; on commence par les shoots
	stx objectId
	RunObjectIA_loopObject:
		ldx objectId
		lda objectPosY, x ; objet est actif ?
		cmp #$f8
		bne .checkSprite ; oui
		jmp RunObjectIA_nextObject ; non, object suivant
		.checkSprite:
			lda objectSpriteNum, x
			cmp #$ff
			bne .shoot
			; todo ici DoEnemyIA
			; jmp .nextObject
		.shoot:
			lda objectLifeCycleCounter, x ; object actif ?
			bne .checkIfOutScreen ; non
			            ; oui
			; todo plein de truc
		.updateMouvementRight:
			lda objectFlags, x
			and #$40
			beq .updateMouvementLeft
			jsr ObjectUpdateMovementRight
			jmp .relocateHorizontally
		.updateMouvementLeft:
			jsr ObjectUpdateMovementLeft
		.relocateHorizontally:
			jsr ObjectRelocateHorizontally	
		.checkIfOutScreen:
			sec
			lda objectPosX, X
			sbc scrollPosX
			lda objectPosScreen, x
			sbc scrollPosScreen
			beq .checkDestroy ; si pas > 0 c'est out of screen
			lda #$01
			jmp ObjectShouldBeDestroyed
		.checkDestroy:
			; jmp ObjectShouldBeDestroyed
	RunObjectIA_nextObject:
		inc objectId ; on passe à l'objet suivant
		lda totalObjects
		cmp objectId ; est-ce que c'était le dernier object ?
		beq .end ; oui fin
		jmp RunObjectIA_loopObject ; non go début de la loop

	.end:

	rts

; /!\ doit suivre RunObjectIA ne pas déplacer /!\
; A = Hit type
;   0 = hit because ObjectFlags had bit #8 clear and Unknown440=0
;   1 = scrolled out from screen
;   3 = $2A was nonzero (at $9961)
;   4 = dropped in a pit (ObjectCheckIfOutScreenVertically returned carry set)
;   5 = hit by weapon
;
ObjectShouldBeDestroyed
	sta objectHitType
	ldy #$2d          ; ObjectHitRoutineTable 15(1 db + 1 wd) = 45 ou 0x2d 
	ldx objectId
	lda objectSpriteNum, X
	.loop:
		cmp objectHitRoutineTable, y
		beq .selectedDestroyRoutine
		dey ; une ligne = 3 db
		dey
		dey
		bpl .loop
		
		ldy #$00 ; si rien de trouvé la default est selectionnée

	.selectedDestroyRoutine:
		LDA objectHitRoutineTable+1,y
		sta $00
		LDA objectHitRoutineTable+2,Y
		sta $01
		jmp [$00]

;
; .db xx   = 1 id du sprite
; .dw xxxx = routine de destroye de l'object 
; 
objectHitRoutineTable:
    .byte $19 ; default id
	.word Sprite19ShouldBeDestroyedAndDefault ; default routine
    .byte $27
	.word Sprite27ShouldBeDestroyedAndDefault
    .byte $32
	.word Sprite19ShouldBeDestroyedAndDefault
    .byte $36
	.word Sprite19ShouldBeDestroyedAndDefault
    .byte $4C
	.word Sprite19ShouldBeDestroyedAndDefault
    .byte $4D
	.word Sprite19ShouldBeDestroyedAndDefault
    .byte $4E
	.word Sprite19ShouldBeDestroyedAndDefault
    .byte $4F
	.word Sprite19ShouldBeDestroyedAndDefault
    .byte $50
	.word Sprite19ShouldBeDestroyedAndDefault
    .byte $51
	.word Sprite19ShouldBeDestroyedAndDefault
    .byte $52
	.word Sprite19ShouldBeDestroyedAndDefault
    .byte $53
	.word Sprite19ShouldBeDestroyedAndDefault
    .byte $39
	.word Sprite19ShouldBeDestroyedAndDefault
    .byte $5A
	.word Sprite19ShouldBeDestroyedAndDefault
    .byte $6C
	.word Sprite19ShouldBeDestroyedAndDefault
    .byte $5D
	.word Sprite19ShouldBeDestroyedAndDefault


Sprite19ShouldBeDestroyedAndDefault:
	lda objectHitType
	cmp #05
	bne DeleteObjectThatWasScrolledOut

DeleteObjectThatWasScrolledOut 
	ldx objectId
	lda #$f8
	sta objectPosY, x
	jmp RunObjectIA_nextObject


Sprite27ShouldBeDestroyedAndDefault:
	; todo
	jmp Sprite19ShouldBeDestroyedAndDefault

RunWeaponIA:
	; todo check object colision
	lda weaponSelect
	asl A
	tay
	lda weaponIATable, y
	sta $00
	lda weaponIATable, y
	sta $01
	jmp [$00]

weaponIATable:
	.dw weaponIAA
	.dw weaponIAB

weaponIAA:
	ldx #$02
	lda #$05
	; got WeaponIATestShotHit
	; /!\ ne pas bouger /!\

; A = id objet max (ex pour A c'est 5 car id min 2 + 3 shoot en même temps)
; X = id objet mini (ex pour A 2)
WeaponIATestShotHit:
	rts

weaponIAB:
	rts

PlayerWeaponFire:
	lda weaponSelect 
	tax
	asl A
	TAY
	beq .selectWeapon ; le premier weapon (coup de poing) n'a pas besoin de meter
	lda weaponMeter, x
	bne .selectWeapon
	rts ; Si plus de jus pour le weapon on fait rien
	.selectWeapon:
		lda weaponSelectTable, y ; table de word
		sta $00
		lda weaponSelectTable + 1, Y
		sta $01
		jmp [$00] ; jump WeaponFireXX

weaponSelectTable:
	.dw WeaponFireHand

WeaponFireHand:
	ldx #$02 ; les shoots commence au troisième objet
	.loopShoot:
		lda objectPosY, x
		cmp #$f8 ; #$f8 = objet dispo
		beq .launch
		inx 
		cpx #$05 ; max 3 shot en même temps sur l'écran
		bne .loopShoot
		beq .end ; aucun de dispo fin
	.launch:	
		lda #$04 ; type de weapon
		jsr LaunchWeaponShot
		lda #$1f
		sta objectFireDelay ; delai avant shoot (player)

		; todo sound

		lda #$c0
		sta weaponFiring

	.end:
		ldx objectId
	rts

; X = objet id
; A = fire type
;     0 = ??
;     1 = ??
;     2 = ??
;     3 = ??
;     4 = hand
;     5 = ??
;     6 = ??
LaunchWeaponShot:
	sta $02 ; *9 ; todo voir pour passer à 8
	asl a ;*2
	asl a ;*4
	asl a ;*8
	clc
	adc $02 ; ajout de $02 pour faire *9
	tay
	lda objectPosY, x
	cmp #$f8 ; dispo, voir si besoin de ce check
	beq .initFire
	sec ; sinon fin
	rts

	.initFire:
		lda weaponFireData, y ; sprite
		sta objectSpriteNum, x
		lda weaponFireData + 1, y ; counter de changement de metasprite
		sta objectActionStateCounter, x
		lda objectFlags ; direction du perso
		and #$40 ; ?droite
		php ; save du sens
		ora weaponFireData + 2, y ; sens du perso + autres flags 
		sta objectFlags, x
		; init divers
		lda #$00
		sta objectLifeCycleCounter, x
		sta objectFireDelay, x
		sta objectPosXFraction, x
		sta objectPosYFraction, x

		lda weaponFireData + 3, y ; y speed fraction
		sta objectYSpeedFraction, x
		lda weaponFireData + 4, y ; y speed
		sta objectYSpeed, x
		lda weaponFireData + 5, y ; x speed fraction
		sta objectXSpeedFraction, x
		lda weaponFireData + 6, y ; x speed
		sta objectXSpeed, x
		clc            
		lda objectPosY            ; on ajout le y du perso
		adc weaponFireData + 7, y 
		cmp #$f8 ; si pas en dehors de l'écran
		bne .setPos
		lda #$f9

	.setPos:
		sta objectPosY, x
		lda objectPosX
		plp ; restore direction perso
		beq .gauche
	; droite:
		clc
		adc weaponFireData + 8, y
		sta objectPosX, x
		lda objectPosScreen
		adc #$00 ; juste add carry
		jmp .end
	.gauche:
		sec
		sbc weaponFireData + 8, y
		sta objectPosX, X
		lda objectPosScreen
		sbc #$00 ; juste substract carry
	.end:
		sta objectPosScreen, x
		clc
	rts 

; 0->ObjectSpriteNum (metaSpritesActionTable)
; 1->objectActionStateCounter
; 2->ObjectFlags
; 3->ObjectYSpeedFraction
; 4->ObjectYSpeed
; 5->ObjectXSpeedFraction
; 6->ObjectXSpeed
; 7->ObjectPosY
; 8->ObjectPosX
weaponFireData:
    .db $60,$00,$04,$00,$00,$00,$04,$00,$10 ;f shoot
    .db $61,$00,$04,$00,$00,$00,$00,$EC,$00 ;f shield (?)
    .db $00,$00,$24,$00,$00,$00,$00,$F0,$10 ;guts debris
    .db $6C,$00,$15,$00,$03,$A0,$03,$F0,$10 ;guts block being carried(?)
    .db $02,$00,$00,$00,$00,$00,$04,$06,$10 ;P


LadderInit:
	; init du flag
	lda objectFlags
	ora #$10 ; set bit5 pour le début d'action
	EOR #$40 ; chengmeent de direction
	sta objectFlags
	; init de l'action
	lda #$00
	sta objectActionStateCounter
	sta objectFireDelay
	lda ladderPosX 
	adc #$08
	sta tmpPosX ; tmp objectPosX
	lda objectPosScreen
	sta tmpPosScreen ; tmp objectPosScreen
	jsr AutoCenterScreen

	
; Ladder_Handler
LadderHandler:
	lda #$15 ; action on Ladder
	sta objectSpriteNum
	; presse bouton B
	lda joyD
	and #$02
	bne .presseB
	; press up, down ou A
	lda joyPad
	and #$31 
	beq .holdStill
	and #$30       ; Si On a pressé A ? (joyPad != 0x30)
	beq .release   ; Non
	and #$10       ; Sinon si  pad down
	beq .climbDown ; NON
	; climbUp:     : Alors on monte
		ldy #$00 ; XSpeed
		ldx #$c0 ; XSpeedFraction (speed +0.75)s
		lda tileLadderState
		and #$0C  ; Si échelle au dessus et player est entrain de monter
		bne .climbUpAvailable ; oui 
      	; non alors on ajuste la position au dessus de l'échelle
		lda objectPosY
		sec
		sbc #$14 ; replace le perso en haut de l'échelle
		sta objectPosY
		jmp .release
	
	.climbUpAvailable:
		and #$08 	; Il y'a une échelle au dessus ?
		bne .climb  ; oui
		lda #$17    ; Non on est entrain de quitter l'échelle
		sta objectSpriteNum ; Sprite de sortie de l'échelle
		bne .climb ; inconditionnel jmp
	.climbDown:
		lda tileLadderState
		cmp #$01
		bne .climbDownPos
		lda objectPosY
		clc
		adc #$14
		sta objectPosY
	.climbDownPos:
		ldy #$ff ; YSpeed
		ldx #$40 ; YSPeedFraciont (-0.75)
		lda tileLadderState
		and #$0C
		bne .climb 
		lda #$17 ; On quitte l'échelle
		sta objectSpriteNum
	.climb:
		lda objectFireDelay ; On est en train de shooter ?
		beq .setClimbPos ; Non 
		ldx #$00         ; Oui
		ldy #$00         ; On stop la montée
	.setClimbPos:
		sty objectYSpeed            ; set de YSpeed et YSpeedFraction
		stx objectYSpeedFraction
		jsr UpdateCurrentTileState  ; recheck des tuiles 
		lda tileLadderState
		beq .release	
		jsr ObjectDoCollisionChecksAndAvoidWalls
		bcs .release 
		rts
	.presseB:
		lda joyPad
		and #$c0 ; si on gauche est pressé
		beq .weaponFire
		jsr SetPlayerFacing
	.weaponFire:
		lda #$1f
		sta objectFireDelay
		jsr PlayerWeaponFire
	.holdStill:
		lda objectActionStateCounter
		and #$f0
		sta objectActionStateCounter
		
		rts
	.release:
		; le player lache l'échelle
		lda #$40 
		sta objectYSpeedFraction
		lda #$ff
		sta objectYSpeed
		jmp ObjectStandStillRight
	rts

AutoCenterScreen:
		sec
		lda tmpPosX ; tmp objectPosX
		sbc #$80  ; centre le perso 
		sta scrollPosX
		lda tmpPosScreen ; tmp objectPosScreen (num écran)
		sbc #$00 ; - carry
		cmp currentBeginScreen
		bmi .isBeginScreen
		bcs .isNotBeginScreen
		.isBeginScreen:
			lda currentBeginScreen
			sta scrollPosScreen
			jmp .resetSrollPosX
		.isNotBeginScreen:
			sta scrollPosScreen
			cmp currentEndScreen ; c'est le dernier écran ?
			bne .setDataPos
		.resetSrollPosX: ; c'est le premier écran on reset scrollPosX
			lda #$00
			sta scrollPosX
			beq ObjectRelocateHorizontally
		.setDataPos:
			sec
			lda objectPosX
			sbc tmpPosX; tmp objectPosX
			sta $0c; courante bande
			lda objectPosScreen
			sbc tmpPosScreen ; tmp objectPosScreen
			bpl .newStrip
			sec
			lda #$00
			sbc $0c
			beq ObjectRelocateHorizontally
			sta $0c
			jsr ScrollingRight
			jmp ObjectRelocateHorizontally ; rts à la fin de la fonction
			.newStrip: 
				lda $0c ; courante bande
				beq ObjectRelocateHorizontally
				jsr ScrollingLeft
		; go direct ObjectRelocateHorizontally

;
; Doit être précédé par 
; AutoCenterScreen
;
ObjectRelocateHorizontally:
	ldx objectId

	lda tmpPosScreen; tmp objectPosScreen
	sta objectPosScreen, x

	lda tmpPosXFraction ; tmp objectPosXFraction
	sta objectPosXFraction, x

	lda tmpPosX ; tmp ObjectPosX
	sta objectPosX, x

	rts	

ObjectDoCollisionChecksAndAvoidWalls:
	ldx objectId
	jsr ObjectCheckIfOutScreenVertically
	bcc .isPlayer
	ldx objectId
	bne .todo
	; todo player tombe en dehors de l'écran
	lda objectFlags
	and #$10 ; climb ?
	beq .checkPlayerEnemyHeight
	clc
	rts
	.todo:
		lda #$ff
		sta currentTuilesState+1
		rts
	.isPlayer:
		cpx #$00 ; Si c'est player
		beq .checkPlayerEnemyHeight 
	
	.checkPlayerEnemyHeight:
		ldy objectSpriteNum, x
		cpy #$ff
		bne .playerHeight
	.enenmyHeight:
		; TODO
	.playerHeight:
		lda playerYHeightTable, y
	.handleHeight:
		sta $10 ; tmp Y valeur à ajouter
		lda objectYSpeed, x
		bmi .speedMI
	.checkCollision:
		lda $10  ; tmp Y valeur à ajouter
		EOR #$ff ; Si A = 0x0c alors EOR = F3
		clc
		adc #$01 ; add 1 px pour le check
		jsr ObjectCollisionCheckHelper ; ici problème
		beq .setterPos
		;collision
		ldx objectId
		cmp #$20
		beq .resetYSpeed
		lda $03 ; tmp tmp objectPosY (tmp object Y + A)
		and #$f0
		clc
		adc #$10
		clc
		adc $10
		sta objectPosY, x
		lda #$00
		sta objectPosYFraction, x
		lda #$ff
		sta objectYSpeed, x
		lda #$40
		sta objectYSpeedFraction, x
		bne .resetYSpeed
	.speedMI:
		lda $10
		ldy objectId ; pour le jump on achoute 08 (8+x0c)
		bne .checkCollisionSMI
		ldy objectSpriteNum
		cpy #$09
		bne .checkCollisionSMI
		clc
		adc #$08 
		.checkCollisionSMI:
			jsr ObjectCollisionCheckHelper ; ici ça pos problème
		beq .setterPos ;
		ldx objectId
		lda $03 ; tmp tmp objectPosY
		and #$f0 ; On garde le MSB
		sec
		sbc $10  ; tmp Y valeur
		sta objectPosY, x 
		lda #$00
		sta objectPosYFraction, x
	.resetYSpeed:
		lda #$ff
		sta objectYSpeed, x
		lda #$40
		sta objectYSpeedFraction, x
		sec
		rts
	.setterPos:
		sec
		ldx objectId
		beq .playerYSpeed
	.playerYSpeed:
		lda objectYSpeedFraction, x
		sbc #$40
		sta objectYSpeedFraction, x
		lda objectYSpeed, x
		sbc #$00
		bpl .setObjectYSpeed
		cmp #$f4
		bcs .setObjectYSpeed
	.setObjectYSpeed:
		sta objectYSpeed, x	
	.setObjectPosY:	
		lda $01	; tmp objectPosY
		sta objectPosY, x
		lda $00 ; tmp objectPosYFraction
		sta objectPosYFraction, x
		clc
	rts

RecalculateActivesLowerIndex:
	ldy #$00
	.loop:
		lda roomActiveTable+1, y ; check l'écran actuel
		cmp scrollPosScreen
		bcs .end
		tya
		clc
		adc #$06 ; next actif
		tay
		bne .loop
	.end:
		sty activesLowerIndex
	rts


; A = tmp Y valeur à ajouter
ObjectCollisionCheckHelper:
	clc
	adc $01 ; tmp objectPosY  ; setté dans ObjectCheckIfOutScreenVertically
	sta $03 ; tmp tmp objectPosY (tmp object Y + A) 
	lda objectPosScreen, x
	sta $02 ; tmp objectPosScreen
	lda objectPosX, X
	sta $0a ; tmp objectPosx
	jsr DoCollisionCheckFor
	rts

ObjectCheckIfOutScreenVertically:
	sec
	lda objectPosYFraction, x
	sbc objectYSpeedFraction, x
	sta $00 ;tmp objectPosYFraction
	lda objectPosY, x
	sbc objectYSpeed, x
	sta $01
	;.isScreenBottom:
		cmp #$E8         ; Si screen bottom (posY >= 0xE8)
		bcc .isScreenTop ; non
		cmp #$f8          ; alors si screen bottom (posY <= 0xF8)
		bcs .posYSUPF8   ; oui c'est pas bon pour le perso
		lda #$03  ; set 3 pour down
		bne .setCurrentStripeEndType
	.isScreenTop:
		cmp #$04   ; Si screen top (posY <= 0x04)
		bcc .upCurrentStripeEndType ; oui 
		bcs .end   ; non
	.posYSUPF8:
		cmp #$FC 
		bcs .end 
		; Y >= #$F8 but < #$FC
		cpx #$00
		bne .false
		lda objectYSpeed
		bmi .false
		lda objectPosYFraction
		sta $00
		lda objectPosY
		sta $01
	.upCurrentStripeEndType:
		lda #$01 ; up pour CurrentStripeEndType
	.setCurrentStripeEndType:
		cpx #$00
		bne .false
		sta currentStripeEndType
	.false:
		sec
		rts	
	.end:
		clc
		rts

ScrollingLeft:
	; init du mouvement gauche
	; Un ennemi peut être chargé
	lda #$01
	sta screenMovedFlag
	; reset du tsaPPUTransfer
	ldx #$00
	stx tsaPPUtransferSize
	stx $0d ; utilisé dans DrawBlockFromActiveLevelMap pour l'offset tsaPPuTransfer

	lda $04 ; save objectPosXFraction
	pha
	lda $05 ; save  ObjectPosX
	pha

	ldx objectPosScreen
	dex
	stx $05

	lda objectPosX
	sta $04 

	jsr PrepareDrawBlock

	pla 
	sta $05 ; restore ObjectPosX
	pla
	sta $04 ; restore objectPosXFraction

	rts

ScrollingRight:
	; init du mouvemnet droite
	; Un ennemi peut être chargé
	lda #$41
	sta screenMovedFlag
	; reset du tsaPPUTransfer
	ldx #$00
	stx tsaPPUtransferSize
	stx $0d ; utilisé dans DrawBlockFromActiveLevelMap pour l'offset tsaPPuTransfer
	
	lda $04 ; save objectPosXFraction
	pha
	lda $05 ; save  ObjectPosX
	pha

	; posScreen +1
	ldx objectPosScreen
	inx
	stx $05
	; posX
	lda objectPosX
	sta $04

	.loop:
		lda $05 ; new objectPosScreen
		cmp currentEndScreen
		beq .process 
		bcs .end
		.process:
	 	lda $04 ; objectPosX
		and #$03
		bne .noDraw
		jsr DrawBlockFromActiveLevelMap
		.noDraw:
			inc $04 ; inc objectPosX
			bne .nextIterateLoop
			inc $05 ; inc objectPosScreen
		.nextIterateLoop:
			dec $0C
			bne .loop
	.end:
		pla 
		sta $05 ; restore ObjectPosX
		pla
		sta $04 ; restore objectPosXFraction
		
		rts

DoScrolling:
	lda #$00 ; init bank sprite ennemi ?
	sta $00  ; $59

	lda currentStripeEndType ;0=right 1=up 2=left 3=down
	and #01 ; Si up/down
	beq HorizontalScrollPoint
	jmp VerticalScrollPoint


; Scrolling horizontal de la ppu
HorizontalScrollPoint:
	lda objectSpriteNum
	cmp #$09 ; Jump/fall ?
	beq .updateAction
	lda objectActionStateCounter
	and #$30
	sta objectActionStateCounter
	lda #$06 ; player run

    .updateAction:
		sta objectSpriteNum
	
	; Ajustement de la position du perso
	lda currentStripeEndType
	sec
	sbc #$01
	lsr a
	tax
	lda #$00
	sta $10 ; $5a
	lda horizontalScrollIncrementTable, x
	sta $11 ; $5b
	lda objectPosX
	cmp #$e0 ; perso est au niveau de la première porte
	bcc .initScrollPos
	lda horizontalScrollPosXFractionTable, x
	sta $10
	lda horizontalScrollPosXtable, x
	sta $11
	
	.initScrollPos:
		lda #$00
		sta scrollPosX
		sta scrollPosY

	ldy horizNumberOfFramesToScroll, x
	.loop:
		clc
		lda objectPosXFraction
		adc $10
		sta objectPosXFraction
		lda objectPosX
		adc $11
		sta objectPosX
		clc
		lda scrollPosX
		adc horizScrollIncrement, x
		sta scrollPosX
		tya
		pha
		txa
		pha

		jsr UpdateGraphics
		; todo loadEnemyGraphics
		jsr NextFrame

		pla
		tax
		pla
		tay

		dey
		bne .loop

	; Reset s
	lda #$18
	sta objectPosX
	lda #$00
	sta scrollPosX
	sta scrollPosY
	sta objectSpriteNum
	sta objectActionStateCounter

	rts

; backward Scrolling, player X increment
horizontalScrollIncrementTable: 
	.db $01, $FF
; Scrolling, increments objectPosXFraction
horizontalScrollPosXFractionTable: 
	.db $B0, $50  
; forward scrolling, player X increment
horizontalScrollPosXtable: 
	.db $00, $FF 
horizNumberOfFramesToScroll: 
	.db $3F, $40
horizScrollIncrement:   
	.db $04, $fc

; Scrolling vertical de la ppu
VerticalScrollPoint:
	lda currentStripeEndType ; 1=up 3=down
	lsr a ; on garde le dernier bit, donc 0=up 1=down (ça épure gauche/droite)
	tax ; devient une key

	; Init scrolling
	lda verticalScrollOffesetTable, x
	sta var33
	lda verticalScrollBeginTable, x
	sta scrollPosY ; $EF

	.loop:
		; Update graphics
		txa
		pha ; Save de la key 
		jsr UpdateGraphics
		jsr DrawBLocksScroll
		; todo loadEnemyGraphics
		jsr NextFrame
		pla ; Restore key X
		tax 

		;move player
		clc
		lda objectPosYFraction
		adc verticalScroolPosYFraction, x
		sta objectPosYFraction
		lda objectPosY
		adc verticalScroolPosY, x
		sta objectPosY

		; scrolling vertical
		CLC
		lda scrollPosY
		adc verticalScrollIncrementTable, x
		sta scrollPosY
		CLC
		lda var33
		adc verticalScroll33Table, x
		sta var33
		bmi .loopEnd
		cmp #$3c
		beq .loopEnd
		bne .loop
	.loopEnd:
		lda #$00
		sta	scrollPosY
		sta objectPosYFraction
		jsr UpdateGraphics

	rts


verticalScrollOffesetTable:
	.db $3B, $00            
verticalScrollBeginTable:
	.db $ef, $00 ; -17, 00 => complément à 2 pour neg
verticalScrollIncrementTable:  
	.db  $fc, $04 ; -4,  4 => complément à 2 pour neg
verticalScroll33Table:
	.db $FF, $01 ; Increment pour var33
verticalScroolPosYFraction: 
	.db $BF, $41 ; Increment of objectPosYFraction+0
verticalScroolPosY: 
	.db $03, $FC ; Increment of ObjectPosY+0


DrawOneScreen
	sta $05  ; ID actuelScreen
	ldx #$ff ; INIT
	stx $04  
	lda #$00 ; id player
	sta objectId 

	.loopDraw:
		lda #$00
		sta $0d
		sta tsaPPUtransferSize

		lda #$08 ; Il y'a 8 colonnes à draw
		sta $0c
		jsr PrepareDrawBlock

		lda PPU2000value ; check si ppu est éteinte
		and #$80
		beq .transfer ; oui alors transfer
		;.nextFrame:  ; non next Frame
			jsr NextFrame
			jmp .loopNext

		.transfer:
			jsr TsaPPUtransfer	

		.loopNext:
			lda $04
			cmp #$ff
			bne .loopDraw
	
	rts


ObjectStandStillRight:
	lda objectFlags ; met le perso face droite
	and #$40
	sta objectFlags
	lda #$00
	sta objectSpriteNum ; sprite standing
	sta joyPadOld ; reset de la touche pressé la frame d'avant

	jmp PlayerIACheckMoving ; go PlayerIA

ObjectUpdateMovementRight:
	ldx objectId ; objet ID
	; objectPosXFraction
	clc
	lda objectPosXFraction, x
	adc objectXSpeedFraction, x
	sta tmpPosXFraction; tmp objectPosXFraction 
	; ObjectPosX
	lda objectPosX, x
	adc objectXSpeed, x ; posX + Xspeed
	tay ; save de la nouvelle valeur objectPosX

	lda objectPosScreen, x
	adc #$00 ; + carry

	ldx objectId
	beq .player ; si player

	.object:
		sta tmpPosScreen
		sty tmpPosX
		lda objectFlags, x
		and #$01 ; collision avec background
		bne .checkCollisionBackground
		rts 

	.player:
		cmp currentEndScreen ; le player est ou par rapport au dernier écran
		beq .beginningEndStrip ; si = ??pas bon label
		bcs .updateStripe ; >
		bne .setObjectPosScrenAndPosX  ; <> (donc ici <)

	.beginningEndStrip:
		cpy #$EF ; si posX est à la fin de l'écran
		bcc .setObjectPosScrenAndPosX

	.updateStripe:
		lda currentEndScreen
		ldy #$ef
		ldx #$02
		stx currentStripeEndType
		ldx $00

	.setObjectPosScrenAndPosX:
		sta tmpPosScreen ; tmp objectPosScreen
		sty tmpPosX ; tmp objectPosX

	.checkCollisionBackground:
		ldy objectSpriteNum, x
		cpy #$ff
		bne .widthTablePlayer
	; widthTableEnemy
	;  TODO

	.widthTablePlayer:
		lda playerXWidthTable, y

	.setPos:
		; POS X
		sta $10 ; xwidth
		clc
		lda tmpPosX ; tmp ObjectPosX
		adc $10 ; posX + width
		sta $00 ; tmp tmp objectPosX
		; POS SCREEN
		lda tmpPosScreen  ; tmp ObjectPosScreen
		adc #$00 ; posScrean + (posX + width)
		sta $01  ; new ObjectPosScreen
		lda objectPosY, x
		sta $03 ; tmp objectPosY
		; checkBackgroundcollision
		jsr ObjectVerifyBackgroundCollision
		beq .end
		lda $00   ; tmp tmp objectPosX
		and #$f0
		ldx objectId
		sec
		sbc $10
		sta tmpPosX
		lda $01
		sbc #$00
		sta tmpPosScreen
	.end:
		rts

ObjectUpdateMovementLeft:
	ldx objectId ; objet ID
	; objectPosXFraction
	sec
	lda objectPosXFraction, x
	sbc objectXSpeedFraction, x
	sta tmpPosXFraction; tmp objectPosXFraction 
	; ObjectPosX
	lda objectPosX, x
	sbc objectXSpeed, x ; posX - Xspeed
	tay ; save de la nouvelle valeur objectPosX

	lda objectPosScreen, x
	sbc #$00 ; - carry

	ldx objectId
	beq .player ; si player

	.object:
		sta tmpPosScreen
		sty tmpPosX
		lda objectFlags, x
		and #$01 ; collision avec background
		bne .checkCollisionBackground
		rts 

	.player:
		cmp currentBeginScreen ; le player est ou par rapport au premier écran
		bmi .beforBeginScreen ; Avant le premier ecran (logiquement pas possible)
		beq .isBeginScreen ; si =
		bcc .beforBeginScreen
		bne .setObjectPosScrenAndPosX  ; <> (donc ici <)

	.isBeginScreen:
		cpy #$10 ; si la posX = #$10
		bcs .setObjectPosScrenAndPosX

	.beforBeginScreen:
		lda currentBeginScreen
		ldy #$10
		ldx #$04
		stx currentStripeEndType 
		ldx #$00

	.setObjectPosScrenAndPosX:
		sta tmpPosScreen ; tmp objectPosScreen
		sty tmpPosX ; tmp objectPosX

	.checkCollisionBackground:
		ldy objectSpriteNum, x
		cpy #$ff
		bne .widthTablePlayer
		; widthTableEnemy
		;  TODO

	.widthTablePlayer:
		lda playerXWidthTable, y

	.setPos:
		; POS X
		sta $10 ; xwidth
		sec
		lda tmpPosX ; tmp ObjectPosX
		sbc $10 ; posX + width
		sta $00 ; tmp tmp objectPosX
		; POS SCREEN
		lda tmpPosScreen  ; tmp ObjectPosScreen
		sbc #$00 ; posScrean + (posX + width)
		sta $01  ; new ObjectPosScreen
		lda objectPosY, x
		sta $03 ; tmp objectPosY
		; checkBackgroundcollision
		jsr ObjectVerifyBackgroundCollision
		beq .end
		clc
		lda $10
		adc #$10
		sta $0f

		lda $00
		and #$f0
		clc
		adc $0f
		sta tmpPosX
		lda $01
		adc #$00
		sta tmpPosScreen

	.end:
		rts

HandleSpeed:
	ldy #$00
	lda objectSpriteNum
	ldx #$08 ; une action à max 8 états

	.loop:
		cmp  xSpeedAndFractionStateTable, x	
		beq .getValue
		dex
		bpl .loop
		; si aucun sprite trouvé c'est un sprite de moving1 ou moving2
		lda joyPad
		and #$c0 ; touche doite/gauche
		beq .ySetted ; no => y = 00
		ldy #$07
		bne .ySetted ; yes => y = 07 
		
	.getValue:	
		ldy xSpeedAndFractionIdTable, x
	.ySetted:
	; set de xSpeed et xSpeedFraction
	ldx #$00
	jsr SetXSpeedAndFraction
	
	lda objectFlags
	and #$40 ; face droite 
	sta $01 ; tmp facing
	
	rts

SetPlayerFacing:
	; Met player face left
	lda objectFlags
	and #$bf        
	sta objectFlags       
	; Met player face droite si "right" touche est appuyée
	lda joyPad
	and #$80        
	lsr a           
	ora objectFlags      
	sta objectFlags

 	rts 

ScrollProcess:
	sta $05 ; current NT
	ldx #$ff ; on se place sur la dernière bande
	stx $04
	lda #$00
	sta objectId

	.loopDraw:
		lda #$00
		sta $0d
		sta tsaPPUtransferSize
		
		lda #$08
		sta $0c ; Il y'a 8 colonnes à draw
		jsr PrepareDrawBlock

		; Si PPU ON le transfère se fera lors de la NMI
		lda PPU2000value
		and #$80
		beq .doTransfer

		.nextFrame:
			jsr NextFrame
			jmp .loopNext

		.doTransfer:
			jsr TsaPPUtransfer

		.loopNext:
			lda $04
			cmp #$ff
			bne .loopDraw

	rts

; Function9FB3
PrepareDrawBlock:
	lda $05
	bmi .end ; $05 sera 01 puis 00 puis $ff (donc NT 24; 20; XX)
	cmp currentBeginScreen
	bcc .end 
	lda $04 ; Check la bande est un multiple de 4
	and #$03
	bne .dontDrawBlock
	lda $04
	and #$10
	beq .drawBlock
	lda varBF ; Todo ?
	bne .dontDrawBlock

	.drawBlock:
		jsr DrawBlockFromActiveLevelMap

	.dontDrawBlock:
		lda $04 ; Si bande = 0 on passe à l'écran prev
		bne .prevColumn
		dec $05  ; NT--
		
	.prevColumn:
		dec $04 ; Dec bande--
		dec $0C ; Dec courante colonne
		bne PrepareDrawBlock

	.end:
		rts

SkitStagePalette:
	lda #$23
	sta PPUADDR
	lda #$E1
	sta PPUADDR

	ldx #$60
	.loop:
		lda #$00
		sta PPUDATA
		dex
		bpl .loop
	rts

SkitText:
	; INIT
	lda #03
	sta $0d  ; nb skit
	lda #$22 ; high
	sta $08  ;     current
	sta $04  ;     save
	lda #$24 ; low
	sta $09  ;     current
	sta $05  ;     save

	.start:
		lda #$00
		sta $0a
	.loopDrawLetter:
		lda #$05
		sta miscCounter ; Une lettre toutes les 5 frames

	.decMisCounter:
		jsr NextFrame
		dec miscCounter
		beq .endDecMisCounter
		bne .decMisCounter
	.endDecMisCounter:

	jsr WriteSkitText
	
	cmp #$ff
	beq .newLine
	cmp #$fe
	beq .change
	sta ppuTransferRawBuf
	lda $08
	sta ppuTransferRawAddr
	lda $09
	sta ppuTransferRawAddr+1
	inc $09
	bne .incPpuTransferRawSize
	inc $08
	.incPpuTransferRawSize:
		inc ppuTransferRawSize
		inc $0a
		jmp .loopDrawLetter ; on réaffiche une lettre

	.newLine:
		clc
		lda $05 ; Get low
		adc #32 ; Add 32
		sta $05 ; save low
		sta $09 ; current low
		lda $04
		adc #0  ; Ajout du carry s'il est setté
		sta $04 ; save High
		sta $08 ; current high
		inc $0a
		jmp .loopDrawLetter

	.change:
		dec $0d
		beq .end
		lda $0d
		cmp #$01
		bne .wait
		jsr SkitTextNt0
		lda #$02 ; 3ème texte
		sta $03
		bne .handlePPU
	; On attend un peu 126 -> 0 
	.wait:
		lda #$80
		sta miscCounter
		.decWait:
			jsr NextFrame
			dec miscCounter
			beq .handlePPU
			bne .decWait
	; Change de NT
	.handlePPU: 
		lda PPU2000value
		eor #01
		sta PPU2000value
		sta PPUCTRL
		and #01
		bne .nt24
		; init skip 3 sur NT20
		lda #$22 ; high
		sta $08  ;     current
		sta $04  ;     save
		lda #$24 ; low
		sta $09  ;     current
		sta $05  ;     save
		jmp .start
		; Init skip 2 sur NT24	
	.nt24:
		inc $03 ; 2ème texte
		lda #$26 ; high
		sta $08  ;     current
		sta $04  ;     save
		lda #$24 ; low
		sta $09  ;     current
		sta $05  ;     save
		jmp .start
	.end:	
		rts

SkitTextNt0:
	; charge title dans les deux Nametables	
	lda #0
	sta $03 ; init save Y
	sta $06 ; init position courante ligne
	sta $08 ; init diver (loop i repeatData ou inc data incData)
	lda #02
	sta $0c ; charge skit 1 nametable 24xx
	lda #$0a
	sta $07

	.loopDataTile:
		jsr NextFrame
		jsr WriteSkitChr
		lda $08
		cmp #$ff
		bne .loopDataTile
		lda #0
		sta $08
		dec $07
		bne .loopDataTile

	; todo delete text
	
	; INIT
	lda #$22 ; high ppuaddr
	sta $01
	lda #$00 ; low ppuaddr
	sta $02
	
	lda #112 ; nombre de tour pour tout erase
	sta $03

	.loopText:
		jsr NextFrame
		; PpuTransferRawAddr
		lda $01
		sta ppuTransferRawAddr
		lda $02
		sta ppuTransferRawAddr+1
		; DATA
		ldy #$00 ; On erase 4 tiles par frame
		.loopDataText:
			lda #00
			sta ppuTransferRawBuf, y
			inc ppuTransferRawSize
			inc $02
			bne .next
			inc $01
			.next:
			iny
			cpy #$04
			bne .loopDataText

		dec $03
		bne .loopText

	rts

SkitStageDrawFrame:
	ldy #01
	lda pointer+2
	sta PPUADDR
	lda #$68
	sta PPUADDR
	lda #$35
	sta PPUDATA
	lda #$36
	ldx #$0d ; 13
	.loopFrameRowTop:
		sta PPUDATA
		dex
		bpl .loopFrameRowTop
	lda #$37
	sta PPUDATA

	lda pointer+3
	sta PPUADDR
	lda #$c8
	sta PPUADDR	
	lda #$32
	sta PPUDATA
	lda #$33
	ldx #$0d ; 13
	.loopFrameRowBottom:
		sta PPUDATA
		dex
		bpl .loopFrameRowBottom
	lda #$34
	sta PPUDATA
	
	lda PPU2000value 
	ora #$04         ; inc ppu par 32
	sta PPU2000value
	sta PPUCTRL
	lda pointer+2
	sta PPUADDR
	lda #$88
	sta PPUADDR
	ldx #$09
	lda #$31
	.loopFrameRowLeft:
		sta PPUDATA
		dex
		bpl .loopFrameRowLeft

	lda pointer+2
	sta PPUADDR
	lda #$97
	sta PPUADDR
	ldx #$09
	lda #$30
	.loopFrameRowRight:
		sta PPUDATA
		dex
		bpl .loopFrameRowRight

	lda PPU2000value  ; inc ppu par 1
 	and #$FB
	sta PPU2000value
	sta PPUCTRL

	rts

;
; Y = starting sprite index
; X = Nombre de sprite consécutif à caché
;
HideSprites:
	lda #$f8 ; est un sprite noir
	.loop:
		sta $0200, y
		iny
		iny
		iny
		iny
		dex
		bne .loop

	rts

ForgetRoomObjects:
	ldx #$1f ; tous les objets sauf le perso
	lda #$f8 ; En dehors de l'écran

	.loop:
		sta objectPosY+1, x
		dex
		bpl .loop

	LDX #$0f
	LDA #$ff
	.loop2:
		sta enemySpawned, x
		dex
		bpl .loop2

	rts	

; Initial screen start (à modifier si on ajoute un niveau)
; par ordre de stage
firstScreenScreenTable:
    .db $00, $00, $00 ; début (premier octet mort car les stages commencent à 1 à revoir)
    .db $00, $18 ; point A

firstScreenEnemyPointer: 
    .byte $00,$FF; nombre d'ennemi sur l'écran de dépare
    ; checkpoint
    .byte $00,$11

dataTitle:
	.db $20,$EB, $32,$33,$00,$34, $ff
	.db $21,$09, $35,$36,$37,$38,$39,$3a,$00,$00,$00,$3b, $ff
	.db $21,$29, $3c,$3d,$3e,$3f,$40,$00,$00,$00,$00,$41,$42, $ff
	.db $21,$49, $43,$44,$45,$00,$46,$00,$47,$48,$49,$4a,$4b,$4c, $ff
	.db $21,$6A, $4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58, $ff
	.db $21,$8B, $59,$5a,$5b,$5c,$5d,$5e,$5f,$60,$61,$62,$63, $ff
	.db $21,$AB, $64,$65,$00,$66,$67,$68,$69,$00,$6a,$6b,$6c, $ff
	.db $21,$CC, $6d,$00,$00,$00,$00,$00,$00,$00,$00,$6e,$6f, $ff
	.db $22,$CA, $10,$12,$05,$13,$13,$00,$00,$13,$14,$01,$12,$14, $FF ; press start
		
  .bank 13		;06
  .org $A000


;;;;;; Object DATA SPRITE ;;;;;
objectXWidthTable
    .db $08, $08, $08, $08 ;00
	.db $08, $08, $08, $08 ;
	.db $08, $08, $08, $08 ;08
	.db $08, $08, $08, $08 ;
	.db $08, $08, $08, $06 ;10

;;;;;; Enemi DATA SPRITE ;;;;;
  .include "data/enemy.asm"

;;;;;; Player DATA SPRITE ;;;;;
  .include "data/player.asm"


;--------------------BANK 7---------------------------------------------------------------------
 
  .bank 14		;07 (1/2 last bank/fixed)
  .org $C000

bankTable: 
	.db 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15

RESET:
	SEI          ; disable IRQs
	CLD          ; disable decimal mode
	LDX #$40
	STX $4017    ; disable APU frame IRQ
	LDX #$FF
	TXS          ; Set up stack
	INX          ; now X = 0
	STX $4010    ; disable DMC IRQs

	lda #01      
	sta PPUCTRL  ; disable NMI
	lda #06
	sta PPUMASK    ; disable rendering
	
	lda #12       ; charge bank 6
	jsr BankSwitch
	jmp Reset2

BankSwitch: ; bank # loaded in A
	php
	;lda saveBank
	sta currentBank
	lsr a
	tax
	sta bankTable, x
	plp
	
	rts
	
BankSwitch10: 
	php
	lda #10
	sta currentBank
	lsr a
	tax
	sta bankTable, x
	plp
	
	rts	

;
; A: 
;	low 2 bits = numéro bank
;	high 6 bits = offset (ex 0x38 + donnera 0xB8 car 80+38)
;
; Modifie X mais pas A ou Y
BankSwitchStage:
	pha
	and #$fc
	lsr a
	lsr a
	ora #$80 ; Une bank commence à 8000
	sta $0b
	lda #00
	sta $0a
	
	pla
	and #$03 ; On récupère le numérao de bank
	tay
	sta currentBank
	sta bankTable, y
	
	rts
	
LoadEnemies:
	lda bossCurrentStrategy ; pas d'enemi si boss
	beq .notBoss
	.return:
		rts
	.notBoss:

	; charge direct banque 5 data logique game
	lda #10
	sta currentBank
	lsr a
	sta $c005

	; todo zigZagFireStatus
	; todo spawn enemi après mort

	lda screenMovedFlag
	and #$01 ; #$01 un enemi peut être loadé
	beq .return ; #00 c'est qu'il n'y à rien à load = return 
	lda screenMovedFlag ; direction  $41 droite $40 gauche
	and #$fe
	bne LoadEnemiesForward
	jmp LoadEnemiesBackward

	rts

; charge les enemis vers l'avant
LoadEnemiesForward:
	; ex: le perso se trouve sur le premier écran id = 0 
	;     et l'enemi à charger écran 1
	;     si scrollPosX -1 donne < 0 ou > 255 on set carry
	;     alors adc à scrollPosScreen sera 1

	clc 
	lda scrollPosX ; pixel du scrolling
	adc #$ff       ; + #$ff pour faire -1
	sta $04

	lda scrollPosScreen ; id de l'écran
	adc #$00            ; on ajoute carry
	sta $05             ; screen du player

	lda currentEnemyIndex
	jsr LoadEnemyNumber
	;ldy #$00
	.loop:
		lda [currentRoomPointer], y ; id écran
		cmp $05
		bcc .setValue; id écran < perso écran 
		bne .checkFoBackward ; <> go check si l'enemi n'est pas avant
		iny ; ici id écran = perso écran
		lda [currentRoomPointer], y ; pos X
		dey ; dey car on a iny pour le check de pos X
		cmp $04 ; scroolPosx -1 du perso
		beq .setValue ; =
		bcs .checkFoBackward ; <

		.setValue:
			; enemi X pos sur SON écran
			iny
			lda [currentRoomPointer], y 
			sta $00 ; /!\ $00 sera verouiller, voir mainLoop
			; Enemi Y pos sur son écran
			iny
			lda [currentRoomPointer], Y
			sta $01
			; Enemi id
			iny
			lda [currentRoomPointer], y
			
			iny
			ldx currentEnemyIndex
			jsr SpawnEnemy
			inc currentEnemyIndex
			bne .loop

	.checkFoBackward:

	lda saveBank
	jsr BankSwitch

	rts
; Charge les enemis vers l'arrière
LoadEnemiesBackward:
	lda saveBank
	jsr BankSwitch

	rts
	
LoadEnemyNumber:
	ldx $00 ; playerX - 7px
	stx currentRoomPointer+1  ;*16
	asl a
	rol currentRoomPointer+1
	asl a
	rol currentRoomPointer+1
	sta currentRoomPointer
	lda stageId ; id pour enemyDataTable
	asl a ; id word
	tax
	clc
	lda enemyDataTable, x
	adc currentRoomPointer
	sta currentRoomPointer
	lda currentRoomPointer+1
	and #$03
	adc enemyDataTable+1, x
	sta currentRoomPointer+1
	ldy #$00

	rts


; Spawn a new object
;  A = object type
;  Y = save y (enemyDataTable[y])
;  X = currentEnemyIndex
;  $00 = object pos x
;  $01 = object pos y
;  $05 = object screen number
; SpawnObject
SpawnEnemy:
	sta $02 ; Enemi id
	sty $03 ; save y
	;cmp #$ff
	;bne
	stx $06 ; $0c save currentEnemyIndex
	lda $01 ; pos Y
	txa
	ldx #$0f
	; cherche si l'enemi est déjà affiché
	.loop:
		cmp enemySpawned, x
		beq .spawned
		dex
		bpl .loop
		bmi .notFound
	.spawned:
		ldy $03
		rts
	.notFound:
		ldx #$10
		jsr FindFreeObject
		bcs .loop
		; set Enemi:	
		lda $06 ; $0c restore currentEnemyIndex
		sta enemySpawned, x
		lda #$ff
		sta objectSpriteNum, x
		lda $02 ; enemi id
		sta objectType, x
		lda $05 ; écran id
		sta objectPosScreen, x 
		lda $00 ; pos x
		sta objectPosX, x
		lda $01 ; pox y
		; cmp #$ff
		sta objectPosY, x
		ldy $02 ; enemi id
		lda defaultEnemyFlags, y
		sta objectFlags, x
		tya
		pha ; save enemi id
		lda defaultEnemySpeedTable, y
		tay 
		jsr InitEnemyDefaultSpeed
		pla ; restore enemi id
		tya
		lda defaultEnemyFireDelay, y
		sta objectFireDelay, x
		lda #$14 
		sta objectLifeMeter

		lda #$00
		sta objectActionStateCounter, x
		sta objectLifeCycleCounter, x
		sta objectPosYFraction, x
		sta objectPosXFraction, x
		; todo ajouter liste de sons
		
		ldy $03 ; restore y
	rts

FindFreeObject:
	lda #$F8
	.loop:
		cmp objectPosY, x 
		beq .objectFree
		inx
		cpx totalObjects
		bne .loop
		.noObjectFree:
			sec
			rts
		.objectFree:
			clc
	rts

;
; Set ObjectXSpeedFraction et ObjectXSpeed
; 
; Y = id xSpeedAndFraction1
;
SetXSpeedAndFraction:
	; Les data sont en bank 6 voir pour un changement de bank

	lda xSpeedAndFraction1, y
	pha 
	and #$f0 ; MSB xSpeedFraction
	sta objectXSpeedFraction, x
	pla
	and #$0f ; LSB xSpeed
	sta objectXSpeed, x

	; voir pour un retour via une bank de stage
	rts

WriteSkitText:
	jsr BankSwitch10

	; Récupération skitTextTable
	lda stageId
	asl a
	tay
	lda skitTextTable, y 
	sta $01
	iny 
	lda skitTextTable, Y
	sta $02

	; récupération skitTextXXTablePointer
	lda $03
	asl a
	tay
	lda [$01], y
	sta $06
	iny
	lda [$01], y
	sta $07
	
	; récupération data text
	ldy $0a
	lda [$06], y

	pha
	lda saveBank
	jsr BankSwitch
	pla
	rts

;; CHR
WriteSkitChr:
	jsr BankSwitch10
	lda stageId
	asl a
	tay
	; récupération dataSkitTable
	lda dataSkitTable, y
	sta $01
	iny 
	lda dataSkitTable, y
	sta $02

	; récupération data dans dataSkitXXTablePointer
	lda $0c
	asl a
	tay
	lda [$01], y
	sta $0a
	iny
	lda [$01], y
	sta $0b

	; PPUADDR
	ldy $03 ; Restore Y
	lda $06 ; Restore current line pointer
	bne .restorePPUAddr
	; Init PPU Addr
	lda [$0a], y
	sta $04
	sta ppuTransferRawAddr
	iny
	lda [$0a], y
	sta $05
	sta ppuTransferRawAddr+1
	bne .handleData
	.restorePPUAddr:
		clc
		lda $04
		sta ppuTransferRawAddr
		lda $05
		adc $06
		sta ppuTransferRawAddr+1

	.handleData:
		; PPUDATA
		iny
		lda [$0a], y
		cmp #$ff
		beq .resetCurrentLine
		cmp #$fe
		beq .repeatData
		cmp #$fd
		beq .incData
		; simple tuile
		sta ppuTransferRawBuf
		inc ppuTransferRawSize
		inc $06 ; inc courante ligne position
		bne .end
		.repeatData:
			ldx #00
			iny
			lda [$0a], y ; Nb de fois que la data sera répétée
			sta $08
			iny
			.loopRepeatData:
				lda [$0a], y ; tile
				sta ppuTransferRawBuf, x
				inc ppuTransferRawSize
				inc $06 ; inc courante ligne position
				inx
				dec $08
				bne .loopRepeatData
			beq .end ; ici $08 sera 0 donc branchement à .end
		.incData:
			ldx #00
			stx $08
			iny 
			lda [$0a], y ; Nb de fois que la data sera inc
			sta $09
			iny
			clc
			.loopIncData:
				lda [$0a], y; tille
				adc $08 ; on ajout++
				sta ppuTransferRawBuf, x
				inc ppuTransferRawSize
				inc $06 ; inc courante ligne position
				inc $08
				inx
				dec $09
				bne .loopIncData
			beq .end	
	.resetCurrentLine:
	iny
	sta $08 ; on save #$FF pour sortir de loopDataTile
	lda #00 ; reset de la courante ligne
	sta $06

	.end:
	; save Y
	sty $03

	lda saveBank
	jsr BankSwitch
	rts

; Appellé en début de chaque stage + reset
; pointer+2 type 0 = room 1 = skit
WriteChr:
	; Récupération de la roomTile pour le stage en cours
	jsr BankSwitch10
	lda stageId
	asl a ; table de words
	tay
	
	lda pointer+2
	beq .room
	lda stikTileTable, y
	sta pointer
	lda stikTileTable+1, y
	sta pointer+1
	bne .next ; inconditionnel branchememnt
	.room:
		lda roomTileTable, y
		sta pointer
		lda roomTileTable+1, y
		sta pointer+1

	.next:
	ldy #0
	lda [pointer], y ; Nombre de bank qu'il faut récupérer
	sta $08
	
	lda pointer+3 ; si pointer = 1 on commentce à 1000
	bne .setPPU1000
	lda #00       ; sinon à zéro
	beq .setPPU   ; inconditionnel branchement
	.setPPU1000:
		lda #$10
	.setPPU:
		sta PPUADDR
		lda #$00
		sta PPUADDR
		sta $09
		inc $09
	
	.loopRetrieveBank:
		jsr BankSwitch10
		lda $09
		pha
		tay
		lda [pointer], y ; Nombre de ligne (256 = 1 ligne)
		tax
		
		pla
		tay
		iny
		lda [pointer], y ; Bank ou se trouve les tiles
		jsr BankSwitchStage
		
		.loop256Bytes:
				ldy #00 
			.loopBytes:
				lda [$0a], y ; On tape dans la bank de tile
				sta $2007
				iny
				bne .loopBytes
			inc $0b 
			dex
			bne .loop256Bytes
			
		lda $09
		clc 
		adc #02
		sta $09		
		dec $08
		bne .loopRetrieveBank
		
	lda saveBank  ; on récupère l'ancienne bank pour le rts
	jmp BankSwitch

;; PALETTE ET ACTIFS
InitStagePaletteAndActive:
	lda stageId
	jsr BankSwitchStage
	;asl a
	;tay
	lda #LOW(ROOM_SPRITE_PALETTE1)
	sta $01
	lda #HIGH(ROOM_SPRITE_PALETTE1)
	sta $02

	; Sprites BG palette
	lda #$20
	jsr WritePalette
	
	; Copie palette bg/sprite en RAM pour l'update
	ldy #$2f
	.loop:
		lda [$01], y
		sta bGPalettes, y
		dey
		bpl .loop

	lda stageId
	beq .end

	; Active
	lda #LOW(ROOM_ACTIVES1) ; adresse mémoire des actifs dans stage bank
	sta $01
	lda #HIGH(ROOM_ACTIVES1)
	sta $02
	
	ldy #$00     ; nombre d'actif
	lda [$01], y ; *6 parce qu'un actif à 6octets d'info
	asl a
	sta roomActiveTable ; *2
	asl a ;*4
	CLC ; pour faire le *6
	adc roomActiveTable
	tay

	.loopActifs:
		lda [$01], y
		sta roomActiveTable - 1, y
		dey
		bne .loopActifs
	 
	.end: 
		lda saveBank
		jmp BankSwitch

	rts 

; A = $ssssp000
; p = palette (=0 BG, =1 sprites)
; s = palette size (16-240. utiliser ssss = 0001 pour 1 palette, ssss = 0010 pour 2) 
;
;   10 = 1 BG pal
;   18 = 1 sprite pal
;   20 = les deux pals, la première BG, puis l'autre sprite
;
; pointer, pointer+1 = palette addr
WritePalette:
	pha
	and #$F0 ; on récupère msb
	tax
	lda #$3f
	sta PPUADDR
	pla
	asl a 
	and #$10
	sta PPUADDR

	ldy #00
	.loop:
		lda [$01], y ; ($35)
		sta PPUDATA
		iny
		dex
		bne .loop
	
	stx paletteParam ; ici X = 0
	
	rts

UpdatePalettes:
	lda paletteUpdateDelay
	and #$07 ; update de la palette toutes les 8 frames
	bne .timer
	
	; Si bit 3 de paletteUpdateDely est set alors on écrit palettePtr et palPtr+1 sinon palPtr+2/+3
	ldx palettePtr+2
	ldy palettePtr+3
	lda paletteUpdateDelay
	and #$08
	beq .writePalette 
	ldx palettePtr
	ldy palettePtr+1
	
	.writePalette:
		stx pointer
		sty pointer+1
		lda paletteParam
		pha
		jsr WritePalette
		pla
		sta paletteParam
		
	.timer:
		dec paletteUpdateDelay
	
	.end:	
	rts

PaletteSetupForBg
	lda #low(bGPalettes)
	sta palettePtr ; 38
	sta palettePtr+2 ;3a
	lda #high(bGPalettes)
	sta palettePtr+1 ; 39
	sta palettePtr+3 ; 3b
	lda #$10 ; set b00010000 pour BG palette
	sta paletteParam
	lda #1
	sta paletteUpdateDelay
	
	rts

PaletteSetupForBGwith3F0:
	jsr PaletteSetupForBg
	lda #low(unknownPalettes)
	sta palettePtr
	lda #high(unknownPalettes)
	sta palettePtr+1
	
	rts

; PPU TRANSFER

; HEADER PPUTranferRow
;--------+----------------
;byte #  | what it tells us
;--------+----------------
;00      | PPU Addr high
;01      | PPU Addr low
;02-xx   | Raw data (tiles/attr)
PPUTransferRaw:
	lda ppuTransferRawAddr
	sta PPUADDR
	lda ppuTransferRawAddr+1
	sta PPUADDR
	ldx #00
	.loop:
		lda ppuTransferRawBuf, x
		sta PPUDATA
		inx
		dec ppuTransferRawSize
		bne .loop

	rts

TsaPPUtransfer:
	lda tsaPPUtransferSize
	bne .pre
	jmp .end
	.pre:	
		and #$c0 ; manipulation ?
		beq .do ; non transfert classic 
		jmp TsaBitManipulation ; oui
	.do:
		lda PPU2000value ; PPU inc en 32 (pas en 1)
		ora #$04
		sta PPU2000value
		sta PPUCTRL
		lda #$02
		sta $0e
		lda #HIGH(tsaPPUTransferNTaddress)
		sta $0f
		lda #LOW(tsaPPUTransferNTaddress)
	.init:
		sta $0c
		tax
		ldy #$00

		lda #HIGH(tsaPPUTransferNTaddress)
		sta $0d ; Affect random number

		; check du bord l'écran (bloc x0,y28 a un offset de 0380)
		; si $300,x et $301,x = #$0380 alors $0c, $0d = #$0100 
		lda tsaPPUTransferNTaddress+1,x ;on garde le 8 dans highbit
		and #$80                        ; 
		ora tsaPPUTransferNTaddress,x   ; on ajoute le 3 du lowbit
		and #$83                        ; 
		cmp #$83 ; on test #83          ; et on regarde si c'est bien 83
		bne .setAddr
		
		; sinon
		lda #01
		sta $0d
	.setAddr:
		lda tsaPPUTransferNTaddress, x
		sta PPUADDR
		lda tsaPPUTransferNTaddress+1, x
		sta PPUADDR
	.setData:
		lda [$0e], y
		sta PPUDATA
		iny
		tya
		and $0d ; si pas 0 on continue
		bne .setData

		; pour être sur d'avoir la bonne adresse
		lda $0D
		cmp #$03
		beq .loop16
		iny
		iny
	.loop16:
		tya
		and #$F
		beq .setAttr
		inc tsaPPUTransferNTaddress+1, x ; on passe à la colonne suivante
		jmp .setAddr ; on repart pour un tour
	.setAttr:
		lda tsaPPUTransferAttrAddress, x
		sta PPUADDR
		lda tsaPPUTransferAttrAddress+1, x
		sta PPUADDR
		lda tsaPPUTransferAttrData, x
		sta PPUDATA

		dec tsaPPUtransferSize
		beq .end
		clc
		lda $0e
		adc #$15
		sta $0e
		clc
		lda $0C
		adc #$15
		bne .init
	.end:
		lda PPU2000value
		AND #$fb ; on remet la ppu inc by 1
		sta PPU2000value
		sta PPUCTRL
		rts

TsaBitManipulation:
	lda tsaPPUtransferSize
	cmp #$40 ; si != 40 
	bne PPURamBitSet
	
	ldx #$00
	sta tsaPPUtransferSize
	.loop:
		lda tsaPPUTransferNTaddress
		sta PPUADDR
		lda tsaPPUTransferNTaddress+1
		sta PPUADDR

		.loopTransfer:
			lda tsaPPUTransferNTdata, x
			sta PPUDATA
			inx
			txa
			and #$07
			bne .loopTransfer
		
		clc
		lda tsaPPUTransferNTaddress+1
		adc #$20
		sta tsaPPUTransferNTaddress+1

		;ldy tsaPPUtransferSize
		;lda tsaPPUTransferRestData, y
		;sta tsaPPUTransferRestData

		ldy #$00
		jsr PPURamBitSetSingle

		inc tsaPPUtransferSize
		inc tsaPPUTransferAttrAddress+1

		cpx #$10
		bne .loop

	lda #$00
	sta tsaPPUtransferSize

	rts

PPURamBitSet:


	rts

PPURamBitSetSingle:
	lda tsaPPUTransferAttrAddress, y
	sta PPUADDR
	lda tsaPPUTransferAttrAddress+1, y
	sta PPUADDR

	lda PPUDATA
	;lda PPUDATA
	;and tsaPPUTransferAttrData, y
	;ora tsaPPUTransferRestData, y

	;pha
	;lda tsaPPUTransferAttrAddress, y
	;sta PPUADDR
	;lda tsaPPUTransferAttrAddress+1, y
	;sta PPUADDR
	;pla
	lda tsaPPUTransferAttrData, y
	sta PPUDATA
	
	rts

;---------------- ROOM-----------------

DrawBlockFromActiveLevelMap:
	lda stageId
	jsr BankSwitchStage
	inc tsaPPUtransferSize
	jsr CalculateNametableAddress

	; Save des prev data
	lda $0c ; Courante colonne
	pha
	lda $0d ; Offset tsaPPuTransfer
	pha
	lda $0e ; Sera écrasé plus loin
	pha
	lda $05 ; Courante room
	pha
	sta $0c
	lda $04 ; tmp objectPosX
	and #$E0
	sta $0d 
	lda $04 ; tmp objectPosX
	and #$1F
	asl a
	asl a
	asl a
	sta $0e 

	; collision avec actifs 
	ldy #$00
	jsr CheckCollisionAgainstActives
	tay
	; Restore data
	pla
	sta $05; Courante room
	pla 
	sta $0e ; Sera écrasé plus loin
	pla
	sta $0d ; Offset tsaPPuTransfer
	pla
	sta $0c ; Courante colonne

	ldy $05 ; on va chercher la room à afficher en fonction de l'ordre
	lda ROOM_ORDER, y ;
	asl a
	tay
	lda ROOM_POINTER_TABLE, y
	sta currentRoomPointer
	lda ROOM_POINTER_TABLE+1, y
	sta currentRoomPointer+1
	
	lda $04 ; le début du bloc matrice / 4 pour avoir l'id du submatrice
	lsr a
	lsr a
	tay
	lda [currentRoomPointer], y ; 4x4 blocs
	
	pha
	ldy #$00
	sty currentRoomPointer+1 ; reset
	asl a
	rol currentRoomPointer+1
	asl a
	rol currentRoomPointer+1
	tay
	; bloc 
	lda #LOW(ROOM_BLOCK_DATA)  ;ROOM_BLOCK_DATA $b000
	sta currentRoomPointer
	lda #HIGH(ROOM_BLOCK_DATA)
	sta currentRoomPointer+1
	jsr Write32x32BlockToBuffer
	jsr Adjust32x32BlockAddress
	; Palette du bloc
	pla
	tay
	lda ROOM_BLOCK_PALETTE, y
	sta tsaPPUTransferNTdata-2,x
	inx
	stx $0d

	lda saveBank
	jsr BankSwitch

	rts

; If $05 = 1 alors $09,$10 = #$2023 sinon #$2427
CalculateNametableAddress:
	ldx #$20 ; ppu à 2023
	ldy #$23

	lda $05
	and #$01
	beq .next
	ldx #$24 ; $05 = 1 ppu à 2427
	ldy #$27
	.next:
	stx $08
	sty currentRoomPointer+3

	lda #$00
	sta currentRoomPointer+1
	; Matrice currentPointer
	lda $04 ; on se place en haut de la colonne (fc & 1f = 1c)
	and #$1F
	asl a  ; *32 donnera la position currente du pointer sur 2o
	asl a
	asl a
	asl a
	rol currentRoomPointer+1
	asl a
	rol currentRoomPointer+1
	sta currentRoomPointer
	; PPUADDR
	lda $04
	and #$e0
	lsr a
	lsr a
	lsr a
	ora currentRoomPointer
	ldx $0d ; offset tsaPPuTransfer
	sta tsaPPUTransferNTaddress+1, x

	lda $08
	ora currentRoomPointer+1
	sta tsaPPUTransferNTaddress, x

	inx
	inx
	
	rts

CheckCollisionAgainstActives:
	lda forcedInputFlag
	bne .end ; ignore ce test si forcedInputFlag

	.loop:
		lda roomActiveTable, y ; type d'actif $00 = on zappe
		beq .next
		lda roomActiveTable+1, y ; écran
		cmp $0c ; object actual screen
		beq .checkX1 ; active = object, check colision   
		bcc .next ;  ; active >= object, safe
		jmp .end
		.checkX1:
			lda roomActiveTable+2, y
			cmp $0d ; object x
			beq .checkX2 ; active = object, next check x2
			bcs .next ; active >= object, safe
		.checkX2:
			lda roomActiveTable+4, y 
			beq .checkY1 ; a = $00 no right no check
			cmp $0d
			beq .next ; active = object, safe object next to active
			bcc .next ; active >= object, idem
		.checkY1:
			lda roomActiveTable+3,y 
			cmp $0E ; object y
			beq .checkY2  ; active = object, check y2
			bcs .next ; active < object, safe
		.checkY2:
			lda roomActiveTable+5, y
			beq .shutter ; a $ 00, top no check y2
			cmp $0E ; object y
			beq .next ; active = object, safe object next to active
			bcc .next ; active >= object, idem
		.shutter:
			lda objectId
			bne .anotherTYpe
			; pour le player
			lda roomActiveTable, y ;type active
			ora #$80
			rts
		.anotherTYpe: ; je sais pas encore
			cmp #$04 
			beq .end
			lda #$01
			rts
		.next:
			tya
			clc
			adc #$06 ; un actif à 6 octets d'info
			tay
			bne .loop

	.end:
		lda #$00
	rts

; Transfere 4x4 blocs dans tsaPPUTransferData
;
; 0   2   8   A
;  (0)     (2)
; 1   3   9   B
;
; 4   6   C   E
;  (1)     (3)
; 5   7   D   F
;
; [16x16] [16x16]
; 
; [16x16] [16x16]
Write32x32BlockToBuffer:
	lda #$02
	sta $0e
	.loopBlockX4:
		lda #$02
		sta $0f
	.loopBlockX2:
		lda [currentRoomPointer], y
		clc
		sta tsaPPUTransferNTaddress, x   ; ici c'est bien de la data qui sera stockée
		adc #$01
		sta tsaPPUTransferNTaddress+1, x
		adc #$01
		sta tsaPPUTransferNTdata+2, x
		adc #$01
		sta tsaPPUTransferNTdata+3, x

		inx
		inx
		iny
		dec $0f
		bne .loopBlockX2

		inx
		inx
		inx
		inx
		dec $0e
		bne .loopBlockX4

	rts

Adjust32x32BlockAddress:
    lda $04
    rol a

    pha
    rol a
    rol a
    rol a
    and #$07
    sta currentRoomPointer+2
    pla

    and #$38
    ora currentRoomPointer+2
    ora #$C0
    sta tsaPPUTransferNTaddress+1,x
	; ici 24 alors que currentRoomPointer+3 devrait etre 27 
    lda currentRoomPointer+3 
    sta tsaPPUTransferNTaddress+0,x
    inx
    inx

    rts


DrawBLocksScroll:
	lda stageId
	jsr BankSwitchStage
	
	; Calcul de l'adresse de la nameTable
	lda var33 ; 0x003b (addr low)
	lsr a ; on récupère le MSB 3 addr low
	lsr a
	lsr a
	lsr a
	sta tsaPPUTransferNTaddress

	lda var33 ; 0x003b (addr high) 
	asl a ; alignement des bit 0 et 1 pour le masque
	asl a
	asl a
	pha
	and #$18 ; récupération des bits 
	sta tsaPPUTransferNTaddress+1
	
	pla
	asl a
	and #$c0
	ora tsaPPUTransferNTaddress+1 ;ajout du res du masque avec addr high
	sta tsaPPUTransferNTaddress+1

	; Calcul de l'adresse des attributs
	;    Pour ça on combine les trois bits du MSB et les deux bits du LSB
	lda var33 ; 0x003b (0011 1011)
	and #$f8  ; save de 1111 1000 (0011 1000)
	ora #$c0  ; ajout des bits 7 et 6 (1111 1000)
	sta tsaPPUTransferAttrAddress+1

	lda var33
	and #$03
	asl a
	ora tsaPPUTransferAttrAddress+1
	sta tsaPPUTransferAttrAddress+1

	; Ajustement de l'adresse en fonction de ScroolPosScreen
	ldx #$20
	lda scrollPosScreen ; si l'id du screen est pair NT 20 sinon 24
	and #$01
	beq .adjustments
		ldx #$24
	.adjustments: ; on l'ajoute au reste pour avor l'adresse complète
		; pour NT
		txa
		ora tsaPPUTransferNTaddress
		sta tsaPPUTransferNTaddress  
		; Pour Attr
		txa
		ora #$03
		sta tsaPPUTransferAttrAddress
	
	; Draw lignes horizontales
	lda #$00
	sta $0c ; Msb pointer 8o
	lda var33 ; iterateur lignes horizontales
	and #$3b ; avant dernières bande de 8 pixels horizontale
	lsr a ; division par 8
	ror $0c
	lsr a
	ror $0c
	lsr a
	ror $0c
	lsr $0c
	ora $0c
	sta $0c

	ldx objectPosScreen
	lda ROOM_ORDER, x
	asl a ; *2 poure le word
	tax ; key pour ROOM_POINTER_TABLE
	lda ROOM_POINTER_TABLE, x
	sta $04
	lda ROOM_POINTER_TABLE+1, x
	sta $05

	ldx #$00
	stx $0d

	.loop:
		lda #$00
		sta currentRoomPointer+1
		; saves
		lda $0d
		PHA
		lda $0e
		PHA
		lda $05
		pha
		lda $0c
		PHA
		and #$38
		asl a
		asl a
		sta $0d
		lda $0c
		and #$07
		lsr A
		ror A
		ror A
		ror A
		sta $0e

		lda objectPosScreen
		sta $0c
		ldy #$00
		jsr CheckCollisionAgainstActives
		tay
		pla
		sta $0c
		pla
		sta $05
		pla
		sta $0e
		pla
		sta $0d

		ldy $0c
		lda [$04], y ; 4x4 blocs
		sta $0f
		asl A
		rol currentRoomPointer+1
		asl A
		rol currentRoomPointer+1
		tay
		lda #HIGH(ROOM_BLOCK_DATA)
		ora currentRoomPointer+1
		sta currentRoomPointer+1
		lda #LOW(ROOM_BLOCK_DATA)
		sta currentRoomPointer
		lda var33
		and #$04 ; Première ligne
		beq .initLoop
		iny
		.initLoop:
			lda #$02
			sta $0e
		.loopBlock:
			lda [currentRoomPointer], y
			;asl a
			;asl a
			clc
			sta tsaPPUTransferNTdata, x
			adc #$01
			sta tsaPPUTransferNTdata+8, x
			adc #$01
			sta tsaPPUTransferNTdata+1, x
			adc #$01
			sta tsaPPUTransferNTdata+9, x
			INX
			INX
			INY
			INY
			dec $0e
			bne .loopBlock

		lda var33
		ldy #$0f
		and #$04
		beq .next2
		ldy #$f0
		.next2:

		sty tsaPPUTransferAttrData
		ldy $0f
		lda ROOM_BLOCK_PALETTE, y
		sta tsaPPUTransferAttrData
		;and tsaPPUTransferAttrData
		;ldy $0d
		;sta tsaPPUTransferRestData, y
		lda $0c
		ora #$08
		sta $0c
		inc $0d
		lda $0d
		cmp #$02
		beq .loopEnd
		jmp .loop

	.loopEnd:
		lda #$40 ; set 0x40 pour Transfer manipulation bit
		sta tsaPPUtransferSize

		;lda #$ff
		;eor tsaPPUTransferAttrData
		;sta tsaPPUTransferAttrData

		lda saveBank
		jsr BankSwitch
	rts

RoomLayoutLoadRoomNum:
	tya ; Save Y key ROOM_LAYOUT_TABLE
	pha

	lda stageId
	jsr BankSwitchStage

	pla ; Restore y 
	tay

	cpy #$00
	bmi .end

	lda ROOM_LAYOUT_TABLE, y
	pha

	TXA
	pha

	lda saveBank
	jsr BankSwitch

	pla
	tax

	pla
	rts

	.end:
		lda saveBank
		jsr BankSwitch
		lda #$00
		rts

UpdateCurrentTileState:
	; Reset de la currenTileState
	lda #$00
	sta tileLadderState
	; POS du player
	lda objectPosX       ; posX
	sta $00              ; tmp objectPosX
	lda objectPosScreen  ; Ecran actuel
	sta $01              ; tmp objectPosScreen
	lda objectPosY       ; posY
	sta $03              ; temp objectPosY
	jsr ObjectVerifyBackgroundCollision
	
	sec
	lda objectPosYFraction
	sbc objectYSpeedFraction
	lda objectPosY
	sbc objectYSpeed
	ldx objectYSpeed
	bmi .movingDown ; si objectYSpeed = 0xff
	; movingUp:
		sec
		sbc #$0c ; 1.5 tuile du dessus
		jmp .readTile
	.movingDown:
		clc
		adc #$14 ; 1.5 tuile du dessous
	.readTile:
		sta $0e ; Y de la tuile à tester 
		lda stageId 
		jsr BankSwitchStage
		jsr ReadCurrentStageMap
		cmp #$02
		bne .end
		lda tileLadderState
		ldx objectYSpeed
		bmi .enableClimbDown
		; enableClimUp:
			ora #$10 ; force climbing up
			bne .setTileLadderState
		.enableClimbDown:
			ora #$01; force climbing down	
		.setTileLadderState:
			sta tileLadderState
	.end:
		lda saveBank
		jsr BankSwitch
		rts

ObjectVerifyBackgroundCollision:
	lda stageId 
	jsr BankSwitchStage
	lda $01 ; tmp objectPosScreen
	sta $0c ; tmp tmp objectPosScreen
	lda $00 ; tmp objectPosX
	sta $0d ; tmp tmp objectPosX
	ldx objectId
	bne .enemy
	ldx #$02 ; id blockHeightTable
	
	.loop:
		clc
		lda $03 ; tmp objectPosY
		adc blockHeightTable, x
		sta $0e ; posY à check
		jsr ReadCurrentStageMap
		sta currentTuilesState,x ; rest currentTileStat
		dex
		bpl .loop

	jsr AnalyzeCurrentTile
	
	pha

	lda saveBank
	jsr BankSwitch

	pla

	rts

	.enemy:
		rts

;
; $0E posY checker
; $0C tmp ObjectPosScreen
; $0d  tmp objectPosX
;
DoCollisionCheckFor:
	lda $0a; save xpos
	pha

	lda stageId
	jsr BankSwitchStage

	pla
	sta $0a ; restor xpos

	lda $03 ; tmp objectPosY
	sta $0e ; tmp tmp objectPosY
	
	ldx objectId
	bne .analyzeTiles

	lda #$00
	sta currentTuilesState+1
	ldx #$03
	
	.loop:
		clc
		lda $0a ; tmp objectPosX
		adc XWidthTable, x
		sta $0D ; tmp objectPosX
		lda $02 ; tmp objectPosScreen
		adc XWidthTable-1, x
		sta $0c
		jsr ReadCurrentStageMap
		dex
		sta currentTuilesState,x
		dex 
		bpl .loop

	.analyzeTiles:
		jsr AnalyzeTiles

	pha ; on push le resultat
	lda saveBank
	jsr BankSwitch
	pla
	rts

XWidthTable:
    .db $00,$07 ;+7
    .db $FF,$F9 ;-7

;
; permet de checker les 3 tuiles sur lesquelles est le perso:
;     $f4: 3 tuiles par rapport au haut du perso 
;     $fc: 2 tuiles ...
;     $0b: 1 tuile  ...
; 
blockHeightTable:
	.db $f4, $fc, $0b


AnalyzeCurrentTile:
	ldx #$02
	ldy #$00
	lda objectFlags
	and #$df ; clear le bit de l'échelle
	sta objectFlags

	.loop:
		lda currentTuilesState, x ; 
		bpl .posTiles
		and #$7f
		cmp #$03
		bne .posTiles
		lda #$02 ; porte doit s'ouvrir
		sta currentStripeEndType
		bne .do1And4
	.posTiles:
		cmp #$01 ; tuile = 01 (transparent)
		beq .do1And4 
		cmp #$04 ; tuile = 04 (transparent)
		bne .killAble
	.do1And4:
		ldy #$01 ; 
		bne .end ; inconditionel jump
	.killAble:
		cmp #$03 ; tuile = 03 (kill) 
		beq .killPlayer	
	.climbable:
		cmp #$02 ; tuile = 02 (climbable)
		bne .resetWalkTimer
		lda tileLadderState
		ora CurrentTileStateTable, x
		sta tileLadderState
		jmp .nextLoop
	.resetWalkTimer:
		cmp #$05
		bne .nextLoop
	.nextLoop: ; tuile = 0 passera directement ici elle valide aucune condition
		dex
		bpl .loop
	.end:
		tya
		rts
	.killPlayer:
		jmp KillPlayer	
	
ReadCurrentStageMap:
	; collision avec actifs
	ldy activesLowerIndex
	jsr CheckCollisionAgainstActives
	cmp #$00
	beq .next
		rts
	.next:
	; check bord de l'écran bas
	lda $0e ; blocY  à checker
	cmp #$f0 ; bord bas de l'écran 
	bcc .checkTuile ;non 
	ldy objectId
	lda objectPosY, y
	cmp #$f1 ; idem bord bas de l'écran
	bcs .resetY
	cmp #$80
	bcs .checkTuile
	.resetY:
		; quand Ypos < #$80 or Ypos >= #$F1 and $0E != #$F0
		lda #$00
		sta $0e
	.checkTuile:
		ldy $0c ; tmp objectPosScreen écrant actuel
		lda ROOM_ORDER, y ; id room affichée
		asl A
		tay
		lda ROOM_POINTER_TABLE, y
		sta $04
		lda ROOM_POINTER_TABLE+1, y
		sta $05
		lda $0d ; tmp objectPosX
		lsr a
		lsr a
		and #$38
		sta $07 ; currentRoomPointer+1
		lda $0e ; tmp posY
		rol a
		rol a
		rol a
		rol a
		and #$07
		ora $07 ; currentRoomPointer+1
		tay

		lda [$04], y
		ldy #$00
		sty $09 ; currentRoomPointer
		asl a   ; x4 sur 2 octets pour avoir ROOM_BLOCK_DATA
		rol $09
		asl a
		rol $09
		tay ; id ROOM_BLOCK_DATA

		lda $0E ;  blocY à checker
		and #$10
		beq .checkY
		iny
	.checkY:
		lda $0d ; tmp objectPosX
		and #$10
		beq .getTile
		iny
		iny
	.getTile:
		lda #LOW(ROOM_BLOCK_DATA)  ;ROOM_BLOCK_DATA $b000
		sta $08
		lda #HIGH(ROOM_BLOCK_DATA)
		ora $09
		sta $09

		lda [$08], y ; La tuile
		and #$c0 ; on veut que les bits 6 et 7
		;asl a    ; décalage vers la gauche de 2
		;asl a
		clc
		ldy stageId
		sty $06
		asl a
		rol $06
		asl a
		rol $06
		ldy $06
		lda BlockTransparencyMap, y
		cmp #$02
		bne .noClimbable ; ou porte
		lda objectId
		bne .noPlayer
		; pour le player
		lda $0d
		and #$f0
		sta ladderPosX ; $2E
	.noPlayer:
		lda #$02
	.noClimbable:
		
	rts
	
;
; bloc par lvl
;
; $02 block climable up
; $03 block killable (pique)
;
BlockTransparencyMap:
	.db $00, $01, $02, $03
	.db $00, $01, $02, $03 ; lvl 1

; CurrentTileStateBits
CurrentTileStateTable:
	.db $08 ; player est sur une tuile echelle
	.db $04 ; tuile échelle au dessus (2px)
	.db $02 ; tuile échelle au dessus (4px)

firstDoorLocations:
	.db $00, $13 ; 1er octet mort...

; FirstDoorShutterDataIndex
bossDoorLocations: 
	.db $00, $16	

OpenFirstDoor:
	;ldx stageId
	lda #$00 ; firstDoorShutterDataIndex, x pour ROOM_SHUTTER_INFO, x
	jmp AnimateDoor

; BossRoomShutterDataIndex
bossRoomShutterData:
	.db $00, $09

CloseBossDoor:
	ldx stageId
	lda bossRoomShutterData, x
	; go AnimateDoor 
	; /!\ NE PAS DEPLACER
AnimateDoor:
	pha ; save iterator pour ROOM_SHUTTER_INFO 

	inc objectLifeCycleCounter ; freeze player
	lda stageId
	jsr BankSwitchStage
	
	; todo sound ouverture porte
	pla ; restore iterator
	TAX
	lda ROOM_SHUTTER_INFO, x ; nb tiles de la porte (généralement #04)
	sta $11 ; $5A data shutter
	inx ; inc et save iterator
	stx $10; 59

	.loop:
        lda stageId
        jsr BankSwitchStage

		ldx $10 ; load iterator
		lda #$00
		sta $0d ; offset tsaPPUTransfer

		lda objectPosScreen
		sta $05
		lda ROOM_SHUTTER_INFO, x ; colonne
		sta $04; numero de colonne
		inx 
		stx $10 ; save iterator
		jsr CalculateNametableAddress ;todo comprendre pour quoi $4 = $f0
		ldy $10 ; restore iterator
		lda ROOM_SHUTTER_INFO, y; tuile
		;asl a
		;asl a
		adc #LOW(ROOM_SHUTTER_BLOCK_DATA)
		sta currentRoomPointer
		lda #HIGH(ROOM_SHUTTER_BLOCK_DATA)
		sta currentRoomPointer+1
		ldy #$00
		jsr Write32x32BlockToBuffer
		jsr Adjust32x32BlockAddress 

		ldx $10 ; restore iterator
		ldy ROOM_SHUTTER_INFO, x
		lda ROOM_SHUTTER_BLOCK_PALS, y
		sta tsaPPUTransferAttrData
		inx 
		stx $10; save iterator

		lda #$01
		sta tsaPPUtransferSize; 1 bloc
		lda #$06
		jsr TimeDelayWithSpriteUpdates
		
		dec $11 ; dec nombre de bloc
		bne .loop

		lda #$00
		sta objectLifeCycleCounter; defreeze player
	rts

; A nb frame en pause
TimeDelayWithSpriteUpdates:
	sta miscCounter
	.loop:
		jsr UpdateGraphics
		jsr NextFrame
		dec miscCounter
		bne .loop

	rts


SpikeKill1:
	jmp KillPlayer
AnalyzeTiles:
	ldx #$02
	lda #$00
	.loop:
		ldy currentTuilesState, x
		bpl .dontTransparent
	; transparent:
	.dontTransparent:
		cpy #$03
		beq SpikeKill2
		cpy #$01
		beq .hardBlock
		cpy #$04
		bne .loopNext
	.hardBlock:
		ora #$01
	.loopNext:
		dex
		bpl .loop
	.loopEnd:
		tay
		ldx objectId
		bne .end
		tya
		bne .end
		lda objectYSpeed
		bpl .end
		lda tileLadderState
		cmp #$01
		bne .end
		ldy #$01	
	.end:
		tya
		rts

SpikeKill2:
	jmp KillPlayer		

KillPlayer:
	; TODO sound
	lda #$00
	sta objectActionStateCounter
	; Death anim
	.initLoop:
		lda #$05
		sta miscCounter
		.loopMisCounter:
			jsr NextFrame
			dec miscCounter
			bne .loopMisCounter

	lda objectSpriteNum
	cmp #$19
	beq .next
		lda #$19
		sta objectSpriteNum
		lda saveBank
		jsr BankSwitch
		jsr UpdateGraphics
		lda #$80
		sta miscCounter
		bne .loopMisCounter
	.next:

	jsr DisableNMIPPU ; stop PPU
	; reset variable
	lda #$00
	sta objectPosScreen
	sta playerWalkTimer
	sta playerBlinkState

	lda #$00
	sta pointer+2
	lda #$01
	sta pointer+3
	jsr WriteChr
	; todo restart point LastRestartPointType
	jmp StageBegin



;-------Graphics-----------
UpdateGraphics:
	lda saveBank
	jsr BankSwitch
	

    ; hide tous les sprites
	ldy #$00
	sty $0d ;  save spriteCounter
	ldx #$40
	jsr HideSprites

	lda frameCounter
	and #$01
	beq .oddFrame
	; frame impaire
	jsr DrawWeaponAndMetters
	lda #$00
	sta $0c ; on commence par draw le début du tableau de sprite
	.loopDrawEven:
		jsr DrawObject
		inc $0c
		lda totalObjects
		cmp $0C
		bne .loopDrawEven

		rts
	; frame paire
	.oddFrame:
		ldx totalObjects
		dex
		stx $0C
	.loopDrawOdd:
		jsr DrawObject
		dec $0C
		bpl .loopDrawOdd
	jsr DrawWeaponAndMetters

	rts

DrawWeaponAndMetters:
	; Life
	ldy $0d ; restore spriteCounter
	lda drawMetersFlag
	bmi .end
	lda #$fe ; index tiles vides
	sta $02
	lda #$fA ; index tiles pleines
	sta $03
	lda #$00 ; sprite attr
	sta $06
	lda #$08 ; X coord
	sta $04
	lda #$48 ; Y coord
	sta $05
	lda meters
	jsr DrawMeter

	; weapon
	lda weaponSelect
	jsr DrawWaepon
	.end:
		rts

DrawWaepon:
	; weapon icon
	lda weaponSelect 
	clc 	 ; tile id		
	adc #$f6 ; emplacement du premier sprite weapon dans la bank
	sta $09
	lda #$08 ; X pos
	sta $04
	lda #$10 ; Y pos
	sta $05
	lda #$00
	sta $06
	jsr WriteSprite

	; Weapon meters
	lda #$E6
	sta $09
	sta $07 ; Save sprite digit 2
	lda weaponSelect
	beq .next
	; digit value + digit bank emplacement (ex weapon 2 + #$e6)
	; digit 1 value
	lda weaponMeter
	clc
	adc #$E6
	sta $09
	; digit 2 value
	lda weaponMeter+1
	clc 
	adc #$E6
	sta $07
	.next:
	
	; digit 1 tile
	lda #$10 ; X pos
	sta $04
	lda #$10 ; Y pos
	sta $05
	lda #$00
	sta $06
	jsr WriteSprite
	; digit 2 tile
	lda $07 ; Restore sprite digit 2
	sta $09
	lda #$18 ; X pos
	sta $04
	lda #$10 ; Y pos
	sta $05
	lda #$00
	sta $06
	jsr WriteSprite

	rts

;
; $0c Id de l'objet
;     Ex: si perso $0c = 0
;
DrawObject:
	ldx $0c ; Id de l'objet
	lda objectPosY, x  
	cmp #$f8 ; Si 0xf8 fin !
	bne .next
	rts
	.next:
	
	; Get config en fonction du meta sprite
	sta $0f ; Save pos Y du meta sprite
	lda objectSpriteNum, x  ; L'id du sprite en cours
	cmp #$ff ;sprite enemi 
	bne .adjustSprite
	jmp DrawEnemy
	.adjustSprite:
		lda objectFireDelay, x  ; Division par 16, (ajoute 1 au sprite actuel pour le changer)
		lsr a                   ; Si action en cours on l'ajoute a l'objectSpriteNum
		lsr a                  
		lsr a
		lsr a
		clc
		adc objectSpriteNum, x
		asl a                 
		tay                    ; Iterator pour metaSpritesActionTable

	; metaSpriteActionXX
	lda metaSpritesActionTable, y
	sta $00;
	lda metaSpritesActionTable+1, y
	sta $01

	lda objectActionStateCounter, x ; Counter qui update du metaSprite 
	lsr a                     ;  de l'action
	lsr a
	lsr a
	lsr a
	tay                    
	iny                     
	
	; todo gestion du boss blinkState
	lda [$00], y ; L'id de l'état du metaSprite
	pha ; save  metaSpriteConfig
		; todo changmement d'état d'un metasprite (D1AD)	
	lda objectLifeCycleCounter, x
	beq .objetFireDelay
	jmp .dataMetaSprite
	.objetFireDelay:
		lda objectFireDelay, x
		beq .resetObjetFireDelay
		and #$0f ; on garde que le LSB todo à voir pourquoi
		tay
		dey
		beq .resetObjetFireDelay
		dec objectFireDelay, x
		bne .updateobjectActionStateCounter
	.resetObjetFireDelay:
		lda #$00
		sta objectFireDelay, x	
	.updateobjectActionStateCounter:
		jsr UpdateObjectActionStateCounter
	.dataMetaSprite:
		
		; dataMetaSpriteXX
		pla
		asl a
		tay
		lda dataMetaSpriteTable, y
		sta $00
		lda dataMetaSpriteTable+1, y
		sta $01
DrawObjectSet:
		ldy #$00 ; reset y
		sty $09 ; save iterator dataSprite

		; Nombre de tiles dans le metaSprite
		lda [$00], y
		sta $0e
		
		; Offset pour les coordonnées
		; offsetTable
		iny
		lda [$00], y ;Id offsetTable
		asl a
		tay
		lda offsetTable, y
		sta $02
		iny 
		lda offsetTable, y
		sta $03

		; Transpose objet posX avec le scrolling
		ldy #$02
		sec
		lda objectPosX, x
		sbc scrollPosX
		sta $0a ; $0f
		
		; Flip du sprite
		lda objectFlags, x
		and #$40 ; 0x40 = tourné vers la droite
		sta $08 ; $011

		; Todo hack behind background

		ldx $0d ; sprite counter
		lda $0c ; id objet
		beq .noBlinkState
		lda playerBlinkState
		and #$02
		beq .noBlinkState
		jmp .skip ; Perso est en blink state on skip
		.noBlinkState:

	; loop tiles
	; $0a = posX
	; $0f = posY
	; $00, $01 = pointer tile et attr
	; $02, $03 pointer offset X et Y
	; y = sprite Id
	.loop:
		; Tile
		lda [$00],y 
		sta CURRENT_SPRITE_DATA+1, x
		iny

		; Attr
		lda [$00], y
		eor $08
		sta CURRENT_SPRITE_DATA+2, x
		iny      ; save du y
		sty $0d

		; PosY
		ldy $09
		lda [$02], y ; Id offset X et Y 
		tay
		clc
		lda offsetY, y ; local posY
		adc $0f             ; On ajoute objet posy locale Y
		sta CURRENT_SPRITE_DATA, x ; sprite pos Y
		;bcc .setPosY           ; si dans l'écran on set Y
		;bcs .skipThisSprite ; sinon skip le sprite

		;.skipThisSprite:	
			;lda #$f8 ; pour être sûr qu'il soit en dehors
			;sta CURRENT_SPRITE_DATA, x ; sprite pos Y
			;ldy $0d ; get spriteCounter
			;jmp .nextSprite
		;.setPosY:
			;sta CURRENT_SPRITE_DATA, x 
			
		; Pos X
		lda $08 ; get flip
		beq .facingLeft
		; facingRight
		lda offsetRightX, y ; local posX right
		jmp .handlePosX

		.facingLeft:
			lda offsetLeftX, y ; local posX left
		.handlePosX:
			clc
			adc $0a ; Additionne localPosX + objetPosX
			;bcc .setPosX;
			;bcs .skipThisSprite

		.setPosX:
			sta CURRENT_SPRITE_DATA+3, x ; pos X
			ldy $0d
			inx
			inx
			inx
			inx
			cpx #$fc
			beq .end

		.nextSprite:
			inc $09 ; iterator dataSprite
			dec $0e ; Nombre de sprite     ICI ça plante
			bne .loop
	.skip:
		stx $0d ; counterSprite
	.end:
		rts

DrawEnemy:
	lda objectFlags, x
	and #$20 ; si invisible
	beq .next
	rts
	.next:

	lda objectType, x
	asl a
	tay

	; metaEnemySpritesTable 
	lda metaEnemySpritesTable, y
	sta $00
	lda metaEnemySpritesTable+1, y
	sta $01

	lda objectActionStateCounter, x
	lsr a ;*16
	lsr a
	lsr a
	lsr a 
	tay 
	iny

	; Counter de chagement de state
	lda [$00], y
	pha
	
	lda objectLifeCycleCounter, x
	beq .updateActionStateCounter
	jmp .dontUpdateActionStateCounter
	.updateActionStateCounter:
		jsr UpdateObjectActionStateCounter
	.dontUpdateActionStateCounter:

	pla
	asl a
	tay

	lda dataMetaSpriteEnemyTable, y
	sta $00
	lda dataMetaSpriteEnemyTable+1, y
	sta $01
	jmp DrawObjectSet


	rts

;
; $00
; 
UpdateObjectActionStateCounter:
	ldy #$00
	lda [$00], y ; Premier octet de metaSpritesActionXX

	; les unités sont le LSB
	pha
	and #$0f
	sta $05 ; save

	; les dizaines sont le MSB
	pla
	and #$f0
	sta $06 ; save

	lda objectActionStateCounter, x
	and #$0f
	cmp $05
	bcs .incDizaine 
	; inc unité
	inc objectActionStateCounter, x
	bne .end
	.incDizaine:
		lda objectActionStateCounter, x
		and #$f0
		cmp $06
		bcs .reset
		clc
		adc #$10
		bne .setActionStateConter
	.reset:
		lda #$00
	.setActionStateConter:
		sta objectActionStateCounter, x
	.end:
		rts


;
; A = meter valeur (#$1c max)
; $04,$05 = x,y coord du point le plus bas de la barre de vie
; Y = index dans la page de sprite
; $02 = index empty tuile aussi utiliser pour les fraction (3/4, 1/2, 1/4, vide)
; $03 = index tuile pleine
;
DrawMeter:
	pha 	
	lsr a
	lsr a
	sta $07
	pla
	and #$03  ; Fraction (3/4, 1/2, 1/4)
	sta $08

	sec     
	lda $02
	sbc $08
	sta $08

	ldx #$00
	.loop:
		; if sprite# < numéro full bloc alors tile#=$03
		; else if sprite# = fraction bloc alors tile#=$08
		; else tile# = $02 empty
		cpx $07       ; vide ? (en fonction de x)
		bcs .noEmpty
		lda $03       ; pleine ?
		bne .setTile
		.noEmpty:
			bne .empty 
			lda $08		; fraction 
			bne .setTile
		.empty:
			lda $02
		.setTile:
			sta $09
			jsr WriteSprite
			inx
			cpx #$07
			bne .loop 
	rts

;
; $04,$05 = x,y coord du sprite
; $09 = tuile id
; $06 = sprite attr
;   Y = index du sprite dans la page 
WriteSprite:
	; X
	lda $04 
	sta CURRENT_SPRITE_DATA+3, y
	; Tuile id
	lda $09
	sta CURRENT_SPRITE_DATA+1, y
	; Attr
	lda $06
	sta CURRENT_SPRITE_DATA+2, y
	; Y
	lda $05
	sta CURRENT_SPRITE_DATA, y
	cpy #$f8
	beq .end
	iny
	iny
	iny
	iny
	cmp #$F8 
	beq .end
	sec
	sbc #$08
	sta $05
	
	sty $0d ; counterSprite 

	.end:
		rts

;-------FRAME---------------

NextFrame:
	tya
	pha

	lda stallTimer
	bne .checkNmiGfxUpdateDone
	lda joyPad
	sta joyPadOld
	lda joyPad+1
	sta joyPadOld+1
	
	; On attend que la nmi ait fini de faire son boulot
	.checkNmiGfxUpdateDone:
		lda #0
		sta nmiGfxUpdateDone
	.waitNmiGfxUpdatedLoop:
		lda nmiGfxUpdateDone
		beq .waitNmiGfxUpdatedLoop
		
	; TODO forced Input flag pour pour le déplacement auto du joueur
	
	lda stallTimer
	bne .handleJoyPad
	jsr ReadJoyPad
	lda joyPad
	
	.handleJoyPad:
		eor joyPadOld
		and joyPad
		sta joyD
		lda joyPad+1
		eor joyPadOld+1
		and joyPad+1
		sta joyD+1
		
	pla
	tay	
	rts
;----------------JoyPad---------------------

ReadJoyPad:
	ldx #$01
	stx $4016
	dex
	stx $4016
	
	.loopButton:
		ldy #08
	.loopPad:
		lda $4016, x
		sta joyD
		lsr a
		ora joyD
		lsr a
		ror joyPad, x
		dey
		bne .loopPad
		dex
		bpl .loopButton
		
	rts

;-----------PPU----------------

; NMI PPU seront réactivés par NMI	
DisableNMIPPU:
	lda PPU2000value
	and #$7F ; disables NMI
	sta PPU2000value
	sta PPUCTRL
	
	lda PPU2001Value
	and #$E7 ; Cache sprites et bg
	sta PPU2001Value
	sta PPUMASK 
	
	rts

;-------------NMI---------------

NMI:
	pha  ; save de a sur la pile
	txa
	pha  ; save x
	tya
	pha  ; save y
	
	; check si si game code a fini
	lda nmiGfxUpdateDone
	beq .gfxUpdated
		; Todo update audio
	.gfxUpdated:
	
	jsr DisableNMIPPU
	lda PPUSTATUS

	; DMA 
	lda #$00 ; lsb
	lda #$00 ; lsb
	sta OAMADDR
	lda #$02 ; msb
	sta OAMDMA
	
	jsr TsaPPUtransfer

	; UpdatePalettes
	lda paletteUpdateDelay
	beq .paletteUpdated
	jsr UpdatePalettes
	.paletteUpdated:
	
	; RawPPUTransfer
	lda ppuTransferRawSize
	beq .ppuTransferRawEnd
	jsr PPUTransferRaw
	.ppuTransferRawEnd:
	
	; scroll position
	lda PPU2000value
	and #$fc
	sta PPU2000value
	lda scrollPosScreen ; Change NT si besoin
	and #$01
	ora PPU2000value
	sta PPU2000value
	sta PPUCTRL

	; Restore NMI
	lda PPU2001Value
	ora #$1e
	sta PPU2001Value
	sta PPUMASK
	
	lda scrollPosX
	sta PPUSCROLL
	lda scrollPosY

	sta PPUSCROLL
	
	lda PPU2000value
	ora #$80
	sta PPU2000value
	sta PPUCTRL
	
	lda #$01
	sta nmiGfxUpdateDone
	inc frameCounter
	
	; update audio
	; random seed
	
	lda currentBank
	lsr a
	tax
	sta bankTable, x
	
	pla ; restore y
	tay 
	pla ; restore x
	tax
	pla ; restore a
	
	RTI 	

;-------------------	
  .bank 15		;07 (2/2 last bank/fixed)
  .org $E000

;InitObjectDefaultSpeed
InitEnemyDefaultSpeed:
	lda defaultEnemySpeed, y
	sta objectYSpeedFraction
	lda defaultEnemySpeed+1, y
	sta objectYSpeed
	lda defaultEnemySpeed+2, y
	sta objectXSpeedFraction
	lda defaultEnemySpeed+3, y
	sta objectXSpeed
	lda defaultEnemySpeed+4, y

	rts

; YSpeedFraction, YSpeed, XSpeedFraction and XSpeed
defaultEnemySpeed:
    .db $E0,$04,$30,$01 ;00 enemi


; Interupt
  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
