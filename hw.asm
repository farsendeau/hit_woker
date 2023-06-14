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
PPU2000value .rs 1
PPU2001Value .rs 1
stageId .rs 1
currentBank .rs 1
saveBank .rs 1
pointer .rs 4
none .rs 2  ; Aussi utilisé pour BankSwitchTile
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

  .rsset $0100
stack .rs 256 ; stack

  .rsset $0200
sprites .rs 256  ;sprite ram

  .rsset $0300
bGPalettes     .rs 16
spritePalettes .rs 16
unknownPalettes .rs 16

tsaPPUTransferNTaddress .rs 2
tsaPPUTransferNTdata    .rs 16
tsaPPUTransferAttrAddress .rs 2
tsaPPUTransferAttrData  .rs 1

ppuTransferRawAddr .rs 2
ppuTransferRawBuf .rs 126  
  
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

;-----------
  .bank 0		;00 (1/2 first bank/swappable)
  .org $8000
  
  .incbin "title.chr"

  .bank 1		;00 (2/2 first bank/swappable)
  .org $A000

;-----------
  .bank 2		;01 (1/2)
  .org $8000

  .bank 3		;01 (2/2)
  .org $A000

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

;-----------

  .bank 10		;05
  .org $8000

; HEADER roomTileTable
;--------+----------------
;byte #  | what it tells us
;--------+----------------
;00      | Nombre de bank
;01      | Nombre de 256 bytes à copier (1 ligne de 16 tiles = 256
;02      | Adresse début de la bank src (offset)
;xx      | Info pour les banks suivantes (comme byte 1 et 2)
;/!\ offset $04 = une ligne; $40 début bank bg

roomTileTable:
	.dw roomTileTitleScreen

roomTileTitleScreen:
	.db $04      ; Nombre de bank
	.db $10, $00 ; bank 1. 16 x 256 (16 lignes) adresse 80+00 = 8000 (début bank sprit)
	.db $03, $40 ; bank 2. 3 x 256 (3 lignes) adresse 80+40 = 9000 (début bank bg)
	.db $04, $4c ; bank 3. 4 x 256 (4 lignes) adresse 80+4c
	.db $09, $5c ; bank 4. 9 x 256 (9 lignes) adresse 80+5c

  .bank 11		;05
  .org $A000

;-----------

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
	lda stageId ; ici stage_id = 0 (landing screen)
	jsr WriteChr
	
	; Init stage palette
	lda #Low(paletteStageTitle)
	sta pointer
	lda #HIGH(paletteStageTitle)
	sta pointer+1
	jsr InitStagePalette

	; 804C
	; $06 = nombre de ligne
	lda #$0B
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

	; init stage 1
	lda #1
	sta stageId
	lda #$11       ; charge bank 11
	jsr BankSwitch
	
	jmp SkitStage

dataTitle:
	.db $20,$CE, $30, $FF
	.db $20,$EA, $31,$32,$33,$00,$34, $FF
	.db $21,$0A, $36,$37,$38,$39,$3a,$00,$00,$00,$3b, $FF 
	.db $21,$29, $3c,$3d,$3e,$3f,$40,$00,$00,$00,$86,$41,$42, $FF
	.db $21,$4B, $45,$00,$46,$00,$47,$48,$49,$4a,$4b,$4c, $FF
	.db $21,$6A, $4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58, $FF
	.db $21,$8B, $59,$5a,$5b,$5c,$5d,$5e,$5f,$60,$61,$62,$63, $FF
	.db $21,$AB, $64,$65,$00,$66,$67,$68,$69,$00,$6a,$6b,$6c, $FF
	.db $21,$CC, $6d,$FF
	.db $21,$D5, $6e,$6f, $FF
	.db $22,$CA, $10,$12,$05,$13,$13,$00,$00,$13,$14,$01,$12,$14, $FF

paletteStageTitle:
	; palette bg
    .db $0f,$02,$2c,$24, $0f,$03,$15,$33, $0f,$0c,$1b,$2c, $0f,$20,$20,$20
	; palette sprite
    .db $0f,$21,$21,$13, $0f,$16,$27,$37, $0f,$16,$27,$37, $0f,$30,$27,$10
	; unknown
    .db $0f,$02,$2c,$24, $0f,$15,$15,$15, $0f,$15,$15,$15, $0f,$0f,$0f,$0f
	
  .bank 13		;06
  .org $A000

;-----------
 
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
	
	lda #12       ; charge bank 12
	jsr BankSwitch
	jmp Reset2

BankSwitch: ; bank # loaded in A
	php
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
BankSwitchTile:
	pha
	and #$fc
	lsr a
	lsr a
	ora #$80 ; Une bank commence à 8000
	sta $0b
	sta echo
	lda #00
	sta $0a
	
	pla
	and #$03 ; On récupère le numérao de bank
	tay
	sta currentBank
	sta bankTable, y
	
	rts
	
;; CHR
; Appellé en début de chaque stage + reset
WriteChr:
	; Récupération de la roomTile pour le stage en cours
	jsr BankSwitch10
	lda stageId
	asl a ; table de words
	tay
	lda roomTileTable, y
	sta pointer
	lda roomTileTable+1, y
	sta pointer+1
	
	ldy #0
	lda [pointer], y ; Nombre de bank qu'il faut récupérer
	sta $08
	
	lda #00
	sta PPUADDR
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
		jsr BankSwitchTile
		
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

;; PALETTE
InitStagePalette:
	; Sprites BG palette
	lda #$20
	jsr WritePalette
	
	; Copie palette bg/sprite en RAM pour l'update
	ldy #$2f
	.loop:
		lda [pointer], y
		sta bGPalettes, y
		dey
		bpl .loop
	
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
		lda [pointer], y ; ($35)
		sta PPUDATA
		iny
		dex
		bne .loop
	
	stx paletteParam ; ici X = 0
	
	lda saveBank
	jmp BankSwitch

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
PPUTranferRaw:
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

NextFrame:
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
		
	rts

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
	
; Scénette de début de stage
SkitStage:
	; Turn off ppu
	jsr DisableNMIPPU
	
	; 

	rts

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
	
	; todo TSAPPUtransfer (juste quelques tuiles)
	
	; UpdatePalettes
	lda paletteUpdateDelay
	beq .paletteUpdated
	jsr UpdatePalettes
	.paletteUpdated:
	
	; todo RawPPUTransfer
	; todo scroll position

	
	; Restore NMI
	lda PPU2001Value
	ora #$1e
	sta PPU2001Value
	sta PPUMASK
	
	lda #0
	sta PPUSCROLL
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

	
mainLoop:	
	inc $0F
	JMP mainLoop
	
;-------------------	
  .bank 15		;07 (2/2 last bank/fixed)
  .org $E000
	
	
; Interupt
  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
