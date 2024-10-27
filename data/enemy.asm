metaEnemySpritesTable:
    .dw metaSpritesEnemy1

metaSpritesEnemy1:
    .db $0c, $00

dataMetaSpriteEnemyTable:
    .dw dataMetaSpriteEnemy1


dataMetaSpriteEnemy1:
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