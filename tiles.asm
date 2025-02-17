INCLUDE "charmap.inc"

FontTiles:
    ; Font tiles used for most text rendering.
    ; Since these tiles are black-and-white, they're stored 1-bit-per-pixel (8 bytes per tile) instead of the usual 2 bpp format

    db $00, $3c, $66, $66, $66, $66, $3c, $00,   $00, $18, $38, $18, $18, $18, $3c, $00 ; '0', '1'
    db $00, $3c, $4e, $0e, $3c, $70, $7e, $00,   $00, $7c, $0e, $3c, $0e, $0e, $7c, $00 ; '2', '3'
    db $00, $3c, $6c, $4c, $4e, $7e, $0c, $00,   $00, $7c, $60, $7c, $0e, $4e, $3c, $00 ; '4', '5'
    db $00, $3c, $60, $7c, $66, $66, $3c, $00,   $00, $7e, $06, $0c, $18, $38, $38, $00 ; '6', '7'
    db $00, $3c, $4e, $3c, $4e, $4e, $3c, $00,   $00, $3c, $4e, $4e, $3e, $0e, $3c, $00 ; '8', '9'
    db $00, $3c, $4e, $4e, $7e, $4e, $4e, $00,   $00, $7c, $66, $7c, $66, $66, $7c, $00 ; 'A', 'B'
    db $00, $3c, $66, $60, $60, $66, $3c, $00,   $00, $7c, $4e, $4e, $4e, $4e, $7c, $00 ; 'C', 'D'
    db $00, $7e, $60, $7c, $60, $60, $7e, $00,   $00, $7e, $60, $60, $7c, $60, $60, $00 ; 'E', 'F'
    db $00, $3c, $66, $60, $6e, $66, $3e, $00,   $00, $46, $46, $7e, $46, $46, $46, $00 ; 'G', 'H'
    db $00, $3c, $18, $18, $18, $18, $3c, $00,   $00, $1e, $0c, $0c, $6c, $6c, $38, $00 ; 'I', 'J'
    db $00, $66, $6c, $78, $78, $6c, $66, $00,   $00, $60, $60, $60, $60, $60, $7e, $00 ; 'K', 'L'
    db $00, $46, $6e, $7e, $56, $46, $46, $00,   $00, $46, $66, $76, $5e, $4e, $46, $00 ; 'M', 'N'
    db $00, $3c, $66, $66, $66, $66, $3c, $00,   $00, $7c, $66, $66, $7c, $60, $60, $00 ; 'O', 'P'
    db $00, $3c, $62, $62, $6a, $64, $3a, $00,   $00, $7c, $66, $66, $7c, $68, $66, $00 ; 'Q', 'R'
    db $00, $3c, $60, $3c, $0e, $4e, $3c, $00,   $00, $7e, $18, $18, $18, $18, $18, $00 ; 'S', 'T'
    db $00, $46, $46, $46, $46, $4e, $3c, $00,   $00, $46, $46, $46, $46, $2c, $18, $00 ; 'U', 'V'
    db $00, $46, $46, $56, $7e, $6e, $46, $00,   $00, $46, $2c, $18, $38, $64, $42, $00 ; 'W', 'X'
    db $00, $66, $66, $3c, $18, $18, $18, $00,   $00, $7e, $0e, $1c, $38, $70, $7e, $00 ; 'Y', 'Z'
    db $00, $00, $00, $00, $00, $60, $60, $00,   $00, $00, $00, $3c, $3c, $00, $00, $00 ; '.', '-'

TitleTiles:
    ; Tileset used on the license & title screens

    db $00, $00, $22, $14, $08, $14, $22, $00, $00, $00, $36, $36, $5f, $49, $5f, $41
    db $7f, $41, $3e, $22, $1c, $14, $08, $08, $ff, $ff, $ff, $81, $c1, $bf, $c1, $bf
    db $c1, $bf, $c1, $bf, $81, $ff, $ff, $ff, $aa, $aa, $00, $00, $00, $00, $00, $00
    db $00, $00, $00, $00, $00, $00, $00, $00, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $fe
    db $fe, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f
    db $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $ff, $00, $ff, $40, $ff, $02, $ff, $00
    db $ff, $10, $ff, $80, $ff, $02, $ff, $00, $f0, $10, $ff, $1f, $ff, $00, $ff, $40
    db $ff, $00, $ff, $02, $ff, $40, $ff, $00, $0f, $08, $ff, $f8, $ff, $00, $ff, $02
    db $ff, $00, $ff, $40, $ff, $02, $ff, $00, $00, $00, $00, $00, $00, $00, $00, $00
    db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $38, $38
    db $18, $18, $18, $18, $18, $18, $3c, $3c, $00, $00, $00, $00, $3c, $3c, $4e, $4e
    db $4e, $4e, $3e, $3e, $0e, $0e, $3c, $3c, $00, $00, $00, $00, $3c, $3c, $4e, $4e
    db $3c, $3c, $4e, $4e, $4e, $4e, $3c, $3c, $00, $00, $38, $38, $44, $44, $ba, $ba
    db $a2, $a2, $ba, $ba, $44, $44, $38, $38, $c6, $c6, $e6, $e6, $e6, $e6, $d6, $d6
    db $d6, $d6, $ce, $ce, $ce, $ce, $c6, $c6, $c0, $c0, $c0, $c0, $00, $00, $db, $db
    db $dd, $dd, $d9, $d9, $d9, $d9, $d9, $d9, $00, $00, $30, $30, $78, $78, $33, $33
    db $b6, $b6, $b7, $b7, $b6, $b6, $b3, $b3, $00, $00, $00, $00, $00, $00, $cd, $cd
    db $6e, $6e, $ec, $ec, $0c, $0c, $ec, $ec, $01, $01, $01, $01, $01, $01, $8f, $8f
    db $d9, $d9, $d9, $d9, $d9, $d9, $cf, $cf, $80, $80, $80, $80, $80, $80, $9e, $9e
    db $b3, $b3, $b3, $b3, $b3, $b3, $9e, $9e, $ff, $00, $ff, $00, $ff, $00, $ef, $00
    db $ff, $00, $ff, $00, $ff, $00, $ff, $00, $ff, $00, $ff, $00, $ff, $00, $e7, $00
    db $e7, $00, $ff, $00, $ff, $00, $ff, $00, $00, $ff, $ff, $ff, $00, $ff, $00, $ff
    db $ff, $00, $00, $ff, $ff, $00, $ff, $00, $00, $ff, $ff, $ff, $01, $ff, $02, $fe
    db $fe, $02, $04, $fc, $fc, $04, $fc, $04, $00, $ff, $ff, $ff, $80, $ff, $40, $7f
    db $ff, $40, $e0, $3f, $ff, $20, $bf, $60, $ff, $00, $ff, $00, $ff, $01, $fe, $02
    db $fe, $02, $fc, $04, $fc, $04, $fc, $04, $ff, $00, $ff, $00, $ff, $80, $7f, $40
    db $ff, $40, $ff, $20, $ff, $20, $bf, $60, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    db $ff, $ff, $ff, $ff, $00, $00, $00, $00, $ff, $02, $ff, $01, $ff, $01, $ff, $01
    db $ff, $01, $ff, $01, $ff, $01, $ff, $01, $7f, $c0, $ff, $80, $ff, $80, $ff, $80
    db $ff, $80, $ff, $80, $ff, $80, $ff, $80, $fe, $02, $fe, $02, $ff, $03, $fc, $05
    db $f8, $09, $f1, $12, $e1, $26, $c3, $4c, $7f, $c0, $7f, $c0, $ff, $c0, $bf, $60
    db $9f, $70, $af, $58, $27, $dc, $33, $ce, $ff, $00, $ff, $01, $fe, $02, $fc, $04
    db $f8, $09, $f0, $13, $e0, $27, $e0, $2f, $87, $98, $06, $39, $0e, $71, $1e, $e1
    db $3c, $c3, $3c, $c3, $78, $87, $78, $87, $35, $cb, $32, $cd, $3a, $c5, $79, $86
    db $78, $87, $78, $87, $7c, $83, $7c, $83, $ff, $00, $ff, $80, $7f, $c0, $3f, $e0
    db $9f, $70, $4f, $b8, $67, $9c, $37, $cc, $c0, $4f, $c0, $4f, $80, $9f, $80, $9f
    db $80, $9f, $80, $9f, $80, $9f, $80, $9f, $f8, $07, $f0, $0f, $f0, $0f, $f0, $0f
    db $f0, $0f, $f0, $0f, $f0, $0f, $f8, $07, $7c, $83, $7e, $81, $7e, $81, $3e, $c1
    db $3f, $c0, $1f, $e0, $1f, $e0, $1f, $e0, $33, $ce, $1b, $e6, $09, $f7, $0d, $f3
    db $0d, $f3, $0d, $f3, $0d, $f3, $09, $f7, $c0, $5f, $c0, $4f, $e0, $2f, $e0, $27
    db $f0, $11, $bf, $4f, $0c, $f4, $07, $ff, $78, $87, $7c, $83, $3c, $c3, $1e, $e1
    db $0f, $f0, $ff, $ff, $ff, $00, $ff, $ff, $0f, $f0, $0f, $f0, $0e, $f1, $0e, $f1
    db $06, $f9, $ff, $ff, $c5, $3f, $ff, $ff, $1b, $e6, $13, $ee, $37, $cc, $27, $dc
    db $4f, $b8, $fc, $f3, $fc, $a3, $e0, $ff, $fe, $02, $fe, $02, $bf, $43, $1c, $e5
    db $b8, $49, $b1, $52, $a1, $66, $43, $cc, $ff, $00, $ff, $00, $ff, $00, $ff, $00
    db $ff, $00, $ff, $00, $ef, $10, $c7, $38, $ff, $00, $fb, $04, $fb, $04, $fb, $04
    db $fb, $04, $f1, $0e, $f1, $0e, $f1, $0e, $83, $7c, $01, $fe, $01, $fe, $01, $fe
    db $83, $7c, $ff, $00, $83, $7c, $83, $7c, $f1, $0e, $e0, $1f, $e0, $1f, $e0, $1f
    db $e0, $1f, $e0, $1f, $80, $7f, $80, $7f, $f7, $08, $eb, $14, $f7, $08, $f7, $08
    db $e3, $1c, $e3, $1c, $63, $9c, $01, $fe, $00, $00, $60, $60, $70, $70, $78, $78
    db $78, $78, $70, $70, $60, $60, $00, $00, $00, $00, $30, $30, $70, $70, $30, $30
    db $30, $30, $30, $30, $78, $78, $00, $00, $e0, $e0, $f0, $e0, $fb, $e0, $fc, $e0
    db $fc, $e1, $fc, $e1, $fc, $e1, $fc, $e1, $00, $00, $00, $00, $ff, $00, $00, $00
    db $00, $ff, $00, $00, $00, $00, $00, $00, $07, $07, $0f, $07, $df, $07, $3f, $07
    db $3f, $87, $3f, $87, $3f, $87, $3f, $87, $fc, $e1, $fc, $e1, $fc, $e1, $fc, $e1
    db $fc, $e1, $fc, $e1, $fc, $e1, $fc, $e1, $3f, $87, $3f, $87, $3f, $87, $3f, $87
    db $3f, $87, $3f, $87, $3f, $87, $3f, $87, $fc, $e1, $fc, $e1, $fc, $e1, $fc, $e1
    db $fc, $e0, $ff, $e7, $ff, $ef, $e0, $ff, $00, $00, $00, $00, $00, $00, $00, $ff
    db $00, $00, $ff, $ff, $ff, $ff, $00, $ff, $3f, $87, $3f, $87, $3f, $87, $3f, $87
    db $3f, $07, $ff, $e7, $ff, $f7, $07, $ff, $f8, $00, $e0, $00, $c0, $00, $80, $00
    db $80, $00, $00, $00, $00, $00, $00, $00, $7f, $00, $1f, $00, $0f, $00, $07, $00
    db $07, $00, $03, $00, $03, $00, $03, $00, $00, $00, $80, $00, $80, $00, $c0, $00
    db $e0, $00, $f8, $00, $ff, $00, $ff, $00, $03, $00, $07, $00, $07, $00, $0f, $00
    db $1f, $00, $7f, $00, $ff, $00, $ff, $00, $01, $01, $01, $01, $81, $81, $c1, $c1
    db $c1, $c1, $e1, $e1, $f1, $f1, $f9, $f9, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $fe
    db $fe, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $7e, $7e, $7f, $7f, $7f, $7f, $7f, $7f
    db $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $3f, $3f, $9f, $9f, $8f, $8f
    db $cf, $cf, $e7, $e7, $f3, $f3, $f7, $f7, $e0, $e0, $e0, $e0, $e0, $e0, $e0, $e0
    db $e0, $e0, $c0, $c0, $c0, $c0, $80, $80, $f0, $f0, $f0, $f0, $f0, $f0, $f0, $f0
    db $f0, $f0, $f0, $f0, $f0, $f0, $f0, $f0, $00, $00, $7c, $7c, $47, $47, $41, $41
    db $40, $40, $40, $40, $40, $40, $7f, $40, $00, $00, $01, $01, $01, $01, $81, $81
    db $c1, $c1, $41, $41, $61, $61, $e1, $61, $00, $00, $fe, $fe, $06, $06, $06, $06
    db $06, $06, $06, $06, $06, $06, $fe, $06, $00, $00, $1b, $1b, $32, $32, $59, $59
    db $4c, $4c, $8c, $8c, $86, $86, $ff, $83, $00, $00, $ff, $ff, $01, $01, $01, $01
    db $81, $81, $41, $41, $41, $41, $3f, $21, $00, $00, $be, $be, $88, $88, $88, $88
    db $88, $88, $88, $88, $80, $80, $80, $80, $00, $00, $88, $88, $d8, $d8, $a8, $a8
    db $88, $88, $88, $88, $00, $00, $00, $00, $7f, $40, $7f, $40, $7f, $40, $7f, $40
    db $7f, $40, $7f, $40, $7f, $40, $47, $7f, $e1, $61, $e1, $61, $e1, $61, $e1, $61
    db $e1, $61, $c1, $c1, $c1, $c1, $81, $81, $fe, $06, $fe, $06, $fe, $06, $fe, $06
    db $fe, $06, $fe, $06, $fe, $06, $06, $fe, $ff, $83, $ff, $81, $7f, $40, $7f, $40
    db $7f, $40, $3f, $20, $3f, $20, $10, $1f, $1f, $11, $9f, $91, $cf, $c9, $c7, $c5
    db $e3, $63, $f3, $33, $f9, $19, $08, $f8, $80, $80, $80, $80, $80, $80, $80, $80
    db $80, $80, $80, $80, $80, $80, $80, $80, $5f, $7f, $78, $78, $60, $60, $50, $70
    db $50, $70, $48, $78, $44, $7c, $7e, $7e, $01, $01, $01, $01, $01, $01, $01, $01
    db $01, $01, $01, $01, $01, $01, $01, $01, $06, $fe, $06, $fe, $06, $fe, $06, $fe
    db $06, $fe, $06, $fe, $06, $fe, $fe, $fe, $08, $0f, $44, $47, $64, $67, $72, $73
    db $51, $71, $59, $79, $4c, $7c, $7e, $7e, $0c, $fc, $06, $fe, $03, $ff, $01, $ff
    db $01, $ff, $00, $ff, $80, $ff, $7f, $7f, $00, $00, $00, $00, $00, $00, $80, $80
    db $80, $80, $c0, $c0, $c0, $c0, $e0, $e0, $7e, $7e, $7f, $7f, $7f, $7f, $7f, $7f
    db $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $00, $00, $03, $03, $02, $02, $02, $02
    db $02, $02, $02, $02, $02, $02, $03, $02, $00, $00, $fb, $fb, $0a, $0a, $12, $12
    db $22, $22, $22, $22, $42, $42, $c3, $42, $00, $00, $fd, $fd, $0d, $0d, $0c, $0c
    db $0c, $0c, $0c, $0c, $0c, $0c, $fc, $0c, $00, $00, $fc, $fc, $0c, $0c, $8c, $8c
    db $4c, $4c, $4c, $4c, $2c, $2c, $3c, $2c, $03, $02, $03, $02, $03, $03, $03, $03
    db $02, $02, $00, $00, $00, $00, $00, $00, $83, $82, $83, $82, $03, $02, $03, $02
    db $03, $02, $03, $02, $03, $02, $02, $03, $fc, $0c, $fc, $0c, $fc, $0c, $fc, $0c
    db $fc, $0c, $fc, $0c, $fc, $0c, $0c, $fc, $1c, $1c, $1c, $1c, $0c, $0c, $0c, $0c
    db $04, $04, $00, $00, $00, $00, $00, $00, $02, $03, $02, $03, $02, $03, $02, $03
    db $02, $03, $02, $03, $02, $03, $03, $03, $0c, $fc, $0c, $fc, $0c, $fc, $0c, $fc
    db $0c, $fc, $0c, $fc, $0c, $fc, $fc, $fc, $03, $03, $03, $03, $03, $03, $03, $03
    db $03, $03, $03, $03, $03, $03, $03, $03, $fc, $fc, $fc, $fc, $fc, $fc, $fc, $fc
    db $fc, $fc, $fc, $fc, $fc, $fc, $fc, $fc, $ff, $00, $ff, $00, $ff, $00, $ff, $00
    db $ff, $00, $ff, $00, $ff, $00, $ff, $00, $00, $ff, $00, $ff, $00, $ff, $00, $ff
    db $00, $ff, $00, $ff, $00, $ff, $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    db $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00, $00, $01, $01, $03, $03, $07, $07
    db $0f, $0f, $1f, $1f, $3f, $3f, $7f, $7f, $00, $00, $ff, $ff, $83, $83, $83, $83
    db $83, $83, $83, $83, $83, $83, $ff, $83, $00, $00, $7f, $7f, $20, $20, $10, $10
    db $08, $08, $04, $04, $02, $02, $01, $01, $00, $00, $f3, $f3, $32, $32, $32, $32
    db $32, $32, $32, $32, $32, $32, $f3, $32, $ff, $83, $ff, $83, $ff, $83, $ff, $83
    db $ff, $83, $ff, $83, $ff, $83, $83, $ff, $00, $00, $00, $00, $01, $01, $03, $03
    db $07, $07, $0f, $0b, $1f, $13, $23, $3f, $f3, $b2, $73, $72, $33, $33, $13, $13
    db $02, $02, $00, $00, $00, $00, $00, $00, $83, $ff, $83, $ff, $83, $ff, $83, $ff
    db $83, $ff, $83, $ff, $83, $ff, $ff, $ff, $43, $7f, $23, $3f, $13, $1f, $0b, $0f
    db $07, $07, $03, $03, $01, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    db $00, $00, $10, $10, $30, $30, $70, $70, $00, $00, $78, $78, $9c, $9c, $1c, $1c
    db $78, $78, $e0, $e0, $fc, $fc, $00, $00, $ff, $00, $00, $00, $00, $00, $00, $00
    db $00, $00, $00, $00, $00, $00, $00, $00, $1b, $1b, $1b, $1b, $09, $09, $00, $00
    db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    db $60, $60, $60, $60, $20, $20, $00, $00, $1b, $1b, $1b, $1b, $09, $09, $00, $00
    db $00, $00, $60, $60, $60, $60, $00, $00

LicenseTilemap:
    ; This tilemap has been stringified since it's entirely text. See `charmap.inc` for how these text characters map to tile indexes

    db "                    "       ;db $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f
    db "\"TM AND ©1987 ELORG,"      ;db $9b, $1d, $16, $2f, $0a, $17, $0d, $2f, $33, $01, $09, $08, $07, $2f, $0e, $15, $18, $1b, $10, $9c
    db " TETRIS LICENSED TO "       ;db $2f, $1d, $0e, $1d, $1b, $12, $1c, $2f, $15, $12, $0c, $0e, $17, $1c, $0e, $0d, $2f, $1d, $18, $2f
    db "    BULLET PROOF    "       ;db $2f, $2f, $2f, $2f, $0b, $1e, $15, $15, $0e, $1d, $2f, $19, $1b, $18, $18, $0f, $2f, $2f, $2f, $2f
    db "    SOFTWARE AND    "       ;db $2f, $2f, $2f, $2f, $1c, $18, $0f, $1d, $20, $0a, $1b, $0e, $2f, $0a, $17, $0d, $2f, $2f, $2f, $2f
    db "   SUB-LICENSED TO  "       ;db $2f, $2f, $2f, $1c, $1e, $0b, $25, $15, $12, $0c, $0e, $17, $1c, $0e, $0d, $2f, $1d, $18, $2f, $2f
    db "      NINTENDO.     "       ;db $2f, $2f, $2f, $2f, $2f, $2f, $17, $12, $17, $1d, $0e, $17, $0d, $18, $24, $2f, $2f, $2f, $2f, $2f
    db "                    "       ;db $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f
    db " ©1989 BULLET PROOF "       ;db $2f, $33, $01, $09, $08, $09, $2f, $0b, $1e, $15, $15, $0e, $1d, $2f, $19, $1b, $18, $18, $0f, $2f
    db "      SOFTWARE.     "       ;db $2f, $2f, $2f, $2f, $2f, $2f, $1c, $18, $0f, $1d, $20, $0a, $1b, $0e, $24, $2f, $2f, $2f, $2f, $2f
    PUSHC bottom_aligned
    db "   ©1989 NINTDO     "       ;db $2f, $2f, $2f, $33, $30, $31, $32, $31, $2f, $34, $35, $36, $37, $38, $39, $2f, $2f, $2f, $2f, $2f
    POPC
    db "                    "       ;db $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f
    db "ALL RIGHTS RESERVED."       ;db $0a, $15, $15, $2f, $1b, $12, $10, $11, $1d, $1c, $2f, $1b, $0e, $1c, $0e, $1b, $1f, $0e, $0d, $24
    db "                    "       ;db $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f
    db "  ORIGINAL CONCEPT, "       ;db $2f, $2f, $18, $1b, $12, $10, $12, $17, $0a, $15, $2f, $0c, $18, $17, $0c, $0e, $19, $1d, $9c, $2f
    db " DESIGN AND PROGRAM "       ;db $2f, $0d, $0e, $1c, $12, $10, $17, $2f, $0a, $17, $0d, $2f, $19, $1b, $18, $10, $1b, $0a, $16, $2f
    db "BY ALEXEY PAZHITNOV.\""     ;db $0b, $22, $2f, $0a, $15, $0e, $21, $0e, $22, $2f, $19, $0a, $23, $11, $12, $1d, $17, $18, $1f, $9d
    db "                    "       ;db $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f

TitleScreenTilemap:
    db $8e, $8e, $8e, $8e, $8e, $8e, $8e, $8e, $8e, $8e, $8e, $8e, $8e, $8e, $8e, $8e, $8e, $8e, $8e, $8e
    db $5a, $5b, $5b, $5b, $5b, $5b, $5b, $5b, $5b, $5b, $5b, $5b, $5b, $5b, $5b, $5b, $5b, $5b, $5b, $5c
    db $5d, $80, $81, $82, $83, $90, $91, $92, $81, $82, $83, $90, $6c, $6d, $6e, $6f, $70, $71, $72, $5e
    db $5d, $84, $85, $86, $87, $93, $94, $95, $85, $86, $87, $93, $73, $74, $75, $76, $77, $78, $2f, $5e
    db $5d, $2f, $88, $89, $2f, $96, $97, $98, $88, $89, $2f, $96, $79, $7a, $7b, $7c, $7d, $7e, $2f, $5e
    db $5d, $2f, $8a, $8b, $2f, $8e, $8f, $6b, $8a, $8b, $2f, $8e, $7f, $66, $67, $68, $69, $6a, $2f, $5e
    db $5f, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $61
    db $8e, $3c, $3c, $3c, $3c, $3c, $3c, $3c, $3c, $3c, $3c, $3c, $3c, $3c, $3d, $3e, $3c, $3c, $3c, $8e
    db $8e, $8c, $8c, $62, $63, $8c, $8c, $3a, $8c, $8c, $8c, $8c, $8c, $3a, $42, $43, $3b, $8c, $8c, $8e
    db $8e, $3a, $8c, $64, $65, $8c, $8c, $8c, $8c, $3b, $8c, $8c, $8c, $8c, $44, $45, $8c, $8c, $8c, $8e
    db $8e, $8c, $8c, $8c, $8c, $8c, $8c, $8c, $8c, $8c, $8c, $8c, $8c, $46, $47, $48, $49, $3f, $40, $8e
    db $8e, $8c, $8c, $8c, $8c, $3a, $8c, $8c, $8c, $8c, $53, $54, $8c, $4a, $4b, $4c, $4d, $42, $43, $8e
    db $8e, $8c, $8c, $8c, $8c, $8c, $8c, $8c, $8c, $54, $55, $56, $57, $4e, $4f, $50, $51, $52, $45, $8e
    db $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41
    db $2f, $2f, $59, $19, $15, $0a, $22, $0e, $1b, $2f, $2f, $2f, $99, $19, $15, $0a, $22, $0e, $1b, $2f
    db $2f, $2f, $9a, $9a, $9a, $9a, $9a, $9a, $9a, $2f, $2f, $2f, $9a, $9a, $9a, $9a, $9a, $9a, $9a, $2f
    db $2f, $2f, $2f, $2f, $33, $30, $31, $32, $31, $2f, $34, $35, $36, $37, $38, $39, $2f, $2f, $2f, $2f
    db $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f

GameOptionsTilemap:
    db $47, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $49
    db $4a, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $4b
    db $4a, $2c, $2c, $2c, $50, $51, $51, $51, $51, $51, $51, $51, $51, $51, $52, $2c, $2c, $2c, $2c, $4b
    db $4a, $2c, $2c, $2c, $53, $10, $0a, $16, $0e, $2f, $1d, $22, $19, $0e, $54, $2c, $2c, $2c, $2c, $4b
    db $4a, $2c, $55, $56, $6d, $58, $58, $58, $58, $58, $a9, $58, $58, $58, $6e, $56, $56, $5a, $2c, $4b
    db $4a, $2c, $5b, $78, $77, $7e, $7f, $9a, $9b, $2f, $aa, $79, $77, $7e, $7f, $9a, $9b, $5c, $2c, $4b
    db $4a, $2c, $2d, $4f, $4f, $4f, $4f, $4f, $4f, $4f, $ac, $4f, $4f, $4f, $4f, $4f, $4f, $2e, $2c, $4b
    db $4a, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $4b
    db $4a, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $4b
    db $4a, $2c, $2c, $2c, $50, $51, $51, $51, $51, $51, $51, $51, $51, $51, $51, $52, $2c, $2c, $2c, $4b
    db $4a, $2c, $2c, $2c, $53, $16, $1e, $1c, $12, $0c, $2f, $1d, $22, $19, $0e, $54, $2c, $2c, $2c, $4b
    db $4a, $2c, $55, $56, $6d, $58, $58, $58, $58, $58, $a9, $58, $58, $58, $58, $6e, $56, $5a, $2c, $4b
    db $4a, $2c, $5b, $78, $77, $7e, $7f, $9a, $9b, $2f, $aa, $79, $77, $7e, $7f, $9a, $9b, $5c, $2c, $4b
    db $4a, $2c, $71, $72, $72, $72, $72, $72, $72, $72, $ab, $72, $72, $72, $72, $72, $72, $74, $2c, $4b
    db $4a, $2c, $5b, $7a, $77, $7e, $7f, $9a, $9b, $2f, $aa, $2f, $9d, $9c, $9c, $2f, $2f, $5c, $2c, $4b
    db $4a, $2c, $2d, $4f, $4f, $4f, $4f, $4f, $4f, $4f, $ac, $4f, $4f, $4f, $4f, $4f, $4f, $2e, $2c, $4b
    db $4a, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $4b
    db $4c, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4e

LevelSelectorTilemap:
    db $47, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $49
    db $4a, $2f, $0a, $25, $1d, $22, $19, $0e, $2f, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $4b
    db $4a, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $4b
    db $4a, $2c, $2c, $2c, $2c, $2c, $50, $51, $51, $51, $51, $51, $52, $2c, $2c, $2c, $2c, $2c, $2c, $4b
    db $4a, $2c, $2c, $2c, $2c, $2c, $53, $15, $0e, $1f, $0e, $15, $54, $2c, $2c, $2c, $2c, $2c, $2c, $4b
    db $4a, $2c, $2c, $2c, $55, $56, $57, $58, $6c, $58, $6c, $58, $59, $56, $5a, $2c, $2c, $2c, $2c, $4b
    db $4a, $2c, $2c, $2c, $5b, $90, $6f, $91, $6f, $92, $6f, $93, $6f, $94, $5c, $2c, $2c, $2c, $2c, $4b
    db $4a, $2c, $2c, $2c, $71, $72, $73, $72, $73, $72, $73, $72, $73, $72, $74, $2c, $2c, $2c, $2c, $4b
    db $4a, $2c, $2c, $2c, $5b, $95, $6f, $96, $6f, $97, $6f, $98, $6f, $99, $5c, $2c, $2c, $2c, $2c, $4b
    db $4a, $2c, $2c, $2c, $2d, $4f, $6b, $4f, $6b, $4f, $6b, $4f, $6b, $4f, $2e, $2c, $2c, $2c, $2c, $4b
    db $4a, $2c, $2c, $2c, $50, $51, $51, $51, $51, $51, $51, $51, $51, $51, $52, $2c, $2c, $2c, $2c, $4b
    db $4a, $2c, $2c, $2c, $53, $1d, $18, $19, $25, $1c, $0c, $18, $1b, $0e, $54, $2c, $2c, $2c, $2c, $4b
    db $4a, $55, $56, $70, $6d, $58, $58, $58, $58, $58, $58, $58, $58, $58, $6e, $56, $56, $56, $5a, $4b
    db $4a, $5b, $01, $6f, $60, $60, $60, $60, $60, $60, $2f, $2f, $60, $60, $60, $60, $60, $60, $5c, $4b
    db $4a, $5b, $02, $6f, $60, $60, $60, $60, $60, $60, $2f, $2f, $60, $60, $60, $60, $60, $60, $5c, $4b
    db $4a, $5b, $03, $6f, $60, $60, $60, $60, $60, $60, $2f, $2f, $60, $60, $60, $60, $60, $60, $5c, $4b
    db $4a, $2d, $4f, $6b, $4f, $4f, $4f, $4f, $4f, $4f, $4f, $4f, $4f, $4f, $4f, $4f, $4f, $4f, $2e, $4b
    db $4c, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4e

