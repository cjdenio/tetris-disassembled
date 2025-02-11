; Disassembly of "Tetris (World).gb"
; This file was created with:
; mgbdis v2.0 - Game Boy ROM disassembler by Matt Currie and contributors.
; https://github.com/mattcurrie/mgbdis

INCLUDE "hardware.inc"

MACRO set_state_a
    ld a, (\1 - RunStateMachine.end) / 2
ENDM

MACRO set_state
    set_state_a \1
    ldh [$ffe1], a
ENDM

SECTION "ROM", ROM0

RST_00::
    db $c3, $8b

    ld [bc], a
    nop
    nop
    nop
    nop
    nop

RST_08::
    jp Start


    rst $38
    rst $38
    rst $38
    rst $38
    rst $38

RST_10::
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38

RST_18::
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38

RST_20::
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38

RST_28::
    add a
    pop hl
    ld e, a
    ld d, $00
    add hl, de
    ld e, [hl]
    inc hl

RST_30::
    ld d, [hl]
    push de
    pop hl
    jp hl


    rst $38
    rst $38
    rst $38
    rst $38

RST_38::
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38

VBlankInterrupt::
    jp Jump_000_01fd

    ds 5

LCDCInterrupt::
    jp Jump_000_2712

    ds 5

TimerOverflowInterrupt::
    jp Jump_000_2712

    ds 5

SerialTransferCompleteInterrupt::
    jp Jump_000_017e

    ds 5

JoypadTransitionInterrupt::
    ds 160

Boot::
    nop
    jp Entrypoint


HeaderLogo::
    db $ce, $ed, $66, $66, $cc, $0d, $00, $0b, $03, $73, $00, $83, $00, $0c, $00, $0d
    db $00, $08, $11, $1f, $88, $89, $00, $0e, $dc, $cc, $6e, $e6, $dd, $dd, $d9, $99
    db $bb, $bb, $67, $63, $6e, $0e, $ec, $cc, $dd, $dc, $99, $9f, $bb, $b9, $33, $3e

HeaderTitle::
    db "TETRIS", $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

HeaderNewLicenseeCode::
    db $00, $00

HeaderSGBFlag::
    db $00

HeaderCartridgeType::
    db CART_ROM

HeaderROMSize::
    db $00

HeaderRAMSize::
    db $00

HeaderDestinationCode::
    db $00

HeaderOldLicenseeCode::
    db $01

HeaderMaskROMVersion::
    db $00

HeaderComplementCheck::
    db $0b

HeaderGlobalChecksum::
    db $00, $00 ; Checksum set to 0 so `rgbfix` will compute it

Entrypoint:
    jp Start


    call Call_000_2a2b

jr_000_0156:
    ldh a, [rSTAT]
    and $03
    jr nz, jr_000_0156

    ld b, [hl]

jr_000_015d:
    ldh a, [rSTAT]
    and $03
    jr nz, jr_000_015d

    ld a, [hl]
    and b
    ret


Call_000_0166:
    ld a, e
    add [hl]
    daa
    ld [hl+], a
    ld a, d
    adc [hl]
    daa
    ld [hl+], a
    ld a, $00
    adc [hl]
    daa
    ld [hl], a
    ld a, $01
    ldh [$ffe0], a
    ret nc

    ld a, $99
    ld [hl-], a
    ld [hl-], a
    ld [hl], a
    ret


Jump_000_017e:
    push af
    push hl
    push de
    push bc
    call Call_000_018e
    ld a, $01
    ldh [$ffcc], a
    pop bc
    pop de
    pop hl
    pop af
    reti


Call_000_018e:
    ldh a, [$ffcd]
    rst $28
    sbc e
    ld bc, $01c2
    rst $00
    ld bc, $01dd
    ld a, $28
    ldh a, [$ffe1]
    cp $07
    jr z, jr_000_01a9

    cp $06
    ret z

    ld a, $06
    ldh [$ffe1], a
    ret


jr_000_01a9:
    ldh a, [rSB]
    cp $55
    jr nz, jr_000_01b7

    ld a, $29
    ldh [$ffcb], a
    ld a, $01
    jr jr_000_01bf

jr_000_01b7:
    cp $29
    ret nz

    ld a, $55
    ldh [$ffcb], a
    xor a

jr_000_01bf:
    ldh [rSC], a
    ret


    ldh a, [rSB]
    ldh [$ffd0], a
    ret


    ldh a, [rSB]
    ldh [$ffd0], a
    ldh a, [$ffcb]
    cp $29
    ret z

    ldh a, [$ffcf]
    ldh [rSB], a
    ld a, $ff
    ldh [$ffcf], a
    ld a, $80
    ldh [rSC], a
    ret


    ldh a, [rSB]
    ldh [$ffd0], a
    ldh a, [$ffcb]
    cp $29
    ret z

    ldh a, [$ffcf]
    ldh [rSB], a
    ei
    call SleepForABit
    ld a, $80
    ldh [rSC], a
    ret


    ldh a, [$ffcd]
    cp $02
    ret nz

    xor a
    ldh [rIF], a
    ei
    ret


Jump_000_01fd:
    push af
    push bc
    push de
    push hl
    ldh a, [$ffce]
    and a
    jr z, jr_000_0218

    ldh a, [$ffcb]
    cp $29
    jr nz, jr_000_0218

    xor a
    ldh [$ffce], a
    ldh a, [$ffcf]
    ldh [rSB], a
    ld hl, $ff02
    ld [hl], $81

jr_000_0218:
    call Call_000_2240
    call Call_000_242c
    call Call_000_2417
    call Call_000_23fe
    call Call_000_23ec
    call Call_000_23dd
    call Call_000_23ce
    call Call_000_23bf
    call Call_000_23b0
    call Call_000_23a1
    call Call_000_2392
    call Call_000_2383
    call Call_000_2358
    call Call_000_2349
    call Call_000_233a
    call Call_000_232b
    call Call_000_231c
    call Call_000_230d
    call Call_000_22fe
    call Call_000_1f32
    call $ffb6
    call Call_000_192e
    ld a, [$c0ce]
    and a
    jr z, jr_000_027a

    ldh a, [$ff98]
    cp $03
    jr nz, jr_000_027a

    ld hl, $986d
    call Call_000_249b
    ld a, $01
    ldh [$ffe0], a
    ld hl, $9c6d
    call Call_000_249b
    xor a
    ld [$c0ce], a

jr_000_027a:
    ld hl, $ffe2
    inc [hl]
    xor a
    ldh [rSCX], a
    ldh [rSCY], a
    inc a
    ldh [$ff85], a
    pop hl
    pop de
    pop bc
    pop af
    reti


; CLEAR WRAM
; vvvvvvvvvv
Start:
    xor a
    ld hl, $dfff ; last byte of WRAM
    ld c, $10
    ld b, $00

jr_000_0293:
    ld [hl-], a
    dec b
    jr nz, jr_000_0293

    dec c
    jr nz, jr_000_0293
; ^^^^^^^^^^

Jump_000_029a:
    ld a, IEF_VBLANK
    di
    ldh [rIF], a
    ldh [rIE], a ; enable VBLANK interrupt

    xor a
    ldh [rSCY], a
    ldh [rSCX], a
    ldh [$ffa4], a ; ???
    ldh [rSTAT], a
    ldh [rSB], a
    ldh [rSC], a

    ld a, LCDCF_ON ; enable LCD
    ldh [rLCDC], a

jr_000_02b2:
    ldh a, [rLY] ; wait for VBlank
    cp $94
    jr nz, jr_000_02b2

    ld a, %00000011 ; OBJ and BG on, LCD off
    ldh [rLCDC], a

    ; PALETTES
    ; vvvvvvvvvv
    ld a, $e4
    ldh [rBGP], a
    ldh [rOBP0], a

    ld a, $c4
    ldh [rOBP1], a
    ; ^^^^^^^^^^

    ; AUDIO
    ; vvvvvvvvvv
    ld hl, $ff26
    ld a, %10000000 ; Audio on, channels off
    ld [hl-], a

    ld a, $ff ; Pan to both channels
    ld [hl-], a
    ld [hl], $77 ; Volume to max
    ; ^^^^^^^^^^

    ; Select ROM bank 1; probably unnecessary
    ld a, $01
    ld [rROMB0], a
    ; ^^^^^^^^^^

    ; Set up stack pointer
    ld sp, $cfff
    ; ^^^^^^^^^^

; CLEAR MOAR RAM
; vvvvvvvvvv
    xor a
    ld hl, $dfff
    ld b, $00

jr_000_02df:
    ld [hl-], a
    dec b
    jr nz, jr_000_02df

    ld hl, $cfff
    ld c, $10
    ld b, $00

jr_000_02ea:
    ld [hl-], a
    dec b
    jr nz, jr_000_02ea

    dec c
    jr nz, jr_000_02ea
; ^^^^^^^^^^

; CLEAR VRAM
; vvvvvvvvvv
    ld hl, $9fff
    ld c, $20
    xor a
    ld b, $00

jr_000_02f9:
    ld [hl-], a
    dec b
    jr nz, jr_000_02f9

    dec c
    jr nz, jr_000_02f9
; ^^^^^^^^^^

; CLEAR OAM
; vvvvvvvvvv
    ld hl, $feff

Jump_000_0303:
    ld b, $00

jr_000_0305:
    ld [hl-], a
    dec b
    jr nz, jr_000_0305
; ^^^^^^^^^^

; CLEAR HRAM
; vvvvvvvvvv
    ld hl, $fffe
    ld b, $80

jr_000_030e:
    ld [hl-], a
    dec b
    jr nz, jr_000_030e
; ^^^^^^^^^^

; Copy DMA procedure into HRAM
; vvvvvvvvvv
    ld c, $b6
    ld b, $0c ; For some reason, this copies 12 bytes (to $FFB6-$FFC1) even though the DMA procedure is only 10 bytes long. The last 2 bytes ($FFC0-$FFC1) are overwritten below
    ld hl, DMAProcedure

jr_000_0319:
    ld a, [hl+]
    ldh [c], a
    inc c
    dec b
    jr nz, jr_000_0319
; ^^^^^^^^^^

    call ClearTilemap ; Fill both tilemaps with blank tiles ($2f)

    call Call_001_7ff3 ; audio stuff

    ld a, %00001001 ; Enable serial + VBlank interrupts
    ldh [rIE], a

    ld a, $37
    ldh [$ffc0], a ; Set default game type
    ld a, $1c
    ldh [$ffc1], a ; Set default music type

    set_state LicenseScreen

    ld a, LCDCF_ON ; Enable LCD
    ldh [rLCDC], a

    ei ; Enable interrupts

    ; Zero out some registers
    xor a
    ldh [rIF], a
    ldh [rWY], a
    ldh [rWX], a
    ldh [rTMA], a

Main:
    call ReadGamepad
    call RunStateMachine
    call Call_001_7ff0 ; probably audio code

    ; Reset game if all 4 buttons pressed (A, B, Start, Select)
    ldh a, [$ff80]
    and %00001111
    cp  %00001111
    jp z, Jump_000_029a
    ; ^^^^^^^^^^

    ; Decrement $FFA6 and $FFA7
    ld hl, $ffa6
    ld b, $02

jr_000_035a:
    ld a, [hl]
    and a
    jr z, jr_000_035f

    dec [hl]

jr_000_035f:
    inc l
    dec b
    jr nz, jr_000_035a
    ; ^^^^^^^^^^

    ; if $FFC5 is set, enable interrupts?
    ldh a, [$ffc5]
    and a
    jr z, jr_000_036c

    ld a, $09
    ldh [rIE], a
    ; ^^^^^^^^^^

jr_000_036c:
    ; Wait for VBlank handler to finish before continuing
    ldh a, [$ff85]
    and a
    jr z, jr_000_036c

    xor a
    ldh [$ff85], a
    ; ^^^^^^^^^^

    jp Main


RunStateMachine:
    ldh a, [$ffe1]
    rst $28
.end

; ----------------------------------------------------

    db $29, $1c, $3d, $1d

    xor b
    ld [de], a
    rst $18
    ld [de], a

    db $61, $1d

    add c
    dec e

    InitTitleScreen:   dw RunInitTitleScreen
    TitleScreen:       dw RunTitleScreen
    InitGameOptionsScreen: dw RunInitGameOptionsScreen

    ldh a, [rNR14]

    db $6b, $1a

    dec de
    ld e, $71
    rra

    db $7a, $1f

    MysteryState2: dw RunMysteryState2

    inc d
    dec d

    db $df, $15, $23, $16

    adc l
    ld d, $de
    ld d, $4f
    rla

    db $77, $19

    db $e4
    ld b, $99
    rlca
    sub d
    ld [$0953], sp
    sub l
    dec bc
    ld c, a
    dec c
    ld b, c
    dec bc
    sub [hl]
    dec c
    add a
    ld c, $76
    ld de, $0dfd
    xor $0e
    add hl, hl
    ld e, $9c
    db $1e

    LicenseScreen:         dw RunLicenseScreen
    WaitForLicenseCounter: dw RunWaitForLicenseCounter

    rl c
    ld c, d
    ld [de], a
    ld h, b
    ld [de], a
    add b
    ld [de], a
    inc [hl]
    ld b, $64
    ld b, $17
    inc de
    ld l, c
    inc de
    adc b
    inc de
    or l
    inc de
    rl e
    ldh [c], a
    inc de
    add hl, de
    inc d
    ld c, c
    inc d
    ld a, a
    inc de
    ld a, $28

RunLicenseScreen:
    call DisableLCD ; Turn off LCD
    call LoadTitleTiles ; Copy over tile data

    ld de, LicenseTilemap
    call LoadTilemap ; Load license tilemap

    call ClearOAM

    ; Copy over some very specific bytes from ROM; purpose unclear
    ld hl, $c300
    ld de, $64d0

jr_000_03fb:
    ld a, [de]
    ld [hl+], a
    inc de
    ld a, h
    cp $c4
    jr nz, jr_000_03fb
    ; ^^^^^^^^^^

    ld a, %11010011
    ldh [rLCDC], a ; Enable LCD

    ld a, 125 ; Display the license screen for 125 frames
    ldh [$ffa6], a

    set_state WaitForLicenseCounter

    ret


RunWaitForLicenseCounter:
    ; Wait for $ffa6 to reach 0
    ldh a, [$ffa6]
    and a
    ret nz

    set_state InitTitleScreen
    ret


RunInitTitleScreen:
    call DisableLCD

    xor a
    ldh [$ffe9], a
    ldh [$ff98], a
    ldh [$ff9c], a
    ldh [$ff9b], a
    ldh [$fffb], a
    ldh [$ff9f], a
    ldh [$ffe3], a
    ldh [$ffe7], a
    ldh [$ffc7], a
    call Call_000_22f3 ; clear some memory
    call Call_000_26a5 ; clear some more memory
    call LoadTitleTiles
    ld hl, $c800

jr_000_043b:
    ld a, $2f
    ld [hl+], a
    ld a, h
    cp $cc
    jr nz, jr_000_043b

    ld hl, $c801
    call Call_000_26fd
    ld hl, $c80c
    call Call_000_26fd
    ld hl, $ca41
    ld b, $0c
    ld a, $8e

jr_000_0456:
    ld [hl+], a
    dec b
    jr nz, jr_000_0456

    ld de, TitleScreenTilemap
    call LoadTilemap

    call ClearOAM

    ; Draw the 1-/2-player selector to the object layer on screen (OAM)
    ld hl, $c000
    ld [hl], $80 ; y-position
    inc l
    ld [hl], $10 ; x-position
    inc l
    ld [hl], $58 ; tile number

    ld a, $03
    ld [$dfe8], a

    ld a, %11010011
    ldh [rLCDC], a ; Enable LCD

    set_state TitleScreen

    ; Set up the counter which keeps track of how long to display the title screen for
    ld a, $7d
    ldh [$ffa6], a
    ld a, $04 ; Set to 4 (wait $047d = 1149 frames) if the demo has already been shown
    ldh [$ffc6], a

    ldh a, [$ffe4] ; Check if the demo has been shown yet
    and a
    ret nz

    ld a, $13 ; Set to 13 (wait $137d = 4989 frames) if the demo has NOT yet been shown
    ldh [$ffc6], a
    ret


StartDemo:
    ld a, $37
    ldh [$ffc0], a
    ld a, $09
    ldh [$ffc2], a
    xor a
    ldh [$ffc5], a
    ldh [$ffb0], a
    ldh [$ffed], a
    ldh [$ffea], a
    ld a, $63
    ldh [$ffeb], a
    ld a, $30
    ldh [$ffec], a
    ldh a, [$ffe4]
    cp $02
    ld a, $02
    jr nz, jr_000_04c7

    ld a, $77
    ldh [$ffc0], a
    ld a, $09
    ldh [$ffc3], a
    ld a, $02
    ldh [$ffc4], a
    ld a, $64
    ldh [$ffeb], a
    ld a, $30
    ldh [$ffec], a
    ld a, $11
    ldh [$ffb0], a
    ld a, $01

jr_000_04c7:
    ldh [$ffe4], a
    ld a, $0a
    ldh [$ffe1], a
    call DisableLCD
    call LoadGameTiles
    ld de, GameOptionsTilemap
    call LoadTilemap
    call ClearOAM
    ld a, $d3
    ldh [rLCDC], a
    ret


    ld a, $ff
    ldh [$ffe9], a
    ret


RunTitleScreen:
    ; Start the demo once the $a6 and $c6 counters hit 0
    ; vvvvvvvvvv
    ldh a, [$ffa6]
    and a
    jr nz, jr_000_04f5

    ld hl, $ffc6
    dec [hl]
    jr z, StartDemo

    ld a, $7d
    ldh [$ffa6], a
    ; ^^^^^^^^^^

jr_000_04f5:
    call SleepForABit ; Sleep for a few cycles?

    ; Something multiplayer-related
    ld a, $55
    ldh [rSB], a
    ld a, $80
    ldh [rSC], a
    ldh a, [$ffcc]
    and a
    jr z, jr_000_050f

    ldh a, [$ffcb]
    and a
    jr nz, jr_000_0544

    xor a
    ldh [$ffcc], a
    jr jr_000_0576

jr_000_050f:
    ldh a, [$ff81] ; Grab the gamepad state from $FF81
    ld b, a

    ldh a, [$ffc5] ; $ffc5 = state of the 1-/2-player selector

    bit PADB_SELECT, b ; Select button
    jr nz, ToggleTitleScreenCursor

    bit PADB_RIGHT, b ; Right d-pad button
    jr nz, SelectTwoPlayer

    bit PADB_LEFT, b ; Left d-pad button
    jr nz, SelectOnePlayer

    bit PADB_START, b ; Start button
    ret z

    ; WHEN START BUTTON PRESSED
    ; vvvvvvvvvvvvvvvvvvvvvvv
    and a ; Set CPU flags for A (specifically, the zero flag used below)
    set_state_a InitGameOptionsScreen
    jr z, StartSinglePlayer ; <- jump to single-player if A register is 0

    ; multi-player |
    ;              v
    ld a, b
    cp $08
    ret nz

    ldh a, [$ffcb]
    cp $29
    jr z, jr_000_0544

    ld a, $29
    ldh [rSB], a
    ld a, $81
    ldh [rSC], a

jr_000_053a:
    ldh a, [$ffcc]
    and a
    jr z, jr_000_053a

    ldh a, [$ffcb]
    and a
    jr z, jr_000_0576

jr_000_0544:
    ld a, $2a

jr_000_0546:
    ldh [$ffe1], a
    xor a
    ldh [$ffa6], a
    ldh [$ffc2], a
    ldh [$ffc3], a
    ldh [$ffc4], a
    ldh [$ffe4], a
    ret


StartSinglePlayer:
    push af
    ldh a, [$ff80]
    bit PADB_DOWN, a ; If the down button is held while pressing start, enable expert mode
    jr z, jr_000_055d

    ldh [$fff4], a

jr_000_055d:
    pop af
    jr jr_000_0546

ToggleTitleScreenCursor:
    xor $01

SetTitleScreenCursor:
    ldh [$ffc5], a
    and a
    ld a, $10
    jr z, jr_000_056b

    ld a, $60

jr_000_056b:
    ld [$c001], a
    ret


SelectTwoPlayer:
    and a
    ret nz

    xor a
    jr ToggleTitleScreenCursor

SelectOnePlayer:
    and a
    ret z

jr_000_0576:
    xor a
    jr SetTitleScreenCursor

Call_000_0579:
    ldh a, [$ffe4]
    and a
    ret z

    call SleepForABit
    xor a
    ldh [rSB], a
    ld a, $80
    ldh [rSC], a
    ldh a, [$ff81]
    bit 3, a
    jr z, jr_000_059a

    ld a, $33
    ldh [rSB], a
    ld a, $81
    ldh [rSC], a
    ld a, $06
    ldh [$ffe1], a
    ret


jr_000_059a:
    ld hl, $ffb0
    ldh a, [$ffe4]
    cp $02
    ld b, $10
    jr z, jr_000_05a7

    ld b, $1d

jr_000_05a7:
    ld a, [hl]
    cp b
    ret nz

    ld a, $06
    ldh [$ffe1], a
    ret


Call_000_05af:
    ldh a, [$ffe4]
    and a
    ret z

    ldh a, [$ffe9]
    cp $ff
    ret z

    ldh a, [$ffea]
    and a
    jr z, jr_000_05c2

    dec a
    ldh [$ffea], a
    jr jr_000_05de

jr_000_05c2:
    ldh a, [$ffeb]
    ld h, a
    ldh a, [$ffec]
    ld l, a
    ld a, [hl+]
    ld b, a
    ldh a, [$ffed]
    xor b
    and b
    ldh [$ff81], a
    ld a, b
    ldh [$ffed], a
    ld a, [hl+]
    ldh [$ffea], a
    ld a, h
    ldh [$ffeb], a
    ld a, l
    ldh [$ffec], a
    jr jr_000_05e1

jr_000_05de:
    xor a
    ldh [$ff81], a

jr_000_05e1:
    ldh a, [$ff80]
    ldh [$ffee], a
    ldh a, [$ffed]
    ldh [$ff80], a
    ret


    xor a
    ldh [$ffed], a
    jr jr_000_05de

    ret


Call_000_05f0:
    ldh a, [$ffe4]
    and a
    ret z

    ldh a, [$ffe9]
    cp $ff
    ret nz

    ldh a, [$ff80]
    ld b, a
    ldh a, [$ffed]
    cp b
    jr z, jr_000_061a

    ldh a, [$ffeb]
    ld h, a
    ldh a, [$ffec]
    ld l, a
    ldh a, [$ffed]
    ld [hl+], a
    ldh a, [$ffea]
    ld [hl+], a
    ld a, h
    ldh [$ffeb], a
    ld a, l
    ldh [$ffec], a
    ld a, b
    ldh [$ffed], a
    xor a
    ldh [$ffea], a
    ret


jr_000_061a:
    ldh a, [$ffea]
    inc a
    ldh [$ffea], a
    ret


Call_000_0620:
    ldh a, [$ffe4]
    and a
    ret z

    ldh a, [$ffe9]
    and a
    ret nz

    ldh a, [$ffee]
    ldh [$ff80], a
    ret


jr_000_062d:
    ld hl, $ff02
    set 7, [hl]
    jr jr_000_063e

    ld a, $03
    ldh [$ffcd], a
    ldh a, [$ffcb]
    cp $29
    jr nz, jr_000_062d

jr_000_063e:
    call Call_000_14b3
    ld a, $80
    ld [$c210], a
    call Call_000_26c5
    ldh [$ffce], a
    xor a
    ldh [rSB], a
    ldh [$ffcf], a
    ldh [$ffdc], a
    ldh [$ffd2], a
    ldh [$ffd3], a
    ldh [$ffd4], a
    ldh [$ffd5], a
    ldh [$ffe3], a
    call $7ff3
    ld a, $2b
    ldh [$ffe1], a
    ret


    ldh a, [$ffcb]
    cp $29
    jr z, jr_000_0680

    ldh a, [$fff0]
    and a
    jr z, jr_000_068d

    xor a
    ldh [$fff0], a
    ld de, $c201
    call Call_000_14f6
    call Call_000_157b
    call Call_000_26c5
    jr jr_000_068d

jr_000_0680:
    ldh a, [$ff81]
    bit 0, a
    jr nz, jr_000_068d

    bit 3, a
    jr nz, jr_000_068d

    call Call_000_1514

jr_000_068d:
    ldh a, [$ffcb]
    cp $29
    jr z, jr_000_06b1

    ldh a, [$ffcc]
    and a
    ret z

    xor a
    ldh [$ffcc], a
    ld a, $39
    ldh [$ffcf], a
    ldh a, [$ffd0]
    cp $50
    jr z, jr_000_06d1

    ld b, a
    ldh a, [$ffc1]
    cp b
    ret z

    ld a, b
    ldh [$ffc1], a
    ld a, $01
    ldh [$fff0], a
    ret


jr_000_06b1:
    ldh a, [$ff81]
    bit 3, a
    jr nz, jr_000_06d9

    bit 0, a
    jr nz, jr_000_06d9

    ldh a, [$ffcc]
    and a
    ret z

    xor a
    ldh [$ffcc], a
    ldh a, [$ffcf]
    cp $50
    jr z, jr_000_06d1

    ldh a, [$ffc1]

jr_000_06ca:
    ldh [$ffcf], a
    ld a, $01
    ldh [$ffce], a
    ret


jr_000_06d1:
    call ClearOAM
    ld a, $16
    ldh [$ffe1], a
    ret


jr_000_06d9:
    ld a, $50
    jr jr_000_06ca

jr_000_06dd:
    ld hl, $ff02
    set 7, [hl]
    jr jr_000_0703

    ld a, $03
    ldh [$ffcd], a
    ldh a, [$ffcb]
    cp $29
    jr nz, jr_000_06dd

    call Call_000_0b10
    call Call_000_0b10
    call Call_000_0b10
    ld b, $00
    ld hl, $c300

jr_000_06fc:
    call Call_000_0b10
    ld [hl+], a
    dec b
    jr nz, jr_000_06fc

jr_000_0703:
    call DisableLCD
    call LoadGameTiles
    ld de, $525c
    call LoadTilemap
    call ClearOAM
    ld a, $2f
    call Call_000_2038
    ld a, $03
    ldh [$ffce], a
    xor a
    ldh [rSB], a
    ldh [$ffcf], a
    ldh [$ffdc], a
    ldh [$ffd2], a
    ldh [$ffd3], a
    ldh [$ffd4], a
    ldh [$ffd5], a
    ldh [$ffe3], a

jr_000_072c:
    ldh [$ffcc], a
    ld hl, $c400
    ld b, $0a
    ld a, $28

jr_000_0735:
    ld [hl+], a
    dec b
    jr nz, jr_000_0735

    ldh a, [$ffd6]
    and a
    jp nz, Jump_000_07da

    call Call_000_157b
    ld a, $d3
    ldh [rLCDC], a
    ld hl, $c080

jr_000_0749:
    ld de, $0772
    ld b, $20

jr_000_074e:
    call $0792
    ld hl, $c200
    ld de, $2741
    ld c, $02
    call Call_000_17da
    call Call_000_087b
    call Call_000_26c5
    xor a
    ldh [$ffd7], a
    ldh [$ffd8], a
    ldh [$ffd9], a
    ldh [$ffda], a
    ldh [$ffdb], a
    ld a, $17
    ldh [$ffe1], a
    ret


    ld b, b
    jr z, @-$50

    nop
    ld b, b
    jr nc, @-$50

    jr nz, @+$4a

    jr z, jr_000_072c

    nop
    ld c, b
    jr nc, @-$4f

    jr nz, jr_000_07fb

    jr z, @-$3e

    nop
    ld a, b
    jr nc, jr_000_0749

    jr nz, @-$7e

    jr z, jr_000_074e

    nop
    add b
    jr nc, @-$3d

    jr nz, @+$1c

    ld [hl+], a
    inc de
    dec b
    jr nz, @-$04

    ret


    ldh a, [$ffcb]
    cp $29
    jr z, jr_000_07c2

    ldh a, [$ffcc]
    and a
    jr z, jr_000_07b7

    ldh a, [$ffd0]
    cp $60
    jr z, jr_000_07d7

    cp $06
    jr nc, jr_000_07b0

    ldh [$ffac], a

jr_000_07b0:
    ldh a, [$ffad]
    ldh [$ffcf], a
    xor a
    ldh [$ffcc], a

jr_000_07b7:
    ld de, $c210
    call DoBlink
    ld hl, $ffad
    jr jr_000_082a

jr_000_07c2:
    ldh a, [$ff81]
    bit 3, a
    jr z, jr_000_07cc

    ld a, $60
    jr jr_000_0819

jr_000_07cc:
    ldh a, [$ffcc]
    and a
    jr z, jr_000_0821

    ldh a, [$ffcf]
    cp $60
    jr nz, jr_000_080f

jr_000_07d7:
    call ClearOAM

Jump_000_07da:
    ldh a, [$ffd6]
    and a
    jr nz, jr_000_07f7

    ld a, $18
    ldh [$ffe1], a
    ldh a, [$ffcb]
    cp $29
    ret nz

    xor a
    ldh [$ffa0], a
    ld a, $06
    ld de, $ffe0
    ld hl, $c9a2
    call Call_000_1bc3
    ret


jr_000_07f7:
    ldh a, [$ffcb]
    cp $29

jr_000_07fb:
    jp nz, Jump_000_0895

    xor a
    ldh [$ffa0], a
    ld a, $06
    ld de, $ffe0
    ld hl, $c9a2
    call Call_000_1bc3
    jp Jump_000_0895


jr_000_080f:
    ldh a, [$ffd0]
    cp $06
    jr nc, jr_000_0817

    ldh [$ffad], a

jr_000_0817:
    ldh a, [$ffac]

jr_000_0819:
    ldh [$ffcf], a
    xor a
    ldh [$ffcc], a
    inc a
    ldh [$ffce], a

jr_000_0821:
    ld de, $c200
    call DoBlink
    ld hl, $ffac

jr_000_082a:
    ld a, [hl]
    bit 4, b
    jr nz, jr_000_0843

    bit 5, b
    jr nz, jr_000_0855

Call_000_0833:
    bit 6, b
    jr nz, jr_000_085b

    bit 7, b
    jr z, jr_000_084e

    cp $03
    jr nc, jr_000_084e

    add $03
    jr jr_000_0848

jr_000_0843:
    cp $05
    jr z, jr_000_084e

    inc a

jr_000_0848:
    ld [hl], a
    ld a, $01
    ld [$dfe0], a

jr_000_084e:
    call Call_000_087b
    call Call_000_26c5
    ret


jr_000_0855:
    and a
    jr z, jr_000_084e

    dec a
    jr jr_000_0848

jr_000_085b:
    cp $03
    jr c, jr_000_084e

    sub $03
    jr jr_000_0848

    ld b, b
    ld h, b
    ld b, b
    ld [hl], b
    ld b, b
    add b
    ld d, b
    ld h, b
    ld d, b
    ld [hl], b
    ld d, b
    add b
    ld a, b
    ld h, b
    ld a, b
    ld [hl], b
    ld a, b
    add b
    adc b
    ld h, b
    adc b
    ld [hl], b
    adc b
    add b

Call_000_087b:
    ldh a, [$ffac]
    ld de, $c201
    ld hl, $0863
    call Call_000_17b9
    ldh a, [$ffad]
    ld de, $c211
    ld hl, $086f
    call Call_000_17b9
    ret


    call DisableLCD

Jump_000_0895:
    xor a
    ld [$c210], a
    ldh [$ff98], a
    ldh [$ff9c], a
    ldh [$ff9b], a
    ldh [$fffb], a
    ldh [$ff9f], a
    ldh [$ffcc], a
    ldh [rSB], a
    ldh [$ffce], a
    ldh [$ffd0], a
    ldh [$ffcf], a
    ldh [$ffd1], a
    call Call_000_26a5
    call Call_000_22f3
    call Call_000_204d
    xor a

jr_000_08b9:
    ldh [$ffe3], a
    ldh [$ffe7], a
    call ClearOAM
    ld de, $53c4
    push de
    ld a, $01
    ldh [$ffa9], a
    ldh [$ffc5], a
    call LoadTilemap

jr_000_08cd:
    pop de
    ld hl, $9c00
    call Call_000_2842
    ld de, $288d
    ld hl, $9c63
    ld c, $0a
    call Call_000_1fd8
    ld hl, $c200
    ld de, $2713
    call Call_000_270a
    ld hl, $c210
    ld de, $271b
    call Call_000_270a
    ld hl, $9951
    ld a, $30
    ldh [$ff9e], a
    ld [hl], $00
    dec l
    ld [hl], $03
    call Call_000_1b43
    xor a
    ldh [$ffa0], a
    ldh a, [$ffcb]
    cp $29
    ld de, $0943
    ldh a, [$ffac]
    jr z, jr_000_0913

    ld de, $0933
    ldh a, [$ffad]

jr_000_0913:
    ld hl, $98b0
    ld [hl], a
    ld h, $9c
    ld [hl], a
    ld hl, $c080
    ld b, $10
    call $0792
    ld a, $77
    ldh [$ffc0], a
    ld a, $d3
    ldh [rLCDC], a
    ld a, $19
    ldh [$ffe1], a
    ld a, $01
    ldh [$ffcd], a

Call_000_0932:
    ret


    jr jr_000_08b9

    ret nz

    nop
    jr @-$72

    ret nz

    jr nz, jr_000_095c

    add h
    pop bc
    nop
    jr nz, jr_000_08cd

    pop bc
    jr nz, jr_000_095c

    add h
    xor [hl]
    nop
    jr @-$72

    xor [hl]
    jr nz, jr_000_096c

    add h
    xor a
    nop
    jr nz, @-$72

    xor a
    jr nz, jr_000_0992

    ld [$ffe0], sp
    xor a
    ldh [rIF], a
    ldh a, [$ffcb]

jr_000_095c:
    cp $29
    jp nz, Jump_000_0a65

jr_000_0961:
    call SleepForABit
    call SleepForABit
    xor a
    ldh [$ffcc], a
    ld a, $29

jr_000_096c:
    ldh [rSB], a
    ld a, $81
    ldh [rSC], a

jr_000_0972:
    ldh a, [$ffcc]
    and a
    jr z, jr_000_0972

    ldh a, [rSB]
    cp $55
    jr nz, jr_000_0961

    ld de, $0016
    ld c, $0a
    ld hl, $c902

jr_000_0985:
    ld b, $0a

jr_000_0987:
    xor a
    ldh [$ffcc], a
    call SleepForABit
    ld a, [hl+]
    ldh [rSB], a
    ld a, $81

jr_000_0992:
    ldh [rSC], a

jr_000_0994:
    ldh a, [$ffcc]
    and a
    jr z, jr_000_0994

    dec b
    jr nz, jr_000_0987

    add hl, de
    dec c
    jr nz, jr_000_0985

    ldh a, [$ffac]
    cp $05
    jr z, jr_000_09e3

    ld hl, $ca22
    ld de, $0040

jr_000_09ac:
    add hl, de
    inc a
    cp $05
    jr nz, jr_000_09ac

    ld de, $ca22
    ld c, $0a

jr_000_09b7:
    ld b, $0a

jr_000_09b9:
    ld a, [de]
    ld [hl+], a
    inc e
    dec b
    jr nz, jr_000_09b9

    push de
    ld de, $ffd6
    add hl, de
    pop de
    push hl
    ld hl, $ffd6
    add hl, de
    push hl
    pop de
    pop hl
    dec c
    jr nz, jr_000_09b7

    ld de, $ffd6

jr_000_09d3:
    ld b, $0a
    ld a, h
    cp $c8
    jr z, jr_000_09e3

    ld a, $2f

jr_000_09dc:
    ld [hl+], a
    dec b
    jr nz, jr_000_09dc

    add hl, de
    jr jr_000_09d3

jr_000_09e3:
    call SleepForABit
    call SleepForABit
    xor a
    ldh [$ffcc], a
    ld a, $29
    ldh [rSB], a
    ld a, $81
    ldh [rSC], a

jr_000_09f4:
    ldh a, [$ffcc]
    and a
    jr z, jr_000_09f4

    ldh a, [rSB]
    cp $55
    jr nz, jr_000_09e3

    ld hl, $c300
    ld b, $00

jr_000_0a04:
    xor a
    ldh [$ffcc], a
    ld a, [hl+]
    call SleepForABit
    ldh [rSB], a
    ld a, $81
    ldh [rSC], a

jr_000_0a11:
    ldh a, [$ffcc]
    and a
    jr z, jr_000_0a11

    inc b
    jr nz, jr_000_0a04

jr_000_0a19:
    call SleepForABit
    call SleepForABit
    xor a
    ldh [$ffcc], a
    ld a, $30
    ldh [rSB], a
    ld a, $81
    ldh [rSC], a

jr_000_0a2a:
    ldh a, [$ffcc]
    and a
    jr z, jr_000_0a2a

    ldh a, [rSB]
    cp $56
    jr nz, jr_000_0a19

Jump_000_0a35:
    call Call_000_0afb
    ld a, $09
    ldh [rIE], a
    ld a, $1c
    ldh [$ffe1], a
    ld a, $02
    ldh [$ffe3], a
    ld a, $03
    ldh [$ffcd], a
    ldh a, [$ffcb]
    cp $29
    jr z, jr_000_0a53

    ld hl, $ff02
    set 7, [hl]

jr_000_0a53:
    ld hl, $c300
    ld a, [hl+]
    ld [$c203], a
    ld a, [hl+]
    ld [$c213], a
    ld a, h
    ldh [$ffaf], a
    ld a, l
    ldh [$ffb0], a
    ret


Jump_000_0a65:
    ldh a, [$ffad]
    inc a
    ld b, a
    ld hl, $ca42
    ld de, $ffc0

jr_000_0a6f:
    dec b
    jr z, jr_000_0a75

    add hl, de
    jr jr_000_0a6f

jr_000_0a75:
    call SleepForABit
    xor a
    ldh [$ffcc], a
    ld a, $55
    ldh [rSB], a
    ld a, $80
    ldh [rSC], a

jr_000_0a83:
    ldh a, [$ffcc]
    and a
    jr z, jr_000_0a83

    ldh a, [rSB]
    cp $29
    jr nz, jr_000_0a75

    ld de, $0016
    ld c, $0a

jr_000_0a93:
    ld b, $0a

jr_000_0a95:
    xor a
    ldh [$ffcc], a
    ldh [rSB], a
    ld a, $80
    ldh [rSC], a

jr_000_0a9e:
    ldh a, [$ffcc]
    and a
    jr z, jr_000_0a9e

    ldh a, [rSB]
    ld [hl+], a
    dec b
    jr nz, jr_000_0a95

    add hl, de
    dec c
    jr nz, jr_000_0a93

jr_000_0aad:
    call SleepForABit
    xor a
    ldh [$ffcc], a
    ld a, $55
    ldh [rSB], a
    ld a, $80
    ldh [rSC], a

jr_000_0abb:
    ldh a, [$ffcc]
    and a
    jr z, jr_000_0abb

    ldh a, [rSB]
    cp $29
    jr nz, jr_000_0aad

    ld b, $00
    ld hl, $c300

jr_000_0acb:
    xor a
    ldh [$ffcc], a
    ldh [rSB], a
    ld a, $80
    ldh [rSC], a

jr_000_0ad4:
    ldh a, [$ffcc]
    and a
    jr z, jr_000_0ad4

    ldh a, [rSB]
    ld [hl+], a
    inc b
    jr nz, jr_000_0acb

jr_000_0adf:
    call SleepForABit
    xor a
    ldh [$ffcc], a
    ld a, $56
    ldh [rSB], a
    ld a, $80
    ldh [rSC], a

jr_000_0aed:
    ldh a, [$ffcc]
    and a
    jr z, jr_000_0aed

    ldh a, [rSB]
    cp $30
    jr nz, jr_000_0adf

    jp Jump_000_0a35


Call_000_0afb:
    ld hl, $ca42
    ld a, $80
    ld b, $0a

jr_000_0b02:
    ld [hl+], a
    dec b
    jr nz, jr_000_0b02

    ret


SleepForABit:
    push bc
    ld b, $fa

.loop:
    ld b, b
    dec b
    jr nz, .loop

    pop bc
    ret


Call_000_0b10:
    push hl
    push bc
    ldh a, [$fffc]
    and $fc
    ld c, a
    ld h, $03

jr_000_0b19:
    ldh a, [rDIV]
    ld b, a

jr_000_0b1c:
    xor a

jr_000_0b1d:
    dec b
    jr z, jr_000_0b2a

    inc a
    inc a
    inc a
    inc a
    cp $1c
    jr z, jr_000_0b1c

    jr jr_000_0b1d

jr_000_0b2a:
    ld d, a
    ldh a, [$ffae]
    ld e, a
    dec h
    jr z, jr_000_0b38

    or d
    or c
    and $fc
    cp c
    jr z, jr_000_0b19

jr_000_0b38:
    ld a, d
    ldh [$ffae], a
    ld a, e
    ldh [$fffc], a
    pop bc
    pop hl
    ret


    ld a, $01
    ldh [rIE], a
    ldh a, [$ffe3]
    and a
    jr nz, jr_000_0b66

    ld b, $44
    ld c, $20
    call Call_000_11a3
    ld a, $02
    ldh [$ffcd], a
    call Call_000_26d7
    call Call_000_26ea
    call Call_000_157b
    xor a
    ldh [$ffd6], a
    ld a, $1a
    ldh [$ffe1], a
    ret


jr_000_0b66:
    cp $05
    ret nz

    ld hl, $c030
    ld b, $12

jr_000_0b6e:
    ld [hl], $f0
    inc hl
    ld [hl], $10
    inc hl
    ld [hl], $b6
    inc hl
    ld [hl], $80
    inc hl
    dec b
    jr nz, jr_000_0b6e

    ld a, [$c3ff]

jr_000_0b80:
    ld b, $0a
    ld hl, $c400

jr_000_0b85:
    dec a
    jr z, jr_000_0b8e

    inc l
    dec b
    jr nz, jr_000_0b85

    jr jr_000_0b80

jr_000_0b8e:
    ld [hl], $2f
    ld a, $03
    ldh [$ffce], a
    ret


    ld a, $01
    ldh [rIE], a
    ld hl, $c09c
    xor a
    ld [hl+], a
    ld [hl], $50
    inc l
    ld [hl], $27
    inc l
    ld [hl], $00
    call Call_000_1c68
    call Call_000_1ce3
    call Call_000_2515
    call Call_000_20f7
    call Call_000_2199
    call Call_000_25f5
    call Call_000_22ad
    call Call_000_0bff
    ldh a, [$ffd5]
    and a
    jr z, jr_000_0bd7

    ld a, $77
    ldh [$ffcf], a
    ldh [$ffb1], a
    ld a, $aa
    ldh [$ffd1], a
    ld a, $1b
    ldh [$ffe1], a
    ld a, $05
    ldh [$ffa7], a
    jr jr_000_0be7

jr_000_0bd7:
    ldh a, [$ffe1]
    cp $01
    jr nz, jr_000_0bf8

    ld a, $aa
    ldh [$ffcf], a
    ldh [$ffb1], a
    ld a, $77
    ldh [$ffd1], a

jr_000_0be7:
    xor a
    ldh [$ffdc], a
    ldh [$ffd2], a
    ldh [$ffd3], a
    ldh [$ffd4], a
    ldh a, [$ffcb]
    cp $29
    jr nz, jr_000_0bf8

    ldh [$ffce], a

jr_000_0bf8:
    call Call_000_0c54
    call Call_000_0cf0
    ret


Call_000_0bff:
    ld de, $0020
    ld hl, $c802
    ld a, $2f
    ld c, $12

jr_000_0c09:
    ld b, $0a
    push hl

jr_000_0c0c:
    cp [hl]
    jr nz, jr_000_0c19

    inc hl
    dec b
    jr nz, jr_000_0c0c

    pop hl
    add hl, de
    dec c
    jr nz, jr_000_0c09

    push hl

jr_000_0c19:
    pop hl
    ld a, c
    ldh [$ffb1], a
    cp $0c
    ld a, [$dfe9]
    jr nc, jr_000_0c2b

    cp $08
    ret nz

    call Call_000_157b
    ret


jr_000_0c2b:
    cp $08
    ret z

    ld a, [$dff0]
    cp $02
    ret z

    ld a, $08
    ld [$dfe8], a
    ret


jr_000_0c3a:
    ldh a, [$ffcb]
    cp $29
    jr z, jr_000_0c92

    ld a, $01
    ld [$df7f], a
    ldh [$ffab], a
    ldh a, [$ffcf]
    ldh [$fff1], a
    xor a
    ldh [$fff2], a
    ldh [$ffcf], a
    call Call_000_1d26
    ret


Call_000_0c54:
    ldh a, [$ffcc]
    and a
    ret z

    ld hl, $c030
    ld de, $0004
    xor a
    ldh [$ffcc], a
    ldh a, [$ffd0]
    cp $aa
    jr z, jr_000_0cc8

    cp $77
    jr z, jr_000_0cb4

    cp $94
    jr z, jr_000_0c3a

    ld b, a
    and a
    jr z, jr_000_0cc4

    bit 7, a
    jr nz, jr_000_0ce6

    cp $13
    jr nc, jr_000_0c92

    ld a, $12
    sub b
    ld c, a
    inc c

jr_000_0c80:
    ld a, $98

jr_000_0c82:
    ld [hl], a
    add hl, de
    sub $08
    dec b
    jr nz, jr_000_0c82

jr_000_0c89:
    ld a, $f0

jr_000_0c8b:
    dec c
    jr z, jr_000_0c92

    ld [hl], a
    add hl, de
    jr jr_000_0c8b

jr_000_0c92:
    ldh a, [$ffdc]
    and a
    jr z, jr_000_0c9e

    or $80
    ldh [$ffb1], a
    xor a
    ldh [$ffdc], a

jr_000_0c9e:
    ld a, $ff
    ldh [$ffd0], a
    ldh a, [$ffcb]
    cp $29
    ldh a, [$ffb1]
    jr nz, jr_000_0cb1

    ldh [$ffcf], a
    ld a, $01
    ldh [$ffce], a
    ret


jr_000_0cb1:
    ldh [$ffcf], a
    ret


jr_000_0cb4:
    ldh a, [$ffd1]
    cp $aa
    jr z, jr_000_0ce0

    ld a, $77
    ldh [$ffd1], a
    ld a, $01
    ldh [$ffe1], a
    jr jr_000_0c92

jr_000_0cc4:
    ld c, $13
    jr jr_000_0c89

jr_000_0cc8:
    ldh a, [$ffd1]
    cp $77
    jr z, jr_000_0ce0

    ld a, $aa
    ldh [$ffd1], a
    ld a, $1b
    ldh [$ffe1], a
    ld a, $05
    ldh [$ffa7], a
    ld c, $01
    ld b, $12
    jr jr_000_0c80

jr_000_0ce0:
    ld a, $01
    ldh [$ffef], a
    jr jr_000_0c92

jr_000_0ce6:
    and $7f
    cp $05
    jr nc, jr_000_0c92

    ldh [$ffd2], a
    jr jr_000_0c9e

Call_000_0cf0:
    ldh a, [$ffd3]
    and a
    jr z, jr_000_0cfc

    bit 7, a
    ret z

    and $07
    jr jr_000_0d06

jr_000_0cfc:
    ldh a, [$ffd2]
    and a
    ret z

    ldh [$ffd3], a
    xor a
    ldh [$ffd2], a
    ret


jr_000_0d06:
    ld c, a
    push bc
    ld hl, $c822
    ld de, $ffe0

jr_000_0d0e:
    add hl, de
    dec c
    jr nz, jr_000_0d0e

    ld de, $c822
    ld c, $11

jr_000_0d17:
    ld b, $0a

jr_000_0d19:
    ld a, [de]
    ld [hl+], a
    inc e
    dec b
    jr nz, jr_000_0d19

    push de
    ld de, $0016
    add hl, de
    pop de
    push hl
    ld hl, $0016
    add hl, de
    push hl
    pop de
    pop hl
    dec c
    jr nz, jr_000_0d17

    pop bc

jr_000_0d31:
    ld de, $c400
    ld b, $0a

jr_000_0d36:
    ld a, [de]
    ld [hl+], a
    inc de
    dec b
    jr nz, jr_000_0d36

    push de
    ld de, $0016
    add hl, de
    pop de
    dec c
    jr nz, jr_000_0d31

    ld a, $02
    ldh [$ffe3], a
    ldh [$ffd4], a
    xor a
    ldh [$ffd3], a
    ret


    ldh a, [$ffa6]
    and a
    ret nz

    ld a, $01
    ldh [rIE], a
    ld a, $03
    ldh [$ffcd], a
    ldh a, [$ffd1]
    cp $77
    jr nz, jr_000_0d6d

    ldh a, [$ffd0]
    cp $aa
    jr nz, jr_000_0d77

jr_000_0d67:
    ld a, $01
    ldh [$ffef], a
    jr jr_000_0d77

jr_000_0d6d:
    cp $aa
    jr nz, jr_000_0d77

    ldh a, [$ffd0]
    cp $77
    jr z, jr_000_0d67

jr_000_0d77:
    ld b, $34
    ld c, $43
    call Call_000_11a3
    xor a
    ldh [$ffe3], a
    ldh a, [$ffd1]
    cp $aa
    ld a, $1e
    jr nz, jr_000_0d8b

    ld a, $1d

jr_000_0d8b:
    ldh [$ffe1], a
    ld a, $28
    ldh [$ffa6], a
    ld a, $1d
    ldh [$ffc6], a
    ret


    ldh a, [$ffa6]
    and a
    ret nz

    ldh a, [$ffef]
    and a
    jr nz, jr_000_0da4

    ldh a, [$ffd7]
    inc a
    ldh [$ffd7], a

jr_000_0da4:
    call Call_000_0fd3
    ld de, $274d
    ldh a, [$ffcb]
    cp $29
    jr z, jr_000_0db3

    ld de, $275f

jr_000_0db3:
    ld hl, $c200
    ld c, $03
    call Call_000_17da
    ld a, $19
    ldh [$ffa6], a
    ldh a, [$ffef]
    and a
    jr z, jr_000_0dc9

    ld hl, $c220
    ld [hl], $80

jr_000_0dc9:
    ld a, $03
    call Call_000_26c7
    ld a, $20
    ldh [$ffe1], a
    ld a, $09
    ld [$dfe8], a
    ldh a, [$ffd7]
    cp $05
    ret nz

    ld a, $11
    ld [$dfe8], a
    ret


jr_000_0de2:
    ldh a, [$ffd7]
    cp $05
    jr nz, jr_000_0def

    ldh a, [$ffc6]
    and a
    jr z, jr_000_0df5

    jr jr_000_0e11

jr_000_0def:
    ldh a, [$ff81]
    bit 3, a
    jr z, jr_000_0e11

jr_000_0df5:
    ld a, $60
    ldh [$ffcf], a
    ldh [$ffce], a
    jr jr_000_0e1a

    ld a, $01
    ldh [rIE], a
    ldh a, [$ffcc]
    jr z, jr_000_0e11

    ldh a, [$ffcb]
    cp $29
    jr z, jr_000_0de2

    ldh a, [$ffd0]
    cp $60
    jr z, jr_000_0e1a

jr_000_0e11:
    call Call_000_0e21
    ld a, $03
    call Call_000_26c7
    ret


jr_000_0e1a:
    ld a, $1f
    ldh [$ffe1], a
    ldh [$ffcc], a
    ret


Call_000_0e21:
    ldh a, [$ffa6]
    and a
    jr nz, jr_000_0e49

    ld hl, $ffc6
    dec [hl]
    ld a, $19
    ldh [$ffa6], a
    call Call_000_0fc4
    ld hl, $c201
    ld a, [hl]
    xor $30
    ld [hl+], a
    cp $60
    call z, Call_000_0f7b
    inc l
    push af
    ld a, [hl]
    xor $01
    ld [hl], a
    ld l, $13
    ld [hl-], a
    pop af
    dec l
    ld [hl], a

jr_000_0e49:
    ldh a, [$ffd7]
    cp $05
    jr nz, jr_000_0e77

    ldh a, [$ffc6]
    ld hl, $c221
    cp $06
    jr z, jr_000_0e73

    cp $08
    jr nc, jr_000_0e77

    ld a, [hl]
    cp $72
    jr nc, jr_000_0e67

    cp $69
    ret z

    inc [hl]
    inc [hl]
    ret


jr_000_0e67:
    ld [hl], $69
    inc l
    inc l
    ld [hl], $57
    ld a, $06
    ld [$dfe0], a
    ret


jr_000_0e73:
    dec l
    ld [hl], $80
    ret


jr_000_0e77:
    ldh a, [$ffa7]
    and a
    ret nz

    ld a, $0f
    ldh [$ffa7], a
    ld hl, $c223
    ld a, [hl]
    xor $01
    ld [hl], a
    ret


    ldh a, [$ffa6]
    and a
    ret nz

    ldh a, [$ffef]
    and a
    jr nz, jr_000_0e95

    ldh a, [$ffd8]
    inc a
    ldh [$ffd8], a

jr_000_0e95:
    call Call_000_0fd3
    ld de, $2771
    ldh a, [$ffcb]
    cp $29
    jr z, jr_000_0ea4

    ld de, $277d

jr_000_0ea4:
    ld hl, $c200
    ld c, $02
    call Call_000_17da
    ld a, $19
    ldh [$ffa6], a
    ldh a, [$ffef]
    and a
    jr z, jr_000_0eba

    ld hl, $c210
    ld [hl], $80

jr_000_0eba:
    ld a, $02
    call Call_000_26c7
    ld a, $21
    ldh [$ffe1], a
    ld a, $09
    ld [$dfe8], a
    ldh a, [$ffd8]
    cp $05
    ret nz

    ld a, $11
    ld [$dfe8], a
    ret


jr_000_0ed3:
    ldh a, [$ffd8]
    cp $05
    jr nz, jr_000_0ee0

    ldh a, [$ffc6]
    and a
    jr z, jr_000_0ee6

    jr jr_000_0f02

jr_000_0ee0:
    ldh a, [$ff81]
    bit 3, a
    jr z, jr_000_0f02

jr_000_0ee6:
    ld a, $60
    ldh [$ffcf], a
    ldh [$ffce], a
    jr jr_000_0f0b

    ld a, $01
    ldh [rIE], a
    ldh a, [$ffcc]
    jr z, jr_000_0f02

    ldh a, [$ffcb]
    cp $29
    jr z, jr_000_0ed3

    ldh a, [$ffd0]
    cp $60
    jr z, jr_000_0f0b

jr_000_0f02:
    call Call_000_0f12
    ld a, $02
    call Call_000_26c7
    ret


jr_000_0f0b:
    ld a, $1f
    ldh [$ffe1], a
    ldh [$ffcc], a
    ret


Call_000_0f12:
    ldh a, [$ffa6]
    and a
    jr nz, jr_000_0f33

    ld hl, $ffc6
    dec [hl]
    ld a, $19
    ldh [$ffa6], a
    call Call_000_0fc4
    ld hl, $c211
    ld a, [hl]
    xor $08
    ld [hl+], a
    cp $68
    call z, Call_000_0f7b
    inc l
    ld a, [hl]
    xor $01
    ld [hl], a

jr_000_0f33:
    ldh a, [$ffd8]
    cp $05
    jr nz, jr_000_0f6b

    ldh a, [$ffc6]
    ld hl, $c201
    cp $05
    jr z, jr_000_0f67

    cp $06
    jr z, jr_000_0f57

    cp $08
    jr nc, jr_000_0f6b

    ld a, [hl]
    cp $72
    jr nc, jr_000_0f67

    cp $61
    ret z

    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    ret


jr_000_0f57:
    dec l
    ld [hl], $00
    inc l
    ld [hl], $61
    inc l
    inc l
    ld [hl], $56
    ld a, $06
    ld [$dfe0], a
    ret


jr_000_0f67:
    dec l
    ld [hl], $80
    ret


jr_000_0f6b:
    ldh a, [$ffa7]
    and a
    ret nz

    ld a, $0f
    ldh [$ffa7], a
    ld hl, $c203
    ld a, [hl]
    xor $01
    ld [hl], a
    ret


Call_000_0f7b:
    push af
    push hl
    ldh a, [$ffd7]
    cp $05
    jr z, jr_000_0f9d

    ldh a, [$ffd8]
    cp $05
    jr z, jr_000_0f9d

    ldh a, [$ffcb]
    cp $29
    jr nz, jr_000_0f9d

    ld hl, $c060
    ld b, $24
    ld de, $0fa0

jr_000_0f97:
    ld a, [de]
    ld [hl+], a
    inc de
    dec b
    jr nz, jr_000_0f97

jr_000_0f9d:
    pop hl
    pop af
    ret


    ld b, d
    jr nc, jr_000_0fb0

    nop
    ld b, d
    jr c, @-$4c

    nop
    ld b, d
    ld b, b
    ld c, $00
    ld b, d
    ld c, b
    inc e
    nop

jr_000_0fb0:
    ld b, d
    ld e, b
    ld c, $00
    ld b, d
    ld h, b
    dec e
    nop
    ld b, d
    ld l, b
    or l
    nop
    ld b, d
    ld [hl], b
    cp e
    nop
    ld b, d
    ld a, b
    dec e
    nop

Call_000_0fc4:
    ld hl, $c060
    ld de, $0004
    ld b, $09
    xor a

jr_000_0fcd:
    ld [hl], a
    add hl, de
    dec b
    jr nz, jr_000_0fcd

    ret


Call_000_0fd3:
    call DisableLCD
    ld hl, $55f4
    ld bc, $1000
    call Call_000_2838
    call ClearTilemap
    ld hl, $9800
    ld de, $552c
    ld b, $04
    call Call_000_2844
    ld hl, $9980
    ld b, $06
    call Call_000_2844
    ldh a, [$ffcb]
    cp $29
    jr nz, jr_000_101d

    ld hl, $9841
    ld [hl], $bd
    inc l
    ld [hl], $b2
    inc l
    ld [hl], $2e
    inc l
    ld [hl], $be
    inc l
    ld [hl], $2e
    ld hl, $9a01
    ld [hl], $b4
    inc l
    ld [hl], $b5
    inc l
    ld [hl], $bb
    inc l
    ld [hl], $2e
    inc l
    ld [hl], $bc

jr_000_101d:
    ldh a, [$ffef]
    and a
    jr nz, jr_000_1025

    call Call_000_10e9

jr_000_1025:
    ldh a, [$ffd7]
    and a
    jr z, jr_000_1073

    cp $05
    jr nz, jr_000_1044

    ld hl, $98a5
    ld b, $0b
    ldh a, [$ffcb]
    cp $29
    ld de, $1157
    jr z, jr_000_103f

    ld de, $1162

jr_000_103f:
    call Call_000_113c
    ld a, $04

jr_000_1044:
    ld c, a
    ldh a, [$ffcb]
    cp $29
    ld a, $93
    jr nz, jr_000_104f

    ld a, $8f

jr_000_104f:
    ldh [$ffa0], a
    ld hl, $99e7
    call Call_000_10ce
    ldh a, [$ffd9]
    and a
    jr z, jr_000_1073

    ld a, $ac
    ldh [$ffa0], a
    ld hl, $99f0
    ld c, $01
    call Call_000_10ce
    ld hl, $98a6
    ld de, $116d
    ld b, $09
    call Call_000_113c

jr_000_1073:
    ldh a, [$ffd8]
    and a
    jr z, jr_000_10b6

    cp $05
    jr nz, jr_000_1092

    ld hl, $98a5
    ld b, $0b
    ldh a, [$ffcb]
    cp $29
    ld de, $1162
    jr z, jr_000_108d

    ld de, $1157

jr_000_108d:
    call Call_000_113c
    ld a, $04

jr_000_1092:
    ld c, a
    ldh a, [$ffcb]
    cp $29
    ld a, $8f
    jr nz, jr_000_109d

    ld a, $93

jr_000_109d:
    ldh [$ffa0], a
    ld hl, $9827
    call Call_000_10ce
    ldh a, [$ffda]
    and a
    jr z, jr_000_10b6

    ld a, $ac
    ldh [$ffa0], a
    ld hl, $9830
    ld c, $01
    call Call_000_10ce

jr_000_10b6:
    ldh a, [$ffdb]
    and a
    jr z, jr_000_10c6

    ld hl, $98a7
    ld de, $1151
    ld b, $06
    call Call_000_113c

jr_000_10c6:
    ld a, $d3
    ldh [rLCDC], a
    call ClearOAM
    ret


Call_000_10ce:
jr_000_10ce:
    ldh a, [$ffa0]
    push hl
    ld de, $0020
    ld b, $02

jr_000_10d6:
    push hl
    ld [hl+], a
    inc a
    ld [hl], a
    inc a
    pop hl
    add hl, de
    dec b
    jr nz, jr_000_10d6

    pop hl
    ld de, $0003
    add hl, de
    dec c
    jr nz, jr_000_10ce

    ret


Call_000_10e9:
    ld hl, $ffd7
    ld de, $ffd8
    ldh a, [$ffd9]
    and a
    jr nz, jr_000_112e

    ldh a, [$ffda]
    and a
    jr nz, jr_000_1135

    ldh a, [$ffdb]
    and a
    jr nz, jr_000_111f

    ld a, [hl]
    cp $04
    jr z, jr_000_1114

    ld a, [de]
    cp $04
    ret nz

jr_000_1107:
    ld a, $05
    ld [de], a
    jr jr_000_1116

    ld a, [de]
    cp $03
    ret nz

jr_000_1110:
    ld a, $03
    jr jr_000_1119

jr_000_1114:
    ld [hl], $05

jr_000_1116:
    xor a
    ldh [$ffdb], a

jr_000_1119:
    xor a
    ldh [$ffd9], a
    ldh [$ffda], a
    ret


jr_000_111f:
    ld a, [hl]
    cp $04
    jr nz, jr_000_112a

    ldh [$ffd9], a

jr_000_1126:
    xor a
    ldh [$ffdb], a
    ret


jr_000_112a:
    ldh [$ffda], a
    jr jr_000_1126

jr_000_112e:
    ld a, [hl]
    cp $05
    jr z, jr_000_1114

    jr jr_000_1110

jr_000_1135:
    ld a, [de]
    cp $05
    jr z, jr_000_1107

    jr jr_000_1110

Call_000_113c:
    push bc
    push hl

jr_000_113e:
    ld a, [de]
    ld [hl+], a
    inc de
    dec b
    jr nz, jr_000_113e

    pop hl
    ld de, $0020
    add hl, de
    pop bc
    ld a, $b6

jr_000_114c:
    ld [hl+], a
    dec b
    jr nz, jr_000_114c

    ret


    or b
    or c
    or d
    or e
    or c
    ld a, $b4
    or l
    cp e
    ld l, $bc
    cpl
    dec l
    ld l, $3d
    ld c, $3e
    cp l
    or d
    ld l, $be
    ld l, $2f
    dec l
    ld l, $3d
    ld c, $3e
    or l
    or b
    ld b, c
    or l
    dec a
    dec e
    or l
    cp [hl]
    or c
    ld a, $01
    ldh [rIE], a
    ldh a, [$ffa6]
    and a
    ret nz

    call ClearOAM
    xor a
    ldh [$ffef], a
    ld b, $27
    ld c, $79
    call Call_000_11a3
    call $7ff3
    ldh a, [$ffd7]
    cp $05
    jr z, jr_000_119e

    ldh a, [$ffd8]
    cp $05
    jr z, jr_000_119e

    ld a, $01
    ldh [$ffd6], a

jr_000_119e:
    ld a, $16
    ldh [$ffe1], a
    ret


Call_000_11a3:
    ldh a, [$ffcc]
    and a
    jr z, jr_000_11bc

    xor a
    ldh [$ffcc], a
    ldh a, [$ffcb]
    cp $29
    ldh a, [$ffd0]
    jr nz, jr_000_11c4

    cp b
    jr z, jr_000_11be

    ld a, $02
    ldh [$ffcf], a
    ldh [$ffce], a

jr_000_11bc:
    pop hl
    ret


jr_000_11be:
    ld a, c
    ldh [$ffcf], a
    ldh [$ffce], a
    ret


jr_000_11c4:
    cp c
    ret z

    ld a, b
    ldh [$ffcf], a
    pop hl
    ret


    call Call_000_1216
    ld hl, $9ce6
    ld de, $147f
    ld b, $07
    call Call_000_149b
    ld hl, $9ce7
    ld de, $1486
    ld b, $07
    call Call_000_149b
    ld hl, $9d08
    ld [hl], $72
    inc l
    ld [hl], $c4
    ld hl, $9d28
    ld [hl], $b7
    inc l
    ld [hl], $b8
    ld de, $27c5
    ld hl, $c200
    ld c, $03
    call Call_000_17da
    ld a, $03
    call Call_000_26c7
    ld a, $db
    ldh [rLCDC], a
    ld a, $bb
    ldh [$ffa6], a
    ld a, $27
    ldh [$ffe1], a
    ld a, $10
    ld [$dfe8], a
    ret


Call_000_1216:
    call DisableLCD
    ld hl, $55f4
    ld bc, $1000
    call Call_000_2838
    ld hl, $9fff
    call Call_000_27ec
    ld hl, $9dc0
    ld de, $520c
    ld b, $04
    call Call_000_2844
    ld hl, $9cec
    ld de, $148d
    ld b, $07
    call Call_000_149b
    ld hl, $9ced
    ld de, $1494
    ld b, $07
    call Call_000_149b
    ret


    ldh a, [$ffa6]
    and a
    ret nz

    ld hl, $c210
    ld [hl], $00
    ld l, $20
    ld [hl], $00
    ld a, $ff
    ldh [$ffa6], a
    ld a, $28
    ldh [$ffe1], a
    ret


    ldh a, [$ffa6]
    and a
    jr z, jr_000_1269

    call Call_000_145e
    ret


jr_000_1269:
    ld a, $29
    ldh [$ffe1], a
    ld hl, $c213
    ld [hl], $35
    ld l, $23
    ld [hl], $35
    ld a, $ff
    ldh [$ffa6], a
    ld a, $2f
    call Call_000_2032
    ret


    ldh a, [$ffa6]
    and a
    jr z, jr_000_1289

    call Call_000_145e
    ret


jr_000_1289:
    ld a, $02
    ldh [$ffe1], a
    ld hl, $9d08
    ld b, $2f
    call Call_000_1a63
    ld hl, $9d09
    call Call_000_1a63
    ld hl, $9d28
    call Call_000_1a63
    ld hl, $9d29
    call Call_000_1a63
    ret


    ldh a, [$ffa6]
    and a
    jr nz, jr_000_12db

    ld a, $0a
    ldh [$ffa6], a
    ld hl, $c201
    dec [hl]
    ld a, [hl]
    cp $58
    jr nz, jr_000_12db

    ld hl, $c210
    ld [hl], $00
    inc l
    add $20
    ld [hl+], a
    ld [hl], $4c
    inc l
    ld [hl], $40
    ld l, $20
    ld [hl], $80
    ld a, $03
    call Call_000_26c7
    ld a, $03
    ldh [$ffe1], a
    ld a, $04
    ld [$dff8], a
    ret


jr_000_12db:
    call Call_000_145e
    ret


    ldh a, [$ffa6]
    and a
    jr nz, jr_000_1301

    ld a, $0a
    ldh [$ffa6], a
    ld hl, $c211
    dec [hl]
    ld l, $01
    dec [hl]
    ld a, [hl]
    cp $d0
    jr nz, jr_000_1301

    ld a, $9c
    ldh [$ffc9], a
    ld a, $82
    ldh [$ffca], a
    ld a, $2c
    ldh [$ffe1], a
    ret


jr_000_1301:
    ldh a, [$ffa7]
    and a
    jr nz, jr_000_1311

    ld a, $06
    ldh [$ffa7], a
    ld hl, $c213
    ld a, [hl]
    xor $01
    ld [hl], a

jr_000_1311:
    ld a, $03
    call Call_000_26c7
    ret


    ldh a, [$ffa6]
    and a
    ret nz

    ld a, $06
    ldh [$ffa6], a
    ldh a, [$ffca]
    sub $82
    ld e, a
    ld d, $00
    ld hl, $1359
    add hl, de
    push hl
    pop de
    ldh a, [$ffc9]
    ld h, a
    ldh a, [$ffca]
    ld l, a
    ld a, [de]
    call Call_000_1a62
    push hl
    ld de, $0020
    add hl, de
    ld b, $b6
    call Call_000_1a63
    pop hl
    inc hl
    ld a, $02
    ld [$dfe0], a
    ld a, h
    ldh [$ffc9], a
    ld a, l
    ldh [$ffca], a
    cp $92
    ret nz

    ld a, $ff
    ldh [$ffa6], a
    ld a, $2d
    ldh [$ffe1], a
    ret


    or e
    cp h
    dec a
    cp [hl]
    cp e
    or l
    dec e
    or d
    cp l
    or l
    dec e
    ld l, $bc
    dec a
    ld c, $3e
    ldh a, [$ffa6]
    and a
    ret nz

    call DisableLCD
    call LoadGameTiles
    call Call_000_22f3
    ld a, $93
    ldh [rLCDC], a
    ld a, $05
    ldh [$ffe1], a
    ret


    ldh a, [$ffa6]
    and a
    ret nz

    ld a, $2e
    ldh [$ffe1], a
    ret


    call Call_000_1216
    ld de, $27d7
    ld hl, $c200
    ld c, $03
    call Call_000_17da
    ldh a, [$fff3]
    ld [$c203], a
    ld a, $03
    call Call_000_26c7
    xor a
    ldh [$fff3], a
    ld a, $db
    ldh [rLCDC], a
    ld a, $bb
    ldh [$ffa6], a
    ld a, $2f
    ldh [$ffe1], a
    ld a, $10
    ld [$dfe8], a
    ret


    ldh a, [$ffa6]
    and a
    ret nz

    ld hl, $c210
    ld [hl], $00
    ld l, $20
    ld [hl], $00
    ld a, $a0
    ldh [$ffa6], a
    ld a, $30
    ldh [$ffe1], a
    ret


    ldh a, [$ffa6]
    and a
    jr z, jr_000_13d4

    call Call_000_145e
    ret


jr_000_13d4:
    ld a, $31
    ldh [$ffe1], a
    ld a, $80
    ldh [$ffa6], a
    ld a, $2f
    call Call_000_2032
    ret


    ldh a, [$ffa6]
    and a
    jr nz, jr_000_1415

    ld a, $0a
    ldh [$ffa6], a
    ld hl, $c201
    dec [hl]
    ld a, [hl]
    cp $6a
    jr nz, jr_000_1415

    ld hl, $c210
    ld [hl], $00
    inc l
    add $10
    ld [hl+], a
    ld [hl], $54
    inc l
    ld [hl], $5c
    ld l, $20
    ld [hl], $80
    ld a, $03
    call Call_000_26c7
    ld a, $32
    ldh [$ffe1], a
    ld a, $04
    ld [$dff8], a
    ret


jr_000_1415:
    call Call_000_145e
    ret


    ldh a, [$ffa6]
    and a
    jr nz, jr_000_1433

    ld a, $0a
    ldh [$ffa6], a
    ld hl, $c211
    dec [hl]
    ld l, $01
    dec [hl]
    ld a, [hl]
    cp $e0
    jr nz, jr_000_1433

    ld a, $33
    ldh [$ffe1], a
    ret


jr_000_1433:
    ldh a, [$ffa7]
    and a
    jr nz, jr_000_1443

    ld a, $06
    ldh [$ffa7], a
    ld hl, $c213
    ld a, [hl]
    xor $01
    ld [hl], a

jr_000_1443:
    ld a, $03
    call Call_000_26c7
    ret


    call DisableLCD
    call LoadGameTiles
    call $7ff3
    call Call_000_22f3
    ld a, $93
    ldh [rLCDC], a
    ld a, $10
    ldh [$ffe1], a
    ret


Call_000_145e:
    ldh a, [$ffa7]
    and a
    ret nz

    ld a, $0a
    ldh [$ffa7], a
    ld a, $03
    ld [$dff8], a
    ld b, $02
    ld hl, $c210

jr_000_1470:
    ld a, [hl]
    xor $80
    ld [hl], a
    ld l, $20
    dec b
    jr nz, jr_000_1470

    ld a, $03
    call Call_000_26c7
    ret


    jp nz, $caca

    jp z, $caca

    jp z, $cbc3

    ld e, b
    ld c, b
    ld c, b
    ld c, b
    ld c, b
    ret z

    ld [hl], e
    ld [hl], e
    ld [hl], e
    ld [hl], e
    ld [hl], e
    ld [hl], e
    ret


    ld [hl], h
    ld [hl], h
    ld [hl], h
    ld [hl], h
    ld [hl], h
    ld [hl], h

Call_000_149b:
jr_000_149b:
    ld a, [de]
    ld [hl], a
    inc de
    push de
    ld de, $0020
    add hl, de
    pop de
    dec b
    jr nz, jr_000_149b

    ret


RunInitGameOptionsScreen:
    ; Disable interrupts/registers related to multi-player
    ld a, IEF_VBLANK 
    ldh [rIE], a

    xor a
    ldh [rSB], a
    ldh [rSC], a
    ldh [rIF], a

Call_000_14b3:
    call DisableLCD
    call LoadGameTiles

    ld de, GameOptionsTilemap
    call LoadTilemap

    call ClearOAM

    ld hl, $c200
    ld de, $2723
    ld c, $02
    call Call_000_17da
    ld de, $c201
    call Call_000_14f1
    ldh a, [$ffc0]
    ld e, $12
    ld [de], a
    inc de
    cp $37
    ld a, $1c
    jr z, jr_000_14e1

    ld a, $1d

jr_000_14e1:
    ld [de], a
    call Call_000_26c5
    call Call_000_157b

    ; Enable LCD
    ld a, %11010011
    ldh [rLCDC], a

    set_state MysteryState2
    ret


Call_000_14f1:
    ld a, $01
    ld [$dfe0], a

Call_000_14f6:
    ldh a, [$ffc1]
    push af
    sub $1c
    add a
    ld c, a
    ld b, $00
    ld hl, $150c
    add hl, bc
    ld a, [hl+]
    ld [de], a
    inc de
    ld a, [hl]
    ld [de], a
    inc de
    pop af
    ld [de], a
    ret


    db $70, $37

    ld [hl], b
    ld [hl], a
    add b
    scf
    add b
    ld [hl], a

Call_000_1514:
    ld de, $c200
    call DoBlink
    ld hl, $ffc1
    ld a, [hl]
    bit 3, b
    jp nz, Jump_000_15c7

    bit 0, b
    jp nz, Jump_000_15c7

    bit 1, b
    jr nz, jr_000_156d

jr_000_152c:
    inc e
    bit 4, b
    jr nz, jr_000_1557

    bit 5, b
    jr nz, jr_000_1562

    bit 6, b
    jr nz, jr_000_154f

    bit 7, b
    jp z, Jump_000_15c3

    cp $1e
    jr nc, jr_000_154b

    add $02

jr_000_1544:
    ld [hl], a
    call Call_000_14f1
    call Call_000_157b

jr_000_154b:
    call Call_000_26c5
    ret


jr_000_154f:
    cp $1e
    jr c, jr_000_154b

    sub $02
    jr jr_000_1544

jr_000_1557:
    cp $1d
    jr z, jr_000_154b

    cp $1f
    jr z, jr_000_154b

    inc a
    jr jr_000_1544

jr_000_1562:
    cp $1c
    jr z, jr_000_154b

    cp $1e
    jr z, jr_000_154b

    dec a
    jr jr_000_1544

jr_000_156d:
    push af
    ldh a, [$ffc5]
    and a
    jr z, jr_000_1576

    pop af
    jr jr_000_152c

jr_000_1576:
    pop af
    ld a, $0e
    jr jr_000_15d6

Call_000_157b:
    ldh a, [$ffc1]
    sub $17
    cp $08
    jr nz, jr_000_1585

    ld a, $ff

jr_000_1585:
    ld [$dfe8], a
    ret


RunMysteryState2:
    ld de, $c210
    call DoBlink

    ld hl, $ffc0 ; Check the selected game type
    ld a, [hl]

    bit PADB_START, b
    jr nz, jr_000_15c7

    bit PADB_A, b
    jr nz, jr_000_15db

    inc e
    inc e

    bit PADB_RIGHT, b
    jr nz, jr_000_15af

    bit PADB_LEFT, b
    jr z, jr_000_15c3

    cp $37
    jr z, jr_000_15c3

    ld a, $37
    ld b, $1c
    jr jr_000_15b7

jr_000_15af:
    cp $77
    jr z, jr_000_15c3

    ld a, $77
    ld b, $1d

jr_000_15b7:
    ld [hl], a
    push af
    ld a, $01
    ld [$dfe0], a
    pop af
    ld [de], a
    inc de
    ld a, b

jr_000_15c2:
    ld [de], a

Jump_000_15c3:
jr_000_15c3:
    call Call_000_26c5
    ret


Jump_000_15c7:
jr_000_15c7:
    ld a, $02
    ld [$dfe0], a
    ldh a, [$ffc0]
    cp $37
    ld a, $10
    jr z, jr_000_15d6

    ld a, $12

jr_000_15d6:
    ldh [$ffe1], a
    xor a
    jr jr_000_15c2

jr_000_15db:
    ld a, $0f
    jr jr_000_15d6

    call DisableLCD
    ld de, $4e87
    call LoadTilemap
    call Call_000_1960
    call ClearOAM
    ld hl, $c200
    ld de, $272f
    ld c, $01
    call Call_000_17da
    ld de, $c201
    ldh a, [$ffc2]
    ld hl, $1679
    call Call_000_17b2
    call Call_000_26c5
    call Call_000_17f9
    call Call_000_192e
    ld a, $d3
    ldh [rLCDC], a
    ld a, $11
    ldh [$ffe1], a
    ldh a, [$ffc7]
    and a
    jr nz, jr_000_161e

    call Call_000_157b
    ret


jr_000_161e:
    ld a, $15

jr_000_1620:
    ldh [$ffe1], a
    ret


    ld de, $c200
    call DoBlink
    ld hl, $ffc2
    ld a, $0a
    bit 3, b
    jr nz, jr_000_1620

    bit 0, b
    jr nz, jr_000_1620

    ld a, $08
    bit 1, b
    jr nz, jr_000_1620

    ld a, [hl]
    bit 4, b
    jr nz, jr_000_1655

    bit 5, b
    jr nz, jr_000_166b

    bit 6, b
    jr nz, jr_000_1671

    bit 7, b
    jr z, jr_000_1667

    cp $05
    jr nc, jr_000_1667

    add $05
    jr jr_000_165a

jr_000_1655:
    cp $09
    jr z, jr_000_1667

    inc a

jr_000_165a:
    ld [hl], a
    ld de, $c201
    ld hl, $1679
    call Call_000_17b2
    call Call_000_17f9

jr_000_1667:
    call Call_000_26c5
    ret


jr_000_166b:
    and a
    jr z, jr_000_1667

    dec a
    jr jr_000_165a

jr_000_1671:
    cp $05
    jr c, jr_000_1667

    sub $05
    jr jr_000_165a

    db $40, $30

    ld b, b
    ld b, b
    ld b, b
    ld d, b
    ld b, b
    ld h, b
    ld b, b
    ld [hl], b
    ld d, b
    jr nc, jr_000_16d6

    ld b, b
    ld d, b
    ld d, b
    ld d, b
    ld h, b
    ld d, b
    ld [hl], b
    call DisableLCD
    ld de, $4fef
    call LoadTilemap
    call ClearOAM
    ld hl, $c200
    ld de, $2735
    ld c, $02
    call Call_000_17da
    ld de, $c201
    ldh a, [$ffc3]
    ld hl, $1736
    call Call_000_17b2
    ld de, $c211
    ldh a, [$ffc4]
    ld hl, $17a5
    call Call_000_17b2
    call Call_000_26c5
    call Call_000_1813
    call Call_000_192e
    ld a, $d3
    ldh [rLCDC], a
    ld a, $13
    ldh [$ffe1], a
    ldh a, [$ffc7]
    and a
    jr nz, jr_000_16d4

    call Call_000_157b
    ret


jr_000_16d4:
    ld a, $15

jr_000_16d6:
    ldh [$ffe1], a
    ret


jr_000_16d9:
    ldh [$ffe1], a
    xor a
    ld [de], a
    ret


    ld de, $c200
    call DoBlink
    ld hl, $ffc3
    ld a, $0a
    bit 3, b
    jr nz, jr_000_16d9

    ld a, $14
    bit 0, b
    jr nz, jr_000_16d9

    ld a, $08
    bit 1, b
    jr nz, jr_000_16d9

    ld a, [hl]
    bit 4, b
    jr nz, jr_000_1712

    bit 5, b
    jr nz, jr_000_1728

    bit 6, b
    jr nz, jr_000_172e

    bit 7, b
    jr z, jr_000_1724

    cp $05
    jr nc, jr_000_1724

    add $05
    jr jr_000_1717

jr_000_1712:
    cp $09
    jr z, jr_000_1724

    inc a

jr_000_1717:
    ld [hl], a
    ld de, $c201
    ld hl, $1736
    call Call_000_17b2
    call Call_000_1813

jr_000_1724:
    call Call_000_26c5
    ret


jr_000_1728:
    and a
    jr z, jr_000_1724

    dec a
    jr jr_000_1717

jr_000_172e:
    cp $05
    jr c, jr_000_1724

    sub $05
    jr jr_000_1717

    ld b, b
    jr jr_000_1779

    jr z, jr_000_177b

    jr c, jr_000_177d

    ld c, b
    ld b, b
    ld e, b
    ld d, b
    jr jr_000_1793

    jr z, @+$52

    jr c, jr_000_1797

    ld c, b
    ld d, b
    ld e, b

jr_000_174a:
    ldh [$ffe1], a
    xor a
    ld [de], a
    ret


    ld de, $c210
    call DoBlink
    ld hl, $ffc4
    ld a, $0a
    bit 3, b
    jr nz, jr_000_174a

    bit 0, b
    jr nz, jr_000_174a

    ld a, $13
    bit 1, b
    jr nz, jr_000_174a

    ld a, [hl]
    bit 4, b
    jr nz, jr_000_1781

    bit 5, b
    jr nz, jr_000_1797

    bit 6, b
    jr nz, jr_000_179d

    bit 7, b
    jr z, jr_000_1793

jr_000_1779:
    cp $03

jr_000_177b:
    jr nc, jr_000_1793

jr_000_177d:
    add $03
    jr jr_000_1786

jr_000_1781:
    cp $05
    jr z, jr_000_1793

    inc a

jr_000_1786:
    ld [hl], a
    ld de, $c211
    ld hl, $17a5
    call Call_000_17b2
    call Call_000_1813

jr_000_1793:
    call Call_000_26c5
    ret


jr_000_1797:
    and a
    jr z, jr_000_1793

    dec a
    jr jr_000_1786

jr_000_179d:
    cp $03
    jr c, jr_000_1793

    sub $03
    jr jr_000_1786

    ld b, b
    ld [hl], b
    ld b, b
    add b
    ld b, b
    sub b
    ld d, b
    ld [hl], b
    ld d, b
    add b
    ld d, b
    sub b
    nop

Call_000_17b2:
    push af
    ld a, $01
    ld [$dfe0], a
    pop af

Call_000_17b9:
    push af
    add a
    ld c, a
    ld b, $00
    add hl, bc
    ld a, [hl+]
    ld [de], a
    inc de
    ld a, [hl]
    ld [de], a
    inc de
    pop af
    add $20
    ld [de], a
    ret


DoBlink:
    ; Read gamepad state into B
    ldh a, [$ff81]
    ld b, a

    ldh a, [$ffa6] ; Check counter, and do nothing if it hasn't reached 0
    and a
    ret nz

    ; When counter hits 0,
    ld a, $10 ; Reset to 10
    ldh [$ffa6], a

    ld a, [de]
    xor $80 ; Toggle the highest bit in [de]
    ld [de], a

    ret


Call_000_17da:
jr_000_17da:
    push hl
    ld b, $06

jr_000_17dd:
    ld a, [de]
    ld [hl+], a
    inc de
    dec b
    jr nz, jr_000_17dd

    pop hl
    ld a, $10
    add l
    ld l, a
    dec c
    jr nz, jr_000_17da

    ld [hl], $80
    ret


ClearOAM:
    xor a
    ld hl, $c000
    ld b, $a0

jr_000_17f4:
    ld [hl+], a
    dec b
    jr nz, jr_000_17f4

    ret


Call_000_17f9:
    call Call_000_1960
    ldh a, [$ffc2]
    ld hl, $d654
    ld de, $001b

jr_000_1804:
    and a
    jr z, jr_000_180b

    dec a
    add hl, de
    jr jr_000_1804

jr_000_180b:
    inc hl
    inc hl
    push hl
    pop de
    call Call_000_1864
    ret


Call_000_1813:
    call Call_000_1960
    ldh a, [$ffc3]
    ld hl, $d000
    ld de, $00a2

jr_000_181e:
    and a
    jr z, jr_000_1825

    dec a
    add hl, de
    jr jr_000_181e

jr_000_1825:
    ldh a, [$ffc4]
    ld de, $001b

jr_000_182a:
    and a
    jr z, jr_000_1831

    dec a
    add hl, de
    jr jr_000_182a

jr_000_1831:
    inc hl
    inc hl
    push hl
    pop de
    call Call_000_1864
    ret


Call_000_1839:
    ld b, $03

jr_000_183b:
    ld a, [hl]
    and $f0
    jr nz, jr_000_184b

    inc e
    ld a, [hl-]
    and $0f
    jr nz, jr_000_1855

    inc e
    dec b
    jr nz, jr_000_183b

    ret


jr_000_184b:
    ld a, [hl]
    and $f0
    swap a
    ld [de], a
    inc e
    ld a, [hl-]
    and $0f

jr_000_1855:
    ld [de], a
    inc e
    dec b
    jr nz, jr_000_184b

    ret


Call_000_185b:
    ld b, $03

Call_000_185d:
jr_000_185d:
    ld a, [hl-]
    ld [de], a
    dec de
    dec b
    jr nz, jr_000_185d

    ret


Call_000_1864:
    ld a, d
    ldh [$fffb], a
    ld a, e
    ldh [$fffc], a
    ld c, $03

jr_000_186c:
    ld hl, $c0a2
    push de
    ld b, $03

jr_000_1872:
    ld a, [de]
    sub [hl]
    jr c, jr_000_1886

    jr nz, jr_000_187d

    dec l
    dec de
    dec b
    jr nz, jr_000_1872

jr_000_187d:
    pop de
    inc de
    inc de
    inc de
    dec c
    jr nz, jr_000_186c

    jr jr_000_18e4

jr_000_1886:
    pop de
    ldh a, [$fffb]
    ld d, a
    ldh a, [$fffc]
    ld e, a
    push de
    push bc
    ld hl, $0006
    add hl, de
    push hl
    pop de
    dec hl
    dec hl
    dec hl

jr_000_1898:
    dec c
    jr z, jr_000_18a0

    call Call_000_185b
    jr jr_000_1898

jr_000_18a0:
    ld hl, $c0a2
    ld b, $03

jr_000_18a5:
    ld a, [hl-]
    ld [de], a
    dec e
    dec b
    jr nz, jr_000_18a5

    pop bc
    pop de
    ld a, c
    ldh [$ffc8], a
    ld hl, $0012
    add hl, de
    push hl
    ld de, $0006
    add hl, de
    push hl
    pop de
    pop hl

jr_000_18bc:
    dec c
    jr z, jr_000_18c6

    ld b, $06
    call Call_000_185d
    jr jr_000_18bc

jr_000_18c6:
    ld a, $60
    ld b, $05

jr_000_18ca:
    ld [de], a
    dec de
    dec b
    jr nz, jr_000_18ca

    ld a, $0a
    ld [de], a
    ld a, d
    ldh [$ffc9], a
    ld a, e
    ldh [$ffca], a
    xor a
    ldh [$ff9c], a
    ldh [$ffc6], a
    ld a, $01
    ld [$dfe8], a
    ldh [$ffc7], a

jr_000_18e4:
    ld de, $c9ac
    ldh a, [$fffb]
    ld h, a
    ldh a, [$fffc]
    ld l, a
    ld b, $03

jr_000_18ef:
    push hl
    push de
    push bc
    call Call_000_1839
    pop bc
    pop de
    ld hl, $0020
    add hl, de
    push hl
    pop de
    pop hl
    push de
    ld de, $0003
    add hl, de
    pop de
    dec b
    jr nz, jr_000_18ef

    dec hl
    dec hl
    ld b, $03
    ld de, $c9a4

jr_000_190e:
    push de
    ld c, $06

jr_000_1911:
    ld a, [hl+]
    and a
    jr z, jr_000_191a

    ld [de], a
    inc de
    dec c
    jr nz, jr_000_1911

jr_000_191a:
    pop de
    push hl
    ld hl, $0020
    add hl, de
    push hl
    pop de
    pop hl
    dec b
    jr nz, jr_000_190e

    call Call_000_26a5
    ld a, $01
    ldh [$ffe8], a
    ret


Call_000_192e:
    ldh a, [$ffe8]
    and a
    ret z

    ld hl, $99a4
    ld de, $c9a4
    ld c, $06

jr_000_193a:
    push hl

jr_000_193b:
    ld b, $06

jr_000_193d:
    ld a, [de]
    ld [hl+], a
    inc e
    dec b
    jr nz, jr_000_193d

    inc e
    inc l
    inc e
    inc l
    dec c
    jr z, jr_000_195b

    bit 0, c
    jr nz, jr_000_193b

    pop hl
    ld de, $0020
    add hl, de
    push hl
    pop de
    ld a, $30
    add d
    ld d, a
    jr jr_000_193a

jr_000_195b:
    pop hl
    xor a
    ldh [$ffe8], a
    ret


Call_000_1960:
    ld hl, $c9a4
    ld de, $0020
    ld a, $60
    ld c, $03

jr_000_196a:
    ld b, $0e
    push hl

jr_000_196d:
    ld [hl+], a
    dec b
    jr nz, jr_000_196d

    pop hl
    add hl, de
    dec c
    jr nz, jr_000_196a

    ret


    ldh a, [$ffc8]
    ld hl, $99e4
    ld de, $ffe0

jr_000_197f:
    dec a
    jr z, jr_000_1985

    add hl, de
    jr jr_000_197f

jr_000_1985:
    ldh a, [$ffc6]
    ld e, a
    ld d, $00
    add hl, de
    ldh a, [$ffc9]
    ld d, a
    ldh a, [$ffca]
    ld e, a
    ldh a, [$ffa6]
    and a
    jr nz, jr_000_19a8

    ld a, $07
    ldh [$ffa6], a
    ldh a, [$ff9c]
    xor $01
    ldh [$ff9c], a
    ld a, [de]
    jr z, jr_000_19a5

    ld a, $2f

jr_000_19a5:
    call Call_000_1a62

jr_000_19a8:
    ldh a, [$ff81]
    ld b, a
    ldh a, [$ff80]
    ld c, a
    ld a, $17
    bit 6, b
    jr nz, jr_000_19eb

    bit 6, c
    jr nz, jr_000_19e3

    bit 7, b
    jr nz, jr_000_1a14

    bit 7, c
    jr nz, jr_000_1a0c

    bit 0, b
    jr nz, jr_000_1a30

    bit 1, b
    jp nz, Jump_000_1a52

    bit 3, b
    ret z

jr_000_19cc:
    ld a, [de]
    call Call_000_1a62
    call Call_000_157b
    xor a
    ldh [$ffc7], a
    ldh a, [$ffc0]
    cp $37
    ld a, $11
    jr z, jr_000_19e0

    ld a, $13

jr_000_19e0:
    ldh [$ffe1], a
    ret


jr_000_19e3:
    ldh a, [$ffaa]
    dec a
    ldh [$ffaa], a
    ret nz

    ld a, $09

jr_000_19eb:
    ldh [$ffaa], a
    ld b, $26
    ldh a, [$fff4]
    and a
    jr z, jr_000_19f6

    ld b, $27

jr_000_19f6:
    ld a, [de]
    cp b
    jr nz, jr_000_1a04

    ld a, $2e

jr_000_19fc:
    inc a

jr_000_19fd:
    ld [de], a
    ld a, $01
    ld [$dfe0], a
    ret


jr_000_1a04:
    cp $2f
    jr nz, jr_000_19fc

    ld a, $0a
    jr jr_000_19fd

jr_000_1a0c:
    ldh a, [$ffaa]
    dec a
    ldh [$ffaa], a
    ret nz

    ld a, $09

jr_000_1a14:
    ldh [$ffaa], a
    ld b, $26
    ldh a, [$fff4]
    and a
    jr z, jr_000_1a1f

    ld b, $27

jr_000_1a1f:
    ld a, [de]
    cp $0a
    jr nz, jr_000_1a29

    ld a, $30

jr_000_1a26:
    dec a
    jr jr_000_19fd

jr_000_1a29:
    cp $2f
    jr nz, jr_000_1a26

    ld a, b
    jr jr_000_19fd

jr_000_1a30:
    ld a, [de]
    call Call_000_1a62
    ld a, $02
    ld [$dfe0], a
    ldh a, [$ffc6]
    inc a
    cp $06
    jr z, jr_000_19cc

    ldh [$ffc6], a
    inc de
    ld a, [de]
    cp $60
    jr nz, jr_000_1a4b

    ld a, $0a
    ld [de], a

jr_000_1a4b:
    ld a, d
    ldh [$ffc9], a
    ld a, e
    ldh [$ffca], a
    ret


Jump_000_1a52:
    ldh a, [$ffc6]
    and a
    ret z

    ld a, [de]
    call Call_000_1a62
    ldh a, [$ffc6]
    dec a
    ldh [$ffc6], a
    dec de
    jr jr_000_1a4b

Call_000_1a62:
    ld b, a

Call_000_1a63:
jr_000_1a63:
    ldh a, [rSTAT]
    and $03
    jr nz, jr_000_1a63

    ld [hl], b
    ret


    call DisableLCD
    xor a
    ld [$c210], a
    ldh [$ff98], a
    ldh [$ff9c], a
    ldh [$ff9b], a
    ldh [$fffb], a
    ldh [$ff9f], a
    ld a, $2f
    call Call_000_2032
    call Call_000_204d
    call Call_000_26a5
    xor a
    ldh [$ffe3], a
    ldh [$ffe7], a
    call ClearOAM
    ldh a, [$ffc0]
    ld de, $403f
    ld hl, $ffc3
    cp $77
    ld a, $50
    jr z, jr_000_1aa5

    ld a, $f1
    ld hl, $ffc2
    ld de, $3ed7

jr_000_1aa5:
    push de
    ldh [$ffe6], a
    ld a, [hl]
    ldh [$ffa9], a
    call LoadTilemap
    pop de
    ld hl, $9c00
    call Call_000_2842
    ld de, $288d
    ld hl, $9c63
    ld c, $0a
    call Call_000_1fd8
    ld h, $98
    ldh a, [$ffe6]
    ld l, a
    ldh a, [$ffa9]
    ld [hl], a
    ld h, $9c
    ld [hl], a
    ldh a, [$fff4]
    and a
    jr z, jr_000_1ad7

    inc hl
    ld [hl], $27
    ld h, $98
    ld [hl], $27

jr_000_1ad7:
    ld hl, $c200
    ld de, $2713
    call Call_000_270a
    ld hl, $c210
    ld de, $271b
    call Call_000_270a
    ld hl, $9951
    ldh a, [$ffc0]
    cp $77
    ld a, $25
    jr z, jr_000_1af5

    xor a

jr_000_1af5:
    ldh [$ff9e], a
    and $0f
    ld [hl-], a
    jr z, jr_000_1afe

    ld [hl], $02

jr_000_1afe:
    call Call_000_1b43
    call Call_000_2062
    call Call_000_2062
    call Call_000_2062
    call Call_000_26d7
    xor a
    ldh [$ffa0], a
    ldh a, [$ffc0]
    cp $77
    jr nz, jr_000_1b3b

    ld a, $34
    ldh [$ff99], a
    ldh a, [$ffc4]
    ld hl, $98b0
    ld [hl], a
    ld h, $9c
    ld [hl], a
    and a
    jr z, jr_000_1b3b

    ld b, a
    ldh a, [$ffe4]
    and a
    jr z, jr_000_1b31

    call Call_000_1b76
    jr jr_000_1b3b

jr_000_1b31:
    ld a, b
    ld de, $ffc0
    ld hl, $9a02
    call Call_000_1bc3

jr_000_1b3b:
    ld a, $d3
    ldh [rLCDC], a
    xor a
    ldh [$ffe1], a
    ret


Call_000_1b43:
    ldh a, [$ffa9]
    ld e, a
    ldh a, [$fff4]
    and a
    jr z, jr_000_1b55

    ld a, $0a
    add e
    cp $15
    jr c, jr_000_1b54

    ld a, $14

jr_000_1b54:
    ld e, a

jr_000_1b55:
    ld hl, $1b61
    ld d, $00
    add hl, de
    ld a, [hl]
    ldh [$ff99], a
    ldh [$ff9a], a
    ret


    db $34

    jr nc, jr_000_1b90

    jr z, jr_000_1b8a

    jr nz, jr_000_1b83

    dec d
    db $10
    ld a, [bc]
    add hl, bc
    ld [$0607], sp
    dec b
    dec b
    inc b
    inc b
    inc bc
    inc bc
    ld [bc], a

Call_000_1b76:
    ld hl, $99c2
    ld de, $1b9b
    ld c, $04

jr_000_1b7e:
    ld b, $0a
    push hl

jr_000_1b81:
    ld a, [de]
    ld [hl], a

jr_000_1b83:
    push hl
    ld a, h
    add $30
    ld h, a
    ld a, [de]
    ld [hl], a

jr_000_1b8a:
    pop hl
    inc l
    inc de
    dec b
    jr nz, jr_000_1b81

jr_000_1b90:
    pop hl
    push de
    ld de, $0020
    add hl, de
    pop de
    dec c
    jr nz, jr_000_1b7e

    ret


    add l
    cpl
    add d
    add [hl]
    add e
    cpl
    cpl
    add b
    add d
    add l
    cpl
    add d
    add h
    add d
    add e
    cpl
    add e
    cpl
    add a
    cpl
    cpl
    add l
    cpl
    add e
    cpl
    add [hl]
    add d
    add b
    add c
    cpl
    add e
    cpl
    add [hl]
    add e
    cpl
    add l
    cpl
    add l
    cpl
    cpl

Call_000_1bc3:
    ld b, a

jr_000_1bc4:
    dec b
    jr z, jr_000_1bca

    add hl, de
    jr jr_000_1bc4

jr_000_1bca:
    ldh a, [rDIV]
    ld b, a

jr_000_1bcd:
    ld a, $80

jr_000_1bcf:
    dec b
    jr z, jr_000_1bda

    cp $80
    jr nz, jr_000_1bcd

    ld a, $2f
    jr jr_000_1bcf

jr_000_1bda:
    cp $2f
    jr z, jr_000_1be6

    ldh a, [rDIV]
    and $07
    or $80
    jr jr_000_1be8

jr_000_1be6:
    ldh [$ffa0], a

jr_000_1be8:
    push af
    ld a, l
    and $0f
    cp $0b
    jr nz, jr_000_1bfb

    ldh a, [$ffa0]
    cp $2f
    jr z, jr_000_1bfb

    pop af
    ld a, $2f
    jr jr_000_1bfc

jr_000_1bfb:
    pop af

jr_000_1bfc:
    ld [hl], a
    push hl
    push af
    ldh a, [$ffc5]
    and a
    jr nz, jr_000_1c08

    ld de, $3000
    add hl, de

jr_000_1c08:
    pop af
    ld [hl], a
    pop hl
    inc hl
    ld a, l
    and $0f
    cp $0c
    jr nz, jr_000_1bca

    xor a
    ldh [$ffa0], a
    ld a, h
    and $0f
    cp $0a
    jr z, jr_000_1c23

jr_000_1c1d:
    ld de, $0016
    add hl, de
    jr jr_000_1bca

jr_000_1c23:
    ld a, l
    cp $2c
    jr nz, jr_000_1c1d

    ret


    call Call_000_1c68
    ldh a, [$ffab]
    and a
    ret nz

    call Call_000_0579
    call Call_000_05af
    call Call_000_05f0
    call Call_000_2515
    call Call_000_20f7
    call Call_000_2199
    call Call_000_25f5
    call Call_000_22ad
    call Call_000_1fec
    call Call_000_0620
    ret


jr_000_1c4f:
    bit 2, a
    ret z

    ld a, [$c0de]
    xor $01
    ld [$c0de], a
    jr z, jr_000_1c65

    ld a, $80

jr_000_1c5e:
    ld [$c210], a
    call Call_000_26ea
    ret


jr_000_1c65:
    xor a
    jr jr_000_1c5e

Call_000_1c68:
    ldh a, [$ff80]
    and $0f
    cp $0f
    jp z, Jump_000_029a

    ldh a, [$ffe4]
    and a
    ret nz

    ldh a, [$ff81]
    bit 3, a
    jr z, jr_000_1c4f

    ldh a, [$ffc5]
    and a
    jr nz, jr_000_1cc5

    ld hl, $ff40
    ldh a, [$ffab]
    xor $01
    ldh [$ffab], a
    jr z, jr_000_1cb5

    set 3, [hl]
    ld a, $01
    ld [$df7f], a
    ld hl, $994e
    ld de, $9d4e
    ld b, $04

jr_000_1c9a:
    ldh a, [rSTAT]
    and $03
    jr nz, jr_000_1c9a

    ld a, [hl+]
    ld [de], a
    inc de
    dec b
    jr nz, jr_000_1c9a

    ld a, $80

jr_000_1ca8:
    ld [$c210], a

jr_000_1cab:
    ld [$c200], a
    call Call_000_26d7
    call Call_000_26ea
    ret


jr_000_1cb5:
    res 3, [hl]
    ld a, $02
    ld [$df7f], a
    ld a, [$c0de]
    and a
    jr z, jr_000_1ca8

    xor a
    jr jr_000_1cab

jr_000_1cc5:
    ldh a, [$ffcb]
    cp $29
    ret nz

    ldh a, [$ffab]
    xor $01
    ldh [$ffab], a
    jr z, jr_000_1d05

    ld a, $01
    ld [$df7f], a
    ldh a, [$ffd0]
    ldh [$fff2], a
    ldh a, [$ffcf]
    ldh [$fff1], a
    call Call_000_1d26
    ret


Call_000_1ce3:
    ldh a, [$ffab]
    and a
    ret z

    ldh a, [$ffcc]
    jr z, jr_000_1d24

    xor a
    ldh [$ffcc], a
    ldh a, [$ffcb]
    cp $29
    jr nz, jr_000_1cfc

    ld a, $94
    ldh [$ffcf], a
    ldh [$ffce], a
    pop hl
    ret


jr_000_1cfc:
    xor a
    ldh [$ffcf], a
    ldh a, [$ffd0]
    cp $94
    jr z, jr_000_1d24

jr_000_1d05:
    ldh a, [$fff2]
    ldh [$ffd0], a
    ldh a, [$fff1]
    ldh [$ffcf], a
    ld a, $02
    ld [$df7f], a
    xor a
    ldh [$ffab], a
    ld hl, $98ee
    ld b, $8e
    ld c, $05

jr_000_1d1c:
    call Call_000_1a63
    inc l
    dec c
    jr nz, jr_000_1d1c

    ret


jr_000_1d24:
    pop hl
    ret


Call_000_1d26:
    ld hl, $98ee
    ld c, $05
    ld de, $1d38

jr_000_1d2e:
    ld a, [de]
    call Call_000_1a62
    inc de
    inc l
    dec c
    jr nz, jr_000_1d2e

    ret


    add hl, de
    ld a, [bc]
    ld e, $1c
    db $0e

    ld a, $80
    ld [$c200], a
    ld [$c210], a
    call Call_000_26d7
    call Call_000_26ea
    xor a
    ldh [$ff98], a
    ldh [$ff9c], a
    call Call_000_22f3
    ld a, $87
    call Call_000_2032
    ld a, $46
    ldh [$ffa6], a
    ld a, $0d
    ldh [$ffe1], a
    ret


    ldh a, [$ff81]
    bit 0, a
    jr nz, jr_000_1d6a

    bit 3, a
    ret z

jr_000_1d6a:
    xor a
    ldh [$ffe3], a
    ldh a, [$ffc5]
    and a
    ld a, $16
    jr nz, jr_000_1d7e

    ldh a, [$ffc0]
    cp $37
    ld a, $10
    jr z, jr_000_1d7e

    ld a, $12

jr_000_1d7e:
    ldh [$ffe1], a
    ret


    ldh a, [$ffa6]
    and a
    ret nz

    ld hl, $c802
    ld de, $28dd
    call Call_000_2858
    ldh a, [$ffc3]
    and a
    jr z, jr_000_1dc1

    ld de, $0040
    ld hl, $c827
    call Call_000_1ddf
    ld de, $0100
    ld hl, $c887
    call Call_000_1ddf
    ld de, $0300
    ld hl, $c8e7
    call Call_000_1ddf
    ld de, $1200
    ld hl, $c947
    call Call_000_1ddf
    ld hl, $c0a0
    ld b, $03
    xor a

jr_000_1dbd:
    ld [hl+], a
    dec b
    jr nz, jr_000_1dbd

jr_000_1dc1:
    ld a, $80
    ldh [$ffa6], a
    ld a, $80
    ld [$c200], a
    ld [$c210], a
    call Call_000_26d7
    call Call_000_26ea
    call $7ff3
    ld a, $25
    ldh [$ff9e], a
    ld a, $0b
    ldh [$ffe1], a
    ret


Call_000_1ddf:
    push hl
    ld hl, $c0a0
    ld b, $03
    xor a

jr_000_1de6:
    ld [hl+], a
    dec b
    jr nz, jr_000_1de6

    ldh a, [$ffc3]
    ld b, a
    inc b

jr_000_1dee:
    ld hl, $c0a0
    call Call_000_0166
    dec b
    jr nz, jr_000_1dee

    pop hl
    ld b, $03
    ld de, $c0a2

jr_000_1dfd:
    ld a, [de]
    and $f0
    jr nz, jr_000_1e0c

    ld a, [de]
    and $0f
    jr nz, jr_000_1e12

    dec e
    dec b
    jr nz, jr_000_1dfd

    ret


jr_000_1e0c:
    ld a, [de]
    and $f0
    swap a
    ld [hl+], a

jr_000_1e12:
    ld a, [de]
    and $0f
    ld [hl+], a
    dec e
    dec b
    jr nz, jr_000_1e0c

    ret


    ldh a, [$ffa6]
    and a
    ret nz

    ld a, $01
    ld [$c0c6], a
    ld a, $05
    ldh [$ffa6], a
    ret


    ldh a, [$ffa6]
    and a
    ret nz

    ld hl, $c802
    ld de, $5157
    call Call_000_2858
    call ClearOAM
    ld hl, $c200
    ld de, $2789
    ld c, $0a
    call Call_000_17da
    ld a, $10
    ld hl, $c266
    ld [hl], a
    ld l, $76
    ld [hl], a
    ld hl, $c20e
    ld de, $1e8c
    ld b, $0a

jr_000_1e55:
    ld a, [de]
    ld [hl+], a
    ld [hl+], a
    inc de
    push de
    ld de, $000e
    add hl, de
    pop de
    dec b
    jr nz, jr_000_1e55

    ldh a, [$ffc4]
    cp $05
    jr nz, jr_000_1e6a

    ld a, $09

jr_000_1e6a:
    inc a
    ld b, a
    ld hl, $c200
    ld de, $0010
    xor a

jr_000_1e73:
    ld [hl], a
    add hl, de
    dec b
    jr nz, jr_000_1e73

    ldh a, [$ffc4]
    add $0a
    ld [$dfe8], a
    ld a, $25
    ldh [$ff9e], a
    ld a, $1b
    ldh [$ffa6], a
    ld a, $23
    ldh [$ffe1], a
    ret


    inc e
    rrca
    ld e, $32
    jr nz, jr_000_1eaa

    ld h, $1d
    jr z, jr_000_1ec1

jr_000_1e96:
    ld a, $0a
    call Call_000_26c7
    ret


    ldh a, [$ffa6]
    cp $14
    jr z, jr_000_1e96

    and a
    ret nz

    ld hl, $c20e
    ld de, $0010

jr_000_1eaa:
    ld b, $0a

jr_000_1eac:
    push hl
    dec [hl]
    jr nz, jr_000_1ec5

    inc l
    ld a, [hl-]
    ld [hl], a
    ld a, l
    and $f0
    or $03
    ld l, a
    ld a, [hl]
    xor $01
    ld [hl], a
    cp $50
    jr z, jr_000_1ee4

jr_000_1ec1:
    cp $51
    jr z, jr_000_1eea

jr_000_1ec5:
    pop hl
    add hl, de
    dec b
    jr nz, jr_000_1eac

    ld a, $0a
    call Call_000_26c7
    ld a, [$dfe9]
    and a
    ret nz

    call ClearOAM
    ldh a, [$ffc4]
    cp $05
    ld a, $26
    jr z, jr_000_1ee1

    ld a, $05

jr_000_1ee1:
    ldh [$ffe1], a
    ret


jr_000_1ee4:
    dec l
    dec l
    ld [hl], $67
    jr jr_000_1ec5

jr_000_1eea:
    dec l
    dec l
    ld [hl], $5d
    jr jr_000_1ec5

jr_000_1ef0:
    xor a
    ld [$c0c6], a
    ld de, $c0c0
    ld a, [de]
    ld l, a
    inc de
    ld a, [de]
    ld h, a
    or l
    jp z, Jump_000_268e

    dec hl
    ld a, h
    ld [de], a
    dec de
    ld a, l
    ld [de], a
    ld de, $0001
    ld hl, $c0c2
    push de
    call Call_000_0166
    ld de, $c0c4
    ld hl, $99a5
    call Call_000_2a7e
    xor a
    ldh [$ffa6], a
    pop de
    ld hl, $c0a0
    call Call_000_0166
    ld de, $c0a2
    ld hl, $9a25
    call Call_000_2a82
    ld a, $02
    ld [$dfe0], a
    ret


Call_000_1f32:
    ld a, [$c0c6]
    and a
    ret z

    ld a, [$c0c5]
    cp $04
    jr z, jr_000_1ef0

    ld de, $0040
    ld bc, $9823
    ld hl, $c0ac
    and a
    jr z, jr_000_1f6d

    ld de, $0100
    ld bc, $9883
    ld hl, $c0b1
    cp $01
    jr z, jr_000_1f6d

    ld de, $0300
    ld bc, $98e3
    ld hl, $c0b6
    cp $02
    jr z, jr_000_1f6d

    ld de, $1200
    ld bc, $9943
    ld hl, $c0bb

jr_000_1f6d:
    call Call_000_262d
    ret


    ldh a, [$ff81]
    and a
    ret z

    ld a, $02
    ldh [$ffe1], a
    ret


    ldh a, [$ffa6]
    and a
    ret nz

    ld a, $04
    ld [$dfe8], a
    ldh a, [$ffc5]
    and a
    jr z, jr_000_1f92

    ld a, $3f
    ldh [$ffa6], a
    ld a, $1b
    ldh [$ffcc], a
    jr jr_000_1fc9

jr_000_1f92:
    ld a, $2f
    call Call_000_2032
    ld hl, $c843
    ld de, $2992
    ld c, $07
    call Call_000_1fd8
    ld hl, $c983
    ld de, $29ca
    ld c, $06
    call Call_000_1fd8
    ldh a, [$ffc0]
    cp $37
    jr nz, jr_000_1fc7

    ld hl, $c0a2
    ld a, [hl]
    ld b, $58
    cp $15
    jr nc, jr_000_1fcc

    inc b
    cp $10
    jr nc, jr_000_1fcc

    inc b
    cp $05
    jr nc, jr_000_1fcc

jr_000_1fc7:
    ld a, $04

jr_000_1fc9:
    ldh [$ffe1], a
    ret


jr_000_1fcc:
    ld a, b
    ldh [$fff3], a
    ld a, $90
    ldh [$ffa6], a
    ld a, $34
    ldh [$ffe1], a
    ret


Call_000_1fd8:
jr_000_1fd8:
    ld b, $08
    push hl

jr_000_1fdb:
    ld a, [de]
    ld [hl+], a
    inc de
    dec b
    jr nz, jr_000_1fdb

    pop hl
    push de
    ld de, $0020
    add hl, de
    pop de
    dec c
    jr nz, jr_000_1fd8

    ret


Call_000_1fec:
    ldh a, [$ffc0]
    cp $37
    ret nz

    ldh a, [$ffe1]
    and a
    ret nz

    ldh a, [$ffe3]
    cp $05
    ret nz

    ld hl, $c0ac
    ld bc, $0005
    ld a, [hl]
    ld de, $0040
    and a
    jr nz, jr_000_201e

    add hl, bc
    ld a, [hl]
    ld de, $0100
    and a
    jr nz, jr_000_201e

    add hl, bc
    ld a, [hl]
    ld de, $0300
    and a
    jr nz, jr_000_201e

    add hl, bc
    ld de, $1200
    ld a, [hl]
    and a
    ret z

jr_000_201e:
    ld [hl], $00
    ldh a, [$ffa9]
    ld b, a
    inc b

jr_000_2024:
    push bc
    push de
    ld hl, $c0a0
    call Call_000_0166
    pop de
    pop bc
    dec b
    jr nz, jr_000_2024

    ret


Call_000_2032:
    push af
    ld a, $02
    ldh [$ffe3], a
    pop af

Call_000_2038:
    ld hl, $c802
    ld c, $12
    ld de, $0020

jr_000_2040:
    push hl
    ld b, $0a

jr_000_2043:
    ld [hl+], a
    dec b
    jr nz, jr_000_2043

    pop hl
    add hl, de
    dec c
    jr nz, jr_000_2040

    ret


Call_000_204d:
    ld hl, $cbc2
    ld de, $0016
    ld c, $02
    ld a, $2f

jr_000_2057:
    ld b, $0a

jr_000_2059:
    ld [hl+], a
    dec b
    jr nz, jr_000_2059

    add hl, de
    dec c
    jr nz, jr_000_2057

    ret


Call_000_2062:
    ld hl, $c200
    ld [hl], $00
    inc l
    ld [hl], $18
    inc l
    ld [hl], $3f
    inc l
    ld a, [$c213]
    ld [hl], a
    and $fc
    ld c, a
    ldh a, [$ffe4]
    and a
    jr nz, jr_000_207f

    ldh a, [$ffc5]
    and a
    jr z, jr_000_209c

jr_000_207f:
    ld h, $c3
    ldh a, [$ffb0]
    ld l, a
    ld e, [hl]
    inc hl
    ld a, h
    cp $c4
    jr nz, jr_000_208e

    ld hl, $c300

jr_000_208e:
    ld a, l
    ldh [$ffb0], a
    ldh a, [$ffd3]
    and a
    jr z, jr_000_20c0

    or $80
    ldh [$ffd3], a
    jr jr_000_20c0

jr_000_209c:
    ld h, $03

jr_000_209e:
    ldh a, [rDIV]
    ld b, a

jr_000_20a1:
    xor a

jr_000_20a2:
    dec b
    jr z, jr_000_20af

    inc a
    inc a
    inc a
    inc a
    cp $1c
    jr z, jr_000_20a1

    jr jr_000_20a2

jr_000_20af:
    ld d, a
    ldh a, [$ffae]
    ld e, a
    dec h
    jr z, jr_000_20bd

    or d
    or c
    and $fc
    cp c
    jr z, jr_000_209e

jr_000_20bd:
    ld a, d
    ldh [$ffae], a

jr_000_20c0:
    ld a, e
    ld [$c213], a
    call Call_000_26ea
    ldh a, [$ff9a]
    ldh [$ff99], a
    ret


jr_000_20cc:
    ld a, [$c0c7]
    and a
    jr z, jr_000_20de

    ldh a, [$ff81]
    and $b0
    cp $80
    jr nz, jr_000_20ff

    xor a
    ld [$c0c7], a

jr_000_20de:
    ldh a, [$ffa7]
    and a
    jr nz, jr_000_210c

    ldh a, [$ff98]
    and a
    jr nz, jr_000_210c

    ldh a, [$ffe3]
    and a
    jr nz, jr_000_210c

    ld a, $03
    ldh [$ffa7], a
    ld hl, $ffe5
    inc [hl]
    jr jr_000_211d

Call_000_20f7:
    ldh a, [$ff80]
    and $b0
    cp $80
    jr z, jr_000_20cc

jr_000_20ff:
    ld hl, $ffe5
    ld [hl], $00
    ldh a, [$ff99]
    and a
    jr z, jr_000_2110

    dec a
    ldh [$ff99], a

jr_000_210c:
    call Call_000_26d7
    ret


jr_000_2110:
    ldh a, [$ff98]
    cp $03
    ret z

    ldh a, [$ffe3]
    and a
    ret nz

    ldh a, [$ff9a]
    ldh [$ff99], a

jr_000_211d:
    ld hl, $c201
    ld a, [hl]
    ldh [$ffa0], a
    add $08
    ld [hl], a
    call Call_000_26d7
    call Call_000_25c7
    and a
    ret z

    ldh a, [$ffa0]
    ld hl, $c201
    ld [hl], a
    call Call_000_26d7
    ld a, $01
    ldh [$ff98], a
    ld [$c0c7], a
    ldh a, [$ffe5]
    and a
    jr z, jr_000_215e

    ld c, a
    ldh a, [$ffc0]
    cp $37
    jr z, jr_000_2181

    ld de, $c0c0
    ld a, [de]
    ld l, a
    inc de
    ld a, [de]
    ld h, a
    ld b, $00
    dec c
    add hl, bc
    ld a, h
    ld [de], a
    ld a, l
    dec de
    ld [de], a

jr_000_215b:
    xor a
    ldh [$ffe5], a

jr_000_215e:
    ld a, [$c201]
    cp $18
    ret nz

    ld a, [$c202]
    cp $3f
    ret nz

    ld hl, $fffb
    ld a, [hl]
    cp $01
    jr nz, jr_000_217f

    call $7ff3
    ld a, $01
    ldh [$ffe1], a
    ld a, $02
    ld [$dff0], a
    ret


jr_000_217f:
    inc [hl]
    ret


jr_000_2181:
    xor a

jr_000_2182:
    dec c
    jr z, jr_000_2189

    inc a
    daa
    jr jr_000_2182

jr_000_2189:
    ld e, a
    ld d, $00
    ld hl, $c0a0
    call Call_000_0166
    ld a, $01
    ld [$c0ce], a
    jr jr_000_215b

Call_000_2199:
    ldh a, [$ff98]
    cp $02
    ret nz

    ld a, $02
    ld [$dff8], a
    xor a
    ldh [$ffa0], a
    ld de, $c0a3
    ld hl, $c842
    ld b, $10

jr_000_21ae:
    ld c, $0a
    push hl

jr_000_21b1:
    ld a, [hl+]
    cp $2f
    jp z, Jump_000_2238

    dec c
    jr nz, jr_000_21b1

    pop hl
    ld a, h
    ld [de], a
    inc de
    ld a, l
    ld [de], a
    inc de
    ldh a, [$ffa0]
    inc a
    ldh [$ffa0], a

jr_000_21c6:
    push de
    ld de, $0020
    add hl, de
    pop de
    dec b
    jr nz, jr_000_21ae

    ld a, $03
    ldh [$ff98], a
    dec a
    ldh [$ffa6], a
    ldh a, [$ffa0]
    and a
    ret z

    ld b, a
    ld hl, $ff9e
    ldh a, [$ffc0]
    cp $77
    jr z, jr_000_21fb

    ldh a, [$ffe7]
    add b
    ldh [$ffe7], a
    ld a, b
    add [hl]
    daa
    ld [hl+], a
    ld a, $00
    adc [hl]
    daa
    ld [hl], a
    jr nc, jr_000_220a

    ld [hl], $99
    dec hl
    ld [hl], $99
    jr jr_000_220a

jr_000_21fb:
    ld a, [hl]
    or a
    sub b
    jr z, jr_000_223b

    jr c, jr_000_223b

    daa
    ld [hl], a
    and $f0
    cp $90
    jr z, jr_000_223b

jr_000_220a:
    ld a, b
    ld c, $06
    ld hl, $c0ac
    ld b, $00
    cp $01
    jr z, jr_000_222f

    ld hl, $c0b1
    ld b, $01
    cp $02
    jr z, jr_000_222f

    ld hl, $c0b6
    ld b, $02
    cp $03
    jr z, jr_000_222f

    ld hl, $c0bb
    ld b, $04
    ld c, $07

jr_000_222f:
    inc [hl]
    ld a, b
    ldh [$ffdc], a
    ld a, c
    ld [$dfe0], a
    ret


Jump_000_2238:
    pop hl
    jr jr_000_21c6

jr_000_223b:
    xor a
    ldh [$ff9e], a
    jr jr_000_220a

Call_000_2240:
    ldh a, [$ff98]
    cp $03
    ret nz

    ldh a, [$ffa6]
    and a
    ret nz

    ld de, $c0a3
    ldh a, [$ff9c]
    bit 0, a
    jr nz, jr_000_228e

    ld a, [de]
    and a
    jr z, jr_000_22a8

jr_000_2256:
    sub $30
    ld h, a
    inc de
    ld a, [de]
    ld l, a
    ldh a, [$ff9c]
    cp $06
    ld a, $8c
    jr nz, jr_000_2266

    ld a, $2f

jr_000_2266:
    ld c, $0a

jr_000_2268:
    ld [hl+], a
    dec c
    jr nz, jr_000_2268

    inc de
    ld a, [de]
    and a
    jr nz, jr_000_2256

jr_000_2271:
    ldh a, [$ff9c]
    inc a
    ldh [$ff9c], a
    cp $07
    jr z, jr_000_227f

    ld a, $0a
    ldh [$ffa6], a
    ret


jr_000_227f:
    xor a
    ldh [$ff9c], a
    ld a, $0d
    ldh [$ffa6], a
    ld a, $01
    ldh [$ffe3], a

jr_000_228a:
    xor a
    ldh [$ff98], a
    ret


jr_000_228e:
    ld a, [de]
    ld h, a
    sub $30
    ld c, a
    inc de
    ld a, [de]
    ld l, a
    ld b, $0a

jr_000_2298:
    ld a, [hl]
    push hl
    ld h, c
    ld [hl], a
    pop hl
    inc hl
    dec b
    jr nz, jr_000_2298

    inc de
    ld a, [de]
    and a
    jr nz, jr_000_228e

    jr jr_000_2271

jr_000_22a8:
    call Call_000_2062
    jr jr_000_228a

Call_000_22ad:
    ldh a, [$ffa6]
    and a
    ret nz

    ldh a, [$ffe3]
    cp $01
    ret nz

    ld de, $c0a3
    ld a, [de]

jr_000_22ba:
    ld h, a
    inc de
    ld a, [de]
    ld l, a
    push de
    push hl
    ld bc, $ffe0
    add hl, bc
    pop de

jr_000_22c5:
    push hl
    ld b, $0a

jr_000_22c8:
    ld a, [hl+]
    ld [de], a
    inc de
    dec b
    jr nz, jr_000_22c8

    pop hl
    push hl
    pop de
    ld bc, $ffe0
    add hl, bc
    ld a, h
    cp $c7
    jr nz, jr_000_22c5

    pop de
    inc de
    ld a, [de]
    and a
    jr nz, jr_000_22ba

    ld hl, $c802
    ld a, $2f
    ld b, $0a

jr_000_22e7:
    ld [hl+], a
    dec b
    jr nz, jr_000_22e7

    call Call_000_22f3
    ld a, $02
    ldh [$ffe3], a
    ret


Call_000_22f3:
    ld hl, $c0a3
    xor a
    ld b, $09

jr_000_22f9:
    ld [hl+], a
    dec b
    jr nz, jr_000_22f9

    ret


Call_000_22fe:
    ldh a, [$ffe3]
    cp $02
    ret nz

    ld hl, $9a22
    ld de, $ca22
    call Call_000_2506
    ret


Call_000_230d:
    ldh a, [$ffe3]
    cp $03
    ret nz

    ld hl, $9a02
    ld de, $ca02
    call Call_000_2506
    ret


Call_000_231c:
    ldh a, [$ffe3]
    cp $04
    ret nz

    ld hl, $99e2
    ld de, $c9e2
    call Call_000_2506
    ret


Call_000_232b:
    ldh a, [$ffe3]
    cp $05
    ret nz

    ld hl, $99c2
    ld de, $c9c2
    call Call_000_2506
    ret


Call_000_233a:
    ldh a, [$ffe3]
    cp $06
    ret nz

    ld hl, $99a2
    ld de, $c9a2
    call Call_000_2506
    ret


Call_000_2349:
    ldh a, [$ffe3]
    cp $07
    ret nz

    ld hl, $9982
    ld de, $c982
    call Call_000_2506
    ret


Call_000_2358:
    ldh a, [$ffe3]
    cp $08
    ret nz

    ld hl, $9962
    ld de, $c962
    call Call_000_2506
    ldh a, [$ffc5]
    and a
    ldh a, [$ffe1]
    jr nz, jr_000_2375

    and a
    ret nz

jr_000_236f:
    ld a, $01
    ld [$dff8], a
    ret


jr_000_2375:
    cp $1a
    ret nz

    ldh a, [$ffd4]
    and a
    jr z, jr_000_236f

    ld a, $05
    ld [$dfe0], a
    ret


Call_000_2383:
    ldh a, [$ffe3]
    cp $09
    ret nz

    ld hl, $9942
    ld de, $c942
    call Call_000_2506
    ret


Call_000_2392:
    ldh a, [$ffe3]
    cp $0a
    ret nz

    ld hl, $9922
    ld de, $c922
    call Call_000_2506
    ret


Call_000_23a1:
    ldh a, [$ffe3]
    cp $0b
    ret nz

    ld hl, $9902
    ld de, $c902
    call Call_000_2506
    ret


Call_000_23b0:
    ldh a, [$ffe3]
    cp $0c
    ret nz

    ld hl, $98e2
    ld de, $c8e2
    call Call_000_2506
    ret


Call_000_23bf:
    ldh a, [$ffe3]
    cp $0d
    ret nz

    ld hl, $98c2
    ld de, $c8c2
    call Call_000_2506
    ret


Call_000_23ce:
    ldh a, [$ffe3]
    cp $0e
    ret nz

    ld hl, $98a2
    ld de, $c8a2
    call Call_000_2506
    ret


Call_000_23dd:
    ldh a, [$ffe3]
    cp $0f
    ret nz

    ld hl, $9882
    ld de, $c882
    call Call_000_2506
    ret


Call_000_23ec:
    ldh a, [$ffe3]
    cp $10
    ret nz

    ld hl, $9862
    ld de, $c862
    call Call_000_2506
    call Call_000_24ab
    ret


Call_000_23fe:
    ldh a, [$ffe3]
    cp $11
    ret nz

    ld hl, $9842
    ld de, $c842
    call Call_000_2506
    ld hl, $9c6d
    call Call_000_249b
    ld a, $01
    ldh [$ffe0], a
    ret


Call_000_2417:
    ldh a, [$ffe3]
    cp $12
    ret nz

    ld hl, $9822
    ld de, $c822
    call Call_000_2506
    ld hl, $986d
    call Call_000_249b
    ret


Call_000_242c:
    ldh a, [$ffe3]
    cp $13
    ret nz

    ld [$c0c7], a
    ld hl, $9802
    ld de, $c802
    call Call_000_2506
    xor a
    ldh [$ffe3], a
    ldh a, [$ffc5]
    and a
    ldh a, [$ffe1]
    jr nz, jr_000_248f

    and a
    ret nz

jr_000_2449:
    ld hl, $994e
    ld de, $ff9f
    ld c, $02
    ldh a, [$ffc0]
    cp $37
    jr z, jr_000_245f

    ld hl, $9950
    ld de, $ff9e
    ld c, $01

jr_000_245f:
    call Call_000_2a84
    ldh a, [$ffc0]
    cp $37
    jr z, jr_000_248b

    ldh a, [$ff9e]
    and a
    jr nz, jr_000_248b

    ld a, $64
    ldh [$ffa6], a
    ld a, $02
    ld [$dfe8], a
    ldh a, [$ffc5]
    and a
    jr z, jr_000_247e

    ldh [$ffd5], a
    ret


jr_000_247e:
    ldh a, [$ffc3]
    cp $09
    ld a, $05
    jr nz, jr_000_2488

    ld a, $22

jr_000_2488:
    ldh [$ffe1], a
    ret


jr_000_248b:
    call Call_000_2062
    ret


jr_000_248f:
    cp $1a
    ret nz

    ldh a, [$ffd4]
    and a
    jr z, jr_000_2449

    xor a
    ldh [$ffd4], a
    ret


Call_000_249b:
    ldh a, [$ffe1]
    and a
    ret nz

    ldh a, [$ffc0]
    cp $37
    ret nz

    ld de, $c0a2
    call Call_000_2a7e
    ret


Call_000_24ab:
    ldh a, [$ffe1]
    and a
    ret nz

    ldh a, [$ffc0]
    cp $37
    ret nz

    ld hl, $ffa9
    ld a, [hl]
    cp $09
    jr nc, jr_000_24fd

    ldh a, [$ffe7]
    cp $0a
    ret c

    sub $0a

jr_000_24c3:
    ldh [$ffe7], a
    inc [hl]
    ld a, [hl]
    cp $15
    jr nz, jr_000_24cd

    ld [hl], $14

jr_000_24cd:
    ld b, [hl]
    xor a

jr_000_24cf:
    or a
    inc a
    daa
    dec b
    jr z, jr_000_24d7

    jr jr_000_24cf

jr_000_24d7:
    ld b, a
    and $0f
    ld c, a
    ld hl, $98f1

jr_000_24de:
    ld [hl], c
    ld h, $9c
    ld [hl], c
    ld a, b
    and $f0
    jr z, jr_000_24f4

    swap a
    ld c, a
    ld a, l
    cp $f0
    jr z, jr_000_24f4

    ld hl, $98f0
    jr jr_000_24de

jr_000_24f4:
    ld a, $02
    ld [$dfe0], a
    call Call_000_1b43
    ret


jr_000_24fd:
    ldh a, [$ffe7]
    cp $14
    ret c

    sub $14
    jr jr_000_24c3

Call_000_2506:
    ld b, $0a

jr_000_2508:
    ld a, [de]
    ld [hl], a
    inc l
    inc e
    dec b
    jr nz, jr_000_2508

    ldh a, [$ffe3]
    inc a
    ldh [$ffe3], a
    ret


Call_000_2515:
    ld hl, $c203
    ld a, [hl]
    ldh [$ffa0], a
    ldh a, [$ff81]
    ld b, a
    bit 1, b
    jr nz, jr_000_2534

    bit 0, b
    jr z, jr_000_255d

    ld a, [hl]
    and $03
    jr z, jr_000_252e

    dec [hl]
    jr jr_000_2542

jr_000_252e:
    ld a, [hl]
    or $03
    ld [hl], a
    jr jr_000_2542

jr_000_2534:
    ld a, [hl]
    and $03
    cp $03
    jr z, jr_000_253e

    inc [hl]
    jr jr_000_2542

jr_000_253e:
    ld a, [hl]
    and $fc
    ld [hl], a

jr_000_2542:
    ld a, $03
    ld [$dfe0], a
    call Call_000_26d7
    call Call_000_25c7
    and a
    jr z, jr_000_255d

    xor a
    ld [$dfe0], a
    ld hl, $c203
    ldh a, [$ffa0]
    ld [hl], a
    call Call_000_26d7

jr_000_255d:
    ld hl, $c202
    ldh a, [$ff81]
    ld b, a
    ldh a, [$ff80]
    ld c, a
    ld a, [hl]
    ldh [$ffa0], a
    bit 4, b
    ld a, $17
    jr nz, jr_000_257b

    bit 4, c
    jr z, jr_000_25a0

    ldh a, [$ffaa]
    dec a
    ldh [$ffaa], a
    ret nz

    ld a, $09

jr_000_257b:
    ldh [$ffaa], a
    ld a, [hl]
    add $08
    ld [hl], a
    call Call_000_26d7
    ld a, $04
    ld [$dfe0], a
    call Call_000_25c7
    and a
    ret z

jr_000_258e:
    ld hl, $c202
    xor a
    ld [$dfe0], a
    ldh a, [$ffa0]
    ld [hl], a
    call Call_000_26d7
    ld a, $01

jr_000_259d:
    ldh [$ffaa], a
    ret


jr_000_25a0:
    bit 5, b
    ld a, $17
    jr nz, jr_000_25b2

    bit 5, c
    jr z, jr_000_259d

    ldh a, [$ffaa]
    dec a
    ldh [$ffaa], a
    ret nz

    ld a, $09

jr_000_25b2:
    ldh [$ffaa], a
    ld a, [hl]
    sub $08
    ld [hl], a
    ld a, $04
    ld [$dfe0], a
    call Call_000_26d7
    call Call_000_25c7
    and a
    ret z

    jr jr_000_258e

Call_000_25c7:
    ld hl, $c010
    ld b, $04

jr_000_25cc:
    ld a, [hl+]
    ldh [$ffb2], a
    ld a, [hl+]
    and a
    jr z, jr_000_25ea

    ldh [$ffb3], a
    push hl
    push bc
    call Call_000_2a2b
    ld a, h
    add $30
    ld h, a
    ld a, [hl]
    cp $2f
    jr nz, jr_000_25ee

    pop bc
    pop hl
    inc l
    inc l
    dec b
    jr nz, jr_000_25cc

jr_000_25ea:
    xor a
    ldh [$ff9b], a
    ret


jr_000_25ee:
    pop bc
    pop hl
    ld a, $01
    ldh [$ff9b], a
    ret


Call_000_25f5:
    ldh a, [$ff98]
    cp $01
    ret nz

    ld hl, $c010
    ld b, $04

jr_000_25ff:
    ld a, [hl+]
    ldh [$ffb2], a
    ld a, [hl+]
    and a
    jr z, jr_000_2623

    ldh [$ffb3], a
    push hl
    push bc
    call Call_000_2a2b
    push hl
    pop de
    pop bc
    pop hl

jr_000_2611:
    ldh a, [rSTAT]
    and $03
    jr nz, jr_000_2611

    ld a, [hl]
    ld [de], a
    ld a, d
    add $30
    ld d, a
    ld a, [hl+]
    ld [de], a
    inc l
    dec b
    jr nz, jr_000_25ff

jr_000_2623:
    ld a, $02
    ldh [$ff98], a
    ld hl, $c200
    ld [hl], $80
    ret


Call_000_262d:
    ld a, [$c0c6]
    cp $02
    jr z, jr_000_267a

    push de
    ld a, [hl]
    or a
    jr z, jr_000_268d

    dec a
    ld [hl+], a
    ld a, [hl]
    inc a
    daa
    ld [hl], a
    and $0f
    ld [bc], a
    dec c
    ld a, [hl+]
    swap a
    and $0f
    jr z, jr_000_264b

    ld [bc], a

jr_000_264b:
    push bc
    ldh a, [$ffc3]
    ld b, a
    inc b

jr_000_2650:
    push hl
    call Call_000_0166
    pop hl
    dec b
    jr nz, jr_000_2650

    pop bc
    inc hl
    inc hl
    push hl
    ld hl, $0023
    add hl, bc
    pop de
    call Call_000_2a82
    pop de
    ldh a, [$ffc3]
    ld b, a
    inc b
    ld hl, $c0a0

jr_000_266c:
    push hl
    call Call_000_0166
    pop hl
    dec b
    jr nz, jr_000_266c

    ld a, $02
    ld [$c0c6], a
    ret


jr_000_267a:
    ld de, $c0a2
    ld hl, $9a25
    call Call_000_2a82
    ld a, $02
    ld [$dfe0], a
    xor a
    ld [$c0c6], a
    ret


jr_000_268d:
    pop de

Jump_000_268e:
    ld a, $21
    ldh [$ffa6], a
    xor a
    ld [$c0c6], a
    ld a, [$c0c5]
    inc a
    ld [$c0c5], a
    cp $05
    ret nz

    ld a, $04
    ldh [$ffe1], a
    ret


Call_000_26a5:
    ld hl, $c0ac
    ld b, $1b
    xor a

jr_000_26ab:
    ld [hl+], a
    dec b
    jr nz, jr_000_26ab

    ld hl, $c0a0
    ld b, $03

jr_000_26b4:
    ld [hl+], a
    dec b
    jr nz, jr_000_26b4

    ret


    ld a, [hl]
    and $f0
    swap a
    ld [de], a
    ld a, [hl]
    and $0f
    inc e
    ld [de], a
    ret


Call_000_26c5:
    ld a, $02

Call_000_26c7:
    ldh [$ff8f], a
    xor a
    ldh [$ff8e], a
    ld a, $c0
    ldh [$ff8d], a
    ld hl, $c200
    call Call_000_2ad1
    ret


Call_000_26d7:
    ld a, $01
    ldh [$ff8f], a
    ld a, $10
    ldh [$ff8e], a
    ld a, $c0
    ldh [$ff8d], a
    ld hl, $c200
    call Call_000_2ad1
    ret


Call_000_26ea:
    ld a, $01
    ldh [$ff8f], a
    ld a, $20
    ldh [$ff8e], a
    ld a, $c0
    ldh [$ff8d], a
    ld hl, $c210
    call Call_000_2ad1
    ret


Call_000_26fd:
    ld b, $20
    ld a, $8e
    ld de, $0020

jr_000_2704:
    ld [hl], a
    add hl, de
    dec b
    jr nz, jr_000_2704

    ret


Call_000_270a:
jr_000_270a:
    ld a, [de]
    cp $ff
    ret z

    ld [hl+], a
    inc de
    jr jr_000_270a

Jump_000_2712:
    reti


    db $00, $18, $3f, $00, $80, $00, $00, $ff, $00, $80, $8f, $00, $80, $00, $00, $ff
    db $00, $70, $37, $1c, $00, $00, $00, $38, $37, $1c, $00, $00, $00, $40, $34, $20
    db $00, $00

    nop
    ld b, b
    inc e
    jr nz, jr_000_273a

jr_000_273a:
    nop
    nop
    ld b, b
    ld [hl], h
    jr nz, jr_000_2740

jr_000_2740:
    nop
    nop
    ld b, b
    ld l, b
    ld hl, $0000
    nop
    ld a, b
    ld l, b
    ld hl, $0000
    nop
    ld h, b
    ld h, b
    ld a, [hl+]
    add b
    nop
    nop
    ld h, b
    ld [hl], d
    ld a, [hl+]
    add b
    jr nz, jr_000_275a

jr_000_275a:
    ld l, b
    jr c, jr_000_279b

    add b
    nop
    nop
    ld h, b
    ld h, b
    ld [hl], $80
    nop
    nop
    ld h, b
    ld [hl], d
    ld [hl], $80
    jr nz, jr_000_276c

jr_000_276c:
    ld l, b
    jr c, jr_000_27a1

    add b
    nop
    nop
    ld h, b
    ld h, b
    ld l, $80
    nop
    nop
    ld l, b
    jr c, jr_000_27b7

    add b
    nop
    nop
    ld h, b
    ld h, b
    ld a, [hl-]
    add b
    nop
    nop
    ld l, b
    jr c, jr_000_27b7

    add b
    nop
    add b
    ccf
    ld b, b
    ld b, h
    nop
    nop
    add b
    ccf
    jr nz, jr_000_27dd

    nop
    nop
    add b
    ccf
    jr nc, jr_000_27df

    nop
    nop

jr_000_279b:
    add b
    ld [hl], a
    jr nz, jr_000_27e7

    nop
    nop

jr_000_27a1:
    add b
    add a
    ld c, b
    ld c, h
    nop
    nop
    add b
    add a
    ld e, b
    ld c, [hl]
    nop
    nop
    add b
    ld h, a
    ld c, l
    ld d, b
    nop
    nop
    add b
    ld h, a
    ld e, l
    ld d, d

jr_000_27b7:
    nop
    nop
    add b
    adc a
    adc b
    ld d, h
    nop
    nop
    add b
    adc a
    sbc b
    ld d, l
    nop
    nop
    nop
    ld e, a
    ld d, a
    inc l
    nop
    nop
    add b
    add b
    ld d, b
    inc [hl]
    nop
    nop
    add b
    add b
    ld h, b
    inc [hl]
    nop
    jr nz, jr_000_27d8

jr_000_27d8:
    ld l, a
    ld d, a
    ld e, b
    nop
    nop

jr_000_27dd:
    add b
    add b

jr_000_27df:
    ld d, l
    inc [hl]
    nop
    nop
    add b
    add b
    ld e, e
    inc [hl]

jr_000_27e7:
    nop
    db $20

ClearTilemap:
    ld hl, $9bff

Call_000_27ec:
    ld bc, $0400

jr_000_27ef:
    ld a, $2f
    ld [hl-], a
    dec bc
    ld a, b
    or c
    jr nz, jr_000_27ef

    ret


LoadTiles:
    ld a, [hl+]
    ld [de], a
    inc de
    dec bc
    ld a, b
    or c
    jr nz, LoadTiles

    ret


LoadGameTiles:
    call LoadFontTiles
    ld bc, $00a0
    call LoadTiles
    ld hl, $3287
    ld de, $8300
    ld bc, $0d00
    call LoadTiles
    ret


LoadFontTiles:
    ld hl, FontTiles
    ld bc, $0138
    ld de, $8000

jr_000_2820:
    ld a, [hl+]
    ld [de], a
    inc de
    ld [de], a
    inc de
    dec bc
    ld a, b
    or c
    jr nz, jr_000_2820

    ret


LoadTitleTiles:
    call LoadFontTiles
    ld bc, $0da0
    call LoadTiles
    ret


    ld bc, $1000

Call_000_2838:
    ld de, $8000
    call LoadTiles
    ret


LoadTilemap:
    ld hl, $9800

Call_000_2842:
    ld b, 18 ; = screen height in tiles

Call_000_2844:
jr_000_2844:
    push hl
    ld c, 20 ; = screen width in tiles

jr_000_2847:
    ld a, [de]
    ld [hl+], a
    inc de
    dec c
    jr nz, jr_000_2847

    pop hl
    push de
    ld de, $0020
    add hl, de
    pop de
    dec b
    jr nz, jr_000_2844

    ret


Call_000_2858:
jr_000_2858:
    ld b, $0a
    push hl

jr_000_285b:
    ld a, [de]
    cp $ff
    jr z, jr_000_286e

    ld [hl+], a
    inc de
    dec b
    jr nz, jr_000_285b

    pop hl
    push de
    ld de, $0020
    add hl, de
    pop de
    jr jr_000_2858

jr_000_286e:
    pop hl
    ld a, $02
    ldh [$ffe3], a
    ret


DisableLCD:
    ldh a, [rIE]
    ldh [$ffa1], a
    res 0, a ; Disable VBlank interrupt
    ldh [rIE], a

jr_000_287c:
    ; Wait for VBlank
    ldh a, [rLY]
    cp $91
    jr nz, jr_000_287c

    ; Turn off LCD
    ldh a, [rLCDC]
    and %01111111
    ldh [rLCDC], a

    ldh a, [$ffa1] ; Re-enable VBlank interrupt
    ldh [rIE], a
    ret


    db $2f, $2f, $11, $12, $1d, $2f, $2f, $2f, $2f, $2f, $29, $29, $29, $2f, $2f, $2f
    db $2f, $1c, $1d, $0a, $1b, $1d, $2f, $2f, $2f, $29, $29, $29, $29, $29, $2f, $2f
    db $2f, $2f, $2f, $1d, $18, $2f, $2f, $2f, $2f, $2f, $2f, $29, $29, $2f, $2f, $2f
    db $0c, $18, $17, $1d, $12, $17, $1e, $0e, $29, $29, $29, $29, $29, $29, $29, $29
    db $2f, $2f, $10, $0a, $16, $0e, $2f, $2f, $2f, $2f, $29, $29, $29, $29, $2f, $2f

    inc e
    ld [de], a
    rla
    db $10
    dec d
    ld c, $2f
    cpl
    cpl
    cpl
    cpl

    nop
    cpl
    ld h, $2f
    inc b
    nop
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    nop
    cpl
    dec c
    jr jr_000_291c

    dec bc
    dec d
    ld c, $2f
    cpl
    cpl
    cpl
    cpl
    nop
    cpl
    ld h, $2f
    ld bc, $0000
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    nop
    cpl
    dec e
    dec de
    ld [de], a

jr_000_291c:
    add hl, de
    dec d
    ld c, $2f
    cpl
    cpl
    cpl
    cpl
    nop
    cpl
    ld h, $2f
    inc bc
    nop
    nop
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    nop
    cpl
    dec e
    ld c, $1d
    dec de
    ld [de], a
    inc e
    cpl
    cpl
    cpl
    cpl
    cpl
    nop
    cpl
    ld h, $2f
    ld bc, $0002
    nop
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    nop
    cpl
    dec c
    dec de
    jr jr_000_2972

    inc e
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    nop
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl

jr_000_2972:
    cpl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    dec e
    ld de, $1c12
    cpl
    inc e
    dec e
    ld a, [bc]
    db $10
    ld c, $2f
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    nop
    cpl
    rst $38

    db $61, $62, $62, $62, $62, $62, $62, $63, $64, $2f, $2f, $2f, $2f, $2f, $2f, $65
    db $64, $2f, $10, $0a, $16, $0e, $2f, $65, $64, $2f, $ad, $ad, $ad, $ad, $2f, $65
    db $64, $2f, $18, $1f, $0e, $1b, $2f, $65, $64, $2f, $ad, $ad, $ad, $ad, $2f, $65
    db $66, $69, $69, $69, $69, $69, $69, $6a, $19, $15, $0e, $0a, $1c, $0e, $2f, $2f
    db $29, $29, $29, $29, $29, $29, $2f, $2f, $2f, $1d, $1b, $22, $2f, $2f, $2f, $2f
    db $2f, $29, $29, $29, $2f, $2f, $2f, $2f, $2f, $2f, $0a, $10, $0a, $12, $17, $27
    db $2f, $2f, $29, $29, $29, $29, $29, $2f

ReadGamepad:
    ; Read the current state of all 8 gamepad buttons into register C
    ; vvvvvvvvvv
    ld a, P1F_GET_DPAD
    ldh [rP1], a
    ldh a, [rP1]
    ldh a, [rP1]
    cpl
    and $0f
    swap a
    ld b, a

    ld a, P1F_GET_BTN
    ldh [rP1], a
    ldh a, [rP1]
    ldh a, [rP1]
    ldh a, [rP1]
    ldh a, [rP1]
    ldh a, [rP1]
    ldh a, [rP1]
    cpl
    and $0f
    or b

    ld c, a
    ; ^^^^^^^^^^

    ; Check which buttons have changed since last check, and store into $FF81
    ldh a, [$ff80] ; $FF80 = previous gamepad state
    xor c
    and c
    ldh [$ff81], a

    ; Record the current gamepad state into $FF80
    ld a, c
    ldh [$ff80], a

    ; Stop reading the gamepad
    ld a, P1F_GET_NONE 
    ldh [rP1], a
    ret


Call_000_2a2b:
    ldh a, [$ffb2]
    sub $10
    srl a
    srl a
    srl a
    ld de, $0000
    ld e, a
    ld hl, $9800
    ld b, $20

jr_000_2a3e:
    add hl, de
    dec b
    jr nz, jr_000_2a3e

    ldh a, [$ffb3]
    sub $08
    srl a
    srl a
    srl a
    ld de, $0000
    ld e, a
    add hl, de
    ld a, h
    ldh [$ffb5], a
    ld a, l
    ldh [$ffb4], a
    ret


    ldh a, [$ffb5]
    ld d, a
    ldh a, [$ffb4]
    ld e, a
    ld b, $04

jr_000_2a60:
    rr d
    rr e
    dec b
    jr nz, jr_000_2a60

    ld a, e
    sub $84
    and $fe
    rlca
    rlca
    add $08
    ldh [$ffb2], a
    ldh a, [$ffb4]
    and $1f
    rla
    rla
    rla
    add $08
    ldh [$ffb3], a
    ret


Call_000_2a7e:
    ldh a, [$ffe0]
    and a
    ret z

Call_000_2a82:
    ld c, $03

Call_000_2a84:
    xor a
    ldh [$ffe0], a

jr_000_2a87:
    ld a, [de]
    ld b, a
    swap a
    and $0f
    jr nz, jr_000_2ab7

    ldh a, [$ffe0]
    and a
    ld a, $00
    jr nz, jr_000_2a98

    ld a, $2f

jr_000_2a98:
    ld [hl+], a
    ld a, b
    and $0f
    jr nz, jr_000_2abf

    ldh a, [$ffe0]
    and a
    ld a, $00
    jr nz, jr_000_2aae

    ld a, $01
    cp c
    ld a, $00
    jr z, jr_000_2aae

    ld a, $2f

jr_000_2aae:
    ld [hl+], a
    dec e
    dec c
    jr nz, jr_000_2a87

    xor a
    ldh [$ffe0], a
    ret


jr_000_2ab7:
    push af
    ld a, $01
    ldh [$ffe0], a
    pop af
    jr jr_000_2a98

jr_000_2abf:
    push af
    ld a, $01
    ldh [$ffe0], a
    pop af
    jr jr_000_2aae

DMAProcedure:
     ld a, $c0
     ldh [rDMA], a
     ld a, 40
.wait
     dec a
     jr nz, .wait

     ret

Call_000_2ad1:
jr_000_2ad1:
    ld a, h
    ldh [$ff96], a
    ld a, l
    ldh [$ff97], a
    ld a, [hl]
    and a
    jr z, jr_000_2af8

    cp $80
    jr z, jr_000_2af6

jr_000_2adf:
    ldh a, [$ff96]
    ld h, a
    ldh a, [$ff97]
    ld l, a
    ld de, $0010
    add hl, de
    ldh a, [$ff8f]
    dec a
    ldh [$ff8f], a
    ret z

    jr jr_000_2ad1

jr_000_2af1:
    xor a
    ldh [$ff95], a
    jr jr_000_2adf

jr_000_2af6:
    ldh [$ff95], a

jr_000_2af8:
    ld b, $07
    ld de, $ff86

jr_000_2afd:
    ld a, [hl+]
    ld [de], a
    inc de
    dec b
    jr nz, jr_000_2afd

    ldh a, [$ff89]
    ld hl, $2bac
    rlca
    ld e, a
    ld d, $00
    add hl, de
    ld e, [hl]
    inc hl
    ld d, [hl]
    ld a, [de]
    ld l, a
    inc de
    ld a, [de]
    ld h, a
    inc de
    ld a, [de]
    ldh [$ff90], a
    inc de
    ld a, [de]
    ldh [$ff91], a
    ld e, [hl]
    inc hl
    ld d, [hl]

Jump_000_2b20:
jr_000_2b20:
    inc hl
    ldh a, [$ff8c]
    ldh [$ff94], a
    ld a, [hl]
    cp $ff
    jr z, jr_000_2af1

    cp $fd
    jr nz, jr_000_2b3c

    ldh a, [$ff8c]
    xor $20
    ldh [$ff94], a
    inc hl
    ld a, [hl]
    jr jr_000_2b40

jr_000_2b38:
    inc de
    inc de
    jr jr_000_2b20

jr_000_2b3c:
    cp $fe
    jr z, jr_000_2b38

jr_000_2b40:
    ldh [$ff89], a
    ldh a, [$ff87]
    ld b, a
    ld a, [de]
    ld c, a
    ldh a, [$ff8b]
    bit 6, a
    jr nz, jr_000_2b53

    ldh a, [$ff90]
    add b
    adc c
    jr jr_000_2b5d

jr_000_2b53:
    ld a, b
    push af
    ldh a, [$ff90]
    ld b, a
    pop af
    sub b
    sbc c
    sbc $08

jr_000_2b5d:
    ldh [$ff93], a
    ldh a, [$ff88]
    ld b, a
    inc de
    ld a, [de]
    inc de
    ld c, a
    ldh a, [$ff8b]
    bit 5, a
    jr nz, jr_000_2b72

    ldh a, [$ff91]
    add b
    adc c
    jr jr_000_2b7c

jr_000_2b72:
    ld a, b
    push af
    ldh a, [$ff91]
    ld b, a
    pop af
    sub b
    sbc c
    sbc $08

jr_000_2b7c:
    ldh [$ff92], a
    push hl
    ldh a, [$ff8d]
    ld h, a
    ldh a, [$ff8e]
    ld l, a
    ldh a, [$ff95]
    and a
    jr z, jr_000_2b8e

    ld a, $ff
    jr jr_000_2b90

jr_000_2b8e:
    ldh a, [$ff93]

jr_000_2b90:
    ld [hl+], a
    ldh a, [$ff92]
    ld [hl+], a
    ldh a, [$ff89]
    ld [hl+], a
    ldh a, [$ff94]
    ld b, a
    ldh a, [$ff8b]
    or b
    ld b, a
    ldh a, [$ff8a]
    or b
    ld [hl+], a
    ld a, h
    ldh [$ff8d], a
    ld a, l
    ldh [$ff8e], a
    pop hl
    jp Jump_000_2b20


    db $68, $2c, $6c, $2c, $70, $2c, $74, $2c, $78, $2c, $7c, $2c, $80, $2c, $84, $2c
    db $88, $2c

    adc h
    inc l
    sub b
    inc l

    db $94, $2c, $98, $2c

    sbc h
    inc l
    and b
    inc l
    and h
    inc l

    db $a8, $2c

    xor h
    inc l
    or b
    inc l

    db $b4, $2c, $b8, $2c

    cp h
    inc l
    ret nz

    inc l
    db $c4
    inc l

    db $c8, $2c, $cc, $2c, $d0, $2c, $d4, $2c, $d8, $2c

    call c, $e02c
    inc l
    db $e4
    inc l

    db $e8, $2c

    db $ec
    inc l
    ldh a, [$ff2c]
    db $f4
    inc l
    ld hl, sp+$2c
    db $fc
    inc l
    nop
    dec l
    inc b
    dec l
    ld [$0c2d], sp
    dec l
    db $10
    dec l
    inc d
    dec l
    rrca
    ld sp, $2d14
    jr jr_000_2c37

    inc e
    dec l
    jr nz, jr_000_2c3b

    inc h
    dec l
    jr z, jr_000_2c3f

    inc l
    dec l
    ld [hl-], a
    ld sp, $3136
    jr nc, jr_000_2c47

    inc [hl]
    dec l
    ld a, [hl-]
    ld sp, $313e
    jr c, jr_000_2c4f

    inc a
    dec l
    ld b, b
    dec l
    ld b, h
    dec l
    ld c, b
    dec l
    ld c, h
    dec l
    ld b, d
    ld sp, $3146
    ld c, h
    dec l
    ld d, b
    dec l
    ld d, b
    dec l
    ld d, h

jr_000_2c37:
    dec l
    ld e, b
    dec l
    ld e, h

jr_000_2c3b:
    dec l
    ld h, b
    dec l
    ld h, h

jr_000_2c3f:
    dec l
    ld l, b
    dec l
    ld l, h
    dec l
    ld [hl], b
    dec l
    ld [hl], h

jr_000_2c47:
    dec l
    ld a, b
    dec l
    ld a, h
    dec l
    add b
    dec l
    add h

jr_000_2c4f:
    dec l
    adc b
    dec l
    adc h
    dec l
    sub b
    dec l
    sub h
    dec l
    sbc b
    dec l
    sbc h
    dec l
    ld d, d
    ld sp, $3156
    ld e, d
    ld sp, $315a
    ld c, d
    ld sp, $314e

    db $a0, $2d, $ef, $f0, $b0, $2d, $ef, $f0, $c2, $2d, $ef, $f0, $d1, $2d, $ef, $f0
    db $e2, $2d, $ef, $f0, $f4, $2d, $ef, $f0, $05, $2e, $ef, $f0, $13, $2e, $ef, $f0
    db $24, $2e, $ef, $f0

    inc sp
    ld l, $ef
    ldh a, [rLY]
    ld l, $ef
    db $f0

    db $53, $2e, $ef, $f0, $64, $2e, $ef, $f0

    db $76
    ld l, $ef
    ldh a, [$ff88]
    ld l, $ef
    ldh a, [$ff9a]
    ld l, $ef
    db $f0

    db $ac, $2e, $ef, $f0

    cp [hl]
    ld l, $ef
    ldh a, [$ffce]
    ld l, $ef
    db $f0

    db $e0, $2e, $ef, $f0, $f0, $2e, $ef, $f0

    ld bc, $ef2f
    ldh a, [rNR12]
    cpl
    rst $28
    ldh a, [rNR44]
    cpl
    rst $28
    db $f0

    db $53, $2f, $ef, $f0, $64, $2f, $ef, $f0, $34, $2f, $ef, $f0, $42, $2f, $ef, $f0
    db $75, $2f, $00, $e8

    ld a, [hl]
    cpl
    nop
    add sp, -$79
    cpl
    nop
    add sp, -$70
    cpl
    nop
    db $e8

    db $99, $2f, $00, $00

    sbc l
    cpl
    nop
    nop
    and c
    cpl
    nop
    nop
    and l
    cpl
    nop
    nop
    xor c
    cpl
    nop
    nop
    xor l
    cpl
    nop
    nop
    or c
    cpl
    nop
    nop
    or l
    cpl
    nop
    nop
    cp c
    cpl
    nop
    nop
    cp l
    cpl
    nop
    nop
    pop bc
    cpl
    ldh a, [$fff8]
    call z, $f02f
    ld hl, sp-$29
    cpl
    ldh a, [$fff0]
    db $eb
    cpl
    ldh a, [$fff0]
    nop
    jr nc, @-$06

jr_000_2d23:
    ld hl, sp+$09
    jr nc, @-$06

jr_000_2d27:
    ld hl, sp+$12
    jr nc, jr_000_2d23

jr_000_2d2b:
    ld hl, sp+$19
    jr nc, jr_000_2d27

jr_000_2d2f:
    ld hl, sp+$20
    jr nc, jr_000_2d23

    ld hl, sp+$2b
    jr nc, jr_000_2d27

    ld hl, sp+$36
    jr nc, jr_000_2d2b

jr_000_2d3b:
    ldh a, [rWX]
    jr nc, jr_000_2d2f

jr_000_2d3f:
    ldh a, [$ff61]
    jr nc, jr_000_2d3b

jr_000_2d43:
    ld hl, sp+$6a
    jr nc, jr_000_2d3f

jr_000_2d47:
    ld hl, sp+$73
    jr nc, jr_000_2d43

jr_000_2d4b:
    ld hl, sp+$7a
    jr nc, jr_000_2d47

jr_000_2d4f:
    ld hl, sp-$7f
    jr nc, jr_000_2d4b

jr_000_2d53:
    ld hl, sp-$78
    jr nc, jr_000_2d4f

jr_000_2d57:
    ld hl, sp-$71
    jr nc, jr_000_2d53

jr_000_2d5b:
    ld hl, sp-$6a
    jr nc, jr_000_2d57

jr_000_2d5f:
    ld hl, sp-$63
    jr nc, jr_000_2d5b

jr_000_2d63:
    ld hl, sp-$5c
    jr nc, jr_000_2d5f

jr_000_2d67:
    ld hl, sp-$51
    jr nc, jr_000_2d63

jr_000_2d6b:
    ld hl, sp-$4a
    jr nc, jr_000_2d67

jr_000_2d6f:
    ld hl, sp-$43
    jr nc, jr_000_2d6b

jr_000_2d73:
    ld hl, sp-$3c
    jr nc, jr_000_2d6f

jr_000_2d77:
    ld hl, sp-$35
    jr nc, jr_000_2d73

jr_000_2d7b:
    ld hl, sp-$2c
    jr nc, jr_000_2d77

jr_000_2d7f:
    ld hl, sp-$23
    jr nc, jr_000_2d7b

jr_000_2d83:
    ld hl, sp-$1a
    jr nc, jr_000_2d7f

jr_000_2d87:
    ld hl, sp-$11
    jr nc, jr_000_2d83

    ld hl, sp-$08
    jr nc, jr_000_2d87

    ld hl, sp+$01
    ld sp, $f8f8
    ld [$f831], sp
    ld hl, sp-$72
    ld sp, $f0f0
    and l
    ld sp, $f8f8

    db $f1, $31, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $84, $84, $84, $fe, $84, $ff
    db $f1, $31, $fe, $fe, $fe, $fe, $fe, $84, $fe, $fe, $fe, $84, $fe, $fe, $fe, $84
    db $84, $ff, $f1, $31, $fe, $fe, $fe, $fe, $fe, $fe, $84, $fe, $84, $84, $84, $fe
    db $ff, $f1, $31, $fe, $fe, $fe, $fe, $84, $84, $fe, $fe, $fe, $84, $fe, $fe, $fe
    db $84, $ff, $f1, $31, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $81, $81, $81, $fe
    db $fe, $fe, $81, $ff, $f1, $31, $fe, $fe, $fe, $fe, $fe, $81, $81, $fe, $fe, $81
    db $fe, $fe, $fe, $81, $ff, $f1, $31, $fe, $fe, $fe, $fe, $81, $fe, $fe, $fe, $81
    db $81, $81, $ff, $f1, $31, $fe, $fe, $fe, $fe, $fe, $81, $fe, $fe, $fe, $81, $fe
    db $fe, $81, $81, $ff, $f1, $31, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $8a, $8b
    db $8b, $8f, $ff

    pop af
    ld sp, $80fe
    cp $fe
    cp $88
    cp $fe
    cp $88
    cp $fe
    cp $89
    rst $38
    pop af
    ld sp, $fefe
    cp $fe
    cp $fe
    cp $fe
    adc d
    adc e
    adc e
    adc a
    rst $38

    db $f1, $31, $fe, $80, $fe, $fe, $fe, $88, $fe, $fe, $fe, $88, $fe, $fe, $fe, $89
    db $ff, $f1, $31, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $83, $83, $fe, $fe
    db $83, $83, $ff

    pop af
    ld sp, $fefe
    cp $fe
    cp $fe
    cp $fe
    cp $83
    add e
    cp $fe
    add e
    add e
    rst $38
    pop af
    ld sp, $fefe
    cp $fe
    cp $fe
    cp $fe
    cp $83
    add e
    cp $fe
    add e
    add e
    rst $38
    pop af
    ld sp, $fefe
    cp $fe
    cp $fe
    cp $fe
    cp $83
    add e
    cp $fe
    add e
    add e
    rst $38

    db $f1, $31, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $82, $82, $fe, $fe, $fe, $82
    db $82, $ff

    pop af
    ld sp, $fefe
    cp $fe
    cp $82
    cp $fe
    add d
    add d
    cp $fe
    add d
    rst $38
    pop af
    ld sp, $fefe
    cp $fe
    cp $fe
    cp $fe
    add d
    add d
    cp $fe
    cp $82
    add d
    rst $38

    db $f1, $31, $fe, $fe, $fe, $fe, $fe, $82, $fe, $fe, $82, $82, $fe, $fe, $82, $ff
    db $f1, $31, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $86, $86, $fe, $86, $86
    db $ff

    pop af
    ld sp, $fefe
    cp $fe
    add [hl]
    cp $fe
    cp $86
    add [hl]
    cp $fe
    cp $86
    rst $38
    pop af
    ld sp, $fefe
    cp $fe
    cp $fe
    cp $fe
    cp $86
    add [hl]
    cp $86
    add [hl]
    rst $38
    pop af
    ld sp, $fefe
    cp $fe
    add [hl]
    cp $fe
    cp $86
    add [hl]

Jump_000_2f2f:
    cp $fe
    cp $86
    rst $38

    db $f1, $31, $fe, $fe, $fe, $fe, $fe, $85, $fe, $fe, $85, $85, $85, $ff, $f1, $31
    db $fe, $fe, $fe, $fe, $fe, $85, $fe, $fe, $85, $85, $fe, $fe, $fe, $85, $ff, $f1
    db $31, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $fe, $85, $85, $85, $fe, $fe, $85, $ff
    db $f1, $31, $fe, $fe, $fe, $fe, $fe, $85, $fe, $fe, $fe, $85, $85, $fe, $fe, $85
    db $ff, $11, $32, $0a, $25, $1d, $22, $19, $0e, $ff

    ld de, $0b32
    dec h
    dec e
    ld [hl+], a
    add hl, de
    ld c, $ff
    ld de, $0c32
    dec h
    dec e
    ld [hl+], a
    add hl, de
    ld c, $ff
    ld de, $2f32
    jr jr_000_2fa4

    rrca
    cpl
    cpl
    rst $38

    db $11, $32, $00, $ff

    ld de, $0132
    rst $38
    ld de, $0232

jr_000_2fa4:
    rst $38
    ld de, $0332
    rst $38
    ld de, $0432
    rst $38
    ld de, $0532
    rst $38
    ld de, $0632
    rst $38
    ld de, $0732
    rst $38
    ld de, $0832
    rst $38
    ld de, $0932
    rst $38
    ld hl, $2f32
    ld bc, $112f
    jr nz, jr_000_2fea

    jr nc, jr_000_2ffc

    rst $38
    ld hl, $2f32
    inc bc
    ld [de], a
    inc de
    ld [hl+], a
    inc hl
    ld [hl-], a
    inc sp
    rst $38
    pop af
    ld sp, $052f
    db $fd
    dec b
    cpl
    cpl
    dec d
    inc b
    rla
    inc h
    dec h
    ld h, $27
    inc [hl]
    dec [hl]
    ld [hl], $2f

jr_000_2fea:
    rst $38
    pop af
    ld sp, $3708
    db $fd
    scf
    db $fd
    ld [$1918], sp
    inc d
    dec de
    jr z, @+$2b

    ld a, [hl+]
    dec hl
    ld h, b

jr_000_2ffc:
    ld [hl], b
    ld [hl], $2f
    rst $38
    ld hl, $b932
    db $fd
    cp c
    cp d
    db $fd
    cp d
    rst $38
    ld hl, $8232
    db $fd
    add d
    add e
    db $fd
    add e
    rst $38
    ld hl, $0932
    ld a, [bc]
    ld a, [hl-]
    dec sp
    rst $38
    ld hl, $0b32
    ld b, b
    ld a, h
    ld l, a
    rst $38
    ld hl, $2f32
    rrca
    cpl
    rra
    ld e, a
    inc l
    cpl
    ccf
    rst $38
    ld hl, $6c32
    inc a
    ld c, e
    ld c, h
    ld e, e
    ld e, h
    ld l, e
    cpl
    rst $38
    pop af
    ld sp, $4d2f
    db $fd
    ld c, l
    cpl
    cpl
    ld e, l
    ld e, [hl]
    ld c, [hl]
    ld e, a
    ld l, l
    ld l, [hl]
    cpl
    cpl
    ld a, l
    db $fd
    ld a, l
    cpl
    rst $38
    pop af
    ld sp, $7708
    db $fd
    ld [hl], a
    db $fd
    ld [$7818], sp
    ld b, e
    ld d, e
    ld a, d
    ld a, e
    ld d, b
    cpl
    cpl
    ld [bc], a
    db $fd
    ld a, l
    cpl
    rst $38
    ld hl, $b932
    db $fd
    cp c
    cp d
    db $fd
    cp d
    rst $38
    ld hl, $8232
    db $fd
    add d
    add e
    db $fd
    add e
    rst $38
    ld hl, $0932
    ld a, [bc]
    ld a, [hl-]
    dec sp
    rst $38
    ld hl, $0b32
    ld b, b
    ld a, h
    ld l, a
    rst $38
    ld hl, $dc32
    db $dd
    ldh [$ffe1], a
    rst $38
    ld hl, $de32
    rst $18
    ldh [$ffe1], a
    rst $38
    ld hl, $de32
    ldh [c], a
    ldh [$ffe4], a
    rst $38
    ld hl, $dc32
    xor $e0
    db $e3
    rst $38
    ld hl, $e532
    and $e7
    add sp, -$01
    ld hl, $fd32
    and $fd
    push hl
    db $fd
    add sp, -$03
    rst $20
    rst $38
    ld hl, $e932
    ld [$eceb], a
    rst $38
    ld hl, $ed32
    ld [$eceb], a
    rst $38
    ld hl, $f232
    db $f4
    di
    cp a
    rst $38
    ld hl, $f432
    ldh a, [c]
    cp a
    di
    rst $38
    ld hl, $c232
    db $fd
    jp nz, $fdc3

    jp $21ff


    ld [hl-], a
    call nz, $c4fd
    push bc
    db $fd
    push bc
    rst $38
    ld hl, $dc32
    db $fd
    call c, $fdef
    rst $28
    rst $38
    ld hl, $f032
    db $fd
    ldh a, [$fff1]
    db $fd
    pop af
    rst $38
    ld hl, $dc32
    db $fd
    ldh a, [$fff1]
    db $fd
    rst $28
    rst $38
    ld hl, $f032
    db $fd
    call c, $fdef
    pop af
    rst $38
    ld hl, $bd32
    cp [hl]
    cp e
    cp h
    rst $38
    ld hl, $b932
    cp d
    jp c, $ffdb

    inc de
    ld sp, $f0e0
    dec a
    ld [hl-], a
    ret nz

    pop bc
    push bc
    add $cc
    call $7675
    and h
    and l
    and [hl]
    and a
    ld d, h
    ld d, l
    ld d, [hl]
    ld d, a
    ld b, h
    ld b, l
    ld b, [hl]
    ld b, a
    and b
    and c
    and d
    and e
    sbc h
    sbc l
    sbc [hl]
    sbc a
    rst $38
    ld e, [hl]
    ld sp, $e8f8
    ld h, h
    ld sp, $e8f0
    ld l, l
    ld sp, $0000
    ld [hl], e
    ld sp, $0000
    ld a, c
    ld sp, $0000
    add d
    ld sp, $0000
    push hl
    ld sp, $0000
    db $eb
    ld sp, $0000
    xor h
    ld sp, $f8d8
    call nz, $e831
    ld hl, sp-$2a
    ld sp, $f8f0
    ld [hl], l
    ld [hl-], a
    ld h, e
    ld h, h
    ld h, l
    rst $38
    ld [hl], l
    ld [hl-], a
    ld h, e
    ld h, h
    ld h, l
    ld h, [hl]
    ld h, a
    ld l, b
    rst $38
    ld [hl], l
    ld [hl-], a
    ld b, c
    ld b, c
    ld b, c
    rst $38
    ld [hl], l
    ld [hl-], a
    ld b, d
    ld b, d
    ld b, d
    rst $38
    ld [hl], l
    ld [hl-], a
    ld d, d
    ld d, d
    ld d, d
    ld h, d
    ld h, d
    ld h, d
    rst $38
    ld [hl], l
    ld [hl-], a
    ld d, c
    ld d, c
    ld d, c
    ld h, c
    ld h, c
    ld h, c
    ld [hl], c
    ld [hl], c
    ld [hl], c
    rst $38
    pop af
    ld sp, $2f2f
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld h, e
    ld h, h
    db $fd
    ld h, h
    db $fd
    ld h, e
    ld h, [hl]
    ld h, a
    db $fd
    ld h, a
    db $fd
    ld h, [hl]
    rst $38
    ld hl, $2f32
    cpl
    ld h, e
    ld h, h
    rst $38
    ld hl, $0032
    db $fd
    nop
    db $10
    db $fd
    db $10
    ld c, a
    db $fd
    ld c, a
    add b
    db $fd
    add b
    add b
    db $fd
    add b
    add c
    db $fd
    add c
    sub a
    db $fd
    sub a
    rst $38
    ld hl, $9832
    db $fd
    sbc b
    sbc c
    db $fd
    sbc c
    add b
    db $fd
    add b
    sbc d
    db $fd
    sbc d
    sbc e
    db $fd
    sbc e
    rst $38
    ld hl, $a832
    db $fd
    xor b
    xor c
    db $fd
    xor c
    xor d
    db $fd
    xor d
    xor e
    db $fd
    xor e
    rst $38
    ld hl, $4132
    cpl
    cpl
    rst $38
    ld hl, $5232
    cpl
    ld h, d
    rst $38
    nop
    nop

    db $00, $08

    nop
    stop
    db $18

    db $08, $00, $08, $08, $08, $10

    db $08
    db $18

    db $10, $00, $10, $08, $10, $10, $10, $18, $18, $00, $18, $08, $18, $10

    jr @+$1a

    db $00, $00, $00, $08, $00, $10, $00, $18, $00, $20, $00, $28

    nop
    jr nc, jr_000_3220

jr_000_3220:
    jr c, jr_000_3222

jr_000_3222:
    nop
    nop
    ld [$0008], sp
    ld [$1008], sp
    nop
    db $10
    ld [$0018], sp
    jr jr_000_3239

    jr nz, jr_000_3233

jr_000_3233:
    jr nz, jr_000_323d

    jr z, jr_000_3237

jr_000_3237:
    jr z, jr_000_3241

jr_000_3239:
    jr nc, jr_000_323b

jr_000_323b:
    jr nc, jr_000_3245

jr_000_323d:
    nop
    ld [$1000], sp

jr_000_3241:
    ld [$0808], sp
    db $10

jr_000_3245:
    stop
    db $10
    ld [$1010], sp
    db $10
    jr @+$1a

    nop
    jr jr_000_3259

    jr jr_000_3263

    jr jr_000_326d

    jr nz, jr_000_3257

jr_000_3257:
    jr nz, jr_000_3261

jr_000_3259:
    jr nz, jr_000_326b

    jr nz, jr_000_3275

    jr z, jr_000_325f

jr_000_325f:
    jr z, jr_000_3269

jr_000_3261:
    jr z, jr_000_3273

jr_000_3263:
    jr z, @+$1a

    jr nc, jr_000_3267

jr_000_3267:
    jr nc, jr_000_3271

jr_000_3269:
    jr nc, jr_000_327b

jr_000_326b:
    jr nc, @+$1a

jr_000_326d:
    jr c, jr_000_326f

jr_000_326f:
    jr c, @+$0a

jr_000_3271:
    jr c, jr_000_3283

jr_000_3273:
    jr c, @+$1a

jr_000_3275:
    nop
    nop
    nop
    ld [$1000], sp

jr_000_327b:
    ld [$0800], sp
    ld [$1008], sp
    stop

jr_000_3283:
    db $10
    ld [$1010], sp

    INCBIN "gfx/image_000_3287.2bpp"

    INCBIN "gfx/image_000_3e87.2bpp"

    db $2a, $7b, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $7b, $30, $31, $31
    db $31, $31, $31, $32, $2a, $7c, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f
    db $7c, $44, $1c, $0c, $18, $1b, $0e, $45, $2a, $7d, $2f, $2f, $2f, $2f, $2f, $2f
    db $2f, $2f, $2f, $2f, $7d, $67, $46, $46, $46, $46, $46, $68, $2a, $7b, $2f, $2f
    db $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $7b, $2f, $2f, $2f, $2f, $2f, $00, $2f
    db $2a, $7c, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $7c, $43, $34, $34
    db $34, $34, $34, $34, $2a, $7d, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f
    db $7d, $30, $31, $31, $31, $31, $31, $32, $2a, $7b, $2f, $2f, $2f, $2f, $2f, $2f
    db $2f, $2f, $2f, $2f, $7b, $36, $15, $0e, $1f, $0e, $15, $37, $2a, $7c, $2f, $2f
    db $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $7c, $36, $2f, $2f, $2f, $2f, $2f, $37
    db $2a, $7d, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $7d, $40, $42, $42
    db $42, $42, $42, $41, $2a, $7b, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f
    db $7b, $36, $15, $12, $17, $0e, $1c, $37, $2a, $7c, $2f, $2f, $2f, $2f, $2f, $2f
    db $2f, $2f, $2f, $2f, $7c, $36, $2f, $2f, $2f, $2f, $2f, $37, $2a, $7d, $2f, $2f
    db $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $7d, $33, $34, $34, $34, $34, $34, $35
    db $2a, $7b, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $7b, $2b, $38, $39
    db $39, $39, $39, $3a, $2a, $7c, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f
    db $7c, $2b, $3b, $2f, $2f, $2f, $2f, $3c, $2a, $7d, $2f, $2f, $2f, $2f, $2f, $2f
    db $2f, $2f, $2f, $2f, $7d, $2b, $3b, $2f, $2f

    ; BANK 1

    db $2f, $2f, $3c, $2a, $7b, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $2f, $7b
    db $2b, $3b, $2f, $2f, $2f, $2f, $3c, $2a, $7c, $2f, $2f, $2f, $2f, $2f, $2f, $2f
    db $2f, $2f, $2f, $7c, $2b, $3b, $2f, $2f, $2f, $2f, $3c, $2a, $7d, $2f, $2f, $2f
    db $2f, $2f, $2f, $2f, $2f, $2f, $2f, $7d, $2b, $3d, $3e, $3e, $3e, $3e, $3f

    ld a, [hl+]
    ld a, e
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, e
    jr nc, jr_001_407f

    ld sp, $3131
    ld sp, $2a32
    ld a, h
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, h
    ld [hl], $15
    ld c, $1f
    ld c, $15
    scf
    ld a, [hl+]
    ld a, l
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, l
    ld [hl], $2f
    cpl
    cpl
    cpl
    cpl
    scf
    ld a, [hl+]
    ld a, e
    cpl
    cpl

jr_001_407f:
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, e
    ld b, b
    ld b, d
    ld b, d
    ld b, d
    ld b, d
    ld b, d
    ld b, c
    ld a, [hl+]
    ld a, h
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, h
    ld [hl], $11
    ld [de], a
    db $10
    ld de, $372f
    ld a, [hl+]
    ld a, l
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, l
    ld [hl], $2f
    cpl
    cpl
    cpl
    cpl
    scf
    ld a, [hl+]
    ld a, e
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, e
    inc sp
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    dec [hl]
    ld a, [hl+]
    ld a, h
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, h
    dec hl
    adc [hl]
    adc [hl]
    adc [hl]
    adc [hl]
    adc [hl]
    adc [hl]
    ld a, [hl+]
    ld a, l
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, l
    jr nc, jr_001_411f

    ld sp, $3131
    ld sp, $2a32
    ld a, e
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, e
    ld [hl], $15
    ld [de], a
    rla
    ld c, $1c
    scf
    ld a, [hl+]
    ld a, h
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, h
    ld [hl], $2f
    cpl
    ld [bc], a
    dec b
    cpl
    scf
    ld a, [hl+]
    ld a, l
    cpl
    cpl

jr_001_411f:
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, l
    inc sp
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    dec [hl]
    ld a, [hl+]
    ld a, e
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, e
    dec hl
    jr c, jr_001_4178

    add hl, sp
    add hl, sp
    add hl, sp
    ld a, [hl-]
    ld a, [hl+]
    ld a, h
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, h
    dec hl
    dec sp
    cpl
    cpl
    cpl
    cpl
    inc a
    ld a, [hl+]
    ld a, l
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, l
    dec hl
    dec sp
    cpl
    cpl
    cpl
    cpl
    inc a
    ld a, [hl+]
    ld a, e
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, e

jr_001_4178:
    dec hl
    dec sp
    cpl
    cpl
    cpl
    cpl
    inc a
    ld a, [hl+]
    ld a, h
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, h
    dec hl
    dec sp
    cpl
    cpl
    cpl
    cpl
    inc a
    ld a, [hl+]
    ld a, l
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, l
    dec hl
    dec a
    ld a, $3e
    ld a, $3e
    ccf

    INCLUDE "tiles.asm"

    db $47, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48
    db $48, $48, $48, $49, $4a, $2f, $0a, $25, $1d, $22, $19, $0e, $2f, $2c, $2c, $2c
    db $2c, $2c, $2c, $2c, $2c, $2c, $2c, $4b, $4a, $2c, $2c, $2c, $2c, $2c, $2c, $2c
    db $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $2c, $4b, $4a, $2c, $2c, $2c
    db $2c, $2c, $50, $51, $51, $51, $51, $51, $52, $2c, $2c, $2c, $2c, $2c, $2c, $4b
    db $4a, $2c, $2c, $2c, $2c, $2c, $53, $15, $0e, $1f, $0e, $15, $54, $2c, $2c, $2c
    db $2c, $2c, $2c, $4b, $4a, $2c, $2c, $2c, $55, $56, $57, $58, $6c, $58, $6c, $58
    db $59, $56, $5a, $2c, $2c, $2c, $2c, $4b, $4a, $2c, $2c, $2c, $5b, $90, $6f, $91
    db $6f, $92, $6f, $93, $6f, $94, $5c, $2c, $2c, $2c, $2c, $4b, $4a, $2c, $2c, $2c
    db $71, $72, $73, $72, $73, $72, $73, $72, $73, $72, $74, $2c, $2c, $2c, $2c, $4b
    db $4a, $2c, $2c, $2c, $5b, $95, $6f, $96, $6f, $97, $6f, $98, $6f, $99, $5c, $2c
    db $2c, $2c, $2c, $4b, $4a, $2c, $2c, $2c, $2d, $4f, $6b, $4f, $6b, $4f, $6b, $4f
    db $6b, $4f, $2e, $2c, $2c, $2c, $2c, $4b, $4a, $2c, $2c, $2c, $50, $51, $51, $51
    db $51, $51, $51, $51, $51, $51, $52, $2c, $2c, $2c, $2c, $4b, $4a, $2c, $2c, $2c
    db $53, $1d, $18, $19, $25, $1c, $0c, $18, $1b, $0e, $54, $2c, $2c, $2c, $2c, $4b
    db $4a, $55, $56, $70, $6d, $58, $58, $58, $58, $58, $58, $58, $58, $58, $6e, $56
    db $56, $56, $5a, $4b, $4a, $5b, $01, $6f, $60, $60, $60, $60, $60, $60, $2f, $2f
    db $60, $60, $60, $60, $60, $60, $5c, $4b, $4a, $5b, $02, $6f, $60, $60, $60, $60
    db $60, $60, $2f, $2f, $60, $60, $60, $60, $60, $60, $5c, $4b, $4a, $5b, $03, $6f
    db $60, $60, $60, $60, $60, $60, $2f, $2f, $60, $60, $60, $60, $60, $60, $5c, $4b
    db $4a, $2d, $4f, $6b, $4f, $4f, $4f, $4f, $4f, $4f, $4f, $4f, $4f, $4f, $4f, $4f
    db $4f, $4f, $2e, $4b, $4c, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4d
    db $4d, $4d, $4d, $4d, $4d, $4d, $4d, $4e

    INCBIN "gfx/image_001_4fef.2bpp"

    ld [hl], d
    ld [hl], e
    ld [hl], d
    ld [hl], e
    ld [hl], d
    ld [hl], e
    ld [hl], d
    ld [hl], h
    ld [hl], c
    ld [hl], d
    ld [hl], e
    ld [hl], d
    ld [hl], e
    ld [hl], d
    ld [hl], h
    ld c, e
    ld c, d
    ld e, e
    sub l
    ld l, a
    sub [hl]
    ld l, a
    sub a
    ld l, a
    sbc b
    ld l, a
    sbc c
    ld e, h
    ld e, e
    sub e
    ld l, a
    sub h
    ld l, a
    sub l
    ld e, h
    ld c, e
    ld c, d
    dec l
    ld c, a
    ld l, e
    ld c, a
    ld l, e
    ld c, a
    ld l, e
    ld c, a
    ld l, e
    ld c, a
    ld l, $2d
    ld c, a
    ld l, e
    ld c, a
    ld l, e
    ld c, a
    ld l, $4b
    ld c, d
    inc l
    inc l
    inc l
    ld d, b
    ld d, c
    ld d, c
    ld d, c
    ld d, c
    ld d, c
    ld d, c
    ld d, c
    ld d, c
    ld d, c
    ld d, d
    inc l
    inc l
    inc l
    inc l
    ld c, e
    ld c, d
    inc l
    inc l
    inc l
    ld d, e
    dec e
    jr jr_001_50ec

    dec h
    inc e
    inc c
    jr jr_001_50f3

    ld c, $54
    inc l
    inc l
    inc l
    inc l
    ld c, e
    ld c, d
    ld d, l
    ld d, [hl]
    ld [hl], b
    ld l, l
    ld e, b
    ld e, b
    ld e, b
    ld e, b
    ld e, b
    ld e, b
    ld e, b
    ld e, b

jr_001_50ec:
    ld e, b
    ld l, [hl]
    ld d, [hl]
    ld d, [hl]
    ld d, [hl]
    ld e, d
    ld c, e

jr_001_50f3:
    ld c, d
    ld e, e
    ld bc, $606f
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    cpl
    cpl
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld e, h
    ld c, e
    ld c, d
    ld e, e
    ld [bc], a
    ld l, a
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    cpl
    cpl
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld e, h
    ld c, e
    ld c, d
    ld e, e
    inc bc
    ld l, a
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    cpl
    cpl
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld e, h
    ld c, e
    ld c, d
    dec l
    ld c, a
    ld l, e
    ld c, a
    ld c, a
    ld c, a
    ld c, a
    ld c, a
    ld c, a
    ld c, a
    ld c, a
    ld c, a
    ld c, a
    ld c, a
    ld c, a
    ld c, a
    ld c, a
    ld l, $4b
    ld c, h
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, [hl]
    call $cdcd
    call $cdcd
    call $cdcd
    call $c98c
    jp z, $8c8c

    adc h
    adc h
    adc h
    adc h
    adc h
    adc h
    set 1, h
    adc h
    adc h
    adc h
    adc h
    adc h
    adc h
    adc $d7
    rst $10
    rst $10
    rst $10
    rst $10
    rst $10
    rst $10
    rst $10
    rst $10
    rst $08
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ret nc

    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    pop de
    jp nc, Jump_000_2f2f

    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    db $d3
    call nc, Call_001_7c7c
    ld a, h
    ld a, h
    ld a, h
    ld a, h
    cpl
    cpl
    push de
    sub $7d
    ld a, l
    ld a, l
    ld a, l
    cpl
    cpl
    cpl
    cpl
    ret c

    cpl
    ld a, e
    ld a, e
    ld a, e
    ld a, e
    cpl
    cpl
    cpl
    cpl
    ret c

    cpl
    ld a, h
    ld a, h
    ld a, h
    ld a, h
    cpl
    cpl
    cpl
    cpl
    ret c

    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ret c

    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, h
    ld a, h
    ld a, h
    ld a, h
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, h
    ld a, l
    ld a, l
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, l
    cpl
    cpl
    cpl
    reti


    cpl
    cpl
    cpl
    cpl
    cpl
    ld a, e
    or a
    cp b
    reti


    or a
    cpl
    ld a, h
    ld a, h
    ld a, h
    ld a, h
    ld a, h
    ld a, l
    ld a, l
    ld a, l
    ld a, l
    ld a, l
    ld a, l
    ld a, l
    ld a, l
    ld a, l
    ld a, l
    rst $38
    ld c, d
    ld c, d
    ld c, d
    ld c, d
    ld c, d
    ld c, d
    ld e, c
    ld l, c
    ld l, c
    ld l, c
    ld l, c
    ld l, c
    ld l, c
    ld c, c
    ld c, d
    ld c, d
    ld c, d
    ld c, d
    ld c, d
    ld c, d
    ld e, d
    ld e, d
    ld e, d
    ld e, d
    ld e, d
    ld e, d
    add l
    add l
    add l
    add l
    add l
    add l
    add l
    add l
    ld e, d
    ld e, d
    jr c, jr_001_526b

    jr c, jr_001_528e

    ld l, d
    ld l, d
    ld l, d
    ld l, d
    ld l, d
    ld l, d
    ld l, d
    ld l, d
    ld l, d
    ld l, d
    ld l, d
    ld l, d
    ld l, d
    ld l, d
    ld l, d
    ld l, d
    ld l, d
    ld l, d
    ld l, d
    ld l, d
    rlca
    rlca
    rlca
    rlca
    rlca
    rlca
    rlca
    rlca
    rlca
    rlca
    rlca
    rlca
    rlca
    rlca
    rlca
    rlca
    rlca
    rlca
    rlca
    rlca
    ld b, a
    ld c, b
    ld c, b
    ld c, b
    ld c, b
    ld c, b
    ld c, b
    ld c, b
    ld c, b
    ld c, b
    ld c, b
    ld c, b
    ld c, b
    ld c, b
    ld c, b

jr_001_526b:
    ld c, b
    ld c, b
    ld c, b
    ld c, b
    ld c, c
    ld c, d
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    ld c, e
    ld c, d
    inc l
    inc l
    ld d, $0a
    dec de
    ld [de], a
    jr @+$31

    rra

jr_001_528e:
    inc e
    inc h
    dec d
    ld e, $12
    db $10
    ld [de], a
    inc l
    inc l
    ld c, e
    ld c, d
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    ld d, b
    ld d, c
    ld d, c
    ld d, c
    ld d, c
    ld d, d
    inc l
    inc l
    inc l
    ld c, e
    ld c, d
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    ld d, e
    ld de, $1012
    ld de, $2c54
    inc l
    inc l
    ld c, e
    ld c, d
    inc l
    inc l
    ld d, l
    ld d, [hl]
    ld d, [hl]
    ld e, d
    inc l
    inc l
    inc l
    ld [hl], l
    ld e, b
    ld l, h
    ld e, b
    ld l, h
    ld l, [hl]
    ld e, d
    inc l
    inc l
    ld c, e
    ld c, d
    inc l
    inc l
    ld e, e
    cpl
    cpl
    ld e, h
    inc l
    inc l
    inc l
    ld e, e
    sub b
    ld l, a
    sub c
    ld l, a
    sub d
    ld e, h
    inc l
    inc l
    ld c, e
    ld c, d
    inc l
    inc l
    ld e, e
    cpl
    cpl
    ld e, h
    inc l
    inc l
    inc l
    ld [hl], c
    ld [hl], d
    ld [hl], e
    ld [hl], d
    ld [hl], e
    ld [hl], d
    ld [hl], h
    inc l
    inc l
    ld c, e
    ld c, d
    inc l
    inc l
    dec l
    ld c, a
    ld c, a
    ld l, $2c
    inc l
    inc l
    ld e, e
    sub e
    ld l, a
    sub h
    ld l, a
    sub l
    ld e, h
    inc l
    inc l
    ld c, e
    ld c, d
    inc l
    inc l
    ld d, $0a
    dec de
    ld [de], a
    jr @+$2e

    inc l
    dec l
    ld c, a
    ld l, e
    ld c, a
    ld l, e
    ld c, a
    ld l, $2c
    inc l
    ld c, e
    ld c, d
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    ld d, b
    ld d, c
    ld d, c
    ld d, c
    ld d, c
    ld d, d
    inc l
    inc l
    inc l
    ld c, e
    ld c, d
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    inc l
    ld d, e
    ld de, $1012
    ld de, $2c54
    inc l
    inc l
    ld c, e
    ld c, d
    inc l
    inc l
    ld d, l
    ld d, [hl]
    ld d, [hl]
    ld e, d
    inc l
    inc l
    inc l
    ld [hl], l
    ld e, b
    ld l, h
    ld e, b
    ld l, h
    ld l, [hl]
    ld e, d
    inc l
    inc l
    ld c, e
    ld c, d
    inc l
    inc l
    ld e, e
    cpl
    cpl
    ld e, h
    inc l
    inc l
    inc l
    ld e, e
    sub b
    ld l, a
    sub c
    ld l, a
    sub d
    ld e, h
    inc l
    inc l
    ld c, e
    ld c, d
    inc l
    inc l
    ld e, e
    cpl
    cpl
    ld e, h
    inc l
    inc l
    inc l
    ld [hl], c
    ld [hl], d
    ld [hl], e
    ld [hl], d
    ld [hl], e
    ld [hl], d
    ld [hl], h
    inc l
    inc l
    ld c, e
    ld c, d
    inc l
    inc l
    dec l
    ld c, a
    ld c, a
    ld l, $2c
    inc l
    inc l
    ld e, e
    sub e
    ld l, a
    sub h
    ld l, a
    sub l
    ld e, h
    inc l
    inc l
    ld c, e
    ld c, d
    inc l
    inc l
    dec d
    ld e, $12
    db $10
    ld [de], a
    inc l
    inc l
    dec l
    ld c, a
    ld l, e
    ld c, a
    ld l, e
    ld c, a
    ld l, $2c
    inc l
    ld c, e
    ld c, h
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, l
    ld c, [hl]
    adc [hl]
    or d
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    or e
    jr nc, jr_001_5404

    ld sp, $3131
    ld sp, $8e32
    or b
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    or l
    ld [hl], $2f
    cpl
    cpl
    cpl
    cpl
    scf
    adc [hl]
    or b
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    or l
    ld [hl], $2f
    cpl
    cpl
    cpl
    cpl
    scf
    adc [hl]
    or b
    cpl
    cpl

jr_001_5404:
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    or l
    ld b, b
    ld b, d
    ld b, d
    ld b, d
    ld b, d
    ld b, d
    ld b, c
    adc [hl]
    or b
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    or l
    ld [hl], $11
    ld [de], a
    db $10
    ld de, $372f
    adc [hl]
    or b
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    or l
    ld [hl], $2f
    cpl
    cpl
    cpl
    cpl
    scf
    adc [hl]
    or b
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    or l
    inc sp
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    dec [hl]
    adc [hl]
    or b
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    or l
    dec hl
    adc [hl]
    adc [hl]
    adc [hl]
    adc [hl]
    adc [hl]
    adc [hl]
    adc [hl]
    or b
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    or l
    jr nc, jr_001_54a4

    ld sp, $3131
    ld sp, $8e32
    or b
    cpl
    cpl
    cpl
    cpl
    cpl

Jump_001_547f:
    cpl
    cpl
    cpl
    cpl
    cpl
    or l
    ld [hl], $15
    ld [de], a
    rla
    ld c, $1c
    scf
    adc [hl]
    or b
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    or l
    ld [hl], $2f
    cpl
    cpl
    cpl
    cpl
    scf
    adc [hl]
    or b
    cpl
    cpl

jr_001_54a4:
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    or l
    inc sp
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    dec [hl]
    adc [hl]
    or b
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    or l
    dec hl
    jr c, jr_001_54fd

    add hl, sp
    add hl, sp
    add hl, sp
    ld a, [hl-]
    adc [hl]
    or b
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    or l
    dec hl
    dec sp
    cpl
    cpl
    cpl
    cpl
    inc a
    adc [hl]
    or b
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    or l
    dec hl
    dec sp
    cpl
    cpl
    cpl
    cpl
    inc a
    adc [hl]
    or b
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    or l

jr_001_54fd:
    dec hl
    dec sp
    cpl
    cpl
    cpl
    cpl
    inc a
    adc [hl]
    or b
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    or l
    dec hl
    dec sp
    cpl
    cpl
    cpl
    cpl
    inc a
    adc [hl]
    or c
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    cpl
    or h
    dec hl
    dec a
    ld a, $3e
    ld a, $3e
    ccf
    rlca
    rlca
    rlca
    rlca
    rlca
    rlca
    add h
    add a
    add a
    adc h
    add a
    add a
    adc h
    add a
    add a
    adc h
    add a
    add a
    add [hl]
    rlca
    rlca
    ld e, $1e
    ld e, $1e
    ld e, $79
    cpl
    cpl
    adc l
    cpl
    cpl
    adc l
    cpl
    cpl
    adc l
    cpl
    cpl
    adc b
    rlca
    rlca
    or h
    or l
    cp e
    ld l, $bc
    ld a, c
    cpl
    cpl
    adc l
    cpl
    cpl
    adc l
    cpl
    cpl
    adc l
    cpl
    cpl
    adc b
    rlca
    rlca
    cp a
    cp a
    cp a
    cp a
    cp a
    adc c
    adc d
    adc d
    adc [hl]
    adc d
    adc d
    adc [hl]
    adc d
    adc d
    adc [hl]
    adc d
    adc d
    adc e
    rlca
    ld b, $06
    ld b, $06
    ld b, $06
    ld b, $06
    ld b, $06
    ld b, $06
    ld b, $06
    ld b, $06
    ld b, $06
    ld b, $06
    ld d, $16
    ld d, $16
    ld d, $16
    ld d, $16
    ld d, $16
    ld d, $16
    ld d, $16
    ld d, $16
    ld d, $16
    ld d, $16
    rlca
    rlca
    rlca
    rlca
    rlca
    rlca
    add h
    add a
    add a
    adc h
    add a
    add a
    adc h
    add a
    add a
    adc h
    add a
    add a
    add [hl]
    rlca
    rlca
    ld e, $1e
    ld e, $1e
    ld e, $79
    cpl
    cpl
    adc l
    cpl
    cpl
    adc l
    cpl
    cpl
    adc l
    cpl
    cpl
    adc b
    rlca
    rlca
    cp l
    or d
    ld l, $be
    ld l, $79
    cpl
    cpl
    adc l
    cpl
    cpl
    adc l
    cpl
    cpl
    adc l
    cpl
    cpl
    adc b
    rlca
    rlca
    cp a
    cp a
    cp a
    cp a
    cp a
    adc c
    adc d
    adc d
    adc [hl]
    adc d
    adc d
    adc [hl]
    adc d
    adc d
    adc [hl]
    adc d
    adc d
    adc e
    rlca
    ld bc, $0101
    ld bc, $0101
    ld [bc], a
    ld [bc], a
    inc bc
    inc bc
    ld bc, $0101
    ld bc, $0202
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    rlca
    rlca
    jr @+$21

    ld hl, $473e
    ld a, a
    ldh a, [c]
    cp $12
    ld e, $12
    ld e, $12
    ld e, $7e
    ld a, [hl]
    rst $38
    add e
    rst $38
    add c
    rst $38
    rst $38
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    rlca
    rlca
    jr jr_001_564f

    ld hl, $473e
    ld a, a
    inc b
    db $fc
    ld [bc], a
    cp $02
    cp $07
    db $fd
    rlca
    db $fd
    rra
    rst $38
    rst $38
    rst $38
    rst $38
    ld a, [$0000]
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop

jr_001_564f:
    nop
    rlca
    rlca
    jr jr_001_5673

    rst $38
    rst $38
    ld [hl], a
    ld de, $11ff
    rst $38
    rst $38
    db $dd
    ld b, h
    rst $38
    ld b, h
    rst $38
    rst $38
    ld [hl], a
    ld de, $ffff
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38

jr_001_5673:
    rst $38
    nop
    nop
    inc bc
    inc bc
    dec b
    inc b
    inc bc
    inc bc
    nop
    nop
    jr jr_001_5698

    inc l
    inc h
    ld a, [de]
    ld a, [de]
    ld [$4008], sp
    ld b, b
    rlca
    rlca
    jr @+$21

    and b
    cp a
    dec sp
    ccf
    ld a, h
    ld b, h
    ld a, h
    ld b, h

jr_001_5694:
    db $10
    db $10
    ld [bc], a
    ld [bc], a

jr_001_5698:
    ldh [$ffe0], a
    jr jr_001_5694

    dec b
    db $fd
    adc h
    db $fc
    ld a, b
    ld c, b
    ld l, h
    ld [hl], h
    nop
    nop
    rlca
    rlca
    jr jr_001_56c9

    jr nz, jr_001_56eb

    jr nc, jr_001_56ed

    rra
    dec e
    ld a, $22
    ld a, $22
    add b
    add b
    add b
    add b
    add b
    add b
    add b
    add b
    nop
    nop
    ret nz

    ret nz

    ldh [$ffe0], a
    ldh [$ffe0], a
    nop
    nop
    ld a, h
    ld a, h
    ld h, [hl]

jr_001_56c9:
    ld h, [hl]
    ld h, [hl]
    ld h, [hl]
    ld a, h
    ld a, h
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    nop
    nop
    nop
    nop
    inc a
    inc a
    ld h, b
    ld h, b
    inc a
    inc a
    ld c, $0e
    ld c, [hl]
    ld c, [hl]
    inc a
    inc a
    nop
    nop
    rlca
    rlca
    rra
    jr jr_001_5727

    jr nz, jr_001_576a

jr_001_56eb:
    ld c, a
    ld a, a

jr_001_56ed:
    ld e, a
    ld [hl], b
    ld [hl], b
    and d
    and d
    or b
    or b
    inc b
    inc b
    rlca
    inc b
    inc b
    inc b
    inc b
    dec c
    inc b
    dec c
    inc b
    inc b
    inc b
    inc b
    inc bc
    ld [bc], a
    ld e, a
    ld a, a
    add hl, sp
    jr nc, @+$7d

    ld h, d
    ei
    or d
    rst $38
    and b
    rst $38
    jp nz, Jump_001_547f

    ld a, a
    ld e, h
    nop
    nop
    nop
    nop
    nop
    nop
    inc bc
    inc bc
    inc b
    inc b
    ld [$0908], sp
    add hl, bc
    inc b
    inc b
    ld e, a
    ld a, a
    add hl, sp

jr_001_5727:
    jr nc, jr_001_57a4

    ld h, d
    ei
    or d
    rst $38
    and b

jr_001_572e:
    rst $38
    jp nz, Jump_001_547f

    ld a, a
    ld e, h
    jr jr_001_572e

    inc b
    db $fc
    ld [bc], a
    cp $02
    cp $07
    db $fd
    rlca
    db $fd
    rst $38
    rst $38
    rst $38
    ld a, [$3f20]
    ld b, b
    ld a, a
    ld b, b
    ld a, a
    ldh [$ffbf], a
    ldh [$ffbf], a
    ld hl, sp-$01
    ld a, a
    ld a, a
    ld a, a
    ld e, a
    rst $38
    ld de, $ffff
    db $dd
    ld b, h
    rst $38
    ld b, h
    rst $38
    rst $38
    ld [hl], a
    ld de, $11ff
    rst $38
    rst $38
    nop
    nop
    nop
    nop
    nop
    nop

jr_001_576a:
    nop
    nop
    nop
    nop
    nop
    nop
    add b
    add b
    ret nz

    ld b, b
    nop
    nop
    nop
    nop
    nop
    nop
    inc b
    inc b
    ld [$1c08], sp
    inc d
    inc d
    inc d
    ld [$1808], sp
    rra
    jr nz, jr_001_57c7

    ld b, b
    ld a, a
    ld b, b
    ld a, a
    ldh [$ffbf], a
    ldh [$ffbf], a
    ld a, a
    ld a, a
    ld a, a
    ld e, a
    db $dd
    ld b, h
    rst $38
    ld b, h
    rst $38
    rst $38
    ld [hl], a
    ld de, $11ff
    rst $38
    rst $38
    db $dd
    ld b, h
    rst $38
    ld b, h

jr_001_57a4:
    nop
    nop
    nop
    nop
    nop
    nop
    jr nz, jr_001_57cc

    db $10
    db $10
    jr c, jr_001_57d8

    jr z, jr_001_57da

    sub b
    sub b
    nop
    nop
    ld b, [hl]
    ld b, [hl]
    ld b, [hl]
    ld b, [hl]
    ld a, [hl]
    ld a, [hl]
    ld b, [hl]
    ld b, [hl]
    ld b, [hl]
    ld b, [hl]
    ld b, [hl]
    ld b, [hl]
    nop
    nop
    nop
    nop
    ld a, [hl]

jr_001_57c7:
    ld a, [hl]
    jr jr_001_57e2

    jr @+$1a

jr_001_57cc:
    jr jr_001_57e6

    jr jr_001_57e8

    jr jr_001_57ea

    nop
    nop
    rst $38
    rst $38
    rst $38
    rst $38

jr_001_57d8:
    rst $38
    rst $38

jr_001_57da:
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    nop
    nop

jr_001_57e2:
    nop
    xor $b4
    or h

jr_001_57e6:
    ld h, h
    ld h, h

jr_001_57e8:
    inc a
    inc a

jr_001_57ea:
    ld l, $2e
    daa
    daa
    ld [hl], b
    ld [hl], b
    db $fc
    sbc h
    rst $30
    sbc a
    nop
    nop
    nop
    nop
    nop
    nop
    ld bc, $0101
    ld bc, $0202
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ccf
    ld l, $7f
    ld h, e
    rst $38
    sbc b
    rst $30
    rra
    rst $30
    inc e
    rst $30
    rst $10
    inc [hl]
    ccf
    xor h
    cp a
    inc bc
    inc bc
    ld bc, $0101
    ld bc, $0000
    nop
    nop
    ld b, $06
    dec b
    dec b

jr_001_5822:
    rlca
    rlca
    rst $38
    xor [hl]
    rst $38
    inc hl
    rst $38
    jr jr_001_5822

    sbc a
    rst $30
    sbc h
    ld [hl], a
    ld d, a
    inc [hl]
    ccf
    ld l, h
    ld a, a
    nop
    nop
    nop
    nop
    nop
    nop
    ld bc, $0101
    ld bc, $0202
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ccf
    cpl
    ld a, a
    ld a, h
    rst $30
    sbc h
    di
    rra
    ldh a, [$ff1f]
    ldh a, [$ffdf]
    jr nc, @+$41

    and b
    cp a
    rst $38
    db $f4
    rst $38
    ld a, $ef
    jr c, @-$2f

    ld hl, sp+$0f
    ei
    ld c, $fa
    inc c
    db $fc
    inc b
    db $fc
    ldh [rNR41], a
    ldh [rNR41], a
    ldh [rNR41], a

jr_001_586a:
    ret nz

    ld b, b
    add b
    add b
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    ld bc, $0101
    ld bc, $0202
    ld [bc], a
    ld [bc], a
    ccf
    cpl
    ccf
    inc a
    ld [hl], a
    ld e, h
    di
    sbc a
    ldh a, [$ff1f]
    ldh a, [$ff1f]
    ldh a, [rIE]
    jr nz, jr_001_58d3

    rst $38
    db $f4
    rst $38
    ld a, $ef
    jr c, jr_001_586a

    ld sp, hl
    ld c, $fa
    ld c, $fa
    inc c
    db $fc
    inc b
    db $fc
    ret nz

    ld b, b
    ret nz

    ld b, b
    ret nz

    ld b, b
    add b
    add b
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    rst $30
    inc e
    rst $30
    inc [hl]
    rst $30
    cp a
    ld l, h
    ld a, a
    db $10
    rra
    ld d, b
    ld e, a
    ld [hl-], a
    ccf
    pop af
    rst $38
    nop
    nop
    ld b, [hl]
    ld b, [hl]
    ld b, [hl]
    ld b, [hl]
    ld d, [hl]
    ld d, [hl]
    ld a, [hl]
    ld a, [hl]
    ld l, [hl]
    ld l, [hl]
    ld b, [hl]
    ld b, [hl]
    nop

jr_001_58d3:
    nop
    nop
    nop
    inc a
    inc a
    jr jr_001_58f2

    jr jr_001_58f4

    jr jr_001_58f6

    jr @+$1a

    inc a
    inc a
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop

jr_001_58f2:
    nop
    nop

jr_001_58f4:
    ld [bc], a
    ld [bc], a

jr_001_58f6:
    ld bc, $0001
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    ld b, b
    ld a, a
    ret nz

    rst $38
    jr nz, @+$41

    ld [hl+], a
    ccf
    ld de, $721f
    ld a, [hl]
    cp a
    cp a
    rst $38
    rst $38
    rlca
    rlca
    ld b, $07
    ld b, $07
    ld b, $07
    rlca
    rlca
    nop
    nop
    nop
    nop
    nop
    nop
    ret nz

    rst $38
    nop
    rst $38
    nop
    rst $38
    ld [bc], a
    rst $38
    rst $38
    rst $38
    nop
    nop
    nop
    nop
    nop
    nop
    ld [bc], a
    ld [bc], a
    ld bc, $0001
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    ld b, b
    ld a, a
    ret nz

    rst $38
    jr nz, jr_001_5989

    jr nz, jr_001_598b

    ld de, $721f
    ld a, [hl]
    rst $38
    rst $38
    rst $38
    rst $38
    ld [bc], a
    cp $02
    cp $04
    db $fc
    inc b
    db $fc
    adc b
    ld hl, sp+$4e
    ld a, [hl]
    rst $38
    rst $38
    rst $38
    rst $38
    nop
    nop
    nop
    nop
    add b
    add b
    ld b, b
    ld b, b
    nop
    nop
    nop
    nop
    nop
    nop
    rlca
    rlca
    nop
    nop
    nop
    nop
    rst $38
    nop
    db $fd
    ld [bc], a
    call Call_000_0932
    or $08
    rst $30
    nop
    rst $38
    nop
    nop
    nop
    nop
    rst $38

jr_001_5989:
    nop
    rst $38

jr_001_598b:
    nop
    rst $38
    nop
    db $fc
    inc bc
    call z, Call_000_0833
    rst $30
    ld a, h
    ld b, h
    ccf
    ccf
    db $10
    rra
    db $10
    rra
    ld [de], a
    rra
    add hl, de
    rra
    ccf
    ccf
    ld a, $3e
    adc $f2
    adc [hl]
    jp c, $f909

    add hl, bc
    ld sp, hl
    ld c, [hl]
    cp $98
    ld hl, sp-$04
    db $fc
    ld a, h
    ld a, h
    rlca
    rlca
    rra
    jr @+$40

    jr nz, jr_001_5a3a

    ld c, a
    ld a, a
    ld e, a
    ld [hl], b
    ld [hl], b
    and d
    and d
    or b
    or b
    nop
    nop
    ld b, [hl]
    ld b, [hl]
    ld h, [hl]
    ld h, [hl]
    db $76
    db $76
    ld e, [hl]
    ld e, [hl]
    ld c, [hl]
    ld c, [hl]
    ld b, [hl]
    ld b, [hl]
    nop
    nop
    nop
    nop
    jr jr_001_59f0

    jr jr_001_59f2

    jr jr_001_59f4

    jr jr_001_59f6

    nop
    nop
    jr jr_001_59fa

    nop
    nop
    ld [de], a
    ld e, $12
    ld e, $12
    ld e, $12
    ld e, $7e
    ld a, [hl]
    cp a
    add e

jr_001_59f0:
    rst $38
    add c

jr_001_59f2:
    rst $38
    rst $38

jr_001_59f4:
    nop
    nop

jr_001_59f6:
    ldh [$ffe0], a
    jr jr_001_59f2

jr_001_59fa:
    inc b
    db $fc
    inc c
    db $fc
    ld hl, sp-$38
    inc l
    inc [hl]
    ld l, $32
    nop
    nop
    ld b, [hl]

jr_001_5a07:
    ld b, [hl]
    ld b, [hl]

jr_001_5a09:
    ld b, [hl]
    ld b, [hl]

jr_001_5a0b:
    ld b, [hl]
    ld b, [hl]
    ld b, [hl]
    inc l
    inc l
    jr jr_001_5a2a

    nop
    nop
    nop
    nop
    ld [hl], $36
    ld e, a
    ld c, c
    ld e, a
    ld b, c
    ld a, a
    ld b, c
    ld a, $22
    inc e
    inc d

jr_001_5a22:
    ld [$fe08], sp
    ld [bc], a
    db $fd
    dec b
    db $fd
    dec b

jr_001_5a2a:
    rst $38
    rra
    rst $38
    db $fc
    rst $38
    cp $ef
    jr c, jr_001_5a22

    add hl, sp
    nop
    inc b
    nop
    inc b
    nop
    inc b

jr_001_5a3a:
    ld bc, $0105
    dec b
    inc bc
    rlca
    ld b, $06
    inc c
    inc c
    jp z, $c8c0

    ret nz

    jp z, $88c0

    add b
    adc b
    add a
    ld [$0a00], sp
    nop
    ld [$6f00], sp
    inc de
    cpl
    inc de
    ld l, a
    inc de
    cpl
    ld de, $d12d
    inc l
    db $10
    ld l, h
    db $10
    inc l
    db $10
    and b
    jr nz, jr_001_5a07

    jr nz, jr_001_5a09

    jr nz, jr_001_5a0b

    and b
    and b
    and b
    ldh [$ffe0], a
    ld h, b
    ld h, b
    jr nc, jr_001_5aa4

    ld [$08a8], sp
    jr jr_001_5a81

    xor b
    ld [$0848], sp
    xor b
    ld [$0818], sp

jr_001_5a81:
    xor b
    ld [$0048], sp
    cp $00
    rst $38
    ld a, a
    rst $38
    ld a, a
    pop bc
    ld a, a
    pop bc
    ld a, a
    db $eb
    ld a, a
    pop bc
    ld bc, $00ff
    nop
    nop
    nop
    nop
    nop
    rst $38
    nop
    nop
    nop
    rst $38
    nop
    nop
    nop
    rst $38
    nop

jr_001_5aa4:
    db $10
    db $10
    dec bc
    dec bc
    rlca
    inc b
    rlca
    inc b
    inc bc
    ld [bc], a
    ld bc, $0001
    nop
    nop
    nop
    or h
    or h
    db $e4
    db $e4
    cp h
    cp h
    xor $6e
    rst $20
    daa
    ldh a, [rNR10]
    db $fc
    sbc h
    ld [hl], a
    ld e, a
    nop
    nop
    nop
    nop
    rlca
    rlca
    rra
    jr @+$41

    jr nz, jr_001_5b4e

    ld b, b
    ld a, a
    ld b, b
    ld a, a
    ld b, b
    nop
    nop
    nop
    nop
    nop
    nop
    add b
    add b
    ret nz

    ld b, b
    ret nz

    ld b, b
    ret nz

    ld b, b
    add b
    add b
    ld [bc], a
    inc bc
    dec b
    inc b
    rlca
    inc b
    inc b
    rlca
    inc b
    rlca
    inc b
    ld b, $04
    dec b
    inc b
    rlca
    adc $fa
    inc c
    db $fc
    ld [$08f8], sp
    ld hl, sp+$08
    ld hl, sp+$08
    ld hl, sp+$08
    ld hl, sp-$78
    ld hl, sp+$00
    inc a
    nop
    ld a, [hl]
    db $10
    ld h, a
    inc h
    jp $c324


    inc h

jr_001_5b0f:
    jp $c324


    inc [hl]

jr_001_5b13:
    jp $3c00


    nop
    ld h, [hl]
    nop
    rst $20
    inc l
    jp $c33c


    inc a
    jp $423c


    jr jr_001_5b8a

    nop
    nop
    nop
    nop
    nop
    nop
    jr nz, jr_001_5b4c

    sub b
    sub b
    cp b
    xor b
    xor b
    xor b
    db $10
    db $10
    ld a, [bc]
    db $10
    ld b, $08
    ld [bc], a
    inc b
    nop
    inc b
    nop
    inc b
    nop
    inc b
    nop
    inc b
    nop
    inc b
    rla
    ld d, b
    jr z, jr_001_5ba8

    ld a, [hl+]
    ld h, b
    jr z, jr_001_5bac

jr_001_5b4c:
    ld a, [hl+]
    ld h, b

jr_001_5b4e:
    jr z, jr_001_5bb0

    jr z, jr_001_5bb9

    ld l, b
    ld h, b
    sbc $2b
    ld l, $17
    ld l, [hl]
    rla
    ld l, $17
    ld l, [hl]
    rla
    ld l, $17
    ld l, $d7
    ld l, $17
    sbc b
    ld c, b
    or b
    ld d, b
    and b
    ld h, b
    and b
    jr nz, @-$5e

    jr nz, jr_001_5b0f

    jr nz, @-$5e

    jr nz, jr_001_5b13

    jr nz, jr_001_5b7d

    xor b
    ld [$0818], sp
    xor b
    ld [$0848], sp

jr_001_5b7d:
    cp b
    ld [$083f], sp
    cp a
    add hl, bc
    ld a, a
    nop
    ld a, a
    nop
    rst $38
    ld a, [hl]
    rst $38

jr_001_5b8a:
    ld a, [hl]
    pop bc
    ld a, [hl]
    pop bc
    ld a, [hl]
    db $eb
    ld a, [hl]
    pop bc
    nop
    rst $38
    nop
    nop
    nop
    nop
    rst $38
    nop
    rst $38
    nop
    rst $38
    nop
    rst $38
    nop
    rst $38
    nop
    rst $38
    nop
    nop
    nop
    jr c, @+$3a

jr_001_5ba8:
    inc [hl]
    inc h
    inc a
    inc h

jr_001_5bac:
    ccf
    daa
    inc a
    daa

jr_001_5bb0:
    inc a
    daa
    ccf
    cpl
    scf
    inc a
    rla
    inc d
    rla

jr_001_5bb9:
    rra
    inc e
    rra
    ldh a, [rIE]
    nop
    rst $38
    ld [bc], a
    rst $38
    rst $38
    rst $38
    cp a
    and b
    cp a
    and b
    cp a
    cp b
    ld a, a
    ld a, a
    cpl
    cpl
    ld a, a
    ld a, a
    rst $30
    sbc h

jr_001_5bd2:
    rst $30
    sbc h
    db $fd
    dec b
    db $fd
    dec b
    db $fd
    dec e
    rst $38
    rst $38
    rst $30
    db $f4
    rst $38
    cp $ef
    jr c, jr_001_5bd2

    jr c, @+$03

    ld bc, $0101
    ld bc, $0201
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld bc, $0001
    nop
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld bc, $0001
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    inc [hl]
    jp $433c


    inc a
    ld b, e
    jr jr_001_5c72

    jr jr_001_5c74

    ld [$0876], sp
    ld [hl], $08
    inc [hl]
    jr jr_001_5c3c

    jr jr_001_5c3c

    jr jr_001_5c3e

    ld [$0034], sp
    jr jr_001_5c1f

jr_001_5c1f:
    ld [$0800], sp
    nop
    ld [$0000], sp
    rrca
    rrca
    rra
    db $10
    inc a
    jr nz, jr_001_5c9d

    ld b, b
    ld [hl], e
    ld b, e
    ld h, a
    ld c, h
    ccf
    jr z, jr_001_5c35

jr_001_5c35:
    nop
    add b
    add b
    call c, $3e5c
    ld [hl+], a

jr_001_5c3c:
    ld [hl-], a
    ldh [c], a

jr_001_5c3e:
    or c
    pop bc
    jp $274b


    ld a, h
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    ldh [$ffe0], a
    ret nc

    db $10
    ret nc

    ret nc

    ldh [rNR41], a
    ld e, h
    ld d, b
    ld a, h
    ld d, b
    add hl, sp
    jr nc, jr_001_5cd7

    ld c, h
    xor $82
    ret nz

    add h
    ld h, b
    ld b, e
    ld sp, $1f26
    inc a
    cp e
    ld h, d
    pop af
    ld b, c
    ld h, c
    ld b, c
    jp $f703


    inc b
    xor $08

jr_001_5c72:
    sbc h
    ld h, b

jr_001_5c74:
    sub b

jr_001_5c75:
    db $10
    ld [$1808], sp
    jr jr_001_5cb7

    ld h, h
    ldh a, [c]
    jp nz, Jump_001_60e3

    add hl, sp
    jr nz, jr_001_5c75

    nop
    nop
    rst $38
    nop
    rst $38
    rst $38
    rst $38
    rst $38
    nop
    rst $38
    nop
    rst $38
    nop
    nop
    rst $38
    nop
    rst $38
    rst $38
    rst $38
    rst $38
    nop
    rst $38
    rst $38
    nop
    rst $38
    nop

jr_001_5c9d:
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    jr c, @+$3a

    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop

jr_001_5cb7:
    nop
    nop
    nop
    nop
    nop
    ld c, $0e
    ld de, $1111
    ld de, $1212
    di
    rra
    ldh a, [$ff3f]
    ldh a, [$ffbf]
    ld h, b
    ld a, a
    db $10
    rra
    ld d, b
    ld e, a
    jr nc, jr_001_5d11

    pop af
    rst $38
    rst $08
    ei
    inc c

jr_001_5cd7:
    db $fc
    ld [$08f8], sp
    ld hl, sp+$08
    ld hl, sp+$08
    ld hl, sp+$08
    ld hl, sp-$78
    ld hl, sp+$4e
    ld a, d
    ret


    reti


    add hl, bc
    ld sp, hl
    ld c, $fe
    ld c, b
    ld hl, sp-$68
    ld hl, sp-$04
    db $fc
    ld a, h
    ld a, h
    and b
    cp a
    ld b, b
    ld a, a
    ldh [rIE], a
    jr nz, jr_001_5d3b

    ld de, $721f
    ld a, [hl]
    rst $38
    rst $38
    rst $38
    rst $38
    nop
    inc a
    nop
    inc e
    nop
    inc e
    nop
    jr jr_001_5d0d

jr_001_5d0d:
    ld [$0000], sp
    nop

jr_001_5d11:
    nop
    nop
    nop
    nop
    rst $38
    nop
    xor e
    nop
    ld d, l
    nop
    rst $38
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    dec d
    nop
    jr jr_001_5d29

jr_001_5d29:
    dec d
    nop
    ld [de], a
    nop
    dec d
    nop
    jr jr_001_5d31

jr_001_5d31:
    dec d
    nop
    ld [de], a
    ld b, b
    ld b, b
    ld b, b
    ret nz

    ld b, b
    ld b, b
    ld b, b

jr_001_5d3b:
    ld b, b
    ld b, b
    ld b, b
    ld b, b
    ret nz

    ld b, b
    ld b, b
    ld b, b
    ld b, b
    ld c, $32
    ld c, $32
    ld c, $32
    ld c, $32
    rrca
    inc sp
    adc a
    or e
    adc $f3
    xor $73
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    add b
    add b
    ret nz

    ld b, b
    nop
    nop
    nop
    nop
    add b
    add b
    ld b, a
    ld b, a
    rra
    jr jr_001_5dae

    jr nz, jr_001_5df0

    ld b, b
    ld a, a
    ld b, b
    ld a, a
    ld b, b
    cp a
    and b
    cp a
    and b
    cp a
    cp b
    ld a, a
    ld a, a
    ccf
    ccf
    ld [hl], a
    ld a, h
    rst $30
    sbc h
    ldh a, [c]
    and $f2
    and $f2
    and $f2
    and $f2
    and $f2
    and $f2
    and $f2
    and $00
    nop
    ld bc, $0101
    ld bc, $0101
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld bc, $f301
    sbc a
    ldh a, [$ff1f]
    ldh a, [$ff3f]
    ldh [$ffbf], a
    ld [hl], b
    ld a, a

jr_001_5dae:
    db $10
    rra
    ld d, b
    ld e, a
    ld sp, $3e3f
    ld [hl+], a
    rra
    rra
    db $10
    rra
    db $10
    rra
    ld [de], a
    rra
    add hl, de
    rra
    ccf
    ccf
    ld a, $3e
    ld [de], a
    ld e, $12
    ld e, $12
    ld e, $12
    ld e, $7e
    ld a, [hl]
    rst $38
    add e
    rst $38
    add c
    rst $38
    rst $38
    ld bc, $0101
    ld bc, $0101
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld bc, $0001
    nop
    ld h, b
    ldh [$ff80], a
    add b
    add b
    add b
    add b
    add b
    add b
    add b
    add b
    add b

jr_001_5df0:
    add b
    add b
    add b
    add b
    rlca
    inc b
    rlca
    inc b
    rlca
    inc b
    rlca
    inc b
    rlca
    inc b
    rlca
    inc b
    rlca
    inc b
    rlca
    inc b
    dec bc
    add hl, bc
    dec bc
    ld a, [bc]
    rrca
    ld a, [bc]
    rla
    ld [de], a
    rla
    inc e
    inc d
    rla
    rla
    inc d
    cpl
    inc h
    nop
    nop
    ld [hl], b
    ld [hl], b
    adc a
    adc a
    sbc b
    sbc a
    ldh [rIE], a
    ldh a, [$ff9f]
    ld a, b
    ld d, a
    ld a, a
    ld c, h
    dec sp
    cpl
    ret nc

    rst $18
    ldh a, [rIE]
    ret nz

    rst $38
    ret nz

    rst $38
    rst $38
    rst $38
    nop
    nop
    nop
    nop
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    ld hl, sp-$08
    ldh a, [$fff2]
    pop hl
    push af
    db $e3
    ldh a, [c]
    and $ff
    rst $38
    rst $38
    add c
    jp $df81


    add l
    rst $18
    add l
    rst $38
    cp l
    rst $38
    add c
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rra
    rra
    rrca
    ld c, a
    add a
    xor a
    rst $00
    ld c, a
    ld h, a
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    nop
    nop
    nop
    nop
    rst $38
    rst $38
    rst $38
    nop
    nop
    ld c, a
    ld h, a
    ld c, a
    ld h, a
    ld c, a
    ld h, a
    ld c, a
    ld h, a
    ld c, a
    ld h, a
    ld c, a
    ld h, a
    ld c, a
    ld h, a
    ld c, a
    ld h, a
    ldh a, [c]
    and $f5
    db $e3
    ldh a, [c]
    pop hl
    ld hl, sp-$10
    rst $38
    ld hl, sp-$01
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    nop
    nop
    rst $38
    rst $38
    nop
    rst $38
    nop
    nop
    rst $38
    nop
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    ld c, a
    ld h, a
    xor a
    rst $00
    ld c, a
    add a
    rra
    rrca
    rst $38
    rra
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    nop
    nop
    nop
    nop
    rst $28
    rst $20
    rst $08
    inc h
    inc c
    inc h
    inc c
    inc h
    inc c
    inc h
    inc c
    inc h
    inc c
    inc h
    inc c
    inc h
    inc c
    inc h
    inc c
    inc h
    inc c
    inc h
    inc c
    rst $20
    rst $08
    nop
    rst $28
    nop
    nop
    rst $38
    nop
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rlca
    rlca
    jr jr_001_5f07

    ld hl, $473e
    ld a, a
    ld e, a
    ld a, a
    add hl, sp
    jr nc, jr_001_5f6c

    ld h, d
    ei
    or d
    ldh [$ffe0], a
    jr @-$06

    add h
    ld a, h
    ldh [c], a
    cp $fa
    cp $9c
    inc c
    sbc $46
    rst $18
    ld c, l
    rst $38
    and b
    rst $38

jr_001_5f07:
    jp nz, Jump_001_547f

    ld a, a
    ld e, h
    ccf
    ld l, $3f
    inc hl
    rra
    jr jr_001_5f1a

    rlca
    rst $38
    dec b
    rst $38
    ld b, e
    cp $2a

jr_001_5f1a:
    cp $3a
    db $fc
    ld [hl], h
    db $fc
    call nz, $18f8
    ldh [$ffe0], a
    rlca
    rlca
    rra
    jr jr_001_5f67

    jr nz, jr_001_5faa

    ld c, a
    ld a, a
    ld e, a
    ld [hl], b
    ld [hl], b
    and d
    and d
    or b
    or b
    ldh [$ffe0], a
    ld hl, sp+$18
    ld a, h
    inc b
    cp $f2
    cp $fa
    ld c, $0e
    ld b, l
    ld b, l
    dec c
    dec c
    or h
    or h
    ld h, h
    ld h, h
    inc a
    inc a
    ld l, $2e
    daa
    daa
    db $10
    db $10
    inc c
    inc c
    inc bc
    inc bc
    dec l
    dec l
    ld h, $26
    inc a
    inc a
    ld [hl], h
    ld [hl], h
    db $e4
    db $e4
    ld [$3008], sp
    jr nc, @-$3e

    ret nz

    cpl
    inc h
    cpl

jr_001_5f67:
    inc h
    cpl
    inc h
    cpl
    inc h

jr_001_5f6c:
    ld h, a
    ld a, h
    cp h
    and a
    rst $38
    db $e4

jr_001_5f72:
    dec de
    dec de
    nop
    nop
    nop
    nop
    ld bc, $0101
    ld bc, $0303
    inc bc
    inc bc
    inc bc
    ld [bc], a
    rlca
    inc b
    inc b
    rlca
    rlca
    inc b
    rlca
    inc b
    inc b
    inc b
    ld b, $06
    dec b
    dec b
    dec b
    dec b
    ld b, $06
    rlca
    inc b
    rlca
    inc b
    inc b
    rlca
    inc b
    inc b
    inc b
    inc b
    rlca
    rlca
    rlca
    rlca
    ld b, $06
    ld b, $06
    ld b, $06
    inc b
    inc b

jr_001_5faa:
    rlca
    rlca
    dec b
    dec b
    inc bc
    inc bc
    dec b
    dec b
    ld c, $0e
    rrca
    rra
    ld bc, $0110
    db $10
    ld bc, $0110
    ld [$0701], sp
    inc b
    add hl, bc
    nop
    rrca
    ld [$f801], sp
    pop af
    ld c, [hl]
    pop bc
    ld [bc], a
    rst $00
    adc h
    cp l
    add h
    xor l

jr_001_5fd0:
    ld h, d
    rst $08
    ld a, [hl]
    cp $ec
    sub b
    rst $28
    sbc a
    ld a, [$daf7]
    rst $20
    cp l
    cp l
    or l
    xor l
    jp nc, Jump_001_7fef

jr_001_5fe3:
    ld a, a
    ld hl, sp-$08
    jr jr_001_5fd0

    jr c, jr_001_5f72

    cp b
    ld [$10b0], sp
    ldh [$ffe0], a
    ret nc

    jr nc, jr_001_5fe3

    ldh a, [rNR23]
    jr jr_001_6027

jr_001_5ff7:
    jr nc, jr_001_6059

    ld h, b
    ret nz

    ret nz

    ret nz

    ret nz

    rst $38
    rst $38
    add e

jr_001_6001:
    add e
    ld h, b
    ld h, d
    ld a, [bc]
    nop
    ld [$0800], sp
    rlca
    ld [$0800], sp
    ld bc, $f1f8
    ld hl, sp-$0f
    ld [$6c01], sp
    db $10
    inc l
    db $10
    inc l
    pop de
    inc l
    ld de, $90ac
    rst $28
    sbc a
    rst $28
    sbc a
    db $ec
    sub b
    jr jr_001_603e

    inc c

jr_001_6027:
    inc c
    ld b, $c6
    inc bc
    jp Jump_000_0303


    rst $38
    rst $38
    pop bc
    pop bc
    ld b, $46
    nop
    inc b
    nop
    inc c
    ld [bc], a
    db $10
    ld [bc], a
    db $10
    ld [bc], a
    db $10

jr_001_603e:
    ld [bc], a
    db $10
    ld [bc], a
    db $10
    ld [bc], a
    db $10
    inc c
    ld c, h
    inc c
    ld c, h
    add hl, bc
    ld c, c
    dec bc
    ld c, e
    ld a, [bc]
    ld c, d
    db $10
    ld d, b
    ld [de], a
    ld d, d
    db $10
    ld d, b
    ld a, [hl]
    inc sp
    ld a, [hl]
    inc sp
    cp [hl]

jr_001_6059:
    sub e
    cp $d3
    ld a, [hl]
    ld d, e
    ld a, $0b
    ld a, [hl]
    ld c, e
    ld a, $0b
    and b
    jr nz, jr_001_5ff7

    jr nc, jr_001_6001

    ld c, b
    sbc b
    ld c, b
    sbc b
    ld c, b
    sbc b
    ld c, b
    sbc b
    ld c, b
    sbc b
    ld c, b
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    ld bc, $0101
    ld bc, $0202
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    nop
    ld bc, $0202
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    inc bc
    ld [bc], a
    inc bc
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    ld [bc], a
    inc bc
    ld [bc], a
    ld [bc], a
    ld b, $06
    ld c, $0a
    ld c, $0a
    dec bc
    ld a, [bc]
    dec bc
    ld a, [bc]
    rrca
    ld a, [bc]
    ld a, [bc]
    ld a, [bc]
    ld b, $06
    ld a, [bc]
    ld a, [bc]
    ld a, [de]
    ld [de], a
    rra
    rra
    nop
    nop
    nop
    nop
    rra
    rra
    ccf
    jr nz, jr_001_613c

    ld b, a
    ld a, h
    ld c, h
    ld a, h
    ld c, h
    ld a, h
    ld c, h
    nop
    nop
    nop
    nop
    ldh [$ffe0], a
    ldh a, [$ff30]
    ld hl, sp+$18
    ld hl, sp-$68
    ld hl, sp-$68
    ld hl, sp-$68
    ld a, a
    ld c, a
    ld a, a
    ld b, b
    ld a, a
    ld c, a
    ld a, h
    ld c, h
    ld a, h
    ld c, h
    ld a, h
    ld a, h
    nop
    nop
    nop

Jump_001_60e3:
    nop
    ld hl, sp-$68
    ld hl, sp+$18
    ld hl, sp-$68
    ld hl, sp-$68
    ld hl, sp-$68
    ld hl, sp-$08
    nop
    nop
    nop
    nop
    nop
    nop
    ld a, h
    ld a, h
    ld c, [hl]
    ld c, [hl]
    ld c, [hl]
    ld c, [hl]
    ld c, [hl]
    ld c, [hl]
    ld c, [hl]
    ld c, [hl]
    ld a, h
    ld a, h
    nop
    nop
    nop
    nop
    ld a, [hl]
    ld a, [hl]
    ld h, b
    ld h, b
    ld a, h
    ld a, h
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld a, [hl]
    ld a, [hl]
    nop
    nop
    nop
    nop
    ld b, [hl]
    ld b, [hl]
    ld b, [hl]
    ld b, [hl]
    ld b, [hl]
    ld b, [hl]
    ld b, [hl]
    ld b, [hl]
    ld c, [hl]
    ld c, [hl]
    inc a
    inc a
    nop
    nop
    nop
    nop
    inc a
    inc a
    ld h, [hl]
    ld h, [hl]
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld h, [hl]
    ld h, [hl]
    inc a
    inc a
    nop
    nop
    nop
    nop
    ld b, [hl]
    ld b, [hl]
    ld l, [hl]
    ld l, [hl]
    ld a, [hl]
    ld a, [hl]

jr_001_613c:
    ld d, [hl]
    ld d, [hl]
    ld b, [hl]
    ld b, [hl]
    ld b, [hl]
    ld b, [hl]
    nop
    nop
    nop
    nop
    inc a
    inc a
    ld c, [hl]
    ld c, [hl]
    ld c, [hl]
    ld c, [hl]
    ld a, [hl]
    ld a, [hl]
    ld c, [hl]
    ld c, [hl]
    ld c, [hl]
    ld c, [hl]
    nop
    nop
    rst $38
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    ld bc, $ff01
    ld bc, $ff01
    rst $38
    rst $38
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    ldh a, [$fff0]
    ldh a, [$ffb0]
    ldh a, [$ffb0]
    ldh a, [$fff0]
    nop
    nop
    nop
    nop
    rlca
    rlca
    jr jr_001_61ab

    jr nz, jr_001_61cd

    jr nc, jr_001_61cf

    jr jr_001_61a9

    ccf
    inc l
    ld a, e
    ld c, a
    ld [hl], b
    ld e, a
    sub b
    sbc a
    sub b
    sbc a
    ld [hl], b
    ld a, a
    ld de, $3e1f
    ld a, $3e
    ld a, $00
    nop
    ld a, h
    ld a, h
    ld h, [hl]

jr_001_61a9:
    ld h, [hl]
    ld h, [hl]

jr_001_61ab:
    ld h, [hl]
    ld a, h
    ld a, h
    ld l, b
    ld l, b
    ld h, [hl]
    ld h, [hl]
    nop
    nop
    nop
    nop
    inc a
    inc a
    ld h, [hl]
    ld h, [hl]
    ld h, [hl]
    ld h, [hl]
    ld h, [hl]
    ld h, [hl]
    ld h, [hl]
    ld h, [hl]
    inc a
    inc a
    nop
    nop
    nop
    nop
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld h, b
    ld h, b

jr_001_61cd:
    ld h, b
    ld h, b

jr_001_61cf:
    ld h, b
    ld a, [hl]
    ld a, [hl]
    nop
    nop
    nop
    nop
    inc a
    inc a
    ld h, [hl]
    ld h, [hl]
    ld h, b
    ld h, b
    ld l, [hl]
    ld l, [hl]
    ld h, [hl]
    ld h, [hl]
    ld a, $3e
    nop
    nop
    nop
    xor $00
    nop
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    nop
    ld bc, $0200
    nop
    ld [bc], a
    nop
    inc b
    nop
    ld [$0800], sp
    nop
    stop
    db $10
    add b
    add b
    ret nz

    ld b, b
    ret nz

    ld b, b
    ldh [rNR41], a
    jr nc, jr_001_625e

    jr nc, jr_001_6260

    jr c, jr_001_625a

    jr jr_001_623c

    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    inc bc
    nop
    inc bc
    nop
    ld [bc], a
    nop
    ld [bc], a
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    ld [$08f8], sp
    jr jr_001_6239

    xor b
    ld [$0048], sp
    add b
    nop
    add b
    nop

jr_001_6239:
    add b
    nop
    add b

jr_001_623c:
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    jr nz, jr_001_6247

jr_001_6247:
    jr nz, jr_001_6249

jr_001_6249:
    jr nz, jr_001_626a

    jr nz, jr_001_624d

jr_001_624d:
    ld b, b
    nop
    ld b, b
    nop
    ld b, b
    nop
    ld b, b
    inc e
    inc h
    inc c
    inc [hl]
    inc c
    inc [hl]

jr_001_625a:
    inc b
    db $fc
    ld c, $32

jr_001_625e:
    ld c, $32

jr_001_6260:
    ld c, $32
    ld c, $32
    nop
    nop
    nop
    nop
    nop
    nop

jr_001_626a:
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    rra
    nop
    jr jr_001_6281

jr_001_6281:
    dec d
    nop
    ld [de], a
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    ld b, b
    ret nz

    ld b, b
    ret nz

    ld b, b
    ld b, b
    ld b, b
    ld b, b
    nop
    ld [bc], a
    nop
    inc bc
    nop
    ld [bc], a
    nop
    ld [bc], a
    nop
    ld [bc], a
    nop
    inc bc
    nop
    ld [bc], a
    nop
    ld [bc], a
    ld [$08af], sp
    ld a, [de]
    ld [$08ad], sp
    ld c, a
    ld [$08a8], sp
    jr jr_001_62b9

    xor b
    ld [$0048], sp
    nop
    nop
    nop
    nop

jr_001_62b9:
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    ld bc, $0200
    nop
    ld b, b
    dec d
    ld b, b
    dec d
    ld b, b
    dec d
    ld b, b
    dec d
    ret nz

    dec d
    pop bc
    rla
    ld b, e
    ld d, $46
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    rst $38
    nop
    ld a, [hl+]
    jr nz, jr_001_6335

    nop

jr_001_6335:
    dec e
    ld bc, $0009
    rlca
    ld bc, $000b
    inc bc
    jr nz, @+$06

    nop
    jr nz, @+$22

    ld b, $00
    ld a, [bc]
    add b
    rla
    nop
    ld b, $01
    ld b, $00
    inc b
    ld bc, $0005
    ld e, $80
    dec bc
    nop
    ld b, $80
    inc e
    nop
    ld a, [bc]
    db $10
    ld [$0411], sp
    ld bc, $0002
    inc b
    ld bc, $0006
    nop
    db $10
    ld b, $00
    inc b
    db $10
    dec b
    nop
    ld a, [de]
    add b
    inc h
    nop
    dec d
    ld bc, $0007
    jr nz, jr_001_6387

    inc b
    nop
    dec b
    db $10
    inc bc
    nop
    dec c
    db $10
    ld b, $00
    inc bc
    db $10
    dec b
    nop
    dec h
    add b

jr_001_6387:
    dec d
    nop
    dec de
    db $10
    inc b
    nop
    inc de
    add b
    inc bc
    nop
    inc e
    add b
    add hl, de
    nop
    ld a, [de]
    ld bc, $0006
    ld a, [bc]
    jr nz, jr_001_639d

    nop

jr_001_639d:
    add hl, bc
    jr nz, jr_001_63a2

    nop
    inc d

jr_001_63a2:
    db $10
    inc bc
    nop
    ld c, $80
    ld d, $00
    ld a, [bc]
    db $10
    ld a, [bc]
    ld de, $1006
    ld d, $00
    inc de
    add b
    dec h
    nop
    inc e
    ld bc, $0006
    inc bc
    jr nz, @+$04

    nop
    ld c, $20
    inc bc
    nop
    inc b
    jr nz, jr_001_63c6

    nop
    inc bc

jr_001_63c6:
    jr nz, @+$07

    nop
    dec c
    add b
    ld hl, $1300
    ld bc, $0007
    dec b
    ld bc, $0006
    inc b
    ld bc, $0005
    ld b, $20
    inc bc
    nop
    dec b
    jr nz, jr_001_63e2

    nop
    inc e

jr_001_63e2:
    jr nz, jr_001_63e7

    nop
    ld c, $80

jr_001_63e7:
    ld [de], a
    nop
    inc c
    db $10
    inc b
    nop
    ld [bc], a
    ld bc, $0008
    db $10
    ld bc, $0008
    ld e, $80
    add hl, de
    nop
    db $10
    db $10
    inc bc
    nop
    inc b
    db $10
    dec b
    nop
    inc h
    add b
    inc e
    nop
    dec b
    ld bc, $0005
    ld de, $0320
    nop
    ld [de], a
    add b
    jr nz, jr_001_6411

jr_001_6411:
    ld a, [bc]
    db $10
    ld bc, $0611
    ld bc, $0000
    inc b
    db $10
    inc b
    nop
    inc b
    db $10
    inc bc
    nop
    ld [bc], a
    db $10
    add hl, de
    nop
    inc b
    db $10
    rlca
    nop
    ld a, [bc]
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    ld c, l
    jr nz, jr_001_643c

    ld hl, $2006
    dec bc
    nop
    rlca
    jr nz, @+$08

jr_001_643c:
    nop
    ld h, h
    stop
    ld de, $1006
    dec b
    nop
    cpl
    add b
    ld d, $00
    rla
    jr nz, jr_001_6451

    nop
    ld b, $20
    ld b, $00

jr_001_6451:
    db $10
    add b
    jr jr_001_6455

jr_001_6455:
    inc [hl]
    ld bc, $0005
    ld bc, $0e10
    ld de, $1006
    jr nz, jr_001_6461

jr_001_6461:
    ld a, [bc]
    add b
    ld a, [bc]
    nop
    dec hl
    jr nz, jr_001_646e

    nop
    ld b, $20
    dec b
    nop
    dec b

jr_001_646e:
    jr nz, jr_001_6476

    nop
    ld a, [bc]
    add b
    inc c
    nop
    ld a, [bc]

jr_001_6476:
    ld bc, $0007
    ld [bc], a
    db $10
    dec bc
    nop
    dec b
    db $10
    inc b
    nop
    dec c
    add b
    inc e
    nop
    ld [hl], l
    ld bc, $0006
    ld c, $80
    rra
    nop
    ld a, [de]
    ld bc, $0006
    nop
    db $10
    rlca
    nop
    dec b
    db $10
    ld b, $00
    inc b
    db $10
    ld [$0300], sp
    db $10
    ld [$0c00], sp
    add b
    rrca
    nop
    ld a, [bc]
    ld bc, $0007
    nop
    db $10
    dec a
    nop
    dec b
    add b
    rra
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop

    db $10, $18, $00, $04, $08, $00, $04, $08, $08, $00, $04, $14, $10, $08, $10, $10
    db $14, $18, $14, $00, $0c, $04, $18, $00, $14, $14, $08, $04, $04, $0c, $00, $18
    db $04, $00, $08, $0c, $0c, $18, $00, $0c, $08, $00, $18, $10, $14, $14, $18, $08
    db $24, $66, $41, $66, $3d, $67, $a3, $66, $71, $67, $f4, $66, $6c, $66, $bc, $66
    db $2c, $66, $49, $66, $51, $67, $49, $66, $49, $66, $04, $67, $72, $66, $c4, $66
    db $11, $68, $19, $68, $da, $67, $e2, $67, $21, $68, $21, $68, $21, $68, $ea, $67
    db $70, $6f, $7b, $6f, $86, $6f, $91, $6f, $9c, $6f, $a7, $6f, $b2, $6f, $bd, $6f
    db $c8, $6f, $d3, $6f, $de, $6f, $e9, $6f, $f4, $6f, $ff, $6f, $0a, $70, $15, $70
    db $20, $70

Call_001_6552:
    ret


Jump_001_6553:
    push af
    push bc
    push de
    push hl
    ld a, [$df7f]
    cp $01
    jr z, jr_001_65a4

    cp $02
    jr z, jr_001_65dd

    ld a, [$df7e]
    and a
    jr nz, jr_001_65e3

jr_001_6568:
    ldh a, [$ffe4]
    and a
    jr z, jr_001_657a

    db $af, $ea, $e0, $df, $ea, $e8, $df, $ea, $f0, $df, $ea, $f8, $df

jr_001_657a:
    call Call_001_6552
    call Call_001_6a0e
    call Call_001_6a2e
    call Call_001_6879
    call Call_001_6a52
    call Call_001_6c75
    call Call_001_6a96

jr_001_658f:
    xor a
    ld [$dfe0], a
    ld [$dfe8], a
    ld [$dff0], a
    ld [$dff8], a
    ld [$df7f], a
    pop hl
    pop de
    pop bc
    pop af
    ret


jr_001_65a4:
    call Call_001_69f8
    xor a
    ld [$dfe1], a
    ld [$dff1], a
    ld [$dff9], a
    ld hl, $dfbf
    res 7, [hl]
    ld hl, $df9f
    res 7, [hl]
    ld hl, $dfaf
    res 7, [hl]
    ld hl, $dfcf
    res 7, [hl]
    ld hl, $6f1a
    call Call_001_69c9
    ld a, $30
    ld [$df7e], a

jr_001_65d0:
    ld hl, $65fb

jr_001_65d3:
    call Call_001_698e
    jr jr_001_658f

jr_001_65d8:
    ld hl, $65ff
    jr jr_001_65d3

jr_001_65dd:
    xor a
    ld [$df7e], a
    jr jr_001_6568

jr_001_65e3:
    ld hl, $df7e
    dec [hl]
    ld a, [hl]
    cp $28
    jr z, jr_001_65d8

    cp $20
    jr z, jr_001_65d0

    cp $18
    jr z, jr_001_65d8

    cp $10
    jr nz, jr_001_658f

    inc [hl]
    jr jr_001_658f

    db $b2, $e3, $83, $c7, $b2, $e3, $c1, $c7

Call_001_6603:
    ld a, [$dff1]
    cp $01
    ret


Call_001_6609:
    ld a, [$dfe1]
    cp $05
    ret


Call_001_660f:
    ld a, [$dfe1]
    cp $07
    ret


    db $00, $b5, $d0, $40, $c7, $00, $b5, $20, $40, $c7, $00, $b6, $a1, $80, $c7

    ld a, $05
    ld hl, $6615
    jp Jump_001_6967


    call Call_001_69bc
    and a
    ret nz

    ld hl, $dfe4
    inc [hl]
    ld a, [hl]
    cp $02
    jp z, Jump_001_664e

    ld hl, $661a
    jp Jump_001_6987


    ld a, $03
    ld hl, $661f
    jp Jump_001_6967


    call Call_001_69bc
    and a
    ret nz

Jump_001_664e:
    xor a
    ld [$dfe1], a
    ldh [rNR10], a
    ld a, $08
    ldh [rNR12], a
    ld a, $80
    ldh [rNR14], a
    ld hl, $df9f
    res 7, [hl]
    ret


    nop
    add b
    pop hl
    pop bc
    add a
    nop
    add b
    pop hl
    xor h
    add a
    ld hl, $6662
    jp Jump_001_6967


    ld hl, $dfe4
    inc [hl]
    ld a, [hl]
    cp $04
    jr z, jr_001_6692

    cp $0b
    jr z, jr_001_6698

    cp $0f
    jr z, jr_001_6692

    cp $18
    jp z, Jump_001_6689

    ret


Jump_001_6689:
    ld a, $01
    ld hl, $dff0
    ld [hl], a
    jp Jump_001_664e


jr_001_6692:
    ld hl, $6667
    jp Jump_001_6987


jr_001_6698:
    ld hl, $6662
    jp Jump_001_6987


    db $48, $bc, $42, $66, $87

    call Call_001_6603
    ret z

    call Call_001_660f
    ret z

    call Call_001_6609
    ret z

    ld a, $02
    ld hl, $669e
    jp Jump_001_6967


    nop
    or b
    pop hl
    or b
    rst $00
    ld a, $07
    ld hl, $66b7
    jp Jump_001_6967


    call Call_001_69bc
    and a
    ret nz

    ld hl, $66b7
    call Call_001_6987
    ld hl, $dfe4
    inc [hl]
    ld a, [hl]
    cp $03
    jp z, Jump_001_664e

    ret


    db $3e, $80, $e3, $00, $c4, $93, $83, $83, $73, $63, $53, $43, $33, $23, $13, $00
    db $00, $23, $43, $63, $83, $a3, $c3, $d3, $e3, $ff

    call Call_001_6603
    ret z

    call Call_001_660f
    ret z

    ld a, $06
    ld hl, $66da
    jp Jump_001_6967


    call Call_001_69bc
    and a
    ret nz

    ld hl, $dfe4
    ld c, [hl]
    inc [hl]
    ld b, $00
    ld hl, $66df
    add hl, bc
    ld a, [hl]
    and a
    jp z, Jump_001_664e

    ld e, a
    ld hl, $66ea
    add hl, bc
    ld a, [hl]
    ld d, a
    ld b, $86

jr_001_6722:
    ld c, $12
    ld a, e
    ldh [c], a
    inc c
    ld a, d
    ldh [c], a
    inc c
    ld a, b
    ldh [c], a
    ret


    db $3b, $80, $b2, $87, $87, $a2, $93, $62, $43, $23, $00, $80, $40, $80, $40, $80

    call Call_001_6603
    ret z

    call Call_001_660f
    ret z

    call Call_001_6609
    ret z

    ld a, $03
    ld hl, $672d
    jp Jump_001_6967


    call Call_001_69bc
    and a
    ret nz

    ld hl, $dfe4
    ld c, [hl]
    inc [hl]
    ld b, $00
    ld hl, $6732
    add hl, bc
    ld a, [hl]
    and a
    jp z, Jump_001_664e

    ld e, a
    ld hl, $6738
    add hl, bc
    ld a, [hl]
    ld d, a
    ld b, $87
    jr jr_001_6722

    call Call_001_660f
    ret z

    ld a, $28
    ld hl, $677d
    jp Jump_001_6967


    or a
    add b
    sub b
    rst $38
    add e

    db $00, $d1, $45, $80, $00, $f1, $54, $80

    nop
    push de
    ld h, l
    add b
    nop
    ld [hl], b
    ld h, [hl]
    add b
    ld h, l
    ld h, l
    ld h, l
    ld h, h
    ld d, a
    ld d, [hl]
    ld d, l
    ld d, h
    ld d, h
    ld d, h
    ld d, h
    ld d, h
    ld b, a
    ld b, [hl]
    ld b, [hl]
    ld b, l
    ld b, l
    ld b, l
    ld b, h
    ld b, h
    ld b, h
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    inc [hl]
    ld [hl], b
    ld h, b
    ld [hl], b
    ld [hl], b
    ld [hl], b
    add b
    sub b
    and b
    ret nc

    ldh a, [$ffe0]
    ret nc

    ret nz

    or b
    and b
    sub b
    add b
    ld [hl], b
    ld h, b
    ld d, b
    ld b, b
    jr nc, jr_001_67fd

    jr nz, jr_001_67ef

    jr nz, @+$22

    jr nz, jr_001_67f3

    jr nz, @+$22

    jr nz, @+$22

    jr nz, @+$12

    db $10
    ld a, $30
    ld hl, $678a
    jp Jump_001_6967


    ld a, $30
    ld hl, $678e
    jp Jump_001_6967


    call Call_001_69bc
    and a
    ret nz

jr_001_67ef:
    ld hl, $dffc
    ld a, [hl]

jr_001_67f3:
    ld c, a
    cp $24
    jp z, Jump_001_6826

    inc [hl]
    ld b, $00
    push bc

jr_001_67fd:
    ld hl, $6792
    add hl, bc
    ld a, [hl]
    ldh [rNR43], a
    pop bc
    ld hl, $67b6
    add hl, bc
    ld a, [hl]
    ldh [rNR42], a
    ld a, $80
    ldh [rNR44], a
    ret


    ld a, $20
    ld hl, $6786
    jp Jump_001_6967


    ld a, $12
    ld hl, $6782
    jp Jump_001_6967


    call Call_001_69bc
    and a
    ret nz

Jump_001_6826:
    xor a
    ld [$dff9], a
    ld a, $08
    ldh [rNR42], a
    ld a, $80
    ldh [rNR44], a
    ld hl, $dfcf

jr_001_6835:
    res 7, [hl]
    ret


    db $80, $3a, $20, $60, $c6

Jump_001_683d:
    ld hl, $6f0a
    call Call_001_693e
    ldh a, [rDIV]
    and $1f
    ld b, a
    ld a, $d0
    add b
    ld [$dff5], a
    ld hl, $6838
    jp Jump_001_6995


Jump_001_6854:
    ldh a, [rDIV]
    and $0f
    ld b, a
    ld hl, $dff4
    inc [hl]
    ld a, [hl]
    ld hl, $dff5
    cp $0e
    jr nc, jr_001_686f

    inc [hl]
    inc [hl]

jr_001_6867:
    ld a, [hl]
    and $f0
    or b
    ld c, $1d
    ldh [c], a
    ret


jr_001_686f:
    cp $1e
    jp z, Jump_001_691f

    dec [hl]
    dec [hl]
    dec [hl]
    jr jr_001_6867

Call_001_6879:
    ld a, [$dff0]
    cp $01
    jp z, Jump_001_68a8

    cp $02
    jp z, Jump_001_683d

    ld a, [$dff1]
    cp $01
    jp z, Jump_001_68f3

    cp $02
    jp z, Jump_001_6854

    ret


    add b
    add b
    jr nz, jr_001_6835

    add a
    add b
    ld hl, sp+$20
    sbc b
    add a
    add b
    ei
    jr nz, @-$68

    add a
    add b
    or $20
    sub l
    add a

Jump_001_68a8:
    ld hl, $6eda
    call Call_001_693e
    ld hl, $6897
    ld a, [hl]
    ld [$dff6], a
    ld a, $01
    ld [$dff5], a
    ld hl, $6894

jr_001_68bd:
    jp Jump_001_6995


jr_001_68c0:
    ld a, $00
    ld [$dff5], a
    ld hl, $689c
    ld a, [hl]
    ld [$dff6], a
    ld hl, $6899
    jr jr_001_68bd

jr_001_68d1:
    ld a, $01
    ld [$dff5], a
    ld hl, $68a1
    ld a, [hl]
    ld [$dff6], a
    ld hl, $689e
    jr jr_001_68bd

jr_001_68e2:
    ld a, $02
    ld [$dff5], a
    ld hl, $68a6
    ld a, [hl]
    ld [$dff6], a
    ld hl, $68a3
    jr jr_001_68bd

Jump_001_68f3:
    ld hl, $dff4
    inc [hl]
    ld a, [hl+]
    cp $09
    jr z, jr_001_68c0

    cp $13
    jr z, jr_001_68d1

    cp $17
    jr z, jr_001_68e2

    cp $20
    jr z, jr_001_691f

    ld a, [hl+]
    cp $00
    ret z

    cp $01
    jr z, jr_001_6915

    cp $02
    jr z, jr_001_6919

    ret


jr_001_6915:
    inc [hl]
    inc [hl]
    jr jr_001_691b

jr_001_6919:
    dec [hl]
    dec [hl]

jr_001_691b:
    ld a, [hl]
    ldh [rNR33], a
    ret


Jump_001_691f:
jr_001_691f:
    xor a
    ld [$dff1], a
    ldh [rNR30], a
    ld hl, $dfbf
    res 7, [hl]
    ld hl, $df9f
    res 7, [hl]
    ld hl, $dfaf
    res 7, [hl]
    ld hl, $dfcf
    res 7, [hl]
    ld hl, $6f1a
    jr jr_001_6963

Call_001_693e:
    push hl
    ld [$dff1], a
    ld hl, $dfbf
    set 7, [hl]
    xor a
    ld [$dff4], a
    ld [$dff5], a
    ld [$dff6], a
    ldh [rNR30], a
    ld hl, $df9f
    set 7, [hl]
    ld hl, $dfaf
    set 7, [hl]
    ld hl, $dfcf
    set 7, [hl]
    pop hl

jr_001_6963:
    call Call_001_69c9
    ret


Jump_001_6967:
    push af
    dec e
    ld a, [$df71]
    ld [de], a
    inc e
    pop af
    inc e
    ld [de], a
    dec e
    xor a
    ld [de], a
    inc e
    inc e
    ld [de], a
    inc e
    ld [de], a
    ld a, e
    cp $e5
    jr z, jr_001_6987

    cp $f5
    jr z, jr_001_6995

    cp $fd
    jr z, jr_001_699c

    ret


Call_001_6987:
Jump_001_6987:
jr_001_6987:
    push bc
    ld c, $10
    ld b, $05
    jr jr_001_69a1

Call_001_698e:
    push bc
    ld c, $16
    ld b, $04
    jr jr_001_69a1

Jump_001_6995:
jr_001_6995:
    push bc
    ld c, $1a
    ld b, $05
    jr jr_001_69a1

jr_001_699c:
    push bc
    ld c, $20
    ld b, $04

jr_001_69a1:
    ld a, [hl+]
    ldh [c], a
    inc c
    dec b
    jr nz, jr_001_69a1

    pop bc
    ret


Call_001_69a9:
    inc e
    ld [$df71], a

Call_001_69ad:
    inc e
    dec a
    sla a
    ld c, a
    ld b, $00
    add hl, bc
    ld c, [hl]
    inc hl
    ld b, [hl]
    ld l, c
    ld h, b
    ld a, h
    ret


Call_001_69bc:
    push de
    ld l, e
    ld h, d
    inc [hl]
    ld a, [hl+]
    cp [hl]
    jr nz, jr_001_69c7

    dec l
    xor a
    ld [hl], a

jr_001_69c7:
    pop de
    ret


Call_001_69c9:
    push bc
    ld c, $30

jr_001_69cc:
    ld a, [hl+]
    ldh [c], a
    inc c
    ld a, c
    cp $40
    jr nz, jr_001_69cc

    pop bc
    ret


Call_001_69d6:
Jump_001_69d6:
    xor a
    ld [$dfe1], a
    ld [$dfe9], a
    ld [$dff1], a
    ld [$dff9], a
    ld [$df9f], a
    ld [$dfaf], a
    ld [$dfbf], a
    ld [$dfcf], a

    ld a, $ff ; Pan to both channels
    ldh [rNR51], a

    ; ???
    ld a, $03
    ld [$df78], a

Call_001_69f8:
    ld a, $08
    ldh [rNR12], a
    ldh [rNR22], a
    ldh [rNR42], a
    ld a, $80
    ldh [rNR14], a
    ldh [rNR24], a
    ldh [rNR44], a
    xor a
    ldh [rNR10], a
    ldh [rNR30], a
    ret


Call_001_6a0e:
    ld de, $dfe0
    ld a, [de]
    and a
    jr z, jr_001_6a21

    ld hl, $df9f
    set 7, [hl]
    ld hl, $6500
    call Call_001_69a9
    jp hl


jr_001_6a21:
    inc e
    ld a, [de]
    and a
    jr z, jr_001_6a2d

    ld hl, $6510
    call Call_001_69ad
    jp hl


jr_001_6a2d:
    ret


Call_001_6a2e:
    ld de, $dff8
    ld a, [de]
    and a
    jr z, jr_001_6a41

    ld hl, $dfcf
    set 7, [hl]
    ld hl, $6520
    call Call_001_69a9
    jp hl


jr_001_6a41:
    inc e
    ld a, [de]
    and a
    jr z, jr_001_6a4d

    ld hl, $6528
    call Call_001_69ad
    jp hl


jr_001_6a4d:
    ret


jr_001_6a4e:
    call Call_001_69d6
    ret


Call_001_6a52:
    ld hl, $dfe8
    ld a, [hl+]
    and a
    ret z

    cp $ff
    jr z, jr_001_6a4e

    ld [hl], a
    ld b, a
    ld hl, $6530
    and $1f
    call Call_001_69ad
    call Call_001_6b44
    call Call_001_6a6d
    ret


Call_001_6a6d:
    ld a, [$dfe9]
    and a
    ret z

    ld hl, $6aef

jr_001_6a75:
    dec a
    jr z, jr_001_6a7e

    inc hl
    inc hl
    inc hl
    inc hl
    jr jr_001_6a75

jr_001_6a7e:
    ld a, [hl+]
    ld [$df78], a
    ld a, [hl+]
    ld [$df76], a
    ld a, [hl+]
    ld [$df79], a
    ld a, [hl+]
    ld [$df7a], a
    xor a
    ld [$df75], a
    ld [$df77], a
    ret


Call_001_6a96:
    ld a, [$dfe9]
    and a
    jr z, jr_001_6ad9

    ld hl, $df75
    ld a, [$df78]
    cp $01
    jr z, jr_001_6add

    cp $03
    jr z, jr_001_6ad9

    inc [hl]
    ld a, [hl+]
    cp [hl]
    jr nz, jr_001_6ae2

    dec l
    ld [hl], $00
    inc l
    inc l
    inc [hl]
    ld a, [$df79]
    bit 0, [hl]
    jp z, Jump_001_6ac0

    ld a, [$df7a]

Jump_001_6ac0:
jr_001_6ac0:
    ld b, a
    ld a, [$dff1]
    and a
    jr z, jr_001_6acb

    set 2, b
    set 6, b

jr_001_6acb:
    ld a, [$dff9]
    and a
    jr z, jr_001_6ad5

    set 3, b
    set 7, b

jr_001_6ad5:
    ld a, b

jr_001_6ad6:
    ldh [rNR51], a
    ret


jr_001_6ad9:
    ld a, $ff
    jr jr_001_6ad6

jr_001_6add:
    ld a, [$df79]
    jr jr_001_6ac0

jr_001_6ae2:
    ld a, [$dff9]
    and a
    jr nz, jr_001_6ad9

    ld a, [$dff1]
    and a
    jr nz, jr_001_6ad9

    ret


    db $01, $24, $ef, $56

    ld bc, $e500
    nop

    db $01, $20, $fd, $00, $01, $20, $de, $f7, $02, $18, $7f, $f7

    inc bc
    jr @-$07

    ld a, a
    inc bc
    ld c, b
    rst $18
    ld e, e
    ld bc, $db18
    rst $20
    ld bc, $fd00
    rst $30
    inc bc
    jr nz, jr_001_6b95

    rst $30
    ld bc, $ed20
    rst $30
    ld bc, $ed20
    rst $30
    ld bc, $ed20
    rst $30
    ld bc, $ed20
    rst $30
    ld bc, $ed20
    rst $30
    ld bc, $ef20
    rst $30
    ld bc, $ef20
    rst $30

Call_001_6b33:
    ld a, [hl+]
    ld c, a
    ld a, [hl]
    ld b, a
    ld a, [bc]
    ld [de], a
    inc e
    inc bc
    ld a, [bc]
    ld [de], a
    ret


Call_001_6b3e:
    ld a, [hl+]
    ld [de], a
    inc e
    ld a, [hl+]
    ld [de], a
    ret


Call_001_6b44:
    call Call_001_69f8
    xor a
    ld [$df75], a
    ld [$df77], a
    ld de, $df80
    ld b, $00
    ld a, [hl+]
    ld [de], a
    inc e
    call Call_001_6b3e
    ld de, $df90
    call Call_001_6b3e
    ld de, $dfa0
    call Call_001_6b3e
    ld de, $dfb0
    call Call_001_6b3e
    ld de, $dfc0
    call Call_001_6b3e
    ld hl, $df90
    ld de, $df94
    call Call_001_6b33
    ld hl, $dfa0
    ld de, $dfa4
    call Call_001_6b33
    ld hl, $dfb0
    ld de, $dfb4
    call Call_001_6b33
    ld hl, $dfc0
    ld de, $dfc4
    call Call_001_6b33

jr_001_6b95:
    ld bc, $0410
    ld hl, $df92

jr_001_6b9b:
    ld [hl], $01
    ld a, c
    add l
    ld l, a
    dec b
    jr nz, jr_001_6b9b

    xor a
    ld [$df9e], a
    ld [$dfae], a
    ld [$dfbe], a
    ret


jr_001_6bae:
    push hl
    xor a
    ldh [rNR30], a
    ld l, e
    ld h, d
    call Call_001_69c9
    pop hl
    jr jr_001_6be4

Jump_001_6bba:
    call Call_001_6bea
    call Call_001_6bff
    ld e, a
    call Call_001_6bea
    call Call_001_6bff
    ld d, a
    call Call_001_6bea
    call Call_001_6bff
    ld c, a
    inc l
    inc l
    ld [hl], e
    inc l
    ld [hl], d
    inc l
    ld [hl], c
    dec l
    dec l
    dec l
    dec l
    push hl
    ld hl, $df70
    ld a, [hl]
    pop hl
    cp $03
    jr z, jr_001_6bae

jr_001_6be4:
    call Call_001_6bea
    jp Jump_001_6c8f


Call_001_6bea:
    push de
    ld a, [hl+]
    ld e, a
    ld a, [hl-]
    ld d, a
    inc de

jr_001_6bf0:
    ld a, e
    ld [hl+], a
    ld a, d
    ld [hl-], a
    pop de
    ret


Call_001_6bf6:
    push de
    ld a, [hl+]
    ld e, a
    ld a, [hl-]
    ld d, a
    inc de
    inc de
    jr jr_001_6bf0

Call_001_6bff:
    ld a, [hl+]
    ld c, a
    ld a, [hl-]
    ld b, a
    ld a, [bc]
    ld b, a
    ret


jr_001_6c06:
    pop hl
    jr jr_001_6c35

Jump_001_6c09:
    ld a, [$df70]
    cp $03
    jr nz, jr_001_6c20

    ld a, [$dfb8]
    bit 7, a
    jr z, jr_001_6c20

    ld a, [hl]
    cp $06
    jr nz, jr_001_6c20

    ld a, $40
    ldh [rNR32], a

jr_001_6c20:
    push hl
    ld a, l
    add $09
    ld l, a
    ld a, [hl]
    and a
    jr nz, jr_001_6c06

    ld a, l
    add $04
    ld l, a
    bit 7, [hl]
    jr nz, jr_001_6c06

    pop hl
    call Call_001_6d98

Jump_001_6c35:
jr_001_6c35:
    dec l
    dec l
    jp Jump_001_6d6a


Jump_001_6c3a:
    dec l
    dec l
    dec l
    dec l
    call Call_001_6bf6

jr_001_6c41:
    ld a, l
    add $04
    ld e, a
    ld d, h
    call Call_001_6b33
    cp $00
    jr z, jr_001_6c6c

    cp $ff
    jr z, jr_001_6c55

    inc l
    jp Jump_001_6c8d


jr_001_6c55:
    dec l
    push hl
    call Call_001_6bf6
    call Call_001_6bff
    ld e, a
    call Call_001_6bea
    call Call_001_6bff
    ld d, a
    pop hl
    ld a, e
    ld [hl+], a
    ld a, d
    ld [hl-], a
    jr jr_001_6c41

jr_001_6c6c:
    ld hl, $dfe9
    ld [hl], $00
    call Call_001_69d6
    ret


Call_001_6c75:
    ld hl, $dfe9
    ld a, [hl]
    and a
    ret z

    ld a, $01
    ld [$df70], a
    ld hl, $df90

Jump_001_6c83:
    inc l
    ld a, [hl+]
    and a
    jp z, Jump_001_6c35

    dec [hl]
    jp nz, Jump_001_6c09

Jump_001_6c8d:
    inc l
    inc l

Jump_001_6c8f:
    call Call_001_6bff
    cp $00
    jp z, Jump_001_6c3a

    cp $9d
    jp z, Jump_001_6bba

    and $f0
    cp $a0
    jr nz, jr_001_6cbc

    ld a, b
    and $0f
    ld c, a
    ld b, $00
    push hl
    ld de, $df81
    ld a, [de]
    ld l, a
    inc de
    ld a, [de]
    ld h, a
    add hl, bc
    ld a, [hl]
    pop hl
    dec l
    ld [hl+], a
    call Call_001_6bea
    call Call_001_6bff

jr_001_6cbc:
    ld a, b
    ld c, a
    ld b, $00
    call Call_001_6bea
    ld a, [$df70]
    cp $04
    jp z, Jump_001_6ced

    push hl
    ld a, l
    add $05
    ld l, a
    ld e, l
    ld d, h
    inc l
    inc l
    ld a, c
    cp $01
    jr z, jr_001_6ce8

    ld [hl], $00
    ld hl, $6e33
    add hl, bc
    ld a, [hl+]
    ld [de], a
    inc e
    ld a, [hl]
    ld [de], a
    pop hl
    jp Jump_001_6d04


jr_001_6ce8:
    ld [hl], $01
    pop hl
    jr jr_001_6d04

Jump_001_6ced:
    push hl
    ld de, $dfc6
    ld hl, $6ec5
    add hl, bc

jr_001_6cf5:
    ld a, [hl+]
    ld [de], a
    inc e
    ld a, e
    cp $cb
    jr nz, jr_001_6cf5

    ld c, $20
    ld hl, $dfc4
    jr jr_001_6d32

Jump_001_6d04:
jr_001_6d04:
    push hl
    ld a, [$df70]
    cp $01
    jr z, jr_001_6d2d

    cp $02
    jr z, jr_001_6d29

    ld c, $1a
    ld a, [$dfbf]
    bit 7, a
    jr nz, jr_001_6d1e

    xor a
    ldh [c], a
    ld a, $80
    ldh [c], a

jr_001_6d1e:
    inc c
    inc l
    inc l
    inc l
    inc l
    ld a, [hl+]
    ld e, a
    ld d, $00
    jr jr_001_6d3e

jr_001_6d29:
    ld c, $16
    jr jr_001_6d32

jr_001_6d2d:
    ld c, $10
    ld a, $00
    inc c

jr_001_6d32:
    inc l
    inc l
    inc l
    ld a, [hl-]
    and a
    jr nz, jr_001_6d88

    ld a, [hl+]
    ld e, a

jr_001_6d3b:
    inc l
    ld a, [hl+]
    ld d, a

jr_001_6d3e:
    push hl
    inc l
    inc l
    ld a, [hl+]
    and a
    jr z, jr_001_6d47

    ld e, $01

jr_001_6d47:
    inc l
    inc l
    ld [hl], $00
    inc l
    ld a, [hl]
    pop hl
    bit 7, a
    jr nz, jr_001_6d65

    ld a, d
    ldh [c], a
    inc c
    ld a, e
    ldh [c], a
    inc c
    ld a, [hl+]
    ldh [c], a
    inc c
    ld a, [hl]
    or $80
    ldh [c], a
    ld a, l
    or $05
    ld l, a
    res 0, [hl]

jr_001_6d65:
    pop hl
    dec l
    ld a, [hl-]
    ld [hl-], a
    dec l

Jump_001_6d6a:
    ld de, $df70
    ld a, [de]
    cp $04
    jr z, jr_001_6d7b

    inc a
    ld [de], a
    ld de, $0010
    add hl, de
    jp Jump_001_6c83


jr_001_6d7b:
    ld hl, $df9e
    inc [hl]
    ld hl, $dfae
    inc [hl]
    ld hl, $dfbe
    inc [hl]
    ret


jr_001_6d88:
    ld b, $00
    push hl
    pop hl
    inc l
    jr jr_001_6d3b

Call_001_6d8f:
    ld a, b
    srl a
    ld l, a
    ld h, $00
    add hl, de
    ld e, [hl]
    ret


Call_001_6d98:
    push hl
    ld a, l
    add $06
    ld l, a
    ld a, [hl]
    and $0f
    jr z, jr_001_6dba

    ld [$df71], a
    ld a, [$df70]
    ld c, $13
    cp $01
    jr z, jr_001_6dbc

    ld c, $18
    cp $02
    jr z, jr_001_6dbc

    ld c, $1d
    cp $03
    jr z, jr_001_6dbc

jr_001_6dba:
    pop hl
    ret


jr_001_6dbc:
    inc l
    ld a, [hl+]
    ld e, a
    ld a, [hl]
    ld d, a
    push de
    ld a, l
    add $04
    ld l, a
    ld b, [hl]
    ld a, [$df71]
    cp $01
    jr jr_001_6dd7

    cp $03
    jr jr_001_6dd2

jr_001_6dd2:
    ld hl, $ffff
    jr jr_001_6df3

jr_001_6dd7:
    ld de, $6dfc
    call Call_001_6d8f
    bit 0, b
    jr nz, jr_001_6de3

    swap e

jr_001_6de3:
    ld a, e
    and $0f
    bit 3, a
    jr z, jr_001_6df0

    ld h, $ff
    or $f0
    jr jr_001_6df2

jr_001_6df0:
    ld h, $00

jr_001_6df2:
    ld l, a

jr_001_6df3:
    pop de
    add hl, de
    ld a, l
    ldh [c], a
    inc c
    ld a, h
    ldh [c], a
    jr jr_001_6dba

    db $00, $00, $00, $00, $00, $00, $10, $00, $0f, $00, $00, $11, $00, $0f, $f0, $01
    db $12, $10

    rst $38
    rst $28
    ld bc, $1012
    rst $38
    rst $28
    ld bc, $1012
    rst $38
    rst $28
    ld bc, $1012
    rst $38
    rst $28
    ld bc, $1012
    rst $38
    rst $28
    ld bc, $1012
    rst $38
    rst $28
    ld bc, $1012
    rst $38
    rst $28
    ld bc, $1012
    rst $38
    rst $28
    nop
    rrca
    inc l
    nop
    sbc h
    nop
    ld b, $01
    ld l, e
    ld bc, $01c9
    inc hl
    ld [bc], a
    ld [hl], a
    ld [bc], a
    add $02
    ld [de], a
    inc bc
    ld d, [hl]
    inc bc
    sbc e
    inc bc
    jp c, $1603

    inc b
    ld c, [hl]
    inc b
    add e
    inc b

    db $b5, $04

    push hl
    inc b

    db $11, $05

    dec sp
    dec b
    ld h, e
    dec b

    db $89, $05

    xor h
    dec b

    db $ce, $05, $ed, $05, $0a, $06

    daa
    db $06

    db $42, $06, $5b, $06, $72, $06, $89, $06, $9e, $06, $b2, $06, $c4, $06, $d6, $06
    db $e7, $06, $f7, $06, $06, $07, $14, $07, $21, $07, $2d, $07, $39, $07, $44, $07
    db $4f, $07, $59, $07, $62, $07, $6b, $07, $73, $07, $7b, $07, $83, $07, $8a, $07
    db $90, $07, $97, $07

    sbc l
    rlca

    db $a2, $07

    and a
    rlca

    db $ac, $07

    or c
    rlca
    or [hl]
    rlca
    cp d
    rlca
    cp [hl]
    rlca
    pop bc
    rlca
    call nz, $c807
    rlca
    rlc a
    adc $07
    pop de
    rlca
    call nc, $d607
    rlca
    reti


    rlca
    db $db
    rlca
    db $dd
    rlca
    rst $18
    rlca
    nop

    db $00, $00, $00, $00, $c0, $a1, $00, $3a, $00, $c0, $b1, $00, $29, $01, $c0

    ld h, c
    nop
    ld a, [hl-]
    nop
    ret nz

    ld [de], a
    inc [hl]
    ld b, l
    ld h, a
    sbc d
    cp h
    sbc $fe
    sbc b
    ld a, d
    or a
    cp [hl]
    xor b
    db $76
    ld d, h
    ld sp, $2301
    ld b, h
    ld d, l
    ld h, a
    adc b
    sbc d
    cp e
    xor c
    adc b
    db $76
    ld d, l
    ld b, h
    inc sp
    ld [hl+], a
    db $11

    db $01, $23, $45, $67, $89, $ab, $cd, $ef, $fe, $dc, $ba, $98, $76, $54, $32, $10
    db $a1, $82, $23, $34, $45, $56, $67, $78, $89, $9a, $ab, $bc, $cd, $64, $32, $10
    db $11, $23, $56, $78, $99, $98, $76, $67, $9a, $df, $fe, $c9, $85, $42, $11, $31

    ld [bc], a
    inc b

    db $08, $10, $20

    ld b, b
    inc c
    jr jr_001_6f63

    dec b
    nop
    ld bc, $0503
    ld a, [bc]
    inc d
    jr z, @+$52

    rrca
    ld e, $3c
    inc bc

    db $06, $0c, $18

    db $30

    db $60, $12, $24, $48, $08, $10

    nop
    rlca
    ld c, $1c
    jr c, jr_001_6fc0

    dec d
    ld a, [hl+]
    ld d, h
    inc b
    ld [$2010], sp
    ld b, b
    add b
    jr @+$32

    ld h, b
    inc b
    add hl, bc
    ld [de], a
    inc h
    ld c, b
    sub b
    dec de

jr_001_6f63:
    ld [hl], $6c
    inc c
    jr jr_001_6f6c

    ld a, [bc]
    inc d
    jr z, jr_001_6fbc

jr_001_6f6c:
    and b
    ld e, $3c
    ld a, b

    db $00, $3f, $6f, $f4, $7c, $fa, $7c, $0c, $7d, $1c, $7d

    nop
    ld [hl], $6f
    ld b, e
    ld a, [hl]
    ccf
    ld a, [hl]
    ld b, l
    ld a, [hl]
    ld b, a
    ld a, [hl]

    db $00, $3f, $6f, $36, $76, $2e, $76, $3c, $76, $5e, $76, $00, $2a, $6f, $fb, $75
    db $f7, $75, $fd, $75, $00, $00, $00, $3f, $6f, $8d, $71, $73, $71, $a7, $71, $c1
    db $71

    nop
    ccf
    ld l, a
    pop bc
    ld [hl], d
    or e
    ld [hl], d
    rst $08
    ld [hl], d
    db $fd
    ld [hl], d
    nop
    ccf
    ld l, a
    add hl, sp
    ld [hl], b
    dec hl
    ld [hl], b
    nop
    nop
    nop

jr_001_6fbc:
    nop
    nop
    ld [hl], $6f

jr_001_6fc0:
    sbc b
    ld a, [hl]
    adc h
    ld a, [hl]
    and h
    ld a, [hl]
    or b
    ld a, [hl]
    nop
    ccf
    ld l, a
    inc hl
    ld a, h
    rra
    ld a, h
    dec h
    ld a, h
    daa
    ld a, h
    nop
    ccf
    ld l, a
    nop
    nop
    ei
    ld a, c
    nop
    nop
    nop
    nop
    nop
    ccf
    ld l, a
    nop
    nop
    ld hl, $257a
    ld a, d
    nop
    nop
    nop
    ccf
    ld l, a
    ld l, [hl]
    ld a, d
    ld l, d
    ld a, d
    ld [hl], b
    ld a, d
    nop
    nop
    nop
    ccf
    ld l, a
    jp c, $de7a

    ld a, d
    ldh [$ff7a], a
    ldh [c], a
    ld a, d
    nop
    ccf
    ld l, a
    ld h, b
    ld a, e
    ld h, [hl]
    ld a, e
    ld l, d
    ld a, e
    ld l, [hl]
    ld a, e
    nop
    ccf
    ld l, a
    ld h, a
    ld a, b
    ld [hl], c
    ld a, b
    ld a, c
    ld a, b
    add c
    ld a, b
    nop
    ld e, h
    ld l, a
    ld a, $75
    ld b, [hl]
    ld [hl], l
    ld c, h
    ld [hl], l
    nop
    nop
    nop
    ccf
    ld l, a
    adc b
    ld [hl], l
    sub b
    ld [hl], l
    sub [hl]
    ld [hl], l
    nop
    nop
    ld b, a
    ld [hl], b
    ld h, l
    ld [hl], b
    ld b, a
    ld [hl], b
    ld a, [hl]
    ld [hl], b
    call nz, $ff70
    rst $38
    dec hl
    ld [hl], b
    sub e
    ld [hl], b
    and l
    ld [hl], b
    sub e
    ld [hl], b
    or [hl]
    ld [hl], b
    dec h
    ld [hl], c
    rst $38
    rst $38
    add hl, sp
    ld [hl], b
    sbc l
    ld [hl], h
    nop
    ld b, c
    and d
    ld b, h
    ld c, h
    ld d, [hl]
    ld c, h
    ld b, d

jr_001_7051:
    ld c, h
    ld b, h
    ld c, h
    ld a, $4c
    inc a
    ld c, h
    ld b, h
    ld c, h
    ld d, [hl]
    ld c, h
    ld b, d
    ld c, h
    ld b, h
    ld c, h
    ld a, $4c
    inc a
    ld c, h
    nop
    ld b, h
    ld c, h
    ld b, h
    ld a, $4e
    ld c, b
    ld b, d
    ld c, b
    ld b, d
    ld a, [hl-]
    ld c, h
    ld b, h
    ld a, $4c
    ld c, b
    ld b, h
    ld b, d
    ld a, $3c
    inc [hl]
    inc a
    ld b, d
    ld c, h
    ld c, b
    nop
    ld b, h
    ld c, h
    ld b, h
    ld a, $4e
    ld c, b
    ld b, d
    ld c, b
    ld b, d
    ld a, [hl-]
    ld d, d
    ld c, b
    ld c, h
    ld d, d
    ld c, h
    ld b, h
    ld a, [hl-]
    ld b, d
    xor b
    ld b, h
    nop
    sbc l
    ld h, h
    nop
    ld b, c
    and e
    ld h, $3e
    inc a
    ld h, $2c
    inc [hl]
    ld a, $36
    inc [hl]
    ld a, $2c
    inc [hl]
    nop
    ld h, $3e
    jr nc, jr_001_70cb

    ld a, [hl-]
    inc l
    ld e, $36
    jr nc, jr_001_7051

    inc [hl]
    ld [hl], $34
    jr nc, jr_001_70e0

    ld a, [hl+]
    nop
    and e
    ld h, $3e
    jr nc, jr_001_70dd

    ld a, [hl-]
    ld a, [hl+]
    inc l
    inc [hl]
    inc [hl]
    inc l
    ld [hl+], a
    inc d
    nop
    and d
    ld d, d
    ld c, [hl]
    ld c, h
    ld c, b
    ld b, h
    ld b, d

jr_001_70cb:
    ld b, h
    ld c, b
    ld c, h
    ld b, h
    ld c, b
    ld c, [hl]
    ld c, h
    ld c, [hl]
    and e
    ld d, d
    ld b, d
    and d
    ld b, h
    ld c, b
    and e
    ld c, h
    ld c, b
    ld c, h

jr_001_70dd:
    ld d, [hl]
    ld d, b
    and d

jr_001_70e0:
    ld d, [hl]
    ld e, d
    and e
    ld e, h
    ld e, d
    and d
    ld d, [hl]
    ld d, d
    ld d, b
    ld c, h
    ld d, b
    ld c, d
    xor b
    ld c, h

jr_001_70ee:
    and a
    ld d, d
    and c
    ld d, [hl]
    ld e, b
    and e
    ld d, [hl]
    and d
    ld d, d
    ld c, [hl]
    ld d, d
    ld c, h
    ld c, [hl]
    ld c, b
    and a
    ld d, [hl]
    and c
    ld e, d

jr_001_7100:
    ld e, h
    and e
    ld e, d
    and d
    ld d, [hl]
    ld d, h
    ld d, [hl]
    ld d, b
    ld d, h
    ld c, h
    ld e, d
    ld d, h
    ld c, h

jr_001_710d:
    ld d, h
    ld e, d
    ld h, b
    ld h, [hl]
    ld d, h
    ld h, h
    ld d, h
    ld h, b
    ld d, h
    and e
    ld e, h
    and d
    ld h, b
    ld e, h
    ld e, d
    ld e, h
    and c
    ld d, [hl]
    ld e, d
    and h
    ld d, [hl]
    and d
    ld bc, $a200
    inc [hl]
    ld a, [hl-]
    ld b, h
    ld a, [hl-]
    jr nc, jr_001_7166

    inc [hl]
    ld a, [hl-]
    inc l
    ld a, [hl-]
    ld a, [hl+]
    ld a, [hl-]
    inc l
    ld a, [hl-]
    ld b, h
    ld a, [hl-]
    jr nc, @+$3c

    inc [hl]
    ld a, [hl-]
    inc l
    ld a, [hl-]
    ld a, [hl+]
    ld a, [hl-]
    inc l
    inc [hl]
    inc l
    ld h, $3e
    jr c, @+$34

    jr c, jr_001_7171

    jr c, @+$34

    jr c, jr_001_70ee

    inc [hl]
    ld b, d
    ld a, [hl+]
    and d
    inc [hl]
    ld a, [hl-]
    ld b, d
    ld a, [hl-]
    jr nc, @+$3c

    ld l, $34
    ld h, $34
    ld l, $34
    xor b
    jr nc, jr_001_7100

    ld [hl-], a
    jr c, @+$2c

    jr c, @+$34

    jr c, jr_001_710d

    inc [hl]

jr_001_7166:
    and e
    inc [hl]
    ld a, [hl+]
    inc h
    inc e
    jr nz, @+$26

    inc l
    jr nc, @+$36

    xor b

jr_001_7171:
    ld h, $00

    db $c7, $71, $d6, $71, $0f, $72, $d6, $71, $41, $72, $a4, $72, $d6, $71, $0f, $72
    db $c7, $71, $d6, $71, $76, $72, $ff, $ff, $73, $71, $cc, $71, $ee, $71, $27, $72
    db $ee, $71, $55, $72, $a9, $72, $ee, $71, $27, $72, $cc, $71, $ee, $71, $8a, $72
    db $ff, $ff, $8d, $71, $d1, $71, $fc, $71, $34, $72, $fc, $71, $64, $72, $ae, $72
    db $fc, $71, $34, $72, $d1, $71, $fc, $71, $97, $72, $ff, $ff, $a7, $71, $0a, $72
    db $ff, $ff, $c1, $71, $9d, $84, $00, $80, $00, $9d, $54, $00, $80, $00, $9d, $1a
    db $6f, $a0, $00, $a2, $44, $48, $44, $42, $44, $48, $4c, $4e, $a3, $52, $a2, $01
    db $56, $a3, $5c, $01, $a9, $58, $5c, $58, $a8, $48, $00, $a3, $01, $3e, $3e, $01
    db $44, $44, $01, $48, $48, $01, $40, $40, $00, $a3, $4e, $4e, $4e, $44, $56, $56
    db $52, $58, $58, $40, $52, $52, $00, $a3, $06, $0b, $0b, $00, $a2, $40, $44, $40
    db $3e, $40, $44, $48, $4c, $a3, $4e, $a2, $01, $52, $a3, $58, $01, $a9, $56, $58
    db $56, $a8, $44, $00, $01, $3a, $3a, $01, $40, $40, $01, $44, $44, $01, $40, $40
    db $00, $44, $4c, $4c, $44, $52, $52, $4e, $56, $56, $44, $4c, $4c, $00, $a3, $58
    db $a7, $56, $a2, $52, $a3, $56, $a7, $4e, $a2, $48, $4c, $4c, $a3, $4c, $4e, $a8
    db $52, $00, $01, $46, $46, $01, $44, $44, $a2, $40, $40, $a3, $40, $40, $a8, $40
    db $00, $46, $4e, $4e, $44, $56, $56, $a2, $52, $52, $a3, $52, $48, $4c, $a7, $48
    db $a2, $46, $00, $a3, $52, $a7, $58, $a2, $56, $a3, $56, $a7, $5c, $a2, $66, $60
    db $60, $a3, $60, $64, $a8, $66, $00, $01, $46, $46, $01, $44, $44, $01, $40, $3a
    db $01, $46, $44, $00, $46, $4e, $4e, $44, $56, $56, $40, $52, $44, $4e, $58, $56
    db $00, $9d, $63, $00, $80, $00, $9d, $44, $00, $80, $00, $9d, $1a, $6f, $a0, $00

    ld b, $73
    ld a, [hl-]
    ld [hl], e
    ld h, d
    ld [hl], e
    ld h, d
    ld [hl], e
    call nz, $ff73
    rst $38
    or e
    ld [hl], d
    inc bc
    ld [hl], e
    scf
    ld [hl], e
    adc c
    ld [hl], e
    adc c
    ld [hl], e
    ld b, [hl]
    ld [hl], h
    rst $38
    rst $38
    pop bc
    ld [hl], d
    ld a, [de]
    ld [hl], e
    ld c, [hl]
    ld [hl], e
    or b
    ld [hl], e
    or b
    ld [hl], e
    or b
    ld [hl], e
    or b
    ld [hl], e
    or b
    ld [hl], e
    or b
    ld [hl], e
    cp e
    ld [hl], h
    reti


    ld [hl], h
    reti


    ld [hl], h
    reti


    ld [hl], h
    jp hl


    ld [hl], h
    ld sp, hl
    ld [hl], h
    ld sp, hl
    ld [hl], h
    add hl, bc
    ld [hl], l
    add hl, bc
    ld [hl], l
    add hl, de
    ld [hl], l
    add hl, de
    ld [hl], l
    add hl, bc
    ld [hl], l
    add hl, hl
    ld [hl], l
    rst $38
    rst $38
    rst $08
    ld [hl], d
    ld l, $73
    rst $38
    rst $38
    db $fd
    ld [hl], d
    and l
    ld bc, $9d00
    ld h, d
    nop
    add b
    and d
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    jr nc, jr_001_7342

    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    jr nc, jr_001_7349

    nop
    sbc l
    ld a, [de]
    ld l, a
    and b
    and d
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    jr nc, jr_001_7356

    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    jr nc, jr_001_735d

    nop
    and d
    ld b, $a1
    ld b, $06
    and d
    ld b, $06
    nop
    and l
    ld bc, $9d00
    ld [hl-], a
    nop
    add b
    and d
    ld a, [hl-]
    and c
    ld a, [hl-]

jr_001_7342:
    ld a, [hl-]
    and d
    jr nc, jr_001_7376

    ld a, [hl-]
    and c
    ld a, [hl-]

jr_001_7349:
    ld a, [hl-]
    and d
    jr nc, jr_001_737d

    nop
    sbc l
    ld a, [de]
    ld l, a
    and b
    and d
    ld a, [hl-]
    and c
    ld a, [hl-]

jr_001_7356:
    ld a, [hl-]
    and d
    jr nc, jr_001_738a

    ld a, [hl-]
    and c
    ld a, [hl-]

jr_001_735d:
    ld a, [hl-]
    and d
    jr nc, jr_001_7391

    nop
    sbc l
    add d
    nop
    add b
    and d
    ld a, [hl-]
    ld c, b
    ld d, d
    ld d, b
    ld d, d
    and c
    ld c, b
    ld c, b
    and d
    ld c, d
    ld b, h
    ld c, b
    and c
    ld b, b
    ld b, b

jr_001_7376:
    and d
    ld b, h
    ld a, $40
    and c
    ld a, [hl-]
    ld a, [hl-]

jr_001_737d:
    and d
    ld a, $38
    ld a, [hl-]
    jr nc, jr_001_73b5

    jr c, jr_001_73bf

    jr nc, jr_001_73b9

    ld a, $00
    sbc l

jr_001_738a:
    ld d, e
    nop
    ld b, b
    and d
    jr nc, jr_001_73d0

    ld b, b

jr_001_7391:
    ld b, h
    ld b, b
    and c
    ld a, $40
    and d
    ld b, h
    ld a, $40
    and c
    jr c, jr_001_73d7

    and d
    ld a, $38
    ld a, [hl-]
    and c
    ld l, $30
    and d
    jr c, jr_001_73d7

    jr nc, jr_001_73d1

    inc l
    inc l
    jr nc, @+$2a

    inc l
    jr c, jr_001_73b0

jr_001_73b0:
    sbc l
    ld a, [de]
    ld l, a
    and b
    and d

jr_001_73b5:
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]

jr_001_73b9:
    and d
    jr nc, jr_001_73ec

    ld a, [hl-]
    and c
    ld a, [hl-]

jr_001_73bf:
    ld a, [hl-]
    and d
    jr nc, jr_001_73f3

    nop
    xor b
    ld a, [hl-]
    and d
    ld a, $38
    xor b
    ld a, [hl-]
    and e
    ld a, $a2
    ld b, b
    and c

jr_001_73d0:
    ld b, b

jr_001_73d1:
    ld b, b
    and d
    ld b, h
    ld a, $40
    and c

jr_001_73d7:
    ld b, b
    ld b, b
    and d
    ld b, h
    ld a, $a8
    ld b, b
    and e
    ld b, h
    and d
    ld c, b
    and c
    ld c, b
    ld c, b
    and d
    ld c, d
    ld b, h
    ld c, b
    and c
    ld c, b

jr_001_73eb:
    ld c, b

jr_001_73ec:
    and d
    ld c, d
    ld b, h
    xor b
    ld c, b

jr_001_73f1:
    and e
    ld c, h

jr_001_73f3:
    and d
    ld c, [hl]
    and c
    ld c, [hl]
    ld c, [hl]
    and d
    ld c, [hl]
    ld c, [hl]
    ld d, d
    ld c, [hl]
    ld c, [hl]
    ld c, h
    ld c, [hl]
    and c
    ld c, [hl]
    ld c, [hl]
    and d
    ld c, [hl]
    ld c, [hl]
    ld d, d
    ld c, [hl]
    ld c, [hl]
    ld c, h
    ld c, [hl]
    and c
    ld c, [hl]
    ld c, [hl]
    and d
    ld c, [hl]
    ld c, [hl]
    ld c, h
    and c
    ld c, h
    ld c, h
    and d
    ld c, h
    ld c, h
    ld c, d
    and c
    ld c, d
    ld c, d
    and d
    ld c, d
    ld b, h
    ld a, $40
    ld b, h
    ld [hl], $44
    and c
    ld b, b
    ld b, b
    and d
    ld [hl], $a3
    ld b, b
    and c
    ld [hl], $3a
    and d
    ld [hl], $30
    ld b, h
    and c
    ld b, b
    ld b, b
    and d
    ld [hl], $a3
    ld b, b
    and c
    ld [hl], $3a
    and d
    ld [hl], $2e
    and l
    ld [hl], $a8

jr_001_7442:
    ld bc, $38a3
    nop
    xor b
    jr nc, jr_001_73eb

    jr nc, jr_001_747b

    xor b
    jr nc, jr_001_73f1

    ld [hl], $a5
    ld bc, $01a8
    and e
    ld a, $a2
    ld b, b
    and c
    ld b, b
    ld b, b
    and d
    ld b, h
    ld a, $40
    and c
    ld b, b
    ld b, b
    and d
    ld b, h
    ld a, $a8
    ld [hl], $a3
    ld a, [hl-]
    and d
    ld a, $a1
    ld b, b
    ld b, h
    and d
    ld a, $44
    ld c, b
    ld c, b
    ld c, b
    ld a, [hl-]
    ld a, $a1
    ld b, b
    ld b, h
    and d
    ld a, $44

jr_001_747b:
    ld b, [hl]
    ld b, [hl]

jr_001_747d:
    ld b, [hl]
    ld a, [hl-]
    ld a, $a1

jr_001_7481:
    ld b, b
    ld b, h
    and d

jr_001_7484:
    ld a, $44
    ld a, [hl-]
    and c

jr_001_7488:
    ld a, $40
    and d
    ld a, [hl-]
    ld b, b

jr_001_748d:
    ld a, [hl-]
    and c
    ld a, $40

jr_001_7491:
    and d
    ld a, $3e

jr_001_7494:
    inc l
    ld a, [hl-]
    ld a, $26

jr_001_7498:
    jr nc, @-$5d

    jr nc, jr_001_74cc

    and d
    jr nc, jr_001_7442

    jr nc, jr_001_7442

    jr nc, jr_001_74d7

    and d

jr_001_74a4:
    jr nc, jr_001_74ce

    ld l, $a1

jr_001_74a8:
    ld l, $2e
    and d
    ld l, $a3
    ld l, $a1
    ld l, $32
    and d
    ld l, $28
    and l
    ld h, $a8
    ld bc, $2ca3
    nop
    and d
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    ld [hl-], a
    inc l
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    jr c, jr_001_74fa

    ld a, [hl-]
    and c

jr_001_74cc:
    ld a, [hl-]
    ld a, [hl-]

jr_001_74ce:
    and d
    ld [hl-], a
    inc l
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    inc l

jr_001_74d7:
    ld e, $00
    and d
    jr z, jr_001_747d

    ld b, b
    jr z, jr_001_7481

    ld e, $36
    jr z, jr_001_7484

    ld b, b
    jr z, jr_001_7488

    ld e, $36
    nop
    and d
    jr z, jr_001_748d

    ld b, b
    jr z, jr_001_7491

    ld e, $36
    jr z, jr_001_7494

    ld b, b
    jr z, jr_001_7498

    inc l
    ld b, h
    nop
    and d

jr_001_74fa:
    ld e, $a1
    ld [hl], $1e
    and d
    ld e, $36
    jr z, jr_001_74a4

    ld b, b
    jr z, jr_001_74a8

    jr z, jr_001_7548

    nop
    and d
    ld e, $a1
    ld [hl], $1e
    and d
    ld e, $36
    ld e, $a1
    ld [hl], $1e
    and d
    ld e, $36
    nop
    and d
    ld [hl+], a
    and c
    ld a, [hl-]
    ld [hl+], a
    and d
    ld [hl+], a
    ld a, [hl-]
    ld [hl+], a
    and c
    ld a, [hl-]
    ld [hl+], a
    and d
    ld [hl+], a
    ld a, [hl-]
    nop
    and d
    ld e, $a1
    ld [hl], $1e
    and d
    ld e, $36
    ld e, $a1
    ld [hl], $1e
    and d
    and h
    ld a, $00
    ld [hl], $3e
    ld b, h
    and h
    ld b, h
    ld d, d
    ld [hl], l
    ld e, l
    ld [hl], l
    rst $38
    rst $38
    ld b, b
    ld [hl], l
    ld e, c
    ld [hl], l

jr_001_7548:
    rst $38
    rst $38
    ld b, [hl]
    ld [hl], l
    ld [hl], a
    ld [hl], l
    rst $38
    rst $38
    ld c, h
    ld [hl], l
    sbc l
    jr nz, jr_001_7555

jr_001_7555:
    add c
    xor d
    ld bc, $9d00
    ld [hl], b
    nop
    add c
    and d
    ld b, d
    ld [hl-], a
    jr c, jr_001_75a4

    ld b, [hl]
    inc [hl]
    inc a
    ld b, [hl]
    ld c, d
    jr c, jr_001_75ab

    ld c, d
    ld c, h
    inc a
    ld b, d
    ld c, h
    ld b, [hl]
    inc [hl]
    inc a
    ld b, [hl]
    ld b, b
    ld l, $34
    ld b, b
    nop
    sbc l
    ld a, [de]
    ld l, a
    ld hl, $42a8
    and e
    ld a, [hl+]
    xor b
    ld b, d
    and e
    ld a, [hl+]
    xor b
    ld b, d
    and e
    ld a, [hl+]
    nop
    sbc h
    ld [hl], l
    and a
    ld [hl], l
    rst $38
    rst $38
    adc d
    ld [hl], l
    and e
    ld [hl], l
    rst $38
    rst $38
    sub b
    ld [hl], l
    jp hl


    ld [hl], l
    rst $38
    rst $38
    sub [hl]
    ld [hl], l
    sbc l
    jr nz, jr_001_759f

jr_001_759f:
    add c
    xor d
    ld bc, $9d00

jr_001_75a4:
    ld [hl], b
    nop
    add c
    and d
    ld c, h
    ld b, d
    ld d, b

jr_001_75ab:
    ld b, d
    ld d, h
    ld b, d
    ld d, b
    ld b, d
    ld d, [hl]
    ld b, d
    ld d, h
    ld b, d
    ld d, b
    ld b, d
    ld d, h
    ld b, d
    ld c, h
    ld b, d
    ld d, b
    ld b, d
    ld d, h
    ld b, d
    ld d, b
    ld b, d
    ld d, [hl]
    ld b, d
    ld d, h
    ld b, d
    ld d, b
    ld b, d
    ld d, h
    ld b, d
    ld e, d
    ld b, [hl]
    ld d, [hl]
    ld b, [hl]
    ld d, h
    ld b, [hl]
    ld d, b
    ld b, [hl]
    ld c, [hl]
    ld b, [hl]
    ld d, b
    ld b, [hl]
    ld d, h
    ld b, [hl]
    ld d, b
    ld b, [hl]
    ld d, b
    ld a, $4c
    ld a, $4c
    ld a, $4a
    ld a, $4a
    ld a, $46
    ld a, $4a
    ld a, $50
    ld a, $00
    sbc l
    ld a, [de]
    ld l, a
    ld hl, $4ca5
    ld c, d
    ld b, [hl]
    ld b, d
    jr c, jr_001_7632

    ld b, d
    ld b, d
    nop

    db $ff, $75, $00, $00, $0f, $76, $1e, $76, $9d, $b2, $00, $80, $a2, $60, $5c, $60
    db $5c, $60, $62, $60, $5c, $a4, $60, $00, $9d, $92, $00, $80, $a2, $52, $4e, $52
    db $4e, $52, $54, $52, $4e, $a4, $52, $9d, $1a, $6f, $20, $a2, $62, $60, $62, $60
    db $62, $66, $62, $60, $a3, $62, $01, $6a, $76

    ld h, h
    ld [hl], a

jr_001_7632:
    ld h, h
    ld [hl], a
    nop
    nop

    db $ba, $76

    and l
    ld [hl], a
    scf
    ld a, b

    db $07, $77

    and $77
    and $77
    ldh a, [rPCM34]
    and $77
    and $77
    ld sp, hl
    ld [hl], a
    ldh a, [rPCM34]
    and $77
    and $77
    ld sp, hl
    ld [hl], a
    ldh a, [rPCM34]
    ld [bc], a
    ld a, b
    inc c
    ld a, b
    ld sp, hl
    ld [hl], a
    ldh a, [rPCM34]
    and $77

    db $56, $77, $56, $77

    dec d
    ld a, b
    dec d
    ld a, b
    dec d
    ld a, b
    dec d
    ld a, b

    db $9d, $c3, $00, $80, $a2, $3c, $3e, $3c, $3e, $38, $50, $a3, $01, $a2, $3c, $3e
    db $3c, $3e, $38, $50, $a3, $01, $a2, $01, $48, $01, $46, $01, $42, $01, $46, $a1
    db $42, $46, $a2, $42, $42, $38, $a3, $3c, $01, $a2, $3e, $42, $3e, $42, $3c, $54
    db $a3, $01, $a2, $3e, $42, $3e, $42, $3c, $54, $a3, $01, $a2, $01, $56, $01, $54
    db $01, $54, $01

    ld d, b
    and d
    ld bc, $50a1
    ld d, h
    and d
    ld d, b
    ld c, [hl]
    and e
    ld d, b
    db $01
    nop

    db $9d, $74, $00, $80, $a2, $36, $38, $36, $38, $2e, $3e, $a3, $01, $a2, $36, $38
    db $36, $38, $2e, $3e, $a3, $01, $a2, $01, $36, $01, $36, $01, $32, $01, $36, $36
    db $32, $32, $30, $a3, $36, $01, $a2, $38, $3c, $38, $3c, $36, $4e, $a3, $01, $a2
    db $38, $3c, $38, $3c, $36, $4e, $a3, $01, $a2, $01, $50, $01, $4e, $01, $46, $01

    ld b, [hl]
    and d
    ld bc, $48a1
    ld c, [hl]
    and d
    ld c, b
    ld b, [hl]
    and e
    ld b, b
    db $01
    nop

    db $9d, $1a, $6f, $20, $a2, $48, $46, $48, $46, $3e, $20, $a3, $01, $a2, $48, $46
    db $48, $46, $3e, $20, $a3, $01, $a2, $2e, $3c, $2e, $24, $24, $24, $24, $3c, $2a
    db $3e, $2a, $3e, $a6, $2e, $a3, $01, $a1, $01, $a2, $48, $46, $48, $46, $2e, $2e
    db $a3, $01, $a2, $48, $46, $48, $46, $2e, $2e, $a3, $01, $a2, $2a, $3c, $2a, $3c
    db $2e, $3e, $2e

    ld a, $2e
    ld b, d
    ld l, $42
    and [hl]
    jr c, @-$5b

    ld bc, $01a1
    nop

    db $a8, $01, $a2, $06, $0b, $a8, $01, $a2, $06, $0b, $a5, $01, $01, $00

    sbc l
    push bc
    nop
    add b
    and c
    ld b, [hl]
    ld c, d
    and h
    ld b, [hl]
    and d
    ld bc, $50a3
    xor b
    ld c, d
    and e
    ld bc, $42a1
    ld b, [hl]
    and h
    ld b, d
    and d
    ld bc, $4ea3
    and c
    ld c, [hl]
    ld d, b
    and h
    ld b, [hl]
    and a
    ld bc, $40a1
    ld b, [hl]
    and h
    ld b, b
    and d
    ld bc, $46a3
    and c
    ld b, [hl]
    ld c, d
    and h
    ld b, d
    and a
    ld bc, $36a1
    jr c, @-$5a

    ld [hl], $a2
    ld bc, $3ca3
    and a
    ld b, d
    and h
    ld b, b
    and d
    ld bc, $9d00
    add h
    nop
    ld b, c
    and c
    ld b, b
    ld b, d
    and h
    ld b, b
    and d
    ld bc, $40a3
    xor b
    ld b, d
    and e
    ld bc, $3ca1
    ld b, b
    and h
    inc a
    and d
    ld bc, $3ca3
    and c
    inc a
    ld b, b
    and h
    ld b, b
    and a
    ld bc, $36a1
    ld [hl-], a
    and h
    ld l, $a2
    ld bc, $40a3
    and c
    ld [hl], $38
    and h
    ld [hl-], a
    and a
    ld bc, $2ea1
    ld [hl-], a
    and h
    ld l, $a2
    ld bc, $2aa3
    and a
    jr nc, @-$5a

    ld l, $a2
    ld bc, $a200
    jr c, @+$3a

    ld bc, $3838
    jr c, @+$03

    jr c, jr_001_77f0

jr_001_77f0:
    ld l, $2e
    ld bc, $2e2e
    ld l, $01
    ld l, $00
    ld a, [hl+]
    ld a, [hl+]
    ld bc, $2a2a
    ld a, [hl+]
    ld bc, $002a
    and d
    jr c, jr_001_783d

    ld bc, $3638
    ld [hl], $01
    ld [hl], $00
    ld [hl-], a
    ld [hl-], a
    ld bc, $2e32
    ld l, $01
    ld l, $00
    and d
    ld b, $0b
    ld bc, $0606
    dec bc
    ld bc, $0606
    dec bc
    ld bc, $0606
    dec bc
    ld bc, $0606
    dec bc
    ld bc, $0606
    dec bc
    ld bc, $0606
    dec bc
    ld bc, $0106
    dec bc
    ld bc, $000b
    sbc l
    ld h, [hl]
    nop
    add c
    and a
    ld e, b

jr_001_783d:
    ld e, d
    and e
    ld e, b
    and a
    ld e, [hl]
    and h
    ld e, d
    and d
    ld bc, $50a7
    ld d, h
    and e
    ld e, b
    and a
    ld e, d
    and h
    ld e, b
    and d
    ld bc, $50a7
    and e
    ld c, [hl]
    and a
    ld c, [hl]
    ld e, b
    ld d, h
    and e
    ld c, d
    and a
    ld e, d
    ld e, [hl]
    and e
    ld e, d
    and a
    ld d, h
    and h
    ld d, b
    and d
    ld bc, $8900
    ld a, b
    inc c
    ld a, c
    adc c
    ld a, b
    sub c
    ld a, c
    nop
    nop
    xor b
    ld a, b
    inc sp
    ld a, c
    xor b
    ld a, b

jr_001_7877:
    or l
    ld a, c
    ret nc

    ld a, b
    ld e, c
    ld a, c
    ret nc

    ld a, b
    ret c

    ld a, c
    ld sp, hl
    ld a, b
    ld a, a
    ld a, c
    ld sp, hl
    ld a, b
    ld a, a
    ld a, c
    sbc l
    pop de
    nop
    add b
    and d
    ld e, h
    and c
    ld e, h
    ld e, d
    and d
    ld e, h
    ld e, h
    ld d, [hl]
    ld d, d
    ld c, [hl]
    ld d, [hl]
    and d
    ld d, d
    and c
    ld d, d
    ld d, b
    and d
    ld d, d
    ld d, d
    ld c, h
    ld c, b
    ld b, h
    and c
    ld c, h
    ld d, d
    nop
    sbc l
    or d
    nop
    add b
    and d
    ld d, d
    and c
    ld d, d
    ld d, d
    and d
    ld d, d
    and c
    ld d, d
    ld d, d
    and d
    ld b, h
    and c
    ld b, h
    ld b, h
    and d
    ld b, h
    ld bc, $a14c
    ld c, h
    ld c, h
    and d
    ld c, h
    and c
    ld c, h
    ld c, h
    and d
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    ld a, [hl-]
    ld bc, $9d00
    ld a, [de]
    ld l, a
    jr nz, jr_001_7877

    ld e, h
    and c
    ld e, h
    ld e, h
    and d
    ld e, h
    and c
    ld e, h
    ld e, h
    and d
    ld c, [hl]
    and c
    ld d, d
    ld d, d
    and d
    ld d, [hl]
    ld bc, $5ca2
    and c
    ld e, h
    ld e, h
    and d
    ld e, h
    and c
    ld e, h
    ld e, h
    and d
    ld b, h
    and c
    ld c, b
    ld c, b
    and d
    ld c, h
    ld bc, $a200
    ld b, $a7
    ld bc, $0ba2
    dec bc
    dec bc
    ld bc, $06a2
    and a
    ld bc, $0ba2
    dec bc
    dec bc
    ld bc, $a200
    ld c, b
    and c
    ld c, b
    ld d, d
    and d
    ld b, h
    and c
    ld b, h
    ld d, d
    and d
    ld b, d
    and c
    ld b, d
    ld d, d
    and d
    ld c, b
    and c
    ld c, b
    ld d, d
    and d
    ld c, h
    and c
    ld c, h
    ld d, d
    and d
    ld b, h
    and c
    ld b, h
    ld d, d
    and d
    ld c, b
    ld b, h
    and c
    ld c, b
    ld d, d
    ld d, [hl]
    ld e, d
    nop
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    ld [hl], $a1
    ld [hl], $36
    and d
    ld [hl], $01
    nop
    ld c, b
    and c
    ld c, b
    ld c, b
    and d
    ld c, b
    and c
    ld c, b
    ld c, b
    and d
    ld c, b
    and c
    ld c, b
    ld c, b
    and d
    ld c, b
    and c
    ld c, b
    ld c, b
    and d
    ld b, h
    and c
    ld b, h
    ld b, h
    and d
    ld b, h
    and c
    ld b, h
    ld b, h
    and d
    ld b, d
    and c
    ld b, d
    ld b, d
    and d
    ld b, d
    ld bc, $a200
    ld bc, $010b
    dec bc
    ld bc, $010b
    dec bc
    ld bc, $010b
    dec bc
    ld bc, $0b0b
    ld bc, $a200
    ld c, b
    and c
    ld c, b
    ld d, d
    and d
    ld b, h
    and c
    ld b, h
    ld d, d
    and d
    ld b, d
    and c
    ld b, d
    ld d, d
    and d
    ld c, b
    and c
    ld c, b
    ld d, d
    and d
    ld c, h
    and c
    ld c, h
    ld d, d
    and d
    ld c, b
    and c
    ld c, b
    ld d, d
    and d
    ld b, h
    ld d, d
    and e
    ld e, h
    nop
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    ld a, [hl-]
    and c
    ld a, [hl-]
    ld a, [hl-]
    and d
    ld bc, $a33a
    ld c, h
    nop
    ld c, b
    and c
    ld c, b
    ld c, b
    and d
    ld c, b
    and c
    ld c, b
    ld c, b
    and d
    ld c, b
    and c
    ld c, b
    ld c, b
    and d
    ld c, b
    and c
    ld c, b
    ld c, b
    and d
    ld b, h

jr_001_79ed:
    and c
    ld b, h
    ld b, h
    and d
    ld b, h
    and c
    ld b, h
    ld b, h
    and d
    ld bc, $a34c
    ld b, h
    nop
    rst $38
    ld a, c
    nop
    nop
    sbc l
    jp nz, $4000

    and d
    ld e, h
    and c
    ld e, h
    ld e, d
    and d
    ld e, h
    ld e, h
    ld d, [hl]
    ld d, d
    ld c, [hl]
    ld d, [hl]
    and d
    ld d, d
    and c
    ld d, d
    ld d, b
    and d
    ld d, d
    ld d, d
    ld c, h
    ld c, b
    and c
    ld b, h
    ld b, d
    and d
    ld b, h
    and h
    ld bc, $2700
    ld a, d
    nop
    nop
    ld b, [hl]
    ld a, d
    sbc l
    jp nz, $8000

    and d
    ld e, h
    and c
    ld e, h
    ld e, d
    and d
    ld e, h
    ld e, h
    ld d, [hl]
    ld d, d
    ld c, [hl]
    ld d, [hl]
    and d
    ld d, d
    and c
    ld d, d
    ld d, b
    and d
    ld d, d
    ld c, h
    ld b, h
    ld d, d
    and e
    ld e, h
    and h
    ld bc, $9d00
    ld a, [de]
    ld l, a
    jr nz, jr_001_79ed

    ld e, h
    and c
    ld e, h
    ld e, h
    and d
    ld e, h
    and c
    ld e, h
    ld e, h
    and d
    ld c, [hl]

jr_001_7a56:
    ld d, d
    ld d, [hl]
    ld bc, $5ca2
    and c
    ld e, h
    ld e, h
    and d
    ld e, h
    and c
    ld e, h
    ld e, h
    and d
    ld d, d
    ld c, h
    ld b, h
    ld bc, $01a5
    ld [hl], d
    ld a, d
    nop
    nop
    sub c
    ld a, d
    xor a
    ld a, d
    sbc l
    jp nz, $8000

    and d
    ld e, h
    and c
    ld e, h
    ld e, d
    and d
    ld e, h
    ld e, h
    ld d, [hl]
    ld d, d
    ld c, [hl]
    ld d, [hl]
    and d
    ld d, d
    and c
    ld d, d
    ld d, b
    and d
    ld d, d
    ld c, h
    ld b, h
    ld d, d
    and e
    ld e, h
    and h
    ld bc, $9d00
    jp nz, $4000

    and d
    ld c, [hl]
    and c
    ld c, [hl]
    ld d, d
    and d
    ld d, [hl]
    ld c, [hl]
    and e
    ld c, b
    ld c, b
    and d
    ld c, h
    and c
    ld c, h
    ld c, d
    and d
    ld c, h
    ld b, h
    inc [hl]
    ld c, h
    and e
    ld c, h
    and l
    ld bc, $9d00
    ld a, [de]
    ld l, a
    jr nz, jr_001_7a56

    ld e, h
    and c
    ld e, h
    ld e, h
    and d
    ld e, h
    and c
    ld e, h
    ld e, h
    and d
    ld c, [hl]
    ld d, d
    and c
    ld d, [hl]
    ld d, [hl]
    and d
    ld d, [hl]
    and d
    ld e, h

jr_001_7ac7:
    and c
    ld e, h
    ld e, h
    and d
    ld e, h
    and c
    ld e, h
    ld e, h
    and d
    ld d, d
    ld c, h
    and c
    ld b, h
    ld b, h
    and d
    ld bc, $01a5
    nop
    db $e4
    ld a, d
    nop
    nop
    inc bc
    ld a, e
    jr nz, jr_001_7b5d

    ld c, d
    ld a, e
    sbc l
    jp nz, $8000

    and d
    ld e, h
    and c
    ld e, h
    ld e, d
    and d
    ld e, h
    ld e, h
    ld d, [hl]
    ld d, d
    ld c, [hl]
    ld d, [hl]
    and d
    ld d, d
    and c
    ld d, d
    ld d, b
    and d
    ld d, d
    ld c, h
    ld b, h
    ld d, d
    and e
    ld e, h
    and h
    ld bc, $9d00
    or d
    nop
    add b
    and d
    ld c, [hl]
    and c
    ld c, [hl]
    ld d, d
    and d
    ld d, [hl]
    ld c, [hl]
    and e
    ld c, b
    ld c, b
    and d
    ld c, h
    and c
    ld c, h
    ld c, d
    and d
    ld c, h
    ld b, h
    inc [hl]
    ld c, h
    and e
    ld c, h
    and l
    ld bc, $1a9d
    ld l, a
    jr nz, jr_001_7ac7

    ld e, h
    and c
    ld e, h
    ld e, h
    and d
    ld e, h
    and c
    ld e, h
    ld e, h
    ld c, [hl]
    ld d, [hl]
    ld e, h
    ld d, [hl]
    ld c, [hl]
    ld b, h
    ld a, $44
    and d
    ld e, h
    and c
    ld e, h
    ld e, h
    and d
    ld e, h
    and c
    ld e, h
    ld e, h
    ld d, d
    ld c, h
    ld b, h
    ld c, h
    ld e, h
    ld bc, $01a2
    and l
    ld bc, $0ba2
    dec bc
    dec bc
    dec bc
    and d
    dec bc
    dec bc
    dec bc
    ld bc, $0ba2
    dec bc
    dec bc
    dec bc
    and d
    dec bc
    dec bc
    dec bc

jr_001_7b5d:
    ld bc, $01a5
    ld [hl], d
    ld a, e
    ret


    ld a, e
    nop
    nop
    sub c
    ld a, e
    db $ed
    ld a, e
    and e
    ld a, e
    db $fd
    ld a, e
    or [hl]
    ld a, e
    dec c
    ld a, h
    sbc l
    pop de
    nop
    add b
    and d
    ld e, h
    and c
    ld e, h
    ld e, d
    and d
    ld e, h
    ld e, h
    ld d, [hl]
    ld d, d
    ld c, [hl]
    ld d, [hl]
    and d
    ld d, d
    and c
    ld d, d
    ld d, b
    and d
    ld d, d
    ld d, d
    ld c, h
    ld c, b
    ld b, h
    and c
    ld c, h
    ld d, d
    nop
    and d
    ld d, d
    and a
    ld bc, $44a2
    ld b, h
    ld b, h
    ld bc, $a74c
    ld bc, $3aa2
    ld a, [hl-]
    ld a, [hl-]
    ld bc, $a200
    ld e, h
    and a
    ld bc, $4ea2
    ld d, d
    ld d, [hl]
    ld bc, $5ca2
    and a
    ld bc, $44a2
    ld c, b
    ld c, h
    ld bc, $a200
    ld b, $a7
    ld bc, $0ba2
    dec bc
    dec bc
    ld bc, $06a2
    and a
    ld bc, $0ba2
    dec bc
    dec bc
    ld bc, $a200
    ld c, b
    and c
    ld c, b
    ld d, d
    and d
    ld b, h
    and c
    ld b, h
    ld d, d
    and d
    ld b, d
    and c
    ld b, d
    ld d, d
    and d
    ld c, b
    and c
    ld c, b
    ld d, d
    and d
    ld c, h
    and c
    ld c, h
    ld d, d
    and d
    ld c, b
    and c
    ld c, b
    ld d, d
    and d
    ld e, h
    ld d, d
    and e
    ld e, h
    nop
    ld bc, $013a
    ld a, [hl-]
    ld bc, $013a
    ld a, [hl-]
    ld bc, $013a
    ld a, [hl-]
    ld bc, $a33a
    inc [hl]
    ld bc, HeaderROMSize
    ld c, b
    ld bc, HeaderROMSize
    ld c, b
    ld bc, HeaderNewLicenseeCode
    ld b, h
    ld bc, $a34c
    ld b, h
    and d
    ld bc, $010b
    dec bc
    ld bc, $010b
    dec bc
    ld bc, $010b
    dec bc
    and d

jr_001_7c1b:
    ld bc, $0b0b
    ld bc, $7c29
    nop
    nop
    ld e, [hl]
    ld a, h
    sub d
    ld a, h
    add $7c

jr_001_7c29:
    sbc l
    or e
    nop
    add b
    and [hl]
    ld d, d
    and c
    ld d, b
    and [hl]
    ld d, d
    and c
    ld d, b
    and [hl]
    ld d, d
    and c
    ld c, b
    and e
    ld bc, $4ca6
    and c
    ld c, d
    and [hl]
    ld c, h
    and c
    ld c, d
    and [hl]
    ld c, h
    and c
    ld b, d
    and e
    ld bc, $3ea6
    and c
    ld b, d
    and [hl]
    ld b, h
    and c
    ld c, b
    and [hl]
    ld c, h
    and c
    ld d, b
    and [hl]
    ld d, d
    and c
    ld d, [hl]
    and [hl]
    ld d, d
    and c
    ld l, d
    nop
    sbc l
    sub e
    nop
    ret nz

    and [hl]
    ld b, d
    and c
    ld b, b
    and [hl]
    ld b, d
    and c
    ld b, b
    and [hl]
    ld b, d
    and c
    ld b, d
    and e
    ld bc, $3aa6
    and c
    jr c, jr_001_7c1b

    ld a, [hl-]
    and c
    jr c, @-$58

    ld a, [hl-]
    and c
    ld a, [hl-]

Call_001_7c7c:
    and e
    ld bc, $38a6
    and c
    jr c, jr_001_7c29

    ld a, [hl-]
    and c
    ld a, $a6
    ld b, d
    and c
    ld b, h
    and [hl]
    ld c, b
    and c
    ld c, h
    and [hl]
    ld b, d
    and c
    ld b, d
    sbc l
    ld a, [de]
    ld l, a
    and b
    and [hl]
    ld c, b
    and c
    ld b, [hl]
    and [hl]
    ld c, b
    and c
    ld b, [hl]
    and [hl]
    ld c, b
    and c
    ld d, d
    and e
    ld bc, $44a6
    and c
    ld b, d
    and [hl]
    ld b, h
    and c
    ld b, d
    and [hl]
    ld b, h
    and c
    ld c, h
    and e
    ld bc, $48a6
    and c
    ld a, [hl-]
    and [hl]
    ld a, $a1
    ld b, d
    and [hl]
    ld b, h
    and c
    ld c, b
    and [hl]
    ld c, h
    and c
    ld d, b
    and [hl]
    ld d, d
    and c
    ld a, [hl-]
    and [hl]
    dec bc
    and c
    ld b, $a6
    dec bc
    and c
    ld b, $a6
    dec bc
    and c
    ld b, $a3
    ld bc, $0ba6
    and c
    ld b, $a6
    dec bc
    and c
    ld b, $a6
    dec bc
    and c
    ld b, $a3
    ld bc, $0ba6
    and c
    ld b, $a6
    dec bc
    and c
    ld b, $a6
    dec bc
    and c
    ld b, $a3
    ld bc, $0ba6
    and c
    db $06

    db $29, $7d, $ff, $ff, $fc, $7c, $24, $7d, $30, $7d, $56, $7d, $7d, $7d, $56, $7d
    db $9f, $7d, $c1, $7d

    rst $38
    rst $38
    cp $7c

    db $36, $7d, $67, $7d, $8e, $7d, $67, $7d, $b0, $7d, $02, $7e

    rst $38
    rst $38
    ld c, $7d

    db $39, $7d, $3c, $7d, $ff, $ff, $1e, $7d, $9d, $60, $00, $81, $00, $9d, $20, $00
    db $81, $aa, $01, $00, $a3, $01, $50, $54, $58, $00, $a5, $01, $00, $a5, $01, $00
    db $a3, $01, $06, $01, $06, $01, $a2, $06, $06, $a3, $01, $06, $a3, $01, $06, $01
    db $06, $01, $a2, $06, $06, $01, $01, $06, $06, $00, $a7, $5a, $a2, $5e, $a7, $5a
    db $a2, $58, $a7, $58, $a2, $54, $a7, $58, $a2, $54, $00, $9d, $fa, $6e, $20, $a2
    db $5a, $62, $68, $70, $5a, $62, $68, $70, $5a, $64, $66, $6c, $5a, $64, $66, $6c
    db $00, $a7, $54, $a2, $50, $a7, $54, $a2, $50, $a7, $50, $a2, $4c, $a7, $4a, $a2
    db $50, $00, $58, $5e, $64, $6c, $58, $5e, $64, $6c, $50, $54, $58, $5e, $50, $58
    db $5e, $64, $00, $a7, $54, $a2, $50, $a7, $54, $a2, $50, $a7, $50, $a2, $4c, $a7
    db $4a, $a2, $46, $00, $58, $5e, $64, $6c, $58, $5e, $64, $6c, $50, $54, $58, $5e
    db $50, $58, $5e, $64, $00, $a7, $4a, $a2, $4c, $a7, $4a, $a2, $46, $a7, $46, $a2
    db $44, $a7, $46, $a2, $4a, $a7, $4c, $a2, $50, $a7, $4c, $a2, $4a, $a7, $4a

    and d
    ld b, [hl]
    and a
    ld c, d
    and d
    ld c, h
    and a
    ld d, b
    and d
    ld c, [hl]
    and a
    ld d, b
    and d
    ld d, d
    and a
    ld e, b
    and d
    ld d, h
    and a
    ld e, d
    and d
    ld d, h
    and a
    ld d, d
    and d
    ld d, b
    and a
    ld c, h
    and d
    ld c, d
    and d
    ld b, d
    jr c, jr_001_7e39

    ld c, d
    and e
    ld b, d
    db $01
    nop

    db $4a, $52, $58, $5e, $4a, $58, $5e, $62, $54, $62, $68, $6c, $54, $62, $68, $6c
    db $46, $4c, $54, $5e, $46, $4c, $54, $5a, $50

    ld e, b
    ld e, [hl]
    ld h, h
    ld d, b
    ld e, [hl]
    ld h, h
    ld l, h
    ld c, d
    ld d, b
    ld e, b
    ld e, [hl]
    ld c, d
    ld e, b
    ld e, [hl]
    ld h, d
    ld c, [hl]
    ld d, h
    ld e, d
    ld h, d
    ld c, [hl]
    ld d, h
    ld e, d
    ld h, [hl]
    ld d, b
    ld e, b
    ld e, [hl]
    ld h, h
    ld d, b
    ld e, [hl]
    ld h, h

jr_001_7e39:
    ld l, b
    xor b
    ld e, d
    and e
    ld bc, $4900
    ld a, [hl]
    nop
    nop
    ld e, c
    ld a, [hl]
    ld l, b
    ld a, [hl]
    ld a, b
    ld a, [hl]
    sbc l
    or c
    nop
    add b
    and a
    ld bc, $5ea1
    ld e, [hl]
    and [hl]
    ld l, b
    and c
    ld e, [hl]
    and h
    ld l, b
    nop
    sbc l
    sub c
    nop
    add b
    and a
    ld bc, $54a1
    ld d, h
    and [hl]
    ld e, [hl]
    and c
    ld e, b
    and h
    ld e, [hl]
    sbc l
    ld a, [de]
    ld l, a
    jr nz, @-$57

    ld bc, $4ea1
    ld c, [hl]
    and [hl]
    ld e, b
    and c
    ld d, b
    and e
    ld e, b
    ld bc, $01a7
    and c
    ld b, $06
    and [hl]
    dec bc
    and c
    ld b, $a0
    ld b, $06
    ld b, $06
    ld b, $06
    ld b, $06
    and e
    ld bc, $7eb6
    inc hl
    ld a, a
    or [hl]
    ld a, [hl]
    ld l, [hl]
    ld a, a
    rst $38
    rst $38
    adc h
    ld a, [hl]
    ldh [$ff7e], a
    ld c, d
    ld a, a
    ldh [$ff7e], a
    sub c
    ld a, a
    rst $38
    rst $38
    sbc b
    ld a, [hl]
    or $7e
    ld e, h
    ld a, a
    or $7e
    xor c
    ld a, a
    rst $38
    rst $38
    and h
    ld a, [hl]
    inc c
    ld a, a
    rst $38
    rst $38
    or b
    ld a, [hl]
    sbc l
    add d
    nop
    add b
    and d
    ld d, h
    and c
    ld d, h
    ld d, h
    ld d, h
    ld c, d
    ld b, [hl]
    ld c, d
    and d
    ld d, h
    and c
    ld d, h
    ld d, h
    ld d, h
    ld e, b
    ld e, h
    ld e, b
    and d
    ld d, h
    and c
    ld d, h
    ld d, h
    ld e, b
    ld d, h
    ld d, d
    ld d, h
    and c
    ld e, b
    ld e, h
    ld e, b
    ld e, h
    and d
    ld e, b
    and c
    ld d, [hl]
    ld e, b
    nop
    sbc l
    ld h, d
    nop
    add b
    and d
    ld bc, HeaderNewLicenseeCode
    ld b, b
    ld bc, HeaderNewLicenseeCode
    ld b, [hl]
    ld bc, HeaderNewLicenseeCode
    ld b, h
    ld bc, $0140
    ld b, b
    nop
    sbc l
    ld a, [de]
    ld l, a
    jr nz, @-$5c

    ld d, h
    ld d, h
    ld c, d
    ld d, d
    ld d, h
    ld d, h
    ld c, d
    ld e, b
    ld d, h
    ld d, h
    ld d, d
    ld d, h
    ld c, [hl]
    ld d, h
    ld c, d
    ld d, d
    nop
    and d
    ld b, $0b
    ld b, $0b
    ld b, $0b
    ld b, $0b
    ld b, $0b
    ld b, $0b
    ld b, $a1
    dec bc
    dec bc
    ld b, $a2
    dec bc
    and c
    ld b, $00
    and d
    ld e, [hl]
    and c
    ld e, [hl]
    ld e, [hl]
    ld e, [hl]
    ld d, h
    ld d, b
    ld d, h
    and d
    ld e, [hl]
    and c
    ld e, [hl]
    ld e, [hl]
    ld e, [hl]
    ld h, d
    ld h, [hl]
    ld h, d
    and d
    ld e, [hl]
    and c
    ld e, [hl]
    ld e, h
    and d
    ld e, b
    and c
    ld e, b
    ld d, h
    and c
    ld d, d
    ld d, h
    ld d, d
    ld d, h
    and d
    ld d, d
    and c
    ld c, [hl]
    ld d, d
    nop
    and d
    ld bc, HeaderSGBFlag
    ld c, d
    ld bc, HeaderSGBFlag
    ld c, d
    ld bc, HeaderSGBFlag
    ld b, [hl]
    ld bc, HeaderSGBFlag
    ld b, [hl]
    nop
    and d
    ld b, [hl]
    ld d, h
    ld d, h
    ld d, h
    ld b, [hl]
    ld d, h
    ld d, h
    ld d, h
    ld b, [hl]
    ld d, h
    ld d, d
    ld e, b
    ld b, h
    ld d, d
    ld c, d
    ld e, b
    nop
    and d
    ld h, d
    and c
    ld h, d
    ld h, d
    ld h, d
    ld e, [hl]
    ld e, d
    ld e, [hl]
    and d
    ld h, d
    and c
    ld h, d
    ld h, d
    ld h, d
    ld e, [hl]
    ld e, d
    ld e, [hl]
    and d
    ld h, d
    and c
    ld c, d
    ld c, [hl]
    and d
    ld d, d
    and c
    ld c, d
    ld e, h
    and e
    ld e, b
    and c
    ld d, h
    and [hl]
    ld l, h
    nop
    and d
    ld bc, HeaderDestinationCode
    ld c, d
    ld bc, HeaderDestinationCode
    ld c, d
    ld bc, $46a1
    ld b, [hl]
    and d
    ld b, [hl]
    and c
    ld b, [hl]
    ld b, [hl]
    and e
    ld b, [hl]
    and d
    ld b, h
    ld bc, $a200
    ld b, d
    ld e, d
    ld d, b
    ld e, d
    ld b, d
    ld e, d
    ld d, b
    ld e, d
    ld c, d
    and c
    ld d, d
    ld d, d
    and d
    ld d, d
    and c
    ld d, d
    ld d, d
    and e
    ld d, d
    and d
    ld d, h
    ld bc, $0000
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop

Jump_001_7fef:
    nop

Call_001_7ff0:
    jp Jump_001_6553


Call_001_7ff3:
    jp Jump_001_69d6


    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
