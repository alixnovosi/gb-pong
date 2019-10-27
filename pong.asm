INCLUDE "includes/gbhw.inc"

_DMACODE EQU $FF80
_OAMDATA EQU _RAM                 ; Must be a multiple of $100
_OAMDATALENGTH EQU $A0

; i'm lazy
LEFTBORDER EQU 8
RIGHTBORDER EQU 160
TOPBORDER EQU 8+8
BOTTOMBORDER EQU 144+8

PADSPEED EQU 2

                     RSSET _OAMDATA     ; Base location is _OAMDATA
BallYPos             RB 1               ; Set each to an incrementing location
BallXPos             RB 1
BallTileNum          RB 1
BallAttrs            RB 1
PlayerPadTopYPos     RB 1
PlayerPadTopXPos     RB 1
PlayerPadTopTileNum  RB 1
PlayerPadTopAttrs    RB 1
PlayerPadMidYPos     RB 1
PlayerPadMidXPos     RB 1
PlayerPadMidTileNum  RB 1
PlayerPadMidAttrs    RB 1
PlayerPadBotYPos     RB 1
PlayerPadBotXPos     RB 1
PlayerPadBotTileNum  RB 1
PlayerPadBotAttrs    RB 1

               RSSET _OAMDATA+_OAMDATALENGTH
_INPUT         RB 1               ; Put input data at the end of the oam data
_SEED          RB 1
_LASTINPUT     RB 1
_BallSpeedY    RB 1
_BallSpeedX    RB 1
_BallYDir      RB 1               ; 1 is down
_BallXDir      RB 1               ; 1 is right
_MaxBallSpeed  RB 1
_MinBallSpeed  RB 1
_P1PadSpeed    RB 1


; stolen macro to help write colors.
dcolor: MACRO  ; $rrggbb -> gbc representation
_r = ((\1) & $ff0000) >> 16 >> 3
_g = ((\1) & $00ff00) >> 8  >> 3
_b = ((\1) & $0000ff) >> 0  >> 3
    dw (_r << 0) | (_g << 5) | (_b << 10)
    ENDM

SECTION "Vblank", ROM0[$0040]
    jp _DMACODE

SECTION "LCDC", ROM0[$0048]
    reti

SECTION "Time_Overflow", ROM0[$0050]
    reti

SECTION "Serial", ROM0[$0058]
    reti

SECTION "p1thru4", ROM0[$0060]
    reti

SECTION "start", ROM0[$0100]
    nop
    jp main

    ROM_HEADER ROM_NOMBC, ROM_SIZE_32KBYTE, RAM_SIZE_0KBYTE

INCLUDE "includes/memory.asm"


main:
    nop
    di                            ; disable interrupts
    ld sp, $ffff                  ; set the stack pointer to the highest memory location

    call initdma                  ; move dma code to hram

    ld a, IEF_VBLANK              ; enable the vblank interrupt
    ld [rIE], a

    ei                            ; re-enable interrupts


initscreen:
    ld a, %11100100               ; Palette colors, darkest to lightest

    ld [rBGP], a                  ; Set background palette
    ldh [rOBP0], a                ; Set sprite palette 0
    ldh [rOBP1], a                ; And palette 1

    call StopLCD                  ; Need to stop LCD before loading vram

    ld hl, Sprites                ; Load the tile data into Vram
    ld de, _VRAM
    ld bc, 16*(SpritesEnd-Sprites)
    call mem_Copy

    ld a, 0                       ; Clear sprite table
    ld hl, _OAMDATA
    ld bc, _OAMDATALENGTH
    call mem_Set

    call StartLCD                 ; free to start the LCD again


initsprite:
    ld a, 30                      ; initialize ball sprite
    ld [BallYPos], a
    ld a, 16
    ld [BallXPos], a
    ld a, 1
    ld [BallTileNum], a
    ld a, %00000000
    ld [BallAttrs], a

    ld a, 80                      ; and paddle
    ld [PlayerPadTopYPos], a
    ld a, 16
    ld [PlayerPadTopXPos], a
    ld a, 2
    ld [PlayerPadTopTileNum], a
    ld a, %00000000
    ld [PlayerPadTopAttrs], a

    ld a, 88                      ; and paddle mid
    ld [PlayerPadMidYPos], a
    ld a, 16
    ld [PlayerPadMidXPos], a
    ld a, 3
    ld [PlayerPadMidTileNum], a
    ld a, %00000000
    ld [PlayerPadMidAttrs], a

    ld a, 96                      ; and paddle bottom
    ld [PlayerPadBotYPos], a
    ld a, 16
    ld [PlayerPadBotXPos], a
    ld a, 4
    ld [PlayerPadBotTileNum], a
    ld a, %00000000
    ld [PlayerPadBotAttrs], a

    ld a, 1
    ld [_BallSpeedY], a
    ld a, 1
    ld [_BallSpeedX], a
    ld a, 1
    ld [_BallYDir], a
    ld a, 1
    ld [_BallXDir], a
    ld a, 5
    ld [_MaxBallSpeed], a
    ld a, 1
    ld [_MinBallSpeed], a
    ld a, 2
    ld [_P1PadSpeed], a


loop:
    halt
    nop                           ; always need nop after halt

    call getinput
    call moveball

    ld a, [_INPUT]                ; load input

    push af                       ; avoid clobbering a with the and
    and PADF_LEFT                 ; see if left is pressed...
    call nz, p1moveleft
    pop af

    push af                       ; don't clobber the a again
    and PADF_RIGHT                ; ...right
    call nz, p1moveright
    pop af

    push af
    and PADF_UP                   ; ...up
    call nz, p1moveup
    pop af

    push af
    and PADF_DOWN                 ; and down
    call nz, p1movedown
    pop af

    jr loop


getinput:
    push af
    push bc

    ; could save lastinput here by copying [_INPUT] into a and then a into somewhere else.
    ld a, %00100000               ; select bit 5 for button keys
    ld [rP1], a

    ld a, [rP1]                   ; Read several times to let the values straighten out
    ld a, [rP1]
    ld a, [rP1]
    ld a, [rP1]

    and $0F                       ; take the bottom four bits
    swap a                        ; swap upper and lower
    ld b, a                       ; save button input in b

    ld a, %00010000               ; choose bit 4 for joystick
    ld [rP1], a

    ld a, [rP1]                   ; Read several times to let the values straighten out
    ld a, [rP1]
    ld a, [rP1]
    ld a, [rP1]

    and $0F                       ; take the bottom four bits
    or  b                         ; combine with the button input saved in b

    cpl                           ; inverse the bits so that 1 is pressed

    ld [_INPUT], a                ; save the result

    pop bc
    pop af
    ret


; leaves 1 or 0 in a.
ballinplayerpaddle:
    push bc

    ld a, [BallXPos]
    ld b, a

    ld a, [PlayerPadTopXPos]     ; we (unfortunately) need to compare to four different bounds.
    sub 8
    sub b
    jr nc, .nocol

    ld a, [PlayerPadTopXPos]
    add 1
    sub b
    jr c, .nocol

    ld a, [BallYPos]
    ld b, a

    ld a, [PlayerPadTopYPos]
    sub 8
    sub b
    jr nc, .nocol

    ld a, [PlayerPadBotYPos]
    add 1
    sub b
    jr c, .nocol

.col
    ld a, 1
    jp .popret

.nocol
    ld a, 0

.popret
    pop bc
    ret

; same
balloobx:
    push bc

    ld a, [BallXPos]
    ld b, a

    ld a, LEFTBORDER
    sub b
    jr nc, .col

    ld a, RIGHTBORDER
    sub b
    jr c, .col

.nocol
    ld a, 0
    jp .popret

.col
    ld a, 1

.popret
    pop bc
    ret


ballooby:
    push bc

    ld a, [BallYPos]
    ld b, a

    ld a, TOPBORDER
    sub b
    jr nc, .col

    ld a, BOTTOMBORDER
    cp b
    jr c, .col

.nocol
    ld a, 0
    jp .popret

.col
    ld a, 1

.popret
    pop bc
    ret


moveball:
    push af
    push bc
    push hl

    call doballxmove
    call doballymove

.popret
    pop hl
    pop bc
    pop af
    ret


doballxmove:
    push af
    push bc
    push hl

    ; update position from speed + direction
    ; abort if vx is zero.
    ld a, [_BallSpeedX]
    cp 0
    jr z, .popret

    ld a, [_BallXDir]
    cp 1
    jr z, .right

.left
    call doballleftmove
    jp .popret

.right
    call doballrightmove

.popret
    pop hl
    pop bc
    pop af
    ret


doballleftmove:
    push af
    push bc
    push de
    push hl

    ; figure out if we collide BEFORE we move.
    ; this indicates the paddle moved into us,
    ; which might not change our movement, but may also reflect the ball.
    call ballinplayerpaddle
    cp 1
    ; store precoll flag (TODO use bits, ever).
    jr z, .setup

    ld a, 0

.setup
    ld d, a

    ld a, [BallXPos]
    ld b, a
    ld a, [_BallSpeedX]

.loop
    dec b
    dec a

    ; do various collisions
    ld l, a

    ld a, b
    ld [BallXPos], a

    call balloobx
    cp 1
    jr z, .fixposwall

    call ballinplayerpaddle
    cp 1
    jr z, .fixpospaddle

    ; leave loop if counter runs out.
.postcollcheck
    ld a, l
    cp 0
    jr z, .loopdone

    jp .loop

; any fix means we hit something and the movement loop should end.
.fixposwall
    ld a, 1
    ld [_BallXDir], a

    ld a, LEFTBORDER
    add 1
    ld b, a

    call slowballxdown

    jp .loopdone

.fixpospaddle
    ; will return 1 if we did precollision.
    call checkperformprecollx
    cp 1
    jr z, .popret     ; we've already done all the movement we need.

.noprecoll
    ld a, 1
    ld [_BallXDir], a

    ld a, [PlayerPadMidXPos]
    add 9
    ld b, a

    jp .loopdone

.loopdone
    ld a, b
    ld [BallXPos], a

.popret
    pop hl
    pop de
    pop bc
    pop af
    ret


doballrightmove:
    push af
    push bc
    push de
    push hl

    ; figure out if we collide BEFORE we move.
    ; this indicates the paddle moved into us,
    ; which might not change our movement, but may also reflect the ball.
    call ballinplayerpaddle
    cp 1
    ; store precoll flag (TODO use bits, ever).
    jr z, .setup

    ld a, 0

.setup
    ld d, a

    ld a, [BallXPos]
    ld b, a
    ld a, [_BallSpeedX]

.loop
    inc b
    dec a

    ; do various collisions
    ld l, a

    ld a, b
    ld [BallXPos], a

    call balloobx
    cp 1
    jr z, .fixposwall

    call ballinplayerpaddle
    cp 1
    jr z, .fixpospaddle

    ; leave loop if counter runs out.
    ld a, l
    cp 0
    jr z, .loopdone

    jp .loop

; any fix means we hit something and the movement loop should end.
.fixposwall
    ld a, 0
    ld [_BallXDir], a

    ld a, RIGHTBORDER
    sub 1
    ld b, a

    call slowballxdown

    jp .loopdone

.fixpospaddle
    call checkperformprecollx
    cp 1
    jr z, .popret

    ld a, 0
    ld [_BallXDir], a

    ld a, [PlayerPadMidXPos]
    sub 9
    ld b, a

    jp .loopdone

.loopdone
    ld a, b
    ld [BallXPos], a

.popret
    pop hl
    pop de
    pop bc
    pop af
    ret


doballymove:
    push af
    push bc
    push hl

    ld a, [_BallSpeedY]
    cp 0
    jr z, .popret

    ld a, [_BallYDir]
    cp 1
    jr z, .down

.up
    call doballupmove
    jp .popret

.down
    call doballdownmove

.popret
    pop hl
    pop bc
    pop af
    ret


doballupmove:
    push af
    push bc
    push de
    push hl

    ; figure out if we collide BEFORE we move.
    ; this indicates the paddle moved into us,
    ; which might not change our movement, but may also reflect the ball.
    call ballinplayerpaddle
    cp 1
    ; store precoll flag (TODO use bits, ever).
    jr z, .setup

    ld a, 0

.setup
    ld d, a

    ld a, [BallYPos]
    ld b, a
    ld a, [_BallSpeedY]

.loop
    dec b
    dec a

    ; do various collisions
    ld l, a

    ld a, b
    ld [BallYPos], a

    call ballooby
    cp 1
    jr z, .fixposwall

    call ballinplayerpaddle
    cp 1
    jr z, .fixpospaddle

    ; leave loop if counter runs out.
    ld a, l
    cp 0
    jr z, .loopdone

    jp .loop

; any fix means we hit something and the movement loop should end.
.fixposwall
    ld a, 1
    ld [_BallYDir], a

    ld a, TOPBORDER
    add 1
    ld b, a

    call slowballydown

    jp .loopdone

.fixpospaddle
    ; will return 1 if we did precollision.
    call checkperformprecolly
    cp 1
    jr z, .popret     ; we've already done all the movement we need.

    ld a, 1
    ld [_BallYDir], a

    ld a, [PlayerPadBotYPos]
    add 9
    ld b, a

    jp .loopdone

.loopdone
    ld a, b
    ld [BallYPos], a

.popret
    pop hl
    pop de
    pop bc
    pop af
    ret


doballdownmove:
    push af
    push bc
    push de
    push hl

    ; figure out if we collide BEFORE we move.
    ; this indicates the paddle moved into us,
    ; which might not change our movement, but may also reflect the ball.
    call ballinplayerpaddle
    cp 1
    ; store precoll flag (TODO use bits, ever).
    jr z, .setup

    ld a, 0

.setup
    ld d, a

    ld a, [BallYPos]
    ld b, a
    ld a, [_BallSpeedY]

.loop
    inc b
    dec a

    ; do various collisions
    ld l, a

    ld a, b
    ld [BallYPos], a

    call ballooby
    cp 1
    jr z, .fixposwall

    call ballinplayerpaddle
    cp 1
    jr z, .fixpospaddle

.nofix
    ld a, l

    ; leave loop if counter runs out.
    cp 0
    jr z, .loopdone

    jp .loop

; any fix means we hit something and the movement loop should end.
.fixposwall
    ld a, 0
    ld [_BallYDir], a

    ld a, BOTTOMBORDER
    sub 1
    ld b, a

    call slowballydown

    jp .loopdone

.fixpospaddle
    ; will return 1 if we did precollision.
    call checkperformprecolly
    cp 1
    jr z, .popret     ; we've already done all the movement we need.

    ld a, 0
    ld [_BallYDir], a

    ld a, [PlayerPadTopYPos]
    sub 1
    ld b, a

    jp .loopdone

.loopdone
    ld a, b
    ld [BallYPos], a

.popret
    pop hl
    pop de
    pop bc
    pop af
    ret


; check if we had collision before we started moving and if so,
; do something about that.
checkperformprecollx:
    ; check precoll flag
    ld a, d
    cp 1
    jr nz, .noprecoll

    ; check player movement - only left and right matter.
    ld a, [_INPUT]                ; load input

    push af
    and PADF_LEFT                 ; see if left is pressed...
    jr z, .noleft

    pop af
    ; bounce left, faster.
    ld a, 0
    ld [_BallXDir], a

    ld a, [PlayerPadMidXPos]
    sub 9
    ld [BallXPos], a

    call speedballxup

    ld a, 1

    jp .popret

.noleft
    pop af
    and PADF_RIGHT              ; ...right
    jr z, .noprecoll            ; ignore collision for up or down.
                                ; y branch will handle that.
    ld a, 1
    ld [_BallXDir], a

    ld a, [PlayerPadMidXPos]
    add 8
    ld [BallXPos], a

    call speedballxup

    ld a, 1

    jp .popret

.noprecoll
    ld a, 0

.popret
    ret


; same for y
checkperformprecolly:
    ld a, d
    cp 1
    jr nz, .noprecoll

    ; here, just up and down
    ld a, [_INPUT]                ; load input

    push af
    and PADF_UP
    jr nz, .noup

    ; bounce up, faster.
    pop af
    ld a, 0
    ld [_BallYDir], a

    ld a, [PlayerPadTopYPos]
    sub 1
    ld [BallYPos], a

    ld a, 1

    jp .popret

.noup
    pop af
    and PADF_DOWN
    jr nz, .noprecoll

    ld a, 0
    ld [_BallYDir], a

    ld a, [PlayerPadBotYPos]
    add 9
    ld [BallYPos], a

    ld a, 1

    jp .popret

.noprecoll
    ld a, 0

.popret
    ret


; speed ball up, but stay under max speed.
speedballxup:
    push af
    push bc

    ; TODO stop being lazy and do a loop here too.
    ld a, [_BallSpeedX]
    add 1
    add 1

    ld b, a
    ld a, [_MaxBallSpeed]
    sub b
    jr nc, .nofix

    ld a, [_MaxBallSpeed]
    ld b, a

.nofix
    ld a, b
    ld [_BallSpeedX], a

.popret
    pop bc
    pop af
    ret


; slow ball down, but stay under min speed.
slowballxdown:
    push af
    push bc

    ld a, [_BallSpeedX]
    ld b, a
    ld a, [_MinBallSpeed]

    cp b
    jr z, .popret

    ld a, b
    sub 1
    ld [_BallSpeedX], a

.popret
    pop bc
    pop af
    ret


; speed ball up, but stay under max speed.
speedballyup:
    push af
    push bc

    ; TODO stop being lazy and do a loop here too.
    ld a, [_BallSpeedY]
    add 1
    add 1

    ld b, a
    ld a, [_MaxBallSpeed]
    sub b
    jr nc, .nofix

    ld a, [_MaxBallSpeed]
    ld b, a

.nofix
    ld a, b
    ld [_BallSpeedY], a

.popret
    pop bc
    pop af
    ret


; same for y
slowballydown:
    push af
    push bc

    ld a, [_BallSpeedY]
    ld b, a
    ld a, [_MinBallSpeed]

    cp b
    jr z, .popret

    ld a, b
    sub 1
    ld [_BallSpeedY], a

.popret
    pop bc
    pop af
    ret


p1moveleft:               ; left and right are nice because top/bot share an xpos.
    push af
    push bc
    push hl

    ld a, [PlayerPadTopXPos]
    ld b, a
    ld a, [_P1PadSpeed]

.loop
    dec b

    dec a
    cp 0
    jr z, .loopdone

    ld l, a
    ld a, b

    sub a, LEFTBORDER
    jr nc, .nofix
    ld a, LEFTBORDER
    ld b, a
    jp .loopdone

.nofix
    ld a, l
    jp .loop

.loopdone
    ld a, b
    ld [PlayerPadTopXPos], a
    ld [PlayerPadMidXPos], a
    ld [PlayerPadBotXPos], a

.popret:
    pop hl
    pop bc
    pop af
    ret


p1moveright:
    push af
    push bc
    push hl

    ld a, [PlayerPadTopXPos]
    ld b, a
    ld a, [_P1PadSpeed]

.loop
    inc b

    dec a
    cp 0
    jr z, .loopdone

    ld l, a
    ld a, RIGHTBORDER

    sub a, b
    jr nc, .nofix
    ld a, RIGHTBORDER
    ld b, a
    jp .loopdone

.nofix
    ld a, l
    jp .loop

.loopdone
    ld a, b
    ld [PlayerPadTopXPos], a
    ld [PlayerPadMidXPos], a
    ld [PlayerPadBotXPos], a

.popret:
    pop hl
    pop bc
    pop af
    ret


p1moveup:
    push af
    push bc
    push hl

    ld a, [PlayerPadTopYPos]
    ld b, a
    ld a, [_P1PadSpeed]

.loop
    dec b

    dec a
    cp 0
    jr z, .loopdone

    ld l, a
    ld a, b

    sub a, TOPBORDER
    jr nc, .nofix
    ld a, TOPBORDER
    ld b, a
    jp .loopdone

.nofix
    ld a, l
    jp .loop

.loopdone
    ld a, b
    ld [PlayerPadTopYPos], a
    add a, 8
    ld [PlayerPadMidYPos], a
    add a, 8
    ld [PlayerPadBotYPos], a

.popret:
    pop hl
    pop bc
    pop af
    ret


p1movedown:
    push af
    push bc
    push hl

    ld a, [PlayerPadBotYPos]
    ld b, a
    ld a, [_P1PadSpeed]

.loop
    inc b

    dec a
    cp 0
    jr z, .loopdone

    ld l, a
    ld a, BOTTOMBORDER

    sub a, b
    jr nc, .nofix
    ld a, BOTTOMBORDER
    ld b, a
    jp .loopdone

.nofix
    ld a, l
    jp .loop

.loopdone
    ld a, b
    ld [PlayerPadBotYPos], a
    sub a, 8
    ld [PlayerPadMidYPos], a
    sub a, 8
    ld [PlayerPadTopYPos], a

.popret:
    pop hl
    pop bc
    pop af
    ret


; DMA stuff
initdma:
    ld de, _DMACODE               ; Copy the dma code to hram
    ld hl, dmacode
    ld bc, dmaend-dmacode
    call mem_CopyVRAM
    ret


dmacode:                          ; Initiate a DMA transfer from _RAM
    push af
    ld a, _RAM/$100               ; First two bytes of transfer start location
    ldh [rDMA], a                 ; Start DMA transfer
    ld a, $28                     ; How many loops to wait


dma_wait:                         ; Wait for transfer to finish
    dec a
    jr nz, dma_wait
    pop af
    reti


dmaend:
; End DMA stuff


; If the lcd is on, wait for vblank then turn it off
StopLCD:
    ld a, [rLCDC]
    rlca                          ; Put the high bit of LCDC into the carry flag
    ret nc                        ; If screen is already off, exit

.stoplcd_wait:                    ; Loop until vblank
    ld a, [rLY]                   ; Get LCDC y coord
    cp 145                        ; Is it on line 145?
    jr nz, .stoplcd_wait          ; if not, keep waiting

    ld a, [rLCDC]                 ; Get the current LCDC val
    res 7, a                      ; reset bit 7
    ld [rLCDC], a                 ; and put it back

    ret


; Start up the LCD with required flags
StartLCD:
    ld a, LCDCF_ON|LCDCF_BG8000|LCDCF_BG9800|LCDCF_BGON|LCDCF_OBJ8|LCDCF_OBJON
    ld [rLCDC], a
    ret


Sprites: {{ sprites("blank", "ball", "ppadtop", "ppadmid", "ppadbot") }}
SpritesEnd:
