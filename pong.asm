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

    ld a, 88                      ; and paddle bottom
    ld [PlayerPadBotYPos], a
    ld a, 16
    ld [PlayerPadBotXPos], a
    ld a, 3
    ld [PlayerPadBotTileNum], a
    ld a, %00000000
    ld [PlayerPadBotAttrs], a

    ld a, 2
    ld [_BallSpeedY], a
    ld a, 1
    ld [_BallSpeedX], a
    ld a, 1
    ld [_BallYDir], a
    ld a, 1
    ld [_BallXDir], a
    ld a, 5
    ld [_MaxBallSpeed], a


loop:
    halt
    nop                           ; always need nop after halt

    call getinput
    call moveball

    ld a, [_INPUT]                ; load input

    push af                       ; avoid clobbering a with the and

    and PADF_LEFT                 ; see if left is pressed...
    call nz, moveleft

    pop af
    push af                       ; don't clobber the a again

    and PADF_RIGHT                ; ...right
    call nz, moveright

    pop af
    push af

    and PADF_UP                   ; ...up
    call nz, moveup

    pop af
    push af

    and PADF_DOWN                 ; and down
    call nz, movedown

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


moveball:
    push af
    push bc
    push hl

; CHECK FOR LEFT BOUNCE
.left
    ; switch vel if we hit left wall
    ld a, [BallXPos]
    cp LEFTBORDER                ; compare with left edge of screen to see if we should skip move
    jr nz, .noleft

    ; AND we are moving left
    ld a, [_BallXDir]
    cp 0
    jr nz, .noleft

    ; then swap vel
    ld a, 1
    ld [_BallXDir], a
    jp .noright                  ; if we hit the left wall, we won't hit the right one.

; CHECK FOR RIGHT BOUNCE
.noleft
.right
    ld a, [BallXPos]
    cp RIGHTBORDER
    jr nz, .noright

    ld a, [_BallXDir]
    cp 1
    jr nz, .noright

    ld a, 0
    ld [_BallXDir], a

; CHECK FOR UP BOUNCE
.noright
.up
    ld a, [BallYPos]
    cp TOPBORDER
    jr nz, .noup

    ld a, [_BallYDir]
    cp 0
    jr nz, .noup

    ld a, 1
    ld [_BallYDir], a
    jp .nodown

; CHECK FOR DOWN BOUNCE
.noup
.down
    ld a, [BallYPos]
    cp BOTTOMBORDER
    jr nz, .doxmove

    ld a, [_BallYDir]
    cp 1
    jr nz, .doxmove

    ld a, 0
    ld [_BallYDir], a

; HANDLE X MOVE
.nodown
.doxmove
    ; update position from speed + direction
    ; abort if vx is zero.
    ld a, [_BallSpeedX]
    cp 0
    jr z, .doymove

    ld a, [_BallXDir]
    cp 1
    jr z, .moveright

; LEFT
.moveleft
    ld a, [BallXPos]
    ld b, a
    ld a, [_BallSpeedX]

.loopleft
    dec b

    ; leave loop if counter runs out.
    dec a
    cp 0
    jr z, .loopleftdone

.preloopleft
    ; bounce early if dec would take us out of bounds
    ld l, a
    ld a, b
    sub a, LEFTBORDER
    jr nc, .nofixleft
    ld a, LEFTBORDER
    ld b, a
    jp .loopleftdone

.nofixleft
    ld a, l
    jp .loopleft

.loopleftdone
    ld a, b
    ld [BallXPos], a
    jp .doymove

; RIGHT
.moveright
    ld a, [BallXPos]
    ld b, a
    ld a, [_BallSpeedX]

.loopright
    inc b

    ; leave loop if counter runs out.
    dec a
    cp 0
    jr z, .looprightdone

.preloopright
    ; bounce early if dec would take us out of bounds
    ld l, a
    ld a, RIGHTBORDER
    sub a, b
    jr nc, .nofixright
    ld a, RIGHTBORDER
    ld b, a
    jp .looprightdone

.nofixright
    ld a, l
    jp .loopright

.looprightdone
    ld a, b
    ld [BallXPos], a

; HANDLE Y MOVE
.doymove
    ; abort if vx is zero.
    ld a, [_BallSpeedY]
    cp 0
    jr z, .popret

    ld a, [_BallSpeedY]
    cp 0
    jr z, .popret

    ld a, [_BallYDir]
    cp 1
    jr z, .movedown

; UP
.moveup
    ld a, [BallYPos]
    ld b, a
    ld a, [_BallSpeedY]

.loopup
    dec b

    ; leave loop if counter runs out.
    dec a
    cp 0
    jr z, .loopupdone

.preloopup
    ; bounce early if dec would take us out of bounds
    ld l, a
    ld a, b
    sub a, TOPBORDER
    jr nc, .nofixup
    ld a, TOPBORDER
    ld b, a
    jp .loopupdone

.nofixup
    ld a, l
    jp .loopup

.loopupdone
    ld a, b
    ld [BallYPos], a
    jp .popret

; DOWN
.movedown
    ld a, [BallYPos]
    ld b, a
    ld a, [_BallSpeedY]

.loopdown
    inc b

    ; leave loop if counter runs out.
    dec a
    cp 0
    jr z, .loopdowndone

.preloopdown
    ; bounce early if dec would take us out of bounds
    ld l, a
    ld a, BOTTOMBORDER
    sub a, b
    jr nc, .nofixdown
    ld a, BOTTOMBORDER
    ld b, a
    jp .loopdowndone

.nofixdown
    ld a, l
    jp .loopdown

.loopdowndone
    ld a, b
    ld [BallYPos], a

.popret
    pop hl
    pop bc
    pop af
    ret


moveleft:                     ; left and right are nice because top/bot share an xpos.
    push af
    push bc

    ld a, [PlayerPadTopXPos]

    cp LEFTBORDER             ; compare with left edge of screen to see if we should skip move
    jr z, .popret

    dec a
    ld [PlayerPadTopXPos], a
    ld [PlayerPadBotXPos], a

.popret:
    pop bc
    pop af
    ret


moveright:
    push af

    ld a, [PlayerPadTopXPos]

    cp RIGHTBORDER
    jr z, .popret

    inc a
    ld [PlayerPadTopXPos], a
    ld [PlayerPadBotXPos], a

.popret:
    pop af
    ret


moveup:
    push af

    ld a, [PlayerPadTopYPos]

    cp TOPBORDER
    jr z, .popret

    dec a
    ld [PlayerPadTopYPos], a
    ld a, [PlayerPadBotYPos]
    dec a
    ld [PlayerPadBotYPos], a

.popret:
    pop af
    ret


movedown:
    push af

    ld a, [PlayerPadBotYPos] ; swapped from top,
                             ; because bottom will hit border first when moving down

    cp BOTTOMBORDER
    jr z, .popret

    inc a
    ld [PlayerPadBotYPos], a
    ld a, [PlayerPadTopYPos]
    inc a
    ld [PlayerPadTopYPos], a

.popret:
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


Sprites: {{ sprites("blank", "ball", "ppadtop", "ppadbot") }}
SpritesEnd:
