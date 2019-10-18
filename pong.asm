INCLUDE "includes/gbhw.inc"

_DMACODE EQU $FF80
_OAMDATA EQU _RAM                 ; Must be a multiple of $100
_OAMDATALENGTH EQU $A0

; i'm lazy
LEFTBORDER EQU 8
RIGHTBORDER EQU 160
TOPBORDER EQU 8+8
BOTTOMBORDER EQU 144+8

               RSSET _OAMDATA     ; Base location is _OAMDATA
BallYPos       RB 1               ; Set each to an incrementing location
BallXPos       RB 1
BallTileNum    RB 1

               RSSET _OAMDATA+_OAMDATALENGTH
_INPUT         RB 1               ; Put input data at the end of the oam data
_SEED          RB 1
_LASTINPUT     RB 1

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
    ldh [rOBP0],a                 ; Set sprite palette 0
    ldh [rOBP1],a                 ; And palette 1

    call StopLCD                  ; Need to stop LCD before loading vram

    ld hl, Sprites                ; Load the tile data into Vram
    ld de, _VRAM
    ld bc, 16*(SpritesEnd-Sprites)
    call mem_Copy

    ld a, 0                       ; Clear sprite table
    ld hl, _OAMDATA
    ld bc, _OAMDATALENGTH
    call mem_Set

    call StartLCD                 ; Free to start the LCD again

initsprite:
    ld a, 64                      ; Initialize ball sprite
    ld [BallYPos], a
    ld a, 16
    ld [BallXPos], a
    ld a, 1
    ld [BallTileNum], a

loop:
    halt
    nop                           ; Always need nop after halt

    call getinput

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

moveleft:
    push af

    ld a, [BallXPos]

    cp LEFTBORDER                ; compare with left edge of screen to see if we should skip move
    jr z, .popret

    dec a
    ld [BallXPos], a
.popret:
    pop af
    ret

moveright:
    push af

    ld a, [BallXPos]

    cp RIGHTBORDER
    jr z, .popret

    inc a
    ld [BallXPos], a
.popret:
    pop af
    ret

moveup:
    push af

    ld a, [BallYPos]

    cp TOPBORDER
    jr z, .popret

    dec a
    ld [BallYPos], a
.popret:
    pop af
    ret

movedown:
    push af

    ld a, [BallYPos]

    cp BOTTOMBORDER
    jr z, .popret

    inc a
    ld [BallYPos], a
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

Sprites: {{ sprites("blank", "ball") }}
SpritesEnd:
