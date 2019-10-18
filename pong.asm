INCLUDE "includes/gbhw.inc"

_DMACODE EQU $FF80
_OAMDATA EQU _RAM                 ; Must be a multiple of $100
_OAMDATALENGTH EQU $A0

              RSSET _OAMDATA      ; Base location is _OAMDATA
BallYPos      RB 1                ; Set each to an incrementing location
BallXPos      RB 1
BallTileNum RB 1

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

    jr loop

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
