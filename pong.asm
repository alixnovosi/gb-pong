INCLUDE "includes/gbhw.asm"
INCLUDE "includes/memory.asm"

INCLUDE "constants.asm"

                     RSSET _OAM_DATA    ; Base location is _OAM_DATA
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
OppPadTopYPos        RB 1
OppPadTopXPos        RB 1
OppPadTopTileNum     RB 1
OppPadTopAttrs       RB 1
OppPadMidYPos        RB 1
OppPadMidXPos        RB 1
OppPadMidTileNum     RB 1
OppPadMidAttrs       RB 1
OppPadBotYPos        RB 1
OppPadBotXPos        RB 1
OppPadBotTileNum     RB 1
OppPadBotAttrs       RB 1

               RSSET _GAME_DATA
_INPUT         RB 1               ; Put input data at the end of the oam data
_SEED          RB 1
_LASTINPUT     RB 1
_BallSpeedY    RB 1
_BallSpeedX    RB 1
_BallYDir      RB 1               ; 1 is down
_BallXDir      RB 1               ; 1 is right
_OppSpeedY     RB 1
_OppSpeedX     RB 1
_OppYDir       RB 1               ; 1 is down
_OppXDir       RB 1               ; 1 is right
_P1PadSpeed    RB 1
_P2PadSpeed    RB 2
_p1_score      RB 3
_p2_score      RB 3
_score_changed RB 1
_rand_index    RB 1               ; index into rand data for a rand value
_rand_size     RB 1
_rand_data     RB 50

               RSSET  _TEXT_BUFFER
_text_buffer   RB 40


SECTION "Text constants", ROMX
p1_label:
    db "P1 ", 0
p1_static_test_score:
    db "00", 0
p2_label:
    db "P2 ", 0
p2_static_test_score:
    db "00", 0


SECTION "Vblank",        ROM0[$0040]
    jp _DMACODE

SECTION "LCDC",          ROM0[$0048]
    reti

SECTION "Time_Overflow", ROM0[$0050]
    reti

SECTION "Serial",        ROM0[$0058]
    reti

SECTION "p1thru4",       ROM0[$0060]
    reti

SECTION "start",         ROM0[$0100]
    nop
    jp main
    ROM_HEADER ROM_NOMBC, ROM_SIZE_32KBYTE, RAM_SIZE_0KBYTE ; fill space

SECTION "main",          ROM0[$0150]
main:
    nop
    di                            ; disable interrupts
    ld sp, $ffff                  ; set the stack pointer to the highest memory location

    call initdma                  ; move dma code to hram

    ld a, IEF_VBLANK              ; enable the vblank interrupt
    ld [rIE], a

    ei                            ; re-enable interrupts


initscreen:
    call StopLCD                  ; Need to stop LCD before loading vram

    ; set auto-increment flag for tile palettes,
    ; so the set to rOCPD in a few lines will fill in all palettes.
    ld a, %10000000
    ld [rBCPS], a

    ld hl, BGPalettes
    REPT 64
    ld a, [hl+]
    ldh [rBCPD], a
    ENDR

    ; sprite has same process
    ld a, %10000000
    ld [rOCPS], a

    ; load multiple palettes
    ld hl, SpritePalettes
    REPT 64
    ld a, [hl+]
    ldh [rOCPD], a
    ENDR

    ld hl, Tiles                ; Load the tile data into Vram
    ld de, $8000
    ld bc, 16*(TilesEnd-Tiles)
    call mem_Copy

    ; load map data into memory
    ld hl, Map
    ld de, _SCRN0
    ld bc, 40*(MapEnd-Map)
    call mem_Copy

    ld a, 1
    ld [rVBK], a

    ; and palettes for map squares
    ld hl, MapData
    ld de, _SCRN0
    ld bc, 40*(MapDataEnd-MapData)
    call mem_Copy

    xor a
    ld [rVBK], a

    ; TODO not sure if this is the best way of keeping it in sync
    ld a, 50
    ld [_rand_size], a

    xor a
    ld [_rand_index], a

    ; put random data in memory
    ld hl, RandData
    ld de, _rand_data
    ld bc, 50
    call mem_Copy

    xor a                         ; Clear sprite table
    ld hl, _OAM_DATA
    ld bc, _SECTION_LENGTH
    call mem_Set

    call StartLCD                 ; free to start the LCD again


init_score:
    ; move default scores to score vars
    ld hl, p1_static_test_score
    ld de, _p1_score
    ld bc, 3
    call mem_Copy

    ld hl, p2_static_test_score
    ld de, _p2_score
    ld bc, 3
    call mem_Copy

    xor a
    ld [_score_changed], a

    ; blank out tile 255
    ld a, 1
    ldh [rVBK], a
    xor a
    ld c, BYTES_PER_TILE
    ld hl, $8800 + ($ff - $80) * BYTES_PER_TILE
    call fill

    call init_scores


; init some game stats that happen ONLY ON GAME START
; and not on point score restart
init_game_start:
    xor a
    ld [_BallXDir], a
    ld a, 2
    ld [_P1PadSpeed], a
    ld [_P2PadSpeed], a


; init sprite. NOTE: after point score, we jump here
; TODO rename
initsprite:

    ; randomize ball speed and vert direction
    call randball

    ld a, 84
    ld [BallYPos], a
    ld [BallXPos], a
    ld a, 0
    ld [BallTileNum], a
    ld a, %00000000               ; priority, x flip, y flip, palette, tile bank, palette (3 bits)
    ld [BallAttrs], a

    ; P1 paddle
    ld a, 80                      ; and paddle
    ld [PlayerPadTopYPos], a
    ld a, 16
    ld [PlayerPadTopXPos], a
    ld a, 1
    ld [PlayerPadTopTileNum], a
    ld a, %00000001
    ld [PlayerPadTopAttrs], a

    ld a, 88                      ; and paddle mid
    ld [PlayerPadMidYPos], a
    ld a, 16
    ld [PlayerPadMidXPos], a
    ld a, 2
    ld [PlayerPadMidTileNum], a
    ld a, %00000001
    ld [PlayerPadMidAttrs], a

    ld a, 96                      ; and paddle bottom
    ld [PlayerPadBotYPos], a
    ld a, 16
    ld [PlayerPadBotXPos], a
    ld a, 1
    ld [PlayerPadBotTileNum], a
    ld a, %01000001
    ld [PlayerPadBotAttrs], a

    ; P2 paddle
    ; priority, y flip, x flip, palette, tile bank, palette (3 bits)
    ld a, 80                      ; and paddle
    ld [OppPadTopYPos], a
    ld a, 152
    ld [OppPadTopXPos], a
    ld a, 1
    ld [OppPadTopTileNum], a
    ld a, %00100010
    ld [OppPadTopAttrs], a

    ld a, 88                      ; and paddle mid
    ld [OppPadMidYPos], a
    ld a, 152
    ld [OppPadMidXPos], a
    ld a, 2
    ld [OppPadMidTileNum], a
    ld a, %00100010
    ld [OppPadMidAttrs], a

    ld a, 96                      ; and paddle bottom
    ld [OppPadBotYPos], a
    ld a, 152
    ld [OppPadBotXPos], a
    ld a, 1
    ld [OppPadBotTileNum], a
    ld a, %01100010
    ld [OppPadBotAttrs], a

    xor a
    ld [_OppXDir], a
    ld [_OppYDir], a
    ld [_OppSpeedX], a

    ld a, 2
    ld [_OppSpeedY], a

loop:
    halt
    nop                           ; always need nop after halt

    call get_input
    call move_ball
    call move_opp

    ld a, [_INPUT]                ; load input

    push af                       ; avoid clobbering a with the and
    and PADF_LEFT                 ; see if left is pressed...
    call nz, p1_move_left
    pop af

    push af                       ; don't clobber the a again
    and PADF_RIGHT                ; ...right
    call nz, p1_move_right
    pop af

    push af
    and PADF_UP                   ; ...up
    call nz, p1_move_up
    pop af

    push af
    and PADF_DOWN                 ; and down
    call nz, p1_move_down
    pop af

    call update_scores

    jr loop


get_input:
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
ball_in_player_paddle:
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

.col:
    ld a, 1
    jp .popret

.nocol:
    xor a

.popret:
    pop bc
    ret


; leaves 0 or 1 in a
ball_in_opp_paddle:
    push bc

    ld a, [BallXPos]
    ld b, a

    ld a, [OppPadTopXPos]     ; we (unfortunately) need to compare to four different bounds.
    sub 8
    sub b
    jr nc, .nocol

    ld a, [OppPadTopXPos]
    add 1
    sub b
    jr c, .nocol

    ld a, [BallYPos]
    ld b, a

    ld a, [OppPadTopYPos]
    sub 8
    sub b
    jr nc, .nocol

    ld a, [OppPadBotYPos]
    add 1
    sub b
    jr c, .nocol

.col:
    ld a, 1
    jp .popret

.nocol:
    xor a

.popret:
    pop bc
    ret


; same
ball_oob_x:
    push bc

    ld a, [BallXPos]
    ld b, a

    ld a, LEFT_BORDER
    sub b
    jr nc, .col

    ld a, RIGHT_BORDER
    sub b
    jr c, .col

.nocol:
    xor a
    jp .popret

.col:
    ld a, 1

.popret:
    pop bc
    ret


ball_oob_y:
    push bc

    ld a, [BallYPos]
    ld b, a

    ld a, TOP_BORDER
    sub b
    jr nc, .col

    ld a, BOTTOM_BORDER
    cp b
    jr c, .col

.nocol:
    xor a
    jp .popret

.col:
    ld a, 1

.popret:
    pop bc
    ret


; draw score labels and pull scores from memory to write them.
init_scores
    call wait_for_vblank

    call draw_labels
    call draw_scores

    ret


; increment score, stored as a decimal string
; a: 0 for p1, 1 for p2
increment_score:
    cp a, 1
    jp z, .p2

.p1:
    ld hl, _p1_score
    jp .loaded
.p2:
    ld hl, _p2_score

.loaded:
    ; get to second byte
    inc hl
    ld a, [hl]

    ; increment first digit and then check for an overflow
    inc a
    ld [hl], a

    sub a, 58 ; ':', one past digit 9
    jr c, .nooverflow

    ; reset first digit
    ld a, 48  ; '0'
    ld [hl], a

    ; we gotta deal with the second digit
    dec hl
    ld a, [hl]
    inc a
    ld [hl], a

.nooverflow:

    ; update the flag
    ld a, 1
    ld [_score_changed], a

    ret


draw_labels:
    ld b, P1_LABEL_OFFSET
    ld de, P1_LABEL_OFFSET_BYTES
    ld hl, p1_label
    ld c, P1_LABEL_SIZE
    call draw_score_sec

    ; garbage prevention by cleaning the palette after our last square
    ld a, P1_LABEL_OFFSET + P1_LABEL_SIZE
    ld d, P1_SCORE_OFFSET - (P1_LABEL_OFFSET + P1_LABEL_SIZE)
    call garbage_cleanup

    ld b, P2_LABEL_OFFSET
    ld de, P2_LABEL_OFFSET_BYTES
    ld hl, p2_label
    ld c, P2_LABEL_SIZE
    call draw_score_sec

    ; garbage prevention by cleaning the palette after our last square
    ld a, P2_LABEL_OFFSET + P2_LABEL_SIZE
    ld d, P2_SCORE_OFFSET - (P2_LABEL_OFFSET + P2_LABEL_SIZE) + 1
    call garbage_cleanup

    ret


; check score change flag and update score if necessary
update_scores:
    ld a, [_score_changed]
    cp 1
    jr nz, .popret

    ; reset
    xor a
    ld [_score_changed], a

    call draw_scores
    jp initsprite

.popret:
    ret


draw_scores:
    ld b, P1_SCORE_OFFSET
    ld de, P1_SCORE_OFFSET_BYTES
    ld hl, _p1_score
    ld c, P1_SCORE_SIZE
    call draw_score_sec

    ; garbage prevention by cleaning the palette after our last square
    ld a, P1_SCORE_OFFSET + P1_SCORE_SIZE
    ld d, P2_LABEL_OFFSET - (P1_SCORE_OFFSET + P1_SCORE_SIZE) + 1
    call garbage_cleanup

    ld b, P2_SCORE_OFFSET
    ld de, P2_SCORE_OFFSET_BYTES
    ld hl, _p2_score
    ld c, P2_SCORE_SIZE
    call draw_score_sec

    ; garbage prevention by cleaning the palette after our last square
    ld a, P2_SCORE_OFFSET + P2_SCORE_SIZE
    ld d, 2
    call garbage_cleanup

    ret


; draw a few characters of text
; de: tile offset (bytes, could easily overflow a single byte so it's gotta be 16bit)
; b: tile offset (number of tiles)
; c: number of tiles to write
; hl: text label
draw_score_sec:

    ; push all onto the stack so we can re-use for row 2
    push hl
    push de
    push bc

    ; SETTING UP SCORE ROW 1
    ld hl, _SCRN0

    ; gotta swap b and c here.
    push bc        ; store bc so we can get c back in a moment

    ld c, b
    ld b, 0

    add hl, bc

    pop bc     ; set c for set_score_tiles

    ; b already has TILE_OFFSET so we can just add to it
    ; or, we could, if registers were nice instead of hell
    ; just swap real quick
    ld a, b
    add a, TEXT_START_TILE_1
    ld b, a

    ; we need hl for math
    push hl

    ; de already had TILE_OFFSET_BYTES, so we can just add to THAT
    ; again, no, it's harder. but doable.
    ld h, d
    ld l, e
    ld de, $8800 + (TEXT_START_TILE_1 - $80) * BYTES_PER_TILE
    add hl, de
    ld d, h
    ld e, l

    pop hl     ; we don't need it but we gotta clear the stack

    ; c was set earlier by the pop
    call set_score_tiles

    ; restore values for a slightly modified repeat
    pop bc
    pop de
    pop hl

    ; re-store for actual draw, and cleanup pass
    push hl ; text label
    push de ; bytes offset

    ; SETTING UP SCORE ROW 2
    ; prepare hl
    ld hl, _SCRN0 + CANVAS_WIDTH_TILES

    ; gotta swap b and c here.
    push bc        ; store bc so we can get c back in a moment

    ld c, b
    ld b, 0

    add hl, bc

    pop bc         ; set c for set_score_tiles

    ; prepare b
    ld a, b
    add a, TEXT_START_TILE_1 + 1
    ld b, a

    ; we still need hl for math
    push hl

    ; prepare de
    ld h, d
    ld l, e
    ld de, $8800 + (TEXT_START_TILE_1 + 1 - $80) * BYTES_PER_TILE
    add hl, de
    ld d, h
    ld e, l

    pop hl           ; clear stack

    ; c was set earlier
    call set_score_tiles

    ; zero out text buffer
    xor a
    ld hl, _text_buffer
    ld c, TEXT_BUFFER_SIZE
    call fill

    ; pull values off the stack once more to clean up,
    ; and to fetch the bytes offset and text label.
    pop hl ; bytes offset. no, this isn't a reg mixup, we need to
           ; swap de and hl in all the other work we're doing.
    ld bc, $8800
    add hl, bc

    pop de ; text label
    call draw_text

    ret


; clean up garbage in a tile
; a: tile offset
; d: number of tiles
garbage_cleanup:
    ld c, a    ; store in c so we don't clobber, and can do a bc add later.

    ; cleanup: clear palette for one tile just before where we started
    ld a, 1
    ld [rVBK], a

    ; and palettes for map squares
    ld hl, _SCRN0

    xor b
    add hl, bc

    dec hl

    ld a, %00001000

    inc d
    push de
.loop0:
    ld [hl+], a

    dec d
    jr nz, .loop0

    dec hl
    ld bc, CANVAS_WIDTH_TILES
    add hl, bc

    pop de

.loop1:
    ld [hl-], a

    dec d
    jr nz, .loop1

    xor a
    ld [rVBK], a

    ret


; set palettes and tile indices for scoreboard tiles.
; hl: where to start filling
; de: pointer to corresponding tile to start erasing
; b: tile to start with
; c: tiles to erase
set_score_tiles:

    ; populate bank 0, the tile proper
    ld a, 0
    ldh [rVBK], a

    ; we need to get this back later
    push bc

    ld a, b
.loop0:
    ld [hl+], a

    ; each successive tile in a row increases by 2
    add a, 2
    dec c
    jr nz, .loop0

    ; populate bank 1, the bank and palette.
    ld a, 1
    ldh [rVBK], a
    ld a, %00001001 ; bank 1, palette 1

    pop bc
    push bc

    dec hl
.loop1:
    ld [hl-], a
    dec c
    jr nz, .loop1

    ; blank out the corresponding tiles
    ld h, d
    ld l, e
    ld de, 8

    ; do this before restoring c
    xor a

    pop bc

.tile_erase_loop
    REPT 8
    ld [hl+], a
    ENDR
    add hl, de
    dec c
    jr nz, .tile_erase_loop

    ret


; this is cobbled together from eevee's anise-cheezball-rising
; code.
; "cobbled together" because I want small text at several defined
; locations instead of long text at one defined location.
; as a consequence I don't fully understand this code,
; and it is a mix of verbatim copied and mostly copied with
; edited comments and commentary.
;
; hoping to not accidentally blow away my sprites this time.
;
; put text on screen
; de: text cursor + current character tiles
; hl: current VRAM tile being drawn into
draw_text:
    nop

    ; TODO we could not hard-code this but I don't think it matters.
    ld b, 2

.next_letter:
    ld a, [de]                 ; get current char
    and a                      ; if it's NUL, we're done.
    jr z, .popret
    inc de                     ; otherwise, increment

    ; get glyph from the font, which means computing font + 33*a.
    ; this requires some register juggling,
    ; because we need hl for 16-bit add, but hl has data we need.
    ; we don't need de until the next loop,
    ;so we can push it, use it for buffer space, and then restore it later.
    push de
    push hl

    ; ASCII offset? I think?
    sub a, 32
    ld hl, Font

    and a
    jr z, .skip_letter_stride

    ld de, 33                   ; 1 width byte + 16*2 tiles.

.letter_stride:
    add hl, de
    dec a
    jr nz, .letter_stride

.skip_letter_stride:
    ; move the glyph address into de, and restore hl
    ld d, h
    ld e, l

    ; read the first byte, which is the char width.
    ; this overwrites the character, but I have the glyph address,
    ; so I don't need it anymore.
    ld a, [de]
    inc de

    ; copy into current chars
    push af                       ; stash width
    ld c, 32                      ; 32 bytes per row
    ld hl, _text_buffer

    ; b is the next x position we're free to write to.
    inc b

.row_copy:
    ld a, [de]                    ; read next row of charater

    ; rotate right by b-1 pixels --- remember, b contains the
    ; x-offset within the current tile - where to start drawing
    push bc                       ; preserve b while shifting
    ld c, $ff                     ; initialize mask
    dec b
    jr z, .skip_rotate

.rotate:
    ; rotate glyph (a), but shift the mask (c), so that the
    ; left end of the mask fills up with zeroes
    rrca
    srl c
    dec b
    jr nz, .rotate

.skip_rotate:
    push af                       ; preserve glyph
    and a, c                      ; mask right pixels

    ; draw to left half of text buffer
    or a, [hl]                    ; OR with current tile
    ld [hl+], a

    ; write the remaining bits to right half
    ld a, c                       ; put mask in a...
    cpl                           ; ...to invert it...
    ld c, a                       ;... then put it back
    pop af                        ; restore unmasked glyph
    and a, c                      ; mask left pixels
    ld [hl+], a                   ; and store them

    ; clean up, and loop to next row
    inc de                        ; next row of glyph
    pop bc                        ; restore counter!
    dec c
    jr nz, .row_copy
    pop af                        ; restore width

    ; draw the buffered tiles to vram
    ; the text buffer is treated like it's 16 pixels wide, but
    ; VRAM is of course only 8 pixels wide, so we need to do this in two iterations:
    ; the left two tiles, then the right
    pop hl                        ; restore hl (VRAM)
    push af                       ; stash width, again

    call wait_for_vblank          ; always wait before drawing

    push bc
    push de

    ; draw left two tiles.
    ld c, $20
    ld de, _text_buffer

.draw_left:
    ld a, [de]

    ; this double inc fixes deinterlacing from our memory storage being different than
    ; what vram wants.
    inc de
    inc de

    ld [hl+], a
    dec c
    jr nz, .draw_left

    ; draw the right two tiles
    ld c, $20

    ; this time, start from the SECOND byte, which will grab all the bytes skipped
    ; by the previous loop
    ld de, _text_buffer + 1

.draw_right:
    ld a, [de]
    inc de
    inc de

    ld [hl+], a
    dec c
    jr nz, .draw_right

    pop de
    pop bc
    pop af                         ; restore width, again

    ; cleanup
    ; undo b increment from way above
    dec b

    ; it's possible we overflowed into the next column, in which case
    ; we want to leave hl where it is: pointing at the next column.
    ; otherwise, we want to back it up to where it was.
    ; we also need to update b, the x offset.
    add a, b                       ; a <- new x offset

    ; regardless of of whether this glyph overflowed, the VRAM
    ; pointer was left at the beginning of the next (empty) column,
    ; and it needs rewinding to the right column
    ld bc, -32                     ; move the VRAM pointer back...
    add hl, bc                     ; ... to the start of the char
    cp a, 8
    jr nc, .wrap_to_next_tile

    ; the new offset is less than 8, sot his character didn't
    ; actually draw anything in the right column. move the VRAM pointer back a second
    ; time, to the left column, which still has space left.
    add hl, bc
    jr .done_wrap

.wrap_to_next_tile:
    ; the new offset is 8 or more, so this character drew into the next char.
    ; subtract 8, but also shift the text buffer by copying all the "right" chars over
    ; the "left" chars.
    sub a, 8                       ; a >= 8: subtract char width
    push hl
    push af

    ; the easy way to do this is to walk backwards through the buffer.
    ; this leaves garbage in the right column, but that's okay --- it gets overwritten in the
    ; next loop, before the buffer is copied into VRAM
    ld hl, _text_buffer + ($40 - 1)
    ld c, $20

.shift_buffer:
    ld a, [hl-]
    ld [hl-], a
    dec c
    jr nz, .shift_buffer

    pop af
    pop hl

.done_wrap:
    ; either way, store the new x offset into b
    ld b, a

    ; and loop!
    pop de                         ; pop text pointer
    jp .next_letter

.popret:
    call wait_for_vblank

    ; remember to reset bank to 0
    xor a
    ldh [rVBK], a

    ret


move_ball:
    call ball_x_move
    call ball_y_move
    ret


ball_x_move:
    push af

    ; update position from speed + direction
    ; abort if vx is zero.
    ld a, [_BallSpeedX]
    cp 0
    jr z, .popret

    ld a, [_BallXDir]
    cp 1
    jr z, .right

.left:
    call ball_left_move
    jp .popret

.right:
    call ball_right_move

.popret:
    pop af
    ret


ball_left_move:
    push af
    push bc
    push de
    push hl

    ; figure out if we collide BEFORE we move.
    ; this indicates the paddle moved into us,
    ; which might not change our movement, but may also reflect the ball.
    call ball_in_player_paddle
    cp 1
    ; store precoll flag (TODO use bits, ever).
    jr z, .setup

    xor a

.setup:
    ld d, a

    ld a, [BallXPos]
    ld b, a
    ld a, [_BallSpeedX]

.loop:
    dec b
    dec a

    ; do various collisions
    ld l, a

    ld a, b
    ld [BallXPos], a

    call ball_oob_x
    cp 1
    jr z, .fixposwall

    call ball_in_player_paddle
    cp 1
    jr z, .fixpospaddle

    call ball_in_opp_paddle
    cp 1
    jr z, .fixposopppaddle

    ; leave loop if counter runs out.
.postcollcheck:
    ld a, l
    cp 0
    jr z, .loopdone

    jp .loop

; any fix means we hit something and the movement loop should end.
.fixposwall:
    ld a, 1
    ld [_BallXDir], a

    ld a, LEFT_BORDER
    add 1
    ld b, a

    call slow_ball_x_down

    jp .loopdone

.fixpospaddle:
    ; will return 1 if we did precollision.
    call perform_precoll_x
    cp 1
    jr z, .popret     ; we've already done all the movement we need.

    ld a, [PlayerPadMidXPos]

    jp .fixposshared

.fixposopppaddle:
    ; will return 1 if we did precollision.
    call perform_precoll_x
    cp 1
    jr z, .popret     ; we've already done all the movement we need.

    ld a, [OppPadMidXPos]

    jp .fixposshared

.fixposshared:
    add 9
    ld b, a

    ld a, 1
    ld [_BallXDir], a

.loopdone:
    ld a, b
    ld [BallXPos], a

    ; leaves 1 or 0 in a
    call check_p2_score
    cp 1
    jp nz, .popret

    xor a
    call increment_score

    ; flip ball dir
    xor a
    ld [_BallXDir], a

    ; fiddle with randomness
    call inc_rand_pointer

.popret:
    pop hl
    pop de
    pop bc
    pop af
    ret


ball_right_move:
    push af
    push bc
    push de
    push hl

    ; figure out if we collide BEFORE we move.
    ; this indicates the paddle moved into us,
    ; which might not change our movement, but may also reflect the ball.
    call ball_in_player_paddle
    cp 1
    ; store precoll flag (TODO use bits, ever).
    jr z, .setup

    xor a

.setup:
    ld d, a

    ld a, [BallXPos]
    ld b, a
    ld a, [_BallSpeedX]

.loop:
    inc b
    dec a

    ; do various collisions
    ld l, a

    ld a, b
    ld [BallXPos], a

    call ball_oob_x
    cp 1
    jr z, .fixposwall

    call ball_in_player_paddle
    cp 1
    jr z, .fixpospaddle

    call ball_in_opp_paddle
    cp 1
    jr z, .fixopppospaddle

    ; leave loop if counter runs out.
    ld a, l
    cp 0
    jr z, .loopdone

    jp .loop

; any fix means we hit something and the movement loop should end.
.fixposwall:
    xor a
    ld [_BallXDir], a

    ld a, RIGHT_BORDER
    sub 1
    ld b, a

    call slow_ball_x_down

    jp .loopdone

.fixpospaddle:
    call perform_precoll_x
    cp 1
    jr z, .popret

    ld a, [PlayerPadMidXPos]

    jp .fixposshared

.fixopppospaddle:
    call perform_precoll_x
    cp 1
    jr z, .popret

    ld a, [OppPadMidXPos]
    sub 9

    jp .fixposshared

.fixposshared:
    sub 9
    ld b, a

    xor a
    ld [_BallXDir], a
    jp .loopdone

.loopdone:
    ld a, b
    ld [BallXPos], a

    ; leaves 1 or 0 in a
    call check_p1_score
    cp 1
    jp nz, .popret

    xor a
    call increment_score

    ; flip ball dir
    ld a, 1
    ld [_BallXDir], a

    ; fiddle with randomness
    call inc_rand_pointer

.popret:
    pop hl
    pop de
    pop bc
    pop af
    ret


; check if p1 scored and put result in a.
check_p1_score:
    ; in goal area,
    sub a, RIGHT_BORDER - 4
    jp c, .noscore

    ; moving left,
    ld a, [_BallXDir]
    cp 0
    jp nz, .noscore

    call in_goal
    ret

.noscore:
    xor a
    ret


; same but for p2
check_p2_score:
    ; in goal area,
    sub a, LEFT_BORDER + 4
    jp nc, .noscore

    ; moving right,
    ld a, [_BallXDir]
    cp 1
    jp nz, .noscore

    call in_goal
    ret

.noscore:
    xor a
    ret


; subroutine just to check goal bounds,
; so it's reusable
in_goal
    ; and at the right y height
    ld a, [BallYPos]
    sub a, GOAL_Y_TOP
    jp c, .noscore

    ld a, [BallYPos]
    sub a, GOAL_Y_BOT
    jp nc, .noscore

    ld a, 1
    ret

.noscore:
    xor a
    ret


ball_y_move:
    push af

    ld a, [_BallSpeedY]
    cp 0
    jr z, .popret

    ld a, [_BallYDir]
    cp 1
    jr z, .down

.up:
    call ball_up_move
    jp .popret

.down:
    call ball_down_move

.popret:
    pop af
    ret


ball_up_move:
    push af
    push bc
    push de
    push hl

    ; figure out if we collide BEFORE we move.
    ; this indicates the paddle moved into us,
    ; which might not change our movement, but may also reflect the ball.
    call ball_in_player_paddle
    cp 1
    ; store precoll flag (TODO use bits, ever).
    jr z, .setup

    xor a

.setup:
    ld d, a

    ld a, [BallYPos]
    ld b, a
    ld a, [_BallSpeedY]

.loop:
    dec b
    dec a

    ; do various collisions
    ld l, a

    ld a, b
    ld [BallYPos], a

    call ball_oob_y
    cp 1
    jr z, .fixposwall

    call ball_in_player_paddle
    cp 1
    jr z, .fixpospaddle

    call ball_in_opp_paddle
    cp 1
    jr z, .fixposopppaddle

    ; leave loop if counter runs out.
    ld a, l
    cp 0
    jr z, .loopdone

    jp .loop

; any fix means we hit something and the movement loop should end.
.fixposwall:
    ld a, 1
    ld [_BallYDir], a

    ld a, TOP_BORDER
    add 1
    ld b, a

    call slow_ball_y_down

    jp .loopdone

.fixpospaddle:
    ; will return 1 if we did precollision.
    call perform_precoll_y
    cp 1
    jr z, .popret     ; we've already done all the movement we need.

    ld a, [PlayerPadBotYPos]
    jp .fixposshared

.fixposopppaddle:
    ld a, [OppPadBotYPos]

    jp .fixposshared

.fixposshared:
    add 9
    ld b, a

    ld a, 1
    ld [_BallYDir], a

    jp .loopdone

.loopdone:
    ld a, b
    ld [BallYPos], a

.popret:
    pop hl
    pop de
    pop bc
    pop af
    ret


ball_down_move:
    push af
    push bc
    push de
    push hl

    ; figure out if we collide BEFORE we move.
    ; this indicates the paddle moved into us,
    ; which might not change our movement, but may also reflect the ball.
    call ball_in_player_paddle
    cp 1
    ; store precoll flag (TODO use bits, ever).
    jr z, .setup

    xor a

.setup:
    ld d, a

    ld a, [BallYPos]
    ld b, a
    ld a, [_BallSpeedY]

.loop:
    inc b
    dec a

    ; do various collisions
    ld l, a

    ld a, b
    ld [BallYPos], a

    call ball_oob_y
    cp 1
    jr z, .fixposwall

    call ball_in_player_paddle
    cp 1
    jr z, .fixpospaddle

.nofix:
    ld a, l

    ; leave loop if counter runs out.
    cp 0
    jr z, .loopdone

    jp .loop

; any fix means we hit something and the movement loop should end.
.fixposwall:
    xor a
    ld [_BallYDir], a

    ld a, BOTTOM_BORDER
    sub 1
    ld b, a

    call slow_ball_y_down

    jp .loopdone

.fixpospaddle:
    ; will return 1 if we did precollision.
    call perform_precoll_y
    cp 1
    jr z, .popret     ; we've already done all the movement we need.

    ld a, [PlayerPadTopYPos]

    jp .fixposshared

.fixposopppaddle:
    ld a, [OppPadTopYPos]

    jp .fixposshared

.fixposshared:
    sub 1
    ld b, a

    xor a
    ld [_BallYDir], a

    jp .loopdone

.loopdone:
    ld a, b
    ld [BallYPos], a

.popret:
    pop hl
    pop de
    pop bc
    pop af
    ret


; check if we had collision before we started moving and if so,
; do something about that.
; TODO probably need to do this for opp too.
perform_precoll_x:
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
    xor a
    ld [_BallXDir], a

    ld a, [PlayerPadMidXPos]
    sub 9
    ld [BallXPos], a

    call speed_ball_x_up

    ld a, 1

    jp .popret

.noleft:
    pop af
    and PADF_RIGHT              ; ...right
    jr z, .noprecoll            ; ignore collision for up or down.
                                ; y branch will handle that.
    ld a, 1
    ld [_BallXDir], a

    ld a, [PlayerPadMidXPos]
    add 8
    ld [BallXPos], a

    call speed_ball_x_up

    ld a, 1

    jp .popret

.noprecoll:
    xor a

.popret:
    ret


; same for y
perform_precoll_y:
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
    xor a
    ld [_BallYDir], a

    ld a, [PlayerPadTopYPos]
    sub 1
    ld [BallYPos], a

    ld a, 1

    jp .popret

.noup:
    pop af
    and PADF_DOWN
    jr nz, .noprecoll

    xor a
    ld [_BallYDir], a

    ld a, [PlayerPadBotYPos]
    add 9
    ld [BallYPos], a

    ld a, 1

    jp .popret

.noprecoll:
    xor a

.popret:
    ret


; speed ball up, but stay under max speed.
speed_ball_x_up:
    push af
    push bc

    ; TODO stop being lazy and do a loop here too.
    ld a, [_BallSpeedX]
    add 1
    add 1

    ld b, a
    ld a, MAX_BALL_SPEED
    sub b
    jr nc, .nofix

    ld a, MAX_BALL_SPEED
    ld b, a

.nofix:
    ld a, b
    ld [_BallSpeedX], a

.popret:
    pop bc
    pop af
    ret


; slow ball down, but stay under min speed.
slow_ball_x_down:
    push af
    push bc

    ld a, [_BallSpeedX]
    ld b, a
    ld a, MIN_BALL_SPEED

    cp b
    jr z, .popret

    ld a, b
    sub 1
    ld [_BallSpeedX], a

.popret:
    pop bc
    pop af
    ret


; speed ball up, but stay under max speed.
speed_ball_y_up:
    push af
    push bc

    ; TODO stop being lazy and do a loop here too.
    ld a, [_BallSpeedY]
    add 1
    add 1

    ld b, a
    ld a, MAX_BALL_SPEED
    sub b
    jr nc, .nofix

    ld a, MAX_BALL_SPEED
    ld b, a

.nofix:
    ld a, b
    ld [_BallSpeedY], a

.popret:
    pop bc
    pop af
    ret


; same for y
slow_ball_y_down:
    push af
    push bc

    ld a, [_BallSpeedY]
    ld b, a
    ld a, MIN_BALL_SPEED

    cp b
    jr z, .popret

    ld a, b
    sub 1
    ld [_BallSpeedY], a

.popret:
    pop bc
    pop af
    ret


move_opp:
    push af

    call move_opp_y
    call move_opp_x

.popret:
    pop af
    ret


move_opp_y:
    push af
    push bc

.up_check:
    ; are we moving up?
    ld a, [_OppYDir]
    cp 0
    jr nz, .down_check

.top_border_check:
    ld a, [OppPadTopYPos]

    sub a, TOP_BORDER

    ; change direction if we're above the top border.
    jp nc, .move_up

    ; if above the border, swap it and return.
    xor a
    ld [_OppYDir], a
    jp .move_down

.move_up:
    push af
    push bc
    push hl

    ld a, [OppPadTopYPos]
    ld b, a
    ld a, [_P2PadSpeed]

.loop:
    dec b

    dec a
    cp 0
    jr z, .loopdone

    ld l, a
    ld a, b

    sub a, TOP_BORDER
    jr nc, .nofix
    ld a, TOP_BORDER
    ld b, a
    jp .loopdone

.nofix:
    ld a, l
    jp .loop

.loopdone:
    ld a, b
    ld [OppPadTopYPos], a
    add a, 8
    ld [OppPadMidYPos], a
    add a, 8
    ld [OppPadBotYPos], a

; loop done
    pop hl
    pop bc
    pop af

    jp .popret

.down_check:
    ; are we moving down?
    ld a, [_OppYDir]
    cp 1
    jr nz, .popret

.bot_border_check:
    ld a, [OppPadBotYPos]

    cp BOTTOM_BORDER

    ; change direction if we're above the top border.
    jp c, .move_down

    ; if above the border, swap it and return.
    xor a
    ld [_OppYDir], a
    jp .popret

.move_down:
    ld a, [OppPadTopYPos]
    add a, 1
    ld [OppPadTopYPos], a

    ld a, [OppPadMidYPos]
    add a, 1
    ld [OppPadMidYPos], a

    ld a, [OppPadBotYPos]
    add a, 1
    ld [OppPadBotYPos], a
    jp .popret

.popret:
    pop bc
    pop af
    ret


move_opp_x:
    push af

.popret:
    pop af
    ret


p1_move_left:                 ; left and right are nice because top/bot share an xpos.
    push af
    push bc
    push hl

    ld a, [PlayerPadTopXPos]
    ld b, a
    ld a, [_P1PadSpeed]

.loop:
    dec b

    dec a
    cp 0
    jr z, .loopdone

    ld l, a
    ld a, b

    sub a, LEFT_BORDER
    jr nc, .nofix
    ld a, LEFT_BORDER
    ld b, a
    jp .loopdone

.nofix:
    ld a, l
    jp .loop

.loopdone:
    ld a, b
    ld [PlayerPadTopXPos], a
    ld [PlayerPadMidXPos], a
    ld [PlayerPadBotXPos], a

.popret:
    pop hl
    pop bc
    pop af
    ret


p1_move_right:
    push af
    push bc
    push hl

    ld a, [PlayerPadTopXPos]
    ld b, a
    ld a, [_P1PadSpeed]

.loop:
    inc b

    dec a
    cp 0
    jr z, .loopdone

    ld l, a
    ld a, RIGHT_BORDER

    sub a, b
    jr c, .fixwall

    ld a, P1_MAX_X
    sub a, b
    jr c, .fixinviswall

    jp .nofix

.fixinviswall:
    ld a, P1_MAX_X
    ld b, a
    jp .loopdone

.fixwall:
    ld a, RIGHT_BORDER
    ld b, a
    jp .loopdone

.nofix:
    ld a, l
    jp .loop

.loopdone:
    ld a, b
    ld [PlayerPadTopXPos], a
    ld [PlayerPadMidXPos], a
    ld [PlayerPadBotXPos], a

.popret:
    pop hl
    pop bc
    pop af
    ret


p1_move_up:
    push af
    push bc
    push hl

    ld a, [PlayerPadTopYPos]
    ld b, a
    ld a, [_P1PadSpeed]

.loop:
    dec b

    dec a
    cp 0
    jr z, .loopdone

    ld l, a
    ld a, b

    sub a, TOP_BORDER
    jr nc, .nofix
    ld a, TOP_BORDER
    ld b, a
    jp .loopdone

.nofix:
    ld a, l
    jp .loop

.loopdone:
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


p1_move_down:
    push af
    push bc
    push hl

    ld a, [PlayerPadBotYPos]
    ld b, a
    ld a, [_P1PadSpeed]

.loop:
    inc b

    dec a
    cp 0
    jr z, .loopdone

    ld l, a
    ld a, BOTTOM_BORDER

    sub a, b
    jr nc, .nofix
    ld a, BOTTOM_BORDER
    ld b, a
    jp .loopdone

.nofix:
    ld a, l
    jp .loop

.loopdone:
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


SECTION "Utility code", ROM0
; idle until next vblank
wait_for_vblank:
    push af

.vblank_loop:
    ei
    halt                        ; wait for interrupt
    di
    ld a, [rLY]                 ; get LCDC y coord
    cp 145                      ; is it on line 145
    jr nz, .vblank_loop         ; if not keep waiting
    ei

    pop af

    ret


; fill c bytes starting at hl with a
; NOTE: c must not be zero
fill:
    ld [hl+], a
    dec c
    jr nz, fill
    ret


; randomize ball vertical direction
randball:

    ld a, 1
    ld [_BallSpeedX], a

    ld b, MIN_BALL_SPEED
    ld c, 2
    call get_bound_rand_data

    xor a ; TEMP TEMP TEMP TEMP TODO
    ld [_BallSpeedY], a

    ld a, 0
    ld [_BallXDir], a

    ld a, 0
    ld b, a
    ld c, 6
    call get_bound_rand_data

    ; coin flip?
    ld b, a
    ld a, 3

    sub a, b
    jp c, .down

.up:
    xor a
    jp .dir

.down:
    ld a, 1

.dir:
    ld [_BallYDir], a

    ret


; move random pointer by one,
; handling overflows
inc_rand_pointer:
    push af
    push bc

    ; need to increment index
    ld a, [_rand_index]
    ld c, a
    inc c

    ld a, [_rand_size]
    dec a
    sub a, c
    jp nc, .nofix

    ; fix overflow
    xor a
    jp .popret

.nofix:
    ld a, c

.popret:
    ld [_rand_index], a
    pop bc
    pop af
    ret


; return data at pointer in a
; increment pointer and loop if necessary
get_rand_data:
    push bc
    push hl

    ld a, [_rand_index]
    ld c, a

    xor a
    ld b, a

    ld hl, _rand_data
    add hl, bc
    ld a, [hl]

    call inc_rand_pointer

.popret:
    pop hl
    pop bc
    ret


; bound data
; b: min (inclusive)
; c: max (inclusive)
; data returned in a
get_bound_rand_data:
    ; gonna need to do some juggling
    push de

    call get_rand_data

    ; now we have a random number in a

    inc c                ; add one, because it's an inclusive bound and we're checking carry
    sub a, c

    jp c, .maxcapped

    ; the only cases are over max, under min, or in middle
    ; so if we crossed the max we can just bound and return
    ld a, c
    jp .popret

.maxcapped:
    add a, c             ; fix sub

    sub a, b
    jp nc, .mincapped

    ld a, b
    jp .popret

.mincapped:
    ; just gotta fix the sub
    add a, b

.popret:
    pop de
    ret


; preprocessor will fill in tiles according to lists inside brackets.
; Tile NamedTuple is (name, type, palette) (where '*' means 'create new one')
Tiles: {{
    preprocess_data.define_tiles(
        tiles=[
            Tile(
                name="void",
                type=BG_TYPE,
                palette="*",
                palette_only=True,
            ),
            Tile(
                name="ball",
                type=SPRITE_TYPE,
                palette="*",
            ),
            Tile(
                name="ppadtop",
                type=SPRITE_TYPE,
                palette="*",
            ),
            Tile(
                name="ppadmid",
                type=SPRITE_TYPE,
                palette="ppadtop",
            ),
            Tile(
                name="enemy_pad",
                type=SPRITE_TYPE,
                palette="*",
                palette_only=True,        # this is a clone of ppad, we just want the palette
            ),
            Tile(
                name="scoreboard",
                type=BG_TYPE,
                palette="*",
            ),
            Tile(
                name="default",
                type=BG_TYPE,
                palette="*",
            ),
            Tile(
                name="goal_mid",
                type=BG_TYPE,
                palette="scoreboard",          # this is fake, we'll replace it later.
            ),
            Tile(
                name="midline",
                type=BG_TYPE,
                palette="*",
            ),
            Tile(
                name="goal_corn",
                type=BG_TYPE,
                palette="scoreboard",
            ),
            Tile(
                name="goal_edge",
                type=BG_TYPE,
                palette="scoreboard",
            ),
        ],
    )
}}
TilesEnd:


ObjectConstants: {{ }}


Map: {{
    preprocess_data.define_map(
        tileset=[
            MapTile(
                tile="default",
                palette="default",
            ),
            MapTile(                  # player goal
                tile="goal_mid",
                palette="ppadmid",
            ),
            MapTile(
                tile="midline",
                palette="midline",
            ),
            MapTile(
                tile="scoreboard",
                palette="void",
            ),
            MapTile(
                tile="goal_mid",       # enemy goal
                palette="enemy_pad",
            ),
            MapTile(
                tile="goal_corn",
                palette="ppadmid",
            ),
            MapTile(
                tile="goal_corn",
                palette="enemy_pad",
            ),
            MapTile(
                tile="goal_edge",
                palette="ppadmid",
            ),
            MapTile(
                tile="goal_edge",
                palette="enemy_pad",
            ),
        ],
        mapfile="default_map",
        hfliplist=["midline","goal_mid","goal_corn","goal_edge",],
        vfliplist=["goal_edge","goal_corn",], # TODO why do spaces here break regex
    )
}}
MapEnd:


MapData: {{ }}
MapDataEnd

; sprites and tiles are paletted PNGs,
; so the preprocessor will extract those palettes and fill out these sections as well.
; these are done last because the map forces some reshuffling (on the bg palettes)
SpritePalettes: {{ }}
SpritePalettesEnd:


BGPalettes: {{ }}
BGPalettesEnd:


Font: {{
    preprocess_data.define_font(fontfile="font.png")
}}
FontEnd:

RandData: {{ }}
RandDataEnd:
