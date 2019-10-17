; we need boilerplate - we gotta start at $0100,
; but code actually starts at $0150.
SECTION "entry point", ROM0[$0100]
    nop
    jp $0150

SECTION "main", ROM0[$0150] ; the GB starts running code
                            ; at $0150.
    ; specify palette.
    ; Set a color palette.  Those aren't exposed directly in main RAM; instead,
    ; you have to write to a register, which will then write the palette to...
    ; somewhere.  Also, colors are two bytes (RGB555), but the register is only
    ; one byte, which complicates things a bit more.
    ; Luckily, if we set the high bit of the control register to 1, it
    ; auto-increments every time we write to the write register, meaning...
    ; well, it looks like this.

    ; this is updating palette 0, the default.
    ld a, %10000000
    ld [$FF68], a

    ; make this shades of blues instead of rgb
    ; so I'm not 100% copying this.
    ld bc, %0010000000000000  ; most blue
    ld a, c
    ld [$FF00+$69], a
    ld a, b
    ld [$FF00+$69], a
    ld bc, %0010110000000000  ; less blue
    ld a, c
    ld [$FF00+$69], a
    ld a, b
    ld [$FF00+$69], a
    ld bc, %0011010000000000  ; even less blue
    ld a, c
    ld [$FF00+$69], a
    ld a, b
    ld [$FF00+$69], a
    ld bc, %0100100000000000  ; least blue
    ld a, c
    ld [$FF00+$69], a
    ld a, b
    ld [$FF00+$69], a

	ld hl, $8000			; load tiles (starting at $8000
			; ) into a reg.
	ld bc, `00112233		; rgbds can write pixel stuff
	; directly.
	REPT 8				  ; and then put that in hl.

    ld a, b
    ld [hl+], a
    ld a, c
    ld [hl+], a
    ENDR

_halt:
    halt
    nop
    jr _halt
