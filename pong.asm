; we need boilerplate - we gotta start at $0100,
; but code actually starts at $0150.
SECTION "entry point", ROM0[$0100]
    nop
    jp $0150

SECTION "main", ROM0[$0150] ; the GB starts running code
                            ; at $0150.
    ; specify palette.
    ld a, %11100100         ; 3 2 1
    ld [$ff47], a

    ld hl, $8000            ; load tiles (starting at $8000
                            ; ) into a reg.
    ld bc, `00112233        ; rgbds can write pixel stuff
                            ; directly.
    REPT 8                  ; and then put that in hl.
    ld a, b
    ld [hl+], a
    ld a, c
    ld [hl+], a
    ENDR

_halt:
    halt
    nop
    jr _halt
