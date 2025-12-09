;
; long32.s - 32-bit operations with effective address in DE
;
; Effective address setup:
;   lea_iy  - DE = IY + A (signed offset)
;   lea_ix  - DE = IX + A (signed offset)
;   lea_bc  - DE = BC
;   lea_hl  - DE = HL
;
; 32-bit value is in HL:HL' (HL = low word, HL' = high word)
;
; Operations use EA in DE:
;   lload32  - load (DE) into HL:HL'
;   lstore32 - store HL:HL' to (DE)
;   lshift32 - shift (DE) left by A bits
;   rshift32 - shift (DE) right by A bits (logical)
;   ashift32 - shift (DE) right by A bits (arithmetic)
;   lor32    - OR HL:HL' into (DE)
;   land32   - AND HL:HL' into (DE)
;   lxor32   - XOR HL:HL' into (DE)
;   ladd32   - add HL:HL' to (DE)
;   lsub32   - subtract HL:HL' from (DE)
;

	.global lea_iy
	.global lea_ix
	.global lea_bc
	.global lea_hl
	.global lload32
	.global lstore32
	.global lshift32
	.global rshift32
	.global ashift32
	.global lor32
	.global land32
	.global lxor32
	.global ladd32
	.global lsub32

	.text

;
; lea_iy - DE = IY + A (signed offset)
;
lea_iy:
	push iy
	pop de
	jp lea_add

;
; lea_ix - DE = IX + A (signed offset)
;
lea_ix:
	push ix
	pop de
	; fall through to lea_add

;
; add signed A to DE
;
lea_add:
	bit 7, a
	jr nz, lea_neg
	add a, e
	ld e, a
	ret nc
	inc d
	ret
lea_neg:
	add a, e
	ld e, a
	ret c
	dec d
	ret

;
; lea_bc - DE = BC
;
lea_bc:
	ld d, b
	ld e, c
	ret

;
; lea_hl - DE = HL
;
lea_hl:
	ex de, hl
	ret

;
; lload32 - load 32-bit value from (DE) into HL:HL'
;
lload32:
	ex de, hl
	ld e, (hl)
	inc hl
	ld d, (hl)
	inc hl
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	exx
	ex de, hl
	exx
	ex de, hl		; HL = low word
	ret

;
; lstore32 - store HL:HL' to (DE)
;
lstore32:
	ex de, hl
	ld (hl), e
	inc hl
	ld (hl), d
	inc hl
	exx
	ld a, l
	exx
	ld (hl), a
	inc hl
	exx
	ld a, h
	exx
	ld (hl), a
	dec hl
	dec hl
	dec hl
	ex de, hl		; restore DE
	ret

;
; lshift32 - shift (DE) left by A bits
;
lshift32:
	or a
	ret z
	push bc
	ld b, a
	call lload32
lsh_loop:
	add hl, hl		; shift low word
	exx
	adc hl, hl		; shift high word with carry
	exx
	djnz lsh_loop
	call lstore32
	pop bc
	ret

;
; rshift32 - shift (DE) right by A bits (logical)
;
rshift32:
	or a
	ret z
	push bc
	ld b, a
	call lload32
rsh_loop:
	exx
	srl h			; shift high word
	rr l
	exx
	rr h			; shift low word with carry
	rr l
	djnz rsh_loop
	call lstore32
	pop bc
	ret

;
; ashift32 - shift (DE) right by A bits (arithmetic)
;
ashift32:
	or a
	ret z
	push bc
	ld b, a
	call lload32
ash_loop:
	exx
	sra h			; arithmetic shift high word
	rr l
	exx
	rr h			; shift low word with carry
	rr l
	djnz ash_loop
	call lstore32
	pop bc
	ret

;
; lor32 - OR HL:HL' into (DE)
;
lor32:
	push bc
	ld b, h
	ld c, l
	exx
	push hl			; save high word
	exx
	call lload32		; load (DE) into HL:HL'
	ld a, l
	or c
	ld l, a
	ld a, h
	or b
	ld h, a
	pop bc			; get RHS high word
	exx
	ld a, l
	or c
	ld l, a
	ld a, h
	or b
	ld h, a
	exx
	call lstore32
	pop bc
	ret

;
; land32 - AND HL:HL' into (DE)
;
land32:
	push bc
	ld b, h
	ld c, l
	exx
	push hl			; save high word
	exx
	call lload32		; load (DE) into HL:HL'
	ld a, l
	and c
	ld l, a
	ld a, h
	and b
	ld h, a
	pop bc			; get RHS high word
	exx
	ld a, l
	and c
	ld l, a
	ld a, h
	and b
	ld h, a
	exx
	call lstore32
	pop bc
	ret

;
; lxor32 - XOR HL:HL' into (DE)
;
lxor32:
	push bc
	ld b, h
	ld c, l
	exx
	push hl			; save high word
	exx
	call lload32		; load (DE) into HL:HL'
	ld a, l
	xor c
	ld l, a
	ld a, h
	xor b
	ld h, a
	pop bc			; get RHS high word
	exx
	ld a, l
	xor c
	ld l, a
	ld a, h
	xor b
	ld h, a
	exx
	call lstore32
	pop bc
	ret

;
; ladd32 - add HL:HL' to (DE)
;
ladd32:
	push bc
	ld b, h
	ld c, l
	exx
	push hl			; save high word
	exx
	call lload32		; load (DE) into HL:HL'
	add hl, bc		; add low words
	pop bc			; get RHS high word
	exx
	adc hl, bc		; add high words with carry
	exx
	call lstore32
	pop bc
	ret

;
; lsub32 - subtract HL:HL' from (DE)
;
lsub32:
	push bc
	ld b, h
	ld c, l
	exx
	push hl			; save high word
	exx
	call lload32		; load (DE) into HL:HL'
	or a			; clear carry
	sbc hl, bc		; subtract low words
	pop bc			; get RHS high word
	exx
	sbc hl, bc		; subtract high words with borrow
	exx
	call lstore32
	pop bc
	ret

;
; vim: tabstop=8 shiftwidth=8 noexpandtab:
;
