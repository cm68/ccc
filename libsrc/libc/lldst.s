;	Long (32-bit) load/store helpers for code generator
;
;	A long lives in HLDE with the HIGH word in HL, which is what
;	ladd, lsub, lrelop and the rest of the runtime use and what the
;	compiler emits.  This file used to claim the opposite in its
;	header and lld used to end with an ex de,hl that made good on the
;	claim, so a value loaded through it came back with its halves
;	swapped.  Nothing called it, which is the only reason it never
;	showed.  lstde was right all along.

	psect	text
	global	lld, lldde, lstde, ldi, stide

; Load 32-bit from (HL) into HLDE
; Entry: HL = pointer to long
; Exit: HLDE = 32-bit value, HL = high word, DE = low word
lld:
	ld	e,(hl)		; low word from the lower address
	inc	hl
	ld	d,(hl)
	inc	hl
	ld	a,(hl)		; high word from the higher one
	inc	hl
	ld	h,(hl)
	ld	l,a
	ret

; Load 32-bit from (DE) into HLDE
; Entry: DE = pointer to long
; Exit: HLDE = 32-bit value
lldde:
	ex	de,hl
	call	lld
	ret

; Store HLDE to (DE)
; Entry: HLDE = 32-bit value, stack has dest pointer
; Exit: value stored, pointer popped
lstde:
	pop	bc		; return address
	ex	(sp),hl		; get dest ptr, save low word
	ld	(hl),e
	inc	hl
	ld	(hl),d
	inc	hl
	pop	de		; get low word back
	ld	(hl),e
	inc	hl
	ld	(hl),d
	push	bc		; return address
	ret

; Load 16-bit from (HL) into HL
; Entry: HL = pointer
; Exit: HL = 16-bit value
ldi:
	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
	ret

; Store DE to (HL) and advance
; Entry: HL = pointer, DE = value
; Exit: value stored, HL advanced by 2
stide:
	ld	(hl),e
	inc	hl
	ld	(hl),d
	inc	hl
	ret

; Load 16-bit from (HL) into DE
; Entry: HL = pointer
; Exit: DE = 16-bit value, HL advanced by 2
ldide:
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	inc	hl
	ret

; vim: tabstop=4 shiftwidth=4 noexpandtab:
