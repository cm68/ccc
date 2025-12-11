;
; ldecp.s - 32-bit in-memory post-decrement
;
.globl ldecp

.text

;
; long post-decrement: (*hl)--, return old value
; HL = address of long
; Returns old value in HL'HL
;
ldecp::
	push	de
	; load old value into hl'hl
	ld	e,(hl)
	push	hl		; save address
	inc	hl
	ld	d,(hl)
	inc	hl
	push	de		; save low word
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	exx
	ex	de,hl		; hl' = high word
	exx
	pop	hl		; hl = low word (old value)
	pop	de		; de = address
	push	hl		; save old low for return
	; now decrement at (de)
	ex	de,hl
	ld	a,(hl)
	dec	a
	ld	(hl),a
	inc	a		; test if was 0
	jr	nz,ldecpdone
	inc	hl
	ld	a,(hl)
	dec	a
	ld	(hl),a
	inc	a
	jr	nz,ldecpdone
	inc	hl
	ld	a,(hl)
	dec	a
	ld	(hl),a
	inc	a
	jr	nz,ldecpdone
	inc	hl
	dec	(hl)
ldecpdone:
	pop	hl		; return old low
	pop	de
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
