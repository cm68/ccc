;
; indexiy.s - IY indexing helpers
;
.globl indexiy, leaiy, ldixi
.globl lLyindex, sLyindex, lLxindex, sLxindex

.text

;
; index iy by signed value in a
;
indexiy::
	push	de		; save register pair
	ld	e,a		; low = offset
	rla			; rotate sign bit into carry
	sbc	a,a		; A = -1 if carry (negative), else 0
	ld	d,a		; sign extend into D
	add	iy,de		; point at destination
	pop	de		; restore register
	ret

;
; take address of stack frame variable at offset a
; leave the result in hl.
;
leaiy::
	push	iy		; save frame pointer
	call	indexiy		; add a to iy
	push	iy		; transfer it to hl
	pop	hl
	pop	iy		; restore frame pointer
	ret

;
; ld ix,(iy+a)
;
ldixi::
	call	leaiy
	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
	push	hl
	pop	ix
	ret

;
; load long from (ix+a) into HLHL'
;
lLxindex::
	push	ix
	jr	lLload
;
; load long from (iy+a) into HLHL'
;
lLyindex::
	push	iy
lLload:
	pop	hl
	ld	e,a
	rla
	sbc	a,a
	ld	d,a
	add	hl,de		; HL = base + offset
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	inc	hl
	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a		; DE = low, HL = high
	push	hl
	ex	de,hl		; HL = low word
	exx
	pop	hl		; HL' = high word
	exx
	ret

;
; store HLHL' to (ix+a)
;
sLxindex::
	exx
	push	hl		; save high word
	exx
	push	hl		; save low word
	push	ix
	jr	sLstore
;
; store HLHL' to (iy+a)
;
sLyindex::
	exx
	push	hl		; save high word
	exx
	push	hl		; save low word
	push	iy
sLstore:
	pop	hl
	ld	e,a
	rla
	sbc	a,a
	ld	d,a
	add	hl,de		; HL = base + offset
	pop	de		; DE = low word
	ld	(hl),e
	inc	hl
	ld	(hl),d
	inc	hl
	pop	de		; DE = high word
	ld	(hl),e
	inc	hl
	ld	(hl),d
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
