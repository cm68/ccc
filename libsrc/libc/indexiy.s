;
; indexiy.s - IY indexing helpers
;
.globl indexiy, leaiy, ldixi

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
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
