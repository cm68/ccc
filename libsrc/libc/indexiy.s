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
	or	a,a
	sbc	a,a		; sign extend
	ld	d,a		; into high
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
