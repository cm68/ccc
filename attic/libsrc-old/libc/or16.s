;
; or16.s - 16-bit bitwise OR
;
.globl or1616

.text

;
; 16-bit or: hl | de -> hl
;
or1616::
	ld	a,l
	or	e
	ld	l,a
	ld	a,h
	or	d
	ld	h,a
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
