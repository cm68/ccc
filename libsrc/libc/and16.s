;
; and16.s - 16-bit bitwise AND
;
.globl and1616

.text

;
; 16-bit and: hl & de -> hl
;
and1616::
	ld	a,l
	and	e
	ld	l,a
	ld	a,h
	and	d
	ld	h,a
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
