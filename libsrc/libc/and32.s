;
; and32.s - 32-bit bitwise AND
;
.globl and32

.text

;
; and de'de into hl'hl
;
and32::
	ld	a,l
	and	e
	ld	l,a
	ld	a,h
	and	d
	ld	h,a
	exx
	ld	a,l
	and	e
	ld	l,a
	ld	a,h
	and	d
	ld	h,a
	exx
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
