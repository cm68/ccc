;
; or32.s - 32-bit bitwise OR
;
.globl or32

.text

;
; or de'de into hl'hl
;
or32::
	ld	a,l
	or	e
	ld	l,a
	ld	a,h
	or	d
	ld	h,a
	exx
	ld	a,l
	or	e
	ld	l,a
	ld	a,h
	or	d
	ld	h,a
	exx
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
