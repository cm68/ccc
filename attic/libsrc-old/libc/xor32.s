;
; xor32.s - 32-bit bitwise XOR
;
.globl xor32

.text

;
; xor de'de into hl'hl
;
xor32::
	ld	a,l
	xor	e
	ld	l,a
	ld	a,h
	xor	d
	ld	h,a
	exx
	ld	a,l
	xor	e
	ld	l,a
	ld	a,h
	xor	d
	ld	h,a
	exx
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
