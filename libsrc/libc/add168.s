;
; add168.s - add 8-bit to 16-bit
;
.globl add168

.text

;
; add 16-bit hl to 8-bit a (sign extended) -> hl
;
add168::
	ld	e,a
	or	a		; check sign
	sbc	a,a		; a = 0 or -1 for sign extension
	ld	d,a
	add	hl,de
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
