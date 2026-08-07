;
; sub32.s - 32-bit subtraction
;
.globl sub32

.text

;
; subtract de'de from hl'hl
;
sub32::
	or	a		; clear carry
	sbc	hl,de
	exx
	sbc	hl,de
	exx
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
