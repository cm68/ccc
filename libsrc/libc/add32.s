;
; add32.s - 32-bit addition
;
.globl add32

.text

;
; add de'de to hl'hl
;
add32::
	add	hl,de
	exx
	adc	hl,de
	exx
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
