;
; shift32.s - 32-bit shift operations
;
.globl shl3232, shr3232

.text

;
; 32-bit right shift hl'hl by de -> hl'hl
;
shr3232::
	ld	a,e
	or	a
	ret	z
shr32lp:
	exx
	sra	h
	rr	l
	exx
	rr	h
	rr	l
	dec	a
	jp	nz,shr32lp
	ret

;
; 32-bit left shift hl'hl by de -> hl'hl
;
shl3232::
	ld	a,e
	or	a
	ret	z
shl32lp:
	add	hl,hl
	exx
	adc	hl,hl
	exx
	dec	a
	jp	nz,shl32lp
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
