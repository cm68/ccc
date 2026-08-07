;
; shift16.s - 16-bit shift operations
;
.globl shl1616, shr1616, ushr1616

.text

;
; 16-bit right shift: hl >> de -> hl (signed)
;
shr1616::
	ld	a,e
	or	a
	ret	z
shr16lp:
	sra	h
	rr	l
	dec	a
	jp	nz,shr16lp
	ret

;
; 16-bit right shift: hl >> de -> hl (unsigned)
;
ushr1616::
	ld	a,e
	or	a
	ret	z
ushr16lp:
	srl	h
	rr	l
	dec	a
	jp	nz,ushr16lp
	ret

;
; 16-bit left shift: hl << de -> hl
;
shl1616::
	ld	a,e
	or	a
	ret	z
shl16lp:
	add	hl,hl
	dec	a
	jp	nz,shl16lp
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
