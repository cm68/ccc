;
; mul32.s - 32-bit multiplication
;
.globl mul3232

.text

;
; 32-bit multiply hl'hl by de'de -> hl'hl
;
mul3232::
	push	bc
	push	ix
	; save multiplier
	push	de
	exx
	push	de
	exx
	; result in bc'bc
	ld	b,0
	ld	c,0
	exx
	ld	b,0
	ld	c,0
	exx
mul32lp:
	; check if multiplier is zero
	exx
	pop	de		; high word of multiplier
	push	de
	exx
	pop	de		; low word of multiplier
	push	de
	ld	a,d
	or	e
	exx
	or	d
	or	e
	exx
	jp	z,mul32done
	; check low bit of multiplier
	ld	a,e
	and	1
	jp	z,mul32skip
	; add multiplicand to result
	push	hl
	exx
	push	hl
	exx
	ld	a,c
	add	a,l
	ld	c,a
	ld	a,b
	adc	a,h
	ld	b,a
	exx
	pop	hl
	ld	a,c
	adc	a,l
	ld	c,a
	ld	a,b
	adc	a,h
	ld	b,a
	exx
	pop	hl
mul32skip:
	; shift multiplier right
	exx
	pop	de
	srl	d
	rr	e
	push	de
	exx
	pop	de
	rr	d
	rr	e
	push	de
	; shift multiplicand left
	add	hl,hl
	exx
	adc	hl,hl
	exx
	jp	mul32lp
mul32done:
	pop	de
	exx
	pop	de
	exx
	; result in bc'bc, move to hl'hl
	ld	h,b
	ld	l,c
	exx
	ld	h,b
	ld	l,c
	exx
	pop	ix
	pop	bc
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
