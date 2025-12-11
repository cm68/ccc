;
; div16.s - 16-bit division
;
.globl div1616, udiv1616

.text

;
; divide hl by de -> hl quotient, remainder discarded
; signed division
;
div1616::
	push	bc
	ld	a,h
	xor	d		; sign of result
	push	af
	; make both positive
	bit	7,h
	jp	z,div16lpos
	xor	a
	sub	l
	ld	l,a
	sbc	a,a
	sub	h
	ld	h,a
div16lpos:
	bit	7,d
	jp	z,div16rpos
	xor	a
	sub	e
	ld	e,a
	sbc	a,a
	sub	d
	ld	d,a
div16rpos:
	call	udiv1616
	pop	af
	bit	7,a
	jp	z,div16done
	; negate result
	xor	a
	sub	l
	ld	l,a
	sbc	a,a
	sub	h
	ld	h,a
div16done:
	pop	bc
	ret

;
; unsigned divide hl by de -> hl quotient
;
udiv1616::
	push	bc
	ld	b,h
	ld	c,l		; bc = dividend
	ld	hl,0		; remainder
	ld	a,16		; bit counter
udiv16lp:
	sla	c
	rl	b		; shift dividend left, msb into carry
	adc	hl,hl		; shift into remainder
	sbc	hl,de		; try subtract divisor
	jp	nc,udiv16ok
	add	hl,de		; restore if negative
	jp	udiv16next
udiv16ok:
	inc	c		; set quotient bit
udiv16next:
	dec	a
	jp	nz,udiv16lp
	ld	h,b
	ld	l,c		; quotient in hl
	pop	bc
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
