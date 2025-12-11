;
; mod16.s - 16-bit modulo
;
.globl mod1616, umod1616

.text

;
; modulo hl by de -> hl remainder
; signed modulo
;
mod1616::
	push	bc
	ld	a,h		; sign of dividend = sign of result
	push	af
	; make both positive
	bit	7,h
	jp	z,mod16lpos
	xor	a
	sub	l
	ld	l,a
	sbc	a,a
	sub	h
	ld	h,a
mod16lpos:
	bit	7,d
	jp	z,mod16rpos
	xor	a
	sub	e
	ld	e,a
	sbc	a,a
	sub	d
	ld	d,a
mod16rpos:
	call	umod1616
	pop	af
	bit	7,a
	jp	z,mod16done
	; negate result
	xor	a
	sub	l
	ld	l,a
	sbc	a,a
	sub	h
	ld	h,a
mod16done:
	pop	bc
	ret

;
; unsigned modulo hl by de -> hl remainder
;
umod1616::
	push	bc
	ld	b,h
	ld	c,l		; bc = dividend
	ld	hl,0		; remainder
	ld	a,16		; bit counter
umod16lp:
	sla	c
	rl	b		; shift dividend left
	adc	hl,hl		; shift into remainder
	sbc	hl,de		; try subtract divisor
	jp	nc,umod16ok
	add	hl,de		; restore if negative
umod16ok:
	dec	a
	jp	nz,umod16lp
	; remainder already in hl
	pop	bc
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
