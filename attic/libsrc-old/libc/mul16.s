;
; mul16.s - 16-bit multiplication
;
.globl mul168, mul1616, mul1616e

.text

;
; multiply 16-bit hl by 8-bit a -> hl
;
mul168::
	ld	e,a
	ld	d,0
	jp	mul1616e	; fall into mul1616 with de set

;
; multiply hl by de -> hl (unsigned)
; uses shift-and-add algorithm
;
mul1616::
	ex	de,hl		; de = multiplicand, hl = multiplier
mul1616e::
	push	bc
	ld	b,h
	ld	c,l		; bc = multiplier
	ld	hl,0		; result
mul16lp:
	ld	a,b
	or	c
	jp	z,mul16done
	srl	b
	rr	c		; shift multiplier right
	jp	nc,mul16skip
	add	hl,de		; add multiplicand if bit was set
mul16skip:
	sla	e
	rl	d		; shift multiplicand left
	jp	mul16lp
mul16done:
	pop	bc
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
