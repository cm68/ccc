; $ is the address of the current instruction, not how far the
; emitter has got through it.  So jr $+2 targets the next
; instruction and assembles to a zero displacement, and jr $ is a
; two-byte infinite loop.
	.text
start:
	nop			; 00
	jr	$+2		; 18 00   - falls through
	jr	$		; 18 fe   - branches to itself
	jr	$-2		; 18 fc   - back to the previous jr
	nop			; 00
	ld	hl,$		; 21 xx xx - address of this ld
	nop			; 00
	jp	$+3		; c3 .. .. - past this 3-byte jp
	nop			; 00
