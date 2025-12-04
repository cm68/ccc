;
; c compiler helper functions
;
; Naming convention: opXXYY where XX=left operand bits, YY=right operand bits
; For comparison ops: return NZ if true, Z if false
; 16-bit values in HL (left) and DE (right)
; 32-bit values in HL'HL (left) and DE'DE (right)
; 8-bit values in A or L
;

;
; allocate stack frame
; input: A = frame size in bytes (for locals)
;
; on entry (after call framealloc):
;   SP -> [ret to func body]
;         [ret to caller]
;         [args...]
;
; on exit:
;   SP -> [locals...]
;         [saved IY]         <- IY points here
;         [ret to caller]    <- IY + 2
;         [args...]          <- IY + 4
;
framealloc:
	ex	(sp),iy		; save iy and get ret addr
	push	iy
	pop	de
	ld	iy,0
	add	iy,sp		; IY = SP (points at saved IY)
	; allocate A bytes for locals: SP -= A
	ld	h,-1
	neg
	ld	l,a
	add	hl,sp		; HL = SP
	ld	sp,hl
	ex	de,hl
	jp	(hl)		; return to func body

;
; deallocate stack frame and return to caller
; called via: jp framefree
;
; on entry:
;   SP -> [locals...]
;         [saved IY]         <- IY points here
;         [ret to caller]    <- IY + 2
;         [args...]
;
framefree:
	ld	sp,iy		; SP = IY, free locals, point at saved IY
	pop	iy		; restore caller's frame pointer
	ret			; return to caller (addr is now on top)

;
; index iy by signed value in a
;
indexiy:
	push	de		; save register pair
	ld	e,a		; low = offset
	or	a,a
	sbc	a,a		; sign extend
	ld	d,a		; into high
	add	iy,de		; point at destination
	pop	de		; restore register
	ret

;
; take address of stack frame variable at offset a
; leave the result in hl.
;
leaiy:
	push	iy		; save frame pointer
	call	indexiy		; add a to iy
	push	iy		; transfer it to hl
	pop	hl
	pop	iy		; restore frame pointer
	ret

;
; ld ix,(iy+a)
;
ldixi:
	call	leaiy
	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
	push	hl
	pop	ix
	ret

;
; write a long word in hl'hl onto the stack frame at offset a
;
putlong:
	push	iy		; get frame pointer
	call	indexiy		; calculate frame offset
	ld	(iy+0),l	; write low low
	ld	(iy+1),h	; write high low
	exx
	ld	(iy+2),l	; write low high
	ld	(iy+3),h	; write high high
	exx
	pop	iy		; restore fp
	ret

;
; read a long word on the stack frame at offset a into hl'hl
;
getlong:
	push	iy		; get frame pointer
	call	indexiy		; calculate frame offset
	ld	l,(iy+0)	; read low low
	ld	h,(iy+1)	; read high low
	exx
	ld	l,(iy+2)	; read low high
	ld	h,(iy+3)	; read high high
	exx
	pop	iy		; restore fp
	ret

;
; load 32-bit immediate into hl'hl
; value follows call instruction
;
load32i:
	ex	(sp),hl		; get return address
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	inc	hl
	exx
	ex	(sp),hl		; restore hl', get return address in alt
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	inc	hl
	ex	(sp),hl		; put updated return address back
	exx
	ex	de,hl		; low word in hl
	exx
	ex	de,hl		; high word in hl'
	exx
	ret

; ============================================================================
; 32-bit arithmetic (hl'hl op de'de -> hl'hl)
; ============================================================================

;
; add de'de to hl'hl
;
add32:
	add	hl,de
	exx
	adc	hl,de
	exx
	ret

;
; subtract de'de from hl'hl
;
sub32:
	or	a		; clear carry
	sbc	hl,de
	exx
	sbc	hl,de
	exx
	ret

;
; and de'de into hl'hl
;
and32:
	ld	a,l
	and	e
	ld	l,a
	ld	a,h
	and	d
	ld	h,a
	exx
	ld	a,l
	and	e
	ld	l,a
	ld	a,h
	and	d
	ld	h,a
	exx
	ret

;
; or de'de into hl'hl
;
or32:
	ld	a,l
	or	e
	ld	l,a
	ld	a,h
	or	d
	ld	h,a
	exx
	ld	a,l
	or	e
	ld	l,a
	ld	a,h
	or	d
	ld	h,a
	exx
	ret

;
; xor de'de into hl'hl
;
xor32:
	ld	a,l
	xor	e
	ld	l,a
	ld	a,h
	xor	d
	ld	h,a
	exx
	ld	a,l
	xor	e
	ld	l,a
	ld	a,h
	xor	d
	ld	h,a
	exx
	ret

; ============================================================================
; 8-bit arithmetic (a op e -> a)
; ============================================================================

;
; 8-bit add: a + e -> a
;
add88:
	add	a,e
	ret

;
; 8-bit and: a & e -> a
;
and88:
	and	e
	ret

; ============================================================================
; Mixed width arithmetic
; ============================================================================

;
; add 16-bit hl to 8-bit a (sign extended) -> hl
;
add168:
	ld	e,a
	or	a		; check sign
	sbc	a,a		; a = 0 or -1 for sign extension
	ld	d,a
	add	hl,de
	ret

;
; multiply 16-bit hl by 8-bit a -> hl
;
mul168:
	ld	e,a
	ld	d,0
	jp	mul1616e	; fall into mul1616 with de set

; ============================================================================
; 16-bit arithmetic (hl op de -> hl)
; ============================================================================

;
; multiply hl by de -> hl (unsigned)
; uses shift-and-add algorithm
;
mul1616:
	ex	de,hl		; de = multiplicand, hl = multiplier
mul1616e:
	push	bc
	ld	b,h
	ld	c,l		; bc = multiplier
	ld	hl,0		; result
mul16lp:
	ld	a,b
	or	c
	jr	z,mul16done
	srl	b
	rr	c		; shift multiplier right
	jr	nc,mul16skip
	add	hl,de		; add multiplicand if bit was set
mul16skip:
	sla	e
	rl	d		; shift multiplicand left
	jr	mul16lp
mul16done:
	pop	bc
	ret

;
; divide hl by de -> hl quotient, remainder discarded
; signed division
;
div1616:
	push	bc
	ld	a,h
	xor	d		; sign of result
	push	af
	; make both positive
	bit	7,h
	jr	z,div16lpos
	xor	a
	sub	l
	ld	l,a
	sbc	a,a
	sub	h
	ld	h,a
div16lpos:
	bit	7,d
	jr	z,div16rpos
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
	jr	z,div16done
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
udiv1616:
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
	jr	nc,udiv16ok
	add	hl,de		; restore if negative
	jr	udiv16next
udiv16ok:
	inc	c		; set quotient bit
udiv16next:
	dec	a
	jr	nz,udiv16lp
	ld	h,b
	ld	l,c		; quotient in hl
	pop	bc
	ret

;
; modulo hl by de -> hl remainder
; signed modulo
;
mod1616:
	push	bc
	ld	a,h		; sign of dividend = sign of result
	push	af
	; make both positive
	bit	7,h
	jr	z,mod16lpos
	xor	a
	sub	l
	ld	l,a
	sbc	a,a
	sub	h
	ld	h,a
mod16lpos:
	bit	7,d
	jr	z,mod16rpos
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
	jr	z,mod16done
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
umod1616:
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
	jr	nc,umod16ok
	add	hl,de		; restore if negative
umod16ok:
	dec	a
	jr	nz,umod16lp
	; remainder already in hl
	pop	bc
	ret

;
; 16-bit and: hl & de -> hl
;
and1616:
	ld	a,l
	and	e
	ld	l,a
	ld	a,h
	and	d
	ld	h,a
	ret

;
; 16-bit or: hl | de -> hl
;
or1616:
	ld	a,l
	or	e
	ld	l,a
	ld	a,h
	or	d
	ld	h,a
	ret

;
; 16-bit right shift: hl >> de -> hl (signed)
;
shr1616:
	ld	a,e
	or	a
	ret	z
shr16lp:
	sra	h
	rr	l
	dec	a
	jr	nz,shr16lp
	ret

;
; 16-bit right shift: hl >> de -> hl (unsigned)
;
ushr1616:
	ld	a,e
	or	a
	ret	z
ushr16lp:
	srl	h
	rr	l
	dec	a
	jr	nz,ushr16lp
	ret

;
; 16-bit left shift: hl << de -> hl
;
shl1616:
	ld	a,e
	or	a
	ret	z
shl16lp:
	add	hl,hl
	dec	a
	jr	nz,shl16lp
	ret

; ============================================================================
; 32-bit multiply and divide
; ============================================================================

;
; 32-bit multiply hl'hl by de'de -> hl'hl
;
mul3232:
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
	jr	z,mul32done
	; check low bit of multiplier
	ld	a,e
	and	1
	jr	z,mul32skip
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
	jr	mul32lp
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
; 32-bit divide hl'hl by de'de -> hl'hl quotient
;
div3232:
	; TODO: implement 32-bit division
	ret

;
; 32-bit modulo hl'hl by de'de -> hl'hl remainder
;
mod3232:
	; TODO: implement 32-bit modulo
	ret

;
; 32-bit right shift hl'hl by de -> hl'hl
;
shr3232:
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
	jr	nz,shr32lp
	ret

; ============================================================================
; 16-bit comparisons (signed): hl op de
; return NZ (A=1) if true, Z (A=0) if false
; ============================================================================

;
; hl == de
;
eq1616:
	or	a
	sbc	hl,de
	jr	z,ret_true
	jr	ret_false

;
; hl != de
;
ne1616:
	or	a
	sbc	hl,de
	jr	nz,ret_true
	jr	ret_false

;
; hl < de (signed)
;
lt1616:
	ld	a,h
	xor	d		; different signs?
	jp	m,lt16diffsign
	or	a
	sbc	hl,de
	jr	c,ret_true	; same sign, use carry
	jr	ret_false
lt16diffsign:
	bit	7,h		; hl negative?
	jr	nz,ret_true	; negative < positive
	jr	ret_false

;
; hl <= de (signed)
;
le1616:
	ld	a,h
	xor	d
	jp	m,le16diffsign
	or	a
	sbc	hl,de
	jr	z,ret_true
	jr	c,ret_true
	jr	ret_false
le16diffsign:
	bit	7,h
	jr	nz,ret_true
	jr	ret_false

;
; hl > de (signed)
;
gt1616:
	ld	a,h
	xor	d
	jp	m,gt16diffsign
	or	a
	sbc	hl,de
	jr	z,ret_false
	jr	nc,ret_true
	jr	ret_false
gt16diffsign:
	bit	7,d		; de negative?
	jr	nz,ret_true	; positive > negative
	jr	ret_false

;
; hl >= de (signed)
;
ge1616:
	ld	a,h
	xor	d
	jp	m,ge16diffsign
	or	a
	sbc	hl,de
	jr	nc,ret_true
	jr	ret_false
ge16diffsign:
	bit	7,d
	jr	nz,ret_true
	jr	ret_false

; common return points
ret_true:
	ld	a,1
	or	a		; set NZ
	ret
ret_false:
	xor	a		; set Z
	ret

; ============================================================================
; 16-bit comparisons (unsigned): hl op de
; ============================================================================

;
; hl < de (unsigned)
;
ult1616:
	or	a
	sbc	hl,de
	jr	c,ret_true
	jr	ret_false

;
; hl <= de (unsigned)
;
ule1616:
	or	a
	sbc	hl,de
	jr	c,ret_true
	jr	z,ret_true
	jr	ret_false

;
; hl > de (unsigned)
;
ugt1616:
	or	a
	sbc	hl,de
	jr	z,ret_false
	jr	nc,ret_true
	jr	ret_false

;
; hl >= de (unsigned)
;
uge1616:
	or	a
	sbc	hl,de
	jr	nc,ret_true
	jr	ret_false

; ============================================================================
; 8-bit comparisons: a op e
; ============================================================================

;
; a == e
;
eq88:
	cp	e
	jr	z,ret_true
	jr	ret_false

;
; a != e
;
ne88:
	cp	e
	jr	nz,ret_true
	jr	ret_false

;
; a < e (signed)
;
lt88:
	ld	b,a
	xor	e
	jp	m,lt8diff
	ld	a,b
	cp	e
	jr	c,ret_true
	jr	ret_false
lt8diff:
	bit	7,b
	jr	nz,ret_true
	jr	ret_false

;
; a <= e (signed)
;
le88:
	ld	b,a
	xor	e
	jp	m,le8diff
	ld	a,b
	cp	e
	jr	z,ret_true
	jr	c,ret_true
	jr	ret_false
le8diff:
	bit	7,b
	jr	nz,ret_true
	jr	ret_false

;
; a > e (signed)
;
gt88:
	ld	b,a
	xor	e
	jp	m,gt8diff
	ld	a,b
	cp	e
	jr	z,ret_false
	jr	nc,ret_true
	jr	ret_false
gt8diff:
	bit	7,e
	jr	nz,ret_true
	jr	ret_false

;
; a >= e (signed)
;
ge88:
	ld	b,a
	xor	e
	jp	m,ge8diff
	ld	a,b
	cp	e
	jr	nc,ret_true
	jr	ret_false
ge8diff:
	bit	7,e
	jr	nz,ret_true
	jr	ret_false

;
; a < e (unsigned)
;
ult88:
	cp	e
	jr	c,ret_true
	jr	ret_false

;
; a > e (unsigned)
;
ugt88:
	cp	e
	jr	z,ret_false
	jr	nc,ret_true
	jr	ret_false

; ============================================================================
; Mixed width comparisons
; ============================================================================

;
; 8-bit a == 16-bit de (sign extend a)
;
eq816:
	ld	l,a
	or	a
	sbc	a,a		; sign extend
	ld	h,a
	or	a
	sbc	hl,de
	jr	z,ret_true
	jr	ret_false

;
; 8-bit a != 16-bit de
;
ne816:
	ld	l,a
	or	a
	sbc	a,a
	ld	h,a
	or	a
	sbc	hl,de
	jr	nz,ret_true
	jr	ret_false

;
; unsigned 8-bit a == 16-bit de (zero extend a)
;
ueq816:
	ld	l,a
	ld	h,0
	or	a
	sbc	hl,de
	jr	z,ret_true
	jr	ret_false

;
; unsigned 8-bit a != 16-bit de
;
une816:
	ld	l,a
	ld	h,0
	or	a
	sbc	hl,de
	jr	nz,ret_true
	jr	ret_false

;
; unsigned 8-bit a > 16-bit de
;
ugt816:
	ld	l,a
	ld	h,0
	or	a
	sbc	hl,de
	jr	z,ret_false
	jr	nc,ret_true
	jr	ret_false

;
; unsigned 8-bit a < 16-bit de
;
ult168:
	; de < hl (unsigned), operands swapped
	ex	de,hl
	or	a
	sbc	hl,de
	jr	c,ret_true
	jr	ret_false

;
; unsigned 8-bit a > 16-bit de
;
ugt168:
	; de > hl (unsigned), operands swapped
	ex	de,hl
	or	a
	sbc	hl,de
	jr	z,ret_false
	jr	nc,ret_true
	jr	ret_false

;
; unsigned 16-bit hl < 32-bit de'de
;
ult1632:
	; if high word of de'de is nonzero, hl < de'de
	exx
	ld	a,d
	or	e
	exx
	jr	nz,ret_true
	; compare hl with low word
	or	a
	sbc	hl,de
	jr	c,ret_true
	jr	ret_false

;
; 16-bit hl > 32-bit de'de (signed)
;
gt1632:
	; sign extend hl to 32-bit, compare
	ld	a,h
	or	a
	sbc	a,a		; a = 0 or -1
	exx
	cp	d		; compare high bytes
	exx
	jr	nz,gt1632diff
	; high bytes equal, compare low 24 bits
	exx
	ld	a,0
	cp	e
	exx
	jr	nz,gt1632diff
	; high word is 0 or -1, compare low words
	or	a
	sbc	hl,de
	jr	z,ret_false
	jr	nc,ret_true
	jr	ret_false
gt1632diff:
	; different high bytes, check sign of de'de
	exx
	bit	7,d
	exx
	jr	nz,ret_true	; de'de negative, hl > de'de
	jr	ret_false

;
; 8-bit a right shift by e (unsigned)
;
ushr816:
	or	e
	ret	z
	ld	b,e
ushr8lp:
	srl	a
	djnz	ushr8lp
	ret

; ============================================================================
; 32-bit comparisons: hl'hl op de'de
; ============================================================================

;
; hl'hl == de'de
;
eq3232:
	exx
	ld	a,h
	cp	d
	jr	nz,eq32fail
	ld	a,l
	cp	e
	jr	nz,eq32fail
	exx
	ld	a,h
	cp	d
	jr	nz,eq32fail
	ld	a,l
	cp	e
	jr	z,ret_true
eq32fail:
	exx
	jp	ret_false

;
; hl'hl != de'de
;
ne3232:
	call	eq3232
	jr	z,ret_true
	jr	ret_false

;
; hl'hl < de'de (signed)
;
lt3232:
	; compare high words first
	exx
	ld	a,h
	xor	d
	jp	m,lt32diffsign
	ld	a,h
	cp	d
	jr	c,lt32true
	jr	nz,lt32false
	ld	a,l
	cp	e
	jr	c,lt32true
	jr	nz,lt32false
	exx
	; high words equal, compare low words
	ld	a,h
	cp	d
	jr	c,lt32true
	jr	nz,lt32false
	ld	a,l
	cp	e
	jr	c,lt32true
	jr	ret_false
lt32diffsign:
	bit	7,h		; hl'hl negative?
	jr	nz,lt32true
lt32false:
	exx
	jp	ret_false
lt32true:
	exx
	jp	ret_true

;
; hl'hl <= de'de (signed)
;
le3232:
	call	eq3232
	jr	z,ret_true
	jp	lt3232

;
; hl'hl > de'de (signed)
;
gt3232:
	call	le3232
	jr	z,ret_true	; was false, so > is true
	jr	ret_false

;
; hl'hl >= de'de (signed)
;
ge3232:
	call	lt3232
	jr	z,ret_true	; was false, so >= is true
	jr	ret_false

; ============================================================================
; Mixed 32/16 comparisons
; ============================================================================

;
; 32-bit hl'hl == 16-bit de (sign extend de)
;
eq3216:
	; sign extend de to de'de
	ld	a,d
	or	a
	sbc	a,a
	exx
	ld	d,a
	ld	e,a
	exx
	jp	eq3232

;
; 32-bit hl'hl != 16-bit de
;
ne3216:
	call	eq3216
	jr	z,ret_true
	jr	ret_false

;
; 32-bit hl'hl < 16-bit de (sign extend de)
;
lt3216:
	ld	a,d
	or	a
	sbc	a,a
	exx
	ld	d,a
	ld	e,a
	exx
	jp	lt3232

;
; 32-bit hl'hl <= 16-bit de
;
le3216:
	ld	a,d
	or	a
	sbc	a,a
	exx
	ld	d,a
	ld	e,a
	exx
	jp	le3232

;
; 32-bit hl'hl > 16-bit de
;
gt3216:
	ld	a,d
	or	a
	sbc	a,a
	exx
	ld	d,a
	ld	e,a
	exx
	jp	gt3232

;
; 32-bit hl'hl >= 16-bit de
;
ge3216:
	ld	a,d
	or	a
	sbc	a,a
	exx
	ld	d,a
	ld	e,a
	exx
	jp	ge3232

;
; 32-bit hl'hl & 16-bit de (sign extend de)
;
and3216:
	ld	a,d
	or	a
	sbc	a,a
	exx
	ld	d,a
	ld	e,a
	exx
	jp	and32

;
; 32-bit hl'hl | 16-bit de (sign extend de)
;
or3216:
	ld	a,d
	or	a
	sbc	a,a
	exx
	ld	d,a
	ld	e,a
	exx
	jp	or32

;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
