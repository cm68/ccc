;
; cmpmix.s - mixed width comparisons
;
.globl eq816, ne816, ueq816, une816, ugt816, ult168, ugt168
.globl ult1632, gt1632
.globl ushr816
.globl ret_true, ret_false

.text

;
; 8-bit a == 16-bit de (sign extend a)
;
eq816::
	ld	l,a
	or	a
	sbc	a,a		; sign extend
	ld	h,a
	or	a
	sbc	hl,de
	jp	z,ret_true
	jp	ret_false

;
; 8-bit a != 16-bit de
;
ne816::
	ld	l,a
	or	a
	sbc	a,a
	ld	h,a
	or	a
	sbc	hl,de
	jp	nz,ret_true
	jp	ret_false

;
; unsigned 8-bit a == 16-bit de (zero extend a)
;
ueq816::
	ld	l,a
	ld	h,0
	or	a
	sbc	hl,de
	jp	z,ret_true
	jp	ret_false

;
; unsigned 8-bit a != 16-bit de
;
une816::
	ld	l,a
	ld	h,0
	or	a
	sbc	hl,de
	jp	nz,ret_true
	jp	ret_false

;
; unsigned 8-bit a > 16-bit de
;
ugt816::
	ld	l,a
	ld	h,0
	or	a
	sbc	hl,de
	jp	z,ret_false
	jp	nc,ret_true
	jp	ret_false

;
; unsigned 8-bit a < 16-bit de
;
ult168::
	; de < hl (unsigned), operands swapped
	ex	de,hl
	or	a
	sbc	hl,de
	jp	c,ret_true
	jp	ret_false

;
; unsigned 8-bit a > 16-bit de
;
ugt168::
	; de > hl (unsigned), operands swapped
	ex	de,hl
	or	a
	sbc	hl,de
	jp	z,ret_false
	jp	nc,ret_true
	jp	ret_false

;
; unsigned 16-bit hl < 32-bit de'de
;
ult1632::
	; if high word of de'de is nonzero, hl < de'de
	exx
	ld	a,d
	or	e
	exx
	jp	nz,ret_true
	; compare hl with low word
	or	a
	sbc	hl,de
	jp	c,ret_true
	jp	ret_false

;
; 16-bit hl > 32-bit de'de (signed)
;
gt1632::
	; sign extend hl to 32-bit, compare
	ld	a,h
	or	a
	sbc	a,a		; a = 0 or -1
	exx
	cp	d		; compare high bytes
	exx
	jp	nz,gt1632diff
	; high bytes equal, compare low 24 bits
	exx
	ld	a,0
	cp	e
	exx
	jp	nz,gt1632diff
	; high word is 0 or -1, compare low words
	or	a
	sbc	hl,de
	jp	z,ret_false
	jp	nc,ret_true
	jp	ret_false
gt1632diff:
	; different high bytes, check sign of de'de
	exx
	bit	7,d
	exx
	jp	nz,ret_true	; de'de negative, hl > de'de
	jp	ret_false

;
; 8-bit a right shift by e (unsigned)
;
ushr816::
	or	e
	ret	z
	ld	b,e
ushr8lp:
	srl	a
	djnz	ushr8lp
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
