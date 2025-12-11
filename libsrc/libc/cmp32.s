;
; cmp32.s - 32-bit comparisons
;
.globl eq3232, ne3232, lt3232, le3232, gt3232, ge3232
.globl ret_true, ret_false

.text

;
; hl'hl == de'de
;
eq3232::
	exx
	ld	a,h
	cp	d
	jp	nz,eq32fail
	ld	a,l
	cp	e
	jp	nz,eq32fail
	exx
	ld	a,h
	cp	d
	jp	nz,eq32fail
	ld	a,l
	cp	e
	jp	z,ret_true
eq32fail:
	exx
	jp	ret_false

;
; hl'hl != de'de
;
ne3232::
	call	eq3232
	jp	z,ret_true
	jp	ret_false

;
; hl'hl < de'de (signed)
;
lt3232::
	; compare high words first
	exx
	ld	a,h
	xor	d
	jp	m,lt32diffsign
	ld	a,h
	cp	d
	jp	c,lt32true
	jp	nz,lt32false
	ld	a,l
	cp	e
	jp	c,lt32true
	jp	nz,lt32false
	exx
	; high words equal, compare low words
	ld	a,h
	cp	d
	jp	c,lt32true
	jp	nz,lt32false
	ld	a,l
	cp	e
	jp	c,lt32true
	jp	ret_false
lt32diffsign:
	bit	7,h		; hl'hl negative?
	jp	nz,lt32true
lt32false:
	exx
	jp	ret_false
lt32true:
	exx
	jp	ret_true

;
; hl'hl <= de'de (signed)
;
le3232::
	call	eq3232
	jp	z,ret_true
	jp	lt3232

;
; hl'hl > de'de (signed)
;
gt3232::
	call	le3232
	jp	z,ret_true	; was false, so > is true
	jp	ret_false

;
; hl'hl >= de'de (signed)
;
ge3232::
	call	lt3232
	jp	z,ret_true	; was false, so >= is true
	jp	ret_false
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
