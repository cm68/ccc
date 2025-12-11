;
; cmp16.s - 16-bit signed comparisons
;
.globl lt1616, le1616, gt1616, ge1616
.globl ret_true, ret_false

.text

;
; hl < de (signed)
;
lt1616::
	ld	a,h
	xor	d		; different signs?
	jp	m,lt16diffsign
	or	a
	sbc	hl,de
	jp	c,ret_true	; same sign, use carry
	jp	ret_false
lt16diffsign:
	bit	7,h		; hl negative?
	jp	nz,ret_true	; negative < positive
	jp	ret_false

;
; hl <= de (signed)
;
le1616::
	ld	a,h
	xor	d
	jp	m,le16diffsign
	or	a
	sbc	hl,de
	jp	z,ret_true
	jp	c,ret_true
	jp	ret_false
le16diffsign:
	bit	7,h
	jp	nz,ret_true
	jp	ret_false

;
; hl > de (signed)
;
gt1616::
	ld	a,h
	xor	d
	jp	m,gt16diffsign
	or	a
	sbc	hl,de
	jp	z,ret_false
	jp	nc,ret_true
	jp	ret_false
gt16diffsign:
	bit	7,d		; de negative?
	jp	nz,ret_true	; positive > negative
	jp	ret_false

;
; hl >= de (signed)
;
ge1616::
	ld	a,h
	xor	d
	jp	m,ge16diffsign
	or	a
	sbc	hl,de
	jp	nc,ret_true
	jp	ret_false
ge16diffsign:
	bit	7,d
	jp	nz,ret_true
	jp	ret_false
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
