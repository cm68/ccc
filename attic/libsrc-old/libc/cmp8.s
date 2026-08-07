;
; cmp8.s - 8-bit comparisons
;
.globl lt88, le88, gt88, ge88, ugt88
.globl ret_true, ret_false

.text

;
; a < e (signed)
;
lt88::
	ld	b,a
	xor	e
	jp	m,lt8diff
	ld	a,b
	cp	e
	jp	c,ret_true
	jp	ret_false
lt8diff:
	bit	7,b
	jp	nz,ret_true
	jp	ret_false

;
; a <= e (signed)
;
le88::
	ld	b,a
	xor	e
	jp	m,le8diff
	ld	a,b
	cp	e
	jp	z,ret_true
	jp	c,ret_true
	jp	ret_false
le8diff:
	bit	7,b
	jp	nz,ret_true
	jp	ret_false

;
; a > e (signed)
;
gt88::
	ld	b,a
	xor	e
	jp	m,gt8diff
	ld	a,b
	cp	e
	jp	z,ret_false
	jp	nc,ret_true
	jp	ret_false
gt8diff:
	bit	7,e
	jp	nz,ret_true
	jp	ret_false

;
; a >= e (signed)
;
ge88::
	ld	b,a
	xor	e
	jp	m,ge8diff
	ld	a,b
	cp	e
	jp	nc,ret_true
	jp	ret_false
ge8diff:
	bit	7,e
	jp	nz,ret_true
	jp	ret_false

;
; a > e (unsigned)
;
ugt88::
	cp	e
	jp	z,ret_false
	jp	nc,ret_true
	jp	ret_false
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
