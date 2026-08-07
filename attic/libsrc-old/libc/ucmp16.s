;
; ucmp16.s - 16-bit unsigned comparisons
;
.globl ule1616, ugt1616, uge1616
.globl ret_true, ret_false

.text

;
; hl <= de (unsigned)
;
ule1616::
	or	a
	sbc	hl,de
	jp	c,ret_true
	jp	z,ret_true
	jp	ret_false

;
; hl > de (unsigned)
;
ugt1616::
	or	a
	sbc	hl,de
	jp	z,ret_false
	jp	nc,ret_true
	jp	ret_false

;
; hl >= de (unsigned)
;
uge1616::
	or	a
	sbc	hl,de
	jp	nc,ret_true
	jp	ret_false
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
