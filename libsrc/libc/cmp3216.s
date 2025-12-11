;
; cmp3216.s - mixed 32/16-bit comparisons
;
.globl eq3216, ne3216, lt3216, le3216, gt3216, ge3216
.globl and3216, or3216
.globl eq3232, lt3232, le3232, gt3232, ge3232
.globl and32, or32
.globl ret_true, ret_false

.text

;
; 32-bit hl'hl == 16-bit de (sign extend de)
;
eq3216::
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
ne3216::
	call	eq3216
	jp	z,ret_true
	jp	ret_false

;
; 32-bit hl'hl < 16-bit de (sign extend de)
;
lt3216::
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
le3216::
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
gt3216::
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
ge3216::
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
and3216::
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
or3216::
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
