;
; ltemp.s - Long (32-bit) temporaries and helpers
;
; Memory-based temps to avoid exx register juggling.
; _lL = left operand, _lR = right operand
;
.globl _lL
.globl _lR
.globl _lldHL
.globl _lldHLR
.globl _lstHL
.globl _lstHLR
.globl _ladd
.globl _lsub
.globl _land
.globl _lor
.globl _lxor
.globl _lneg
.globl _lcom
.globl _lcmp
.globl _lshl
.globl _lshr
.globl _lashr

.data
_lL:	.ds 4		; left long temp
_lR:	.ds 4		; right long temp

.text

;
; _lldHL - load 4 bytes from (HL) into _lL
;
_lldHL::
	ld	de,_lL
	ldi
	ldi
	ldi
	ldi
	ret

;
; _lldHLR - load 4 bytes from (HL) into _lR
;
_lldHLR::
	ld	de,_lR
	ldi
	ldi
	ldi
	ldi
	ret

;
; _lstHL - store 4 bytes from _lL to (HL)
;
_lstHL::
	ld	de,_lL
	ex	de,hl
	ldi
	ldi
	ldi
	ldi
	ret

;
; _lstHLR - store 4 bytes from _lR to (HL)
;
_lstHLR::
	ld	de,_lR
	ex	de,hl
	ldi
	ldi
	ldi
	ldi
	ret

;
; _ladd - _lR = _lL + _lR
;
_ladd::
	ld	hl,(_lL)
	ld	de,(_lR)
	add	hl,de
	ld	(_lR),hl
	ld	hl,(_lL+2)
	ld	de,(_lR+2)
	adc	hl,de
	ld	(_lR+2),hl
	ret

;
; _lsub - _lR = _lL - _lR
;
_lsub::
	ld	hl,(_lL)
	ld	de,(_lR)
	or	a
	sbc	hl,de
	ld	(_lR),hl
	ld	hl,(_lL+2)
	ld	de,(_lR+2)
	sbc	hl,de
	ld	(_lR+2),hl
	ret

;
; _land - _lR = _lL & _lR
;
_land::
	ld	hl,_lL
	ld	de,_lR
	ld	a,(de)
	and	(hl)
	ld	(de),a
	inc	hl
	inc	de
	ld	a,(de)
	and	(hl)
	ld	(de),a
	inc	hl
	inc	de
	ld	a,(de)
	and	(hl)
	ld	(de),a
	inc	hl
	inc	de
	ld	a,(de)
	and	(hl)
	ld	(de),a
	ret

;
; _lor - _lR = _lL | _lR
;
_lor::
	ld	hl,_lL
	ld	de,_lR
	ld	a,(de)
	or	(hl)
	ld	(de),a
	inc	hl
	inc	de
	ld	a,(de)
	or	(hl)
	ld	(de),a
	inc	hl
	inc	de
	ld	a,(de)
	or	(hl)
	ld	(de),a
	inc	hl
	inc	de
	ld	a,(de)
	or	(hl)
	ld	(de),a
	ret

;
; _lxor - _lR = _lL ^ _lR
;
_lxor::
	ld	hl,_lL
	ld	de,_lR
	ld	a,(de)
	xor	(hl)
	ld	(de),a
	inc	hl
	inc	de
	ld	a,(de)
	xor	(hl)
	ld	(de),a
	inc	hl
	inc	de
	ld	a,(de)
	xor	(hl)
	ld	(de),a
	inc	hl
	inc	de
	ld	a,(de)
	xor	(hl)
	ld	(de),a
	ret

;
; _lneg - _lR = -_lR
;
_lneg::
	ld	hl,0
	ld	de,(_lR)
	or	a
	sbc	hl,de
	ld	(_lR),hl
	ld	hl,0
	ld	de,(_lR+2)
	sbc	hl,de
	ld	(_lR+2),hl
	ret

;
; _lcom - _lR = ~_lR
;
_lcom::
	ld	hl,_lR
	ld	a,(hl)
	cpl
	ld	(hl),a
	inc	hl
	ld	a,(hl)
	cpl
	ld	(hl),a
	inc	hl
	ld	a,(hl)
	cpl
	ld	(hl),a
	inc	hl
	ld	a,(hl)
	cpl
	ld	(hl),a
	ret

;
; _lcmp - compare _lL vs _lR, set flags
; Returns: Z if equal, C if _lL < _lR (unsigned)
;
_lcmp::
	ld	hl,(_lL+2)
	ld	de,(_lR+2)
	or	a
	sbc	hl,de
	ret	nz		; high words differ
	ld	hl,(_lL)
	ld	de,(_lR)
	or	a
	sbc	hl,de
	ret

;
; _lshl - _lR <<= A
;
_lshl::
	or	a
	ret	z
	ld	b,a
_lshl1:
	ld	hl,_lR
	sla	(hl)
	inc	hl
	rl	(hl)
	inc	hl
	rl	(hl)
	inc	hl
	rl	(hl)
	djnz	_lshl1
	ret

;
; _lshr - _lR >>= A (logical, unsigned)
;
_lshr::
	or	a
	ret	z
	ld	b,a
_lshr1:
	ld	hl,_lR+3
	srl	(hl)
	dec	hl
	rr	(hl)
	dec	hl
	rr	(hl)
	dec	hl
	rr	(hl)
	djnz	_lshr1
	ret

;
; _lashr - _lR >>= A (arithmetic, signed)
;
_lashr::
	or	a
	ret	z
	ld	b,a
_lashr1:
	ld	hl,_lR+3
	sra	(hl)
	dec	hl
	rr	(hl)
	dec	hl
	rr	(hl)
	dec	hl
	rr	(hl)
	djnz	_lashr1
	ret

;
; vim: tabstop=8 shiftwidth=8 noexpandtab:
;
