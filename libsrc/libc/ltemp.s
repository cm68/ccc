;
; ltemp.s - Long (32-bit) temporaries and helpers
;
; Memory-based temps to avoid exx register juggling.
; lL = left operand, lR = right operand
;
.globl lL
.globl lR
.globl lldHL
.globl lldHLR
.globl lstHL
.globl lstHLR
.globl ladd
.globl lsub
.globl land
.globl lor
.globl lxor
.globl lneg
.globl lcom
.globl lcmp
.globl lshl
.globl lshr
.globl lashr

.data
lL:	.ds 4		; left long temp
lR:	.ds 4		; right long temp

.text

;
; lldHL - load 4 bytes from (HL) into lL
;
lldHL::
	ld	de,lL
	ldi
	ldi
	ldi
	ldi
	ret

;
; lldHLR - load 4 bytes from (HL) into lR
;
lldHLR::
	ld	de,lR
	ldi
	ldi
	ldi
	ldi
	ret

;
; lstHL - store 4 bytes from lL to (HL)
;
lstHL::
	ld	de,lL
	ex	de,hl
	ldi
	ldi
	ldi
	ldi
	ret

;
; lstHLR - store 4 bytes from lR to (HL)
;
lstHLR::
	ld	de,lR
	ex	de,hl
	ldi
	ldi
	ldi
	ldi
	ret

;
; ladd - lR = lL + lR
;
ladd::
	ld	hl,(lL)
	ld	de,(lR)
	add	hl,de
	ld	(lR),hl
	ld	hl,(lL+2)
	ld	de,(lR+2)
	adc	hl,de
	ld	(lR+2),hl
	ret

;
; lsub - lR = lL - lR
;
lsub::
	ld	hl,(lL)
	ld	de,(lR)
	or	a
	sbc	hl,de
	ld	(lR),hl
	ld	hl,(lL+2)
	ld	de,(lR+2)
	sbc	hl,de
	ld	(lR+2),hl
	ret

;
; land - lR = lL & lR
;
land::
	ld	hl,lL
	ld	de,lR
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
; lor - lR = lL | lR
;
lor::
	ld	hl,lL
	ld	de,lR
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
; lxor - lR = lL ^ lR
;
lxor::
	ld	hl,lL
	ld	de,lR
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
; lneg - lR = -lR
;
lneg::
	ld	hl,0
	ld	de,(lR)
	or	a
	sbc	hl,de
	ld	(lR),hl
	ld	hl,0
	ld	de,(lR+2)
	sbc	hl,de
	ld	(lR+2),hl
	ret

;
; lcom - lR = ~lR
;
lcom::
	ld	hl,lR
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
; lcmp - compare lL vs lR, set flags
; Returns: Z if equal, C if lL < lR (unsigned)
;
lcmp::
	ld	hl,(lL+2)
	ld	de,(lR+2)
	or	a
	sbc	hl,de
	ret	nz		; high words differ
	ld	hl,(lL)
	ld	de,(lR)
	or	a
	sbc	hl,de
	ret

;
; lshl - lR <<= A
;
lshl::
	or	a
	ret	z
	ld	b,a
lshl1:
	ld	hl,lR
	sla	(hl)
	inc	hl
	rl	(hl)
	inc	hl
	rl	(hl)
	inc	hl
	rl	(hl)
	djnz	lshl1
	ret

;
; lshr - lR >>= A (logical, unsigned)
;
lshr::
	or	a
	ret	z
	ld	b,a
lshr1:
	ld	hl,lR+3
	srl	(hl)
	dec	hl
	rr	(hl)
	dec	hl
	rr	(hl)
	dec	hl
	rr	(hl)
	djnz	lshr1
	ret

;
; lashr - lR >>= A (arithmetic, signed)
;
lashr::
	or	a
	ret	z
	ld	b,a
lashr1:
	ld	hl,lR+3
	sra	(hl)
	dec	hl
	rr	(hl)
	dec	hl
	rr	(hl)
	dec	hl
	rr	(hl)
	djnz	lashr1
	ret

;
; vim: tabstop=8 shiftwidth=8 noexpandtab:
;
