;
; ldec.s - 32-bit in-memory pre-decrement
;
.globl ldec

.text

;
; long pre-decrement: --(*hl)
; HL = address of long
; Returns new value in HL'HL
;
ldec::
	push	de
	; decrement low byte - borrow if result is FF
	ld	a,(hl)
	dec	a
	ld	(hl),a
	inc	a		; test if was 0 (now FF)
	jr	nz,ldecdone
	; borrow from byte 1
	inc	hl
	ld	a,(hl)
	dec	a
	ld	(hl),a
	inc	a
	jr	nz,ldecdone1
	; borrow from byte 2
	inc	hl
	ld	a,(hl)
	dec	a
	ld	(hl),a
	inc	a
	jr	nz,ldecdone2
	; borrow from byte 3
	inc	hl
	dec	(hl)
	dec	hl
ldecdone2:
	dec	hl
ldecdone1:
	dec	hl
ldecdone:
	; load result into hl'hl
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	inc	hl
	push	de
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	exx
	ex	de,hl
	exx
	pop	hl
	pop	de
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
