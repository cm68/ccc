;
; linc.s - 32-bit in-memory pre-increment
;
.globl linc

.text

;
; long pre-increment: ++(*hl)
; HL = address of long
; Returns new value in HL'HL
;
linc::
	push	de
	; increment low byte
	inc	(hl)
	jr	nz,lincdone
	; carry to byte 1
	inc	hl
	inc	(hl)
	jr	nz,lincdone1
	; carry to byte 2
	inc	hl
	inc	(hl)
	jr	nz,lincdone2
	; carry to byte 3
	inc	hl
	inc	(hl)
	dec	hl
lincdone2:
	dec	hl
lincdone1:
	dec	hl
lincdone:
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
