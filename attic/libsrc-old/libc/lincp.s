;
; lincp.s - 32-bit in-memory post-increment
;
.globl lincp

.text

;
; long post-increment: (*hl)++, return old value
; HL = address of long
; Returns old value in HL'HL
;
lincp::
	push	de
	; load old value into hl'hl
	ld	e,(hl)
	push	hl		; save address
	inc	hl
	ld	d,(hl)
	inc	hl
	push	de		; save low word
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	exx
	ex	de,hl		; hl' = high word
	exx
	pop	hl		; hl = low word (old value)
	pop	de		; de = address
	push	hl		; save old low for return
	; now increment at (de)
	ex	de,hl
	inc	(hl)
	jr	nz,lincpdone
	inc	hl
	inc	(hl)
	jr	nz,lincpdone
	inc	hl
	inc	(hl)
	jr	nz,lincpdone
	inc	hl
	inc	(hl)
lincpdone:
	pop	hl		; return old low
	pop	de
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
