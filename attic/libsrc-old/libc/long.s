;
; long.s - 32-bit load/store helpers for IY and IX indexed access
;
.globl getlong, putlong, getLiy, putLiy, getLix, putLix
.globl indexiy, indexix

.text

;
; index ix by signed value in a
;
indexix::
	push	de		; save register pair
	ld	e,a		; low = offset
	or	a,a
	sbc	a,a		; sign extend
	ld	d,a		; into high
	add	ix,de		; point at destination
	pop	de		; restore register
	ret

;
; write a long word in hl'hl onto the stack frame at offset a
;
putlong::
putLiy::
	push	iy		; save frame pointer
	call	indexiy		; iy = iy + (signed)a
	ld	(iy+0),l
	ld	(iy+1),h
	exx
	ld	(iy+2),l
	ld	(iy+3),h
	exx
	pop	iy
	ret

;
; read a long word on the stack frame at offset a into hl'hl
;
getlong::
getLiy::
	push	iy		; save frame pointer
	call	indexiy		; iy = iy + (signed)a
	ld	l,(iy+0)
	ld	h,(iy+1)
	exx
	ld	l,(iy+2)
	ld	h,(iy+3)
	exx
	pop	iy
	ret

;
; write a long word in hl'hl to (ix+a)
;
putLix::
	push	ix		; save pointer
	call	indexix		; ix = ix + (signed)a
	ld	(ix+0),l
	ld	(ix+1),h
	exx
	ld	(ix+2),l
	ld	(ix+3),h
	exx
	pop	ix
	ret

;
; read a long word from (ix+a) into hl'hl
;
getLix::
	push	ix		; save pointer
	call	indexix		; ix = ix + (signed)a
	ld	l,(ix+0)
	ld	h,(ix+1)
	exx
	ld	l,(ix+2)
	ld	h,(ix+3)
	exx
	pop	ix
	ret
;
; vim: tabstop=8 shiftwidth=8 noexpandtab:
;
