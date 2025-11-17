;
; c compiler helper functions
;

framealloc:	

framefree:

;
; index iy by signed value in a
;
indexiy:
	push	de			; 1 get a register pair
	ld		e,a			; 1 low = offset
	or		a,a			; 1
	sbc		a,a			; 1 sign extend
	ld		d,a			; 1 into high
	add		iy,de		; 2 point at destination
	pop		de			; 1 restore register
	ret

;
; take address of stack frame variable at offset a
;
leaiy:
	push	iy			; 2 save frame pointer
	call	indexiy		; 3 add a to iy
	push	iy			; 2 transfer it to hl
	pop		hl			; 1
	pop		iy			; 2 restore frame pointer
	ret					; 1
;
; write a long word in hl'hl onto the stack frame at offset a
;
putlong:
	push	iy			; 2 get frame pointer
	call	indexiy		; 3 calculate frame offset
	ld		(iy+0),l	; 3 write low low
	ld		(iy+1),h	; 3 write high low
	exx					; 1
	ld		(iy+2),l	; 3 write low high
	ld		(iy+3),h	; 3 write high high
	exx					; 1
	pop		iy			; 2 restore fp
	ret					; 1

;
; read a long word on the stack frame at offset a into hl'hl
;
getlong:
	push	iy			; 2 get frame pointer
	call	indexiy		; 3 calculate frame offset
	ld		l,(iy+0)	; 3 read low low
	ld		h,(iy+1)	; 3 read high low
	exx					; 1
	ld		l,(iy+2)	; 3 read low high
	ld		h,(iy+3)	; 3 read high high
	exx					; 1
	pop		iy			; restore fp
	ret					; 1	

add32:
		ret
sub32:
		ret
and32:
		ret
or32:
		ret

xor32:
		ret

;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
