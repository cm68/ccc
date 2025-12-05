;
; c compiler helper functions
;

;
; allocate stack frame
; input: A = frame size in bytes (for locals)
;
; on entry (after call framealloc):
;   SP -> [ret to func body]
;         [ret to caller]
;         [args...]
;
; on exit:
;   SP -> [locals...]
;         [saved IY]         <- IY points here
;         [ret to caller]    <- IY + 2
;         [args...]          <- IY + 4
;
framealloc:
	ex	(sp),iy		; save iy and get ret addr
	push	iy
	pop	de
	ld	iy,0
	add	iy,sp		; IY = SP (points at saved IY)
	; allocate A bytes for locals: SP -= A
	ld	h,-1
	neg
	ld	l,a
	add	hl,sp		; HL = SP
	ld	sp,hl
	ex	de,hl
	jp	(hl)		; return to func body

;
; deallocate stack frame and return to caller
; called via: jp framefree
;
; on entry:
;   SP -> [locals...]
;         [saved IY]         <- IY points here
;         [ret to caller]    <- IY + 2
;         [args...]
;
framefree:
	ld	sp,iy		; SP = IY, free locals, point at saved IY
	pop	iy		; restore caller's frame pointer
	ret			; return to caller (addr is now on top)

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
; leave the result in hl.
leaiy:
	push	iy			; 2 save frame pointer
	call	indexiy		; 3 add a to iy
	push	iy			; 2 transfer it to hl
	pop		hl			; 1
	pop		iy			; 2 restore frame pointer
	ret					; 1

;
; ld ix,(iy+a)
;
ldixi:
	call	leaiy		; 3
	ld		a,(hl)
	inc		hl
	ld		h,(hl)
	ld		l,a
	push	hl
	pop		ix
	ret

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

;
; add de'de to hl'hl
;
add32:
	add	hl,de
	exx
	adc	hl,de
	exx
	ret

;
; subtract de'de from hl'hl
;
sub32:
	or	a		; clear carry
	sbc	hl,de
	exx
	sbc	hl,de
	exx
	ret

;
; and de'de into hl'hl
; return z if all zero
;
and32:
	ld	a,h
	and	a,d
	ld	h,a
	ld	a,l
	and	a,e
	exx
	ld	h,a
	ld	a,h
	and	a,d
	ld	h,a
	ld	a,h
	and	a,d
	ld	h,a
	ret

or32:
	ret

xor32:
	ret

eq3232:
	exx
	ld	a,d
	cp	h
	jr	nz,eqlose
	ld	a,e
	cp	l
	exx
	ret	nz
	jr	eq1616
eqlose:
	exx
	ret
; falls into:
;
; compare de, hl. return z if equal
;
eq1616:
	ld	a,d
	cp	h
	ret	nz
	ld	a,e
	cp	l
	ret

;
; compare de, hl. return z if eq, c if gt
; if de >= hl, return nonzero 
;
ge1616:
	ld	a,h		; h - d sets carry if d > h
	cp	a,d
	ret	c
	jr	z,geheq
gelt:
	xor	a,a
	ret
geheq:			; here if h == d
	ld	a,l		; l - e sets carry if e > h
	cp	a,e
	ret	c
	jr	nz,gelt
	inc	a
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
