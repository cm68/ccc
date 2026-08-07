;
; frame.s - stack frame allocation/deallocation
;
.globl framealloc, framefree

.text

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
framealloc::
	ex	(sp),iy		; save iy and get ret addr
	push	iy
	pop	de
	ld	iy,0
	add	iy,sp		; IY = SP (points at saved IY)
	; allocate A bytes for locals: SP -= A
	neg			; A = -A (sets C if A was non-zero)
	ld	l,a
	sbc	a,a		; A = 0xFF if C, else 0 (sign extend)
	ld	h,a
	add	hl,sp		; HL = SP - frame_size
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
framefree::
	ld	sp,iy		; SP = IY, free locals, point at saved IY
	pop	iy		; restore caller's frame pointer
	ret			; return to caller (addr is now on top)
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
