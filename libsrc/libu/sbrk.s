;
; brk/sbrk - user-space break tracking over the _break system call
;
; memtop holds the current break, lazily started at the end of bss
; (__Hbss).  sbrk(n) grows the break by n and returns the OLD break
; (base of the granted region); sbrk(0) returns the current break.
; malloc depends on both.  brk(addr) sets the break, 0/-1 result.
;
; BC is preserved: hitech and ccc keep register variables there.
;
	.extern __break, __Hbss
	.global _brk, _sbrk, _memtop

	.text
_brk:
	pop	de		; return address
	pop	hl		; addr
	push	hl
	push	de
	push	hl		; arg for _break
	call	__break
	pop	de		; clean arg; de = addr
	ld	a,h
	or	l
	ret	nz		; failed: hl = -1
	ld	(_memtop),de
	ret			; hl = 0

_sbrk:
	push	bc		; callee-saved (C register variables)
	ld	hl,4		; [bc][ret][increment]
	add	hl,sp
	ld	e,(hl)
	inc	hl
	ld	d,(hl)		; de = increment
	ld	hl,(_memtop)
	ld	a,h
	or	l
	jr	nz,1f
	ld	hl,__Hbss	; first use: break starts past bss
	ld	(_memtop),hl
1:
	ld	a,d
	or	e
	jr	z,3f		; sbrk(0): return current break
	push	hl		; save old break
	add	hl,de
	push	hl		; arg: new break
	call	__break
	pop	de		; clean arg; de = new break
	ld	a,h
	or	l
	jr	z,2f
	pop	hl		; unwind saved old break
	ld	hl,-1		; failed
	jr	3f
2:
	ld	(_memtop),de
	pop	hl		; return old break
3:
	pop	bc
	ret

	.data
_memtop:
	.dw	0

; vim: tabstop=8 shiftwidth=8 noexpandtab:
