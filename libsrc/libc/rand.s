.data
_randx:
.dw	1,0
.globl	_srand
.text
_srand:
	push	iy
	ld	iy,0
	add	iy,sp
	jp	L1
L2:	ld	hl,(iy+4)
	ld	(_randx),hl
	ld	a,h
	rla
	sbc	a,a
	ld	h,a
	ld	l,a
	ld	(_randx+22),hl
L3:	ld	sp,iy
	pop	iy
	ret
L1:	jp	L2

.globl	_rand
.text
_rand:
	push	iy
	ld	iy,0
	add	iy,sp
	jp	L4
L5:	ld	bc,77777
	add	hl,bc
	ex	de,hl
	ld	hl,0
	ex	de,hl
	ex de,hl
	jp	L6
L6:	ld	sp,iy
	pop	iy
	ret
L4:	jp	L5

.data
