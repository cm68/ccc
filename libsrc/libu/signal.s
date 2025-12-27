.globl	_signal
.text
_signal:
	push	iy
	ld	iy,0
	add	iy,sp
; sig=4
; handler=6
	jp	L1
L2:; ret=177766
	ld	l,(iy+4)
	ld	h,(iy+5)
	ld	de,1
or	a
sbc	hl,de
call jgt	L10000
	ld	l,(iy+4)
	ld	h,(iy+5)
	ld	de,15
or	a
sbc	hl,de
jp p,	L4
L10000:	ld	hl,-1
	jp	L3
L4:	ld	l,(iy+6)
	ld	h,(iy+7)
ld	a,h
or	l
jp z,	L10001
	ld	l,(iy+6)
	ld	h,(iy+7)
	ld	de,1
or	a
sbc	hl,de
jp nz,	L5
L10001:ld	hl,__signal
	ld	(iy-10),l
	ld	(iy-9),h
	jp	L6
L5:	ld	l,(iy+4)
	ld	h,(iy+5)
add hl,hl
ld	de,(iy+6)
ld	(hl),e
inc	hl
ld	(hl),d
ld	hl,__signal
	ld	(iy-10),l
	ld	(iy-9),h
L6:	ld	l,(iy-10)
	ld	h,(iy-9)
	ld	de,1
or	a
sbc	hl,de
jp z,	L7
	ld	l,(iy-10)
	ld	h,(iy-9)
ld	a,h
or	l
jp z,	L7
	ld	l,(iy-10)
	ld	h,(iy-9)
	ld	de,-1
or	a
sbc	hl,de
jp z,	L7
	ld	l,(iy+4)
	ld	h,(iy+5)
add hl,hl
ex	de,hl
ld	e,(hl)
inc	hl
ld	d,(hl)
ex	de,hl
	ld	(iy-10),l
	ld	(iy-9),h
L7:	ld	l,(iy-10)
	ld	h,(iy-9)
	jp	L3
L3:	ld	sp,iy
	pop	iy
	ret
L1:	dec	sp
	dec	sp
	jp	L2
.bss
_stab:	.ds 30
.data
