.bss
_name:
.ds 2
.bss
_str:
.ds 2
.bss
_bp:
.ds 2
.bss
_interactive:
.ds 1
.bss
_redone:
.ds 3
.globl	__getargs
.text
__getargs:
	push	iy
	ld	iy,0
	add	iy,sp
	jp	L1
L2:L3:	ld	sp,iy
	pop	iy
	ret
L1:	jp	L2
.text
_nxtch:
	push	iy
	ld	iy,0
	add	iy,sp
	jp	L4
L5:	ld	hl,(_interactive)
ld	a,h
or	l
jp z,	L7
	ld	hl,(_str)

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
ld	de,134
call cmp16
jp nz,	L7
	ld	hl,(_str)
	ld	hl,1
ld	de,1
call mul16
add hl,de
ld	a,(hl)
inc	hl
or	(hl)
jp nz,	L7
	ld	hl,(_bp)
ld	a,h
or	l
jp nz,	L8
ld	hl,_alloc
	ld	(_bp),hl
L8:	ld	hl,(_stdin)
call	ldi
ld	de,377
call and16
	push hl
call	call (hl)_isatty
	ld a,h
	or l
jp z,	L9
	ld	hl,(_name)
push	hl
	ld	hl,L10
push	hl
	ld	hl,(_stderr)
push	hl
call	call (hl)_fprintf
L9:	ld	hl,(_bp)
push	hl
call	call (hl)_gets
ld	hl,_bp
	ld	(_str),hl
L7:	ld	hl,(_str)

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
ld	a,h
or	l
jp z,	L11
	ld	hl,(_str)

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
	ld	hl,1
ld	de,1
call mul16
	ld	(_str),hl
	jp	L6
L11:ld	hl,0
	jp	L6
L6:	ld	sp,iy
	pop	iy
	ret
L4:	jp	L5
.text
_error:
	push	iy
	ld	iy,0
	add	iy,sp
	jp	L12
L13:ld	hl,(iy+4)
	push	hl
	pop	iy
L15:	push	iy
	pop	hl

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
ld	a,h
or	l
jp z,	L16
	push	iy
	pop	hl

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
push	hl
	ld	hl,1
add hl,hl
	push	hl
	pop	iy
call	call (hl)_sputs
	jp	L15
L16:	ld	hl,L17
push	hl
call	call (hl)_sputs
	ld	hl,-1
push	hl
call	call (hl)_exit
L14:	ld	sp,iy
	pop	iy
	ret
L12:	jp	L13
.text
_sputs:
	push	iy
	ld	iy,0
	add	iy,sp
ld	hl,(iy+4)
	push	hl
	pop	iy
	jp	L18
L19:L21:	push	iy
	pop	hl

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
ld	a,h
or	l
jp z,	L22
	push	iy
	pop	hl

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
ld	de,12
call cmp16
jp nz,	L23
	ld	hl,13
push	hl
	ld	hl,2
push	hl
call	call (hl)_bdos
L23:	push	iy
	pop	hl

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
	push hl
	ld	hl,1
ld	de,1
call mul16
	push	hl
	pop	iy
	ld	hl,2
push	hl
call	call (hl)_bdos
	pop	de
	jp	L21
L22:L20:	ld	sp,iy
	pop	iy
	ret
L18:	jp	L19
.text
_alloc:
	push	iy
	ld	iy,0
	add	iy,sp
	jp	L24
L25:	ld	hl,-1
ld	a,h
or	l
jp z,	L27
ld	hl,0
push	hl
	ld	hl,L28
push	hl
call	call (hl)_error
L27:	ld	l,(iy-2)
	ld	h,(iy-1)
	jp	L26
L26:	ld	sp,iy
	pop	iy
	ret
L24:	jp	L25
.text
_redirect:
	push	iy
	ld	iy,0
	add	iy,sp
	jp	L29
L30:	ld	hl,_redone
	ld	l,(iy+10)
	ld	h,(iy+11)
ld	de,0
ld	bc,__iob
add	hl,bc
ex	de,hl
ld	hl,0
sbc hl,de
ex	de,hl
ld	de,10
call	div16
ld	de,1
call mul16
add hl,de
push	de
ex	de,hl
ld	e,(hl)
inc	hl
ld	d,(hl)
ex	de,hl
ex	(sp),hl
ld	a,(hl)
inc hl	a
ld	(hl),a
inc	hl
ld	a,(hl)
adc hl,de	a
ld	(hl),a
pop	hl
	ld h,0
	ld a,h
	or l
jp z,	L32
ld	hl,0
push	hl
	ld	hl,L34
push	hl
	ld	l,(iy+4)
	ld	h,(iy+5)
push	hl
	ld	hl,L33
push	hl
call	call (hl)_error
L32:	ld	hl,(_freopen)
	ld	e,(iy+10)
	ld	d,(iy+11)
or	a
sbc	hl,de
jp z,	L35
ld	hl,0
push	hl
	ld	l,(iy+4)
	ld	h,(iy+5)
push	hl
	ld	hl,L37
push	hl
	ld	l,(iy+6)
	ld	h,(iy+7)
push	hl
	ld	hl,L36
push	hl
call	call (hl)_error
L35:L31:	ld	sp,iy
	pop	iy
	ret
L29:	jp	L30
.text
_iswild:
	push	iy
	ld	iy,0
	add	iy,sp
	jp	L38
L39:	ld	hl,42
push	hl
	ld	l,(iy+4)
	ld	h,(iy+5)
push	hl
call	call (hl)_strchr
	ld a,h
	or l
jp nz,	L10000
	ld	hl,63
push	hl
	ld	l,(iy+4)
	ld	h,(iy+5)
push	hl
call	_strchr
	pop	de
	pop	de
	ld a,h
	or l
jp nz,	L10000
ld	hl,0
	jp	L10001
L10000:	ld	hl,1
L10001:	jp	L40
L40:	ld	sp,iy
	pop	iy
	ret
L38:	jp	L39
.text
_isspecial:
	push	iy
	ld	iy,0
	add	iy,sp
	jp	L41
L42:	ld	l,(iy+4)
	ld	h,(iy+5)
ld	de,74
call cmp16
jp z,	L10002
	ld	l,(iy+4)
	ld	h,(iy+5)
ld	de,76
call cmp16
jp z,	L10002
ld	hl,0
	jp	L10003
L10002:	ld	hl,1
L10003:	jp	L43
L43:	ld	sp,iy
	pop	iy
	ret
L41:	jp	L42
.text
_isseparator:
	push	iy
	ld	iy,0
	add	iy,sp
	jp	L44
L45:	ld	l,(iy+4)
	ld	h,(iy+5)
ld	de,40
call cmp16
jp z,	L10004
	ld	l,(iy+4)
	ld	h,(iy+5)
ld	de,11
call cmp16
jp z,	L10004
	ld	l,(iy+4)
	ld	h,(iy+5)
ld	de,12
call cmp16
jp z,	L10004
ld	hl,0
	jp	L10005
L10004:	ld	hl,1
L10005:	jp	L46
L46:	ld	sp,iy
	pop	iy
	ret
L44:	jp	L45
.data
L10:.db 45,163,76,40,0
L17:.db 12,0
L28:.db 156,157,40,162,157,157,155,40,146,157,162,40,141,162,147
.db 165,155,145,156,164,163,0
L33:.db 101,155,142,151,147,165,157,165,163,40,0
L34:.db 40,162,145,144,151,162,145,143,164,151,157,156,0
L36:.db 103,141,156,47,164,40,157,160,145,156,40,0
L37:.db 40,146,157,162,40,0
