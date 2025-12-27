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
.ds 2
.bss
_redone:
.ds 4
.globl	__getargs
.text
__getargs:
	push	iy
	ld	iy,0
	add	iy,sp
; _str=4
; _name=6
	jp	L1
L2:; argv=177766
; fc=177714
; fx=177712
; ap=iy
; cp=177710
; argc=177706
; c=177704
; quote=177702
; i=177700
; j=177676
; argbuf=177056
; dmabuf=176656
; buf=176512
ld	hl,0
ld	hl,0
ld	(_redone+2),hl
	ld h,0
ld	(_redone+1),hl
	ld h,0
ld	(_redone),hl
	ld h,0
	ld	(iy-62),l
	ld	(iy-61),h
ld	hl,(iy+6)
	ld	(_name),hl
ld	hl,(iy+4)
	ld	(_str),hl
	ld	hl,0
ld	a,h
or	l
jp z,	L4
ld	hl,L5
	ld	(_str),hl
L4:ld	hl,_name
	ld	(iy-466),l
	ld	(iy-465),h
ld	hl,1
	ld	(iy-58),l
	ld	(iy-57),h
L6:	ld	hl,(_str)

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
ld	a,h
or	l
jp z,	L7
	ld	l,(iy-58)
	ld	h,(iy-57)
	ld	de,200
or	a
sbc	hl,de
jp nz,	L8
ld	hl,0
push	hl
	ld	hl,L9
push	hl
call	call (hl)_error
L8:L10:	ld	hl,(_nxtch)
ld	((iy-60)),hl
	ld h,0
	push hl
call	call (hl)_isseparator
	ld a,h
	or l
jp z,	L11
	jp	L10
	jp	L10
L11:	ld	l,(iy-60)
	ld	h,(iy-59)
ld	a,h
or	l
jp z,	L7
ld	hl,-1266
	push	hl
	pop	iy
	ld	l,(iy-60)
	ld	h,(iy-59)
	push hl
call	call (hl)_isspecial
	ld a,h
	or l
jp z,	L12
ld	hl,(iy-60)

	ld	l,(iy-60)
	ld	h,(iy-59)
	ld	de,62
or	a
sbc	hl,de
jp nz,	L13
	ld	hl,(_str)

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
	ld	de,62
or	a
sbc	hl,de
jp nz,	L13
ld	hl,_nxtch

L13:	jp	L14
L12:L15:	ld	l,(iy-60)
	ld	h,(iy-59)
ld	a,h
or	l
jp z,	L16
	ld	l,(iy-62)
	ld	h,(iy-61)
ld	a,h
or	l
jp nz,	L10000
	ld	l,(iy-60)
	ld	h,(iy-59)
	push hl
call	call (hl)_isspecial
	ld a,h
	or l
jp nz,	L16
	ld	l,(iy-60)
	ld	h,(iy-59)
	push hl
call	call (hl)_isseparator
	ld a,h
	or l
jp nz,	L16
L10000:	push	iy
	pop	hl
	push	iy
	pop	de
or	a
sbc	hl,de
jp nz,	L17
ld	hl,0
push	hl
	ld	hl,L18
push	hl
call	call (hl)_error
L17:	ld	l,(iy-60)
	ld	h,(iy-59)
	ld	e,(iy-62)
	ld	d,(iy-61)
or	a
sbc	hl,de
jp nz,	L19
ld	((iy-62)),0
	jp	L20
L19:	ld	l,(iy-62)
	ld	h,(iy-61)
ld	a,h
or	l
jp nz,	L21
	ld	l,(iy-60)
	ld	h,(iy-59)
	ld	de,39
or	a
sbc	hl,de
jp z,	L10001
	ld	l,(iy-60)
	ld	h,(iy-59)
	ld	de,34
or	a
sbc	hl,de
jp nz,	L21
L10001:ld	hl,(iy-60)
	ld	(iy-62),l
	ld	(iy-61),h
	jp	L22
L21:	ld	l,(iy-62)
	ld	h,(iy-61)
ld	a,h
or	l
jp z,	L23
ld	hl,200
	ld	(iy-60),l
	ld	(iy-59),h
L23:ld	hl,(iy-60)

L22:L20:	ld	l,(iy-62)
	ld	h,(iy-61)
ld	a,h
or	l
jp nz,	L10002
	ld	hl,(_str)

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
	push hl
call	call (hl)_isspecial
	ld a,h
	or l
jp nz,	L16
L10002:ld	hl,_nxtch
	ld	(iy-60),l
	ld	(iy-59),h
	jp	L15
L16:L14:ld	((iy+0)),0
	push	iy
	pop	hl
push	hl
ld	de,-1266
pop	hl
add hl,de
push	hl
call	call (hl)_iswild
	ld a,h
	or l
jp z,	L24
ld	hl,-1266
	push	hl
	pop	iy
L25:	ld	l,(iy+0)
	ld	h,(iy+1)
ld	de,4
ld	a,(hl)
and	e
ld	e,a
inc	hl
ld	a,(hl)
and	d
or	e
jp z,	L26
inc hl	(iy)
	jp	L25
L26:	ld	l,(iy+0)
	ld	h,(iy+1)
	ld	de,58
or	a
sbc	hl,de
jp nz,	L10004
	push	iy
	pop	hl
	push	iy
	pop	de
or	a
sbc	hl,de
jp nz,	L10003
L10004:ld	hl,0
	jp	L10005
L10003:	ld	hl,1
L10005:	ld	(iy-62),l
	ld	(iy-61),h
	push	iy
	pop	hl
push	hl
ld	de,-1266
pop	hl
add hl,de
push	hl
	push	iy
	pop	hl
push	hl
ld	de,-64
pop	hl
add hl,de
push	hl
call	call (hl)_setfcb
	push	iy
	pop	hl
push	hl
ld	de,-1122
pop	hl
add hl,de
push	hl
	ld	hl,26
push	hl
call	call (hl)_bdos
ld	hl,_getuid
	ld	(iy-60),l
	ld	(iy-59),h
ld	a,((iy-11))
ld	l,a
ld	h,0
push	hl
call	call (hl)_setuid
	ld	hl,(_bdos)
ld	((iy-66)),hl
ld	de,377
call cmp16
jp z,	L27
L30:ld	hl,-1266
	push	hl
	pop	iy
	ld	l,(iy-62)
	ld	h,(iy-61)
ld	a,h
or	l
jp z,	L31
ld	a,((iy-11))
ld	l,a
ld	h,0
push	hl
	ld	hl,L32
push	hl
	push	iy
	pop	hl
push	hl
call	call (hl)_sprintf
ld	hl,_strlen
	push	hl
	pop	iy
L31:	ld	l,(iy-52)
	ld	h,(iy-51)
ld	a,h
or	l
jp z,	L33
ld	hl,(iy-52)

ld	hl,72

L33:	ld	l,(iy-66)
	ld	h,(iy-65)
ld	de,5
add hl,hl
ld	de,-1122
add hl,de
ld	hl,(iy-54)
	ld	(iy-56),l
	ld	(iy-55),h
L34:	ld	l,(iy-56)
	ld	h,(iy-55)

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
	ld	de,32
or	a
sbc	hl,de
jp z,	L35
	ld	l,(iy-54)
	ld	h,(iy-53)
	ld	e,(iy-56)
	ld	d,(iy-55)
or	a
sbc	hl,de
jp nc,	L35
	ld	l,(iy-56)
	ld	h,(iy-55)
inc hl	((iy-56))
ex	de,hl
ld	e,(hl)
inc	hl
ld	d,(hl)
ex	de,hl

	jp	L34
L35:ld	hl,56

ld	hl,(iy-54)
	ld	(iy-56),l
	ld	(iy-55),h
L36:	ld	l,(iy-56)
	ld	h,(iy-55)

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
	ld	de,32
or	a
sbc	hl,de
jp z,	L37
	ld	l,(iy-54)
	ld	h,(iy-53)
	ld	e,(iy-56)
	ld	d,(iy-55)
or	a
sbc	hl,de
jp nc,	L37
	ld	l,(iy-56)
	ld	h,(iy-55)
inc hl	((iy-56))
ex	de,hl
ld	e,(hl)
inc	hl
ld	d,(hl)
ex	de,hl

	jp	L36
L37:ld	((iy)),0
	push	iy
	pop	hl
push	hl
ld	de,-1266
pop	hl
add hl,de
push	hl
	ld	l,(iy-58)
	ld	h,(iy-57)
add hl,hl
inc hl	((iy-58))
ld	de,iy
add hl,de
ld	de,_alloc
ld	(hl),e
inc	hl
ld	(hl),d
ex	de,hl
	push hl
call	call (hl)_strcpy
L28:	ld	hl,(_bdos)
ld	((iy-66)),hl
ld	de,377
call cmp16
jp nz,	L30
L29:	jp	L38
L27:	ld	l,(iy-60)
	ld	h,(iy-59)
	push hl
call	call (hl)_setuid
ld	hl,0
push	hl
	ld	hl,L39
push	hl
	push	iy
	pop	hl
push	hl
ld	de,-1266
pop	hl
add hl,de
push	hl
call	call (hl)_error
L38:	ld	l,(iy-60)
	ld	h,(iy-59)
	push hl
call	call (hl)_setuid
	jp	L40
L24:ld	hl,_alloc
	push	hl
	pop	iy
	ld	l,(iy-58)
	ld	h,(iy-57)
add hl,hl
inc hl	((iy-58))
ld	de,iy
add hl,de
ld	de,iy
ld	(hl),e
inc	hl
ld	(hl),d
ld	hl,iy
	ld	(iy-56),l
	ld	(iy-55),h
L43:ld	hl,((iy-56))

L41:	ld	l,(iy-56)
	ld	h,(iy-55)
inc hl	((iy-56))
ld	a,(hl)
inc	hl
or	(hl)
jp nz,	L43
L42:L40:	jp	L6
L7:ld	hl,0
ld	((iy-66)),hl
	ld	(iy-64),l
	ld	(iy-63),h
	jp	L46
L47:	jp	L48
L44:inc hl	((iy-66))
L46:	ld	l,(iy-66)
	ld	h,(iy-65)
	ld	e,(iy-58)
	ld	d,(iy-57)
or	a
sbc	hl,de
call jhi	L47
	jp	L45
L48:	ld	l,(iy-66)
	ld	h,(iy-65)
add hl,hl
ld	de,iy
add hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
ld	((iy-60)),hl
	ld h,0
	push hl
call	call (hl)_isspecial
	ld a,h
	or l
jp z,	L49
	ld	l,(iy-58)
	ld	h,(iy-57)
	ld	e,(iy-66)
	ld	d,(iy-65)
or	a
sbc	hl,de
jp nz,	L50
ld	hl,0
push	hl
	ld	l,(iy-66)
	ld	h,(iy-65)
add hl,hl
ld	de,iy
add hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
push	hl
	ld	hl,L51
push	hl
call	call (hl)_error
L50:	ld	l,(iy-60)
	ld	h,(iy-59)
	ld	de,60
or	a
sbc	hl,de
jp nz,	L52
	ld	hl,(_stdin)
push	hl
	ld	hl,L54
push	hl
	ld	l,(iy-66)
	ld	h,(iy-65)
add hl,hl
ld	de,iy
add hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
push	hl
	ld	hl,L53
push	hl
call	call (hl)_redirect
	jp	L55
L52:	ld	l,(iy-66)
	ld	h,(iy-65)
add hl,hl
ld	de,iy
add hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
ld	de,76
or	a
sbc	hl,de
jp z,	L10006
ld	hl,0
	jp	L10007
L10006:	ld	hl,1
L10007:	push	hl
	pop	iy
	ld	hl,(_stdout)
push	hl
	push	iy
	pop	hl
push	hl
	ld	l,(iy-66)
	ld	h,(iy-65)
add hl,hl
ld	de,iy
add hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
push	hl
	ld	hl,L58
push	hl
call	call (hl)_redirect
L55:inc hl	((iy-66))
	jp	L59
L49:	ld	l,(iy-64)
	ld	h,(iy-63)
add hl,hl
inc hl	((iy-64))
ld	de,iy
add hl,de
	ld	l,(iy-66)
	ld	h,(iy-65)
add hl,hl
ld	de,iy
add hl,de
push	hl
ex	de,hl
ld	e,(hl)
inc	hl
ld	d,(hl)
ex	de,hl
ex	de,hl
pop	hl
ld	(hl),e
inc	hl
ld	(hl),d
L59:	jp	L44
L45:ld	hl,(iy-64)
	ld	(__argc_),hl
ld	hl,0
	ld	l,(iy-64)
	ld	h,(iy-63)
add hl,hl
	push hl
call	call (hl)_alloc
	ld	l,(iy-64)
	ld	h,(iy-63)
add hl,hl
	push hl
	ld	l,(iy-10)
	ld	h,(iy-9)
push	hl
	push	iy
	pop	hl
push	hl
ld	de,-722
pop	hl
add hl,de
push	hl
call	call (hl)_bmove
	pop	de
	pop	de
	ld	l,(iy-10)
	ld	h,(iy-9)
	jp	L3
L3:	ld	sp,iy
	pop	iy
	ret
L1:	ld	hl,-1256
	add	hl,sp
	ld	sp,hl
	jp	L2
.text
_nxtch:
	push	iy
	ld	iy,0
	add	iy,sp
	jp	L60
L61:	ld	hl,(_interactive)
ld	a,h
or	l
jp z,	L63
	ld	hl,(_str)

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
	ld	de,92
or	a
sbc	hl,de
jp nz,	L63
	ld	hl,(_str)
ld	a,(hl)
inc	hl
or	(hl)
jp nz,	L63
	ld	hl,(_bp)
ld	a,h
or	l
jp nz,	L64
ld	hl,_alloc
	ld	(_bp),hl
L64:	ld	hl,(_stdin)
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
ld	de,-400
call andn16
	push hl
call	call (hl)_isatty
	ld a,h
	or l
jp z,	L65
	ld	hl,(_name)
push	hl
	ld	hl,L66
push	hl
	ld	hl,(_stderr)
push	hl
call	call (hl)_fprintf
L65:	ld	hl,(_bp)
push	hl
call	call (hl)_gets
ld	hl,_bp
	ld	(_str),hl
L63:	ld	hl,(_str)

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
ld	a,h
or	l
jp z,	L67
	ld	hl,(_str)

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
inc hl	(_str)
	jp	L62
L67:ld	hl,0
	jp	L62
L62:	ld	sp,iy
	pop	iy
	ret
L60:	jp	L61
.text
_error:
	push	iy
	ld	iy,0
	add	iy,sp
; s=4
	jp	L68
L69:; sp=iy
ld	hl,4
	push	hl
	pop	iy
L71:	ld	l,(iy+0)
	ld	h,(iy+1)
ld	a,h
or	l
jp z,	L72

push	hl
call	call (hl)_sputs
	jp	L71
L72:	ld	hl,L73
push	hl
call	call (hl)_sputs
	ld	hl,-1
push	hl
call	call (hl)_exit
L70:	ld	sp,iy
	pop	iy
	ret
L68:	jp	L69
.text
_sputs:
	push	iy
	ld	iy,0
	add	iy,sp
ld	hl,(iy+4)
	push	hl
	pop	iy
; s=iy
	jp	L74
L75:L77:	ld	l,(iy+0)
	ld	h,(iy+1)
ld	a,h
or	l
jp z,	L78
	ld	l,(iy+0)
	ld	h,(iy+1)
	ld	de,10
or	a
sbc	hl,de
jp nz,	L79
	ld	hl,13
push	hl
	ld	hl,2
push	hl
call	call (hl)_bdos
L79:
	push hl
	ld	hl,2
push	hl
call	call (hl)_bdos
	pop	de
	jp	L77
L78:L76:	ld	sp,iy
	pop	iy
	ret
L74:	jp	L75
.text
_alloc:
	push	iy
	ld	iy,0
	add	iy,sp
; n=4
	jp	L80
L81:; bp=177766
	ld	hl,-1
ld	a,h
or	l
jp z,	L83
ld	hl,0
push	hl
	ld	hl,L84
push	hl
call	call (hl)_error
L83:	ld	l,(iy-10)
	ld	h,(iy-9)
	jp	L82
L82:	ld	sp,iy
	pop	iy
	ret
L80:	dec	sp
	dec	sp
	jp	L81
.text
_redirect:
	push	iy
	ld	iy,0
	add	iy,sp
; str_name=4
; file_name=6
; mode=10
; stream=12
	jp	L85
L86:	ld	l,(iy+10)
	ld	h,(iy+11)
ld	de,0
ld	bc,__iob
add	hl,bc
ex	de,hl
ld	hl,0
sbc hl,de
ex	de,hl
	push hl
	push de
	pop hl
	ex (sp),hl
	pop de
ld	de,-3
call lshl
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
jp z,	L88
ld	hl,0
push	hl
	ld	hl,L90
push	hl
	ld	l,(iy+4)
	ld	h,(iy+5)
push	hl
	ld	hl,L89
push	hl
call	call (hl)_error
L88:	ld	hl,(_freopen)
	ld	e,(iy+10)
	ld	d,(iy+11)
or	a
sbc	hl,de
jp z,	L91
ld	hl,0
push	hl
	ld	l,(iy+4)
	ld	h,(iy+5)
push	hl
	ld	hl,L93
push	hl
	ld	l,(iy+6)
	ld	h,(iy+7)
push	hl
	ld	hl,L92
push	hl
call	call (hl)_error
L91:L87:	ld	sp,iy
	pop	iy
	ret
L85:	jp	L86
.text
_iswild:
	push	iy
	ld	iy,0
	add	iy,sp
; buf=4
	jp	L94
L95:	ld	hl,42
push	hl
	ld	l,(iy+4)
	ld	h,(iy+5)
push	hl
call	call (hl)_strchr
	ld a,h
	or l
jp nz,	L10008
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
jp nz,	L10008
ld	hl,0
	jp	L10009
L10008:	ld	hl,1
L10009:	jp	L96
L96:	ld	sp,iy
	pop	iy
	ret
L94:	jp	L95
.text
_isspecial:
	push	iy
	ld	iy,0
	add	iy,sp
; c=4
	jp	L97
L98:	ld	l,(iy+4)
	ld	h,(iy+5)
	ld	de,60
or	a
sbc	hl,de
jp z,	L10010
	ld	l,(iy+4)
	ld	h,(iy+5)
	ld	de,62
or	a
sbc	hl,de
jp z,	L10010
ld	hl,0
	jp	L10011
L10010:	ld	hl,1
L10011:	jp	L99
L99:	ld	sp,iy
	pop	iy
	ret
L97:	jp	L98
.text
_isseparator:
	push	iy
	ld	iy,0
	add	iy,sp
; c=4
	jp	L100
L101:	ld	l,(iy+4)
	ld	h,(iy+5)
	ld	de,32
or	a
sbc	hl,de
jp z,	L10012
	ld	l,(iy+4)
	ld	h,(iy+5)
	ld	de,9
or	a
sbc	hl,de
jp z,	L10012
	ld	l,(iy+4)
	ld	h,(iy+5)
	ld	de,10
or	a
sbc	hl,de
jp z,	L10012
ld	hl,0
	jp	L10013
L10012:	ld	hl,1
L10013:	jp	L102
L102:	ld	sp,iy
	pop	iy
	ret
L100:	jp	L101
.data
L5:.db 134,0
L9:.db 164,157,157,40,155,141,156,171,40,141,162,147,165,155,145
.db 156,164,163,0
L18:.db 141,162,147,165,155,145,156,164,40,164,157,157,40,154,157
.db 156,147,0
L32:.db 45,165,72,0
L39:.db 72,40,156,157,40,155,141,164,143,150,0
L51:.db 156,157,40,156,141,155,145,40,141,146,164,145,162,40,0
L53:.db 151,156,160,165,164,0
L54:.db 162,0
L56:.db 141,0
L57:.db 167,0
L58:.db 157,165,164,160,165,164,0
L66:.db 45,163,76,40,0
L73:.db 12,0
L84:.db 156,157,40,162,157,157,155,40,146,157,162,40,141,162,147
.db 165,155,145,156,164,163,0
L89:.db 101,155,142,151,147,165,157,165,163,40,0
L90:.db 40,162,145,144,151,162,145,143,164,151,157,156,0
L92:.db 103,141,156,47,164,40,157,160,145,156,40,0
L93:.db 40,146,157,162,40,0
