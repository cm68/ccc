; Generated from lib/libu/uname.o by wsnm -g
	.extern c.r0
	.extern c.ulmod
	.extern c.ret
	.extern _getpid
	.extern c.ent
	.extern _ltob
	.extern _time
	.extern _itob
	.extern _cpystr
	.global _uname

	.text
_.text:
	cpl
	ld (hl),h
	ld l,l
	ld (hl),b
	cpl
	nop
_uname:
	call c.ent
	ld hl,_.data+18
	ld a,(hl)
	inc hl
	or (hl)
	inc hl
	or (hl)
	inc hl
	or (hl)
	jp nz,_uname+56
	ld hl,_.data+18
	push hl
	call _time
	pop af
	ld hl,_.data+18
	push hl
	ld a,027h
	add a,a
	sbc a,a
	ld (c.r0),a
	ld (c.r0+1),a
	ld a,027h
	ld (c.r0+3),a
	ld a,010h
	ld (c.r0+2),a
	ld hl,c.r0
	push hl
	call c.ulmod
	pop af
	ld hl,0000h
	push hl
	ld hl,_.text+0
	push hl
	ld hl,_.data+0
	push hl
	call _cpystr
	pop af
	pop af
	pop af
	ld l,c
	ld h,b
	ld (_.data+16),hl
	ld hl,000ah
	push hl
	call _getpid
	push bc
	ld hl,(_.data+16)
	push hl
	call _itob
	pop af
	pop af
	pop af
	ld hl,(_.data+16)
	add hl,bc
	ld (_.data+16),hl
	ld hl,(_.data+16)
	push hl
	ld hl,(_.data+16)
	inc hl
	ld (_.data+16),hl
	pop hl
	ld (hl),02dh
	ld hl,000ah
	push hl
	ld hl,_.data+18
	inc hl
	inc hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	push bc
	dec hl
	dec hl
	dec hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	push bc
	ld hl,(_.data+16)
	push hl
	call _ltob
	pop af
	pop af
	pop af
	pop af
	ld hl,(_.data+16)
	add hl,bc
	ld (_.data+16),hl
	ld hl,(_.data+16)
	ld (hl),000h
	ld hl,_.data+0
	ld c,l
	ld b,h
	jp c.ret

	.data
_.data:
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h
	.db 000h

; vim: tabstop=4 shiftwidth=4 noexpandtab:

