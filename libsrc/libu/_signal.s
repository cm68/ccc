; Generated from lib/libu/_signal.o by wsnm -g
	.extern c.ihl
	.extern _errno
	.global __signal
	.global __stab
	.global __jtab

	.text
_.text:
__signal:
	ld hl,0002h
	add hl,sp
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	ld (_.data+2),hl
	ld hl,0004h
	add hl,sp
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	ld (_.data+4),hl
	rst 08h
	.db 000h
	.dw _.data+0
	ret nc
	ld (_errno),hl
	ld hl,0ffffh
	ret
__jtab:
	push hl
	ld hl,(_.data+6)
	jp __jtab+105
	push hl
	ld hl,(_.data+8)
	jp __jtab+105
	push hl
	ld hl,(_.data+10)
	jp __jtab+105
	push hl
	ld hl,(_.data+12)
	jp __jtab+105
	push hl
	ld hl,(_.data+14)
	jp __jtab+105
	push hl
	ld hl,(_.data+16)
	jp __jtab+105
	push hl
	ld hl,(_.data+18)
	jp __jtab+105
	push hl
	ld hl,(_.data+20)
	jp __jtab+105
	push hl
	ld hl,(_.data+22)
	jp __jtab+105
	push hl
	ld hl,(_.data+24)
	jp __jtab+105
	push hl
	ld hl,(_.data+26)
	jp __jtab+105
	push hl
	ld hl,(_.data+28)
	jp __jtab+105
	push hl
	ld hl,(_.data+30)
	jp __jtab+105
	push hl
	ld hl,(_.data+32)
	jp __jtab+105
	push hl
	ld hl,(_.data+34)
	jp __jtab+105
	push de
	push bc
	push af
	call c.ihl
	pop af
	pop bc
	pop de
	pop hl
	ret

	.data
_.data:
	.db 0cfh
	.db 030h
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
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
