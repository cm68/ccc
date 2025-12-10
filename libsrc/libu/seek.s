;
; seek system call
;
; seek(fd, offset, whence)
;
; Moves the read/write pointer of an open file:
;   whence 0: set to offset (from beginning)
;   whence 1: set to current + offset
;   whence 2: set to end + offset
;   whence 3,4,5: same as 0,1,2 but offset * 512
;
; Seeks are not allowed on pipes, but are allowed on
; character devices (though most ignore them). Seeking
; past end of file and writing creates a hole.
;
; passes fd in hl
;
; returns 0 on success, -1 on failure
;
	.extern _errno
	.global _seek

	.text
_seek:
	pop 	hl		; discard ret addr
	pop 	de		; fd
	pop 	hl		; offset
	ld 	(offset),hl
	pop 	hl		; whence
	ld 	(whence),hl

	ld 	hl,-8		; restore stack
	add 	hl,sp
	ld 	sp,hl

	ex 	de,hl		; fd in hl
	rst 	08h
	.db 	000h
	.dw 	scall
	ex 	de,hl
	ld 	hl,0
	ret 	nc
	ld 	(_errno),de
	dec 	hl
	ret

	.data
scall:	.db 	0cfh
	.db 	013h
offset:	.dw 	0
whence:	.dw 	0

;
; vim: tabstop=8 shiftwidth=8 noexpandtab:
;
