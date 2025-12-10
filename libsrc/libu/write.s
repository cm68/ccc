;
; write system call
;
; write(fd, buf, nbytes)
; char *buf;
;
; Writes nbytes from the indicated buffer to the given
; open file. The number of bytes actually written is
; returned. Unlike read, this number should be the same
; as requested; otherwise, an error is indicated.
;
; passes fd in hl
;
; returns -1 on error, else nbytes written
;
	.extern _errno
	.global _write

	.text
_write:
	pop 	hl		; discard ret addr
	pop 	de		; fd
	pop 	hl		; buf
	ld 	(buf),hl
	pop 	hl		; nbytes
	ld 	(count),hl

	ld 	hl,-8		; restore stack
	add 	hl,sp
	ld 	sp,hl

	ex 	de,hl		; fd in hl
	rst 	08h
	.db 	000h
	.dw 	scall
	ret 	nc		; write count in hl
	ld 	(_errno),hl
	ld 	hl,-1
	ret

	.data
scall:	.db 	0cfh
	.db 	004h
buf:	.dw 	0
count:	.dw 	0

;
; vim: tabstop=8 shiftwidth=8 noexpandtab:
;
