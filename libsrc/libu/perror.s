;
; perror: perror(char *message)
;
	.extern _errno

	.text
_perror::
	pop	hl
	pop	de	; get message pointer
	push	de
	push	hl

	ld	a,(_errno)
	bit	7,a
	jr	nz,unkerr	
	cp	a,32
	jr	nc,ercalc
unkerr:	xor	a
ercalc:	add	a,a
	ld	hl,etab
	add	a,l
	ld	l,a
	jr	nc,erget
ercary:	inc	h
erget:	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
	push	hl
	ex	de,hl
	call	wrtz
	ld	hl,colon
	call	wrtz
	pop	hl
	call	wrtz
	ld	hl,newln
wrtz:	
	ld	(buf),hl
	ld	de,0
	xor	a
next:	cp	(hl)
	jr	z,go
	inc	de
	inc	hl
	jr	next
go:	ld	(count),de
	ld	hl,2
	rst	08h
	.db	4
buf:	.dw	0
count:	.dw	0
	ret

	.data

etab:	.dw	eunk, eperm, enoent, esrch
	.dw	eintr, eio, enxio, e2big
	.dw	enoexec, ebadf, echild, eagain
	.dw	enomem, eaccess, esys, enotblk
	.dw	ebusy, eexist, exdev, enodev
	.dw	enotdir, eisdir, einval, enfile
	.dw	emfile, enotty, etxtbsy, efbig
	.dw	enospc, espipe, erofs, emlink
	.dw	epipe

eunk:	.db	"Unknown error",0
eperm:	.db	"Not super-user",0
enoent:	.db	"No such file or directory",0
esrch:	.db	"No such process",0
eintr:	.db	"Interrupted system call",0
eio:	.db	"I/O error",0
enxio:	.db	"No such device or address",0
e2big:	.db	"Arg list too long",0
enoexec:.db	"Exec format error",0
ebadf:	.db	"Bad file number",0
echild:	.db	"No children",0
eagain:	.db	"No more processes",0
enomem:	.db	"Not enough memory",0
eaccess:.db	"Permission denied",0
esys:	.db	"System error",0
enotblk:.db	"Block device required",0
ebusy:	.db	"File or device busy",0
eexist:	.db	"File exists",0
exdev:	.db	"Cross-device link",0
enodev:	.db	"No such device",0
enotdir:.db	"Not a directory",0
eisdir:	.db	"Is a directory",0
einval:	.db	"Invalid argument",0
enfile:	.db	"File table overflow",0
emfile:	.db	"Too many open files",0
enotty:	.db	"Not a typewriter",0
etxtbsy:.db	"Text file busy",0
efbig:	.db	"File too large",0
enospc:	.db	"No space left on Device",0
espipe:	.db	"Illegal seek",0
erofs:	.db	"Read-only file system",0
emlink:	.db	"Too many links",0
epipe:	.db	"Broken pipe",0

colon:	.db	" : ", 0
newln:	.db	"\n", 0

;
; vim: tabstop=8 shiftwidth=8 noexpandtab:
;
