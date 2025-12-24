; Generated from /vault/big/src/hitech/lib/printf.obj by wsnm -g
	extern	__doprnt
	extern	__iob
	extern	cret
	extern	csv
	extern	indir
	extern	ncsv
	global	_printf

	.text
_printf:
	call csv
	push ix
	pop de
	ld hl,0008h
	add hl,de
	push hl
	ld l,(ix+6)
	ld h,(ix+7)
	push hl
	ld hl,__iob+8
	push hl
	call __doprnt
	pop bc
	pop bc
	pop bc
	jp cret
