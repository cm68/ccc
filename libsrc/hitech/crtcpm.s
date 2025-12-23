;
; cp/m startup code
;
	.extern _exit
	.extern _main

	.text
start:
	ld		hl,(0006h)		; get the bdos address
	ld		sp,hl			; and put the stack right below it

	ld		de,__Lbss		; clear bss
	or		a
	ld		hl,__Hbss
	sbc		hl,de
	jr		z,nobss
	ld		c,l
	ld		b,h
	dec		bc
	ld		l,e
	ld		h,d
	inc		de
	ld		(hl),00h
	ldir

nobss:
	ld		a,(80)			; allocate stack space for argv
	inc		a
	neg
	ld		l,a
	ld		h,-1
	add		hl,sp			; sp -= (cmdlen + 1)
	ld		sp,hl

	ld		bc,0			; flag end of args
	push	bc

	ld		hl,80h			; address of argument buffer
	ld		c,(hl)			; c = cmd len
	ld		b,0
	add		hl,bc			; point at last char in cmd
	ld		b,c				; b = cmd len
	ex		de,hl			; save end in de

	ld		hl,(0006)		; get bdos address
	ld		c,1				; argc
	dec		hl				; hl is argv string pointer
	ld		(hl),0			; string terminator
	inc		b
	jr		3f				; enter loop

2:	ld		a,(de)			; get end character
	cp		a,' '			; is it space?
	dec		de				; bump source
	jr		nz,1f			; not space

	push	hl				; is space, next arg
	inc		c				; argc++

4:	ld		a,(de)			; remove extra spaces
	cp		a,' '
	jr		nz,5f
	dec		de
	djnz	4b
	jr		6f

5:
	xor		a				; change space to null

1:	dec		hl				; bump
	ld		(hl),a			; store argv char
3:
	djnz	2b				; loop for cmd bytes

6:
	push	hl

	ld		hl,0
	add		hl,sp
	push	hl
	push	bc

	call	_main			; exit(main(argc, argv))
	push	hl
	call	_exit
	jp		0000h

;
; these symbols are filled out by the linker
;
	.data
__Htext::	.dw	0
__Ltext::	.dw	0
__Hdata::	.dw	0
__Ldata::	.dw	0
__Hbss::	.dw	0
__Lbss::	.dw	0

;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;

