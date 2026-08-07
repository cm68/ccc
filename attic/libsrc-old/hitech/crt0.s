;
; c runtime startup code for micronix
; with hl return value for hitech compiler
;
	.extern _exit
	.extern _main
	.global start

	.text
start:
	pop	bc	; this is argc
	ld	hl,0	; load argv
	add	hl,sp
	push	hl
	push	bc
	call	_main	; call main
	push	hl	; return value
	jp	_exit	; wzir

	.data
;
; these symbols are filled out by the linker
;
__Htext::	.dw	0
__Ltext::	.dw	0
__Hdata::	.dw	0
__Ldata::	.dw	0
__Hbss::	.dw	0
__Lbss::	.dw	0

