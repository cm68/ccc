;
; crt0 for micronix and CCC
;
	.extern _exit
	.extern _main

	.text
_.text:
	ld iy,0		; init frame pointer
	pop bc		; get argc
	ld hl,0000h
	add hl,sp	; get argv
	push hl
	push bc
	call _main	; go run
	push hl		; get return value
	call _exit	; die

