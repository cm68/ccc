;
; execv and execl - wrappers for exec
;
; execv(name, argv)
; char *name;
; char *argv[];
;
; execl(name, arg0, arg1, ..., argn, 0)
; char *name, *arg0, *arg1, ..., *argn;
;
; Execv is useful when the number of arguments is not known
; in advance. Pointers to the argument strings are collected
; into a list, a null pointer is appended to mark the end,
; and execv is called with the address of the list.
;
; Execl is useful when a known file is being executed with
; known arguments. Any number of arguments may be given,
; but the last must be a 0.
;
	.extern _exec
	.global _execv
	.global _execl

	.text
_execv:
	pop 	hl		; discard ret addr
	pop 	de		; name
	pop 	bc		; argv
	push 	bc
	push 	de
	push 	hl

	push 	bc		; push argv
	push 	de		; push name
	call 	_exec
	pop 	af
	pop 	af
	ret

_execl:
	pop 	hl		; discard ret addr
	pop 	de		; name
	ld 	bc,4
	add 	hl,bc		; hl = &arg0
	push 	hl
	push 	de
	push 	bc		; dummy ret addr

	push 	hl		; push &arg0 as argv
	push 	de		; push name
	call 	_exec
	pop 	af
	pop 	af
	ret

; vim: tabstop=8 shiftwidth=8 noexpandtab:
