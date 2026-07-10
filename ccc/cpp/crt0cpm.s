	;; crt0cpm.s - minimal CP/M startup for the sdcc build (com-sdcc)
	;;
	;; Loads at 0x100 (CP/M TPA), puts the stack under the BDOS,
	;; copies initialized data, calls main, warm-boots on return.
	;; No argv parsing - the size build never runs.

	.module crt0
	.globl	_main
	.globl	l__INITIALIZER
	.globl	s__INITIALIZER
	.globl	s__INITIALIZED

	.area	_HEADER (ABS)
	.org	0x0100

init:
	ld	hl, (0x0006)	; BDOS entry = top of usable TPA
	ld	sp, hl
	call	gsinit
	call	_main

	.globl	_exit
_exit:
	jp	0x0000		; warm boot

	;; Segment ordering for the linker
	.area	_HOME
	.area	_CODE
	.area	_INITIALIZER
	.area	_GSINIT
	.area	_GSFINAL
	.area	_DATA
	.area	_INITIALIZED
	.area	_BSEG
	.area	_BSS
	.area	_HEAP

	.area	_GSINIT
gsinit::
	ld	bc, #l__INITIALIZER
	ld	a, b
	or	a, c
	jr	Z, gsinit_next
	ld	de, #s__INITIALIZED
	ld	hl, #s__INITIALIZER
	ldir
gsinit_next:

	.area	_GSFINAL
	ret
