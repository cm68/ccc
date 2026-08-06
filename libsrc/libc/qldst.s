;	Long (32-bit) load and store through a pointer
;
;	See QLONG.md for the convention.  These replace lld/lldde/lstde,
;	which were ccc's own - zc3 loads and stores a long inline and
;	never called them - so unlike the arithmetic there is no Hi-Tech
;	version to keep alongside.

	psect	text
	global	qld, qldde, qst

; Load 32-bit from (HL) into HL':HL
; Entry: HL = pointer to the long
; Exit:  HL' = high word, HL = low word
; Clobbers: DE, A.  DE':DE must be dead.
;
; The pointer sits in the pair the low word has to end up in, so the
; value is assembled in HL and DE first and the high half handed over
; to HL' through the stack.
qld:
	ld	e,(hl)		;low word, from the lower address
	inc	hl
	ld	d,(hl)
	inc	hl
	ld	a,(hl)		;high word, from the higher one
	inc	hl
	ld	h,(hl)
	ld	l,a		;hl = high word, de = low word
	push	hl
	exx
	pop	hl		;hl' = high word
	exx
	ex	de,hl		;hl = low word
	ret

; Load 32-bit from (DE) into HL':HL
qldde:
	ex	de,hl
	jp	qld

; Store HL':HL through the pointer on the stack
; Entry: HL':HL = the value; the dest pointer is a pushed argument,
;        so it sits above the return address
; Exit:  stored, pointer consumed, and the value still in HL':HL
; Clobbers: DE'.  DE, A and BC come through untouched.
;
; BC being alive is not incidental.  It is the register-variable home,
; and an early version of the Hi-Tech lstde parked the return address
; there: every long store made by a function keeping a pointer in BC -
; tokcpy, with the destination token - handed that pointer back as a
; text address, and the fields stored after the long went through it
; into whatever it now named.  That was cpp's intern pool, six-byte
; token strides at a time.  The fix after that used a scratch word in
; .data, which has no reentrancy at all: a longjmp past a frame midway
; through, or a signal handler that reached stdio, left it holding a
; dead address.
;
; Neither is needed here.  The whole thing runs in the shadow bank,
; where BC' is free and holds the return address, and the low word -
; which starts in the main bank, the wrong side of the exx - comes
; across on the stack.
;
; The parity is the thing to keep hold of.  Between the exx below and
; the one at the end, the register named hl is the HIGH word and the
; LOW word is sitting untouched in the prime, which is exactly where
; the final exx needs it to be.
qst:
	push	hl		;the low word.  stack: low, ret, ptr
	exx			;hl = high word; the low one is safe in hl'
	pop	de		;de' = low word.    stack: ret, ptr
	pop	bc		;bc' = return addr. stack: ptr
	ex	(sp),hl		;hl = dest pointer; the high word is on the stack
	ld	(hl),e
	inc	hl
	ld	(hl),d		;low word down, at the lower address
	inc	hl
	pop	de		;de' = high word.   stack: as the caller left it
	ld	(hl),e
	inc	hl
	ld	(hl),d		;high word above it
	ex	de,hl		;hl = high word again
	push	bc		;the return address, for the ret below
	exx			;hl = low word, hl' = high word, bc the caller's
	ret

; vim: tabstop=4 shiftwidth=4 noexpandtab:
