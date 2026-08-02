; this fragment is called to make a stack frame
; generally, this is the function prolog that happens
; right after a C function is called with stacked args

; here's what the stack looks like when we enter

;        arg1
;        arg0
;        caller's caller return address
; sp->   return to function head

;
; and when we exit, via the jp (hl), the stack and registers looks like this
;
;	  arg1
;         arg0
;         return
;         iy
; ix,sp-> ix
;
; so, arg0 is (ix+6) and (ix+7)
;
	global	csv,cret,indir, ncsv
	psect	text
csv:	pop	hl		;return address
	push	iy
	push	ix
	ld	ix,0
	add	ix,sp		;new frame pointer
	jp	(hl)

cret:	ld	sp,ix
	pop	ix
	pop	iy
	ret

indir:	jp	(hl)

;	ccc's own pair.  The frame above is HiTech's: ix is the frame
;	pointer and both index registers are saved.  ccc points iy at
;	the frame and leaves ix free for the code generator to use as a
;	pointer, so it cannot share csv/cret - the offsets would all be
;	wrong.  What these do is exactly what the compiler was writing
;	out at the top and bottom of every function:
;
;		push iy		2
;		ld iy,0		4
;		add iy,sp	2
;	...
;		ld sp,iy	2
;		pop iy		2
;		ret		1
;
;	thirteen bytes in each of them, against three for a call and
;	three for a jump.  The peephole substitutes these; the frame
;	that results is laid out identically, which is what lets it be
;	a substitution and not a change of calling convention - every
;	(iy+d) the compiler already emitted still means what it did.
;
;	fenter clobbers hl, which holds nothing on entry to a function
;	whose arguments came in on the stack.  fexit touches neither hl
;	nor the flags, so it can be jumped to with a return value or a
;	condition already set up.
;
;	Register saves and the frame allocation stay inline: they are
;	conditional, and they have to happen after iy is established.

	global	fenter, fexit

fenter:
	pop	hl		;return address
	push	iy
	ld	iy,0
	add	iy,sp		;new frame pointer
	jp	(hl)

fexit:
	ld	sp,iy
	pop	iy
	ret

;	bcsv: run the rest of this function with BC preserved.
;
;	The register-variable convention keeps a caller's variable in
;	BC across calls.  A handful of hand-written routines in this
;	library use BC as scratch - strcmp pops its return address
;	into it - and until now every ordinary call site in the
;	compiler paid two bytes of push bc/pop bc on their account.
;	The knowledge belongs in the callee: a routine that clobbers
;	BC opens with "call bcsv", three bytes once, and its own ret
;	comes back through the restore below.
;
;	The saved copy lives in a static, not on the stack: a stacked
;	copy would shift every argument offset the body was written
;	against.  Non-reentrant, like lstde's scratch word, and for
;	the same reason acceptable: nothing here recurses.

	global	bcsv

bcsv:
	ld	(bcsave),bc
	pop	hl		;body address (the call pushed it)
	ld	de,bcret
	push	de		;body returns through the restore
	jp	(hl)
bcret:
	ld	bc,(bcsave)
	ret

;	New csv: allocates space for stack based on word following
;	call ncsv

ncsv:
	pop	hl
	push	iy
	push	ix
	ld	ix,0
	add	ix,sp
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	inc	hl
	ex	de,hl
	add	hl,sp
	ld	sp,hl
	ex	de,hl
	jp	(hl)

; vim: tabstop=4 shiftwidth=4 noexpandtab:

	.data
bcsave:	.dw	0
