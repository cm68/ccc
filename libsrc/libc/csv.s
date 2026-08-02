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
;	The frame allocation stays inline: it is a constant the caller
;	knows and one add.  The register saves do not - see below.

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

;	Callee-save exits.  A function that keeps a variable in BC or in
;	IX has to hand the caller's back, and that was written out at
;	the end of every one of them: twelve bytes for IX, because it
;	has to come back through A to leave HL and the flags alone, and
;	six for BC.  Two thousand bytes of it in c1 alone.
;
;	Here instead, once.  The saved pair sits just under the scalar
;	area, so where it is depends on the frame - the caller says so
;	with a word after the call, the offset of the LOWER save from
;	the frame pointer, and the helper points the stack at it and
;	pops.  Five bytes at the call site against twenty-one.
;
;	The shadow registers are what make it free: the offset has to be
;	read and added somewhere, and every ordinary register is either
;	the return value (HL, and DE with it for a long) or one of the
;	pair being restored.  exx and ex af,af' cost a byte each and
;	touch nothing the caller can see.  _signal saves the shadow set
;	around a handler, so a signal arriving inside here is
;	transparent.

	global	fexbx, fexx, fexb

fexbx:				;restore IX and BC
	ex	af,af'
	exx
	pop	hl		;the word after the call
	ld	c,(hl)
	inc	hl
	ld	b,(hl)		;bc = offset of the lower save from iy
	push	iy
	pop	hl
	add	hl,bc
	ld	sp,hl		;stack now points at the saves
	exx
	ex	af,af'
	pop	ix		;pushed last, so it is underneath
	pop	bc
	jr	fxdone

fexx:				;restore IX alone
	ex	af,af'
	exx
	pop	hl
	ld	c,(hl)
	inc	hl
	ld	b,(hl)
	push	iy
	pop	hl
	add	hl,bc
	ld	sp,hl
	exx
	ex	af,af'
	pop	ix
	jr	fxdone

fexb:				;restore BC alone
	ex	af,af'
	exx
	pop	hl
	ld	c,(hl)
	inc	hl
	ld	b,(hl)
	push	iy
	pop	hl
	add	hl,bc
	ld	sp,hl
	exx
	ex	af,af'
	pop	bc

fxdone:
	ld	sp,iy
	pop	iy
	ret

;	And the entries that match them.  A prologue was the frame
;	pointer (three, through fenter), the scalar area (five, and it
;	is a constant), and the pushes (three) - eleven bytes, of which
;	only the constant is particular to the function.  These take it
;	as the word after the call, so the whole prologue is five.
;
;	The order matters and is the old one exactly: frame pointer,
;	then the scalar area, then the saves just under it, so that the
;	saves stay inside the 7-bit (iy+d) window and everything the
;	compiler has already emitted still means what it did.  Arrays
;	are allocated by the caller afterwards; they live below the
;	saves and are reached by arithmetic, not by (iy+d).

	global	fentbx, fentx, fentb, fentn

fentbx:				;frame, scalar area, save IX and BC
	pop	hl		;-> the word after the call
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	inc	hl		;hl = return address, de = -scalars
	push	iy
	ld	iy,0
	add	iy,sp		;new frame pointer
	ex	de,hl
	add	hl,sp
	ld	sp,hl		;scalar area allocated
	ex	de,hl		;hl = return address
	push	bc
	push	ix
	jp	(hl)

fentx:				;the same, saving IX alone
	pop	hl
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	inc	hl
	push	iy
	ld	iy,0
	add	iy,sp
	ex	de,hl
	add	hl,sp
	ld	sp,hl
	ex	de,hl
	push	ix
	jp	(hl)

fentb:				;saving BC alone
	pop	hl
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	inc	hl
	push	iy
	ld	iy,0
	add	iy,sp
	ex	de,hl
	add	hl,sp
	ld	sp,hl
	ex	de,hl
	push	bc
	jp	(hl)

fentn:				;no saves, but a scalar area to allocate
	pop	hl
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	inc	hl
	push	iy
	ld	iy,0
	add	iy,sp
	ex	de,hl
	add	hl,sp
	ld	sp,hl
	ex	de,hl
	jp	(hl)

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
;	The saved copies cannot go on the stack: the bodies read their
;	arguments by popping them, so a copy parked there would come
;	back as an argument.  They go in a small stack of their own,
;	four deep - fputc calls itself to put the carriage return in
;	front of a newline, so one slot is not enough, and a depth
;	beyond two would mean a library routine calling a library
;	routine calling itself.
;
;	Nothing here is live on entry to a C function but BC, whose
;	whole point this is, so hl and de are free scratch.  The
;	restore has to be more careful: hl is the return value and de
;	is the rest of it when the value is long, so it saves hl and
;	touches neither de nor the flags.

	global	bcsv

bcsv:
	pop	hl		;body address (the call pushed it)
	ld	de,(bcsp)
	ex	de,hl		;hl = the free slot, de = body address
	ld	(hl),c
	inc	hl
	ld	(hl),b
	inc	hl
	ld	(bcsp),hl
	ld	hl,bcret
	push	hl		;body returns through the restore
	ex	de,hl		;hl = body address
	jp	(hl)

bcret:
	push	hl		;the return value
	ld	hl,(bcsp)
	dec	hl
	ld	b,(hl)
	dec	hl
	ld	c,(hl)
	ld	(bcsp),hl
	pop	hl
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
bcsp:	.dw	bcsave		;next free slot
bcsave:	.dw	0,0,0,0		;four deep: see bcsv
