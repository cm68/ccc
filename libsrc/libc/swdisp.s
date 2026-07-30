;	Switch dispatch helpers for the code generator
;
;	A chain of "cp v / jp z,L" costs 5 bytes a case, which over the
;	tree's own 85 switches and 835 cases came to 4175 bytes of pure
;	dispatch.  Both helpers here put the table inline after the call
;	and read it through the return address, so a switch costs the
;	call and the data and nothing else.  Falling off the end of
;	either table jumps to the byte just past it, which is where the
;	compiler puts the no-match label - so "not found" needs no
;	address stored anywhere.
;
;	Case values are bytes, so a value is one byte and a label two.
;	The compiler picks between these and the chain by counting:
;	the chain is 5n, swtab is 4+3n, swidx is 5+2*span.

	psect	text
	global	swtab, swidx

; Sparse dispatch: scan a table of values for A, jump to its label.
;
; Entry:  A = control value, return address -> table
; Table:  .db n / .db v0..v(n-1) / .dw L(n-1)..L0
;
; The values are contiguous so the scan is one cpir rather than a
; loop, which is 21 T-states a case against the chain's 17 - near
; enough that the size comes free.
;
; cpir leaves HL just past the byte it matched and BC holding what was
; left to scan, and HL+BC is the end of the value array however it
; ended.  That is where the labels start.  With the labels stored in
; reverse, the one wanted sits at HL+BC+2*BC, so the index never has
; to be reconstructed - which is the whole reason they are backwards.
swtab:
	pop	hl		; -> count
	ld	c,(hl)
	inc	hl		; -> values
	ld	b,0
	ld	e,c		; keep n for the no-match path
	cpir
	jr	nz,swtno
	ld	d,b		; slot = hl + 3*bc
	ld	e,c
	add	hl,de
	add	hl,de
	add	hl,de
	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
	jp	(hl)

; nothing matched: hl is the end of the values, so hl+2n is the end of
; the table and the byte after it is the no-match label
swtno:
	ld	d,0
	add	hl,de
	add	hl,de
	jp	(hl)

; Dense dispatch: bias A and index straight into a table of labels.
;
; Entry:  A = control value, return address -> table
; Table:  .db lo / .db span / .dw L[0]..L[span-1]
;
; Gaps inside the span hold the no-match label, which is what makes
; this worth choosing only when the values are dense: every hole costs
; the same two bytes as a case.
;
; A value below lo wraps on the subtract into something large, and
; large is out of range, so one unsigned compare covers both ends.
swidx:
	pop	hl		; -> lo
	sub	(hl)
	inc	hl		; -> span
	ld	e,(hl)
	inc	hl		; -> table
	cp	e
	jr	c,swiin
	ld	d,0		; out of range: past the table
	add	hl,de
	add	hl,de
	jp	(hl)

swiin:
	ld	e,a		; 16-bit index: span can exceed 127,
	ld	d,0		; so this cannot be add a,a
	add	hl,de
	add	hl,de
	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
	jp	(hl)

; vim: tabstop=4 shiftwidth=4 noexpandtab:
