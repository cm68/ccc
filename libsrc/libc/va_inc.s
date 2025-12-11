;
; va_inc.s - helpers for stdarg macros
;
.globl ___va_set
.globl ___va_inc

.text

;
; void __va_set(char **ap, char *val)
;
; Sets *ap = val.
;
; Args on stack:
;   IY+4: ap (pointer to va_list)
;   IY+6: val (value to store)
;
___va_set::
	xor	a
	call	framealloc
	; Load ap pointer into HL
	ld	l, (iy + 4)
	ld	h, (iy + 5)
	; Load val into DE
	ld	e, (iy + 6)
	ld	d, (iy + 7)
	; Store val to *ap
	ld	(hl), e
	inc	hl
	ld	(hl), d
	jp	framefree

;
; char *__va_inc(char **ap, int size)
;
; Returns old value of *ap, then increments *ap by size.
; This is: tmp = *ap; *ap += size; return tmp;
;
; Args on stack:
;   IY+4: ap (pointer to va_list pointer)
;   IY+6: size (increment amount)
;
___va_inc::
	xor	a
	call	framealloc
	; Load ap pointer into HL
	ld	l, (iy + 4)
	ld	h, (iy + 5)
	; Save ap pointer in DE for later store
	ld	d, h
	ld	e, l
	; Load *ap (the current va_list value) into HL
	ld	a, (hl)
	inc	hl
	ld	h, (hl)
	ld	l, a
	; Save old *ap in BC (return value)
	ld	b, h
	ld	c, l
	; Load size into HL, add to old *ap
	ld	a, (iy + 6)
	add	a, l
	ld	l, a
	ld	a, (iy + 7)
	adc	a, h
	ld	h, a
	; Store new value back to *ap (DE points to ap)
	ex	de, hl
	ld	(hl), e
	inc	hl
	ld	(hl), d
	; Return old value (BC -> HL)
	ld	h, b
	ld	l, c
	jp	framefree
