; switch - switch statement helper
; Input: A = switch value (byte), HL = pointer to jump table
; Table format: count, val1, addr1, val2, addr2, ..., default_addr
;
; Searches table for matching value, jumps to corresponding address.
; If no match found, jumps to default address at end of table.
; Uses DE as temp (BC may hold register variable).

.text
.global switch

switch:
	ld d, (hl)	; D = case count
	inc hl
	ld e, a		; E = value to search for
.loop:
	ld a, (hl)	; get match byte
	cp e		; check
	inc hl		; point at addr - does not affect z flag
	jr z, .hit	; load and go
	inc hl		; miss: skip handler addr
	inc hl		; point at match byte or default
	dec d
	jr nz, .loop
.hit:
	ld a, (hl)	; load addr low
	inc hl
	ld h, (hl)	; load addr high
	ld l, a
	jp (hl)
