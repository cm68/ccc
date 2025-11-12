; Function: identity(x:_short_) -> _short_
_identity:
	ld a, 0
	call framealloc
	; load address of $Ax
	; load word from address
	call framefree
; Function: empty() -> _short_
_empty:
	ld hl, 42
