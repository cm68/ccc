; Function: test(x:_short_) -> _short_
_test:
	ld a, 6
	call framealloc
	; load address of $a
	; load address of $Ax
	; load word from address
	; op X (0x58) size=4
	ld hl, 10
	call add1616
	; op N (0x4e) size=2
	; store word to address
	; load address of $a
	; load word from address
	; op X (0x58) size=4
	ld hl, 5
	call gt1616
	; load address of $b
	; load address of $a
	; load word from address
	; op X (0x58) size=4
	ld hl, 2
	call mul1616
	; op N (0x4e) size=2
	; store word to address
	; load address of $c
	; load address of $b
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call add1616
	; op N (0x4e) size=2
	; store word to address
	; load address of $c
	; load address of $Ax
	; load word from address
	; store word to address
_if_end_1:
	; load address of $c
	; load word from address
	call framefree
