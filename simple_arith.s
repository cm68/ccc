; Function: main() -> _short_
_main:
	; load address of $a
	ld hl, 10
	; op N (0x4e) size=2
	; store word to address
	; load address of $b
	ld hl, 20
	; op N (0x4e) size=2
	; store word to address
	; load address of $c
	; load address of $a
	; load word from address
	; load address of $b
	; load word from address
	call add1616
	; store word to address
	; load address of $c
	; load word from address
