; Function: test_multiple_locals() -> _short_
_test_multiple_locals:
	; load address of $a
	ld hl, 10
	; op N (0x4e) size=1
	; store byte to address
	; load address of $b
	ld hl, 20
	; op N (0x4e) size=2
	; store word to address
	; load address of $c
	ld hl, 30
	; store long to address
	; load address of $d
	ld hl, 40
	; op N (0x4e) size=1
	; store byte to address
	; load address of $e
	ld hl, 50
	; op N (0x4e) size=2
	; store word to address
	; load address of $b
	; load word from address
	; load address of $e
	; load word from address
	call add1616
