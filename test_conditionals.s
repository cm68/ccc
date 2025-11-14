; Function: main() -> _short_
; Variable lifetimes:
;   b: labels 0-4 (3 refs)
;   a: labels 0-9 (6 refs)
;   result: labels 0-9 (9 refs)
_main:
	ld a, 6
	call framealloc
	; load address of $a
	ld hl, 10
	; op N (0x4e) size=2
	; store word to address
	; load address of $a
	; load word from address
	; op X (0x58) size=4
	ld hl, 5
	call gt1616
	; load address of $result
	ld hl, 100
	; op N (0x4e) size=2
	; store word to address
_if_0:
	; load address of $b
	ld hl, 3
	; op N (0x4e) size=2
	; store word to address
	; load address of $b
	; load word from address
	; op X (0x58) size=4
	ld hl, 5
	call gt1616
	; load address of $result
	ld hl, 200
	; op N (0x4e) size=2
	; store word to address
	; load address of $result
	ld hl, 300
	; op N (0x4e) size=2
	; store word to address
_if_end_2:
	; load address of $a
	; load word from address
	; op X (0x58) size=4
	ld hl, 5
	call gt1616
	; load address of $b
	; load word from address
	; op X (0x58) size=4
	ld hl, 5
	call lt1616
	; load address of $result
	ld hl, 400
	; op N (0x4e) size=2
	; store word to address
_if_4:
_if_3:
	; load address of $a
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call lt1616
	; load address of $result
	ld hl, 1
	; op N (0x4e) size=2
	; store word to address
	; load address of $a
	; load word from address
	; op X (0x58) size=4
	ld hl, 5
	call lt1616
	; load address of $result
	ld hl, 2
	; op N (0x4e) size=2
	; store word to address
	; load address of $a
	; load word from address
	; op X (0x58) size=4
	ld hl, 15
	call lt1616
	; load address of $result
	ld hl, 500
	; op N (0x4e) size=2
	; store word to address
	; load address of $result
	ld hl, 4
	; op N (0x4e) size=2
	; store word to address
_if_end_10:
_if_end_8:
_if_end_6:
	; load address of $result
	; load word from address
	call framefree
