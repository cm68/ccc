; Function: test_if(x:_short_) -> _short_
_test_if:
	; load address of $Ax
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call gt1616
	ld hl, 1
_if_0:
	ld hl, 0
